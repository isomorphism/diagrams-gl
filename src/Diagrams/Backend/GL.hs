{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE StandaloneDeriving #-}
module Diagrams.Backend.GL where

import Data.Foldable (foldMap, toList)
import qualified Data.Text as T
import Data.Text (Text)
import qualified Data.Tree as Tr
import Data.Tree (Tree(..))
import qualified Data.Map as M
import Data.Map (Map)
import Data.Maybe

import Control.Arrow ((&&&))
import Control.Monad
import Control.Monad.Reader 
import Control.Monad.State
import Data.Typeable
import qualified Data.Vector as V
import qualified Data.Vector.Storable as VS

import Diagrams.Core.Compile
import Diagrams.Core.Types (Annotation (..))
import Diagrams.Core.Style

import Diagrams.Prelude hiding (view, (.~))
import Diagrams.TwoD.Adjust
import Diagrams.TwoD.Attributes hiding (FillTexture, LineTexture, LineWidth)
import qualified Diagrams.TwoD.Path as Dia
import Diagrams.TwoD.Size
import Diagrams.TwoD.Text
import Codec.Picture

import Graphics.GL
import Graphics.GL.Low.Shader
import Graphics.GL.Low hiding (Texture)

import Math.Spline
import Control.Lens
import qualified Linear as L
import Linear (V2(..), V3(..), V4(..), M33, M44)


data GL = GL deriving (Show, Typeable)


newtype GLColor = GLColor { glColorRGBA :: (Double, Double, Double, Double) }
  deriving (Eq, Ord, Show, Typeable)

fromColor (GLColor (r,g,b,a)) = V4 r g b a
toColor r g b a = GLColor (r,g,b,a)

-- | Variable context present during a render operation, not known when 
--   processing the diagram initially.
data GLRenderContext r = GLRenderContext
    { _viewportSize     :: V2 Int
    , _projectionMatrix :: M44 Float
    , _viewMatrix       :: M44 Float
    , _renderOpts       :: Options GL r
    } deriving (Typeable)

deriving instance (Eq (Options GL r)) => Eq (GLRenderContext r)
deriving instance (Ord (Options GL r)) => Ord (GLRenderContext r)
deriving instance (Show (Options GL r)) => Show (GLRenderContext r)

--  instance Field1 GLColor 

data GLLineStyle = GLLineStyle
    { _lsColor :: GLColor
    , _lsWidth :: Double
    } deriving (Eq, Ord, Show, Typeable)

-- | Local diagram drawing context present while preparing the diagram for 
--   rendering. 
data GLDiagramContext r = GLDiagramContext
    { _cxtFillColor :: GLColor
    , _cxtLineStyle :: GLLineStyle
    , _diagramOpts  :: Options GL r
    } deriving (Typeable)

deriving instance (Eq (Options GL r)) => Eq (GLDiagramContext r)
deriving instance (Ord (Options GL r)) => Ord (GLDiagramContext r)
deriving instance (Show (Options GL r)) => Show (GLDiagramContext r)

data LineVertex r = LineVertex
    { _lvPos     :: Point r
    , _lvTangent :: r
    } deriving (Eq, Ord, Show, Typeable)

-- | Global state while preparing a diagram to render.
data GLDiagramState r = GLDiagramState
    { _diaImages    :: Map FilePath DynamicImage
    , _diaLines     :: Map GLLineStyle [[LineVertex r]]
    }  deriving (Typeable)

type GLDiagramT r m = ReaderT (GLDiagramContext r) (StateT (GLDiagramState r) m)
type GLRenderT r = ReaderT (GLRenderContext r)

makeLenses ''GLRenderContext
makeLenses ''GLDiagramContext
makeLenses ''GLDiagramState
makeLenses ''GLLineStyle
makeLenses ''LineVertex

instance Backend GL R2 where
    newtype Render  GL R2 = R (GLDiagramT R2 IO (GLRenderT R2 IO ()))
    type    Result  GL R2 = IO (GLRenderT R2 IO ()) 
    data    Options GL R2 = GLOptionsR2 
                { _smoothness :: Int
                } deriving (Eq, Ord, Show)
    renderRTree _ opts rt = runRenderGL2 (renderRTreeGL2 opts rt) cxt st
      where cxt = initialDiagramContext opts
            st  = initialDiagramState

initialDiagramState = GLDiagramState M.empty M.empty
initialDiagramContext = GLDiagramContext (toColor 0 0 0 0) 
                                         (GLLineStyle (toColor 0 0 0 1) 1.0)

runRenderGL2 :: Render GL R2 -> GLDiagramContext R2 -> GLDiagramState R2 -> IO (GLRenderT R2 IO ())
runRenderGL2 (R r) cxt st = evalStateT (runReaderT r cxt) st

localR f (R r) = R $ local f r

instance Monoid (Render GL R2) where
    mempty = R . return $ return ()
    R x `mappend` R y = R $ do
        x' <- x
        y' <- y
        return $ x' >> y'

smoothness :: Lens' (Options GL R2) Int
smoothness = lens _smoothness (\opt s -> opt { _smoothness = s})

renderRTreeGL2 :: Options GL R2 -> RTree GL R2 Annotation -> Render GL R2
renderRTreeGL2 opts (Node (RAnnot a) rs) = foldMap (renderRTreeGL2 opts) rs
renderRTreeGL2 opts (Node REmpty     rs) = foldMap (renderRTreeGL2 opts) rs
renderRTreeGL2 opts (Node (RStyle s) rs) = withStyle s $ foldMap (renderRTreeGL2 opts) rs
renderRTreeGL2 opts (Node (RPrim  p) rs) = R $ undefined

localStyle :: Attribute v -> GLDiagramContext r -> GLDiagramContext r
localStyle (FillTexture (SC clr)) = cxtFillColor .~ GLColor (colorToSRGBA clr)
localStyle (LineTexture (SC clr)) = cxtLineStyle.lsColor .~ GLColor (colorToSRGBA clr)
localStyle (Opacity t) = id
localStyle (LineWidth t) = cxtLineStyle.lsWidth .~ t
localStyle (Clip t) = id
localStyle _ = id



withStyle :: Style v -> Render GL R2 -> Render GL R2
withStyle (Style s) = localR . appEndo . foldMap (Endo . localStyle) $ M.elems s

attrOf :: (AttributeClass a) => (a -> b) -> Attribute v -> Maybe b
attrOf f = fmap f . unwrapAttr

pattern FillTexture s <- (attrOf getFillTexture             -> Just s) 
pattern LineTexture s <- (attrOf getLineTexture             -> Just s)
pattern Opacity     s <- (attrOf getOpacity                 -> Just s)
pattern LineWidth   s <- (attrOf $ fromOutput.getLineWidth  -> Just s)
pattern Clip        s <- (attrOf (\(Dia.Clip p) -> p)       -> Just s)


