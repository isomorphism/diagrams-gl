{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
module Diagrams.Backend.GL (
    GL(..),
    GLRenderContext(..),
    Options(..),
    renderGL
    ) where

import Data.Foldable (foldMap, toList)
import qualified Data.Text as T
import Data.Tree (Tree(..))
import qualified Data.Map as M
import Data.Map (Map)
import qualified Data.HashMap.Lazy as HM
import Data.HashMap.Lazy (HashMap)
import Data.Default
import Data.Monoid.Recommend
import Data.Maybe (fromMaybe)
import Data.Word

import Control.Monad.Reader 
import Control.Monad.State
import Data.Typeable
import qualified Data.Vector.Storable as VS

import Diagrams.Core.Compile
import Diagrams.Core.Types (Annotation (..))
import Diagrams.Core.Style
import Diagrams.Attributes

import Diagrams.Prelude hiding (view, local, (.~))
import qualified Diagrams.TwoD.Path as Dia
import Diagrams.TwoD.Path (Clip)
import Codec.Picture

import Graphics.GL
import Graphics.GL.Low.Shader
import Graphics.GL.Low.Internal.Shader
import Graphics.GL.Low.Internal.Types
import Graphics.GL.Low hiding (Texture)

import Control.Lens
import qualified Linear as L
import Linear hiding (_x, _y, R2)


instance (Num n, Additive v) => Parametric (Tangent (FixedSegment v n)) where
    atParam (Tangent s) = atParam . Tangent $ fromFixedSeg s

deriving instance Ord LineCap
deriving instance Ord LineJoin


-- | Render a diagram using OpenGL. 
renderGL :: Options GL V2 Double -> Diagram GL -> IO (GLRenderT IO ())
renderGL opts d = renderDia GL opts d

data GL = GL deriving (Show, Typeable)

type instance V GL = V2
type instance N GL = Double

newtype GLColor = GLColor { glColorRGBA :: (Double, Double, Double, Double) }
  deriving (Eq, Ord, Show, Typeable)

fromColor (GLColor (r,g,b,a)) = V4 r g b a
toColor r g b a = GLColor (r,g,b,a)

-- | Variable context present during a render operation, not known when 
--   processing the diagram initially.
data GLRenderContext = GLRenderContext
    { _viewportSize     :: V2 Int
    , _projectionMatrix :: M44 Float
    , _viewMatrix       :: M44 Float
    , _renderOpts       :: Options GL V2 Double
    } deriving (Typeable)

deriving instance (Eq (Options GL V2 Double)) => Eq GLRenderContext
deriving instance (Ord (Options GL V2 Double)) => Ord GLRenderContext
deriving instance (Show (Options GL V2 Double)) => Show GLRenderContext

-- | Collects the line style information that we can render usefully
data GLLineStyle = GLLineStyle
    { _lsColor :: GLColor
    , _lsWidth :: Double
    , _lsCap   :: LineCap
    , _lsJoin  :: LineJoin
    } deriving (Eq, Ord, Show, Typeable)

-- | Collects the fill style information that we can render usefully
data GLFillStyle = GLFillStyle
    { _fsColor :: GLColor
    } deriving (Eq, Ord, Show, Typeable)

-- | Local diagram drawing context present while preparing the diagram for 
--   rendering. 
data GLDiagramContext = GLDiagramContext
    { _cxtFillStyle :: GLFillStyle
    , _cxtLineStyle :: GLLineStyle
    , _diagramOpts  :: Options GL V2 Double
    } deriving (Typeable)

deriving instance (Eq (Options GL V2 Double)) => Eq GLDiagramContext
deriving instance (Ord (Options GL V2 Double)) => Ord GLDiagramContext
deriving instance (Show (Options GL V2 Double)) => Show GLDiagramContext

initialDiagramContext = GLDiagramContext (GLFillStyle (toColor 0 0 0 0))
                                         (GLLineStyle (toColor 0 0 0 1) 1 def def)

data LineVertex = LV
    { _lvPos     :: Point V2 Double
    , _lvTangent :: V2 Double
    } deriving (Eq, Ord, Show, Typeable)

-- | Global state while preparing a diagram to render.
data GLDiagramState = GLDiagramState
    { _diaImages    :: Map FilePath DynamicImage
    , _diaShader    :: Maybe Program
    }  deriving (Typeable)

type GLDiagramT m = ReaderT GLDiagramContext (StateT GLDiagramState m)
type GLRenderT = ReaderT GLRenderContext

initialDiagramState = GLDiagramState M.empty Nothing

makeLenses ''GLRenderContext
makeLenses ''GLDiagramContext
makeLenses ''GLDiagramState
makeLenses ''GLFillStyle
makeLenses ''GLLineStyle
makeLenses ''LineVertex

instance Backend GL V2 Double where
    newtype Render  GL V2 Double = R (GLDiagramT IO (GLRenderT IO ()))
    type    Result  GL V2 Double = IO (GLRenderT IO ()) 
    data    Options GL V2 Double = GLOptionsV2 
                { _smoothness :: Int
                } deriving (Eq, Ord, Show)
    renderRTree _ opts rt = runRenderGL2 (renderRTreeGL2 rt) cxt st
      where cxt = initialDiagramContext opts
            st  = initialDiagramState


runRenderGL2 :: Render GL V2 Double -> GLDiagramContext -> GLDiagramState -> IO (GLRenderT IO ())
runRenderGL2 (R r) cxt st = evalStateT (runReaderT r cxt) st

localR f (R r) = R $ local f r

instance Monoid (Render GL V2 Double) where
    mempty = R . return $ return ()
    R x `mappend` R y = R $ do
        x' <- x
        y' <- y
        return $ x' >> y'

smoothness :: Lens' (Options GL V2 Double) Int
smoothness = lens _smoothness (\opt s -> opt { _smoothness = s})

renderRTreeGL2 :: RTree GL V2 Double Annotation -> Render GL V2 Double
renderRTreeGL2 (Node (RAnnot _) rs) = foldMap (renderRTreeGL2) rs
renderRTreeGL2 (Node REmpty     rs) = foldMap (renderRTreeGL2) rs
renderRTreeGL2 (Node (RStyle s) rs) = withStyle s $ foldMap (renderRTreeGL2) rs
renderRTreeGL2 (Node (RPrim  p) []) = render GL p
renderRTreeGL2 (Node (RPrim  p) rs) = render GL p  -- this should never happen
                                 `mappend` foldMap (renderRTreeGL2) rs


localStyle :: Attribute V2 Double -> GLDiagramContext -> GLDiagramContext
localStyle (FTexture (Recommend (SC clr))) = cxtFillStyle.fsColor .~ GLColor (colorToSRGBA clr) 
localStyle (LTexture (SC clr)) = cxtLineStyle.lsColor .~ GLColor (colorToSRGBA clr)
localStyle (Opacity t) = id -- TODO
localStyle (LineWidth t) = cxtLineStyle.lsWidth .~ t
localStyle (Clip t) = id -- TODO
localStyle _ = id


withStyle :: Style V2 Double -> Render GL V2 Double -> Render GL V2 Double
withStyle (Style s) = localR . appEndo . foldMap (Endo . localStyle) $ HM.elems s


-- | Get the shader program, loading it if necessary
getShader :: (MonadState GLDiagramState m, MonadIO m) => m Program
getShader = do
    sh <- use diaShader
    case sh of
        Just s -> return s
        Nothing -> do
            prog <- loadProgram ["dia.vert", "dia.frag"]
            diaShader .= Just prog
            return prog

instance Renderable (Path V2 Double) GL where
    render _ p = R $ do
        lstyle <- view cxtLineStyle
        fstyle <- view cxtFillStyle
        let renderl = linesVisible p lstyle
        let renderf = fillsVisible p fstyle
        
        verts <- mapM (foldM connectVerts [] <=< mapM segmentVerts) (fixPath p)
        ixs <- lineIxs verts
        
        let verts' = VS.fromList $ fmap (fmap realToFrac) . toVA <$> concat verts :: VS.Vector (V2 (V2 Float))
        let ixs' = VS.fromList $ fromIntegral <$> ixs :: VS.Vector GLuint
        
        -- quick and dirty line drawing for the sake of checking that it's working
        -- will be replaced with something nicer once other stuff works
        
        p <- getShader
        vbo <- newVBO verts' StaticDraw
        ebl <- newElementArray ixs' StaticDraw
        
        vao <- withNewVAO $ do
            bindVBO vbo
            useProgram p
            setVertexLayout [ Attrib "position" 2 (GLScalarAttrib (GLFloat Single)) 
                            , Attrib "normal" 2 (GLScalarAttrib (GLFloat Single))
                            ]
        
        return $ do
            bindVAO (Just vao)
            useProgram p
            
            proj <- view projectionMatrix
            setUniform44 (T.pack "proj") [proj]
            setUniform4f (T.pack "drawColor") [realToFrac <$> fromColor (lstyle^.lsColor)]
            
            when renderl $ do
                bindElementArray ebl
                drawIndexedLines (fromIntegral . VS.length $ ixs') UIntIndices
                
            return ()

linesVisible :: Path V2 Double -> GLLineStyle -> Bool
linesVisible _ ls = and [ ls^.lsColor /= toColor 0 0 0 0
                        , ls^.lsWidth > 0
                        ]

fillsVisible :: Path V2 Double -> GLFillStyle -> Bool
fillsVisible p fs = and [ fs^.fsColor /= toColor 0 0 0 0
                        , any (isLoop . unLoc) $ pathTrails p
                        ]

lineIxs :: [[LineVertex]] -> GLDiagramT IO [Int]
lineIxs lvs = return $ foldr combineTrailIxs [] (trailIxs <$> lvs)
  where
    trailIxs tvs = zip <*> drop 1 $ [0 .. length tvs - 1]
    combineTrailIxs t1 t2 = (toListOf each =<< t1) ++ ((length t1 +) <$> t2)

toVA :: LineVertex -> V2 (V2 Double)
toVA (LV v n) = V2 (V2 (v^._x) (v^._y)) (V2 (n^._x) (n^._y))

segmentVerts :: FixedSegment V2 Double -> GLDiagramT IO [LineVertex]
segmentVerts seg@(FLinear v1 v2) = return [LV v1 n, LV v2 n]
  where n = seg `tangentAtParam` 0
segmentVerts seg@FCubic{} = do
    smooth <- view $ diagramOpts.smoothness
    return $ getVertInfo <$> [0.0, 1 / fromIntegral smooth .. 1.0]
  where getVertInfo p = LV (seg `atParam` p) (seg `normalAtParam` p)

connectVerts :: [LineVertex] -> [LineVertex] -> GLDiagramT IO [LineVertex]
connectVerts l1 l2 = case (lastOf each l1, firstOf each l2) of
    (Nothing, _) -> return l2
    (_, Nothing) -> return l1
    (Just lv1, Just lv2) -> do
        merge <- closeEnough lv1 lv2
        if merge 
          then return $ l1 ++ drop 1 l2
          else do
            lvj <- joinVerts lv1 lv2
            return $ l1 ++ lvj ++ l2

closeEnough :: LineVertex -> LineVertex -> GLDiagramT IO Bool
closeEnough (LV v1 n1) (LV v2 n2) = return $ any (< ε) [dvx, dvy, dnx, dny]
  where 
    dvx = abs $ v1^._x - v2^._x
    dvy = abs $ v1^._y - v2^._y
    dnx = abs $ n1^._x - n2^._x
    dny = abs $ n1^._y - n2^._y
    ε = 10^^(-9)

joinVerts :: LineVertex -> LineVertex -> GLDiagramT IO [LineVertex]
joinVerts _ _ = return [] -- TODO


_ALineTexture :: Prism' (Attribute V2 Double) (Texture Double)
_ALineTexture = _Attribute . _LineTexture

_AFillTexture :: Prism' (Attribute V2 Double) (Recommend (Texture Double))
_AFillTexture = _Attribute . _FillTexture

_AOpacity :: Prism' (Attribute V2 Double) (Double)
_AOpacity = _Attribute . _Opacity

_clip :: Iso' (Clip Double) [Path V2 Double]
_clip = iso (\(Dia.Clip p) -> p) Dia.Clip

_AClip :: Prism' (Attribute V2 Double) [Path V2 Double]
_AClip = _Attribute . _clip

aGetLineWidth :: Attribute V2 Double -> Maybe Double
aGetLineWidth = preview $ _Attribute . to getLineWidth

pattern LTexture  s <- (preview _ALineTexture -> Just s) 
pattern FTexture  s <- (preview _AFillTexture -> Just s) 
pattern Opacity   s <- (preview _AOpacity -> Just s)
pattern LineWidth s <- (aGetLineWidth -> Just s)
pattern Clip      s <- (preview _AClip -> Just s)

