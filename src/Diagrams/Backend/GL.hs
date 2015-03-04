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
import Data.Default

import Control.Monad.Reader 
import Control.Monad.State
import Data.Typeable
import qualified Data.Vector.Storable as VS

import Diagrams.Core.Compile
import Diagrams.Core.Types (Annotation (..))
import Diagrams.Core.Style

import Diagrams.Prelude hiding (view, (.~))
import qualified Diagrams.TwoD.Path as Dia
import Codec.Picture

import Graphics.GL
import Graphics.GL.Low.Shader
import Graphics.GL.Low hiding (Texture)

import Control.Lens
import qualified Linear as L
import Linear hiding (_x, _y, R2)


type instance Codomain (Tangent (FixedSegment v)) = v

instance (Num (Scalar v), VectorSpace v) => Parametric (Tangent (FixedSegment v)) where
    atParam (Tangent s) = atParam . Tangent $ fromFixedSeg s

deriving instance Ord LineCap
deriving instance Ord LineJoin


-- | Render a diagram using OpenGL. 
renderGL :: Options GL R2 -> Diagram GL R2 -> IO (GLRenderT R2 IO ())
renderGL opts d = renderDia GL opts d

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
data GLDiagramContext r = GLDiagramContext
    { _cxtFillStyle :: GLFillStyle
    , _cxtLineStyle :: GLLineStyle
    , _diagramOpts  :: Options GL r
    } deriving (Typeable)

deriving instance (Eq (Options GL r)) => Eq (GLDiagramContext r)
deriving instance (Ord (Options GL r)) => Ord (GLDiagramContext r)
deriving instance (Show (Options GL r)) => Show (GLDiagramContext r)

initialDiagramContext = GLDiagramContext (GLFillStyle (toColor 0 0 0 0))
                                         (GLLineStyle (toColor 0 0 0 1) 1.0 def def)

data LineVertex r = LV
    { _lvPos     :: Point r
    , _lvTangent :: r
    } deriving (Eq, Ord, Show, Typeable)

-- | Global state while preparing a diagram to render.
data GLDiagramState r = GLDiagramState
    { _diaImages    :: Map FilePath DynamicImage
    , _diaShader    :: Maybe Program
    }  deriving (Typeable)

type GLDiagramT r m = ReaderT (GLDiagramContext r) (StateT (GLDiagramState r) m)
type GLRenderT r = ReaderT (GLRenderContext r)

initialDiagramState = GLDiagramState M.empty Nothing

makeLenses ''GLRenderContext
makeLenses ''GLDiagramContext
makeLenses ''GLDiagramState
makeLenses ''GLFillStyle
makeLenses ''GLLineStyle
makeLenses ''LineVertex

instance Backend GL R2 where
    newtype Render  GL R2 = R (GLDiagramT R2 IO (GLRenderT R2 IO ()))
    type    Result  GL R2 = IO (GLRenderT R2 IO ()) 
    data    Options GL R2 = GLOptionsR2 
                { _smoothness :: Int
                } deriving (Eq, Ord, Show)
    renderRTree _ opts rt = runRenderGL2 (renderRTreeGL2 rt) cxt st
      where cxt = initialDiagramContext opts
            st  = initialDiagramState


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

renderRTreeGL2 :: RTree GL R2 Annotation -> Render GL R2
renderRTreeGL2 (Node (RAnnot _) rs) = foldMap (renderRTreeGL2) rs
renderRTreeGL2 (Node REmpty     rs) = foldMap (renderRTreeGL2) rs
renderRTreeGL2 (Node (RStyle s) rs) = withStyle s $ foldMap (renderRTreeGL2) rs
renderRTreeGL2 (Node (RPrim  p) []) = render GL p
renderRTreeGL2 (Node (RPrim  p) rs) = render GL p  -- this should never happen
                                 `mappend` foldMap (renderRTreeGL2) rs

localStyle :: Attribute v -> GLDiagramContext r -> GLDiagramContext r
localStyle (FillTexture (SC clr)) = cxtFillStyle.fsColor .~ GLColor (colorToSRGBA clr)
localStyle (LineTexture (SC clr)) = cxtLineStyle.lsColor .~ GLColor (colorToSRGBA clr)
localStyle (Opacity t) = id -- TODO
localStyle (LineWidth t) = cxtLineStyle.lsWidth .~ t
localStyle (Clip t) = id -- TODO
localStyle _ = id

withStyle :: Style v -> Render GL R2 -> Render GL R2
withStyle (Style s) = localR . appEndo . foldMap (Endo . localStyle) $ M.elems s

attrOf :: (AttributeClass a) => (a -> b) -> Attribute v -> Maybe b
attrOf f = fmap f . unwrapAttr

-- | Get the shader program, loading it if necessary
getShader = do
    sh <- use diaShader
    case sh of
        Just s -> return s
        Nothing -> do
            prog <- loadProgram ["dia.vert", "dia.frag"]
            diaShader .= Just prog
            return prog

instance Renderable (Path R2) GL where
    render _ p = R $ do
        lstyle <- view cxtLineStyle
        fstyle <- view cxtFillStyle
        let renderl = linesVisible p lstyle
        let renderf = fillsVisible p fstyle
        verts <- mapM (foldM connectVerts [] <=< mapM segmentVerts) (fixPath p)
        ixs <- lineIxs verts
        
        -- quick and dirty line drawing for the sake of checking that it's working
        -- will be replaced with something nicer once other stuff works
        p <- getShader
        vbo <- newVBO (VS.fromList $ fmap (fmap realToFrac) . toVA <$> concat verts :: VS.Vector (V2 (V2 Float))) StaticDraw
        ebl <- newElementArray (VS.fromList $ fromIntegral <$> ixs :: VS.Vector GLuint) StaticDraw
        
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
                drawIndexedLines (fromIntegral . length $ ixs) UIntIndices
            
            return ()

linesVisible :: Path R2 -> GLLineStyle -> Bool
linesVisible _ ls = and [ ls^.lsColor /= toColor 0 0 0 0
                        , ls^.lsWidth > 0
                        ]

fillsVisible :: Path R2 -> GLFillStyle -> Bool
fillsVisible p fs = and [ fs^.fsColor /= toColor 0 0 0 0
                        , any (isLoop . unLoc) $ pathTrails p
                        ]

lineIxs :: [[LineVertex R2]] -> GLDiagramT R2 IO [Int]
lineIxs lvs = return $ foldr combineTrailIxs [] (trailIxs <$> lvs)
  where
    trailIxs tvs = zip <*> drop 1 $ [0 .. length tvs - 1]
    combineTrailIxs t1 t2 = (toListOf each =<< t1) ++ ((length t1 +) <$> t2)

toVA (LV v n) = V2 (V2 (v^._x) (v^._y)) (V2 (n^._x) (n^._y))

segmentVerts :: FixedSegment R2 -> GLDiagramT R2 IO [LineVertex R2]
segmentVerts seg@(FLinear v1 v2) = return [LV v1 n, LV v2 n]
  where n = seg `tangentAtParam` 0
segmentVerts seg@FCubic{} = do
    smooth <- view $ diagramOpts.smoothness
    return $ getVertInfo <$> [0.0, 1 / fromIntegral smooth .. 1.0]
  where getVertInfo p = LV (seg `atParam` p) (seg `normalAtParam` p)

connectVerts :: [LineVertex R2] -> [LineVertex R2] -> GLDiagramT R2 IO [LineVertex R2]
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

closeEnough :: LineVertex R2 -> LineVertex R2 -> GLDiagramT R2 IO Bool
closeEnough (LV v1 n1) (LV v2 n2) = return $ any (< ε) [dvx, dvy, dnx, dny]
  where 
    dvx = abs $ v1^._x - v2^._x
    dvy = abs $ v1^._y - v2^._y
    dnx = abs $ n1^._x - n2^._x
    dny = abs $ n1^._y - n2^._y
    ε = 10^^(-9)

joinVerts :: LineVertex R2 -> LineVertex R2 -> GLDiagramT R2 IO [LineVertex R2]
joinVerts _ _ = return [] -- TODO



pattern FillTexture s <- (attrOf getFillTexture             -> Just s) 
pattern LineTexture s <- (attrOf getLineTexture             -> Just s)
pattern Opacity     s <- (attrOf getOpacity                 -> Just s)
pattern LineWidth   s <- (attrOf $ fromOutput.getLineWidth  -> Just s)
pattern Clip        s <- (attrOf (\(Dia.Clip p) -> p)       -> Just s)




