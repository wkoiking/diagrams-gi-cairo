{-# LANGUAGE DeriveDataTypeable    #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RecordWildCards       #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE ViewPatterns          #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  Diagrams.Backend.Cairo
-- Copyright   :  (c) 2011-2019 diagrams team (see LICENSE)
-- License     :  BSD-style (see LICENSE)
-- Maintainer  :  diagrams-discuss@googlegroups.com
--
-----------------------------------------------------------------------------
module Diagrams.Backend.Cairo where


-- import           Control.Exception               (try)
import           Control.Monad                   (when)
import           Control.Monad.IO.Class
import           Data.Bits                       (rotateL, (.&.))
import qualified Data.Foldable                   as F
import           Data.Hashable                   (Hashable (..))
import           Data.List                       (isSuffixOf)
import           Data.Maybe                      (fromMaybe, isJust)
import           Data.Typeable
import           Data.Word                       (Word32)
import           System.FilePath
import           System.IO.Unsafe                (unsafePerformIO)

import           Codec.Picture
import           Codec.Picture.Types             (convertImage, packPixel,
                                                  promoteImage)

-- <<< �ǉ�
-- import Foreign.C.Types
import Foreign.ForeignPtr.Unsafe (unsafeForeignPtrToPtr)
-- import Diagrams.TwoD.Image (dimageSize)
-- >>> �ǉ�

import qualified Data.Vector.Storable as SV
import Foreign.ForeignPtr (newForeignPtr)
import Foreign.Marshal.Alloc (finalizerFree)
import Foreign.Marshal.Array (callocArray)
import Foreign.Ptr (Ptr, castPtr)

-- gi-cairo-render
import GI.Cairo.Render (Format (..), formatStrideForWidth, renderWith, withImageSurfaceForData)
import qualified GI.Cairo.Render as C
import qualified GI.Cairo.Render.Matrix as CM
-- gi-cairo-connector
import qualified GI.Cairo.Render.Connector as Connect
-- gi-pango
import qualified GI.Pango as P
-- gi-pangocairo
import qualified GI.PangoCairo.Functions as P (showLayout, createLayout, updateLayout)
-- gi-object
import qualified GI.GObject.Objects.Object as GI (objectUnref)
-- haskell-gi-base
import qualified Data.GI.Base.ManagedPtr as GI (disownObject, disownBoxed)

-- base
import Data.Int (Int32)
-- text
import qualified Data.Text as T
-- safe-exception
import Control.Exception.Safe

-- import Graphics.Rendering.Cairo (Format (..), formatStrideForWidth, renderWith, withImageSurfaceForData)
-- import qualified Graphics.Rendering.Cairo        as C
-- import qualified Graphics.Rendering.Cairo.Matrix as CM
-- import qualified Graphics.Rendering.Pango        as P

import           Diagrams.Backend
import           Diagrams.Backend.Compile
import           Diagrams.Prelude                hiding (clip, opacity, output)
import           Diagrams.TwoD.Text              hiding (Font)
import           Diagrams.Types                  hiding (local)

import           Data.Word                       (Word8)



-- | This data declaration is simply used as a token to distinguish
--   the cairo backend: (1) when calling functions where the type
--   inference engine would otherwise have no way to know which
--   backend you wanted to use, and (2) as an argument to the
--   'Backend' and 'Renderable' type classes.
data Cairo = Cairo
  deriving (Eq,Ord,Read,Show,Typeable)

type instance V Cairo = V2
type instance N Cairo = Double

default2DAttrs :: Diagram V2 -> Diagram V2
default2DAttrs
  = lineWidth medium
  . lineTexture black

instance Backend Cairo where
  type Result  Cairo = C.Render ()
  data Options Cairo = CairoOptions
          { _cairoSizeSpec   :: SizeSpec V2 Int -- ^ The requested size of the output
          }
    deriving (Show)

  backendInfo = const cairoInfo
  renderDiaT opts dia = (sz, t2, r) where
    (sz, t2, dia') = adjustSize2D (opts^.sizeSpec) (default2DAttrs dia # reflectY)
    r = toRender t2 dia'

instance BackendBuild Cairo where
  saveDiagram' outPath opts d = do
    let (V2 w h,_,r) = renderDiaT opts d
        V2 w' h' = fmap round (V2 w h)
        f surf = C.renderWith surf r
    case takeExtension outPath of
      ".pdf" -> C.withPDFSurface outPath w h f
      ".svg" -> C.withSVGSurface outPath w h f
      ".ps"  -> C.withPSSurface outPath w h f
      ".png" ->
        C.withImageSurface C.FormatARGB32 w' h' $ \surface -> do
          f surface
          C.surfaceWriteToPNG surface outPath
      ext -> error $ "Unknown extension " <> show ext

  mkOptions sz = def & sizeSpec .~ sz
  sizeSpec     = cairoSizeSpec
  showOptions  = show

instance RenderOutcome Cairo (Diagram V2) where
  type MainOpts Cairo (Diagram V2) = (FilePath, Options Cairo)

  resultParser _ _ = (,) <$> outputParser <*> (CairoOptions <$> sizeParser)
  renderOutcome _ (path, opts) = saveDiagram' path opts

------------------------------------------------------------------------
-- Rendering
------------------------------------------------------------------------

toRender :: T2 Double -> Diagram V2 -> C.Render ()
toRender = foldDiaA renderPrim renderAnnot
  where
    renderPrim t2 attrs prim = case renderPrimitive t2 attrs prim of
      Just r  -> C.save >> r >> C.restore
      Nothing -> error $ "Unknown primitive"

renderPrimitive
  :: T2 Double -> Attributes -> Prim V2 Double -> Maybe (C.Render ())
renderPrimitive t2 attrs = \case
  Path_ path              -> Just $ renderPath t2 attrs path
  Text_ t                 -> Just $ renderText t2 attrs t
  ExternalImage_ w h path -> Just $ renderExternal t2 w h path
  EmbeddedImage_  i       -> Just $ renderEmbedded t2 i
  Prim _                  -> Nothing

renderAnnot :: Annotation V2 Double -> C.Render () -> C.Render ()
renderAnnot a r
  -- -- | Just x <- getAnnot _GroupOpacity a = C.save >> r >> C.paintWithAlpha x >> C.restore
  | Just p <- getAnnot _Clip         a = C.save >> clip (F.toList p) r >> C.restore
  | otherwise                          = r

clip :: [Path V2 Double] -> C.Render () -> C.Render ()
clip clips m = do
  F.for_ clips $ \path -> do
    cairoPath path
    C.clip
  m

instance Hashable (Options Cairo) where
  hashWithSalt s (CairoOptions sz)
    = s   `hashWithSalt`
      sz

instance Default (Options Cairo) where
  def = CairoOptions (dims2D 64 64)

cairoSizeSpec :: Lens' (Options Cairo) (SizeSpec V2 Int)
cairoSizeSpec = lens (\(CairoOptions {_cairoSizeSpec = s}) -> s)
                     (\o s -> o {_cairoSizeSpec = s})

cairoStyle :: Attributes -> C.Render ()
cairoStyle s = sequence_
  [ f _FillRule (C.setFillRule . fromFillRule)
  , f _LineWidth C.setLineWidth
  , f _LineCap (C.setLineCap . fromLineCap)
  , f _LineJoin (C.setLineJoin . fromLineJoin)
  , f _Dashing (\(Dashing ds offs) -> C.setDash ds offs)
  ]
  where
    f :: Typeable a => Getting r a r -> (r -> C.Render ()) -> C.Render ()
    f g r = mapM_ r (getAttr g s)

-- fromFontSlant :: FontSlant -> P.FontStyle
fromFontSlant :: FontSlant -> P.Style
fromFontSlant FontSlantNormal   = P.StyleNormal
fromFontSlant FontSlantItalic   = P.StyleItalic
fromFontSlant FontSlantOblique  = P.StyleOblique

-- GI.Pango.Enums
-- data Style = 
--       StyleNormal
--     -- ^ the font is upright.
--     | StyleOblique
--     -- ^ the font is slanted, but in a roman style.
--     | StyleItalic
--     -- ^ the font is slanted in an italic style.
--     | AnotherStyle Int
--     -- ^ Catch-all for unknown values
    --     deriving (Show, Eq)

fromFontWeight :: FontWeight -> P.Weight
fromFontWeight FontWeightBold   = P.WeightBold
fromFontWeight _                = P.WeightNormal

-- Enum Weight
-- | An enumeration specifying the weight (boldness) of a font. This is a numerical
-- value ranging from 100 to 1000, but there are some predefined values:
-- data Weight = 
--       WeightThin
--     -- ^ the thin weight (= 100; Since: 1.24)
--     | WeightUltralight
--     -- ^ the ultralight weight (= 200)
--     | WeightLight
--     -- ^ the light weight (= 300)
--     | WeightSemilight
--     -- ^ the semilight weight (= 350; Since: 1.36.7)
--     | WeightBook
--     -- ^ the book weight (= 380; Since: 1.24)
--     | WeightNormal
--     -- ^ the default weight (= 400)
--     | WeightMedium
--     -- ^ the normal weight (= 500; Since: 1.24)
--     | WeightSemibold
--     -- ^ the semibold weight (= 600)
--     | WeightBold
--     -- ^ the bold weight (= 700)
--     | WeightUltrabold
--     -- ^ the ultrabold weight (= 800)
--     | WeightHeavy
--     -- ^ the heavy weight (= 900)
--     | WeightUltraheavy
--     -- ^ the ultraheavy weight (= 1000; Since: 1.24)
--     | AnotherWeight Int
--     -- ^ Catch-all for unknown values
--     deriving (Show, Eq)

-- | Multiply the current transformation matrix by the given 2D
--   transformation.
cairoTransf :: T2 Double -> C.Render ()
cairoTransf t = C.transform m
  where m = CM.Matrix a1 a2 b1 b2 c1 c2
        (unr2 -> (a1,a2)) = apply t unitX
        (unr2 -> (b1,b2)) = apply t unitY
        (unr2 -> (c1,c2)) = transl t

fromLineCap :: LineCap -> C.LineCap
fromLineCap LineCapButt   = C.LineCapButt
fromLineCap LineCapRound  = C.LineCapRound
fromLineCap LineCapSquare = C.LineCapSquare

fromLineJoin :: LineJoin -> C.LineJoin
fromLineJoin LineJoinMiter = C.LineJoinMiter
fromLineJoin LineJoinRound = C.LineJoinRound
fromLineJoin LineJoinBevel = C.LineJoinBevel

fromFillRule :: FillRule -> C.FillRule
fromFillRule Winding = C.FillRuleWinding
fromFillRule EvenOdd = C.FillRuleEvenOdd

renderPath
  :: T2 Double
  -> Attributes
  -> Path V2 Double
  -> C.Render ()
renderPath t attr p = do
  pathT t p
  let fill = getAttr _FillTexture attr
      line = getAttr _LineTexture attr
      o    = fromMaybe 1 $ getAttr _Opacity attr
  cairoStyle attr
  when (isJust fill) $ do
    setTexture o fill
    C.fillPreserve
  setTexture o line
  C.stroke

segment :: Segment V2 Double -> C.Render ()
segment = \case
  Linear (V2 x y)                        -> C.relLineTo x y
  Cubic (V2 x1 y1) (V2 x2 y2) (V2 x3 y3) -> C.relCurveTo x1 y1 x2 y2 x3 y3

pathT :: T2 Double -> Path V2 Double -> C.Render ()
pathT t p = C.newPath >> mapMOf_ each (trail . transform t) p

cairoPath :: Path V2 Double -> C.Render ()
cairoPath (Path path) = do
  C.newPath
  F.mapM_ trail path

trail :: Located (Trail V2 Double) -> C.Render ()
trail (Loc (P2 x y) t) = do
  C.moveTo x y
  case t of
    OpenTrail l            -> lineRender l
    ClosedTrail (Loop l c) -> do
      lineRender l
      close (negated $ offset l) c

lineRender :: Line V2 Double -> C.Render ()
lineRender = traverseOf_ segments segment

close :: V2 Double -> ClosingSegment V2 Double -> C.Render ()
close (V2 x3 y3) = \case
  LinearClosing      -> C.closePath
  CubicClosing (V2 x1 y1) (V2 x2 y2) ->
    C.relCurveTo x1 y1 x2 y2 x3 y3 >> C.closePath

-- Add a path to the Cairo context, without stroking or filling it.

addStop :: MonadIO m => C.Pattern -> GradientStop -> m ()
addStop p s = C.patternAddColorStopRGBA p (s^.stopFraction) r g b a
  where
    (r,g,b,a) = colorToSRGBA (s^.stopColor)

cairoSpreadMethod :: SpreadMethod -> C.Extend
cairoSpreadMethod GradPad = C.ExtendPad
cairoSpreadMethod GradReflect = C.ExtendReflect
cairoSpreadMethod GradRepeat = C.ExtendRepeat

-- XXX should handle opacity in a more straightforward way, using
-- cairo's built-in support for transparency?  See also
-- https://github.com/diagrams/diagrams-cairo/issues/15 .
setTexture :: Double -> Maybe Texture -> C.Render ()
setTexture o (Just t) = case t of
  SC (SomeColor c) ->
    let (r,g,b,a) = colorToSRGBA c
    in  C.setSourceRGBA r g b (o*a)
  LG g ->
    let m = CM.Matrix a1 a2 b1 b2 c1 c2
        [[a1, a2], [b1, b2], [c1, c2]] = matrixHomRep (inv (g^.gradientTransform))
        (x0, y0) = unp2 (g^.gradientStart)
        (x1, y1) = unp2 (g^.gradientEnd)
    in  C.withLinearPattern x0 y0 x1 y1 $ \pat -> do
          mapM_ (addStop pat) (g^.gradientStops)
          C.patternSetMatrix pat m
          C.patternSetExtend pat (cairoSpreadMethod (g^.gradientSpreadMethod))
          C.setSource pat
  RG g ->
    let m = CM.Matrix a1 a2 b1 b2 c1 c2
        [[a1, a2], [b1, b2], [c1, c2]] = matrixHomRep (inv (g^.gradientTransform))
        (r0, r1) = (g^.gradientRadius0, g^.gradientRadius1)
        (x0', y0') = unp2 (g^.gradientCenter0)
        (x1', y1') = unp2 (g^.gradientCenter1)
        (x0, y0, x1, y1) = (x0' * (r1 - r0) / r1, y0' * (r1 - r0) / r1, x1' ,y1')
    in  C.withRadialPattern x0 y0 r0 x1 y1 r1 $ \pat -> do
          mapM_ (addStop pat) (g^.gradientStops)
          C.patternSetMatrix pat m
          C.patternSetExtend pat (cairoSpreadMethod (g^.gradientSpreadMethod))
          C.setSource pat
setTexture _ _ = return ()

-- Can only do PNG files at the moment...
renderExternal :: T2 Double -> Int -> Int -> FilePath -> C.Render ()
renderExternal tr w h file = do
  if ".png" `isSuffixOf` file
    then do
      C.save
      cairoTransf (tr <> reflectionY)
      pngSurfChk <- liftIO (try $ C.imageSurfaceCreateFromPNG file
                            :: IO (Either IOError C.Surface))
      case pngSurfChk of
        Right pngSurf -> do
          w' <- C.imageSurfaceGetWidth pngSurf
          h' <- C.imageSurfaceGetHeight pngSurf
          let sz = fromIntegral <$> dims2D w h
          cairoTransf $ requiredScaling sz (fromIntegral <$> V2 w' h')
          C.setSourceSurface pngSurf (-fromIntegral w' / 2)
                                     (-fromIntegral h' / 2)
        Left _ ->
          liftIO . putStrLn $
            "Warning: can't read image file <" ++ file ++ ">"
      C.paint
      C.restore
    else do
      liftIO (readImage file) >>= \case
        Left err   -> liftIO (putStrLn err)
        Right dImg -> renderEmbedded tr dImg

-- Copied from Rasterific backend. This function should probably be in JuicyPixels!
toImageRGBA8 :: DynamicImage -> Image PixelRGBA8
toImageRGBA8 (ImageRGBA8 i)  = i
toImageRGBA8 (ImageRGB8 i)   = promoteImage i
toImageRGBA8 (ImageYCbCr8 i) = promoteImage (convertImage i :: Image PixelRGB8)
toImageRGBA8 (ImageY8 i)     = promoteImage i
toImageRGBA8 (ImageYA8 i)    = promoteImage i
toImageRGBA8 (ImageCMYK8 i)  = promoteImage (convertImage i :: Image PixelRGB8)
toImageRGBA8 _               = error "Unsupported Pixel type"

renderEmbedded :: T2 Double -> DynamicImage -> C.Render ()
renderEmbedded tr dImg = do
    let Image w' h' img = toImageRGBA8 dImg
        w = dynamicMap imageWidth dImg
        h = dynamicMap imageHeight dImg
    let (fptr, _len) = SV.unsafeToForeignPtr0 img
        pixelData = castPtr $ unsafeForeignPtrToPtr fptr
        sz = fromIntegral <$> dims2D w h
        stride = C.formatStrideForWidth C.FormatARGB32 w'
    C.save
    cairoTransf (tr <> reflectionY)
    surf <- C.liftIO $ C.createImageSurfaceForData pixelData C.FormatARGB32 w' h' stride
    cairoTransf $ requiredScaling sz (fromIntegral <$> V2 w' h')
    C.setSourceSurface surf (-fromIntegral w' / 2) (-fromIntegral h' / 2)
    C.paint
    C.restore

-- renderNative :: T2 Double -> Int -> Int -> (PixelData, Int, Int) -> C.Render ()
-- renderNative tr w h (img, w', h') = do
--     let sz = fromIntegral <$> dims2D w h
--         stride = C.formatStrideForWidth C.FormatARGB32 w'
--     C.save
--     cairoTransf (tr <> reflectionY)
--     surf <- C.liftIO $ C.createImageSurfaceForData pixelData C.FormatARGB32 w' h' stride
--     cairoTransf $ requiredScaling sz (fromIntegral <$> V2 w' h')
--     C.setSourceSurface surf (-fromIntegral w' / 2) (-fromIntegral h' / 2)
--     C.paint
--     C.restore

{-# INLINE toARGB #-}
-- Actually the name should be toBGRA, since that's the component order used by Cairo.
-- Really, what's happening here is just a swap of the R and B channels.
-- It seems a lot like this is dependent on endianness; perhaps we should handle this...
toARGB :: PixelRGBA8 -> Word32
toARGB px = ga + rotateL rb 16
 where rgba = packPixel px
       rb = rgba .&. 0x00FF00FF
       ga = rgba .&. 0xFF00FF00

-- text ----------------------------------------------------------------

if' :: Monad m => (a -> m ()) -> Maybe a -> m ()
if' = mapM_

-- instance Renderable (Text Double) Cairo where
renderText
  :: T2 Double
  -> Attributes
  -> Text Double
  -> C.Render ()
renderText t sty (Text al str) = do
    -- initializing
    cr <- Connect.getContext
    layout <- P.createLayout cr
    fontD <- P.fontDescriptionNew

    -- non-uniform scale
    let o = fromMaybe 1 (getAttr _Opacity sty)
        tt = t <> scaling (1 / avgScale t)
    setTexture o $ getAttr _FillTexture sty
    let tr = tt <> reflectionY
        ff = fmap T.pack $ getAttr _Font sty
        fs = fromFontSlant <$> getAttr _FontSlant sty
        fw = fromFontWeight <$> getAttr _FontWeight sty
        size' = fmap puToInt $ getAttr _FontSize sty
    cairoTransf tr

    P.layoutSetText layout (T.pack str) (fromIntegral $ length str)

    -- set font, including size
    if' (P.fontDescriptionSetFamily fontD) ff
    if' (P.fontDescriptionSetStyle fontD) fs
    if' (P.fontDescriptionSetWeight fontD) fw
    if' (P.fontDescriptionSetSize fontD) size'
    P.layoutSetFontDescription layout $ Just fontD
    -- geometric translation
    iter <- P.layoutGetIter layout
    ref <- case al of
      BoxAlignedText xt yt -> do
        (_, rectExtent) <- P.layoutGetExtents layout
        w <- fmap intToPu $ P.getRectangleWidth rectExtent
        h <- fmap intToPu $ P.getRectangleHeight rectExtent
        return $ r2 (w * xt, h * (1 - yt))
      BaselineText -> do
        baseline <- fmap intToPu $ P.layoutIterGetBaseline iter
        return $ r2 (0, baseline)
    let t = moveOriginBy ref mempty :: T2 Double
    cairoTransf t
    -- update context
    P.updateLayout cr layout
    P.showLayout cr layout

    C.newPath

    -- finalizing
    P.layoutIterFree iter
    C.liftIO $ GI.disownBoxed iter
    P.fontDescriptionFree fontD
    C.liftIO $ GI.disownBoxed fontD
    GI.objectUnref layout
    C.liftIO $ GI.disownObject layout
    return ()

data PangoOptions = PangoOptions
  { pangoFont   :: Maybe String
  , pangoSlant  :: FontSlant
  , pangoWeight :: FontWeight
  , pangoSize   :: Double
  }

instance Default PangoOptions where
  def = PangoOptions
    { pangoFont      = Nothing
    , pangoSlant     = FontSlantNormal
    , pangoWeight    = FontWeightNormal
    , pangoSize      = 12
    }

pangoScale :: Double
pangoScale = 1024
{-# LINE 167 "Graphics/Rendering/Pango/Structs.hsc" #-}

puToInt :: Double -> Int32
puToInt u = truncate (u * pangoScale)

puToUInt :: Double -> Int32
puToUInt u = let u' = u * pangoScale in if u' < 0 then 0 else truncate u'

intToPu :: Int32 -> Double
intToPu i = fromIntegral i / pangoScale

uIntToPu :: Int32 -> Double
uIntToPu i = fromIntegral i / pangoScale

lay
  :: PangoOptions
  -> String
--   -> C.Render P.PangoLayout
  -> C.Render P.Layout
lay PangoOptions{..} str = do
  cr <- Connect.getContext
  layout <- P.createLayout cr
  P.layoutSetText layout (T.pack str) (fromIntegral $ length str)
  -- set font, including size
  fontD <- P.fontDescriptionNew
  mapM_ (P.fontDescriptionSetFamily fontD) $ fmap T.pack pangoFont
  P.fontDescriptionSetStyle fontD (fromFontSlant pangoSlant)
  P.fontDescriptionSetWeight fontD (fromFontWeight pangoWeight)
  P.fontDescriptionSetSize fontD $ puToInt pangoSize
  P.layoutSetFontDescription layout $ Just fontD

  P.fontDescriptionFree fontD
  C.liftIO $ GI.disownBoxed fontD

  P.updateLayout cr layout
  return layout

queryCairo :: C.Render a -> IO a
queryCairo c = C.withImageSurface C.FormatA1 0 0 (`C.renderWith` c)

-- | Get the bounding box for some pango text
fontBB :: PangoOptions -> String -> IO (BoundingBox V2 Double)
fontBB opts str = do
  layout <- queryCairo $ lay opts str
  -- x0 and y0 correspond to the top left of the text from the cairo origin (top left)
  (rectExtent, _) <- P.layoutGetExtents layout
  -- P.Rectangle x0 y0 w h
  x0 <- fmap intToPu $ P.getRectangleX rectExtent
  y0 <- fmap intToPu $ P.getRectangleY rectExtent
  w <- fmap intToPu $ P.getRectangleWidth rectExtent
  h <- fmap intToPu $ P.getRectangleHeight rectExtent

  -- the distance from the cairo origin to the diagrams baseline text origin
  iter <- P.layoutGetIter layout
  baseline <- fmap intToPu $ P.layoutIterGetBaseline iter
  -- y0 + h gives the distance of the cairo origin to the bottom of the
  -- text, subtracting this from the baseline gives the distance from
  -- the baseline to the bottom of the text
  let y = baseline - (y0 + h)

  P.layoutIterFree iter
  C.liftIO $ GI.disownBoxed iter
  GI.objectUnref layout
  C.liftIO $ GI.disownObject layout

  pure $ fromCorners (P2 x0 y) (P2 (x0 + w) (y + h))

pangoTextIO
  :: PangoOptions
  -> String
  -> IO (Diagram V2)
pangoTextIO opts@(PangoOptions{..}) str = do
  bb <- fontBB opts str
  return $ mkQD (Prim (Text BaselineText str)) (getEnvelope bb) mempty mempty
    # fontSizeL pangoSize
    # fontWeight pangoWeight
    # fontSlant pangoSlant
    # maybe id font pangoFont

-- | Use pango to get the envelope of some text.
--   Font styles do not apply to this text because they would affect the
--   size.
pangoText'
  :: PangoOptions
  -> String
  -> Diagram V2
pangoText' opts str = unsafePerformIO (pangoTextIO opts str)

-- | Use pango to get the envelope of some text with default options.
--   Font styles do not apply to this text because they would affect the
--   size.
pangoText
  :: String
  -> Diagram V2
pangoText = pangoText' def

-- Rendering -----------------------------------------------------------

-- | Rasterise a 'C.Render' to a raw pointer.
rasterPtr :: Int -> Int -> Format -> C.Render () -> IO (Ptr Word8)
rasterPtr w h fmt r = do
  let stride = formatStrideForWidth fmt w
  b <- callocArray (stride * h)
  withImageSurfaceForData b fmt w h stride (`renderWith` r)
  pure (castPtr b)

-- | Rasterise a 'C.Render' to a JuicyPixels image.
rasterImage :: Int -> Int -> C.Render () -> IO (Image PixelRGBA8)
rasterImage w h render = do
  ptr  <- rasterPtr w h FormatARGB32 render
  fptr <- newForeignPtr finalizerFree ptr
  let vec = SV.unsafeFromForeignPtr0 fptr (w*h*4)
  -- cairo uses bgr
  let fromBGR (PixelRGBA8 b g r a) = PixelRGBA8 r g b a
  pure $ pixelMap fromBGR (Image w h vec)

-- | Rasterise a 'C.Render' to a JuicyPixels image.
rasterDia :: SizeSpec V2 Int -> Diagram V2 -> IO (Image PixelRGBA8)
rasterDia sz = rasterDia' (def & sizeSpec .~ sz)

-- | Rasterise a 'C.Render' to a JuicyPixels image.
rasterDia' :: Options Cairo -> Diagram V2 -> IO (Image PixelRGBA8)
rasterDia' opts d = do
  let (V2 w h, _, r) = renderDiaT opts d
  rasterImage (ceiling w) (ceiling h) r
