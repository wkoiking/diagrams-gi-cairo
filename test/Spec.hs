module Main where

-- gi-object
import qualified GI.GObject.Objects.Object as GI (objectUnref)
import qualified Data.GI.Base.ManagedPtr as GI (disownObject)
-- gi-cairo-connector
import qualified GI.Cairo.Render.Connector as Connect
-- gi-pangocairo
import qualified GI.PangoCairo.Functions as P (createLayout)
-- gi-cairo-render
import qualified GI.Cairo.Render as C
import GI.Cairo.Render (renderWith, createImageSurface, Format(..))
-- base
import Control.Monad (forever, replicateM_)
import Control.Concurrent (threadDelay)

main :: IO ()
main = do
    cairoSurface <- createImageSurface FormatRGB24 500 500
    forever $ do
        putStrLn "Rendering ..."
        renderWith cairoSurface $ replicateM_ 500 $ renderText
        threadDelay 1000000

renderText
  :: C.Render ()
renderText = do
    cr <- Connect.getContext
    layout <- P.createLayout cr
    GI.objectUnref layout
    C.liftIO $ GI.disownObject layout
    return ()

-- saveDiagram Cairo "Test.png" (mkWidth 500) $ center $ lw thick $ bg lightgray $ mconcat
--     [ circle 2
--     , rotate (45 @@ deg) $ font "Arial" $ fontWeight FontWeightBold $ fc blue $ text "ABC"
--     ]
