module Main where

import GI.Cairo.Render

-- -- diagrams
-- import Diagrams.Prelude
-- import Diagrams.TwoD.Text
-- -- diagrams-gi-cairo
-- import Diagrams.Backend.Cairo (Cairo(..))

drawRectangle :: Render ()
drawRectangle = do
    setSourceRGB 1.0 0.0 0.0    -- set the source layer to red color
    setLineWidth 10             -- set virtual pen width
    rectangle 50 50 100 100     -- draw a rectangle at (50,50)
    stroke                      -- transfer to mask

main :: IO ()
main = do
    withPDFSurface "sample.pdf" 100 100 $ \ surface -> do
        renderWith surface drawRectangle

-- main :: IO ()
-- main = saveDiagram Cairo "Test.png" (mkWidth 300) $ center $ lw thick $ bg lightgray $ mconcat
--     [ rect 20 5
--     , text "あいうえお↓↑"
--     ]
