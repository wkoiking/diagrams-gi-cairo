module Main where

-- diagrams
import Diagrams.Prelude
import Diagrams.TwoD.Text
-- diagrams-gi-cairo
import Diagrams.Backend.Cairo (Cairo(..))

main :: IO ()
main = saveDiagram Cairo "Test.png" (mkWidth 6000) $ center $ lw thick $ bg lightgray $ mconcat
    [ circle 2
    , text "abcde"
    ]
