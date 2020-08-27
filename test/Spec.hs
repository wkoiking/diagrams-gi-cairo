module Main where

-- diagrams
import Diagrams.Prelude
-- diagrams-gi-cairo
import Diagrams.Backend.Cairo (Cairo(..))

main :: IO ()
main = saveDiagram Cairo "Test.png" (mkWidth 6000) $ center $ lw thick $ bg lightgray $ circle 2
