module Main where

-- diagrams
import Diagrams.Prelude
import Diagrams.TwoD.Text
-- diagrams-gi-cairo
import Diagrams.Backend.Cairo (Cairo(..))

main :: IO ()
main = saveDiagram Cairo "Test.png" (mkWidth 500) $ center $ lw thick $ bg lightgray $ mconcat
    [ circle 2
    , rotate (45 @@ deg) $ font "Arial" $ fontWeight FontWeightBold $ fc blue $ text "ABC"
    ]
