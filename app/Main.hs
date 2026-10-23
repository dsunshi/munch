{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE TypeFamilies              #-}

import Diagrams.Prelude
import Diagrams.Backend.SVG.CmdLine

myCircle :: Diagram B
myCircle = circle 2 # lc red

str :: Diagram B
str = text "hello" # lc red

example :: Diagram B
example = fromVertices vertices # strokeLine # lc blue # center # pad 1.1
  where
        vertices = map p2 $ [(0, 0), (0, 200), (0, 0), (200, 0)]

-- Verwendet eine Liste von (Punkt, Diagramm)
myDiagram2 :: Diagram B
myDiagram2 = position [ (p2 (0, 0), example)
                      , (p2 (-200/2, -200/2), myCircle)
                      , (p2 (-200/2, -200/2), text' 30 "origin" # italic)
                      ]

main = mainWith myDiagram2
-- main = mainWith (example <> myCircle)
-- main = mainWith (example <> (text "hello" # moveTo (p2 (0, 200))))
-- text' s t = text t # fontSize (local s) <> strutY (s * 1.3)
text' s t = baselineText t # fontSize (local s) <> strutY (s * 1.3)
-- example :: Diagram B
-- example = center $
--       text' 10 "Hello" # italic
--   === text' 5 "there"  # bold # font "freeserif"
--   === text' 3 "world"  # fc green
