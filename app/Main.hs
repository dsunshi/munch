{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE TypeFamilies              #-}

import Diagrams.Prelude
import Diagrams.Backend.SVG.CmdLine

myCircle :: Diagram B
myCircle = circle 1

example :: Diagram B
example = fromVertices vertices # strokeLine # lc red # center # pad 1.1
  where
        vertices = map p2 $ [(x,y) | x <- [0,0.2 .. 2], y <- [0,1]]

main = mainWith example
