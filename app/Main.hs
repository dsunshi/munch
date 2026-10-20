module Main (main) where

import Munch (projectName)

import Linear.V2
import Linear.Vector
import Linear.Metric

data TurtleData = TurtleData {
  position :: V2 Double,
  heading  :: V2 Double
  } deriving (Show)

-- Turtle at the orgin heading y+ direction
empty :: TurtleData
empty = TurtleData (V2 0.0 0.0) (normalize $ V2 1.0 0.0)

rotate :: Floating a => a -> V2 a -> V2 a
rotate a v = cos a *^ v + sin a *^ (perp v) 

forward :: Double -> TurtleData -> TurtleData
forward d t = t { position = t.position + d *^ t.heading }

right :: Double -> TurtleData -> TurtleData
right a t = t { heading = rotate (a * pi / 180.0) t.heading } -- deg -> rad

main :: IO ()
main = print $ forward 10 $ right 90 $ forward 5 $ empty
