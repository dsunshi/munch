{- |
Copyright: (c) 2026 D. Sunshine
SPDX-License-Identifier: MIT
Maintainer: D. Sunshine <david@sunshines.org>

See README for more info
-}

module Turtle where

import Linear.V2
import Linear.V3
import Linear.Vector
import Linear.Metric
import Control.Lens.Combinators
import Control.Lens.Getter
import Control.Lens

data TurtleData = TurtleData {
  position :: V3 Double,
  heading  :: V2 Double
  } deriving stock (Show)

-- Turtle at the orgin heading y+ direction
empty :: TurtleData
empty = TurtleData (V3 0.0 0.0 0.0) (normalize $ V2 1.0 0.0)

rotate :: Floating a => a -> V2 a -> V2 a
rotate a v = cos a *^ v + sin a *^ (perp v) 
{-# WARNING in "x-unsafe-internals" rotate "This is exported only for testing" #-}

up :: Double -> TurtleData -> TurtleData
up d t = t { position = t.position & _z %~ (+ d) }

down :: Double -> TurtleData -> TurtleData
down a t = up (-1.0 * a) t

forward :: Double -> TurtleData -> TurtleData
forward d t = t { position = fromPlane z $ p + d *^ t.heading }
  where
    (z, p) = slice t.position

backward :: Double -> TurtleData -> TurtleData
backward a t = forward (-1.0 * a) t

right :: Double -> TurtleData -> TurtleData
right a t = t { heading = rotate (a * pi / 180.0) t.heading } -- deg -> rad

left :: Double -> TurtleData -> TurtleData
left a t = right (-1.0 * a) t

slice :: V3 a -> (a, V2 a)
slice (V3 x y z) = (z, V2 x y)
{-# WARNING in "x-unsafe-internals" slice "This is exported only for testing" #-}

fromPlane :: a -> V2 a -> V3 a
fromPlane z (V2 x y) = V3 x y z
{-# WARNING in "x-unsafe-internals" fromPlane "This is exported only for testing" #-}
