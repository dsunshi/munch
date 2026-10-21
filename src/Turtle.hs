{- |
Copyright: (c) 2026 D. Sunshine
SPDX-License-Identifier: MIT
Maintainer: D. Sunshine <david@sunshines.org>

See README for more info
-}

module Turtle (TurtleData, position, empty, up, down, forward, backward, right, left, rotate) where

import Linear.V2
import Linear.V3
import Linear.Vector
import Linear.Metric
import Control.Lens

data TurtleData = TurtleData {
  position :: V3 Double,
  heading  :: V2 Double
  } deriving stock (Show)

-- Turtle at the origin heading y+ direction
empty :: TurtleData
empty = TurtleData (V3 0.0 0.0 0.0) (normalize $ V2 1.0 0.0)
{-# WARNING in "x-unsafe-internals" empty "This is exported only for testing" #-}

up :: Double -> TurtleData -> TurtleData
up d t = t { position = t.position & _z %~ (+ d) }

down :: Double -> TurtleData -> TurtleData
down a = up (-1.0 * a)

forward :: Double -> TurtleData -> TurtleData
forward d t = t { position = t.position & _xy %~ (+ d *^ t.heading) }

backward :: Double -> TurtleData -> TurtleData
backward a = forward (-1.0 * a)

right :: Double -> TurtleData -> TurtleData
right a t = t { heading = rotate (a * pi / 180.0) t.heading } -- deg -> rad

left :: Double -> TurtleData -> TurtleData
left a = right (-1.0 * a)

rotate :: Floating a => a -> V2 a -> V2 a
rotate a v = cos a *^ v + sin a *^ (perp v) 
{-# WARNING in "x-unsafe-internals" rotate "This is exported only for testing" #-}
