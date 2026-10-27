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

data TurtleData n = TurtleData {
  position :: V3 n,
  heading  :: V2 n
  } deriving stock (Show)

-- Turtle at the origin heading y+ direction
empty :: TurtleData Double
empty = TurtleData (V3 0.0 0.0 0.0) (normalize $ V2 1.0 0.0)
{-# WARNING in "x-unsafe-internals" empty "This is exported only for testing" #-}

up :: Num a => a -> TurtleData a -> TurtleData a
up d t = t { position = t.position & _z %~ (+ d) }

down :: Num a => a -> TurtleData a -> TurtleData a
down = up . negate

forward :: Num a => a -> TurtleData a -> TurtleData a
forward d t = t { position = t.position & _xy %~ (+ d *^ t.heading) }

backward :: Num a => a -> TurtleData a -> TurtleData a
backward = forward . negate

right :: Floating a => a -> TurtleData a -> TurtleData a
right a t = t { heading = rotate (a * pi / 180.0) t.heading } -- deg -> rad

left :: Floating a => a -> TurtleData a -> TurtleData a
left = right . negate

rotate :: Floating a => a -> V2 a -> V2 a
rotate a v = cos a *^ v + sin a *^ (perp v) 
{-# WARNING in "x-unsafe-internals" rotate "This is exported only for testing" #-}
