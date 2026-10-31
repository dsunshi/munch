{- |
Copyright: (c) 2026 D. Sunshine
SPDX-License-Identifier: MIT
Maintainer: D. Sunshine <david@sunshines.org>

See README for more info
-}

module TurtleState (TurtleData, path, position, initialT, up, down, forward, backward, right, left, rotate) where

import Linear.V2
import Linear.V3
import Linear.Vector
import Linear.Metric
import Control.Lens
import Linear.Epsilon


-- TODO:
-- Push ([): Saves the turtle's current state (coordinates x, y, heading angle, pen settings, etc.) onto a memory stack.
-- Pop (]): Removes and restores the most recently saved state from the stack, moving the turtle back to that exact point and direction.

data TurtleData n = TurtleData {
  position :: V3 n,
  heading  :: V2 n,
  path     :: [V3 n]
  } deriving stock (Show)

origin :: (Linear.Epsilon.Epsilon a, Floating a) => V3 a
origin = V3 0.0 0.0 0.0

-- Turtle at the origin heading y+ direction
initialT :: (Linear.Epsilon.Epsilon a, Floating a) => TurtleData a
initialT = TurtleData origin (normalize $ V2 0.0 1.0) [origin]

up :: Num a => a -> TurtleData a -> TurtleData a
up = move up'

up' :: Num a => a -> TurtleData a -> TurtleData a
up' d t = t { position = t.position & _z %~ (+ d) }
{-# WARNING in "x-unsafe-internals" up' "This is exported only for testing" #-}

down :: Num a => a -> TurtleData a -> TurtleData a
down = up . negate

forward :: Num a => a -> TurtleData a -> TurtleData a
forward = move forward'

forward' :: Num a => a -> TurtleData a -> TurtleData a
forward' d t = t { position = t.position & _xy %~ (+ d *^ t.heading) }
{-# WARNING in "x-unsafe-internals" forward' "This is exported only for testing" #-}

backward :: Num a => a -> TurtleData a -> TurtleData a
backward = forward . negate

right :: Floating a => a -> TurtleData a -> TurtleData a
right a t = t { heading = rotate (a * pi / 180.0) t.heading } -- deg -> rad

left :: Floating a => a -> TurtleData a -> TurtleData a
left = right . negate

rotate :: Floating a => a -> V2 a -> V2 a
rotate a v = cos a *^ v + sin a *^ (perp v) 
{-# WARNING in "x-unsafe-internals" rotate "This is exported only for testing" #-}

move :: Num a => (a -> TurtleData a -> TurtleData a) -> a -> TurtleData a -> TurtleData a
move f d t = TurtleData t'.position t'.heading (t'.path <> [t'.position - t.position])
  where
    t' = f d t
{-# WARNING in "x-unsafe-internals" move "This is exported only for testing" #-}
