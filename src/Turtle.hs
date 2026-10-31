
{-# LANGUAGE MultiParamTypeClasses #-}

module Turtle where

import qualified Control.Monad.State as ST
import qualified TurtleState as T

import Data.Functor.Identity
import Linear.Epsilon
import Linear.V3

type TurtleT n = ST.StateT (T.TurtleData n)
type Turtle n = TurtleT n Identity

-- | A more general way to run the turtle. Returns a computation in the
-- underlying monad @m@ yielding the final turtle state.
-- runTurtleT :: (Floating n, Monad m) => TurtleT n m a -> m (T.TurtleData n)
-- empty :: (Linear.Epsilon.Epsilon a, Floating a) => TurtleData a
runTurtleT :: (Linear.Epsilon.Epsilon n, Floating n, Monad m) => TurtleT n m a -> m (T.TurtleData n)
runTurtleT t = ST.execStateT t T.initialT

-- -- | Run the turtle, yielding the final turtle state.
-- runTurtle :: Floating n => Turtle n a -> T.TurtleData n
-- runTurtle :: Monad m => TurtleT Double m a -> m (T.TurtleData Double)
runTurtle :: (Linear.Epsilon.Epsilon n, Floating n)  => Turtle n a -> T.TurtleData n
runTurtle = runIdentity . runTurtleT

-- -- Motion commands

up :: (Num n, Monad m) => n -> TurtleT n m ()
up x = ST.modify $ T.up x

down :: (Num n, Monad m) => n -> TurtleT n m ()
down x = ST.modify $ T.down x

-- | Move the turtle forward, along the current heading.
forward :: (Num n, Monad m) => n -> TurtleT n m ()
forward x = ST.modify $ T.forward x

-- -- | Move the turtle backward, directly away from the current heading.
backward :: (Num n, Monad m) => n -> TurtleT n m ()
backward x = ST.modify $ T.backward x

-- -- | Modify the current heading to the left by the specified angle in degrees.
left :: (Floating n, Monad m) => n -> TurtleT n m ()
left d = ST.modify $ T.left d

-- -- | Modify the current heading to the right by the specified angle in degrees.
right :: (Floating n, Monad m) => n -> TurtleT n m ()
right d = ST.modify $ T.right d
