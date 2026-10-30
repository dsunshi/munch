module Main (main) where

import qualified TurtleState as T
import Turtle

import Test.Hspec
import Linear.V3

testTurtle :: Turtle Double ()
testTurtle = do
  forward 100
  right 90
  forward 50

testEmpty :: Spec
testEmpty = describe "Turtle.empty" $ do
  it "An empty turtle sits at the origin (0, 0, 0) and faces y+" $ do
    let t = T.empty
    ((V3 0.0 0.0 0.0) :: V3 Double) `shouldBe` t.position

testUp :: Spec
testUp = describe "Turtle.up" $ do
  it "Test that the turtle can move up in the z dirction" $ do
    let t = T.up 5.0 $ T.empty
    ((V3 0.0 0.0 5.0) :: V3 Double) `shouldBe` t.position

testDown :: Spec
testDown = describe "Turtle.down" $ do
  it "Test that the turtle can move down in the z dirction" $ do
    let t = T.down 4.0 $ T.up 10.0 $ T.empty
    ((V3 0.0 0.0 6.0) :: V3 Double) `shouldBe` t.position

testForward :: Spec
testForward = describe "Turtle.forward" $ do
  it "Test that the turtle can move forward in the x dirction" $ do
    let t = T.forward 10.0 $ T.empty
    ((V3 10.0 0.0 0.0) :: V3 Double) `shouldBe` t.position

testBackward :: Spec
testBackward = describe "Turtle.backward" $ do
  it "Test that the turtle can move backward in the x dirction" $ do
    let t = T.backward 2 $ T.forward 4.0 $ T.empty
    ((V3 2.0 0.0 0.0) :: V3 Double) `shouldBe` t.position

testMoveZUp :: Spec
testMoveZUp = describe "Turtle.up" $ do
  it "Test that the turtle can move while the pen is up" $ do
    let t = T.down 1.0 $ T.backward 2 $ T.forward 4.0 $ T.up 5.0 T.empty
    ((V3 2.0 0.0 4.0) :: V3 Double) `shouldBe` t.position

testRight :: Spec
testRight = describe "Turtle.right" $ do
  it "Test that the turtle can turn right" $ do
    let t = T.forward 5 $ T.right 53.17 $ T.empty
    t.position `shouldSatisfy` (`closeTo` (V3 3.0 4.0 0.0))

main :: IO ()
main = hspec $ do 
        testEmpty
        testUp
        testDown
        testForward
        testBackward
        testMoveZUp
        testRight

closeTo :: (Fractional a, Ord a) => V3 a -> V3 a -> Bool
closeTo x y = foldr (&&) True $ fmap ((< epsilon) . abs) (x - y)
  where
    epsilon = 0.005

