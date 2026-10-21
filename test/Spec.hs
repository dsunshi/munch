module Main (main) where

import qualified Turtle as T

import Test.Hspec
import Linear.V3

testEmpty :: Spec
testEmpty = describe "Turtle.empty" $ do
  it "An empty turtle sits at the origin (0, 0, 0) and faces y+" $ do
    let t = T.empty
    ((V3 0.0 0.0 0.0) :: V3 Double) `shouldBe` t.position

main :: IO ()
main = hspec $ do 
        testEmpty
