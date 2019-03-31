-- test/Main.purs
-- AndrewJ 2019-03-31

module Test.Main where

import Prelude

import Data.Tuple (Tuple(..))
import Effect (Effect)
import Jaipur (Cards, Resource(..), count, reset, scoreTokens, sumSubset)
import Test.Unit (suite, test)
import Test.Unit.Assert (assert)
import Test.Unit.Main (runTest)

main :: Effect Unit
main = runTest do
  suite "Unit tests" do

    test "Sanity check" do
      assert "2+2=4" $ (2 + 2) == 4

    test "Number of cards" do
      let s = reset
      assert "Cards" $ count s.deck == 55
  
    test "Score tokens" do
      let s = reset
      let t1 = (Tuple Diamond 2) :: Cards
      let t2 = (Tuple Leather 9) :: Cards
      assert "sumSubset" $ sumSubset [3, 2, 1] 1 == 3 
      assert "sumSubset" $ sumSubset [3, 2, 1] 3 == 6 
      assert "sumSubset" $ sumSubset [3, 2, 1] 4 == 6 
      assert "scoreTokens" $ scoreTokens s.tokens t1 == 14
      assert "scoreTokens" $ scoreTokens s.tokens t2 == 15
 