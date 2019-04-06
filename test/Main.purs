-- test/Main.purs
-- AndrewJ 2019-03-31

module Test.Main where

import Prelude

import Data.Tuple (Tuple(..))
import Effect (Effect)
import Jaipur (CardCount, Resource(..), count, reset, scoreAllTokens, scoreTokens, sumSubset)
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
      let t0 = (Tuple Spice 0) :: CardCount
      let t1 = (Tuple Diamond 2) :: CardCount
      let t2 = (Tuple Leather 9) :: CardCount
      assert "sumSubset" $ sumSubset [3, 2, 1] 1 == 3 
      assert "sumSubset" $ sumSubset [3, 2, 1] 3 == 6 
      assert "sumSubset" $ sumSubset [3, 2, 1] 4 == 6 
      assert "scoreTokens" $ scoreTokens t0 == 0
      assert "scoreTokens" $ scoreTokens t1 == 14
      assert "scoreTokens" $ scoreTokens t2 == 15
      assert "total tokens" $ scoreAllTokens s.tokens == 130
 
{-     test "Deal card" do
      let s = reset
      let s' = dealToMarket s 1
      assert "deal to market" $ count s'.deck == 55 -}