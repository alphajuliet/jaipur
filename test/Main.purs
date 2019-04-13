-- test/Main.purs
-- AndrewJ 2019-03-31

module Test.Main where

import Prelude

import Data.Lens (view)
import Data.Lens.At (at)
import Data.Maybe (Maybe(..))
import Data.Tuple (Tuple(..))
import Effect (Effect)
import Jaipur (_hand, _herd, count, dealCard, initialState, scoreAllTokens, scoreTokens, sumSubset, takeCamels, takeCard)
import Model (CardCount, PlayerId(..), Resource(..))
import Test.Unit (suite, test)
import Test.Unit.Assert (assert)
import Test.Unit.Main (runTest)

main :: Effect Unit
main = runTest do
  suite "Unit tests" do

    test "Sanity check" do
      assert "2+2=4" $ (2 + 2) == 4

    test "Number of cards" do
      let s = initialState
      assert "Cards" $ count s.deck == 55
  
    test "Score tokens" do
      let s = initialState
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
 
    test "Deal cards" do
      let s = initialState
      let s' = dealCard Diamond s 
      assert "dealCard" $ count s'.deck == 54
      assert "dealCard" $ count s'.market == 1

    test "Take cards" do
      let s = initialState
      let s0 = dealCard Diamond $ dealCard Diamond s
      let s1 = takeCard PlayerA Diamond s0
      assert "takeCard" $ (count <$> view (_hand <<< at PlayerA) s1) == Just 1

    test "Take camels" do
      let s = initialState
      let s0 = dealCard Camel $ dealCard Camel s
      let s1 = takeCamels PlayerA s0
      assert "takeCamels" $ view (_herd <<< at PlayerA) s1 == Just 2