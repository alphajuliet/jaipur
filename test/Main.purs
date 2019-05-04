-- test/Main.purs
-- AndrewJ 2019-03-31
-- [i:258115]

module Test.Main where

import Data.Lens (setJust, traversed, view)
import Data.Lens.At (at)
import Data.Map (fromFoldable) as M
import Data.Maybe (Maybe(..))
import Data.Tuple (Tuple(..))
import Effect (Effect)
import Jaipur (count, countHandResource, dealCard, exchangeCards, minimumSell, scoreAllTokens, scoreTokens, sellCards, sumSubset, takeCamels, takeCard)
import Model (CardCount, PlayerId(..), Resource(..), _hand, _herd, _points, _tokens, initialState)
import Prelude (Unit, discard, ($), (+), (<$>), (<<<), (==))
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

    test "Minimum sell" do
      assert "Diamond" $ minimumSell Diamond == 2
      assert "Leather" $ minimumSell Leather == 1
  
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
      assert "deck reduced by 1" $ count s'.deck == 54
      assert "market increased by 1" $ count s'.market == 1

    test "Take cards" do
      let s = initialState
      let s0 = (dealCard Diamond <<< dealCard Diamond) s
      let s1 = takeCard PlayerA Diamond s0
      let s2 = takeCard PlayerA Diamond s1
      assert "take first card" $ (count <$> view (_hand <<< at PlayerA) s1) == Just 1
      assert "take second card" $ (count <$> view (_hand <<< at PlayerA) s2) == Just 2

    test "Take camels" do
      let s = initialState
      let s0 = (dealCard Camel <<< dealCard Camel) s
      let s1 = takeCamels PlayerA s0
      assert "take all the camels" $ view (_herd <<< at PlayerA) s1 == Just 2

    test "Sell cards" do
      let s = initialState
      let s1 = (dealCard Leather 
            <<< dealCard Diamond 
            <<< takeCard PlayerA Leather 
            <<< takeCard PlayerA Diamond) s
      let s2 = sellCards PlayerA Leather s1
      let s3 = sellCards PlayerA Diamond s2
      
      assert "sell Leather" $ countHandResource PlayerA Leather s2 == 0
      assert "Leather tokens reduced" $ view (_tokens <<< at Leather) s2 == Just 8
      assert "gained points for Leather" $ view (_points <<< at PlayerA) s2 == Just 4
      assert "no change to Diamond" $ countHandResource PlayerA Diamond s2 == 1

      assert "can't sell Diamond" $ countHandResource PlayerA Diamond s3 == 1
      assert "no points for Diamond" $ view (_points <<< at PlayerA) s3 == Just 4

    test "Exchange cards" do
      let s = initialState
      let s1 = setJust (_hand <<< at PlayerA <<< traversed <<< at Leather) 2 s
      let s2 = (dealCard Diamond <<< dealCard Diamond) s1
      let s3 = exchangeCards PlayerA (M.fromFoldable [Tuple Leather 2]) (M.fromFoldable [Tuple Diamond 2]) s2
      assert "hold 2 Diamond" $ countHandResource PlayerA Diamond s3 == 2
      assert "hold 0 Leather" $ countHandResource PlayerA Leather s3 == 0
      let s4 = exchangeCards PlayerA (M.fromFoldable [Tuple Leather 3]) (M.fromFoldable [Tuple Diamond 1]) s2
      assert "unchanged state for unequal card numbers" $ s4 == s2

-- The End