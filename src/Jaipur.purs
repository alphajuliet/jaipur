-- src/Jaipur.purs
-- AndrewJ 2019-03-31

module Jaipur where
  
import Prelude

import Data.Array (foldl, index, length, slice)
import Data.Foldable (sum)
import Data.Lens (Lens', lens, view)
import Data.Lens.At (at)
import Data.Map (empty, fromFoldable, toUnfoldable, insertWith, lookup, values) as M
import Data.Maybe (Maybe, fromJust)
import Data.Tuple (Tuple(..), fst, snd)
import Effect (Effect)
import Effect.Random (randomInt)
import Model (Action(..), CardCount, CardSet, Resource(..), State, StepOutput, PlayerId)

-- ----------------
-- Return a random element from an array
randomElement :: ∀ a. Array a -> Effect (Maybe a)
randomElement arr = do
  n <- randomInt 0 $ length arr - 1
  pure $ index arr n

-- Calculate the sum of the first n elements
-- sumSubset [7, 7, 5, 5, 5] 3 => 19
-- sumSubset [7, 7, 5, 5, 5] 6 => 29
sumSubset :: Array Int -> Int -> Int
sumSubset arr n = foldl add 0 s
  where s = (slice 0 n arr)

-- ----------------
-- Count the number of cards in a pile
count :: CardSet -> Int
count ts = sum $ (snd <$> (M.toUnfoldable ts)) :: Array Int

-- Score a pile of 0 or more of a single type of token
-- scoreTokens (Tuple Diamond 2) => 14
scoreTokens :: CardCount -> Int
scoreTokens tokens = p
  where 
  n = snd tokens
  p = case (fst tokens) of
    Diamond -> sumSubset [7, 7, 5, 5, 5] n
    Gold -> sumSubset [6, 6, 5, 5, 5] n
    Silver -> sumSubset [5, 5, 5, 5, 5] n
    Cloth -> sumSubset [5, 3, 3, 2, 2, 1, 1] n
    Spice -> sumSubset [5, 3, 3, 2, 2, 1, 1] n
    Leather -> sumSubset [4, 3, 2, 1, 1, 1, 1, 1, 1] n
    _ -> 0

-- Score all tokens in a pile
scoreAllTokens :: CardSet -> Int
scoreAllTokens allTokens = foldl (+) 0 scores 
  where scores = (scoreTokens <$> (M.toUnfoldable allTokens)) :: Array Int

-- ----------------
-- reset :: State
initialState :: State
initialState = 
  { deck: M.fromFoldable [ 
      (Tuple Diamond 6), (Tuple Gold 6), (Tuple Silver 6), (Tuple Cloth 8), 
      (Tuple Spice 8), (Tuple Leather 10), (Tuple Camel 11) ]
  , market: M.empty
  , hand: []
  , herd: [ (Tuple Camel 0), (Tuple Camel 0) ]
  , points: [0, 0]
  , tokens: M.fromFoldable [ 
      (Tuple Diamond 5), (Tuple Gold 5), (Tuple Silver 5), (Tuple Cloth 7), 
      (Tuple Spice 7), (Tuple Leather 9)]
  }

-- observation_space :: State -> Observation
step :: State -> Action -> StepOutput 
step st action = { observation: st', reward: 0.0, isDone: false, info: "" }
  where 
  st' = case action of 
    Take _ _ -> st
    Exchange _ _ -> st
    Sell _ _ -> st

-- ----------------
-- Actions

-- exchangeCards :: PlayerId -> CardSet -> CardSet -> State -> State
-- sellCards :: PlayerId -> Resource -> State -> State

-- Add a resource card to a CardSet
-- addCard :: CardCount -> CardSet -> CardSet
-- addCard resources deck = 

{- takeCards :: PlayerId -> Resource -> State -> State
takeCards id rsrc st = st'
  where
  n = fromJust $ lookup rsrc $ view _market st
  market = insertWith (-) rsrc n st.deck
  hand = updateAt id (insertWith (+) rsrc n) st.hand
  st' = { market: market, hand: hand }
 -}  
-- ----------------
-- Lenses into the state

_deck :: Lens' State CardSet
_deck = lens _.deck $ _ { deck = _ }

_market :: Lens' State CardSet
_market = lens _.market $ _ { market = _ }

_hand :: Lens' State (Array CardSet)
_hand = lens _.hand $ _ { hand = _ }

_herd :: Lens' State (Array CardCount)
_herd = lens _.herd $ _ { herd = _ }

{- _rsrc :: Resource -> Lens' State (Maybe Int)
_rsrc r = at r
 -}
-- _hand :: PlayerId -> Lens' State CardSet
-- _hand id = lens (at 1 <<< _.herd) $ _ { hand = _ }


-- The End