-- src/Jaipur.purs
-- AndrewJ 2019-03-31
-- [i:258115]

module Jaipur where
  
import Prelude

import Data.Array (foldl, index, length, slice)
import Data.Foldable (sum)
import Data.Lens (Lens', over, preview, setJust, view)
import Data.Lens.At (at)
import Data.Map (empty, toUnfoldable, unionWith) as M
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Tuple (Tuple(..), fst, snd)
import Effect (Effect)
import Effect.Random (randomInt)
import Model (Action(..), CardCount, CardSet, PlayerId, Resource(..), State, StepOutput, CardLens, CardTraversal, _deck, _hand, _herd, _market, _points, _tokens, _handResource)

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

-- Minimum sale of each resource
minimumSell :: Resource -> Int
minimumSell rsrc = case rsrc of 
  Camel -> 99
  Diamond -> 2
  Gold -> 2
  Silver -> 2
  _ -> 1

-- Score all tokens in a pile
scoreAllTokens :: CardSet -> Int
scoreAllTokens allTokens = foldl (+) 0 scores 
  where scores = (scoreTokens <$> (M.toUnfoldable allTokens)) :: Array Int

-- ----------------
-- observation_space :: State -> Observation
step :: State -> Action -> StepOutput 
step st action = { observation: st', reward: 0.0, isDone: false, info: "" }
  where 
  st' = case action of 
    Take _ _ -> st
    Exchange _ _ _ -> st
    Sell _ _ -> st

-- ----------------
-- Utility functions 

-- Count held resources
countHandResource :: PlayerId -> Resource -> State -> Int
countHandResource id rsrc st = 
  fromMaybe 0 $ join $ preview (_handResource id rsrc) st

countResource :: Resource -> CardSet -> Int
countResource rsrc c = fromMaybe 0 $ view (at rsrc) c

-- Add or take n resources from a pile of cards
-- addTo :: CardLens -> Int -> State -> State
addTo :: CardTraversal -> Int -> State -> State
addTo _ 0 st = st
addTo _lens n st = st'
  where
    current = join $ preview _lens st
    st' = case current of
      Just _ -> over _lens (map (_+n)) st
      Nothing -> setJust _lens n st

takeFrom :: CardLens -> Int -> State -> State 
takeFrom _ 0 st = st
takeFrom _lens n st = over _lens (map (_-n)) st

-- Move n cards from src to dest
-- Check first that cards are available to take
moveCards :: CardLens -> CardLens -> Int -> State -> State
moveCards _ _ 0 st = st
moveCards _src _dest n st = st'
  where
    current = fromMaybe 0 $ join $ preview _src st
    n' | n <= current = n
       | otherwise = 0
    st' = (takeFrom _src n' <<< addTo _dest n') st 

-- ----------------
-- Actions

-- Deal a resource from the deck to the market
dealCard :: Resource -> State -> State
dealCard rsrc = moveCards (_deck <<< at rsrc) (_market <<< at rsrc) 1

-- A player takes a resource from the market 
takeCard :: PlayerId -> Resource -> State -> State
takeCard id rsrc st = st'
  where 
    _src = (_market <<< at rsrc) :: Lens' State (Maybe Int)
    _dest = _handResource id rsrc

    nMarket = fromMaybe 0 $ join $ preview _src st
    nHand = count $ fromMaybe M.empty $ view (_hand <<< at id) st

    n | (nMarket > 0) && (nHand < 7) = 1
      | otherwise = 0

    st' = (takeFrom _src 1 <<< addTo _dest 1) st 
    
-- A player takes all the camels in the market
takeCamels :: PlayerId -> State -> State
takeCamels id st = st'
  where
    _src = (_market <<< at Camel) :: Lens' State (Maybe Int)
    _dest = (_herd <<< at id) :: Lens' State (Maybe Int)

    n = fromMaybe 0 $ view _src st
    st' = moveCards _src _dest n st

-- A player sells all of a given resource
sellCards :: PlayerId -> Resource -> State -> State
sellCards id rsrc st = st'
  where
    -- Check that player is holding the required minimum number of cards
    n = countHandResource id rsrc st
    n' | n < minimumSell rsrc = 0 
       | otherwise = n

    t = scoreTokens (Tuple rsrc n')
    st' = (over (_handResource id rsrc) (map (_-n'))
      <<< addTo (_tokens <<< at rsrc) (-n')
      <<< addTo (_points <<< at id) t) st

-- Exchange hand cards with market cards
exchangeCards :: PlayerId -> CardSet -> CardSet -> State -> State
exchangeCards id inSet outSet st = st'
  where 
    -- Functions to add/subtract maps with numeric values
    addSet = M.unionWith (+)
    subSet = M.unionWith $ flip (-)

    -- Ensure card numbers match
    n = (count inSet) - (count outSet)
    -- Do the card swaps
    st' | n == 0 = (over _market (addSet inSet) 
                    <<< over _market (subSet outSet)
                    <<< over (_hand <<< at id) (map $ addSet outSet)
                    <<< over (_hand <<< at id) (map $ subSet inSet)) st
        | otherwise = st

-- The End