-- src/Jaipur.purs
-- AndrewJ 2019-03-31

module Jaipur where
  
import Prelude

import Data.Array (foldl, index, length, slice)
import Data.Foldable (sum)
import Data.Lens (Lens', over, preview, set, setJust, traversed, view)
import Data.Lens.At (at)
import Data.Map (toUnfoldable, singleton) as M
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Tuple (Tuple(..), fst, snd)
import Effect (Effect)
import Effect.Random (randomInt)
import Model (Action(..), CardCount, CardSet, PlayerId, Resource(..), State, StepOutput, 
              _market, _deck, _hand, _herd, _tokens, _points)

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


{- newtype TMap = TMap M.Map
instance showTMap :: Show TMap where
  show m = map (\t -> show (fst t) <> ": " <> show (snd t)) 
               (M.toUnfoldable $ TMap m)
 -}
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

type CardLens = Lens' State (Maybe Int)

-- Utility function to add or subtract a resource card
addToTarget :: CardLens -> Int -> State -> State
addToTarget _lens n st = st'
  where
    current = view _lens st
    st' = case current of
      Just _ -> over _lens (map (_+n)) st
      Nothing -> setJust _lens n st

-- Move n cards from src to dest
moveCards :: CardLens -> CardLens -> Int -> State -> State
moveCards _src _dest n st = st'
  where
    st' = (addToTarget _src (-n) <<< addToTarget _dest n) st 

-- Deal a card from the Deck to the Market
dealCard :: Resource -> State -> State
dealCard rsrc = moveCards (_deck <<< at rsrc) (_market <<< at rsrc) 1

-- Take a card of a given type from the market 
takeCard :: PlayerId -> Resource -> State -> State
takeCard id rsrc st = st'
  where 
    s0 = addToTarget (_market <<< at rsrc) (-1) st 
    _lens = _hand <<< at id <<< traversed <<< at rsrc
    current = join $ preview _lens s0
    st' = case current of 
      Just _ -> over (_hand <<< at id <<< traversed <<< at rsrc) (map (_+1)) s0
      Nothing -> setJust (_hand <<< at id <<< traversed <<< at rsrc) 1 s0

-- Take all the camels in the market
takeCamels :: PlayerId -> State -> State
takeCamels id st = st'
  where
    n = fromMaybe 0 $ view (_market <<< at Camel) st
    st' = moveCards (_market <<< at Camel) (_herd <<< at id) n st

-- Sell all of a given card
sellCards :: PlayerId -> Resource -> State -> State
sellCards id rsrc st = st'
  where
    n = countHandResource id rsrc st
    r = (M.singleton rsrc 0) :: CardSet
    s1 = set (_hand <<< at id <<< traversed <<< at rsrc) Nothing st
    s2 = addToTarget (_tokens <<< at rsrc) (-n) s1
    t = scoreTokens (Tuple rsrc n)
    st' = addToTarget (_points <<< at id) t s2

-- exchangeCards :: PlayerId -> CardSet -> CardSet -> State -> State
-- exchangeCards id inSet outSet st = ...

-- _hand_id_rsrc :: PlayerId -> Resource -> CardLens
-- _hand_id_rsrc id rsrc = _hand <<< at id <<< traversed <<< at rsrc 

countHandResource :: PlayerId -> Resource -> State -> Int
countHandResource id rsrc st = 
  fromMaybe 0 $ join $ 
    preview (_hand <<< at id <<< traversed <<< at rsrc) st

-- The End