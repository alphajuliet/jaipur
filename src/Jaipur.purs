-- src/Jaipur.purs
-- AndrewJ 2019-03-31

module Jaipur where
  
import Prelude

import Data.Array (foldl, index, length, slice)
import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Show (genericShow)
import Data.HashMap (HashMap, empty, fromArray, values)
import Data.Hashable (class Hashable, hash)
import Data.Maybe (Maybe)
import Data.Tuple (Tuple(..), fst, snd)
import Effect (Effect)
import Effect.Random (randomInt)

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
sumSubset arr n = x
  where 
    s = (slice 0 n arr)
    x = foldl add 0 s

-- ----------------
data Resource = Diamond | Gold | Silver | Cloth | Spice | Leather | Camel
derive instance genericResource :: Generic Resource _
derive instance eqResource :: Eq Resource
instance showResource :: Show Resource where
  show = genericShow
instance hashResource :: Hashable Resource where
  hash = hash <<< show

-- ----------------
type Cards = Tuple Resource Int
type CardSet = HashMap Resource Int

-- ----------------
-- Total state of the game
type State = 
  { deck :: CardSet
  , market :: CardSet
  , handA :: CardSet
  , handB :: CardSet
  , herdA :: CardSet
  , herdB :: CardSet
  , pointsA :: Int
  , pointsB :: Int
  , tokens :: CardSet
  }

-- Subset of total state that is observable
type Observation = 
  { deckSize :: Int
  , market :: CardSet
  , handA :: CardSet
  , handB :: CardSet
  , herdA :: CardSet
  , herdB :: CardSet
  , pointsA :: Int
  , pointsB :: Int
  , tokens :: CardSet
  }

-- ----------------
-- Available actions
data Action 
  = Take Resource
  | Exchange CardSet
  | Sell Cards

-- ----------------
type StepOutput =
  { observation :: Observation
  , reward :: Number
  , done :: Boolean
  , info :: String 
  }

-- ----------------
-- reset :: State
reset :: State
reset = 
  { deck: fromArray [ (Tuple Diamond 6), (Tuple Gold 6), (Tuple Silver 6), (Tuple Cloth 8), 
                      (Tuple Spice 8), (Tuple Leather 10), (Tuple Camel 11) ]
  , market: empty
  , handA: empty
  , handB: empty
  , herdA: empty
  , herdB: empty
  , pointsA: 0
  , pointsB: 0
  , tokens: fromArray [ (Tuple Diamond 5), (Tuple Gold 5), (Tuple Silver 5), (Tuple Cloth 7), 
                        (Tuple Spice 7), (Tuple Leather 9)]
  }

-- observation_space :: State -> Observation
-- step :: State -> Action -> StepOutput

-- Count the number of CardSet in a pile
count :: CardSet -> Int
count = values >>> foldl add 0

-- Deal a card from the deck to the market
deal :: State -> State
deal s = s' 
  where
    s' = s

-- Score the tokens
scoreTokens :: CardSet -> Cards -> Int
scoreTokens c t = p
  where 
  n = snd t
  p = case (fst t) of
    Diamond -> sumSubset [7, 7, 5, 5, 5] n
    Gold -> sumSubset [6, 6, 5, 5, 5] n
    Silver -> sumSubset [5, 5, 5, 5, 5] n
    Cloth -> sumSubset [5, 3, 3, 2, 2, 1] n
    Spice -> sumSubset [5, 3, 3, 2, 2, 1] n
    Leather -> sumSubset [4, 3, 2, 1, 1, 1, 1, 1, 1] n
    _ -> 0


-- The Endn 