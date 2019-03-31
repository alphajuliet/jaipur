-- src/Jaipur.purs
-- AndrewJ 2019-03-31

module Jaipur where
  
import Prelude
import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Show (genericShow)
import Data.HashMap (HashMap, empty, fromArray)
import Data.Hashable (class Hashable, hash)
import Data.Tuple (Tuple(..))

-- ----------------
data Resource = Diamond | Gold | Silver | Cloth | Spice | Leather | Camel
derive instance genericResource :: Generic Resource _
derive instance eqResource :: Eq Resource
instance showResource :: Show Resource where
  show = genericShow
instance hashResource :: Hashable Resource where
  hash = hash <<< show

-- ----------------
type Cards = HashMap Resource Int

-- ----------------
-- Total state of the game
type State = 
  { deck :: Cards
  , market :: Cards
  , handA :: Cards
  , handB :: Cards
  , herdA :: Cards
  , herdB :: Cards
  , pointsA :: Int
  , pointsB :: Int
  , tokens :: Cards
  }

-- Subset of total state that is observable
type Observation = 
  { deckSize :: Int
  , market :: Cards
  , handA :: Cards
  , handB :: Cards
  , herdA :: Cards
  , herdB :: Cards
  , pointsA :: Int
  , pointsB :: Int
  , tokens :: Cards
  }

-- ----------------
-- reset :: State
reset :: State
reset = 
  { deck: fromArray [ (Tuple Diamond 6), (Tuple Gold 6), (Tuple Silver 6), (Tuple Cloth 8), (Tuple Spice 8), (Tuple Leather 10), (Tuple Camel 11) ]
  , market: empty
  , handA: empty
  , handB: empty
  , herdA: empty
  , herdB: empty
  , pointsA: 0
  , pointsB: 0
  , tokens: fromArray [ (Tuple Diamond 5), (Tuple Gold 5), (Tuple Silver 5), (Tuple Cloth 7), (Tuple Spice 7), (Tuple Leather 9)]
  }


-- The End