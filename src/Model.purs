-- Model.purs
-- AndrewJ 2019-04-06

module Model where

import Prelude

import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Show (genericShow)
import Data.HashMap (HashMap, lookup, insert, delete) as M
import Data.Hashable (class Hashable, hash)
import Data.Lens (Lens', lens)
import Data.Lens.At (class At, at)
import Data.Maybe (maybe)
import Data.Tuple (Tuple)

-- ----------------
data Resource = Diamond | Gold | Silver | Cloth | Spice | Leather | Camel
derive instance genericResource :: Generic Resource _
derive instance eqResource :: Eq Resource
instance showResource :: Show Resource where
  show = genericShow
instance hashResource :: Hashable Resource where
  hash = hash <<< show

{- instance atHashMap :: Ord k => At (M.HashMap k v) k v where
  at k =
    lens (M.lookup k) \m ->
      maybe (M.delete k m) \v -> 
        M.insert k v m
 -}
-- ----------------
type CardCount = Tuple Resource Int
type CardSet = M.HashMap Resource Int

-- ----------------
-- Total state of the game
type State = 
  { deck    :: CardSet
  , market  :: CardSet
  , hand    :: Array CardSet
  , herd    :: Array CardCount -- must all be camels
  , points  :: Array Int
  , tokens  :: CardSet
  }

-- Player ID, in this case 0 or 1
type PlayerId = Int

-- ----------------
-- Available player actions
data Action 
  = Take PlayerId Resource
  | Exchange PlayerId CardSet
  | Sell PlayerId Resource

-- ----------------
type StepOutput =
  { observation :: State
  , reward :: Number
  , isDone :: Boolean
  , info :: String 
  }


-- The End