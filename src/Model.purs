-- Model.purs
-- AndrewJ 2019-04-06

module Model where

import Prelude

import Data.Map as M
import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Show (genericShow)
import Data.Hashable (class Hashable, hash)
import Data.Tuple (Tuple)

-- ----------------
data Resource = Diamond | Gold | Silver | Cloth | Spice | Leather | Camel
derive instance genericResource :: Generic Resource _
derive instance eqResource :: Eq Resource
derive instance ordResource :: Ord Resource
instance showResource :: Show Resource where
  show = genericShow
instance hashResource :: Hashable Resource where
  hash = hash <<< show

data PlayerId = PlayerA | PlayerB
derive instance genericPlayerId :: Generic PlayerId _
derive instance eqPlayerId :: Eq PlayerId
derive instance ordPlayerId :: Ord PlayerId
instance showPlayerId :: Show PlayerId where
  show = genericShow
instance hashPlayerId :: Hashable PlayerId where
  hash = hash <<< show

-- ----------------
type CardCount = Tuple Resource Int
type CardSet = M.Map Resource Int

-- ----------------
-- Total state of the game
type State = 
  { deck    :: CardSet
  , market  :: CardSet
  , hand    :: M.Map PlayerId CardSet
  , herd    :: M.Map PlayerId Int
  , points  :: M.Map PlayerId Int
  , tokens  :: CardSet
  }

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