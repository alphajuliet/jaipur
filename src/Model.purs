-- Model.purs
-- AndrewJ 2019-04-06

module Model where

import Prelude
import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Show (genericShow)
import Data.Lens (Lens')
import Data.Tuple (Tuple)


data Resource = Diamond | Gold | Silver | Cloth | Spice | Leather | Camel
derive instance genericResource :: Generic Resource _
derive instance eqResource :: Eq Resource
instance showResource :: Show Resource where
  show = genericShow
--instance hashResource :: Hashable Resource where
--  hash = hash <<< show

-- ----------------
type CardCount = Tuple Resource Int
type CardSet = Array CardCount

type CardLens = Lens' CardSet Int

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

-- ----------------
-- Available actions
data Action 
  = Take Resource
  | Exchange CardSet
  | Sell CardCount

-- ----------------
type StepOutput =
  { observation :: State
  , reward :: Number
  , done :: Boolean
  , info :: String 
  }

-- The End