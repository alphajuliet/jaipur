-- Model.purs
-- AndrewJ 2019-04-06
-- [i:258115]

module Model where

import Prelude

import Data.Array (null)
import Data.Foldable (intercalate)
import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Show (genericShow)
import Data.Hashable (class Hashable, hash)
import Data.Lens (Lens', lens, view)
import Data.Lens.At (at)
import Data.Map as M
import Data.Maybe (Maybe, fromMaybe)
import Data.Tuple (Tuple(..), fst, snd)

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
  | Exchange PlayerId CardSet CardSet
  | Sell PlayerId Resource

-- ----------------
type StepOutput =
  { observation :: State
  , reward :: Number
  , isDone :: Boolean
  , info :: String 
  }

-- ----------------
-- Lenses into the state, to encapsulate the model
-- The convention is to prefix a lens with an underscore

type CardLens = Lens' State (Maybe Int)

_deck :: Lens' State CardSet
_deck = lens _.deck $ _ { deck = _ }

_market :: Lens' State CardSet
_market = lens _.market $ _ { market = _ }

_hand :: Lens' State (M.Map PlayerId CardSet)
_hand = lens _.hand $ _ { hand = _ }

_herd :: Lens' State (M.Map PlayerId Int)
_herd = lens _.herd $ _ { herd = _ }

_tokens :: Lens' State CardSet
_tokens = lens _.tokens $ _ { tokens = _ }

_points :: Lens' State (M.Map PlayerId Int)
_points = lens _.points $ _ { points = _ }

-- Initial state of the game
initialState :: State
initialState = 
  { deck: M.fromFoldable 
      [ (Tuple Diamond 6), (Tuple Gold 6), (Tuple Silver 6), (Tuple Cloth 8)
      , (Tuple Spice 8), (Tuple Leather 10), (Tuple Camel 11) ]
  , market: M.empty
  , hand: M.fromFoldable [ (Tuple PlayerA M.empty), (Tuple PlayerB M.empty) ]
  , herd: M.fromFoldable [ (Tuple PlayerA 0), (Tuple PlayerB 0) ]
  , points: M.empty
  , tokens: M.fromFoldable 
      [ (Tuple Diamond 5), (Tuple Gold 5), (Tuple Silver 5), (Tuple Cloth 7)
      , (Tuple Spice 7), (Tuple Leather 9)]
  }

-- --------
-- Prettier print the current state

showMap :: forall k v. Show k => Show v => M.Map k v -> String
showMap c = str
  where
    tupleArray = (M.toUnfoldable c) :: Array (Tuple k v)
    strArray = map (\t -> (show $ fst t) <> ": " <> (show $ snd t)) $ tupleArray
    str | null strArray = "(empty)"
        | otherwise = intercalate " | " strArray

showState :: State -> String
showState st = intercalate ", " 
  [ "Deck: " <> (showMap st.deck)
  , "Market: " <> (showMap st.market)
  , "Hand A: " <> (showMap $ fromMaybe M.empty $ view (at PlayerA) st.hand)
  , "Hand B: " <> (showMap $ fromMaybe M.empty $ view (at PlayerB) st.hand)
  , "Herds: " <> showMap st.herd
  , "Tokens: " <> (showMap st.tokens) 
  ]

-- The End