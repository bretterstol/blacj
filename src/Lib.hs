module Lib
    ( Color
    , Value
    , Card
    , Deck
    , Hand
    , HandScore
    , GameState
    , createDeck
    , getRandomCard
    , handState
    , getState
    , fromCardToInt
    ) where

import System.Random (randomRIO)
import Control.Monad.State

data Color = Hearts | Diamonds | Clubs | Space deriving (Show, Enum)

data Value = Two | Three | Four | Five | Six | Seven | Eight | Nine | Ten | Jack | Queen | King | Ace deriving (Show, Enum)

type Card = (Color, Value)

type Deck = [Card]

createDeck :: Deck
createDeck = do
  value <- [Two ..]
  color <- [Hearts ..]
  return (color, value)

getRandomCard :: Deck -> IO Card
getRandomCard deck = (deck !!) <$> randomRIO (0, length deck - 1)


type Hand = Deck
type HandScore = (Hand, Int)
type GameState = State HandScore


handState :: Hand -> GameState HandScore
handState [] = do
  (hand, score) <- get
  return (hand, score)
handState (card:cards) = do
  (hand, val) <- get
  put (card:hand, val + fromCardToInt card)
  handState cards

fromCardToInt :: Card -> Int
fromCardToInt c = case c of (_, Two) -> 2
                            (_, Three) -> 3
                            (_, Four) -> 4
                            (_, Five) -> 5
                            (_, Six) -> 6
                            (_, Seven) -> 7
                            (_, Eight) -> 8
                            (_, Nine) -> 9
                            _ -> 10


getState :: Hand -> HandScore
getState cards = evalState (handState cards) ([], 0)

