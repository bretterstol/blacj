module Lib
    ( Color
    , Value
    , Card
    , Deck
    , createDeck
    , getRandomCard
    , handState
    , getState
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

getRandomCard :: Deck -> IO(Card)
getRandomCard deck = (deck !!) <$> randomRIO (0, length deck - 1)



type HandScore = (Deck, Int)

type GameState = State (Hand, Int)

handState :: Card -> GameState Int
handState card = do
  (hand, score) <- get
  put ([card] ++ hand, 10)
  (hand, newScore) <- get
  return newScore

getState :: Card -> Int
getState c = evalState (handState c) ([], 0)

calculateValue :: Card -> Int
calculateValue c = 10
