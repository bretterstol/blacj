module Main where

import Lib

main :: IO ()
main = do
  putStrLn "Lets play a game!"
  controller [] createDeck

controller :: Hand -> Deck -> IO()
controller hand deck = do
  putStrLn "Do you want a card? y/n"
  line <- getLine
  if line == "y" then do
      card <- getRandomCard deck
      print card
      controller (card : hand) (filter (/= card) deck)
  else print $ getState hand

