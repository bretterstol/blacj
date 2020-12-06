module Main where

import Lib

main :: IO ()
main = do
  putStrLn "Lets play a game!"
  controller

controller :: IO()
controller = do
  putStrLn "Do you want a card? y/n"
  line <- getLine
  if line == "y" then do
      card <- getRandomCard createDeck
      print card
      print $ getState card
      controller
    else putStrLn "Bye"
