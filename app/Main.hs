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
      (newDeck, card) <- getNewCard deck
      print card
      handleResult (card:hand) newDeck
  else do
    dealerState <- dealer [] deck
    declareAWinner (getState hand) dealerState

handleResult :: Hand -> Deck -> IO()
handleResult hand deck = do
      state <- checkState hand
      case state of
        Right num -> do
          putStrLn "Score so far: "
          print num
          controller hand deck
        Left num -> do
          putStrLn "You lost"
          putStrLn "Final score: "
          print num
          print $ getState hand


shouldQuit :: HandScore -> Bool
shouldQuit (_, score) = score > 16

dealer :: Hand -> Deck -> IO HandScore
dealer hand deck = do
  let state = getState hand
  if shouldQuit state then return state
  else do
    (newDeck, card) <- getNewCard deck
    (newDeck, card) <- getNewCard deck
    dealer (card:hand) newDeck


declareAWinner :: HandScore -> HandScore -> IO ()
declareAWinner (userHand, userScore) (dealerHand, dealerScore) =
  if dealerScore < 22 && dealerScore >= userScore then do
    putStrLn "Dealer won!"
    putStrLn "You lost!"
    putStr "Dealer hand: "
    print (dealerHand, dealerScore)
    putStr "Your hand: "
    print (userHand, userScore)
  else do
    putStrLn "You won!"
    putStr "Your hand: "
    print (userHand, userScore)
    putStr "Dealer hand: "
    print (dealerHand, dealerScore)