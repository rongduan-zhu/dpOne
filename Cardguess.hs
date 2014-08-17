--  File     : Cardguess.hs
--  Author   : Rongduan Zhu
--  Purpose  : A guesser program used for guessing card

-- Todo:
-- initialGuess :: Int -> ([Card],GameState)
-- nextGuess :: ([Card],GameState) -> (Int,Int,Int,Int,Int) -> ([Card],GameState)

--module Cardguess (initialGuess, nextGuess, GameState(..)) where
module Cardguess (initialGuess, GameState(..)) where

import Card

data GameState = GameState [Card] (Int, Int, Int, Int, Int)
    deriving (Show)

stateCorrect :: (Int, Int, Int, Int, Int) -> Int
stateCorrect (a, _, _, _, _) = a

stateLower :: (Int, Int, Int, Int, Int) -> Int
stateLower (_, a, _, _, _) = a

stateSameRank :: (Int, Int, Int, Int, Int) -> Int
stateSameRank (_, _, a, _, _) = a

stateHigher :: (Int, Int, Int, Int, Int) -> Int
stateHigher (_, _, _, a, _) = a

stateSameSuit :: (Int, Int, Int, Int, Int) -> Int
stateSameSuit (_, _, _, _, a) = a


initialGuess :: Int -> ([Card], GameState)
initialGuess numCards = ((genInitCards firstSplit firstSplit), gameState)
    where
    firstSplit = ceiling (52 / fromIntegral (numCards + 1))
    gameState = GameState [(Card Club R2)..(Card Spade Ace)] (0, 0, 0, 0, 0)
    genInitCards a increment
        | a > 51 = []
        | otherwise = (Card s r) : (genInitCards nextSplit increment)
            where
            s = toEnum (a `div` 13)
            r = toEnum (a `mod` 13)
            nextSplit = a + increment


--nextGuess :: ([Card],GameState) -> (Int,Int,Int,Int,Int) -> ([Card],GameState)
--nextGuess reply = guessNewCards

searchBound :: Int -> Int -> Int -> Int
searchBound guess answer step
    | newGuess < answer = searchBound guess answer (2 * step)
    | otherwise = newGuess
    where
    newGuess = boundCard (guess + step)

-- |This function bounds an overflow card rather than wrapping around
boundCard :: Int -> Int
boundCard card
    | card > 51 = 51
    | card < 0 = 0
    | otherwise = card
