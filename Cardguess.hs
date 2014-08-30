--  File     : Cardguess.hs
--  Author   : Rongduan Zhu
--  Purpose  : A guesser program used for guessing card

-- Todo:
-- initialGuess :: Int -> ([Card],GameState)
-- nextGuess :: ([Card],GameState) -> (Int,Int,Int,Int,Int) -> ([Card],GameState)

--module Cardguess (initialGuess, nextGuess, GameState(..)) where
module Cardguess (initialGuess, GameState(..)) where

import Data.List
import Card

data GameState = GameState [[Card]] (Int, Int, Int, Int, Int)
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

sMakeCombination :: Int -> [Card] -> [[Card]]
sMakeCombination numCards c = map sort (makeCombination numCards c)

--Credit: http://www.haskell.org/haskellwiki/99_questions/Solutions/26
makeCombination :: Int -> [a] -> [[a]]
makeCombination 0 _  = [ [] ]
makeCombination n xs = [ y:ys | y:xs' <- tails xs
                           , ys <- makeCombination (n-1) xs']

initialGuess :: Int -> ([Card], GameState)
initialGuess numCards = (guessedCards, gameState)
    where
    guessedCards = sGenInitCards firstSplit firstSplit
        where firstSplit = ceiling (52 / fromIntegral (numCards + 1))
    gameState = GameState updatedCombo (0, 0, 0, 0, 0)
        where
        allCombo = sMakeCombination
            numCards [(Card Club R2)..(Card Spade Ace)]
        updatedCombo = delete guessedCards allCombo

sGenInitCards :: Int -> Int -> [Card]
sGenInitCards newCard increment = sort (genInitCards newCard increment)

genInitCards :: Int -> Int -> [Card]
genInitCards newCard increment
    | newCard > 51 = []
    | otherwise = (Card s r) : (genInitCards nextCard increment)
        where
        s = toEnum (newCard `mod` 4)
        r = toEnum (newCard `div` 4)
        nextCard = newCard + increment

--nextGuess :: ([Card],GameState) -> (Int,Int,Int,Int,Int) -> ([Card],GameState)
--nextGuess reply = guessNewCards

--old functions
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
