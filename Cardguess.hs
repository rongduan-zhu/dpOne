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

type BelieveSpace = [[Card]]
type GuessedCards = [[Card]]
type Hint = (Int, Int, Int, Int, Int)
type GameStateBundle = ([Card], GameState)

--GameState BelieveSpace GuessedCards
data GameState = GameState BelieveSpace GuessedCards Hint
    deriving (Show)

getBelieve :: GameState -> BelieveSpace
getBelieve (GameState believeSpace _ _) = believeSpace

getGuessed :: GameState -> GuessedCards
getGuessed (GameState _ guessedCards _) = guessedCards

getHint :: GameState -> Hint
getHint (GameState _ _ hint) = hint

stateCorrect :: GameState -> Int
stateCorrect = correct.getHint

stateLower :: GameState -> Int
stateLower = lower.getHint

stateSameRank :: GameState -> Int
stateSameRank = sameRank.getHint

stateHigher :: GameState -> Int
stateHigher = higher.getHint

stateSameSuit :: GameState -> Int
stateSameSuit = sameSuit.getHint

correct :: Hint -> Int
correct (a, _, _, _, _) = a

lower :: Hint -> Int
lower (_, a, _, _, _) = a

sameRank :: Hint -> Int
sameRank (_, _, a, _, _) = a

higher :: Hint -> Int
higher (_, _, _, a, _) = a

sameSuit :: Hint -> Int
sameSuit (_, _, _, _, a) = a

sMakeCombination :: Int -> [Card] -> [[Card]]
sMakeCombination numCards c = map (sortBy
    compRank) (makeCombination numCards c)

--Credit: http://www.haskell.org/haskellwiki/99_questions/Solutions/26
makeCombination :: Int -> [a] -> [[a]]
makeCombination 0 _  = [ [] ]
makeCombination n xs = [ y:ys | y:xs' <- tails xs
                           , ys <- makeCombination (n-1) xs']

initialGuess :: Int -> GameStateBundle
initialGuess numCards = (guessedCards, gameState)
    where
    guessedCards = sGenInitCards firstSplit firstSplit
        where firstSplit = ceiling (52 / fromIntegral (numCards + 1))
    gameState = GameState updatedCombo [guessedCards, []] (0, 0, 0, 0, 0)
        where
        allCombo = sMakeCombination
            numCards [(Card Club R2)..(Card Spade Ace)]
        updatedCombo = delete guessedCards allCombo

sGenInitCards :: Int -> Int -> [Card]
sGenInitCards newCard increment = sortBy
    compRank (genInitCards newCard increment)

genInitCards :: Int -> Int -> [Card]
genInitCards newCard increment
    | newCard > 51 = []
    | otherwise = (Card s r) : (genInitCards nextCard increment)
        where
        s = toEnum (newCard `mod` 4)
        r = toEnum (newCard `div` 4)
        nextCard = newCard + increment

nextGuess :: GameStateBundle -> Hint -> GameStateBundle
nextGuess (prevGuess, gameState) hint =
    (prevGuess, gameState)

{- Checking ranks -}

--checkCorrect :: GameStateBundle -> Hint -> GameStateBundle
--checkCorrect bundle@(prevGuess, gameState) hint
--    | stateCorrect gameState >= length prevGuess = error "Error at check rank"
--    | otherwise = filterCorrect bundle diff
--    where
--    diff = stateCorrect gameState - correct hint

--filterCorrect :: GameStateBundle -> Int -> GameStateBundle
--filterCorrect bundle@(prevGuess, GameState believe guessed hint) diff
--    | diff > 0 = (prevGuess, GameState filteredBelieve guessed hint)
--    | diff == 0 = bundle
--    | otherwise = bundle
--    where
--    filteredBelieve = reduceBelieve filterCard prevGuess believe

filterCard :: [Card] -> BelieveSpace -> BelieveSpace
filterCard guess believe = filter (elems guess) believe

--{- reduce function -}
--reduceBelieve :: (Card -> BelieveSpace -> BelieveSpace) -> [Card]
--    -> BelieveSpace -> BelieveSpace
--reduceBelieve filterFunc (c:cs) believe =
--    reduceBelieve filterFunc cs updatedBelieve
--    where
--    updatedBelieve = filterFunc c believe
--reduceBelieve _ [] believe = believe

elems :: [Card] -> [Card] -> Bool
elems (x:xs) cards = elem x cards || elems xs cards
elems [] cards = False

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

--helper functions

compRank :: Card -> Card -> Ordering
compRank (Card s1 r1) (Card s2 r2) =
    let rankOrder = compare r1 r2 in
    if rankOrder == EQ then compare s1 s2 else rankOrder
