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
type ResponseBundle = (GameStateBundle, Hint)

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
    gameState = GameState believeSpace [guessedCards, []] (0, 0, 0, 0, 0)
        where
        allCombo = sMakeCombination
            numCards [(Card Club R2)..(Card Spade Ace)]
        believeSpace = delete guessedCards allCombo

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

rdBelieveSpace :: ResponseBundle -> ResponseBundle
rdBelieveSpace = reduceCorrect . reduceRank . reduceSuit . reduceBound

--checkCorrect :: GameStateBundle -> Hint -> GameStateBundle
--checkCorrect bundle@(prevGuess, gameState) hint
--    | stateCorrect gameState >= length prevGuess = error "Error at check rank"
--    | otherwise = filterCorrect bundle hint

reduceCorrect :: ResponseBundle -> ResponseBundle
reduceCorrect ((prevGuess, GameState believe guessed hint), newHint)
    = ((prevGuess, GameState filteredBelieve guessed hint), newHint)
    where
    filteredBelieve = filterCorrect (correct newHint) prevGuess believe

reduceRank :: ResponseBundle -> ResponseBundle
reduceRank ((prevGuess, GameState believe guessed hint), newHint)
    = ((prevGuess, GameState filteredBelieve guessed hint), newHint)
    where
    filteredBelieve = filterRank (sameRank newHint) prevGuess believe

reduceSuit :: ResponseBundle -> ResponseBundle
reduceSuit ((prevGuess, GameState believe guessed hint), newHint)
    = ((prevGuess, GameState filteredBelieve guessed hint), newHint)
    where
    filteredBelieve = filterSuit (sameSuit newHint) prevGuess believe

reduceBound :: ResponseBundle -> ResponseBundle
reduceBound ((prevGuess, GameState believe guessed hint), newHint)
    = ((prevGuess, GameState filteredRight guessed hint), newHint)
    where
    filteredLeft = filterLBound (lower hint) prevGuess believe
    filteredRight = filterRBound (higher hint) prevGuess filteredLeft

{- functions for reducing believe space -}
filterCorrect :: Int -> [Card] -> BelieveSpace -> BelieveSpace
filterCorrect num guess believe
    = filter (containExact num getOccurances guess) believe

filterRank :: Int -> [Card] -> BelieveSpace -> BelieveSpace
filterRank num guess believe
    = filter (containExact num getOccurancesR guess) believe

filterSuit :: Int -> [Card] -> BelieveSpace -> BelieveSpace
filterSuit num guess believe
    = filter (containExact num getOccurancesS guess) believe

filterLBound :: Int -> [Card] -> BelieveSpace -> BelieveSpace
filterLBound less guess believe
    | less > 0 = filter (lessThan (guess !! (less - 1))) believe

filterRBound :: Int -> [Card] -> BelieveSpace -> BelieveSpace
filterRBound greater guess believe
    | greater > 0 = filter (greaterThan (guess !! (greater - 1))) believe

containExact :: Eq a => Int -> ([a] -> [a] -> Int) -> [a] -> [a] -> Bool
containExact num filterFunc xs cards
    | num == filterFunc xs cards = True
    | otherwise = False

getOccurances :: Eq a => [a] -> [a] -> Int
getOccurances guess answer
    = length answer - length (removeOccurances guess answer)

removeOccurances :: Eq a => [a] -> [a] -> [a]
removeOccurances (x:xs) cards = removeOccurances xs (delete x cards)
removeOccurances [] cards = cards

getOccurancesR :: [Card] -> [Card] -> Int
getOccurancesR xs cards = getOccurances (card2Rank xs) (card2Rank cards)

getOccurancesS :: [Card] -> [Card] -> Int
getOccurancesS xs cards = getOccurances (card2Suit xs) (card2Suit cards)

card2Rank :: [Card] -> [Rank]
card2Rank cards = map rank cards

card2Suit :: [Card] -> [Suit]
card2Suit cards = map suit cards

--old functions
searchBound :: Int -> Int -> Int -> Int
searchBound guess answer step
    | newGuess < answer = searchBound guess answer (2 * step)
    | otherwise = newGuess
    where
        newGuess = boundCard (guess + step)

elems :: [Card] -> [Card] -> Bool
elems (x:xs) cards = elem x cards || elems xs cards
elems [] cards = False

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

lessThan :: Card -> [Card] -> Bool
lessThan (Card s r) guess
    | compare r (rank (head guess)) == LT = True
    | otherwise = False

greaterThan :: Card -> [Card] -> Bool
greaterThan (Card s r) guess
    | compare r (rank (head guess)) == GT = True
    | otherwise = False

extractGuess :: GameStateBundle -> [Card]
extractGuess (_, GameState (b:believe) _ _)
    = b

finalizeGameState :: [Card] -> Hint -> GameStateBundle -> GameStateBundle
finalizeGameState newGuess newHint (prev, GameState believe guessed hint)
    = (prev, GameState newBelieve (newGuess:guessed) newHint)
    where
    newBelieve = delete newGuess believe
