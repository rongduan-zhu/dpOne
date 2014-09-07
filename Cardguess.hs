--  File     : Cardguess.hs
--  Author   : Rongduan Zhu
--  Purpose  : A guesser program used for guessing card

module Cardguess (initialGuess, nextGuess, GameState(..)) where

import Data.List
import Card
import Data.Ord(comparing)

type BelieveSpace = [[Card]]
type Hint = (Int, Int, Int, Int, Int)
type GameStateBundle = ([Card], GameState)
type ResponseBundle = (GameStateBundle, Hint)

data GameState = GameState {getBelieve :: BelieveSpace}
    deriving (Show)

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
sMakeCombination numCards c = sort $
    map (sortBy compRank) (makeCombination numCards c)

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
    gameState = GameState believeSpace
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
nextGuess (prevGuess, GameState believeSpace) hint =
    (bestGuess, GameState (delete bestGuess newBelieve))
    where
    newBelieve = filter (\x -> response prevGuess x == hint) believeSpace
    bestGuess = calcBestG nBelieveSpace nBelieveSpace
        where
        nBelieveSpace =
            if length newBelieve > 1000 then
                (take 500 newBelieve) ++ (take 500 $ reverse believeSpace)
                else
                    newBelieve

calcBestG :: BelieveSpace -> [[Card]] -> [Card]
calcBestG believeSpace guesses = bestG
    where
    weightedSums =
        zip guesses (map calcWS (map (groupResponses believeSpace) guesses))
    bestG = fst $ minimumBy (comparing snd) weightedSums

calcWS :: [Hint] -> Double
calcWS hints = weightedSum
    where
    groupCount = map length . group . sort $ hints
    top = foldr (+) 0 (map (\x -> x ^ 2) groupCount)
    bottom = foldr (+) 0 groupCount
    weightedSum = fromIntegral top / fromIntegral bottom

groupResponses :: BelieveSpace -> [Card] -> [Hint]
groupResponses believeSpace guess = map (response guess) believeSpace

response :: [Card] -> [Card] -> Hint
response guess answer = (c, l, r, u, s)
    where
    c = getOccurances guess answer
    r = getOccurancesR guess answer
    s = getOccurancesS guess answer
    l = length $ filter
        (\x -> x < minimum (card2Rank guess)) (card2Rank answer)
    u = length $ filter
        (\x -> x > maximum (card2Rank guess)) (card2Rank answer)

getOccurances :: Eq a => [a] -> [a] -> Int
getOccurances guess answer
    = length answer - length (removeOccurances guess answer)

getOccurancesR :: [Card] -> [Card] -> Int
getOccurancesR guess answer =
    getOccurances (card2Rank guess) (card2Rank answer)

getOccurancesS :: [Card] -> [Card] -> Int
getOccurancesS guess answer =
    getOccurances (card2Suit guess) (card2Suit answer)

removeOccurances :: Eq a => [a] -> [a] -> [a]
removeOccurances (x:xs) cards = removeOccurances xs (delete x cards)
removeOccurances [] cards = cards

card2Rank :: [Card] -> [Rank]
card2Rank cards = map rank cards

card2Suit :: [Card] -> [Suit]
card2Suit cards = map suit cards

compRank :: Card -> Card -> Ordering
compRank (Card s1 r1) (Card s2 r2) =
    let rankOrder = compare r1 r2 in
    if rankOrder == EQ then compare s1 s2 else rankOrder
