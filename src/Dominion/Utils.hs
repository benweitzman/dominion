module Dominion.Utils where
import Control.Monad
import Control.Monad.State
import qualified Data.Map.Lazy as M
import Data.Random.Extras
import Data.Random hiding (shuffle)
import System.Random
import Language.Haskell.HsColour.ANSI
import Data.List

red = highlight [Foreground Red]
green = highlight [Foreground Green]
yellow = highlight [Foreground Yellow]
blue = highlight [Foreground Blue]
cyan = highlight [Foreground Cyan]
dim = highlight [Dim]

count :: Eq a => a -> [a] -> Int
count x list = length $ filter (==x) list

countBy func list = length $ filter func list

for = flip map

deckShuffle :: [a] -> IO [a]
deckShuffle deck = do
    gen <- getStdGen
    let (shuffled, newGen) = sampleState (shuffle deck) gen
    setStdGen newGen
    return shuffled

-- times :: Monad m => Int -> m a -> [m b]
times iterations block = forM_ [1..iterations] $ const block

indices :: [a] -> [Int]
indices arr = [0..(length arr - 1)]

join = intercalate

failIf :: Bool -> String -> Maybe String
failIf True str = Just str
failIf False str = Nothing

maybeToBool :: Maybe a -> Bool
maybeToBool (Just _) = False
maybeToBool Nothing = True

-- | rotate a list
--
-- >>> rotate 2 [1, 2, 3]
-- [3, 2, 1]
--
-- >>> rotate 6 [1, 2, 3]
-- [1, 2, 3]
--
rotate :: Int -> [a] -> [a]
rotate n xs = drop n' xs ++ take n' xs
  where n' = n `mod` length xs

decrement :: Ord a => a -> M.Map a Int -> M.Map a Int
decrement = M.adjust (\x -> x - 1)

modifyListElem :: Eq a => a -> (a -> a) -> [a] -> [a]
modifyListElem x = modifyListElemBy (== x)

modifyListElemBy :: (a -> Bool) -> (a -> a) -> [a] -> [a]
modifyListElemBy _ _ [] = []
modifyListElemBy e f (x:xs)
    | e x == True = f x:xs
    | otherwise = x:modifyListElemBy e f xs

modifyListIndex :: Int -> (a -> a) -> [a] -> [a]
modifyListIndex _ _ [] = []
modifyListIndex 0 f (x:xs) = f x:xs
modifyListIndex n f (x:xs) = x:modifyListIndex (n-1) f xs