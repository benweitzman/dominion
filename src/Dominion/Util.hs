{-# LANGUAGE OverloadedStrings #-}

module Dominion.Util where
import           Control.Arrow
import           Control.Monad
import           Control.Monad.Logger
import           Control.Monad.State
import qualified Data.ByteString.Char8          as S8
import           Data.List
import qualified Data.Map.Lazy                  as M
import           Data.Ord
import           Data.Random                    hiding (shuffle)
import           Data.Random.Extras
import qualified Data.Text                      as T
import           Data.Text.Format
import           Data.Text.Format.Params
import qualified Data.Text.Lazy                 as TL
import           Language.Haskell.HsColour.ANSI
import           System.IO
import           System.Log.FastLogger
import           System.Random

red = highlight [Foreground Red]
green = highlight [Foreground Green]
yellow = highlight [Foreground Yellow]
blue = highlight [Foreground Blue]
cyan = highlight [Foreground Cyan]
dim = highlight [Dim]

gatedOutput :: Handle
            -> LogLevel
            -> Loc
            -> LogSource
            -> LogLevel
            -> LogStr
            -> IO ()
gatedOutput h minLevel loc src level msg =
    when (level >= minLevel) $ S8.hPutStrLn h ls
  where
    ls = defaultLogStrBS loc src level msg

defaultLogStrBS :: Loc
                -> LogSource
                -> LogLevel
                -> LogStr
                -> S8.ByteString
defaultLogStrBS a b c d =
    fromLogStr $ defaultLogStr a b c d


every :: Int -> [a] -> [a]
every n xs = case drop (n-1) xs of
              (y:ys) -> y : every n ys
              [] -> []

fmt :: Params ps => Format -> ps -> T.Text
fmt f ps = TL.toStrict $ format f ps

printGraph :: (MonadLogger m, MonadIO m) => (Int, Int) -> [Int] -> m ()
printGraph dimmensions values = printMultiGraph dimmensions [(values, '*')]

printMultiGraph :: (MonadLogger m, MonadIO m) => (Int, Int) -> [([Int], Char)] -> m ()
printMultiGraph _ [] = return ()
printMultiGraph (width, height) sequences = liftIO $ mapM_ putStrLn . transpose . map reverse $ rows
    where n = length . fst $ head sequences
          trimmed = map (first (take n)) sequences
          maxVal = maximum $ map (maximum . fst) trimmed
          minVal = minimum $ map (minimum . fst) trimmed
          yDiff = maxVal - minVal
          sampleRate = max 1 $ n `div` width
          samples = map (first (every sampleRate)) trimmed
          numSamples = length .fst $ head samples
          collatedSamples = map (\idx -> map (\(values, c) -> (values !! idx, c)) samples) [0..numSamples-1]
          sortedRows = map (sortBy (comparing fst)) collatedSamples :: [[(Int, Char)]]
          differenceRows = map diffList sortedRows
          diffList [] = []
          diffList xs@(y:ys) = y:zipWith (\(a1, b1) (a2, b2) -> (a1 - a2, b1)) ys xs
          rows = map toRow differenceRows
          toRow points = pad $ foldl accumRow "" points
          accumRow row (0, _) = tail row ++ "*"
          accumRow row (point, c) = row ++ replicate (fill point - 1) ' ' ++ [c]
          pad row = row ++ replicate (height - length row) ' '
          fill x = ((x - minVal) * height) `div` yDiff

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

fst3 :: (a, b, c) -> a
fst3 (a, _, _) = a

snd3 :: (a, b, c) -> b
snd3 (_, b, _) = b

trd :: (a, b, c) -> c
trd (_, _, c) = c

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

collate :: Ord a => [M.Map a Int] -> M.Map a [Int]
collate [] = M.fromList []
collate maps@(m:_) = M.fromList $ map (\k -> (k, map (M.! k) maps)) (M.keys m)

decrement :: Ord a => a -> M.Map a Int -> M.Map a Int
decrement = M.adjust (\x -> x - 1)

modifyListElem :: Eq a => a -> (a -> a) -> [a] -> [a]
modifyListElem x = modifyListElemBy (== x)

modifyListElemBy :: (a -> Bool) -> (a -> a) -> [a] -> [a]
modifyListElemBy _ _ [] = []
modifyListElemBy e f (x:xs)
    | e x = f x:xs
    | otherwise = x:modifyListElemBy e f xs

modifyListIndex :: Int -> (a -> a) -> [a] -> [a]
modifyListIndex _ _ [] = []
modifyListIndex 0 f (x:xs) = f x:xs
modifyListIndex n f (x:xs) = x:modifyListIndex (n-1) f xs
