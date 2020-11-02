module Lib where
import System.Environment
import Prelude hiding (flip)
import Data.List
import Data.Ord
import Data.Maybe
import Debug.Trace
--import System.Random
import Control.Monad



flip :: Int -> [a] -> [a]
flip _ [] = []
flip i xs = reverse (take i xs) ++ (drop i xs)


flipSequence :: Ord a => [a] -> [Int]
flipSequence xs = list_to_return2 xs (length xs)

isSorted :: (Ord a) => [a] -> Bool
isSorted [] = True
isSorted [x] = True
isSorted (x:y:xs) = x <= y && isSorted(y:xs)

find_maximum_index :: Ord a => [a] -> Int
find_maximum_index xs = fromMaybe 0 $ elemIndex (maximum xs) xs

list_to_return2 :: Ord a => [a] -> Int -> [Int]
list_to_return2 xs 0 = []
list_to_return2 xs n
        | x /= 0 && x /= (n -1) = [x+1] ++ [n] ++ list_to_return2 (flip  n (flip (x+1) xs) ) (n-1)
        | x == (n-1) =  list_to_return2 xs (n - 1)
        | otherwise = [n] ++ list_to_return2 (flip n xs) (n-1)
                where x = find_maximum_index (take n xs)

optFlipSequence :: Ord a => [a] -> [Int]
optFlipSequence xs = optflipSequence_helper xs (permute (generate_array n) 1) 1 0 (flipSequence xs)
        where n = length xs
       

optflipSequence_helper :: Ord a => [a] -> [[Int]] -> Int -> Int -> [Int] -> [Int]
optflipSequence_helper list lists_to_compare level i max_n
        | level == length (max_n) = max_n
        | i == (length (lists_to_compare)) = optflipSequence_helper list (permute (generate_array (length list)) (level + 1)) (level + 1) 0 max_n
        | otherwise = 
                if isSorted (applyFlipSequence (lists_to_compare !! i) list)
                        then (lists_to_compare !! i)
                        else optflipSequence_helper list lists_to_compare level (i+1) max_n

applyFlipSequence :: [Int] -> [a] -> [a]
applyFlipSequence xs [] = []
applyFlipSequence [] ys = ys
applyFlipSequence xs ys
        | length ys == 1 = flip (head xs) ys
        | otherwise = applyFlipSequence (tail xs) (flip (head xs) ys)

permute :: [Int] -> Int -> [[Int]]
permute xs n = replicateM n xs

generate_array :: Int -> [Int]
generate_array 0 = []
generate_array n = [x | x <- [1..n]]

--ys is the flips from opt_seq
--xs is the inputed list
is_valid :: Ord a => [a] -> [Int] -> Bool
is_valid [] [] = True
is_valid xs ys = length ys > x
        where x = length (flipSequence xs)
