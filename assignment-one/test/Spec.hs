import Lib 
import Prelude hiding (flip)
import Test.Hspec
import Test.Hspec.QuickCheck


same_flip :: Eq a => [a] -> Int -> Bool
same_flip xs i = if flip i (flip i xs) == xs then True else False

sorted_flips :: Ord a => [a] -> Bool
sorted_flips xs = if flipSequence xs == [] then True else False

length_comparison :: Ord a =>[a] -> Bool
length_comparison xs = if length (flipSequence xs) <= (2*(length xs) - 3) then True else False

length_comparison2 :: Ord a => [a] -> Bool
length_comparison2 xs = if length (optFlipSequence xs) <= length (flipSequence xs) then True else False 

optimum_flipSequence :: Ord a => [a] -> Bool 
optimum_flipSequence xs = if length (optFlipSequence xs) <= (18 `div` 11) * (length xs) then True else False

is_sorted_test :: Ord a => [a] -> Bool
is_sorted_test xs = if isSorted xs == True then True else False

is_not_sorted_test :: Ord a => [a] -> Bool
is_not_sorted_test xs = if isSorted xs == False then True else False

apply_flip_test :: Ord a => [a] -> Bool
apply_flip_test xs = if isSorted (applyFlipSequence (flipSequence xs) xs) == True then True else False

apply_flip_test_opt :: Ord a => [a] -> Bool
apply_flip_test_opt xs = if isSorted (applyFlipSequence (optFlipSequence xs) xs) == True then True else False

main :: IO ()
main = hspec $ do
    describe "same_flip" $ do
        it "List stays the same on double flip" $
            same_flip [1,2,3]  1 `shouldBe` True

    describe "sorted_flips" $ do
        it "Sorted List must return empty flip sequence list" $
            sorted_flips [1,2,3] `shouldBe` True
    
    describe "length comparison" $ do
        it "Length of flipSequence list must lie in bounds" $
            length_comparison [-2, 1, 4, 4, 9] `shouldBe` True

    describe "length comparison between flipSequence and Optimum flip sequence" $ do
        it "Length of flipSequence must be > optFlipsequence" $
            length_comparison2 [-2, 1, 4, 4, 9] `shouldBe` True
    
    describe "Bound comparison on Optimum flip sequence" $ do
        it "Length of flipSequence must be > optFlipsequence" $
            optimum_flipSequence [-2, 1, 4, 4, 9] `shouldBe` True
    
    describe "Sorted List must return true" $ do
        it "Sorted list must return true" $
            is_sorted_test [1,2,3,4,5] `shouldBe` True

    describe "Not Sorted List must return false" $ do
        it "Sorted list must return true" $
            is_not_sorted_test [1,2,3,5,4]  `shouldBe` True

    describe "apply_flip sequence" $ do
        it "apply flip sequence leads to a sorted list" $
            apply_flip_test [-2, 1, 4, 4, 9]  `shouldBe` True
    
    describe "apply_flip sequence" $ do
        it "apply optimum flip sequence leads to a sorted list" $
            apply_flip_test_opt [-2, 1, 4, 4, 9]  `shouldBe` True