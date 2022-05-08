{-# LANGUAGE FlexibleContexts #-}
module Ex04 where
import Text.Read (readMaybe)
import System.IO
import Data.Char
import System.Environment
import Control.Monad.State
import System.Random
import Test.QuickCheck

--Product of ints in file
fileProduct :: IO ()
fileProduct = do
    files <- getArgs 
    deets <- readFile (files !! 0)
    writeFile (files !! 1) $ show $ product $ map (read . filter isNumber) $ lines deets

--Filter only letters in file and output to file
onlyAlphabetic :: FilePath -> FilePath -> IO ()
onlyAlphabetic input output = do 
    deets <- readFile input
    writeFile output $ filter isLetter $ deets


data Player m = Player { guess :: m Int
                       , wrong :: Answer -> m ()
                       }
data Answer = Lower | Higher

guessingGame :: (Monad m) => Int -> Int -> Player m -> m Bool
guessingGame x n p = go n
  where
   go 0 = pure False
   go n = do
     x' <- guess p
     case compare x x' of
       LT -> wrong p Lower  >> go (n-1)
       GT -> wrong p Higher >> go (n-1)
       EQ -> pure True

human :: Player IO
human = Player { guess = guess, wrong = wrong }
  where
    guess = do
      putStrLn "Enter a number (1-100):"
      x <- getLine
      case readMaybe x of
        Nothing -> guess
        Just i  -> pure i

    wrong Lower  = putStrLn "Lower!"
    wrong Higher = putStrLn "Higher!"

play :: IO ()
play = do
  x <- randomRIO (1,100)
  b <- guessingGame x 5 human
  putStrLn (if b then "You got it!" else "You ran out of guesses!")


midpoint :: Int -> Int -> Int
midpoint lo hi | lo <= hi  = lo + div (hi - lo) 2
               | otherwise = midpoint hi lo

--AI that that does binary search guessing
ai :: Player (State (Int,Int))
ai = Player { guess = guess, wrong = wrong }
  where 
    wrong Higher = do 
      (lowNum, highNum) <- get
      put (midpoint lowNum highNum + 1 , highNum)

    wrong Lower  = do 
      (lowNum, highNum) <- get
      put (lowNum, midpoint lowNum highNum - 1)

    guess = do 
      (lowNum, highNum) <- get
      -- pure is return
      pure $ midpoint lowNum highNum



prop_basic (Positive n) = forAll (choose (1,n)) $ \x -> evalState (guessingGame x n ai) (1,n)

prop_optimality (Positive n) = forAll (choose (1,n)) $ \x -> evalState (guessingGame x (bound n) ai) (1,n)
  where bound n = ceiling (logBase 2 (fromIntegral n)) + 1


