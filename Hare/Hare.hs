{-# LANGUAGE GADTs, DataKinds, KindSignatures, TupleSections, PolyKinds, TypeOperators, TypeFamilies, PartialTypeSignatures #-}
module Hare where
import Control.Monad
import Control.Applicative 
import HareMonad 

-- z5232937
-- [Char] is string

data RE :: * -> * where 
  Empty :: RE () -- Matches the empty string
  Fail :: RE a -- Matches no strings
  Char :: String -> RE Char  -- Matches a single character from the list
  Seq :: RE a -> RE b -> RE (a, b) -- Matches the two expressions in sequence
  Choose :: RE a -> RE a -> RE a -- Matches either the first, or the second
  Star :: RE a -> RE [a] -- Matches the expression zero or more times
  Action :: (a -> b) -> RE a -> RE b -- Arbitrary transformation (custom fmap)

--matching algorithm
match :: (Alternative f, Monad f) => RE a -> Hare f a
match (Char cs) = do
  -- if character, removes a character from the String state and returns it
  x <- readCharacter
  -- condition checker
  guard (x `elem` cs)
  pure x
-- match seq. of expressions
match (Seq a b) = (,) <$> match a <*> match b
match (Choose a b) =    
  match a <|> 
  match b
match (Star a) =
  -- fmap prepend a <*> match Star a == match fmap fmap prepend a Star a 
  prepend <$> match a <*> match (Star a) <|> pure []
  where 
  prepend start rest = start:rest
-- fmap RE functor on a 
match (Action f a) = f <$> match a 
-- failure condition
match Fail = failure
-- minimal value with context
match Empty = pure ()


matchAnywhere :: (Alternative f, Monad f) => RE a -> Hare f a
matchAnywhere re = match re <|> (readCharacter >> matchAnywhere re)

--regex matching operator
(=~) :: (Alternative f, Monad f) => String -> RE a -> f a 
(=~) = flip (hare . matchAnywhere)

-- future: check if it can be rewritten w applicatives + list comprehension
-- match type a + default cons functionality
infixr `cons`  
cons :: RE a -> RE [a] -> RE [a]
-- lambda to put in list
-- apply action to sequence 
cons start rest = Action (\(regx_start, regx_end) -> regx_start:regx_end) (Seq start rest)

-- minimum one occurence
plus :: RE a -> RE [a]
plus expr = expr `cons` (Star expr)
-- Action (\(regx_start, regx_end) -> regx_start:regx_end) (Seq start Star(rest))

-- similar to plus (but looking for string/no strinf)
-- exact string matching
string :: String -> RE String
string [] = Action (const []) Empty -- empty case
string (start : rest) = (Char [start]) `cons` (string rest) -- recursive checking

-- Choose for lists
choose :: [RE a] -> RE a
-- can't choose nothing
choose [] = Fail
choose (start:rest) = Choose start (choose rest)

-- expression repeated n times
rpt :: Int -> RE a -> RE [a]
rpt n expr = do 
  if n > 0
    then expr `cons` rpt (n-1) expr
    else Action (const []) Empty

-- 0 or 1 times
option :: RE a -> RE (Maybe a)
option re = error "'option' unimplemented"

-- rpt but on range
rptRange :: (Int, Int) -> RE a -> RE [a]
rptRange (x,y) re = error "'rptRange' unimplemented"

