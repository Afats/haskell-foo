{-# LANGUAGE GADTs, DataKinds, KindSignatures, TupleSections, PolyKinds, TypeOperators, TypeFamilies, PartialTypeSignatures #-}
module Hare where
import Control.Monad
import Control.Applicative 
import HareMonad 

data RE :: * -> * where 
  Empty :: RE ()
  Fail :: RE a
  Char :: [Char] -> RE Char
  Seq :: RE a -> RE b -> RE (a,b)
  Choose :: RE a -> RE a -> RE a
  Star :: RE a -> RE [a]
  Action :: (a -> b) -> RE a -> RE b
  
match :: (Alternative f, Monad f) => RE a -> Hare f a
match Empty = pure ()
match Fail = failure
match (Char c) = do
    x <- readCharacter
    guard (x `elem` c)
    pure x
match (Seq a b) = (,) <$> match a <*> match b    
match (Choose a b) = match a <|> match b
match (Star a) = (:) <$> match a <*> match (Star a) <|> pure []
match (Action f a) = fmap f (match a)


matchAnywhere :: (Alternative f, Monad f) => RE a -> Hare f a
matchAnywhere re = match re <|> (readCharacter >> matchAnywhere re)

(=~) :: (Alternative f, Monad f) => String -> RE a -> f a 
(=~) = flip (hare . matchAnywhere)

-- to convert tuple to list
tupleToList :: (a, [a]) -> [a]
tupleToList (x, xs) = (x:xs)

infixr `cons`  
cons :: RE a -> RE [a] -> RE [a]
cons x xs = Action tupleToList (Seq x xs)

string :: String -> RE String
string [] = Action (const []) Empty
string (x:xs) = Char (x:xs) `cons` string xs

rpt :: Int -> RE a -> RE [a]
rpt n re | n <= 0 = Action (const []) Empty
         | otherwise = re `cons` rpt (n-1) re


rptRange :: (Int, Int) -> RE a -> RE [a]
rptRange (x,y) re | x == y = rpt y re
                  | otherwise = choose [(rpt y re)] `cons` rptRange (x, y-1) re

option :: RE a -> RE (Maybe a)
option re = Choose (Action Just re) (Action (const Nothing) Empty)
    

plus :: RE a -> RE [a]
plus re = Action tupleToList (Seq re (Star re))

choose :: [RE a] -> RE a
choose [] = Fail
choose (x:xs) = Choose x (choose xs)