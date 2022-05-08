module Ex05 where

import Text.Read (readMaybe)

data Token = Number Int | Operator (Int -> Int -> Int)

parseToken :: String -> Maybe Token
parseToken "+" = Just (Operator (+))
parseToken "-" = Just (Operator (-))
parseToken "/" = Just (Operator div)
parseToken "*" = Just (Operator (*))
parseToken str = fmap Number (readMaybe str)

tokenise :: String -> Maybe [Token]
tokenise = error "'tokenise' unimplemented"


newtype Calc a = C ([Int] -> Maybe ([Int], a))

-- push ele
push :: Int -> Calc ()
push x = C (\xs -> Just(x:xs, ()))


-- pop ele
pop :: Calc Int
pop = C popHead
  where 
    popHead :: [t] -> Maybe ([t], t)
    popHead []      = Nothing 
    popHead (head: rest) = Just (rest,head)



instance Functor Calc where
  fmap f (C sa) = C $ \s ->
      case sa s of 
        Nothing      -> Nothing
        Just (s', a) -> Just (s', f a)

instance Applicative Calc where
  pure x = C (\s -> Just (s,x))
  C sf <*> C sx = C $ \s -> 
      case sf s of 
          Nothing     -> Nothing
          Just (s',f) -> case sx s' of
              Nothing      -> Nothing
              Just (s'',x) -> Just (s'', f x)

instance Monad Calc where
  return = pure
  C sa >>= f = C $ \s -> 
      case sa s of 
          Nothing     -> Nothing
          Just (s',a) -> unwrapCalc (f a) s'
    where unwrapCalc (C a) = a

evaluate :: [Token] -> Calc Int
evaluate ts = error "'evaluate' unimplemented"

calculate :: String -> Maybe Int
calculate s = error "'calculate' unimplemented"

