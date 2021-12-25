-- |
module Lesson10 where
import Control.Applicative

--Functor
f :: [Integer]
f = fmap (+1) [1,2,3]

f' :: Maybe Integer
f' = fmap (+1) (Just 42)

-- <$> - fmap infix synonim
f'' :: [Integer]
f'' = (+1) <$> [1,2,3,4,5]

add :: Integer -> Integer -> Integer
add x y = x+y

--aplicatives
a :: Maybe Integer
a  = add <$> (Just 55) <*> (Just 44)

--bigExample
data Person = Person
  {
  name :: String,
  surname :: String,
  age :: Int
  }
  deriving (Show)

getAge :: IO Int
getAge = read <$> getLine


readPerson :: IO Person
readPerson = do
  name <- getLine
  surname <- getLine
  age <- getAge
  return $ Person name surname age

readPersonAppl :: IO Person
readPersonAppl = Person <$> getLine <*> getLine <*> getAge

--pure
p a = pure a


readPersonAppl' :: IO Person
readPersonAppl' = liftA3 Person getLine getLine getAge
