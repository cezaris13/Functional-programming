module Lesson2 where

-- :t show
-- show a => a-> string

str :: String -> String
str a = read (show a)::String



list :: [Integer]
list = [1,2,3,4]

incByOne :: [Integer] -> [Integer]
incByOne [] = []
incByOne (a:b) = (a+1): incByOne b

incByN :: [Integer] -> Integer -> [Integer]
incByN [] _ = []
incByN (a:b) n = (a+n): incByN b n



incByOneTailedRecursion :: [Integer] -> [Integer]
incByOneTailedRecursion l  = incByOne' l []
  where
    incByOne' [] acc = reverse acc
    incByOne' (h:t) acc = incByOne' t ((h+1):acc)




incByN' :: [Integer] -> Integer -> [Integer]
incByN' l i = incByN'' l i []
  where
    incByN'' [] _ acc = reverse acc
    incByN'' (h : t) n acc = incByN'' t n ((h+n) : acc)




infant :: Integer -> Bool
infant 0 = True
infant _ = False


-- ADT
-- gesintuvas
--- tipas ir turis
-- data FTypes = A | B | C
--   deriving (Show)
-- data Extinguisher = Extinguisher Int FTypes
--   deriving (Show)
data FTypes = A | B | C
  deriving (Show)
data Extinguisher = Extinguisher
  {
  volume :: Int,
  typeOfExtinguisher :: FTypes
  }
  deriving (Show)


printTypeOfExtinguisher :: Extinguisher -> FTypes
printTypeOfExtinguisher ext = (typeOfExtinguisher ext)

newtype Extinguisher1 = Extinguisher1 Int
