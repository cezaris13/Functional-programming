-- |

module Desktop.FunctionalProgrammingLessons.Lesson4 where
import Data.Char as C

data SomeType = SomeType Int String Int Int Float
  deriving (Show)

data SomeRecordType = SomeRecordType
  {
  field1 :: Int,
  field2 :: String,
  field3 :: Float,
  field4 :: Int
  }
  deriving (Show)
func :: SomeType -> Int
func (SomeType _ _ i1 i2 _) = i1 * i2


func2 :: SomeRecordType -> Int
func2 (SomeRecordType i1 _ _ _) = i1


func2' :: SomeRecordType -> Int
func2' a = field1 a * field4 a

sr :: SomeRecordType
sr = SomeRecordType 1 "2" 3.0 4

srUpdate :: SomeRecordType
srUpdate = sr {field1 = 42}




add1 :: Int -> Int
add1 a = a + 1


add1AndShow :: Int -> String
add1AndShow a = show $ add1 a



-- compose functions

add1AndShow' :: Int -> String
add1AndShow' = show . add1


boo :: [Int] -> Bool -> Int
boo a b=
  case a of
    [] -> if b
      then 1
      else -1
    _ -> 12

-- guards
g :: Int -> String
g a
  | a < 10 = "less than 10"
  | otherwise = "omm"


-- list comprehensions
--[(show i,j) | i <- [1..4],j <- [1..5],i/=3]

parseInt :: String -> Either String (Integer,String)
parseInt str
  | [] == numberAsString = Left "empty number"
  | otherwise = Right(read numberAsString, leftovers)
  where
    numberAsString = takeWhile C.isDigit str
    leftovers = drop (length numberAsString) str
