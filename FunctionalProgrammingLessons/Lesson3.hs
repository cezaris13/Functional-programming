-- |

module Desktop.FunctionalProgrammingLessons.Lesson3 where
import GHC.Show as Show
import Data.Char as C
data FTypes = A | B | C

instance Show FTypes where
  show A = "Water"
  show B = "Foam"
  show C = "CO2"



data Extinguisher = Extinguisher
  {
  volume :: Int,
  typeOfExtinguisher :: FTypes
  }
instance Show Extinguisher where
  show (Extinguisher v t) = "Fire extinguisher;\n\tcapacity:" ++ show v ++ "\n\ttype:" ++ show t
printTypeOfExtinguisher :: Extinguisher -> FTypes
printTypeOfExtinguisher ext = (typeOfExtinguisher ext)

newtype Extinguisher1 = Extinguisher1 Int


type Map a = [(String,a)]

emptyMap :: Map a
emptyMap = []

putToMap :: String -> a -> Map a -> Map a
putToMap key value map = (key,value) : map

findInMap :: String -> Map a -> Maybe a
findInMap _ [] = Nothing
findInMap key (head:map)
  | key == (fst head) = Just (snd head)
  | otherwise = findInMap key map

-- sring parsing
parseInt :: String -> (Maybe Integer,String)
parseInt str
  | [] == numberAsString = (Nothing,leftovers)
  | otherwise = (Just $ read numberAsString, leftovers)
  where
    numberAsString = takeWhile C.isDigit str
    leftovers = drop (length numberAsString) str

-- parseInt :: String -> Integer
-- parseInt str = read $
--   takeWhile C.isDigit str
-- parseInt = read
parseListOfInts :: String -> ([Integer],String)
parseListOfInts [] = ([],[])
parseListOfInts ('[':inputData) = parseNumbers inputData
parseListOfInts _ = error "IllegalList"

-- parseNumbers :: String -> [Integer]
-- parseNumbers "]" = []
-- parseNumbers (',':tail) = parseNumbers tail
-- parseNumbers str = fst a : parseNumbers (snd a)
--   where a = parseInt str

parseNumbers :: String -> ([Integer],String)
parseNumbers s =
  let (value,rest) = parseInt s
    in case rest of
      (']':rest1) -> (convertMaybeToInt value,rest1)
      _ -> readCommaNumber rest $ convertMaybeToInt value

convertMaybeToInt :: (Integral a) => Maybe a -> [a]
convertMaybeToInt (Just a) = [a]
convertMaybeToInt Nothing = []

readCommaNumber :: String -> [Integer] -> ([Integer],String)
readCommaNumber (',':s) acc =
  let (value,rest) = parseInt s
  in case rest of
    (']':rest2) -> (acc ++ convertMaybeToInt value,rest2)
    _ -> readCommaNumber rest (acc ++ convertMaybeToInt value)
readCommaNumber _ _ = error "illegal list; comma expected"
