{-# LANGUAGE FlexibleInstances #-}

module Lib2 where
import Lib1 (createRenders, iterLoop, RenderData (RenderData), appendBlocks)
import qualified Data.Char as C (isDigit)
import qualified Data.List as L (find,delete)
import qualified Data.Maybe as M (isJust)
import Control.Applicative

data InitData = InitData
  { gameWidth :: Int,
    gameHeight :: Int
  }
  deriving (Show)

data JsonLike
  = JsonLikeInteger Int
  | JsonLikeString String
  | JsonLikeObject [(String, JsonLike)]
  | JsonLikeList [JsonLike]
  | JsonLikeNull
  deriving (Show)

-- | Change State the way need but please keep
--  the name of the type, i.e. "State"
data State = ErrorMessage String | State [RenderData] InitData
  deriving (Show)

playerChar :: Char
playerChar = 'i'

parseJsonMessage :: String -> Either String JsonLike
parseJsonMessage a =
  let parseResult = runParser parseJsonLike a
    in case parseResult of
      Right a -> Right $ fst a
      Left err -> Left err

-- | Is called in a very beginning of a game
init ::
  -- | Initial data of the game
  InitData ->
  -- | First json message before any moves performed
  JsonLike ->
  -- | An initial state based on initial data and first message
  State
init i j = state j i
--State j i

-- | Is called after every user interaction (key pressed)
update ::
  -- | Current state
  State ->
  -- | Json message from server
  JsonLike ->
  -- | A new state, probably based on the message arrived
  State
update (State oldBlocks i) j = do
  let (State newBlocks tempInitData) = state j i
  let finalBlocks = appendBlocks newBlocks oldBlocks
  let afterExplosion = triggerBombs finalBlocks newBlocks
  State afterExplosion tempInitData

-- Input: oldBlocks -> newBlocks
-- Returns: oldBlocks with cleared explosions and if oldBlocks contained bombs and newBlocks did not. Those bombs exploded.
triggerBombs :: [RenderData] -> [RenderData] -> [RenderData]
triggerBombs oldBlocks newBlocks = do
  let afterExplosion = removeExplosion oldBlocks -- clear explosions from map
  let wasBomb = findBomb afterExplosion
  let isBomb = findBomb newBlocks
  -- if there was a bomb and no longer is, it must explode
  let triggerExplosion = if wasBomb /= (-1, -1) && isBomb == (-1, -1)
            then do
              let allExplosionCoordinates = explosionCoordinates wasBomb
              explodeBomb afterExplosion allExplosionCoordinates
            else
              afterExplosion -- perhaps this should be new blocks
  triggerExplosion

-- Input
-- [RenderData {gameBlock = 'i', y = 1, x = 1}, RenderData {gameBlock = 'b', y = 2, x = 2}]
-- Output
-- (2, 2)
findBomb :: [RenderData] -> (Int, Int)
findBomb [] = (-1, -1)
findBomb (RenderData symbol y x :arrs)
  | symbol == 'b' = (y, x)
  | otherwise = findBomb arrs

-- creates all explosion coordinates
explosionCoordinates :: (Int, Int) -> [(Int, Int)]
explosionCoordinates (bombY, bombX) = [(bombY, bombX), (bombY, bombX - 1), (bombY, bombX + 1), (bombY - 1, bombX), (bombY + 1, bombX)]

-- Input: [RenderData {gameBlock = 'x', y = 2, x = 1}, RenderData {gameBlock = 'b', y = 2, x = 2}, RenderData {gameBlock = '#', y = 3, x = 3}], [(2,2), (2,1), (2,3)]
-- Output: [RenderData {gameBlock = '#', y = 3, x = 3}]
explodeBomb :: [RenderData] -> [(Int, Int)] -> [RenderData]
explodeBomb [] [] = []
explodeBomb [] ((y, x): leftExplosions) = RenderData '*' y x : explodeBomb [] leftExplosions
explodeBomb (RenderData symbol y x :arrs) allExplosions
  | elem (y, x) allExplosions && canExplode symbol = do
      let leftExplosions = L.delete (y, x) allExplosions
      RenderData '*' y x : explodeBomb arrs leftExplosions
  | otherwise = RenderData symbol y x : explodeBomb arrs allExplosions

-- Clears all RenderData with '*' from the array
removeExplosion :: [RenderData] -> [RenderData]
removeExplosion [] = []
removeExplosion (RenderData symbol y x :arrs)
  | symbol == '*' = removeExplosion arrs
  | otherwise = RenderData symbol y x : removeExplosion arrs

explodable :: [Char]
explodable = [playerChar, 'x', 'b']

canExplode :: Char -> Bool
canExplode c = c `elem` explodable

-- | Renders the current state
render ::
  -- | A state to be rendered
  State ->
  -- | A string which represents the state. The String is rendered from the upper left corner of terminal.
  String
render (ErrorMessage _) = "Error Message"
render (State json (InitData width height)) = iterLoop json width 0 height
-- render (ErrorMessage message) = message


state :: JsonLike -> InitData -> State
state json (InitData width height) = do
    let objectList = jsonLikeToListOfObjects json
    let coordinateArrays =  map snd objectList
    let renders = createRenders coordinateArrays [playerChar, 'x', 'H', 'O', '#', 'b']
    State renders (InitData width height)

-----------------------------------
-------------objects---------------
gameObjects :: [(String,Char)]
gameObjects = [
  ("bomb",'b'),
  ("bombermans",'i'),
  ("bricks",'#'),
  ("gates",'c'),
  ("ghosts",'g'),
  ("wall",'=')]

-----------------------------------
-- Geting the data from JsonLike---
jsonLikeToListOfObjects :: JsonLike -> [(String,[(Int,Int)])]
jsonLikeToListOfObjects input  = jsonLikeToListOfObjects' (jsonLikeToObject input) []

jsonLikeToListOfObjects' :: [(String,JsonLike)] -> [(String,[(Int,Int)])]-> [(String,[(Int,Int)])]
jsonLikeToListOfObjects' [] arr = arr
jsonLikeToListOfObjects' (h:t) arr
  | fst h == "bomb" && isJsonNull (snd h) = jsonLikeToListOfObjects' t ((fst h,[]):arr)
  | fst h == "bomb" && not (isJsonNull (snd h)) = jsonLikeToListOfObjects' t ((fst h,jsonLikeListToListCoordinates $ snd h):arr)
  | M.isJust (L.find (\x -> fst x == fst h) gameObjects) && isJsonNull (snd h) = jsonLikeToListOfObjects' t ((fst h,[]):arr)
  | M.isJust (L.find (\x -> fst x == fst h) gameObjects) = jsonLikeToListOfObjects' t ((fst h,jsonLikeListToListCoordinates (snd h)):arr)
--   | M.isJust (L.find (\x -> fst x == fst h) gameObjects) = jsonLikeToListOfObjects' t ((fst h,parseHeadTail (jsonLikeToObject $ snd h) []):arr)
  | fst h == "bomb_surrounding" && isJsonNull (snd h) = jsonLikeToListOfObjects' t ((fst h,[]):arr)
  | fst h == "bomb_surrounding" && not (isJsonNull (snd h)) = jsonLikeToListOfObjects' t (reverse (jsonLikeToListOfObjects $ snd h) ++ arr)
  | fst h == "surrounding" = jsonLikeToListOfObjects' t (reverse (jsonLikeToListOfObjects $ snd h) ++ arr)
  | otherwise = arr

parseHeadTail :: [(String,JsonLike)] -> [(Int,Int)] -> [(Int,Int)]
parseHeadTail [] arr = arr
parseHeadTail (h:list) arr
  | fst h == "head" && isJsonNull (snd h) = arr
  | fst h == "head" = parseHeadTail list (jsonLikeToCoordinates (snd h):arr)
  | fst h == "tail" = parseHeadTail (jsonLikeToObject $ snd h) arr
  | otherwise  = arr

jsonLikeToCoordinates :: JsonLike -> (Int,Int)
jsonLikeToCoordinates input
  | null parsedList = (-1,-1)
  | length parsedList /= 2 = (-1,-1)
  | otherwise  = (x,y)
  where
    parsedList = reverse $ jsonLikeToList input
    x = head parsedList
    y = head $ tail parsedList

jsonLikeListToListCoordinates :: JsonLike -> [(Int,Int)]
jsonLikeListToListCoordinates (JsonLikeList []) = []
jsonLikeListToListCoordinates (JsonLikeList input) =
  case head input of
    JsonLikeList b -> map jsonLikeToCoordinates input

jsonLikeToList :: JsonLike -> [Int]
jsonLikeToList input  =
  case input of
    JsonLikeList a -> foldl (\acc x -> jsonLikeToInt x:acc) [] a
    _ -> []

isJsonNull :: JsonLike -> Bool
isJsonNull input =
  case input of
    JsonLikeNull -> True
    _ -> False

jsonLikeToInt :: JsonLike -> Int
jsonLikeToInt input =
  case input of
    JsonLikeInteger a -> a
    _ -> 0

jsonLikeToString :: JsonLike -> String
jsonLikeToString input =
  case input of
    JsonLikeString a -> a
    _ -> ""

jsonLikeToObject :: JsonLike -> [(String,JsonLike)]
jsonLikeToObject input =
  case input of
    JsonLikeObject a -> a
    _ -> [("",JsonLikeNull)]

--------------------------------------------------
--monadic approach
newtype Parser a = Parser
  {
    runParser :: String -> Either String (a, String)
  }

instance Functor Parser where
  fmap function (Parser x) =
    Parser (\input -> do
        (x',leftovers) <- x input
        return (function x', leftovers))

instance Applicative Parser where
  pure x = Parser (\input -> Right(x,input))
  (Parser p1) <*> (Parser p2) =
      Parser (\input -> do
          (x,leftovers) <- p1 input
          (x',leftovers') <- p2 leftovers
          return (x x',leftovers'))

instance Alternative (Either String) where
    empty = Left "empty"
    Left _ <|> b = b
    a <|> _ = a

instance Alternative Parser where
  empty = Parser Left
  (Parser p1) <|> (Parser p2) = Parser (\input -> p1 input <|> p2 input)

parseChar :: Char -> Parser Char
parseChar chr = Parser f
  where
    f (x:xs)
      | x == chr = Right (x,xs)
      | otherwise = Left ("Expected: " ++ show chr ++ "but got: " ++ show x)
    f [] = Left "Empty list"

parseString :: String -> Parser String
parseString str = Parser(\input->
        case runParser (traverse parseChar str) input of
          Left err -> Left ("Error while parsing string; " ++ err)
          result -> result)

takeWhileP :: (Char -> Bool) -> Parser String
takeWhileP find = Parser (Right . span find)

-- runParser (parseJsonNull) "nulla"
-- Right (JsonLikeNull,"a")

parseJsonNull :: Parser JsonLike
parseJsonNull = JsonLikeNull <$ parseString "null"

-- runParser (parseJsonString) "\"labas\"aaa"
-- Right (JsonLikeString "labas","aaa")

-- runParser (parseJsonString) "a\"labas\"aaa"
-- Left "given value is not a String"

parseJsonString :: Parser JsonLike
parseJsonString = JsonLikeString <$> (parseChar '"' *> takeWhileP (/= '"') <* parseChar '"')

notNull :: Parser [a] -> Parser [a]
notNull (Parser x) =
    Parser(\input -> do
             (parsed,leftovers) <- x input
             if null parsed
                then Left "Error: empty Integer"
                else Right(parsed, leftovers))

-- runParser (parseJsonInteger) "123a"
-- Right (JsonLikeInteger 123,"a")

-- runParser (parseJsonInteger) "a1"
-- Left "given value is not Integer"

parseJsonInteger :: Parser JsonLike
parseJsonInteger = JsonLikeInteger . read <$> notNull (takeWhileP C.isDigit)

-- runParser (parseJsonList) "[1,2,3,4,5,6]"
-- Right (JsonLikeList [JsonLikeInteger 1,JsonLikeInteger 2,JsonLikeInteger 3,JsonLikeInteger 4,JsonLikeInteger 5,JsonLikeInteger 6],"")
-- runParser (parseJsonList) "1,2,3,4,5,6]"
-- Left "invalid array"

parseJsonList :: Parser JsonLike
parseJsonList = fmap JsonLikeList (parseChar '[' *> takeWhiteSpace *> splitByComma parseJsonLike <* takeWhiteSpace <* parseChar ']')

splitByP :: Parser a -> Parser b -> Parser [b]
splitByP sep elements = (fmap (:) elements <*> many (sep *> elements)) <|> pure []

takeWhiteSpace :: Parser String
takeWhiteSpace = takeWhileP (==' ')

-- runParser (parseObject) "{\"objectA\":12,\"objectB\":[1,2,3,4,5,6]}"
-- Right (JsonLikeObject [("objectA",JsonLikeInteger 12),("objectB",JsonLikeList [JsonLikeInteger 1,JsonLikeInteger 2,JsonLikeInteger 3,JsonLikeInteger 4,JsonLikeInteger 5,JsonLikeInteger 6])],"")

parseJsonObject :: Parser JsonLike
parseJsonObject = JsonLikeObject <$> (parseChar '{' *> takeWhiteSpace *>  splitByComma pair <* takeWhiteSpace <* parseChar '}')
                  where pair = (,) <$> (parseChar '"' *> takeWhileP (/='"') <* parseChar '"' <* takeWhiteSpace <* parseChar ':' <* takeWhiteSpace) <*> parseJsonLike

splitByComma :: Parser a -> Parser [a]
splitByComma = splitByP (takeWhiteSpace *> parseChar ',' <* takeWhiteSpace)

parseJsonLike :: Parser JsonLike
parseJsonLike = parseJsonNull <|> parseJsonString <|> parseJsonInteger <|> parseJsonList <|> parseJsonObject
