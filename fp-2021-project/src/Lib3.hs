{-# LANGUAGE FlexibleInstances #-}

module Lib3 where

import Lib1 (createRenders, iterLoop, RenderData (RenderData), appendBlocks, gameBlock,ghostChar)

import Data.Either as E (Either (..), either, isLeft, fromRight)
import Data.List as L (lookup)
import Data.Maybe
import Lib2
    ( parseJsonMessage,
      jsonLikeToListOfObjects,
      playerChar,
      InitData(InitData),
      JsonLike(..),
      State(..) )
import GHC.Generics
import Data.List
import qualified Data.Either as Either
import GHC.TypeLits
-- Keep this type as is
type GameId = String

-- Keep these classes as is:
-- You will want to implement instances for them
class ToJsonLike a where
  toJsonLike :: a -> Either String JsonLike

class FromJsonLike a where
  fromJsonLike :: JsonLike -> Either String a

class ContainsGameId a where
  gameId :: a -> GameId

-- Further it is your code: you can change whatever you wish

-- Converts a JsonLike into a String: renders json
instance FromJsonLike String where
    fromJsonLike o@(JsonLikeList list) = do
      let ans = constructRules list
      E.Right ans
    fromJsonLike o@(JsonLikeString input) = do
      E.Right input
    fromJsonLike v = E.Left $ "Unexpected value: " ++ show v

constructRules :: [JsonLike] -> String
constructRules [] = "null"
constructRules list = do
  let oneCommand = head list
  let oneCommandAsString = handle (fromJsonLike oneCommand) :: String
  if "MoveBomberman" `isInfixOf` oneCommandAsString
    then do
      let var1 = split ' ' oneCommandAsString
      "{\"command\":{\"direction\":\"" ++ var1 !! 1 ++ "\",\"name\":\"" ++ head var1 ++ "\"},\"additional\":" ++ constructRules (tail list) ++ "}"
    else "{\"command\":{\"name\":\"" ++ oneCommandAsString ++ "\"},\"additional\":" ++ constructRules (tail list) ++ "}"

split :: Char -> String -> [String]
split c xs = case break (==c) xs of
  (ls, "") -> [ls]
  (ls, x:rs) -> ls : split c rs

-- Acts as a parser from a String
instance ToJsonLike String where
  toJsonLike = Lib2.parseJsonMessage
data NewGame = NewGame
  { uuid :: GameId ,
    height :: Int,
    width :: Int
  }
   deriving ( Show)

instance ContainsGameId NewGame where
  gameId (NewGame gid _ _) = gid

findStringObject :: [(String , JsonLike )] -> String -> Either String String
findStringObject object name =
  case L.lookup name object of
      Nothing -> E.Left $ "no uuid field in " ++ show object
      Just (JsonLikeString s) -> E.Right s
      _ -> E.Left "Invalid value type"

findIntObject :: [(String , JsonLike)] -> String -> Either String Int
findIntObject object name =
  case L.lookup name object of
      Nothing -> E.Left $ "no uuid field in " ++ show object
      Just (JsonLikeInteger s) -> E.Right s
      _ -> E.Left "Invalid value type"

instance FromJsonLike NewGame where
  fromJsonLike o@(JsonLikeObject m)
                                    | E.isLeft uuid = E.Left $ "no uuid field in " ++ show o
                                    | E.isLeft height = E.Left $ "no height field in " ++ show o
                                    | E.isLeft width = E.Left $ "no width field in " ++ show o
                                    | otherwise = E.Right $ NewGame gameUuid gameHeight gameWidth
    where
      uuid = findStringObject m "uuid"
      height = findIntObject m "height"
      width = findIntObject m "width"
      gameUuid = E.fromRight "error" uuid
      gameHeight = E.fromRight 0 height
      gameWidth  = E.fromRight 0 width
  fromJsonLike o@(JsonLikeString input) = do
    let s1 = removeUntillColon input
    let height = saveUntilComma s1

    let s2 = saveUntilComma (reverse input)
    let s3 = removeUntillColon (reverse s2)
    let width = take 2 s3

    let x = removeUntillComma (reverse input)
    let y = removeUntillComma (reverse (tail x))

    E.Right $ NewGame (drop 8 y) (read height) (read width)
  fromJsonLike v = E.Left $ "Unexpected value: " ++ show v

removeUntillComma :: String -> String
removeUntillComma input = do
  let nextSymbol = head input
  case nextSymbol of
    ',' -> tail input
    _ -> removeUntillComma (tail input)

data Direction = Right | Left | Up | Down
  deriving (Show)

data Command
  = MoveBomberman Direction
  | FetchSurrounding
  | PlantBomb
  | FetchBombStatus
  | FetchBombSurrounding
  deriving (Show)

data Commands = Commands
  { command :: Command,
    additional :: Maybe Commands
  }
  deriving (Show)

instance ToJsonLike Commands where
  toJsonLike input = do
    let list = constructCommandList input
    let jsonLikeStrings = map JsonLikeString list
    E.Right (JsonLikeList jsonLikeStrings)

constructCommandList :: Commands -> [String]
constructCommandList commands = do
  let oneCommand = command commands
  let oneCommandAsString = show oneCommand
  case additional commands of
    Just value -> oneCommandAsString : constructCommandList value
    Nothing    -> [oneCommandAsString]

instance FromJsonLike Commands where
  fromJsonLike _ = E.Left "Not used"

data CommandsResponse = CommandsResponse
  {
    response :: JsonLike
  }
  deriving (Show)

instance ToJsonLike CommandsResponse where
  toJsonLike _ = E.Right JsonLikeNull

instance FromJsonLike CommandsResponse where
  fromJsonLike input = do
    E.Right (CommandsResponse input)

handle :: Either String a -> a
handle = E.either error id

removeUntillColon :: String -> String
removeUntillColon input = do
  let nextSymbol = head input
  case nextSymbol of
    ':' -> tail input
    _ -> removeUntillColon (tail input)

saveUntilComma :: [Char] -> [Char]
saveUntilComma = saveUntilComma' []

saveUntilComma' :: [Char] -> [Char] -> [Char]
saveUntilComma' saved input = do
  let keep = head input
  case keep of
    ',' -> saved
    _ -> saveUntilComma' (saved ++ [keep]) (tail input)

removeItem :: (String, [Int]) -> [(String , [(Int, Int)])] -> [(String , [(Int, Int)])]
removeItem _ []                 = []
removeItem (check_string, check_value) ((value_string, value ):ys) | check_string == value_string       = removeItem (check_string, check_value ) ys
                                                                   | otherwise = (value_string , value) : removeItem (check_string, check_value ) ys

findBombInObjects :: (Num a, Num b) => [(String, [(a, b)])] -> [(a, b)]
findBombInObjects [] = [(-1, -1)]
findBombInObjects ((string, value) :xs)  = if string == "bomb"
                                            then value
                                            else findBombInObjects xs

update :: State -> JsonLike -> Bool -> State
update (State oldBlocks i) j bombPlanted = do
  let newState = Lib3.state j i bombPlanted--State j i
    in case newState of
      ErrorMessage err -> ErrorMessage err
      (State newBlocks tempInitData) -> do
        let finalBlocks = Lib3.appendBlocks newBlocks oldBlocks
        -- let afterExplosion = triggerBombs finalBlocks newBlocks
        State finalBlocks tempInitData


-- state :: JsonLike -> InitData -> State
-- state json (InitData width height) = do
--     let objectList = jsonLikeToListOfObjects json
--     let coordinateArrays =  map (\x-> snd x) objectList
--     let renders = createRenders coordinateArrays [playerChar, 'x', 'H', 'O', '#', 'b']
--     State renders (InitData width height)

state :: JsonLike -> InitData -> Bool -> State
state json (InitData width height) bombPlanted = do
  -- let jsonAsString = jsonLikeToString json
  let objectList = jsonLikeToListOfObjects json
  let parsedArray = removeItem ("bomb_surrounding",[]) objectList
  let coordinateArrays =  map snd parsedArray
  let renders = createRenders coordinateArrays [playerChar, 'x', 'H', 'O', '#', 'b']
  if bombPlanted
    then do
      let bomb = findBombInObjects objectList
      let newRenders
            | null bomb = renders
            | otherwise = renders ++ [uncurry (RenderData 'b') (head bomb)]
      State newRenders (InitData width height)
    else State renders (InitData width height)

--expected format:
-- [RenderData {gameBlock = 'i', y = 1, x = 1},RenderData {gameBlock = 'x', y = 1, x = 6}]
-- [RenderData {gameBlock = 'i', y = 1, x = 1},RenderData {gameBlock = 'x', y = 1, x = 7}]
-- [RenderData {gameBlock = 'i', y = 1, x = 1},RenderData {gameBlock = 'x', y = 1, x = 6}]
--returns [RenderData {gameBlock = 'i', y = 1, x = 1},RenderData {gameBlock = 'x', y = 1, x = 6},RenderData {gameBlock = 'x', y = 1, x = 7}]
appendBlocks :: [RenderData] -> [RenderData] -> [RenderData]
appendBlocks renderDataList [] = renderDataList
appendBlocks renderDataList (oldListHead:oldListTail) = do
  if not (findEquals renderDataList oldListHead) && gameBlock oldListHead /= playerChar && gameBlock oldListHead /= ghostChar && notDuplicateBomb renderDataList oldListHead
    then Lib3.appendBlocks (oldListHead : renderDataList) oldListTail
    else Lib3.appendBlocks renderDataList oldListTail

notDuplicateBomb :: [RenderData] -> RenderData -> Bool
notDuplicateBomb [] _ = True
notDuplicateBomb (RenderData symbol y x :arrs) (RenderData checkSymbol checkY checkX)
  | symbol == 'b' &&  checkSymbol == 'b' = False
  | otherwise = notDuplicateBomb arrs (RenderData checkSymbol checkY checkX)
--expected format:
-- [RenderData {gameBlock = 'i', y = 1, x = 1},RenderData {gameBlock = 'x', y = 1, x = 6}]
-- RenderData {gameBlock = 'x', y = 1, x = 7}
--returns Bool False (if there is no such block in state) or Bool True (if there already exists such a block)
findEquals :: [RenderData] -> RenderData -> Bool
findEquals [] _ = False
findEquals (RenderData symbol y x :arrs) (RenderData checkSymbol checkY checkX)
  | symbol == checkSymbol && y == checkY && x == checkX = True
  | otherwise = findEquals arrs (RenderData checkSymbol checkY checkX)

isBombNull :: [(String, [(Int, Int)])] -> Bool
isBombNull [] = True
isBombNull ((name, arr):xs) = do
  if name == "bomb"
    then null arr
    else isBombNull xs

getRenderData ::  Lib2.State -> [RenderData]
getRenderData (ErrorMessage message) = []
getRenderData (Lib2.State renderData tempInitData) = renderData

getInitData :: Lib2.State -> Lib2.InitData
getInitData (ErrorMessage message) = Lib2.InitData 0 0
getInitData (Lib2.State renderData tempInitData) = tempInitData


validateState :: Lib2.State -> Either ErrorMessage Lib2.State
validateState = Either.Right


removeBomb :: (Int, Int) -> [RenderData] -> [RenderData]
removeBomb _ [] = []
removeBomb (x, y) ((RenderData char xRender yRender):xs) | x == xRender && y ==yRender = removeBomb (x, y)  xs
                                                         | otherwise = RenderData char xRender yRender : removeBomb (x, y) xs
