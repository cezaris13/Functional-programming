module Lib1 where

import GHC.StableName (StableName)
import qualified Data.Char as C (isDigit, isLetter)

data InitData = InitData
  { gameWidth :: Int,
    gameHeight :: Int
  }
  deriving (Show)

data RenderData = RenderData
    {
        gameBlock:: Char,
        y :: Int,
        x :: Int
    }
    deriving (Show)

data State = State [RenderData] InitData
  deriving (Show)

playerChar :: Char
playerChar = 'i'

ghostChar :: Char
ghostChar = 'O'

-- | Change State the way need but please keep
--  the name of the type, i.e. "State"
-- data State = State String InitData
--   deriving (Show)

-- | Is called in a very beginning of a game
init ::
  -- | Initial data of the game
  InitData ->
  -- | First json message before any moves performed
  String ->
  -- | An initial state based on initial data and first message
  State
init i j = state j i
-- init i j = State j i

-- | Is called after every user interaction (key pressed)
update ::
  -- | Current state
  State ->
  -- | Json message from server
  String ->
  -- | A new state, probably based on the message arrived
  State
-- update (State _ i) j = state j i
update (State oldBlocks i) j = do
  let (State newBlocks tempInitData) = state j i
  let finalBlocks = appendBlocks newBlocks oldBlocks
  State finalBlocks tempInitData

--checks if block in state, if not adds it to block list, makes exception for the player
--expected format:
-- [RenderData {gameBlock = 'i', y = 1, x = 1},RenderData {gameBlock = 'x', y = 1, x = 6}]
-- [RenderData {gameBlock = 'i', y = 1, x = 1},RenderData {gameBlock = 'x', y = 1, x = 7}]
-- [RenderData {gameBlock = 'i', y = 1, x = 1},RenderData {gameBlock = 'x', y = 1, x = 6}]
--returns [RenderData {gameBlock = 'i', y = 1, x = 1},RenderData {gameBlock = 'x', y = 1, x = 6},RenderData {gameBlock = 'x', y = 1, x = 7}]
appendBlocks :: [RenderData] -> [RenderData] -> [RenderData]
appendBlocks renderDataList [] = renderDataList
appendBlocks renderDataList (oldListHead:oldListTail) = do
  if not (findEquals renderDataList oldListHead) && gameBlock oldListHead /= playerChar && gameBlock oldListHead /= ghostChar
    then appendBlocks (oldListHead : renderDataList) oldListTail 
    else appendBlocks renderDataList oldListTail 


--expected format:
-- [RenderData {gameBlock = 'i', y = 1, x = 1},RenderData {gameBlock = 'x', y = 1, x = 6}]
-- RenderData {gameBlock = 'x', y = 1, x = 7}
--returns Bool False (if there is no such block in state) or Bool True (if there already exists such a block)
findEquals :: [RenderData] -> RenderData -> Bool
findEquals [] _ = False
findEquals (RenderData symbol y x :arrs) (RenderData checkSymbol checkY checkX)
  | symbol == checkSymbol && y == checkY && x == checkX = True 
  | otherwise = findEquals arrs (RenderData checkSymbol checkY checkX)


-- | Renders the current state
render ::
  -- | A state to be rendered
  State ->
  -- | A string which represents the state. The String is rendered from the upper left corner of terminal.
  String
render (State coords (InitData width height) ) = iterLoop coords width 0 height

iterLoop :: [RenderData] -> Int -> Int -> Int -> String
iterLoop coords width y height = do
  if y == height
  then findX 0 width y coords
  else do
    findX 0 width y coords ++ iterLoop coords width (y+1) height

findX :: Int -> Int -> Int -> [RenderData] -> String
findX x width y arr = do
  if x == width
    then do
      "\n"
    else do
      findSymbol arr x y ++ findX (x+1) width y arr

findSymbol :: [RenderData] -> Int -> Int -> String
findSymbol [] _ _ = [' ']
findSymbol (RenderData symbol rY rX  :arrs) x y = do
  if x == rX && y == rY
    then [symbol]
    else findSymbol arrs x y

-- state :: String -> State
state :: String -> InitData -> State
state json (InitData width height) = do
--json = "{\"surrounding\":{\"bombermans\":[[1,1],[1,7],[4,5]],\"ghosts\":[],\"bricks\":[[1,6],[1,7]],\"wall\":[[8,6],[8,8]]}}"
  let (names, arrays) = parseBomberJson json
  -- let renders = createRenders arrays ['B', 'b', 'G', 'g', 'w']
  let renders = createRenders arrays [playerChar, 'x', 'H', 'O', '#']
  --init data should not be hardcoded
  let calculated = State renders (InitData width height)
  calculated


createRenders :: [[(Int, Int)]] -> [Char]-> [RenderData]
createRenders [] _ = []
createRenders _ [] = []
createRenders arrays symbols = do

  let renders1 = createRendersForOneArray (head arrays) (head symbols)
  let renders2 = createRenders (tail arrays) (tail symbols)
  renders1 ++ renders2
  
createRendersForOneArray :: [(Int, Int)] -> Char -> [RenderData]
createRendersForOneArray [] _ = []
createRendersForOneArray (pair : array) symbol = do
  --let newPair = RenderData 'X' (fst pair) (snd pair)
  let newPair = uncurry (RenderData symbol) pair
  newPair : createRendersForOneArray array symbol

{- expected format
{"surrounding":{
"bombermans":[[1,1]],
"bricks":[[1,6],[1,7]],
"gates":[],
"ghosts":[],
"wall":[[8,6],[8,8]]}
}
-}
parseBomberJson :: String -> ([String], [[(Int, Int)]])
parseBomberJson text = do
  let (name, arrays) = parseArrayName (tail text)
  let (names, coordinates, leftover) = parseSurroundings (tail arrays)
  (names, coordinates)

{- expected format
"bombermans":[[1,1]],
"bricks":[[1,6],[1,7]],
"gates":[],
"ghosts":[],
"wall":[[8,6],[8,8]]}
-}

parseSurroundings :: String -> ([String], [[(Int, Int)]], String)
parseSurroundings text = do
  let (arrayName, coordinates, leftover1) = parseArray text
  let nextSymbol = head leftover1
  let (arrayNameList, coordinatesList, leftover2) = case nextSymbol of
        '}' -> ([], [], leftover1)
        _ -> parseSurroundings (tail leftover1)
  (arrayName : arrayNameList, coordinates : coordinatesList, leftover2)
  

--expected format "bricks":[[1,6],[1,7],[1,8]]abc
--returns bricks [(1, 6), (1,7), (1,8)] abc
parseArray :: String -> (String, [(Int, Int)], String)
parseArray text = do
  let (arrayName, coordinatesAsText) = parseArrayName text
  let (coordinates, leftover) = parseListOfCoordinates (tail coordinatesAsText)
  (arrayName, coordinates, leftover)


--expected format "bricks":[[1,6],[1,7],[1,8]]
--returns bricks [[1,6],[1,7],[1,8]]
parseArrayName :: String -> (String, String)
parseArrayName text = do
  let textWithoutQuotation = tail text
  let arrayName = takeWhile C.isLetter textWithoutQuotation
  let len = length arrayName + 2 -- remove ":
  (arrayName, drop len textWithoutQuotation)

--expected format [1,6],[1,7],[1,8]]abc
-- returns [(1, 6), (1,7), (1,8)] abc
parseListOfCoordinates :: String -> ([(Int, Int)], String)
parseListOfCoordinates text = do
  let maybeEmpty = head text
  if maybeEmpty == ']'
    then ([], tail text)
  else do
    let (newCoordinate, leftover1) = parseCoordinates text
    let nextSymbol = head leftover1
    let (coordinatesList, leftover2) = case nextSymbol of
              ']' -> ([], tail leftover1)
              _ -> parseListOfCoordinates (tail leftover1)
    (newCoordinate : coordinatesList, leftover2)

--expected format [x,y]abc
--returns (x,y) abc
parseCoordinates :: String -> ((Int, Int), String)
parseCoordinates text = do
  let (x, notParsed) = parseInt (tail text)
  let (y, notParsed2) = parseInt (tail notParsed)
  ((x, y), tail notParsed2)

--expected format 123abc
--returns 123 abc
parseInt :: String -> (Int, String)
parseInt s =
  let numberAsString = takeWhile C.isDigit s
      len = length numberAsString
   in (read numberAsString, drop len s)

