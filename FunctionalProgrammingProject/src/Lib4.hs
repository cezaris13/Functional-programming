{-# LANGUAGE TupleSections #-}

module Lib4 where

import Data.List
import Data.Maybe ( isJust,fromJust )
import Control.Applicative ( Alternative((<|>)) )
import Control.Monad.STM
import Control.Concurrent.STM.TVar
import Control.Concurrent.STM.TMVar
import Data.ByteString.Lazy (ByteString, unpack)
import Lib2
import Data.Char (chr)
import Control.Concurrent
--------------------------------------------------
-- initialData
getMapHeight :: [String] -> Int
getMapHeight = length

getMapWidth :: [String] -> Int
getMapWidth (h:_) = length h

initialDataToJson :: String -> [String] -> String
initialDataToJson gameId initMap =
  "{\"uuid\":\"" ++ gameId ++
  "\",\"height\":" ++ show(getMapHeight initMap) ++
  ",\"width\":" ++ show(getMapWidth initMap) ++
  "}"

--------------------------------------------------
-- game details
data GameBlocks = GameBlocks
  {
  bomb :: [(Int,Int)],
  bomberman :: [(Int,Int)],
  bricks :: [(Int,Int)],
  gates :: [(Int,Int)],
  ghosts :: [(Int,Int)],
  wall :: [(Int,Int)]
  }
  deriving (Show)

data GameDetails = GameDetails
  {
  gameId :: String,
  gameBlocks :: GameBlocks,
  dimensions :: (Int,Int),
  ongoing :: Bool
  }
  deriving (Show)

emptyGameBlocks :: GameBlocks
emptyGameBlocks = GameBlocks [] [] [] [] [] []

convertMapToGameBlocks :: [String] -> GameBlocks
convertMapToGameBlocks currMap = convertMapToGameBlocks' currMap 0 emptyGameBlocks

convertMapToGameBlocks' :: [String] -> Int -> GameBlocks -> GameBlocks
convertMapToGameBlocks' [] _ acc = acc
convertMapToGameBlocks' (h:t) x acc = convertMapToGameBlocks' t (x+1) newMapDetailAcc
  where resultCoords = convertOneMapLine h x
        newMapDetailAcc =
          GameBlocks
          (bomb acc ++ bomb resultCoords)
          (bomberman acc ++ bomberman resultCoords)
          (bricks acc ++ bricks resultCoords)
          (gates acc ++ gates resultCoords)
          (ghosts acc ++ ghosts resultCoords)
          (wall acc ++ wall resultCoords)

convertOneMapLine :: String -> Int -> GameBlocks
convertOneMapLine line xpos =
  GameBlocks
  (map (xpos,) ('b' `elemIndices` line)) -- bomb
  (map (xpos,) ('i' `elemIndices` line)) -- bomberman
  (map (xpos,) ('x' `elemIndices` line)) -- brick
  (map (xpos,) ('H' `elemIndices` line)) -- gate
  (map (xpos,) ('O' `elemIndices` line)) -- ghost
  (map (xpos,) ('#' `elemIndices` line)) -- wall

mapToJson :: GameBlocks -> String
mapToJson input =
  "{\"bomb\":" ++ serializeListOfInts (bomb input) ++
  ",\"surrounding\":{" ++
  "\"bombermans\":" ++ serializeListOfInts (bomberman input) ++
  ",\"bricks\":" ++ serializeListOfInts (bricks input) ++
  ",\"gates\":" ++ serializeListOfInts (gates input) ++
  ",\"ghosts\":" ++ serializeListOfInts (ghosts input) ++
  ",\"wall\":" ++ serializeListOfInts (wall input) ++
  "}}"

serializeListOfInts :: [(Int,Int)] -> String
serializeListOfInts input = "[" ++ intercalate "," (map (\x-> "[" ++ show (fst x) ++ "," ++ show (snd x) ++ "]") input) ++ "]"
--------------------------------------------------
-- actions

moveBomberman :: GameDetails -> String  -> GameBlocks
moveBomberman (GameDetails gameId currMap dimensions ongoing) direction = resultMap
  where bombermanCoords = head $ bomberman currMap
        bombermanNewPosition
          | direction == "Right" = (fst bombermanCoords, snd bombermanCoords + 1)
          | direction == "Left" = (fst bombermanCoords, snd bombermanCoords - 1)
          | direction == "Up" = (fst bombermanCoords - 1, snd bombermanCoords)
          | direction == "Down" = (fst bombermanCoords + 1, snd bombermanCoords)
          | otherwise  = bombermanCoords
        resultMap
          | reachedEdge bombermanNewPosition dimensions = currMap
          | isJust (bombermanNewPosition `elemIndex`  bricks currMap <|>
            bombermanNewPosition `elemIndex` wall currMap) = currMap
          | otherwise = GameBlocks (bomb currMap)
                                   [bombermanNewPosition]
                                   (bricks currMap)
                                   (gates currMap)
                                   (ghosts currMap)
                                   (wall currMap)

addBomb :: GameDetails -> GameDetails
addBomb currGame = resultGame
  where currMap = gameBlocks currGame
        isBombAdded
          | Data.List.null (bomb currMap) = False
          | otherwise = True
        resultMap
          | isBombAdded = currMap
          | otherwise = currMap {bomb = bomberman currMap}
        resultGame = currGame {gameBlocks = resultMap}

explodingBomb :: GameBlocks -> GameBlocks
explodingBomb oldMap = newMap
  where bombCoords = head $ bomb oldMap
        upExplode = (fst bombCoords - 1, snd bombCoords)
        downExplode = (fst bombCoords + 1, snd bombCoords)
        leftExplode = (fst bombCoords, snd bombCoords - 1)
        rightExplode = (fst bombCoords, snd bombCoords + 1)
        listOfExplodingPlaces = [upExplode, downExplode, leftExplode, rightExplode]
        newMap
          | Prelude.null $ bomb oldMap = oldMap
          | otherwise = GameBlocks []
                            (bomberman oldMap)
                            (bricks oldMap \\ listOfExplodingPlaces)
                            (gates oldMap)
                            (ghosts oldMap \\ listOfExplodingPlaces) -- \\ takes list difference - [1,2,3] \\ [2,1] -> [3]
                            (wall oldMap)

moveGhost :: GameDetails -> GameBlocks
moveGhost gameDetails = newMap
  where oldMap = gameBlocks gameDetails
        newMap = GameBlocks (bomb oldMap)
                            (bomberman oldMap)
                            (bricks oldMap)
                            (gates oldMap)
                            (map (\x -> moveOneGhost x gameDetails) (ghosts oldMap))
                            (wall oldMap)

moveOneGhost :: (Int,Int) -> GameDetails -> (Int,Int)
moveOneGhost oldGhostPos (GameDetails _ oldMap dimensions ongoing) = finalGhostPos
  where bombermanCoords = head $ bomberman oldMap
        remainder = (uncurry (+) bombermanCoords + uncurry (+) oldGhostPos) `mod` 4
        newGhostPos
          | remainder == 0 = (fst oldGhostPos - 1, snd oldGhostPos)
          | remainder == 1 = (fst oldGhostPos + 1, snd oldGhostPos)
          | remainder == 2 = (fst oldGhostPos, snd oldGhostPos - 1)
          | otherwise      = (fst oldGhostPos, snd oldGhostPos + 1)
        finalGhostPos
          | reachedEdge newGhostPos dimensions = oldGhostPos
          | isJust (newGhostPos `elemIndex`  bricks oldMap <|>
            newGhostPos `elemIndex` wall oldMap) = oldGhostPos
          | otherwise = newGhostPos

reachedEdge :: (Int,Int) -> (Int, Int) -> Bool
reachedEdge (x,y) (maxX, maxY) =
  x < 0 || y < 0 || y >= maxY || x >= maxX
--------------------------------------------------
-- operations in API
getGameMap :: String -> TMVar [GameDetails] -> IO (Maybe GameDetails)
getGameMap gameId maps = do
  let stmMaps = readTMVar maps
  currMaps <- atomically stmMaps
  let returnResult = find (\(GameDetails id _ _ _) ->id == gameId) currMaps

  return returnResult

addNewGameToList :: TMVar [GameDetails] -> GameDetails -> STM()
addNewGameToList gameList newGame = do
  currGameList <- takeTMVar gameList
  putTMVar gameList (newGame : currGameList)

updateOneGame :: TMVar [GameDetails] -> GameDetails -> STM()
updateOneGame mapList movedMap = do
  currMapList <- takeTMVar mapList
  let currGame = checkGameOver movedMap
  putTMVar mapList (map (\gameDetails-> if gameId gameDetails == gameId currGame then currGame else gameDetails) currMapList)
  return()

checkGameOver :: GameDetails -> GameDetails
checkGameOver gameDetails = resultGame
  where resultGame = gameDetails {ongoing = isGameOver (gameBlocks gameDetails)}

isGameOver :: GameBlocks -> Bool
isGameOver gameBlocks = do
  let existance = find (\x -> x == head (bomberman gameBlocks)) (ghosts gameBlocks ++ gates gameBlocks)
  case existance of
    Nothing -> True
    Just _ -> False

bombExplodeThread :: GameDetails -> TMVar [GameDetails]-> IO()
bombExplodeThread gameDetails gameMaps = do
  let newGameBlocks = moveGhost (addBomb gameDetails)
  let newGameDetails = gameDetails {gameBlocks = newGameBlocks}
  if not (bombExists (gameBlocks gameDetails))
    then do
      atomically $ updateOneGame gameMaps newGameDetails
    else do
    forkIO $ do
      atomically $ updateOneGame gameMaps newGameDetails
      threadDelay 3000000
      parsed <- getGameMap (gameId gameDetails) gameMaps
      atomically $ updateOneGame gameMaps newGameDetails {gameBlocks = explodingBomb $ gameBlocks $ fromJust parsed}
      Prelude.putStrLn "Bomb exploded"
    return()

moveBombermanIO :: GameDetails -> String -> TMVar [GameDetails] -> IO()
moveBombermanIO gameDetails direction gameMaps = do
  let newGameDetails = gameDetails {gameBlocks = moveBomberman gameDetails direction}
  let newGameDetails1 = gameDetails {gameBlocks = moveGhost newGameDetails}
  atomically $ updateOneGame gameMaps newGameDetails1
  print ("Bomberman moved to the: " ++ direction)

fetchSurroundings :: GameDetails -> TMVar [GameDetails] -> IO()
fetchSurroundings gameDetails gameMaps = do
  let newGameDetails = gameDetails {gameBlocks = moveGhost gameDetails}
  atomically $ updateOneGame gameMaps newGameDetails
  print "Fetched surroundings"

userInput :: String -> String -> TMVar [GameDetails] -> IO()
userInput json gameId gameMaps = do
  gameBlocks <- getGameMap gameId gameMaps
  if ongoing $ fromJust gameBlocks
    then do
      let parsedJson = runParser parseJsonLike json
      case parsedJson of
        Left errorMessage -> do
          print errorMessage
        Right parsed -> do
          let name = getCommandName $ fst parsed
          print name
          case name of
            "PlantBomb" -> bombExplodeThread (fromJust gameBlocks) gameMaps
            "FetchSurrounding" -> fetchSurroundings (fromJust gameBlocks) gameMaps
            "FetchBombSurrounding" -> fetchSurroundings (fromJust gameBlocks) gameMaps
            "FetchBombStatus" ->  return()
            "MoveBomberman" -> do
              let direction = getDirection $ fst parsed
              moveBombermanIO (fromJust gameBlocks) direction gameMaps
          print parsed
    else do
      return ()

createNewGame :: String -> [String] -> TMVar[GameDetails] -> IO()
createNewGame gameId initialMap gameMaps = do
  let newGame = GameDetails gameId (convertMapToGameBlocks initialMap) (getMapHeight initialMap, getMapWidth initialMap) True
  atomically $ addNewGameToList gameMaps newGame
  print ("New game created, id:" ++ gameId)

byteStringToString :: ByteString -> String
byteStringToString input = Prelude.map (chr . fromEnum) (unpack input)

bombExists :: GameBlocks -> Bool
bombExists mapp = Prelude.null $ bomb mapp

getDirection :: JsonLike -> String -- example Right
getDirection (JsonLikeObject input) = (\(JsonLikeString x) -> x)((\(JsonLikeObject x)-> snd $ head x) (snd $ head input))

getCommandName :: JsonLike -> String -- example: MoveBomberman
getCommandName (JsonLikeObject input) = (\(JsonLikeString x) -> x)((\(JsonLikeObject x)-> snd $ last x) (snd $ head input))
