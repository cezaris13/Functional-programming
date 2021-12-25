{-# LANGUAGE OverloadedStrings #-}

import Web.Scotty (post, scotty, liftAndCatchIO, param, text, body, addHeader)
import Control.Monad.IO.Class
import Data.UUID
import Data.UUID.V4
import Data.Maybe (fromJust)
import Control.Concurrent.STM.TVar
import Control.Concurrent.STM.TMVar
import Data.Text.Lazy (pack)
import Lib4

initialMap :: [String]
initialMap =
  ["############################################################",
   "# i   xxxxxxxx#                                           H#",
   "#x#x# # # # # #                                            #",
   "#   x x       #                                            #",
   "# # # # # #x#x#                                            #",
   "#   x   x     #                                            #",
   "# # #x#x# #x#x#          xxxxxxxxxxxxxx                    #",
   "#         x x #          x      O     x                    #",
   "#x#x# #x#x#x#x#          xxxxxxxxxxxxxx                    #",
   "#             #                                            #",
   "#x#x#x#x#x#x# #                                            #",
   "#             #                        O                   #",
   "# #x#x#x#x#x#x#                                            #",
   "#                                                          #",
   "############################################################"]

main :: IO ()
main = do
    gameMaps <- newTMVarIO ([] :: [GameDetails])
    scotty 3000 $ do
        post "/v1/game/new/random" $ do
          newGameId <- liftAndCatchIO nextRandom
          liftIO $ createNewGame (toString newGameId) initialMap gameMaps
          let initialJson = initialDataToJson (toString newGameId) initialMap
          addHeader "Connection" "keep-alive"
          addHeader "Content-Length" (pack $ show $ length initialJson)
          addHeader "Content-Type" "application/json"
          text (pack $ initialDataToJson (toString newGameId) initialMap)
        post "/v3/game/:uuid" $ do
          gameId <- param "uuid"
          gameCommands <- body
          liftIO $ userInput (byteStringToString gameCommands) gameId gameMaps
          parsed <- liftIO $ getGameMap gameId gameMaps
          let serializedJson = mapToJson $ gameBlocks $ fromJust parsed
          addHeader "Connection" "keep-alive"
          addHeader "Content-Length" (pack $ show $ length serializedJson)
          addHeader "Content-Type" "application/json"
          text $ pack serializedJson
