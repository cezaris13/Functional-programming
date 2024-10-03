module Main where

import Control.Exception (bracket)
import Control.Lens ((^.))
import qualified Data.ByteString as B
import Network.Wreq (post, responseBody)
import qualified Network.Wreq.Session as Sess
import Network.Wreq.Lens (Response (..))
import Data.String.Conversions (cs)
import Data.Function ((&))
import Data.List as L (concat, (++))
import Lib2
import Lib3
import System.Console.ANSI as ANSI
  ( clearScreen,
    hideCursor,
    setCursorPosition,
    showCursor,
  )
import System.IO (BufferMode (..), hSetBuffering, hSetEcho, stderr, stdin, stdout)
import Prelude hiding (Left, Right)
import Lib1
    ( createRenders,
      RenderData(RenderData),
      playerChar,
      InitData(InitData),
      State(..),
      RenderData,
      render )
import Control.Concurrent

-- MANDATORY CODE
host :: String
host = "http://localhost:3000"
-- host = "http://bomberman.homedir.eu"

createGame ::
  (FromJsonLike a) =>
  Sess.Session ->
  IO a
createGame sess = do
  r <- Sess.post sess (host ++ "/v1/game/new/random") B.empty
  let resp = cs $ r ^. responseBody :: String
  return $ toJsonLike resp & e & fromJsonLike & e

postCommands ::
  (FromJsonLike a, ToJsonLike a, FromJsonLike b, ToJsonLike b) =>
  GameId ->
  Sess.Session ->
  a ->
  IO b
postCommands uuid sess commands = do
  let str = toJsonLike commands & e & fromJsonLike & e :: String
  let req = cs str :: B.ByteString
  r <- Sess.post sess (L.concat [host, "/v3/game/", uuid]) req
  let respStr = cs $ r ^. responseBody :: String
  return $ toJsonLike respStr & e & fromJsonLike & e


e :: Either String a -> a
e = either error id

-- MANDATORY CODE END

draw :: Lib2.State -> IO ()
draw state = do
  _ <- ANSI.clearScreen
  _ <- ANSI.setCursorPosition 0 0
  Prelude.putStrLn $ Lib2.render state


main :: IO ()
main = do
  hSetBuffering stdin NoBuffering
  hSetBuffering stderr NoBuffering
  hSetBuffering stdout NoBuffering
  hSetEcho stdin False
  bracket
    (ANSI.hideCursor >> Sess.newAPISession)
    (const showCursor)
    ( \sess -> do
        -- you are free to do whatever you want but:
        -- a) reuse sess (connection to the server)
        -- b) use createGame and postCommands to interact with the game server

        game <- createGame sess :: IO Lib3.NewGame
        let extraCommand = Just (Commands FetchSurrounding Nothing)
        let commands = Commands FetchBombSurrounding extraCommand :: Commands

        let initData = Lib2.InitData {gameWidth = width game, gameHeight = height game}

        bombSurr <- postCommands (gameId game) sess commands :: IO CommandsResponse

        let initialState = Lib2.init initData (response bombSurr)


        draw initialState
        mVar <- newEmptyMVar
        putMVar mVar initialState
        loop (gameId game) sess mVar
    )

getAllInfo :: Maybe Commands
getAllInfo = Just (Commands FetchSurrounding (Just (Commands FetchBombStatus Nothing)))

-- test (',':t) arr = parseObject' t arr

loop :: String -> Sess.Session -> MVar Lib2.State -> IO ()
loop uuid sess mVar = do
  c <- getChar
  let commands = case c of
        'a' -> Commands (MoveBomberman Left) getAllInfo
        's' -> Commands (MoveBomberman Down) getAllInfo
        'd' -> Commands (MoveBomberman Right) (Just (Commands FetchSurrounding Nothing))
        'w' -> Commands (MoveBomberman Up) getAllInfo
        'b' -> Commands PlantBomb getAllInfo
        _-> Commands FetchSurrounding Nothing
  postState <- takeMVar mVar
  let state = Lib2.State (removeExplosion (getRenderData postState)) (getInitData postState)
  r <- postCommands uuid sess commands :: IO CommandsResponse
  -- print (response r)
  let newState = Lib3.update state (response r) ('b' == c)

  -- let jsonAsString = jsonLikeToString jl
  -- let objectList = stringToListOfObject jsonAsString

  if 'b' == c
    then
      do
        putMVar mVar newState
        forkIO $ threadBombExplosion mVar uuid sess
        loop uuid sess mVar
    else
      do
        _ <- draw newState
        putMVar mVar newState
        loop uuid sess mVar

threadBombExplosion :: MVar Lib2.State -> [Char] -> Sess.Session -> IO ()
threadBombExplosion mVar uuid sess = do
  let command = Commands FetchBombStatus Nothing
  check <- postCommands uuid sess command :: IO CommandsResponse
  -- let jsonAsString = jsonLikeToString (response check)
  -- let objectList = stringToListOfObject (response check)

  let objectList = jsonLikeToListOfObjects (response check)
  if isBombNull objectList
    then do
      newestState <- takeMVar mVar
      let bombPosition = Lib2.findBomb (getRenderData newestState)
      let allExplosionCoordinates = explosionCoordinates bombPosition

      let explosionRender = Lib2.explodeBomb (getRenderData newestState) allExplosionCoordinates
      let newRenderData = removeBomb bombPosition explosionRender ++ [RenderData '*' (fst bombPosition) (snd bombPosition)]

      let newState = Lib2.State newRenderData (getInitData newestState)
      draw newState
      putMVar mVar newState
    else threadBombExplosion mVar uuid sess

    -- in case objectList of
    --   Either.Left err -> print ""
    --   Either.Right array -> do
    --     if isBombNull array
    --       then do
    --         newestState <- takeMVar mVar
    --         let bombPosition = Lib2.findBomb (getRenderData newestState)
    --         let allExplosionCoordinates = explosionCoordinates bombPosition

    --         let explosionRender = Lib2.explodeBomb (getRenderData newestState) allExplosionCoordinates
    --         let newRenderData = removeBomb bombPosition explosionRender ++ [RenderData '*' (fst bombPosition) (snd bombPosition)]

    --         let newState = (Lib2.State newRenderData (getInitData newestState))
    --         draw newState
    --         putMVar mVar newState
    --       else threadBombExplosion mVar uuid sess
