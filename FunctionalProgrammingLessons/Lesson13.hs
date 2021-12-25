module Desktop.FunctionalProgrammingLessons.Lesson13 where

import Control.Monad.STM
import Control.Monad
import Control.Concurrent.STM.TVar
import Control.Concurrent
import Control.Applicative

-- stm!!!

transfer :: TVar Integer -> TVar Integer -> Integer -> STM()
transfer a b amount= do
  aAmount <- readTVar a
  bAmount <- readTVar b
  when (aAmount-amount <0) retry
  -- check (aAmount-amount >=0)
  writeTVar a (aAmount-amount)
  writeTVar b (bAmount+amount)
  return ()

main :: IO()
main =  do
  a <- newTVarIO 12
  b <- newTVarIO 15
  atomically $ transfer a b 6
  newA <- readTVarIO a
  newB <- readTVarIO b
  putStrLn $ concat ["a=",show newA, ",b=",show newB]


main' :: IO()
main' =  do
  a <- newTVarIO 12
  b <- newTVarIO 15
  forkIO $ do
    atomically $ transfer a b 1000
    newA <- readTVarIO a
    newB <- readTVarIO b
    putStrLn $ concat ["a=",show newA, ",b=",show newB]
  atomically $ modifyTVar a (\old-> old + 100000)



-- <|>
data JLS = JSString String | JSNumber Int deriving Show

parseJLS :: String ->Maybe JLS
parseJLS input = parseInt input <|> parseString input

parseInt ::String-> Maybe JLS
parseInt input =
  Just $ JSNumber 12

parseString :: String ->Maybe JLS
parseString input = Nothing
