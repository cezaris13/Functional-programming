-- |
module Desktop.FunctionalProgrammingLessons.Lesson8 where
import Control.Concurrent
import Control.Concurrent.Chan
-------------------- 3rd assignment thingies needed
pr :: IO()
pr = putStrLn "labas"

-- getLine



-- () - unit, kinda like void
-- () :: ()
-- takes no value and returns no value like this (returns () )

greetings :: IO()
greetings = do
  putStrLn "What is your name?"
  name <- getLine
  putStrLn $ "Hello, " ++ name

greetings' :: IO()-- this is how we should code in haskell
greetings' = do
  putStrLn "What is your name?"
  name <- getLine
  putStrLn $ bl name

bl :: String -> String
bl name = "Hello "++ name

lenOfIO :: IO Int
lenOfIO = do
  str <- getLine
  return $ length str


-- escapeIO :: Int
-- escapeIO = do
--   str <- getLine
--   let l =length str
--   l




-------------------threads--------------------

doSmt :: IO()
doSmt = do
  putStrLn "Bar"

doInParallel :: IO ()
doInParallel = do
  forkIO doSmt
  putStrLn "Foo"

-- both write to terminal -- fix this
--FooB
--ar

writer :: Chan String -> IO ()
writer channel= do
  threadDelay 1000000
  str <- readChan(channel)
  putStrLn str
  writer channel

doSmtWithChan :: Chan String -> IO()
doSmtWithChan ch = do
  writeChan ch "Bar"

doInParallelFixed :: IO ()
doInParallelFixed = do
  ch <- newChan
  forkIO $ writer ch
  forkIO $ doSmtWithChan ch
  putStrLn "Foo"

-- threadDelay :: Int -> IO()
