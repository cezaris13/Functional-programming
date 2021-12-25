-- |

module Desktop.FunctionalProgrammingLessons.Lesson12 where
-- usually 5 questions

-- tail recursions
-- list of expressions and provide type of these expressions
-- lambda calculus
-- Concurrency and parralelism (code snippets, fork io, stm)
-- seniau buvo lengvas(siemet nebus taip : ( )
import Control.Monad.Trans.Class(lift)
import Control.Monad.Trans.Except(ExceptT,runExceptT,throwE)
import Control.Monad.Trans.State.Strict(State, StateT,get,put,runState)
import Control.Monad.IO.Class
import Data.Char as C (isDigit)

type Parser a = ExceptT String (State String) a

-- parseInt :: State String Int
-- parseInt = do
--   s <- get
--   let numberAsString = takeWhile C.isDigit s
--   let len = length numberAsString
--   put $ drop len s
--   return $ read numberAsString

-- -- 123 123
-- parseTwoNumbers :: State String (Int,Int)
-- parseTwoNumbers = do
--   a <- parseInt
--   parseSpace
--   b <- parseInt
--   return (a,b)

-- parseSpace :: State String ()
-- parseSpace = do
--   s <- get
--   case s of
--     ' ' :str -> do
--       put str
--       return ()
--     _ -> error "space expected" -- next week we will fix this with either

parseInt :: Parser Int
parseInt = do
  s <- lift get
  let numberAsString = takeWhile C.isDigit s
  let len = length numberAsString
  lift $ put $ drop len s
  return $ read numberAsString

-- 123 123
parseTwoNumbers :: Parser (Int,Int)
parseTwoNumbers = do
  a <- parseInt
  parseSpace
  b <- parseInt
  return (a,b)

parseSpace :: Parser ()
parseSpace = do
  s <- lift get
  case s of
    ' ' : str -> do
      lift $ put str
      return ()
    _ -> throwE "space expected" -- next week we will fix this with either

--runState (runExceptT  parseTwoNumbers) "123aaaa"
-- (Left "space expected","aaaa")


type OtherParser a = ExceptT String (StateT String IO) a
-- to reach inner thignies you call as many lifts as we need

otherComp :: OtherParser Int
otherComp = do
  a <- list $ lift getLine
  b <- liftIO getLine
  return 42
