-- |

module Desktop.FunctionalProgrammingLessons.Lesson11 where

import Data.Char as C
--state monad

--parseM :: M a


-- bad
-- newtype State s a = State
--   {
--     runState :: s -> (a, s)
--   }

-- get :: State s s
-- get = State (\s-> (s,s))

-- put :: s -> State s ()
-- put st = State (\_ -> ((),st))

-- data Person = Person
--   {
--     name :: String,
--     age :: Int
--   }
--   deriving (Show)

-- fff :: State String Person-- state strig person name
-- fff = do
--   n <- get
--   put ""
--   return $ Person n 42


--good
newtype State s a = State
  {
    runState :: s -> (a, s)
  }

get :: State s s
get = State (\s-> (s,s))

put :: s -> State s ()
put st = State (\_ -> ((),st))

data Person = Person
  {
    name :: String,
    age :: Int
  }
  deriving (Show)


instance Functor (State st) where
  fmap f functor = State $  \s ->
    let(a, sNew) = runState functor s
    in (f a, sNew)

instance Applicative (State st) where
  pure x = State $ \s ->(x, s)
  fa <*> xa = State $ \s ->
    let (f,s1) = runState fa s
        (x,s2) = runState xa s1
    in (f x,s2)

ff :: State Int Person-- state strig person name
ff = Person "Name" <$> get

-- run like runState fff Int

instance Monad (State st) where
  ma >>= f = State $ \s ->
    let (a,newState) = runState ma s
    in runState (f a) newState

fff :: State String Person
fff = do
  name <- get
  put ""
  return $ Person name 42

parseInt :: State String Int
parseInt = do
  s <- get
  let numberAsString = takeWhile C.isDigit s
  let len = length numberAsString
  put $ drop len s
  return $ read numberAsString

-- 123 123
parseTwoNumbers :: State String (Int,Int)
parseTwoNumbers = do
  a <- parseInt
  parseSpace
  b <- parseInt
  return (a,b)

parseSpace :: State String ()
parseSpace = do
  s <- get
  case s of
    ' ' :str -> do
      put str
      return ()
    _ -> error "space expected" -- next week we will fix this with either
