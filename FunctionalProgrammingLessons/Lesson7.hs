-- |


module Desktop.FunctionalProgrammingLessons.Lesson7 where

l :: [(Integer,[Char])]
l =[(a,b) | a <- [1..5],b<- ["labas","rytas"]]

l' :: [(Integer,[Char])]
l' = do
  a <- [1..5]
  b <- ["labas","rytas"]
  return(a,b)

ll :: [Integer]
ll = do
  a<- []
  b <- [1,2,3,4]
  return b

ll' :: [Integer]
ll' = do
  a<- [1]
  b <- [1,2,3,4]
  return b

ll'' :: [Integer]
ll'' = do
  a<- [1]
  b <- [1,2,3,4]
  [b,b]

-- wrong :: [Integer]
-- wrong = do
--   a<- [1]
--   b <- [1,2,3,4]
--   b

lll :: [(Integer,Integer,Char)]
lll = do
  a <- [1,2,4]
  b <- [1,2,3,4]
  c <- ['a','b']
  return (a,b,c)

lllskip = do
  a <- [1,2,4]
  b <- [1,2,3,4]
  c <- ['a','b']
  return (a,c)

lll' :: [(Integer,Integer,Char)]
lll' = do
  a <- [1,2,4]
  b <- [1..a]
  c <- ['a','b']
  return (a,b,c)


m':: [Integer]
m' = do
  a <- [54]
  b <- [36]
  return (a+b)


m:: Maybe Integer
m = do
  a <- Just 54
  b <- Just 36
  return (a+b)

m'':: Maybe Integer
m'' = do
  a <- Nothing
  b <- Just 36
  return (a+b)


m''':: Maybe Integer
m''' = do
  a <- Just 36
  b <- Just 36
  c <- Nothing
  return (a+b)

validatedName = Right "vaclovas"
validatedAge = Right 77

data Person = Person String Int deriving (Show)

vp :: Either String Person
vp = do
  name <- validatedName
  age <- validatedAge
  return $ Person name age

vp' :: Either String Person
vp' = do
  name <- validatedName
  age <- Left "invalid age"
  return $ Person name age

vp'' :: Either String Person
vp'' = do
  name <- validatedName
  age <- validatedAge
  _ <- Left "invalid weerr"
  return $ Person name age

vp''' :: Either String Person
vp''' = do
  _ <- Left "invalid weerr"
  name <- validatedName
  age <- validatedAge
  return $ Person name age
