-- |

module Desktop.FunctionalProgrammingLessons.Lesson9 where

-- >>= bind

-- [1,2,3] >>= (\a->[a,a])
-- result [1,1,2,2,3,3]

-- ["labas","medi"] >>= (\a->[length a])
-- result [5,4]


f' = do
  a <-  ["labas","medi" ]
  return (length a)

boo = do
  a <- Just "labas"
  b <- Just 5
  return (b* length a)


boo' =
  Just "labas"
  >>= (\a ->
         Just 5 >>= (\b->return (b* length a))
      )

l :: [Integer]
l =do
  a<-[]
  b<-[1,2,3]
  return $ b*b


-- >>


t :: IO()
t = do
  putStrLn "hello"
  putStrLn "world"

t' = putStrLn "hello" >> putStrLn "world"

-- mapM
mm = map (\a->Just a) [1,2,3] -- [Just 1, Just 2, Just 3]
mm' =mapM (\a->Just a) [1,2,3]-- Just [1, 2, 3]

sequence [Just 5,Nothing] -> Nothing
sequence [Just 5,Just 4] -> Just [5,4]
