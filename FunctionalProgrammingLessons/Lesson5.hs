-- |

module Desktop.FunctionalProgrammingLessons.Lesson5 where



ans :: [Int]
ans = map length ["labas","medi"]

ans' :: [Int]
ans' = map (\x->length x +2) ["labas","medi"]

comp :: String ->Int
comp str = length str + 2



foldExample :: Int
foldExample = foldl (\acc x-> acc + x ) 0 [1,2,3,4]


-- concat
