module Lesson1 where

--- f (x) = (x + 1)^2

--- p (x) = {
--- x1 = x + 1
--- r = x1 ^ 2
--- return r
--- }

f1 :: Integer
f1 = 5

add1 :: Integer -> Integer -> Integer
add1 a b = a - b

even :: Integer -> Bool
even a = mod a 2 == 0

even1 :: Integer -> Bool
even1 a = a `mod` 2 == 0

branch :: Integer -> String
branch a = if even1 a then "Even" else "Odd"

ch :: Char
ch = 'h'

l1 :: [Int]
l1 = [1, 2, 3, 4]

type T1 = (String, Int)

tup :: T1
tup = ("Labas", 4)

type T2 = (Int, Char, Bool)

tup1 :: T2
tup1 = (1, 'H', False)

type T3 = (T1, T2)

tup2 :: T3
tup2 = (("", 4), (1, 'G', True))

type T4 = (T2, T1)
