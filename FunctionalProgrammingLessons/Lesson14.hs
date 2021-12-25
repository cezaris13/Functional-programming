{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}

module Desktop.FunctionalProgrammingLessons.Lesson14 where

import Data.Foldable
import Data.Monoid

-- initial vs final
-- tags vs tagless

--initial grammar
data Exp =  Lit Int | Neg Exp | Add Exp Exp | Mult Exp Exp
  deriving (Show)

i1 :: Exp
i1 = Add (Lit 8) (Neg (Add (Lit 1) (Lit 1)))

eval :: Exp -> Int
eval (Lit i) = i
eval (Neg e) = -(eval e)
eval (Add e1 e2) = (eval e1) + (eval e2)
eval (Mult e1 e2) = (eval e1) * (eval e2)

view :: Exp -> String
view (Lit i) = show i
view (Neg e) ="-("++ view e ++")"
view (Add e1 e2) = "(" ++ (view e1) ++ "+" ++ (view e2) ++ ")"
view (Mult e1 e2) = "(" ++ (view e1) ++ "*" ++ (view e2) ++ ")"

-- decode :: String -> Exp -- todo
-- decode input =

-- final grammar

class ExpF repr where
  lit :: Int -> repr
  neg :: repr -> repr
  add :: repr -> repr -> repr

class MultExpF repr where
  mult :: repr->repr->repr

instance MultExpF Int where
  mult e1 e2 = e1*e2

instance MultExpF String where
  mult e1 e2 = "(" ++ e1 ++ "*" ++ e2 ++ ")"

-- f1 = add (lit 8) (neg (add (lit 1) (lit 1)))

instance ExpF Int where
  lit i = i
  neg e = - e
  add e1 e2 = e1 + e2

instance ExpF String where
  lit i = show i
  neg e = "-("++ e ++")"
  add e1 e2 = "(" ++ e1 ++ "+" ++ e2 ++ ")"

evalF :: Int -> Int
evalF = id

viewF :: String -> String
viewF = id




d = foldMap Sum [1,3,5]
