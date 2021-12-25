-- |
module Desktop.FunctionalProgrammingLessons.Lesson6 where

-- lambda calculus

data Term = Var String | Abs String Term | App Term Term

instance Show Term where
  show(Var n) = n
  show(Abs n t) = concat["(λ",n,".",show t, ")"]
  show(App t1 t2) = concat["(",show t1," ",show t2, ")"]

tru :: Term
tru = Abs "t" (Abs "f" (Var "t"))

false :: Term
false = Abs "t" (Abs "f" (Var "f"))
