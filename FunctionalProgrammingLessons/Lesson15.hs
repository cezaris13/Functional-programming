module Desktop.FunctionalProgrammingLessons.Lesson15 where


import Data.List as L
-- lambda calculus Pt. 2

data Term = Var String | Abs String Term | App Term Term

instance Show Term where
  show(Var n) = n
  show(Abs n t) = concat["(λ",n,".",show t, ")"]
  show(App t1 t2) = concat["(",show t1," ",show t2, ")"]

tru :: Term
tru = Abs "t" (Abs "f" (Var "t"))

false :: Term
false = Abs "t" (Abs "f" (Var "f"))

apps :: [Term] -> Term
apps [] = error "Empty application"
apps [_] = error "two term needed to application"
apps (t1 :t2:ts) = apps' (App t1 t2) ts
  where  apps' t [] = t
         apps' t (x:xs) = apps' (App t x) xs

land :: Term
land = Abs "b" $ Abs "c" $ apps [Var "b",Var "c",Var "b"]

lor :: Term
lor = Abs "b" $ Abs "c" $ apps [Var "b",Var "b",Var "c"]

-- de bruijn "de brown"

data ITerm
  = IVar Int
  | IAbs ITerm
  | IApp ITerm ITerm
  deriving (Eq)

instance Show ITerm where
  show (IVar i) = show i
  show (IAbs i) = concat ["(λ.",show i,")"]
  show (IApp a b)= concat ["(",show a, " ", show b,")"]

deBruijnIndices :: [String] -> Term -> ITerm
deBruijnIndices ctx t = walk [] t
  where
    walk stack (Var n) = IVar (findInd stack n)
    walk stack (Abs n t) = IAbs (walk (n : stack) t)
    walk stack (App t1 t2) = IApp (walk stack t1) (walk stack t2)
    findInd stack n =
      case (n `L.elemIndex` stack, n `L.elemIndex` ctx) of
        (Just i,_) -> i
        (Nothing, Just i) -> L.length stack +i
        _ -> error $ "No index for free variable " ++ n

termShift :: Int -> ITerm -> ITerm
termShift d it= walk 0 it
  where
    walk c (IVar x)
      | x >= c = termShift c it
      | otherwise = IVar (x+d)
    walk d (IAbs t) = IAbs (walk (d + 1) t)
    walk d (IApp t1 t2) = IApp (walk d t1) (walk d t2)

termSubst :: ITerm-> ITerm -> ITerm
termSubst s= walk 0
  where
    walk c (IVar x)
      | x == c = termShift c s
      | otherwise = IVar x
    walk c (IAbs t') = IAbs (walk (c+1) t')
    walk c (IApp t1 t2) = IApp (walk c t1) (walk c t2)


termSubstTop :: ITerm -> ITerm -> ITerm
termSubstTop s t = termShift (-1) (termSubst (termShift 1 s) t)

eval :: ITerm -> ITerm
eval (IApp (IAbs t')v2) = termSubstTop v2 t'
eval (IApp v1 v2) = IApp (eval v1) (eval v2)
eval t =t

fullEval :: ITerm -> ITerm
fullEval t =fullEval' t$ eval t
  where
    fullEval' t1 t2 | t1 == t2 = t2
    fullEval' _ t2 = fullEval' t2 $ eval t2
