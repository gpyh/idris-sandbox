module Ex7_2_4

%access public export

%default total

data Expr : (num : Type) -> Type where
  Val : (x : num) -> Expr num
  Add : (l : Expr num) -> (r : Expr num) -> Expr num
  Sub : (l : Expr num) -> (r : Expr num) -> Expr num
  Mul : (l : Expr num) -> (r : Expr num) -> Expr num
  Div : (l : Expr num) -> (r : Expr num) -> Expr num
  Abs : (e : Expr num) -> Expr num

eval : (Neg num, Integral num) => Expr num -> num
eval (Val x) = x
eval (Add l r) = (eval l) + (eval r)
eval (Sub l r) = (eval l) - (eval r)
eval (Mul l r) = (eval l) * (eval r)
eval (Div l r) = (eval l) `div` (eval r)
eval (Abs e) = abs (eval e)

Num ty => Num (Expr ty) where
  (+) = Add
  (*) = Mul
  fromInteger = Val . fromInteger

Neg ty => Neg (Expr ty) where
  negate x = 0 - x
  (-) = Sub
  abs = Abs

Show ty => Show (Expr ty) where
  show (Val x) = show x
  show (Add l r) = "(" ++ show l ++ "+" ++ show r ++ ")"
  show (Sub l r) = "(" ++ show l ++ "-" ++ show r ++ ")"
  show (Mul l r) = "(" ++ show l ++ "×" ++ show r ++ ")"
  show (Div l r) = "(" ++ show l ++ "÷" ++ show r ++ ")"
  show (Abs e) = "|" ++ show e ++ "|"

(Neg ty, Integral ty, Eq ty) => Eq (Expr ty) where
  (==) e1 e2 = (eval e1) == (eval e2)

(Neg ty, Integral ty) => Cast (Expr ty) ty where
  cast = eval

