module Ex7_3_4

import Ex7_2_4

%default total

Functor Expr where
  map f (Val x) = Val (f x)
  map f (Add l r) = Add (map f l) (map f r)
  map f (Sub l r) = Sub (map f l) (map f r)
  map f (Mul l r) = Mul (map f l) (map f r)
  map f (Div l r) = Div (map f l) (map f r)
  map f (Abs e) = Abs (map f e)

data Vect : (n : Nat) -> (a : Type) -> Type where
  Nil : Vect Z a
  (::) : a -> Vect n a -> Vect (S n) a

Eq a => Eq (Vect n a) where
  (==) Nil Nil = True
  (==) (x :: xs) (y :: ys) = (x == y) && (xs == ys)

Foldable (Vect n) where
  foldr f acc Nil = acc
  foldr f acc (x :: xs) = foldr f (f x acc) xs

