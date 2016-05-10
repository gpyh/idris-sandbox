module Ex3_3_3

import Data.Vect

Matrix : Nat -> Nat -> Type -> Type
Matrix n m a = Vect m (Vect n a) 

transpose_mat : Matrix n m a -> Matrix m n a
transpose_mat Nil = replicate _ Nil
transpose_mat (col::rest) = zipWith (::) col (transpose_mat rest)

addMatrix : Num a => Matrix n m a -> Matrix n m a -> Matrix n m a
addMatrix = zipWith (zipWith (+))

multMatrix : Num a => Matrix n m a -> Matrix m p a -> Matrix n p a
multMatrix u v = map (\col => map (\lin => sum $ zipWith (*) lin col) (transpose u)) v

