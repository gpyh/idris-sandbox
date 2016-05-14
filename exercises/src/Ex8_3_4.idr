module Ex8_3_4

import Data.Vect
import Ex8_1_7

head_unequal : DecEq a => {xs : Vect n a} -> {ys : Vect n a} ->
               (contra : (x = y) -> Void) -> ((x :: xs) = (y :: ys)) -> Void
head_unequal contra Refl = contra Refl


tail_unequal : DecEq a => {xs : Vect n a} -> {ys : Vect n a} ->
               (contra : (xs = ys) -> Void) -> ((x :: xs) = (y :: ys)) -> Void        
tail_unequal contra Refl = contra Refl

decEqCons : DecEq a => {xs : Vect n a} -> {ys : Vect n a} ->
            Dec (x = y) -> Dec (xs = ys) -> Dec (x :: xs = y :: ys)
decEqCons (Yes Refl) (Yes Refl) = Yes Refl
decEqCons (Yes prf) (No contra) = No (tail_unequal contra)
decEqCons (No contra) _ = No (head_unequal contra)

[MyVectDecEq] DecEq a => DecEq (Vect n a) where
  decEq [] [] = Yes Refl
  decEq (x :: xs) (y :: ys) = decEqCons (decEq x y) (decEq xs ys)

