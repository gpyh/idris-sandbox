module Ex3_2_4

import Data.Vect

%default total

mylength : List a -> Nat
mylength Nil = 0
mylength (_::xs) = S (mylength xs)

myreverse : List a -> List a
myreverse = foldl (\acc, e => e::acc) Nil

mymaplist : (a -> b) -> List a -> List b
mymaplist _ Nil = Nil
mymaplist f (x::xs) = (f x)::(mymaplist f xs)

mymapvect : (a -> b) -> Vect n a -> Vect n b
mymapvect _ Nil = Nil
mymapvect f (x::xs) = (f x)::(mymapvect f xs)

