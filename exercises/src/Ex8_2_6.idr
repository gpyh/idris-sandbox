module Ex8_2_6

import Data.Vect

my_plusCommutes : (n : Nat) -> (m : Nat) -> n + m = m + n
my_plusCommutes n Z = plusZeroRightNeutral n
my_plusCommutes n (S k) = rewrite my_plusCommutes k n in
                                 sym $ plusSuccRightSucc n k

my_reverse : Vect n a -> Vect n a
my_reverse xs = reverse' [] xs
  where reverse' : Vect n a -> Vect m a -> Vect (n+m) a
        reverse' {n} acc [] = rewrite my_plusCommutes n 0 in acc
        reverse' {n} acc ((::) {k} x xs)
          = rewrite sym $ plusSuccRightSucc n k in (reverse' (x::acc) xs)  


