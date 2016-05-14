module Ex8_1_7

%default total
%access public

same_cons : {xs : List a} -> {ys : List a} ->
            xs = ys -> x :: xs = x :: ys
same_cons Refl = Refl

same_lists : {xs : List a} -> {ys : List a} ->
             x = y -> xs = ys -> x :: xs = y :: ys
same_lists Refl Refl = Refl

data ThreeEq : a -> b -> c -> Type where
  Proofs : a = b -> b = c -> ThreeEq a b c

allSameS : (x, y, z : Nat) -> ThreeEq x y z -> ThreeEq (S x) (S y) (S z)
allSameS x y z (Proofs eq_xy eq_yz) = Proofs (cong eq_xy) (cong eq_yz)

