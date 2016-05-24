module Main

-- %default total

data Tree : (height : Nat) -> (k : Type) -> (v : Type) -> Type where
  Empty : Tree Z k v
  Node : (key : k)
      -> (val : v)
      -> (l : Tree lh k v)
      -> (r : Tree rh k v)
      -> Tree (S (maximum lh rh)) k v

test : Tree 2 Int Int
test = Node 0 0 (Node 1 1 Empty Empty) Empty

data Direction = Left | Right

heightChild : Direction -> Tree (S h) k v -> Nat
heightChild Left (Node key val l {lh} r) = lh
heightChild Right (Node key val l r {rh}) = rh

child : (d : Direction) -> (tree : Tree (S h) k v) -> Tree (heightChild d tree) k v
child Left (Node key val l r) = l
child Right (Node key val l r) = r

yo : (x : Nat) -> (y : Nat) -> maximum x y = x -> maximum (S x) y = S (maximum x y)
yo Z Z Refl = Refl
yo Z (S _) Refl impossible
yo (S k) Z Refl = Refl
yo (S k) (S j) prf = cong (yo k j (succInjective (maximum k j) k prf))

covering
rotRight : (tree : Tree (S h) k v) -> Not (heightChild Left tree = Z) -> Tree ?hrotr k v
rotRight (Node key val l r) x = ?rotRight_rhs_1

-- partial
-- rotLeft : (tree : Tree (S (S h)) k v) -> Tree ?hrotl k v
-- rotLeft (Node key val l (Node rkey rval rl rr))
--   = Node rkey rval (Node key val l rl) rr

-- rotIsInvertible : (tree : Tree h k v) -> rotRight (rotLeft tree) = tree
-- rotIsInvertible tree = ?rotIsInvertible_rhs

closeTo : Nat -> Nat -> Bool
closeTo Z Z = True
closeTo Z (S Z) = True
closeTo Z (S (S k)) = False
closeTo (S Z) Z = True
closeTo (S (S k)) Z = False
closeTo (S k) (S j) = closeTo k j

max : Ord k => (tree : Tree (S h) k v) -> k
max (Node key val _ Empty) = key
max (Node key val _ r@(Node _ _ _ _)) = max r

min : Ord k => (tree : Tree (S h) k v) -> k
min (Node key val Empty _) = key
min (Node key val l@(Node _ _ _ _) _) = min l

isBalanced : Tree h k v -> Bool
isBalanced Empty = True
isBalanced (Node key val l {lh} r {rh})
  = (lh `closeTo` rh) && isBalanced l && isBalanced r

balanced : Tree h k v -> Type
balanced tree = isBalanced tree = True

idPreserveBalance : (tree : Tree h k v) -> balanced tree -> balanced (id tree)
idPreserveBalance tree prop = prop

-- idPreserveBalance : (x : Tree h k v) -> (isBalanced x = isBalanced (id x))
-- idPreserveBalance Empty = Refl
-- idPreserveBalance (Node key val l r) = Refl


main : IO ()
main = do
  putStrLn "yo"
