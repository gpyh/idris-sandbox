module Main

%default total

-- A binary tree without data
-- Height is encoded in the type
-- (it is actually height + 1)
data Tree : (height : Nat) -> Type where
  Empty : Tree Z
  Node : (l : Tree lh) -> (r : Tree rh) -> Tree (S (max lh rh))

-- To avoid repetition
data Direction = Left | Right

-- Capture the height of a child
heightChild : Direction -> Tree (S h) -> Nat
heightChild Left (Node l {lh} r) = lh
heightChild Right (Node l r {rh}) = rh

-- Simple getter
child : (d : Direction) -> (tree : Tree (S h)) -> Tree (heightChild d tree)
child Left (Node l r) = l
child Right (Node l r) = r

-- This has been written using interactive editing
-- (cases split consecutively)
-- Why is there `impossible` ?
nonsense : Tree h -> Tree h
nonsense Empty = ?nonsense_rhs_1
nonsense (Node Empty _) impossible
nonsense (Node (Node _ _) _) impossible

main : IO ()
main = do
  putStrLn "yo"
