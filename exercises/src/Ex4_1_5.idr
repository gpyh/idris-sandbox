module Ex4

import Data.Vect
import Data.Fin

%default total

-- Ex 4.1.5

data BSTree : Type -> Type where
  Empty : Ord a => BSTree a
  Node : Ord a =>
         (left : BSTree a) -> (val : a) -> (right : BSTree a) -> BSTree a

insert : a -> BSTree a -> BSTree a
insert x Empty = Node Empty x Empty
insert x node@(Node left val right) =
  case compare x val of
       LT => Node (insert x left) val right
       EQ => node
       GT => Node left val (insert x right)

listToTree : Ord a => List a -> BSTree a
listToTree = foldr insert Empty

treeToList : BSTree a -> List a
treeToList Empty = Nil
treeToList (Node left val right) = treeToList left ++ val::(treeToList right)

data Expr = Value Int
          | Add Expr Expr
          | Minus Expr Expr
          | Mult Expr Expr

evaluate : Expr -> Int
evaluate expr =
  case expr of
       (Value v) => v
       (Add x y) => use (+) x y
       (Minus x y) => use (-) x y
       (Mult x y) => use (*) x y
  where
    use : (Int -> Int -> Int) -> Expr -> Expr -> Int
    use f x y = f (evaluate x) (evaluate y)

maxMaybe : Ord a => Maybe a -> Maybe a -> Maybe a
maxMaybe Nothing my = my
maxMaybe mx Nothing = mx
maxMaybe (Just x) (Just y) = ifThenElse (x > y) (Just x) (Just y)

data Shape = Triangle Double Double
           | Rectangle Double Double
           | Circle Double

data Picture = Primitive Shape
             | Combine Picture Picture
             | Rotate Double Picture
             | Translate Double Double Picture

biggestTriangle : Picture -> Maybe Double
biggestTriangle (Primitive shape) =
  case shape of 
       (Triangle x y) => Just (x * y / 2)
       (Rectangle x y) => Nothing
       (Circle x) => Nothing
biggestTriangle (Combine pic1 pic2) =
  maxMaybe (biggestTriangle pic1) (biggestTriangle pic2)
biggestTriangle (Rotate angle pic) = biggestTriangle pic
biggestTriangle (Translate x y pic) = biggestTriangle pic


