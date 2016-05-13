module Ex7_1_6

data Shape = Triangle Double Double
           | Rectangle Double Double
           | Circle Double

area : Shape -> Double
area (Triangle x y) = (x * y) / 2
area (Rectangle x y) = x * y
area (Circle r) = pi * (r `pow` 2)

Eq Shape where
  (==) (Triangle x1 y1) (Triangle x2 y2) = (x1 == x2) && (y1 == y2)
  (==) (Rectangle x1 y1) (Rectangle x2 y2) = (x1 == x2) && (y1 == y2)
  (==) (Circle r1) (Circle r2) = (r1 == r2)
  (==) _ _ = False

Ord Shape where
  compare s1 s2 = compare (area s1) (area s2)
