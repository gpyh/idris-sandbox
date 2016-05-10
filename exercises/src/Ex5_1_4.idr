module Ex5_1_4

length2StrDo : IO ()
length2StrDo = do
  x <- getLine
  y <- getLine
  putStrLn . show $ max (length x) (length y)

length2StrBind : IO ()
length2StrBind =
  getLine >>=
    \x => getLine >>=
      \y => putStrLn . show $ max (length x) (length y)
