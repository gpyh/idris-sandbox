module Ex5_2_4

import System

parseNat : String -> Maybe Nat
parseNat str = if all isDigit (unpack str) then Just (cast str) else Nothing

readNat : IO (Maybe Nat)
readNat = getLine >>= (\str => return (parseNat str))

simpleGuess : (target : Nat) -> IO ()
simpleGuess target = do
  putStr "Please enter a natural number: "
  Just number <- readNat
    | Nothing => (putStrLn "Not a natural number" >>= retry)
  case compare number target of
       LT => putStrLn "Not enough... Try again!" >>= retry
       GT => putStrLn "Too much! Try again..." >>= retry
       EQ => putStrLn "This is it! You won!"
where
  retry : () -> IO ()
  retry _ = simpleGuess target 

randomNat : IO (Nat)
randomNat = time >>= (\t => return $ toNat t)

guess : (target : Nat) -> IO ()
guess target = guess' 0 where
  guess' : (attempts : Nat) -> IO ()
  guess' attempts = do
    putStr "Please enter a natural number: "
    Just number <- readNat
      | Nothing => (putStrLn "Please enter a natural number" >>= retry False)
    case compare number target of
         LT => putStrLn "Not enough... Try again!" >>= retry True
         GT => putStrLn "Too much! Try again..." >>= retry True
         EQ => putStrLn
          ("This is it! You won with " ++ show attempts ++ " attempts.")
  where
    retry : (valid : Bool) -> () -> IO ()
    retry True _ = guess' (S attempts)
    retry False _ = guess' attempts

play : IO ()
play = randomNat >>= guess

myrepl : String -> (String -> String) -> IO ()
myrepl prompt eval = do
  putStr prompt
  l <- getLine
  putStr (eval l)
  myrepl prompt eval

myreplWith : a -> String -> (a -> String -> String) -> IO ()
myreplWith state prompt eval = do
  putStr prompt
  l <- getLine
  putStr (eval state l)
  myreplWith state prompt eval
 
