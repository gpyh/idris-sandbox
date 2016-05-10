module Ex2

%default total

infixr 10 .>
(.>) : (a -> b) -> (b -> c) -> a -> c
(.>) g f = f . g

palindrome : String -> Bool
palindrome s = s == reverse s

palindromeN : Nat -> String -> Bool
palindromeN n str = ifThenElse (length str < n) False $
  (\s => s == reverse s) (toLower str)

counts : String -> (Nat, Nat)
counts str = (length $ words str, length str)

top_ten : Ord a => List a -> List a
top_ten = sort .> reverse .> take 10

over_length : Nat -> List String -> Nat
over_length n = foldr (\str, acc => (+) acc $ ifThenElse (n < length str) 1 0) 0

accord : Nat -> String -> String
accord n word = addSuffix $ show n ++ " " ++ word where
  addSuffix s = if n <= 1 then s else s ++ "s"

partial palindromeRepl : IO ()
palindromeRepl = repl "Enter a string: "
  (\str => case (palindrome str) of
    True => str ++ " is a palindrome"
    False => str ++ " is not a palindrom\n" ) 

partial countsRepl : IO ()
countsRepl = repl "Enter a string: " $ counts .>
  (\(wordc, len) => "The string has " ++
                   accord wordc "word" ++ " and " ++
                   accord len "character" ++ "\n")

partial main : IO ()
main = countsRepl
      
