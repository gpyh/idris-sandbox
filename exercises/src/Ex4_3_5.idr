module Ex4_3_5

import Data.Vect

%default total

data DataStore : Type where
  MkDataStore : (size : Nat) -> (items : Vect size String) -> DataStore

size : DataStore -> Nat
size (MkDataStore n _) = n

items : (store : DataStore) -> Vect (size store) String
items (MkDataStore _ is) = is

addToStore : DataStore -> String -> DataStore
addToStore (MkDataStore _ is) i = MkDataStore _ (addToData is) where
  addToData : Vect n String -> Vect (S n) String
  addToData Nil = [i]
  addToData (x :: xs) = x :: (addToData xs)

data Command = Add String
             | Get Integer
             | Size
             | Search String
             | Quit

parseCommand : (cmd : String) -> (arg : String) -> Maybe Command
parseCommand "add" str = Just (Add str)
parseCommand "get" val = case all isDigit (unpack val) of
                              False => Nothing
                              True => Just (Get (cast val))
parseCommand "size" "" = Just Size
parseCommand "search" keyword = Just (Search keyword)
parseCommand "quit" "" = Just Quit
parseCommand _ _ = Nothing

parse : (input : String) -> Maybe Command
parse input =
  case span (/= ' ') input of
       (cmd, arg) => parseCommand cmd (ltrim arg)


getEntry : (pos : Integer) -> (store : DataStore) ->  Maybe (String, DataStore)
getEntry pos store =
  case integerToFin pos (size store) of
        Nothing =>
          Just ("Out of range\n", store)
        (Just p) => Just (index p (items store) ++ "\n", store)

getSize : (store : DataStore) -> Maybe (String, DataStore)
getSize store = Just ("Number of items: " ++ show (size store) ++ "\n", store)

getSearchResults : (keyword : String) -> (store : DataStore) -> Maybe (String, DataStore)
getSearchResults "" store = Just ("Missing search entry\n", store)
getSearchResults keyword store =
  Just ("Found items\n" ++ display results ++ "\n", store) where
    display : (p : Nat ** Vect p String) -> String
    display (_ ** vec) = foldl (\acc, str => acc ++ "  " ++ str ++ "\n") "" vec
    results : (p : Nat ** Vect p String)
    results = filter (\str => keyword `isInfixOf` str) (items store)

getSearchResultsWithIndex : (keyword : String) -> (store : DataStore) -> Maybe (String, DataStore)
getSearchResultsWithIndex "" store = Just ("Missing search entry\n", store)
getSearchResultsWithIndex keyword store =
  Just ("Found items\n" ++ display results ++ "\n", store) where
    display : List (Nat, String) -> String
    display = foldr (\(i, str), acc => acc ++ "  at index " ++ show i ++ ": " ++ str ++ "\n") ""
    results : List (Nat, String)
    results = snd $ foldr f (0, []) (items store) where
      f : String -> (Nat, List (Nat, String)) -> (Nat, List (Nat, String))
      f str (i, acc) = (S i, if keyword `isInfixOf` str then (i, str) :: acc else acc)

partial
interactiveInsert : IO()
interactiveInsert = replWith (MkDataStore _ []) "Command: " processInput where
  processInput : DataStore -> String -> Maybe (String, DataStore)
  processInput store input =
    case parse input of
         Nothing => Just ("Invalid command\n", store)
         Just (Add item) =>
            Just ("ID " ++ show (size store) ++ "\n", addToStore store item)
         Just (Get pos) => getEntry pos store
         Just Size => getSize store
         Just (Search keyword) => getSearchResults keyword store
         Just Quit => Nothing

