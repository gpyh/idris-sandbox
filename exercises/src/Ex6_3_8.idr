module Ex6_3_8

import Data.Vect

%default total

infixr 5 .+.

data Schema = SString | SInt | (.+.) Schema Schema

SchemaType : Schema -> Type
SchemaType SString = String
SchemaType SInt = Int
SchemaType (x .+. y) = (SchemaType x, SchemaType y)

record DataStore where
  constructor MkDataStore
  schema : Schema
  size : Nat
  items : Vect size (SchemaType schema)

addToStore : (store : DataStore) -> SchemaType (schema store) -> DataStore
addToStore (MkDataStore schema _ is) i = MkDataStore schema _ (addToData is)
where
  addToData : Vect n (SchemaType schema) -> Vect (S n) (SchemaType schema)
  addToData Nil = [i]
  addToData (x :: xs) = x :: addToData xs

display : SchemaType schema -> String
display {schema = SString} item = item
display {schema = SInt} item = show item
display {schema = (x .+. y)} (itemx, itemy)
  = (display itemx) ++ ", " ++ (display itemy)

getEntry : (pos : Integer) -> (store : DataStore) ->  Maybe (String, DataStore)
getEntry pos store
  = case integerToFin pos (size store) of
        Nothing =>
          Just ("Out of range\n", store)
        Just p => Just (display (index p (items store)) ++ "\n", store)

data Command : (schema : Schema) -> Type where
  SetSchema : (newschema : Schema) -> Command schema
  Add : SchemaType schema -> Command schema
  Get : Integer -> Command schema
  Size : Command schema
  Quit : Command schema

parsePrefix : (schema : Schema) -> (input : String) -> Maybe (SchemaType schema, String)
parsePrefix SString input = getQuoted (unpack input)
  where
    getQuoted : List Char -> Maybe (String, String)
    getQuoted ('"' :: xs)
      = case span (/= '"') xs of
             (quoted, '"' :: rest) => Just (pack quoted, ltrim (pack rest))
             _ => Nothing
    getQuoted _ = Nothing
parsePrefix SInt input
  = case span isDigit input of
       ("", rest) => Nothing
       (num, rest) => Just (cast num, ltrim rest)
parsePrefix (schemal .+. schemar) input
  = case parsePrefix schemal input of
       Nothing => Nothing
       Just (lval, input')
         => case parsePrefix schemar input' of
                 Nothing => Nothing
                 Just (rval, input'') => Just((lval, rval), input'')

parseBySchema : (schema : Schema) -> String -> Maybe (SchemaType schema)
parseBySchema schema input
  = case parsePrefix schema input of
       Just (res, "") => Just res
       Just _ => Nothing
       Nothing => Nothing

parseCommand : (schema : Schema) -> (cmd : String) -> (arg : String) -> Maybe (Command schema)
parseCommand _ "add" str
  = case parseBySchema schema str of
       Nothing => Nothing
       Just strOk => Just (Add strOk)
parseCommand _ "get" val
  = case all isDigit (unpack val) of
       False => Nothing
       True => Just (Get (cast val))
parseCommand _ "size" "" = Just Size
parseCommand _ "quit" "" = Just Quit
parseCommand _ _ _ = Nothing

parse : (schema : Schema) -> (input : String) -> Maybe (Command schema)
parse schema input
  = case span (/= ' ') input of
       (cmd, arg) => parseCommand schema cmd (ltrim arg)

getSize : (store : DataStore) -> Maybe (String, DataStore)
getSize store = Just ("Number of items: " ++ show (size store) ++ "\n", store)

placeholder : a -> String
placeholder _ = "yo"

partial
interactiveInsert : IO()
interactiveInsert = replWith (MkDataStore SString _ []) "Command: " processInput where
  processInput : DataStore -> String -> Maybe (String, DataStore)
  processInput store input
    = case parse (schema store) input of
         Nothing => Just ("Invalid command\n", store)
         Just (SetSchema newschema ) => ?todo
         Just (Add item) =>
            Just ("ID " ++ show (size store) ++ "\n", addToStore store item)
         Just (Get pos) => getEntry pos store
         Just Size => getSize store
         Just Quit => Nothing

