module Ex6_3_8

import Data.Vect

%default total

infixr 5 .+.

data Schema = SString | SInt | SChar | (.+.) Schema Schema

SchemaType : Schema -> Type
SchemaType SString = String
SchemaType SInt = Int
SchemaType SChar = Char
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
display {schema = SChar} item = show item
display {schema = (x .+. y)} (itemx, itemy)
  = (display itemx) ++ ", " ++ (display itemy)

getEntry : (pos : Integer) -> (store : DataStore) ->  Maybe (String, DataStore)
getEntry pos store
  = case integerToFin pos (size store) of
        Just p => Just (display (index p (items store)) ++ "\n", store)
        Nothing => Just ("Out of range\n", store)

data Command : (schema : Schema) -> Type where
  SetSchema : (newschema : Schema) -> Command schema
  Add : SchemaType schema -> Command schema
  Get : Integer -> Command schema
  GetAll : Command schema
  Size : Command schema
  Quit : Command schema

getAll : (store : DataStore) -> Maybe (String, DataStore)
getAll store
  = if (size store) == 0
       then Just ("The store is empty", store)
       else Just (foldr ((++) . display) "" (items store), store)

getSurroundedBy : Char -> Char -> List Char -> Maybe (List Char, List Char)
getSurroundedBy _ _ Nil  = Nothing
getSurroundedBy before after (x :: xs)
  = if x /= before
       then Nothing
       else case span (/= after) xs of
                 (enclosed, _ :: rest) => Just (enclosed, unpack $ ltrim (pack rest))
                 _ => Nothing

-- List Char allows to use getSurroundedBy for SChar and SString but it sucks
-- Use String instead
parsePrefix' : (schema : Schema) -> (input : List Char) -> Maybe (SchemaType schema, List Char)
parsePrefix' SString input = do
  (quoted, rest) <- getSurroundedBy '"' '"' input
  pure (pack quoted, rest)
parsePrefix' SInt input
  = case span isDigit input of
         (Nil, rest) => Nothing
         (num, rest) => Just (cast $ pack num, unpack $ ltrim $ pack rest)
parsePrefix' SChar input
  = case getSurroundedBy '\'' '\'' input of
         Just ([char], rest) => Just (char, rest)
         Just (['\\', '\''], rest) => Just ('\'', rest)
         _ => Nothing 
parsePrefix' (schemal .+. schemar) input = do
  (lval, input') <- parsePrefix' schemal input
  (rval, input'') <- parsePrefix' schemar input'
  pure ((lval, rval), input'')

parsePrefix : (schema : Schema) -> (input : String) -> Maybe (SchemaType schema, String)
parsePrefix schema input = do
  (pre, suf) <- parsePrefix' schema (unpack input)
  pure (pre, pack suf)

readSchemaAtom : String -> Maybe Schema
readSchemaAtom "String" = Just SString
readSchemaAtom "Int" = Just SInt
readSchemaAtom _ = Nothing

parseSchema : List String -> Maybe Schema
parseSchema (x :: Nil) = Just !(readSchemaAtom x)
parseSchema (x :: xs) = do
  x_sch <- readSchemaAtom x
  xs_sch <- parseSchema xs
  pure (x_sch .+. xs_sch)
parseSchema _ = Nothing

parseBySchema : (schema : Schema) -> String -> Maybe (SchemaType schema)
parseBySchema schema input = do
  (res, "") <- parsePrefix schema input | _ => Nothing
  pure res

parseCommand : (schema : Schema) -> (cmd : String) -> (arg : String) -> Maybe (Command schema)
parseCommand _ "schema" sch = Just (SetSchema !(parseSchema (words sch)))
parseCommand _ "add" str = Just (Add !(parseBySchema schema str))
parseCommand _ "get" val
  = case all isDigit (unpack val) of
       False => Nothing
       True => Just (Get (cast val))
parseCommand _ "size" "" = Just Size
parseCommand _ "quit" "" = Just Quit
parseCommand _ _ _ = Nothing

parse : (schema : Schema) -> (input : String) -> Maybe (Command schema)
parse schema input
  = let (cmd, arg) = span (/= ' ') input in
        parseCommand schema cmd (ltrim arg)

getSize : (store : DataStore) -> Maybe (String, DataStore)
getSize store = Just ("Number of items: " ++ show (size store) ++ "\n", store)

setSchema : (store : DataStore) -> Schema -> Maybe DataStore
setSchema store schema
  = if (size store) == 0
       then Just (MkDataStore schema _ [])
       else Nothing

partial
interactiveInsert : IO()
interactiveInsert = replWith (MkDataStore SString _ []) "Command: " processInput where
  processInput : DataStore -> String -> Maybe (String, DataStore)
  processInput store input
    = case parse (schema store) input of
         Nothing => Just ("Invalid command\n", store)
         Just (SetSchema newschema) =>
            case setSchema store newschema of
                 Nothing => Just ("Can't update schema\n", store)
                 (Just store') => Just ("OK\n", store')
         Just (Add item) =>
            Just ("ID " ++ show (size store) ++ "\n", addToStore store item)
         Just (Get pos) => getEntry pos store
         Just GetAll => getAll store
         Just Size => getSize store
         Just Quit => Nothing

