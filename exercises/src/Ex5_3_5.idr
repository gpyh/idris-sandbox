module Ex5_3_5

import Data.Vect

%access public export

readToBlank : IO (List String)
readToBlank = do
  str <- getLine
  if str == "" then pure [] else do
    strs <- readToBlank
    pure (str :: strs)

readAndSave : IO ()
readAndSave = do
  lines <- readToBlank
  filepath <- getLine
  Right _ <- writeFile filepath (unlines lines)
    | Left fileError => putStrLn (show fileError)
  putStrLn "File written"

readVectFile : (filepath : String) -> IO (n ** Vect n String)
readVectFile filepath = do
  Right file <- openFile filepath Read
    | Left _ => pure (_ ** [])
  Just vec <- getLines file
    | Nothing => pure (_ ** [])
  pure vec
where
  getLines : File -> IO (Maybe (n ** Vect n String))
  getLines file = do
    Right line <- fGetLine file
      | Left _ => pure Nothing
    _ <- fEOF file
      | True => pure (Just (_ ** []))
    Just (n ** vec) <- getLines file
      | Nothing => pure Nothing
    pure (Just ((S n) ** (line :: vec)))
  
  -- let open =
  --   Right file <- openFile filename Append
  --     | Left fileError => putStrLn (show fileError)
  -- foldl (\app, line => app >>= addLine line) empty lines)
  -- putStrLn filename
-- where
  -- addLine : String -> IO ()
  -- addLine line = putStrLn "yo"
  -- addLine line = fPutStrLn file line
