module Ex6_2_3

import Data.Vect

Matrix : Nat -> Nat -> Type
Matrix n m = Vect m (Vect n Double)

data Format = Number Format
            | Chr Format
            | Dbl Format
            | Str Format
            | Lit String Format
            | End

PrintfType : Format -> Type
PrintfType (Number fmt) = (i : Int) -> PrintfType fmt
PrintfType (Chr fmt) = (c : Char) -> PrintfType fmt
PrintfType (Dbl fmt) = (d : Double) -> PrintfType fmt
PrintfType (Str fmt) = (str : String) -> PrintfType fmt
PrintfType (Lit str fmt) = PrintfType fmt
PrintfType End = String

-- strAppend : Show a => Format -> String -> a -> String
-- strAppend fmt acc toAdd = printfFmt desc fmt (acc ++ show toAdd)

printfFmt : (fmt : Format) -> (acc : String) -> PrintfType fmt
printfFmt f acc =
  case f of
       (Number fmt) => strAppend fmt
       (Chr fmt) => strAppend fmt
       (Dbl fmt) => strAppend fmt
       (Str fmt) => strAppend fmt
       (Lit lit fmt) => strAppend fmt lit
       End => acc
  where
    strAppend : Show a => (fmt : Format) -> a -> PrintfType fmt
    strAppend fmt toAdd = printfFmt fmt (acc ++ show toAdd)

TupleVect : Nat -> Type -> Type
TupleVect Z a = ()
TupleVect (S k) a = (a, TupleVect k a)

test : TupleVect 4 Nat
test = (1, 2, 3, 4, ())
