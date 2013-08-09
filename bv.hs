import Data.Word
import Text.Printf

data Program = Program Identifier Expression
data Expression = Zero | One
                | Variable Identifier
                | If Expression Expression Expression
                | Fold Expression Expression Identifier Identifier Expression
                | UnaryForm UnaryOp Expression
                | BinaryForm BinaryOp Expression Expression

data UnaryOp = UnaryOp String (Word64 -> Word64)
data BinaryOp = BinaryOp String (Word64 -> Word64 -> Word64)

type Identifier = String

instance Show Program where
  show (Program param body) =
    printf "(lambda (%s) %s)" param (show body)

instance Show Expression where
  show Zero = "0"
  show One  = "1"
  show (Variable name) = name

  show (If cond zero nonzero) =
    printf "(if0 %s %s %s)" (show cond) (show zero) (show nonzero)

  show (Fold bytes initial param1 param2 body) =
    printf "(fold %s %s (lambda (%s %s) %s))" (show bytes) (show initial) (show param1) (show param2) (show body)

  show (UnaryForm op arg) =
    printf "(%s %s)" (show op) (show arg)

  show (BinaryForm op arg1 arg2) =
    printf "(%s %s %s)" (show op) (show arg1) (show arg2)

instance Show UnaryOp where
  show (UnaryOp name _) = name

instance Show BinaryOp where
  show (BinaryOp name _) = name

psize :: Program -> Int
psize (Program param body) = 1 + size body

size :: Expression -> Int
size Zero = 1
size One = 1
size (Variable _) = 1
size (If cond zero nonzero) = 1 + size cond + size zero + size nonzero
size (Fold bytes initial param1 param2 body) = 2 + size bytes + size initial + size body
size (UnaryForm op arg) = 1 + size arg
size (BinaryForm op arg1 arg2) = 1 + size arg1 + size arg2

main = do
  let program = Program "x" (If (Variable "x") One Zero)
  putStrLn $ show program
  putStrLn $ show $ psize program
