import Control.Monad
import Control.Monad.Reader
import Data.Bits
import Data.Char (toLower)
import Data.Maybe (fromJust)
import Data.Word (Word64)
import Text.Printf (printf)

data Program = Program Identifier Expression
data Expression = Zero | One
                | Var Identifier
                | If Expression Expression Expression
                | Fold Expression Expression Identifier Identifier Expression
                | UF UnaryOp Expression -- unary form
                | BF BinaryOp Expression Expression -- binary form
data UnaryOp = Not | Shl1 | Shr1 | Shr4 | Shr16 deriving (Eq, Show)
data BinaryOp = And | Or | Xor | Plus deriving (Eq, Show)
-- We never need more than 3 variables, and ziv likes brevity....
data Identifier = Input | Byte | Acc deriving Eq

instance Show Program where
  show (Program param body) =
    printf "(lambda (%s) %s)" (show param) (show body)

instance Show Expression where
  show Zero = "0"
  show One  = "1"
  show (Var name) = show name

  show (If cond zero nonzero) =
    printf "(if0 %s %s %s)" (show cond) (show zero) (show nonzero)

  show (Fold bytes initial param1 param2 body) =
    printf "(fold %s %s (lambda (%s %s) %s))" (show bytes) (show initial) (show param1) (show param2) (show body)

  show (UF op arg) =
    printf "(%s %s)" (map toLower $ show op) (show arg)

  show (BF op arg1 arg2) =
    printf "(%s %s %s)" (map toLower $ show op) (show arg1) (show arg2)

instance Show Identifier where
  show Input = "input"
  show Byte = "byte"
  show Acc = "acc"

psize :: Program -> Int
psize (Program param body) = 1 + size body

size :: Expression -> Int
size Zero = 1
size One = 1
size (Var _) = 1
size (If cond zero nonzero) = 1 + size cond + size zero + size nonzero
size (Fold bytes initial param1 param2 body) = 2 + size bytes + size initial + size body
size (UF op arg) = 1 + size arg
size (BF op arg1 arg2) = 1 + size arg1 + size arg2

-- Because we only ever have 3 variables, a list is fine.
type Context a = Reader [(Identifier,Word64)] a

-- If this fails because lookup gives Nothing, the program is malformed.
getVar :: Identifier -> Context Word64
getVar x = asks $ fromJust . lookup x

putVar :: Identifier -> Word64 -> Context a -> Context a
putVar param value = local ((param,value):)

-- Least significant is first.
-- e.g. toBytes 1023 = [255,3,0,0..]
toBytes :: Word64 -> [Word64]
toBytes = take 8 . toBytesH where
    toBytesH x = let (d,m) = x `divMod` 256 in m : toBytesH d

eval :: Program -> Word64 -> Word64
eval (Program param body) value = flip runReader [] $ putVar param value $ (eeval body)

eeval :: Expression -> Context Word64
eeval Zero = return 0
eeval One = return 1
eeval (Var x) = getVar x
eeval (If cond zero nonzero) = do
  c <- eeval cond
  eeval $ if c == 0 then zero else nonzero
eeval (Fold bytes initial param1 param2 body) = do
  bs <- eeval bytes
  i <- eeval initial
  -- We have a left fold in foldM, but they specify a right fold.
  -- Our list from toBytes is least-significant first, so we flip the arguments of the operation.
  -- That is, the accumulator (param2) is on the left, and the current byte (param1) is on the right.
  let op acc b = putVar param2 acc . putVar param1 b $ eeval body
  foldM op i . toBytes $ bs
eeval (UF op arg) = liftM (applyUnary op) (eeval arg)
eeval (BF op arg1 arg2) = liftM2 (applyBinary op) (eeval arg1) (eeval arg2)

applyUnary :: UnaryOp -> Word64 -> Word64
applyUnary Not = complement
applyUnary Shl1 = flip shiftL 1
applyUnary Shr1 = flip shiftR 1
applyUnary Shr4 = flip shiftR 4
applyUnary Shr16 = flip shiftR 16

applyBinary :: BinaryOp -> Word64 -> Word64 -> Word64
applyBinary And = (.&.)
applyBinary Or = (.|.)
applyBinary Xor = xor
applyBinary Plus = (+)

-- Aliases because ziv is lazy.
program = Program Input
fold bytes initial = Fold bytes initial Byte Acc
tfold = program . fold (Var Input) Zero

-- Reverses the bytes of the input.
-- The program is equivalent to foldl (\xs x -> xs*256 + x) 0 . toBytes
main = print $ eval (tfold
                     (BF Plus (Var Byte)
                      (UF Shl1 . UF Shl1 . UF Shl1 . UF Shl1 . UF Shl1 . UF Shl1 . UF Shl1 . UF Shl1 $
                       (Var Acc)))) 2553
