import Control.Monad
import Control.Monad.Reader
import Data.Bits
import Data.List
import Data.Maybe (fromJust)
import Data.Word (Word64)
import Text.Printf (printf)

data Program = Program Identifier Expression
data Expression = Zero | One
                | Variable Identifier
                | If Expression Expression Expression
                | Fold Expression Expression Identifier Identifier Expression
                | UnaryForm UnaryOp Expression
                | BinaryForm BinaryOp Expression Expression

data UnaryOp = UnaryOp String (Word64 -> Word64)
data BinaryOp = BinaryOp String (Word64 -> Word64 -> Word64)

-- We never need more than 3 variables, and ziv likes brevity....
data Identifier = X | Y | Z deriving (Eq, Show)

instance Show Program where
  show (Program param body) =
    printf "(lambda (%s) %s)" (show param) (show body)

instance Show Expression where
  show Zero = "0"
  show One  = "1"
  show (Variable name) = show name

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

-- Because we only ever have 3 variables, a list is fine.
type Context a = Reader [(Identifier,Word64)] a

-- If this fails because lookup gives Nothing, the program is malformed.
getVar :: Identifier -> Context Word64
getVar x = asks $ fromJust . lookup x

putVar :: Identifier -> Word64 -> Context a -> Context a
putVar param value = local ((param,value):)

toBytes :: Word64 -> [Word64]
toBytes = take 8 . toBytesH where
    toBytesH x = let (d,m) = x `divMod` 256 in m : toBytesH d

eval :: Program -> Word64 -> Word64
eval (Program param body) value = flip runReader [] $ putVar param value $ (eeval body)

eeval :: Expression -> Context Word64
eeval Zero = return 0
eeval One = return 1
eeval (Variable x) = getVar x
eeval (If cond zero nonzero) = do
  c <- eeval cond
  eeval $ if c == 0 then zero else nonzero
eeval (Fold bytes initial param1 param2 body) = do
  bs <- eeval bytes
  i <- eeval initial
  let op acc b = putVar param2 acc . putVar param1 b $ eeval body
  foldM op i . reverse . toBytes $ bs
eeval (UnaryForm (UnaryOp _ op) arg) = liftM op (eeval arg)
eeval (BinaryForm (BinaryOp _ op) arg1 arg2) = liftM2 op (eeval arg1) (eeval arg2)

bnot = UnaryForm $ UnaryOp "shl1" $ complement
shl1 = UnaryForm $ UnaryOp "shl1" $ flip shiftL 1
shr1 = UnaryForm $ UnaryOp "shr1" $ flip shiftR 1
shr4 = UnaryForm $ UnaryOp "shr4" $ flip shiftR 4
shr16 = UnaryForm $ UnaryOp "shr16" $ flip shiftR 16
band = BinaryForm $ BinaryOp "and" (.&.)
bor = BinaryForm $ BinaryOp "or" (.|.)
bxor = BinaryForm $ BinaryOp "xor" xor
plus = BinaryForm $ BinaryOp "plus" (+)
x = Variable X
y = Variable Y
z = Variable Z

main = print $ eval
       (Program X
        (Fold x Zero Y Z
         (plus y z)))
       363
