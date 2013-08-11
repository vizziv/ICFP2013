module Main where

import BV

-- Aliases because ziv is lazy.
program = Program Input
fold bytes initial = Fold bytes initial Byte Acc
tfold = program . fold (Var Input) Zero

-- Dummy main for now.
main = let f = tfold (BF Plus (Var Byte) (Var Acc)) in
       (putStrLn $ show f) >> putStrLn (show $ eval f $ 1024+32)
