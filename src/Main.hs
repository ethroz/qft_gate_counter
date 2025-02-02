module Main where

import System.Environment ( getArgs )
import System.Random ( newStdGen )

import Quipper
    ( comment,
      controlled,
      hadamard,
      rGate,
      print_generic,
      qubit,
      Circ,
      Qubit,
      Format(GateCount) )
import Quipper.Libraries.Synthesis ( digits )
import Quipper.Libraries.Decompose ( decompose_generic, GateBase(Standard) )
import Quipper.Utils.RandomSource ( RandomSource(RandomSource) )

myQftImpl :: [Qubit] -> Circ [Qubit]
myQftImpl [] = return []
myQftImpl (x:xs) = do
    xs' <- myQftImpl xs
    xs'' <- rotations (length xs) x xs'
    x' <- hadamard x
    return (x':xs'')
    where
        rotations :: Int -> Qubit -> [Qubit] -> Circ [Qubit]
        rotations _ _ [] = return []
        rotations n c (q:qs) = do
            q' <- rGate (n + 1 - length qs) q `controlled` c
            qs' <- rotations n c qs
            return (q':qs')

myQft :: [Qubit] -> Circ [Qubit]
myQft qs = do
    comment "Reverse"
    let qs' = reverse qs
    myQftImpl qs'

myApp :: [String] -> IO ()
myApp args = do
    let (size, numDigits) = case args of
            [s, d] -> (read s, read d)
            _ -> error "Usage: <program> <size> <digits>"
    g <- newStdGen
    let circ = myQft
    let decomposed_circ = decompose_generic (Standard (numDigits * digits) (RandomSource g)) circ
    print_generic GateCount circ (replicate size qubit)
    print_generic GateCount decomposed_circ (replicate size qubit)

main :: IO ()
main = do
    args <- getArgs
    myApp args
