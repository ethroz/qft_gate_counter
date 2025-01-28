module Main where

import System.Random

import Quipper
import Quipper.Libraries.Synthesis
import Quipper.Libraries.Decompose
import Quipper.Utils.RandomSource

my_qft_impl :: [Qubit] -> Circ [Qubit]
my_qft_impl [] = return []
my_qft_impl (x:xs) = do
    xs' <- my_qft_impl xs
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

my_qft :: [Qubit] -> Circ [Qubit]
my_qft qs = do
    comment "Reverse"
    let qs' = reverse qs
    qs' <- my_qft qs'
    return qs'

main :: IO ()
main = do
    g <- newStdGen
    let circ = my_qft
    let decomposed_circ = decompose_generic (Standard (6 * digits) (RandomSource g)) circ
    print_generic Preview circ (replicate 8 qubit)
    print_generic Preview decomposed_circ (replicate 8 qubit)
