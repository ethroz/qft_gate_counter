module Main where

import System.Environment ( getArgs )
import System.Random ( newStdGen, StdGen )

import Text.Printf ( printf )

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
import Quipper.Utils.RandomSource ( RandomSource(RandomSource) )
import Quipper.Libraries.Decompose.GateBase
    ( decompose_generic,
      GateBase(Standard, CliffordT, Strict, Approximate) )
import Quipper.Libraries.Decompose (Precision)

-- myQftImpl :: [Qubit] -> Circ [Qubit]
-- myQftImpl [] = return []
-- myQftImpl (x:xs) = do
--     xs' <- myQftImpl xs
--     xs'' <- rotations (length xs) x xs'
--     x' <- hadamard x
--     return (x':xs'')
--     where
--         rotations :: Int -> Qubit -> [Qubit] -> Circ [Qubit]
--         rotations _ _ [] = return []
--         rotations n c (q:qs) = do
--             q' <- rGate (n + 1 - length qs) q `controlled` c
--             qs' <- rotations n c qs
--             return (q':qs')

-- myQft :: [Qubit] -> Circ [Qubit]
-- myQft qs = do
--     comment "Reverse"
--     let qs' = reverse qs
--     myQftImpl qs'


aqftImpl :: Int -> [Qubit] -> Circ [Qubit]
aqftImpl _ [] = return []
aqftImpl approx (x:xs) = do
    xs' <- aqftImpl approx xs
    xs'' <- rotations (length xs) approx x xs'
    x' <- hadamard x
    return (x':xs'')
    where
        rotations :: Int -> Int -> Qubit -> [Qubit] -> Circ [Qubit]
        rotations _ _ _ [] = return []
        rotations n approx c (q:qs) = do
            let l = n - length qs
            if l < approx
                then do
                    q' <- rGate (l + 1) q `controlled` c
                    qs' <- rotations n approx c qs
                    return (q':qs')
                else return (q:qs)

aqft :: Int -> [Qubit] -> Circ [Qubit]
aqft approx qs = do
    comment "Reverse"
    let qs' = reverse qs
    aqftImpl approx qs'

aqftError :: Int -> Int -> Double
aqftError n m = sum
    [ 2 * sin (pi * 2 ** (-fromIntegral j)) * fromIntegral (n - j + 1)
    | j <- [m + 1 .. n]
    ]

createAllAqft :: Int -> Double -> [([Qubit] -> Circ[Qubit], Double, Double)]
createAllAqft n error =
    [ (aqft m, aqftErr, error - aqftErr)
    | m <- [n, n-1..1],
    let aqftErr = aqftError n m,
        error - aqftErr > 0
    ]

printCircuit :: Int -> (Precision -> GateBase) -> ([Qubit] -> Circ[Qubit], Double, Double) -> IO ()
printCircuit size base (circ, aqftErr, decompErr) = do
    printf "\nAQFT Error:   %f\n" aqftErr
    printf   "Decomp Error: %f\n" decompErr
    let precision = (-log decompErr) * digits
    let decompCirc = decompose_generic (base precision) circ
    putStrLn "Circuit:"
    print_generic GateCount decompCirc (replicate size qubit)

baseFromString :: String -> StdGen -> Precision -> GateBase
baseFromString baseStr g precision = do
    case baseStr of
        "CliffordT" -> CliffordT True precision (RandomSource g)
        "Standard" -> Standard precision (RandomSource g)
        "Strict" -> Strict precision (RandomSource g)
        "Approximate" -> Approximate True precision (RandomSource g)
        _ -> error "Unknown base"    

main :: IO ()
main = do
    args <- getArgs
    let (size, baseStr, digits) = case args of
            [a, b, c] -> (read a, b, read c)
            _ -> error "Usage: <program> <size> <base> <digits>"
    g <- newStdGen
    let base = baseFromString baseStr g
    let error = 10 ** (-digits)
    let circuits_with_errors = createAllAqft size error
    mapM_ (printCircuit size base) circuits_with_errors
