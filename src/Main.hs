module Main where

import Aqft (aqft)
import CatalyticAqft (catalytic_aqft)
import Quipper (Circ, Qubit, qubit)
import Quipper.Internal.Printing
  ( Format (GateCount),
    print_generic,
  )
import Quipper.Libraries.Decompose (Precision)
import Quipper.Libraries.Decompose.GateBase
  ( GateBase (Approximate, CliffordT, Standard, Strict),
    decompose_generic,
  )
import Quipper.Libraries.Synthesis (digits)
import Quipper.Utils.RandomSource (RandomSource (RandomSource))
import System.Environment (getArgs)
import System.Random (StdGen, newStdGen)
import Text.Printf (printf)

circFromString :: String -> Int -> [Qubit] -> Circ [Qubit]
circFromString typeStr approx =
  case typeStr of
    "Aqft" -> aqft approx
    "CatAqft" -> catalytic_aqft approx
    _ -> error "Unknown Aqft type"

baseFromString :: String -> StdGen -> Precision -> GateBase
baseFromString baseStr g precision =
  case baseStr of
    "CliffordT" -> CliffordT True precision (RandomSource g)
    "Standard" -> Standard precision (RandomSource g)
    "Strict" -> Strict precision (RandomSource g)
    "Approximate" -> Approximate True precision (RandomSource g)
    _ -> error "Unknown base"

aqftError :: Int -> Int -> Double
aqftError n m =
  sum
    [ 2 * sin (pi * 2 ** (- fromIntegral j)) * fromIntegral (n - j + 1)
      | j <- [m + 1 .. n]
    ]

createAllAqft :: (Int -> ([Qubit] -> Circ [Qubit])) -> Int -> Double -> [([Qubit] -> Circ [Qubit], Double, Double)]
createAllAqft circFunc n error =
  [ (circFunc m, aqftErr, error - aqftErr)
    | m <- [n, n -1 .. 1],
      let aqftErr = aqftError n m,
      error - aqftErr > 0
  ]

printCircuit :: Int -> (Precision -> GateBase) -> ([Qubit] -> Circ [Qubit], Double, Double) -> IO ()
printCircuit size baseFunc (circ, aqftErr, decompErr) = do
  printf "\nAQFT Error:   %f\n" aqftErr
  printf "Decomp Error: %f\n" decompErr
  let precision = (- log decompErr) * digits
  let decompCirc = decompose_generic (baseFunc precision) circ
  putStrLn "Circuit:"
  print_generic GateCount decompCirc (replicate size qubit)

main :: IO ()
main = do
  args <- getArgs
  let (typeStr, size, baseStr, digits) = case args of
        [a, b, c, d] -> (a, read b, c, read d)
        _ -> error "Usage: <program> <type> <size> <base> <digits>"
  g <- newStdGen
  let circFunc = circFromString typeStr
  let baseFunc = baseFromString baseStr g
  let error = 10 ** (- digits)
  let circuits_with_errors = createAllAqft circFunc size error
  mapM_ (printCircuit size baseFunc) circuits_with_errors

-- main :: IO ()
-- main = do
--   args <- getArgs
--   let (size, approx) = case args of
--         [a, b] -> (read a, read b)
--         _ -> error "Usage: <program> <size> <approx>"
--   print_generic Preview catalytic_aqft approx (replicate size qubit)

-- lol :: [Qubit] -> [Qubit] -> Circ [Qubit]
-- lol x y =
--   if not (null x) && not (null y)
--     then do
--       comment_with_label "ENTER" (x ++ y) "qs"
--       let x' = qdint_of_qulist_lh x
--       let y' = qdint_of_qulist_lh y
--       (x'', y'') <- q_sub_in_place x' y'
--       let x_final = qulist_of_qdint_lh x''
--       let y_final = qulist_of_qdint_lh y''
--       comment_with_label "EXIT" (x_final ++ y_final) "qs"
--       return (x_final ++ y_final)
--     else return (x ++ y)

-- main :: IO ()
-- main = do
--   args <- getArgs
--   let (size1, size2) = case args of
--         [a, b] -> (read a, read b)
--         _ -> error "Usage: <program> <size1> <size2>"
--   print_generic Preview lol (replicate size1 qubit) (replicate size2 qubit)
