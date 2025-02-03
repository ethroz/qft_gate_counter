module Main where

import Quipper
  ( Circ,
    Qubit,
    comment,
    controlled,
    hadamard,
    map_hadamard,
    qubit,
    rGate,
  )
import Quipper.Internal.Generic (with_ancilla_list)
import Quipper.Internal.Labels (comment_with_label)
import Quipper.Internal.Printing
  ( Format (GateCount, Preview),
    print_generic,
  )
import Quipper.Libraries.Arith (q_sub_in_place, qdint_of_qulist_lh, qulist_of_qdint_lh)
import Quipper.Libraries.Decompose (Precision)
import Quipper.Libraries.Decompose.GateBase
  ( GateBase (Approximate, CliffordT, Standard, Strict),
    decompose_generic,
  )
import Quipper.Libraries.Synthesis (digits)
import Quipper.Utils.RandomSource (RandomSource (RandomSource))
import System.Environment (getArgs)
import System.Random (StdGen)
import Text.Printf (printf)

aqftImpl :: Int -> [Qubit] -> Circ [Qubit]
aqftImpl _ [] = return []
aqftImpl approx (x : xs) = do
  xs <- aqftImpl approx xs
  xs <- rotations (length xs) approx x xs
  x <- hadamard x
  return (x : xs)
  where
    rotations :: Int -> Int -> Qubit -> [Qubit] -> Circ [Qubit]
    rotations _ _ _ [] = return []
    rotations n approx c (q : qs) = do
      let l = n - length qs
      if l < approx
        then do
          q <- rGate (l + 1) q `controlled` c
          qs <- rotations n approx c qs
          return (q : qs)
        else return (q : qs)

aqft :: Int -> [Qubit] -> Circ [Qubit]
aqft approx qs = do
  comment_with_label "ENTER: aqft" qs "qs"
  comment "Reverse"
  let qs' = reverse qs
  qs <- aqftImpl approx qs'
  comment_with_label "EXIT: aqft" qs "qs"
  return qs

mapPhase :: [Qubit] -> Circ [Qubit]
mapPhase [] = return []
mapPhase (q : qs) = do
  qs' <- mapPhase qs
  q' <- rGate (length qs + 1) q
  return (q' : qs')

catalyticAqftImpl :: Int -> [Qubit] -> [Qubit] -> Circ ([Qubit], [Qubit])
catalyticAqftImpl _ [] as = return ([], as)
catalyticAqftImpl approx (x : xs) as = do
  (xs, as) <- catalyticAqftImpl approx xs as
  (xs, as) <- rotations approx x xs as
  x <- hadamard x
  return (x : xs, as)
  where
    rotations :: Int -> Qubit -> [Qubit] -> [Qubit] -> Circ ([Qubit], [Qubit])
    rotations _ _ [] _ = return ([], as)
    rotations approx c qs as = do
      let m = min (length qs) approx
      let ancillas = take (m + 1) . drop (approx - m) $ as
      let others = take (approx - m) . drop (m + 1) $ as
      let x = qdint_of_qulist_lh qs
      let y = qdint_of_qulist_lh ancillas
      (x, y) <- q_sub_in_place x y `controlled` c
      let qs = qulist_of_qdint_lh x
      let as = qulist_of_qdint_lh y ++ others
      return (qs, as)

catalyticAqft :: Int -> [Qubit] -> Circ [Qubit]
catalyticAqft approx qs = do
  (qs, _) <- with_ancilla_list approx $ \as -> do
    as <- map_hadamard as
    as <- mapPhase as
    comment_with_label "ENTER: catalytic aqft" qs "qs"
    comment "Reverse"
    let qs' = reverse qs
    (qs, as) <- catalyticAqftImpl approx qs' as
    comment_with_label "EXIT: catalytic aqft" qs "qs"
    return (qs, as)
  return qs

aqftError :: Int -> Int -> Double
aqftError n m =
  sum
    [ 2 * sin (pi * 2 ** (- fromIntegral j)) * fromIntegral (n - j + 1)
      | j <- [m + 1 .. n]
    ]

createAllAqft :: Int -> Double -> [([Qubit] -> Circ [Qubit], Double, Double)]
createAllAqft n error =
  [ (aqft m, aqftErr, error - aqftErr)
    | m <- [n, n -1 .. 1],
      let aqftErr = aqftError n m,
      error - aqftErr > 0
  ]

printCircuit :: Int -> (Precision -> GateBase) -> ([Qubit] -> Circ [Qubit], Double, Double) -> IO ()
printCircuit size base (circ, aqftErr, decompErr) = do
  printf "\nAQFT Error:   %f\n" aqftErr
  printf "Decomp Error: %f\n" decompErr
  let precision = (- log decompErr) * digits
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

-- main :: IO ()
-- main = do
--   args <- getArgs
--   let (size, baseStr, digits) = case args of
--         [a, b, c] -> (read a, b, read c)
--         _ -> error "Usage: <program> <size> <base> <digits>"
--   g <- newStdGen
--   let base = baseFromString baseStr g
--   let error = 10 ** (- digits)
--   let circuits_with_errors = createAllAqft size error
--   mapM_ (printCircuit size base) circuits_with_errors

main :: IO ()
main = do
  args <- getArgs
  let (size, approx) = case args of
        [a, b] -> (read a, read b)
        _ -> error "Usage: <program> <size> <approx>"
  print_generic Preview catalyticAqft   approx (replicate size qubit)
