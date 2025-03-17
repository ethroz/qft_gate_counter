{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use camelCase" #-}
module Tools (map_phase_little_endian, q_quadratic_sub_in_place, q_linear_sub_in_place, q_fast_sub_in_place) where

import Control.Monad (unless)
import Quipper (Circ, Qubit, controlled, gate_S, gate_T, gate_T_inv, gate_Z, gate_iX, gate_iX_inv, hadamard, qc_measure, qnot, rGate, with_ancilla, (.==.))
import Quipper.Libraries.Arith (QDInt, list_of_xint_lh, xint_of_list_lh)

map_phase_little_endian :: [Qubit] -> Circ [Qubit]
map_phase_little_endian [] = return []
map_phase_little_endian (q : qs) = do
  qs' <- map_phase_little_endian qs
  q' <- rGate (length qs + 1) q
  return (q' : qs')

q_quadratic_sub_in_place :: QDInt -> QDInt -> Circ (QDInt, QDInt)
q_quadratic_sub_in_place x y = do
  let x' = list_of_xint_lh x
      y' = list_of_xint_lh y
  (x', y') <- q_quadratic_sub_in_place_qulist x' y'
  let x = xint_of_list_lh x'
      y = xint_of_list_lh y'
  return (x, y)

-- Little Endian Subtraction
q_quadratic_sub_in_place_qulist :: [Qubit] -> [Qubit] -> Circ ([Qubit], [Qubit])
q_quadratic_sub_in_place_qulist [] [] = return ([], [])
q_quadratic_sub_in_place_qulist xs [] = return (xs, [])
q_quadratic_sub_in_place_qulist [] ys = return ([], ys)
q_quadratic_sub_in_place_qulist (x : xs) (y : ys) = do
  (y : ys) <- q_decrement (y : ys) `controlled` x
  (xs, ys) <- q_quadratic_sub_in_place_qulist xs ys
  return (x : xs, y : ys)

q_decrement :: [Qubit] -> Circ [Qubit]
q_decrement [] = return []
q_decrement qs = do
  let (qs', [q]) = splitAt (length qs - 1) qs
  qs' <- q_decrement qs'
  q' <- qnot q `controlled` qs'
  return (qs' ++ [q'])

q_linear_sub_in_place :: QDInt -> QDInt -> Circ (QDInt, QDInt)
q_linear_sub_in_place x y = do
  let x' = list_of_xint_lh x
      y' = list_of_xint_lh y
  (x', y') <- q_linear_sub_in_place_qulist x' y'
  let x = xint_of_list_lh x'
      y = xint_of_list_lh y'
  return (x, y)

-- Little Endian Subtraction
q_linear_sub_in_place_qulist :: [Qubit] -> [Qubit] -> Circ ([Qubit], [Qubit])
q_linear_sub_in_place_qulist xs ys = do
  let nx = length xs
      ny = length ys
  unless (nx == ny || nx + 1 == ny) $
    error "Can only subtract integers of the same size or one less than the result"
  sequence_ [qnot y `controlled` x | (x, y) <- zip xs ys]
  (xs, ys) <- ripple_sub xs ys
  return (xs, ys)
  where
    ripple_sub :: [Qubit] -> [Qubit] -> Circ ([Qubit], [Qubit])
    ripple_sub [] [] = return ([], [])
    ripple_sub (a : as) [] = return (a : as, [])
    ripple_sub [] (b : bs) = return ([], b : bs)
    ripple_sub [a] [b] = return ([a], [b])
    ripple_sub (a : as) (b : bs) = do
      with_ancilla $ \c -> do
        c <- gate_iX_inv c `controlled` a `controlled` b
        (as, bs) <- ripple_sub_carry c as bs
        _ <- gate_iX c `controlled` a `controlled` b
        return (a : as, b : bs)
    ripple_sub_carry :: Qubit -> [Qubit] -> [Qubit] -> Circ ([Qubit], [Qubit])
    ripple_sub_carry _ [] [] = return ([], [])
    ripple_sub_carry _ (a : as) [] = return (a : as, [])
    ripple_sub_carry c [] (b : bs) = do
      b <- qnot b `controlled` c
      return ([], b : bs)
    ripple_sub_carry c [a] [b] = do
      b <- qnot b `controlled` c
      return ([a], [b])
    ripple_sub_carry c1 (a : as) (b : bs) = do
      a <- qnot a `controlled` c1
      (a : as, b : bs) <- with_ancilla $ \c2 -> do
        c2 <- gate_iX_inv c2 `controlled` a `controlled` b
        c2 <- qnot c2 `controlled` c1
        (as, bs) <- ripple_sub_carry c2 as bs
        c2 <- qnot c2 `controlled` c1
        _ <- gate_iX c2 `controlled` a `controlled` b
        return (a : as, b : bs)
      b <- qnot b `controlled` c1
      a <- qnot a `controlled` c1
      return (a : as, b : bs)

q_fast_sub_in_place :: QDInt -> QDInt -> Circ (QDInt, QDInt)
q_fast_sub_in_place x y = do
  let x' = list_of_xint_lh x
      y' = list_of_xint_lh y
  (x', y') <- q_fast_sub_in_place_qulist x' y'
  let x = xint_of_list_lh x'
      y = xint_of_list_lh y'
  return (x, y)

-- Little Endian Subtraction
q_fast_sub_in_place_qulist :: [Qubit] -> [Qubit] -> Circ ([Qubit], [Qubit])
q_fast_sub_in_place_qulist xs ys = do
  let nx = length xs
      ny = length ys
  unless (nx == ny || nx + 1 == ny) $
    error "Can only subtract integers of the same size or one less than the result"
  sequence_ [qnot y `controlled` x | (x, y) <- zip xs ys]
  (xs, ys) <- ripple_sub xs ys
  return (xs, ys)
  where
    quick_toffoli :: Qubit -> Qubit -> Qubit -> Circ (Qubit, Qubit, Qubit)
    quick_toffoli c1 c2 q = do
      q <- hadamard q
      q <- qnot q `controlled` c1
      q <- qnot q `controlled` c2
      c1 <- qnot c1 `controlled` q
      c2 <- qnot c2 `controlled` q
      q <- gate_T q
      c1 <- gate_T_inv c1
      c2 <- gate_T_inv c2
      c1 <- qnot c1 `controlled` q
      c2 <- qnot c2 `controlled` q
      q <- hadamard q
      q <- gate_S q
      return (c1, c2, q)
    measure_toffoli :: Qubit -> Qubit -> Qubit -> Circ (Qubit, Qubit)
    measure_toffoli c1 c2 q = do
      q <- hadamard q
      b <- qc_measure q
      c2 <- gate_Z c2 `controlled` c1 `controlled` (b .==. 1)
      return (c1, c2)
    ripple_sub :: [Qubit] -> [Qubit] -> Circ ([Qubit], [Qubit])
    ripple_sub [] [] = return ([], [])
    ripple_sub (a : as) [] = return (a : as, [])
    ripple_sub [] (b : bs) = return ([], b : bs)
    ripple_sub [a] [b] = return ([a], [b])
    ripple_sub (a : as) (b : bs) = do
      with_ancilla $ \c -> do
        (a, b, c) <- quick_toffoli a b c
        (as, bs) <- ripple_sub_carry c as bs
        (a, b) <- measure_toffoli a b c
        return (a : as, b : bs)
    ripple_sub_carry :: Qubit -> [Qubit] -> [Qubit] -> Circ ([Qubit], [Qubit])
    ripple_sub_carry _ [] [] = return ([], [])
    ripple_sub_carry _ (a : as) [] = return (a : as, [])
    ripple_sub_carry c [] (b : bs) = do
      b <- qnot b `controlled` c
      return ([], b : bs)
    ripple_sub_carry c [a] [b] = do
      b <- qnot b `controlled` c
      return ([a], [b])
    ripple_sub_carry c1 (a : as) (b : bs) = do
      a <- qnot a `controlled` c1
      (a : as, b : bs) <- with_ancilla $ \c2 -> do
        (a, b, c2) <- quick_toffoli a b c2
        c2 <- qnot c2 `controlled` c1
        (as, bs) <- ripple_sub_carry c2 as bs
        c2 <- qnot c2 `controlled` c1
        (a, b) <- measure_toffoli a b c2
        return (a : as, b : bs)
      b <- qnot b `controlled` c1
      a <- qnot a `controlled` c1
      return (a : as, b : bs)
