{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use camelCase" #-}
module Tools (map_phase_little_endian, q_linear_sub_in_place) where

import Control.Monad (unless)
import Quipper (Circ, Qubit, controlled, qnot, rGate, with_ancilla, gate_iX_inv)
import Quipper.Libraries.Arith (QDInt, list_of_xint_lh, xint_of_list_lh)
import Quipper.Internal.Monad (gate_iX)

map_phase_little_endian :: [Qubit] -> Circ [Qubit]
map_phase_little_endian [] = return []
map_phase_little_endian (q : qs) = do
  qs' <- map_phase_little_endian qs
  q' <- rGate (length qs + 1) q
  return (q' : qs')

q_linear_sub_in_place :: QDInt -> QDInt -> Circ (QDInt, QDInt)
q_linear_sub_in_place x y = do
  let x' = list_of_xint_lh x
      y' = list_of_xint_lh y
  (x', y') <- q_sub_in_place_qulist x' y'
  let x = xint_of_list_lh x'
      y = xint_of_list_lh y'
  return (x, y)

-- Little Endian Addition
q_sub_in_place_qulist :: [Qubit] -> [Qubit] -> Circ ([Qubit], [Qubit])
q_sub_in_place_qulist xs ys = do
  let nx = length xs
      ny = length ys
  unless (nx == ny || nx + 1 == ny) $
    error "Can only subtract integers of the same size or one less than the result"
  sequence_ [qnot y `controlled` x | (x, y) <- zip xs ys]
  (xs, ys) <- ripple_add xs ys
  return (xs, ys)
  where
    ripple_add :: [Qubit] -> [Qubit] -> Circ ([Qubit], [Qubit])
    ripple_add [] [] = return ([], [])
    ripple_add (a : as) [] = return (a : as, [])
    ripple_add [] (b : bs) = return ([], b : bs)
    ripple_add [a] [b] = return ([a], [b])
    ripple_add (a : as) (b : bs) = do
      with_ancilla $ \c -> do
        c <- gate_iX_inv c `controlled` a `controlled` b
        (as, bs) <- ripple_add_carry c as bs
        _ <- gate_iX c `controlled` a `controlled` b
        return (a : as, b : bs)
    ripple_add_carry :: Qubit -> [Qubit] -> [Qubit] -> Circ ([Qubit], [Qubit])
    ripple_add_carry _ [] [] = return ([], [])
    ripple_add_carry _ (a : as) [] = return (a : as, [])
    ripple_add_carry c [] (b : bs) = do
      b <- qnot b `controlled` c
      return ([], b : bs)
    ripple_add_carry c [a] [b] = do
      b <- qnot b `controlled` c
      return ([a], [b])
    ripple_add_carry c1 (a : as) (b : bs) = do
      a <- qnot a `controlled` c1
      (a : as, b : bs) <- with_ancilla $ \c2 -> do
        c2 <- gate_iX_inv c2 `controlled` a `controlled` b
        c2 <- qnot c2 `controlled` c1
        (as, bs) <- ripple_add_carry c2 as bs
        c2 <- qnot c2 `controlled` c1
        _ <- gate_iX c2 `controlled` a `controlled` b
        return (a : as, b : bs)
      b <- qnot b `controlled` c1
      a <- qnot a `controlled` c1
      return (a : as, b : bs)
