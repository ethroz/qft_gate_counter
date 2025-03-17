{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use camelCase" #-}
module Tools (map_phase_little_endian, q_sub_in_place, q_add_in_place) where

import Quipper
  ( Circ,
    Qubit,
    controlled,
    rGate,
    reverse_generic_endo,
  )
import Quipper.Libraries.Arith (QDInt, list_of_xint_lh, xint_of_list_lh)
import Quipper.Internal.Monad (qnot)

map_phase_little_endian :: [Qubit] -> Circ [Qubit]
map_phase_little_endian [] = return []
map_phase_little_endian (q : qs) = do
  qs' <- map_phase_little_endian qs
  q' <- rGate (length qs + 1) q
  return (q' : qs')

q_add_in_place :: QDInt -> QDInt -> Circ (QDInt, QDInt)
q_add_in_place x y = do
  let x' = list_of_xint_lh x
      y' = list_of_xint_lh y
  (x', y') <- q_add_in_place_qulist x' y'
  let x = xint_of_list_lh x'
      y = xint_of_list_lh y'
  return (x, y)

-- Little Endian Addition
q_add_in_place_qulist :: [Qubit] -> [Qubit] -> Circ ([Qubit], [Qubit])
q_add_in_place_qulist [] [] = return ([], [])
q_add_in_place_qulist xs [] = return (xs, [])
q_add_in_place_qulist [] ys = return ([], ys)
q_add_in_place_qulist (x : xs) (y : ys) = do
  (y : ys) <- q_increment (y : ys) `controlled` x
  (xs, ys) <- q_add_in_place_qulist xs ys
  return (x : xs, y : ys)

q_increment :: [Qubit] -> Circ [Qubit]
q_increment [] = return []
q_increment qs = do
  let (qs', [q]) = splitAt (length qs - 1) qs
  q' <- qnot q `controlled` qs'
  qs'' <- q_increment qs'
  return (qs'' ++ [q'])

q_sub_in_place :: QDInt -> QDInt -> Circ (QDInt, QDInt)
q_sub_in_place x y = reverse_generic_endo (uncurry q_add_in_place) (x, y)
