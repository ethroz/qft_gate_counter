{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use camelCase" #-}
module Tools (map_phase_little_endian, q_sub_in_place, q_add_in_place) where

import Quipper
  ( Circ,
    Qubit,
    controlled,
    qnot,
    rGate,
    reverse_generic_endo,
  )
import Quipper.Libraries.Arith (QDInt, list_of_xint_lh, xint_of_list_lh)

map_phase_little_endian :: [Qubit] -> Circ [Qubit]
map_phase_little_endian [] = return []
map_phase_little_endian (q : qs) = do
  qs' <- map_phase_little_endian qs
  q' <- rGate (length qs + 1) q
  return (q' : qs')

q_add_in_place :: QDInt -> QDInt -> Circ (QDInt, QDInt)
q_add_in_place x y = do
  let x' = list_of_xint_lh x
  let y' = list_of_xint_lh y
  (x', y') <- q_add_in_place_qulist x' y'
  let x = xint_of_list_lh x'
  let y = xint_of_list_lh y'
  return (x, y)

-- Little Endian Addition
q_add_in_place_qulist :: [Qubit] -> [Qubit] -> Circ ([Qubit], [Qubit])
q_add_in_place_qulist [] [] = return ([], [])
q_add_in_place_qulist xs [] = return (xs, [])
q_add_in_place_qulist [] ys = return ([], ys)
q_add_in_place_qulist (x : xs) (y : ys) = do
  let ys' = reverse (y : ys)
  (x, ys') <- add_all x ys'
  let (y'' : ys'') = reverse ys'
  (xs, ys'') <- q_add_in_place_qulist xs ys''
  return (x : xs, y'' : ys'')
  where
    add_all :: Qubit -> [Qubit] -> Circ (Qubit, [Qubit])
    add_all q [] = return (q, [])
    add_all q (c : cs) = do
      c <- qnot c `controlled` (q : cs)
      (q, cs) <- add_all q cs
      return (q, c : cs)

q_sub_in_place :: QDInt -> QDInt -> Circ (QDInt, QDInt)
q_sub_in_place x y = reverse_generic_endo (uncurry q_add_in_place) (x, y)
