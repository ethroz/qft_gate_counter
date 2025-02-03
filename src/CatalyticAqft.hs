{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use camelCase" #-}
module CatalyticAqft (catalytic_aqft) where

import Quipper
  ( Circ,
    Qubit,
    comment,
    controlled,
    hadamard,
    map_hadamard,
  )
import Quipper.Internal.Generic (with_ancilla_list)
import Quipper.Internal.Labels (comment_with_label)
import Quipper.Libraries.Arith (qdint_of_qulist_bh, qdint_of_qulist_lh, qulist_of_qdint_bh, qulist_of_qdint_lh)
import Tools (map_phase_little_endian, q_sub_in_place)
import Control.Monad (when)

catalytic_aqft_impl :: Int -> [Qubit] -> [Qubit] -> Circ ([Qubit], [Qubit])
catalytic_aqft_impl _ [] as = return ([], as)
catalytic_aqft_impl approx (x : xs) as = do
  (xs, as) <- catalytic_aqft_impl approx xs as
  (xs, as) <- rotations approx x xs as
  x <- hadamard x
  return (x : xs, as)
  where
    rotations :: Int -> Qubit -> [Qubit] -> [Qubit] -> Circ ([Qubit], [Qubit])
    rotations _ _ [] _ = return ([], as)
    rotations approx c qs as = do
      let n = length qs
      let num_qubits = min n (approx - 1)
      let qubits = take num_qubits qs
      let other_qubits = take (n - num_qubits) . drop num_qubits $ qs
      let num_ancillas = min (min n approx + 1) approx
      let ancillas = take num_ancillas . drop (approx - num_ancillas) $ as
      let other_ancillas = take (approx - num_ancillas) as
      let x = qdint_of_qulist_bh qubits
      let y = qdint_of_qulist_lh ancillas
      (x, y) <- q_sub_in_place x y `controlled` c
      let qs = other_qubits ++ qulist_of_qdint_bh x
      let as = other_ancillas ++ qulist_of_qdint_lh y
      return (qs, as)

catalytic_aqft :: Int -> [Qubit] -> Circ [Qubit]
catalytic_aqft approx qs = do
  when (approx < 1 || approx > length qs) $ error "approx must be between 1 and the length of the qubit list"
  let approx' = if approx > 1 then approx else 0
  (qs, _) <- with_ancilla_list approx' $ \as -> do
    as <- map_hadamard as
    as <- map_phase_little_endian as
    comment_with_label "ENTER: catalytic aqft" qs "qs"
    comment "Reverse"
    let qs' = reverse qs
    (qs, as) <- catalytic_aqft_impl approx' qs' as
    comment_with_label "EXIT: catalytic aqft" qs "qs"
    return (qs, as)
  return qs
