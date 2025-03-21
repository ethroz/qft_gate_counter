{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use camelCase" #-}
module CatalyticAqft (catalytic_aqft) where

import Quipper
  ( Circ,
    Qubit,
    comment,
    hadamard,
    map_hadamard,
  )
import Quipper.Internal.Generic (with_ancilla_list)
import Quipper.Internal.Labels (comment_with_label)
import Quipper.Libraries.Arith (qdint_of_qulist_bh, qdint_of_qulist_lh, qulist_of_qdint_bh, qulist_of_qdint_lh)
import Arithmetic (q_controlled_linear_sub_in_place)
import Control.Monad (when)
import Phases (map_phase_little_endian)

catalytic_aqft_impl :: Int -> [Qubit] -> [Qubit] -> Circ ([Qubit], [Qubit])
catalytic_aqft_impl _ [] as = return ([], as)
catalytic_aqft_impl approx (x : xs) as = do
  (xs, as) <- catalytic_aqft_impl approx xs as
  (xs, as) <- controlled_subtract approx xs as x
  x <- hadamard x
  return (x : xs, as)
  where
    controlled_subtract :: Int -> [Qubit] -> [Qubit] -> Qubit -> Circ ([Qubit], [Qubit])
    controlled_subtract _ [] _ _ = return ([], as)
    controlled_subtract approx qs as c = do
      let (qubits, other_qubits) = splitAt (approx - 1) qs
          num_ancillas = length qs + 1
          (other_ancillas, ancillas) = splitAt (length as - num_ancillas) as
          x = qdint_of_qulist_bh qubits
          y = qdint_of_qulist_lh ancillas
      (x, y) <- q_controlled_linear_sub_in_place x y (Just c)
      let qs = qulist_of_qdint_bh x ++ other_qubits
          as = other_ancillas ++ qulist_of_qdint_lh y
      return (qs, as)

catalytic_aqft :: Int -> [Qubit] -> Circ [Qubit]
catalytic_aqft approx qs = do
  when (approx < 1 || approx > length qs) $ error "approx must be between 1 and the number of qubits"
  let approx' = catalytic_aqft_phase_gate_count approx
  with_ancilla_list approx' $ \as -> do
    as <- map_hadamard as
    as <- map_phase_little_endian as
    comment_with_label "ENTER: catalytic aqft" qs "qs"
    comment "Reverse"
    let qs' = reverse qs
    (qs, _) <- catalytic_aqft_impl approx' qs' as
    comment_with_label "EXIT: catalytic aqft" qs "qs"
  return qs

catalytic_aqft_phase_gate_count :: Int -> Int
catalytic_aqft_phase_gate_count m = if m > 1 then m else 0
