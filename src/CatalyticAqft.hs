module CatalyticAqft (catalyticAqft) where

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
import Quipper.Libraries.Arith (qdint_of_qulist_lh, qulist_of_qdint_lh)
import Tools (map_phase_little_endian, q_sub_in_place)

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
      let m = min (min (length qs) approx + 1) approx
      let qubits = take (m - 1) qs
      let ancillas = take m . drop (approx - m) $ as
      let others = take (approx - m) as
      let x = qdint_of_qulist_lh qs
      let y = qdint_of_qulist_lh ancillas
      (x, y) <- q_sub_in_place x y `controlled` c
      let qs = qulist_of_qdint_lh x
      let as = others ++ qulist_of_qdint_lh y
      return (qs, as)

catalyticAqft :: Int -> [Qubit] -> Circ [Qubit]
catalyticAqft a qs = do
  let approx = if a > 1 then a else 0
  (qs, _) <- with_ancilla_list approx $ \as -> do
    as <- map_hadamard as
    as <- map_phase_little_endian as
    comment_with_label "ENTER: catalytic aqft" qs "qs"
    comment "Reverse"
    let qs' = reverse qs
    (qs, as) <- catalyticAqftImpl approx qs' as
    comment_with_label "EXIT: catalytic aqft" qs "qs"
    return (qs, as)
  return qs
