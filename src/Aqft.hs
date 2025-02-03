module Aqft (aqft) where

import Quipper
  ( Circ,
    Qubit,
    comment,
    controlled,
    hadamard,
    rGate,
  )
import Quipper.Internal.Labels (comment_with_label)

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
