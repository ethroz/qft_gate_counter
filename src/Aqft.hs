{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use camelCase" #-}
module Aqft (aqft, aqft_error, aqft_approx_gate_count) where

import Quipper
  ( Circ,
    Qubit,
    comment,
    controlled,
    hadamard,
    rGate,
  )
import Quipper.Internal.Labels (comment_with_label)
import Control.Monad (when)

aqft_impl :: Int -> [Qubit] -> Circ [Qubit]
aqft_impl _ [] = return []
aqft_impl approx (x : xs) = do
  xs <- aqft_impl approx xs
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
  when (approx < 1 || approx > length qs) $ error "approx must be between 1 and the length of the qubit list"
  comment_with_label "ENTER: aqft" qs "qs"
  comment "Reverse"
  let qs' = reverse qs
  qs <- aqft_impl approx qs'
  comment_with_label "EXIT: aqft" qs "qs"
  return qs

aqft_error :: Int -> Int -> Double
aqft_error n m =
  sum
    [ 2 * sin (pi * 2 ** (- fromIntegral j)) * fromIntegral (n - j + 1)
      | j <- [m + 1 .. n]
    ]

aqft_controlled_rotation_count :: Int -> Int -> Int
aqft_controlled_rotation_count n m =
  let m' = m - 1
  in n * m' - (m * m') `div` 2 -- This should always be an integer

aqft_approx_gate_count :: Int -> Int -> Int
aqft_approx_gate_count n m = 
  let total = aqft_controlled_rotation_count n m
      s_or_larger_gates = aqft_controlled_rotation_count n 2
  in total - s_or_larger_gates
