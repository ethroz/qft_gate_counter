{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use camelCase" #-}
{-# HLINT ignore "Use fewer imports" #-}
module Ancillas (init_and_ancilla, term_and_ancilla, with_and_ancilla, init_fast_and_ancilla, term_fast_and_ancilla, with_fast_and_ancilla) where

import Quipper
  ( Circ,
    Qubit,
    controlled,
    gate_S,
    gate_T,
    gate_T_inv,
    gate_Z,
    qc_measure,
    qnot,
    qterm,
    (.==.),
  )
import Quipper.Internal.Generic (qinit)
import Quipper.Internal.Monad (hadamard)
import Quipper (gate_iX)
import Quipper (gate_iX_inv)

init_and_ancilla :: Qubit -> Qubit -> Circ (Qubit, Qubit, Qubit)
init_and_ancilla c1 c2 = do
  a <- qinit False
  a <- gate_iX a `controlled` (c1, c2)
  return (c1, c2, a)

term_and_ancilla :: Qubit -> Qubit -> Qubit -> Circ (Qubit, Qubit)
term_and_ancilla c1 c2 a = do
  a <- gate_iX_inv a `controlled` (c1, c2)
  qterm False a
  return (c1, c2)

with_and_ancilla :: Qubit -> Qubit -> (Qubit -> Circ a) -> Circ a
with_and_ancilla c1 c2 f = do
  (c1, c2, a) <- init_and_ancilla c1 c2
  q <- f a
  (_, _) <- term_and_ancilla c1 c2 a
  return q

init_fast_and_ancilla :: Qubit -> Qubit -> Circ (Qubit, Qubit, Qubit)
init_fast_and_ancilla c1 c2 = do
  a <- qinit False
  a <- hadamard a
  a <- gate_T a
  a <- qnot a `controlled` c1
  a <- qnot a `controlled` c2
  c1 <- qnot c1 `controlled` a
  c2 <- qnot c2 `controlled` a
  a <- gate_T a
  c1 <- gate_T_inv c1
  c2 <- gate_T_inv c2
  c1 <- qnot c1 `controlled` a
  c2 <- qnot c2 `controlled` a
  a <- hadamard a
  a <- gate_S a
  return (c1, c2, a)

term_fast_and_ancilla :: Qubit -> Qubit -> Qubit -> Circ (Qubit, Qubit)
term_fast_and_ancilla c1 c2 a = do
  a <- hadamard a
  y <- qc_measure a
  c2 <- gate_Z c2 `controlled` c1 `controlled` (y .==. 1)
  qterm False a
  return (c1, c2)

with_fast_and_ancilla :: Qubit -> Qubit -> (Qubit -> Circ a) -> Circ a
with_fast_and_ancilla c1 c2 f = do
  (c1, c2, a) <- init_fast_and_ancilla c1 c2
  q <- f a
  (_, _) <- term_fast_and_ancilla c1 c2 a
  return q
