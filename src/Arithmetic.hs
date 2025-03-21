{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use camelCase" #-}
module Arithmetic (q_controlled_cubic_sub_in_place, q_controlled_linear_sub_in_place, q_controlled_fast_sub_in_place) where

import Ancillas (init_and_ancilla, init_fast_and_ancilla, term_and_ancilla, term_fast_and_ancilla, with_and_ancilla, with_fast_and_ancilla)
import Control.Monad (unless)
import Data.Maybe (fromJust, isJust)
import GHC.Base (when)
import Quipper (Circ, Qubit, controlled, qnot)
import Quipper.Libraries.Arith (QDInt, list_of_xint_lh, xint_of_list_lh)

q_cubic_sub_in_place_qulist :: [Qubit] -> [Qubit] -> Circ ([Qubit], [Qubit])
q_cubic_sub_in_place_qulist [] [] = return ([], [])
q_cubic_sub_in_place_qulist xs [] = return (xs, [])
q_cubic_sub_in_place_qulist [] ys = return ([], ys)
q_cubic_sub_in_place_qulist (x : xs) (y : ys) = do
  (y : ys) <- q_decrement (y : ys) `controlled` x
  (xs, ys) <- q_cubic_sub_in_place_qulist xs ys
  return (x : xs, y : ys)
  where
    q_decrement :: [Qubit] -> Circ [Qubit]
    q_decrement [] = return []
    q_decrement qs = do
      let (qs', [q]) = splitAt (length qs - 1) qs
      qs' <- q_decrement qs'
      q' <- qnot q `controlled` qs'
      return (qs' ++ [q'])

q_controlled_cubic_sub_in_place :: QDInt -> QDInt -> Maybe Qubit -> Circ (QDInt, QDInt)
q_controlled_cubic_sub_in_place x y c = do
  let x' = list_of_xint_lh x
      y' = list_of_xint_lh y
  (x', y') <- maybe
    (q_cubic_sub_in_place_qulist x' y')
    (controlled (q_cubic_sub_in_place_qulist x' y')) c
  let x = xint_of_list_lh x'
      y = xint_of_list_lh y'
  return (x, y)

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
    ripple_sub (x : xs) [] = return (x : xs, [])
    ripple_sub [] (y : ys) = return ([], y : ys)
    ripple_sub [x] [y] = return ([x], [y])
    ripple_sub (x : xs) (y : ys) = do
      with_and_ancilla x y $ \c -> do
        (xs, ys) <- ripple_sub_carry c xs ys
        return (x : xs, y : ys)
    ripple_sub_carry :: Qubit -> [Qubit] -> [Qubit] -> Circ ([Qubit], [Qubit])
    ripple_sub_carry _ [] [] = return ([], [])
    ripple_sub_carry _ (x : xs) [] = return (x : xs, [])
    ripple_sub_carry c [] (y : ys) = do
      y <- qnot y `controlled` c
      return ([], y : ys)
    ripple_sub_carry c [x] [y] = do
      y <- qnot y `controlled` c
      return ([x], [y])
    ripple_sub_carry c1 (x : xs) (y : ys) = do
      x <- qnot x `controlled` c1
      (x : xs, y : ys) <- with_and_ancilla x y $ \c2 -> do
        c2 <- qnot c2 `controlled` c1
        (xs, ys) <- ripple_sub_carry c2 xs ys
        _ <- qnot c2 `controlled` c1
        return (x : xs, y : ys)
      x <- qnot x `controlled` c1
      y <- qnot y `controlled` c1
      return (x : xs, y : ys)

q_controlled_linear_sub_in_place :: QDInt -> QDInt -> Maybe Qubit -> Circ (QDInt, QDInt)
q_controlled_linear_sub_in_place x y c = do
  let xs = list_of_xint_lh x
      ys = list_of_xint_lh y
  inputs <-
    if isJust c
      then mapM (\x -> do (_, _, x) <- init_and_ancilla (fromJust c) x; return x) xs
      else return xs
  (inputs, ys) <- q_linear_sub_in_place_qulist inputs ys
  when (isJust c) $ sequence_ [term_and_ancilla (fromJust c) x a | (x, a) <- zip (reverse xs) (reverse inputs)]
  let x = xint_of_list_lh xs
      y = xint_of_list_lh ys
  return (x, y)

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
    ripple_sub :: [Qubit] -> [Qubit] -> Circ ([Qubit], [Qubit])
    ripple_sub [] [] = return ([], [])
    ripple_sub (x : xs) [] = return (x : xs, [])
    ripple_sub [] (y : ys) = return ([], y : ys)
    ripple_sub [x] [y] = return ([x], [y])
    ripple_sub (x : xs) (y : ys) = do
      with_fast_and_ancilla x y $ \c -> do
        (xs, ys) <- ripple_sub_carry c xs ys
        return (x : xs, y : ys)
    ripple_sub_carry :: Qubit -> [Qubit] -> [Qubit] -> Circ ([Qubit], [Qubit])
    ripple_sub_carry _ [] [] = return ([], [])
    ripple_sub_carry _ (x : xs) [] = return (x : xs, [])
    ripple_sub_carry c [] (y : ys) = do
      y <- qnot y `controlled` c
      return ([], y : ys)
    ripple_sub_carry c [x] [y] = do
      y <- qnot y `controlled` c
      return ([x], [y])
    ripple_sub_carry c1 (x : xs) (y : ys) = do
      x <- qnot x `controlled` c1
      (x : xs, y : ys) <- with_fast_and_ancilla x y $ \c2 -> do
        c2 <- qnot c2 `controlled` c1
        (xs, ys) <- ripple_sub_carry c2 xs ys
        _ <- qnot c2 `controlled` c1
        return (x : xs, y : ys)
      y <- qnot y `controlled` c1
      x <- qnot x `controlled` c1
      return (x : xs, y : ys)

q_controlled_fast_sub_in_place :: QDInt -> QDInt -> Maybe Qubit -> Circ (QDInt, QDInt)
q_controlled_fast_sub_in_place x y c = do
  let xs = list_of_xint_lh x
      ys = list_of_xint_lh y
  inputs <-
    if isJust c
      then mapM (\x -> do (_, _, x) <- init_fast_and_ancilla (fromJust c) x; return x) xs
      else return xs
  (inputs, ys) <- q_fast_sub_in_place_qulist inputs ys
  when (isJust c) $ sequence_ [term_fast_and_ancilla (fromJust c) x a | (x, a) <- zip (reverse xs) (reverse inputs)]
  let x = xint_of_list_lh xs
      y = xint_of_list_lh ys
  return (x, y)
