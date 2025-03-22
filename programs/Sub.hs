{-# LANGUAGE GADTs #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use fewer imports" #-}

module Main where

import Options.Applicative
    ( Parser,
      argument,
      auto,
      execParser,
      fullDesc,
      header,
      help,
      helper,
      info,
      metavar,
      progDesc,
      (<**>),
      long,
      short )
import Quipper (Circ, Format (Preview), Qubit, qubit)
import Quipper.Internal.Printing
  ( print_generic,
  )
import Quipper.Libraries.Arith (qdint_of_qulist_lh, qulist_of_qdint_lh, QDInt)
import Arithmetic (q_controlled_linear_sub_in_place, q_controlled_cubic_sub_in_place, q_controlled_fast_sub_in_place)
import Options.Applicative.Builder (switch)
import Options.Applicative (str)
import Quipper.Internal.Labels (comment_with_label)

data Args where
  Args ::
    { typeStr :: String,
      size :: Int,
      optControl :: Bool
    } ->
    Args

type ControlledSubCirc = QDInt -> QDInt -> Maybe Qubit -> Circ (QDInt, QDInt)

args :: Parser Args
args =
  Args
    <$> argument
      str
      ( metavar "TYPE"
          <> help "The type of subtraction circuit to view"
      )
    <*> argument
      auto
      ( metavar "SIZE"
          <> help "The number of qubits in the aqft"
      )
    <*> switch
      ( long "control"
          <> short 'c'
          <> help "Add an extra control to the circuit"
      )

main :: IO ()
main = mainBody =<< execParser opts
  where
    opts =
      info
        (args <**> helper)
        ( fullDesc
            <> progDesc "View a subtraction circuit"
            <> header "test - a testing program for quipper"
        )

mainBody :: Args -> IO ()
mainBody (Args typeStr size optControl) = do
  let implCirc = circFromString typeStr
      (subCirc, realSize) = if optControl
      then (subControlCircuit, size + 1)
      else (subCircuit, size)
      unlabledCirc = subCirc implCirc
      circ = labelCircuit unlabledCirc
  printCircuit realSize circ

subCircuit :: ControlledSubCirc -> [Qubit] -> Circ [Qubit]
subCircuit circ qs = do
  let len = length qs
      half = len `div` 2
      (as, bs) = splitAt half qs
      aInt = qdint_of_qulist_lh as
      bInt = qdint_of_qulist_lh bs
  (aInt, bInt) <- circ aInt bInt Nothing
  let as = qulist_of_qdint_lh aInt
      bs = qulist_of_qdint_lh bInt
      qs = as ++ bs
  return qs

subControlCircuit :: ControlledSubCirc -> [Qubit] -> Circ [Qubit]
subControlCircuit _ [] = return []
subControlCircuit circ (q : qs) = do
  let len = length qs
      half = len `div` 2
      (as, bs) = splitAt half qs
      aInt = qdint_of_qulist_lh as
      bInt = qdint_of_qulist_lh bs
  (aInt, bInt) <- circ aInt bInt (Just q)
  let as = qulist_of_qdint_lh aInt
      bs = qulist_of_qdint_lh bInt
      qs = as ++ bs
  return (q : qs)

circFromString :: String -> ControlledSubCirc
circFromString typeStr =
  case typeStr of
    "cubic" -> q_controlled_cubic_sub_in_place
    "linear" -> q_controlled_linear_sub_in_place
    "fast" -> q_controlled_fast_sub_in_place
    _ -> error "Unknown Subtraction circuit type"

labelCircuit :: ([Qubit] -> Circ [Qubit]) -> [Qubit] -> Circ [Qubit]
labelCircuit circ qs = do
  comment_with_label "ENTER: subtraction" qs "qs"
  qs <- circ qs
  comment_with_label "EXIT: subtraction" qs "qs"
  return qs

printCircuit :: Int -> ([Qubit] -> Circ [Qubit]) -> IO ()
printCircuit size circ = do
  print_generic Preview circ (replicate size qubit)
