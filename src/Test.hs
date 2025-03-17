{-# LANGUAGE GADTs #-}

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
  )
import Quipper (Circ, Format (Preview), Qubit, qubit)
import Quipper.Internal.Printing
  ( print_generic,
  )
import Quipper.Libraries.Arith (qdint_of_qulist_lh, qulist_of_qdint_lh)
import Tools (q_linear_sub_in_place, q_quadratic_sub_in_place)

data Args where
  Args :: {size :: Int} -> Args

args :: Parser Args
args =
  Args
    <$> argument
      auto
      ( metavar "SIZE"
          <> help "The number of qubits in the aqft"
      )

main :: IO ()
main = mainBody =<< execParser opts
  where
    opts =
      info
        (args <**> helper)
        ( fullDesc
            <> progDesc "Print the gate counts for the AQFT in the specified GateBase"
            <> header "test - a testing program for quipper"
        )

mainBody :: Args -> IO ()
mainBody (Args size) = do
  printCircuit size circ

printCircuit :: Int -> ([Qubit] -> Circ [Qubit]) -> IO ()
printCircuit size circ = do
  print_generic Preview circ (replicate size qubit)

circ :: [Qubit] -> Circ [Qubit]
circ qs = do
  let len = length qs
      half = len `div` 2
      (as, bs) = splitAt half qs
      aInt = qdint_of_qulist_lh as
      bInt = qdint_of_qulist_lh bs
  (aInt, bInt) <- q_quadratic_sub_in_place aInt bInt
  let as = qulist_of_qdint_lh aInt
      bs = qulist_of_qdint_lh bInt
      qs = as ++ bs
  return qs
