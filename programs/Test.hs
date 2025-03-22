module Main where
import Quipper ( Circ, Qubit, rGate, controlled, Format(GateCount) )
import Quipper.Libraries.Decompose (decompose_generic, digits, GateBase (Standard))
import Quipper.Internal.QData (qubit)
import Quipper.Internal.Printing (print_generic)
import System.Random (newStdGen)
import Quipper.Utils.RandomSource (RandomSource(RandomSource))

main :: IO ()
main = do
  g <- newStdGen
  let circ = decompose_generic (Standard (1.0 * digits) (RandomSource g)) rotCirc
  print_generic GateCount circ (replicate 2 qubit)

rotCirc :: [Qubit] -> Circ [Qubit]
rotCirc (q1:q2:qs) = do
  q1 <- rGate 4 q1 `controlled` q2
  return (q1:q2:qs)
rotCirc qs = return qs
