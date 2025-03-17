{-# LANGUAGE GADTs #-}

module Main where

import Aqft (aqft, aqft_error, aqft_approx_gate_count)
import CatalyticAqft (catalytic_aqft, catalytic_aqft_approx_gate_count)
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
    long,
    metavar,
    progDesc,
    short,
    str,
    switch,
    (<**>),
  )
import Quipper (Circ, Qubit, qubit)
import Quipper.Internal.Printing
  ( Format (GateCount),
    print_generic,
  )
import Quipper.Libraries.Decompose
  ( GateBase (TrimControls),
    Precision,
  )
import Quipper.Libraries.Decompose.GateBase
  ( GateBase (Approximate, CliffordT, Standard, Strict),
    decompose_generic,
  )
import Quipper.Libraries.Synthesis (digits)
import Quipper.Utils.RandomSource (RandomSource (RandomSource))
import System.Random (StdGen, newStdGen)
import Text.Printf (printf)

data Args where
  Args ::
    { typeStr :: String,
      size :: Int,
      baseStr :: String,
      numDigits :: Double,
      optRemoveControls :: Bool
    } ->
    Args

args :: Parser Args
args =
  Args
    <$> argument
      str
      ( metavar "TYPE"
          <> help "The type of aqft"
      )
    <*> argument
      auto
      ( metavar "SIZE"
          <> help "The number of qubits in the aqft"
      )
    <*> argument
      str
      ( metavar "GATE_BASE"
          <> help "The base to decompose into"
      )
    <*> argument
      auto
      ( metavar "DIGITS"
          <> help "The number of digits for the minimum accuracy"
      )
    <*> switch
      ( long "trim-controls"
          <> short 't'
          <> help "Whether to trim excess controls before decomposing"
      )

main :: IO ()
main = mainBody =<< execParser opts
  where
    opts =
      info
        (args <**> helper)
        ( fullDesc
            <> progDesc "Print the gate counts for the AQFT in the specified GateBase"
            <> header "count - a gate counting program built with quipper"
        )

mainBody :: Args -> IO ()
mainBody (Args typeStr size baseStr numDigits optRemoveControls) = do
  g <- newStdGen
  let (baseCircFunc, approxGateCountFunc) = circAndApproxGateCountFromString typeStr
      circFunc =
        if optRemoveControls
          then decompose_generic TrimControls . baseCircFunc
          else baseCircFunc
      baseFunc = baseFromString baseStr g
      error = 10 ** (- numDigits)
      circuits_with_errors = createAllAqft circFunc approxGateCountFunc size error
  mapM_ (printCircuit size baseFunc) circuits_with_errors

circAndApproxGateCountFromString :: String -> (Int -> [Qubit] -> Circ [Qubit], Int -> Int -> Int)
circAndApproxGateCountFromString typeStr =
  case typeStr of
    "Aqft" -> (aqft, aqft_approx_gate_count)
    "CatAqft" -> (catalytic_aqft, \_ m -> catalytic_aqft_approx_gate_count m)
    _ -> error "Unknown Aqft type"

baseFromString :: String -> StdGen -> Precision -> GateBase
baseFromString baseStr g precision =
  case baseStr of
    "CliffordT" -> CliffordT True precision (RandomSource g)
    "Standard" -> Standard precision (RandomSource g)
    "Strict" -> Strict precision (RandomSource g)
    "Approximate" -> Approximate False precision (RandomSource g)
    _ -> error "Unknown base"

createAllAqft :: (Int -> [Qubit] -> Circ [Qubit]) -> (Int -> Int -> Int) -> Int -> Double -> [([Qubit] -> Circ [Qubit], Double, Double, Int, Double, Double)]
createAllAqft circFunc approxGateCountFunc n error =
  [ (circFunc m, error, gateCutErr, approxGateCount, gateErr, decompErr2)
    | m <- [n, n -1 .. 1],
      let gateCutErr = aqft_error n m
          decompErr = error - gateCutErr
          approxGateCount = approxGateCountFunc n m
          gateErr = decompErr / fromIntegral approxGateCount
          decompErr2 = gateErr * fromIntegral approxGateCount,
      decompErr > 0 && approxGateCount > 0
  ]

printCircuit :: Int -> (Precision -> GateBase) -> ([Qubit] -> Circ [Qubit], Double, Double, Int, Double, Double) -> IO ()
printCircuit size baseFunc (circ, totalErr, gateCutErr, approxGateCount, gateErr, decompErr) = do
  printf "\nTotal error:                  %f\n" totalErr
  printf   "Gate cutting error:           %f\n" gateCutErr
  printf   "Number of approximate gates:  %d\n" approxGateCount
  printf   "Decomposition error per gate: %f\n" gateErr
  printf   "Decomposition error:          %f\n" decompErr
  let precisionPerGate = (- log gateErr) * digits
      decompCirc = decompose_generic (baseFunc precisionPerGate) circ
  putStrLn "Circuit:"
  print_generic GateCount decompCirc (replicate size qubit)
