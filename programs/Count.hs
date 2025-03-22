{-# LANGUAGE GADTs #-}

module Main where

import Aqft (aqft, aqft_approx_gate_count, aqft_error)
import CatalyticAqft (catalytic_aqft)
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
  ( GateBase (Exact, TrimControls),
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
      numDigits :: Double,
      baseStr :: String,
      optRemoveControls :: Bool,
      optExact :: Bool
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
      auto
      ( metavar "DIGITS"
          <> help "The number of digits for the minimum accuracy"
      )
    <*> argument
      str
      ( metavar "GATE_BASE"
          <> help "The base to decompose into"
      )
    <*> switch
      ( long "trim-controls"
          <> short 't'
          <> help "Whether to trim excess controls before decomposing"
      )
    <*> switch
      ( long "exact"
          <> short 'e'
          <> help "Convert exact gates to Clifford+T and leave everything else"
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
mainBody (Args typeStr size numDigits baseStr optRemoveControls optExact) = do
  g <- newStdGen
  let baseCircFunc = circFromString typeStr
      trimmedCircFunc =
        if optRemoveControls
          then decompose_generic TrimControls . baseCircFunc
          else baseCircFunc
      circFunc =
        if optExact
          then decompose_generic Exact . trimmedCircFunc
          else trimmedCircFunc
      baseFunc = baseFromString baseStr g
      error = 10 ** (- numDigits)
      circuits_with_errors = createAllAqft circFunc size error
  mapM_ (printCircuit size baseFunc) circuits_with_errors

circFromString :: String -> Int -> [Qubit] -> Circ [Qubit]
circFromString typeStr approx =
  case typeStr of
    "Aqft" -> aqft approx
    "CatAqft" -> catalytic_aqft approx
    _ -> error "Unknown Aqft type"

baseFromString :: String -> StdGen -> Precision -> GateBase
baseFromString baseStr g precision =
  case baseStr of
    "CliffordT" -> CliffordT True precision (RandomSource g)
    "Standard" -> Standard precision (RandomSource g)
    "Strict" -> Strict precision (RandomSource g)
    "Approximate" -> Approximate False precision (RandomSource g)
    _ -> error "Unknown base"

createAllAqft :: (Int -> [Qubit] -> Circ [Qubit]) -> Int -> Double -> [([Qubit] -> Circ [Qubit], Double, Int, Double, Int, Double, Double)]
createAllAqft circFunc n error =
  [ (circFunc m, error, m, gateCutErr, approxGateCount, gateErr, decompErr2)
    | m <- [1 .. n],
      let gateCutErr = aqft_error n m
          decompErr = error - gateCutErr
          approxGateCount = aqft_approx_gate_count n m
          gateErr = decompErr / fromIntegral approxGateCount
          decompErr2 = gateErr * fromIntegral approxGateCount,
      decompErr > 0 && approxGateCount > 0
  ]

printCircuit :: Int -> (Precision -> GateBase) -> ([Qubit] -> Circ [Qubit], Double, Int, Double, Int, Double, Double) -> IO ()
printCircuit size baseFunc (circ, totalErr, approx, gateCutErr, approxGateCount, gateErr, decompErr) = do
  printf "\nTotal error:                  %f\n" totalErr
  printf   "m:                            %d\n" approx
  printf   "Gate cutting error:           %f\n" gateCutErr
  printf   "Gate cutting ratio:           %f\n" (gateCutErr / totalErr)
  printf   "Number of approximate gates:  %d\n" approxGateCount
  printf   "Decomposition error per gate: %f\n" gateErr
  printf   "Decomposition error:          %f\n" decompErr
  let precisionPerGate = (- logBase 10 gateErr) * digits
      decompCirc = decompose_generic (baseFunc precisionPerGate) circ
  putStrLn "Circuit:"
  print_generic GateCount decompCirc (replicate size qubit)
