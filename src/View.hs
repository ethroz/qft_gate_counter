{-# LANGUAGE GADTs #-}

module Main where

import Aqft (aqft, aqft_error, aqft_approx_gate_count)
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
    showDefault,
    str,
    switch,
    value,
    (<**>),
  )
import Quipper (Circ, Format (GateCount, Preview), Qubit, qubit)
import Quipper.Internal.Printing
  ( print_generic,
  )
import Quipper.Libraries.Decompose (GateBase (Exact, Logical, TrimControls), Precision)
import Quipper.Libraries.Decompose.GateBase
  ( GateBase (Approximate, CliffordT, Standard, Strict),
    decompose_generic,
  )
import Quipper.Libraries.Synthesis (digits)
import Quipper.Utils.RandomSource (RandomSource (RandomSource))
import System.Random (StdGen, newStdGen)

data Args where
  Args ::
    { typeStr :: String,
      size :: Int,
      approx :: Int,
      baseStr :: String,
      numDigits :: Double,
      optRemoveControls :: Bool,
      optExact :: Bool,
      optCount :: Bool
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
      ( metavar "APPROX"
          <> help "The number of rotation to leave in the aqft"
      )
    <*> argument
      str
      ( metavar "GATE_BASE"
          <> help "The base to decompose into"
          <> showDefault
          <> value ""
      )
    <*> argument
      auto
      ( metavar "DIGITS"
          <> help "The number of digits for the minimum accuracy"
          <> showDefault
          <> value 0.0
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
    <*> switch
      ( long "count"
          <> short 'c'
          <> help "Show a gate count alongside the circuit preview"
      )

main :: IO ()
main = mainBody =<< execParser opts
  where
    opts =
      info
        (args <**> helper)
        ( fullDesc
            <> progDesc "Show the circuits for the AQFT"
            <> header "view - a circuit viewing program built with quipper"
        )

mainBody :: Args -> IO ()
mainBody (Args typeStr size approx baseStr numDigits optRemoveControls optExact optCount) = do
  g <- newStdGen
  let baseCirc = circFromString typeStr approx
      trimmedCirc =
        if optRemoveControls
          then decompose_generic TrimControls baseCirc
          else baseCirc
      exactCirc =
        if optExact
          then decompose_generic Exact trimmedCirc
          else trimmedCirc
      error = 10 ** (- numDigits)
      gateCutErr = aqft_error size approx
      decompErr = error - gateCutErr
      approxGateCount = aqft_approx_gate_count size approx
      gateErr = decompErr / fromIntegral approxGateCount
      precisionPerGate = (- log gateErr) * digits
      base = baseFromString baseStr g precisionPerGate
      circ = decompose_generic base exactCirc
  let action = if optCount then GateCount else Preview
  print_generic action circ (replicate size qubit)

circFromString :: String -> Int -> [Qubit] -> Circ [Qubit]
circFromString typeStr approx =
  case typeStr of
    "Aqft" -> aqft approx
    "CatAqft" -> catalytic_aqft approx
    _ -> error "Unknown Aqft type"

baseFromString :: String -> StdGen -> Precision -> GateBase
baseFromString baseStr g precision = do
  let _ = print precision
  case baseStr of
    "CliffordT" -> CliffordT True precision (RandomSource g)
    "Standard" -> Standard precision (RandomSource g)
    "Strict" -> Strict precision (RandomSource g)
    "Approximate" -> Approximate False precision (RandomSource g)
    "" -> Logical -- Do nothing
    _ -> error "Unknown base"
