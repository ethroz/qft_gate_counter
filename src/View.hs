{-# LANGUAGE GADTs #-}

module Main where

import Aqft (aqft)
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
import Quipper (Circ, Format (GateCount, Preview), Qubit, qubit)
import Quipper.Internal.Printing
  ( print_generic,
  )
import Quipper.Libraries.Decompose (GateBase (Exact, TrimControls))
import Quipper.Libraries.Decompose.GateBase
  ( decompose_generic,
  )

data Args where
  Args ::
    { typeStr :: String,
      size :: Int,
      approx :: Int,
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
mainBody (Args typeStr size approx optRemoveControls optExact optCount) = do
  let baseCirc = circFromString typeStr approx
      trimmedCirc =
        if optRemoveControls
          then decompose_generic TrimControls baseCirc
          else baseCirc
      circ =
        if optExact
          then decompose_generic Exact trimmedCirc
          else trimmedCirc
  let action = if optCount then GateCount else Preview
  print_generic action circ (replicate size qubit)

circFromString :: String -> Int -> [Qubit] -> Circ [Qubit]
circFromString typeStr approx =
  case typeStr of
    "Aqft" -> aqft approx
    "CatAqft" -> catalytic_aqft approx
    _ -> error "Unknown Aqft type"
