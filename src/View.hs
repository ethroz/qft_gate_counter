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
import Quipper (Circ, Format (Preview), Qubit, qubit)
import Quipper.Internal.Printing
  ( print_generic,
  )
import Quipper.Libraries.Decompose
  ( GateBase (TrimControls),
  )
import Quipper.Libraries.Decompose.GateBase
  ( decompose_generic,
  )

data Args = Args
  { typeStr :: String,
    size :: Int,
    approx :: Int,
    optRemoveControls :: Bool
  }

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
mainBody (Args typeStr size approx optRemoveControls) = do
  let baseCirc = circFromString typeStr approx
      circ =
        if optRemoveControls
          then decompose_generic TrimControls baseCirc
          else baseCirc
  printCircuit size circ

circFromString :: String -> Int -> [Qubit] -> Circ [Qubit]
circFromString typeStr approx =
  case typeStr of
    "Aqft" -> aqft approx
    "CatAqft" -> catalytic_aqft approx
    _ -> error "Unknown Aqft type"

printCircuit :: Int -> ([Qubit] -> Circ [Qubit]) -> IO ()
printCircuit size circ = do
  print_generic Preview circ (replicate size qubit)
