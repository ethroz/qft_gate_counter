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

data Args = Args
  { typeStr :: String,
    size :: Int,
    baseStr :: String,
    numDigits :: Double,
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
  let baseCircFunc = circFromString typeStr
      circFunc =
        if optRemoveControls
          then decompose_generic TrimControls . baseCircFunc
          else baseCircFunc
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
    "Approximate" -> Approximate True precision (RandomSource g)
    _ -> error "Unknown base"

aqftError :: Int -> Int -> Double
aqftError n m =
  sum
    [ 2 * sin (pi * 2 ** (- fromIntegral j)) * fromIntegral (n - j + 1)
      | j <- [m + 1 .. n]
    ]

createAllAqft :: (Int -> ([Qubit] -> Circ [Qubit])) -> Int -> Double -> [([Qubit] -> Circ [Qubit], Double, Double)]
createAllAqft circFunc n error =
  [ (circFunc m, aqftErr, error - aqftErr)
    | m <- [n, n -1 .. 1],
      let aqftErr = aqftError n m,
      error - aqftErr > 0
  ]

printCircuit :: Int -> (Precision -> GateBase) -> ([Qubit] -> Circ [Qubit], Double, Double) -> IO ()
printCircuit size baseFunc (circ, aqftErr, decompErr) = do
  printf "\nAQFT Error:   %f\n" aqftErr
  printf "Decomp Error: %f\n" decompErr
  let precision = (- log decompErr) * digits
      decompCirc = decompose_generic (baseFunc precision) circ
  putStrLn "Circuit:"
  print_generic GateCount decompCirc (replicate size qubit)
