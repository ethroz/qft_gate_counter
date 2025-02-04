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

data Sample = Sample
  { typeStr :: String,
    size :: Int,
    baseStr :: String,
    numDigits :: Double,
    optRemoveControls :: Bool
  }

sample :: Parser Sample
sample =
  Sample
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
      ( long "remove-controls"
          <> short 'r'
          <> help "Whether to remove excess controls before decomposing"
      )

main :: IO ()
main = mainBody =<< execParser opts
  where
    opts =
      info
        (sample <**> helper)
        ( fullDesc
            <> progDesc "Print the gate counts for the AQFT in the specified GateBase"
            <> header "quip - a gate counting program built with quipper"
        )

mainBody :: Sample -> IO ()
mainBody (Sample typeStr size baseStr numDigits optRemoveControls) = do
  g <- newStdGen
  let circFunc =
        if optRemoveControls
          then decompose_generic TrimControls . circFromString typeStr
          else circFromString typeStr
  let baseFunc = baseFromString baseStr g
  let error = 10 ** (- numDigits)
  let circuits_with_errors = createAllAqft circFunc size error
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
  let decompCirc = decompose_generic (baseFunc precision) circ
  putStrLn "Circuit:"
  print_generic GateCount decompCirc (replicate size qubit)
