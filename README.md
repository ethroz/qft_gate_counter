# qft_gate_counter
The gate counter I used for my Honour's Research

## Overview
This project is designed to count the quantum gates used in various quantum
algorithms. It includes several modules and executables to facilitate this
process.

## Structure
The project is structured as follows:
- `src/`: Contains the source code for the various modules.
- `programs/`: Contains the main programs that utilize the modules.
- `LICENSE`: Contains the license information for the project.

## Modules
The main modules included in this project are:
- `Aqft`: Implements the Approximate Quantum Fourier Transform.
- `CatalyticAqft`: Implements the Catalytic Approximate Quantum Fourier
  Transform.
- `Arithmetic`: Contains arithmetic operations used in quantum algorithms.
- `Phases`: Manages phase operations in quantum algorithms.
- `Ancillas`: Handles ancilla qubits used in quantum algorithms.

## Executables
The project includes two main executables:
- `count`: This executable is defined in `Count.hs` and is used to count the
  quantum gates.
- `view`: This executable is defined in `View.hs` and is used to view the
  quantum circuits.

## Python Scripts
The project includes two Python scripts for generating graphs of the gate
counts:
- `gate_estimator.py`: This script estimates the gate counts for the AQFT.
- `gate_counter.py`: This script counts the gates used in the AQFT by running
  the `count` haskell program.

### Using the Python Scripts
Run the scripts using the following commands:
```
python gate_estimator.py TYPE EST_TYPE MAX_SIZE MAX_DIGITS [SAVE_FILE]
```
```
python gate_counter.py TYPE GATE_TYPE MAX_SIZE MAX_DIGITS [SAVE_FILE] [--single] [--cache-file CACHE_FILE]
```

## Dependencies
The project depends on the following libraries:
- `base ^>=4.13.0.0`
- `random ==1.1`
- `quipper-language ==0.9.0.0`
- `quipper-libraries ==0.9.0.0`
- `quipper-utils ==0.9.0.0`
- `optparse-applicative ==0.18.1.0`

## Building and Running
To build the project, use the following command:
```
cabal build
```

To run the `count` executable, use:
```
cabal run count -- --help
```

To run the `view` executable, use:
```
cabal run view -- --help
```

## Author
The project is authored by ethroz. For any suggestions, bug reports, or patches,
please contact ethroz@gmail.com.
