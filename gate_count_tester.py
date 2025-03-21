import argparse
import matplotlib.pyplot as plt
import os
import re
import subprocess

def run_haskell_program(type_str, size, base_str, num_digits, trim_controls=False):
    cmd = [
        "cabal", "run", "count", "-O2", "--",
        type_str,
        str(size),
        str(num_digits),
        base_str,
    ]
    if trim_controls:
        cmd.append("--trim-controls")

    this_dir = os.path.dirname(__file__)
    result = subprocess.run(cmd, cwd=this_dir, capture_output=True, text=True)
    if result.returncode != 0:
        print(result.stderr)
        exit(result.returncode)
    return result.stdout

gate_cutting_error_pattern = re.compile(r'Gate cutting error:\s+([\d.]+)')
decomposition_error_pattern = re.compile(r'Decomposition Error:\s+([\d.]+)')
t_star_gate_pattern = re.compile(r'(\d+): "T\*, arity 1"')
t_gate_pattern = re.compile(r'(\d+): "T, arity 1"')
cnot_gate_pattern = re.compile(r'(\d+): "not, arity 1", controls 1')
total_gates_pattern = re.compile(r'Total gates: (\d+)')

def parse_output(output):
    gate_cutting_errors = [float(match.group(1)) for match in gate_cutting_error_pattern.finditer(output)]
    decomposition_errors = [float(match.group(1)) for match in decomposition_error_pattern.finditer(output)]
    t_star_gate_counts = [int(match.group(1)) for match in t_star_gate_pattern.finditer(output)]
    t_gate_counts = [int(match.group(1)) for match in t_gate_pattern.finditer(output)]
    cnot_gate_counts = [int(match.group(1)) for match in cnot_gate_pattern.finditer(output)]
    total_gate_counts = [int(match.group(1)) for match in total_gates_pattern.finditer(output)]

    total_t_gate_counts = [x + y for x, y in zip(t_star_gate_counts, t_gate_counts)]
    
    return gate_cutting_errors, decomposition_errors, total_t_gate_counts, cnot_gate_counts, total_gate_counts    

if __name__ == "__main__":
    parser = argparse.ArgumentParser(description="Print the gate counts for the AQFT in the specified GateBase")
    parser.add_argument("type", metavar="TYPE", type=str, help="The type of AQFT to use")
    parser.add_argument("size", metavar="SIZE", type=int, help="The number of qubits in the aqft")
    parser.add_argument("num_digits", metavar="DIGITS", type=float, help="The number of digits for the minimum accuracy")
    parser.add_argument("base_str", metavar="GATE_BASE", nargs="?", type=str, default="Standard", help="The base to decompoose into")
    parser.add_argument("save_file", nargs="?", type=str, help="The file to save the output if provided. Otherwise, the graph is shown")
    parser.add_argument("--trim-controls", "-t", action="store_true", help="Whether to trim excess controls before decomposing")
    
    args = parser.parse_args()

    error = pow(10, -args.num_digits)
    type_str = args.type

    output = run_haskell_program(type_str, args.size, args.base_str, args.num_digits, args.trim_controls)
    gate_cutting_errors, decomposition_errors, t_gate_counts, cnot_gate_counts, total_gate_counts = parse_output(output)
    fractional_gate_cutting_errors = [x / error for x in gate_cutting_errors]
    data = [
        (t_gate_counts, 'T Count'),
        (cnot_gate_counts, 'CNOT Count'),
        (total_gate_counts, 'Total Count')
    ]
    
    fig, axes = plt.subplots(1, len(data), figsize=(18, 6), dpi=100)
    fig.suptitle(f'{args.size} Qubit {type_str} with Error {error} and {args.base_str} Gate Set', fontsize=24)

    x_data = fractional_gate_cutting_errors
    for col, (y_data, y_label) in enumerate(data):
        ax = axes[col]
        ax.plot(x_data, y_data)
        ax.set_title(f"{type_str} - {y_label}", fontsize=14)
        ax.set_xlabel('Fractional of Error for Gate Cutting', fontsize=12)
        ax.set_ylabel(y_label, fontsize=12)

    plt.tight_layout(pad=2)
    if args.save_file:
        plt.savefig(args.save_file)
    else:
        plt.show()
