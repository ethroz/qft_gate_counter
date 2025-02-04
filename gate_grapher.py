import argparse
import matplotlib.pyplot as plt
import os
import re
import subprocess

def run_haskell_program(type_str, size, base_str, num_digits, trim_controls=False):
    cmd = [
        "cabal", "run", "quip", "-O2", "--",
        type_str,
        str(size),
        base_str,
        str(num_digits)
    ]
    if trim_controls:
        cmd.append("--trim-controls")

    this_dir = os.path.dirname(__file__)
    result = subprocess.run(cmd, cwd=this_dir, capture_output=True, text=True)
    if result.returncode != 0:
        print(result.stderr)
        exit(result.returncode)
    return result.stdout

aqft_error_pattern =   re.compile(r'AQFT Error:\s+([\d.]+)')
decomp_error_pattern = re.compile(r'Decomp Error:\s+([\d.]+)')
t_star_gate_pattern =  re.compile(r'(\d+): "T\*, arity 1"')
t_gate_pattern =       re.compile(r'(\d+): "T, arity 1"')
cnot_gate_pattern =    re.compile(r'(\d+): "not, arity 1", controls 1')
total_gates_pattern =  re.compile(r'Total gates: (\d+)')

def parse_output(output):
    aqft_errors =        [float(match.group(1)) for match in aqft_error_pattern.finditer(output)]
    decomp_errors =      [float(match.group(1)) for match in decomp_error_pattern.finditer(output)]
    t_star_gate_counts = [int(match.group(1)) for match in t_star_gate_pattern.finditer(output)]
    t_gate_counts =      [int(match.group(1)) for match in t_gate_pattern.finditer(output)]
    cnot_gate_counts =   [int(match.group(1)) for match in cnot_gate_pattern.finditer(output)]
    total_gate_counts =  [int(match.group(1)) for match in total_gates_pattern.finditer(output)]

    total_t_gate_counts = [x + y for x, y in zip(t_star_gate_counts, t_gate_counts)]
    
    return aqft_errors, decomp_errors, total_t_gate_counts, cnot_gate_counts, total_gate_counts    

if __name__ == "__main__":
    parser = argparse.ArgumentParser(description="Print the gate counts for the AQFT in the specified GateBase")
    parser.add_argument("type_str", metavar="TYPE", type=str, help="The type of aqft")
    parser.add_argument("size", metavar="SIZE", type=int, help="The number of qubits in the aqft")
    parser.add_argument("base_str", metavar="GATE_BASE", type=str, help="The base to decompose into")
    parser.add_argument("num_digits", metavar="DIGITS", type=float, help="The number of digits for the minimum accuracy")
    parser.add_argument("--trim-controls", "-t", action="store_true", help="Whether to trim excess controls before decomposing")
    
    args = parser.parse_args()

    output = run_haskell_program(args.type_str, args.size, args.base_str, args.num_digits, args.trim_controls)
    aqft_errors, decomp_errors, t_gate_counts, cnot_gate_counts, total_gate_counts = parse_output(output)
    error = pow(10, -args.num_digits)
    fractional_aqft_errors = [x / error for x in aqft_errors]
    print(fractional_aqft_errors)
    
    x_data = fractional_aqft_errors
    x_label = 'Fractional Error'
    data = [
        (t_gate_counts, 'T Count'),
        (cnot_gate_counts, 'CNOT Count'),
        (total_gate_counts, 'Total Count')
        ]
    
    fig, axes = plt.subplots(len(data), 1, figsize=(8, 8), dpi=150)
    fig.suptitle(f'{args.size} Qubit {args.type_str} with error {error}', fontsize=24)

    for ax, (y_data, y_label) in zip(axes, data):
        ax.plot(x_data, y_data)
        ax.set_title(f"{x_label} vs {y_label}", fontsize=14)
        ax.set_xlabel(x_label, fontsize=12)
        ax.set_ylabel(y_label, fontsize=12)

    plt.tight_layout(pad=2)
    plt.show()
