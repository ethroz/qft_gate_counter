from typing import Tuple
import argparse
import matplotlib.pyplot as plt
import numpy as np
import os
import re
import subprocess

def run_haskell_program(type_str, size, num_digits, base_str="Standard", trim_controls=False):
    args = [
        "cabal", "run", "count", "-O2", "--",
        type_str,
        str(size),
        str(num_digits),
        base_str,
    ]
    if trim_controls:
        args.append("--trim-controls")

    this_dir = os.path.dirname(__file__)
    result = subprocess.run(args, cwd=this_dir, capture_output=True, text=True)
    if result.returncode != 0:
        print(result.stderr)
        exit(result.returncode)
    return result.stdout

m_pattern = re.compile(r'm:\s+(\d+)')
t_star_gate_pattern = re.compile(r'(\d+): "T\*, arity 1"')
t_gate_pattern = re.compile(r'(\d+): "T, arity 1"')

def parse_output(output):
    ms = [int(match.group(1)) for match in m_pattern.finditer(output)]
    t_star_gate_counts = [int(match.group(1)) for match in t_star_gate_pattern.finditer(output)]
    t_gate_counts = [int(match.group(1)) for match in t_gate_pattern.finditer(output)]

    total_t_gate_counts = [x + y for x, y in zip(t_star_gate_counts, t_gate_counts)]
    
    return ms, total_t_gate_counts

def get_min_t_count(aqft_type: str, n: int, num_digits: int) -> Tuple[int, int]:
    output = run_haskell_program(aqft_type, n, num_digits)
    if not output:
        return 0, None
    ms, t_counts = parse_output(output)

    min_t_count = min(t_counts)
    min_index = t_counts.index(min_t_count)
    best_m = ms[min_index]
    
    return min_t_count, best_m

def main():
    parser = argparse.ArgumentParser(description="Print the gate counts for the AQFT in the specified GateBase")
    parser.add_argument("type", metavar="TYPE", type=str, help="The type of AQFT to use")
    parser.add_argument("max_size", metavar="MAX_SIZE", type=int, help="The maximum number of qubits in the aqft")
    parser.add_argument("max_digits", metavar="MAX_DIGITS", type=int, help="The maximum number of digits for the minimum accuracy")
    parser.add_argument("save_file", nargs="?", type=str, help="The file to save the output if provided. Otherwise, the graph is shown")
    parser.add_argument("--single", "-s", action="store_true", help="If present, will find the gate count for only the specified arguments")
    
    args = parser.parse_args()

    aqft_type = args.type
    max_size = args.max_size
    max_digits = args.max_digits
        
    if args.single:
        t_count, m = get_min_t_count(aqft_type, max_size, max_digits)
        print(f"T count: {t_count}")
        print(f"Approx:  {m}")
        return

    sizes = range(2, max_size + 1)
    digits = range(1, max_digits + 1)

    min_t_counts = np.zeros((len(sizes), len(digits)))
    
    max_t_count = 0
    max_t_count_size = 0
    max_t_count_approx = 0
    max_t_count_digits = 0

    length = len(sizes)
    for i, n in enumerate(sizes):
        for j, num_digits in enumerate(digits):
            t_count, m = get_min_t_count(aqft_type, n, num_digits)
            min_t_counts[i, j] = t_count

            # Track the largest T count
            if t_count > max_t_count:
                max_t_count = t_count
                max_t_count_size = n
                max_t_count_approx = m
                max_t_count_digits = num_digits

        progress = i / length * 100
        print(f"Progress: {progress:.2f}%", end="\r", flush=True)

    print(f"Largest T count:      {max_t_count}")
    print(f"Corresponding size:   {max_t_count_size}")
    print(f"Corresponding approx: {max_t_count_approx}")
    print(f"Corresponding digits: {max_t_count_digits}")

    X, Y = np.meshgrid(digits, sizes)
    Z = min_t_counts

    fig = plt.figure(figsize=(12, 8), dpi=100)
    ax = fig.add_subplot(111, projection='3d')
    ax.plot_surface(X, Y, Z, cmap='viridis')

    ax.set_xlabel('Digits of Accuracy')
    ax.set_ylabel('Number of Qubits')
    ax.set_zlabel('Minimum T-count')

    if args.save_file:
        plt.savefig(args.save_file)
    else:
        plt.show()

if __name__ == "__main__":
    main()
