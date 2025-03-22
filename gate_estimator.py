import argparse
import math
from typing import Callable
import matplotlib.pyplot as plt
import numpy as np

def aqft_t_count(n: int, d: int) -> float:
    lgn = math.log(n)
    return n * lgn * (lgn + d)

def get_gate_count_func(aqft_type: str, gate_type: str) -> Callable[[int, int], float]:
    if aqft_type == "Aqft":
        if gate_type == "T":
            return aqft_t_count
        else:
            raise ValueError("Not implemented")
    else:
        raise ValueError("Not implemented")

if __name__ == "__main__":
    parser = argparse.ArgumentParser(description="Print the gate counts for the AQFT in the specified GateBase")
    parser.add_argument("type", metavar="TYPE", type=str, help="The type of AQFT to use")
    parser.add_argument("gate_type", metavar="GATE_TYPE", type=str, choices=["T", "CNOT"], help="The type of gate to count")
    parser.add_argument("max_size", metavar="MAX_SIZE", type=int, help="The maximum number of qubits in the aqft")
    parser.add_argument("max_digits", metavar="MAX_DIGITS", type=int, help="The maximum number of digits for the minimum accuracy")
    parser.add_argument("save_file", nargs="?", type=str, help="The file to save the output if provided. Otherwise, the graph is shown")
    
    args = parser.parse_args()

    aqft_type = args.type
    max_size = args.max_size
    max_digits = args.max_digits
    gate_type = args.gate_type

    sizes = range(2, max_size + 1)
    digits = range(1, max_digits + 1)

    min_gate_counts = np.zeros((len(sizes), len(digits)))
    
    max_gate_count = 0
    max_gate_count_size = 0
    max_gate_count_approx = 0
    max_gate_count_digits = 0

    gate_count_func = get_gate_count_func(aqft_type, gate_type)

    for n in sizes:
        for num_digits in digits:
            min_gate_counts[n - 2, num_digits - 1] = gate_count_func(n, num_digits)

    X, Y = np.meshgrid(digits, sizes)
    Z = min_gate_counts

    fig = plt.figure(figsize=(12, 8), dpi=100)
    ax = fig.add_subplot([0.05, 0.05, 0.95, 0.95], projection='3d')
    ax.plot_surface(X, Y, Z, cmap='viridis', antialiased=False)
    ax.azim = -135

    ax.set_xlabel('Digits of Accuracy')
    ax.set_ylabel('Number of Qubits')
    ax.set_zlabel(f'Minimum {gate_type}-count')

    if args.save_file:
        plt.savefig(args.save_file)
    else:
        plt.show()
