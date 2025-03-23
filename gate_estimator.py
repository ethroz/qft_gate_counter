import argparse
import math
from typing import Callable
import matplotlib.pyplot as plt
import numpy as np

def aqft_best_t_count(n: int, d: int) -> float:
    lgn = math.log(n)
    return n * lgn * (lgn + d)

def aqft_worst_t_count(n: int, d: int) -> float:
    return n ** 2 * (math.log(n) + d)

def cataqft_best_t_count(n: int, d: int) -> float:
    lgn = math.log(n)
    return n * lgn + d * lgn

def cataqft_worst_t_count(n: int, d: int) -> float:
    return n ** 2 + n * d

def get_gate_count_func(aqft_type: str, est_type: str) -> Callable[[int, int], float]:
    if aqft_type == "Aqft":
        if est_type == "best":
            return aqft_best_t_count
        else:
            return aqft_worst_t_count
    else:
        if est_type == "best":
            return cataqft_best_t_count
        else:
            return cataqft_worst_t_count

if __name__ == "__main__":
    parser = argparse.ArgumentParser(description="Print the gate counts for the AQFT in the specified GateBase")
    parser.add_argument("type", metavar="TYPE", type=str, choices=["Aqft", "CatAqft"], help="The type of AQFT to use")
    parser.add_argument("est_type", metavar="EST_TYPE", type=str, choices=["best", "worst"], help="The type of estimation to generate")
    parser.add_argument("max_size", metavar="MAX_SIZE", type=int, help="The maximum number of qubits in the aqft")
    parser.add_argument("max_digits", metavar="MAX_DIGITS", type=int, help="The maximum number of digits for the minimum accuracy")
    parser.add_argument("save_file", nargs="?", type=str, help="The file to save the output if provided. Otherwise, the graph is shown")
    
    args = parser.parse_args()

    aqft_type = args.type
    max_size = args.max_size
    max_digits = args.max_digits
    est_type = args.est_type

    sizes = range(2, max_size + 1)
    digits = [x * max_digits / 20 for x in range(0, 21)]

    min_gate_counts = np.zeros((len(sizes), len(digits)))
    
    max_gate_count = 0
    max_gate_count_size = 0
    max_gate_count_approx = 0
    max_gate_count_digits = 0

    gate_count_func = get_gate_count_func(aqft_type, est_type)

    for n in sizes:
        for num_digits in digits:
            min_gate_counts[sizes.index(n), digits.index(num_digits)] = gate_count_func(n, num_digits)
    
    X, Y = np.meshgrid(digits, sizes)
    Z = min_gate_counts

    fig = plt.figure(figsize=(12, 8), dpi=100)
    ax = fig.add_subplot([0.0, 0.0, 1.0, 1.0], projection='3d')
    ax.plot_surface(X, Y, Z, cmap='viridis', antialiased=False)
    ax.azim = -135

    ax.set_xlabel('Digits of Accuracy')
    ax.set_ylabel('Number of Qubits')
    ax.set_zlabel(f'T-count')

    if args.save_file:
        plt.savefig(args.save_file)
    else:
        plt.show()
