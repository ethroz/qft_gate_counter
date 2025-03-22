from typing import List, Tuple
import argparse
import math
import matplotlib.pyplot as plt
import numpy as np
import subprocess

def aqft_error(n, m):
    return sum(2 * math.sin(math.pi * 2 ** (-j)) * (n - j + 1) for j in range(m + 1, n + 1))

def aqft_controlled_rotation_count(n, m):
    m_prime = m - 1
    return n * m_prime - (m * m_prime) // 2

def aqft_rotation_layer_count(n, m):
    # return aqft_controlled_rotation_count(n, m) - aqft_controlled_rotation_count(n, m - 1)
    return n - m + 1

def aqft_approx_gate_count(n, m):
    total = aqft_controlled_rotation_count(n, m)
    s_or_larger_gates = aqft_controlled_rotation_count(n, min(m, 3))
    return total - s_or_larger_gates

def valid_approximations(n: int, error: float) -> Tuple[List[int], List[int]]:
    ms: List[int] = []
    ratios: List[float] = []
    for m in range(1, n + 1):
        gateCutErr = aqft_error(n, m)
        ratio = gateCutErr / error
        decompErr = error - gateCutErr
        approxGateCount = aqft_approx_gate_count(n, m)
        if decompErr > 0 and approxGateCount > 0:
            ms.append(m)
            ratios.append(ratio)
    return ms, ratios

def z_synthesis_t_count(rotation_exp: int, epsilon_per_gate: float)->int:
    if epsilon_per_gate == 1 or epsilon_per_gate == 0:
        raise ValueError("This should never happen")
    
    frac = pow(2, rotation_exp - 1)
    digits = -math.log10(epsilon_per_gate)
    
    cmd = f'gridsynth --digits={digits} pi/{frac}'
    result = subprocess.run(cmd, shell=True, capture_output=True, text=True)
    if result.returncode != 0:
        raise ValueError(f'gridsynth command: "{cmd}"\ncommand failed with error: {result.stderr.strip()}')
    gates_str = result.stdout.strip()
    
    t_count = gates_str.count("T")
    return t_count

def aqft_t_counts(n: int, epsilon: float, ms: List[int], ratios: List[float])->List[int]:
    t_counts: List[int] = []
    for m, gate_cut_ratio in zip(ms, ratios):
        gate_cut_epsilon = epsilon * gate_cut_ratio
        decomposition_epsilon = epsilon - gate_cut_epsilon
        approx_gate_count = aqft_approx_gate_count(n, m)
        epsilon_per_gate = decomposition_epsilon / approx_gate_count

        total_t_count = 0
        for k in range(2, m + 1):
            ancilla_t_count = 8
            if k == 2:
                rotation_t_count = 3
            elif k == 3:
                rotation_t_count = 1 + ancilla_t_count
            else:
                rotation_t_count = z_synthesis_t_count(k, epsilon_per_gate) + ancilla_t_count
            rotations = aqft_rotation_layer_count(n, k)
            total_t_count += rotation_t_count * rotations

        t_counts.append(total_t_count)
    return t_counts

def total_subtraction_t_count(n: int, m: int)->int:
    def subtraction_t_count(j: int)->int:
        num_xi_gates = 4 * j
        xi_t_gates = 4
        return xi_t_gates * num_xi_gates
    sum = 0
    for j in range(1, m):
        sum += subtraction_t_count(j)
    for j in range(m, n):
        sum += subtraction_t_count(m - 1)
    return sum

def cataqft_t_counts(n: int, epsilon: float, ms: List[int], ratios: List[float])->List[int]:
    t_counts: List[int] = []
    for m, gate_cut_ratio in zip(ms, ratios):
        gate_cut_epsilon = epsilon * gate_cut_ratio
        decomposition_epsilon = epsilon - gate_cut_epsilon
        approx_gate_count = aqft_approx_gate_count(n, m)
        epsilon_per_gate = decomposition_epsilon / approx_gate_count
        
        synthesis_t_count = 0
        for k in range(4, m + 1):
            rotation_t_count = z_synthesis_t_count(k, epsilon_per_gate)
            synthesis_t_count += rotation_t_count

        subtraction_t_count = total_subtraction_t_count(n, m)

        total_t_count = synthesis_t_count + subtraction_t_count

        t_counts.append(total_t_count)
    return t_counts

def get_min_t_count(aqft_type: str, n: int, num_digits: int) -> Tuple[int, int]:
    epsilon = pow(10, -num_digits)
    ms, ratios = valid_approximations(n, epsilon)
    if not ms:
        return 0, None

    if aqft_type == "Aqft":
        t_counts = aqft_t_counts(n, epsilon, ms, ratios)
    elif aqft_type == "CatAqft":
        t_counts = cataqft_t_counts(n, epsilon, ms, ratios)
    else:
        raise ValueError("Invalid AQFT type")
    
    min_t_count = min(t_counts)
    min_index = t_counts.index(min_t_count)
    best_m = ms[min_index]
    
    return min_t_count, best_m

def main():
    parser = argparse.ArgumentParser(description="Estimate the gate counts for the AQFT")
    parser.add_argument("type", metavar="TYPE", type=str, choices=["Aqft", "CatAqft"], help="The type of AQFT to use")
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
