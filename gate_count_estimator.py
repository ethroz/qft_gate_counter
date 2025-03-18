from typing import List, Tuple
import argparse
import math

def aqft_error(n, m):
    return sum(2 * math.sin(math.pi * 2 ** (-j)) * (n - j + 1) for j in range(m + 1, n + 1))

def aqft_controlled_rotation_count(n, m):
    m_prime = m - 1
    return n * m_prime - (m * m_prime) // 2

def aqft_approx_gate_count(n, m):
    total = aqft_controlled_rotation_count(n, m)
    s_or_larger_gates = aqft_controlled_rotation_count(n, 2)
    return total - s_or_larger_gates

def valid_approximations(n: int, error: float)->Tuple[int]:
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

def z_synthesis_t_count(epsilon_per_gate: float)->int:
    t_count_double_ix = 8
    K = -9
    return math.ceil(t_count_double_ix + K + 12 * math.log2(1 / epsilon_per_gate))

def aqft_t_counts(n: int, epsilon: float, ms: List[int], ratios: List[float])->List[int]:
    t_counts: List[int] = []
    for m, gate_cut_ratio in zip(ms, ratios):
        gate_cut_epsilon = epsilon * gate_cut_ratio
        decomposition_epsilon = epsilon - gate_cut_epsilon
        approx_gate_count = aqft_approx_gate_count(n, m)
        epsilon_per_gate = decomposition_epsilon / approx_gate_count

        t_count_per_rotation = z_synthesis_t_count(epsilon_per_gate)

        total_t_count = approx_gate_count * t_count_per_rotation

        t_counts.append(total_t_count)
    return t_counts

def total_subtraction_t_count(n: int, m: int)->int:
    def subtraction_t_count(j: int)->int:
        num_xi_gates = 10 * j - 4
        num_toff_gates = 2 * j
        xi_t_gates = 4
        toff_t_gates = 7
        return xi_t_gates * num_xi_gates + toff_t_gates * num_toff_gates
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

        num_rotations = m if m > 1 else 0
        t_count_per_rotation = z_synthesis_t_count(epsilon_per_gate)

        synthesis_t_count = num_rotations * t_count_per_rotation
        subtraction_t_count = total_subtraction_t_count(n, m)

        total_t_count = synthesis_t_count + subtraction_t_count

        t_counts.append(total_t_count)
    return t_counts

def main():
    parser = argparse.ArgumentParser(description="Estimate the gate counts for the AQFT")
    parser.add_argument("type", metavar="TYPE", type=str, choices=["Aqft", "CatAqft"], help="The type of AQFT to use")
    parser.add_argument("size", metavar="SIZE", type=int, help="The number of qubits in the aqft")
    parser.add_argument("num_digits", metavar="DIGITS", type=float, help="The number of digits for the minimum accuracy")

    args = parser.parse_args()

    aqft_type = args.type
    n = args.size
    epsilon = pow(10, -args.num_digits)
    ms, ratios = valid_approximations(n, epsilon)

    if aqft_type == "Aqft":
        t_counts = aqft_t_counts(n, epsilon, ms, ratios)
    elif aqft_type == "CatAqft":
        t_counts = cataqft_t_counts(n, epsilon, ms, ratios)
    else:
        raise ValueError("wtf")

    min_t_count = min(t_counts)
    min_index = t_counts.index(min_t_count)
    best_m = ms[min_index]
    best_ratio = ratios[min_index]

    print(f"Minimum T-count: {min_t_count} (m: {best_m}, gate_cut_ratio: {best_ratio})")

if __name__ == "__main__":
    main()
