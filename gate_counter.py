from typing import List, Tuple, Optional
import argparse
import csv
import matplotlib.pyplot as plt
import numpy as np
import os
import re
import subprocess
import sys
from concurrent.futures import ThreadPoolExecutor, as_completed
import threading
from io import TextIOWrapper
from scipy.optimize import curve_fit

class Cache:
    def __init__(self, cache_file: str):
        self.cache_file = cache_file
        self.cache = self._load_cache()
        self.csvfile = open(self.cache_file, "a", newline="")
        self.csvwriter = csv.writer(self.csvfile)
        self.lock = threading.Lock()

    def __del__(self):
        csvfile: TextIOWrapper = getattr(self, "csvfile", None)
        if csvfile:
            csvfile.close()

    def _load_cache(self) -> dict:
        if not os.path.exists(self.cache_file):
            return {}
        cache = {}
        with open(self.cache_file, "r") as csvfile:
            reader = csv.reader(csvfile)
            for row in reader:
                aqft_type, gate_type, n, num_digits, gate_count, m = row
                cache[(aqft_type, gate_type, int(n), float(num_digits))] = (int(gate_count), int(m) if m else None)
        return cache

    def get(self, aqft_type: str, gate_type: str, n: int, num_digits: int) -> Optional[Tuple[int, int]]:
        return self.cache.get((aqft_type, gate_type, n, num_digits))

    def set(self, aqft_type: str, gate_type: str, n: int, num_digits: int, gate_count: int, m: int):
        with self.lock:
            if (aqft_type, gate_type, n, num_digits) in self.cache:
                return
            self.cache[(aqft_type, gate_type, n, num_digits)] = (gate_count, m)
            self.csvwriter.writerow([aqft_type, gate_type, n, num_digits, gate_count, m])
            self.csvfile.flush()

def aqft_type_map(aqft_type: str) -> str:
    return {
        "Aqft": "Approximate Quantum Fourier Transform",
        "CatAqft": "Catalytic Approximate Quantum Fourier Transform",
    }.get(aqft_type)

def build_haskell_program() -> None:
    args = ["cabal", "build", "count", "-O2"]
    this_dir = os.path.dirname(__file__)
    result = subprocess.run(args, cwd=this_dir, capture_output=True, text=True)
    if result.returncode != 0:
        print(result.stderr)
        exit(result.returncode)

def run_haskell_program(type_str: str, size: int, num_digits: float, exact: bool) -> str:
    args = [
        "cabal", "run", "count", "-O2", "--",
        type_str,
        str(size),
        str(num_digits),
    ]
    if exact:
        args.append("Logical")
        args.append("--trim-controls")
        args.append("--exact")
    else:
        args.append("Standard")

    this_dir = os.path.dirname(__file__)
    result = subprocess.run(args, cwd=this_dir, capture_output=True, text=True)
    if result.returncode != 0:
        print(result.stderr)
        exit(result.returncode)
    return result.stdout

m_pattern = re.compile(r'm:\s+(\d+)')
t_star_gate_pattern = re.compile(r'(\d+): "T\*, arity 1"')
t_gate_pattern = re.compile(r'(\d+): "T, arity 1"')
cnot_gate_pattern = re.compile(r'(\d+): "not, arity 1", controls 1')

def parse_output(output: str) -> Tuple[List[int], List[int], List[int]]:
    ms = [int(match.group(1)) for match in m_pattern.finditer(output)]

    t_star_gate_counts = [int(match.group(1)) for match in t_star_gate_pattern.finditer(output)]
    t_gate_counts = [int(match.group(1)) for match in t_gate_pattern.finditer(output)]
    t_gate_counts = [x + y for x, y in zip(t_star_gate_counts, t_gate_counts)]
    cnot_gate_counts = [int(match.group(1)) for match in cnot_gate_pattern.finditer(output)]

    return ms, t_gate_counts, cnot_gate_counts

def get_min_gate_count(aqft_type: str, gate_type: str, n: int, num_digits: int, exact: bool, cache: Optional[Cache] = None) -> Tuple[int, int]:
    if cache:
        cached_result = cache.get(aqft_type, gate_type, n, num_digits)
        if cached_result:
            return cached_result

    output = run_haskell_program(aqft_type, n, num_digits, exact)
    if not output:
        return 0, None
    ms, t_gate_counts, cnot_gate_counts = parse_output(output)

    min_t_gate_count = min(t_gate_counts)
    min_index = t_gate_counts.index(min_t_gate_count)
    min_cnot_gate_count = cnot_gate_counts[min_index]
    best_m = ms[min_index]

    min_gate_count = min_t_gate_count if gate_type == "T" else min_cnot_gate_count

    if cache:
        cache.set(aqft_type, gate_type, n, num_digits, min_gate_count, best_m)

    return min_gate_count, best_m

def get_min_gate_count_parallel(args):
    aqft_type, gate_type, n, num_digits, exact, cache = args
    return n, num_digits, get_min_gate_count(aqft_type, gate_type, n, num_digits, exact, cache)

if __name__ == "__main__":
    parser = argparse.ArgumentParser(description="Print the gate counts for the AQFT in the specified GateBase")
    parser.add_argument("type", metavar="TYPE", type=str, help="The type of AQFT to use")
    parser.add_argument("gate_type", metavar="GATE_TYPE", type=str, choices=["T", "CNOT"], help="The type of gate to count")
    parser.add_argument("max_size", metavar="MAX_SIZE", type=int, help="The maximum number of qubits in the aqft")
    parser.add_argument("max_digits", metavar="MAX_DIGITS", type=int, help="The maximum number of digits for the minimum accuracy")
    parser.add_argument("save_file", nargs="?", type=str, help="The file to save the output if provided. Otherwise, the graph is shown")
    parser.add_argument("--exact", "-e", action="store_true", help="If present, will convert all exact circuits to Clifford+T")
    parser.add_argument("--single", "-s", action="store_true", help="If present, will find the gate count for only the specified arguments")
    parser.add_argument("--cache-file", type=str, default="cache.csv", help="The file to cache the results")

    args = parser.parse_args()

    aqft_type = args.type
    max_size = args.max_size
    max_digits = args.max_digits
    gate_type = args.gate_type
    cache_file = args.cache_file
    exact = args.exact
    is_3d = max_digits != 0

    cache = Cache(args.cache_file) if args.cache_file and not exact else None

    if args.single:
        gate_count, m = get_min_gate_count(aqft_type, gate_type, max_size, max_digits, exact, cache)
        print(f"{gate_type} count: {gate_count}")
        print(f"Approx:  {m}")
        sys.exit(0)

    sizes = [x for x in range(2, max_size + 1)]
    digits = [x * max_digits / 20 for x in range(0, 21)] if is_3d else [0.0]

    min_gate_counts = np.zeros((len(sizes), len(digits)))

    max_gate_count = 0
    max_gate_count_size = 0
    max_gate_count_approx = 0
    max_gate_count_digits = 0

    tasks = [(aqft_type, gate_type, n, num_digits, exact, cache) for n in sizes for num_digits in digits]
    length = len(tasks)

    build_haskell_program()

    with ThreadPoolExecutor() as executor:
        futures = {executor.submit(get_min_gate_count_parallel, task): task for task in tasks}
        for i, future in enumerate(as_completed(futures)):
            n, num_digits, (gate_count, m) = future.result()
            min_gate_counts[sizes.index(n), digits.index(num_digits)] = gate_count

            if gate_count > max_gate_count:
                max_gate_count = gate_count
                max_gate_count_size = n
                max_gate_count_approx = m
                max_gate_count_digits = num_digits

            progress = (i + 1) / length * 100
            print(f"Progress: {progress:.2f}%", end="\r", flush=True)

    print(f"Largest {gate_type} count:\t{max_gate_count}")
    print(f"Corresponding size:\t{max_gate_count_size}")
    print(f"Corresponding approx:\t{max_gate_count_approx}")
    print(f"Corresponding digits:\t{max_gate_count_digits}")

    if is_3d:
        X, Y = np.meshgrid(digits, sizes)
        Z = min_gate_counts

        fig = plt.figure(figsize=(12, 8), dpi=100)
        ax = fig.add_subplot([0.0, 0.0, 1.0, 1.0], projection="3d")
        ax.plot_surface(X, Y, Z, cmap="viridis", antialiased=False)
        ax.view_init(elev=30, azim=-135)

        ax.set_xlabel("Digits of Accuracy")
        ax.set_ylabel("Number of Qubits")
        ax.set_zlabel(f"Minimum {gate_type}-count")
    else:
        # Perform regression to determine if the plot is closer to n*log(n), n^2, or n*log^2(n)
        def n_log_n(x, a):
            return a * x * np.log(x)

        def n_squared(x, a):
            return a * x**2

        def n_log_squared_n(x, a):
            return a * x * (np.log(x)**2)

        x_data = np.array(sizes)
        y_data = min_gate_counts[:, 0]

        # Fit the data to n*log(n)
        params_nlogn, _ = curve_fit(n_log_n, x_data, y_data)
        y_fit_nlogn = n_log_n(x_data, *params_nlogn)

        # Fit the data to n^2
        params_n2, _ = curve_fit(n_squared, x_data, y_data)
        y_fit_n2 = n_squared(x_data, *params_n2)

        # Fit the data to n*log^2(n)
        params_nlog2n, _ = curve_fit(n_log_squared_n, x_data, y_data)
        y_fit_nlog2n = n_log_squared_n(x_data, *params_nlog2n)

        # Plot the original data and the fits
        plt.plot(x_data, y_data, label="Original Data")
        plt.plot(x_data, y_fit_nlogn, label="n*log(n) fit", linestyle="--")
        plt.plot(x_data, y_fit_n2, label="n^2 fit", linestyle="--")
        plt.plot(x_data, y_fit_nlog2n, label="n*log^2(n) fit", linestyle="--")
        plt.legend()

        plt.xlabel("Number of Qubits")
        plt.ylabel(f"Minimum {gate_type}-count")
        plt.title(f"Minimum {gate_type}-count vs Number of Qubits for 0 Digits of Accuracy")

        # Determine which fit is better
        residuals_nlogn = y_data - y_fit_nlogn
        residuals_n2 = y_data - y_fit_n2
        residuals_nlog2n = y_data - y_fit_nlog2n

        ss_res_nlogn = np.sum(residuals_nlogn**2)
        ss_res_n2 = np.sum(residuals_n2**2)
        ss_res_nlog2n = np.sum(residuals_nlog2n**2)
        print(f"Residual sum of squares for n*log(n) fit: {ss_res_nlogn}")
        print(f"Residual sum of squares for n^2 fit: {ss_res_n2}")
        print(f"Residual sum of squares for n*log^2(n) fit: {ss_res_nlog2n}")

        if ss_res_nlogn < ss_res_n2 and ss_res_nlogn < ss_res_nlog2n:
            print("The n*log(n) fit is better.")
        elif ss_res_n2 < ss_res_nlogn and ss_res_n2 < ss_res_nlog2n:
            print("The n^2 fit is better.")
        else:
            print("The n*log^2(n) fit is better.")

    if args.save_file:
        plt.savefig(args.save_file)
    else:
        plt.show()
