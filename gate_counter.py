from typing import Tuple, Optional
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

class Cache:
    def __init__(self, cache_file: str):
        self.cache_file = cache_file
        self.cache = self._load_cache()
        self.csvfile = open(self.cache_file, 'a', newline='')
        self.csvwriter = csv.writer(self.csvfile)
        self.lock = threading.Lock()
    
    def __del__(self):
        del self.csvwriter
        self.csvfile.close()

    def _load_cache(self) -> dict:
        if not os.path.exists(self.cache_file):
            return {}
        cache = {}
        with open(self.cache_file, 'r') as csvfile:
            reader = csv.reader(csvfile)
            for row in reader:
                aqft_type, gate_type, n, num_digits, gate_count, m = row
                cache[(aqft_type, gate_type, int(n), int(num_digits))] = (int(gate_count), int(m) if m else None)
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
cnot_gate_pattern = re.compile(r'(\d+): "not, arity 1", controls 1')

def parse_output(output: str, gate_type: str):
    ms = [int(match.group(1)) for match in m_pattern.finditer(output)]
    
    if gate_type == "T":
        t_star_gate_counts = [int(match.group(1)) for match in t_star_gate_pattern.finditer(output)]
        t_gate_counts = [int(match.group(1)) for match in t_gate_pattern.finditer(output)]
        gate_counts = [x + y for x, y in zip(t_star_gate_counts, t_gate_counts)]
    elif gate_type == "CNOT":
        gate_counts = [int(match.group(1)) for match in cnot_gate_pattern.finditer(output)]
    else:
        raise ValueError(f"Unrecognized gate type: {gate_type}")
    
    return ms, gate_counts

def get_min_gate_count(aqft_type: str, gate_type: str, n: int, num_digits: int, cache: Optional[Cache] = None) -> Tuple[int, int]:
    if cache:
        cached_result = cache.get(aqft_type, gate_type, n, num_digits)
        if cached_result:
            return cached_result
    
    output = run_haskell_program(aqft_type, n, num_digits)
    if not output:
        return 0, None
    ms, gate_counts = parse_output(output, gate_type)

    min_gate_count = min(gate_counts)
    min_index = gate_counts.index(min_gate_count)
    best_m = ms[min_index]
    
    if cache:
        cache.set(aqft_type, gate_type, n, num_digits, min_gate_count, best_m)
    
    return min_gate_count, best_m

def get_min_gate_count_parallel(args):
    aqft_type, gate_type, n, num_digits, cache = args
    return n, num_digits, get_min_gate_count(aqft_type, gate_type, n, num_digits, cache)

if __name__ == "__main__":
    parser = argparse.ArgumentParser(description="Print the gate counts for the AQFT in the specified GateBase")
    parser.add_argument("type", metavar="TYPE", type=str, help="The type of AQFT to use")
    parser.add_argument("gate_type", metavar="GATE_TYPE", type=str, choices=["T", "CNOT"], help="The type of gate to count")
    parser.add_argument("max_size", metavar="MAX_SIZE", type=int, help="The maximum number of qubits in the aqft")
    parser.add_argument("max_digits", metavar="MAX_DIGITS", type=int, help="The maximum number of digits for the minimum accuracy")
    parser.add_argument("save_file", nargs="?", type=str, help="The file to save the output if provided. Otherwise, the graph is shown")
    parser.add_argument("--single", "-s", action="store_true", help="If present, will find the gate count for only the specified arguments")
    parser.add_argument("--cache-file", type=str, default="cache.csv", help="The file to cache the results")
    
    args = parser.parse_args()

    aqft_type = args.type
    max_size = args.max_size
    max_digits = args.max_digits
    gate_type = args.gate_type
    cache_file = args.cache_file

    cache = Cache(args.cache_file) if args.cache_file else None
        
    if args.single:
        gate_count, m = get_min_gate_count(aqft_type, gate_type, max_size, max_digits, cache)
        print(f"T count: {gate_count}")
        print(f"Approx:  {m}")
        sys.exit(0)

    sizes = range(2, max_size + 1)
    digits = range(1, max_digits + 1)

    min_gate_counts = np.zeros((len(sizes), len(digits)))
    
    max_gate_count = 0
    max_gate_count_size = 0
    max_gate_count_approx = 0
    max_gate_count_digits = 0

    tasks = [(aqft_type, gate_type, n, num_digits, cache) for n in sizes for num_digits in digits]
    length = len(tasks)

    with ThreadPoolExecutor() as executor:
        futures = {executor.submit(get_min_gate_count_parallel, task): task for task in tasks}
        for i, future in enumerate(as_completed(futures)):
            n, num_digits, (gate_count, m) = future.result()
            min_gate_counts[n - 2, num_digits - 1] = gate_count

            if gate_count > max_gate_count:
                max_gate_count = gate_count
                max_gate_count_size = n
                max_gate_count_approx = m
                max_gate_count_digits = num_digits

            progress = (i + 1) / length * 100
            print(f"Progress: {progress:.2f}%", end="\r", flush=True)

    print(f"Largest T count:      {max_gate_count}")
    print(f"Corresponding size:   {max_gate_count_size}")
    print(f"Corresponding approx: {max_gate_count_approx}")
    print(f"Corresponding digits: {max_gate_count_digits}")

    X, Y = np.meshgrid(digits, sizes)
    Z = min_gate_counts

    fig = plt.figure(figsize=(12, 8), dpi=100)
    ax = fig.add_subplot([0.05, 0.05, 0.95, 0.95], projection='3d')
    ax.plot_surface(X, Y, Z, cmap='viridis', antialiased=False)
    ax.azim = -135

    ax.set_xlabel('Digits of Accuracy')
    ax.set_ylabel('Number of Qubits')
    ax.set_zlabel('Minimum T-count')

    if args.save_file:
        plt.savefig(args.save_file)
    else:
        plt.show()
