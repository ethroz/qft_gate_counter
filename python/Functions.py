from Gate import *
from functools import cache
import math
import subprocess

def accurate_error(n:int, m:int):
    error = 0.0
    for j in range(m + 1, n + 1):
        error += 2 * math.sin(math.pi * pow(2, -j)) * (n - j + 1)
    return error

def calc_error(n:int, m:int):
    return math.pi * 2 * (n - m - 1 + pow(2, m - n)) / pow(2, m)

def create_reverse_gate(rng:range)->Gate:
    return Gate.create('Rev', tuple(x for x in rng))

def x_to_clifford_t(wire:int|tuple[int])->list[Gate]:
    return [Gate.create(g, wire) for g in ['H', 'S', 'S', 'H']]

def z_to_clifford_t(wire:int|tuple[int])->list[Gate]:
    return [Gate.create(g, wire) for g in ['S', 'S']]

def t_prime_to_clifford_t(wire:int|tuple[int])->list[Gate]:
    return [Gate.create(g, wire) for g in ['S', 'S', 'S']] + [Gate.create('T', wire)]

def reverse_to_clifford_t(gate:Gate)->list[Gate]:
    gates = []
    half = len(gate.wires) // 2
    for wire1, wire2 in zip(gate.wires[:half], reversed(gate.wires[half:])):
        gates.append(Gate.create('X', wire1, controls=wire2))
        gates.append(Gate.create('X', wire2, controls=wire1))
        gates.append(Gate.create('X', wire1, controls=wire2))
    return gates

@cache
def synthesize_z_rotation(gate:Gate, digits:int):
    if gate.wires and len(gate.wires) > 1:
        raise ValueError('Cannot synthesize multi wire gates')
    if gate.controls:
        raise ValueError('Cannot synthesize gates with controls')

    exp_map = {
        'Z': Fraction(1, 1),
        'S': Fraction(1, 2),
        'T': Fraction(1, 4),
    }
    if gate.symbol not in exp_map.keys():
        raise ValueError('Can only synthesize Z rotations')
    implicit_exp:Fraction = exp_map.get(gate.symbol)
    overall_exp = implicit_exp * gate.exp
    phase = (math.pi * (float(overall_exp) + 2)) % (2 * math.pi)

    args = ['gridsynth', f'--rseed=0', f'--digits={digits}', '%f' % phase]
    result = subprocess.run(args, shell=True, capture_output=True, text=True)
    if result.returncode != 0:
        cmd = ' '.join(args)
        raise ValueError(f'gridsynth command: "{cmd}"\ncommand failed with error: {result.stderr.strip()}')
    gates_str = result.stdout.strip()

    gates = []
    for gate_str in gates_str:
        if gate_str in ['H', 'S', 'T']:
            gates.append(Gate.create(gate_str, gate.wires))
        elif gate_str == 'X':
            gates.extend(x_to_clifford_t(gate.wires))
        elif gate_str == 'I':
            continue
        else:
            raise ValueError(f'Unexpected gate from z rotation synthesizer: {gate_str}')

    return gates

def controlled_z_to_clifford_t(gate:Gate, digits:int)->list[Gate]:
    if not gate.is_z_rotation() or len(gate.controls) != 1:
        raise ValueError(f'Expecting a singly controlled Z gate. Got: {gate}')

    gates = []
    gates.extend(x_to_clifford_t(gate.wires))
    gates.append(Gate.create('X', gate.controls, controls=gate.wires))
    gates.extend(x_to_clifford_t(gate.wires))

    synth_circ = synthesize_z_rotation(Gate.create(gate.symbol, gate.wires, gate.exp / 2), digits)
    gates.extend(synth_circ)
    synth_circ2 = [Gate.create(g.symbol, gate.controls) for g in synth_circ]
    gates.extend(synth_circ2)

    gates.extend(x_to_clifford_t(gate.wires))
    gates.append(Gate.create('X', gate.controls, controls=gate.wires))
    gates.extend(x_to_clifford_t(gate.wires))

    return gates

def remove_a_control(gate:Gate)->list[Gate]:
    if not gate.controls:
        raise ValueError('Cannot remove a control from a gate without any controls')

    return [
        Gate.create('X',         gate.controls[0],          controls=gate.controls[1:]),
        Gate.create(gate.symbol, gate.wires, gate.exp / -2, controls=gate.controls[0]),
        Gate.create('X',         gate.controls[0],          controls=gate.controls[1:]),
        Gate.create(gate.symbol, gate.wires, gate.exp / 2,  controls=gate.controls[0]),
        Gate.create(gate.symbol, gate.wires, gate.exp / 2,  controls=gate.controls[1:])
    ]

def toffoli_to_clifford_t(gate:Gate):
    return [Gate.create('H', gate.wires)] + [
        Gate.create('T', gate.controls[1]),
        Gate.create('T', gate.controls[0]),
        Gate.create('T', gate.wires),
        Gate.create('X', gate.controls[1], controls=gate.controls[0]),
        Gate.create('X', gate.controls[0], controls=gate.wires),
        Gate.create('X', gate.wires, controls=gate.controls[1]),
    ] + t_prime_to_clifford_t(gate.controls[0]) + [
        Gate.create('X', gate.controls[0], controls=gate.controls[1]),
    ] + t_prime_to_clifford_t(gate.controls[1]) + t_prime_to_clifford_t(gate.controls[0]) + [
        Gate.create('T', gate.wires),
        Gate.create('X', gate.controls[0], controls=gate.wires),
        Gate.create('X', gate.wires, controls=gate.controls[1]),
        Gate.create('X', gate.controls[1], controls=gate.controls[0]),
        Gate.create('H', gate.wires)
    ]

def multi_controlled_x_to_clifford_t(gate:Gate, digits:int, keep_toffoli:bool)->list[Gate]:    
    iterations = len(gate.controls) - 1
    gates = [gate]
    for _ in range(iterations):
        new_gates = []
        for gate in gates:
            if len(gate.controls) > 1 and not gate.is_toffoli():
                new_gates.extend(remove_a_control(gate))
            else:
                new_gates.append(gate)
        gates = new_gates

    new_gates = []
    for gate in gates:
        if gate.is_toffoli():
            if keep_toffoli:
                new_gates.append(gate)
            else:
                new_gates.extend(toffoli_to_clifford_t(gate))
        elif gate.is_cnot():
            new_gates.append(gate)
        elif gate.is_controlled():
            new_gates.append(Gate.create('H', gate.wires))
            new_gates.extend(controlled_z_to_clifford_t(Gate.create('Z', gate.wires, gate.exp, gate.controls), digits))
            new_gates.append(Gate.create('H', gate.wires))
        else:
            new_gates.append(gate)

    return new_gates

def make_aqft_approx(size:int, approx:int)->list[Gate]:
    if size < 1 or approx < 1:
        raise ValueError('Size and Approx must be positive integers')
    if approx > size:
        raise ValueError('Approx must be less than or equal to Size')

    operations = [create_reverse_gate(range(size))]
    for i in range(size):
        for j in range(min(i, approx - 1)):
            exp = Fraction(1, pow(2, j + 1))
            operations.append(Gate.create('Z', j, exp, controls=i))
        operations.append(Gate.create('H', i))

    return operations

def make_aqft_error(size:int, digits:int)->list[Gate]:
    if size < 1:
        raise ValueError('Size must be a positive integer')

    # TODO: Fix the error cutoff
    error = math.pow(10, -digits)
    operations = [create_reverse_gate(range(size))]
    for i in range(size):
        for j in range(i):
            exp = Fraction(1, pow(2, j + 1))
            phase = math.pi * float(exp)
            if phase > error:
                operations.append(Gate.create('Z', j, exp, i))
        operations.append(Gate.create('H', i))

    return operations

def make_catalytic_aqft_approx(size:int, approx:int)->list[Gate]:
    if size < 1 or approx < 1:
        raise ValueError('Size and Approx must be positive integers')
    if approx > size:
        raise ValueError('Approx must be less than or equal to Size')

    operations = []
    if approx > 1:
        for i in range(approx):
            operations.append(Gate.create('H', i + size))
            exp = Fraction(1, pow(2, approx - 1 - i))
            operations.append(Gate.create('Z', i + size, exp))

    operations.append(create_reverse_gate(range(size)))
    for i in range(size):
        for j in range(max(i - approx + 1, 0), i):
            min_point = size + approx - 1 - i + j
            for k in range(min_point, size + approx):
                controls = tuple([x for x in reversed(range(min_point, k))] + [i, j])
                operations.append(Gate.create('X', k, controls=controls))
        operations.append(Gate.create('H', i))

    return operations

def make_catalytic_aqft_error(size:int, digits:int)->list[Gate]:
    # TODO: Fix the error cutoff
    return make_catalytic_aqft_approx(size, size)

def clifford_t_conversion(circ:list[Gate], digits:int)->list[Gate]:
    new_circ = []
    for gate in circ:
        if gate.is_controlled() and gate.is_z_rotation():
            new_circ.extend(controlled_z_to_clifford_t(gate, digits))
        elif gate.is_clifford_t():
            new_circ.append(gate)
        elif gate.is_z_rotation():
            new_circ.extend(synthesize_z_rotation(gate, digits))
        elif gate.is_controlled() and gate.is_x_rotation():
            new_circ.extend(multi_controlled_x_to_clifford_t(gate, digits, False))
        elif gate.symbol == 'Rev':
            new_circ.extend(reverse_to_clifford_t(gate))
        else:
            raise ValueError(f"Unrecognized gate: {gate}")
    return new_circ

def toffoli_clifford_t_conversion(circ:list[Gate], digits:int)->list[Gate]:
    new_circ = []
    for gate in circ:
        if gate.is_controlled() and gate.is_z_rotation():
            new_circ.extend(controlled_z_to_clifford_t(gate, digits))
        elif gate.is_clifford_t() or gate.is_toffoli():
            new_circ.append(gate)
        elif gate.is_z_rotation():
            new_circ.extend(synthesize_z_rotation(gate, digits))
        elif gate.is_controlled() and gate.is_x_rotation():
            new_circ.extend(multi_controlled_x_to_clifford_t(gate, digits, True))
        elif gate.symbol == 'Rev':
            new_circ.extend(reverse_to_clifford_t(gate))
        else:
            raise ValueError(f"Unrecognized gate: {gate}")
    return new_circ

def convert_to(set:str, circ:list[Gate], digits:int)->list[Gate]:
    if set == 'clift':
        print('Converting to Clifford+T...')
        return clifford_t_conversion(circ, digits)
    elif set == 'tofclift':
        print('Converting to Toffoli+Clifford+T...')
        return toffoli_clifford_t_conversion(circ, digits)
    else:
        raise ValueError(f'Unexpected set type: {set}')

def count_clifford_t(circ:list[Gate])->tuple[int,int,int,int]:
    hadamard_count = 0
    s_count = 0
    t_count = 0
    cnot_count = 0
    for gate in circ:
        if gate.is_h():
            hadamard_count += 1
        elif gate.is_s():
            s_count += 1
        elif gate.is_t():
            t_count += 1
        elif gate.is_cnot():
            cnot_count += 1
        else:
            raise ValueError(f'Non Clifford+T gate found: {gate}')
    return hadamard_count, s_count, t_count, cnot_count

def count_clifford_t_fast(circ:list[Gate])->tuple[int,int,int,int]:
    hadamard_count = 0
    s_count = 0
    t_count = 0
    cnot_count = 0
    for gate in circ:
        if gate.is_h_fast():
            hadamard_count += 1
        elif gate.is_s_fast():
            s_count += 1
        elif gate.is_t_fast():
            t_count += 1
        else:
            cnot_count += 1
    return hadamard_count, s_count, t_count, cnot_count

def count_toffoli_clifford_t(circ:list[Gate])->tuple[int,int,int,int,int]:
    hadamard_count = 0
    s_count = 0
    t_count = 0
    cnot_count = 0
    tof_count = 0
    for gate in circ:
        if gate.is_h():
            hadamard_count += 1
        elif gate.is_s():
            s_count += 1
        elif gate.is_t():
            t_count += 1
        elif gate.is_cnot():
            cnot_count += 1
        elif gate.is_toffoli():
            tof_count += 1
        else:
            raise ValueError(f'Non Clifford+T gate found: {gate}')
    return hadamard_count, s_count, t_count, cnot_count, tof_count

def count_toffoli_clifford_t_fast(circ:list[Gate])->tuple[int,int,int,int,int]:
    hadamard_count = 0
    s_count = 0
    t_count = 0
    cnot_count = 0
    tof_count = 0
    for gate in circ:
        if gate.is_h_fast():
            hadamard_count += 1
        elif gate.is_s_fast():
            s_count += 1
        elif gate.is_t_fast():
            t_count += 1
        elif gate.is_cnot_fast():
            cnot_count += 1
        else:
            tof_count += 1
    return hadamard_count, s_count, t_count, cnot_count, tof_count

def count_qubits(circ:list[Gate])->int:
    wires = []
    for gate in circ:
        wires.extend(list(gate.wires))
        wires.extend(list(gate.controls))
    return max(wires) + 1 # Wires are zero-indexed

def print_gate_count(set:str, circ:list[Gate]):
    print('Counting gates...')
    if set == 'clift':
        hadamard_count, s_count, t_count, cnot_count = count_clifford_t(circ)
        print(f'H count:', hadamard_count)
        print(f'S count:', s_count)
        print(f'T count:', t_count)
        print(f'CNOT count:', cnot_count)
    elif set == 'tofclift':
        hadamard_count, s_count, t_count, cnot_count, tof_count = count_toffoli_clifford_t(circ)
        print(f'H count:', hadamard_count)
        print(f'S count:', s_count)
        print(f'T count:', t_count)
        print(f'CNOT count:', cnot_count)
        print(f'TOF count:', tof_count)
    else:
        raise ValueError(f'Unexpected set type: {set}')
    print(f'Total:', len(circ))
    num_qubits = count_qubits(circ)
    print('Qubits:', num_qubits)


def clifford_t_optimizer(circ:list[Gate])->list[Gate]:
    optimized = []

    for gate in circ:
        while True:
            if not optimized:
                break

            top = optimized[-1]

            same_wire = (top.wires == gate.wires)

            if same_wire and top.is_t_fast() and gate.is_t_fast():
                optimized.pop()
                gate = Gate.create('S', top.wires)
                continue

            if same_wire and top.is_h_fast() and gate.is_h_fast():
                optimized.pop()
                gate = None
                break

            if (top.is_cnot_fast() and gate.is_cnot_fast()
                and top.controls == gate.controls
                and top.wires == gate.wires):
                optimized.pop()
                gate = None
                break

            if gate.is_s_fast() and same_wire:
                s_count = 0
                idx = len(optimized) - 1
                while idx >= 0 and optimized[idx].is_s_fast() and optimized[idx].wires == gate.wires:
                    s_count += 1
                    idx -= 1

                if s_count + 1 == 4:
                    for _ in range(3):
                        optimized.pop()
                    gate = None
                    break

            break

        if gate is not None:
            optimized.append(gate)

    return optimized

def toffoli_clifford_t_optimizer(circ:list[Gate])->list[Gate]:
    # TODO
    raise NotImplementedError()

def optimizer(set:str, circ:list[Gate])->list[Gate]:
    if set == 'clift':
        return clifford_t_optimizer(circ)
    elif set == 'tofclift':
        return toffoli_clifford_t_optimizer(circ)
    else:
        raise ValueError(f'Unexpected set type: {set}')
