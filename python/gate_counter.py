from Functions import *
import argparse
import time

def main():
    parser = argparse.ArgumentParser('gate_counter')
    parser.add_argument('type', type=str, choices=['qft', 'aqft', 'catqft', 'cataqft'], help='The type of QFT circuit to use')
    parser.add_argument('set', type=str, choices=['clift', 'tofclift'], help='The gate set to convert to')
    parser.add_argument('size', type=int, help='Size of the AQFT circuit')
    parser.add_argument('digits', type=int, help='Number of digits')
    parser.add_argument('--show', action='store_true', help='Print the circuits to the console after generating them')
    parser.add_argument('--time', action='store_true', help='Time the operations')
    parser.add_argument('--optimize', action='store_true', help='Enable optimization')
    args = parser.parse_args()

    TYPE = args.type
    SET = args.set
    SIZE = args.size
    DIGITS = args.digits
    show = args.show
    timer = args.time
    optimize = args.optimize
    
    # Make the original circuit.
    if TYPE == 'qft':
        qft_circ = make_aqft_approx(SIZE, SIZE)
    elif TYPE == 'aqft':
        qft_circ = make_aqft_error(SIZE, DIGITS)
    elif TYPE == 'catqft':
        qft_circ = make_catalytic_aqft_approx(SIZE, SIZE)
    elif TYPE == 'cataqft':
        qft_circ = make_catalytic_aqft_error(SIZE, DIGITS)
    else:
        raise ValueError(f'Unexpected circuit type: {TYPE}')
    if show:
        print('Original AQFT:')
        print('\n'.join(map(str, qft_circ)))
    
    # Convert the circuit to a different set of gates.
    if timer:
        start_time = time.time()
    conv_qft_circ = convert_to(SET, qft_circ, DIGITS)
    if timer:
        print(f"Conversion took {time.time() - start_time:.4f} seconds")
    print('Clifford+T AQFT:')
    if show:
        print('\n'.join(map(str, conv_qft_circ)))
    
    # Show the gate count.
    print_gate_count(SET, conv_qft_circ)
    
    if optimize:
        # Optimize the gate count.
        if timer:
            start_time = time.time()
        print('Optimizing...')
        optimized_circ = optimizer(SET, conv_qft_circ)
        if timer:
            print(f"Optimization took {time.time() - start_time:.4f} seconds")
        print('Clifford+T AQFT:')
        if show:
            print('\n'.join(map(str, optimized_circ)))
        
        # Show the new gate count.
        print_gate_count(SET, optimized_circ)

if __name__ == '__main__':
    main()
