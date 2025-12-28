import concurrent.futures
import random
import sys, os
from zipfile import error
import time

sys.path.append(os.path.abspath(os.path.join(os.path.dirname(__file__), '..')))
import numpy as np

from constants import RX, CNOT_func, I, H, TOFFOLI, X, Z
from symulator import NQubitSimulator


def shor9(P=0.05, debug=False):
    global pauli_x_error
    sim = NQubitSimulator(9) # 9 qub
    sim.apply_single_qubit_gate(RX(np.pi / 5), 0) # R к 0-му Pi/5
    inital_state = sim.get_qubit_state_raw(0) # сохраняем начальное => 1 логический 0 + 8 вспомогат.

    if debug:
        print('Initial system state:')
        for i in range(sim.dimension):
            print(f'State {i}: {sim.get_qubit_state(i)}')

    # Coding.
    # S1
    CNOT_03 = CNOT_func(9, 0, 3) # Кодирование состояния 0 в кубит 3
    sim.apply_n_qubit_gate(CNOT_03)

    # S2
    CNOT_06 = CNOT_func(9, 0, 6) # Кодирование состояния 0 в кубит 6
    sim.apply_n_qubit_gate(CNOT_06)
    # => тройка запутанных 0,3,6
    # S3
    sim.apply_n_gates(H, I, I, H, I, I, H, I, I) # Гейты адамара к 0,3,6 Z-> X что б убрать Z ошибки

    # S4
    CNOT_01 = CNOT_func(N=9, c=0, t=1) # Еще кодируем кубиты в 1 в 4 в 7 = (0,1,2)
    CNOT_34 = CNOT_func(N=9, c=3, t=4) # 3,4,5
    CNOT_67 = CNOT_func(N=9, c=6, t=7) # 6,7,8
    S4_operator = np.dot(np.dot(CNOT_01, CNOT_34), CNOT_67)
    sim.apply_n_qubit_gate(S4_operator)

    # S5
    CNOT_02 = CNOT_func(N=9, c=0, t=2)
    CNOT_35 = CNOT_func(N=9, c=3, t=5)
    CNOT_68 = CNOT_func(N=9, c=6, t=8)
    S5_operator = np.dot(np.dot(CNOT_02, CNOT_35), CNOT_68)
    sim.apply_n_qubit_gate(S5_operator)

    if debug:
        print('Finishing coding part:')
        for i in range(sim.dimension):
            print(f'State {i}: {sim.get_qubit_state(i)}')

    error_count = 0

    for idx in range(0, sim.dimension):
        if random.random() <= P: # с вероятностью P
            error_count += 1
            if error_count > 1:
                return False, error_count
            if debug:
                print(f'Error in {idx}')
                # sim.apply_single_qubit_gate(X, idx) # X-ошибка ЛИБО ПОСТОЯННО Х
            pauli_x_error = random.random() > 0.5
            if pauli_x_error:
                sim.apply_single_qubit_gate(X, idx) # X-ошибка
            else:
                sim.apply_single_qubit_gate(Z, idx) # Z-ошибка
    if debug:
        print('Finishing error simulation part:')
        for i in range(sim.dimension):
            print(f'State {i}: {sim.get_qubit_state(i)}')

        if error_count:
            print(f'{error_count} - P: {P}')
    # Decoding
    # S6 - clone of S4
    sim.apply_n_qubit_gate(S4_operator) # декод отменяет запутаность

    # S7 - clone of S5
    sim.apply_n_qubit_gate(S5_operator) # так же

    # S8
    TOFFOLI_120 = TOFFOLI(N=9, controls=[1, 2], target=0) # Тоффоли меняет целевой кубит если оба 1
    TOFFOLI_453 = TOFFOLI(N=9, controls=[4, 5], target=3)
    TOFFOLI_876 = TOFFOLI(N=9, controls=[8, 7], target=6)

    # Комбинируем все Toffoli-гейты в один оператор
    S8_operator = np.dot(np.dot(TOFFOLI_120, TOFFOLI_453), TOFFOLI_876)
    sim.apply_n_qubit_gate(S8_operator)

    # S9 - clone of S3
    sim.apply_n_gates(H, I, I, H, I, I, H, I, I)

    # S10 - clone of S1
    sim.apply_n_qubit_gate(CNOT_03)

    # S11 - clone of S2
    sim.apply_n_qubit_gate(CNOT_06)

    # S12
    TOFFOLI_360 = TOFFOLI(N=9, controls=[3, 6], target=0) # Коррекция от двух других троек
    sim.apply_n_qubit_gate(TOFFOLI_360)
    if debug:
        print('Finishing decoding part')
        for i in range(sim.dimension):
            print(f'State {i}: {sim.state[0]}')

    finite_state = sim.get_qubit_state_raw(0)
    if np.isclose(finite_state['|0>'], inital_state['|0>'], 0.01) and np.isclose(finite_state['|1>'],
                                                                                 inital_state['|1>'], 0.01):
        return (True, error_count) # исправели
    else:
        return (False, error_count)

def no_correction(P=0.05, debug = False): # для ошибок без коррекций

    sim = NQubitSimulator(1)
    sim.apply_single_qubit_gate(RX(np.pi / 5), 0)
    inital_state = sim.get_qubit_state_raw(0)

   # ошибк
    error_count = 0
    if random.random() <= P:
        error_count += 1
        if debug:
            print(f'Error applied')
        sim.apply_single_qubit_gate(X, 0)

    # Check.
    finite_state = sim.get_qubit_state_raw(0)
    if np.isclose(finite_state['|0>'], inital_state['|0>'], 0.0001) and np.isclose(finite_state['|1>'],
                                                                                 inital_state['|1>'], 0.0001):
        return True, error_count
    else:
        return False, error_count

def compute_failure_probability(P, total_rounds=500): # 500 раз
    failure_count = 0
    failure_count2 = 0
    start_time = time.time()

    # Если P == 0, то вероятность ошибки = 0
    if P == 0:
        return (P, 0, 0)

    for i in range(total_rounds):
        correct = shor9(P=P)
        if not correct[0]:
            failure_count += 1

    p_e = failure_count / total_rounds  # все вер
    p_e_nc = P  # Все без
    print(f'finished P={P}. Spent time: {time.time() - start_time}')
    return (P, p_e, p_e_nc)

def parallel_compute_failure_probability(p_values, total_rounds=500): # ускоряем
    results = []
    with concurrent.futures.ProcessPoolExecutor() as executor:
        results = list(executor.map(compute_failure_probability, p_values, [total_rounds]*len(p_values)))
    return results

def theory(p):
    return 1 - (1 + 8 * p) * (1 - p) ** 8

def shor9_withplot():
    import matplotlib.pyplot as plt

    p = [0, 0.01, 0.02, 0.03, 0.04, 0.05, 0.06, 0.07, 0.08, 0.09, 0.1] # вероятности ошибок
    p_e = [] # c
    p_e_nc = [] # без
    total_rounds = 500

    results = parallel_compute_failure_probability(p, total_rounds)


    for result in results:
        P, p_e_value, p_e_nc_value = result
        p_e.append(p_e_value)
        p_e_nc.append(p_e_nc_value)

        print(f'finished {P} with P_e: {p_e_value} || P_e_nc: {p_e_nc_value}')

    plt.plot(p, p_e, marker='o',label='Исправление ошибки', linestyle='-', color='b', alpha=0.7)
    plt.plot(p, p_e_nc,label='Без исправления', marker='o', linestyle='-', color='r', alpha=0.7)

    p_values = np.linspace(0, max(p), 100)
    theory_values = theory(p_values)

    plt.plot(p_values, theory_values, label='Теоретическое значение', color='green', linestyle='--', linewidth=2.5)
    plt.xlabel('Вероятность ошибки P')
    plt.ylabel('Общая вероятность ошибки P_e')
    plt.title('Зависимость вероятности ошибки от P')
    plt.grid(True)
    plt.legend()
    plt.savefig('error_probability_graph.pdf', format='pdf')
    plt.show()


if __name__ == '__main__':
    shor9_withplot()