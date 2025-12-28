import numpy as np
from shors_9_cubit_code.interface import Qubit, QuantumDevice
from shors_9_cubit_code.constants import H, X, KET_0,rotation_matrix, CNOT, P_0, P_1
import itertools


class SimulatedQubit(Qubit):
    def __init__(self):
        self.reset()

    def h(self):
        """Применение матрицы Адамара (50/50 распределение)."""
        self.state = H @ self.state

    def x(self):
        """Применение матрицы х (NOT) чтобы инвертировать состояние кубита"""
        self.state = X @ self.state

    def rotation(self, theta:float):
        """Применение поворотной матрицы для изменения распределения."""
        R_THETA = rotation_matrix(theta)
        self.state = R_THETA @ self.state

    def measure(self) -> bool: #смотри ниже объяснение работы этой функции
        """Измерение состояния кубита."""
        pr0 = np.abs(self.state[0, 0]) ** 2
        #вероятность состояния |0⟩, которая вычисляется как квадрат модуля амплитуды вероятности
        sample = np.random.random() <= pr0
        #генерируется случайное число от 0 до 1. Если оно меньше или равно вероятности состояния |0⟩,
        # результатом будет 0, иначе 1
        return bool(0 if sample else 1)

    def reset(self):
        """Сброс состояния кубита в |0>."""
        self.state = KET_0.copy()


class SingleQubitSimulator(QuantumDevice):
    def allocate_qubit(self) -> Qubit:
        return SimulatedQubit()

    def deallocate_qubit(self, qubit: Qubit):
        pass  # Не нужно ничего делать для симулятора


class TwoQubitSimulator(QuantumDevice):
    qubits = [SimulatedQubit(), SimulatedQubit()]
    state = np.kron(qubits[0].state, qubits[1].state) #тензорное произведение

    def __init__(self):
        self.reset()
    def allocate_qubit(self) -> SimulatedQubit:
        if self.qubits:
            return self.qubits.pop()

    def deallocate_qubit(self, qubit: SimulatedQubit):
        self.qubits.append(qubit)

    def apply_single_qubit_gate(self, gate, qubit_idx: int):
        if qubit_idx == 0:
            identity = np.eye(2)
            operation = np.kron(gate, identity)  # gate к 1му
        elif qubit_idx == 1:
            identity = np.eye(2)
            operation = np.kron(identity, gate)  # gate ко 2му
        else:
            raise ValueError("Недопустимый индекс кубита.")

        # Apply operation to quantum state.
        self.state = operation @ self.state

    def apply_two_qubit_gate(self, gate):
        """Применяем двухкубитную операцию к состоянию."""
        self.state = gate @ self.state  # применяем двухкубитную операцию ко всей системе

    def cnot(self):
        self.apply_two_qubit_gate(CNOT)

    def measure(self, qubit_idx: int) -> bool:
        """
        Измерить состояние одного кубита в системе
        """
        if qubit_idx == 0:
            # Вероятность |00> или |01>.
            probability0 = np.abs(self.state[0, 0]) ** 2 + np.abs(self.state[1, 0]) ** 2
        elif qubit_idx == 1:
            # Вероятность |00> или |10>.
            probability0 = np.abs(self.state[0, 0]) ** 2 + np.abs(self.state[2, 0]) ** 2
        else:
            raise ValueError("Недопустимый индекс кубита.")

        is_measured_0 = np.random.random() <= probability0
        return bool(0 if is_measured_0 else 1)

    def reset(self):
        # Сбрасываем состояние каждого кубита индивидуально
        for qubit in self.qubits:
            qubit.reset()
        # Сбрасываем состояние всей системы (два кубита)
        self.state = np.kron(self.qubits[0].state, self.qubits[1].state)  # Сбрасываем систему в |00> состояние
        #self.state = np.kron(KET_0, KET_0)  # Сбрасываем систему в |00> состояние

    def set_state(self, state):
        self.state = state



import numpy as np
import itertools

class NQubitSimulator:
    def __init__(self, n: int):
        qubits = [SimulatedQubit() for _ in range(n)]
        self.dimension = n
        self.reset()
        self.collapsed = [None for _ in range(n)]  # Инициализация коллапсированных кубитов

    def apply_single_qubit_gate(self, gate, qubit_idx: int):
        if qubit_idx < 0 or qubit_idx >= self.dimension:
            raise ValueError(f"Недопустимый индекс кубита. Ожидается индекс в диапазоне [0;{self.dimension - 1}].")

        # Создаем тензорный оператор для применения гейта к заданному кубиту
        operation = np.eye(1)  # Начальный оператор - тождественная матрица размерности 1

        for i in range(self.dimension):
            if i == qubit_idx:
                operation = np.kron(operation, gate)  # Применяем гейт для выбранного кубита
            else:
                operation = np.kron(operation, np.eye(2))  # Для остальных - тождественная матрица (I)

        self.state = operation @ self.state

    def apply_n_qubit_gate(self, gate):
        self.state = gate @ self.state

    def measure(self, qubit_idx: int) -> int:
        """
        Измерить состояние одного кубита в системе.
        Если кубит уже был измерен, вернуть сохраненное значение.
        """
        if self.collapsed[qubit_idx] is not None:
            # Если кубит уже был измерен, возвращаем коллапсированное значение
            return self.collapsed[qubit_idx]

        if qubit_idx > self.dimension or qubit_idx < 0:
            raise ValueError("Недопустимый индекс кубита.")

        # Вероятность того, что кубит находится в состоянии |0>
        prob_zero = 0
        combinations = list(itertools.product([0, 1], repeat=self.dimension))

        for i in range(len(combinations)):
            if combinations[i][qubit_idx] == 0:
                prob_zero += np.abs(self.state[i, 0]) ** 2

        is_measured_zero = np.random.random() <= prob_zero
        measured_value = 0 if is_measured_zero else 1

        # Сохраняем результат измерения
        self.collapsed[qubit_idx] = measured_value
        return measured_value

    def get_qubit_state(self, idx: int):
        if idx < 0 or idx >= self.dimension:
            raise ValueError(f"Invalid qubit index. Must be in range [0; {self.dimension}).")

        # Инициализируем проекторы для |0> и |1> состояний
        projector_0 = np.eye(1)  # Начальная единичная матрица для проектора на |0>
        projector_1 = np.eye(1)  # Начальная единичная матрица для проектора на |1>

        for i in range(self.dimension):
            if i == idx:
                projector_0 = np.kron(projector_0, P_0)  # Проектор на |0> для кубита idx
                projector_1 = np.kron(projector_1, P_1)  # Проектор на |1> для кубита idx
            else:
                projector_0 = np.kron(projector_0, np.eye(2))  # Для остальных кубитов — единичная матрица
                projector_1 = np.kron(projector_1, np.eye(2))

        # Применяем проекторы к текущему состоянию
        projected_state_0 = projector_0 @ self.state
        projected_state_1 = projector_1 @ self.state

        # Вычисляем вероятности для |0> и |1>
        prob_0 = np.abs(np.vdot(projected_state_0, projected_state_0))
        prob_1 = np.abs(np.vdot(projected_state_1, projected_state_1))

        return {'|0>': prob_0, '|1>': prob_1}

    def measure_multiple_qubits(self, qubit_indices: list) -> list:
        """
        Измерить несколько кубитов за один шаг.
        :param qubit_indices: Список индексов кубитов.
        :return: результат измерений.
        """
        for qubit_idx in qubit_indices:
            if qubit_idx >= self.dimension or qubit_idx < 0:
                raise ValueError(f"Invalid qubit index: {qubit_idx}. Only in range [0; {self.dimension}).")

        # Все возможные варианты исходов: |000...000>, |000...001>, ..., |111...111>
        possible_outcomes = list(itertools.product([0, 1], repeat=len(qubit_indices)))

        probabilities = []
        projectors = []

        for outcome in possible_outcomes:
            operator = np.eye(1)
            outcome_projectors = []

            # Генерация оператора проектора для каждого исхода
            for idx, qubit_idx in enumerate(qubit_indices):
                projector = P_0 if outcome[idx] == 0 else P_1
                outcome_projectors.append(projector)

            # Тензорное произведение операторов
            for i in range(self.dimension):
                if i in qubit_indices:
                    idx_in_measure = qubit_indices.index(i)
                    operator = np.kron(operator, outcome_projectors[idx_in_measure])
                else:
                    operator = np.kron(operator, np.eye(2))

            # Применяем оператор проектора к системе
            projected_state = operator @ self.state
            probability = np.abs(np.vdot(projected_state, projected_state))

            # Сохраняем вероятность и проектор для каждого исхода
            probabilities.append(probability)
            projectors.append(operator)

        # Нормализуем вероятности
        total_probability = sum(probabilities)
        probabilities = [p / total_probability for p in probabilities]

        random_value = np.random.random()
        cumulative_probability = 0
        measured_outcome = None

        # Выбор результата измерения
        for idx, prob in enumerate(probabilities):
            cumulative_probability += prob
            if random_value <= cumulative_probability:
                measured_outcome = possible_outcomes[idx]
                break

        # Коллапсируем состояние
        projector = projectors[possible_outcomes.index(measured_outcome)]
        self.state = projector @ self.state / np.sqrt(probabilities[possible_outcomes.index(measured_outcome)])
        for idx, qubit_idx in enumerate(qubit_indices):
            self.collapsed[qubit_idx] = measured_outcome[idx]

        return list(measured_outcome)

    def controlled_by_measurement(self, gate_if_0, gate_if_1, measured_value, target_qubit_idx: int):
        """
        Применить гейт к целевому кубиту на основе результата измерения.
        :param gate_if_0: Гейт для применения, если измеренное значение 0.
        :param gate_if_1: Гейт для применения, если измеренное значение 1.
        :param measured_value: Результат измерения (0 или 1).
        :param target_qubit_idx: Индекс целевого кубита, к которому применяется гейт.
        """
        if measured_value == 0:
            self.apply_single_qubit_gate(gate_if_0, target_qubit_idx)
        elif measured_value == 1:
            self.apply_single_qubit_gate(gate_if_1, target_qubit_idx)
        else:
            raise ValueError(f"Invalid measured value {measured_value}. Must be 0 or 1.")

    def set_state(self, state):
        self.state = state

    def get_state(self):
        return self.state

    def reset(self):
        """Сбрасываем состояние системы кубитов в |00...0>"""
        self.state = np.copy(KET_0)  # Начинаем с состояния |0>
        for _ in range(1, self.dimension):
            self.state = np.kron(self.state, KET_0)  # Применяем тензорное произведение для каждого кубита
        self.collapsed = [None for _ in range(self.dimension)]  # Сброс коллапсированных кубитов

    def get_qubit_state_raw(self, idx: int):
        if idx < 0 or idx >= self.dimension:
            raise ValueError(f"Invalid qubit index. Must be in range [0; {self.dimension}).")

        P_0 = np.array([[1, 0], [0, 0]])
        P_1 = np.array([[0, 0], [0, 1]])

        projector_0 = np.eye(1)
        projector_1 = np.eye(1)

        for i in range(self.dimension):
            if i == idx:
                projector_0 = np.kron(projector_0, P_0)
                projector_1 = np.kron(projector_1, P_1)
            else:
                projector_0 = np.kron(projector_0, np.eye(2))
                projector_1 = np.kron(projector_1, np.eye(2))

        projected_state_0 = projector_0 @ self.state
        projected_state_1 = projector_1 @ self.state

        amplitude_0 = projected_state_0[0]
        amplitude_1 = projected_state_1[0]
        return {'|0>': sum(projected_state_0), '|1>': sum(projected_state_1)}

    def apply_n_gates(self, *gates):
        # Start with the first gate
        operation = gates[0]

        # Perform Kronecker product for each gate, if no gate provided for a specific qubit, use identity matrix
        for i in range(1, len(gates)):
            gate = gates[i]
            if gate is None:
                gate = np.eye(2)  # Identity gate for qubits that don't have a specific gate
            operation = np.kron(operation, gate)

        # Apply the final operation to the quantum state
        self.state = operation @ self.state