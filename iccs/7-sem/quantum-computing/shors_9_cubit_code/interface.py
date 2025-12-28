from abc import ABCMeta, abstractmethod
from contextlib import contextmanager
class Qubit(metaclass=ABCMeta):
    @abstractmethod
    def h(self): pass #Применение матрицы Адамара

    @abstractmethod
    def x(self): pass # квантовый NOT

    @abstractmethod
    def measure(self) -> bool: pass
    # Измерение состояния кубита, возвращает True(1) или False(0)

    @abstractmethod
    def reset(self): pass # Сбрасывает состояние кубита в |0⟩

class QuantumDevice(metaclass=ABCMeta):
    @abstractmethod
    def allocate_qubit(self) -> Qubit: #выделение кубита
        pass

    @abstractmethod
    def deallocate_qubit(self, qubit: Qubit): #освобождение кубита
        pass

    @contextmanager #@contextmanager позволяет использовать метод в блоках with
    def using_qubit(self):
        qubit = self.allocate_qubit()
        try:
            yield qubit #yield - возврат
        finally:
            qubit.reset()
        self.deallocate_qubit(qubit)