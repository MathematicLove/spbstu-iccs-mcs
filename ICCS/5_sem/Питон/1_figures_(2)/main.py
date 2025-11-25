class Shape:
    def __init__(self, x=0, y=0):
        self.x = x
        self.y = y
    @property
    def area(self):
        raise NotImplementedError("Метод area должен быть реализован в подклассе")

class Rectangle(Shape):
    def __init__(self, width, height, x=0, y=0):
        super().__init__(x, y)
        self.width = width
        self.height = height
    @property
    def width(self):
        return self._width

    @width.setter
    def width(self, value):
        if value <= 0:
            raise ValueError("Ширина должна быть положительным числом")
        self._width = value

    @property
    def height(self):
        return self._height

    @height.setter
    def height(self, value):
        if value <= 0:
            raise ValueError("Высота должна быть положительным числом")
        self._height = value

    @property
    def area(self):
        return self.width * self.height

class Square(Rectangle):
    def __init__(self, side, x=0, y=0):
        super().__init__(side, side, x, y)
    @Rectangle.width.setter
    def width(self, value):
        if value <= 0:
            raise ValueError("Сторона квадрата должна быть положительным числом")
        self._width = self._height = value

    @Rectangle.height.setter
    def height(self, value):
        if value <= 0:
            raise ValueError("Сторона квадрата должна быть положительным числом")
        self._width = self._height = value

    @property
    def side(self):
        return self.width

    @side.setter
    def side(self, value):
        self.width = self.height = value


def main():
    rect = Rectangle(10, 20, 0, 0)
    print(f"Прямоугольник: ширина={rect.width}, высота={rect.height}, площадь={rect.area}")
    square = Square(5, 0, 0)
    print(f"Квадрат: сторона={square.side}, площадь={square.area}")
    rect.width = 15
    rect.height = 25
    print(f"Измененный прямоугольник: ширина={rect.width}, высота={rect.height}, площадь={rect.area}")
    square.side = 10
    print(f"Измененный квадрат: сторона={square.side}, площадь={square.area}")

if __name__ == "__main__":
    main()
