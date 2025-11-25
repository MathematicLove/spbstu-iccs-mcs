import itertools
import os
from typing import List, Tuple

# Ненастраиваемые параметры, их изменение может всё сломать (а может и нет)!
DISK_COUNT = 6
DISK_SIZE = 64
MESSAGE_SIZE = 12
BLOCK_SIZE = 3  # MESSAGE_SIZE / (DISK_COUNT - 2) два диска для избыточности
# Коеффициенты для вычисления блоков избыточности
X_COEFFICIENTS = [1, -1, 1, -1]  # X = A - B + C - D
Y_COEFFICIENTS = [1, 2, -3, 4]  # Y = A + 2 * B - 3 * C + 4 * D


def generate_matrix(data_disks: int, parity_disks: int) -> List[List[bool]]:
    """
    Генерирует матрицу размещений блоков данных и избыточности.

    Пример для 3 дисков данных и 1 диска избыточности:
    Address % 4   Disk 0   Disk 1   Disk 2   Disk 3
        0         True     True     True     False
        1         True     True     False    True
        2         True     False    True     True
        3         False    True     True     True

    Здесь True означает, что на этот диск должен загружаться блок данных,
    а False - блок избыточности. Порядок строк может отличаться, но все варианты
    размещений будут.

    Всего в такой матрице будет C из n по k строк, где n = data_disks, k = parity_disks.
    """
    # Создаем начальный список с a значениями True и b значениями False
    initial_list = [True] * data_disks + [False] * parity_disks

    # Генерируем все уникальные перестановки начального списка
    permutations = set(itertools.permutations(initial_list))

    # Преобразуем множество перестановок в список списков
    matrix = [list(perm) for perm in permutations]

    return matrix


def create_disk(disk_id: int) -> None:
    """Функция создает файл для диска. Если файл уже существует, он будет очищен."""
    with open(f"disk{disk_id}.txt", "w") as f:
        pass


def init_disks(disk_count: int) -> None:
    """Функция создает disk_count файлов. Если файл уже существует, он будет очищен."""
    for disk_id in range(disk_count):
        create_disk(disk_id)


def read_from_disk(disk_id: int, address: int) -> str | None:
    """
    Читает строку с указанным адресом из файла. Возвращает строку или None, если файл
    удалён, нет нужной строки или строка пуста.
    """
    try:
        with open(f"disk{disk_id}.txt", "r") as f:
            lines = f.readlines()
            if address < len(lines):
                line = lines[address].strip()
                return line if line else None
            else:
                return None
    except FileNotFoundError:
        return None


def write_to_disk(disk_id: int, address: int, data: str) -> None:
    """
    Записывает данные в указанную строку файла. Создаст файл, если его нет. Добавляет
    пустые строки, если нужно.
    """
    if not os.path.exists(f"disk{disk_id}.txt"):
        create_disk(disk_id)

    with open(f"disk{disk_id}.txt", "r+") as f:
        lines = f.readlines()
        if address < len(lines):
            lines[address] = data + "\n"
        else:
            lines += ["\n"] * (address - len(lines))
            lines.append(data + "\n")
        f.seek(0)
        f.writelines(lines)


def to_int(hex: str | None):
    """Преобразует сообщение из hex в число. None оставляет None."""
    return None if hex is None else int(hex, base=16)


def to_hex(num: int | None):
    """Преобразует число в hex. None оставляет None."""
    # Важно использовать именно replace, а не просто отбрасывать первые два символа,
    # так как перед 0x может быть минус
    return None if num is None else hex(num).replace("0x", "").upper()


def int_blocks_to_hex_message(data_blocks: List[int]) -> str:
    """Переводит блоки данных из чисел в hex и склеивает."""
    return "".join([to_hex(d) for d in data_blocks])


def poly_parity(data_blocks: List[str]) -> Tuple[str]:
    """Возвращает две строки - блоки избыточности. Принимает список блоков сообщения."""
    # Чтобы что-то считать, нужно сначала перевести блоки сообщения из hex в числа
    data_blocks = [to_int(block) for block in data_blocks]

    # Просто вычисляем полиномы
    X = sum([с * d for с, d in zip(X_COEFFICIENTS, data_blocks)])
    Y = sum([с * d for с, d in zip(Y_COEFFICIENTS, data_blocks)])

    return to_hex(X), to_hex(Y)


def check_disks(disk_count: int) -> List[int]:
    """Проверяет работоспособность дисков. Возвращает индексы неработающих."""
    invalid_disks = []

    for disk_id in range(disk_count):
        if not os.path.exists(f"disk{disk_id}.txt"):
            invalid_disks.append(disk_id)

    return invalid_disks


def write(matrix: List[List[bool]], address: int, message: str):
    """
    Сохраняет сообщение на диск. Ожидается, что при вызове этой функции
    все диски в рабочем состоянии. matrix - это матрица размещений блоков данных и
    блоков избыточности (см. ф-ю generate_matrix).
    """
    # Разделяем сообщение на блоки
    data_blocks = [
        message[i : i + BLOCK_SIZE] for i in range(0, len(message), BLOCK_SIZE)
    ]

    # Вычисляем блоки избыточности
    parity_blocks = poly_parity(data_blocks)

    # Выбираем нужный вариант размещения
    row = matrix[address % len(matrix)]

    # Записываем блоки на диски
    data_block_index = 0
    parity_block_index = 0

    for disk_id, is_data in enumerate(row):
        if is_data:
            write_to_disk(disk_id, address, data_blocks[data_block_index])
            print(
                f"Блок данных {data_blocks[data_block_index]} записан на диск {disk_id}."
            )
            data_block_index += 1
        else:
            write_to_disk(disk_id, address, parity_blocks[parity_block_index])
            print(
                f"Блок избыточности {parity_blocks[parity_block_index]} записан на диск {disk_id}."
            )
            parity_block_index += 1


def read(matrix: List[List[bool]], address: int):
    """
    Читает сообщение с диска. Ожидается, что при вызове этой функции не больше двух
    дисков в нерабочем состоянии. matrix - это матрица размещений блоков данных и
    блоков избыточности (см. ф-ю generate_matrix).
    """
    # Выбираем нужный вариант размещения
    row = matrix[address % len(matrix)]

    # Читаем с дисков всё, что можно прочитать
    data_blocks = []
    valid_data_blocks = 0
    parity_blocks = []
    valid_parity_blocks = 0

    for disk_id, is_data in enumerate(row):
        info_from_disk = read_from_disk(disk_id, address)

        # Сохраняем результат считывания, даже если это None (диск повреждён)
        if is_data:
            data_blocks.append(info_from_disk)

            if info_from_disk is not None:
                valid_data_blocks += 1
                print(
                    f"Удалось прочитать блок данных с диска {disk_id} по адресу {address}."
                )
            else:
                print(
                    f"Не удалось прочитать блок данных с диска {disk_id} по адресу {address}."
                )
        else:
            parity_blocks.append(info_from_disk)

            if info_from_disk is not None:
                valid_parity_blocks += 1
                print(
                    f"Удалось прочитать блок избыточности с диска {disk_id} по адресу {address}."
                )
            else:
                print(
                    f"Не удалось прочитать блок избыточности с диска {disk_id} по адресу {address}."
                )

    # Случай, когда либо ничего не повреждено, либо повреждены только блоки избыточности
    if valid_data_blocks == DISK_COUNT - 2:
        return "".join(data_blocks)

    # Если повреждено больше двух дисков, то восстановить данные не сможем
    if valid_data_blocks + valid_parity_blocks < DISK_COUNT - 2:
        raise RuntimeError(
            "Ошибка чтения! Повреждено более двух дисков, либо данные не были добавлены. "
            "Данные восстановить невозможно."
        )

    # Тут точно знаем, что повреждён один или два блока данных, а также ноль или один
    # блок избыточности
    print("Один или два блока данных повреждены, восстанавливаем данные...")

    data_blocks = [to_int(block) for block in data_blocks]
    parity_blocks = [to_int(block) for block in parity_blocks]

    X, Y = parity_blocks

    # Повреждены два блока данных
    if valid_data_blocks == DISK_COUNT - 4:
        # Определяем индексы первого и второго недостающих блоков данных
        first_invalid, second_invalid = [
            i for i, d in enumerate(data_blocks) if d is None
        ]

        # Выражаем первую неизвестную из первого уравнения

        # Вычислить точное значение мы не можем из-за второй неизвестной,
        # для неё просто найдём коеффициент. Знак меняем из-за переноса.
        # Пример: 12 = 2A + 3B + 8 => коеффициент перед B будет -3/2.
        second_coeff = -X_COEFFICIENTS[second_invalid] / X_COEFFICIENTS[first_invalid]

        # Значения остальных переменных нам известны, поэтому их можно просто вычислить
        first_eq_remainder = X

        for i, (d, c) in enumerate(zip(data_blocks, X_COEFFICIENTS)):
            if i in {first_invalid, second_invalid}:
                continue
            first_eq_remainder -= c * d

        # Не забываем поделить на коеффициент перед первой неизвестной
        first_eq_remainder /= X_COEFFICIENTS[first_invalid]

        # Теперь первая переменная выражена через вторую и некоторое число
        # Пример: 12 = 2A + 3B + 8 => A = (12 - 8)/2 - 3/2B = 2 - 3/2B

        # Теперь надо подставить получившееся значение во второе уравнение.
        # Для этого первую неизвестную надо просто заменить получившимся числом,
        # умноженным на коеффициент первой неизвестной во втором уравнении,
        # а к коеффициенту второй неизвестной во втором уравенении прибавить найденный
        # ранее коеффициент умноженный на коеффициент первой неизвестной во втором
        # уравнении.

        final_second_coeff = (
            second_coeff * Y_COEFFICIENTS[first_invalid]
            + Y_COEFFICIENTS[second_invalid]
        )

        second_eq_remainder = Y
        for i, (d, c) in enumerate(zip(data_blocks, Y_COEFFICIENTS)):
            if i == second_invalid:
                continue
            if i == first_invalid:
                # Меняем знак из-за переноса
                second_eq_remainder -= first_eq_remainder * c
                continue

            second_eq_remainder -= c * d

        # Теперь мы можем вычислить вторую неизвестную
        data_blocks[second_invalid] = int(second_eq_remainder / final_second_coeff)

    # Повреждён один блок данных и один блок избыточности, либо мы уже восстановили
    # один блок данных в выше

    # Выбираем полином, из которого будем восстанавливать данные
    if X is not None:
        coefficients = X_COEFFICIENTS
        parity = X
    else:
        coefficients = Y_COEFFICIENTS
        parity = Y

    # Определяем индекс недостающего блока данных
    invalid_index = data_blocks.index(None)

    # Решаем уравнение и восстанавливаем недостающий блок данных
    # Переносим всё в левую часть, например:
    # X = A - B + C - D => X - (-B + C - D) = A

    # Не меняем знак у значения избыточности, потому что не переносим его
    # на другую сторону
    data_blocks[invalid_index] = parity

    for i, (d, c) in enumerate(zip(data_blocks, coefficients)):
        if i == invalid_index:
            continue
        data_blocks[invalid_index] -= c * d  # Добавляем минус при переносе

    # Делим результат на коеффициент перед переменной
    # Знак не меняем, так как оставили её справа
    data_blocks[invalid_index] = int(
        data_blocks[invalid_index] / coefficients[invalid_index]
    )

    return int_blocks_to_hex_message(data_blocks)


def main():
    matrix = generate_matrix(DISK_COUNT - 2, 2)

    print(
        "\nРеализация RAID-6 массива из 6 дисков с длинной сообщения в 12 байт.\n"
        "Избыточность подсчитывается с помощью полиномов:\n"
        "X = A - B + C - D\n"
        "Y = A + 2B - 3C + 4D\n\n"
        "Адрес задаётся десятичным числом от 0 до 63.\n"
        "Примеры команд:\n"
        "'0 write 123456789ABC' - записать данные в массив по адресу 0,\n"
        "'0 read' - прочитать данные из массива по адресу 0,\n"
        "'restart' - очистить диски и начать заново,\n"
        "'exit' - выйти из программы."
    )

    while True:
        init_disks(DISK_COUNT)
        print("Диски созданы.")

        while True:
            command = input(" >>> ").strip()

            if command == "restart":
                break

            if command == "exit":
                return

            parts = command.split()

            if len(parts) < 2:
                print("Неккоректная команда!")
                continue

            try:
                address = int(parts[0], 16)
            except ValueError:
                print("Неккоректный адрес!")
                continue

            if address < 0 or address >= DISK_SIZE:
                print(
                    f"Неккоректный адрес! {address} выходит за допустимый диапазон [0, {DISK_SIZE - 1}]!"
                )

            operation = parts[1]

            if operation == "write" and len(parts) == 3:
                message = parts[2]

                if len(message) != MESSAGE_SIZE:
                    print("Неккоректная длина сообщения!")
                    continue

                try:
                    int(message, 16)
                except ValueError:
                    print(
                        "Неккорректное сообщение! Пожалуйста, используйте только цифры "
                        "от 0 до 9 или латинские буквы от A до F."
                    )
                    continue

                if message.startswith("-"):
                    print(
                        "Неккорректное сообщение! Пожалуйста, используйте только цифры "
                        "от 0 до 9 или латинские буквы от A до F."
                    )
                    continue

                write(matrix, address, message.upper())
            elif operation == "read" and len(parts) == 2:
                try:
                    print("Считанные данные: ", read(matrix, address))
                except RuntimeError as e:
                    print(e)
            else:
                print("Неккоректная команда!")


if __name__ == "__main__":
    main()
