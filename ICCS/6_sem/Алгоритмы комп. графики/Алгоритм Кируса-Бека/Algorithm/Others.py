import matplotlib.pyplot as plt
import numpy as np
import random

# -- Биты (константы) --
LEFT   = 1   # 0001
RIGHT  = 2   # 0010
BOTTOM = 4   # 0100
TOP    = 8   # 1000

def compute_outcode(x, y, xmin, ymin, xmax, ymax):
    """
    Вычисляем 4-битный код региона (Outcode) для точки (x, y).
    """
    code = 0
    if x < xmin:
        code |= LEFT
    elif x > xmax:
        code |= RIGHT
    if y < ymin:
        code |= BOTTOM
    elif y > ymax:
        code |= TOP
    return code

def cohen_sutherland_clip_stepwise(x0, y0, x1, y1, xmin, ymin, xmax, ymax):
    """
    Пошаговый вариант алгоритма Коэн–Сазерленда для демонстрации.
    Сохраняем картинки после каждого шага.
    Возвращает (ok, cx0, cy0, cx1, cy1).
    """
    outcode0 = compute_outcode(x0, y0, xmin, ymin, xmax, ymax)
    outcode1 = compute_outcode(x1, y1, xmin, ymin, xmax, ymax)

    # Для визуализации создадим счётчик шагов
    step = 0
    def save_fig(label):
        nonlocal step
        step += 1
        plt.savefig(f"cohen_step_{step}_{label}.png", dpi=150)
        plt.close()

    while True:
        # 1) Если оба кода == 0 => внутри
        if outcode0 == 0 and outcode1 == 0:
            # Отрезок целиком внутри
            return (True, x0, y0, x1, y1)

        # 2) Если (outcode0 & outcode1) != 0 => вне
        elif (outcode0 & outcode1) != 0:
            return (False, None, None, None, None)

        else:
            # Обрезаем один из концов
            if outcode0 != 0:
                outcode_out = outcode0
            else:
                outcode_out = outcode1

            # пересечение
            x, y = 0.0, 0.0

            # Рисуем текущий этап (до обрезки)
            plt.figure(figsize=(5,5))
            # Рисуем прямоугольник
            plt.plot([xmin,xmax,xmax,xmin,xmin],[ymin,ymin,ymax,ymax,ymin],'r-')
            # Отрезок
            plt.plot([x0, x1],[y0, y1],'b--', label=f'Step: outcode_out={outcode_out}')
            plt.title("Cohen–Sutherland (step before cutting)")
            plt.legend()
            save_fig("before")

            # Определяем, с какой стороной пересекать
            if outcode_out & TOP:     # выше окна
                x = x0 + (x1 - x0) * (ymax - y0)/(y1 - y0)
                y = ymax
            elif outcode_out & BOTTOM: # ниже окна
                x = x0 + (x1 - x0) * (ymin - y0)/(y1 - y0)
                y = ymin
            elif outcode_out & RIGHT:  # правее окна
                y = y0 + (y1 - y0) * (xmax - x0)/(x1 - x0)
                x = xmax
            elif outcode_out & LEFT:   # левее окна
                y = y0 + (y1 - y0) * (xmin - x0)/(x1 - x0)
                x = xmin

            # Обновляем соответствующий конец
            if outcode_out == outcode0:
                x0, y0 = x, y
                outcode0 = compute_outcode(x0, y0, xmin, ymin, xmax, ymax)
            else:
                x1, y1 = x, y
                outcode1 = compute_outcode(x1, y1, xmin, ymin, xmax, ymax)

            # Рисуем текущий этап (после обрезки)
            plt.figure(figsize=(5,5))
            plt.plot([xmin,xmax,xmax,xmin,xmin],[ymin,ymin,ymax,ymax,ymin],'r-')
            plt.plot([x0, x1],[y0, y1],'g-', linewidth=2, label='After cut')
            plt.title("Cohen–Sutherland (step after cutting)")
            plt.legend()
            save_fig("after")


def demo_cohen_sutherland():
    # Прямоугольное окно отсечения
    xmin, ymin = -4, -3
    xmax, ymax =  4,  3

    # 1) Для наглядности изобразим "карту" out-кодов в некоторых точках
    #    вокруг прямоугольника
    xx = np.linspace(-6, 6, 7)
    yy = np.linspace(-5, 5, 6)

    plt.figure(figsize=(6,6))
    plt.plot([xmin,xmax,xmax,xmin,xmin],[ymin,ymin,ymax,ymax,ymin],'r-', label='Clipping window')
    for x in xx:
        for y in yy:
            outc = compute_outcode(x,y, xmin,ymin, xmax,ymax)
            plt.text(x, y, f"{outc:02X}", ha='center', va='center', 
                     fontsize=8, color='blue')
    plt.title("Побитовые коды (hex) для сетки точек вокруг окна")
    plt.legend()
    plt.savefig("cohen_outcode_map.png", dpi=150)
    plt.close()

    # 2) Генерируем случайный отрезок (или фиксированный)
    x0, y0 = (random.uniform(-6,6), random.uniform(-5,5))
    x1, y1 = (random.uniform(-6,6), random.uniform(-5,5))

    # 3) Запускаем пошаговый алгоритм
    result, cx0, cy0, cx1, cy1 = cohen_sutherland_clip_stepwise(x0, y0, x1, y1, xmin, ymin, xmax, ymax)

    # 4) Финальный результат
    plt.figure(figsize=(6,6))
    # Прямоугольник
    plt.plot([xmin,xmax,xmax,xmin,xmin],[ymin,ymin,ymax,ymax,ymin],'r-')
    # Исходный отрезок
    plt.plot([x0, x1],[y0, y1],'b--', alpha=0.4, label='Исходный')
    if result:
        plt.plot([cx0, cx1],[cy0, cy1],'g-', linewidth=3, label='Отсечённый')
        plt.title("Cohen–Sutherland: финальный результат (внутри)")
    else:
        plt.title("Cohen–Sutherland: отрезок вне")
    plt.legend()
    plt.savefig("cohen_final_result.png", dpi=150)
    plt.close()

    print("Готово! См. PNG-файлы:\n"
          "  cohen_outcode_map.png (с картой кодов)\n"
          "  cohen_step__*_before/after.png (промежуточные шаги усечения)\n"
          "  cohen_final_result.png (итог)")

if __name__ == "__main__":
    demo_cohen_sutherland()
