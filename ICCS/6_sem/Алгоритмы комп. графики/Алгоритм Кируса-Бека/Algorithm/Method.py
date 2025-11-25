import numpy as np
import matplotlib.pyplot as plt

def clip_line_halfplane(p0, p1, normal, c):
    """
    Отсекаем отрезок p0->p1 полуплоскостью, заданной неравенством:
       normal . x + c >= 0.
    Возвращаем (True, q0, q1), если кусок остался,
               (False, None, None) если пусто.
    """
    d = p1 - p0
    num_p0 = np.dot(normal, p0) + c
    num_p1 = np.dot(normal, p1) + c

    inside_p0 = (num_p0 >= 0)
    inside_p1 = (num_p1 >= 0)

    # Если оба конца вне -> отсечение целиком
    if not inside_p0 and not inside_p1:
        return (False, None, None)

    # Если оба внутри -> сохраняем как есть
    if inside_p0 and inside_p1:
        return (True, p0, p1)

    # Иначе один внутри, другой вне -> найдём точку пересечения
    den = np.dot(normal, d)
    if abs(den) < 1e-12:
        # Почти параллелен
        if inside_p0:
            return (True, p0, p1)
        else:
            return (False, None, None)

    t = - num_p0 / den
    inter_pt = p0 + t * d

    if inside_p0 and not inside_p1:
        # Обрезаем конец
        return (True, p0, inter_pt)
    else:
        # inside_p1 and not inside_p0
        return (True, inter_pt, p1)

def clip_segment_with_halfplanes_stepwise(p0, p1, planes):
    """
    Последовательно «отрезаем» p0->p1 набором полуплоскостей (planes).
    На каждом шаге рисуем картинку.
    planes - [(normal, c), ...] для неравенства normal . x + c >= 0
    """
    current_p0, current_p1 = p0, p1

    # Область для отрисовки
    XMIN, XMAX = -7, 7
    YMIN, YMAX = -7, 7

    step = 0
    def save_fig(label):
        nonlocal step
        step += 1
        plt.savefig(f"halfplane_step_{step}_{label}.png", dpi=150)
        plt.close()

    for i, (normal, c) in enumerate(planes, start=1):
        # Рисуем "до" обрезки
        plt.figure(figsize=(6,6))
        xs = np.linspace(XMIN, XMAX, 200)
        for (n, cc) in planes:
            # Просто рисуем все граничные прямые (красным)
            if abs(n[1])<1e-12:
                x_line = -cc/n[0]
                ys = np.linspace(YMIN,YMAX,100)
                plt.plot([x_line]*100, ys, 'r-')
            else:
                y_line = (-cc - n[0]*xs)/n[1]
                plt.plot(xs, y_line, 'r-')
        # Исходный отрезок (серый)
        plt.plot([p0[0], p1[0]],[p0[1], p1[1]],'k--',alpha=0.3,label='Исходный')
        # Текущий отрезок (синий)
        plt.plot([current_p0[0], current_p1[0]], [current_p0[1], current_p1[1]],
                 'b--', label=f'До шага {i}')
        plt.xlim(XMIN, XMAX)
        plt.ylim(YMIN, YMAX)
        plt.legend()
        plt.title(f"Шаг {i} (до обрезки) - полуплоскость: n={normal}, c={c}")
        save_fig("before")

        # Выполняем обрезку
        res, new_p0, new_p1 = clip_line_halfplane(current_p0, current_p1, normal, c)
        if not res:
            # Полностью вне
            plt.figure(figsize=(6,6))
            for (n, cc) in planes:
                if abs(n[1])<1e-12:
                    x_line = -cc/n[0]
                    ys = np.linspace(YMIN,YMAX,100)
                    plt.plot([x_line]*100, ys, 'r-')
                else:
                    y_line = (-cc - n[0]*xs)/n[1]
                    plt.plot(xs, y_line, 'r-')
            plt.plot([p0[0], p1[0]],[p0[1], p1[1]], 'k--',alpha=0.3,label='Исходный')
            plt.plot([current_p0[0], current_p1[0]],[current_p0[1], current_p1[1]], 'b--', label='До')
            plt.title(f"Шаг {i}: отрезок полностью вне")
            plt.xlim(XMIN, XMAX)
            plt.ylim(YMIN, YMAX)
            plt.legend()
            save_fig("after")
            return (False, None, None)

        # Иначе подрезаем
        current_p0, current_p1 = new_p0, new_p1

        # Рисуем "после" обрезки
        plt.figure(figsize=(6,6))
        for (n, cc) in planes:
            if abs(n[1])<1e-12:
                x_line = -cc/n[0]
                ys = np.linspace(YMIN,YMAX,100)
                plt.plot([x_line]*100, ys, 'r-')
            else:
                y_line = (-cc - n[0]*xs)/n[1]
                plt.plot(xs, y_line, 'r-')
        plt.plot([p0[0], p1[0]],[p0[1], p1[1]],'k--',alpha=0.3,label='Исходный')
        # Что осталось (зелёный)
        plt.plot([current_p0[0], current_p1[0]], [current_p0[1], current_p1[1]],
                 'g-', linewidth=3, label=f'После шага {i}')
        plt.xlim(XMIN, XMAX)
        plt.ylim(YMIN, YMAX)
        plt.legend()
        plt.title(f"Шаг {i} (после) - полуплоскость: n={normal}, c={c}")
        save_fig("after")

    # Закончили все полуплоскости
    return (True, current_p0, current_p1)

def demo_halfplane():
    # Набор полуплоскостей, задающих выпуклый «ромб»
    planes = [
        (np.array([1,  1], dtype=float),  -2),  # x + y >= -2
        (np.array([1, -1], dtype=float),  -2),  # x - y >= -2
        (np.array([-1,1], dtype=float),   -2),  # -x + y >= -2
        (np.array([-1,-1],dtype=float),   -2),  # -x - y >= -2
    ]

    # Отрезок:
    p0 = np.array([-5, -2], dtype=float)
    p1 = np.array([ 3,  4], dtype=float)

    res, q0, q1 = clip_segment_with_halfplanes_stepwise(p0, p1, planes)
    if res:
        print(f"Результат: {q0} - {q1}")
    else:
        print("Отрезок полностью снаружи.")

if __name__ == "__main__":
    demo_halfplane()
