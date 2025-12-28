from PIL import Image, ImageDraw, ImageFilter
import random, math, os

# ════════════════════════════════════════════════════════════
#                БАЗОВЫЕ ПАРАМЕТРЫ (как были)
# ════════════════════════════════════════════════════════════
MIN_X, MAX_X   = 380, 1300         # стартовый диапазон X
START_Y, PEAK_Y = 800, 10          # нижняя и верхняя Y
MAX_X_DEV      = 180               # максимальное отклонение в стороны
CURVATURE      = 1.15
SPEED_RANGE    = (0.4, 0.7)
LINGER_FRAMES  = 2

MIN_RADIUS, MAX_RADIUS = 8, 19
SIZE_WIGGLE     = .35

MASK_POLY = [(430,760),(530,10),(1300,10),(1370,845)]   # ромб-маска

BASE_OPACITY     = 55
EXTRA_BUBBLES    = 25
ADD_MIN_X, ADD_MAX_X = 1300, 1600
ADD_SPEED_RANGE  = (.015,.05)

NUM_FRAMES, BUBBLE_COUNT = 60, 180

# ════════════════════════════════════════════════════════════
#           СДВИГ (оставляем, чтобы всё стояло на месте)
# ════════════════════════════════════════════════════════════
SHIFT_X = 450
SHIFT_Y = 450

MIN_X   += SHIFT_X
MAX_X   += SHIFT_X
ADD_MIN_X += SHIFT_X
ADD_MAX_X += SHIFT_X
MASK_POLY = [(x+SHIFT_X, y+0) for (x, y) in MASK_POLY]

START_Y += SHIFT_Y
PEAK_Y  += SHIFT_Y
MASK_POLY = [(x, y+SHIFT_Y) for (x, y) in MASK_POLY]

# ════════════════════════════════════════════════════════════
#         РАСШИРЯЕМ ШИРИНУ и ВЫСОТУ (БЕЗ СДВИГА ЦЕНТРА)
# ════════════════════════════════════════════════════════════
EXPAND_WIDTH   = 250   # ←  +200 px по ширине (100 слева + 100 справа)
EXPAND_HEIGHT  = 300   # ↑  +150 px вверх (пузыри всплывают выше)

# --- шире ---------------------------------------------------
half_w = EXPAND_WIDTH // 2
MIN_X -= half_w
MAX_X += half_w
MAX_X_DEV += half_w               # больший «болтанка» радиус

# корректируем маску
MASK_POLY = [
    (x - half_w if i in (0,1) else x + half_w, y)
    for i, (x, y) in enumerate(MASK_POLY)
]

# диапазон появления «доп.» пузырей справа
ADD_MIN_X -= half_w
ADD_MAX_X += half_w

# --- выше ---------------------------------------------------
PEAK_Y -= EXPAND_HEIGHT           # поднимаем точку вершины

MASK_POLY = [
    (x, y - EXPAND_HEIGHT if i in (1,2) else y)
    for i, (x, y) in enumerate(MASK_POLY)
]

# --- чуть больше пузырей для заполнения увеличенного объёма --
BUBBLE_COUNT  = int(BUBBLE_COUNT * 1.4)   # ≈ 250
EXTRA_BUBBLES = int(EXTRA_BUBBLES * 1.6)  # ≈ 40

# ════════════════════════════════════════════════════════════
#                     ВСПОМОГАТЕЛЬНЫЕ ФУНКЦИИ
# ════════════════════════════════════════════════════════════
def point_in_polygon(pt, poly):
    x, y = pt
    inside = False
    for i in range(len(poly)):
        x1, y1 = poly[i]
        x2, y2 = poly[(i + 1) % len(poly)]
        if ((y1 > y) != (y2 > y)) and (x < (x2 - x1) * (y - y1) / (y2 - y1) + x1):
            inside = not inside
    return inside


def trajectory(t, start_x, speed, linger, kind):
    """Координаты пузыря на кадре t."""
    if kind == 'main':
        t_peak = (START_Y - PEAK_Y) / (speed * 120)
        cycle = t_peak + linger
        τ = t % cycle
        if τ < t_peak:  # подъём
            prog = τ / t_peak
            y = START_Y - τ * speed * 120
            x_off = -CURVATURE * MAX_X_DEV * math.sin(prog * math.pi)
        else:  # зависание
            y = PEAK_Y
            x_off = CURVATURE * MAX_X_DEV
    else:  # хаотичные «доп»
        center_x = start_x - 120
        center_y = random.randint(120 + SHIFT_Y, 680 + SHIFT_Y)
        x_off = 40 * math.sin(t * speed * 22) + 20 * math.sin(t * speed * 7)
        y_off = 20 * math.cos(t * speed * 18)
        return center_x + x_off, center_y + y_off

    x = max(start_x - MAX_X_DEV, min(start_x + MAX_X_DEV, start_x + x_off))
    return x, y


def radial_gradient(radius, base_color, highlight=1.0):
    """Круг с мягкой прозрачной обводкой."""
    size = radius * 2
    layer = Image.new("RGBA", (size, size), (0, 0, 0, 0))
    draw = ImageDraw.Draw(layer)
    for r in range(radius, 0, -1):
        alpha = int(base_color[3] * (r / radius) ** highlight)
        color = (*base_color[:3], alpha)
        draw.ellipse([(radius - r, radius - r), (radius + r, radius + r)], fill=color)
    return layer


def build_bubbles():
    bubbles = []
    for _ in range(BUBBLE_COUNT):
        bubbles.append({
            'x': random.uniform(MIN_X, MAX_X),
            'speed': random.uniform(*SPEED_RANGE),
            'phase': random.uniform(0, 100),
            'rad': random.randint(MIN_RADIUS, MAX_RADIUS),
            'wig': (random.uniform(.25, .55), random.uniform(0, 2 * math.pi)),
            'type': 'main'
        })
    for _ in range(EXTRA_BUBBLES):
        bubbles.append({
            'x': random.uniform(ADD_MIN_X, ADD_MAX_X),
            'speed': random.uniform(*ADD_SPEED_RANGE),
            'phase': random.uniform(0, 100),
            'rad': random.randint(MIN_RADIUS - 1, MAX_RADIUS - 8),
            'wig': (random.uniform(.3, .6), random.uniform(0, 2 * math.pi)),
            'type': 'extra'
        })
    return bubbles


def draw_bubble(canvas, pos, base_rad, t, wiggle, opacity):
    freq, phi = wiggle
    rad = base_rad * (1 + SIZE_WIGGLE * math.sin(freq * t * 0.1 + phi))

    # --- слой "стекло" -------------------------------------------------
    glass = radial_gradient(int(rad), (200, 220, 255, int(opacity*0.35)), highlight=1.0)
    glass = glass.filter(ImageFilter.GaussianBlur(1.5))  # мягкий край

    # --- внутренний слой ("толща воды") -------------------------------
    core_rad = int(rad * 0.8)
    core = radial_gradient(core_rad, (140, 200, 255, int(opacity*0.6)), highlight=1.8)

    # вклеиваем core в центр glass
    glass.paste(core, (glass.width//2-core_rad, glass.height//2-core_rad), core)

    # --- два блика -----------------------------------------------------
    draw = ImageDraw.Draw(glass)
    # главный
    r1 = rad * 0.45
    draw.ellipse([(rad - r1, rad - r1),
                  (rad + r1*0.2, rad + r1*0.2)],
                 fill=(255,255,255,min(255,opacity+80)))
    # вторичный слабее
    r2 = rad * 0.2
    draw.ellipse([(rad*0.3, rad*0.5),
                  (rad*0.3+r2, rad*0.5+r2)],
                 fill=(255,255,255,int(opacity*0.4)))

    # --- пульсация прозрачности всего пузыря --------------------------
    puls = 0.8 + 0.2 * math.sin(t*0.6 + phi)
    glass.putalpha(glass.split()[-1].point(lambda a: int(a*puls)))

    canvas.paste(glass, (int(pos[0]-rad), int(pos[1]-rad)), glass)

# ════════════════════════════════════════════════════════════
#                       ГЛАВНАЯ ФУНКЦИЯ
# ════════════════════════════════════════════════════════════
def create_bubble_animation(src_path, out_path, frames=NUM_FRAMES):
    src = Image.open(src_path).convert('RGBA')
    bubbles = build_bubbles()
    result = []

    for f in range(frames):
        frame = Image.new('RGBA', src.size, (0, 0, 0, 0))
        frame.paste(src, (0, 0), src)

        for b in bubbles:
            x, y = trajectory(f + b['phase'], b['x'], b['speed'], LINGER_FRAMES, b['type'])

            if y >= PEAK_Y - 40 and point_in_polygon((x, y), MASK_POLY):
                fade = 1 - ((y - PEAK_Y) / (START_Y - PEAK_Y))
                opac = int(255 * (BASE_OPACITY / 100) * pow(fade, 1.3))
                if opac <= 8:
                    continue
                draw_bubble(frame, (x, y), b['rad'], f, b['wig'], opac)

        result.append(frame)

    os.makedirs(os.path.dirname(out_path), exist_ok=True)
    result[0].save(out_path, save_all=True, append_images=result[1:],
                   duration=80, loop=0, lossless=True, background=(0, 0, 0, 0))
    return out_path

# ════════════════════════════════════════════════════════════
#                        ЗАПУСК
# ════════════════════════════════════════════════════════════
if __name__ == '__main__':
    create_bubble_animation(
        'Animation/image.png',
        'Animation/boiling_kettle.webp',
        NUM_FRAMES
    )
    print("✅ Готово: Animation/boiling_kettle.webp")
