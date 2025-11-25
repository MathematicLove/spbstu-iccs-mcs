import random
import time
import numpy as np
import matplotlib.pyplot as plt

try:
    from shapely.geometry import Polygon, LineString
    shapely_available = True
except ImportError:
    shapely_available = False

def convex_hull(points):
    points = sorted(points, key=lambda p: (p[0], p[1]))

    def cross(o, a, b):
        return (a[0] - o[0])*(b[1] - o[1]) - (a[1] - o[1])*(b[0] - o[0])

    lower = []
    for p in points:
        while len(lower) >= 2 and cross(lower[-2], lower[-1], p) <= 0:
            lower.pop()
        lower.append(p)

    upper = []
    for p in reversed(points):
        while len(upper) >= 2 and cross(upper[-2], upper[-1], p) <= 0:
            upper.pop()
        upper.append(p)

    lower.pop()
    upper.pop()
    hull = lower + upper
    return np.array(hull)

def generate_random_convex_polygon(num_points=7, radius=5):
    points = []
    for _ in range(num_points):
        r = radius * random.random()**0.5
        theta = 2 * np.pi * random.random()
        x = r * np.cos(theta)
        y = r * np.sin(theta)
        points.append((x, y))
    hull = convex_hull(points)
    return hull

def is_segment_outside_bounding(p0, p1, polygon):
    poly_x = polygon[:, 0]
    poly_y = polygon[:, 1]
    min_x, max_x = min(poly_x), max(poly_x)
    min_y, max_y = min(poly_y), max(poly_y)

    seg_x = [p0[0], p1[0]]
    seg_y = [p0[1], p1[1]]
    seg_min_x, seg_max_x = min(seg_x), max(seg_x)
    seg_min_y, seg_max_y = min(seg_y), max(seg_y)

    if seg_max_x < min_x or seg_min_x > max_x:
        return True
    if seg_max_y < min_y or seg_min_y > max_y:
        return True
    return False

def cyrus_beck_clip(p0, p1, polygon):
    d = p1 - p0
    t_min, t_max = 0.0, 1.0
    n = len(polygon)

    for i in range(n):
        cur = polygon[i]
        nxt = polygon[(i + 1) % n]
        edge_vec = nxt - cur
        normal = np.array([-edge_vec[1], edge_vec[0]], dtype=float)  
        numerator   = np.dot(normal, (p0 - cur))
        denominator = np.dot(normal, d)

        if abs(denominator) < 1e-12:
            if numerator < 0:
                return (False, None, None)  
            else:
                continue  

        t = - numerator / denominator
        if denominator > 0:
            if t > t_max:
                return (False, None, None)
            if t > t_min:
                t_min = t
        else:
            if t < t_min:
                return (False, None, None)
            if t < t_max:
                t_max = t

        if t_min > t_max:
            return (False, None, None)

    if t_max < 0 or t_min > 1:
        return (False, None, None)

    q0 = p0 + t_min * d
    q1 = p0 + t_max * d
    return (True, q0, q1)

def clip_segment_with_cyrus_beck(p0, p1, polygon):
    if is_segment_outside_bounding(p0, p1, polygon):
        return (False, None, None)
    return cyrus_beck_clip(p0, p1, polygon)

def shapely_clip(p0, p1, polygon):
    poly  = Polygon(polygon)
    line  = LineString([p0, p1])
    inter = poly.intersection(line)
    if inter.is_empty:
        return (False, None, None)
    if inter.geom_type == 'LineString':
        coords = list(inter.coords)
        q0 = np.array(coords[0])
        q1 = np.array(coords[-1])
        return (True, q0, q1)
    if inter.geom_type == 'Point':
        q = np.array(inter.coords[0])
        return (True, q, q)
    if inter.geom_type == 'MultiPoint':
        coords = sorted([np.array(pt.coords[0]) for pt in inter.geoms], key=lambda x: (x[0], x[1]))
        return (True, coords[0], coords[-1])
    return (True, None, None)

def compare_performance(num_tests=1000, polygon_size=30, radius=10):
    polygon = generate_random_convex_polygon(num_points=polygon_size, radius=radius)

    cyrus_times = []
    shapely_times = []
    
    for _ in range(num_tests):
        p0 = np.array([random.uniform(-2*radius, 2*radius),
                       random.uniform(-2*radius, 2*radius)])
        p1 = np.array([random.uniform(-2*radius, 2*radius),
                       random.uniform(-2*radius, 2*radius)])
        
        start = time.perf_counter()
        res1 = clip_segment_with_cyrus_beck(p0, p1, polygon)
        end = time.perf_counter()
        cyrus_times.append(end - start)

        if shapely_available:
            start = time.perf_counter()
            res2 = shapely_clip(p0, p1, polygon)
            end = time.perf_counter()
            shapely_times.append(end - start)
        else:
            shapely_times.append(float('nan'))

    mean_cyrus   = np.mean(cyrus_times)
    mean_shapely = np.mean([t for t in shapely_times if not np.isnan(t)])
    return mean_cyrus, mean_shapely

def demo_comparison():
    scenarios = [
        (500,  10,  10),
        (500,  30,  10),
        (500,  50,  10),
        (500, 100, 15),
    ]
    
    cyrus_results = []
    shapely_results = []
    labels = []
    
    for i, (num_tests, polygon_size, radius) in enumerate(scenarios):
        print(f"Сценарий {i+1}: Тестов={num_tests}, Вершин={polygon_size}, Радиус={radius}")
        mean_cyrus, mean_shapely = compare_performance(num_tests, polygon_size, radius)
        cyrus_results.append(mean_cyrus)
        shapely_results.append(mean_shapely)
        labels.append(f"n={polygon_size}")
        if shapely_available:
            print(f"  Cyrus-Beck: {mean_cyrus*1e6:.2f} мкс на тест, Shapely: {mean_shapely*1e6:.2f} мкс на тест")
        else:
            print(f"  Cyrus-Beck: {mean_cyrus*1e6:.2f} мкс на тест, Shapely: N/A (не установлена)")
    x = np.arange(len(scenarios))
    width = 0.35
    
    plt.figure()
    plt.bar(x - width/2, cyrus_results, width, label='Cyrus-Beck')
    if shapely_available:
        plt.bar(x + width/2, shapely_results, width, label='Shapely')
    
    plt.xticks(x, labels)
    plt.ylabel("Среднее время (секунды)")
    plt.title("Сравнение производительности (Cyrus-Beck vs Shapely)")
    plt.legend()
    plt.tight_layout()
    plt.savefig("comparison_cyrus_vs_shapely.png", dpi=200)
    plt.close()
    print("График сохранён в файл 'comparison_cyrus_vs_shapely.png'.")

if __name__ == "__main__":
    demo_comparison()
