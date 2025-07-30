import random
import time
from typing import List, Tuple

Point = Tuple[int, int]
TPointArray = List[Point]

def generate_points(n: int) -> TPointArray:
    return [(random.randint(0, 1000), random.randint(0, 1000)) for _ in range(n)]

def split_points(points_in: List[Point], dist_x: float, dist_y: float) -> List[List[Point]]:
    if len(points_in) == 0:
        return []
    elif len(points_in) == 1:
        return [points_in.copy()]

    points = points_in.copy()
    clusters: List[List[Point]] = []

    xsq = dist_x * dist_x
    ysq = dist_y * dist_y
    xxyy = xsq * ysq

    last_index = len(points) - 1
    proc_count = 0

    while (last_index - proc_count) >= 0:
        current: List[Point] = [points[0]]
        points[0] = points[last_index - proc_count]
        proc_count += 1

        clust_size = 1
        i = 0

        while i < clust_size:
            j = 0
            while j <= (last_index - proc_count):
                p1 = current[i]
                p2 = points[j]

                dx = p1[0] - p2[0]
                dy = p1[1] - p2[1]
                if (dx * dx * ysq + dy * dy * xsq) <= xxyy:
                    current.append(p2)
                    points[j] = points[last_index - proc_count]
                    proc_count += 1
                    clust_size += 1
                    j -= 1
                j += 1
            i += 1

        clusters.append(current)

    return clusters

def benchmark():
    p = generate_points(10000)

    t0 = time.time()
    clusters = split_points(p, 7, 7)
    t1 = time.time()

    print("Cluster count:", len(clusters))
    print("Time (ms):", int((t1 - t0) * 1000))

benchmark()