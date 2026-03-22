import random
import time
import sys

# Increase recursion depth for deep recursive partitioning
sys.setrecursionlimit(200000)

def quicksort_functional(arr):
    if len(arr) <= 1:
        return arr
    
    pivot = arr[0]
    rest = arr[1:]
    
    # Python List Comprehensions (Matches your 'where' logic)
    left = [x for x in rest if x <= pivot]
    right = [x for x in rest if x > pivot]
    
    # In Python, + is the equivalent of your .Concat()
    return quicksort_functional(left) + [pivot] + quicksort_functional(right)

def quicksort_inplace(arr, low, high):
    if low < high:
        pivot = arr[(low + high) // 2]
        i, j = low, high
        while i <= j:
            while arr[i] < pivot: i += 1
            while arr[j] > pivot: j -= 1
            if i <= j:
                arr[i], arr[j] = arr[j], arr[i]
                i += 1
                j -= 1
        quicksort_inplace(arr, low, j)
        quicksort_inplace(arr, i, high)

def benchmark():
    n = 100000
    data = [random.randint(0, n) for _ in range(n)]
    
    # Test Functional
    data_func = data.copy()
    t0 = time.perf_counter()
    res_func = quicksort_functional(data_func)
    t1 = time.perf_counter()
    print(f"Functional (Comprehension): {(t1 - t0) * 1000:.2f} ms")

    # Test In-place
    data_inplace = data.copy()
    t0 = time.perf_counter()
    quicksort_inplace(data_inplace, 0, len(data_inplace) - 1)
    t1 = time.perf_counter()
    print(f"In-place (Swapping): {(t1 - t0) * 1000:.2f} ms")


if __name__ == "__main__":
    benchmark()
