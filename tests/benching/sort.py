from __future__ import division
import time
#CPython: 7900ms (both tests)
#PyPy   : 140ms, 1200ms

def sort_it(arr):
    n = len(arr)
    gap = 0;
    while (gap < (n-1) / 3): gap = gap * 3 + 1;

    while(gap >= 1):
      for i in range(n):
        j = i
        while (j >= gap) and (arr[j] < arr[j-gap]):
          tmp = arr[j]
          arr[j] = arr[j-gap]
          j -= gap
          arr[j] = tmp
      gap //= 3;


def listsort_int(n):
    arr = []
    seed = 6239351;
    for i in range(n): 
      seed = (1103515245 * seed + 12345) % 2147483648;
      arr.append(int(seed)) #note the int(..)
    
    before = time.time()
    sort_it(arr)
    after = time.time()
    print('Integer sort: %.3f ms' % ((after - before) * 1000))


def listsort_object(n):
    arr = []
    seed = 6239351;
    for i in range(n):
      seed = (1103515245 * seed + 12345) % 2147483648;
      arr.append(seed) 
    
    before = time.time()
    sort_it(arr)
    after = time.time()
    print('No cast sort: %.3f ms' % ((after - before) * 1000))


listsort_int(300000)
listsort_object(300000)
    
    
    
    
    
