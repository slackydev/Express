from __future__ import division
import time, random
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
    for i in range(n): 
      arr.append(int(round(random.random()*1000000))) #note the int(..)
    
    before = time.time()
    sort_it(arr)
    after = time.time()
    print('Integer sort: %.3f ms' % ((after - before) * 1000))


def listsort_object(n):
    arr = []
    for i in range(n): 
      arr.append(round(random.random()*1000000)) #note the int(..)
    
    
    before = time.time()
    sort_it(arr)
    after = time.time()
    print('No cast sort: %.3f ms' % ((after - before) * 1000))


listsort_int(1000000)
listsort_object(1000000)
    
    
    
    
    
