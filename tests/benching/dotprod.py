import time
import random

a = [round(random.random()*20) for x in range(10000000)]
b = [round(random.random()*20) for x in range(10000000)]

t = time.time()
d = 0
for i in range(len(a)): 
  d = d + a[i] * b[i];

print(time.time() - t)
print(d);