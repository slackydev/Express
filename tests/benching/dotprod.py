import time

def main():
    n = 10_000_000
    a = [0] * n
    b = [0] * n
    dot = 0

    before = time.time()

    for i in range(n):
        a[i] = i % 1000
        b[i] = (i * 7) % 1000

    beforedot = time.time()

    for i in range(n):
        dot += a[i] * b[i]

    after = time.time()
    afterdot = time.time()
    
    print(f"{(after - before) * 1000:.2f} ms")
    print("dot: "+f"{(afterdot - beforedot) * 1000:.2f} ms")
    print(dot)




main()