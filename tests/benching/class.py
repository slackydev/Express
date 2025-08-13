import time

# Define the equivalent classes
class TVector:
    def __init__(self):
        self.x = 0.0
        self.y = 0.0
    
    def Write(self):
        print('Its impossible')
        
    def Free(self):
        # In Python, memory is managed by the GC, so this is a no-op.
        # We include it to match the operation count.
        pass

class TVector3(TVector):
    def __init__(self, x, y, z):
        # Python doesn't require explicit super() call if the parent __init__ does nothing.
        # For a direct comparison, we'll assign all fields.
        self.x = x
        self.y = y
        self.z = z
        
    def Write(self):
        print('Hello world')
        
    def WriteItems(self):
        print(f"{self.x}, {self.y}, {self.z}")

# --- Benchmark Starts Here ---

# Get a high-resolution timer
start_time = time.perf_counter()

for i in range(1000000):
    # 'new' is implicit in Python
    vec3 = TVector3(1.0, 2.0, 3.0)
    
    # Polymorphic assignment (this is standard in Python)
    vec = vec3
    vec.x = 100.0

    # The 'is' operator in Python is for identity, not type.
    # The correct equivalent for type checking is `isinstance()`.
    if isinstance(vec, TVector3):
        # The 'as' cast is implicit in Python after the isinstance check.
        # The interpreter doesn't need to be told vec is a TVector3 again.
        vec.z += 1.0
    else:
        print('Its impossible!')
        
    # Manual 'Free' call to match the operation.
    del vec3

end_time = time.perf_counter()

# --- Print Results ---
duration_ms = (end_time - start_time) * 1000
print(f"Executed in {duration_ms:.3f} ms")