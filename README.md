# Express — WIP Language & Interpreter

**Express** is an experimental, self-managed programming language designed for performance within an interpreted environment.
It features a Pascal-inspired syntax and aims to blend the clarity of Object Pascal with the low-level control of C and the readability of Python.

---

## 🚧 Project Status

Express is actively in development and its features and performance characteristics are subject to change.

**Microbenchmark Performance:** In numerical microbenchmarks (like those from SciMark, Cluster, etc.), Express significantly outperforms:
*   Lape: By a factor of 3-4x.
*   JVM (Interpreted Mode): By a factor of 2x.
*   Python: By an order of magnitude.

However, using global vars and references incurs a penalty due to design choices.
The same goes for type mixing, and is not recommended where avoidable.

---

## ✨ Features

-   **Static typing** (e.g., `Int64`, `TIntArray`)
-   **Functions** with value and `ref` arguments (`ref` = like `var` in Pascal)
-   **Type methods** (`function Int64.methodName()`)
-   **Operators:** Full suite of arithmetic (`+`, `-`, `*`, `/`, `%`, `**`), bitwise (`&`, `|`, `xor`, `shl`, `shr`, `sar`), and compound assignment (`+=`, `&=`, etc.) operators.
-   **Control flow:** `if / elif / else`, `while`, `for`, and `repeat..until` loops.
-   **Loop flow control:** `break` and `continue` statements.
-   **Ternary expressions** for concise conditional values (`var x := if (a > b) a else b`).
-   **Newline-based syntax** (no semicolons needed), with `\` for line continuation.
-   **Records**: C-style structs with full support for direct, deep assignment, including for records with managed fields.
-   **Try-except**: Basic error trapping is supported.
-   **Self-managed memory model** — No GC; arrays are reference-counted.
-   **Namespaces** - Basic support `import`
-   **Nested functions** using frame pointers for parent references.

---

## 🔴 Missing or Limited Features

-   ❌ No closures or anonymous functions
-   ⚠️ strings are not copy on write (yet?), with some known bugs
-   ⚠️ `print` is a limited statement, you can use `.ToStr()`.
-   ⚠️ `try-except` works for escaping errors, but not for detailed exception handling (e.g., matching specific exception types).
-   ⚠️ Classes are experimental without method inheritance call option.

---

## 🔍 Syntax Examples

### Function and Method

```pascal
function show(x: Int64);
  print x
end;

function Int64.inc();
  self := self + 1
end;

var x: Int64 = 10
x.inc()
show(x)
```

---

### Ternary Expression and Compound Operators
```pascal
function max(a, b: Int64): Int64;
  return if (a > b) a else b
end;

var counter := 0
counter += max(5, 10) // counter is now 10
print counter
```

---

### Loop control

```pascal
for (var i := 0; i < 10; i := i + 1) do
  if (i = 3) then
    continue // Skips printing 3
  end
  if (i = 7) then
    break // Exits the loop
  end
  print i
end
// Output: 0, 1, 2, 4, 5, 6

// repeat..until loop
var countdown := 3
repeat
  print countdown
  countdown -= 1
until (countdown = 0)
// Output: 3, 2, 1
```

---

### Line continuation
```pascal
// A long expression can be broken across lines with a backslash
var long_result := 100 + 200 + 300 + \
                   400 + 500 + 600
print long_result
```

---

### Bitwise Operations

```Pascal
print 42 & 15
print 5 shl 2
print 100 xor 7
```

---

### Array Handling (Experimental)

```Pascal
var arr: array of Int32
arr.SetLen(10)
arr[0] := 42
print arr[0]
```

---

### Try-Except (Minimal)

```pascal
try
  var a := 1 / 0
except
  print 'error caught'
end
```
> No way to inspect exception type or message yet.


---

### Import

```pascal
import 'path/math.xpr'
print math.max(100,40)

import 'path/math.xpr' as Mathlib
print Mathlib.max(100,40)
```

Allows importing into current namespace:
```pascal
import 'path/math.xpr' as *
print max(100,40)
```

---

### Classes and Polymorphism

```pascal
type TAnimal = class 
  var name: String

  function Create(aName: String)
    self.name := aName
  end
  
  function Speak(): String
    return self.name + ' makes a sound.'
  end
end

type TCat = class(TAnimal)
  function Speak(): String // No 'override' keyword is needed.
    return self.name + ' says Meow!'
  end
end

var AnimalName := 'Misty';
var myCat := new TCat(AnimalName)

var myPet: TAnimal = myCat
print myPet.Speak() //; .. says Meow!

// check if it is a cat
if (myPet is TCat) then
  print 'The pet is indeed a cat!'
end

// Clean up
myPet.Free()
```


---

## 🛠 Planned Features

- Strings are currently limited to Ansistring, and experimental.
- Classes with inheritance
- Tuple types with destructuring assignment `var (a, b) := MyFunc()`
- Exception type handling
- Operator overloading
- Properties for class and maybe records
- Default function parameters with assign by name
- Function override with inheritance (for classes)
- raise Exception(..)

---

## ⚙ Memory Management

Express is a **self-managed language**:

- No garbage collection (GC)
- Manual memory control is expected
- Arrays are managed internally by refcounting




