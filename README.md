# Express — WIP Language & Interpreter

**Express** is an experimental, self-managed programming language designed for performance within an interpreted environment.
It features a Pascal-inspired syntax, iwth features taken from a number of newer languages. 

The aim is a programming language that will be a direct conduit from a developer's mind to the machine, 
not a maze of mandatory abstractions, or symbolic verbose boilerplate.


---

## 🚧 Project Status

Express is actively in development and its features and performance characteristics are subject to change.

**Microbenchmark Performance:** The interpreter's Just-In-Time (JIT) compiler allows Express to achieve improved performance on algorithmic code. 

In numerical microbenchmarks, Express significantly outperforms:
*   Lape: By a factor of 3-4x.
*   JVM (Interpreted Mode): By a factor of 2x.
*   Python: By an order of magnitude.

Note: Using global references, and reference args incurs a small penalty due to design choices.
The same goes for type mixing, which even hits harder, and is not recommended where avoidable.

In limited tests the performance peaks at about (JS) v8 Node.js/chrome speed, but usually slower. 

---

## ✨ Features at a Glance

- **Operators:** Full suite of arithmetic (+, -, *, /, %, **), bitwise (&, |, xor, shl, shr, sar), and compound assignment
- **Statically-Typed:** Clear and safe code with types like `Int32`, `String`, `Boolean`, and user-defined classes.
- **Object-Oriented:** `class`-based OOP with single inheritance, virtual methods, `const` (readonly) fields, and `inherited` calls.
- **Modern Control Flow:** `if/elif/else`, `for`, `while`, `repeat..until`, `break`, `continue`.
- **Class-Based Exceptions:** Safe error handling with `try...except on E: TExceptionType do`.
- **Extension Methods:** Add new methods to *any* type, including built-in arrays and records.
- **Automatic Memory Management:** Reference counting for strings, arrays, and class instances. No manual `free` is needed in most cases.
- **Module System:** Organize code with `import 'path' as Alias`.
- **Pointers:** Supports native pointers, with `addr(x)` you can get the address of a variable.
- **Destructuring assignment** Records can be assigned directly to local variables `(x,y) := myPoint`
- **Anonymous functions** Separate function type that captures references to local variables.


---

## 🔍 Code Examples

These short examples showcase some of its current key features.

### 1. Classes, Inheritance, and Polymorphism

Express features a simple and powerful object model. No `override` keyword is needed.

```pascal
type TAnimal = class
  var name: String
  func Create(aName: String)
    self.name := aName
  end
  func Speak()
    print self.name + ' makes a sound.'
  end
end

type TCat = class(TAnimal)
  func Speak() // This automatically overrides the parent's method
    print self.name + ' says "Meow!"'
  end
end

// Polymorphism: a TAnimal variable can hold a TCat object.
var myPet: TAnimal := new TCat('Misty')

// The correct, overridden method is called at runtime.
myPet.Speak() // Output: Misty says "Meow!"
```

### 2. Recursive Functions

A classic recursive Fibonacci implementation.

```Pascal
func Fib(n: Int64): Int64
  if (n <= 1) then
    return n
  end
  return Fib(n - 1) + Fib(n - 2)
end

var n := Fib(10)
print 'Fib(10) is ' + n.ToStr() // Output: Fib(10) is 55
```

### 3. Error Handling with Typed Exceptions

Catch specific errors using class-based exception handling.

```pascal
type EMyError = class(Exception) end

try
  print 'About to raise an error...'
  raise EMyError('Something went wrong!')
except on E: EMyError do
  print 'Caught it: ' + E.Message
except on E: Exception do  
  // anything goes with capture
except
  // anything goes
end

print 'Program continued safely.'
```

### 4. Extending Built-in Types

Add new functionality to any existing type, like TIntArray.

```pascal
type TIntArray = array of Int64;

// Add a 'Sum' method to all TIntArray variables.
func TIntArray.Sum(): Int64
  var total: Int64 = 0
  for item in self do
    total += item
  end
  return total
end

var numbers: TIntArray
numbers.SetLen(3)
numbers := 10
numbers := 20
numbers := 70

print 'Sum is ' + numbers.Sum().ToStr() // Output: Sum is 100
```

### 5. Loops and Ternary Expressions

Familiar control flow with a clean, semicolon-free syntax.

```pascal
var total := 0
for (var i := 1; i <= 10; i += 1) do
  if (i % 2 = 0) then
    total += i // Add even numbers
  end
end

// Ternary expressions are great for simple assignments.
var message := if (total > 20) 'Big number' else 'Small number'
print message + ': ' + total.ToStr() // Output: Big number: 30


var i := 0
while (i < 10) do
  i += 1
  if (i % 2 <> 0) then
    continue // Skips the print for odd numbers
  end
  if (i > 8) then
    break // Exits the loop early
  end
  print 'Processing even number: ' + i.ToStr()
end
// Output: 2, 4, 6, 8

var countdown := 3
repeat // This loop body always executes at least once.
  print countdown.ToStr() + '...'
  countdown -= 1
until (countdown = 0)
// Output: 3..., 2..., 1...
```


### 6. Low-Level Control with Typed Pointers

Express is not just a high-level language; it provides the power of a systems language like C for when you need performance and direct memory control. It features:

- **Typed Pointers:** Create pointers to any type, like `^Int32` or `^MyRecord`.
- **Address-Of Operator:** Use the `addr()` intrinsic to get the memory address of any variable.
- **C-Style Indexing:** Use the familiar `ptr[i]` syntax to treat any pointer as an array.
- **Pascal-Style Dereferencing:** Use the `ptr^` syntax for direct dereferencing.
- **Pointer Arithmetic:** Add offsets to pointers to manually traverse memory layouts.


### 7. Modern & Ergonomic Record Handling

Express modernizes Pascals record type, adopting features from languages like Go and Swift to make them more lightweight and powerful. 
Records are value types (copied on assignment) and are perfect for grouping data without the overhead of classes.

- Simpler record declaration: Define record types in a short simple manner on the fly
- Initializer Lists: Construct and assign to records with a clean, literal syntax.
- Destructuring Assignment: Unpack record fields into local variables in a single, readable line.

```pascal
// A function can return an anonymous record type, similar to Go or TypeScript.
// And use list init for the return value.
func GetPoint(): (x,y:int)
  Result := [100,200]
end

// Use destructuring assignment to unpack records into new variables.
var (px, py) := GetPoint()

print 'The point is (' + px.ToStr() + ', ' + py.ToStr() + ')'
// Output: The point is (100, 200)

// You can also assign to **any compatible** variables.
var fx, fy: float
(fx, fy) := GetPoint()
```


## 🛠 Planned Features

Express is evolving. Here are some of the key features planned for the near future:
Enums and Sets: For more expressive and safe code.

- Operator Overloading: Allowing user-defined types to work with standard operators.
- Properties: Class and record fields with custom getter/setter logic.
- Default function parameters with assign by name
- Strings are currently limited to Ansistring.
