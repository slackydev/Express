# Express — WIP Language & Interpreter

**Express** is an experimental, self-managed programming language designed for performance within an interpreted environment. 
It features a Pascal-inspired syntax and aims to blend the clarity of Object Pascal with the low-level control of C and the readability of Python.

---

## 🚧 Project Status

Express is actively in development and its features and performance characteristics are subject to change.

Microbenchmark Performance: In numerical microbenchmarks (like those from SciMark,Cluster, etc), Express significantly outperforms:
* Lape: By a factor of 3-4x.
* JVM (Interpreted Mode): By a factor of 2x.
* Python by an order of magnitude.

However using global vars, and references incures a penalty due to deisgn choices.
The same goes for type mixing, and is not recommended where avoidable.


---

## ✨ Features

- **Static typing** (e.g. `Int64`, `TIntArray`)
- **Functions** with value and `ref` arguments (`ref` = like `var` in Pascal)
- **Type methods** (`function Int64.methodName()`)
- **Operators:** `+ - * / % **`, `& | xor shl shr sar`
- **Control flow:** `if / elif / else`, `while`, `repeat`, `for`
- **Newline-based syntax** (no semicolons needed)
- **Try-except**: exists but limited (no exception type matching)
- **Self-managed memory model** — no GC; not ref-counted yet
- **Records**: Like strunct in C: `type TPoint = record x,y: Int64; end;`

---

## 🔴 Missing or Limited Features

- ❌ No strings (planned)
- ❌ No classes (yet)
- ❌ No closures or anonymous functions
- ❌ No nested functions
- ❌ No imports/modules
- ❌ Records lack proper handling (no direct assignment)
- ⚠️ `print` is a limited statement for digits.
- ⚠️ `try-except` works for escaping errors, but not for detailed exception handling

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

### If / While / For

```pascal
if(x > 100)then
  print 100000000
elif(x = 0)then
  print 0
else
  print 50
end

while(x > 0)do
  x := x - 1
end

while(x < 10) x := x + 1

for(var i := 0; i < 5; i := i + 1)
  print i
```

---

### Bitwise Operations

```Pascal
print 42 & 15
print 5 shl 2
print 100 xor 7
```

---

### Basic Array Handling (Experimental)

```Pascal
var arr: array of Int32
arr.SetLen(10)
arr[0] := 42
print arr[0]
```

---

### Try-Except (Minimal)

```
try
  var a := 1 / 0
except
  print "error caught"
end
```
> No way to inspect exception type or message yet.


---

## 🛠 Planned Features

- Strings and string methods
- Inline `if` expressions (`var x := if(a > b) a else b`)
- Classes / user-defined types
- Modules/imports (namespaces)
- Exception type handling

---

## ⚙ Memory Management

Express is a **self-managed language**:

- No garbage collection (GC)
- Manual memory control is expected
- Arrays are (experimentally) managed internally - refcounted




