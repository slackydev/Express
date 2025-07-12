# Express (Typed version) — WIP Language & Interpreter

**Express (Typed)** is an experimental, self-managed programming language with a fast interpreter and a Pascal-inspired syntax. 
It mixes elements of **Object Pascal**, **C**, and **Python**, with a focus on clarity and performance.

---

## 🚧 Project Status

Express is **in development** and subject to change. The interpreter is approximately **6–12× slower** than native code, 
making it fast by interpreted standards.

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
- ❌ No general-purpose arrays (only `TIntArray` with hardcoded support)
- ❌ No closures or anonymous functions
- ❌ No nested functions
- ❌ No imports/modules
- ⚠️ `print` is a statement, not a method
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
if x > 100 then
  print "large"
elif x = 0 then
  print "zero"
else
  print "small"
end

while x > 0 do
  x := x - 1
end

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
var arr: TIntArray
SetLength(arr, 10)
arr[0] := 42
print arr[0]
```
> Only works with Int64 arrays for now (TIntArray). SetLength and Length are not generic.


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
- Proper array support and type inference
- Modules/imports
- Exception type handling

---

## ⚙ Memory Management

Express is a **self-managed language**:

- No garbage collection (GC)
- Manual memory control is expected
- Arrays are managed internally - ref-counted




