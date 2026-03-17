# Express

Express is an experimental scripting language designed as a companion for Free Pascal. 
It takes a lot of syntactic inspiration from Pascal but drops a lot of the verbosity. 
It's indentation-based, statically typed, and aims to be fast enough for real workloads.

Still a work in progress. Things are moving quickly.

---

## Syntax

Express uses indentation to define blocks, similar to Python. 
No `begin`/`end` required, no semicolons. If you know Pascal the structure will feel familiar, just cleaner.

```pascal
func Greet(name: String)
  var msg := 'Hello, ' + name
  print msg
```

---

## Performance

The interpreter is reasonably fast on its own, and there are two JIT tiers available via annotations. 
The first tier copies native machine code to avoid dispatch overhead. 
The second is an x86-64 JIT that can produce code competitive with unoptimized FPC output.

In practice, tight numeric loops with JIT enabled run several times faster than Lape and well ahead of Python. 
No JIT at all is still roughly on par with JVM interpreted mode.

You can control JIT behavior per-function:

```pascal
@jit('max')
func HeavyWork(n: Int)
  // ...
```

---

## Features

- Static typing with integers, floats, strings, booleans, records and classes
- Single-inheritance classes with virtual dispatch and `inherited` calls
- `if/elif/else`, `for`, `while`, `repeat..until`, `break`, `continue`
- Typed exceptions via `try/except on E: SomeType do`
- Extension methods - attach new methods to any type including built-in arrays
- Reference counting for strings, arrays and class instances
- Module imports with `import 'path' as Alias`
- Pointers, `addr()`, pointer indexing and dereferencing
- Destructuring assignment from records: `(x, y) := myPoint`
- Anonymous functions and closures
- Simple embedding API for FPC host applications

---

## Examples

### Classes and inheritance

Methods override automatically when the signature matches. No `override` keyword.

```pascal
type TAnimal = class
  var name: String

  func Create(aName: String)
    self.name := aName

  func Speak()
    print self.name + ' makes a sound.'

type TCat = class(TAnimal)
  func Speak()
    print self.name + ' says Meow!'

var pet: TAnimal := new TCat('Misty')
pet.Speak() // Misty says Meow!
```

### Exception handling

```pascal
type EMyError = class(Exception)

try
  raise EMyError('Something went wrong!')
except on E: EMyError do
  print 'Caught: ' + E.Message
except
  print 'Something else happened'
```

### Extension methods

You can add methods to any type, including built-in arrays.

```pascal
type TIntArray = array of Int64

func TIntArray.Sum(): Int64
  @jit('max')
  for ref item in self do
    Result += item

var nums: TIntArray
nums.SetLen(3)
nums[0] := 10
nums[1] := 20
nums[2] := 70

print nums.Sum().ToStr() // 100
```

### Records and destructuring

```pascal
func GetPoint(): (x, y: Int)
  Result := [100, 200]

var (px, py) := GetPoint()
print px.ToStr() + ', ' + py.ToStr() // 100, 200
```

### Pointers

```pascal
var x: Int32 := 42
var p: ^Int32 := addr(x)

p^ := 100
print x.ToStr() // 100
```

---

## Embedding in FPC

`TExpress` wraps everything up. You can pass variables in from FPC, read them back after the script runs, and register native FPC functions the script can call.

```pascal
uses xpr.Express;

var
  Script: TExpress;
  MyVar: Int32 = 10;
begin
  Script := TExpress.Create;
  try
    Script.Bind.AddVar('SharedVar', @MyVar, Script.Context.GetType(xtInt32));
    Script.RunCode('SharedVar := SharedVar + 5');
    WriteLn(MyVar); // 15
  finally
    Script.Free;
  end;
end.
```

Global variables written by the script can be read back with `Script.GetVar('name')`. 
Keep in mind arrays and strings are freed when the script context is destroyed, but this can be delayed.

---

## What's missing

- Enums and sets
- Operator overloading
- Properties with getters/setters
- Default and named parameters
- Unicode strings (currently AnsiString only)
- Memory managment may still have edge cases
