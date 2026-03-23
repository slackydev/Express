- Type system needs to be AST nodes for a proper implementation allowing for scoped types.

- Functions exist in delayed code, this means that the functions are actually declared 
  after all existing global variables, so essentilly we can reach ALL variables from ANY 
  function, and functions can reach all other functions. Make the order logical.
  
- Generics [after type system redesign] design remains undecided.

- Properties [read/write] and index for classes and maybe records.

- Operator overload that integrates naturally for class objects.
  Allow classes to come close to being "fake" base-types.
  
- Diver deeper in threading, cleaner user interface, consider pools.

- Foreign function interface (FFI), consider stealing the one in Lape.

- UTF16 (UnicodeString) string literals? `u'mystring'`
  Delphi defaults to UTF16 since Delphi 2009 I believe.

- Inlining functions by rewriting inside bytecode emitter.

- Temporary reusage, can reduce memory footprint and improve performance
  Further more this can aid in array allocation if we know we can reuse existing thrashed [but not yet freed temps].
  
- Enums and sets, I do not want the ancient Pascal style enums, undecided design.

- Functions should be able to be called with named arguments `Foo(y=99)`.

- Functions should support default arguments `func Foo(x:int=0; y:int=0)`.
