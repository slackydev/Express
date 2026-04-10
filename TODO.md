- Type system should probably be AST nodes for a more proper implementation.

- Undecided: Functions exist in delayed code, this means that the functions are actually  
  declared after all existing global variables, so essentilly we can reach ALL global 
  variables from ANY function, and functions can reach all other functions. 
  
  Is this a problem?
  
- Properties [read/write] and index for classes and maybe records.

- Operator overload that integrates naturally for class objects.
  Allow classes to come close to being "fake" base-types.
  
- Dive deeper in threading, cleaner user interface, consider pools.
  Current: Active work in progress

- Foreign function interface (FFI), consider stealing the one in Lape.
  Current: Active work in progress

- UTF16 (UnicodeString) string literals? `u'mystring'`
  Delphi defaults to UTF16 since Delphi 2009 I believe.

- Temporary reusage, can reduce memory footprint and improve performance
  Further more this can aid in array allocation if we know we can reuse existing trashed [but not yet freed temps].
  
- Enums and sets, I do not want the ancient Pascal style enums, undecided design.

- ~Generics [after type system redesign] design remains undecided.~

  Generics are now a thing, though still fresh feature which means bugs.

- ~Inlining functions by rewriting inside bytecode emitter.~

  Basics are working, however restrictions and some dynamic selection may be valuable.

- ~Functions should be able to be called with named arguments `Foo(y:=99)`.~

- ~Functions should support default arguments `func Foo(x:int:=0; y:int:=0)`.~

- ~Lambda needs capture refinement, we should only capture references where variables are actually used.~

- ~Local methods paramter capture should only parameter-refer actually used variables~
