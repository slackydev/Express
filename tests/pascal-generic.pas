// pascal generics here still extremly limited
program GenericTest;

type
  TArray<T> = array of T
  TIntArray = specialize TArray<Integer>;


var
  test: TIntArray;
begin
  test.append(100)
  WriteLn(test);
  
  High(test);
end.