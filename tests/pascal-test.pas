program VirtualExample;

type
  { Base Class }
  TAnimal = class
  public
    AnimalID: Int32;
    procedure Speak; virtual; // Initial virtual declaration
  end;

  { Descendant Class }
  TDog = class(TAnimal)
  public
    procedure Speak; override; // Overriding the base method
  end;

{ Implementation of TAnimal }
procedure TAnimal.Speak; 
begin
  WriteLn('The animal makes a sound');
end;

{ Implementation of TDog }
procedure TDog.Speak;
begin
  WriteLn('The dog barks: Woof!');
end;


var
  MyAnimal: TAnimal;
begin
  { Polymorphism in action }
  MyAnimal := TDog.Create;
  try
    // Even though MyAnimal is declared as TAnimal, 
    // it calls TDog.Speak because the method is virtual.
    MyAnimal.Speak; 
  finally
    MyAnimal.Free;
  end;
end.