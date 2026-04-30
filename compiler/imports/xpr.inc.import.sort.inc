type
  TComparator = function(a, b: Pointer): NativeInt; cdecl;

procedure InsertionSort(base: Pointer; count, size: NativeInt; cmp: TComparator);
var
  i, j: NativeInt;
  temp: Pointer;
  a, b: PByte;
begin
  GetMem(temp, size);

  for i := 1 to count - 1 do
  begin
    a := PByte(base) + i * size;
    Move(a^, temp^, size);

    j := i - 1;
    while j >= 0 do
    begin
      b := PByte(base) + j * size;
      if cmp(b, temp) <= 0 then Break;

      Move(b^, (b + size)^, size);
      Dec(j);
    end;

    Move(temp^, (PByte(base) + (j + 1) * size)^, size);
  end;

  FreeMem(temp);
end;

procedure Merge(src, dest: Pointer; left, mid, right, size: NativeInt; cmp: TComparator);
var
  i, j, k: NativeInt;
  a, b, d: PByte;
begin
  i := left;
  j := mid;
  k := left;

  while (i < mid) and (j < right) do
  begin
    a := PByte(src) + i * size;
    b := PByte(src) + j * size;

    if cmp(a, b) <= 0 then
    begin
      Move(a^, (PByte(dest) + k * size)^, size);
      Inc(i);
    end
    else
    begin
      Move(b^, (PByte(dest) + k * size)^, size);
      Inc(j);
    end;

    Inc(k);
  end;

  while i < mid do
  begin
    Move((PByte(src) + i * size)^, (PByte(dest) + k * size)^, size);
    Inc(i);
    Inc(k);
  end;

  while j < right do
  begin
    Move((PByte(src) + j * size)^, (PByte(dest) + k * size)^, size);
    Inc(j);
    Inc(k);
  end;
end;

procedure ExpressMergeSort(base: Pointer; count, size: NativeInt; cmp: TComparator);
const
  INSERTION_CUTOFF = 16;
var
  temp: Pointer;
  width, i: NativeInt;
  src, dest: Pointer;
  left, mid, right: NativeInt;
begin
  if count <= 1 then Exit;

  // Pre-pass insertion sort
  i := 0;
  while i < count do
  begin
    if i + INSERTION_CUTOFF < count then
      InsertionSort(PByte(base) + i * size, INSERTION_CUTOFF, size, cmp)
    else
      InsertionSort(PByte(base) + i * size, count - i, size, cmp);

    Inc(i, INSERTION_CUTOFF);
  end;

  GetMem(temp, count * size);

  src := base;
  dest := temp;
  width := INSERTION_CUTOFF;

  while width < count do
  begin
    i := 0;

    while i < count do
    begin
      left := i;
      mid := i + width;
      right := i + 2 * width;

      if mid > count then mid := count;
      if right > count then right := count;

      if mid < right then
      begin
        // Optional optimization: skip merge if already ordered
        if cmp(PByte(src) + (mid - 1) * size,
               PByte(src) + mid * size) <= 0 then
        begin
          Move((PByte(src) + left * size)^,
               (PByte(dest) + left * size)^,
               (right - left) * size);
        end
        else
          Merge(src, dest, left, mid, right, size, cmp);
      end
      else
      begin
        Move((PByte(src) + left * size)^,
             (PByte(dest) + left * size)^,
             (right - left) * size);
      end;

      Inc(i, 2 * width);
    end;

    // swap buffers
    if src = base then
    begin
      src := temp;
      dest := base;
    end
    else
    begin
      src := base;
      dest := temp;
    end;

    width := width * 2;
  end;

  if src <> base then
    Move(src^, base^, count * size);

  FreeMem(temp);
end;

procedure _ExpressMergeSort(const Params: PParamArray);
begin
  ExpressMergeSort(Pointer(Params^[0]^), SizeInt(Params^[1]^), SizeInt(Params^[2]^), TComparator(Params^[3]^));
end;
