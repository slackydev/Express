unit xprDictionary;
{
  Author: Jarl K. Holta
  License: GNU Lesser GPL (http://www.gnu.org/licenses/lgpl.html)

  A generic dictionary structure that should work for most purposes.
  ----------------------------------------------------------------------
  You need to write your own hash function if you want the `key` to be
  anything other than Int8/UInt8, Int32/UInt32, Int64/UInt64, or string.
  ----------------------------------------------------------------------
  The hash-function prototype looks like:
  >> `function(constref k: T): UInt32;`
  ----------------------------------------------------------------------
  Removing items will never reduce the hashmap-size, only the bucket-size.
  This is because with a number of removal, there will often come a nearly
  identical number of a new additions. And so we avoid expensive resizing.
  ----------------------------------------------------------------------
  Rough example:
    type TStringToFloatMap = specialize TDictionary<string, Double>
    var d:TStringToFloatMap;
    begin
      d := TStringToFloatMap.Create(@HashStr);

      d['hello'] := 99.152;
      WriteLn( d['hello'] );
      WriteLn( d.GetDef('not here', -1) );

      d.Destroy;
    end;
}
{$mode delphi}{$H+}
{$hints off}
{.$WARN off}
{$rangechecks OFF}
interface

uses
  Classes, SysUtils;

const
  // Minimum size of the dictionary
  DICT_MIN_SIZE = 32;

  // The basic growth strategy
  // EG: 2 means that it will double the size every time it resizes
  DICT_GROWTH_STRAT = 2;

  // The growth strategy when the dict grows beyond 50,000 items
  // EG: 3 means that it will tripple the size every time it resizes
  DICT_GROWTH_STRAT_50K = 2;


type
  DictException = class(Exception);

  THashIndex = packed record 
    hash,idx:UInt32; 
  end;

  {
    TDictionary<K,V>

    A simple datastructure that allows for efficient indexing using "keys".
    The key can be just about any datatype, as long as it can be hashed in a
    useful manner:
    Hashing a key should always give the same result, and preferably be fast
    as the hash-value will be computed every time you lookup, add or delete an
    item, as well as whenever the map needs to grow.
  }
  TDictionary<K,V> = class
  public type
    PtrV         = ^V;
    TSelfType    = TDictionary<K,V>;
    THashElement = record key:K; val:V; end;
    THashBucket  = array of THashElement;
    TMap         = array of THashBucket;
    THash        = function(constref key:K): UInt32;
  private
    FData: TMap;         //map
    FSize: UInt32;       //num items
    FHigh: UInt32;       //real size
    FResizable: Boolean; //static sized map? Default = False
    {$IFDEF TRACK_DISTRIBUTION}
    FUsedBins: UInt32;
    {$ENDIF}
  public
    HashFunc: THash;

    procedure _growRebuild();
    function _addItem(h:UInt32; key:K; value:V; checkResize:Boolean=True): Boolean; inline;
    function _delItem(pos:THashIndex): Boolean; overload; inline;
    function _delItem(hash,idx:UInt32): Boolean; overload; inline;
  public
    // create
    constructor Create(AHashFunc:THash; BaseSize:Int32=0);
    
    // create a copy
    function Copy: TSelfType;

    // Sets the base-size of the dictionary
    // Can be used to reduce the number of rebuilds.
    // Can't be used after items has been added to the dict.
    procedure SetSize(k:SizeInt); inline;

    // Clear the dictionary - removes all elements, and sizes it down to 0.
    procedure Clear; inline;

    {$IFDEF TRACK_DISTRIBUTION}
    function AvgBinsize: Double;
    {$ENDIF}
    
    // function used to hash the key
    function Hash(constref key:K): UInt32; inline;

    // Returns position `pos` of they item `key`
    // it can then be used with the the read-only property `Items`
    function Find(constref key:K; out pos:THashIndex): Boolean; inline;

    // Add a key-value pair to the hashmap. If the key already exists it's value
    // will be changed to `value`.
    // Same as `dict[key] := value`.
    procedure AddFast(key:K; value:V); inline;

    // Look up a key. Will raise an exception if it's not found.
    // Same as `value := dict[key]`
    function GetFast(key:K): V; inline;
    
    // Add a key-value pair to the hashmap. Will not modify existing keys
    // instead return False.
    function Add(key:K; value:V): Boolean; inline;

    // Add a key-value pair to the hashmap. If the key already exists it's value
    // will be changed to `value`.
    // Returns True it already existed.
    function AddOrModify(key:K; value:V): Boolean; inline;

    // Look up a key, sets the `value`. Returns False if it's not found.
    function Get(key:K; var value:V): Boolean; inline;

    // Look up a key. Returns the given default value if not found.
    function GetDef(key:K; default:V): V; inline;
    
    // Look up a key. Returns the given default value if not found.
    function GetRef(key:K): PtrV; inline;

    // Removes the given key, will return False if it doesn't exist.
    //
    // Will never reduce the hashmap-size, only the bucket-size.
    // This is because with a number of removal, there will often come
    // a nearly identical number of a new additions. And so we avoid expensive
    // resizing.
    function Remove(key:K): Boolean; inline;

    // Check if a key exists. If it does it will return True.
    function Contains(key:K): Boolean; inline;

    // property item (default) - used to index the dictionary as if it's
    // a regualar array like structure. Can be used to add new key-value pairs.
    //
    // > mydict['hello world'] := 123;
    // > value := mydict['hello world'];
    property Item[key:K]: V read GetFast write AddFast; default;


    //-------| Field access properties |------------------------------------

    // Access hashmap-items directly [r]
    // value := Dict.Items[hash,idx]
    property Items:TMap read FData write FData;

    // Sets whether the map can (automatically) resize, or not (Default = True);
    property Resizable:Boolean read FResizable write FResizable;

    // Should not be modified unless you know what you are doing
    property Size:UInt32 read FHigh write FHigh;
    property RealSize:UInt32 read FSize write FSize;
  end;



// hash-functions to go with the hashtable.
function HashByte(constref k: Byte): UInt32; inline;
function HashInt32(constref k: Int32): UInt32; inline;
function HashInt64(constref k: Int64): UInt32; inline;
function HashNative(constref k: NativeInt): UInt32; inline;
function HashPointer(constref k: PtrUInt): UInt32; inline;
function HashFloat(constref k: Double): UInt32; inline;
function HashStr(constref k: string): UInt32; inline;

//------------------------------------------------------------------------------
implementation

uses
  math, xprUtils;

(******************************* Hash Functions *******************************)
function HashByte(constref k: Byte): UInt32;
begin
  Result := k;
end;

function HashInt32(constref k: Int32): UInt32;
begin
  Result := k;
end;

function HashInt64(constref k: Int64): UInt32;
begin
  Result := k;
end;

function HashNative(constref k: NativeInt): UInt32;
begin
  Result := k;
end;

function HashPointer(constref k: PtrUInt): UInt32;
begin
  Result := k;
end;

function HashFloat(constref k: Double): UInt32;
begin
  Result := UInt64(k);
end;

function HashStr(constref k: string): UInt32;
var i:Int32;
begin
  Result := $811C9DC5;
  for i:=1 to Length(k) do
  begin
    Result := Result xor Ord(k[i]);
    Result := Result * $1000193;
  end;
end;


(******************************************************************************)
constructor TDictionary<K,V>.Create(AHashFunc:THash; BaseSize:Int32=0);
begin
  if BaseSize = 0 then
    BaseSize := DICT_MIN_SIZE;

  FHigh := 0;
  FSize := BaseSize;
  SetLength(FData, BaseSize);
  
  HashFunc := AHashFunc;
  FResizable := True;
  {$IFDEF TRACK_DISTRIBUTION}FUsedBins := 0;{$ENDIF}
end;


function TDictionary<K,V>.Copy(): TSelfType;
var i:Int32;
begin
  Result := TSelfType.Create(@HashFunc);
  Result.Resizable := Self.FResizable;
  Result.FSize := Self.FSize;
  Result.FHigh := Self.FHigh;
  {$IFDEF TRACK_DISTRIBUTION}Result.FUsedBins := Self.FUsedBins;{$ENDIF}
  
  SetLength(Result.FData, Length(Self.FData));
  for i:=0 to High(Self.FData) do
    Result.FData[i] := System.Copy(Self.FData[i]);
end;


procedure TDictionary<K,V>.SetSize(k:SizeInt);
begin
  if FHigh <> 0 then
    raise DictException.Create('Can''t set size after dictionary has been filled. Call `clear` first');
  FSize := Max(DICT_MIN_SIZE-1, Round(k*1.2));
  SetLength(FData, FSize);
end;


procedure TDictionary<K,V>.Clear;
begin
  SetLength(FData, 0);
  FHigh := 0;
  FSize := DICT_MIN_SIZE;
  SetLength(FData, DICT_MIN_SIZE);
end;

{$IFDEF TRACK_DISTRIBUTION}
function TDictionary<K,V>.AvgBinsize: Double;
begin
  if FUsedBins <> 0 then
    Result := FHigh / FUsedBins
  else
    Result := NaN;
end;
{$ENDIF}

function TDictionary<K,V>.Hash(constref key: K): UInt32;
begin
  Result := (HashFunc(key) * 31) mod FSize;
end;


procedure TDictionary<K,V>._growRebuild();
var
  i,j,k,hi:Int32;
  temp:Array of THashElement;
  hval: UInt32;
begin
  {$IFDEF TRACK_DISTRIBUTION}FUsedBins := 0;{$ENDIF}
  SetLength(temp, FHigh);
  k := 0;
  for i:=0 to FSize-1 do
  begin
    for j:=0 to High(FData[i]) do
    begin
      temp[k] := FData[i][j];
      inc(k);
    end;
    SetLength(FData[i], 0);
  end;

  if FHigh < 50000 then 
    FSize := FSize * DICT_GROWTH_STRAT
  else 
    FSize := FSize * DICT_GROWTH_STRAT_50K;

  
  SetLength(FData, FSize);
  hi := FHigh;
  FHigh := 0;
  for i:=0 to hi-1 do
  begin
    hval := self.hash(temp[i].key);
    self._addItem(hval, temp[i].key, temp[i].val, False);
  end;
end;


function TDictionary<K,V>._addItem(h:UInt32; key: K; value:V; checkResize:Boolean=True): Boolean;
var l: Int32;
begin
  l := Length(FData[h]);
  SetLength(FData[h], l+1);
  FData[h][l].key := key;
  FData[h][l].val := value;
  Inc(FHigh);
  
  {$IFDEF TRACK_DISTRIBUTION}if l = 0 then Inc(FUsedBins);{$ENDIF}
  
  if FResizable and checkResize and (FHigh > FSize*0.8) then
    self._growRebuild(); //grow if it's close to being filled.

  Result := True;
end;


function TDictionary<K,V>._delItem(pos:THashIndex): Boolean;
var
  l: Int32;
begin
  l := High(FData[pos.hash]);
  if pos.idx <> l then
    FData[pos.hash][pos.idx] := FData[pos.hash][l];
  SetLength(FData[pos.hash], l);
  Dec(FHigh);
  Result := True;
  {$IFDEF TRACK_DISTRIBUTION}if l = 0 then Dec(FUsedBins);{$ENDIF}
end;

function TDictionary<K,V>._delItem(hash,idx:UInt32): Boolean;
var
  l: Int32;
begin
  l := High(FData[hash]);
  if idx <> l then FData[hash][idx] := FData[hash][l];
  SetLength(FData[hash], l);
  Dec(FHigh);
  Result := True;
  {$IFDEF TRACK_DISTRIBUTION}if l = 0 then Dec(FUsedBins);{$ENDIF}
end;


function TDictionary<K,V>.Find(constref key: K; out pos:THashIndex): Boolean;
var
  l: Int32;
begin
  pos.hash := Hash(key);
  l := High(FData[pos.hash]);
  pos.idx := 0;
  while pos.idx <= l do
  begin
    if FData[pos.hash][pos.idx].key = key then
      Exit(True);
    Inc(pos.idx);
  end;
  Result := False;
end;

procedure TDictionary<K,V>.AddFast(key: K; value:V);
var pos: THashIndex;
begin
  if Find(key, pos) then
    FData[pos.hash][pos.idx].val := value
  else
    _addItem(pos.hash, key, value);
end;


function TDictionary<K,V>.GetFast(key: K): V;
var pos: THashIndex;
begin
  if not Find(key, pos) then
    raise DictException.Create('The key does not exist') at get_caller_addr(get_frame);
  Result := FData[pos.hash][pos.idx].val;
end;


function TDictionary<K,V>.Add(key: K; value:V): Boolean;
var pos: THashIndex;
begin
  if Find(key, pos) then Exit(False);
  Result := _addItem(pos.hash, key, value);
end;


function TDictionary<K,V>.AddOrModify(key: K; value:V): Boolean;
var pos: THashIndex;
begin
  if Find(key, pos) then
  begin
    FData[pos.hash][pos.idx].val := value;
    Result := True;
  end else
  begin
    _addItem(pos.hash, key, value);
    Result := False;
  end;
end;


function TDictionary<K,V>.Get(key: K; var value: V): Boolean;
var pos: THashIndex;
begin
  if not Find(key, pos) then Exit(False);
  Value := FData[pos.hash][pos.idx].val;
  Result := True;
end;


function TDictionary<K,V>.GetDef(key: K; default:V): V;
var pos: THashIndex;
begin
  if not Find(key, pos) then Exit(default);
  Result := FData[pos.hash][pos.idx].val;
end;

function TDictionary<K,V>.GetRef(key: K): PtrV;
var pos: THashIndex;
begin
  if not Find(key, pos) then Exit(nil);
  Result := @FData[pos.hash][pos.idx].val;
end;


function TDictionary<K,V>.Remove(key: K): Boolean;
var pos: THashIndex;
begin
  if not Find(key, pos) then Exit(False);
  Result := _delItem(pos);
end;


function TDictionary<K,V>.Contains(key: K): Boolean;
var idx: THashIndex;
begin
  Result := Find(key, idx);
end;

end.
