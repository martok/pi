unit uCIntegerBucketList;

interface

uses
  SysUtils, Classes;

type
  TIntegerBucketList = class
  private
    FBuckets: TList;
    FSorted: boolean;
    function IndexOf(const aValue: Integer): Integer;
    function GetCount: Cardinal;
  public
    constructor Create;
    destructor Destroy; override;

    procedure Clear;
    procedure Push(const aValue: Integer; const aMulitplicity: Integer = 1);
    procedure SortByMultiplicity;
    property Count: Cardinal read GetCount;
    function GetMostCommon: Integer;
  end;


implementation

type
  PIBLItem = ^TIBLItem;
  TIBLItem = record
    Value: Integer;
    Count: Cardinal;
  end;

{ TIntegerBucketList }

constructor TIntegerBucketList.Create;
begin
  inherited Create;
  FBuckets:= TList.Create;
  FSorted:= false;
end;

destructor TIntegerBucketList.Destroy;
begin
  Clear;
  FreeAndNil(FBuckets);
  inherited;
end;

procedure TIntegerBucketList.Clear;
var
  i: integer;
begin
  for i:= 0 to FBuckets.Count-1 do
    Dispose(FBuckets[i]);
  FBuckets.Clear;        
  FSorted:= true;
end;

procedure TIntegerBucketList.Push(const aValue, aMulitplicity: Integer);
var
  i: integer;
  p: PIBLItem;
begin
  i:= IndexOf(aValue);
  if i<0 then begin
    New(p);
    p^.Value:= aValue;
    p^.Count:= aMulitplicity;
    FBuckets.Add(p);
  end else begin
    p:= PIBLItem(FBuckets[i]);
    Inc(p^.Count, aMulitplicity);
  end; 
  FSorted:= false;
end;

function TIntegerBucketList.IndexOf(const aValue: Integer): Integer;
var
  i: integer;
begin
  Result:= -1;
  for i:= 0 to FBuckets.Count-1 do
    if PIBLItem(FBuckets[i]).Value=aValue then begin
      Result:= i;
      exit;
    end;
end;

function TIntegerBucketList.GetCount: Cardinal;
begin
  Result:= FBuckets.Count;
end;

function TIntegerBucketList_SortByMultiplicity(It1,It2: Pointer): integer;
begin
  Result:= PIBLItem(It2)^.Count - PIBLItem(It1)^.Count;
end;

procedure TIntegerBucketList.SortByMultiplicity;
begin
  FBuckets.Sort(TIntegerBucketList_SortByMultiplicity);       
  FSorted:= true;
end;

function TIntegerBucketList.GetMostCommon: Integer;
begin
  Result:= 0;
  if not FSorted then
    SortByMultiplicity;
  if GetCount>0 then
    Result:= PIBLItem(FBuckets[0])^.Value;
end;

end.
