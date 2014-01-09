unit uExpressionMatcher;

interface

uses
  SysUtils, Classes, IniFiles, uMathIntf, uMath, uFunctionsSymbolics;

type
  TExpressionMatcher = class
  private
    fSystem: TMathSystem;
    fPackage: TPackageSymbolics;
    fPattern: IExpression;
    fContext: IContext;
    fFreeVars: TStringList;
    fVars: array of THashedStringList;
    function VerifyVariables(const Expr: IExpression; const v: integer): boolean;
    function FreeVarName(const i: integer): string;
    function FreeVarIndex(const name: string): integer;
    procedure StoreFreeVariable(const v: integer; const aValue: IExpression);
  protected
    function MatchExpression(const Ex, Pat: IExpression): boolean;
    function MatchCallOrderless(const Ex, Pat: TExprList; const fp: TFunctionProperties; const OrigEx: IFunctionCall): Boolean;
    function MatchCallOrdered(const Ex, Pat: TExprList; const fp: TFunctionProperties; const OrigEx: IFunctionCall): Boolean;
    function MatchCallPartitioned(const Ex, Pat: TExprList; const fp: TFunctionProperties; const OrigEx: IFunctionCall): Boolean;
    function MatchCallAsIs(const Ex, Pat: TExprList; const fp: TFunctionProperties; const OrigEx: IFunctionCall): Boolean;
  public
    constructor Create(System: TMathSystem; Pattern: IExpression; FreeVars: TStringList);
    destructor Destroy; override;

    // Define all free vars as unassigned in Context and store it
    procedure SetupContext(const Context: IContext);
    // Match an expression
    // Return: true if matching and variable resolution worked
    // Status: Context is one matching set of assignments
    function Match(const Expr: IExpression): boolean;
  end;

implementation

uses
  uMathValues;

type
  TVariableAssignment = class
    Value: IExpression;
  end;


{ TExpressionMatcher }

constructor TExpressionMatcher.Create(System: TMathSystem; Pattern: IExpression; FreeVars: TStringList);
begin
  inherited Create;
  fSystem:= System;
  fPattern:= Pattern;
  fPackage:= fSystem.GetPackageInstance(TPackageSymbolics) as TPackageSymbolics;
  if not Assigned(fPackage) then
    raise EMathSysError.Create('Package Symbolics not present in current Kernel');

  fFreeVars:= TStringList.Create;
  fFreeVars.Sorted:= true;
  fFreeVars.Duplicates:= dupIgnore;
  if Assigned(FreeVars) then
    fFreeVars.AddStrings(FreeVars);
end;

destructor TExpressionMatcher.Destroy;
begin
  SetupContext(nil);
  FreeAndNil(fFreeVars);
  inherited;
end;

procedure TExpressionMatcher.SetupContext(const Context: IContext);
var
  i, j: integer;
begin
  for i:= 0 to high(fVars) do begin
    for j:= 0 to fVars[i].Count-1 do
      fVars[i].Objects[j].Free;
    FreeAndNil(fVars[i]);
  end;

  fContext:= Context;
  if Assigned(fContext) then begin
    fContext.Clear;

    SetLength(fVars, fFreeVars.Count);
    for i:= 0 to fFreeVars.Count-1 do begin
      fContext.Define(FreeVarName(i), TValueUnassigned.Create);
      fVars[i]:= THashedStringList.Create;
    end;
  end;
end;

function TExpressionMatcher.Match(const Expr: IExpression): boolean;
  procedure DebugCandidates;
  var
    i, j: integer;
    e: IExpression;
    sc: IStringConvertible;
  begin
    Writeln('Candidates dump:');
    for i:= 0 to high(fVars) do begin
      Writeln(FreeVarName(i), ' = ');
      for j:= 0 to fVars[i].Count-1 do begin
        e:= TVariableAssignment(fVars[i].Objects[j]).Value;
        if e.Represents(IStringConvertible, sc) then
          Writeln('  ', sc.AsString(STR_FORMAT_INPUT_EXPANDED))
        else
          Writeln('  ', e.NativeObject.ClassName);
      end;
    end;
  end;

var
  i: integer;
begin
  if not Assigned(fContext) or (length(fVars)<>fFreeVars.Count) then
    raise EMathSysError.Create('Call SetupContext first!');

  Result:= MatchExpression(Expr, fPattern);
  if Result and (length(fVars) > 0) then begin
    // wurde für jede Var etwas gefunden? wenn nicht, sollten wir gar nicht hier sein...
    for i:= 0 to high(fVars) do
      if fVars[i].Count = 0 then begin
        Result:= false;
        exit;
      end;
    //DebugCandidates;
    Result:= VerifyVariables(Expr, 0);
  end;
end;

function TExpressionMatcher.VerifyVariables(const Expr: IExpression; const v: integer): boolean;
var
  i: integer;
  test: IExpression;
begin
  Result:= false;
  if v = length(fVars) then begin
    // alle variablen eintragen, der ausdruck ist jetzt un-frei
    test:= SubstituteContextVars(fPattern, fContext);
    // ist diese einsetzung identisch mit dem was wir suchen?
    // MatchExpressions geht, weil nix mehr frei ist, also große teile gar nicht durchlaufen werden
    Result:= MatchExpression(Expr, test);
  end else begin
    for i:= 0 to fVars[v].Count-1 do begin
      fContext.Define(FreeVarName(v), TVariableAssignment(fVars[v].Objects[i]).Value);
      Result:= VerifyVariables(Expr, v+1);
      if Result then
        exit;
    end;
  end;
end;

procedure TExpressionMatcher.StoreFreeVariable(const v: integer; const aValue: IExpression);
var
  sc: IStringConvertible;
  c,k: string;
  va: TVariableAssignment;
begin
  // Constraints
  c:= LowerCase(fFreeVars.ValueFromIndex[v]);
  if c > '' then begin
    if (c='const') and not aValue.Represents(IExpressionAtom) then exit;
  end;

  // yes, a faster hashCode would be nice, but this is just so cheap to code :)
  if aValue.Represents(IStringConvertible, sc) then
    k:= sc.AsString(STR_FORMAT_INPUT)
  else
    k:= '(addr)'+IntToHex(Cardinal(aValue),8);
  if fVars[v].IndexOf(k) < 0 then begin
    va:= TVariableAssignment.Create;
    va.Value:= aValue;
    fVars[v].AddObject(k, va);
  end;
end;

function TExpressionMatcher.MatchExpression(const Ex, Pat: IExpression): boolean;
var
  ea, pa: IExpressionAtom;
  esy, psy: ISymbolReference;
  efc, pfc: IFunctionCall;
  epl, ppl: TExprList;
  fp: TFunctionProperties;
  vi: Integer;
begin
  // atoms
  if ex.Represents(IExpressionAtom, ea) and pat.Represents(IExpressionAtom, pa) then begin
    Result:= ea.CompareTo(pa) = crSame;
  end else
  // symbol reference
  if ex.Represents(ISymbolReference, esy) and pat.Represents(ISymbolReference, psy) then begin
    vi:= FreeVarIndex(psy.Name);
    if vi < 0 then
      Result:= SameText(esy.Name,psy.Name)
    else begin
      StoreFreeVariable(vi, esy);
      Result:= true;
    end;
  end else
  // function call
  if ex.Represents(IFunctionCall, efc) and pat.Represents(IFunctionCall, pfc) and
    (SameText(efc.Name, pfc.Name)) then begin
    // check arguments
    epl:= efc.GetArgs;
    ppl:= pfc.GetArgs;

    fp:= fPackage.Properties(efc.Name);
    if fp.Commutative then
      Result:= MatchCallOrderless(epl, ppl, fp, efc)
    else
      Result:= MatchCallOrdered(epl, ppl, fp, efc);
  end else 
  // symbol reference in pattern only
  if pat.Represents(ISymbolReference, psy) then begin
    vi:= FreeVarIndex(psy.Name);
    if vi < 0 then
      Result:= false
    else begin
      StoreFreeVariable(vi, ex);
      Result:= true;
    end;
  end else
    Result:= false;
end;

// non-associative, ordered expression
function TExpressionMatcher.MatchCallAsIs(const Ex, Pat: TExprList; const fp: TFunctionProperties; const OrigEx: IFunctionCall): Boolean;
var
  i: integer;
begin
  Result:= high(Ex) = high(Pat);
  if Result then
    for i:= 0 to high(Pat) do begin
      Result:= MatchExpression(Ex[i], Pat[i]);
    end;
end;

// associative, ordered expression
function TExpressionMatcher.MatchCallPartitioned(const Ex, Pat: TExprList; const fp: TFunctionProperties; const OrigEx: IFunctionCall): Boolean;

  // Result: does a partition exist that is entirely possible?
  function PartitionIntoVars(ef,el,pf,pl: integer): boolean;
  var
    st: boolean;
    subset: IExpression;
    ea: TExprList;
    tr, l: integer;
  begin
    // one cursor reached the end
    if (pf > pl) or (ef > el) then begin
      // if the other did as well, we're positively done
      Result:= (pf > pl) and (ef > el);
      exit;
    end;
    Result:= false;
    // pat[pf] could be any partition from ef..el
    for tr:= ef to el do begin
      l:= tr-ef+1;
      if l > 1 then begin
        // "real" partition?
        subset:= OrigEx.Clone(true);
        ea:= Copy(ex, ef, l);
        subset.SetArgs(ea);
      end else
        // false, only one expression, no need to uselessly wrap it
        subset:= Ex[tr];
      // store this assignment (if it even is one)
      st:= MatchExpression(subset, pat[pf]);
      // divide the rest
      st:= st and PartitionIntoVars(tr+1, el, pf+1, pl);
      // st: is this subtree leading to a good solution?
      if st then
        Result:= true;
    end;
  end;
begin
  Result:= PartitionIntoVars(0, high(ex), 0, high(pat));
end;

// possibly associative, ordered expression
function TExpressionMatcher.MatchCallOrdered(const Ex, Pat: TExprList; const fp: TFunctionProperties; const OrigEx: IFunctionCall): Boolean;
begin
  if fp.Associative then
    Result:= MatchCallPartitioned(Ex, Pat, fp, OrigEx)
  else
    Result:= MatchCallAsIs(Ex, Pat, fp, OrigEx);
end;

type
  TDynIntArray = array of integer;

procedure IndexPermutationFirst(out a:TDynIntArray; const Count: integer);
var
  i: integer;
begin
  SetLength(a, Count);
  for i:= 0 to Count-1 do
    a[i]:= i;
end;

function IndexPermutationNext(var a:TDynIntArray): Boolean;
var
   n,k,j,r,s : integer;
   procedure swap(i,j :integer);
   var
     temp : integer;
   begin
     temp := a[i];
     a[i] := a[j];
     a[j] := temp;
   end;

begin
  n := Length(a);
  k := n-1;
  while a[k] > a[k+1] do
    k:=k-1;
  if k <> 0 then begin
    j := n;
    while a[k] > a[j] do
      j:=j-1;
    swap(j,k);
    r:=n;
    s:=k+1;
    while r>s do begin
      swap(r,s);
      r:=r-1;
      s:=s+1;
    end;
    Result:= true;
  end else
    {Reverse order reached -> end}
    Result := false
end;


// possibly associative, commutative expression
function TExpressionMatcher.MatchCallOrderless(const Ex, Pat: TExprList; const fp: TFunctionProperties; const OrigEx: IFunctionCall): Boolean;
  function FreeVar(const e: IExpression): integer;
  var
    sy: ISymbolReference;
  begin
    Result:= -1;
    if e.Represents(ISymbolReference, sy) then
      Result:= FreeVarIndex(sy.Name);
  end;

  procedure PushList(var List: TExprList; Item: IExpression);
  var
    k: integer;
  begin
    k:= length(List);
    SetLength(List, k+1);
    List[k]:= Item;
  end;

var
  vex, vpat: TExprList;
  used: array of Boolean;
  solved: boolean;
  i, j: integer;
  pati: TDynIntArray;
  patt: TExprList;
begin
  Result:= false;
  SetLength(used, length(ex));
  FillChar(used[0], length(ex), 0);
  SetLength(vpat, 0);
  // first check everything that is not a variable
  for i:= 0 to high(pat) do
    if FreeVar(pat[i])<0 then begin
      solved:= false;
      // anything looking like this non-free var?
      for j:= 0 to high(ex) do
        if not used[j] and MatchExpression(pat[i], ex[j]) then begin
          used[j]:= true;
          solved:= true;
          break;
        end;
      if not solved then  // found nothing for a non-var, can't fix this
        exit;
    end else begin
      // save free var for later
      PushList(vpat, pat[i]);
    end;

  // remaining expressions can be used for free variables
  SetLength(vex, 0);
  for i:= 0 to high(used) do
    if not used[i] then
      PushList(vex, ex[i]);

  j:= Length(vpat);

  if j = 0 then begin
    // no free variables, check if we used all of the expression 
    Result:= length(vex) = 0;
    exit;
  end;

  // generate pattern permutations
  SetLength(patt, j);
  IndexPermutationFirst(pati, j);
  repeat
    for i:= 0 to j-1 do
      patt[i]:= vpat[pati[i]];

    if fp.Associative then
      Result:= MatchCallPartitioned(vex, patt, fp, OrigEx)
    else
      Result:= MatchCallAsIs(vex, patt, fp, OrigEx);
    if Result then
      exit;
  until not IndexPermutationNext(pati);
end;

function TExpressionMatcher.FreeVarName(const i: integer): string;
begin
  Result:= fFreeVars.Names[i];
  if Result='' then
    Result:= fFreeVars[i];
end;

function TExpressionMatcher.FreeVarIndex(const name: string): integer;
begin
  Result:= fFreeVars.IndexOf(name);
  if Result<0 then
    Result:= fFreeVars.IndexOfName(name);
end;

end.
