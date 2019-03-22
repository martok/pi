{-----------------------------------------------------------------------------
 Datatype support unit for dimensions and conversions between them.

 Logic, unit prefix notation and conversion tables based on MATLAB code
 originally (c) 2011-2013 Alexander Zinser
-----------------------------------------------------------------------------}
unit uMathDimensions;

interface

uses
  uMathIntf, uMath, Classes, Math;

type
  EMathDimensionError = class(EMathSysError);

  TValueDimension = class(TAggregatedObject, IDimensions)
  private
    FUnits: TMathUnits;
    FScale: Number;
    FCreatedAs,FDisplayString: String;
  public
    constructor Create(const Owner: IValueNumber; const Scale: Number; const Dimension: TMathUnits; const CreatedAs: string='');
    function IsScalar: boolean;
    function Units: TMathUnits;
    function UnitCompatible(const Units: TMathUnits): boolean;
    function UnitFactor: Number;
    function UnitString: String;
  end;

  TPackageDimensions = class(TFunctionPackage)
  protected
    procedure OnImport(const MS: TMathSystem); override;
  published
    function Unit_2(Context: IContext; args: TExprList): IExpression;
    function Convert_2(Context: IContext; args: TExprList): IExpression;
    function Express_2_opt(Context: IContext; args: TExprList; Options: TDynamicArguments): IExpression;
  end;

  TDimensionsList = array of record
    DimIndex: Integer;
    PrefixIndex: integer;
    Exponent: Integer;
  end;
  TDimensionParser = object
  private
    FFactor: Number;
    FDim: TMathUnits;
    function TryParseUnit(u: String; out UnitI, PrefixI, Expon: integer): boolean;
    function FormatExponent(const base: String; u: ShortInt): string;
    function FormatSIExponent(Dim: TMathBaseUnit; u: ShortInt): string;
    function GetStringFromList(const Units: TDimensionsList; const UseExponents: boolean): string;
    function GetFactorFromList(const Units: TDimensionsList): MTFloat;
    function FormatPrefix(const pref, un: String): string;
  protected
    function CombineDL(const A, B: TDimensionsList): TDimensionsList;
  public
    function SplitDimensionPart(const Str: String; const PreExponent: Shortint): TDimensionsList;
    function SplitDimension(const Str: String): TDimensionsList;
    function ApplyDimList(const DL: TDimensionsList; out ConversionFactor: Number): TMathUnits;
    function ParseUnitString(const Str: string; out ConversionFactor: Number): TMathUnits;

    function GetSIString(const Units: TMathUnits; const UseExponents: boolean): string;
    property Factor: Number read FFactor;
    property Dimension: TMathUnits read FDim write FDim;
  end;

function MakeDimension(const Dim: array of Shortint): TMathUnits;
function MultDimensions(const A, B: TMathUnits): TMathUnits;
function InverseDimensions(const A: TMathUnits): TMathUnits;
function PowerDimensions(const A: TMathUnits; const Expo: integer): TMathUnits;
function RootDimensions(const A: TMathUnits; const Expo: integer): TMathUnits;
function DimensionIsScalar(const A: TMathUnits): boolean;                
function SameDimension(const A, B: TMathUnits): boolean;

function MakeQuantity(const N: IValueNumber; const uni: String): IValueNumber;

implementation

uses
  SysUtils, StrUtils, uCIntegerBucketList, uMathConstants, uFPUSupport, uMathValues;

type
  TDefinedUnit = record
    Sign, Name: string;
    Factor: Number;
    Dim: TMathUnits;
  end;
  TDefinedPrefix = record
    Factor: Number;
    Symbol, Name: string;
  end;

var
  // ATTENTION: the first element in this table needs to be the dimensionless value
  UnitDimTable: array of TDefinedUnit;
const
  // ATTENTION: the first element in this table needs to be the neutral element
  DimPrefixTable: array[0..28] of TDefinedPrefix = (
    (Factor: 1   ; Symbol: ''; Name: ''),
    (Factor: 1e24; Symbol: 'Y'; Name: 'yotta'),
    (Factor: 1e21; Symbol: 'Z'; Name: 'zetta'),
    (Factor: 1e18; Symbol: 'E'; Name: 'exa'),
    (Factor: 1e15; Symbol: 'P'; Name: 'peta'),
    (Factor: 1e12; Symbol: 'T'; Name: 'tera'),
    (Factor: 1e9 ; Symbol: 'G'; Name: 'giga'),
    (Factor: 1e6 ; Symbol: 'M'; Name: 'mega'),
    (Factor: 1e3 ; Symbol: 'k'; Name: 'kilo'),
    (Factor: 1e2 ; Symbol: 'h'; Name: 'hecto'),
    (Factor: 1e1 ; Symbol: 'da'; Name: 'deca'),
    (Factor: 1e-1; Symbol: 'd'; Name: 'deci'),
    (Factor: 1e-2; Symbol: 'c'; Name: 'centi'),
    (Factor: 1e-3; Symbol: 'm'; Name: 'milli'),
    (Factor: 1e-6; Symbol: 'u'; Name: 'micro'),
    (Factor: 1e-9; Symbol: 'n'; Name: 'nano'),
    (Factor: 1e-12 ; Symbol: 'p'; Name: 'pico'),
    (Factor: 1e-15 ; Symbol: 'f'; Name: 'femto'),
    (Factor: 1e-18 ; Symbol: 'a'; Name: 'atto'),
    (Factor: 1e-21 ; Symbol: 'z'; Name: 'zepto'),
    (Factor: 1e-24 ; Symbol: 'y'; Name: 'yocto'),
    
    (Factor: Int64(1) shl 10 ; Symbol: 'Ki'; Name: 'Kibi'),
    (Factor: Int64(1) shl 20 ; Symbol: 'Mi'; Name: 'Mebi'),
    (Factor: Int64(1) shl 30 ; Symbol: 'Gi'; Name: 'Gibi'),
    (Factor: Int64(1) shl 40 ; Symbol: 'Ti'; Name: 'Tebi'),
    (Factor: Int64(1) shl 50 ; Symbol: 'Pi'; Name: 'Pebi'),
    (Factor: Int64(1) shl 60 ; Symbol: 'Ei'; Name: 'Exbi'),
    (Factor: Int64(1) shl 70 ; Symbol: 'Zi'; Name: 'Zebi'),
    (Factor: Int64(1) shl 80 ; Symbol: 'Yi'; Name: 'Yobi')
  );


procedure DefineUnit(Sign, Name: string; Factor: Number; Dim: array of ShortInt);
var
  i: integer;
begin
  i:= length(UnitDimTable);
  SetLength(UnitDimTable, i + 1);
  UnitDimTable[i].Sign:= Sign;
  UnitDimTable[i].Name:= Name;
  UnitDimTable[i].Factor:= Factor;
  UnitDimTable[i].Dim:= MakeDimension(Dim);
end;


function MakeQuantity(const N: IValueNumber; const uni: String): IValueNumber;
var
  u: TMathUnits;
  f: MTFloat;
  dp: TDimensionParser;
begin
  u:= dp.ParseUnitString(uni, f);
  Result:= TValueFloatDimension.Create(N.ValueFloat * f, u, f, uni);
end;

{ TValueDimension }

constructor TValueDimension.Create(const Owner: IValueNumber; const Scale: Number; const Dimension: TMathUnits; const CreatedAs: string);
begin
  inherited Create(Owner);
  FScale:= Scale;
  FUnits:= Dimension;
  FCreatedAs:= CreatedAs;
end;

function TValueDimension.IsScalar: boolean;
begin
  Result:= DimensionIsScalar(FUnits);
end;

function TValueDimension.UnitCompatible(const Units: TMathUnits): boolean;
begin
  Result:= SameDimension(FUnits, Units);
end;

function TValueDimension.UnitFactor: Number;
begin
  Result:= FScale;
end;

function TValueDimension.Units: TMathUnits;
begin
  Result:= FUnits;
end;

function TValueDimension.UnitString: String;
var
  dp: TDimensionParser;
begin
  if (FDisplayString = '') and not IsScalar then begin
    if FCreatedAs>'' then
      FDisplayString:= FCreatedAs
    else
      FDisplayString:= dp.GetSIString(FUnits, false);
  end;
  Result:= FDisplayString;
end;

{ TDimensionParser }

function TDimensionParser.ParseUnitString(const Str: string; out ConversionFactor: Number): TMathUnits;   
var
  dl: TDimensionsList;
begin
  dl:= SplitDimension(Str);
  Result:= ApplyDimList(dl, ConversionFactor);
end;

function TDimensionParser.ApplyDimList(const DL: TDimensionsList; out ConversionFactor: Number): TMathUnits;
var
  i: integer;
  f: Number;
begin
  Result:= MakeDimension([]);
  ConversionFactor:= 1.0;

  for i:= 0 to high(dl) do
    with dl[i] do begin
      f:= DimPrefixTable[PrefixIndex].Factor * UnitDimTable[DimIndex].Factor;
      Result:= MultDimensions(Result, PowerDimensions(UnitDimTable[DimIndex].Dim, Exponent));
      ConversionFactor:= ConversionFactor * Power(f, Exponent);
    end;
end;

function TDimensionParser.CombineDL(const A, B: TDimensionsList): TDimensionsList;
var
  i, j, k: integer;
  f: boolean;
begin
  SetLength(Result, Length(A) + Length(B));
  i:= 0;
  for j:= 0 to high(A) do begin
    Result[i]:= A[j];
    inc(i);
  end;
  for j:= 0 to high(B) do begin
    f:= false;
    for k:= 0 to i do 
      if (Result[k].DimIndex = B[j].DimIndex) and (Result[k].PrefixIndex = B[j].PrefixIndex) then begin
        Inc(Result[k].Exponent, b[j].Exponent);
        f:= true;
        break;
      end;
    if not f then begin
      Result[i]:= B[j];
      inc(i);
    end;
  end;
  SetLength(Result, i);
end;

function TDimensionParser.SplitDimension(const Str: String): TDimensionsList;
var
  nom, den: String;
  dl1,dl2: TDimensionsList;
begin
  if Pos('/', Str)>0 then begin
    if Pos('/', Str) <> LastDelimiter('/', Str) then
      raise EMathDimensionError.Create('Unit string must not contain more than one fraction bar.');
    nom:= Trim(Copy(Str, 1, Pos('/', Str)-1));
    den:= Trim(Copy(Str, Pos('/', Str)+1, MaxInt));
  end else begin
    nom:= trim(Str);
    den:= '';
  end;
  dl1:= SplitDimensionPart(nom, 1);
  dl2:= SplitDimensionPart(den, -1);
  Result:= CombineDL(dl1, dl2);
end;

function TDimensionParser.SplitDimensionPart(const Str: String; const PreExponent: Shortint): TDimensionsList;
var
  pts: TStringList;
  i, ut, pt, pp: integer;
begin
  SetLength(Result, 0);
  pts:= TStringList.Create;
  try
    pts.Delimiter:= ' ';
    pts.DelimitedText:= Str;
    for i:= 0 to pts.Count-1 do begin
      if not TryParseUnit(pts[i], ut, pt, pp) then
        raise EMathDimensionError.CreateFmt('Could not parse unit string: %s',[pts[i]]);
      SetLength(Result, Length(Result) + 1);
      with Result[high(Result)] do begin
        DimIndex:= ut;
        PrefixIndex:= pt;
        Exponent:= pp * PreExponent;
      end;
    end;
  finally
    FreeAndNil(pts);
  end;
end;

// k:m^-1 km^-1 k:m-1 km-1 k:m km
function TDimensionParser.TryParseUnit(u: String; out UnitI, PrefixI, Expon: integer): boolean;
var
  expo, pref, un: string;
  d: integer;
  i,j,k: integer;
begin
  Result:= false;
  PrefixI:= -1;
  UnitI:= -1;
  
  i:= 1;
  while TryStrToInt(RightStr(u, i), d) and (i <= length(u)) do
    inc(i);
  dec(i);
  expo:= RightStr(u, i);
  if MidStr(u, Length(u) - i, 1) = '^' then
    inc(i);
  un:= Copy(u, 1, Length(u) - i);
  if Pos(':', un)>0 then begin
    if Pos(':', un) <> LastDelimiter(':', un) then
      raise EMathDimensionError.Create('Unit part must not contain more than one prefix indicator.');
    pref:= Trim(copy(un, 1, Pos(':', un)-1));
    un:= Trim(copy(un, Pos(':', un)+1, MaxInt));

    // Prefix
    for i:= 0 to high(DimPrefixTable) do
      if pref = DimPrefixTable[i].Symbol then begin
        PrefixI:= i;
        break;
      end;
    if PrefixI<0 then
      raise EMathDimensionError.CreateFmt('Unknown unit prefix: %s',[pref]);

    // Dimension
    for i:= 0 to high(UnitDimTable) do
      if un = UnitDimTable[i].Sign then begin
        UnitI:= i;
        break;
      end;
    if UnitI<0 then
      raise EMathDimensionError.CreateFmt('Unknown unit symbol: %s',[un]);
  end else begin
    k:= 0;
    // is there a single unit named exactly like what we look for?
    for j:= 0 to high(UnitDimTable) do
      if un = UnitDimTable[j].Sign then begin
        PrefixI:= 0;
        UnitI:= j;
        inc(k);
      end;
    if k>1 then
      // more than one unit has the same name. this is bad. very bad.
      raise EMathDimensionError.CreateFmt('Ambiguous unit name. This is a bug: %s', [un]);
    if k < 1 then begin
      k:= 0;
      // is there a prefix-unit combination that matches?
      for i:= 1 to high(DimPrefixTable) do
        if AnsiStartsStr(DimPrefixTable[i].Symbol, un) then
          for j:= 1 to high(UnitDimTable) do
            if un = DimPrefixTable[i].Symbol+UnitDimTable[j].Sign then begin
              PrefixI:= i;
              UnitI:= j;
              inc(k);
            end;
      if k>1 then
        // there is more than one
        raise EMathDimensionError.CreateFmt('Ambiguous unit/prefix combination: %s', [un]);
      if k<1 then
        // none found
        raise EMathDimensionError.CreateFmt('Cannot resolve unit/prefix combination: %s', [un]);
      // k=1, everything is already set
    end;
  end;

  // Exponent
  Expon:= StrToIntDef(Expo, 1);

  Result:= true;
end;

function TDimensionParser.FormatPrefix(const pref, un:String): string;
begin
  if pref>'' then
    Result:= pref + ':' + un
  else
    Result:= un;
end;

function TDimensionParser.FormatExponent(const base:String; u: ShortInt): string;
begin
  if u = 1 then
    Result:= base
  else
    Result:= format('%s^%d', [base, u]);
end;

function TDimensionParser.FormatSIExponent(Dim: TMathBaseUnit; u: ShortInt): string;
begin
  if u = 1 then
    Result:= MATH_UNIT_NAME[Dim]
  else
    Result:= format('%s^%d', [MATH_UNIT_NAME[Dim], u]);
end;

function TDimensionParser.GetSIString(const Units: TMathUnits; const UseExponents: boolean): string;
var
  d: TMathBaseUnit;
  no, de: string;

begin
  Result:= '';
  if UseExponents then begin
    for d:= low(d) to high(d) do
      if Units[d] <> 0 then
        Result:= Result + FormatSIExponent(d, Units[d]) + ' ';
  end else begin
    no:= '';
    for d:= low(d) to high(d) do
      if Units[d] > 0 then
        no:= no + FormatSIExponent(d, Units[d]) + ' ';

    de:= '';
    for d:= low(d) to high(d) do
      if Units[d] < 0 then
        de:= de + FormatSIExponent(d, -Units[d]) + ' ';

    if de = '' then
      Result:= no
    else begin
      if no = '' then
        Result:= '1 / ' + de
      else
        Result:= no + '/ ' + de;
    end;
  end;
  Result:= TrimRight(Result);
end;

function TDimensionParser.GetStringFromList(const Units: TDimensionsList; const UseExponents: boolean): string;
var
  i: integer;
  no, de: string;
begin
  Result:= '';
  if UseExponents then begin
    for i:= 0 to high(Units) do
      if Units[i].Exponent<>0 then
        Result:= Result + FormatExponent(FormatPrefix(DimPrefixTable[Units[i].PrefixIndex].Symbol, UnitDimTable[Units[i].DimIndex].Sign), Units[i].Exponent) + ' ';
  end else begin
    no:= '';
    for i:= 0 to high(Units) do
      if Units[i].Exponent>0 then
          no:= no + FormatExponent(FormatPrefix(DimPrefixTable[Units[i].PrefixIndex].Symbol, UnitDimTable[Units[i].DimIndex].Sign), Units[i].Exponent) + ' ';

    de:= '';
    for i:= 0 to high(Units) do
      if Units[i].Exponent<0 then
        de:= de + FormatExponent(FormatPrefix(DimPrefixTable[Units[i].PrefixIndex].Symbol, UnitDimTable[Units[i].DimIndex].Sign), -Units[i].Exponent) + ' ';

    if de = '' then
      Result:= no
    else begin
      if no = '' then
        Result:= '1 / ' + de
      else
        Result:= no + '/ ' + de;
    end;
  end;
  Result:= TrimRight(Result);
end;

function TDimensionParser.GetFactorFromList(const Units: TDimensionsList): MTFloat;
var
  i: integer;
begin
  Result:= 1.0;
  for i:= 0 to high(Units) do
    if Units[i].Exponent<>0 then
      Result:= Result * Power(
        DimPrefixTable[Units[i].PrefixIndex].Factor*UnitDimTable[Units[i].DimIndex].Factor,
        Units[i].Exponent);
end;


function MakeDimension(const Dim: array of Shortint): TMathUnits;
var
  d: TMathBaseUnit;
  i: integer;
begin
  i:= 0;
  for d:= low(d) to high(d) do begin
    if i <= high(Dim) then
      Result[d]:= Dim[i]
    else
      Result[d]:= 0;
    inc(i);
  end;
end;

function MultDimensions(const A, B: TMathUnits): TMathUnits;
var
  d: TMathBaseUnit;
begin
  for d:= low(d) to high(d) do begin
    Result[d]:= A[d] + B[d];
  end;
end;

function InverseDimensions(const A: TMathUnits): TMathUnits;
var
  d: TMathBaseUnit;
begin
  for d:= low(d) to high(d) do begin
    Result[d]:= -A[d];
  end;
end;

function PowerDimensions(const A: TMathUnits; const Expo: integer): TMathUnits;
var
  d: TMathBaseUnit;
begin
  for d:= low(d) to high(d) do begin
    Result[d]:= A[d] * Expo;
  end;
end;

function RootDimensions(const A: TMathUnits; const Expo: integer): TMathUnits;
var
  d: TMathBaseUnit;
begin
  for d:= low(d) to high(d) do begin
    if A[d] mod Expo = 0 then
      Result[d]:= A[d] div Expo
    else
      raise EMathDimensionError.CreateFmt('Cannot take %dth root of unit.',[Expo]);
  end;
end;

function DimensionIsScalar(const A: TMathUnits): boolean;
var
  d: TMathBaseUnit;
begin
  Result:= false;
  for d:= low(d) to high(d) do
    if A[d]<>0 then
      exit;
  Result:= true;
end;

function SameDimension(const A, B: TMathUnits): boolean;
var
  d: TMathBaseUnit;
begin
  Result:= false;
  for d:= low(d) to high(d) do
    if A[d]<>B[d] then
      exit;
  Result:= true;
end;

{ TPackageDimensions }

function TPackageDimensions.Unit_2(Context: IContext; args: TExprList): IExpression;
var
  nn: IExpression;
  n: IValueNumber;
  un: string;
begin
  nn:= args[0].Evaluate(Context);

  if nn.Represents(IDimensions) then begin
    Result:= Convert_2(Context, Args);
  end else
  if nn.Represents(IValueNumber, n) then begin
    un:= EvaluateToString(Context, args[1]);
    Result:= MakeQuantity(n, un);
  end else
    raise EMathSysError.Create('Convert requires a unit value.');
end;

function TPackageDimensions.Convert_2(Context: IContext; args: TExprList): IExpression;
var
  dp: TDimensionParser;
  nn: IExpression;
  nv: IValueNumber;
  nd: IDimensions;
  f: Number;
  u: TMathUnits;
  un: string;
begin
  nn:= args[0].Evaluate(Context);

  if not nn.Represents(IValueNumber, nv) then
    raise EMathSysError.Create('Convert requires a unit value.');

  if not nn.Represents(IDimensions, nd) then
    raise EMathSysError.Create('Convert requires a unit value.');

  un:= EvaluateToString(Context, args[1]);
  u:= dp.ParseUnitString(un, f);

  if not nd.UnitCompatible(u) then
    raise EMathDimensionError.Create('Dimension of the target unit differs from current object.');
  Result:= TValueFloatDimension.Create(nv.ValueFloat, u, f, un);
end;

function TPackageDimensions.Express_2_opt(Context: IContext; args: TExprList; Options: TDynamicArguments): IExpression;
var
  optAcceptSameDimensions: boolean;
  dp: TDimensionParser;
  e: IExpression;
  nn: IValueNumber;
  nd: IDimensions;
  l: IValueList;
  ls: IValueString;
  unitNames: TStringList;
  i: integer;

  usedUnits: TDimensionsList;
  buildDim, remDim: TMathUnits;

  function ComputeExponent(const target, u: TMathUnits): Integer;
  var
    j: TMathBaseUnit;
    ibl: TIntegerBucketList;
  begin
    ibl:= TIntegerBucketList.Create;
    try
      for j:= low(j) to high(j) do
        if (u[j]<>0) then begin
          if target[j] mod u[j] = 0 then
            ibl.Push(target[j] div u[j], 1);
        end;
      Result:= ibl.GetMostCommon;      
    finally
      FreeAndNil(ibl);
    end;
  end;

  procedure ApplyWantedUnits;
  var
    i, j: integer;
    dl: TDimensionsList;
    dim: TMathUnits;
    f: Number;
    e: Integer;
  begin
    remDim:= nd.Units;
    buildDim:= MakeDimension([]);

    for i:= 0 to unitNames.Count-1 do begin
      dl:= dp.SplitDimension(unitNames[i]);
      dim:= dp.ApplyDimList(dl, f);
      e:= ComputeExponent(remDim, dim);
      for j:= 0 to high(dl) do
        dl[j].Exponent:= dl[j].Exponent * e;
      usedUnits:= dp.CombineDL(usedUnits, dl);
      buildDim:= MultDimensions(buildDim, PowerDimensions(dim, e));
      remDim:= MultDimensions(remDim, PowerDimensions(dim, -e));
    end;
  end;

  procedure RecurseFindMore(const target: TMathUnits; const Used: TDimensionsList; var BestDims: TDimensionsList; var BestPresent: boolean);
    function IsUsed(const Un: integer): boolean;
    var
      i: integer;
    begin
      Result:= true;
      if optAcceptSameDimensions then begin
        // Filter based on exact name only
        for i:= 0 to high(Used) do
          if Used[i].DimIndex = Un then
            exit;
      end else begin
        // Filter based on equivalent dimensions, i.e. m/pc/ly or g/t/lb
        for i:= 0 to high(Used) do
          if SameDimension(UnitDimTable[Used[i].DimIndex].Dim, UnitDimTable[Un].Dim) then
            exit;
      end;
      Result:= false;
    end;

    function ContributesDim(const dim: TMathUnits): Boolean;
    var
      d: TMathBaseUnit;
    begin
      // any dimensions overlap?
      Result:= true;
      for d:= low(d) to high(d) do
        if (dim[d]<>0) and (target[d]<>0) then
          exit;
      Result:= false;
    end;

  var
    u: integer;
    tDim, rDim: TMathUnits;
    tDL: TDimensionsList;
  begin
    // goal reached?
    if DimensionIsScalar(target) then begin
      if not BestPresent or (Length(BestDims) > length(Used)) then begin
        BestPresent:= True;
        BestDims:= Used;
        exit;
      end;
    end;
    // would just using base units be better?
    if Length(Used)> Ord(High(TMathBaseUnit)) - Ord(Low(TMathBaseUnit)) then
      exit;
    // can we still improve? (cutoff)
    if BestPresent and (Length(Used)> length(BestDims)) then
      exit;
    SetLength(tDL, 1);
    tDL[0].PrefixIndex:= 0;
    for u:= 1 to high(UnitDimTable) do begin
      if IsUsed(u) then
        continue;

      tDim:= UnitDimTable[u].Dim;

      if not ContributesDim(tDim) then
        continue;

      tDL[0].DimIndex:= u;
      tDL[0].Exponent:= ComputeExponent(target, tDim);

      rDim:= MultDimensions(target, PowerDimensions(tDim, -tDL[0].Exponent));
      RecurseFindMore(rDim, dp.CombineDL(Used, tDL), BestDims, BestPresent);
    end;
  end;

  procedure FindMoreUnits;
  var
    dl: TDimensionsList;
    dlSet: boolean;
    i: integer;
    d: TMathUnits;
  begin
    dlSet:= false;
    SetLength(dl, 0);
    RecurseFindMore(remDim, usedUnits, dl, dlSet);
    if dlSet then begin
      for i:= length(usedUnits) to high(dl) do begin
        d:= UnitDimTable[dl[i].DimIndex].Dim;
        buildDim:= MultDimensions(buildDim, PowerDimensions(d, dl[i].Exponent));
      end;
      usedUnits:= dl;
    end else
      raise EMathDimensionError.Create('Could not match Dimensions. Possible conflicting input?');
  end;

begin
  e:= args[0].Evaluate(Context);

  if not e.Represents(IValueNumber, nn) then
    raise EMathSysError.Create('Express requires a unit value.');

  if not e.Represents(IDimensions, nd) then
    raise EMathSysError.Create('Express requires a unit value.');

  e:= args[1].Evaluate(Context);


  optAcceptSameDimensions:= Options.IsSet('AcceptSameDimensions');

  unitNames:= TStringList.Create;
  try
    if e.Represents(IValueList, l) then begin
      for i:= 0 to l.Length-1 do begin
        e:= l.Arg[i].Evaluate(Context);
        if e.Represents(IValueString, ls) then
          unitNames.Add(ls.Value)
        else
          raise EMathDimensionError.Create('Express requires one or a list of unit strings.');
      end;
    end else
    if e.Represents(IValueString, ls) then
      unitNames.Add(ls.Value)
    else
      raise EMathDimensionError.Create('Express requires one or a list of unit strings.');

    SetLength(usedUnits, 0);

    ApplyWantedUnits;

    FindMoreUnits;

    Result:= TValueFloatDimension.Create(nn.ValueFloat, buildDim, dp.GetFactorFromList(usedUnits), dp.GetStringFromList(usedUnits, false));
  finally
    FreeAndNil(unitNames);
  end;
end;

procedure TPackageDimensions.OnImport(const MS: TMathSystem);
begin
  inherited;
  MS.RegisterAsInfix('_', 1, [],Self,'unit');
end;

initialization
  //                                                                   m, kg,  s,  K,mol,  A, cd,rad,bit
  //SI base
  DefineUnit(''       , ''                      , 1               , [  0,  0,  0,  0,  0,  0,  0]);
  DefineUnit('m'      , 'Meter'                 , 1               , [  1,  0,  0,  0,  0,  0,  0]);
  DefineUnit('g'      , 'Gram'                  , 1e-3            , [  0,  1,  0,  0,  0,  0,  0]);
  DefineUnit('s'      , 'Second'                , 1               , [  0,  0,  1,  0,  0,  0,  0]);
  DefineUnit('K'      , 'Kelvin'                , 1               , [  0,  0,  0,  1,  0,  0,  0]);
  DefineUnit('mol'    , 'Mole'                  , 1               , [  0,  0,  0,  0,  1,  0,  0]);
  DefineUnit('A'      , 'Ampere'                , 1               , [  0,  0,  0,  0,  0,  1,  0]);
  DefineUnit('Cd'     , 'Candela'               , 1               , [  0,  0,  0,  0,  0,  0,  1]);
  DefineUnit('C'      , 'Coulomb'               , 1               , [  0,  0,  1,  0,  0,  1,  0]);
  // SI Derived
  DefineUnit('F'      , 'Farad'                 , 1               , [ -2, -1,  4,  0,  0,  2,  0]);
  DefineUnit('H'      , 'Henry'                 , 1               , [  2,  1, -2,  0,  0, -2,  0]);
  DefineUnit('Hz'     , 'Hertz'                 , 1               , [  0,  0, -1,  0,  0,  0,  0]);
  DefineUnit('J'      , 'Joule'                 , 1               , [  2,  1, -2,  0,  0,  0,  0]);
  DefineUnit('N'      , 'Newton'                , 1               , [  1,  1, -2,  0,  0,  0,  0]);
  DefineUnit('Ohm'    , 'Ohm'                   , 1               , [  2,  1, -3,  0,  0, -2,  0]);
  DefineUnit('Pa'     , 'Pascal'                , 1               , [ -1,  1, -2,  0,  0,  0,  0]);
  DefineUnit('S'      , 'Siemens'               , 1               , [ -2, -1,  3,  0,  0,  2,  0]);
  DefineUnit('T'      , 'Tesla'                 , 1               , [  0,  1, -2,  0,  0, -1,  0]);
  DefineUnit('V'      , 'Volt'                  , 1               , [  2,  1, -3,  0,  0, -1,  0]);
  DefineUnit('W'      , 'Watt'                  , 1               , [  2,  1, -3,  0,  0,  0,  0]);
  DefineUnit('Wb'     , 'Weber'                 , 1               , [  2,  1, -2,  0,  0, -1,  0]);
  // Length
  DefineUnit('in'     , 'inch'                  , 0.0254          , [  1,  0,  0,  0,  0,  0,  0]);
  DefineUnit('ft'     , 'foot'                  , 0.3048          , [  1,  0,  0,  0,  0,  0,  0]);
  DefineUnit('yd'     , 'yard'                  , 0.9144          , [  1,  0,  0,  0,  0,  0,  0]);
  DefineUnit('mi'     , 'mile'                  , 1609.344        , [  1,  0,  0,  0,  0,  0,  0]);
  DefineUnit('nmi'    , 'nautic mile'           , 1852            , [  1,  0,  0,  0,  0,  0,  0]);
  DefineUnit('AU'     , 'astronomical unit'     , 149.597870691e9 , [  1,  0,  0,  0,  0,  0,  0]);
  DefineUnit('ly'     , 'light-year'            , 94607304725808e2, [  1,  0,  0,  0,  0,  0,  0]);
  DefineUnit('pc'     , 'parsec'                , 30.857e15       , [  1,  0,  0,  0,  0,  0,  0]);
  DefineUnit('Ang'    , 'Angström'              , 1E-10           , [  1,  0,  0,  0,  0,  0,  0]);
  // Arc
  DefineUnit('rad'    , 'radiant'               , 1               , [  0,  0,  0,  0,  0,  0,  0,  1]);
  DefineUnit('°'      , 'degree'                , cTau / 360      , [  0,  0,  0,  0,  0,  0,  0,  1]);
  DefineUnit('deg'    , 'degree'                , cTau / 360      , [  0,  0,  0,  0,  0,  0,  0,  1]);
  DefineUnit('gon'    , 'gradian'               , cTau / 400      , [  0,  0,  0,  0,  0,  0,  0,  1]);
  DefineUnit('sr'     , 'steradiant'            , 1               , [  0,  0,  0,  0,  0,  0,  0,  2]);
  // Area
  DefineUnit('ha'     , 'hectare'               , 1E4             , [  2,  0,  0,  0,  0,  0,  0,  0]);
  // Volume
  DefineUnit('L'      , 'litre'                 , 1e-3            , [  3,  0,  0,  0,  0,  0,  0]);
  DefineUnit('gal'    , 'gallon'                , 3.785e-3        , [  3,  0,  0,  0,  0,  0,  0]);
  DefineUnit('fl-oz'  , 'fluid ounce'           , 29.57e-6        , [  3,  0,  0,  0,  0,  0,  0]);
  // Mass
  DefineUnit('t'      , 'tonne'                 , 1e3             , [  0,  1,  0,  0,  0,  0,  0]);
  DefineUnit('oz'     , 'ounce'                 , 28.349523125e-3 , [  0,  1,  0,  0,  0,  0,  0]);
  DefineUnit('lb'     , 'pound'                 , 453.59237e-3    , [  0,  1,  0,  0,  0,  0,  0]);
  DefineUnit('amu'    , 'atomic unit'           , 1.66053886E-27  , [  0,  1,  0,  0,  0,  0,  0]);
  // Time
  DefineUnit('min'    , 'minute'                , 60              , [  0,  0,  1,  0,  0,  0,  0]);
  DefineUnit('h'      , 'hour'                  , 3600            , [  0,  0,  1,  0,  0,  0,  0]);
  DefineUnit('d'      , 'day'                   , 86400           , [  0,  0,  1,  0,  0,  0,  0]);
  DefineUnit('wk'     , 'week'                  , 604800          , [  0,  0,  1,  0,  0,  0,  0]);
  DefineUnit('a'      , 'year'                  , 31536000        , [  0,  0,  1,  0,  0,  0,  0]); // gregorian year, 365.0 days
  DefineUnit('ja'     , 'year'                  , 31557600        , [  0,  0,  1,  0,  0,  0,  0]); // julian year, 365.25 days
  // Thermodynamic Temperature
  DefineUnit('R'      , 'Rankine'               , 1.8             , [  0,  0,  0,  1,  0,  0,  0]);
  // Substance
  DefineUnit('lb-mol' , 'pound - mole'          , 453.59237       , [  0,  0,  0,  0,  1,  0,  0]);
  // Force
  DefineUnit('dyn'    , 'Dyne'                  , 1e-5            , [  1,  1, -2,  0,  0,  0,  0]);
  DefineUnit('lbf'    , 'Pound-force'           , 4.4482216152605 , [  1,  1, -2,  0,  0,  0,  0]);
  // Pressure
  DefineUnit('bar'    , 'Bar'                   , 1e5             , [ -1,  1, -2,  0,  0,  0,  0]);
  DefineUnit('atm'    , 'atmosphere'            , 1.01325e5       , [ -1,  1, -2,  0,  0,  0,  0]);
  DefineUnit('at'     , 'technical atm'         , 0.980665e5      , [ -1,  1, -2,  0,  0,  0,  0]);
  DefineUnit('Torr'   , 'Torr'                  , 133.322         , [ -1,  1, -2,  0,  0,  0,  0]);
  DefineUnit('psi'    , 'pound per sq inch'     , 6894.75729316836, [ -1,  1, -2,  0,  0,  0,  0]);
  DefineUnit('mmhg'   , 'mm mercury column'     , 133.3           , [ -1,  1, -2,  0,  0,  0,  0]);
  DefineUnit('mmwc'   , 'mm water column'       , 9.80665         , [ -1,  1, -2,  0,  0,  0,  0]);
  // Energy
  DefineUnit('cal'    , 'Calories'              , 4.1868          , [  2,  1, -2,  0,  0,  0,  0]);
  DefineUnit('btu'    , 'british thermal unit'  , 1055.056        , [  2,  1, -2,  0,  0,  0,  0]);
  DefineUnit('erg'    , 'Erg'                   , 1e-7            , [  2,  1, -2,  0,  0,  0,  0]);
  DefineUnit('eV'     , 'Electronvolt'          , 1.6021766208e-19, [  2,  1, -2,  0,  0,  0,  0]);
  // Power
  DefineUnit('hp'     , 'Horsepower'            , 745.7           , [  2,  1, -3,  0,  0,  0,  0]);
  // Photometry
  DefineUnit('lm'     , 'lumen'                 , 1               , [  0,  0,  0,  0,  0,  0,  1, -2]);
  DefineUnit('lx'     , 'lux'                   , 1               , [ -2,  0,  0,  0,  0,  0,  1, -2]);
  // Information Theory
  DefineUnit('bit'    , 'Bit'                   , 1               , [  0,  0,  0,  0,  0,  0,  0,  0,  1]);
  DefineUnit('B'      , 'Byte'                  , 8               , [  0,  0,  0,  0,  0,  0,  0,  0,  1]);
  // (Physical) Chemistry, Atomic Units
  DefineUnit('Bohr'   , 'Bohr Radius'           , 5.2917721067E-11, [  1,  0,  0,  0,  0,  0,  0]);
  DefineUnit('Bmag'   , 'Bohr megneton'         , 9.274009994E-21 , [  2,  0,  0,  0,  0,  1,  0]);
  DefineUnit('Ha'     , 'Hartree'               , 4.359744650E-18 , [  2,  1, -2,  0,  0,  0,  0]);
  DefineUnit('Ry'     , 'Rydberg'               , 2.179872325E-18 , [  2,  1, -2,  0,  0,  0,  0]);
end.

