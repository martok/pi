{-----------------------------------------------------------------------------
 Datatype support unit for dimensions and conversions between them.

 Logic, unit prefix notation and conversion tables based on MATLAB code
 originally (c) 2011-2013 Alexander Zinser
-----------------------------------------------------------------------------}
unit uMathDimensions;

interface

uses
  uMathIntf, uMathValues, uMath, Classes, Math;

type
  EMathDimensionError = class(EMathSysError);

  TValueDimension = class(TE_Atom, IValueNumber, IValueDimension, IStringConvertible, IOperationAddition, IOperationMultiplication, IOperationPower)
  private
    FSIValue: Number;
    FUnits: TMathUnits;
    FCreatedAs: String;
  public
    constructor Create(const aVal: Number; const aUnits: TMathUnits; const aCreatedAs: String = '');

    // IExpression
    function Clone(Deep: Boolean): IExpression; override;
    // IValueNumber
    function NumberValue: Number;
    function IValueNumber.Value = NumberValue;
    // IValueDimension
    function Value: Number;
    function Units: TMathUnits;
    function IsCompatible(const Units: TMathUnits): boolean;
    function IsScalar: boolean;
    // IStringConvertible
    function AsString(const Format: TStringFormat): string;
    // IOperationAddition;
    function OpAdd(const B: IExpression): IExpression;
    function OpSubtract(const B: IExpression): IExpression;
    // IOperationMultiplication
    function OpDivide(const B: IExpression): IExpression;
    function OpMultiply(const B: IExpression): IExpression;
    function OpNegate: IExpression;
    // IOperationPower
    function OpPower(const B: Number): IExpression;
    function OpRoot(const B: Number): IExpression;
  end;

  TPackageDimensions = class(TFunctionPackage)
  published
    function Unit_2(Context: IContext; args: TExprList): IExpression;
    function Convert_2(Context: IContext; args: TExprList): IExpression;
    function Express_2(Context: IContext; args: TExprList): IExpression;
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

implementation

uses
  SysUtils, StrUtils;

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
  DimPrefixTable: array[0..20] of TDefinedPrefix = (
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
    (Factor: 1e-24 ; Symbol: 'y'; Name: 'yocto')
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

{ TValueDimension }

constructor TValueDimension.Create(const aVal: Number; const aUnits: TMathUnits; const aCreatedAs: String);
begin
  inherited Create;
  FSIValue:= aVal;
  FUnits:= aUnits;
  FCreatedAs:= aCreatedAs;
end;

function TValueDimension.Clone(Deep: Boolean): IExpression;
begin
  Result:= TValueDimension.Create(FSIValue, FUnits);
end;

function TValueDimension.IsCompatible(const Units: TMathUnits): boolean;
var
  d: TMathBaseUnit;
begin
  Result:= true;
  for d:= low(d) to high(d) do
    if Units[d] <> FUnits[d] then begin
      Result:= false;
      exit;
    end;
end;

function TValueDimension.NumberValue: Number;
var
  dp: TDimensionParser;
  u: TMathUnits;
  f: Number;
begin
  u:= dp.ParseUnitString(FCreatedAs, f);

  Result:= FSIValue / f;
end;

function TValueDimension.Units: TMathUnits;
begin
  Result:= FUnits;
end;

function TValueDimension.Value: Number;
begin
  Result:= FSIValue;
end;

function TValueDimension.IsScalar: boolean;
begin
  Result:= DimensionIsScalar(FUnits);
end;

function TValueDimension.AsString(const Format: TStringFormat): string;
var
  dp: TDimensionParser;
begin
  case Format of
    STR_FORMAT_OUTPUT: begin
      Result:= (TValueNumber.Create(NumberValue) as IStringConvertible).AsString(Format);
      if FCreatedAs >'' then
         Result:= Result + ' ' + FCreatedAs
      else
         Result:= Result + ' ' + dp.GetSIString(FUnits, false);
    end;
  else
    Result:=
      (TValueNumber.Create(FSIValue) as IStringConvertible).AsString(Format) + ' ' +
      dp.GetSIString(FUnits, true);
  end;
end;

function TValueDimension.OpAdd(const B: IExpression): IExpression;
var
  nb: IValueNumber;
  ub: IValueDimension;
begin
   if b.Represents(IValueDimension, ub) then begin
      if IsCompatible(ub.Units) then
        Result:= TValueDimension.Create(FSIValue + ub.Value, FUnits)
      else
        raise EMathDimensionError.Create('Only objects of the same dimension can be added');
    end else begin
      if b.Represents(IValueNumber, nb) then begin
        if IsScalar then
          Result:= TValueNumber.Create(FSIValue + nb.Value)
        else
          raise EMathDimensionError.Create('Only objects of the same dimension can be added');
      end else
        raise EMathTypeError.CreateFmt(sCannotConvertExpression, ['Number']);
   end;
end;

function TValueDimension.OpSubtract(const B: IExpression): IExpression;
var
  nb: IValueNumber;
  ub: IValueDimension;
begin
   if b.Represents(IValueDimension, ub) then begin
      if IsCompatible(ub.Units) then
        Result:= TValueDimension.Create(FSIValue - ub.Value, FUnits)
      else
        raise EMathDimensionError.Create('Only objects of the same dimension can be subtracted');
    end else begin
      if b.Represents(IValueNumber, nb) then begin
        if IsScalar then
          Result:= TValueNumber.Create(FSIValue - nb.Value)
        else
          raise EMathDimensionError.Create('Only objects of the same dimension can be subtracted');
      end else
        raise EMathTypeError.CreateFmt(sCannotConvertExpression, ['Number']);
   end;
end;

function TValueDimension.OpDivide(const B: IExpression): IExpression;
var
  nd: IValueNumber;
  ud: IValueDimension;
begin
  if B.Represents(IValueDimension, ud) then
    Result:= TValueDimension.Create(DivideNumber(FSIValue, ud.Value), MultDimensions(FUnits, InverseDimensions(ud.Units)))
  else if B.Represents(IValueNumber, nd) then
    Result:= TValueDimension.Create(DivideNumber(FSIValue, nd.Value), FUnits)
  else
    raise EMathTypeError.CreateFmt(sCannotConvertExpression, ['Number']);
end;

function TValueDimension.OpMultiply(const B: IExpression): IExpression;
var
  nb: IValueNumber;
  ub: IValueDimension;
begin
  if b.Represents(IValueDimension, ub) then
    Result:= TValueDimension.Create(FSIValue * ub.Value, MultDimensions(FUnits, ub.Units))
  else if b.Represents(IValueNumber, nb) then
    Result:= TValueDimension.Create(FSIValue * nb.Value, FUnits)
  else
    raise EMathTypeError.CreateFmt(sCannotConvertExpression, ['Number']);
end;

function TValueDimension.OpNegate: IExpression;
begin
  Result:= TValueDimension.Create(-FSIValue, FUnits, FCreatedAs);
end;

function TValueDimension.OpPower(const B: Number): IExpression;
begin
  if IsZero(frac(b)) then
    Result:= TValueDimension.Create(IntPower(FSIValue, trunc(b)), PowerDimensions(FUnits, trunc(b)))
  else
    raise EMathDimensionError.Create('Exponent has to be a whole number');
end;

function TValueDimension.OpRoot(const B: Number): IExpression;
begin
  if IsZero(frac(B)) then
    Result:= TValueDimension.Create(Math.Power(FSIValue, 1 / B), RootDimensions(FUnits, trunc(B)))
  else
    raise EMathDimensionError.Create('Root has to be a whole number');
end;

{ TPackageDimensions }

function TPackageDimensions.Unit_2(Context: IContext; args: TExprList): IExpression;
var
  dp: TDimensionParser;
  n, f: Number;
  u: TMathUnits;
  un: string;
begin
  n:= EvaluateToNumber(Context, args[0]);
  un:= EvaluateToString(Context, args[1]);
  u:= dp.ParseUnitString(un, f);
  n:= n * f;
  Result:= TValueDimension.Create(n, u, un);
end;

function TPackageDimensions.Convert_2(Context: IContext; args: TExprList): IExpression;
var
  dp: TDimensionParser;
  nn: IExpression;
  nd: IValueDimension;
  f: Number;
  u: TMathUnits;
  un: string;
begin
  nn:= args[0].Evaluate(Context);

  un:= EvaluateToString(Context, args[1]);
  u:= dp.ParseUnitString(un, f);
  if not nn.Represents(IValueDimension, nd) then
    raise EMathSysError.Create('Convert requires a unit value.');

  if not nd.IsCompatible(u) then
    raise EMathDimensionError.Create('Dimension of the target unit differs from current object.');
  Result:= TValueDimension.Create(nd.Value, nd.Units, un);
end;

function TPackageDimensions.Express_2(Context: IContext; args: TExprList): IExpression;
var
  dp: TDimensionParser;
  e: IExpression;
  nd: IValueDimension;
  l: IValueList;
  ls: IValueString;
  unitNames: TStringList;
  i: integer;

  usedUnits: TDimensionsList;
  buildDim, remDim: TMathUnits;

  function ComputeExponent(const target, u: TMathUnits): Integer;
  var
    lowv: Integer;
    lowd, j: TMathBaseUnit;
  begin
    // find lowest power
    lowv:= 1000;
    lowd:= TMathBaseUnit(-1);
    for j:= low(j) to high(j) do
      if (u[j]<>0) and (u[j] < lowv)  then begin
        lowv:= u[j];
        lowd:= j;
      end;
    if lowv<>0 then
      Result:= target[lowd] div lowv
    else
      Result:= 1;
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
      for i:= 0 to high(Used) do
        if Used[i].DimIndex = Un then
          exit;
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

  if not e.Represents(IValueDimension, nd) then
    raise EMathSysError.Create('Convert requires a unit value.');

  e:= args[1].Evaluate(Context);

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

    Result:= TValueDimension.Create(nd.Value, buildDim, dp.GetStringFromList(usedUnits, true));
  finally
    FreeAndNil(unitNames);
  end;
end;

initialization
  //                                                                   m, kg,  s,  K,mol,  A, cd
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
  DefineUnit('AU'     , 'astronomical unit'     , 149.59787e9     , [  1,  0,  0,  0,  0,  0,  0]);
  DefineUnit('ly'     , 'light-year'            , 9.46073e15      , [  1,  0,  0,  0,  0,  0,  0]);
  DefineUnit('pc'     , 'parsec'                , 30.857e15       , [  1,  0,  0,  0,  0,  0,  0]);
  // Volume                                                       
  DefineUnit('L'      , 'litre'                 , 1e-3            , [  3,  0,  0,  0,  0,  0,  0]);
  // Mass
  DefineUnit('t'      , 'tonne'                 , 1e3             , [  0,  1,  0,  0,  0,  0,  0]);
  DefineUnit('oz'     , 'ounce'                 , 28.349523125e-3 , [  0,  1,  0,  0,  0,  0,  0]);
  DefineUnit('lb'     , 'pound'                 , 453.59237e-3    , [  0,  1,  0,  0,  0,  0,  0]);
  DefineUnit('amu'    , 'atomic unit'           , 1.660539E-27    , [  0,  1,  0,  0,  0,  0,  0]);
  // Time
  DefineUnit('min'    , 'minute'                , 60              , [  0,  0,  1,  0,  0,  0,  0]);
  DefineUnit('h'      , 'hour'                  , 3600            , [  0,  0,  1,  0,  0,  0,  0]);
  DefineUnit('d'      , 'day'                   , 86400           , [  0,  0,  1,  0,  0,  0,  0]);
  DefineUnit('wk'     , 'week'                  , 604800          , [  0,  0,  1,  0,  0,  0,  0]);
  DefineUnit('a'      , 'year'                  , 31536000        , [  0,  0,  1,  0,  0,  0,  0]);
  // Thermodynamic Temperature                                    
  DefineUnit('R'      , 'Rankine'               , 1.8             , [  0,  0,  0,  1,  0,  0,  0]);
  // Substance                                                    
  DefineUnit('lb-mol' , 'pound - mole'          , 453.59237       , [  0,  0,  0,  0,  1,  0,  0]);
  // Pressure
  DefineUnit('bar'    , 'Bar'                   , 1e5             , [ -1,  1, -2,  0,  0,  0,  0]);
  DefineUnit('atm'    , 'atmosphere'            , 1.01325e5       , [ -1,  1, -2,  0,  0,  0,  0]);
  DefineUnit('at'     , 'technical atm'         , 0.980665e5      , [ -1,  1, -2,  0,  0,  0,  0]);
  DefineUnit('Torr'   , 'Torr'                  , 133.322         , [ -1,  1, -2,  0,  0,  0,  0]);
  DefineUnit('psi'    , 'pound per sq inch'     , 6.895e3         , [ -1,  1, -2,  0,  0,  0,  0]);
  // Energy
  DefineUnit('cal'    , 'Calories'              , 4.1868          , [  2,  1, -2,  0,  0,  0,  0]);
  DefineUnit('btu'    , 'british thermal unit'  , 1055.056        , [  2,  1, -2,  0,  0,  0,  0]);
  DefineUnit('erg'    , 'Erg'                   , 1e-7            , [  2,  1, -2,  0,  0,  0,  0]);
  DefineUnit('eV'     , 'Electronvolt'          , 1.6021766e-19   , [  2,  1, -2,  0,  0,  0,  0]);     
  // Power
  DefineUnit('hp'      , 'Horsepower'           , 745.7           , [  2,  1, -3,  0,  0,  0,  0]);
end.

