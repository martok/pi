{-----------------------------------------------------------------------------
 Package: Trig
 Package: Elementary
 Package: Numerical
 Package: Lists
 Package: Data
-----------------------------------------------------------------------------}
unit uFunctions;

interface

uses SysUtils, Classes, uMathIntf, uMath, uMathValues, uFPUSupport, uMathConstants;

type
  TPackageTrig = class(TFunctionPackage)
  published
    function Sin_1(Context: IContext; args: TExprList): IExpression;
    function Cos_1(Context: IContext; args: TExprList): IExpression;
    function Tan_1(Context: IContext; args: TExprList): IExpression;
    function ArcSin_1(Context: IContext; args: TExprList): IExpression;
    function ArcCos_1(Context: IContext; args: TExprList): IExpression;
    function ArcTan_1(Context: IContext; args: TExprList): IExpression;
    function ArcTan_2(Context: IContext; args: TExprList): IExpression;
    function Sinh_1(Context: IContext; args: TExprList): IExpression;
    function Cosh_1(Context: IContext; args: TExprList): IExpression;
    function Tanh_1(Context: IContext; args: TExprList): IExpression;
    function ArSinh_1(Context: IContext; args: TExprList): IExpression;
    function ArCosh_1(Context: IContext; args: TExprList): IExpression;
    function ArTanh_1(Context: IContext; args: TExprList): IExpression;
  end;

  TPackageElementary = class(TFunctionPackage)
  private
    function LogPossible(Val: Number; var FailVal: IExpression): boolean;
  published
    function Exp_1(Context: IContext; args: TExprList): IExpression;
    function Ln_1(Context: IContext; args: TExprList): IExpression;
    function Lg_1(Context: IContext; args: TExprList): IExpression;
    function Ld_1(Context: IContext; args: TExprList): IExpression;
    function Loga_2(Context: IContext; args: TExprList): IExpression;
    function Sqrt_1(Context: IContext; args: TExprList): IExpression;
    function NRt_2(Context: IContext; args: TExprList): IExpression;
    function Random_0(Context: IContext; args: TExprList): IExpression;
  end;

  TPackageNumerical = class(TFunctionPackage)
  private
    function Base_atoi(base: integer; Param: IValueString): IExpressionAtom;
    function Base_itoa(base: integer; Param: IValueNumber): IExpressionAtom;
    function GCD(a,b: int64): int64;
  published
    function Abs_1(Context: IContext; args: TExprList): IExpression;
    function Sign_1(Context: IContext; args: TExprList): IExpression;
    function Step_1(Context: IContext; args: TExprList): IExpression;
    function Round_1(Context: IContext; args: TExprList): IExpression;
    function Round_2(Context: IContext; args: TExprList): IExpression;   
    function Trunc_1(Context: IContext; args: TExprList): IExpression;
    function Ceil_1(Context: IContext; args: TExprList): IExpression;
    function Floor_1(Context: IContext; args: TExprList): IExpression;
    function Fac_1_opt(Context: IContext; args: TExprList; Options: TDynamicArguments): IExpression;
    function Binomial_2(Context: IContext; args: TExprList): IExpression;
    function Permutations_2(Context: IContext; args: TExprList): IExpression;

    function AtoI_2(Context: IContext; args: TExprList): IExpression;
    function ItoA_2(Context: IContext; args: TExprList): IExpression;

    function h_1(Context: IContext; args: TExprList): IExpression;
    function b_1(Context: IContext; args: TExprList): IExpression;
    function o_1(Context: IContext; args: TExprList): IExpression;
    function exthex_1(Context: IContext; args: TExprList): IExpression;

    function GCD_2(Context: IContext; args: TExprList): IExpression;
    function LCM_2(Context: IContext; args: TExprList): IExpression;
    function Fraction_1_opt(Context: IContext; args: TExprList; Options: TDynamicArguments): IExpression;

    function ND_3_opt(Context: IContext; args: TExprList; Options: TDynamicArguments): IExpression;
  end;

  TPackageLists = class(TFunctionPackage)
  protected
    procedure OnImport(const MS: TMathSystem); override;
  published
    function L_N(Context: IContext; args: TExprList): IExpression;
    function Range_3(Context: IContext; args: TExprList): IExpression;
    function Each_3(Context: IContext; args: TExprList): IExpression;
    function HoldList_1(Context: IContext; args: TExprList): IExpression;
    function Flatten_1(Context: IContext; args: TExprList): IExpression;
    function Aggregate_5(Context: IContext; args: TExprList): IExpression;
    function Merge_2(Context: IContext; args: TExprList): IExpression;
    function Splice_2(Context: IContext; args: TExprList): IExpression;
    function Part_3(Context: IContext; args: TExprList): IExpression;
    function LGet_2(Context: IContext; args: TExprList): IExpression;
    function Count_1(Context: IContext; args: TExprList): IExpression;
  end;

  TPackageData = class(TFunctionPackage)
  published
    function PWD_0(Context: IContext; args: TExprList): IExpression;
    function CWD_1(Context: IContext; args: TExprList): IExpression;
    function Glob_1(Context: IContext; args: TExprList): IExpression;
    function CSVLoad_1_opt(Context: IContext; args: TExprList; Options: TDynamicArguments): IExpression;
    function Source_1(Context: IContext; args: TExprList): IExpression;
    function Table_1(Context: IContext; args: TExprList): IExpression;
    function Bucket_4(Context: IContext; args: TExprList): IExpression;
    function Min_1(Context: IContext; args: TExprList): IExpression;
    function Max_1(Context: IContext; args: TExprList): IExpression;
    function SortBy_3_opt(Context: IContext; args: TExprList; Options: TDynamicArguments): IExpression;
    function NumDerive_1(Context: IContext; args: TExprList): IExpression;
  end;

implementation

uses Math, uCCSVList, uMathDimensions;

const
  UNITS_RAD: TMathUnits = (0, 0, 0, 0, 0, 0, 0, 1, 0);
  sTrigFunctionAngle = '%s needs argument as quantity in Rad or as scalar';

{ TPackageTrig }

function TPackageTrig.Sin_1(Context: IContext; args: TExprList): IExpression;
var
  a: IExpression;
  v: IValueNumber;
begin
  a:= args[0].Evaluate(Context);
  if not a.Represents(IValueNumber, v) then
    raise EMathTypeError.CreateFmt(sTrigFunctionAngle, ['Sin']);
  if not v.Represents(IDimensions) then
    v:= MakeQuantity(v, 'rad');
  if (v as IDimensions).UnitCompatible(UNITS_RAD) then
    Result:= TValueFactory.Float(Sin(v.ValueFloat))
  else
    raise EMathTypeError.CreateFmt(sTrigFunctionAngle, ['Sin']);
end;

function TPackageTrig.Cos_1(Context: IContext; args: TExprList): IExpression;
var
  a: IExpression;
  v: IValueNumber;
begin
  a:= args[0].Evaluate(Context);
  if not a.Represents(IValueNumber, v) then
    raise EMathTypeError.CreateFmt(sTrigFunctionAngle, ['Cos']);
  if not v.Represents(IDimensions) then
    v:= MakeQuantity(v, 'rad');
  if (v as IDimensions).UnitCompatible(UNITS_RAD) then
    Result:= TValueFactory.Float(Cos(v.ValueFloat))
  else
    raise EMathTypeError.CreateFmt(sTrigFunctionAngle, ['Cos']);
end;

function TPackageTrig.Tan_1(Context: IContext; args: TExprList): IExpression;
var
  a: IExpression;
  v: IValueNumber;
begin
  a:= args[0].Evaluate(Context);
  if not a.Represents(IValueNumber, v) then
    raise EMathTypeError.CreateFmt(sTrigFunctionAngle, ['Tan']);
  if not v.Represents(IDimensions) then
    v:= MakeQuantity(v, 'rad');
  if (v as IDimensions).UnitCompatible(UNITS_RAD) then
    Result:= TValueFactory.Float(Tan(v.ValueFloat))
  else
    raise EMathTypeError.CreateFmt(sTrigFunctionAngle, ['Tan']);
end;

function TPackageTrig.ArcSin_1(Context: IContext; args: TExprList): IExpression;
begin
  Result:= TValueFactory.DimFloat(ArcSin(EvaluateToFloat(Context, args[0])), UNITS_RAD);
end;

function TPackageTrig.ArcCos_1(Context: IContext; args: TExprList): IExpression;
begin
  Result:= TValueFactory.DimFloat(ArcCos(EvaluateToFloat(Context, args[0])), UNITS_RAD);
end;

function TPackageTrig.ArcTan_1(Context: IContext; args: TExprList): IExpression;
begin
  Result:= TValueFactory.DimFloat(ArcTan(EvaluateToFloat(Context, args[0])), UNITS_RAD);
end;

function TPackageTrig.ArcTan_2(Context: IContext; args: TExprList): IExpression;
begin
  Result:= TValueFactory.DimFloat(ArcTan2(EvaluateToFloat(Context, args[0]), EvaluateToFloat(Context, args[1])), UNITS_RAD);
end;

function TPackageTrig.Sinh_1(Context: IContext; args: TExprList): IExpression;
begin
  Result:= TValueFactory.Float(fSinh(EvaluateToFloat(Context, args[0])));
end;

function TPackageTrig.Cosh_1(Context: IContext; args: TExprList): IExpression;
begin
  Result:= TValueFactory.Float(Cosh(EvaluateToFloat(Context, args[0])));
end;

function TPackageTrig.Tanh_1(Context: IContext; args: TExprList): IExpression;
begin
  Result:= TValueFactory.Float(fTanh(EvaluateToFloat(Context, args[0])));
end;

function TPackageTrig.ArSinh_1(Context: IContext; args: TExprList): IExpression;
begin
  Result:= TValueFactory.Float(ArcSinh(EvaluateToFloat(Context, args[0])));
end;

function TPackageTrig.ArCosh_1(Context: IContext; args: TExprList): IExpression;
begin
  Result:= TValueFactory.Float(ArcCosh(EvaluateToFloat(Context, args[0])));
end;

function TPackageTrig.ArTanh_1(Context: IContext; args: TExprList): IExpression;
begin
  Result:= TValueFactory.Float(ArcTanh(EvaluateToFloat(Context, args[0])));
end;

{ TPackageElementary }

function TPackageElementary.Exp_1(Context: IContext; args: TExprList): IExpression;
begin
  Result:= TValueFactory.Float(System.Exp(EvaluateToFloat(Context, args[0])));
end;

function TPackageElementary.LogPossible(Val: Number; var FailVal: IExpression): boolean;
begin
  Result:= false;
  if fzero(Val) then
    FailVal:= TValueFactory.Float(NegInfinity)
  else if Val < 0 then
    FailVal:= TValueFactory.Float(NaN)
  else
    Result:= true;
end;

function TPackageElementary.Ln_1(Context: IContext; args: TExprList): IExpression;
var
  a: Number;
begin
  a:= EvaluateToFloat(Context, args[0]);
  if LogPossible(a, Result) then
    Result:= TValueFactory.Float(System.Ln(a));
end;

function TPackageElementary.Lg_1(Context: IContext; args: TExprList): IExpression;
var
  a: Number;
begin
  a:= EvaluateToFloat(Context, args[0]);
  if LogPossible(a, Result) then
    Result:= TValueFactory.Float(Math.Log10(a));
end;

function TPackageElementary.Ld_1(Context: IContext; args: TExprList): IExpression;
var
  a: Number;
begin
  a:= EvaluateToFloat(Context, args[0]);
  if LogPossible(a, Result) then
    Result:= TValueFactory.Float(Math.Log2(a));
end;

function TPackageElementary.Loga_2(Context: IContext; args: TExprList): IExpression;
var
  b, a: Number;
begin
  b:= EvaluateToFloat(Context, args[0]);
  a:= EvaluateToFloat(Context, args[1]);
  if LogPossible(a, Result) then
    Result:= TValueFactory.Float(Math.LogN(b, a));
end;

function TPackageElementary.Sqrt_1(Context: IContext; args: TExprList): IExpression;
begin
  Result:= NRt_2(Context, MakeArgs([TValueFactory.Float(2), args[0]]));
end;

function TPackageElementary.NRt_2(Context: IContext; args: TExprList): IExpression;
var
  a,b: IExpression;
  op: IOperationPower;
begin
  b:= args[0].Evaluate(Context);
  a:= args[1].Evaluate(Context);

  Result:= nil;
  if a.Represents(IOperationPower, op) then
    Result:= op.OpRoot(b);
  if not Assigned(Result) then
    raise EMathSysError.CreateFmt(sUnsupportedOperation, ['Root']);
end;

function TPackageElementary.Random_0(Context: IContext; args: TExprList): IExpression;
begin
  Result:= TValueFactory.Float(Random);
end;

{ TPackageNumerical }

function TPackageNumerical.Abs_1(Context: IContext; args: TExprList): IExpression;
begin
  Result:= TValueFactory.Float(System.Abs(EvaluateToFloat(Context, args[0])));
end;

function TPackageNumerical.Sign_1(Context: IContext; args: TExprList): IExpression;
var
  n, m: Number;
begin
  n:= EvaluateToFloat(Context, args[0]);
  if n>0 then
    m:= 1.0
  else if n<0 then
    m:= -1.0
  else
    m:= 0.0;
  Result:= TValueFactory.Float(m);
end;

function TPackageNumerical.Step_1(Context: IContext; args: TExprList): IExpression;
var
  n, m: Number;
begin
  n:= EvaluateToFloat(Context, args[0]);
  if fzero(n) or (n>0) then
    m:= 1.0
  else
    m:= 0;
  Result:= TValueFactory.Float(m);
end;

function TPackageNumerical.Round_1(Context: IContext; args: TExprList): IExpression;
var
  v: Number;
begin
  v:= EvaluateToFloat(Context, args[0]);
  v:= Round(v);
  Result:= TValueFactory.Float(v);
end;

function TPackageNumerical.Round_2(Context: IContext; args: TExprList): IExpression;
var
  pot: integer;
  f,v: Number;
begin
  v:= EvaluateToFloat(Context, args[0]);
  pot:= trunc(EvaluateToFloat(Context, args[1]));
  f:= IntPower(10, pot);
  v:= Round(v / f) * f;
  Result:= TValueFactory.Float(v);
end;

function TPackageNumerical.Trunc_1(Context: IContext; args: TExprList): IExpression;
begin
  Result:= TValueFactory.Float(trunc(EvaluateToFloat(Context, args[0])));
end;

function TPackageNumerical.Ceil_1(Context: IContext; args: TExprList): IExpression;
begin
  Result:= TValueFactory.Float(Ceil(EvaluateToFloat(Context, args[0])));
end;

function TPackageNumerical.Floor_1(Context: IContext; args: TExprList): IExpression;
begin
  Result:= TValueFactory.Float(Floor(EvaluateToFloat(Context, args[0])));
end;


function TPackageNumerical.Fac_1_opt(Context: IContext; args: TExprList; Options: TDynamicArguments): IExpression;
const
  SQRT_2PI: Number = 2.506628274631000502415765;   // sqrt(2*Pi)
var
  accu, p: Number;
  desiredFact: Int64;
begin
  p:= EvaluateToFloat(Context, args[0]);
  if ftruncable(p) and not Options.IsSet('Approximate') then begin
    accu:= 1;
    desiredFact:= trunc(p);
    while desiredFact >= 2 do begin
      accu:= accu * desiredFact;
      Dec(desiredFact);
    end;
  end else begin
    accu:= SQRT_2PI * fpower(p, 0.5) * fPower(p/cE, p);
    Context.Output.Hint('Fac: Using Stirling''s approximation.',[]);
  end;
  Result:= TValueFactory.Float(accu);
end;

function bin(n,k: int64): int64;
  { binomial(n,k) = n!/k!/(n-k)! = n*(n-1).../k!}
var
  i, b: int64;
begin
  if k = 0 then
    Result:= 0
  else
    if 2 * k > n then
      Result:= bin(n, n-k)
    else begin
      b := n;
      i:= 2;
      while (i<=k) do begin
        b:= b * (n+1-i) div i;
        inc(i);
      end;
      Result:= b;
    end;
end;

function TPackageNumerical.Binomial_2(Context: IContext; args: TExprList): IExpression;
var
  n,k,p: Int64;
begin
  n:= trunc(EvaluateToFloat(Context, args[0]));
  k:= trunc(EvaluateToFloat(Context, args[1]));
  p:= bin(n,k);
  Result:= TValueFactory.Float(p);
end;

function TPackageNumerical.Permutations_2(Context: IContext; args: TExprList): IExpression;
var
  n,k,p: Int64;
begin
  n:= trunc(EvaluateToFloat(Context, args[0]));
  k:= trunc(EvaluateToFloat(Context, args[1]));
  p:= bin(n,k);
  while k >= 2 do begin
    p:= p * k;
    Dec(k);
  end;
  Result:= TValueFactory.Float(p);
end;

const
  BaseString = '0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZ';

function TPackageNumerical.Base_atoi(base: integer; Param: IValueString): IExpressionAtom;
var
  i, j: integer;
  v: MTInteger;
  n: string;
begin
  n:= Param.Value;
  n:= UpperCase(n);
  if base > Length(BaseString) then
    raise EMathSysError.CreateFmt('Base %d exceeds maximum allowed value of %d', [base, Length(BaseString)]);
  v:= 0;
  for i:= 1 to Length(n) do begin
    j:= Pos(n[i], BaseString) - 1;
    if j >= base then
      raise EMathSysError.CreateFmt('Invalid numeral in base %d: ''%s''', [base, n[i]]);
    v:= v * base + j;
  end;
  Result:= TValueFactory.Integer(v);
end;

function TPackageNumerical.Base_itoa(base: integer; Param: IValueNumber): IExpressionAtom;
var
  i: integer;
  n: string;
  v: MTInteger;
begin
  v:= Param.ValueInt;

  if v = 0 then begin
    Result:= TValueString.Create('0');
    exit;
  end;

  if base > Length(BaseString) then
    raise EMathSysError.CreateFmt('Base %d exceeds maximum allowed value of %d', [base, Length(BaseString)]);

  n:= '';

  while v <> 0 do begin
    i:= v mod base;
    v:= v div base;
    n:= BaseString[i + 1] + n;
  end;
  Result:= TValueString.Create(n);
end;

function TPackageNumerical.AtoI_2(Context: IContext; args: TExprList): IExpression;
var
  base: integer;
  st: IValueString;
begin
  base:= Trunc(EvaluateToFloat(Context, args[1]));
  if args[0].Evaluate(Context).Represents(IValueString, st) then
    Result:= Base_atoi(base, st) as IExpression
  else
    raise EMathTypeError.Create('AtoI: Input needs to be a string value.');
end;

function TPackageNumerical.ItoA_2(Context: IContext; args: TExprList): IExpression;
var
  base: integer;
  st: IValueNumber;
begin
  base:= Trunc(EvaluateToFloat(Context, args[1]));
  if args[0].Evaluate(Context).Represents(IValueNumber, st) then
    Result:= Base_itoa(base, st) as IExpression
  else
    raise EMathTypeError.Create('ItoA: Input needs to be a string value.');
end;

function TPackageNumerical.b_1(Context: IContext; args: TExprList): IExpression;
var
  v: IExpression;
  s: IValueString;
  n: IValueNumber;
begin
  v:= args[0].Evaluate(Context);
  if v.Represents(IValueString, s) then
    Result:= Base_atoi(2, s) as IExpression
  else if v.Represents(IValueNumber, n) then
    Result:= Base_itoa(2, n) as IExpression
  else
    Result:= v;
end;

function TPackageNumerical.h_1(Context: IContext; args: TExprList): IExpression;
var
  v: IExpression;
  s: IValueString;
  n: IValueNumber;
begin
  v:= args[0].Evaluate(Context);
  if v.Represents(IValueString, s) then
    Result:= Base_atoi(16, s) as IExpression
  else if v.Represents(IValueNumber, n) then
    Result:= Base_itoa(16, n) as IExpression
  else
    Result:= v;
end;

function TPackageNumerical.o_1(Context: IContext; args: TExprList): IExpression;
var
  v: IExpression;
  s: IValueString;
  n: IValueNumber;
begin
  v:= args[0].Evaluate(Context);
  if v.Represents(IValueString, s) then
    Result:= Base_atoi(8, s) as IExpression
  else if v.Represents(IValueNumber, n) then
    Result:= Base_itoa(8, n) as IExpression
  else
    Result:= v;
end;

function TPackageNumerical.exthex_1(Context: IContext; args: TExprList): IExpression;
var
  a: TExtRec;
var
  v: IExpression;
  s: IValueString;
  n: IValueNumber;
begin
  v:= args[0].Evaluate(Context);
  if v.Represents(IValueString, s) then  begin
    a.xp:= StrToInt('$' + copy(s.Value, 1, 4));
    a.m:= StrToInt64('$' + copy(s.Value, 5, 16));
    Result:= TValueFactory.Float(a.e);
  end else if v.Represents(IValueNumber, n) then begin
    a.e:= n.ValueFloat;
    Result:= TValueString.Create(IntToHex(a.xp,4) + IntToHex(a.m,16))
  end
  else
    Result:= v;
end;

function TPackageNumerical.GCD(a, b: int64): int64;
// Euclid's Algorithm
var
  r: Int64;
begin
  if b<>0 then
    repeat
      r:= a mod b;
      a:= b;
      b:= r;
    until r=0;
  Result:= a;
end;

function TPackageNumerical.GCD_2(Context: IContext; args: TExprList): IExpression;
var
  a,b: MTInteger;
begin
  a:= EvaluateToInteger(Context, args[0]);
  b:= EvaluateToInteger(Context, args[1]);

  Result:= TValueFactory.Float(GCD(a,b));
end;

function TPackageNumerical.LCM_2(Context: IContext; args: TExprList): IExpression;
var
  a,b,g: MTInteger;
begin
  a:= EvaluateToInteger(Context, args[0]);
  b:= EvaluateToInteger(Context, args[1]);

  g:= gcd(a,b);
  if g<>0 then
    Result:= TValueFactory.Integer(a div g * b)
  else
    Result:= TValueFactory.Zero;
end;

function TPackageNumerical.Fraction_1_opt(Context: IContext; args: TExprList; Options: TDynamicArguments): IExpression;
var
  prec: Number;
  mixed: boolean;
var
  x, p, lastp, q, lastq, ptemp, qtemp, u, err, d: Number;
  nom,den,w: int64;
begin
  prec:= EvaluateToFloat(Context, Options.GetDefault('precision', TValueFactory.Float(1E-11))) / 100;
  mixed:= Options.IsSet('mixed');

  x:= EvaluateToFloat(Context, Args[0]);

  // Initialisierung
  p := 1;
  q := 0;
  lastp := 0;
  lastq := 1;
  u := x;

  repeat
    // Einen ganzzahligen Anteil abspalten
    d := round(u);
    u := u - d;

    // Update von p und q: Kettenbruch (siehe unten) nachführen. Es gilt: p/q ~= x
    ptemp := p*d+lastp;
    qtemp := q*d+lastq;
    lastp := p;
    lastq := q;
    p := ptemp;
    q := qtemp;

    // Approximationsfehler relativ
    err := abs((p/q-x) / x);

    // Abbruchkriterien
    if (u=0) or (err<prec) or (x+err/4=x {sic!}) then  // (*) numerisches Ende abfangen
     break;

    // Bruch umkehren
    u := 1/u;
  until false;

  // Vor Integerkonversion auf Bereich überprüfen
  if (p>high(Int64)) or (q>high(Int64)) or
     (p<low(Int64)) or (p<low(Int64)) then
    raise EIntOverflow.Create('Fraction: Integer conversion overflow.');

  // Vorzeichen von Nenner zum Zähler
  if q < 0 then
    nom := -Trunc(p)
  else
    nom := Trunc(p);
  den := abs(Trunc(q));

  if mixed then begin
    w:= nom div den;
    if w<>0 then
      nom:= abs(nom);
    nom:= nom mod den;
    Result:= TValueList.CreateAs([TValueFactory.Float(w),TValueFactory.Float(Nom),TValueFactory.Float(den)])
  end else
    Result:= TValueList.CreateAs([TValueFactory.Float(Nom),TValueFactory.Float(den)]);
end;


function TPackageNumerical.ND_3_opt(Context: IContext; args: TExprList; Options: TDynamicArguments): IExpression;
var
  f: IExpression;
  ev: ISymbolReference;
  v: String;                        
  opt_method: String;
  c: IContext;
  t,h,dr: Number;
  r: array of array[0..1] of Number;

  procedure Compute;
  var
    i: integer;
  begin
    for i:= 0 to high(r) do begin
      c.Define(v, TValueFactory.Float(r[i, 0]));
      r[i, 1]:= EvaluateToFloat(c, f);
    end;
  end;

  function Method_Direct: Number;
  begin
    SetLength(r, 2);
    r[0,0]:= t - h;
    r[1,0]:= t + h;

    Compute;

    Result:= (r[1,1] - r[0,1]) / (r[1,0] - r[0,0]);
  end;

  function Method_FivePointStencil: Number;
  begin
    SetLength(r, 4);
    r[0,0]:= t - 2*h;
    r[1,0]:= t - h;
    r[2,0]:= t + h;
    r[3,0]:= t + 2*h;

    Compute;

    Result:= 1 / (12 * h) * (r[0,1] - 8*r[1,1] + 8*r[2,1] - r[3,1]);
  end;

  function Method_SixthOrder: Number;
  begin
    SetLength(r, 6);
    r[0,0]:= t - 3*h;
    r[1,0]:= t - 2*h;
    r[2,0]:= t - h;
    r[3,0]:= t + h;
    r[4,0]:= t + 2*h;  
    r[5,0]:= t + 3*h;
                        
    Compute;

    Result:= 1 / (60 * h) * (-r[0,1] + 9*r[1,1] - 45*r[2,1] + 45*r[3,1] - 9*r[4,1] + r[5,1]);
  end;

begin
  f:= args[0];
  if not args[1].Represents(ISymbolReference, ev) then
    raise EMathSysError.Create('Function ND requires a variable reference');
  v:= ev.Name;
  t:= EvaluateToFloat(Context, args[2]);
  opt_method:= LowerCase(EvaluateToString(Context,Options.GetDefault('Method', TValueString.Create('Direct'))));

  h:= Max(sqrt(FPU_NUMBER_EPSILON * abs(t)), FPU_SUCC0*10);
  c:= TContext.Create(Context);
  c.SetSilent(true);

  if opt_method='direct' then
    dr:= Method_Direct
  else
  if opt_method='fivepointstencil' then
    dr:= Method_FivePointStencil
  else
  if opt_method='sixthorder' then
    dr:= Method_SixthOrder
  else
    raise EMathSysError.CreateFmt('ND: unknown method specified: ''%s''',[opt_method]);

  Result:= TValueFactory.Float(dr);
end;


{ TPackageLists }

function TPackageLists.L_N(Context: IContext; args: TExprList): IExpression;
var
  i: integer;
  ls: IValueList;
begin
  ls:= TValueList.Create;
  ls.Length:= length(args);
  for i:= 0 to ls.Length - 1 do
    ls.Item[i]:= args[i].Evaluate(Context);
  ls.QueryInterface(IExpression, Result);
end;

function TPackageLists.Range_3(Context: IContext; args: TExprList): IExpression;
var
  a, max, st: Number;
begin
  a:= EvaluateToFloat(Context, args[0]);
  max:= EvaluateToFloat(Context, args[1]);
  st:= EvaluateToFloat(Context, args[2]);
  Result:= TValueRange.Create(a, st, max);
end;

function TPackageLists.Each_3(Context: IContext; args: TExprList): IExpression;
var
  list: IValueList;
  v: ISymbolReference;
  ex: IExpression;
  i: integer;
  n: string;
  ctx: IContext;
  res: IValueList;
begin
  if not args[0].Evaluate(Context).Represents(IValueList, list) then
    raise EMathSysError.Create('Function Each requires a list to work on');
  if not args[1].Represents(ISymbolReference, v) then
    raise EMathSysError.Create('Function Each requires a variable reference');
  ex:= args[2];

  ctx:= TContext.Create(Context);
  try
    ctx.SetSilent(true);
    res:= TValueList.Create;
    res.Length:= list.Length;
    n:= v.Name;
    for i:= 0 to list.length - 1 do begin
      ctx.Define(n, list.Item[i]);
      res.Item[i]:= ex.Evaluate(ctx);
    end;
    Result:= res as IExpression;
  finally
    ctx:= nil;
  end;
end;

function TPackageLists.HoldList_1(Context: IContext; args: TExprList): IExpression;
var
  list: IValueList;
  i: integer;
  res: IValueList;
begin
  if not Supports(args[0].Evaluate(Context), IValueList, list) then
    raise EMathSysError.Create('Function Hold requires a list to work on');

  res:= TValueList.Create;
  res.Length:= list.Length;
  for i:= 0 to list.length - 1 do
    res.Item[i]:= list.Item[i];
  Result:= res as IExpression;
end;

function TPackageLists.Flatten_1(Context: IContext; args: TExprList): IExpression;
var
  list, it: IValueList;
  i, j: integer;
  res: IValueList;
begin
  if not Supports(args[0].Evaluate(Context), IValueList, list) then
    raise EMathSysError.Create('Function Flatten requires a list to work on');

  res:= TValueList.Create;
  res.Length:= 0;
  for i:= 0 to list.length - 1 do begin
    if Supports(list.Item[i], IValueList, it) then begin
      for j:= 0 to it.Length-1 do begin
        res.Length:= res.Length+1;
        res.Item[res.Length-1]:= it.Item[j];
      end;
    end else begin
      res.Length:= res.Length+1;
      res.Item[res.Length-1]:= list.Item[i];
    end;
  end;
  Result:= res as IExpression;
end;

function TPackageLists.Aggregate_5(Context: IContext; args: TExprList): IExpression;
var
  list: IValueList;
  agg: ISymbolReference;                                    //TE_ExprRef
  init: IExpression;
  v: ISymbolReference;                                      //TE_ExprRef
  ex: IExpression;
  i: integer;
  ctx: IContext;
begin
  if not Supports(args[0].Evaluate(Context), IValueList, list) then
    raise EMathSysError.Create('Function Aggregate requires a list to work on');
  if not args[1].Represents(ISymbolReference, agg) then
    raise EMathSysError.Create('Function Aggregate requires a variable reference as aggregate');
  init:= args[2];
  if not args[3].Represents(ISymbolReference, v) then
    raise EMathSysError.Create('Function Aggregate requires a variable reference');
  ex:= args[4];

  ctx:= TContext.Create(Context);
  try
    ctx.SetSilent(true);
    ctx.Define(TE_SymbolRef(agg.NativeObject).Name, init.Evaluate(ctx));

    for i:= 0 to list.length - 1 do begin
      ctx.Define(TE_SymbolRef(v.NativeObject).Name, list.Item[i]);
      ctx.Define(TE_SymbolRef(agg.NativeObject).Name, ex.Evaluate(ctx));
    end;
    Result:= ctx.Definition(TE_SymbolRef(agg.NativeObject).Name).Evaluate(Context);
  finally
    ctx:= nil;
  end;
end;

function TPackageLists.Merge_2(Context: IContext; args: TExprList): IExpression;
var
  i: integer;
  a, b: IValueList;
  res: IValueList;
begin
  if not args[0].Evaluate(Context).Represents(IValueList, a) or
    not args[1].Evaluate(Context).Represents(IValueList, b) then
    raise EMathSysError.Create('Merge requires 2 lists.');
  res:= TValueList.Create;
  res.Length:= a.Length + b.Length;
  for i:= 0 to a.Length - 1 do
    res.Item[i]:= a.Item[i];
  for i:= 0 to b.Length - 1 do
    res.Item[a.Length + i]:= b.Item[i];
  Result:= res as IExpression;
end;

function TPackageLists.Splice_2(Context: IContext; args: TExprList): IExpression;
var
  i: integer;
  a, b: IValueList;
  res: IValueList;
begin
  if not args[0].Evaluate(Context).Represents(IValueList, a) or
    not args[1].Evaluate(Context).Represents(IValueList, b) then
    raise EMathSysError.Create('Splice requires 2 lists.');

  if a.Length <> b.Length then
    raise EMathSysError.Create('Splice requires 2 lists of equal length');
  res:= TValueList.Create;
  res.Length:= a.Length;
  for i:= 0 to a.Length - 1 do begin
    res.Item[i]:= TValueList.CreateAs([a.Item[i], b.Item[i]]);
  end;
  Result:= res as IExpression;
end;

function TPackageLists.Part_3(Context: IContext; args: TExprList): IExpression;
var
  i, f, l: integer;
  a: IValueList;
  res: IValueList;
begin
  if not Supports(args[0].Evaluate(Context), IValueList, a) then
    raise EMathSysError.Create('Part requires a list.');
  f:= EvaluateToInteger(Context, args[1]);
  l:= EvaluateToInteger(Context, args[2]);
  if f >= a.Length then
    f:= a.Length - 1;
  if l >= a.Length then
    l:= a.Length - 1;
  if f < 0 then
    f:= a.Length + f;
  if f < 0 then
    f:= 0;
  if l < 0 then
    l:= a.Length + l;
  if l < 0 then
    l:= 0;
  res:= TValueList.Create;
  res.Length:= l - f + 1;
  for i:= f to l do
    res.Item[i - f]:= a.Item[i];
  Result:= res as IExpression;
end;

function TPackageLists.LGet_2(Context: IContext; args: TExprList): IExpression;
var
  a: IValueList;
  f: integer;
begin
  if not args[0].Evaluate(Context).Represents(IValueList, a) then
    raise EMathSysError.Create('LGet requires a list.');
  f:= EvaluateToInteger(Context, args[1]);
  if f < 0 then
    f:= a.Length + f;
  if f < 0 then
    f:= 0;
  if (f >= a.Length) then
    Result:= TValueUnassigned.Create
  else
    Result:= a.Item[f];
end;

function TPackageLists.Count_1(Context: IContext; args: TExprList): IExpression;
var
  a: IValueList;
begin
  if not Supports(args[0].Evaluate(Context), IValueList, a) then
    raise EMathSysError.Create('Count requires a list.');
  Result:= TValueFactory.Integer(a.Length);
end;

procedure TPackageLists.OnImport(const MS: TMathSystem);
begin
  MS.RegisterAsInfix('@', 18,[],Self,'lget');
end;

{ TPackageData }

function TPackageData.CWD_1(Context: IContext; args: TExprList): IExpression;
begin
  SetCurrentDir(EvaluateToString(Context, args[0]));
end;

function TPackageData.PWD_0(Context: IContext; args: TExprList): IExpression;
begin
  Result:= TValueString.Create(GetCurrentDir);
end;  

function TPackageData.Glob_1(Context: IContext; args: TExprList): IExpression;
var
  filter: string;
  sr: TSearchRec;
  ls: TStringList;
  lst: IValueList;
  i: integer;
begin
  filter:= EvaluateToString(Context, args[0]);
  ls:= TStringList.Create;
  try
    if FindFirst(filter, faAnyFile, sr) = 0 then begin
      repeat
        ls.Add(sr.Name);
      until FindNext(sr) <> 0;
      FindClose(sr);
    end;
    lst:= TValueList.Create;
    lst.Length:= ls.Count;
    for i:= 0 to ls.Count-1 do
      lst.Item[i]:= TValueString.Create(ls[i]);
    Result:= lst;
  finally
    FreeAndNil(ls);
  end;
end;

function TPackageData.CSVLoad_1_opt(Context: IContext; args: TExprList; Options: TDynamicArguments): IExpression;
var
  list, line: TCSVStringList;
  d: string;
  i, j, k, first, last: integer;
  res, row: IValueList;
  cn: IExpression;
  o_fs: TFormatSettings;
  skip, count: integer;
  ignempt: boolean;
begin
  list:= TCSVStringList.Create;
  line:= TCSVStringList.Create;
  o_fs:= NeutralFormatSettings;
  try
    if Options.IsSet('Delimiter') then begin
      d:= CastToString(Options['Delimiter']);
      if d > '' then
        line.Delimiter:= d[1];
    end else
      line.Delimiter:= ';';
    if Options.IsSet('QuoteChar') then begin
      d:= CastToString(Options['QuoteChar']);
      if d > '' then
        line.QuoteChar:= d[1];
    end else
      line.QuoteChar:= '"';
    if Options.IsSet('Decimal') then begin
      d:= CastToString(Options['Decimal']);
      if d > '' then
        NeutralFormatSettings.DecimalSeparator:= d[1];
    end;

    try
      list.LoadFromFile(EvaluateToString(Context, args[0]));
    except
      on E: Exception do
        Context.Output.Error('CSVLoad: %s: %s', [E.ClassName, E.Message]);
    end;

    if Options.IsSet('Skip') then
      skip:= CastToInteger(Options['Skip'])
    else
      skip:= 0;

    ignempt:= Options.IsSet('IgnoreEmpty');

    first:= skip;
    if Options.IsSet('Count') then begin
      count:= CastToInteger(Options['Count']);
      last:= first + count - 1;
      if last>List.Count-1 then
        last:= List.Count-1;
    end else
      last:= List.Count - 1;

    if last < 0 then
      last:= 0;

    res:= TValueList.Create;
    res.Length:= last - first + 1;
    for i:= first to last do begin
      line.StrictDelimitedText:= list[i];
      row:= TValueList.Create;
      k:= 0;
      for j:= 0 to line.Count - 1 do begin
        if (Line[j] = '') and ignempt then
          continue;
        cn:= TValueFactory.FromString(Line[j], NeutralFormatSettings);
        row.Length:= k + 1;
        row.Item[k]:= cn;
        inc(k);
      end;
      res.Item[i - first]:= row as IExpression;
    end;

    Result:= res as IExpression;
  finally
    NeutralFormatSettings:= o_fs;
    FreeAndNil(list);
    FreeAndNil(line);
  end;
end;

function TPackageData.Source_1(Context: IContext; args: TExprList): IExpression;
var
  list: TStringList;
  i: integer;
  s, expr: string;
  inPara: boolean;
  sys: TMathSystem;

  procedure RunExp(s: string);   
  var
    X: IExpression;
    sc: IStringConvertible;
  begin
    x:= sys.Parse(s);
    if Assigned(x) then begin
      x:= x.Evaluate(Context);
      if Assigned(x) then begin
        Result:= X;
        Context.Define('ans', x);
        if not Context.Silent and x.Represents(IStringConvertible, sc) then
          Context.Output.Result(sc.AsString(STR_FORMAT_OUTPUT));
      end;
    end;
  end;

begin
  list:= TStringList.Create;
  try
    s:= EvaluateToString(Context, args[0]);
    if not FileExists(s) and FileExists(s+'.pi') then
      s:= s+'.pi';
    try
      sys:= TContext.SystemFrom(Context);
      list.LoadFromFile(s);
      expr:= '';
      inPara:= false;
      for i:= 0 to list.Count-1 do begin
        s:= trim(list[i]);
        if s = '' then
          Continue; 
        case s[1] of
          ';': Continue;
          '§': begin
            if inPara and (expr > '') then
              RunExp(expr);
            inPara:= true;
            expr:= Copy(s, 2, Maxint);
          end;
        else
          begin
            if inPara then
              expr:= expr + s
            else
              RunExp(s);
          end;
        end;
      end;
      if inPara and (expr > '') then
        RunExp(expr);
    except
      on e: Exception do begin
        Context.Output.Error('%s: %s',[e.ClassName, e.Message]);
        raise;
      end;
    end;
  finally
    FreeAndNil(list);
  end;
end;

function TPackageData.Table_1(Context: IContext; args: TExprList): IExpression;
var
  a, b: IValueList;
  s: IStringConvertible;
  i, j, total: integer;
  l: string;
begin
  if not args[0].Evaluate(Context).Represents(IValueList, a) then
    raise EMathSysError.Create('Table requires a list.');
  total:= 0;
  for i:= 0 to a.Length - 1 do begin
    if a.Item[i].Represents(IValueList, b) then begin
      l:= '';
      for j:= 0 to b.Length - 1 do begin
        if l > '' then
          l:= l + #9;
        if b.Item[j].Represents(IStringConvertible, s) then
          l:= l + s.AsString(STR_FORMAT_OUTPUT)
        else
          l:= l + CastToString(b.Item[j]);
        inc(total);
      end;
    end else begin
      if a.Item[i].Represents(IStringConvertible, s) then
        l:= s.AsString(STR_FORMAT_OUTPUT)
      else
        l:= CastToString(a.Item[i]);
      inc(total);
    end;
    Context.Output.Hint('%s', [l]);
  end;
  Result:= TValueFactory.Float(total);
end;

function TPackageData.Bucket_4(Context: IContext; args: TExprList): IExpression;
var
  a, r, e: IValueList;
  mi, mx, width: Number;
  count: MTInteger;
  Counter: array of Int64;
  nn: Number;
  i: integer;
begin
  if not args[0].Evaluate(Context).Represents(IValueList, a) then
    raise EMathSysError.Create('Bucket requires a list.');
  mi:= EvaluateToFloat(Context, args[1]);
  mx:= EvaluateToFloat(Context, args[2]);
  count:= EvaluateToInteger(Context, args[3]);
  SetLength(Counter, count);
  for i:= 0 to high(Counter) do
    Counter[i]:= 0;
  width:= (mx - mi) / count;
  Context.Output.Hint('Bucket Width: %n', [width]);
  for i:= 0 to a.Length - 1 do begin
    nn:= CastToFloat(a.Item[i]);
    if IsNan(nn) or (nn < mi) or (nn > mx) then
      continue;
    Inc(Counter[trunc((nn - mi) / width)]);
  end;
  r:= TValueList.Create;
  r.Length:= length(counter);
  for i:= 0 to high(Counter) do begin
    e:= TValueList.Create;
    e.Length:= 2;
    e.Item[0]:= TValueFactory.Float(mi + width * (i + 0.5));
    e.Item[1]:= TValueFactory.Integer(Counter[i]);

    r.Item[i]:= e as IExpression;
  end;
  Result:= r as IExpression;
end;

function TPackageData.Max_1(Context: IContext; args: TExprList): IExpression;
var
  a: IValueList;
  n, m: Number;
  i: integer;
begin
  if not args[0].Evaluate(Context).Represents(IValueList, a) then
    raise EMathSysError.Create('Max requires a list.');
  m:= MinExtended;
  for i:= 0 to a.Length - 1 do begin
    n:= CastToFloat(a.Item[i]);
    if n > m then
      m:= n;
  end;
  Result:= TValueFactory.Float(m);
end;

function TPackageData.Min_1(Context: IContext; args: TExprList): IExpression;
var
  a: IValueList;
  n, m: Number;
  i: integer;
begin
  if not Supports(args[0].Evaluate(Context), IValueList, a) then
    raise EMathSysError.Create('Min requires a list.');
  m:= MaxExtended;
  for i:= 0 to a.Length - 1 do begin
    n:= CastToFloat(a.Item[i]);
    if n < m then
      m:= n;
  end;
  Result:= TValueFactory.Float(m);
end;

type
  TSortByConfig = class
    OrderDesc: boolean;
    StringCompare: boolean;
  end;
  TSortByListElement = class
    Key: IExpression;
    OriginalIndex: integer;
    SortConfig: TSortByConfig;
  end;

function ListSortCompare_SortBy(A, B: Pointer): integer;
var
  L, R: TSortByListElement;
  X,Y: Number;
  c: TSortByConfig;
begin
  L:= TSortByListElement(A);
  R:= TSortByListElement(B);
  c:= L.SortConfig;

  if c.StringCompare then begin
    Result:= AnsiCompareText(CastToString(L.Key), CastToString(R.Key));
  end else begin
    X:= CastToFloat(L.Key);
    Y:= CastToFloat(R.Key);
    if X<Y then Result:= -1
    else if X>Y then Result:= +1
    else Result:= 0;
  end;
  if c.OrderDesc then
    Result:= - Result;
end;

function TPackageData.SortBy_3_opt(Context: IContext; args: TExprList; Options: TDynamicArguments): IExpression;
var
  list: IValueList;
  v: ISymbolReference;                                           //TE_ExprRef
  ex: IExpression;
  i: integer;
  ctx: IContext;
  res: IValueList;
  sortList: TList;
  sbc: TSortByConfig;
  sle: TSortByListElement;
begin
  if not Supports(args[0].Evaluate(Context), IValueList, list) then
    raise EMathSysError.Create('Function SortBy requires a list to work on');
  if not args[1].Represents(ISymbolReference, v) then
    raise EMathSysError.Create('Function SortBy requires a variable reference');
  ex:= args[2];

  sbc:= TSortByConfig.Create;
  try
    sbc.OrderDesc:= Options.IsSet('Desc');
    sbc.StringCompare:= Options.IsSet('String');

    sortList:= TList.Create;
    try
      sortList.Capacity:= list.Length;
      ctx:= TContext.Create(Context);
      try
        ctx.SetSilent(true);
        for i:= 0 to list.length - 1 do begin
          sle:= TSortByListElement.Create;
          try
            sle.OriginalIndex:= i;
            sle.SortConfig:= sbc;
            ctx.Define(TE_SymbolRef(v.NativeObject).Name, list.Item[i]);
            sle.Key:= ex.Evaluate(ctx);
            sortList.Add(sle);
          except
            FreeAndNil(sle);
          end;
        end;

        sortList.Sort(ListSortCompare_SortBy);

        res:= TValueList.Create;
        try
          res.Length:= list.Length;
          for i:= 0 to sortList.Count-1 do
            res.Item[i]:= list.Item[TSortByListElement(sortList.Items[i]).OriginalIndex];
         Result:= res as IExpression;
        except
          FreeAndNil(res);
        end;
      finally
        FreeAndNil(ctx);
      end;
    finally
      try
        for i:= 0 to sortList.Count-1 do
          TSortByListElement(sortList.Items[i]).Free;
      finally
        FreeAndNil(sortList);
      end;
    end;
  finally
    FreeAndNil(sbc);
  end;
end;

function TPackageData.NumDerive_1(Context: IContext; args: TExprList): IExpression;
var
  l: IValueList;
  res,tup: IValueList;

  i: integer;
  a,b,x,y: Number;
begin
  if not args[0].Evaluate(Context).Represents(IValueList, l) or
     not CheckForTuples(l, 2) then
    raise EMathSysError.Create('Function NumDerive requires a list of 2-tuples');

  res:= TValueList.Create;

  res.Length:= l.Length-1;
  tup:= l.Item[0] as IValueList;
  a:= CastToFloat(tup.Item[0]);
  b:= CastToFloat(tup.Item[1]);
  for i:= 1 to l.Length-1 do begin
    tup:= l.Item[i] as IValueList;
    x:= CastToFloat(tup.Item[0]);
    y:= CastToFloat(tup.Item[1]);
    tup:= TValueList.Create;
    tup.Length:= 2;
    tup.Item[0]:= TValueFactory.Float((x+a) / 2);
    tup.Item[1]:= TValueFactory.Float((y-b) / (x-a));
    Res.Item[i-1]:= tup as IExpression;
    a:= x;
    b:= y;
  end;
  Result:= res as IExpression;
end;

initialization
  Randomize;
end.

