unit uFunctions;

interface

uses SysUtils, Classes, uMath;

type
  TPackageTrig = class(TFunctionPackage)
  published
    function Deg2Rad_1(Context: TContext; args: TExprList): IValue;
    function Rad2Deg_1(Context: TContext; args: TExprList): IValue;
    function Sin_1(Context: TContext; args: TExprList): IValue;
    function Cos_1(Context: TContext; args: TExprList): IValue;
    function Tan_1(Context: TContext; args: TExprList): IValue;
    function ArcSin_1(Context: TContext; args: TExprList): IValue;
    function ArcCos_1(Context: TContext; args: TExprList): IValue;
    function ArcTan_1(Context: TContext; args: TExprList): IValue;
    function ArcTan_2(Context: TContext; args: TExprList): IValue;
    function Sinh_1(Context: TContext; args: TExprList): IValue;
    function Cosh_1(Context: TContext; args: TExprList): IValue;
    function Tanh_1(Context: TContext; args: TExprList): IValue;
    function ArSinh_1(Context: TContext; args: TExprList): IValue;
    function ArCosh_1(Context: TContext; args: TExprList): IValue;
    function ArTanh_1(Context: TContext; args: TExprList): IValue;
  end;

  TPackageElementary = class(TFunctionPackage)
  private
    function LogPossible(Val: Number; var FailVal: IValue): boolean;
  published
    function Exp_1(Context: TContext; args: TExprList): IValue;
    function Ln_1(Context: TContext; args: TExprList): IValue;
    function Lg_1(Context: TContext; args: TExprList): IValue;
    function Ld_1(Context: TContext; args: TExprList): IValue;
    function Loga_2(Context: TContext; args: TExprList): IValue;
    function Sqrt_1(Context: TContext; args: TExprList): IValue;
    function NRt_2(Context: TContext; args: TExprList): IValue;
    function Random_0(Context: TContext; args: TExprList): IValue;
  end;

  TPackageNumerical = class(TFunctionPackage)
  private
    function Base_atoi(base: integer; Param: IValue): IValue;
    function Base_itoa(base: integer; Param: IValue): IValue;
  published
    function Abs_1(Context: TContext; args: TExprList): IValue;
    function Round_1(Context: TContext; args: TExprList): IValue;
    function Round_2(Context: TContext; args: TExprList): IValue;
    function Fac_1(Context: TContext; args: TExprList): IValue;

    function AtoI_2(Context: TContext; args: TExprList): IValue;
    function ItoA_2(Context: TContext; args: TExprList): IValue;

    function h_1(Context: TContext; args: TExprList): IValue;
    function b_1(Context: TContext; args: TExprList): IValue;
    function o_1(Context: TContext; args: TExprList): IValue;
  end;

  TPackageLists = class(TFunctionPackage)
  published
    function L_N(Context: TContext; args: TExprList): IValue;
    function Range_3(Context: TContext; args: TExprList): IValue;
    function Each_3(Context: TContext; args: TExprList): IValue;
    function Flatten_1(Context: TContext; args: TExprList): IValue;
    function Aggregate_5(Context: TContext; args: TExprList): IValue;
    function Merge_2(Context: TContext; args: TExprList): IValue;
    function Part_3(Context: TContext; args: TExprList): IValue;
    function LGet_2(Context: TContext; args: TExprList): IValue;
    function Count_1(Context: TContext; args: TExprList): IValue;
  end;

  TPackageData = class(TFunctionPackage)
  published
    function PWD_0(Context: TContext; args: TExprList): IValue;
    function CWD_1(Context: TContext; args: TExprList): IValue;
    function CSVLoad_1_opt(Context: TContext; args: TExprList; Options: TDynamicArguments): IValue;
    function Source_1(Context: TContext; args: TExprList): IValue;
    function Table_1(Context: TContext; args: TExprList): IValue;
    function Bucket_4(Context: TContext; args: TExprList): IValue;
    function Min_1(Context: TContext; args: TExprList): IValue;
    function Max_1(Context: TContext; args: TExprList): IValue;
    function SortBy_3_opt(Context: TContext; args: TExprList; Options: TDynamicArguments): IValue;
    function NumDerive_1(Context: TContext; args: TExprList): IValue;
  end;

implementation

uses Math, uCCSVList;

{ TPackageTrig }

function TPackageTrig.Deg2Rad_1(Context: TContext; args: TExprList): IValue;
begin
  Result:= TValue.Create(DegToRad(args[0].Evaluate(Context).GetNumber));
end;

function TPackageTrig.Rad2Deg_1(Context: TContext; args: TExprList): IValue;
begin
  Result:= TValue.Create(RadToDeg(args[0].Evaluate(Context).GetNumber));
end;

function TPackageTrig.Sin_1(Context: TContext; args: TExprList): IValue;
begin
  Result:= TValue.Create(Sin(args[0].Evaluate(Context).GetNumber));
end;

function TPackageTrig.Cos_1(Context: TContext; args: TExprList): IValue;
begin
  Result:= TValue.Create(Cos(args[0].Evaluate(Context).GetNumber));
end;

function TPackageTrig.Tan_1(Context: TContext; args: TExprList): IValue;
begin
  Result:= TValue.Create(Tan(args[0].Evaluate(Context).GetNumber));
end;

function TPackageTrig.ArcSin_1(Context: TContext; args: TExprList): IValue;
begin
  Result:= TValue.Create(ArcSin(args[0].Evaluate(Context).GetNumber));
end;

function TPackageTrig.ArcCos_1(Context: TContext; args: TExprList): IValue;
begin
  Result:= TValue.Create(ArcCos(args[0].Evaluate(Context).GetNumber));
end;

function TPackageTrig.ArcTan_1(Context: TContext; args: TExprList): IValue;
begin
  Result:= TValue.Create(ArcTan(args[0].Evaluate(Context).GetNumber));
end;

function TPackageTrig.ArcTan_2(Context: TContext; args: TExprList): IValue;
begin
  Result:= TValue.Create(ArcTan2(args[0].Evaluate(Context).GetNumber, args[1].Evaluate(Context).GetNumber));
end;

function TPackageTrig.Sinh_1(Context: TContext; args: TExprList): IValue;
begin
  Result:= TValue.Create(Sinh(args[0].Evaluate(Context).GetNumber));
end;

function TPackageTrig.Cosh_1(Context: TContext; args: TExprList): IValue;
begin
  Result:= TValue.Create(Cosh(args[0].Evaluate(Context).GetNumber));
end;

function TPackageTrig.Tanh_1(Context: TContext; args: TExprList): IValue;
begin
  Result:= TValue.Create(Tanh(args[0].Evaluate(Context).GetNumber));
end;

function TPackageTrig.ArSinh_1(Context: TContext; args: TExprList): IValue;
begin
  Result:= TValue.Create(ArcSinh(args[0].Evaluate(Context).GetNumber));
end;

function TPackageTrig.ArCosh_1(Context: TContext; args: TExprList): IValue;
begin
  Result:= TValue.Create(ArcCosh(args[0].Evaluate(Context).GetNumber));
end;

function TPackageTrig.ArTanh_1(Context: TContext; args: TExprList): IValue;
begin
  Result:= TValue.Create(ArcTanh(args[0].Evaluate(Context).GetNumber));
end;

{ TPackageElementary }

function TPackageElementary.Exp_1(Context: TContext; args: TExprList): IValue;
begin
  Result:= TValue.Create(System.Exp(args[0].Evaluate(Context).GetNumber));
end;

function TPackageElementary.LogPossible(Val: Number; var FailVal: IValue): boolean;
begin
  Result:= false;
  if IsZero(Val) then
    FailVal:= TValue.Create(NegInfinity)
  else if Val < 0 then
    FailVal:= TValue.Create(NaN)
  else
    Result:= true;
end;

function TPackageElementary.Ln_1(Context: TContext; args: TExprList): IValue;
var
  a: Number;
begin
  a:= args[0].Evaluate(Context).GetNumber;
  if LogPossible(a, Result) then
    Result:= TValue.Create(System.Ln(a));
end;

function TPackageElementary.Lg_1(Context: TContext; args: TExprList): IValue;
var
  a: Number;
begin
  a:= args[0].Evaluate(Context).GetNumber;
  if LogPossible(a, Result) then
    Result:= TValue.Create(Math.Log10(a));
end;

function TPackageElementary.Ld_1(Context: TContext; args: TExprList): IValue;
var
  a: Number;
begin
  a:= args[0].Evaluate(Context).GetNumber;
  if LogPossible(a, Result) then
    Result:= TValue.Create(Math.Log2(a));
end;

function TPackageElementary.Loga_2(Context: TContext; args: TExprList): IValue;
var
  b, a: Number;
begin
  b:= args[0].Evaluate(Context).GetNumber;
  a:= args[1].Evaluate(Context).GetNumber;
  if LogPossible(a, Result) then
    Result:= TValue.Create(Math.LogN(b, a));
end;

function TPackageElementary.Sqrt_1(Context: TContext; args: TExprList): IValue;
begin
  Result:= TValue.Create(Sqrt(args[0].Evaluate(Context).GetNumber));
end;

function TPackageElementary.NRt_2(Context: TContext; args: TExprList): IValue;
begin
  Result:= TValue.Create(Math.Power(args[1].Evaluate(Context).GetNumber, 1 / args[0].Evaluate(Context).GetNumber));
end;

function TPackageElementary.Random_0(Context: TContext; args: TExprList): IValue;
begin
  Result:= TValue.Create(Random);
end;

{ TPackageNumerical }

function TPackageNumerical.Abs_1(Context: TContext; args: TExprList): IValue;
begin
  Result:= TValue.Create(System.Abs(args[0].Evaluate(Context).GetNumber));
end;

function TPackageNumerical.Round_1(Context: TContext; args: TExprList): IValue;
var
  v: Number;
begin
  v:= args[0].Evaluate(Context).GetNumber;
  v:= Round(v);
  Result:= TValue.Create(v);
end;

function TPackageNumerical.Round_2(Context: TContext; args: TExprList): IValue;
var
  pot: integer;
  f,v: Number;
begin
  v:= args[0].Evaluate(Context).GetNumber;
  pot:= trunc(args[1].Evaluate(Context).GetNumber);
  f:= IntPower(10, pot);
  v:= Round(v / f) * f;
  Result:= TValue.Create(v);
end;

function TPackageNumerical.Fac_1(Context: TContext; args: TExprList): IValue;
var
  accu, k, desiredFact: Number;
begin
  accu:= 1;
  desiredFact:= args[0].Evaluate(Context).GetNumber;
  k:= 2;
  while k <= desiredFact do begin
    accu:= accu * k;
    k:= k + 1;
  end;
  Result:= TValue.Create(accu);
end;

const
  BaseString = '0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZ';

function TPackageNumerical.Base_atoi(base: integer; Param: IValue): IValue;
var
  i, j: integer;
  v: int64;
  n: string;
begin
  n:= Param.GetString;
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
  Result:= TValue.Create(v);
end;

function TPackageNumerical.Base_itoa(base: integer; Param: IValue): IValue;
var
  i: integer;
  n: string;
  v: int64;
begin
  v:= Trunc(Param.GetNumber);

  if v = 0 then begin
    Result:= TValue.Create('0');
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
  Result:= TValue.Create(n);
end;

function TPackageNumerical.AtoI_2(Context: TContext; args: TExprList): IValue;
var
  base: integer;
begin
  base:= Trunc(args[1].Evaluate(Context).GetNumber);
  Result:= Base_atoi(base, args[0].Evaluate(Context));
end;

function TPackageNumerical.ItoA_2(Context: TContext; args: TExprList): IValue;
var
  base: integer;
begin
  base:= Trunc(args[1].Evaluate(Context).GetNumber);
  Result:= Base_itoa(base, args[0].Evaluate(Context));
end;

function TPackageNumerical.b_1(Context: TContext; args: TExprList): IValue;
var
  v: IValue;
begin
  v:= args[0].Evaluate(Context);
  case v.ValueType of
    vtNumber: Result:= Base_itoa(2, v);
    vtString: Result:= Base_atoi(2, v);
  else
    Result:= v;
  end;
end;

function TPackageNumerical.h_1(Context: TContext; args: TExprList): IValue;
var
  v: IValue;
begin
  v:= args[0].Evaluate(Context);
  case v.ValueType of
    vtNumber: Result:= Base_itoa(16, v);
    vtString: Result:= Base_atoi(16, v);
  else
    Result:= v;
  end;
end;

function TPackageNumerical.o_1(Context: TContext; args: TExprList): IValue;
var
  v: IValue;
begin
  v:= args[0].Evaluate(Context);
  case v.ValueType of
    vtNumber: Result:= Base_itoa(8, v);
    vtString: Result:= Base_atoi(8, v);
  else
    Result:= v;
  end;
end;

{ TPackageLists }

function TPackageLists.L_N(Context: TContext; args: TExprList): IValue;
var
  i: integer;
  ls: IValueList;
begin
  ls:= TValueFixedList.Create;
  ls.Length:= length(args);
  for i:= 0 to ls.Length - 1 do
    ls.ListItem[i]:= args[i].Evaluate(Context);
  ls.QueryInterface(IValue, Result);
end;

function TPackageLists.Range_3(Context: TContext; args: TExprList): IValue;
var
  a, max, st: Number;
begin
  a:= args[0].Evaluate(Context).GetNumber;
  max:= args[1].Evaluate(Context).GetNumber;
  st:= args[2].Evaluate(Context).GetNumber;
  Result:= TValueRangeList.Create(a, st, max);
end;

function TPackageLists.Each_3(Context: TContext; args: TExprList): IValue;
var
  list: IValueList;
  v: IExpression;                                           //TE_ExprRef
  ex: IExpression;
  i: integer;
  ctx: TContext;
  res: IValueList;
begin
  if not Supports(args[0].Evaluate(Context), IValueList, list) then
    raise EMathSysError.Create('Function Each requires a list to work on');
  v:= args[1];
  if v.GetClassType <> TE_ExprRef then
    raise EMathSysError.Create('Function Each requires a variable reference');
  ex:= args[2];

  ctx:= TContext.Create(Context.System, Context);
  try
    ctx.Silent:= true;
    res:= TValueFixedList.Create;
    res.Length:= list.Length;
    for i:= 0 to list.length - 1 do begin
      ctx.DefineValue(TE_ExprRef(v.GetObject).Name, list.ListItem[i]);
      res.ListItem[i]:= ex.Evaluate(ctx);
    end;
    Result:= res as IValue;
  finally
    FreeAndNil(ctx);
  end;
end;

function TPackageLists.Flatten_1(Context: TContext; args: TExprList): IValue;
var
  list: IValueList;
  i: integer;
  res: IValueList;
begin
  if not Supports(args[0].Evaluate(Context), IValueList, list) then
    raise EMathSysError.Create('Function Flatten requires a list to work on');

  res:= TValueFixedList.Create;
  res.Length:= list.Length;
  for i:= 0 to list.length - 1 do
    res.ListItem[i]:= list.ListItem[i];
  Result:= res as IValue;
end;

function TPackageLists.Aggregate_5(Context: TContext; args: TExprList): IValue;
var
  list: IValueList;
  agg: IExpression;                                         //TE_ExprRef
  init: IExpression;
  v: IExpression;                                           //TE_ExprRef
  ex: IExpression;
  i: integer;
  ctx: TContext;
begin
  if not Supports(args[0].Evaluate(Context), IValueList, list) then
    raise EMathSysError.Create('Function Aggregate requires a list to work on');
  agg:= args[1];
  if agg.GetClassType <> TE_ExprRef then
    raise EMathSysError.Create('Function Aggregate requires a variable reference as aggregate');
  init:= args[2];
  v:= args[3];
  if v.GetClassType <> TE_ExprRef then
    raise EMathSysError.Create('Function Aggregate requires a variable reference');
  ex:= args[4];

  ctx:= TContext.Create(Context.System, Context);
  try
    ctx.Silent:= true;
    ctx.DefineValue(TE_ExprRef(agg.GetObject).Name, init.Evaluate(ctx));

    for i:= 0 to list.length - 1 do begin
      ctx.DefineValue(TE_ExprRef(v.GetObject).Name, list.ListItem[i]);
      ctx.DefineValue(TE_ExprRef(agg.GetObject).Name, ex.Evaluate(ctx));
    end;
    Result:= ctx.Value(TE_ExprRef(agg.GetObject).Name);
  finally
    FreeAndNil(ctx);
  end;
end;

function TPackageLists.Merge_2(Context: TContext; args: TExprList): IValue;
var
  i: integer;
  a, b: IValueList;
  res: IValueList;
begin
  if not Supports(args[0].Evaluate(Context), IValueList, a) or
    not Supports(args[1].Evaluate(Context), IValueList, b) then
    raise EMathSysError.Create('Merge requires 2 lists.');
  res:= TValueFixedList.Create;
  res.Length:= a.Length + b.Length;
  for i:= 0 to a.Length - 1 do
    res.ListItem[i]:= a.ListItem[i];
  for i:= 0 to b.Length - 1 do
    res.ListItem[a.Length + i]:= b.ListItem[i];
  Result:= res as IValue;
end;

function TPackageLists.Part_3(Context: TContext; args: TExprList): IValue;
var
  i, f, l: integer;
  a: IValueList;
  res: IValueList;
begin
  if not Supports(args[0].Evaluate(Context), IValueList, a) then
    raise EMathSysError.Create('Part requires a list.');
  f:= trunc(args[1].Evaluate(Context).GetNumber);
  l:= trunc(args[2].Evaluate(Context).GetNumber);
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
  res:= TValueFixedList.Create;
  res.Length:= l - f + 1;
  for i:= f to l do
    res.ListItem[i - f]:= a.ListItem[i];
  Result:= res as IValue;
end;

function TPackageLists.LGet_2(Context: TContext; args: TExprList): IValue;
var
  a: IValueList;
  f: integer;
begin
  if not Supports(args[0].Evaluate(Context), IValueList, a) then
    raise EMathSysError.Create('LGet requires a list.');
  f:= trunc(args[1].Evaluate(Context).GetNumber);
  if f < 0 then
    f:= a.Length + f;
  if f < 0 then
    f:= 0;
  Result:= a.ListItem[f].AsNative;
end;

function TPackageLists.Count_1(Context: TContext; args: TExprList): IValue;
var
  a: IValueList;
begin
  if not Supports(args[0].Evaluate(Context), IValueList, a) then
    raise EMathSysError.Create('Count requires a list.');
  Result:= TValue.Create(a.Length);
end;

{ TPackageData }

function TPackageData.CWD_1(Context: TContext; args: TExprList): IValue;
begin
  SetCurrentDir(args[0].Evaluate(Context).GetString);
end;

function TPackageData.PWD_0(Context: TContext; args: TExprList): IValue;
begin
  Result:= TValue.Create(GetCurrentDir);
end;

function TPackageData.CSVLoad_1_opt(Context: TContext; args: TExprList; Options: TDynamicArguments): IValue;
var
  list, line: TCSVStringList;
  i, j, first, last: integer;
  res, row: IValueList;
  cn: IValue;
  o_fs: TFormatSettings;
  skip, count: integer;
begin
  list:= TCSVStringList.Create;
  line:= TCSVStringList.Create;
  o_fs:= NeutralFormatSettings;
  try
    if Options.IsSet('Delimiter') and (Options['Delimiter'].GetString > '') then
      line.Delimiter:= Options['Delimiter'].GetString[1]
    else
      line.Delimiter:= ';';
    if Options.IsSet('QuoteChar') and (Options['QuoteChar'].GetString > '') then
      line.QuoteChar:= Options['QuoteChar'].GetString[1]
    else
      line.QuoteChar:= '"';
    if Options.IsSet('Decimal') and (Options['Decimal'].GetString > '') then
      NeutralFormatSettings.DecimalSeparator:= Options['Decimal'].GetString[1];

    list.LoadFromFile(args[0].Evaluate(Context).GetString);

    if Options.IsSet('Skip') then
      skip:= trunc(Options['Skip'].GetNumber)
    else
      skip:= 0;

    first:= skip;
    if Options.IsSet('Count') then begin
      count:= trunc(Options['Count'].GetNumber);
      last:= first + count - 1;
      if last>List.Count-1 then
        last:= List.Count-1;
    end else
      last:= List.Count - 1;

    res:= TValueFixedList.Create;
    res.Length:= last - first + 1;
    for i:= first to last do begin
      line.StrictDelimitedText:= list[i];
      row:= TValueFixedList.Create;
      row.Length:= line.Count;
      for j:= 0 to row.Length - 1 do begin
        cn:= TValue.Create(line[j]);
        row.ListItem[j]:= cn.AsNative;
      end;
      res.ListItem[i - first]:= row as IValue;
    end;

    Result:= res as IValue;
  finally
    NeutralFormatSettings:= o_fs;
    FreeAndNil(list);
    FreeAndNil(line);
  end;
end;

function TPackageData.Source_1(Context: TContext; args: TExprList): IValue;
var
  list: TStringList;
  i: integer;
  s: string;
begin
  list:= TStringList.Create;
  try
    s:= args[0].Evaluate(Context).GetString;
    if not FileExists(s) and FileExists(s+'.pi') then
      s:= s+'.pi';
    list.LoadFromFile(s);
    for i:= 0 to list.Count-1 do begin
      s:= trim(list[i]);
      if (Length(s)>0) and (s[1]<>';') then begin
        try
          Context.System.Run(s);
        except
          on e: Exception do begin
            Context.System.Output.Error('%s: %s',[e.ClassName, e.Message]);
            raise;
          end;
        end;
      end;
    end;
  finally
    FreeAndNil(list);
  end;
end;

function TPackageData.Table_1(Context: TContext; args: TExprList): IValue;
var
  a, b: IValueList;
  s: IStringConvertible;
  i, j, total: integer;
  l: string;
begin
  if not Supports(args[0].Evaluate(Context), IValueList, a) then
    raise EMathSysError.Create('Table requires a list.');
  total:= 0;
  for i:= 0 to a.Length - 1 do begin
    if Supports(a.ListItem[i], IValueList, b) then begin
      l:= '';
      for j:= 0 to b.Length - 1 do begin
        if l > '' then
          l:= l + #9;
        if Supports(b.ListItem[j], IStringConvertible, s) then
          l:= l + s.OutputForm
        else
          l:= l + b.ListItem[j].GetString;
        inc(total);
      end;
    end else begin
      l:= a.ListItem[i].GetString;
      inc(total);
    end;
    Context.System.Output.Hint(l, []);
  end;
  Result:= TValue.Create(total);
end;

function TPackageData.Bucket_4(Context: TContext; args: TExprList): IValue;
var
  a, r, e: IValueList;
  mi, mx, count, width: Number;
  Counter: array of Int64;
  nn: Number;
  i: integer;
begin
  if not Supports(args[0].Evaluate(Context), IValueList, a) then
    raise EMathSysError.Create('Bucket requires a list.');
  mi:= args[1].Evaluate(Context).GetNumber;
  mx:= args[2].Evaluate(Context).GetNumber;
  count:= args[3].Evaluate(Context).GetNumber;
  SetLength(Counter, trunc(count));
  for i:= 0 to high(Counter) do
    Counter[i]:= 0;
  width:= (mx - mi) / count;
  Context.System.Output.Hint('Bucket Width: %n', [width]);
  for i:= 0 to a.Length - 1 do begin
    nn:= a.ListItem[i].GetNumber;
    if IsNan(nn) or (nn < mi) or (nn > mx) then
      continue;
    Inc(Counter[trunc((nn - mi) / width)]);
  end;
  r:= TValueFixedList.Create;
  r.Length:= length(counter);
  for i:= 0 to high(Counter) do begin
    e:= TValueFixedList.Create;
    e.Length:= 2;
    e.ListItem[0]:= TValue.Create(mi + width * (i + 0.5));
    e.ListItem[1]:= TValue.Create(Counter[i]);

    r.ListItem[i]:= e as IValue;
  end;
  Result:= r as IValue;
end;

function TPackageData.Max_1(Context: TContext; args: TExprList): IValue;
var
  a: IValueList;
  n, m: Number;
  i: integer;
begin
  if not Supports(args[0].Evaluate(Context), IValueList, a) then
    raise EMathSysError.Create('Max requires a list.');
  m:= MinExtended;
  for i:= 0 to a.Length - 1 do begin
    n:= a.ListItem[i].GetNumber;
    if n > m then
      m:= n;
  end;
  Result:= TValue.Create(m);
end;

function TPackageData.Min_1(Context: TContext; args: TExprList): IValue;
var
  a: IValueList;
  n, m: Number;
  i: integer;
begin
  if not Supports(args[0].Evaluate(Context), IValueList, a) then
    raise EMathSysError.Create('Min requires a list.');
  m:= MaxExtended;
  for i:= 0 to a.Length - 1 do begin
    n:= a.ListItem[i].GetNumber;
    if n < m then
      m:= n;
  end;
  Result:= TValue.Create(m);
end;

type
  TSortByConfig = class
    OrderDesc: boolean;
    StringCompare: boolean;
  end;
  TSortByListElement = class
    Key: IValue;
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
    Result:= AnsiCompareText(L.Key.GetString, R.Key.GetString);
  end else begin
    X:= L.Key.GetNumber;
    Y:= R.Key.GetNumber;
    if X<Y then Result:= -1
    else if X>Y then Result:= +1
    else Result:= 0;
  end;
  if c.OrderDesc then
    Result:= - Result;
end;

function TPackageData.SortBy_3_opt(Context: TContext; args: TExprList; Options: TDynamicArguments): IValue;
var
  list: IValueList;
  v: IExpression;                                           //TE_ExprRef
  ex: IExpression;
  i: integer;
  ctx: TContext;
  res: IValueList;
  sortList: TList;
  sbc: TSortByConfig;
  sle: TSortByListElement;
begin
  if not Supports(args[0].Evaluate(Context), IValueList, list) then
    raise EMathSysError.Create('Function SortBy requires a list to work on');
  v:= args[1];
  if v.GetClassType <> TE_ExprRef then
    raise EMathSysError.Create('Function SortBy requires a variable reference');
  ex:= args[2];

  sbc:= TSortByConfig.Create;
  try
    sbc.OrderDesc:= Options.IsSet('Desc');
    sbc.StringCompare:= Options.IsSet('String');

    sortList:= TList.Create;
    try
      sortList.Capacity:= list.Length;
      ctx:= TContext.Create(Context.System, Context);
      try
        ctx.Silent:= true;
        for i:= 0 to list.length - 1 do begin
          sle:= TSortByListElement.Create;
          try
            sle.OriginalIndex:= i;
            sle.SortConfig:= sbc;
            ctx.DefineValue(TE_ExprRef(v.GetObject).Name, list.ListItem[i]);
            sle.Key:= ex.Evaluate(ctx);
            sortList.Add(sle);
          except
            FreeAndNil(sle);
          end;
        end;

        sortList.Sort(ListSortCompare_SortBy);

        res:= TValueFixedList.Create;
        try
          res.Length:= list.Length;
          for i:= 0 to sortList.Count-1 do
            res.ListItem[i]:= list.ListItem[TSortByListElement(sortList.Items[i]).OriginalIndex];
         Result:= res as IValue;
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

function TPackageData.NumDerive_1(Context: TContext; args: TExprList): IValue;
var
  l: IValueList;
  res,tup: IValueList;

  i: integer;
  a,b,x,y: Number;
begin
  if not TValue.CheckForTuples(args[0].Evaluate(Context), 2) then
    raise EMathSysError.Create('Function XYPlot requires a list of 2-tuples');

  Supports(args[0].Evaluate(Context), IValueList, l);
  res:= TValueFixedList.Create;

  res.Length:= l.Length-1;
  tup:= l.ListItem[0] as IValueList;
  a:= tup.ListItem[0].GetNumber;
  b:= tup.ListItem[1].GetNumber;
  for i:= 1 to l.Length-1 do begin
    tup:= l.ListItem[i] as IValueList;
    x:= tup.ListItem[0].GetNumber;
    y:= tup.ListItem[1].GetNumber;
    tup:= TValueFixedList.Create;
    tup.Length:= 2;
    tup.ListItem[0]:= TValue.Create((x+a) / 2);
    tup.ListItem[1]:= TValue.Create((y-b) / (x-a));
    Res.ListItem[i-1]:= tup as IValue;
    a:= x;
    b:= y;
  end;
  Result:= res as IValue;
end;

initialization
  Randomize;
  TFunctionPackage.RegisterPackage(TPackageTrig);
  TFunctionPackage.RegisterPackage(TPackageElementary);
  TFunctionPackage.RegisterPackage(TPackageNumerical);
  TFunctionPackage.RegisterPackage(TPackageLists);
  TFunctionPackage.RegisterPackage(TPackageData);
end.

