{-----------------------------------------------------------------------------
 Implementations for the value interfaces
-----------------------------------------------------------------------------}
unit uMathValues;

interface

uses
  SysUtils, Math, uMath, uMathIntf, uFPUSupport, uMathDimensions;

type
  TE_Atom = class(TExpression, IExpressionAtom)
  public
    function Evaluate(const Context: IContext): IExpression; override;
    function CompareTo(const B: IExpressionAtom): TAtomCompareResult; virtual;
  end;

  TValueUnassigned = class(TE_Atom, IValueUnassigned, IStringConvertible)
  public
    function Clone(Deep: Boolean): IExpression; override;
    function AsString(const Format: TStringFormat): String;
    function CompareTo(const B: IExpressionAtom): TAtomCompareResult; override;
  end;

  TValueNull = class(TE_Atom, IValueNull, IStringConvertible)
    function Clone(Deep: Boolean): IExpression; override;
    function AsString(const Format: TStringFormat): String;
    function CompareTo(const B: IExpressionAtom): TAtomCompareResult; override;
  end;

  TValueFactory = class
    class function Zero: IValueNumber;
    class function ZeroF: IValueNumber;

    class function FromString(const Str: String; const FS: TFormatSettings): IValueNumber;

    class function Float(const Val: MTFloat): IValueNumber;
    class function Integer(const Val: MTInteger): IValueNumber;

    class function DimScalar(const Template: IValueNumber): IValueNumber;
    class function DimFloat(const Value: MTFloat; const Units: TMathUnits): IValueNumber;
  end;

  TValueNumberBase = class(TE_Atom, IValueNumber)
  public
    function BaseType: TValueNumeralType;
    function IsScalar: Boolean;
    function ValueFloat: MTFloat;
    function ValueInt: MTInteger;
  end;

  TValueInteger = class(TValueNumberBase, IValueNumber, IStringConvertible,
    IOperationAddition, IOperationMultiplication, IOperationPower)
  private
    FVal: MTInteger;
  public
    constructor Create(const Value: MTInteger);
    // IValueNumber
    function BaseType: TValueNumeralType;
    function ValueFloat: MTFloat;
    function ValueInt: MTInteger;
    function Clone(Deep: Boolean): IExpression; override;
    // IStringConvertible
    function AsString(const Format: TStringFormat): String;
    // IOperationAddition
    function OpAdd(const B: IExpression): IExpression;
    function OpSubtract(const B: IExpression): IExpression;
    // IOperationMultiplication
    function OpMultiply(const B: IExpression): IExpression;
    function OpDivide(const B: IExpression): IExpression;
    function OpNegate: IExpression;
    // IOperationPower
    function OpPower(const B: IExpression): IExpression;
    function OpRoot(const B: IExpression): IExpression;
  end;

  TValueFloat = class(TValueNumberBase, IValueNumber, IStringConvertible,
    IOperationAddition, IOperationMultiplication, IOperationPower)
  private
    FVal: MTFloat;
  public
    constructor Create(const Value: MTFloat);
    // IValueNumber
    function BaseType: TValueNumeralType;
    function ValueFloat: MTFloat;
    function ValueInt: MTInteger;
    function Clone(Deep: Boolean): IExpression; override;
    // IStringConvertible
    function AsString(const Format: TStringFormat): String;
    // IOperationAddition
    function OpAdd(const B: IExpression): IExpression;
    function OpSubtract(const B: IExpression): IExpression;
    // IOperationMultiplication
    function OpMultiply(const B: IExpression): IExpression;
    function OpDivide(const B: IExpression): IExpression;
    function OpNegate: IExpression;
    // IOperationPower
    function OpPower(const B: IExpression): IExpression;
    function OpRoot(const B: IExpression): IExpression;
  end;

  TValueFloatDimension = class(TValueFloat, IDimensions, IStringConvertible,
    IOperationAddition, IOperationMultiplication, IOperationPower)
  private
    FDim: IDimensions;
  public
    // 1km: aValueSIBase=1000, aScale=1000, aUnits=[1 0..] aCreatedAs='km'
    constructor Create(const aValueSIBase: MTFloat; const aUnits: TMathUnits; const aScale: MTFloat = 1.0; const aCreatedAs: String = '');
    property Dim: IDimensions read FDim implements IDimensions;
    // IStringConvertible
    function AsString(const Format: TStringFormat): String;
    // IOperationAddition
    function OpAdd(const B: IExpression): IExpression;
    function OpSubtract(const B: IExpression): IExpression;
    // IOperationMultiplication
    function OpMultiply(const B: IExpression): IExpression;
    function OpDivide(const B: IExpression): IExpression;
    function OpNegate: IExpression;
    // IOperationPower
    function OpPower(const B: IExpression): IExpression;
    function OpRoot(const B: IExpression): IExpression;
  end;

  TValueString = class(TE_Atom, IValueString, IStringConvertible)
  private
    FValue: String;
  public
    constructor Create(const aVal: String);
    // IExpression
    function Clone(Deep: Boolean): IExpression; override;    
    function CompareTo(const B: IExpressionAtom): TAtomCompareResult; override;
    // IValueString
    function Value: String;
    // IStringConvertible
    function AsString(const Format: TStringFormat): String;
  end;

  TValueList = class(TE_Atom, IValueList, IStringConvertible)
  protected
    function GetStringMaxLen(MaxLen: integer; Fmt: TStringFormat): String;
  public
    constructor Create;
    constructor CreateAs(Items: array of IExpression);
    // IExpression
    function Evaluate(const Context: IContext): IExpression; override;
    function Clone(Deep: Boolean): IExpression; override;
    function CompareTo(const B: IExpressionAtom): TAtomCompareResult; override;
    // IValueList
    function GetLength: Integer;
    procedure SetLength(const NewLength: Integer);
    procedure SetItem(Index: Integer; val: IExpression);
    function GetItem(Index: Integer): IExpression;
    // IStringConvertible
    function AsString(const Format: TStringFormat): String;
  end;

  TValueRange = class(TE_Atom, IValueList, IStringConvertible)
  private
    FStart, FStep, FEnd: Number;
  public
    constructor Create(Start, Step, Max: Number);
    // IExpression
    function Clone(Deep: Boolean): IExpression; override;
    function CompareTo(const B: IExpressionAtom): TAtomCompareResult; override;
    // IValueList
    function GetLength: Integer;
    procedure SetLength(const NewLength: Integer);
    procedure SetItem(Index: Integer; val: IExpression);
    function GetItem(Index: Integer): IExpression;
    // IStringConvertible
    function AsString(const Format: TStringFormat): String;
  end;

function CastToString(const Exp: IExpression): String;
function CheckForTuples(Param: IValueList; count: integer): boolean;

function EvaluateToString(Context: IContext; ex: IExpression; out s: string): Boolean; overload;
function EvaluateToString(Context: IContext; ex: IExpression): String; overload;

implementation

function CastToString(const Exp: IExpression): String;
var
  v: IValueString;
  s: IStringConvertible;
begin
  if Exp.Represents(IValueString, v) then
    Result:= v.Value
  else begin
    if Exp.Represents(IStringConvertible, s) then
      raise EMathTypeError.CreateFmt('Cannot convert expression to String: %s',[s.AsString(STR_FORMAT_INPUT)])
    else
      raise EMathTypeError.CreateFmt('Cannot convert expression to String: <%s>',[exp.NativeObject.ClassName]);
  end;
end;

function CheckForTuples(Param: IValueList; count: integer): boolean;
var
  l2: IValueList;
  i: integer;
begin
  Result:= true;
  for i:= 0 to Param.Length - 1 do
    if not Param.Item[i].Represents(IValueList, l2) or
       (l2.Length <> count) then begin
         Result:= false;
         exit;
       end;
end;

function EvaluateToString(Context: IContext; ex: IExpression; out s: string): Boolean;
var
  e: IExpression;
  v: IValueString;
begin
  e:= ex.Evaluate(Context);
  Result:= Assigned(e) and e.Represents(IValueString, v);
  if Result then
    s:= v.Value;
end;

function EvaluateToString(Context: IContext; ex: IExpression): String;
begin
  Result:= CastToString(ex.Evaluate(Context));
end;

{ TE_Atom }

function TE_Atom.Evaluate(const Context: IContext): IExpression;
begin
  Result:= Self;
end;

function TE_Atom.CompareTo(const B: IExpressionAtom): TAtomCompareResult;
begin
  Result:= crIncompatible;
end;

{ TValueUnassigned }

function TValueUnassigned.AsString(const Format: TStringFormat): String;
begin
  case Format of
    STR_FORMAT_OUTPUT: Result:= '<?>';
  else
    Result:= '';
  end;
end;

function TValueUnassigned.Clone(Deep: Boolean): IExpression;
begin
  Result:= TValueUnassigned.Create;
end;

function TValueUnassigned.CompareTo(const B: IExpressionAtom): TAtomCompareResult;
begin
  if B.Represents(IValueUnassigned) then
    Result:= crSame
  else
    Result:= inherited CompareTo(B);
end;

{ TValueNull }

function TValueNull.AsString(const Format: TStringFormat): String;
begin
  case Format of
    STR_FORMAT_OUTPUT: Result:= '<NULL>';
  else
    Result:= '';
  end;
end;

function TValueNull.Clone(Deep: Boolean): IExpression;
begin
  Result:= TValueNull.Create;
end;

function TValueNull.CompareTo(const B: IExpressionAtom): TAtomCompareResult;
begin
  if B.Represents(IValueNull) then
    Result:= crSame
  else
    Result:= inherited CompareTo(B);
end;

{ TValueString }

function TValueString.AsString(const Format: TStringFormat): String;
begin
  case Format of
    STR_FORMAT_INPUT,
    STR_FORMAT_INPUT_EXPANDED: Result:= QuotedStr(FValue);
  else
     Result:= FValue;
  end;
end;

function TValueString.Clone(Deep: Boolean): IExpression;
begin
  Result:= TValueString.Create(FValue);
end;

function TValueString.CompareTo(const B: IExpressionAtom): TAtomCompareResult;
var
  sb: IValueString;
begin
  if B.Represents(IValueString, sb) then begin
    if Value > sb.Value then
      Result:= crGreater
    else if Value < sb.Value then
      Result:= crSmaller
    else
      Result:= crSame;
  end else
    Result:= inherited CompareTo(B);
end;

constructor TValueString.Create(const aVal: String);
begin
  inherited Create;
  FValue:= aVal;
end;

function TValueString.Value: String;
begin
  Result:= FValue;
end;

{ TValueList }

constructor TValueList.Create;
begin
  inherited Create;
end;

constructor TValueList.CreateAs(Items: array of IExpression);
var
  i: integer;
begin
  inherited Create;
  System.SetLength(Arguments, Length(Items));
  for i:= 0 to high(Items) do begin
    if Assigned(Items[i]) then
      Arguments[i]:= Items[i]
    else
      Arguments[i]:= TValueUnassigned.Create;
  end;
end;

function TValueList.GetLength: Integer;
begin
  Result:= Length(Arguments);
end;

procedure TValueList.SetLength(const NewLength: Integer);
begin
  System.SetLength(Arguments, NewLength);
end;

function TValueList.GetItem(Index: Integer): IExpression;
begin
  Result:= Arguments[Index];
end;

procedure TValueList.SetItem(Index: Integer; val: IExpression);
begin
  Arguments[Index]:= val;
end;

function TValueList.AsString(const Format: TStringFormat): String;
begin
  case Format of
    STR_FORMAT_OUTPUT: Result:= GetStringMaxLen(200, Format);
  else
    Result:= '{'+StringOfArgs(Format, ',')+'}';
  end;
end;

function TValueList.Evaluate(const Context: IContext): IExpression;
var
  i: integer;
  l: IValueList;
begin
  l:= TValueList.Create;
  l.SetLength(GetLength);
  for i:= 0 to GetLength-1 do
    l.Item[i]:= Arg[i].Evaluate(Context);
  Result:= l as IExpression;
end;

function TValueList.Clone(Deep: Boolean): IExpression;
var
  i: integer;
  vl: TValueList;
begin
  if not Deep then
    Result:= TValueList.CreateAs(Arguments)
  else begin
    vl:= TValueList.Create;
    vl.SetLength(GetLength);
    for i:= 0 to GetLength - 1 do
      vl.SetItem(i, GetItem(i).Clone(Deep));
    Result:= vl as IExpression;
  end;
end;

function TValueList.GetStringMaxLen(MaxLen: integer; Fmt: TStringFormat): String;
var
  i: integer;
  function gets(index: integer): string;
  var
    s: IStringConvertible;
  begin
    if Supports(GetItem(index), IStringConvertible, s) then
      Result:= s.AsString(Fmt)
    else
      Result:= '<Unknown>';
  end;
begin
  if GetLength = 0 then
    Result:= ''
  else begin
    Result:= gets(0);
    for i:= 1 to GetLength - 1 do begin
      if Length(Result) > MaxLen then begin
        Result:= Result + ', ... (' + IntToStr(GetLength - i + 1) + ')';
        break;
      end else
        Result:= Result + ', ' + gets(i);
    end;
  end;
  Result:= '{' + Result + '}';
end;

function TValueList.CompareTo(const B: IExpressionAtom): TAtomCompareResult;
var
  lb: IValueList;
  ia, ib: IExpressionAtom;
  i: integer;
begin
  if B.Represents(IValueList, lb) and lb.IsClass(TValueList) then begin
    if GetLength > lb.length then
      Result:= crGreater
    else if GetLength < lb.length then
      Result:= crSmaller
    else begin
      Result:= crSame;
      for i:= 0 to GetLength-1 do begin
        // TODO: handle non-atoms
        if not Arg[i].Represents(IExpressionAtom, ia) or not lb.Item[i].Represents(IExpressionAtom, ib) or
          (ia.CompareTo(ib)<>crSame) then begin
          Result:= crDifferent;
          exit;
        end;
      end;
    end;
  end else
    Result:= inherited CompareTo(B);
end;

{ TValueRange }

constructor TValueRange.Create(Start, Step, Max: Number);
begin
  inherited Create;
  FStart:= Start;
  FStep:= Step;
  FEnd:= Max;
end;

function TValueRange.GetItem(Index: Integer): IExpression;
begin
  Result:= TValueFactory.Float(FStart + Index * FStep);
end;

function TValueRange.GetLength: Integer;
begin
  Result:= Trunc(1 + (FEnd - FStart) / FStep);
end;

procedure TValueRange.SetItem(Index: Integer; val: IExpression);
begin
end;

procedure TValueRange.SetLength(const NewLength: Integer);
begin
end;

function TValueRange.AsString(const Format: TStringFormat): String;
begin
  case Format of
    STR_FORMAT_INPUT,
    STR_FORMAT_INPUT_EXPANDED: Result:= SysUtils.Format('range(%s->%s, %s)',[
      NumberToStr(FStart,NeutralFormatSettings, false),
      NumberToStr(FEnd,NeutralFormatSettings, false),
      NumberToStr(FEnd,NeutralFormatSettings, false)
    ]);
  else
    Result:= SysUtils.Format('{%s->%s, %s}',[
      NumberToStr(FStart,NeutralFormatSettings, false),
      NumberToStr(FEnd,NeutralFormatSettings, false),
      NumberToStr(FEnd,NeutralFormatSettings, false)
    ]);
  end;
end;

function TValueRange.Clone(Deep: Boolean): IExpression;
begin
  Result:= TValueRange.Create(FStart, FStep, FEnd);
end;

function TValueRange.CompareTo(const B: IExpressionAtom): TAtomCompareResult;
var
  lb: IValueList;
  bc: TValueRange;
begin
  if B.Represents(IValueList, lb) and lb.IsClass(TValueRange) then begin
    if GetLength > lb.length then
      Result:= crGreater
    else if GetLength < lb.length then
      Result:= crSmaller
    else begin
      bc:= TValueRange(lb.NativeObject);
      if fzero(bc.FStart-FStart) and fzero(bc.FStep-fStep) and fzero(bc.FEnd-FEnd) then
        Result:= crSame
      else
        Result:= crDifferent;
    end;
  end else
    Result:= inherited CompareTo(B);
end;

{ TValueFactory }

class function TValueFactory.Zero: IValueNumber;
begin
  Result:= TValueInteger.Create(0);
end;

class function TValueFactory.ZeroF: IValueNumber;
begin
  Result:= TValueFloat.Create(0.0);
end;

class function TValueFactory.FromString(const Str: String; const FS: TFormatSettings): IValueNumber;
var
  t: Int64;
begin
  {$IF NOT( (SizeOf(Int64)=SizeOf(MTInteger)) and (High(MTInteger)=high(Int64)) )}
    {$ERROR Int64 != MTInteger}
  {$IFEND}
  if TryStrToInt64(Str, t) then
    Result:= TValueFactory.Integer(t)
  else
    Result:= TValueFactory.Float(StrToFloat(Str, FS));
end;

class function TValueFactory.Float(const Val: MTFloat): IValueNumber;
begin
  Result:= TValueFloat.Create(Val);
end;

class function TValueFactory.Integer(const Val: MTInteger): IValueNumber;
begin
  Result:= TValueInteger.Create(Val);
end;

class function TValueFactory.DimScalar(const Template: IValueNumber): IValueNumber;
begin
  Result:= TValueFloatDimension.Create(Template.ValueFloat,MakeDimension([]),1.0,'');
end;

class function TValueFactory.DimFloat(const Value: MTFloat; const Units: TMathUnits): IValueNumber;
begin
  Result:= TValueFloatDimension.Create(Value, Units);
end;

{ TValueNumberBase }

function TValueNumberBase.BaseType: TValueNumeralType;
begin
  Result:= tiUnknown;
end;

function TValueNumberBase.IsScalar: Boolean;
var
  d: IDimensions;
begin
  Result:= not Represents(IDimensions, d) or d.IsScalar;
end;

function TValueNumberBase.ValueFloat: Number;
begin
  Result:= NAN;
end;

function TValueNumberBase.ValueInt: MTInteger;
begin
  Result:= 0;
end;

{ TValueInteger }

constructor TValueInteger.Create(const Value: MTInteger);
begin
  inherited Create;
  FVal:= Value;
end;

function TValueInteger.BaseType: TValueNumeralType;
begin
  Result:= tiInt;
end;

function TValueInteger.ValueFloat: Number;
begin
  Result:= FVal;
end;

function TValueInteger.ValueInt: MTInteger;
begin
  Result:= FVal;
end;

function TValueInteger.Clone(Deep: Boolean): IExpression;
begin
  Result:= TValueInteger.Create(FVal);
end;

function TValueInteger.AsString(const Format: TStringFormat): String;
begin
  //TODO IntToStr?
  case Format of
    STR_FORMAT_OUTPUT: Result:= NumberToStr(FVal,NeutralFormatSettings, true);
  else
     Result:= NumberToStr(FVal,NeutralFormatSettings, false);
  end;
end;

function TValueInteger.OpAdd(const B: IExpression): IExpression;
var
  v: IValueNumber;
begin
  Result:= nil;
  if B.Represents(IDimensions) then
    Result:= (TValueFactory.DimScalar(Self) as IOperationAddition).OpAdd(B)
  else
  if B.Represents(IValueNumber, v) then begin
    case v.BaseType of
      tiInt: Result:= TValueFactory.Integer(FVal + v.ValueInt);
    else
      Result:= TValueFactory.Float(FVal + v.ValueFloat);
    end;
  end;
end;

function TValueInteger.OpSubtract(const B: IExpression): IExpression;
var
  v: IValueNumber;
begin
  Result:= nil;
  if B.Represents(IDimensions) then
    Result:= (TValueFactory.DimScalar(Self) as IOperationAddition).OpSubtract(B)
  else
  if B.Represents(IValueNumber, v) then begin
    case v.BaseType of
      tiInt: Result:= TValueFactory.Integer(FVal - v.ValueInt);
    else
      Result:= TValueFactory.Float(FVal - v.ValueFloat);
    end;
  end;
end;

function TValueInteger.OpMultiply(const B: IExpression): IExpression;
var
  v: IValueNumber;
begin
  Result:= nil;
  if B.Represents(IDimensions) then
    Result:= (TValueFactory.DimScalar(Self) as IOperationMultiplication).OpMultiply(B)
  else
  if B.Represents(IValueNumber, v) then begin
    case v.BaseType of
      tiInt: Result:= TValueFactory.Integer(FVal * v.ValueInt);
      //TODO: in case of overflow, return float
    else
      Result:= TValueFactory.Float(FVal * v.ValueFloat);
    end;
  end;
end;

function TValueInteger.OpDivide(const B: IExpression): IExpression;
var
  v: IValueNumber;
begin
  Result:= nil;
  if B.Represents(IDimensions) then
    Result:= (TValueFactory.DimScalar(Self) as IOperationMultiplication).OpDivide(B)
  else
  if B.Represents(IValueNumber, v) then begin
    case v.BaseType of
      tiInt: Result:= TValueFactory.Float(FVal / v.ValueInt);
      //TODO: if no remainder, return int (but only if we're sure!!!)
    else
      Result:= TValueFactory.Float(FVal / v.ValueFloat);
    end;
  end;
end;

function TValueInteger.OpNegate: IExpression;
begin
  Result:= TValueFactory.Integer(-FVal);
end;

function TValueInteger.OpPower(const B: IExpression): IExpression;
var
  v: IValueNumber;
begin
  Result:= nil;
  if B.Represents(IValueNumber, v) then
    Result:= TValueFactory.Float(Power(FVal, v.ValueFloat));
end;

function TValueInteger.OpRoot(const B: IExpression): IExpression;
var
  v: IValueNumber;
begin
  Result:= nil;
  if B.Represents(IValueNumber, v) then
    Result:= TValueFactory.Float(Power(FVal, 1 / v.ValueFloat));
end;

{ TValueFloat }

constructor TValueFloat.Create(const Value: MTFloat);
begin
  inherited Create;
  FVal:= Value;
end;

function TValueFloat.BaseType: TValueNumeralType;
begin
  Result:= tiFloat;
end;

function TValueFloat.ValueFloat: MTFloat;
begin
  Result:= FVal;
end;

function TValueFloat.ValueInt: MTInteger;
begin
  if ftruncable(FVal) and fzero(frac(FVal)) then
    Result:= Floor64(FVal)
  else
    raise EMathTypeError.CreateFmt('Cannot cast Float to Integer: %f',[FVal]);
  //TODO Systemweite Formatierung nutzen
end;

function TValueFloat.Clone(Deep: Boolean): IExpression;
begin
  Result:= TValueFloat.Create(FVal);
end;

function TValueFloat.AsString(const Format: TStringFormat): String;
begin
  case Format of
    STR_FORMAT_OUTPUT: Result:= NumberToStr(FVal,NeutralFormatSettings, true);
  else
     Result:= NumberToStr(FVal,NeutralFormatSettings, false);
  end;
end;

function TValueFloat.OpAdd(const B: IExpression): IExpression;
var
  v: IValueNumber;
begin
  Result:= nil;
  if B.Represents(IDimensions) then
    Result:= (TValueFactory.DimScalar(Self) as IOperationAddition).OpAdd(B)
  else
  if B.Represents(IValueNumber, v) then begin
    Result:= TValueFactory.Float(FVal + v.ValueFloat);
  end;
end;

function TValueFloat.OpSubtract(const B: IExpression): IExpression;
var
  v: IValueNumber;
begin
  Result:= nil;
  if B.Represents(IDimensions) then
    Result:= (TValueFactory.DimScalar(Self) as IOperationAddition).OpSubtract(B)
  else
  if B.Represents(IValueNumber, v) then begin
    Result:= TValueFactory.Float(FVal - v.ValueFloat);
  end;
end;

function TValueFloat.OpMultiply(const B: IExpression): IExpression;
var
  v: IValueNumber;
begin
  Result:= nil;
  if B.Represents(IDimensions) then
    Result:= (TValueFactory.DimScalar(Self) as IOperationMultiplication).OpMultiply(B)
  else
  if B.Represents(IValueNumber, v) then begin
    Result:= TValueFactory.Float(FVal * v.ValueFloat);
  end;
end;

function TValueFloat.OpDivide(const B: IExpression): IExpression;
var
  v: IValueNumber;
begin
  Result:= nil;
  if B.Represents(IDimensions) then
    Result:= (TValueFactory.DimScalar(Self) as IOperationMultiplication).OpDivide(B)
  else
  if B.Represents(IValueNumber, v) then begin
    Result:= TValueFactory.Float(FVal / v.ValueFloat);
  end;
end;

function TValueFloat.OpNegate: IExpression;
begin
  Result:= TValueFactory.Float(-FVal);
end;

function TValueFloat.OpPower(const B: IExpression): IExpression;
var
  v: IValueNumber;
begin
  Result:= nil;
  if B.Represents(IValueNumber, v) then
    Result:= TValueFactory.Float(Power(FVal, v.ValueFloat));
end;

function TValueFloat.OpRoot(const B: IExpression): IExpression;
var
  v: IValueNumber;
begin
  Result:= nil;
  if B.Represents(IValueNumber, v) then
    Result:= TValueFactory.Float(Power(FVal, 1 / v.ValueFloat));
end;

{ TValueFloatDimension }

constructor TValueFloatDimension.Create(const aValueSIBase: MTFloat; const aUnits: TMathUnits; const aScale: MTFloat; const aCreatedAs: String);
begin
  inherited Create(aValueSIBase);
  FDim:= TValueDimension.Create(Self, aScale, aUnits, aCreatedAs);
end;

function TValueFloatDimension.AsString(const Format: TStringFormat): String;
var
  ds: string;
begin
  case Format of
    STR_FORMAT_OUTPUT: Result:= NumberToStr(FVal / FDim.UnitFactor,NeutralFormatSettings, true);
  else
     Result:= NumberToStr(FVal / FDim.UnitFactor,NeutralFormatSettings, false);
  end;
  ds:= FDim.UnitString;
  if ds > '' then
    Result:= Result + ' ' + ds;
end;

function TValueFloatDimension.OpAdd(const B: IExpression): IExpression;
var
  v: IValueNumber;
  d: IDimensions;
begin
  Result:= nil;
  if B.Represents(IValueNumber, v) then begin
    if (B.Represents(IDimensions, d) and d.UnitCompatible(Dim.Units)) then
      Result:= TValueFactory.DimFloat(FVal + v.ValueFloat, d.Units)
    else
    if IsScalar then
      Result:= TValueFactory.Float(FVal + v.ValueFloat);
  end;
end;

function TValueFloatDimension.OpSubtract(const B: IExpression): IExpression;
var
  v: IValueNumber;
  d: IDimensions;
begin
  Result:= nil;
  if B.Represents(IValueNumber, v) then begin
    if (B.Represents(IDimensions, d) and d.UnitCompatible(Dim.Units)) then
      Result:= TValueFactory.DimFloat(FVal - v.ValueFloat, d.Units)
    else
    if IsScalar then
      Result:= TValueFactory.Float(FVal - v.ValueFloat);
  end;
end;

function TValueFloatDimension.OpMultiply(const B: IExpression): IExpression;
var
  v: IValueNumber;
  d: IDimensions;
begin
  Result:= nil;
  if B.Represents(IValueNumber, v) then begin
    if B.Represents(IDimensions, d) then
      Result:= TValueFactory.DimFloat(FVal * v.ValueFloat, MultDimensions(dim.Units,d.Units))
    else
      Result:= TValueFactory.DimFloat(FVal * v.ValueFloat, dim.Units);
  end;
end;

function TValueFloatDimension.OpDivide(const B: IExpression): IExpression;
var
  v: IValueNumber;
  d: IDimensions;
begin
  Result:= nil;
  if B.Represents(IValueNumber, v) then begin
    if B.Represents(IDimensions, d) then
      Result:= TValueFactory.DimFloat(fdiv(FVal, v.ValueFloat), MultDimensions(dim.Units,InverseDimensions(d.Units)))
    else
      Result:= TValueFactory.DimFloat(fdiv(FVal, v.ValueFloat), dim.Units);
  end;
end;

function TValueFloatDimension.OpNegate: IExpression;
begin
  Result:= TValueFactory.DimFloat(-FVal, Dim.Units);
end;

function TValueFloatDimension.OpPower(const B: IExpression): IExpression;
var
  v: IValueNumber;
  x: MTInteger;
begin
  Result:= nil;
  //TODO: IntegerExponent
  if B.Represents(IValueNumber, v) and not B.Represents(IDimensions) and
    fzero(frac(v.ValueFloat)) then begin
    x:= trunc(v.ValueFloat);
    Result:= TValueFactory.DimFloat(IntPower(FVal, x), PowerDimensions(Dim.Units, x));
  end else
    raise EMathDimensionError.Create('Exponent has to be a whole number');
end;

function TValueFloatDimension.OpRoot(const B: IExpression): IExpression;
var
  v: IValueNumber;
  x: MTInteger;
begin
  Result:= nil;
  //TODO: IntegerExponent
  if B.Represents(IValueNumber, v) and not B.Represents(IDimensions) and
    fzero(frac(v.ValueFloat)) then begin
    x:= trunc(v.ValueFloat);
    Result:= TValueFactory.DimFloat(Math.Power(FVal, 1 / x), RootDimensions(Dim.Units, x));
  end else
    raise EMathDimensionError.Create('Exponent has to be a whole number');
end;


end.
