unit uMathValues;

interface

uses
  SysUtils, Math, uMath, uMathIntf;

type
  TE_Atom = class(TExpression, IExpressionAtom)
  public
    function Evaluate(const Context: IContext): IExpression; override;
  end;

  TValueUnassigned = class(TE_Atom, IValueUnassigned, IStringConvertible)
  public
    function Clone(Deep: Boolean): IExpression; override;
    function AsString(const Format: TStringFormat): String;
  end;

  TValueNull = class(TE_Atom, IValueNull, IStringConvertible)
    function Clone(Deep: Boolean): IExpression; override;
    function AsString(const Format: TStringFormat): String;
  end;

  TValueNumber = class(TE_Atom, IValueNumber, IStringConvertible, IOperationAddition, IOperationMultiplication, IOperationPower)
  private
    FValue: Number;
  public
    constructor Create(const aVal: Number);
    // IExpression
    function Clone(Deep: Boolean): IExpression; override;
    // IValueNumber
    function Value: Number;
    // IStringConvertible
    function AsString(const Format: TStringFormat): String;
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

  TValueString = class(TE_Atom, IValueString, IStringConvertible)
  private
    FValue: String;
  public
    constructor Create(const aVal: String);
    // IExpression
    function Clone(Deep: Boolean): IExpression; override;
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
    // IValueList
    function GetLength: Integer;
    procedure SetLength(const NewLength: Integer);
    procedure SetItem(Index: Integer; val: IExpression);
    function GetItem(Index: Integer): IExpression;
    // IStringConvertible
    function AsString(const Format: TStringFormat): String;
  end;

function CastToNumber(const Exp: IExpression): Number;
function CastToString(const Exp: IExpression): String;
function CheckForTuples(Param: IValueList; count: integer): boolean;

function EvaluateToNumber(Context: IContext; ex: IExpression; out n: Number): Boolean; overload;     
function EvaluateToNumber(Context: IContext; ex: IExpression): Number; overload;
function EvaluateToString(Context: IContext; ex: IExpression; out s: string): Boolean; overload;
function EvaluateToString(Context: IContext; ex: IExpression): String; overload;
function DivideNumber(a, d: Number): Number;


implementation

uses
  uMathDimensions;

function CastToNumber(const Exp: IExpression): Number;
var
  v: IValueNumber;
  s: IStringConvertible;
begin
  if Exp.Represents(IValueNumber, v) then
    Result:= v.Value
  else begin
    if Exp.Represents(IStringConvertible, s) then
      raise EMathTypeError.CreateFmt('Cannot convert expression to Number: %s',[s.AsString(STR_FORMAT_INPUT)])
    else
      raise EMathTypeError.CreateFmt('Cannot convert expression to Number: <%s>',[exp.NativeObject.ClassName]);
  end;
end;

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

function EvaluateToNumber(Context: IContext; ex: IExpression; out n: Number): Boolean;
var
  e: IExpression;
  v: IValueNumber;
begin
  e:= ex.Evaluate(Context);
  Result:= Assigned(e) and e.Represents(IValueNumber, v);
  if Result then
    n:= v.Value;
end;

function EvaluateToNumber(Context: IContext; ex: IExpression): Number;
begin
  Result:= CastToNumber(ex.Evaluate(Context));
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

function DivideNumber(a, d: Number): Number;
begin
  Result:= 0;
  if IsZero(d) then begin
    if IsZero(a) then
      Result:= NaN
    else if a < 0 then
      Result:= NegInfinity
    else if a > 0 then
      Result:= Infinity;
  end else
    Result:= a / d;
end;


{ TE_Atom }

function TE_Atom.Evaluate(const Context: IContext): IExpression;
begin
  Result:= Self;
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

{ TValueNumber }

constructor TValueNumber.Create(const aVal: Number);
begin
  inherited Create;
  FValue:= aVal;
end;

function TValueNumber.Value: Number;
begin
  Result:= FValue;
end;
 
function TValueNumber.AsString(const Format: TStringFormat): String;
begin
  case Format of
    STR_FORMAT_OUTPUT: Result:= NumberToStr(FValue,NeutralFormatSettings, true);
  else
     Result:= NumberToStr(FValue,NeutralFormatSettings, false);
  end;
end;

function TValueNumber.Clone(Deep: Boolean): IExpression;
begin
  Result:= TValueNumber.Create(FValue);
end;

function TValueNumber.OpAdd(const B: IExpression): IExpression;
var
  nb: IValueNumber;
  ub: IValueDimension;
begin
  if B.Represents(IValueDimension, ub) then begin
    if ub.IsScalar then
      Result:= TValueNumber.Create(FValue + ub.Value)
    else
      raise EMathDimensionError.Create('Only objects of the same dimension can be added');
  end else
    if b.Represents(IValueNumber, nb) then
      Result:= TValueNumber.Create(FValue + nb.Value);
end;

function TValueNumber.OpSubtract(const B: IExpression): IExpression;
var
  nb: IValueNumber;
  ub: IValueDimension;
begin
  if B.Represents(IValueDimension, ub) then begin
    if ub.IsScalar then
      Result:= TValueNumber.Create(FValue - ub.Value)
    else
      raise EMathDimensionError.Create('Only objects of the same dimension can be subtracted');
  end else
    if b.Represents(IValueNumber, nb) then
      Result:= TValueNumber.Create(FValue - nb.Value);
end;

function TValueNumber.OpDivide(const B: IExpression): IExpression;
var
  ud: IValueDimension;
  nd: IValueNumber;
begin
  if B.Represents(IValueDimension, ud) then
    Result:= TValueDimension.Create(DivideNumber(FValue, ud.Value), InverseDimensions(ud.Units))
  else if B.Represents(IValueNumber, nd) then
    Result:= TValueNumber.Create(DivideNumber(FValue, nd.Value))
  else
    raise EMathTypeError.CreateFmt(sCannotConvertExpression, ['Number']);
end;

function TValueNumber.OpMultiply(const B: IExpression): IExpression;
var
  ud: IValueDimension;
  nd: IValueNumber;
begin
  if B.Represents(IValueDimension, ud) then
    Result:= TValueDimension.Create(FValue * ud.Value, ud.Units)
  else if B.Represents(IValueNumber, nd) then
    Result:= TValueNumber.Create(FValue * nd.Value)
  else
    raise EMathTypeError.CreateFmt(sCannotConvertExpression, ['Number']);
end;

function TValueNumber.OpNegate: IExpression;
begin
  Result:= TValueNumber.Create(-FValue);
end;

function TValueNumber.OpPower(const B: Number): IExpression;
begin
  Result:= TValueNumber.Create(Power(FValue, b));
end;

function TValueNumber.OpRoot(const B: Number): IExpression;
begin
  Result:= TValueNumber.Create(Power(FValue, 1 / B));
end;

{ TValueString }

function TValueString.AsString(const Format: TStringFormat): String;
begin
  case Format of
    STR_FORMAT_INPUT: Result:= QuotedStr(FValue);
  else
     Result:= FValue;
  end;
end;

function TValueString.Clone(Deep: Boolean): IExpression;
begin
  Result:= TValueString.Create(FValue);
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
  Result:= TValueNumber.Create(FStart + Index * FStep);
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
    STR_FORMAT_INPUT: Result:= SysUtils.Format('range(%s->%s, %s)',[
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

end.
