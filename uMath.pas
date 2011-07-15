unit uMath;

interface

uses SysUtils, Classes, IniFiles, ComCtrls, Variants, Math, Windows, TypInfo,
     Messages, Graphics;

type
  TValueType = (vtUnassigned, vtNull, vtNumber, vtString);
  Number = Extended;

  TValue = object
  private
    FType: TValueType;
    FNum: Number;
    FStr: String;
  public
    function ValueType: TValueType;
    procedure SetNumber(const num: Number);
    procedure SetString(const str: String);
    function GetNumber: Number;
    function GetString: string;
    procedure SetNull;
    procedure SetUnassigned;
  end;

  TContext = class;
  TOutput = class;
  TExpression = class;
  IExpression = interface;
  EMathSysError = class(Exception);

  TMathSystem = class
  private
    FContext: TContext;
    FOutput: TOutput;
    FConstants: TContext;
  public
    constructor Create;
    destructor Destroy; override;
    function Parse(const Expr: String): IExpression;
    function Eval(const Expr: String): TValue;
    procedure Run(const Expr: String);
    property Context: TContext read FContext;
    property Constants: TContext read FConstants;
    property Output: TOutput read FOutput;
  end;

  TOutput = class
  private
    FRender: TRichEdit;
    procedure LineOut(const Line: string; Indent: integer; Color: TColor; Style: TFontStyles);
  public
    constructor Create;
    procedure Input(const Line: string);
    procedure Hint(const Line: string; Params: array of const);
    procedure Error(const Line: string; Params: array of const);
    procedure Result(const Line: string);
    property Render : TRichEdit read FRender write FRender;
  end;

  TContext = class
  private
    FExpressions: THashedStringList;
    FParent: TContext;
    FSystem: TMathSystem;
    function GetCount: integer;
    function GetName(index: integer): string;
  public
    constructor Create(ASystem: TMathSystem; AParent: TContext);
    destructor Destroy; override;
    property Parent: TContext read FParent;
    property System : TMathSystem read FSystem; 
    procedure Define(const Name: string; Expression: IExpression);
    procedure DefineValue(const Name: string; Value: TValue);
    procedure Undefine(const Name: string);
    function Definition(const Name: string): IExpression;
    function Value(const Name: string): TValue;

    property Count: integer read GetCount;
    property Name[index: integer]: string read GetName;
  end;

  TExpressionClass = class of TExpression;

  IExpression = interface
    function GetLHS: IExpression;
    function GetRHS: IExpression;
    procedure SetLHS(const Value: IExpression);
    procedure SetRHS(const Value: IExpression);

    function Evaluate(Context: TContext): TValue;
    function StringForm: string;

    function GetClassType: TClass;
    function GetObject: TExpression;

    property LHS: IExpression read GetLHS write SetLHS;
    property RHS: IExpression read GetRHS write SetRHS;
  end;

  TExpression = class(TInterfacedObject, IExpression)
  private
    LHS,RHS: IExpression;
    function GetLHS: IExpression;
    function GetRHS: IExpression;
    procedure SetLHS(const Value: IExpression);
    procedure SetRHS(const Value: IExpression);
  public
    function Evaluate(Context: TContext): TValue; virtual; abstract;
    function GetClassType: TClass;
    function GetObject: TExpression;
    function StringForm: String; virtual;
    constructor Create;
  end;

  TE_ConstantN = class(TExpression)
  private
    FValue: Number;
  public
    constructor Create(Val: Number);
    function Evaluate(Context: TContext): TValue; override;
    function StringForm: String; override;
  end;

  TE_ConstantS = class(TExpression)
  private
    FValue: String;
  public
    constructor Create(Val: String);
    function Evaluate(Context: TContext): TValue; override;
    function StringForm: String; override;
  end;

  TE_ExprRef = class(TExpression)
  private
    FName: string;
  public
    constructor Create(Name: string);
    function Evaluate(Context: TContext): TValue; override;
    function StringForm: String; override;
  end;

  TExprList = array of IExpression;
  TUDFHeader = function(Context: TContext; args: TExprList) : TValue of object;
  TE_FunctionCall = class(TExpression)
  private
    FName: string;
    Arguments: IExpression;
  public
    constructor Create(Name: string);
    function Evaluate(Context: TContext): TValue; override;
    function StringForm: String; override;
  published
    function Abs_1(Context: TContext; args: TExprList): TValue;
    function Exp_1(Context: TContext; args: TExprList): TValue;
    function Ln_1(Context: TContext; args: TExprList): TValue;
    function Lg_1(Context: TContext; args: TExprList): TValue;
    function Ld_1(Context: TContext; args: TExprList): TValue;
    function Loga_2(Context: TContext; args: TExprList): TValue;
    function Fac_1(Context: TContext; args: TExprList): TValue;

    function Deg2Rad_1(Context: TContext; args: TExprList): TValue;
    function Rad2Deg_1(Context: TContext; args: TExprList): TValue;
    function Sin_1(Context: TContext; args: TExprList): TValue;
    function Cos_1(Context: TContext; args: TExprList): TValue;
    function Tan_1(Context: TContext; args: TExprList): TValue;
    function ArcSin_1(Context: TContext; args: TExprList): TValue;
    function ArcCos_1(Context: TContext; args: TExprList): TValue;
    function ArcTan_1(Context: TContext; args: TExprList): TValue;
    function ArcTan_2(Context: TContext; args: TExprList): TValue;
    function Sinh_1(Context: TContext; args: TExprList): TValue;
    function Cosh_1(Context: TContext; args: TExprList): TValue;
    function Tanh_1(Context: TContext; args: TExprList): TValue;
    function ArSinh_1(Context: TContext; args: TExprList): TValue;
    function ArCosh_1(Context: TContext; args: TExprList): TValue;
    function ArTanh_1(Context: TContext; args: TExprList): TValue;
  end;

  TE_ArgList = class(TExpression)
  private
    function CollectAll: TExprList;
  public
    function Evaluate(Context: TContext): TValue; override;
    function StringForm: String; override;
  end;

  TE_Subcontext = class(TExpression)
  private
    FName: string;
    Arguments: IExpression;
  public
    constructor Create(Name: string);
    function Evaluate(Context: TContext): TValue; override;
    function StringForm: String; override;
  end;

  TE_Describe = class(TExpression)
  private
  public
    function Evaluate(Context: TContext): TValue; override;
  end;

  TE_AssignmentDynamic = class(TExpression)
  private
  public
    function Evaluate(Context: TContext): TValue; override;
  end;

  TE_AssignmentStatic = class(TExpression)
  private
  public
    function Evaluate(Context: TContext): TValue; override;
  end;

  TE_Power = class(TExpression)
  private
  public
    function Evaluate(Context: TContext): TValue; override;
  end;
  
  TE_Multiplication = class(TExpression)
  private
  public
    function Evaluate(Context: TContext): TValue; override;
  end;

  TE_Division = class(TExpression)
  private
  public
    function Evaluate(Context: TContext): TValue; override;
  end;

  TE_Modulus = class(TExpression)
  private
  public
    function Evaluate(Context: TContext): TValue; override;
  end;

  TE_Addition = class(TExpression)
  private
  public
    function Evaluate(Context: TContext): TValue; override;
  end;

  TE_Subtraction = class(TExpression)
  private
  public
    function Evaluate(Context: TContext): TValue; override;
  end;

  TExpressionDef = record
    P: integer;
    Infix: String;
    Unary: boolean;
    Cls: TExpressionClass;
  end;

const
  Expressions : array[0..9] of TExpressionDef = (
    (P: 10; Infix: '?'; Unary: true; Cls: TE_Describe),

    (P: 19; Infix: '^'; Unary: False; Cls: TE_Power),
    (P: 20; Infix: '*'; Unary: False; Cls: TE_Multiplication),
    (P: 20; Infix: '/'; Unary: False; Cls: TE_Division),
    (P: 20; Infix: '%'; Unary: False; Cls: TE_Modulus),

    (P: 30; Infix: '+'; Unary: False; Cls: TE_Addition),
    (P: 30; Infix: '-'; Unary: False; Cls: TE_Subtraction),
    
    (P:100; Infix: '='; Unary: False; Cls: TE_AssignmentStatic),
    (P:105; Infix: ':='; Unary: False; Cls: TE_AssignmentDynamic),
    (P:110; Infix: ','; Unary: False; Cls: TE_ArgList)
  );

var
  NeutralFormatSettings: TFormatSettings;  
const
  cPi: Number = 3.1415926535897932384626433832795;
  cE : Number = 2.718281828459045235360287471353;
implementation

{ TValue }

function TValue.GetNumber: Number;
begin
  {$WARNINGS OFF}
  case FType of
    vtNumber: Result:= FNum;
    vtString: Result:= StrToFloat(FStr, NeutralFormatSettings);
  else
    Result:= NAN;
  end;
  {$WARNINGS ON}
end;

function TValue.GetString: string;
begin
  case FType of
    vtNumber: Result:= FloatToStrF(FNum, ffGeneral, 22, 18, NeutralFormatSettings);
    vtString: Result:= FStr;
  else
    Result:= '';
  end;
end;

procedure TValue.SetNumber(const num: Number);
begin
  FNum:= num;
  FType:= vtNumber;
end;

procedure TValue.SetString(const str: String);
begin
  FStr:= str;
  FType:= vtString;
end;

procedure TValue.SetNull;
begin
  FType:= vtNull;
end;

procedure TValue.SetUnassigned;
begin
  FType:= vtUnassigned;
end;

function TValue.ValueType: TValueType;
begin
  Result:= FType;
end;

{ TMathSystem }

constructor TMathSystem.Create;
begin
  inherited;
  FConstants:= TContext.Create(Self, nil);
  FContext:= TContext.Create(Self, FConstants);
  FOutput:= TOutput.Create;
  FConstants.Define('Pi',  TE_ConstantN.Create(cPi));
  FConstants.Define('e',   TE_ConstantN.Create(cE));
end;

destructor TMathSystem.Destroy;
var ctx, fr: TContext;
begin
  fr:= FContext;
  while Assigned(fr) do begin
    ctx:= fr.Parent;
    FreeAndNil(fr);
    fr:= ctx;
  end;
  FreeAndNil(FOutput);
  inherited;
end;

function TMathSystem.Eval(const Expr: String): TValue;
var ex: IExpression;
begin
  ex:= Parse(Expr);
  try
    if Assigned(ex) then
      Result:= ex.Evaluate(FContext)
    else
      Result.SetNull;
  finally
    ex:= nil;
  end;
end;

procedure TMathSystem.Run(const Expr: String);
var re: TValue;
begin
  try
    re:= Eval(Expr);
    case re.ValueType of
      vtUnassigned: ;
      vtNull: ;
    else
      begin
        Output.Result(re.GetString);
        Context.DefineValue('ans',re);
      end;
    end;
  except
    on e: EMathSysError do
      Output.Error(E.Message,[]);
  end;
end;

function TMathSystem.Parse(const Expr: String): IExpression;
const
  CharsSpace = [' ',#9];
  CharsNum   = ['0'..'9'];
  CharsAlpha = ['a'..'z','A'..'Z','_'];
  CharsAlphaNum = CharsAlpha + CharsNum;

  CharQuote = '''';

  CharBraceOpen = '(';
  CharBraceClose = ')';
  CharContextOpen = '[';
  CharContextClose = ']';

type
  TParserState = (psLHS, psOP, psRHS);

var p: Integer;
    ps: TParserState;
    lhs,rhs: IExpression;
    i,op:integer;
  procedure pSpace;
  begin
    while (p<=Length(Expr)) and (Expr[p] in CharsSpace) do
      inc(p);
  end;

  function pToken: string;
  begin
    Result:= '';
    while (p<=Length(Expr)) and (Expr[p] in CharsAlphaNum) do begin
      Result:= Result + Expr[p];
      inc(p);
    end;
  end;

  function blindParse(O,C: Char; Sub: boolean=true): string;
  var l: integer;
  begin
    l:= 0;
    Result:= '';
    while p<= Length(Expr) do begin
      if Expr[p] = O then begin
        inc(l);
        if not Sub and (l>1) then
          raise EMathSysError.CreateFmt('Subexpression contained by %s and %s does not allow nesting.',[O,C]);
        if l>1 then
          Result:= Result + Expr[p];
      end else begin
        Result:= Result + Expr[p];
        if Expr[p] = C then begin
          dec(l);
          if l=0 then begin
            inc(P);
            SetLength(Result, Length(Result)-1);
            Exit;
          end;
        end else
      end;
      inc(p);
    end;
    if l>0 then
      raise EMathSysError.CreateFmt('Unterminated Subexpression contained by %s and %s.',[O,C])
  end;

  procedure parseTerm(var into: IExpression);
  var mode: (tmNumberSign, tmNumber, tmNumberDecimals, tmNumberExponentSign, tmNumberExponent,
             tmString,
             tmVarname);
      data, bd: string;
      b: PChar;
  begin
    case Expr[p] of
      '0'..'9': mode:= tmNumber;
      '-': mode:= tmNumberSign;
      '.': mode:= tmNumberDecimals;
      CharQuote: mode:= tmString;
      CharBraceOpen: begin
        bd := blindParse(CharBraceOpen, CharBraceClose);
        into:= Parse(bd);
        pSpace;
        exit;
      end;
      'a'..'z','A'..'Z','_': mode:= tmVarname;
    else
      into:= nil;
      exit;
    end;

    data:= Expr[p];
    inc(p);

    while p<=Length(Expr) do begin
      case mode of
        tmNumberSign:
          if Expr[p] in ['0'..'9'] then begin
            data:= data + Expr[p];
            mode:= tmNumber;
          end else
            break;
        tmNumber:
          if Expr[p] in ['0'..'9'] then
            data:= data + Expr[p]
          else
          if Expr[p]=NeutralFormatSettings.DecimalSeparator then begin
            data:= data + Expr[p];
            mode:= tmNumberDecimals;
          end else
          if Expr[p] in ['e','E'] then begin
            data:= data + Expr[p];
            mode:= tmNumberExponentSign;
          end else
            break;
        tmNumberDecimals:
          if Expr[p] in ['0'..'9'] then
            data:= data + Expr[p]
          else
          if Expr[p] in ['e','E'] then begin
            data:= data + Expr[p];
            mode:= tmNumberExponentSign;
          end else
            break;
        tmNumberExponentSign: begin
          if Expr[p] in ['0'..'9','-'] then begin
            data:= data + Expr[p];
            mode:= tmNumberExponent;
          end else
            break;
          end;
        tmNumberExponent:
          if Expr[p] in ['0'..'9'] then
            data:= data + Expr[p]
          else
            break;

        tmString:
          if (Expr[p]<>CharQuote) then
            data:= data + Expr[p]
          else begin
            data:= data + Expr[p];
            inc(p);
            if (p<=length(Expr)) and (Expr[p]=CharQuote) then
              data:= data + Expr[p]
            else
              break;
          end;
        tmVarname:
          if Expr[p] in CharsAlphaNum then
            data:= data + Expr[p]
          else
            Break;
      end;
      inc(p);
    end;

    case mode of
      tmNumberSign, tmNumber, tmNumberDecimals, tmNumberExponentSign, tmNumberExponent:
        into:= TE_ConstantN.Create(StrToFloat(data, NeutralFormatSettings));
      tmString: begin
        b:= PChar(data);
        into:= TE_ConstantS.Create(AnsiExtractQuotedStr(b,CharQuote))
      end;
      tmVarname: begin
        if (data>'') and (Expr[p] = CharBraceOpen) then begin
          into:= TE_FunctionCall.Create(data);
          bd := blindParse(CharBraceOpen, CharBraceClose);
          TE_FunctionCall(into.GetObject).Arguments:= Parse(bd);
        end else
        if (data>'') and (Expr[p] = CharContextOpen) then begin
          into:= TE_Subcontext.Create(data);
          bd := blindParse(CharContextOpen, CharContextClose);
          TE_Subcontext(into.GetObject).Arguments:= Parse(bd);
        end else
        into:= TE_ExprRef.Create(data);
      end;
    end;

    pSpace;
  end;

  procedure StoreExpr(AsClass: TExpressionClass);
  var i, j, cpr, npr: integer;
      no: TExpression;
      refs: array of IExpression;
  begin
    if not Assigned(Result) then begin
      no:= AsClass.Create;
      no.LHS:= lhs;
      no.RHS:= rhs;
      Result:= no;
    end
    else begin
      SetLength(refs, 1);
      Refs[0]:= Result;
      while (refs[high(refs)].RHS<>nil) and (refs[high(refs)].RHS.RHS<>nil) do begin
        SetLength(refs, length(refs)+1);
        refs[high(refs)]:= refs[high(refs)-1].rhs;
      end;

      npr:= 0;
      for i:= 0 to High(Expressions) do
        if Expressions[i].Cls = AsClass then begin
          npr:= Expressions[i].P;
          Break;
        end;
      Assert(npr<>0);
      // wenn prec < als bisher: als rhs des aktuellen ablegen, aktuelle rhs als lhs verwenden
      // wenn prec >= bisher: als neuen ref annehmen und altes als lhs ablegen

      // scan upwards, until weaker bound is found
      j:= 0;
      repeat
        cpr:= 0;
        for i:= 0 to High(Expressions) do
          if Expressions[i].Cls = refs[high(refs)-j].GetClassType then begin
            cpr:= Expressions[i].P;
            break;
          end;
        inc(j);
      until (npr < cpr) or (j>high(refs));
      SetLength(refs, Length(refs)-j+1);

      // insert just below that
      no:= AsClass.Create;
      if Length(refs)>0 then begin
        if npr < cpr then begin
          no.LHS:= refs[high(refs)].RHS;
          no.RHS:= rhs;
          refs[high(refs)].rhs:= no;
        end else begin
          no.LHS:= refs[high(refs)];
          no.RHS:= rhs;
          if length(refs)>1 then
            refs[high(refs)-1].rhs:= no
          else
            Result:= no;
        end;
      end
      else begin
        no.LHS:= Result;
        no.RHS:= rhs;
        Result:= no;
      end;
    end;
  end;

begin
  Result:= nil;
  op:= 0;
  p:= 1;
  PS:= psLHS;

  pSpace;
// sum(x+1) / y * !5
// e=m*c^2
  while p<= Length(Expr) do begin
    case PS of
      psLHS: begin
        parseTerm(lhs);
        PS:= psOP;
      end;

      psOp: begin
        op:= -1;
        for i:= 0 to high(Expressions) do
          if (Expressions[i].Infix>'') and 
            (Copy(Expr,p,Length(Expressions[i].Infix))=Expressions[i].Infix) then begin
            op:= i;
            inc(p, Length(Expressions[op].Infix));
            PS:= psRHS;
            if Expressions[op].Unary and (lhs<>nil) then
                raise EMathSysError.CreateFmt('Unary operator %s was given an LHS!',[Expressions[op].Infix]);
            if not Expressions[op].Unary and (lhs=nil) then
                raise EMathSysError.CreateFmt('Binary operator %s was not given an LHS!',[Expressions[op].Infix]);
          end;
        if op<0 then
          raise EMathSysError.CreateFmt('Expected Operator at %d, none found.',[p]);
        pSpace;
      end;

      psRHS: begin
        parseTerm(rhs);

        StoreExpr(Expressions[op].Cls);
        lhs:= nil;
        rhs:= nil;
        PS:= psOp;
        pSpace;
      end;
    end;
  end;
  if not Assigned(Result) and Assigned(lhs) then
    Result:= lhs; 
end;

{ TOutput }

constructor TOutput.Create;
begin
  inherited;
  FRender:= nil;
end;

procedure TOutput.LineOut(const Line: string; Indent: integer; Color: TColor; Style: TFontStyles);
begin
  FRender.SelStart:= length(FRender.Text);
  FRender.SelLength:= 0;
  if Indent<0 then begin
    FRender.Paragraph.FirstIndent:= 0;
    FRender.Paragraph.LeftIndent:= -Indent;
    FRender.Paragraph.TabCount:= 1;
    FRender.Paragraph.Tab[0]:= -Indent;
  end else begin
    FRender.Paragraph.FirstIndent:= Indent;
    FRender.Paragraph.LeftIndent:= 0;
    FRender.Paragraph.TabCount:= 0;
  end;
  FRender.SelAttributes.Color:= Color;   
  FRender.SelAttributes.Style:= Style;
  FRender.Lines.Add(Line);
  PostMessage(FRender.Handle, EM_SCROLLCARET, 0, 0);
end;

procedure TOutput.Hint(const Line: string; Params: array of const);
begin
  LineOut(Format(Line, Params), 10, clNavy,[fsItalic]);
end;

procedure TOutput.Error(const Line: string; Params: array of const);
begin
  LineOut(Format(Line, Params), 10, $009eff,[]);
end;

procedure TOutput.Result(const Line: string);
begin
  LineOut('='#9+Line, -10, FRender.Font.Color,[]);
end;

procedure TOutput.Input(const Line: string);
var s: string;
begin
  s:= '>'#9+Line;
  if FRender.Lines.Count>0 then
    s:= #13#10+s;
  LineOut(s, -10, clDkGray,[]);
end;

{ TContext }

constructor TContext.Create(ASystem: TMathSystem; AParent: TContext);
begin
  inherited Create;
  FExpressions:= THashedStringList.Create;
  FParent:= AParent;
  FSystem:= ASystem;
end;

destructor TContext.Destroy;
var i:Integer;
begin
  for i:= FExpressions.Count-1 downto 0 do
    Undefine(FExpressions[i]);
  FreeAndNil(FExpressions);
  inherited;
end;

procedure TContext.Define(const Name: string; Expression: IExpression);
var intf: IExpression;
    i: integer;
begin
  i:= FExpressions.IndexOf(Name);
  if i>=0 then begin
    intf:= IExpression(Pointer(FExpressions.Objects[i]));
    intf._Release;
    FExpressions.Objects[i]:= TObject(Expression);
    // only warn on non-system variables
    if not SameText(Name,'ans') then
      FSystem.Output.Hint('Reassigned Variable: %s',[Name]);
  end else
    FExpressions.AddObject(Name,TObject(Expression));
  Expression._AddRef;
end;

procedure TContext.Undefine(const Name: string);
var intf: IExpression;
    i:Integer;
begin
  i:= FExpressions.IndexOf(Name);
  if i>=0 then begin
    intf:= IExpression(Pointer(FExpressions.Objects[i]));
    intf._Release;
    FExpressions.Delete(i);
  end;
end;

function TContext.Definition(const Name: string): IExpression;
var i: integer;
begin
  i:= FExpressions.IndexOf(Name);
  if i>=0 then begin
    Result:= IExpression(Pointer(FExpressions.Objects[i]));
  end
  else begin
    if Assigned(FParent) then
      Result:= FParent.Definition(Name)
    else
      Result:= nil;
  end;
end;

function TContext.Value(const Name: string): TValue;
var ex: IExpression;
begin
  ex:= Definition(Name);
  if Assigned(ex) then
    Result:= ex.Evaluate(Self)
  else begin
    Result.SetUnassigned;
    raise EMathSysError.CreateFmt('Expression unknown in current Context: %s',[Name]);
  end;
end;

function TContext.GetCount: integer;
begin
  Result:= FExpressions.Count;
end;

function TContext.GetName(index: integer): string;
begin
  Result:= FExpressions[Index];
end;


procedure TContext.DefineValue(const Name: string; Value: TValue);
var expr: IExpression;
begin
  expr:= nil;
  case Value.ValueType of
    vtUnassigned,
    vtNull:;
    vtNumber: expr:= TE_ConstantN.Create(Value.GetNumber);
    vtString: expr:= TE_ConstantS.Create(Value.GetString);
  end;
  if Assigned(expr) then
    Define(Name,expr);
end;

{ TExpression }

constructor TExpression.Create;
begin
  inherited Create;
  LHS:= nil;
  RHS:= nil;
end;

function TExpression.GetClassType: TClass;
begin
  Result:= ClassType;
end;

function TExpression.GetLHS: IExpression;
begin
  Result:= LHS;
end;

function TExpression.GetObject: TExpression;
begin
  Result:= Self;
end;

function TExpression.GetRHS: IExpression;
begin
  Result:= RHS;
end;

procedure TExpression.SetLHS(const Value: IExpression);
begin
  LHS:= Value;
end;

procedure TExpression.SetRHS(const Value: IExpression);
begin
  RHS:= Value;
end;

function TExpression.StringForm: String;
begin
  if LHS = nil then
    Result:= format('%s(%s)',[Copy(ClassName,4,10000),RHS.StringForm])
  else
    Result:= format('%s(%s,%s)',[Copy(ClassName,4,10000),LHS.StringForm,RHS.StringForm]);
end;

{ TE_ConstantN }

constructor TE_ConstantN.Create(Val: Number);
begin                  
  inherited Create;
  FValue:= Val;
end;

function TE_ConstantN.Evaluate(Context: TContext): TValue;
begin
  Result.SetNumber(FValue);
end;

function TE_ConstantN.StringForm: String;
begin
  Result:= FloatToStrF(FValue,ffGeneral,18,12,NeutralFormatSettings);
end;

{ TE_ConstantS }

constructor TE_ConstantS.Create(Val: String);
begin
  inherited Create;
  FValue:= Val;
end;

function TE_ConstantS.Evaluate(Context: TContext): TValue;
begin
  Result.SetString(FValue);
end;

function TE_ConstantS.StringForm: String;
begin
  Result:= QuotedStr(FValue);
end;

{ TE_ExprRef }

constructor TE_ExprRef.Create(Name: string);
begin
  inherited Create;
  FName:= Name;
end;

function TE_ExprRef.Evaluate(Context: TContext): TValue;
begin
  Result:= Context.Value(FName);
end;

function TE_ExprRef.StringForm: String;
begin
  Result:= FName;
end;

{ TE_FunctionCall }

constructor TE_FunctionCall.Create(Name: string);
begin
  inherited Create;
  FName:= Name;
end;

function TE_FunctionCall.Evaluate(Context: TContext): TValue;
var meth: TMethod;
    ls: TExprList;
begin
  if Assigned(Arguments) then begin
    if Arguments.GetObject is TE_ArgList then
      ls:= TE_ArgList(Arguments.GetObject).CollectAll
    else begin
      SetLength(ls,1);
      ls[0]:= Arguments;
    end;
  end else
    SetLength(ls,0);
  meth.Code:= MethodAddress(FName+'_'+IntToStr(Length(ls)));
  if Assigned(meth.Code) then begin
    meth.Data:= Self;
    Result:= TUDFHeader(meth)(Context, ls);
  end else
    raise EMathSysError.CreateFmt('Function %s has no version with %d parameters',[FName, Length(ls)]);
end;

function TE_FunctionCall.StringForm: String;
begin
  Result:= FName+'('+Arguments.StringForm+')';
end;


function TE_FunctionCall.Abs_1(Context: TContext; args: TExprList): TValue;
begin
  Result.SetNumber(System.Abs(args[0].Evaluate(Context).GetNumber));
end;

function TE_FunctionCall.Exp_1(Context: TContext; args: TExprList): TValue;
begin
  Result.SetNumber(System.Exp(args[0].Evaluate(Context).GetNumber));
end;

function TE_FunctionCall.Ln_1(Context: TContext; args: TExprList): TValue;
begin
  Result.SetNumber(System.Ln(args[0].Evaluate(Context).GetNumber));
end;

function TE_FunctionCall.Lg_1(Context: TContext; args: TExprList): TValue;
begin
  Result.SetNumber(Math.Log10(args[0].Evaluate(Context).GetNumber));
end;

function TE_FunctionCall.Ld_1(Context: TContext; args: TExprList): TValue;
begin
  Result.SetNumber(Math.Log2(args[0].Evaluate(Context).GetNumber));
end;

function TE_FunctionCall.Loga_2(Context: TContext; args: TExprList): TValue;
begin
  Result.SetNumber(Math.LogN(args[0].Evaluate(Context).GetNumber,args[1].Evaluate(Context).GetNumber));
end;

function TE_FunctionCall.Fac_1(Context: TContext; args: TExprList): TValue;
var accu,k,desiredFact: Number;
begin
  accu:= 1;
  desiredFact:= args[0].Evaluate(Context).GetNumber;
  k:= 2;
  while k<=desiredFact do begin
    accu := accu * k;
    k:= k+1;
  end;
  Result.SetNumber(accu);
end;

function TE_FunctionCall.Deg2Rad_1(Context: TContext; args: TExprList): TValue;
begin
  Result.SetNumber(DegToRad(args[0].Evaluate(Context).GetNumber));
end;

function TE_FunctionCall.Rad2Deg_1(Context: TContext; args: TExprList): TValue;
begin
  Result.SetNumber(RadToDeg(args[0].Evaluate(Context).GetNumber));
end;

function TE_FunctionCall.Sin_1(Context: TContext; args: TExprList): TValue;
begin
  Result.SetNumber(Sin(args[0].Evaluate(Context).GetNumber));
end;

function TE_FunctionCall.Cos_1(Context: TContext; args: TExprList): TValue;
begin
  Result.SetNumber(Cos(args[0].Evaluate(Context).GetNumber));
end;

function TE_FunctionCall.Tan_1(Context: TContext; args: TExprList): TValue;
begin
  Result.SetNumber(Tan(args[0].Evaluate(Context).GetNumber));
end;

function TE_FunctionCall.ArcSin_1(Context: TContext; args: TExprList): TValue;
begin
  Result.SetNumber(ArcSin(args[0].Evaluate(Context).GetNumber));
end;

function TE_FunctionCall.ArcCos_1(Context: TContext; args: TExprList): TValue;
begin
  Result.SetNumber(ArcCos(args[0].Evaluate(Context).GetNumber));
end;

function TE_FunctionCall.ArcTan_1(Context: TContext; args: TExprList): TValue;
begin
  Result.SetNumber(ArcTan(args[0].Evaluate(Context).GetNumber));
end;

function TE_FunctionCall.ArcTan_2(Context: TContext; args: TExprList): TValue;
begin
  Result.SetNumber(ArcTan2(args[0].Evaluate(Context).GetNumber,args[1].Evaluate(Context).GetNumber));
end;

function TE_FunctionCall.Sinh_1(Context: TContext; args: TExprList): TValue;
begin
  Result.SetNumber(Sinh(args[0].Evaluate(Context).GetNumber));
end;

function TE_FunctionCall.Cosh_1(Context: TContext; args: TExprList): TValue;
begin
  Result.SetNumber(Cosh(args[0].Evaluate(Context).GetNumber));
end;

function TE_FunctionCall.Tanh_1(Context: TContext; args: TExprList): TValue;
begin
  Result.SetNumber(Tanh(args[0].Evaluate(Context).GetNumber));
end;

function TE_FunctionCall.ArSinh_1(Context: TContext; args: TExprList): TValue;
begin
  Result.SetNumber(ArcSinh(args[0].Evaluate(Context).GetNumber));
end;

function TE_FunctionCall.ArCosh_1(Context: TContext; args: TExprList): TValue;
begin
  Result.SetNumber(ArcCosh(args[0].Evaluate(Context).GetNumber));
end;

function TE_FunctionCall.ArTanh_1(Context: TContext; args: TExprList): TValue;
begin
  Result.SetNumber(ArcTanh(args[0].Evaluate(Context).GetNumber));
end;

{ TE_ArgList }

function TE_ArgList.CollectAll: TExprList;
  procedure SubCollect(e: IExpression);
  var tmp: TExprList;
      i: integer;
      o: TExpression;
  begin
    if e=nil then
      exit;
    o:= e.GetObject;
    if o is TE_ArgList then begin
      tmp:= TE_ArgList(o).CollectAll;
      for i:= 0 to high(tmp) do begin
        SetLength(Result, Length(Result)+1);
        Result[high(Result)]:= tmp[i];
      end;
    end
    else begin
      SetLength(Result, Length(Result)+1);
      Result[high(Result)]:= e;
    end;
  end;
begin
  SetLength(Result, 0);
  SubCollect(LHS);
  SubCollect(RHS);
end;

function TE_ArgList.Evaluate(Context: TContext): TValue;
begin
  LHS.Evaluate(Context);
  Result:= RHS.Evaluate(Context);
end;

function TE_ArgList.StringForm: String;
var list: TExprList;
    i: integer;
begin
  list:= CollectAll;
  if Length(list)>0 then begin
    Result:= list[0].StringForm;
    for i:= 1 to high(list) do
      Result:= Result + ','+ list[i].StringForm;
    Result:= 'ArgList('+Result+')';
  end else
    Result:= '';
end;

{ TE_Subcontext }

constructor TE_Subcontext.Create(Name: string);
begin
  inherited Create;
  FName:= Name;
  Arguments:= nil;
end;

function TE_Subcontext.Evaluate(Context: TContext): TValue;
var ctx: TContext;
begin
  ctx:= TContext.Create(Context.System, Context);
  try
    Arguments.Evaluate(ctx);
    Result:= ctx.Value(FName);
  finally
    FreeAndNil(ctx);
  end;
end;

function TE_Subcontext.StringForm: String;
begin
  Result:= FName+'['+Arguments.StringForm+']';
end;

{ TE_Describe }

function TE_Describe.Evaluate(Context: TContext): TValue;
var name: string;
    e: IExpression;
begin
  name:= (RHS.GetObject as TE_ExprRef).FName;
  e:= Context.Definition(name);
  if Assigned(e) then
    Result.SetString(e.StringForm)
  else
    Result.SetString('<Unknown>');
end;

{ TE_AssignmentDynamic }

function TE_AssignmentDynamic.Evaluate(Context: TContext): TValue;
var name: string;
begin
  name:= (LHS.GetObject as TE_ExprRef).FName;
  Context.Define(name, RHS);
end;

{ TE_AssignmentStatic }

function TE_AssignmentStatic.Evaluate(Context: TContext): TValue;
var name: string;
    v: TValue;
begin
  name:= (LHS.GetObject as TE_ExprRef).FName;
  v:= RHS.Evaluate(Context);
  Context.DefineValue(name, v);
end;


{ TE_Power }

function TE_Power.Evaluate(Context: TContext): TValue;
begin
  Result.SetNumber(Power(LHS.Evaluate(Context).GetNumber, RHS.Evaluate(Context).GetNumber));
end;

{ TE_Multiplication }

function TE_Multiplication.Evaluate(Context: TContext): TValue;
begin
  Result.SetNumber(LHS.Evaluate(Context).GetNumber * RHS.Evaluate(Context).GetNumber);
end;

{ TE_Division }

function TE_Division.Evaluate(Context: TContext): TValue;
begin
  Result.SetNumber(LHS.Evaluate(Context).GetNumber / RHS.Evaluate(Context).GetNumber);
end;

{ TE_Modulus }

function TE_Modulus.Evaluate(Context: TContext): TValue;
begin
  Result.SetNumber(trunc(LHS.Evaluate(Context).GetNumber) mod trunc(RHS.Evaluate(Context).GetNumber));
end;

{ TE_Addition }

function TE_Addition.Evaluate(Context: TContext): TValue;
begin
  Result.SetNumber(LHS.Evaluate(Context).GetNumber + RHS.Evaluate(Context).GetNumber);
end;

{ TE_Subtraction }

function TE_Subtraction.Evaluate(Context: TContext): TValue;
begin
  Result.SetNumber(LHS.Evaluate(Context).GetNumber - RHS.Evaluate(Context).GetNumber);
end;

initialization
  GetLocaleFormatSettings(GetThreadLocale, NeutralFormatSettings);
  with NeutralFormatSettings do
  begin
    ThousandSeparator := #0;
    DecimalSeparator := '.';
    DateSeparator := '-';
    ShortDateFormat := 'yy-mm-dd';
    LongDateFormat := 'mmmm d, yyyy';
    TimeSeparator := ':';
  end;
end.
