{-----------------------------------------------------------------------------
 The TAU Math Kernel

 Main class TMathSystem, expression primitives and internal packages.
-----------------------------------------------------------------------------}
unit uMath;

interface

uses
  SysUtils, Classes, IniFiles, Windows, Contnrs,
  uMathIntf, Math, uFPUSupport;

type
  TFunctionPackage = class;
  TFunctionPackageClass = class of TFunctionPackage;
  TContext = class;

  TOperatorOptions = set of (ooUnary, ooUnparsed, ooFlat, ooFlatAssociative, ooUnpackInArguments, ooHoldPackedArguments);
  TMathSystem  = class(TIntfNoRefCount, IMathSystem)
  const
    CharQuote = '''';

    CharBraceOpen = '(';
    CharBraceClose = ')';
    CharContextOpen = '[';
    CharContextClose = ']';
    CharListOpen = '{';
    CharListClose = '}';
  private type     
    TInfixDefinition = record
      Operator: String[16];
      Precedence: integer;
      Options: TOperatorOptions;
      Pack: TFunctionPackage;
      Func: String[64];
    end;
    PInfixDefinition = ^TInfixDefinition;

    TTokenKind = (tokVoid, tokExpression, tokEmpty,
      tokNumber, tokString, tokFuncRef, tokExprRef, tokExprContext,
      tokBraceOpen, tokBraceClose, tokContextOpen, tokContextClose, tokListOpen, tokListClose, tokList,
      tokOperator);
    TToken = record
      Pos: integer;
      Value: string;
      Expr: IExpression;
      case Kind: TTokenKind of
        tokVoid: ();
        tokExpression: ();
        tokNumber: ();
        tokString: ();
        tokFuncRef: ();
        tokExprRef: ();
        tokExprContext: ();
        //  tokBraceOpen, tokBraceClose, tokContextOpen, tokContextClose, tokListOpen, tokListClose
        tokOperator: (OpDef: PInfixDefinition);
    end;
    TTokenList = array of TToken;

  private
    fOutput: IOutputProvider;

    fInfixTable,
    fOperatorTable: TList;
    fPackages: TObjectList;
    fContext,
    fConstants: IContext;
    function ApplyFlatOperators(ex: IExpression): IExpression;
  protected
    FEvaluationStack: TStringList;
    FNegateDefinition: PInfixDefinition;
    procedure EvaluationBegin(varname: string);
    procedure EvaluationEnd;
    procedure PredefineConstants(const Context: IContext);
    function Tokenize(const Expr: string): TTokenList;       
    procedure Fold(var Tokens: TTokenList; const L, R: integer);
  public
    constructor Create(const Output: IOutputProvider);
    destructor Destroy; override;

    property Output: IOutputProvider read fOutput;
    property Context: IContext read fContext;
    procedure NewContext(const Name: string);
    procedure DropContext;

    function RegisterPackage(const Package: TFunctionPackage): boolean;
    function RegisterAsInfix(Operator: String; Precedence: integer; Options: TOperatorOptions; Pack: TFunctionPackage; Func: String): boolean;
    function GetPackageInstance(const PackageClass: TFunctionPackageClass): TFunctionPackage;

    function Evaluate(const Expr: IExpression): IExpression;
    procedure Run(const Expr: String);
    // IMathSystem
    function Parse(const Expr: String): IExpression;
    function HasPackage(const PackageClassName: string): Boolean;
    function HasFunction(const FuncName: String; const ArgCount: integer): IPackagedFunction;
  end;

  TContextWrappedOutput = class(TInterfacedObject, IOutputProvider)
  private
    FOutput: IOutputProvider;
    FContext: TContext;
  public
    constructor Create(const Wrap: IOutputProvider; const Context: TContext);
    destructor Destroy; override;
    // IOutputProvider
    procedure Input(const Line: string);
    procedure Hint(const Line: string; Params: array of const);
    procedure Error(const Line: string; Params: array of const);
    procedure Result(const Line: string);
    procedure Clear;
  end;

  TContext = class(TInterfacedObject, IContext)
  private
    FExpressions: THashedStringList;
    FParent: IContext;
    FSystem: TMathSystem;
    FSilent: boolean;
    FOutWrap: IOutputProvider;
    FContextName: string;
    function GetCount: integer;
    function GetName(index: integer): string;
  public
    class function SystemFrom(const ref: IContext): TMathSystem;

    constructor Create(ASystem: TMathSystem; AParent: TContext); overload;
    constructor Create(AParent: IContext); overload;
    destructor Destroy; override;
    property Parent: IContext read FParent;
    function Bake: TContext;
    property System: TMathSystem read FSystem;
    property ContextName: string read FContextName write FContextName;

    property Count: integer read GetCount;
    property Name[index: integer]: string read GetName;
    // IContext
    function NativeObject: TObject;
    procedure Define(const Name: string; Expression: IExpression);
    procedure Undefine(const Name: string);
    procedure Clear;
    function Definition(const Name: string): IExpression;
    function Defines(const Name: string): boolean;
    function Output: IOutputProvider;
    function RawOutput: IOutputProvider;
    function GetSystem: IMathSystem;
    procedure SetSilent(const Silent: Boolean);
    function GetSilent: Boolean;
    property Silent: boolean read GetSilent write SetSilent;
    function Combine(const UseDefine: boolean): IExpression;
  end;

  TExpression = class(TInterfacedObject, IExpression)
  private
    procedure SetArgument(Index: integer; const Value: IExpression);
  protected
    Arguments: TExprList;
    function StringOfArgs(fmt: TStringFormat; Delim: String): String;
  public
    constructor Create;
    // IExpression
    function NativeObject: TObject;
    function Evaluate(const Context: IContext): IExpression; virtual; abstract;
    function Clone(Deep: Boolean): IExpression; virtual; abstract;
    function Represents(const IID: TGUID; out Intf): boolean; overload;
    function Represents(const IID: TGUID): boolean; overload;
    function IsClass(const Cls: TClass): Boolean;

    procedure SetArgs(const aArgs: array of IExpression);
    function GetArgs: TExprList;
    function ArgCount: Integer;
    function GetArgument(Index: Integer): IExpression;
    property Arg[Index: integer]: IExpression read GetArgument write SetArgument;
  end;

////////////////////////////////////////////////////////////////////////////////
//   Function Call Related
////////////////////////////////////////////////////////////////////////////////

  TDynamicArgument = record
    Name: string;
    Value: IExpression;
  end;

  TDynamicArguments = class
  private
    FItems: array of TDynamicArgument;
    function GetValue(Name: string): IExpression;
  protected
    procedure Add(Name: string; Value: IExpression);
  public
    constructor Create(Args: TExprList; FromIndex: integer; Context: IContext);
    function IsSet(Name: string): boolean;
    function GetDefault(Name: string; Default: IExpression): IExpression;
    property Value[Name: string]: IExpression read GetValue; default;
  end;

  TUDFHeader = function(Context: IContext; Args: TExprList): IExpression of object;
  TUDFHeaderOptions = function(Context: IContext; Args: TExprList; Options: TDynamicArguments): IExpression of object;

  TPackagedFunction = class(TInterfacedObject, IPackagedFunction)
  private
    FName: string;
    FPtr: TUDFHeader;
    FDynamicFrom: integer;
  public
    constructor Create(const Name: string; const Ptr: TUDFHeader; const DynamicFrom: integer);
    function Call(Context: IContext; Args: TExprList): IExpression;
    function GetName: String;
    function IsDynamic: boolean;
  end;

  TFunctionPackage = class
  protected
    procedure OnImport(const MS: TMathSystem); virtual;
    procedure PublishedMethods(Names: TStringList);
  public
    function FunctionExists(FunctionName: string; ParamCount: integer): boolean;
    function GetFunction(FunctionName: string; ParamCount: integer; out DynFrom: integer): TUDFHeader;
  end;


////////////////////////////////////////////////////////////////////////////////
//   Expression Primitives
////////////////////////////////////////////////////////////////////////////////

  TE_SymbolRef = class(TExpression, ISymbolReference, IStringConvertible)
  private
    FName: string;
    function GetName: string;
  public
    constructor Create(AName: string);
    function Evaluate(const Context: IContext): IExpression; override;
    function Clone(Deep: Boolean): IExpression; override;
    // ISymbolReference
    property Name: string read GetName;
    // IStringConvertible
    function AsString(const Format: TStringFormat): String;
  end;

  TE_Call = class(TExpression, IFunctionCall, IStringConvertible)
  private
    FName: string;      
    FFunctionBound: Boolean;      
    FFunction: IPackagedFunction;
    FCreatedFrom: Pointer;
    class function CheckSysCalls(StringForm: string): Boolean;
    function GetName: string;
  public
    constructor Create(AName: string);
    function Evaluate(const Context: IContext): IExpression; override;
    function Clone(Deep: Boolean): IExpression; override;
    // IFunctionCall
    property Name: string read GetName;
    // IStringConvertible
    function AsString(const Format: TStringFormat): String;
  end;

  TE_Subcontext = class(TExpression, IStringConvertible)
  private
    FName: string;
  public
    constructor Create(AName: string);
    property Name: string read FName;
    function Evaluate(const Context: IContext): IExpression; override;
    function Clone(Deep: Boolean): IExpression; override;
    // IStringConvertible
    function AsString(const Format: TStringFormat): String;
  end;

////////////////////////////////////////////////////////////////////////////////
//   Internal Packages
////////////////////////////////////////////////////////////////////////////////

  TPackageCore = class(TFunctionPackage)
  protected
    procedure OnImport(const MS: TMathSystem); override;
  published
    function _dump_1(Context: IContext; Args: TExprList): IExpression;
    function _describe_1(Context: IContext; Args: TExprList): IExpression;   
    function TypeOf_1(Context: IContext; Args: TExprList): IExpression;

    function Undef_1(Context: IContext; args: TExprList): IExpression;
    function New_0(Context: IContext; args: TExprList): IExpression;
    function New_1(Context: IContext; args: TExprList): IExpression;
    function Drop_0(Context: IContext; args: TExprList): IExpression;
    function Clear_0(Context: IContext; args: TExprList): IExpression;

    function const_1(Context: IContext; args: TExprList): IExpression;
    function constinfo_0(Context: IContext; args: TExprList): IExpression;
    function constinfo_1(Context: IContext; args: TExprList): IExpression;

    //function nvl_2(Context: IContext; args: TExprList): IExpression;
    function Hold_1(Context: IContext; args: TExprList): IExpression;
    function Eval_1(Context: IContext; args: TExprList): IExpression;
    function AbsoluteTime_1(Context: IContext; args: TExprList): IExpression;
  end;

  TPackageAlgebra = class(TFunctionPackage)
  protected
    procedure OnImport(const MS: TMathSystem); override;
  published
    function _char_1(Context: IContext; Args: TExprList): IExpression;
    function _negate_1(Context: IContext; Args: TExprList): IExpression;
    function _pow_2(Context: IContext; Args: TExprList): IExpression;
    function _mult_N(Context: IContext; Args: TExprList): IExpression;
    function _div_2(Context: IContext; Args: TExprList): IExpression;
    function _mod_2(Context: IContext; Args: TExprList): IExpression;
    function _plus_N(Context: IContext; Args: TExprList): IExpression;
    function _subtract_N(Context: IContext; Args: TExprList): IExpression;

    function _assign_2(Context: IContext; Args: TExprList): IExpression;
    function _define_2(Context: IContext; Args: TExprList): IExpression;
    function _comma_N(Context: IContext; Args: TExprList): IExpression;
  end;

const
  STR_FORMAT_INPUT = TStringFormat(1);
  STR_FORMAT_OUTPUT = TStringFormat(2);
  STR_FORMAT_INPUT_EXPANDED = TStringFormat(3);

var
  NeutralFormatSettings: TFormatSettings;

function NumberToStr(const Value: Number; ShowThousands: boolean): string;
function IntegerToStr(const Value: MTInteger; ShowThousands: boolean): string;

function MakeArgs(const Args: array of IExpression): TExprList;

resourcestring
  sConstants = 'Constants';
  sWork = 'Work';

  sCannotConvertExpression = 'Cannot convert expression to type %s';
  sUnsupportedOperation = 'Unsupported operation: %s';

implementation

uses
  StrUtils, uMathValues, uMathConstants, uMathDimensions;

type
  PInfixDefinition = TMathSystem.PInfixDefinition;

function InsertThousandSep(const Value: string; aFrom, aTo, aDecimalPoint: integer): string;
var
  p: integer;
begin
  Result:= Value;

  p:= aDecimalPoint;
  inc(p, 3);

  while p < aTo do begin
    Insert(NeutralFormatSettings.ThousandSeparator, Result, p + 1);
    inc(p, 4);
    inc(aTo);
  end;

  p:= aDecimalPoint;
  dec(p, 3);

  while p > aFrom do begin
    Insert(NeutralFormatSettings.ThousandSeparator, Result, p);
    dec(p, 3);
  end;
end;

function NumberToStr(const Value: MTFloat; ShowThousands: boolean): string;
var
  p, e: integer;
begin
  Result:= FloatToStrF(Value, ffGeneral, 22, 18, NeutralFormatSettings);
  if Showthousands then begin
    e:= Pos('E', Result);
    if e = 0 then
      e:= length(Result);

    p:= Pos(NeutralFormatSettings.DecimalSeparator, Result);
    if p = 0 then
      p:= e + 1;

    Result:= InsertThousandSep(Result, 1, e, p);
  end;
end;

function IntegerToStr(const Value: MTInteger; ShowThousands: boolean): string;
begin
  Result:= IntToStr(Value);
  if Showthousands then begin
    Result:= InsertThousandSep(Result, 1, length(Result), length(Result)+1);
  end;
end;


function MakeArgs(const Args: array of IExpression): TExprList;
var
  i: integer;
begin
  SetLength(Result, length(Args));
  for i:= 0 to high(Args) do
    Result[i]:= Args[i];
end;

{ TMathSystem }

constructor TMathSystem.Create(const Output: IOutputProvider);
var
  ctx: TContext;
begin
  inherited Create;     
  FEvaluationStack:= TStringList.Create;
  fOutput:= Output;
  fPackages:= TObjectList.Create(true);
  fInfixTable:= TList.Create;
  fOperatorTable:= TList.Create;

  ctx:= TContext.Create(Self, nil);
  ctx.ContextName:= sConstants;
  FContext:= ctx;
  FConstants:= ctx;

  NewContext(sWork);

  RegisterPackage(TPackageCore.Create);
  RegisterPackage(TPackageAlgebra.Create);
  PredefineConstants(fConstants);
end;

destructor TMathSystem.Destroy;
var
  i: integer;
begin
  for i:= 0 to fInfixTable.Count-1 do
    Dispose(fInfixTable[i]);
  FreeAndNil(fInfixTable);
  FreeAndNil(fOperatorTable);
  FreeAndNil(fPackages);      
  FreeAndNil(FEvaluationStack);
  fContext:= nil;
  fOutput:= nil;
  inherited;
end;

function TMathSystem.RegisterPackage(const Package: TFunctionPackage): boolean;
begin
  Result:= fPackages.IndexOf(Package) < 0;
  if Result then
    Result:= fPackages.Add(Package) >= 0;
  if Result then
    Package.OnImport(Self);
end;

function TMathSystem.GetPackageInstance(const PackageClass: TFunctionPackageClass): TFunctionPackage;
var
  i: integer;
begin
  for i:= 0 to fPackages.Count-1 do begin
    if fPackages[i] is PackageClass then begin
      Result:= TFunctionPackage(fPackages[i]);
      exit;
    end;
  end;
  Result:= nil;
end;

function LSC_InfixByLength(Item1, Item2: Pointer): Integer;
begin
  Result:= Length(PInfixDefinition(Item2)^.Operator) - Length(PInfixDefinition(Item1)^.Operator);
end;

function LSC_OperatorByPrecedence(Item1, Item2: Pointer): Integer;
begin
  Result:= PInfixDefinition(Item1)^.Precedence - PInfixDefinition(Item2)^.Precedence; 
end;

function TMathSystem.RegisterAsInfix(Operator: String; Precedence: integer; Options: TOperatorOptions; Pack: TFunctionPackage; Func: String): boolean;
var
  i: integer;
  inf: PInfixDefinition;
begin
  Result:= false;
  for i:= 0 to fInfixTable.Count-1 do begin
    inf:= PInfixDefinition(fInfixTable[i]);
    if (inf.Operator = Operator) and not (ooUnparsed in (inf.Options+Options)) then
      exit;
  end;
  new(inf);
  inf^.Operator:= Operator;
  inf^.Precedence:= Precedence;
  inf^.Options:= Options;
  inf^.Pack:= Pack;
  inf^.Func:= Func;
  fInfixTable.Add(inf);
  fInfixTable.Sort(LSC_InfixByLength);
  fOperatorTable.Add(inf);
  fOperatorTable.Sort(LSC_OperatorByPrecedence);
end;

procedure TMathSystem.DropContext;
begin
  if TContext(FContext.NativeObject).Parent <> FConstants then begin
    fContext:= TContext(FContext.NativeObject).Parent;
  end else
    raise EMathSysError.Create('Cannot drop this context.');
end;

procedure TMathSystem.NewContext(const Name: string);
var
  cont: TContext;
begin
  cont:= TContext.Create(FContext);
  cont.ContextName:= Name;
  FContext:= cont as IContext;
end;

procedure TMathSystem.EvaluationBegin(varname: string);
begin
  if FEvaluationStack.IndexOf(varname) > -1 then
    raise EMathSysError.CreateFmt('Evaluation aborted at maximum depth: cyclic reference found for "%s".', [varname])
  else
    FEvaluationStack.Add(varname);
end;

procedure TMathSystem.EvaluationEnd;
begin
  FEvaluationStack.Delete(FEvaluationStack.Count - 1);
end;

function TMathSystem.Tokenize(const Expr: string): TTokenList;
var
  p, i: integer;
  t: TToken;
  inf: PInfixDefinition;

  procedure ParseNumber(var Nr: TToken);
  var
    mode: (tmNumberSign, tmNumber, tmNumberDecimals, tmNumberExponentSign, tmNumberExponent);
    data: string;
  begin
    case Expr[p] of
      '0'..'9': mode:= tmNumber;
      '-': mode:= tmNumberSign;
      '.': mode:= tmNumberDecimals;
    else
      exit;
    end;
    data:= Expr[p];
    inc(p);

    while p <= Length(Expr) do begin
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
          else if Expr[p] = NeutralFormatSettings.DecimalSeparator then begin
            data:= data + Expr[p];
            mode:= tmNumberDecimals;
          end else if Expr[p] in ['e', 'E'] then begin
            data:= data + Expr[p];
            mode:= tmNumberExponentSign;
          end else
            break;
        tmNumberDecimals:
          if Expr[p] in ['0'..'9'] then
            data:= data + Expr[p]
          else if Expr[p] in ['e', 'E'] then begin
            data:= data + Expr[p];
            mode:= tmNumberExponentSign;
          end else
            break;
        tmNumberExponentSign: begin
            if Expr[p] in ['0'..'9', '-'] then begin
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
      end;
      inc(p);
    end;
    Nr.Kind:= tokNumber;
    Nr.Value:= data;
  end;

  procedure ParseString(var Str: TToken);
  var
    data: string;
  begin
    data:= Expr[p];
    inc(p);
    while p <= Length(Expr) do begin
      if (Expr[p] <> CharQuote) then
        data:= data + Expr[p]
      else begin
        data:= data + Expr[p];
        inc(p);
        if (p <= length(Expr)) and (Expr[p] = CharQuote) then
          data:= data + Expr[p]
        else
          break;
      end;
      inc(p);
    end;
    Str.Kind:= tokString;
    Str.Value:= data;
  end;

  procedure ParseIdentifier(var Id: TToken);
  var
    data: string;
  begin
    data:= Expr[p];
    inc(p);
    while p <= Length(Expr) do begin
      if Expr[p] in ['a'..'z', 'A'..'Z', '_', '0'..'9'] then
        data:= data + Expr[p]
      else
        Break;
      inc(p);
    end;
    case Expr[p] of
      CharBraceOpen: Id.Kind:= tokFuncRef;
      CharContextOpen: Id.Kind:= tokExprContext;
    else
      Id.Kind:= tokExprRef;
    end;
    Id.Value:= data;
  end;

begin        
  SetLength(Result, 0);
  p:= 1;
  while p <= length(expr) do begin
    t.Value:= '';
    FillChar(t, sizeof(t), 0);
    t.Pos:= p;
    case Expr[p] of
      ' ',#9,#13,#10: begin
          inc(p);
          continue;
        end;
      '0'..'9',
        //        '-',
      '.': ParseNumber(t);
      CharQuote: ParseString(t);
      CharBraceOpen: begin
          t.Kind:= tokBraceOpen;
          inc(p);
        end;
      CharBraceClose: begin
          t.Kind:= tokBraceClose;
          inc(p);
        end;
      CharContextOpen: begin
          t.Kind:= tokContextOpen;
          inc(p);
        end;
      CharContextClose: begin
          t.Kind:= tokContextClose;
          inc(p);
        end;
      CharListOpen: begin
          t.Kind:= tokListOpen;
          inc(p);
        end;
      CharListClose: begin
          t.Kind:= tokListClose;
          inc(p);
        end;
      'a'..'z', 'A'..'Z'{, '_'}: ParseIdentifier(t);
    else
      for i:= 0 to fInfixTable.Count-1 do begin
        inf:= PInfixDefinition(fInfixTable[i]);
        if not (ooUnparsed in inf.Options) and
          (inf.Operator > '') and
          SameText(inf.Operator, Copy(Expr, p, length(inf.Operator))) then begin
          t.Kind:= tokOperator;
          t.Value:= inf.Operator;
          t.OpDef:= inf;
          inc(p, length(t.Value));
          break;
        end;
      end;
      if t.Kind = tokVoid then
        raise ESyntaxError.CreateFmt(p,'Unexpected Character %s', [Expr[p]]);
    end;

    SetLength(Result, Length(Result) + 1);
    Result[high(Result)]:= t;
  end;
end;

procedure TMathSystem.Fold(var Tokens: TTokenList; const L, R: integer);
var
  i, A, ll, rr: integer;
  tmp: IExpression;
  inf,exi: PInfixDefinition;
  b: pchar;

  function NextR(k: integer): integer;
  var
    j: integer;
  begin
    Result:= -1;
    for j:= k + 1 to R do
      if Tokens[j].Kind <> tokVoid then begin
        Result:= j;
        exit;
      end;
  end;
  function NextL(k: integer): integer;
  var
    j: integer;
  begin
    Result:= -1;
    for j:= k - 1 downto L do
      if Tokens[j].Kind <> tokVoid then begin
        Result:= j;
        exit;
      end;
  end;

  procedure ProcessBraces(const Open, Close: TTokenKind; const Str: string;
                  SetTo: TTokenKind = tokVoid; IfEmpty: TTokenKind = tokEmpty);
  var
    i: integer;
  begin
    repeat
      A:= -1;
      for i:= L to R do begin
        if Tokens[i].Kind = Open then
          A:= i
        else if Tokens[i].Kind = Close then begin
          if A >= 0 then begin
            if A = I - 1 then begin
              Tokens[A].Kind:= IfEmpty;
            end else begin
              Fold(Tokens, A + 1, I - 1);
              Tokens[A].Kind:= SetTo;
            end;
            Tokens[I].Kind:= tokVoid;
          end else
            raise ESyntaxError.CreateFmt(Tokens[i].Pos, 'Closing %s never opened', [Str]);
          A:= -2;
          break;
        end;
      end;
      if A >= 0 then
        raise ESyntaxError.CreateFmt(Tokens[A].Pos, '%s is never closed', [Str]);
    until A = -1;
  end;
begin
  //given stupid values?
  if R < L then
    exit;

  //then subcontexts
  ProcessBraces(tokListOpen, tokListClose, '{}', tokList, tokList);
  //collapse braces first
  ProcessBraces(tokBraceOpen, tokBraceClose, '()');
  //then subcontexts
  ProcessBraces(tokContextOpen, tokContextClose, '[]');

  // simple expressions
  for i:= L to R do
    case Tokens[i].Kind of
      tokNumber: begin
          Tokens[i].Kind:= tokExpression;
          Tokens[i].Expr:= TValueFactory.NumberFromString(Tokens[i].Value, NeutralFormatSettings);
        end;
      tokString: begin
          Tokens[i].Kind:= tokExpression;
          b:= PChar(Tokens[i].Value);
          Tokens[i].Expr:= TValueString.Create(AnsiExtractQuotedStr(b, CharQuote));
        end;
      tokExprRef: begin
          Tokens[i].Kind:= tokExpression;
          Tokens[i].Expr:= TE_SymbolRef.Create(Tokens[i].Value);
        end;
      tokExprContext: begin
          Tokens[i].Kind:= tokExpression;
          tmp:= TE_Subcontext.Create(Tokens[i].Value);
          A:= NextR(i);
          if (A>=0) then begin
            if Tokens[A].Kind <> tokEmpty then begin
              tmp.SetArgs([Tokens[A].Expr]);
            end;
            Tokens[A].Expr:= nil;
            Tokens[a].Kind:= tokVoid;
          end;
          Tokens[i].Expr:= tmp;
        end;
      tokFuncRef: begin
          Tokens[i].Kind:= tokExpression;
          tmp:= TE_Call.Create(Tokens[i].Value);
          A:= NextR(i);
          if (A>=0) then begin
            if Tokens[A].Kind <> tokEmpty then begin
              tmp.SetArgs([Tokens[A].Expr]);
            end;
            Tokens[A].Expr:= nil;
            Tokens[a].Kind:= tokVoid;
          end;
          Tokens[i].Expr:= tmp;
        end;
      tokList: begin
          Tokens[i].Kind:= tokExpression;
          tmp:= TValueList.Create;
          A:= NextR(i);
          if (A=I+1) then begin
            if Tokens[A].Kind <> tokEmpty then begin
              tmp.SetArgs([Tokens[A].Expr]);
            end;
            Tokens[A].Expr:= nil;
            Tokens[a].Kind:= tokVoid;
          end;
          Tokens[i].Expr:= tmp;
        end;
    end;

  //finally, operators
  // Note: this wastes some extra iterations per Prio-Class, just ignore that, okay?
  for A:= 0 to fOperatorTable.Count-1 do begin
    inf:= PInfixDefinition(fOperatorTable[A]);
    if ooUnparsed in  inf^.Options then
      continue;
    for i:= L to R do
      if (Tokens[i].Kind = tokOperator) and
        (Tokens[i].OpDef^.Precedence = inf^.Precedence) then begin
        exi:= Tokens[i].OpDef;
        rr:= NextR(i);
        ll:= NextL(i);

        if rr < 0 then
          raise ESyntaxError.CreateFmt(Tokens[i].Pos, 'Operator has no RHS', []);

        if Tokens[rr].Kind <> tokExpression then
          raise ESyntaxError.CreateFmt(Tokens[rr].Pos, 'Expected expression, found %s', [Tokens[rr].Value]);

        if (exi^.Func = '_subtract') and
          ((ll < 0) or (Tokens[ll].Kind <> tokExpression)) then begin
          tmp:= TE_Call.Create('_negate');
          TE_Call(tmp.NativeObject).FCreatedFrom:= FNegateDefinition;
          tmp.SetArgs([Tokens[rr].Expr]);
        end else begin
          tmp:= TE_Call.Create(exi^.Func);
          TE_Call(tmp.NativeObject).FCreatedFrom:= exi;

          if ooUnary in exi^.Options then
            tmp.SetArgs([Tokens[rr].Expr])
          else begin
            if ll < 0 then
              raise ESyntaxError.CreateFmt(Tokens[i].Pos, 'Operator has no LHS', []);
            if Tokens[ll].Kind <> tokExpression then
              raise ESyntaxError.CreateFmt(Tokens[ll].Pos, 'Expected expression, found %s', [Tokens[ll].Value]);
            tmp.SetArgs([Tokens[ll].Expr, Tokens[rr].Expr]);
            Tokens[ll].Kind:= tokVoid;
          end;
        end;
        Tokens[rr].Kind:= tokVoid;
        Tokens[i].Kind:= tokExpression;
        Tokens[i].Expr:= tmp;
      end;
  end;

  //shift to L position if neccessary
  i:= NextR(L - 1);
  if i <> L then begin
    Tokens[L]:= Tokens[i];
    Tokens[i].Kind:= tokVoid;
  end;
end;

function TMathSystem.ApplyFlatOperators(ex: IExpression): IExpression;
var
  i: integer;
  inf: PInfixDefinition;
  procedure Flatten(var x: IExpression);
  var
    k,m,c: integer;
    el: TExprList;
    xc,ac: IFunctionCall;
  begin
    // find any node that consists entirely of "inf" by first flattening out children, then see if anything happened
    for k:= 0 to x.ArgCount-1 do
      Flatten(TExpression(x.NativeObject).Arguments[k]);
    if x.Represents(IFunctionCall, xc) and ((xc.Name = inf.Func)) then begin
      SetLength(el,0);
      c:= 0;
      if ooFlatAssociative in inf^.Options then begin
        // ordering does not matter
        for k:= 0 to x.ArgCount-1 do begin
          if x.Arg[k].Represents(IFunctionCall, ac) and (ac.Name = inf.Func) then begin
            SetLength(el, c + ac.ArgCount);
            for m:= 0 to ac.ArgCount-1 do
              el[c + m]:= ac.Arg[m];
            inc(c, ac.ArgCount);
          end else begin
            SetLength(el, c + 1);
            el[c]:= x.Arg[k];
            inc(c);
          end;
        end;
      end else begin
        // order matters, only flatten first element
        if x.ArgCount > 0 then begin
          if x.Arg[0].Represents(IFunctionCall, ac) and (ac.Name = inf.Func) then begin
            SetLength(el, c + ac.ArgCount);
            for m:= 0 to ac.ArgCount-1 do
              el[c + m]:= ac.Arg[m];
            inc(c, ac.ArgCount);
          end else begin
            SetLength(el, c + 1);
            el[c]:= x.Arg[0];
            inc(c);
          end;
          // keep the rest
          for k:= 1 to x.ArgCount-1 do begin
            SetLength(el, c + 1);
            el[c]:= x.Arg[k];
            inc(c);
          end;
        end;
      end;

      x.SetArgs(el);
    end;

    // should we unpack, or is x one that will never be unpacked?
    if (ooUnpackInArguments in inf.Options) and
       (x.ArgCount = 1) and
       (x.Arg[0].Represents(IFunctionCall, ac) and (ac.Name = inf.Func)) and
       not (x.IsClass(TE_Call) and Assigned(TE_Call(x.NativeObject).FCreatedFrom) and (ooHoldPackedArguments in PInfixDefinition(TE_Call(x.NativeObject).FCreatedFrom).Options)) then begin
      x.SetArgs(TE_Call(ac.NativeObject).Arguments);
    end;
  end;
begin
  for i:= 0 to fOperatorTable.Count-1 do begin
    inf:= PInfixDefinition(fOperatorTable[i]);
    if ooFlat in inf^.Options then begin
      Flatten(ex);
    end;
  end;
  Result:= ex;
end;

function TMathSystem.Parse(const Expr: String): IExpression;
var
  Tokens: TTokenList;
  i: integer;

  function CheckResults: boolean;
  var
    i: integer;
  begin
    Result:= length(Tokens) > 0;
    if not Result then
      exit;
    for i:= 1 to high(Tokens) do
      if Tokens[i].Kind <> tokVoid then
        raise ESyntaxError.CreateFmt(Tokens[i].Pos, 'Leftover token', []);
    Result:= true;
  end;

begin
  Result:= nil;
  FNegateDefinition:= nil;
  for i:= 0 to fInfixTable.Count-1 do
    if PInfixDefinition(fInfixTable[i]).Func='_negate' then begin
      FNegateDefinition:= PInfixDefinition(fInfixTable[i]);
      break;
    end;
  if not Assigned(FNegateDefinition) then
    raise EParserError.Create('Could not find definition of negate operator!');

  Tokens:= Tokenize(Expr);
  Fold(Tokens, 0, high(Tokens));
  if CheckResults then begin
    Result:= Tokens[0].Expr;
    Result:= ApplyFlatOperators(Result);
  end;
end;

function TMathSystem.Evaluate(const Expr: IExpression): IExpression;
var
  sc: IStringConvertible;
begin
  FEvaluationStack.Clear;
  if Expr.Represents(IStringConvertible, sc) and
     not (TE_Call.CheckSysCalls(sc.AsString(STR_FORMAT_INPUT))) then
    exit;

  Result:= Expr.Evaluate(FContext);
  if not Assigned(Result) then
    Result:= TValueUnassigned.Create;
end;

procedure TMathSystem.Run(const Expr: String);
var
  x, r: IExpression;
  sc: IStringConvertible;
begin
  try
    x:= Parse(Expr);
    if Assigned(x) then begin
      r:= x.Evaluate(Context);
      if Assigned(r) then begin
        Context.Define('ans', r);
        if r.Represents(IStringConvertible, sc) then
          Output.Result(sc.AsString(STR_FORMAT_OUTPUT));
      end else
        Output.Error('Expression did not return anything',[]);
    end;   
  except          
    on e: ESyntaxError do begin
      Output.Error('%s: %s',[e.ClassName, e.Message]);
      Output.Error('At Position %d: %s',[e.Position, Copy(Expr, e.Position, 5)]);
    end;
    on e: EMathSysError do
      Output.Error(E.Message, []);
  end;
end;

procedure TMathSystem.PredefineConstants(const Context: IContext);
var
  setter: IExpression;
  code: string;
begin
  Code:= '0';
  Code:= Code + Format(',%s=const(''%0:s'')',['tau']);   
  Code:= Code + Format(',%s=const(''%0:s'')',['pi']);
  Code:= Code + Format(',%s=const(''%0:s'')',['e']);
  setter:= Parse(Code);
  setter.Evaluate(Context);
end;

function TMathSystem.HasPackage(const PackageClassName: string): Boolean;
var
  i: integer;
begin
  Result:= false;
  for i:= 0 to fPackages.Count-1 do
    if fPackages[i].ClassName = PackageClassName then begin
      Result:= True;
      exit;
    end;
end;

function TMathSystem.HasFunction(const FuncName: String; const ArgCount: integer): IPackagedFunction;
var
  i, fdyn: integer;
  func: TUDFHeader;
begin
  Result:= nil;
  for i:= 0 to fPackages.Count-1 do begin
    func:= TFunctionPackage(fPackages[i]).GetFunction(FuncName, ArgCount, fdyn);
    if Assigned(func) then
      Result:= TPackagedFunction.Create(FuncName, func, fdyn);
  end;
end;

{ TContextWrappedOutput }

constructor TContextWrappedOutput.Create(const Wrap: IOutputProvider; const Context: TContext);
begin
  inherited Create;
  FOutput:= Wrap;
  FContext:= Context;
end;

destructor TContextWrappedOutput.Destroy;
begin
  FOutput:= nil;
  inherited;
end;

procedure TContextWrappedOutput.Error(const Line: string; Params: array of const);
begin
  FOutput.Error(Line,Params);
end;

procedure TContextWrappedOutput.Hint(const Line: String; Params: array of Const);
begin
  if not FContext.Silent then
    FOutput.Hint(Line,Params);
end;

procedure TContextWrappedOutput.Input(const Line: string);
begin
  FOutput.Input(Line);
end;

procedure TContextWrappedOutput.Result(const Line: string);
begin
  FOutput.Result(Line);
end;

procedure TContextWrappedOutput.Clear;
begin
  FOutput.Clear;
end;

{ TContext }

class function TContext.SystemFrom(const ref: IContext): TMathSystem;
begin
  Result:= TContext(ref.NativeObject).System;
end;

constructor TContext.Create(ASystem: TMathSystem; AParent: TContext);
begin
  inherited Create;
  FExpressions:= THashedStringList.Create;
  FParent:= AParent;
  FSystem:= ASystem;
  FSilent:= false;
  FOutWrap:= TContextWrappedOutput.Create(FSystem.Output, Self);
end;

constructor TContext.Create(AParent: IContext);
begin
  Create(SystemFrom(AParent), TContext(AParent.NativeObject));
end;

destructor TContext.Destroy;
var
  i: Integer;
begin
  for i:= FExpressions.Count - 1 downto 0 do
    Undefine(FExpressions[i]);
  FreeAndNil(FExpressions);
  inherited;
end;

procedure TContext.Define(const Name: string; Expression: IExpression);
var
  intf: IExpression;
  i: integer;
begin
  i:= FExpressions.IndexOf(Name);
  if i >= 0 then begin
    intf:= IExpression(Pointer(FExpressions.Objects[i]));
    intf._Release;
    FExpressions.Objects[i]:= TObject(Pointer(Expression));
    // only warn on non-system variables
    if not FSilent and not SameText(Name, 'ans') then
      FSystem.Output.Hint('Reassigned Variable: %s', [Name]);
  end else
    FExpressions.AddObject(Name, TObject(Pointer(Expression)));
  Expression._AddRef;
end;

procedure TContext.Undefine(const Name: string);
var
  intf: IExpression;
  i: Integer;
begin
  i:= FExpressions.IndexOf(Name);
  if i >= 0 then begin
    intf:= IExpression(Pointer(FExpressions.Objects[i]));
    intf._Release;
    FExpressions.Delete(i);
  end;
end;

function TContext.Definition(const Name: string): IExpression;
var
  i: integer;
begin
  i:= FExpressions.IndexOf(Name);
  if i >= 0 then begin
    Result:= IExpression(Pointer(FExpressions.Objects[i]));
  end else begin
    if Assigned(FParent) then
      Result:= FParent.Definition(Name)
    else
      Result:= nil;
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

function TContext.Defines(const Name: string): boolean;
begin
  Result:= FExpressions.IndexOf(Name) > -1;
end;

function TContext.Bake: TContext;
  procedure Cook(C: TContext);
  var
    i: integer;
    n: string;
  begin
    if Assigned(C.Parent) then
      Cook(TContext(C.Parent.NativeObject));
    for i:= 0 to C.Count - 1 do begin
      n:= C.Name[i];
      Result.Define(n, C.Definition(n));
    end;
  end;
begin
  Result:= TContext.Create(FSystem, nil);
  try
    Result.Silent:= true;
    Cook(Self);
  except
    FreeAndNil(Result);
  end;
end;

function TContext.NativeObject: TObject;
begin
  Result:= Self;
end;

function TContext.Output: IOutputProvider;
begin
  Result:= FOutWrap;
end;

function TContext.RawOutput: IOutputProvider;
begin
  Result:= FSystem.Output;
end;

procedure TContext.SetSilent(const Silent: Boolean);
begin
  FSilent:= Silent;
end;

function TContext.Combine(const UseDefine: boolean): IExpression;
var
  ret: array of IExpression;
  i: integer;
  ass: IExpression;
begin
  SetLength(ret, Count);
  for i:= 0 to Count-1 do begin
    if UseDefine then
      ass:= FSystem.Parse('a:=b')
    else
      ass:= FSystem.Parse('a=b');
    ass.SetArgs([
      TE_SymbolRef.Create(FExpressions[i]),
      IExpression(Pointer(FExpressions.Objects[i]))
    ]);
    ret[i]:= ass;
  end;
  Result:= TValueList.CreateAs(ret);
end;

procedure TContext.Clear;
var
  intf: IExpression;
  i: Integer;
begin
  for i:= FExpressions.Count - 1 downto 0 do begin
    intf:= IExpression(Pointer(FExpressions.Objects[i]));
    intf._Release;
    FExpressions.Delete(i);
  end;
end;

function TContext.GetSystem: IMathSystem;
begin
  Result:= FSystem as IMathSystem;
end;

function TContext.GetSilent: Boolean;
begin
  Result:= FSilent;
end;

{ TExpression }

constructor TExpression.Create;
begin
  inherited Create;
  SetLength(Arguments, 0);
end;

function TExpression.ArgCount: Integer;
begin
  Result:= length(Arguments);
end;

function TExpression.GetArgument(Index: Integer): IExpression;
begin
  Result:= Arguments[Index];
end;

function TExpression.IsClass(const Cls: TClass): Boolean;
begin
  Result:= InheritsFrom(Cls);
end;

function TExpression.NativeObject: TObject;
begin
  Result:= Self;
end;

function TExpression.Represents(const IID: TGUID; out Intf): boolean;
begin
  Result:= Supports(Self, IID, Intf);
end;

function TExpression.Represents(const IID: TGUID): boolean;
begin
  Result:= Supports(Self, IID);
end;

procedure TExpression.SetArgs(const aArgs: array of IExpression);
var
  i: integer;
begin
  SetLength(Arguments, Length(aArgs));
  for i:= 0 to high(aArgs) do
    Arguments[i]:= aArgs[i];
end;

function TExpression.GetArgs: TExprList;
var
  i: integer;
begin
  SetLength(Result, Length(Arguments));
  for i:= 0 to high(Arguments) do
    Result[i]:= Arguments[i];
end;

function TExpression.StringOfArgs(fmt: TStringFormat; Delim: String): String;
var
  i: integer;
  s: IStringConvertible;
begin
  if Length(Arguments)=0 then
    Result:= ''
  else begin
    if Arguments[0].Represents(IStringConvertible, s) then
      Result:= s.AsString(fmt)
    else
      Result:= '<?>';
    for i:= 1 to high(Arguments) do
      if Arguments[i].Represents(IStringConvertible, s) then
        Result:= Result + Delim + s.AsString(fmt)
      else
        Result:= Result + Delim + '<?>';
  end;
end;

procedure TExpression.SetArgument(Index: integer; const Value: IExpression);
begin
  Arguments[Index]:= Value;
end;

{ TDynamicArguments }

constructor TDynamicArguments.Create(Args: TExprList; FromIndex: integer; Context: IContext);
var
  i: integer;
  ass: TE_Call;
begin
  for i:= FromIndex to High(Args) do begin
    if Args[i].IsClass(TE_SymbolRef) then
      Add((Args[i].NativeObject as TE_SymbolRef).Name, TValueNull.Create)
    else if Args[i].IsClass(TE_Call) then begin
      ass:= Args[i].NativeObject as TE_Call;
      if (ass.Name='_assign') and (ass.Arg[0].IsClass(TE_SymbolRef)) then
        Add((ass.Arg[0].NativeObject as TE_SymbolRef).Name, ass.Arg[1].Evaluate(Context));
    end;
  end;
end;

procedure TDynamicArguments.Add(Name: string; Value: IExpression);
var
  i: integer;
begin
  i:= length(FItems);
  SetLength(FItems, i + 1);
  FItems[i].Name:= Name;
  FItems[i].Value:= Value;
end;

function TDynamicArguments.IsSet(Name: string): boolean;
var
  i: integer;
begin
  Result:= false;
  for i:= 0 to high(FItems) do
    if AnsiSameText(FItems[i].Name, Name) then begin
      Result:= true;
      exit;
    end;
end;

function TDynamicArguments.GetValue(Name: string): IExpression;
begin
  Result:= GetDefault(Name, TValueUnassigned.Create);
end;

function TDynamicArguments.GetDefault(Name: string; Default: IExpression): IExpression;
var
  i: integer;
begin
  for i:= 0 to high(FItems) do
    if AnsiSameText(FItems[i].Name, Name) then begin
      Result:= FItems[i].Value;
      exit;
    end;
  Result:= Default;
end;

{ TFunctionPackage }

procedure TFunctionPackage.PublishedMethods(Names: TStringList);
type
  PVMTEntry = ^TVMTEntry;
  TVMTEntry = packed record
    EntrySize: Word;
    Code: Pointer;
    Name: ShortString;
  end;
  PMethodTable = ^TMethodTable;
  TMethodTable = packed record
    Count: Word;
    Methods: array of TVMTEntry;
  end;
var
  pVMT: Pointer;
  pClass: TClass;
  vmt: PMethodTable;
  entry: PVMTEntry;
  i: integer;
begin
  pClass:= Self.ClassType;
  while pClass <> nil do begin
    pVMT:= Pointer(Integer(pClass) + vmtMethodTable);
    vmt:= Pointer(PCardinal(pVMT)^);
    if Assigned(vmt) then begin
      entry:= @vmt^.Methods;
      for i:= 0 to vmt^.Count - 1 do begin
        Names.Add(entry^.Name);
        Inc(Cardinal(entry), entry^.EntrySize);
      end;
    end;
    pClass:= pClass.ClassParent;
  end;
end;

function TFunctionPackage.GetFunction(FunctionName: string; ParamCount: integer; out DynFrom: integer): TUDFHeader;
var
  meth: TMethod;
  list: TStringList;
  s, cnt: string;
  i: integer;
  opt: boolean;
begin
  Result:= nil;
  DynFrom:= -1;
  list:= TStringList.Create;
  try
    meth.Data:= Self;
    PublishedMethods(list);
    list.Sort;
    for i:= 0 to list.Count - 1 do begin
      s:= list[i];
      opt:= AnsiEndsStr('_opt', s);
      if opt then
        Delete(s, LastDelimiter('_', s), 1000);
      cnt:= Copy(s, LastDelimiter('_', s) + 1, 1000);
      Delete(s, LastDelimiter('_', s), 1000);
      if AnsiCompareText(FunctionName, s) = 0 then begin
        if (cnt = 'N') or
           (opt and (ParamCount >= StrToInt(cnt))) or
           (not opt and (ParamCount = StrToInt(cnt)))  then begin
          meth.Code:= MethodAddress(list[i]);
          Result:= TUDFHeader(Meth);
          if opt then
            DynFrom:= StrToInt(cnt);
          exit;
        end;
      end;
    end;
  finally
    FreeAndNil(list);
  end;
end;

function TFunctionPackage.FunctionExists(FunctionName: string; ParamCount: integer): boolean;
var
  k: integer;
begin
  Result:= Assigned(GetFunction(FunctionName, ParamCount, k));
end;

procedure TFunctionPackage.OnImport(const MS: TMathSystem);
begin
end;

{ TPackageAlgebra }

procedure TPackageAlgebra.OnImport(const MS: TMathSystem);
begin
  MS.RegisterAsInfix('#', 15,[ooUnary],Self,'_char');
  MS.RegisterAsInfix('-', 15,[ooUnary, ooUnparsed],Self,'_negate'); // Done by Parse/Fold if it finds a Subtraction with no LHS

  MS.RegisterAsInfix('^', 19,[],Self,'_pow');
  MS.RegisterAsInfix('*', 20,[ooFlat, ooFlatAssociative],Self,'_mult');
  MS.RegisterAsInfix('/', 20,[],Self,'_div');
  MS.RegisterAsInfix('%', 20,[],Self,'_mod');

  MS.RegisterAsInfix('+', 30,[ooFlat, ooFlatAssociative],Self,'_plus');
  MS.RegisterAsInfix('-', 30,[ooFlat],Self,'_subtract');

  MS.RegisterAsInfix('=', 100,[],Self,'_assign');
  MS.RegisterAsInfix(':=', 105,[],Self,'_define');
  MS.RegisterAsInfix(',', 110,[ooFlat, ooUnpackInArguments],Self,'_comma');
end;

function TPackageAlgebra._char_1(Context: IContext; Args: TExprList): IExpression;
var
  v: IValueNumber;
begin
  if Args[0].Evaluate(Context).Represents(IValueNumber, v) then
    Result:= TValueString.Create(chr(v.ValueInt))
  else
    raise EMathTypeError.CreateFmt(sCannotConvertExpression, ['Number']);
end;

function TPackageAlgebra._negate_1(Context: IContext; Args: TExprList): IExpression;
var
  e: IExpression;
  op: IOperationMultiplication;
begin
  e:= Args[0].Evaluate(Context);
  if e.Represents(IOperationMultiplication, op) then
    Result:= op.OpNegate;
  if not Assigned(Result) then
    raise EMathSysError.CreateFmt(sUnsupportedOperation, ['Negate']);
end;

function TPackageAlgebra._pow_2(Context: IContext; Args: TExprList): IExpression;
var
  b,e: IExpression;
  op: IOperationPower;
begin
  b:= Args[0].Evaluate(Context);
  e:= Args[1].Evaluate(Context);

  Result:= nil;
  if b.Represents(IOperationPower, op) then
    Result:= op.OpPower(e)
  else
    raise EMathSysError.CreateFmt(sUnsupportedOperation, ['Power']);
end;

function TPackageAlgebra._mult_N(Context: IContext; Args: TExprList): IExpression;
  function mul(a,b: IExpression): IExpression;
  var
    op: IOperationMultiplication;
  begin
    if a.Represents(IOperationMultiplication, op) then
      Result:= op.OpMultiply(b)
    else
      raise EMathSysError.CreateFmt(sUnsupportedOperation, ['Multiply']);
  end;
var
  i: integer;
  v: IExpression;
begin
  if Length(Args)=0 then
    Result:= TValueUnassigned.Create
  else begin
    Result:= Args[0].Evaluate(Context);

    for i:= 1 to high(Args) do begin
      v:= Args[i].Evaluate(Context);
      Result:= mul(Result, v);
      if not Assigned(Result) then
        raise EMathSysError.CreateFmt(sUnsupportedOperation, ['Multiply']);
    end;
  end;
end;

function TPackageAlgebra._div_2(Context: IContext; Args: TExprList): IExpression;
var
  n,d: IExpression;
  op: IOperationMultiplication;
begin
  n:= Args[0].Evaluate(Context);
  d:= Args[1].Evaluate(Context);

  Result:= nil;
  if n.Represents(IOperationMultiplication, op) then
    Result:= op.OpDivide(d);
  if not Assigned(Result) then
    raise EMathSysError.CreateFmt(sUnsupportedOperation, ['Divide']);
end;

function TPackageAlgebra._mod_2(Context: IContext; Args: TExprList): IExpression;
var
  a,b: IValueNumber;
  va,vb,f: MTFloat;
begin
  if Args[0].Evaluate(Context).Represents(IValueNumber, a) and
     Args[1].Evaluate(Context).Represents(IValueNumber, b) then begin
    // both integers?
    if (a.BaseType = tiInt) and (b.BaseType = tiInt) then
      Result:= TValueFactory.Integer(a.ValueInt mod b.ValueInt)
    else
    // trivial case?
    if b.ValueFloat > a.ValueFloat then
      Result:= a
    else
    if fzero(a.ValueFloat) then
      Result:= TValueFactory.Zero
    else begin
      va:= a.ValueFloat;
      vb:= b.ValueFloat;
      if fzero((va/vb) - ((va-1)/vb)) or
         fzero(((va+1)/vb) - (va/vb)) then
        Context.Output.Hint('Mod: result is not exact.',[]);
//      f:= a - Int(a / b) * b;
//      f:= frac(a / b) * b;
      f:= fmod(va, vb);
      Result:= TValueFactory.Float(f);
    end;
  end
  else
    raise EMathTypeError.CreateFmt(sCannotConvertExpression, ['Number']);
end;

function TPackageAlgebra._plus_N(Context: IContext; Args: TExprList): IExpression;
  function add(a,b: IExpression): IExpression;
  var
    op: IOperationAddition;
  begin
    if a.Represents(IOperationAddition, op) then
      Result:= op.OpAdd(b)
    else
      raise EMathSysError.CreateFmt(sUnsupportedOperation, ['Add']);
  end;
var
  i: integer;
  v: IExpression;
begin
  if Length(Args)=0 then
    Result:= TValueUnassigned.Create
  else begin
    Result:= Args[0].Evaluate(Context);

    for i:= 1 to high(Args) do begin
      v:= Args[i].Evaluate(Context);
      Result:= add(Result, v);    
      if not Assigned(Result) then
        raise EMathSysError.CreateFmt(sUnsupportedOperation, ['Add']);
    end;
  end;
end;

function TPackageAlgebra._subtract_N(Context: IContext; Args: TExprList): IExpression;
  function sub(a,b: IExpression): IExpression;
  var
    op: IOperationAddition;
  begin
    if a.Represents(IOperationAddition, op) then
      Result:= op.OpSubtract(b)
    else
      raise EMathSysError.CreateFmt(sUnsupportedOperation, ['Subtract']);
  end;
var
  i: integer;
  v: IExpression;
begin
  if Length(Args)=0 then
    Result:= TValueUnassigned.Create
  else begin
    Result:= Args[0].Evaluate(Context);

    for i:= 1 to high(Args) do begin
      v:= Args[i].Evaluate(Context);
      Result:= sub(Result, v);
      if not Assigned(Result) then
        raise EMathSysError.CreateFmt(sUnsupportedOperation, ['Subtract']);
    end;
  end;
end;

function TPackageAlgebra._assign_2(Context: IContext; Args: TExprList): IExpression;
var
  r: ISymbolReference;
  v: IExpression;
  l,d: IValueList;
  i: integer;
begin
  if Args[0].Represents(ISymbolReference, r) then begin
    v:= Args[1].Evaluate(Context);
    Context.Define(r.name, v);
    Result:= v;
  end else if Args[0].Represents(IValueList, d) then begin
    for i:= 0 to d.Length-1 do
      if not d.Item[i].Represents(ISymbolReference, r) then
        raise EMathTypeError.Create('LHS of assignment needs to be a list of symbol references');
    v:= Args[1].Evaluate(Context);
    if v.Represents(IValueList, l) then begin
      for i:= 0 to min(l.Length, d.Length)-1 do begin
        d.Item[i].Represents(ISymbolReference, r);
        Context.Define(r.Name, l.Item[i]);
      end;
    end else
      if (d.Length>0) and (d.Item[0].Represents(ISymbolReference, r)) then
        Context.Define(r.Name, v);
    Result:= v;
  end else
    raise EMathTypeError.Create('LHS of assignment needs to be a symbol reference or a list of symbol references');
end;

function TPackageAlgebra._define_2(Context: IContext; Args: TExprList): IExpression;
var
  name: string;
begin
  if not Args[0].IsClass(TE_SymbolRef) then
    raise EMathTypeError.Create('LHS of assignment needs to be a symbol reference');
  name:= TE_SymbolRef(Args[0].NativeObject).Name;
  Context.Define(name, Args[1]);
  Result:= Args[1];
end;

function TPackageAlgebra._comma_N(Context: IContext; Args: TExprList): IExpression;
var
  i: integer;
begin
  if Length(Args)=0 then
    Result:= TValueUnassigned.Create
  else begin
    for i:= 0 to high(Args) do
      Result:= args[i].Evaluate(Context);
  end;
end;

{ TE_SymbolRef }

constructor TE_SymbolRef.Create(AName: string);
begin
  inherited Create;
  FName:= AName;
end;

function TE_SymbolRef.AsString(const Format: TStringFormat): String;
begin
  Result:= Name;
end;

function TE_SymbolRef.Evaluate(const Context: IContext): IExpression;
var
  x: IExpression;
begin
  x:= Context.Definition(FName);
  if Assigned(x) then begin
    Result:= x;
  end
  else begin
    Result:= TValueUnassigned.Create;
    raise EMathSysError.CreateFmt('Expression unknown in current Context: %s', [Name]);
  end;
end;

function TE_SymbolRef.Clone(Deep: Boolean): IExpression;
begin
  Result:= TE_SymbolRef.Create(FName);
end;

function TE_SymbolRef.GetName: string;
begin
  Result:= FName;
end;

{ TE_Call }

constructor TE_Call.Create(AName: string);
begin
  inherited Create;
  FName:= AName;      
  FFunctionBound:= false;
end;

function TE_Call.Evaluate(const Context: IContext): IExpression;
begin
  if not FFunctionBound then begin
    FFunctionBound:= True;
    FFunction:= Context.GetSystem.HasFunction(FName, length(Arguments));
  end;

  if Assigned(FFunction) then
    Result:= FFunction.Call(Context, Arguments)
  else
    raise EMathSysError.CreateFmt('Function %s has no version with %d parameters', [FName, Length(Arguments)]);
end;

class function TE_Call.CheckSysCalls(StringForm: string): Boolean;
// perform check on string form, so we could only encounter (new() or ,new()
// in non-toplevel calls. if we don't find that, it might be top level (pos 0)
// but that would evaluate to true anyway -> don't bother checking
  function Chk(f: string): boolean;
  begin
    Result:= (pos(',' + f + '(', StringForm) = 0) and
      (pos('(' + f + '(', StringForm) = 0);
  end;
begin
  StringForm:= LowerCase(StringForm);
  Result:= Chk('new') and Chk('drop') and Chk('help');
  if not Result then
    raise EMathSysError.Create('System functions can only be used stand-alone');
end;

function TE_Call.AsString(const Format: TStringFormat): String;
var
  inf: PInfixDefinition;
begin
  case Format of
    STR_FORMAT_INPUT_EXPANDED: Result:= Name + '('+StringOfArgs(Format, ',')+')'
  else
    if not Assigned(FCreatedFrom) then
      Result:= Name + '('+StringOfArgs(Format, ',')+')'
    else begin
      inf:= FCreatedFrom;
      if ooUnary in inf.Options then
        Result:= inf.Operator + StringOfArgs(Format, inf.Operator)
      else
        Result:= StringOfArgs(Format, inf.Operator);
    end;
  end;
end;

function TE_Call.Clone(Deep: Boolean): IExpression;
var
  sc: TE_Call;
  i: integer;
begin
  sc:= TE_Call.Create(FName);
  sc.FCreatedFrom:= FCreatedFrom;

  if Deep then begin
    SetLength(sc.Arguments, Length(Arguments));
    for i:= 0 to High(Arguments) do
      sc.Arguments[i]:= Arguments[i].Clone(Deep);
  end else
    sc.SetArgs(Arguments);
  Result:= sc as IExpression;
end;

function TE_Call.GetName: string;
begin
  Result:= FName;
end;

{ TE_Subcontext }

function TE_Subcontext.AsString(const Format: TStringFormat): String;
begin
  case Format of
    $FFFF: ;
  else
    Result:= Name + '['+StringOfArgs(Format, ',')+']';
  end;
end;

function TE_Subcontext.Clone(Deep: Boolean): IExpression;
var
  sc: TE_Subcontext;
  i: integer;
begin
  sc:= TE_Subcontext.Create(FName);

  if Deep then begin
    SetLength(sc.Arguments, Length(Arguments));
    for i:= 0 to High(Arguments) do
      sc.Arguments[i]:= Arguments[i].Clone(Deep);
  end else
    sc.SetArgs(Arguments);
  Result:= sc as IExpression;
end;

constructor TE_Subcontext.Create(AName: string);
begin
  inherited Create;
  FName:= AName;
end;

function TE_Subcontext.Evaluate(const Context: IContext): IExpression;
var
  ctx: IContext;
  target: IExpression;
  i: integer;
begin
  ctx:= TContext.Create(Context);
  try
    target:= Context.Definition(Name);   
    if Assigned(target) then begin
      for i:= 0 to high(Arguments) do
        Arguments[i].Evaluate(ctx);
      Result:= target.Evaluate(ctx);
    end else begin
      Result:= TValueUnassigned.Create;
      raise EMathSysError.CreateFmt('Expression unknown in current Context: %s', [Name]);
    end;
  finally
    ctx:= nil;
  end;
end;

{ TPackageCore }

procedure TPackageCore.OnImport(const MS: TMathSystem);
begin
  MS.RegisterAsInfix('??', 0,[ooUnary,ooHoldPackedArguments],Self,'_dump');
  MS.RegisterAsInfix('?', 10,[ooUnary],Self,'_describe');
end;

function TPackageCore._dump_1(Context: IContext; Args: TExprList): IExpression;
  procedure DumpNode(ex: IExpression; Lv: Integer);
  var
    str: IStringConvertible;
    s: string;
    i: integer;
  begin
    s:= '';
    if ex.IsClass(TE_Atom) and ex.Represents(IStringConvertible, str) then
      s:= str.AsString(STR_FORMAT_DEFAULT)
    else
    if ex.IsClass(TE_SymbolRef) then
      s:= TE_SymbolRef(ex.NativeObject).Name
    else
    if ex.IsClass(TE_Subcontext) then
      s:= TE_Subcontext(ex.NativeObject).Name + '[]'
    else
    if ex.IsClass(TE_Call) then
      s:= TE_Call(ex.NativeObject).Name + '()'
    else
      s:= ex.NativeObject.ClassName;
    Context.Output.Hint('%*s%s',[Lv*2, '', s]);
    for i:= 0 to ex.ArgCount-1 do
      DumpNode(ex.Arg[i], lv+1);
  end;
begin
  DumpNode(Args[0], 0);
  Result:= Args[0];
end;

function TPackageCore._describe_1(Context: IContext; Args: TExprList): IExpression;
var
  name: string;
  e: IExpression;
  s: IStringConvertible;
begin
  if Args[0].IsClass(TE_SymbolRef) then begin
    name:= TE_SymbolRef(Args[0].NativeObject).Name;
    e:= Context.Definition(name);
  end else
    e:= Args[0];
  if Assigned(e) and e.Represents(IStringConvertible, s) then
    Result:= TValueString.Create(s.AsString(STR_FORMAT_INPUT))
  else
    Result:= TValueString.Create('<Unknown>');
end;

function TPackageCore.TypeOf_1(Context: IContext; Args: TExprList): IExpression;
var
  x: IExpression;
  n: IValueNumber;
  d: string;
begin
  x:= args[0].Evaluate(Context);
  if x.Represents(IValueNull) then
    Result:= TValueString.Create('Null')
  else
  if x.Represents(IValueUnassigned) then
    Result:= TValueString.Create('Unassigned')
  else
  if x.Represents(IValueString) then
    Result:= TValueString.Create('String')
  else
  if x.Represents(IValueList) then
    Result:= TValueString.Create('List')
  else
  if x.Represents(IValueNumber, n) then begin
    if n.Represents(IDimensions) then
      d:= 'Dimension'
    else
      d:= '';
    case n.BaseType of
      tiUnknown: Result:= TValueString.Create('Number'+d);
      tiInt: Result:= TValueString.Create('Integer'+d);
      tiFloat: Result:= TValueString.Create('Float'+d);
    end;
  end else
    Result:= TValueString.Create('Unknown');
end;

function TPackageCore.New_0(Context: IContext; args: TExprList): IExpression;
begin
  TContext.SystemFrom(Context).NewContext('');
end;

function TPackageCore.New_1(Context: IContext; args: TExprList): IExpression;
begin
  TContext.SystemFrom(Context).NewContext(EvaluateToString(Context, args[0]));
end;

function TPackageCore.Drop_0(Context: IContext; args: TExprList): IExpression;
begin
  TContext.SystemFrom(Context).DropContext();
end;

function TPackageCore.Undef_1(Context: IContext; args: TExprList): IExpression;
begin
  Context.Undefine(EvaluateToString(Context, Args[0]));
end;

function TPackageCore.Clear_0(Context: IContext; args: TExprList): IExpression;
begin
  Context.Output.Clear;
  Result:= TValueUnassigned.Create;
end;

function FindConstant(Name: string; out Definition: TConstantDef): boolean;
type
  TConstDefRef = array[0..0] of TConstantDef;
  PConstDef = ^TConstDefRef;
  function FindIn(List: PConstDef; Lo, Hi: integer): Boolean;
  var
    i: integer;
  begin
    Result:= false;
    for i:= lo to hi do begin
      if SameText(List^[i].LongName, Name) then begin
        Result:= true;
        Definition:= List^[i];
        break;
      end;
    end;
  end;
begin
  Result:= FindIn(@MathematicalConstants, Low(MathematicalConstants), high(MathematicalConstants)) or
    FindIn(@PhysicalConstants, Low(PhysicalConstants), high(PhysicalConstants));
end;

function FormatConstInfo(def: TConstantDef): string;
var
  val: IValueString;
begin
  val:= TValueString.Create(NumberToStr(def.Value, False));
  Result:= Format('%s = %s', [def.LongName, val.Value]);
  if def.Uni > '' then
    Result:= Format('%s [%s]', [Result, def.Uni]);
  if def.Comment > '' then
    Result:= Format('%s (%s)', [Result, def.Comment]);
end;

function TPackageCore.const_1(Context: IContext; args: TExprList): IExpression;
var
  nm: string;
  res: TConstantDef;
  f: IPackagedFunction;
begin
  nm:= EvaluateToString(Context, args[0]);
  if FindConstant(nm, res) then begin
    Result:= TValueFactory.Float(res.Value);
    if (res.Uni>'') then begin
      f:= Context.GetSystem.HasFunction('unit', 2);
      if Assigned(f) then
        Result:= f.Call(Context, MakeArgs([Result, TValueString.Create(res.Uni)]));
    end;
  end else
    raise EMathSysError.CreateFmt('Unknown Constant: %s', [nm]);
end;

function TPackageCore.constinfo_0(Context: IContext; args: TExprList): IExpression;
type
  TConstDefRef = array[0..0] of TConstantDef;
  PConstDef = ^TConstDefRef;
var
  Count: integer;
  procedure ListIn(List: PConstDef; Lo, Hi: integer);
  var
    i: integer;
  begin
    for i:= lo to hi do begin
      Context.Output.Hint(FormatConstInfo(List^[i]), []);
      Inc(Count);
    end;
  end;
begin
  Count:= 0;
  ListIn(@MathematicalConstants, Low(MathematicalConstants), high(MathematicalConstants));
  ListIn(@PhysicalConstants, Low(PhysicalConstants), high(PhysicalConstants));
  Result:= TValueFactory.Integer(Count);
end;

function TPackageCore.constinfo_1(Context: IContext; args: TExprList): IExpression;
var
  nm: string;
  res: TConstantDef;
begin
  nm:= EvaluateToString(Context, args[0]);
  if FindConstant(nm, res) then begin
    Result:= TValueString.Create(FormatConstInfo(res));
  end else
    raise EMathSysError.CreateFmt('Unknown Constant: %s', [nm]);
end;

function TPackageCore.Hold_1(Context: IContext; args: TExprList): IExpression;
begin
  Result:= args[0];
end;

function TPackageCore.Eval_1(Context: IContext; args: TExprList): IExpression;
begin
  Result:= args[0].Evaluate(Context);
end;

function TPackageCore.AbsoluteTime_1(Context: IContext; args: TExprList): IExpression;
var
  pc1,pc2, pf: Int64;
  st: IExpression;
begin
  QueryPerformanceFrequency(pf);
  QueryPerformanceCounter(pc1);
  st:= args[0].Evaluate(Context);
  QueryPerformanceCounter(pc2);

  Result:= TValueList.CreateAs([TValueFactory.Float((pc2-pc1) / pf), st]);
end;

{ TPackagedFunction }

constructor TPackagedFunction.Create(const Name: string; const Ptr: TUDFHeader; const DynamicFrom: integer);
begin
  inherited Create;
  FName:= Name;
  FPtr:= Ptr;
  FDynamicFrom:= DynamicFrom;
end;

function TPackagedFunction.Call(Context: IContext; Args: TExprList): IExpression;
var
  dyn: TDynamicArguments;
begin
  if IsDynamic then begin
    dyn:= TDynamicArguments.Create(Args, FDynamicFrom, Context);
    try
      Result:= TUDFHeaderOptions(FPtr)(Context, Args, dyn);
    finally
      FreeAndNil(dyn);
    end;
  end else
    Result:= FPtr(Context, Args);
end;

function TPackagedFunction.GetName: String;
begin
  Result:= FName;
end;

function TPackagedFunction.IsDynamic: boolean;
begin
  Result:= FDynamicFrom >= 0;
end;

initialization
  GetLocaleFormatSettings(GetThreadLocale, NeutralFormatSettings);
  with NeutralFormatSettings do begin
    ThousandSeparator:= #160;
    DecimalSeparator:= '.';
    DateSeparator:= '-';
    ShortDateFormat:= 'yy-mm-dd';
    LongDateFormat:= 'mmmm d, yyyy';
    TimeSeparator:= ':';
  end;
end.

