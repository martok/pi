unit uMath;

interface

uses
  SysUtils, Classes, IniFiles, Windows, Contnrs,
  uMathIntf, Math;

type
  TFunctionPackage = class;

  TOperatorOptions = set of (ooUnary, ooUnparsed, ooFlat, ooUnpackInArguments, ooHoldPackedArguments);
  TMathSystem  = class(TIntfNoRefCount, IMathSystem)
  private
    fOutput: IOutputProvider;

    fInfixTable,
    fOperatorTable: TList;
    fPackages: TObjectList;
    fContext,
    fConstants: IContext;
  protected
    FEvaluationStack: TStringList;
    procedure EvaluationBegin(varname: string);
    procedure EvaluationEnd;
    procedure PredefineConstants(const Context: IContext);
  public
    constructor Create(const Output: IOutputProvider);
    destructor Destroy; override;

    property Output: IOutputProvider read fOutput;
    property Context: IContext read fContext;
    procedure NewContext(const Name: string);
    procedure DropContext;

    function RegisterPackage(const Package: TFunctionPackage): boolean;
    function RegisterAsInfix(Operator: String; Precedence: integer; Options: TOperatorOptions; Pack: TFunctionPackage; Func: String): boolean;
    // IMathSystem
    function Parse(const Expr: String): IExpression;
    function Evaluate(const Expr: IExpression): IExpression;
    procedure Run(const Expr: String);
  end;

  TContext = class(TInterfacedObject, IContext)
  private
    FExpressions: THashedStringList;
    FParent: IContext;
    FSystem: TMathSystem;
    FSilent: boolean;
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
    property Silent: boolean read FSilent write FSilent;

    property Count: integer read GetCount;
    property Name[index: integer]: string read GetName;
    // IContext
    function NativeObject: TObject;
    procedure Define(const Name: string; Expression: IExpression);
    procedure Undefine(const Name: string);
    function Definition(const Name: string): IExpression;
    function Defines(const Name: string): boolean;
    function Output: IOutputProvider;
  end;


  TExprList = array of IExpression;
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
    function Represents(const IID: TGUID; out Intf): boolean;
    function IsClass(const Cls: TClass): Boolean;

    procedure SetArgs(const aArgs: array of IExpression);
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
  TFunctionPackageClass = class of TFunctionPackage;
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
    FFunction: TUDFHeader;
    FFirstDynamic: Integer;
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
    function AbsoluteTime_1(Context: IContext; args: TExprList): IExpression;
  end;

  TPackageAlgebra = class(TFunctionPackage)
  private
    function Divide(a, d: Number): Number;
    function Multiply(a, b: IExpression): IExpression;
    function Add(a, b: IExpression; Premul: integer): IExpression;
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

var
  NeutralFormatSettings: TFormatSettings;

function NumberToStr(const Value: Number; FS: TFormatSettings; ShowThousands: boolean): string;

resourcestring
  sConstants = 'Constants';
  sWork = 'Work';

  sCannotConvertExpression = 'Cannot convert expression to type %s';

implementation

uses
  StrUtils, uMathValues, uMathConstants, uMathDimensions;

type
  TInfixDefinition = record
    Operator: String[16];
    Precedence: integer;
    Options: TOperatorOptions;
    Pack: TFunctionPackage;
    Func: String[64];
  end;
  PInfixDefinition = ^TInfixDefinition;

function NumberToStr(const Value: Number; FS: TFormatSettings; ShowThousands: boolean): string;
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
    dec(p, 3);
    while p > 0 do begin
      Insert(NeutralFormatSettings.ThousandSeparator, Result, p);
      dec(p, 3);
      inc(e);
    end;
    p:= Pos(NeutralFormatSettings.DecimalSeparator, Result);
    if p > 0 then begin
      inc(p, 3);
      while p < e do begin
        Insert(NeutralFormatSettings.ThousandSeparator, Result, p + 1);
        inc(p, 4);
        inc(e);
      end;
    end;
  end;
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

function TMathSystem.Parse(const Expr: String): IExpression;
const
  CharQuote = '''';

  CharBraceOpen = '(';
  CharBraceClose = ')';
  CharContextOpen = '[';
  CharContextClose = ']';
  CharListOpen = '{';
  CharListClose = '}';
type
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
var
  Tokens: TTokenList;
  _negate: PInfixDefinition;
  i: integer;

  procedure Tokenize;
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
    p:= 1;
    while p <= length(expr) do begin
      t.Value:= '';
      FillChar(t, sizeof(t), 0);
      t.Pos:= p;
      case Expr[p] of
        ' ',#9: begin
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
        'a'..'z', 'A'..'Z', '_': ParseIdentifier(t);
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
          raise ESyntaxError.CreateFmt('Position %d: Unexpected Character %s', [p, Expr[p]]);
      end;

      SetLength(Tokens, Length(Tokens) + 1);
      Tokens[high(Tokens)]:= t;
    end;
  end;

  procedure Fold(L, R: integer);
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
                Fold(A + 1, I - 1);
                Tokens[A].Kind:= SetTo;
              end;
              Tokens[I].Kind:= tokVoid;
            end else
              raise ESyntaxError.CreateFmt('Position %d: Closing %s never opened', [Tokens[i].Pos, Str]);
            A:= -2;
            break;
          end;
        end;
        if A >= 0 then
          raise ESyntaxError.CreateFmt('%s opened at position %d is never closed', [Str, Tokens[A].Pos]);
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
            Tokens[i].Expr:= TValueNumber.Create(StrToFloat(Tokens[i].Value, NeutralFormatSettings));
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
            raise ESyntaxError.CreateFmt('Position %d: Operator has no RHS', [Tokens[i].Pos]);

          if Tokens[rr].Kind <> tokExpression then
            raise ESyntaxError.CreateFmt('Position %d: expected expression, found %s', [Tokens[rr].Pos, Tokens[rr].Value]);

          if (exi^.Func = '_subtract') and
            ((ll < 0) or (Tokens[ll].Kind <> tokExpression)) then begin
            tmp:= TE_Call.Create('_negate');
            TE_Call(tmp.NativeObject).FCreatedFrom:= _negate;
            tmp.SetArgs([Tokens[rr].Expr]);
          end else begin
            tmp:= TE_Call.Create(exi^.Func);
            TE_Call(tmp.NativeObject).FCreatedFrom:= exi;

            if ooUnary in exi^.Options then
              tmp.SetArgs([Tokens[rr].Expr])
            else begin
              if ll < 0 then
                raise ESyntaxError.CreateFmt('Position %d: Operator has no LHS', [Tokens[i].Pos]);
              if Tokens[ll].Kind <> tokExpression then
                raise ESyntaxError.CreateFmt('Position %d: expected expression, found %s', [Tokens[ll].Pos, Tokens[ll].Value]);
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

  function CheckResults: boolean;
  var
    i: integer;
  begin
    Result:= length(Tokens) > 0;
    if not Result then
      exit;
    for i:= 1 to high(Tokens) do
      if Tokens[i].Kind <> tokVoid then
        raise ESyntaxError.CreateFmt('Position %d: Leftover token', [Tokens[i].Pos]);
    Result:= true;
  end;

  function ApplyFlatOperators(ex: IExpression): IExpression;
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
        for k:= 0 to x.ArgCount-1 do begin
          if x.Arg[k].Represents(IFunctionCall, ac) and (ac.Name = inf.Func) then begin
            c:= Length(el);
            SetLength(el, c + ac.ArgCount);
            for m:= 0 to ac.ArgCount-1 do
              el[c + m]:= ac.Arg[m];
          end else begin
            c:= Length(el);
            SetLength(el, c + 1);
            el[c]:= x.Arg[k];
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

begin
  Result:= nil;
  SetLength(Tokens, 0);
  Tokenize;
  for i:= 0 to fInfixTable.Count-1 do
    if PInfixDefinition(fInfixTable[i]).Func='_negate' then begin
      _negate:= PInfixDefinition(fInfixTable[i]);
      break;
    end;
  if not Assigned(_negate) then
    raise EParserError.Create('Could not find definition of negate operator!');
  Fold(0, high(Tokens));
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
  Result:= FSystem.Output;
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

procedure TExpression.SetArgs(const aArgs: array of IExpression);
var
  i: integer;
begin
  SetLength(Arguments, Length(aArgs));
  for i:= 0 to high(aArgs) do
    Arguments[i]:= aArgs[i];
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
  MS.RegisterAsInfix('*', 20,[ooFlat],Self,'_mult');
  MS.RegisterAsInfix('/', 20,[],Self,'_div');
  MS.RegisterAsInfix('%', 20,[],Self,'_mod');

  MS.RegisterAsInfix('+', 30,[ooFlat],Self,'_plus');
  MS.RegisterAsInfix('-', 30,[ooFlat],Self,'_subtract');

  MS.RegisterAsInfix('=', 100,[],Self,'_assign');
  MS.RegisterAsInfix(':=', 105,[],Self,'_define');
  MS.RegisterAsInfix(',', 110,[ooFlat, ooUnpackInArguments],Self,'_comma');
end;

function TPackageAlgebra._char_1(Context: IContext; Args: TExprList): IExpression;
var
  v: Number;
begin
  if EvaluateToNumber(Context, Args[0], v) then
    Result:= TValueString.Create(chr(trunc(v)))
  else
    raise EMathTypeError.CreateFmt(sCannotConvertExpression, ['Number']);
end;

function TPackageAlgebra._negate_1(Context: IContext; Args: TExprList): IExpression;
var
  e: IExpression;
  ua: IValueDimension;
  na: IValueNumber;
begin
  e:= Args[0].Evaluate(Context);
  if e.Represents(IValueDimension, ua) then
    Result:= TValueDimension.Create(-ua.Value, ua.Units)
  else
  if e.Represents(IValueNumber, na) then
    Result:= TValueNumber.Create(-na.Value)
  else
    raise EMathTypeError.CreateFmt(sCannotConvertExpression, ['Number']);
end;

function TPackageAlgebra._pow_2(Context: IContext; Args: TExprList): IExpression;
var
  ea: IExpression;
  b: Number;
  ua: IValueDimension;
begin
  ea:= args[0].Evaluate(Context);
  if EvaluateToNumber(Context, Args[1], b) then begin
    if ea.Represents(IValueDimension, ua) then begin
      if IsZero(frac(b)) then
        Result:= TValueDimension.Create(IntPower(ua.Value, trunc(b)), PowerDimensions(ua.Units, trunc(b)))
      else
        raise EMathDimensionError.Create('Exponent has to be a whole number');
    end else
      Result:= TValueNumber.Create(Power(CastToNumber(ea), b));
  end else
    raise EMathTypeError.CreateFmt(sCannotConvertExpression, ['Number']);
end;

function TPackageAlgebra.Multiply(a, b: IExpression): IExpression;
var
  na,nb: IValueNumber;
  ua,ub: IValueDimension;
begin
  if a.Represents(IValueDimension, ua) then begin
    if b.Represents(IValueDimension, ub) then
      Result:= TValueDimension.Create(ua.Value * ub.Value, MultDimensions(ua.Units, ub.Units))
    else if b.Represents(IValueNumber, nb) then
      Result:= TValueDimension.Create(ua.Value * nb.Value, ua.Units)
    else
      raise EMathTypeError.CreateFmt(sCannotConvertExpression, ['Number']);
  end else
  if a.Represents(IValueNumber, na) then begin
    if b.Represents(IValueDimension, ub) then
      Result:= TValueDimension.Create(na.Value*ub.Value, ub.Units)
    else if b.Represents(IValueNumber, nb) then
      Result:= TValueNumber.Create(na.Value*nb.Value)
    else
      raise EMathTypeError.CreateFmt(sCannotConvertExpression, ['Number']);
  end else
    raise EMathTypeError.CreateFmt(sCannotConvertExpression, ['Number']);
end;

function TPackageAlgebra._mult_N(Context: IContext; Args: TExprList): IExpression;
var
  i: integer;
begin
  if Length(Args)=0 then
    Result:= TValueUnassigned.Create
  else begin
    Result:= Args[0].Evaluate(Context);

    for i:= 1 to high(Args) do 
      Result:= Multiply(Result, Args[1].Evaluate(Context));
  end;
end;

function TPackageAlgebra.Divide(a, d: Number): Number;
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

function TPackageAlgebra._div_2(Context: IContext; Args: TExprList): IExpression;
var
  n,d: IExpression;
  na,nd: IValueNumber;
  ua,ud: IValueDimension;
begin
  n:= Args[0].Evaluate(Context);
  d:= Args[1].Evaluate(Context);

  if n.Represents(IValueDimension, ua) then begin
    if d.Represents(IValueDimension, ud) then
      Result:= TValueDimension.Create(Divide(ua.Value, ud.Value), MultDimensions(ua.Units, InverseDimensions(ud.Units)))
    else if d.Represents(IValueNumber, nd) then
      Result:= TValueDimension.Create(Divide(ua.Value, nd.Value), ua.Units)
    else
      raise EMathTypeError.CreateFmt(sCannotConvertExpression, ['Number']);
  end else
  if n.Represents(IValueNumber, na) then begin
    if d.Represents(IValueDimension, ud) then
      Result:= TValueDimension.Create(Divide(na.Value, ud.Value), InverseDimensions(ud.Units))
    else if d.Represents(IValueNumber, nd) then
      Result:= TValueNumber.Create(Divide(na.Value, nd.Value))
    else
      raise EMathTypeError.CreateFmt(sCannotConvertExpression, ['Number']);
  end else
    raise EMathTypeError.CreateFmt(sCannotConvertExpression, ['Number']);
end;

function TPackageAlgebra._mod_2(Context: IContext; Args: TExprList): IExpression;
var
  a,b,c,f: Number;
begin
  if EvaluateToNumber(Context, Args[0], a) and
     EvaluateToNumber(Context, Args[1], b) then begin
    c:= a / b;
    if c > 1000000 then begin
      c:= Int(c);
      f:= a - b * c;
    end else
      f:= frac(c) * b;
    Result:= TValueNumber.Create(f);
  end
  else
    raise EMathTypeError.CreateFmt(sCannotConvertExpression, ['Number']);
end;

function TPackageAlgebra.Add(a, b: IExpression; Premul: integer): IExpression;
var
  na,nb: IValueNumber;
  ua,ub: IValueDimension;
begin
  if a.Represents(IValueDimension, ua) then begin
    if b.Represents(IValueDimension, ub) then begin
      if ua.IsCompatible(ub.Units) then
        Result:= TValueDimension.Create(ua.Value + ub.Value * Premul, ua.Units)
      else
        raise EMathDimensionError.Create('Only objects of the same dimension can be added');
    end else
      if b.Represents(IValueNumber, nb) then begin
        if ua.IsScalar then
          Result:= TValueNumber.Create(ua.Value + nb.Value * Premul)
        else
          raise EMathDimensionError.Create('Only objects of the same dimension can be added');
      end else
        raise EMathTypeError.CreateFmt(sCannotConvertExpression, ['Number']);
  end else
  if a.Represents(IValueNumber, na) then begin
    if b.Represents(IValueDimension, ub) then begin
      if ub.IsScalar then
        Result:= TValueNumber.Create(na.Value + ub.Value * Premul)
      else
        raise EMathDimensionError.Create('Only objects of the same dimension can be added');
    end else
      if b.Represents(IValueNumber, nb) then
        Result:= TValueNumber.Create(na.Value + nb.Value * Premul);
  end;
end;

function TPackageAlgebra._plus_N(Context: IContext; Args: TExprList): IExpression;
var
  i: integer;
begin
  if Length(Args)=0 then
    Result:= TValueUnassigned.Create
  else begin
    Result:= Args[0].Evaluate(Context);

    for i:= 1 to high(Args) do 
      Result:= Add(Result, Args[i].Evaluate(Context), 1);
  end;
end;

function TPackageAlgebra._subtract_N(Context: IContext; Args: TExprList): IExpression;
var
  i: integer;
begin
  if Length(Args)=0 then
    Result:= TValueUnassigned.Create
  else begin
    Result:= Args[0].Evaluate(Context);

    for i:= 1 to high(Args) do 
      Result:= Add(Result, Args[i].Evaluate(Context), -1);
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
        raise ESyntaxError.Create('LHS of assignment needs to be a list of symbol references');
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
    raise ESyntaxError.Create('LHS of assignment needs to be a symbol reference or a list of symbol references');
end;

function TPackageAlgebra._define_2(Context: IContext; Args: TExprList): IExpression;
var
  name: string;
begin
  if not Args[0].IsClass(TE_SymbolRef) then
    raise ESyntaxError.Create('LHS of assignment needs to be a symbol reference');
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
    TContext.SystemFrom(Context).EvaluationBegin(FName);
    try
      Result:= x.Evaluate(Context)
    finally
      TContext.SystemFrom(Context).EvaluationEnd;
    end;
  end
  else begin
    Result:= TValueUnassigned.Create;
    raise EMathSysError.CreateFmt('Expression unknown in current Context: %s', [Name]);
  end;;
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
var
  i: integer;
  dyn: TDynamicArguments;
  pks: TObjectList;
begin
  if not FFunctionBound then begin
    FFunctionBound:= True;
    pks:= TContext.SystemFrom(Context).fPackages;
    for i:= 0 to pks.Count-1 do begin
      FFunction:= TFunctionPackage(pks[i]).GetFunction(FName, Length(Arguments), FFirstDynamic);
      if Assigned(FFunction) then
        break;
    end;
  end;

  if Assigned(FFunction) then begin
    if FFirstDynamic >= 0 then begin
      dyn:= TDynamicArguments.Create(Arguments, FFirstDynamic, Context);
      try
        Result:= TUDFHeaderOptions(FFunction)(Context, Arguments, dyn);
      finally
        FreeAndNil(dyn);
      end;
    end else
      Result:= FFunction(Context, Arguments);
    exit;
  end else
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
    $FFFF: ;
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
  ctx:= TContext.Create(TContext.SystemFrom(Context), TContext(Context.NativeObject));
  try
    target:= Context.Definition(FName);
    for i:= 0 to high(Arguments) do
      Arguments[i].Evaluate(ctx);
    Result:= target.Evaluate(ctx);
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
  val:= TValueString.Create(NumberToStr(def.Value, NeutralFormatSettings, False));
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
begin
  nm:= EvaluateToString(Context, args[0]);
  if FindConstant(nm, res) then
    Result:= TValueNumber.Create(res.Value)
  else
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
  Result:= TValueNumber.Create(Count);
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


function TPackageCore.AbsoluteTime_1(Context: IContext; args: TExprList): IExpression;
var
  pc1,pc2, pf: Int64;
  st: IExpression;
begin
  QueryPerformanceFrequency(pf);
  QueryPerformanceCounter(pc1);
  st:= args[0].Evaluate(Context);
  QueryPerformanceCounter(pc2);

  Result:= TValueList.CreateAs([TValueNumber.Create((pc2-pc1) / pf), st]);
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

