unit uMath;

interface

uses SysUtils, Classes, IniFiles, ComCtrls, Variants, Math, Windows, TypInfo,
  Messages, Graphics;

type
  Number = Extended;

  TContext = class;
  TOutput = class;
  IValue = interface;
  TExpression = class;
  IExpression = interface;
  EMathSysError = class(Exception);
  ESyntaxError = class(EMathSysError);

  TMathSystem = class
  private
    FContext: TContext;
    FOutput: TOutput;
    FConstants: TContext;
  protected
    FEvaluationStack: TStringList;
    procedure EvaluationBegin(varname: string);
    procedure EvaluationEnd;
  public
    constructor Create;
    destructor Destroy; override;
    function Parse(const Expr: string): IExpression;
    function Eval(const Expr: string): IValue;
    procedure Run(const Expr: string);
    property Output: TOutput read FOutput;
    property Context: TContext read FContext;
    property Constants: TContext read FConstants;

    procedure NewContext(const Name: string);
    procedure DropContext;
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
    procedure Clear;
    property Render: TRichEdit read FRender write FRender;
  end;

  TContext = class
  private
    FExpressions: THashedStringList;
    FParent: TContext;
    FSystem: TMathSystem;
    FSilent: boolean;
    FContextName: string;
    function GetCount: integer;
    function GetName(index: integer): string;
  public
    constructor Create(ASystem: TMathSystem; AParent: TContext);
    destructor Destroy; override;
    property Parent: TContext read FParent;
    function Bake: TContext;
    property System: TMathSystem read FSystem;
    property ContextName: string read FContextName write FContextName;
    property Silent: boolean read FSilent write FSilent;
    procedure Define(const Name: string; Expression: IExpression);
    procedure DefineValue(const Name: string; Value: IValue);
    procedure Undefine(const Name: string);
    function Definition(const Name: string): IExpression;
    function Defines(const Name: string): boolean;
    function Value(const Name: string): IValue;

    property Count: integer read GetCount;
    property Name[index: integer]: string read GetName;
  end;

  TValueType = (vtUnassigned, vtNull, vtNumber, vtString);
  IValue = interface
    ['{6E37EAE1-DD73-4825-893D-970D168165EE}']
    function ValueType: TValueType;
    procedure SetNumber(const num: Number);
    procedure SetString(const str: string);
    function GetNumber: Number;
    function GetString: string;
    procedure SetNull;
    procedure SetUnassigned;
    function AsNative: IValue;
  end;

  IStringConvertible = interface
    ['{0CB290C2-D6A2-48EE-8037-9D7F43428ED6}']
    function OutputForm: string;
    function StringForm: string;
  end;

  IValueList = interface
    ['{5A1493DB-DD9F-42EA-9054-A4BC1BDAC9B9}']
    function GetLength: integer;
    procedure SetLength(const Value: Integer);
    function GetItem(Index: Integer): IValue;
    procedure SetItem(Index: Integer; const Value: IValue);

    property Length: integer read GetLength write SetLength;
    property ListItem[Index: integer]: IValue read GetItem write SetItem;
  end;

  IValueObject = interface
    ['{D7CDF405-E960-4DDE-A0B8-2E5B5B6BF768}']
    function GetObject: TObject;
    function GetClass: TClass;
  end;

  TExpressionClass = class of TExpression;

  IExpression = interface
    ['{8CD322FF-C125-4507-9519-05C536496CDD}']
    function GetLHS: IExpression;
    function GetRHS: IExpression;
    procedure SetLHS(const Value: IExpression);
    procedure SetRHS(const Value: IExpression);

    function Evaluate(Context: TContext): IValue;
    function StringForm: string;

    function GetClassType: TClass;
    function GetObject: TExpression;

    property LHS: IExpression read GetLHS write SetLHS;
    property RHS: IExpression read GetRHS write SetRHS;
  end;

  TExpression = class(TInterfacedObject, IExpression, IStringConvertible)
  private
    LHS, RHS: IExpression;
    function GetLHS: IExpression;
    function GetRHS: IExpression;
    procedure SetLHS(const Value: IExpression);
    procedure SetRHS(const Value: IExpression);
  public
    function Evaluate(Context: TContext): IValue; virtual; abstract;
    function GetClassType: TClass;
    function GetObject: TExpression;
    function OutputForm: string; virtual;
    function StringForm: string; virtual;
    constructor Create;
  end;

  TValue = class(TInterfacedObject, IValue, IStringConvertible)
  private
    FValueType: TValueType;
    FNumber: Number;
    FString: string;
  public
    constructor Create(Val: Number); overload;
    constructor Create(Val: string); overload;
    constructor CreateUnassigned;
    constructor CreateNull;
    //IValue
    function ValueType: TValueType;
    procedure SetNumber(const num: Number);
    procedure SetString(const str: string);
    function GetNumber: Number;
    function GetString: string;
    function AsNative: IValue;
    procedure SetNull;
    procedure SetUnassigned;
    //IStringConvertible
    function StringForm: string; virtual;
    function OutputForm: string; virtual;

    class function CheckForTuples(Param: IValue; count: integer): boolean;
  end;

  TE_Constant = class(TExpression)
  private
    FValue: IValue;
  public
    constructor Create(Val: IValue); overload;
    function Evaluate(Context: TContext): IValue; override;
    function StringForm: string; override;
  end;

  TValueObject = class(TValue, IValueObject)
  private
    FObject: TObject;
  public
    constructor Create(Obj: TObject);
    destructor Destroy; override;
    function StringForm: string; override;
    function OutputForm: string; override;
    // IValueObject
    function GetClass: TClass;
    function GetObject: TObject;
  end;

  TValueGenericList = class(TValue, IValueList)
  protected
    function GetItem(Index: Integer): IValue; virtual; abstract;
    function GetLength: Integer; virtual; abstract;
    procedure SetItem(Index: Integer; const Value: IValue); virtual; abstract;
    procedure SetLength(const Value: Integer); virtual; abstract;
  public
    constructor Create;
    function StringForm: string; override;
  end;

  TValueFixedList = class(TValueGenericList)
  private
    FItems: array of IValue;
  protected
    function GetItem(Index: Integer): IValue; override;
    function GetLength: Integer; override;
    procedure SetItem(Index: Integer; const Value: IValue); override;
    procedure SetLength(const Value: Integer); override;
  public
    function OutputForm: string; override;
  end;

  TValueRangeList = class(TValueGenericList)
  private
    FStart, FStep, FEnd: Number;
  protected
    function GetItem(Index: Integer): IValue; override;
    function GetLength: Integer; override;
    procedure SetItem(Index: Integer; const Value: IValue); override;
    procedure SetLength(const Value: Integer); override;
  public
    constructor Create(Start, Step, Max: Number);
    function OutputForm: string; override;
  end;

  TE_ExprRef = class(TExpression)
  private
    FName: string;
  public
    constructor Create(Name: string);
    function Evaluate(Context: TContext): IValue; override;
    function StringForm: string; override;
    property Name: string read FName;
  end;

  TDynamicArgument = record
    Name: string;
    Value: IValue;
  end;

  TExprList = array of IExpression;

  TDynamicArguments = class
  private
    FItems: array of TDynamicArgument;
    function GetValue(Name: string): IValue;
  protected
    procedure Add(Name: string; Value: IValue);
  public
    constructor Create(Args: TExprList; FromIndex: integer; Context: TContext);
    function IsSet(Name: string): boolean;
    function GetDefault(Name: string; Default: IValue): IValue;
    property Value[Name: string]: IValue read GetValue; default;
  end;

  TUDFHeader = function(Context: TContext; Args: TExprList): IValue of object;
  TUDFHeaderOptions = function(Context: TContext; Args: TExprList; Options: TDynamicArguments): IValue of object;
  TFunctionPackageClass = class of TFunctionPackage;
  TFunctionPackage = class
  protected
    class procedure PublishedMethods(Names: TStringList);
    class function RegisterPackageFirst(Package: TFunctionPackageClass): boolean;
  public
    class function FunctionExists(FunctionName: string; ParamCount: integer): boolean;
    class function GetFunction(FunctionName: string; ParamCount: integer; out DynFrom: integer): TUDFHeader;
    class function RegisterPackage(Package: TFunctionPackageClass): boolean;
  end;

  TE_FunctionCall = class(TExpression)
  private
    FName: string;
    Arguments: IExpression;
  public
    constructor Create(Name: string);
    function Evaluate(Context: TContext): IValue; override;
    function StringForm: string; override;

    class function CheckSysCalls(StringForm: string): Boolean;
  end;

  TPackageCore = class(TFunctionPackage)
  published
    class function Undef_1(Context: TContext; args: TExprList): IValue;
    class function New_0(Context: TContext; args: TExprList): IValue;
    class function New_1(Context: TContext; args: TExprList): IValue;
    class function Drop_0(Context: TContext; args: TExprList): IValue;
    class function Clear_0(Context: TContext; args: TExprList): IValue;

    class function const_1(Context: TContext; args: TExprList): IValue;
    class function constinfo_0(Context: TContext; args: TExprList): IValue;
    class function constinfo_1(Context: TContext; args: TExprList): IValue;

    class function nvl_2(Context: TContext; args: TExprList): IValue;
  end;

  TE_ArgList = class(TExpression)
  private
    function CollectAll: TExprList;
  public
    function Evaluate(Context: TContext): IValue; override;
    function StringForm: string; override;
  end;

  TE_Subcontext = class(TExpression)
  private
    FName: string;
    Arguments: IExpression;
  public
    constructor Create(Name: string);
    function Evaluate(Context: TContext): IValue; override;
    function StringForm: string; override;
  end;

  TE_Describe = class(TExpression)
  private
  public
    function Evaluate(Context: TContext): IValue; override;
  end;

  TE_Character = class(TExpression)
  private
  public
    function Evaluate(Context: TContext): IValue; override;
  end;

  TE_AssignmentDynamic = class(TExpression)
  private
  public
    function Evaluate(Context: TContext): IValue; override;
  end;

  TE_AssignmentStatic = class(TExpression)
  private
  public
    function Evaluate(Context: TContext): IValue; override;
  end;

  TE_Power = class(TExpression)
  private
  public
    function Evaluate(Context: TContext): IValue; override;
  end;

  TE_Multiplication = class(TExpression)
  private
  public
    function Evaluate(Context: TContext): IValue; override;
  end;

  TE_Division = class(TExpression)
  private
  public
    function Evaluate(Context: TContext): IValue; override;
  end;

  TE_Modulus = class(TExpression)
  private
  public
    function Evaluate(Context: TContext): IValue; override;
  end;

  TE_Addition = class(TExpression)
  private
  public
    function Evaluate(Context: TContext): IValue; override;
  end;

  TE_Subtraction = class(TExpression)
  private
  public
    function Evaluate(Context: TContext): IValue; override;
  end;

  TE_Concatenation = class(TExpression)
  private
  public
    function Evaluate(Context: TContext): IValue; override;
  end;

  TE_ListAccess = class(TExpression)
  private
  public
    function Evaluate(Context: TContext): IValue; override;
  end;

  TE_Negation = class(TExpression)
  private
  public
    function Evaluate(Context: TContext): IValue; override;
  end;

  TExpressionDef = record
    P: integer;
    Infix: string;
    Unary: boolean;
    Cls: TExpressionClass;
  end;

const
  Expressions: array[0..12] of TExpressionDef = (
    (P: 10; Infix: '?'; Unary: true; Cls: TE_Describe),
    (P: 15; Infix: '#'; Unary: true; Cls: TE_Character),

    //    (P: 15; Infix: '-'; Unary: True; Cls: TE_Negation), // Done by Parse/Fold if it finds a Subtraction with no LHS

    (P: 18; Infix: '@'; Unary: False; Cls: TE_ListAccess),

    (P: 19; Infix: '^'; Unary: False; Cls: TE_Power),
    (P: 20; Infix: '*'; Unary: False; Cls: TE_Multiplication),
    (P: 20; Infix: '/'; Unary: False; Cls: TE_Division),
    (P: 20; Infix: '%'; Unary: False; Cls: TE_Modulus),

    (P: 30; Infix: '+'; Unary: False; Cls: TE_Addition),
    (P: 30; Infix: '-'; Unary: False; Cls: TE_Subtraction),

    (P: 40; Infix: '||'; Unary: False; Cls: TE_Concatenation),

    (P: 100; Infix: '='; Unary: False; Cls: TE_AssignmentStatic),
    (P: 105; Infix: ':='; Unary: False; Cls: TE_AssignmentDynamic),
    (P: 110; Infix: ','; Unary: False; Cls: TE_ArgList)
    );

var
  NeutralFormatSettings: TFormatSettings;

type
  TConstantDef = record
    LongName: string;
    Value: Number;
    Uni,
      Comment: string;
  end;

const
  cPi: Number = 3.1415926535897932384626433832795028841972; // required for tests
  MathematicalConstants: array[0..7] of TConstantDef = (
    (LongName: 'Pi'; Value: 3.1415926535897932384626433832795028841972),
    (LongName: 'E'; Value: 2.7182818284590452353602874713526624977572),
    (LongName: 'Degree'; Value: 0.0174532925199432957692369076848861271344),
    (LongName: 'GoldenRatio'; Value: 1.6180339887498948482045868343656381177203),
    (LongName: 'EulerGamma'; Value: 0.57721566490153286060651209008240243104216),
    (LongName: 'Catalan'; Value: 0.91596559417721901505460351493238411077415),
    (LongName: 'Glaisher'; Value: 1.2824271291006226368753425688697917277677),
    (LongName: 'Khinchin'; Value: 2.6854520010653064453097148354817956938204)
    );
  PhysicalConstants: array[0..37] of TConstantDef = (
    //Auswahl nach: Formelsammlung, PAETEC. 4. Auflage, Berlin, 2004
    //Quelle dort: CODATA
    //Erweitert: Wolfram Research Mathematica, PhysicalConstants package (Auswahl)
      //Measurement
    (LongName: 'AbsoluteZero'; Value: - 273.15; Uni: '°C'; Comment: 'for conversion purposes'),
    (LongName: 'AtomicUnit'; Value: 1.660539E-27; Uni: 'kg'),
    (LongName: 'SpeedOfLight'; Value: 2.99792458E8; Uni: 'm/s'),
    (LongName: 'SpeedOfSound'; Value: 343.051; Uni: 'm/s'),
    (LongName: 'Planck'; Value: 6.626069E-34; Uni: 'J/s'),
    (LongName: 'PlanckMass'; Value: 2.177E-8; Uni: 'kg'),
    (LongName: 'Rydberg'; Value: 1.097373E7; Uni: 'm^-1'),
    //Electromagnetics
    (LongName: 'ElectricalFieldConstant'; Value: 8.854187818E-12; Uni: 'A s/V m'),
    (LongName: 'MagneticFieldConstant'; Value: 1.256637061E-6; Uni: 'V s/A m'),
    //Thermodynamics
    (LongName: 'TriplePointWater'; Value: 273.16; Uni: 'K'),
    (LongName: 'StefanBoltzmann'; Value: 5.670400E-8; Uni: 'W/m^2 K^4'),
    (LongName: 'MolarGasConstant'; Value: 8.314472; Uni: 'J/K mol'),
    (LongName: 'WiensDisplacement'; Value: 2.8977685E-3; Uni: 'm K'),
    //Astronomy
    (LongName: 'GravitationConstant'; Value: 6.673E-11; Uni: 'm^3 / kg s^2'),
    (LongName: 'EarthSolarConstant'; Value: 1.366E3; Uni: 'W/m^2'),
    (LongName: 'EarthApparentGravity'; Value: 9.80665; Uni: 'm/s^2'),
    (LongName: 'EarthRadius'; Value: 6.371E6; Uni: 'm'),
    (LongName: 'EarthMass'; Value: 5.9742E24; Uni: 'kg'),
    (LongName: 'SolRadius'; Value: 6.955E8; Uni: 'm'),
    (LongName: 'SolMass'; Value: 1.988435E30; Uni: 'kg'),
    (LongName: 'SolLuminosity'; Value: 3.846E26; Uni: 'W'),
    (LongName: 'AstronomicalUnit'; Value: 149.6E9; Uni: 'm'),
    (LongName: 'Lightyear'; Value: 9.4605E15; Uni: 'm'),
    (LongName: 'Parsec'; Value: 3.08568E16; Uni: 'm'),
    //Chemistry
    (LongName: 'Avogadro'; Value: 6.022142E32; Uni: 'mol^-1'),
    (LongName: 'Boltzmann'; Value: 1.380650E-23; Uni: 'J/K'; Comment: 'MolarGasConstant/Avogadro'),
    (LongName: 'MolarVolume'; Value: 22.414E-3; Uni: 'm^3/mol'),
    (LongName: 'Faraday'; Value: 9.648534E4; Uni: 'A s/mol'; Comment: 'Avogadro*ElectronCharge'),
    (LongName: 'Loschmidt'; Value: 2.686778E25; Uni: 'm^-3'; Comment: 'Avogadro/MolarVolume'),
    (LongName: 'StandardPressure'; Value: 101325; Uni: 'Pa'),
    (LongName: 'StandardTemperature'; Value: 273.15; Uni: 'K'),
    //Paricles
    (LongName: 'ElectronComptonWavelength'; Value: 2.426310303E-12; Uni: 'm'; Comment: 'Planck/(ElectronMass*SpeedOfLight)'),
    (LongName: 'ElectronCharge'; Value: 1.60217646E-19; Uni: 'C'),
    (LongName: 'ElectronMass'; Value: 9.10938188E-31; Uni: 'kg'),
    (LongName: 'NeutronComptonWavelength'; Value: 1.319590943E-15; Uni: 'm'; Comment: 'Planck/(NeutronMass*SpeedOfLight)'),
    (LongName: 'NeutronMass'; Value: 1.67492716E-27; Uni: 'kg'),
    (LongName: 'ProtonComptonWavelength'; Value: 1.321409898E-15; Uni: 'm'; Comment: 'Planck/(ProtonMass*SpeedOfLight)'),
    (LongName: 'ProtonMass'; Value: 1.67262158E-27; Uni: 'kg')
    );

var
  FunctionPackages: array of TFunctionPackageClass;

implementation

uses
  uFunctions, uFunctionsGraphing, StrUtils;

resourcestring
  sConstants = 'Constants';
  sWork = 'Work';

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

constructor TMathSystem.Create;
  procedure imp(sn, ln: string);
  begin
    Parse(sn + '=const(''' + ln + ''')').Evaluate(FConstants);
  end;
begin
  inherited;
  FEvaluationStack:= TStringList.Create;
  FContext:= TContext.Create(Self, nil);
  FContext.ContextName:= sConstants;
  FConstants:= FContext;
  NewContext(sWork);
  FOutput:= TOutput.Create;
  imp('pi', 'Pi');
  imp('e', 'E');
end;

destructor TMathSystem.Destroy;
var
  ctx, fr: TContext;
begin
  fr:= FContext;
  while Assigned(fr) do begin
    ctx:= fr.Parent;
    FreeAndNil(fr);
    fr:= ctx;
  end;
  FreeAndNil(FOutput);
  FreeAndNil(FEvaluationStack);
  inherited;
end;

function TMathSystem.Parse(const Expr: string): IExpression;
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
      tokOperator: (OpIdx: integer);
  end;
  TTokenList = array of TToken;
var
  Tokens: TTokenList;

  procedure Tokenize;
  var
    p, i: integer;
    t: TToken;
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
        ' ': begin
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
        for i:= 0 to high(Expressions) do begin
          if (Expressions[i].Infix > '') and
            SameText(Expressions[i].Infix, Copy(Expr, p, length(Expressions[i].Infix))) then begin
            t.Kind:= tokOperator;
            t.Value:= expressions[i].Infix;
            t.OpIdx:= i;
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
    i, A, eid, ll, rr: integer;
    tmp: IExpression;
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
            Tokens[i].Expr:= TE_Constant.Create(TValue.Create(StrToFloat(Tokens[i].Value, NeutralFormatSettings)));
          end;
        tokString: begin
            Tokens[i].Kind:= tokExpression;
            b:= PChar(Tokens[i].Value);
            Tokens[i].Expr:= TE_Constant.Create(TValue.Create(AnsiExtractQuotedStr(b, CharQuote)));
          end;
        tokExprRef: begin
            Tokens[i].Kind:= tokExpression;
            Tokens[i].Expr:= TE_ExprRef.Create(Tokens[i].Value);
          end;
        tokExprContext: begin
            Tokens[i].Kind:= tokExpression;
            tmp:= TE_Subcontext.Create(Tokens[i].Value);
            A:= NextR(i);
            if Tokens[A].Kind = tokEmpty then
              TE_Subcontext(tmp.GetObject).Arguments:= nil
            else
              TE_Subcontext(tmp.GetObject).Arguments:= Tokens[A].Expr;
            Tokens[A].Expr:= nil;
            Tokens[a].Kind:= tokVoid;
            Tokens[i].Expr:= tmp;
          end;
        tokFuncRef: begin
            Tokens[i].Kind:= tokExpression;
            tmp:= TE_FunctionCall.Create(Tokens[i].Value);
            A:= NextR(i);
            if Tokens[A].Kind = tokEmpty then
              TE_FunctionCall(tmp.GetObject).Arguments:= nil
            else
              TE_FunctionCall(tmp.GetObject).Arguments:= Tokens[A].Expr;
            Tokens[A].Expr:= nil;
            Tokens[a].Kind:= tokVoid;
            Tokens[i].Expr:= tmp;
          end;
        tokList: begin
            Tokens[i].Kind:= tokExpression;
            tmp:= TE_FunctionCall.Create('L');
            A:= NextR(i);
            if A >= 0 then begin
              if Tokens[A].Kind = tokEmpty then
                TE_FunctionCall(tmp.GetObject).Arguments:= nil
              else
                TE_FunctionCall(tmp.GetObject).Arguments:= Tokens[A].Expr;
              Tokens[A].Expr:= nil;
              Tokens[A].Kind:= tokVoid;
            end else
              TE_FunctionCall(tmp.GetObject).Arguments:= nil;
            Tokens[i].Expr:= tmp;
          end;
      end;

    //finally, operators
    // Note: this wastes some extra iterations per Prio-Class, just ignore that, okay?
    for A:= 0 to high(Expressions) do begin
      for i:= L to R do
        if (Tokens[i].Kind = tokOperator) and
          (Expressions[Tokens[i].OpIdx].P = Expressions[A].P) then begin
          eid:= Tokens[i].OpIdx;
          rr:= NextR(i);
          ll:= NextL(i);

          if rr < 0 then
            raise ESyntaxError.CreateFmt('Position %d: Operator has no RHS', [Tokens[i].Pos]);

          if Tokens[rr].Kind <> tokExpression then
            raise ESyntaxError.CreateFmt('Position %d: expected expression, found %s', [Tokens[rr].Pos, Tokens[rr].Value]);

          if (Expressions[eid].Cls = TE_Subtraction) and
            ((ll < 0) or (Tokens[ll].Kind <> tokExpression)) then begin
            tmp:= TE_Negation.Create;
            tmp.RHS:= Tokens[rr].Expr;
          end else begin
            tmp:= Expressions[eid].Cls.Create;

            tmp.RHS:= Tokens[rr].Expr;
            if not Expressions[eid].Unary then begin
              if ll < 0 then
                raise ESyntaxError.CreateFmt('Position %d: Operator has no LHS', [Tokens[i].Pos]);
              if Tokens[ll].Kind <> tokExpression then
                raise ESyntaxError.CreateFmt('Position %d: expected expression, found %s', [Tokens[ll].Pos, Tokens[ll].Value]);
              tmp.LHS:= Tokens[ll].Expr;
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
begin
  Result:= nil;
  SetLength(Tokens, 0);
  Tokenize;
  Fold(0, high(Tokens));
  if CheckResults then begin
    Result:= Tokens[0].Expr;
  end;
end;

function TMathSystem.Eval(const Expr: string): IValue;
var
  ex: IExpression;
  s: string;
begin
  ex:= Parse(Expr);
  try
    if Assigned(ex) then begin
      FEvaluationStack.Clear;
      s:= ex.StringForm;
      if TE_FunctionCall.CheckSysCalls(s) then begin
        Result:= ex.Evaluate(FContext);
        if not Assigned(Result) then
          Result:= TValue.CreateUnassigned;
      end;
    end else
      Result:= TValue.CreateNull;
  finally
    ex:= nil;
  end;
end;

procedure TMathSystem.Run(const Expr: string);
var
  re: IValue;
  rs: IStringConvertible;
begin
  try
    re:= Eval(Expr);
    if re.ValueType <> vtUnassigned then begin
      if Supports(re, IStringConvertible, rs) then
        Output.Result(rs.OutputForm);
      Context.DefineValue('ans', re);
    end;
  except
    on e: EMathSysError do
      Output.Error(E.Message, []);
  end;
end;

procedure TMathSystem.DropContext;
var
  cont: TContext;
begin
  if FContext.Parent <> FConstants then begin
    cont:= FContext.Parent;
    FContext.Free;
    FContext:= cont;
  end else
    raise EMathSysError.Create('Cannot drop this context.');
end;

procedure TMathSystem.NewContext(const Name: string);
var
  cont: TContext;
begin
  cont:= TContext.Create(Self, FContext);
  cont.ContextName:= Name;
  FContext:= cont;
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

{ TOutput }

constructor TOutput.Create;
begin
  inherited;
  FRender:= nil;
end;

procedure TOutput.LineOut(const Line: string; Indent: integer; Color: TColor; Style: TFontStyles);
var
  p: integer;
begin
  FRender.SelStart:= length(FRender.Text);
  FRender.SelLength:= 0;
  if Indent < 0 then begin
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
  p:= Pos(NeutralFormatSettings.ThousandSeparator, FRender.Text);
  while p > 0 do begin
    FRender.SelStart:= p - 1;
    FRender.SelLength:= 1;
    FRender.SelAttributes.Name:= 'Arial';
    FRender.SelText:= ' ';
    p:= Pos(NeutralFormatSettings.ThousandSeparator, FRender.Text);
  end;
  PostMessage(FRender.Handle, EM_SCROLLCARET, 0, 0);
end;

procedure TOutput.Hint(const Line: string; Params: array of const);
begin
  LineOut(Format(Line, Params), 10, clNavy, [fsItalic]);
end;

procedure TOutput.Error(const Line: string; Params: array of const);
begin
  LineOut(Format(Line, Params), 10, $009EFF, []);
end;

procedure TOutput.Result(const Line: string);
begin
  LineOut('='#9 + Line, -10, FRender.Font.Color, []);
end;

procedure TOutput.Input(const Line: string);
var
  s: string;
begin
  s:= '>'#9 + Line;
  if FRender.Lines.Count > 0 then
    s:= #13#10 + s;
  LineOut(s, -10, clDkGray, []);
end;

procedure TOutput.Clear;
begin
  FRender.Clear;
end;

{ TContext }

constructor TContext.Create(ASystem: TMathSystem; AParent: TContext);
begin
  inherited Create;
  FExpressions:= THashedStringList.Create;
  FParent:= AParent;
  FSystem:= ASystem;
  FSilent:= false;
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

function TContext.Value(const Name: string): IValue;
var
  ex: IExpression;
begin
  ex:= Definition(Name);
  if Assigned(ex) then begin
    FSystem.EvaluationBegin(Name);
    try
      Result:= ex.Evaluate(Self)
    finally
      FSystem.EvaluationEnd;
    end;
  end else begin
    Result:= TValue.CreateUnassigned;
    raise EMathSysError.CreateFmt('Expression unknown in current Context: %s', [Name]);
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

procedure TContext.DefineValue(const Name: string; Value: IValue);
begin
  if Value.ValueType <> vtUnassigned then
    Define(Name, TE_Constant.Create(Value));
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
      Cook(C.Parent);
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

function TExpression.OutputForm: string;
begin
  Result:= StringForm;
end;

function TExpression.StringForm: string;
begin
  if LHS = nil then
    Result:= format('%s(%s)', [Copy(ClassName, 4, 10000), RHS.StringForm])
  else
    Result:= format('%s(%s,%s)', [Copy(ClassName, 4, 10000), LHS.StringForm, RHS.StringForm]);
end;

{ TValue }

constructor TValue.Create(Val: Number);
begin
  inherited Create;
  FValueType:= vtNumber;
  FNumber:= Val;
end;

constructor TValue.Create(Val: string);
begin
  inherited Create;
  FValueType:= vtString;
  FString:= Val;
end;

constructor TValue.CreateUnassigned;
begin
  inherited Create;
  FValueType:= vtUnassigned;
end;

constructor TValue.CreateNull;
begin
  inherited Create;
  FValueType:= vtNull;
end;

function TValue.StringForm: string;
begin
  case FValueType of
    vtUnassigned: Result:= '<?>';
    vtNull: Result:= 'null';
    vtNumber: Result:= NumberToStr(FNumber, NeutralFormatSettings, false);
    vtString: Result:= QuotedStr(GetString);
  end;
end;

function TValue.GetNumber: Number;
begin
{$WARNINGS OFF}
  case FValueType of
    vtNumber: Result:= FNumber;
    vtString: if not TryStrToFloat(FString, Result, NeutralFormatSettings) then
      Result:= NaN;
  else
    Result:= NAN;
  end;
{$WARNINGS ON}
end;

function TValue.GetString: string;
begin
  case FValueType of
    vtNumber: Result:= NumberToStr(FNumber, NeutralFormatSettings, false);
    vtString: Result:= FString;
  else
    Result:= '';
  end;
end;

procedure TValue.SetNumber(const num: Number);
begin
  FNumber:= num;
  FValueType:= vtNumber;
end;

procedure TValue.SetString(const str: string);
begin
  FString:= str;
  FValueType:= vtString;
end;

function TValue.AsNative: IValue;
var
  f: Number;
begin
  case ValueType of
    vtUnassigned,
      vtNull: Result:= self;
    vtNumber: Result:= self;
    vtString:
      if TryStrToFloat(FString, f, NeutralFormatSettings) then
        Result:= TValue.Create(f)
      else
        Result:= self;
  end;
end;

procedure TValue.SetNull;
begin
  FValueType:= vtNull;
end;

procedure TValue.SetUnassigned;
begin
  FValueType:= vtUnassigned;
end;

function TValue.ValueType: TValueType;
begin
  Result:= FValueType;
end;

function TValue.OutputForm: string;
begin
  case FValueType of
    vtNumber: Result:= NumberToStr(FNumber, NeutralFormatSettings, true);
    vtString: Result:= QuotedStr(GetString);
  end;
end;

class function TValue.CheckForTuples(Param: IValue; count: integer): boolean;
var
  l,l2: IValueList;
  i: integer;
begin
  Result:= Supports(Param, IValueList, l);
  if Result then
    for i:= 0 to l.Length - 1 do
      if not Supports(l.ListItem[0], IValueList, l2) or
         (l2.Length <> count) then begin
           Result:= false;
           exit;
         end;
end;

{ TE_Constant }

constructor TE_Constant.Create(Val: IValue);
begin
  inherited Create;
  FValue:= Val;
end;

function TE_Constant.Evaluate(Context: TContext): IValue;
begin
  Result:= FValue;
end;

function TE_Constant.StringForm: string;
begin
  Result:= (FValue as IStringConvertible).StringForm;
end;

{ TValueGenericList }

constructor TValueGenericList.Create;
begin
  inherited Create;
  FValueType:= vtNull;
end;

function TValueGenericList.StringForm: string;
begin
  Result:= OutputForm;
end;

{ TValueFixedList }

function TValueFixedList.GetLength: Integer;
begin
  Result:= Length(FItems);
end;

procedure TValueFixedList.SetLength(const Value: Integer);
begin
  System.SetLength(FItems, Value);
end;

function TValueFixedList.GetItem(Index: Integer): IValue;
begin
  Result:= FItems[Index];
end;

procedure TValueFixedList.SetItem(Index: Integer; const Value: IValue);
begin
  FItems[Index]:= Value;
end;

function TValueFixedList.OutputForm: string;
var
  i: integer;
  function gets(index: integer): string;
  var
    s: IStringConvertible;
  begin
    if Supports(GetItem(index), IStringConvertible, s) then
      Result:= s.OutputForm
    else
      Result:= '<Unknown>';
  end;
begin
  if GetLength = 0 then
    Result:= ''
  else begin
    Result:= gets(0);
    for i:= 1 to GetLength - 1 do begin
      if Length(Result) > 200 then begin
        Result:= Result + ', ... (' + IntToStr(GetLength - i + 1) + ')';
        break;
      end else
        Result:= Result + ', ' + gets(i);
    end;
  end;
  Result:= '{' + Result + '}';
end;

{ TValueRangeList }

function TValueRangeList.GetItem(Index: Integer): IValue;
begin
  Result:= TValue.Create(FStart + Index * FStep);
end;

function TValueRangeList.GetLength: Integer;
begin
  Result:= Trunc(1 + (FEnd - FStart) / FStep);
end;

procedure TValueRangeList.SetItem(Index: Integer; const Value: IValue);
begin
end;

procedure TValueRangeList.SetLength(const Value: Integer);
begin
end;

function TValueRangeList.OutputForm: string;
begin
  Result:= Format('{%0:s->%2:s,%1:s}', [NumberToStr(FStart, NeutralFormatSettings, false), NumberToStr(FStep, NeutralFormatSettings, false), NumberToStr(FEnd,
      NeutralFormatSettings, false)]);
end;

constructor TValueRangeList.Create(Start, Step, Max: Number);
begin
  inherited Create;
  FStart:= Start;
  FStep:= Step;
  FEnd:= Max;
end;

{ TE_ExprRef }

constructor TE_ExprRef.Create(Name: string);
begin
  inherited Create;
  FName:= Name;
end;

function TE_ExprRef.Evaluate(Context: TContext): IValue;
begin
  Result:= Context.Value(FName);
end;

function TE_ExprRef.StringForm: string;
begin
  Result:= FName;
end;

{ TFunctionPackage }

class procedure TFunctionPackage.PublishedMethods(Names: TStringList);
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
  pClass:= Self;
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

class function TFunctionPackage.GetFunction(FunctionName: string; ParamCount: integer; out DynFrom: integer): TUDFHeader;
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

class function TFunctionPackage.FunctionExists(FunctionName: string; ParamCount: integer): boolean;
var
  k: integer;
begin
  Result:= Assigned(GetFunction(FunctionName, ParamCount, k));
end;

class function TFunctionPackage.RegisterPackage(Package: TFunctionPackageClass): boolean;
var
  j: integer;
begin
  Result:= false;
  for j:= 0 to high(FunctionPackages) do
    if FunctionPackages[j] = Package then
      exit;
  j:= length(FunctionPackages);
  SetLength(FunctionPackages, j + 1);
  FunctionPackages[j]:= Package;
  Result:= true;
end;

class function TFunctionPackage.RegisterPackageFirst(Package: TFunctionPackageClass): boolean;
var
  j: integer;
begin
  Result:= false;
  for j:= 0 to high(FunctionPackages) do
    if FunctionPackages[j] = Package then
      exit;
  j:= length(FunctionPackages);
  SetLength(FunctionPackages, j + 1);
  for j:= high(FunctionPackages) downto 1 do
    FunctionPackages[j]:= FunctionPackages[j - 1];
  FunctionPackages[0]:= Package;
  Result:= true;
end;

{ TE_FunctionCall }

constructor TE_FunctionCall.Create(Name: string);
begin
  inherited Create;
  FName:= Name;
end;

function TE_FunctionCall.Evaluate(Context: TContext): IValue;
var
  u: TUDFHeader;
  ls: TExprList;
  i, d: integer;
  dyn: TDynamicArguments;
begin
  if Assigned(Arguments) then begin
    if Arguments.GetObject is TE_ArgList then
      ls:= TE_ArgList(Arguments.GetObject).CollectAll
    else begin
      SetLength(ls, 1);
      ls[0]:= Arguments;
    end;
  end else
    SetLength(ls, 0);

  for i:= 0 to high(FunctionPackages) do begin
    u:= FunctionPackages[i].GetFunction(Fname, Length(ls), d);
    if Assigned(u) then begin
      if d >= 0 then begin
        dyn:= TDynamicArguments.Create(ls, d, Context);
        try
          Result:= TUDFHeaderOptions(u)(Context, ls, dyn);
        finally
          FreeAndNil(dyn);
        end;
      end else
        Result:= u(Context, ls);
      exit;
    end;
  end;
  raise EMathSysError.CreateFmt('Function %s has no version with %d parameters', [FName, Length(ls)]);
end;

function TE_FunctionCall.StringForm: string;
begin
  if Assigned(Arguments) then
    Result:= FName + '(' + Arguments.StringForm + ')'
  else
    Result:= FName + '()';
end;

class function TE_FunctionCall.CheckSysCalls(StringForm: string): Boolean;
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

{ TPackageCore }

class function TPackageCore.Undef_1(Context: TContext; args: TExprList): IValue;
begin
  Context.Undefine(args[0].Evaluate(Context).GetString);
end;

class function TPackageCore.New_0(Context: TContext; args: TExprList): IValue;
begin
  Context.System.NewContext('');
end;

class function TPackageCore.New_1(Context: TContext; args: TExprList): IValue;
begin
  Context.System.NewContext(args[0].Evaluate(Context).GetString);
end;

class function TPackageCore.Drop_0(Context: TContext; args: TExprList): IValue;
begin
  Context.System.DropContext();
end;

class function TPackageCore.Clear_0(Context: TContext; args: TExprList): IValue;
begin
  Context.System.Output.Clear;
  Result:= TValue.CreateUnassigned;
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
  val: IValue;
begin
  val:= TValue.Create(def.Value);
  Result:= Format('%s = %s', [def.LongName, val.GetString]);
  if def.Uni > '' then
    Result:= Format('%s [%s]', [Result, def.Uni]);
  if def.Comment > '' then
    Result:= Format('%s (%s)', [Result, def.Comment]);
end;

class function TPackageCore.const_1(Context: TContext; args: TExprList): IValue;
var
  nm: string;
  res: TConstantDef;
begin
  nm:= args[0].Evaluate(Context).GetString;
  if FindConstant(nm, res) then
    Result:= TValue.Create(res.Value)
  else
    raise EMathSysError.CreateFmt('Unknown Constant: %s', [nm]);
end;

class function TPackageCore.constinfo_0(Context: TContext; args: TExprList): IValue;
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
      Context.System.Output.Hint(FormatConstInfo(List^[i]), []);
      Inc(Count);
    end;
  end;
begin
  Count:= 0;
  ListIn(@MathematicalConstants, Low(MathematicalConstants), high(MathematicalConstants));
  ListIn(@PhysicalConstants, Low(PhysicalConstants), high(PhysicalConstants));
  Result:= TValue.Create(Count);
end;

class function TPackageCore.constinfo_1(Context: TContext; args: TExprList): IValue;
var
  nm: string;
  res: TConstantDef;
begin
  nm:= args[0].Evaluate(Context).GetString;
  if FindConstant(nm, res) then begin
    Result:= TValue.Create(FormatConstInfo(res));
  end else
    raise EMathSysError.CreateFmt('Unknown Constant: %s', [nm]);
end;

class function TPackageCore.nvl_2(Context: TContext; args: TExprList): IValue;
begin
  if args[0].Evaluate(Context).ValueType in [vtNull, vtUnassigned] then
    Result:= args[1].Evaluate(Context)
  else
    Result:= args[0].Evaluate(Context);
end;

{ TDynamicArguments }

constructor TDynamicArguments.Create(Args: TExprList; FromIndex: integer; Context: TContext);
var
  i: integer;
  ex: IExpression;
begin
  for i:= FromIndex to High(Args) do begin
    if Args[i].GetObject is TE_ExprRef then
      Add((Args[i].GetObject as TE_ExprRef).Name, TValue.CreateNull)
    else if Args[i].GetObject is TE_AssignmentStatic then begin
      ex:= args[i].LHS;
      if ex.GetObject is TE_ExprRef then
        Add((ex.GetObject as TE_ExprRef).Name, Args[i].RHS.Evaluate(Context));
    end;
  end;
end;

procedure TDynamicArguments.Add(Name: string; Value: IValue);
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

function TDynamicArguments.GetValue(Name: string): IValue;
begin
  Result:= GetDefault(Name, TValue.CreateUnassigned);
end;

function TDynamicArguments.GetDefault(Name: string; Default: IValue): IValue;
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

{ TE_ArgList }

function TE_ArgList.CollectAll: TExprList;
  procedure SubCollect(e: IExpression);
  var
    tmp: TExprList;
    i: integer;
    o: TExpression;
  begin
    if e = nil then
      exit;
    o:= e.GetObject;
    if o is TE_ArgList then begin
      tmp:= TE_ArgList(o).CollectAll;
      for i:= 0 to high(tmp) do begin
        SetLength(Result, Length(Result) + 1);
        Result[high(Result)]:= tmp[i];
      end;
    end else begin
      SetLength(Result, Length(Result) + 1);
      Result[high(Result)]:= e;
    end;
  end;
begin
  SetLength(Result, 0);
  SubCollect(LHS);
  SubCollect(RHS);
end;

function TE_ArgList.Evaluate(Context: TContext): IValue;
begin
  LHS.Evaluate(Context);
  Result:= RHS.Evaluate(Context);
end;

function TE_ArgList.StringForm: string;
var
  list: TExprList;
  i: integer;
begin
  list:= CollectAll;
  if Length(list) > 0 then begin
    Result:= list[0].StringForm;
    for i:= 1 to high(list) do
      Result:= Result + ',' + list[i].StringForm;
    Result:= 'ArgList(' + Result + ')';
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

function TE_Subcontext.Evaluate(Context: TContext): IValue;
var
  ctx: TContext;
begin
  ctx:= TContext.Create(Context.System, Context);
  try
    if Assigned(Arguments) then
      Arguments.Evaluate(ctx);
    Result:= ctx.Value(FName);
  finally
    FreeAndNil(ctx);
  end;
end;

function TE_Subcontext.StringForm: string;
begin
  if Assigned(Arguments) then
    Result:= FName + '[' + Arguments.StringForm + ']'
  else
    Result:= FName + '[]';
end;

{ TE_Describe }

function TE_Describe.Evaluate(Context: TContext): IValue;
var
  name: string;
  e: IExpression;
begin
  if RHS.GetObject is TE_ExprRef then begin
    name:= (RHS.GetObject as TE_ExprRef).FName;
    e:= Context.Definition(name);
  end else
    e:= RHS;
  if Assigned(e) then
    Result:= TValue.Create(e.StringForm)
  else
    Result:= TValue.Create('<Unknown>');
end;

{ TE_Character }

function TE_Character.Evaluate(Context: TContext): IValue;
var
  c: Integer;
begin
  c:= trunc(RHS.Evaluate(Context).GetNumber);
  Result:= TValue.Create(chr(c));
end;

{ TE_AssignmentDynamic }

function TE_AssignmentDynamic.Evaluate(Context: TContext): IValue;
var
  name: string;
begin
  if not (LHS.GetObject is TE_ExprRef) then
    raise ESyntaxError.Create('LHS of assignment needs to be a simple expression reference');
  name:= (LHS.GetObject as TE_ExprRef).FName;
  Context.Define(name, RHS);
end;

{ TE_AssignmentStatic }

function TE_AssignmentStatic.Evaluate(Context: TContext): IValue;
var
  name: string;
  v: IValue;
begin
  if not (LHS.GetObject is TE_ExprRef) then
    raise ESyntaxError.Create('LHS of assignment needs to be a simple expression reference');
  name:= (LHS.GetObject as TE_ExprRef).FName;
  v:= RHS.Evaluate(Context);
  Context.DefineValue(name, v);
  Result:= v;
end;

{ TE_Power }

function TE_Power.Evaluate(Context: TContext): IValue;
begin
  Result:= TValue.Create(Power(LHS.Evaluate(Context).GetNumber, RHS.Evaluate(Context).GetNumber));
end;

{ TE_Multiplication }

function TE_Multiplication.Evaluate(Context: TContext): IValue;
begin
  Result:= TValue.Create(LHS.Evaluate(Context).GetNumber * RHS.Evaluate(Context).GetNumber);
end;

{ TE_Division }

function TE_Division.Evaluate(Context: TContext): IValue;
var
  a, d: Number;
begin
  a:= LHS.Evaluate(Context).GetNumber;
  d:= RHS.Evaluate(Context).GetNumber;
  if IsZero(d) then begin
    if IsZero(a) then
      Result:= TValue.Create(NaN)
    else if a < 0 then
      Result:= TValue.Create(NegInfinity)
    else if a > 0 then
      Result:= TValue.Create(Infinity);
  end else
    Result:= TValue.Create(a / d)
end;

{ TE_Modulus }

function TE_Modulus.Evaluate(Context: TContext): IValue;
var
  a, b, c, f: Extended;
begin
  a:= LHS.Evaluate(Context).GetNumber;
  b:= RHS.Evaluate(Context).GetNumber;
  c:= a / b;
  if c > 1000000 then begin
    c:= Int(c);
    f:= a - b * c;
  end else
    f:= frac(c) * b;
  Result:= TValue.Create(round(f));
end;

{ TE_Addition }

function TE_Addition.Evaluate(Context: TContext): IValue;
begin
  Result:= TValue.Create(LHS.Evaluate(Context).GetNumber + RHS.Evaluate(Context).GetNumber);
end;

{ TE_Subtraction }

function TE_Subtraction.Evaluate(Context: TContext): IValue;
begin
  Result:= TValue.Create(LHS.Evaluate(Context).GetNumber - RHS.Evaluate(Context).GetNumber);
end;

{ TE_Concatenation }

function TE_Concatenation.Evaluate(Context: TContext): IValue;
begin
  Result:= TValue.Create(LHS.Evaluate(Context).GetString + RHS.Evaluate(Context).GetString);
end;

{ TE_ListAccess }

function TE_ListAccess.Evaluate(Context: TContext): IValue;
var
  a: IValueList;
  f: integer;
begin
  if Supports(LHS.Evaluate(Context), IValueList, a) then begin
    f:= trunc(RHS.Evaluate(Context).GetNumber);
    if f < 0 then
      f:= a.Length + f;
    if f < 0 then
      f:= 0;
    if (f >= 0) and (f < a.Length) then
      Result:= a.ListItem[f].AsNative
    else
      Result:= TValue.CreateNull;
  end else
    Result:= TValue.CreateNull;
end;

{ TE_Negation }

function TE_Negation.Evaluate(Context: TContext): IValue;
begin
  Result:= TValue.Create(0 - RHS.Evaluate(Context).GetNumber);
end;

{ TValueObject }

constructor TValueObject.Create(Obj: TObject);
begin
  inherited Create;
  FObject:= Obj;
  FValueType:= vtNull;
end;

destructor TValueObject.Destroy;
begin
  FreeAndNil(FObject);
  inherited;
end;

function TValueObject.GetClass: TClass;
begin
  Result:= FObject.ClassType;
end;

function TValueObject.GetObject: TObject;
begin
  Result:= FObject;
end;

function TValueObject.OutputForm: string;
begin
  Result:= StringForm;
end;

function TValueObject.StringForm: string;
begin
  Result:= '<' + GetClass.ClassName + '>';
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
  TFunctionPackage.RegisterPackageFirst(TPackageCore);
end.

