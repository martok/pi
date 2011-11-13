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
  public
    constructor Create;
    destructor Destroy; override;
    function Parse(const Expr: String): IExpression;
    function Eval(const Expr: String): IValue;
    procedure Run(const Expr: String);
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
    property Render : TRichEdit read FRender write FRender;
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
    property System : TMathSystem read FSystem;
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
    procedure SetString(const str: String);
    function GetNumber: Number;
    function GetString: string;
    procedure SetNull;
    procedure SetUnassigned;
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
    LHS,RHS: IExpression;
    function GetLHS: IExpression;
    function GetRHS: IExpression;
    procedure SetLHS(const Value: IExpression);
    procedure SetRHS(const Value: IExpression);
  public
    function Evaluate(Context: TContext): IValue; virtual; abstract;
    function GetClassType: TClass;
    function GetObject: TExpression;
    function OutputForm: string; virtual;
    function StringForm: String; virtual;
    constructor Create;
  end;

  TE_Constant = class(TExpression, IValue)
  private
    FValueType: TValueType;
    FNumber: Number;
    FString: String;
  public
    constructor Create(Val: IValue); overload;
    constructor Create(Val: Number); overload;
    constructor Create(Val: String); overload;
    constructor CreateUnassigned;
    constructor CreateNull;
    function Evaluate(Context: TContext): IValue; override;
    //IValue
    function ValueType: TValueType;
    procedure SetNumber(const num: Number);
    procedure SetString(const str: String);
    function GetNumber: Number;
    function GetString: string;
    procedure SetNull;
    procedure SetUnassigned;
    //IStringConvertible
    function StringForm: String; override;
    function OutputForm: string; override;
  end;

  TE_ExprRef = class(TExpression)
  private
    FName: string;
  public
    constructor Create(Name: string);
    function Evaluate(Context: TContext): IValue; override;
    function StringForm: String; override;
  end;

  TExprList = array of IExpression;
  TUDFHeader = function(Context: TContext; args: TExprList) : IValue of object;
  TE_FunctionCall = class(TExpression)
  private
    FName: string;
    Arguments: IExpression;
  public
    constructor Create(Name: string);
    function Evaluate(Context: TContext): IValue; override;
    function StringForm: String; override;

    class function CheckSysCalls(StringForm: string): Boolean;
  published
    function Abs_1(Context: TContext; args: TExprList): IValue;
    function Exp_1(Context: TContext; args: TExprList): IValue;
    function Ln_1(Context: TContext; args: TExprList): IValue;
    function Lg_1(Context: TContext; args: TExprList): IValue;
    function Ld_1(Context: TContext; args: TExprList): IValue;
    function Loga_2(Context: TContext; args: TExprList): IValue;
    function Fac_1(Context: TContext; args: TExprList): IValue;

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

    function ToBase_2(Context: TContext; args: TExprList): IValue;
    function FromBase_2(Context: TContext; args: TExprList): IValue;

    function Undef_1(Context: TContext; args: TExprList): IValue;
    function New_0(Context: TContext; args: TExprList): IValue;
    function New_1(Context: TContext; args: TExprList): IValue;
    function Drop_0(Context: TContext; args: TExprList): IValue;
    function Clear_0(Context: TContext; args: TExprList): IValue;

    function const_1(Context: TContext; args: TExprList): IValue;
    function constinfo_0(Context: TContext; args: TExprList): IValue;
    function constinfo_1(Context: TContext; args: TExprList): IValue;

(*    function L_N(Context: TContext; args: TExprList): IValue;
    function Range_3(Context: TContext; args: TExprList): IValue;
    function Merge_2(Context: TContext; args: TExprList): IValue;
    function Each_3(Context: TContext; args: TExprList): IValue;
    function Aggregate_5(Context: TContext; args: TExprList): IValue;
*)  end;

  TE_ArgList = class(TExpression)
  private
    function CollectAll: TExprList;
  public
    function Evaluate(Context: TContext): IValue; override;
    function StringForm: String; override;
  end;

  TE_Subcontext = class(TExpression)
  private
    FName: string;
    Arguments: IExpression;
  public
    constructor Create(Name: string);
    function Evaluate(Context: TContext): IValue; override;
    function StringForm: String; override;
  end;

  TE_Describe = class(TExpression)
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

  TE_Negation = class(TExpression)
  private
  public
    function Evaluate(Context: TContext): IValue; override;
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

//    (P: 15; Infix: '-'; Unary: True; Cls: TE_Negation), // Done by Parse/Fold if it finds a Subtraction with no LHS 

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

type
  TConstantDef = record
    LongName: string;
    Value: Number;
    Uni,
    Comment: string;
  end;

const
  cPi : Number = 3.1415926535897932384626433832795028841972; // required for tests
  MathematicalConstants: array[0..7] of TConstantDef = (
    (LongName:             'Pi'; Value: 3.1415926535897932384626433832795028841972),
    (LongName:              'E'; Value: 2.7182818284590452353602874713526624977572),
    (LongName:         'Degree'; Value: 0.0174532925199432957692369076848861271344),
    (LongName:    'GoldenRatio'; Value: 1.6180339887498948482045868343656381177203),
    (LongName:     'EulerGamma'; Value: 0.57721566490153286060651209008240243104216),
    (LongName:        'Catalan'; Value: 0.91596559417721901505460351493238411077415),
    (LongName:       'Glaisher'; Value: 1.2824271291006226368753425688697917277677),
    (LongName:       'Khinchin'; Value: 2.6854520010653064453097148354817956938204)
  );
  PhysicalConstants: array[0..37] of TConstantDef = (
  //Auswahl nach: Formelsammlung, PAETEC. 4. Auflage, Berlin, 2004
  //Quelle dort: CODATA
  //Erweitert: Wolfram Research Mathematica, PhysicalConstants package (Auswahl)
    //Measurement
    (LongName: 'AbsoluteZero';         Value: -273.15;         Uni: '°C';         Comment: 'for conversion purposes'),
    (LongName: 'AtomicUnit';           Value: 1.660540E-21;    Uni: 'kg'),
    (LongName: 'SpeedOfLight';         Value: 2.99792458E8;    Uni: 'm/s'),
    (LongName: 'SpeedOfSound';         Value: 343.051;         Uni: 'm/s'),
    (LongName: 'Planck';               Value: 6.626069E-34;    Uni: 'J/s'),
    (LongName: 'PlanckMass';           Value: 2.177E-8;        Uni: 'kg'),
    (LongName: 'Rydberg';              Value: 1.097373E7;      Uni: 'm^-1'),
    //Electromagnetics
    (LongName: 'ElectricalFieldConstant'; Value: 8.854187818E-12; Uni: 'A s/V m'),
    (LongName: 'MagneticFieldConstant';   Value: 1.256637061E-6;  Uni: 'V s/A m'),
    //Thermodynamics
    (LongName: 'TriplePointWater';     Value: 273.16;          Uni: 'K'),
    (LongName: 'StefanBoltzmann';      Value: 5.670400E-8;     Uni: 'W/m^2 K^4'),
    (LongName: 'MolarGasConstant';     Value: 8.314472;        Uni: 'J/K mol'),
    (LongName: 'WiensDisplacement';    Value: 2.8977685E-3;    Uni: 'm K'),
    //Astronomy
    (LongName: 'GravitationConstant';  Value: 6.673E-11;       Uni: 'm^3 / kg s^2'),
    (LongName: 'EarthSolarConstant';   Value: 1.366E3;         Uni: 'W/m^2'),
    (LongName: 'EarthApparentGravity'; Value: 9.80665;         Uni: 'm/s^2'),
    (LongName: 'EarthRadius';          Value: 6.371E6;         Uni: 'm'),
    (LongName: 'EarthMass';            Value: 5.9742E24;       Uni: 'kg'),
    (LongName: 'SolRadius';            Value: 6.955E8;         Uni: 'm'),
    (LongName: 'SolMass';              Value: 1.988435E30;     Uni: 'kg'),   
    (LongName: 'SolLuminosity';        Value: 3.846E26;        Uni: 'W'),
    (LongName: 'AstronomicalUnit';     Value: 149.6E9;         Uni: 'm'),
    (LongName: 'Lightyear';            Value: 9.4605E15;       Uni: 'm'),
    (LongName: 'Parsec';               Value: 3.08568E16;      Uni: 'm'),
    //Chemistry
    (LongName: 'Avogadro';             Value: 6.022142E32;     Uni: 'mol^-1'),
    (LongName: 'Boltzmann';            Value: 1.380650E-23;    Uni: 'J/K';        Comment:'MolarGasConstant/Avogadro'),
    (LongName: 'MolarVolume';          Value: 22.414E-3;       Uni: 'm^3/mol'),
    (LongName: 'Faraday';              Value: 9.648534E4;      Uni: 'A s/mol';    Comment:'Avogadro*ElectronCharge'),
    (LongName: 'Loschmidt';            Value: 2.686778E25;     Uni: 'm^-3';       Comment:'Avogadro/MolarVolume'),
    (LongName: 'StandardPressure';     Value: 101325;          Uni: 'Pa'),
    (LongName: 'StandardTemperature';  Value: 273.15;          Uni: 'K'),
    //Paricles
    (LongName: 'ElectronComptonWavelength';  Value: 2.426310303E-12;   Uni: 'm'; Comment: 'Planck/(ElectronMass*SpeedOfLight)'),
    (LongName: 'ElectronCharge';       Value: 1.60217646E-19;  Uni: 'C'),
    (LongName: 'ElectronMass';         Value: 9.10938188E-31;  Uni: 'kg'),
    (LongName: 'NeutronComptonWavelength';   Value: 1.319590943E-15;   Uni: 'm'; Comment: 'Planck/(NeutronMass*SpeedOfLight)'),
    (LongName: 'NeutronMass';          Value: 1.67492716E-27;  Uni: 'kg'),
    (LongName: 'ProtonComptonWavelength';    Value: 1.321409898E-15;   Uni: 'm'; Comment: 'Planck/(ProtonMass*SpeedOfLight)'),
    (LongName: 'ProtonMass';           Value: 1.67262158E-27;  Uni: 'kg')
  );
  
implementation

resourcestring
  sConstants = 'Constants';
  sWork      = 'Work';

function NumberToStr(const Value: Number; FS: TFormatSettings; ShowThousands: boolean):string;
var p,e: integer;
begin
  Result:= FloatToStrF(Value, ffGeneral, 22, 18, NeutralFormatSettings);
  if Showthousands then begin
    e:= Pos('E', Result);
    if e=0 then
      e:= length(Result);

    p:= Pos(NeutralFormatSettings.DecimalSeparator, Result);
    if p=0 then p:= e+1;
    dec(p,3);
    while p>0 do begin
      Insert(NeutralFormatSettings.ThousandSeparator, Result, p);
      dec(p,3);
      inc(e);
    end;
    p:= Pos(NeutralFormatSettings.DecimalSeparator, Result);
    if p>0 then begin
      inc(p,3);
      while p<e do begin
        Insert(NeutralFormatSettings.ThousandSeparator, Result, p+1);
        inc(p,4);
        inc(e);
      end;
    end;
  end;
end;


{ TMathSystem }

constructor TMathSystem.Create;
  procedure imp(sn, ln: string);
  begin
    Parse(sn+'=const('''+ln+''')').Evaluate(FConstants);
  end;
begin
  inherited;
  FContext:= TContext.Create(Self, nil);
  FContext.ContextName:= sConstants;
  FConstants:= FContext;
  NewContext(sWork);
  FOutput:= TOutput.Create;
  imp('pi','Pi');
  imp('e', 'E');
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

function TMathSystem.Parse(const Expr: String): IExpression;
const
  CharQuote = '''';

  CharBraceOpen = '(';
  CharBraceClose = ')';
  CharContextOpen = '[';
  CharContextClose = ']';
type
  TTokenKind = (tokVoid, tokExpression, tokEmpty,
                tokNumber, tokString, tokFuncRef, tokExprRef, tokExprContext,
                tokBraceOpen, tokBraceClose, tokContextOpen, tokContextClose,
                tokOperator);
  TToken= record
    Pos: integer;
    Value: String;
    Expr: IExpression;
    case Kind: TTokenKind of
      tokVoid: ();
      tokExpression: ();
      tokNumber: ();
      tokString: ();
      tokFuncRef: ();
      tokExprRef: ();
      tokExprContext: ();
  //  tokBraceOpen, tokBraceClose, tokContextOpen, tokContextClose
      tokOperator: (OpIdx: integer);
  end;
  TTokenList= array of TToken;
var Tokens: TTokenList;

  procedure Tokenize;
  var p, i:integer;
      t: TToken;
    procedure ParseNumber(var Nr: TToken);
    var mode: (tmNumberSign, tmNumber, tmNumberDecimals, tmNumberExponentSign, tmNumberExponent);
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
      while p<=Length(Expr) do begin
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
      while p<=Length(Expr) do begin
       if Expr[p] in ['a'..'z','A'..'Z','_','0'..'9'] then
          data:= data + Expr[p]
        else
          Break;
        inc(p);
      end;
      case  Expr[p] of
        CharBraceOpen: Id.Kind:= tokFuncRef;
        CharContextOpen: Id.Kind:= tokExprContext;
      else
        Id.Kind:= tokExprRef;
      end;
      Id.Value:= data;
    end;

  begin
    p:= 1;
    while p<=length(expr) do begin
      t.Value:= '';
      FillChar(t,sizeof(t),0);
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
        'a'..'z','A'..'Z','_': ParseIdentifier(t);
      else
        for i:= 0 to high(Expressions) do begin
          if (Expressions[i].Infix>'') and
             SameText(Expressions[i].Infix, Copy(Expr, p, length(Expressions[i].Infix))) then begin
            t.Kind:= tokOperator;
            t.Value:= expressions[i].Infix;
            t.OpIdx:= i;
            inc(p, length(t.Value));
            break;
          end;
        end;
        if t.Kind=tokVoid then 
          raise ESyntaxError.CreateFmt('Position %d: Unexpected Character %s',[p, Expr[p]]);
      end;

      SetLength(Tokens, Length(Tokens)+1);
      Tokens[high(Tokens)]:= t;
    end;
  end;

  procedure Fold(L,R: integer);
  var i,A,eid,ll,rr:integer;
      tmp: IExpression;
      b: pchar;

    function NextR(k: integer): integer;
    var j:integer;
    begin
      Result:= -1;
      for j:= k+1 to R do
        if Tokens[j].Kind<>tokVoid then begin
          Result:= j;
          exit;
        end;
    end;
    function NextL(k: integer): integer;
    var j:integer;
    begin
      Result:= -1;
      for j:= k-1 downto L do
        if Tokens[j].Kind<>tokVoid then begin
          Result:= j;
          exit;
        end;
    end;

    procedure ProcessBraces(const Open, Close: TTokenKind; const Str: string);
    var i:integer;
    begin
      repeat
        A:= -1;
        for i:= L to R do begin
          if Tokens[i].Kind=Open then
            A:= i
          else if Tokens[i].Kind=Close then begin
            if A>=0 then begin
              if A=I-1 then begin
                Tokens[A].Kind:= tokEmpty;
              end else begin
                Fold(A+1,I-1);
                Tokens[A].Kind:= tokVoid;
              end;
              Tokens[I].Kind:= tokVoid;
            end else
              raise ESyntaxError.CreateFmt('Position %d: Closing %s never opened',[Tokens[i].Pos, Str]);
            A:= -2;
            break;
          end;
        end;
        if A>=0 then
          raise ESyntaxError.CreateFmt('%s opened at position %d is never closed',[Str, Tokens[A].Pos]);
      until A=-1;
    end;
  begin
    //given stupid values?
    if R<L then
      exit;

    //collapse braces first
    ProcessBraces(tokBraceOpen, tokBraceClose, '()');
    //then subcontexts
    ProcessBraces(tokContextOpen, tokContextClose, '[]');

    // simple expressions
    for i:= L to R do
      case Tokens[i].Kind of
        tokNumber: begin
          Tokens[i].Kind:= tokExpression;
          Tokens[i].Expr:= TE_Constant.Create(StrToFloat(Tokens[i].Value, NeutralFormatSettings));
        end;
        tokString: begin
          Tokens[i].Kind:= tokExpression;
          b:= PChar(Tokens[i].Value);
          Tokens[i].Expr:= TE_Constant.Create(AnsiExtractQuotedStr(b,CharQuote));
        end;
        tokExprRef: begin
          Tokens[i].Kind:= tokExpression;
          Tokens[i].Expr:= TE_ExprRef.Create(Tokens[i].Value);
        end;
        tokExprContext: begin
          Tokens[i].Kind:= tokExpression;
          tmp:= TE_Subcontext.Create(Tokens[i].Value);
          A:= NextR(i);
          if Tokens[A].Kind=tokEmpty then
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
          if Tokens[A].Kind=tokEmpty then
            TE_FunctionCall(tmp.GetObject).Arguments:= nil
          else
            TE_FunctionCall(tmp.GetObject).Arguments:= Tokens[A].Expr;
          Tokens[A].Expr:= nil;
          Tokens[a].Kind:= tokVoid;
          Tokens[i].Expr:= tmp;
        end;
      end;

    //finally, operators
    // Note: this wastes some extra iterations per Prio-Class, just ignore that, okay?
    for A:= 0 to high(Expressions) do begin
      for i:= L to R do
        if (Tokens[i].Kind=tokOperator) and
           (Expressions[Tokens[i].OpIdx].P=Expressions[A].P) then begin
          eid:= Tokens[i].OpIdx;
          rr:= NextR(i);
          ll:= NextL(i);

          if rr<0 then
            raise ESyntaxError.CreateFmt('Position %d: Operator has no RHS',[Tokens[i].Pos]);

          if Tokens[rr].Kind<>tokExpression then
            raise ESyntaxError.CreateFmt('Position %d: expected expression, found %s',[Tokens[rr].Pos, Tokens[rr].Value]);

          if (Expressions[eid].Cls = TE_Subtraction) and
             ((ll<0) or (Tokens[ll].Kind<>tokExpression)) then begin
            tmp:= TE_Negation.Create;
            tmp.RHS:= Tokens[rr].Expr;
          end else begin
            tmp:= Expressions[eid].Cls.Create;

            tmp.RHS:= Tokens[rr].Expr;
            if not Expressions[eid].Unary then begin
              if ll<0 then
                raise ESyntaxError.CreateFmt('Position %d: Operator has no LHS',[Tokens[i].Pos]);
              if Tokens[ll].Kind<>tokExpression then
                raise ESyntaxError.CreateFmt('Position %d: expected expression, found %s',[Tokens[ll].Pos, Tokens[ll].Value]);
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
    i:= NextR(L-1);
    if i<>L then begin
      Tokens[L]:= Tokens[i];
      Tokens[i].Kind:= tokVoid;
    end;
  end;

  function CheckResults: boolean;
  var i:integer;
  begin
    Result:= length(Tokens)>0;
    if not Result then
      exit;
    for i:= 1 to high(Tokens) do
      if Tokens[i].Kind<>tokVoid then
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

function TMathSystem.Eval(const Expr: String): IValue;
var ex: IExpression;
    s: string;
begin
  ex:= Parse(Expr);
  try
    if Assigned(ex) then begin
      s:= ex.StringForm;
      if TE_FunctionCall.CheckSysCalls(s) then begin
        Result:= ex.Evaluate(FContext);
        if not Assigned(Result) then
          Result:= TE_Constant.CreateUnassigned;
      end;
    end
    else
      Result:= TE_Constant.CreateNull;
  finally
    ex:= nil;
  end;
end;

procedure TMathSystem.Run(const Expr: String);
var re: IValue;
    rs: IStringConvertible;
begin
  try
    re:= Eval(Expr);
    case re.ValueType of
      vtUnassigned: ;
      vtNull: ;
    else
      begin
        if Supports(re, IStringConvertible, rs) then
          Output.Result(rs.OutputForm);
        Context.DefineValue('ans',re);
      end;
    end;
  except
    on e: EMathSysError do
      Output.Error(E.Message,[]);
  end;
end;

procedure TMathSystem.DropContext;
var cont: TContext;
begin
  if FContext.Parent<>FConstants then begin
    cont:= FContext.Parent;
    FContext.Free;
    FContext:= cont;
  end else
    raise EMathSysError.Create('Cannot drop this context.');
end;

procedure TMathSystem.NewContext(const Name: string);
var cont: TContext;
begin
  cont:= TContext.Create(Self, FContext);
  cont.ContextName:= Name;
  FContext:= cont;
end;

{ TOutput }

constructor TOutput.Create;
begin
  inherited;
  FRender:= nil;
end;

procedure TOutput.LineOut(const Line: string; Indent: integer; Color: TColor; Style: TFontStyles);
var p:integer;
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
  p:= Pos(NeutralFormatSettings.ThousandSeparator, FRender.Text);
  while p>0 do begin
    FRender.SelStart:= p-1;
    FRender.SelLength:= 1;
    FRender.SelAttributes.Name:= 'Arial';
    FRender.SelText:= ' ';
    p:= Pos(NeutralFormatSettings.ThousandSeparator, FRender.Text);
  end;
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
    if not FSilent and not SameText(Name,'ans') then
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

function TContext.Value(const Name: string): IValue;
var ex: IExpression;
begin
  ex:= Definition(Name);
  if Assigned(ex) then
    Result:= ex.Evaluate(Self)
  else begin
    Result:= TE_Constant.CreateUnassigned;
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


procedure TContext.DefineValue(const Name: string; Value: IValue);
var expr: IExpression;
begin
  case Value.ValueType of
    vtUnassigned: ;
  else
    expr:= TE_Constant.Create(Value);
  end;
  if Assigned(expr) then
    Define(Name,expr);
end;

function TContext.Defines(const Name: string): boolean;
begin
  Result:= FExpressions.IndexOf(Name)>-1;
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

function TExpression.StringForm: String;
begin
  if LHS = nil then
    Result:= format('%s(%s)',[Copy(ClassName,4,10000),RHS.StringForm])
  else
    Result:= format('%s(%s,%s)',[Copy(ClassName,4,10000),LHS.StringForm,RHS.StringForm]);
end;

{ TE_Constant }

constructor TE_Constant.Create(Val: IValue);
begin
  inherited Create;
  FValueType:= Val.ValueType;
  case FValueType of
    vtNumber: FNumber:= Val.GetNumber;
    vtString: FString:= Val.GetString;
  end;
end;

constructor TE_Constant.Create(Val: Number);
begin
  inherited Create;
  FValueType:= vtNumber;
  FNumber:= Val;
end;

constructor TE_Constant.Create(Val: String);
begin
  inherited Create;
  FValueType:= vtString;
  FString:= Val;
end;

constructor TE_Constant.CreateUnassigned;
begin
  inherited Create;
  FValueType:= vtUnassigned;
end;

constructor TE_Constant.CreateNull;
begin
  inherited Create;
  FValueType:= vtNull;
end;

function TE_Constant.Evaluate(Context: TContext): IValue;
begin
  QueryInterface(IValue, Result);
end;

function TE_Constant.StringForm: String;
begin
  case FValueType of
    vtUnassigned: Result:= '<?>';
    vtNull: Result:= 'null';
    vtNumber: Result:= NumberToStr(FNumber, NeutralFormatSettings, false);
    vtString: Result:= QuotedStr(GetString);
  end;
end;

function TE_Constant.GetNumber: Number;
begin
  {$WARNINGS OFF}
  case FValueType of
    vtNumber: Result:= FNumber;
    vtString: Result:= StrToFloat(FString, NeutralFormatSettings);
  else
    Result:= NAN;
  end;
  {$WARNINGS ON}
end;

function TE_Constant.GetString: string;
begin
  case FValueType of
    vtNumber: Result:= NumberToStr(FNumber, NeutralFormatSettings, false);
    vtString: Result:= FString;
  else
    Result:= '';
  end;
end;

procedure TE_Constant.SetNumber(const num: Number);
begin
  FNumber:= num;
  FValueType:= vtNumber;
end;

procedure TE_Constant.SetString(const str: String);
begin
  FString:= str;
  FValueType:= vtString;
end;

procedure TE_Constant.SetNull;
begin
  FValueType:= vtNull;
end;

procedure TE_Constant.SetUnassigned;
begin
  FValueType:= vtUnassigned;
end;

function TE_Constant.ValueType: TValueType;
begin
  Result:= FValueType;
end;

function TE_Constant.OutputForm: string;
var i:integer;
begin
  case FValueType of
    vtNumber: Result:= NumberToStr(FNumber, NeutralFormatSettings, true);
    vtString: Result:= QuotedStr(GetString);
  end;
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

function TE_FunctionCall.Evaluate(Context: TContext): IValue;
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
  meth.Data:= Self;
  meth.Code:= MethodAddress(FName+'_'+IntToStr(Length(ls)));
  if not Assigned(meth.Code) then begin
    meth.Code:= MethodAddress(FName+'_N');
    if not Assigned(meth.Code) then
      raise EMathSysError.CreateFmt('Function %s has no version with %d parameters',[FName, Length(ls)]);
  end;
  Result:= TUDFHeader(meth)(Context, ls);
end;

function TE_FunctionCall.StringForm: String;
begin
  if Assigned(Arguments) then
    Result:= FName+'('+Arguments.StringForm+')'
  else
    Result:= FName+'()';
end;

class function TE_FunctionCall.CheckSysCalls(StringForm: string): Boolean;
  // perform check on string form, so we could only encounter (new() or ,new()
  // in non-toplevel calls. if we don't find that, it might be top level (pos 0)
  // but that would evaluate to true anyway -> don't bother checking
  function Chk(f: string): boolean;
  begin
    Result:= (pos(',' + f+'(',StringForm) = 0) and
             (pos('(' + f+'(',StringForm) = 0);
  end;
begin
  StringForm:= LowerCase(StringForm);
  Result:= Chk('new') and Chk('drop');
  if not Result then
    raise EMathSysError.Create('System functions can only be used stand-alone');
end;

function TE_FunctionCall.Abs_1(Context: TContext; args: TExprList): IValue;
begin
  Result:= TE_Constant.Create(System.Abs(args[0].Evaluate(Context).GetNumber));
end;

function TE_FunctionCall.Exp_1(Context: TContext; args: TExprList): IValue;
begin
  Result:= TE_Constant.Create(System.Exp(args[0].Evaluate(Context).GetNumber));
end;

function TE_FunctionCall.Ln_1(Context: TContext; args: TExprList): IValue;
begin
  Result:= TE_Constant.Create(System.Ln(args[0].Evaluate(Context).GetNumber));
end;

function TE_FunctionCall.Lg_1(Context: TContext; args: TExprList): IValue;
begin
  Result:= TE_Constant.Create(Math.Log10(args[0].Evaluate(Context).GetNumber));
end;

function TE_FunctionCall.Ld_1(Context: TContext; args: TExprList): IValue;
begin
  Result:= TE_Constant.Create(Math.Log2(args[0].Evaluate(Context).GetNumber));
end;

function TE_FunctionCall.Loga_2(Context: TContext; args: TExprList): IValue;
begin
  Result:= TE_Constant.Create(Math.LogN(args[0].Evaluate(Context).GetNumber,args[1].Evaluate(Context).GetNumber));
end;

function TE_FunctionCall.Fac_1(Context: TContext; args: TExprList): IValue;
var accu,k,desiredFact: Number;
begin
  accu:= 1;
  desiredFact:= args[0].Evaluate(Context).GetNumber;
  k:= 2;
  while k<=desiredFact do begin
    accu := accu * k;
    k:= k+1;
  end;
  Result:= TE_Constant.Create(accu);
end;

function TE_FunctionCall.Deg2Rad_1(Context: TContext; args: TExprList): IValue;
begin
  Result:= TE_Constant.Create(DegToRad(args[0].Evaluate(Context).GetNumber));
end;

function TE_FunctionCall.Rad2Deg_1(Context: TContext; args: TExprList): IValue;
begin
  Result:= TE_Constant.Create(RadToDeg(args[0].Evaluate(Context).GetNumber));
end;

function TE_FunctionCall.Sin_1(Context: TContext; args: TExprList): IValue;
begin
  Result:= TE_Constant.Create(Sin(args[0].Evaluate(Context).GetNumber));
end;

function TE_FunctionCall.Cos_1(Context: TContext; args: TExprList): IValue;
begin
  Result:= TE_Constant.Create(Cos(args[0].Evaluate(Context).GetNumber));
end;

function TE_FunctionCall.Tan_1(Context: TContext; args: TExprList): IValue;
begin
  Result:= TE_Constant.Create(Tan(args[0].Evaluate(Context).GetNumber));
end;

function TE_FunctionCall.ArcSin_1(Context: TContext; args: TExprList): IValue;
begin
  Result:= TE_Constant.Create(ArcSin(args[0].Evaluate(Context).GetNumber));
end;

function TE_FunctionCall.ArcCos_1(Context: TContext; args: TExprList): IValue;
begin
  Result:= TE_Constant.Create(ArcCos(args[0].Evaluate(Context).GetNumber));
end;

function TE_FunctionCall.ArcTan_1(Context: TContext; args: TExprList): IValue;
begin
  Result:= TE_Constant.Create(ArcTan(args[0].Evaluate(Context).GetNumber));
end;

function TE_FunctionCall.ArcTan_2(Context: TContext; args: TExprList): IValue;
begin
  Result:= TE_Constant.Create(ArcTan2(args[0].Evaluate(Context).GetNumber,args[1].Evaluate(Context).GetNumber));
end;

function TE_FunctionCall.Sinh_1(Context: TContext; args: TExprList): IValue;
begin
  Result:= TE_Constant.Create(Sinh(args[0].Evaluate(Context).GetNumber));
end;

function TE_FunctionCall.Cosh_1(Context: TContext; args: TExprList): IValue;
begin
  Result:= TE_Constant.Create(Cosh(args[0].Evaluate(Context).GetNumber));
end;

function TE_FunctionCall.Tanh_1(Context: TContext; args: TExprList): IValue;
begin
  Result:= TE_Constant.Create(Tanh(args[0].Evaluate(Context).GetNumber));
end;

function TE_FunctionCall.ArSinh_1(Context: TContext; args: TExprList): IValue;
begin
  Result:= TE_Constant.Create(ArcSinh(args[0].Evaluate(Context).GetNumber));
end;

function TE_FunctionCall.ArCosh_1(Context: TContext; args: TExprList): IValue;
begin
  Result:= TE_Constant.Create(ArcCosh(args[0].Evaluate(Context).GetNumber));
end;

function TE_FunctionCall.ArTanh_1(Context: TContext; args: TExprList): IValue;
begin
  Result:= TE_Constant.Create(ArcTanh(args[0].Evaluate(Context).GetNumber));
end;

const
  BaseString = '0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZ';

function TE_FunctionCall.FromBase_2(Context: TContext; args: TExprList): IValue;
var base, i, j: integer;
    v: int64;
    n: string;
begin
  base:= Trunc(args[1].Evaluate(Context).GetNumber);
  n:= args[0].Evaluate(Context).GetString;
  if base > Length(BaseString) then
    raise EMathSysError.CreateFmt('Base %d exceeds maximum allowed value of %d',[base, Length(BaseString)]);
  v:= 0;
  for i:= 1 to Length(n) do begin
    j:= Pos(n[i], BaseString)-1;
    if j>=base then
      raise EMathSysError.CreateFmt('Invalid numeral in base %d: ''%s''',[base, n[i]]);
    v:= v * base + j;
  end;
  Result:= TE_Constant.Create(v);
end;

function TE_FunctionCall.ToBase_2(Context: TContext; args: TExprList): IValue;
var base, i: integer;
    n: string;
    v: int64;
begin
  base:= Trunc(args[1].Evaluate(Context).GetNumber);
  v:= Trunc(args[0].Evaluate(Context).GetNumber);

  if v = 0 then begin
    Result:= TE_Constant.Create('0');
    exit;
  end;

  if base > Length(BaseString) then
    raise EMathSysError.CreateFmt('Base %d exceeds maximum allowed value of %d',[base, Length(BaseString)]);

  n:= '';

  while v <> 0 do begin
    i:= v mod base;
    v:= v div base;
    n:= BaseString[i + 1] + n;
  end;
  Result:= TE_Constant.Create(n);
end;

function TE_FunctionCall.Undef_1(Context: TContext;
  args: TExprList): IValue;
begin
  Context.Undefine(args[0].Evaluate(Context).GetString);
end;

function TE_FunctionCall.New_0(Context: TContext; args: TExprList): IValue;
begin
  Context.System.NewContext('');
end;

function TE_FunctionCall.New_1(Context: TContext;
  args: TExprList): IValue;
begin
  Context.System.NewContext(args[0].Evaluate(Context).GetString);
end;

function TE_FunctionCall.Drop_0(Context: TContext;
  args: TExprList): IValue;
begin
  Context.System.DropContext();
end;

function FindConstant(Name: string; out Definition: TConstantDef): boolean;
type
  TConstDefRef = array[0..0] of TConstantDef;
  PConstDef = ^TConstDefRef;
  function FindIn(List: PConstDef; Lo, Hi: integer): Boolean;
  var i:integer;
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
  val.SetNumber(def.Value);
  Result:= Format('%s = %s',[def.LongName, val.GetString]);
  if def.Uni>'' then
    Result:= Format('%s [%s]',[Result, def.Uni]);
  if def.Comment>'' then
    Result:= Format('%s (%s)',[Result, def.Comment]);
end;

function TE_FunctionCall.const_1(Context: TContext; args: TExprList): IValue;
var nm: string;
    res: TConstantDef;
begin
  nm:= args[0].Evaluate(Context).GetString;
  if FindConstant(nm, res) then
    Result:= TE_Constant.Create(res.Value)
  else
    raise EMathSysError.CreateFmt('Unknown Constant: %s',[nm]);
end;

function TE_FunctionCall.constinfo_0(Context: TContext;
  args: TExprList): IValue;
type
  TConstDefRef = array[0..0] of TConstantDef;
  PConstDef = ^TConstDefRef;
var
  Count: integer;
  procedure ListIn(List: PConstDef; Lo, Hi: integer);
  var i:integer;
  begin
    for i:= lo to hi do begin
      Context.System.Output.Hint(FormatConstInfo(List^[i]),[]);
      Inc(Count);
    end;
  end;
begin
  Count:= 0;
  ListIn(@MathematicalConstants, Low(MathematicalConstants), high(MathematicalConstants));
  ListIn(@PhysicalConstants, Low(PhysicalConstants), high(PhysicalConstants));
  Result:= TE_Constant.Create(Count);
end;

function TE_FunctionCall.constinfo_1(Context: TContext;
  args: TExprList): IValue;
var nm: string;
    res: TConstantDef;
begin
  nm:= args[0].Evaluate(Context).GetString;
  if FindConstant(nm, res) then begin
    Result:= TE_Constant.Create(FormatConstInfo(res));
  end else
    raise EMathSysError.CreateFmt('Unknown Constant: %s',[nm]);
end;

(*function TE_FunctionCall.L_N(Context: TContext; args: TExprList): IValue;
var i:integer;
begin
  Result.Length:= length(args);
  for i:= 0 to Result.Length-1 do
    Result.ListItem[i]:= args[i].Evaluate(Context);
end;

function TE_FunctionCall.Range_3(Context: TContext; args: TExprList): IValue;
var a,max,st: Number;
    i:integer;
    v: IValue;
begin
  a:= args[0].Evaluate(Context).GetNumber;
  max:= args[1].Evaluate(Context).GetNumber;
  st:= args[2].Evaluate(Context).GetNumber;
  Result.Length:= 0;
  repeat
    i:= Result.Length;
    Result.Length:= i+1;
    v.SetNumber(a);
    Result.SetItem(i, v);
    a:= a + st;
  until a > max;
end;

function TE_FunctionCall.Merge_2(Context: TContext; args: TExprList): IValue;
var i:integer;
    a,b: IValue;
begin
  a:= args[0].Evaluate(Context);
  b:= args[1].Evaluate(Context);
  if (a.ValueType<>vtList) or (b.ValueType<>vtList) then
    raise EMathSysError.Create('Merge requires 2 lists.');
  Result.Length:= a.Length + b.Length;
  for i:= 0 to a.Length-1 do
    Result.ListItem[i]:= a.ListItem[i];
  for i:= 0 to b.Length-1 do
    Result.ListItem[a.Length+i]:= b.ListItem[i];
end;

function TE_FunctionCall.Each_3(Context: TContext; args: TExprList): IValue;
var list:IValue;
    v: IExpression; //TE_ExprRef
    ex: IExpression;
    i:integer;
   ctx: TContext;
begin
  list:= args[0].Evaluate(Context);
  if list.ValueType<>vtList then
    raise EMathSysError.Create('Function Each requires a list to work on');
  v:= args[1];
  if v.GetClassType<>TE_ExprRef then
    raise EMathSysError.Create('Function Each requires a variable reference');
  ex:= args[2];

  ctx:= TContext.Create(Context.System, Context);
  try
    ctx.Silent:= true;
    Result.Length:= list.Length;
    for i:= 0 to list.length-1 do begin
      ctx.DefineValue(TE_ExprRef(v.GetObject).FName, list.ListItem[i]);
      Result.ListItem[i]:= ex.Evaluate(ctx);
    end;
  finally
    FreeAndNil(ctx);
  end;
end;

function TE_FunctionCall.Aggregate_5(Context: TContext; args: TExprList): IValue;
var list:IValue;
    agg: IExpression; //TE_ExprRef
    init: IExpression;
    v: IExpression; //TE_ExprRef
    ex: IExpression;
    i:integer;
   ctx: TContext;
begin
  list:= args[0].Evaluate(Context);
  if list.ValueType<>vtList then
    raise EMathSysError.Create('Function Aggregate requires a list to work on');
  agg:= args[1];
  if agg.GetClassType<>TE_ExprRef then
    raise EMathSysError.Create('Function Aggregate requires a variable reference as aggregate');
  init:= args[2];
  v:= args[3];
  if v.GetClassType<>TE_ExprRef then
    raise EMathSysError.Create('Function Aggregate requires a variable reference');
  ex:= args[4];

  ctx:= TContext.Create(Context.System, Context);
  try
    ctx.Silent:= true;
    ctx.DefineValue(TE_ExprRef(agg.GetObject).FName, init.Evaluate(ctx));

    Result.Length:= list.Length;
    for i:= 0 to list.length-1 do begin
      ctx.DefineValue(TE_ExprRef(v.GetObject).FName, list.ListItem[i]);
      ctx.DefineValue(TE_ExprRef(agg.GetObject).FName, ex.Evaluate(ctx));
    end;
    Result:= ctx.Value(TE_ExprRef(agg.GetObject).FName);
  finally
    FreeAndNil(ctx);
  end;
end;    *)

function TE_FunctionCall.Clear_0(Context: TContext;
  args: TExprList): IValue;
begin
  Context.System.Output.Clear;
  Result:= TE_Constant.CreateUnassigned;
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

function TE_ArgList.Evaluate(Context: TContext): IValue;
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

function TE_Subcontext.Evaluate(Context: TContext): IValue;
var ctx: TContext;
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

function TE_Subcontext.StringForm: String;
begin
  if Assigned(Arguments) then
    Result:= FName+'['+Arguments.StringForm+']'
  else
    Result:= FName+'[]';
end;

{ TE_Describe }

function TE_Describe.Evaluate(Context: TContext): IValue;
var name: string;
    e: IExpression;
begin
  if RHS.GetObject is TE_ExprRef then begin
    name:= (RHS.GetObject as TE_ExprRef).FName;
    e:= Context.Definition(name);
  end else
    e:= RHS;
  if Assigned(e) then
    Result:= TE_Constant.Create(e.StringForm)
  else
    Result:= TE_Constant.Create('<Unknown>');
end;

{ TE_AssignmentDynamic }

function TE_AssignmentDynamic.Evaluate(Context: TContext): IValue;
var name: string;
begin
  if not (LHS.GetObject is TE_ExprRef) then
    raise ESyntaxError.Create('LHS of assignment needs to be a simple expression reference');
  name:= (LHS.GetObject as TE_ExprRef).FName;
  Context.Define(name, RHS);
end;

{ TE_AssignmentStatic }

function TE_AssignmentStatic.Evaluate(Context: TContext): IValue;
var name: string;
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
  Result:= TE_Constant.Create(Power(LHS.Evaluate(Context).GetNumber, RHS.Evaluate(Context).GetNumber));
end;

{ TE_Multiplication }

function TE_Multiplication.Evaluate(Context: TContext): IValue;
begin
  Result:= TE_Constant.Create(LHS.Evaluate(Context).GetNumber * RHS.Evaluate(Context).GetNumber);
end;

{ TE_Division }

function TE_Division.Evaluate(Context: TContext): IValue;
begin
  Result:= TE_Constant.Create(LHS.Evaluate(Context).GetNumber / RHS.Evaluate(Context).GetNumber);
end;

{ TE_Modulus }

function TE_Modulus.Evaluate(Context: TContext): IValue;
begin
  Result:= TE_Constant.Create(trunc(LHS.Evaluate(Context).GetNumber) mod trunc(RHS.Evaluate(Context).GetNumber));
end;

{ TE_Addition }

function TE_Addition.Evaluate(Context: TContext): IValue;
begin
  Result:= TE_Constant.Create(LHS.Evaluate(Context).GetNumber + RHS.Evaluate(Context).GetNumber);
end;

{ TE_Subtraction }

function TE_Subtraction.Evaluate(Context: TContext): IValue;
begin
  Result:= TE_Constant.Create(LHS.Evaluate(Context).GetNumber - RHS.Evaluate(Context).GetNumber);
end;

{ TE_Negation }

function TE_Negation.Evaluate(Context: TContext): IValue;
begin
  Result:= TE_Constant.Create(0 - RHS.Evaluate(Context).GetNumber);
end;

initialization
  GetLocaleFormatSettings(GetThreadLocale, NeutralFormatSettings);
  with NeutralFormatSettings do
  begin
    ThousandSeparator := #160;
    DecimalSeparator := '.';
    DateSeparator := '-';
    ShortDateFormat := 'yy-mm-dd';
    LongDateFormat := 'mmmm d, yyyy';
    TimeSeparator := ':';
  end;
end.
