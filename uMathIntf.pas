{-----------------------------------------------------------------------------
 The TAU Math Kernel

 Interface declaration for use with other modules.
-----------------------------------------------------------------------------}
unit uMathIntf;

interface

uses
  SysUtils, Classes;

type
  Number = type Extended;
  MTInteger = type Int64;
  MTFloat   = Number;

  EMathSysError = class(Exception);
  ESyntaxError = class(EMathSysError);
  EMathTypeError = class(EMathSysError);

  IExpression = interface;
  IPackagedFunction = interface;
  IContext = interface;

  IOutputProvider = interface['{CDB4BA33-F6B1-4172-9AF9-EA7A8FAF4C9F}']
    procedure Input(const Line: string);
    procedure Hint(const Line: string; Params: array of const);
    procedure Error(const Line: string; Params: array of const);
    procedure Result(const Line: string);
    procedure Clear;
  end;

  IMathSystem = interface ['{3F3F358B-3066-43C6-8021-872145A5927C}']
    function Parse(const Expr: String): IExpression;
    function HasPackage(const PackageClassName: string): Boolean;  
    function HasFunction(const FuncName: string; const ArgCount: integer): IPackagedFunction;
  end;


  IContext = interface['{AA415A3A-AA57-4361-8D23-95F9959CBA3B}']
    function NativeObject: TObject;

    procedure Define(const Name: string; Expression: IExpression);
    procedure Undefine(const Name: string);
    procedure Clear;
    function Definition(const Name: string): IExpression;
    function Defines(const Name: string): boolean;
    function Output: IOutputProvider;
    function RawOutput: IOutputProvider;  
    function GetSystem: IMathSystem;
    procedure SetSilent(const Silent: boolean);
    function GetSilent: boolean;
    property Silent: boolean read GetSilent write SetSilent;
    function Combine(const UseDefine: boolean): IExpression;
  end;

  TExprList = array of IExpression;
  IExpression = interface['{56A7A015-3E85-483F-9EC5-4C3C0E232CA5}']
    function NativeObject: TObject;
    function Represents(const IID: TGUID; out Intf): boolean; overload;
    function Represents(const IID: TGUID): boolean; overload;
    function IsClass(const Cls: TClass): boolean;
    procedure SetArgs(const aArgs: array of IExpression);
    function GetArgs: TExprList;
    function ArgCount: integer;
    function GetArgument(Index: Integer): IExpression;                          
    property Arg[Index: integer]: IExpression read GetArgument;
    function Evaluate(const Context: IContext): IExpression;
    function Clone(Deep: boolean): IExpression;
  end;

  TStringFormat = type Word;
  IStringConvertible = interface['{CA261D7B-A5D8-4197-B115-ECDE88A664FE}']
    function AsString(const Format: TStringFormat): string;
  end;

  ISymbolReference = interface(IExpression)['{1B46951B-6531-4499-B251-6C59A3E21B0A}']
    function GetName: string;
    property Name: string read GetName;
  end;

  IFunctionCall = interface(IExpression)['{F717E01E-6329-4326-91B2-CC7F742D9493}']  
    function GetName: string;
    property Name: string read GetName;
  end;

  IPackagedFunction = interface['{5E6324DF-4F1B-4711-A6F4-CF1D62315911}']
    function GetName: string;
    function IsDynamic: boolean;
    function Call(Context: IContext; Args: TExprList): IExpression;
  end;

  TAtomCompareResult = (crIncompatible, crSame, crEquivalent, crGreater, crSmaller, crDifferent);
  IExpressionAtom = interface(IExpression)['{EC6BC9B9-A248-4765-8F62-2BF378A869AB}']
    // This is ... to/than B
    function CompareTo(const B: IExpressionAtom): TAtomCompareResult;
  end;

  IValueUnassigned = interface(IExpressionAtom)['{3352D0C9-D960-43D5-A09A-5D5B4C97F663}']
  end;

  IValueNull = interface(IExpressionAtom)['{915EE73F-F88E-4806-B04D-66C5B0836D85}']
  end;

  TMathBaseUnit = (siM, siKG, siS, siK, siMOL, siA, siCD, siRAD, siBIT);
  TMathUnits = array[TMathBaseUnit] of Shortint;
  IDimensions = interface['{0048A6D6-0B10-4C87-9F84-630F43CA6FC4}']
    // Dimension handling for any kind of Number                
    function IsScalar: boolean;
    function Units: TMathUnits;
    function UnitCompatible(const Units: TMathUnits): boolean;
  end;

  TValueNumeralType = (
    tiUnknown = 0,
    tiInt = 1,
    tiFloat = 2
    //tiRational = 3
  );
  IValueNumber = interface(IExpressionAtom)['{FFE94239-E1C3-4F53-9D94-FAC80C4BD085}']
    // Description of this value for specialised handling
    function BaseType: TValueNumeralType;
    function IsScalar: boolean;
    // Numbers, raises EMathTypeError (too large, float->int)
    function ValueFloat: MTFloat;
    function ValueInt: MTInteger;   

    // Promote Self and Other to a compatible type
    //   ThisC, OtherC: can be just the same objects if no change neccessary
    //   return false if not possible
    function Promote(const Other: IValueNumber; out ThisC, OtherC: IValueNumber): boolean; overload;
    // Promote Self and Other to a compatible type
    //   Result, OtherC: can be just the same objects if no change neccessary
    //   raise EMathTypeError if not possible
    function Promote(const Other: IValueNumber; out OtherC: IValueNumber): IValueNumber; overload;
  end;

  IValueString = interface(IExpressionAtom)['{6B954DBB-0C95-4CD1-A533-4E28204B71DB}']
    function Value: String;
  end;

  IValueList = interface(IExpressionAtom)['{C63F0621-9E52-408C-867B-501691E859EB}']
    function GetLength: integer;
    procedure SetLength(const NewLength: integer);
    property Length: integer read GetLength write SetLength;
    procedure SetItem(Index: integer; val: IExpression);
    function GetItem(Index: integer): IExpression;
    property Item[Index: integer]: IExpression read GetItem write SetItem;
  end;

  IOperationAddition = interface['{4BBFAF74-4630-4B05-8732-888C1D9C5A24}']
    // Result = This + B
    function OpAdd(const B: IExpression): IExpression;
    // Result = This - B
    function OpSubtract(const B: IExpression): IExpression;
  end;

  IOperationMultiplication = interface['{F1559788-3942-4195-BB99-90CF6E6E901B}']
    // Result = This * B
    function OpMultiply(const B: IExpression): IExpression;
    // Result = This / B
    function OpDivide(const B: IExpression): IExpression;
    // Result = -B
    function OpNegate: IExpression;
  end;

  IOperationPower = interface['{B59CF38E-1800-4BF6-8FCB-20B687FEB17E}']
    // Result = This ^ B
    function OpPower(const B: IExpression): IExpression;
    // Result = This ^ (1 / B)
    function OpRoot(const B: IExpression): IExpression;
  end;
  


const
  STR_FORMAT_DEFAULT = TStringFormat(0);

const
  MATH_UNIT_NAME : array[TMathBaseUnit] of string = ('m', 'kg', 's', 'K', 'mol', 'A', 'cd', 'rad', 'bit');

type
  TIntfNoRefCount = class(TInterfacedObject, IUnknown)
  protected
    // IUnknown
    function QueryInterface(const IID: TGUID; out Obj): HResult; stdcall;
    function _AddRef: Integer; stdcall;
    function _Release: Integer; stdcall;
  end;


implementation


{ TIntfNoRefCount }

function TIntfNoRefCount._AddRef: Integer;
begin
  Result:= -1;
end;

function TIntfNoRefCount._Release: Integer;
begin
  Result:= -1;
end;

function TIntfNoRefCount.QueryInterface(const IID: TGUID; out Obj): HResult;
begin
  if GetInterface(IID, Obj) then
    Result := 0
  else
    Result := E_NOINTERFACE;
end;



end.
