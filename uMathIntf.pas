unit uMathIntf;

interface

uses
  SysUtils, Classes;

type
  Number = type Extended;

  EMathSysError = class(Exception);
  ESyntaxError = class(EMathSysError);
  EMathTypeError = class(EMathSysError);

  IExpression = interface;
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
  end;


  IContext = interface['{AA415A3A-AA57-4361-8D23-95F9959CBA3B}']  
    function NativeObject: TObject;
    procedure Define(const Name: string; Expression: IExpression);
    procedure Undefine(const Name: string);
    function Definition(const Name: string): IExpression;
    function Defines(const Name: string): boolean;
    function Output: IOutputProvider;
  end;

  IExpression = interface['{56A7A015-3E85-483F-9EC5-4C3C0E232CA5}']
    function NativeObject: TObject;
    function Represents(const IID: TGUID; out Intf): boolean;
    function IsClass(const Cls: TClass): boolean;
    procedure SetArgs(const aArgs: array of IExpression);
    function ArgCount: integer;
    function Argument(Index: Integer): IExpression;
    property Arg[Index: integer]: IExpression read Argument;
    function Evaluate(const Context: IContext): IExpression;
  end;

  TStringFormat = type Word;
  IStringConvertible = interface['{CA261D7B-A5D8-4197-B115-ECDE88A664FE}']
    function AsString(const Format: TStringFormat): string;
  end;

const
  STR_FORMAT_DEFAULT = TStringFormat(0);

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
