unit uMathValues;

interface

uses
  SysUtils, Math, uMath, uMathIntf;

type
  IExpressionAtom = interface['{EC6BC9B9-A248-4765-8F62-2BF378A869AB}']
  end;

  IValueUnassigned = interface(IExpressionAtom)['{3352D0C9-D960-43D5-A09A-5D5B4C97F663}']
  end;

  IValueNull = interface(IExpressionAtom)['{915EE73F-F88E-4806-B04D-66C5B0836D85}']
  end;

  IValueNumber = interface(IExpressionAtom)['{915EE73F-F88E-4806-B04D-66C5B0836D85}']
    function Value: Number;
  end;

  IValueString = interface(IExpressionAtom)['{6B954DBB-0C95-4CD1-A533-4E28204B71DB}']
    function Value: String;
  end;

  IValueList = interface(IExpressionAtom)['{C63F0621-9E52-408C-867B-501691E859EB}']
  end;

  TE_Atom = class(TExpression, IExpressionAtom)
  public
    function Evaluate(const Context: IContext): IExpression; override;
  end;

  TValueUnassigned = class(TE_Atom, IValueUnassigned)
  end;

  TValueNull = class(TE_Atom, IValueNull)
  end;

  TValueNumber = class(TE_Atom, IValueNumber, IStringConvertible)
  private
    FValue: Number;
  public
    constructor Create(const aVal: Number);
    // IValueNumber
    function Value: Number;
    // IStringConvertible
    function AsString(const Format: TStringFormat): String;
  end;

  TValueString = class(TE_Atom, IValueString, IStringConvertible)
  private
    FValue: String;
  public
    constructor Create(const aVal: String);
    // IValueString
    function Value: String;   
    // IStringConvertible
    function AsString(const Format: TStringFormat): String;
  end;

  TValueList = class(TE_Atom, IValueList)
  private
  public
    constructor Create;
    property Items: TExprList read Arguments;
  end;

implementation

{ TE_Atom }

function TE_Atom.Evaluate(const Context: IContext): IExpression;
begin
  Result:= Self;
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
    $FFFF: ;
  else
     Result:= NumberToStr(FValue,NeutralFormatSettings, false);
  end;
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

end.
