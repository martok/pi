unit uFunctionsTest;

interface

uses SysUtils, Classes, Graphics, uMath, Math;

type
  TPackageTest = class(TFunctionPackage)
  published
    function TimesTwo_1(Context: TContext; Args: TExprList): IValue;
  end;

implementation

{ TPackageTest }

function TPackageTest.TimesTwo_1(Context: TContext; Args: TExprList): IValue;
var
  ex,r:IExpression;
begin
  ex:= args[0];
  r:= TE_Multiplication.Create;
  r.LHS:= TE_Constant.Create(TValue.Create(2));
  r.RHS:= ex;
  Result:= TValueExpression.Create(r);
end;

initialization
  TFunctionPackage.RegisterPackage(TPackageTest);
end.
