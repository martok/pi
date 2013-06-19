unit uFunctionsSymbolics;

interface

uses SysUtils, uMathIntf, uMath;

type
  TPackageSymbolics = class(TFunctionPackage)
  published
    function Subs_2(Context: IContext; args: TExprList): IExpression;
    function Clone_1(Context: IContext; args: TExprList): IExpression;
  end;

implementation

uses
  Math, uMathValues;

{ TPackageSymbolics }

function TPackageSymbolics.Subs_2(Context: IContext; args: TExprList): IExpression;
var
  orig: IExpression;
  rules: IValueList;
  rc: IContext;
  i: integer;

  procedure Replace(var x: IExpression);
  var
    r: IExpression;
    j: integer;
  begin
    if x.IsClass(TE_SymbolRef) then begin
      r:= rc.Definition(TE_SymbolRef(x.NativeObject).Name);
      if Assigned(r) then begin
        x:= r;
        exit;
      end;
    end;
    for j:= 0 to x.ArgCount-1 do begin
      r:= x.Arg[j];
      Replace(r);
      TExpression(x.NativeObject).Arg[j]:= r;
    end;
  end;

begin
  orig:= args[0].Evaluate(Context);
  if not args[1].Represents(IValueList, rules) then
    raise EMathSysError.Create('Subs requires a list of replacements.');

  rc:= TContext.Create(TContext.SystemFrom(Context), nil);
  for i:= 0 to rules.Length-1 do
    rules.Item[i].Evaluate(rc);

  Result:= orig.Clone(true);
  Replace(Result);
end;

function TPackageSymbolics.Clone_1(Context: IContext; args: TExprList): IExpression;
begin
  Result:= args[0].Clone(true);
end;



end.
