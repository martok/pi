unit uFunctionsString;

interface

uses SysUtils, uMathIntf, uMath;

type
  TPackageString = class(TFunctionPackage)
  private
  published
    function Insert_3(Context: IContext; args: TExprList): IExpression;
    function Delete_2(Context: IContext; args: TExprList): IExpression;
    function Delete_3(Context: IContext; args: TExprList): IExpression;
    function Substr_3(Context: IContext; args: TExprList): IExpression; 
    function Split_2(Context: IContext; args: TExprList): IExpression;
    function Join_2(Context: IContext; args: TExprList): IExpression;
  end;

implementation

uses
  uMathValues, StrUtils;

{ TPackageString }

function TPackageString.Insert_3(Context: IContext; args: TExprList): IExpression;
var
  s: string;
  p: integer;
begin
  s:= EvaluateToString(Context, args[0]);
  p:= CastToInteger(args[2].Evaluate(Context));
  Insert(EvaluateToString(Context, args[1]), s, p);
  Result:= TValueString.Create(s);
end;

function TPackageString.Delete_2(Context: IContext; args: TExprList): IExpression;
var
  s: string;
  p: integer;
begin
  s:= EvaluateToString(Context, args[0]);
  p:= CastToInteger(args[1].Evaluate(Context));
  Delete(s, p, 1);
  Result:= TValueString.Create(s);
end;

function TPackageString.Delete_3(Context: IContext; args: TExprList): IExpression;
var
  s: string;
  p, q: integer;
begin
  s:= EvaluateToString(Context, args[0]);
  p:= CastToInteger(args[1].Evaluate(Context));
  q:= CastToInteger(args[2].Evaluate(Context));
  Delete(s, p, q);
  Result:= TValueString.Create(s);
end;

function TPackageString.Substr_3(Context: IContext; args: TExprList): IExpression;
var
  s: string;
  p, q: integer;
begin
  s:= EvaluateToString(Context, args[0]);
  p:= CastToInteger(args[1].Evaluate(Context));
  q:= CastToInteger(args[2].Evaluate(Context));
  Result:= TValueString.Create(Copy(S, p, q));
end;

function TPackageString.Split_2(Context: IContext; args: TExprList): IExpression;
var
  res: IValueList;
  s, d: string;
  p, q, k: integer;
begin                 
  s:= EvaluateToString(Context, args[0]);  
  d:= EvaluateToString(Context, args[1]);

  res:= TValueList.Create;
  if d = '' then begin
    res.Length:= Length(s);
    for p := 1 to Length(s) do
      res.Item[p-1]:= TValueString.Create(s[p]);
  end else begin
    k:= 0;
    p:= 1;
    q:= PosEx(d, s, p+1);
    while q > 0 do begin
      res.Length:= k + 1;
      res.Item[k]:= TValueString.Create(Copy(s, p, q-p));
      inc(k);
      p:= q + 1; 
      q:= PosEx(d, s, p+1);
    end;
    if p < Length(s) then begin
      res.Length:= k + 1;
      res.Item[k]:= TValueString.Create(Copy(s, p, Length(s)));
    end;
  end;

  Result:= res;
end;

function TPackageString.Join_2(Context: IContext; args: TExprList): IExpression;
var
  a: IValueList;
  s, d: string;
  i: integer;
begin
  if not args[0].Evaluate(Context).Represents(IValueList, a) then
    raise EMathSysError.Create('Join requires a lists.');
  d:= EvaluateToString(Context, args[1]);

  if a.Length = 0 then begin
    Result:= TValueString.Create('');
    Exit;
  end;

  s:= CastToString(a.Item[0]);
  for i := 1 to a.Length - 1 do
    s:= s + d + CastToString(a.Item[i]);

  Result:= TValueString.Create(s);  
end;

end.
