{-----------------------------------------------------------------------------
 Package: Symbolics
-----------------------------------------------------------------------------}
unit uFunctionsSymbolics;

interface

uses SysUtils, uMathIntf, uMath;

type
  TPackageSymbolics = class(TFunctionPackage)
  published
    function Subs_2(Context: IContext; args: TExprList): IExpression;
    function Clone_1(Context: IContext; args: TExprList): IExpression;  
    function Match_2_opt(Context: IContext; args: TExprList; Options: TDynamicArguments): IExpression;
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
    s: ISymbolReference;
    j: integer;
  begin
    if x.Represents(ISymbolReference, s) then begin
      r:= rc.Definition(s.Name);
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
  if not args[1].Evaluate(Context).Represents(IValueList, rules) then
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

function TPackageSymbolics.Match_2_opt(Context: IContext; args: TExprList; Options: TDynamicArguments): IExpression;
var
  expr,
  pattern: IExpression;
  vars: IValueList;
  assignments: TContext;

  function MatchExpressions(const ex, pat: IExpression): boolean; forward;

  // deep exact compare
  function CompareExpressions(const ex, pat: IExpression): boolean;
  var
    esy,psy: ISymbolReference;
    efc,pfc: IFunctionCall;    
    ea,pa: IExpressionAtom;
    i: integer;
  begin
    Result:= false;

    // symbol reference
    if ex.Represents(ISymbolReference, esy) and pat.Represents(ISymbolReference, psy) and
      (SameText(esy.Name, psy.Name)) then begin
      Result:= true;
    end else
    // function call
    if ex.Represents(IFunctionCall, efc) and pat.Represents(IFunctionCall, pfc) and
      (SameText(efc.Name, pfc.Name)) and (efc.ArgCount = pfc.ArgCount) then begin
      // check arguments
      for i:= 0 to efc.ArgCount - 1 do
        if not MatchExpressions(efc.Arg[i], pfc.Arg[i]) then
          exit; // return false

      Result:= true;
    end else
    // atoms
    if ex.Represents(IExpressionAtom, ea) and pat.Represents(IExpressionAtom, pa) then begin
      Result:= ea.CompareTo(pa) = crSame;
    end;
  end;

  // matching with assignments
  function MatchExpressions(const ex, pat: IExpression): boolean;
  var
    ad: IExpression;
    psy: ISymbolReference;
  begin
    // is the current pattern a placeholder var?
    if pat.Represents(ISymbolReference, psy) and (assignments.Defines(psy.Name)) then begin
      // yes. is it already assigned?
      ad:= assignments.Definition(psy.Name);
      if ad.Represents(IValueUnassigned) then begin
        // first encounter, store assignment
        assignments.Define(psy.name, ex);
        Result:= true;
      end else begin
        // already assigned, is it the same thing as before?
        Result:= CompareExpressions(ex, ad)
      end;
      exit;
    end;

    // not a placeholder, so it has to match exactly
    Result:= CompareExpressions(ex, pat);
  end;

var
  i: integer;
  vs: ISymbolReference;
begin
  expr:= args[0].Evaluate(Context);
  pattern:= args[1].Evaluate(Context);
  if not Options.GetDefault('vars', TValueList.CreateAs([])).Represents(IValueList, vars) then
    raise EMathSysError.Create('Match: vars need to be a list of expressions');

  assignments:= TContext.Create(TContext.SystemFrom(Context), nil);
  try
    assignments.Silent:= true;
    for i:= 0 to vars.Length-1 do begin
      if vars.Item[i].Represents(ISymbolReference, vs) then
        assignments.Define(vs.Name, TValueUnassigned.Create);
    end;

    if MatchExpressions(expr, pattern) then begin
      Result:= assignments.Combine(true);
    end else
      Result:= TValueNull.Create;
  finally
    FreeAndNil(assignments);
  end;
end;

end.
