{-----------------------------------------------------------------------------
 Package: Symbolics
-----------------------------------------------------------------------------}
unit uFunctionsSymbolics;

interface

uses SysUtils, uMathIntf, uMath, Classes, uMathValues, Contnrs;

type
  ISymbolicPattern = interface(IExpressionAtom)['{826E9E16-602F-406D-B2CD-F19617BA3752}']
    function GetPattern: IExpression;
    function Match(const Expr: IExpression; const Assignments: IContext): boolean;
  end;
  ISymbolicRule = interface(ISymbolicPattern)['{5430D7E7-9121-4612-A64E-D5D5574D9B81}']
    function GetTarget: IExpression; 
   // function Transform(const Expr: IExpression; const Assignments: IContext; out Output: IExpression): boolean;
   // function TransformRecursive(const Expr: IExpression; const Assignments: IContext; out Output: IExpression): boolean;
  end;

  TFunctionProperties = class
    Associative,                      //  a.(b.c) == (a.b).c
    Commutative: boolean;             //  a.b == b.a
    NeutralElement: IExpressionAtom;  //  a.n == a
    constructor Create(const ass, comm: boolean; const Neutral: IExpressionAtom);
  end;

  TPackageSymbolics = class(TFunctionPackage)
  private
    fDefaultProperties: TFunctionProperties;
    fFunctionProperties: TStringList;
  protected
    procedure OnImport(const MS: TMathSystem); override;
  public
    procedure SetupAlgebra;
    constructor Create;
    destructor Destroy; override;
    function Properties(const Name: String): TFunctionProperties;
  published
    function Subs_2(Context: IContext; args: TExprList): IExpression;
    function Clone_1(Context: IContext; args: TExprList): IExpression;
    function Match_2(Context: IContext; args: TExprList; Options: TDynamicArguments): IExpression;

    function Pattern_2(Context: IContext; args: TExprList): IExpression;
    function Rule_3(Context: IContext; args: TExprList): IExpression;

    function Replace_2(Context: IContext; args: TExprList): IExpression;
    function ReplaceAll_2(Context: IContext; args: TExprList): IExpression;
  end;

  TSymbolicPattern = class(TE_Atom, ISymbolicPattern, IStringConvertible)
  private
    fVars: TStringList;
    fPattern: IExpression;
  public
    constructor Create(const Pattern: IExpression; const Vars: TStringList);
    destructor Destroy; override;
    // IExpression
    function Clone(Deep: Boolean): IExpression; override;
    // IStringConvertible
    function AsString(const Format: TStringFormat): String;
    // ISymbolicPattern
    function GetPattern: IExpression;
    function Match(const Expr: IExpression; const Assignments: IContext): boolean;
  end;

  TSymbolicRule = class(TSymbolicPattern, IStringConvertible, ISymbolicRule)
  private
    fTarget: IExpression;
  public
    constructor Create(const Pattern, Target: IExpression; const Vars: TStringList);
    destructor Destroy; override;
    // IExpression
    function Clone(Deep: Boolean): IExpression; override;
    // IStringConvertible
    function AsString(const Format: TStringFormat): String;
    // ISymbolicRule
    function GetTarget: IExpression;
  end;

function SubstituteContextVars(const Expr: IExpression; const Context: IContext): IExpression;
function ApplyRules(const Expr: IExpression; const Context: IContext; const rules: IValueList; const Recurse: Boolean): IExpression;

implementation

uses
  Math, uExpressionMatcher;

function SubstituteContextVars(const Expr: IExpression; const Context: IContext): IExpression;
var
  Package: TPackageSymbolics;

  procedure Replace(var x: IExpression);
  var
    r: IExpression;
    s: ISymbolReference;
    arnew: TExprList;
    xfc, rfc: IFunctionCall;
    j, i, k: integer;
  begin
    if x.Represents(ISymbolReference, s) then begin
      r:= Context.Definition(s.Name);
      if Assigned(r) then begin
        x:= r;
        exit;
      end;
    end;
    SetLength(arnew, 0);
    i:= 0;
    for j:= 0 to x.ArgCount-1 do begin
      r:= x.Arg[j];
      Replace(r);
      if x.arg[j].Represents(ISymbolReference) and r.Represents(IFunctionCall, rfc) and
        x.Represents(IFunctionCall, xfc) and SameText(xfc.Name, rfc.Name) and
        Package.Properties(xfc.Name).Associative then begin  

        SetLength(arnew, i+rfc.ArgCount);
        for k:= 0 to rfc.ArgCount-1 do begin
          arnew[i]:= rfc.Arg[k];
          inc(i);
        end;
      end else begin
        SetLength(arnew, i+1);
        arnew[i]:= r;
        inc(i);
      end;
    end;
    x.SetArgs(arnew);
  end;
begin
  Package:= TPackageSymbolics(TContext.SystemFrom(Context).GetPackageInstance(TPackageSymbolics));
  if not Assigned(Package) then
    raise EMathSysError.Create('Package Symbolics not present in current Kernel');

  Result:= Expr.Clone(true);
  Replace(Result);
end;

function ApplyRules(const Expr: IExpression; const Context: IContext; const rules: IValueList; const Recurse: Boolean): IExpression;
var
  rule: ISymbolicRule;
  i, j: integer;
  assignments: IContext;
  asso: TContext;
  v: string;
  nargs: array of IExpression;
begin
  for i:= 0 to rules.Length-1 do begin
    if rules.Item[i].Represents(ISymbolicRule, rule) then begin
      assignments:= TContext.Create(TContext.SystemFrom(Context), nil);
      try
        assignments.SetSilent(true);
        if rule.Match(expr, assignments) then begin

          if Recurse then begin
            asso:= TContext(assignments.NativeObject);
            // transform all assignments
            for j:= 0 to asso.Count-1 do begin
              v:= asso.Name[i];
              asso.Define(v,
                ApplyRules(
                  asso.Definition(v),
                  Context, rules, true)
              );
            end;
          end;  
          // apply (transformed) subparts
          Result:= SubstituteContextVars(rule.GetTarget, assignments);
          exit;
        end else begin
          if Recurse then begin
            SetLength(nargs, Expr.ArgCount);
            for j:= 0 to Expr.ArgCount-1 do
              nargs[j]:= ApplyRules(Expr.Arg[j], Context, rules, true);
            Expr.SetArgs(nargs);
          end;
        end;
      finally
        assignments:= nil;
      end;
    end;
  end;
  Result:= expr;
end;

{ TSymbolicPattern }

constructor TSymbolicPattern.Create(const Pattern: IExpression; const Vars: TStringList);
begin
  inherited Create;
  fVars:= TStringList.Create;
  fVars.Sorted:= true;
  fVars.Duplicates:= dupIgnore;
  fVars.AddStrings(Vars);
  fPattern:= Pattern;
end;

destructor TSymbolicPattern.Destroy;
begin
  FreeAndNil(fVars);
  fPattern:= nil;
  inherited Destroy;
end;

function TSymbolicPattern.Clone(Deep: Boolean): IExpression;
begin
  Result:= TSymbolicPattern.Create(fPattern, fVars);
end;

function TSymbolicPattern.AsString(const Format: TStringFormat): String;
var
  s: string;
  sc: IStringConvertible;
begin
  if fPattern.Represents(IStringConvertible,sc) then
    s:= sc.AsString(Format)
  else
    s:= fPattern.NativeObject.ClassName;
  Result:= SysUtils.format('pattern(%s  |{%s})', [s, fVars.CommaText])
end;

function TSymbolicPattern.Match(const Expr: IExpression; const Assignments: IContext): boolean;
var
  Matcher: TExpressionMatcher;
begin
  Matcher:= TExpressionMatcher.Create(TContext.SystemFrom(Assignments), fPattern, fVars);
  try
    Matcher.SetupContext(Assignments);
    Result:= Matcher.Match(Expr);
  finally
    FreeAndNil(Matcher);
  end;
end;

function TSymbolicPattern.GetPattern: IExpression;
begin
  Result:= fPattern;
end;

{ TSymbolicRule }

constructor TSymbolicRule.Create(const Pattern, Target: IExpression; const Vars: TStringList);
begin
  inherited Create(Pattern, Vars);
  fTarget:= Target;
end;

destructor TSymbolicRule.Destroy;
begin
  fTarget:= nil;
  inherited;
end;

function TSymbolicRule.AsString(const Format: TStringFormat): String;
var
  p, t: string;
  sc: IStringConvertible;
begin
  if fPattern.Represents(IStringConvertible,sc) then
    p:= sc.AsString(Format)
  else
    p:= fPattern.NativeObject.ClassName;
  if fTarget.Represents(IStringConvertible,sc) then
    t:= sc.AsString(Format)
  else
    t:= fTarget.NativeObject.ClassName;
  Result:= SysUtils.format('rule(%s -> %s |{%s})', [p, t, fVars.CommaText])
end;

function TSymbolicRule.Clone(Deep: Boolean): IExpression;
begin
  Result:= TSymbolicRule.Create(fPattern, fTarget, fVars);
end;

function TSymbolicRule.GetTarget: IExpression;
begin
  Result:= fTarget;
end;


{ TFunctionProperties }

constructor TFunctionProperties.Create(const ass, comm: boolean; const Neutral: IExpressionAtom);
begin
  inherited Create;
  Associative:= ass;
  Commutative:= comm;
  NeutralElement:= Neutral;
end;

{ TPackageSymbolics }

function TPackageSymbolics.Subs_2(Context: IContext; args: TExprList): IExpression;
var
  orig: IExpression;
  rules: IValueList;
  rc: IContext;
  i: integer;
begin
  orig:= args[0].Evaluate(Context);
  if not args[1].Evaluate(Context).Represents(IValueList, rules) then
    raise EMathSysError.Create('Subs requires a list of replacements.');

  rc:= TContext.Create(TContext.SystemFrom(Context), nil);
  for i:= 0 to rules.Length-1 do
    rules.Item[i].Evaluate(rc);

  Result:= SubstituteContextVars(orig, rc);
end;

function TPackageSymbolics.Clone_1(Context: IContext; args: TExprList): IExpression;
begin
  Result:= args[0].Clone(true);
end;

function TPackageSymbolics.Match_2(Context: IContext; args: TExprList; Options: TDynamicArguments): IExpression;
var
  expr: IExpression;
  pattern: ISymbolicPattern;
  assignments: IContext;
begin
  expr:= args[0].Evaluate(Context);
  if not args[1].Evaluate(Context).Represents(ISymbolicPattern, pattern) then
    raise EMathSysError.Create('Match requires a pattern to match');

  assignments:= TContext.Create(TContext.SystemFrom(Context), nil);
  try
    assignments.SetSilent(true);
    if pattern.Match(expr, assignments) then begin
      Result:= assignments.Combine(true);
    end else
      Result:= TValueNull.Create;
  finally
    assignments:= nil;
  end;
end;

function TPackageSymbolics.Pattern_2(Context: IContext; args: TExprList): IExpression;
var
  pat: IExpression;
  ls: IValueList;
  vs: ISymbolReference;
  varList: TStringList;
  i: integer;
begin
  pat:= args[0].Evaluate(Context);
  if not args[1].Represents(IValueList, ls) then
    raise EMathSysError.Create('Pattern variable list needs to be a list');

  varList:= TStringList.Create;
  try
    varList.Sorted:= true;
    varList.Duplicates:= dupIgnore;
    for i:= 0 to ls.Length-1 do begin
      if ls.Item[i].Represents(ISymbolReference, vs) then
        varList.Add(vs.Name)
      else
        raise EMathSysError.Create('Pattern variable list needs to be a list of symbolic references');
    end;

    Result:= TSymbolicPattern.Create(pat, varList);
  finally
    FreeAndNil(varList);
  end;
end;

function TPackageSymbolics.Rule_3(Context: IContext; args: TExprList): IExpression;
var
  pat,tar: IExpression;
  ls: IValueList;
  vs: ISymbolReference;
  varList: TStringList;
  i: integer;
begin
  pat:= args[0].Evaluate(Context);
  if not args[1].Represents(IValueList, ls) then
    raise EMathSysError.Create('Rule variable list needs to be a list');
  tar:= args[2].Evaluate(Context);

  varList:= TStringList.Create;
  try
    varList.Sorted:= true;
    varList.Duplicates:= dupIgnore;
    for i:= 0 to ls.Length-1 do begin
      if ls.Item[i].Represents(ISymbolReference, vs) then
        varList.Add(vs.Name)
      else
        raise EMathSysError.Create('Rule variable list needs to be a list of symbolic references');
    end;

    Result:= TSymbolicRule.Create(pat, tar, varList);
  finally
    FreeAndNil(varList);
  end;
end;

function TPackageSymbolics.Replace_2(Context: IContext; args: TExprList): IExpression;
var
  expr, r: IExpression;
  rule: ISymbolicRule;
  rules: IValueList;
  i: integer;
begin
  expr:= args[0].Evaluate(Context);
  r:= args[1].Evaluate(Context);
  if r.Represents(ISymbolicRule, rule) then
    rules:= TValueList.CreateAs([rule])
  else
  if not r.Represents(IValueList, rules) then
    raise EMathSysError.Create('Replace requires a rule or a list of rules to apply');

  for i:= 0 to rules.Length-1 do
    if not rules.Item[i].Represents(ISymbolicRule, rule) then
      raise EMathSysError.Create('Replace requires a list of rules to apply');

  Result:= ApplyRules(Expr, Context, rules, false);
end;

function TPackageSymbolics.ReplaceAll_2(Context: IContext; args: TExprList): IExpression;
var
  expr, r: IExpression;
  rule: ISymbolicRule;
  rules: IValueList;
  i: integer;
begin
  expr:= args[0].Evaluate(Context);
  r:= args[1].Evaluate(Context);
  if r.Represents(ISymbolicRule, rule) then
    rules:= TValueList.CreateAs([rule])
  else
  if not r.Represents(IValueList, rules) then
    raise EMathSysError.Create('ReplaceAll requires a rule or a list of rules to apply');

  for i:= 0 to rules.Length-1 do
    if not rules.Item[i].Represents(ISymbolicRule, rule) then
      raise EMathSysError.Create('ReplaceAll requires a list of rules to apply');

  Result:= ApplyRules(Expr, Context, rules, true);
end;

////////////////////////////////////////////////////////////////////////////////

constructor TPackageSymbolics.Create;
begin
  inherited Create;
  fFunctionProperties:= TStringList.Create;
  fFunctionProperties.CaseSensitive:= false;
  fFunctionProperties.Sorted:= true;
  fFunctionProperties.Duplicates:= dupIgnore;
  fDefaultProperties:= TFunctionProperties.Create(false, false, nil);
end;

destructor TPackageSymbolics.Destroy;
var
  i: integer;
begin
  for I:= 0 to fFunctionProperties.Count-1 do
    fFunctionProperties.Objects[i].Free;
  FreeAndNil(fFunctionProperties);
  FreeAndNil(fDefaultProperties);
  inherited;
end;

procedure TPackageSymbolics.SetupAlgebra;
begin
  with fFunctionProperties do begin
    AddObject('_plus', TFunctionProperties.Create(true, true, TValueNumber.Create(0)));
    AddObject('_mult', TFunctionProperties.Create(true, true, TValueNumber.Create(1)));
  end;
end;

procedure TPackageSymbolics.OnImport(const MS: TMathSystem);
begin
  inherited;
  SetupAlgebra;
end;

function TPackageSymbolics.Properties(const Name: String): TFunctionProperties;
var
  i: integer;
begin
  i:= fFunctionProperties.IndexOf(Name);
  if i >= 0 then
    Result:= TFunctionProperties(fFunctionProperties.Objects[i])
  else
    Result:= fDefaultProperties;
end;

end.
