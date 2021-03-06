{-----------------------------------------------------------------------------
 Selftests for the kernel and several functions

 NOT 100% coverage. Tests are added as needed.
-----------------------------------------------------------------------------}
unit uMathSelfTests;

interface

uses SysUtils, uMath, uMathIntf;

type
  TMathSysTest = class
  private
    FIndent: string;
    Sys: TMathSystem;
    FTotal, FFailed: integer;
    procedure BeginGroup(Name: string);
    procedure EndGroup;
    procedure Expect(Condition: Boolean; FailMessage: string; SuccessMessage: string='Success');
    procedure ExpectEqual(A, Ref: Variant);
    procedure ExpectZero(A: Number);
  protected
    procedure TestParser;
    procedure TestDatatypes;
    procedure TestOperations;
    procedure TestFunctions;
    procedure TestFunctional;
    function ExprString(e: IExpression): String;
    function EvalNum(Expr: String): Number;
    function EvalStr(Expr: String): string;
  public
    constructor Create(Host: TMathSystem);
    destructor Destroy; override;
    procedure Run;
  end;

implementation

uses
  Windows, Math, uMathValues, uMathConstants, uFPUSupport;

{ TMathSysTest }

constructor TMathSysTest.Create(Host: TMathSystem);
begin
  inherited Create;
  Sys:= Host;
end;

destructor TMathSysTest.Destroy;
begin
  inherited;
end;

procedure TMathSysTest.BeginGroup(Name: string);
begin
  Sys.Output.Hint(FIndent + 'Begin: ' + Name,[]);
  FIndent:= FIndent + '  ';
  Sys.NewContext(Name);
end;

procedure TMathSysTest.EndGroup;
begin                                
  Sys.DropContext;
  FIndent:= Copy(FIndent,1, length(FIndent)-2);
end;

procedure TMathSysTest.Expect(Condition: Boolean; FailMessage: string; SuccessMessage: string);
begin
  inc(FTotal);
  if Condition then
    Sys.Output.Hint(FIndent + SuccessMessage,[])
  else begin
    Sys.Output.Error(FIndent + FailMessage,[]);
    inc(FFailed);
  end;
end;

procedure TMathSysTest.ExpectEqual(A,Ref: Variant);
begin
  Expect(Ref=A,'Expected: '+String(Ref)+#13#10+FIndent+
               'found   : '+String(A))
end;

procedure TMathSysTest.ExpectZero(A: Number);
begin
  Expect(fzero(a),'Expected 0, found   : '+NumberToStr(a,false));
end;

procedure TMathSysTest.Run;
var
  pc1,pc2,pf: Int64;
begin
  FTotal:= 0;
  FFailed:= 0;
  QueryPerformanceFrequency(pf);
  QueryPerformanceCounter(pc1);
  BeginGroup('Running MathSystem Tests');
  try
    TestParser;
    TestDatatypes;  
    TestOperations;
    TestFunctions;
    TestFunctional;
  finally
    EndGroup;
  end;               
  QueryPerformanceCounter(pc2);
  Sys.Output.Result(Format('---- Finished ----'#13#10+
                           'Tests Run   : %-4d'#13#10+
                           'Tests Failed: %-4d'#13#10+
                           'Time        : %-4.2fs',
                           [FTotal, FFailed, (pc2-pc1)/pf]));
end;



procedure TMathSysTest.TestParser;
var
  expr: IExpression;
begin
  BeginGroup('Parser');
  try
    BeginGroup('Empty String');
      expr:= Sys.Parse('');
      Expect(expr=nil,'Should be nil');
      EndGroup;

    BeginGroup('Binary: Addition');
      expr:= Sys.Parse('1+2');
      ExpectEqual(ExprString(expr),'_plus(1,2)');
      EndGroup;

    BeginGroup('Unary: Describe');
      expr:= Sys.Parse('?foo');
      ExpectEqual(ExprString(expr),'_describe(foo)');
      EndGroup;

    BeginGroup('Ascending order');
      expr:= Sys.Parse('1+2*3^4');
      ExpectEqual(ExprString(expr),'_plus(1,_mult(2,_pow(3,4)))');
      EndGroup;

    BeginGroup('Descending order');
      expr:= Sys.Parse('4^3*2+1');
      ExpectEqual(ExprString(expr),'_plus(_mult(_pow(4,3),2),1)');
      EndGroup;

    BeginGroup('Mixed order');
      expr:= Sys.Parse('1+2^3*4');
      ExpectEqual(ExprString(expr),'_plus(1,_mult(_pow(2,3),4))');
      EndGroup;
    BeginGroup('Mixed order2');
      expr:= Sys.Parse('1*2^3+4');
      ExpectEqual(ExprString(expr),'_plus(_mult(1,_pow(2,3)),4)');
      EndGroup;

    BeginGroup('Braces');
      expr:= Sys.Parse('1+(2+3)');
      ExpectEqual(ExprString(expr),'_plus(1,2,3)');
      EndGroup;
    BeginGroup('Braces 2');
      expr:= Sys.Parse('1*(2+3)');
      ExpectEqual(ExprString(expr),'_mult(1,_plus(2,3))');
      EndGroup;
    BeginGroup('Braces 3');
      expr:= Sys.Parse('(1+2)*3');
      ExpectEqual(ExprString(expr),'_mult(_plus(1,2),3)');
      EndGroup;
    BeginGroup('Braces 4');
      expr:= Sys.Parse('1+(2*3)+4');
      ExpectEqual(ExprString(expr),'_plus(1,_mult(2,3),4)');
      EndGroup;

    BeginGroup('Enum');
      expr:= Sys.Parse('1,2,3');
      ExpectEqual(ExprString(expr),'_comma(1,2,3)');
      EndGroup;
    BeginGroup('Enum 2');
      expr:= Sys.Parse('1,2,3=4');
      ExpectEqual(ExprString(expr),'_comma(1,2,_assign(3,4))');
      EndGroup;
    BeginGroup('Enum 3');
      expr:= Sys.Parse('f:= 1,2');
      ExpectEqual(ExprString(expr),'_comma(_define(f,1),2)');
      EndGroup;
    BeginGroup('Enum 4');
      expr:= Sys.Parse('f:= (1,2)');
      ExpectEqual(ExprString(expr),'_define(f,_comma(1,2))');
      EndGroup;
    BeginGroup('Enum 5');
      expr:= Sys.Parse('f:= (1,2),3');
      ExpectEqual(ExprString(expr),'_comma(_define(f,_comma(1,2)),3)');
      EndGroup;
    BeginGroup('Enum 6');
      expr:= Sys.Parse('a=1,b=(2,3),c:=4,d:= 5');
      ExpectEqual(ExprString(expr),'_comma(_assign(a,1),_assign(b,_comma(2,3)),_define(c,4),_define(d,5))');
      EndGroup;

    BeginGroup('Function 1');
      expr:= Sys.Parse('f:= sin(x)');
      ExpectEqual(ExprString(expr),'_define(f,sin(x))');
      EndGroup;
    BeginGroup('Function 2');
      expr:= Sys.Parse('sin(cos(sin(cos(x))+1)+2)');
      ExpectEqual(ExprString(expr),'sin(_plus(cos(_plus(sin(cos(x)),1)),2))');
      EndGroup;

    BeginGroup('Subcontext');
      expr:= Sys.Parse('f[x=1,y:=3,4]');
      ExpectEqual(ExprString(expr),'f[_assign(x,1),_define(y,3),4]');
      EndGroup;

  finally
    EndGroup;
  end;
end;

procedure TMathSysTest.TestDatatypes;
var
  n: IValueNumber;
  s: IValueString;
  e: IExpression;
begin
  BeginGroup('DataTypes');
  try
    BeginGroup('Num->Num');
      n:= TValueFactory.Integer(42);
      Expect(fzero(n.ValueFloat - 42),'42!=42');
      EndGroup;
    BeginGroup('Num->Str');
      n:= TValueFactory.Integer(42);
      ExpectEqual(ExprString(n), '42');
      EndGroup;
    BeginGroup('Str->Str');
      s:= TValueString.Create('hallo');
      ExpectEqual(s.Value, 'hallo');
      EndGroup;
    BeginGroup('NumStr->Num'); 
      e:= Sys.Evaluate(Sys.Parse('123'));
      Expect(e.Represents(IValueNumber, n),'Cannot cast Result to IValueNumber');
      Expect(fzero(n.ValueFloat - 123),'"123"!=123');
      EndGroup;
    BeginGroup('HiP->Str');
      n:= TValueFactory.Float(uMathConstants.cPi);
      ExpectEqual(ExprString(n), '3.14159265358979324');
      EndGroup;
    BeginGroup('Str->HiP');
      e:= Sys.Evaluate(Sys.Parse('3.1415926535897932384626433832795'));
      Expect(e.Represents(IValueNumber, n),'Cannot cast Result to IValueNumber');
      Expect(fzero(n.ValueFloat - uMathConstants.cPi),'"pi"!=pi');
      EndGroup;
  finally
    EndGroup;
  end;
end;

procedure TMathSysTest.TestOperations;
begin
  BeginGroup('Arithmetic Operations');
  try
    BeginGroup('Numeric');
      BeginGroup('Add');
        ExpectEqual(EvalNum('1+1'),2);
        ExpectEqual(EvalNum('pi+pi'),cTau);
        EndGroup;
      BeginGroup('Sub');
        ExpectZero(EvalNum('1-1'));
        ExpectEqual(EvalNum('123456789012345678-1'),123456789012345677);
        ExpectZero(EvalNum('pi-pi'));
        EndGroup;
      BeginGroup('Mul');
        ExpectEqual(EvalNum('2*pi'),cTau);  
     //   ExpectZero(EvalNum('(1/0)*0'));
        EndGroup;
      BeginGroup('Div');
        ExpectEqual(EvalNum('1/0'),Infinity);
        ExpectEqual(EvalNum('(-1)/0'),NegInfinity);
        Expect(IsNan(EvalNum('0/0')),'Failed');
        EndGroup;
      BeginGroup('Mod');
        ExpectEqual(EvalNum('23%5'),3);
        ExpectEqual(EvalNum('-23%5'),-3);
        ExpectEqual(EvalNum('356458567243513653%5'),3);
        ExpectEqual(EvalNum('356458567243513653%15'),3);
        ExpectEqual(EvalNum('35645856724351365347%5'),2);
        ExpectEqual(EvalNum('35645856724351365347%15'),2);
        EndGroup;
      EndGroup;
  finally
    EndGroup;
  end;
end;

procedure TMathSysTest.TestFunctions;
begin
  BeginGroup('Functions - TBD');
  try
    BeginGroup('Numeric');
      BeginGroup('Abs');
        ExpectEqual(EvalNum('Abs(-10)'),10);
        EndGroup;
      BeginGroup('Exp');
        ExpectEqual(EvalNum('exp(3)'),20.085536923187667740928529654582);
        EndGroup;
      BeginGroup('Ln');
        ExpectEqual(EvalNum('ln(e)'),1);
        ExpectEqual(EvalNum('ln(10)'),2.3025850929940456840179914546844);
        EndGroup;
      BeginGroup('Lg');
        ExpectEqual(EvalNum('lg(10)'),1);
        ExpectEqual(EvalNum('lg(15)'),1.1760912590556812420812890085306);
        EndGroup;
      BeginGroup('Ld');
        ExpectEqual(EvalNum('ld(2)'),1);
        ExpectEqual(EvalNum('ld(4.2)'),2.070389327891397941025388831690257);
        EndGroup;
      BeginGroup('Loga');
        ExpectEqual(EvalNum('loga(4,25)'),2.32192809488736234787031942948);
        EndGroup;
      BeginGroup('Fac');
        ExpectEqual(EvalNum('fac(8)'),40320);
        EndGroup;
      EndGroup;
    BeginGroup('Geometric');
      BeginGroup('deg2rad');
        ExpectEqual(EvalNum('90_''�''_''rad'''),cPi/2);
        EndGroup;
      BeginGroup('rad2deg');
        ExpectEqual(EvalStr('(pi/2)_''rad''_''�'''),'90 �');
        EndGroup;
      BeginGroup('Sin');
        ExpectEqual(EvalNum('sin(pi/2)'),1);
        ExpectZero(EvalNum('sin(0)'));
        EndGroup;
      BeginGroup('Cos');
        ExpectZero(EvalNum('cos(pi/2)'));
        ExpectEqual(EvalNum('cos(0)'),1);
        EndGroup;
      BeginGroup('Tan');
        ExpectEqual(EvalNum('tan(80_''�''_''rad'')'), 5.671281819617709530994418439864);
        ExpectEqual(EvalNum('tan(pi/4)'),1.0);
        ExpectEqual(EvalNum('tan(0)'),0);
        EndGroup;
      BeginGroup('arcSin');
        ExpectEqual(EvalStr('arcsin(1) _''rad''_''�'''),'90 �');
        EndGroup;
      BeginGroup('arcCos');
        ExpectEqual(EvalStr('arccos(0) _''rad''_''�'''),'90 �');
        EndGroup;
      BeginGroup('arcTan');
        ExpectEqual(EvalStr('arctan(1) _''rad''_''�'''),'45 �');
        ExpectEqual(EvalNum('arctan(10,100)'),0.0996686524911620273784461198780205);
        EndGroup;
      BeginGroup('Sinh');
        ExpectEqual(EvalNum('sinh(12)'),81377.39570642985422733849);
        EndGroup;
      BeginGroup('Cosh');
        ExpectEqual(EvalNum('cosh(3)'),10.0676619957777658116);
        EndGroup;
      BeginGroup('Tanh');
        ExpectEqual(EvalNum('tanh(1)'),0.76159415595576488811945828260479);
        EndGroup;
      BeginGroup('arSinh');
        ExpectEqual(EvalNum('arsinh(12)'),3.1797854376998788269169715277695);
        EndGroup;
      BeginGroup('arCosh');
        ExpectEqual(EvalNum('arcosh(3)'),1.7627471740390860504652186499596);
        EndGroup;
      BeginGroup('arTanh');
        ExpectEqual(EvalNum('artanh(0.5)'),0.54930614433405484569762261846126);
        EndGroup;
      EndGroup;
  finally
    EndGroup;
  end;
end;

procedure TMathSysTest.TestFunctional;
var
  expr: IExpression;
begin
  BeginGroup('Functional definitions');
  try
    BeginGroup('Definition');
      expr:= Sys.Parse('{a} -> a*3');
      ExpectEqual(ExprString(expr),'_function({a},_mult(a,3))');

      expr:= Sys.Parse('f:= {a} -> a*3');
      ExpectEqual(ExprString(expr),'_define(f,_function({a},_mult(a,3)))');

      Sys.Evaluate(expr);
      ExpectEqual(EvalNum('f[3]'), 9);

      EndGroup;
  finally
    EndGroup;
  end;
end;

function TMathSysTest.ExprString(e: IExpression): String;
begin
  Result:= e.AsString(STR_FORM_FULL);
end;

function TMathSysTest.EvalNum(Expr: String): Number;
var
  e: IExpression;
  n: IValueNumber;
begin
  e:= Sys.Evaluate(Sys.Parse(Expr));
  Expect(e.Represents(IValueNumber, n), 'Cannot cast Result to Number');
  Result:= n.ValueFloat;
end;

function TMathSysTest.EvalStr(Expr: String): string;
var
  e: IExpression;
begin
  e:= Sys.Evaluate(Sys.Parse(Expr));
  Result:= e.AsString(STR_FORM_STANDARD);
end;

end.
