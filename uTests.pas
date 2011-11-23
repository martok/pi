unit uTests;

interface

uses SysUtils, uMath, ComCtrls;

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
  protected
    procedure TestParser;
    procedure TestDatatypes;
    procedure TestContexts;
    procedure TestFunctions;
  public
    constructor Create(Output: TRichEdit);
    destructor Destroy; override;
    procedure Run;
  end;

implementation

uses Math;

{ TMathSysTest }

constructor TMathSysTest.Create(Output: TRichEdit);
begin
  inherited Create;
  Sys:= TMathSystem.Create;
  Sys.Output.Render:= Output;
end;

destructor TMathSysTest.Destroy;
begin
  FreeAndNil(Sys);
  inherited;
end;

procedure TMathSysTest.BeginGroup(Name: string);
begin
  Sys.Output.Hint(FIndent + 'Begin: ' + Name,[]);
  FIndent:= FIndent + '  ';
end;

procedure TMathSysTest.EndGroup;
begin
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

procedure TMathSysTest.Run;
begin
  FTotal:= 0;
  FFailed:= 0;
  BeginGroup('Running MathSystem Tests');
  try
    TestParser;
    TestDatatypes;
    TestContexts;
    TestFunctions;
  finally
    EndGroup;
  end;
  Sys.Output.Result(Format('---- Finished ----'#13#10+
                           'Tests Run   : %-4d'#13#10+
                           'Tests Failed: %-4d',[FTotal, FFailed]));
end;

procedure TMathSysTest.TestParser;
var expr: IExpression;
begin
  BeginGroup('Parser');
  try
    BeginGroup('Empty String');
      expr:= Sys.Parse('');
      Expect(expr=nil,'Should be nil');
      EndGroup;

    BeginGroup('Binary: Addition');
      expr:= Sys.Parse('1+2');
      ExpectEqual(expr.StringForm,'Addition(1,2)');
      EndGroup;

    BeginGroup('Unary: Describe');
      expr:= Sys.Parse('?foo');
      ExpectEqual(expr.StringForm,'Describe(foo)');
      EndGroup;

    BeginGroup('Ascending order');
      expr:= Sys.Parse('1+2*3^4');
      ExpectEqual(expr.StringForm,'Addition(1,Multiplication(2,Power(3,4)))');
      EndGroup;

    BeginGroup('Descending order');
      expr:= Sys.Parse('4^3*2+1');
      ExpectEqual(expr.StringForm,'Addition(Multiplication(Power(4,3),2),1)');
      EndGroup;

    BeginGroup('Mixed order');
      expr:= Sys.Parse('1+2^3*4');
      ExpectEqual(expr.StringForm,'Addition(1,Multiplication(Power(2,3),4))');
      EndGroup;
    BeginGroup('Mixed order2');
      expr:= Sys.Parse('1*2^3+4');
      ExpectEqual(expr.StringForm,'Addition(Multiplication(1,Power(2,3)),4)');
      EndGroup;

    BeginGroup('Braces');
      expr:= Sys.Parse('1+(2+3)');
      ExpectEqual(expr.StringForm,'Addition(1,Addition(2,3))');
      EndGroup;
    BeginGroup('Braces 2');
      expr:= Sys.Parse('1*(2+3)');
      ExpectEqual(expr.StringForm,'Multiplication(1,Addition(2,3))');
      EndGroup;
    BeginGroup('Braces 3');
      expr:= Sys.Parse('(1+2)*3');
      ExpectEqual(expr.StringForm,'Multiplication(Addition(1,2),3)');
      EndGroup;
    BeginGroup('Braces 4');
      expr:= Sys.Parse('1+(2*3)+4');
      ExpectEqual(expr.StringForm,'Addition(Addition(1,Multiplication(2,3)),4)');
      EndGroup;

    BeginGroup('Enum');
      expr:= Sys.Parse('1,2,3');
      ExpectEqual(expr.StringForm,'ArgList(1,2,3)');
      EndGroup;
    BeginGroup('Enum 2');
      expr:= Sys.Parse('1,2,3=4');
      ExpectEqual(expr.StringForm,'ArgList(1,2,AssignmentStatic(3,4))');
      EndGroup;
    BeginGroup('Enum 3');
      expr:= Sys.Parse('f:= 1,2');
      ExpectEqual(expr.StringForm,'ArgList(AssignmentDynamic(f,1),2)');
      EndGroup;
    BeginGroup('Enum 4');
      expr:= Sys.Parse('f:= (1,2)');
      ExpectEqual(expr.StringForm,'AssignmentDynamic(f,ArgList(1,2))');
      EndGroup;
    BeginGroup('Enum 5');
      expr:= Sys.Parse('f:= (1,2),3');
      ExpectEqual(expr.StringForm,'ArgList(AssignmentDynamic(f,ArgList(1,2)),3)');
      EndGroup;
    BeginGroup('Enum 6');
      expr:= Sys.Parse('a=1,b=(2,3),c:=4,d:= 5');
      ExpectEqual(expr.StringForm,'ArgList(AssignmentStatic(a,1),AssignmentStatic(b,ArgList(2,3)),AssignmentDynamic(c,4),AssignmentDynamic(d,5))');
      EndGroup;

    BeginGroup('Function 1');
      expr:= Sys.Parse('f:= sin(x)');
      ExpectEqual(expr.StringForm,'AssignmentDynamic(f,sin(x))');
      EndGroup;
    BeginGroup('Function 2');
      expr:= Sys.Parse('sin(cos(sin(cos(x))+1)+2)');
      ExpectEqual(expr.StringForm,'sin(Addition(cos(Addition(sin(cos(x)),1)),2))');
      EndGroup;

    BeginGroup('Subcontext');
      expr:= Sys.Parse('f[x=1,y:=3,4]');
      ExpectEqual(expr.StringForm,'f[ArgList(AssignmentStatic(x,1),AssignmentDynamic(y,3),4)]');
      EndGroup;
  finally
    EndGroup;
  end;
end;

procedure TMathSysTest.TestDatatypes;
var result: IValue;
begin
  Result:= TValue.Create();
  BeginGroup('DataTypes');
  try
    BeginGroup('Num->Num');
      result.SetNumber(42);
      Expect(IsZero(result.GetNumber - 42),'42!=42');
      EndGroup;
    BeginGroup('Num->Str');
      result.SetNumber(42);
      ExpectEqual(result.GetString, '42');
      EndGroup;
    BeginGroup('Str->Str');
      result.SetString('hallo');
      ExpectEqual(result.GetString, 'hallo');
      EndGroup;
    BeginGroup('NumStr->NumStr');
      result.SetString('123');
      ExpectEqual(result.GetString, '123');
      EndGroup;
    BeginGroup('NumStr->Num');
      result.SetString('123');
      Expect(IsZero(result.GetNumber - 123),'"123"!=123');
      EndGroup;
    BeginGroup('HiP->Str');
      result.SetNumber(uMath.cPi);
      ExpectEqual(result.GetString, '3.14159265358979324');
      EndGroup;
    BeginGroup('Str->HiP');
      result.SetString('3.1415926535897932384626433832795');
      Expect(IsZero(result.GetNumber - uMath.cPi),'"pi"!=pi');
      EndGroup;
  finally
    EndGroup;
  end;
end;

procedure TMathSysTest.TestContexts;
var cnst: IExpression;
begin
  BeginGroup('Contexts');
  try
    cnst:= TE_Constant.Create(TValue.Create(42.000));
    BeginGroup('Define 1');
      Sys.Context.Define('test',cnst);
      Expect(Sys.Context.Definition('test')=cnst ,'Didn''t receive same object.');
      EndGroup;
    BeginGroup('Define 2');
      Sys.Context.Define('test',cnst);
      Expect(SameValue(Sys.Context.Value('test').GetNumber, 42),'Value failed.');
      EndGroup;
    BeginGroup('Undefine');
      Sys.Context.Undefine('test');
      Expect(Sys.Context.Definition('test')=nil ,'Should have been gone.');
      EndGroup;
    BeginGroup('Undefined Value');
      try
        Expect(Sys.Context.Value('foo').ValueType=vtUnassigned ,'Undefined value exists.');
      except
        on e: EMathSysError do ;
      end;
      EndGroup;
    BeginGroup('Stacking 1');
      Expect(SameValue(uMath.cPi, Sys.Context.Value('pi').GetNumber), 'Inherited constant missing.');
      EndGroup;
    BeginGroup('Stacking 2');
      Sys.Context.Define('pi',cnst);
      Expect(Sys.Context.Definition('pi')=cnst ,'Shadowed constant still present.');
      EndGroup;
    BeginGroup('Stacking 3');
      Sys.Context.Undefine('pi');
      Expect(SameValue(uMath.cPi, Sys.Context.Value('pi').GetNumber),'Shadowed constant did not resurface.');
      EndGroup;
  finally
    EndGroup;
  end;
end;

procedure TMathSysTest.TestFunctions;
begin
  BeginGroup('Functions');
  try
    BeginGroup('Numeric');
      BeginGroup('Abs');
        ExpectEqual(Sys.Eval('Abs(-10)').GetNumber,10);
        EndGroup;
      BeginGroup('Exp');
        ExpectEqual(Sys.Eval('exp(3)').GetNumber,20.085536923187667740928529654582);
        EndGroup;
      BeginGroup('Ln');
        ExpectEqual(Sys.Eval('ln(e)').GetNumber,1);
        ExpectEqual(Sys.Eval('ln(10)').GetNumber,2.3025850929940456840179914546844);
        EndGroup;
      BeginGroup('Lg');
        ExpectEqual(Sys.Eval('lg(10)').GetNumber,1);
        ExpectEqual(Sys.Eval('lg(15)').GetNumber,1.1760912590556812420812890085306);
        EndGroup;
      BeginGroup('Ld');
        ExpectEqual(Sys.Eval('ld(2)').GetNumber,1);
        ExpectEqual(Sys.Eval('ld(4.2)').GetNumber,2.070389327891397941025388831690257);
        EndGroup;
      BeginGroup('Loga');
        ExpectEqual(Sys.Eval('loga(4,25)').GetNumber,2.32192809488736234787031942948);
        EndGroup;
      BeginGroup('Fac');
        ExpectEqual(Sys.Eval('fac(8)').GetNumber,40320);
        EndGroup;
      EndGroup;
    BeginGroup('Geometric');
      BeginGroup('deg2rad');
        ExpectEqual(Sys.Eval('deg2rad(90)').GetNumber,cPi/2);
        EndGroup;
      BeginGroup('rad2deg');
        ExpectEqual(Sys.Eval('rad2deg(pi/2)').GetNumber,90);
        EndGroup;
      BeginGroup('Sin');
        ExpectEqual(Sys.Eval('sin(pi/2)').GetNumber,1);
        ExpectEqual(Sys.Eval('sin(0)').GetNumber,0);
        EndGroup;
      BeginGroup('Cos');
        Expect(IsZero(Sys.Eval('cos(pi/2)').GetNumber),'Failed');
        ExpectEqual(Sys.Eval('cos(0)').GetNumber,1);
        EndGroup;
      BeginGroup('Tan');
        ExpectEqual(Sys.Eval('tan(deg2rad(80))').GetNumber,5.671281819617709530994418439864);
        ExpectEqual(Sys.Eval('tan(0)').GetNumber,0);
        EndGroup;
      BeginGroup('arcSin');
        ExpectEqual(Sys.Eval('rad2deg(arcsin(1))').GetNumber,90);
        EndGroup;
      BeginGroup('arcCos');
        ExpectEqual(Sys.Eval('rad2deg(arccos(0))').GetNumber,90);
        EndGroup;
      BeginGroup('arcTan');
        ExpectEqual(Sys.Eval('rad2deg(arctan(1))').GetNumber,45);
        ExpectEqual(Sys.Eval('rad2deg(arctan(10,100))').GetNumber,5.71059313749964251269588);
        EndGroup;
      BeginGroup('Sinh');
        ExpectEqual(Sys.Eval('sinh(12)').GetNumber,81377.39570642985422733849);
        EndGroup;
      BeginGroup('Cosh');
        ExpectEqual(Sys.Eval('cosh(3)').GetNumber,10.0676619957777658116);
        EndGroup;
      BeginGroup('Tanh');
        ExpectEqual(Sys.Eval('tanh(1)').GetNumber,0.76159415595576488811945828260479);
        EndGroup;
      BeginGroup('arSinh');
        ExpectEqual(Sys.Eval('arsinh(12)').GetNumber,3.1797854376998788269169715277695);
        EndGroup;
      BeginGroup('arCosh');
        ExpectEqual(Sys.Eval('arcosh(3)').GetNumber,1.7627471740390860504652186499596);
        EndGroup;
      BeginGroup('arTanh');
        ExpectEqual(Sys.Eval('artanh(0.5)').GetNumber,0.54930614433405484569762261846126);
        EndGroup;
      EndGroup;
  finally
    EndGroup;
  end;
end;

end.
 