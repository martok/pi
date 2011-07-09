program pi;

uses
  FastMM4,
  Forms,
  uMain in 'uMain.pas' {Form1},
  uMath in 'uMath.pas',
  uTests in 'uTests.pas';

{$R *.res}

begin
  fastmm4.ReportMemoryLeaksOnShutdown:= True;
  Application.Initialize;
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.
