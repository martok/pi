program pi;

uses
  FastMM4,
  Forms,
  uMain in 'uMain.pas' {fmPiMain},
  uMath in 'uMath.pas',
  uTests in 'uTests.pas',
  uFunctions in 'uFunctions.pas',
  uCCSVList in 'uCCSVList.pas',
  uFunctionsGraphing in 'uFunctionsGraphing.pas',
  uGraphWindow in 'uGraphWindow.pas' {GraphWindow};

{$R *.res}

begin
  fastmm4.ReportMemoryLeaksOnShutdown:= True;
  Application.Initialize;
  Application.CreateForm(TfmPiMain, fmPiMain);
  Application.Run;
end.
