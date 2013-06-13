program pi;

{$DEFINE USEFASTMM}

uses
  FastMM4,
  Forms,
  uMain in 'uMain.pas' {fmPiMain},
  uMath in 'uMath.pas',
  uTests in 'uTests.pas',
  uFunctions in 'uFunctions.pas',
  uCCSVList in 'uCCSVList.pas',
  uFunctionsGraphing in 'uFunctionsGraphing.pas',
  uGraphWindow in 'uGraphWindow.pas' {GraphWindow},
  uFunctionsStatistics in 'uFunctionsStatistics.pas',
  uMathIntf in 'uMathIntf.pas';

{$R *.res}

begin
  {$IFDEF USEFASTMM}FastMM4.ReportMemoryLeaksOnShutdown:= True;{$ENDIF}
  Application.Initialize;
  Application.CreateForm(TfmPiMain, fmPiMain);
  Application.Run;
end.
