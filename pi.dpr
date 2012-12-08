program pi;

{$DEFINE USEFASTMM}

uses
  {$IFDEF USEFASTMM}FastMM4,{$ENDIF}
  Forms,
  uMain in 'uMain.pas' {fmPiMain},
  uMath in 'uMath.pas',
  uTests in 'uTests.pas',
  uFunctions in 'uFunctions.pas',
  uCCSVList in 'uCCSVList.pas',
  uFunctionsGraphing in 'uFunctionsGraphing.pas',
  uGraphWindow in 'uGraphWindow.pas' {GraphWindow},
  uFunctionsStatistics in 'uFunctionsStatistics.pas';

{$R *.res}

begin
  {$IFDEF USEFASTMM}FastMM4.ReportMemoryLeaksOnShutdown:= True;{$ENDIF}
  Application.Initialize;
  Application.CreateForm(TfmPiMain, fmPiMain);
  Application.Run;
end.
