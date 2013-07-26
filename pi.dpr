program pi;

{$DEFINE USEFASTMM}

uses
  FastMM4,
  Forms,
  uMain in 'uMain.pas' {fmPiMain},
  uMath in 'uMath.pas',
  uFunctions in 'uFunctions.pas',
  uCCSVList in 'uCCSVList.pas',
  uFunctionsGraphing in 'uFunctionsGraphing.pas',
  uGraphWindow in 'uGraphWindow.pas' {GraphWindow},
  uFunctionsStatistics in 'uFunctionsStatistics.pas',
  uMathIntf in 'uMathIntf.pas',
  uMathConstants in 'uMathConstants.pas',
  uMathValues in 'uMathValues.pas',
  uFunctionsSymbolics in 'uFunctionsSymbolics.pas',
  uMathOutputRichEdit in 'uMathOutputRichEdit.pas',
  uMathOutputStdout in 'uMathOutputStdout.pas',
  uMathDimensions in 'uMathDimensions.pas',
  uCIntegerBucketList in 'uCIntegerBucketList.pas';

{$R *.res}

begin
  {$IFDEF USEFASTMM}FastMM4.ReportMemoryLeaksOnShutdown:= True;{$ENDIF}
  Application.Initialize;
  Application.CreateForm(TfmPiMain, fmPiMain);
  Application.Run;
end.
