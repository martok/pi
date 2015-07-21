program pi;

uses
  Forms,
  uMain in 'uMain.pas' {fmPiMain},
  uGraphWindow in 'uGraphWindow.pas' {GraphWindow},
  uFrmInput in 'uFrmInput.pas' {frmInput: TFrame},
  uDockableForms in 'uDockableForms.pas',
  uFPUSupport in 'src\kernel\uFPUSupport.pas',
  uMath in 'src\kernel\uMath.pas',
  uMathConstants in 'src\kernel\uMathConstants.pas',
  uMathDimensions in 'src\kernel\uMathDimensions.pas',
  uMathIntf in 'src\kernel\uMathIntf.pas',
  uMathOutputRichEdit in 'src\kernel\uMathOutputRichEdit.pas',
  uMathOutputStdout in 'src\kernel\uMathOutputStdout.pas',
  uMathValues in 'src\kernel\uMathValues.pas',
  uExpressionMatcher in 'src\packages\uExpressionMatcher.pas',
  uFunctions in 'src\packages\uFunctions.pas',
  uFunctionsGraphing in 'src\packages\uFunctionsGraphing.pas',
  uFunctionsStatistics in 'src\packages\uFunctionsStatistics.pas',
  uFunctionsSymbolics in 'src\packages\uFunctionsSymbolics.pas',
  uCCSVList in 'src\uCCSVList.pas',
  uChartPainter in 'src\uChartPainter.pas',
  uChartScale in 'src\uChartScale.pas',
  uCIntegerBucketList in 'src\uCIntegerBucketList.pas',
  uMathSelfTests in 'src\uMathSelfTests.pas',
  uChartExport in 'src\uChartExport.pas';

{$R *.res}

begin
  {$IFDEF USEFASTMM}FastMM4.ReportMemoryLeaksOnShutdown:= True;{$ENDIF}
  Application.Initialize;
  Application.CreateForm(TfmPiMain, fmPiMain);
  Application.Run;
end.
