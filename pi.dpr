program pi;

uses
  Forms,
  uMain in 'uMain.pas' {fmPiMain},
  uGraphWindow in 'uGraphWindow.pas' {GraphWindow},
  uFrmInput in 'uFrmInput.pas' {frmInput: TFrame},
  uDockableForms in 'uDockableForms.pas';

{$R *.res}

begin
  {$IFDEF USEFASTMM}FastMM4.ReportMemoryLeaksOnShutdown:= True;{$ENDIF}
  Application.Initialize;
  Application.CreateForm(TfmPiMain, fmPiMain);
  Application.Run;
end.
