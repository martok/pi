{-----------------------------------------------------------------------------
 The PI Advanced Calculator

 Graphical interface to TAU.
-----------------------------------------------------------------------------}
unit uMain;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, uMath, uMathOutputRichedit, ImgList, ActnList, StdCtrls, ComCtrls, ToolWin, ExtCtrls,
  uFrmInput;

type
  TfmPiMain = class(TForm)
    Panel1: TPanel;
    Splitter1: TSplitter;
    ToolBar1: TToolBar;
    ActionList1: TActionList;
    acRunCmd: TAction;
    acExit: TAction;
    acRunTest: TAction;
    ToolButton1: TToolButton;
    ToolButton2: TToolButton;
    ToolButton3: TToolButton;
    ToolButton4: TToolButton;
    ilButtons: TImageList;
    ToolButton5: TToolButton;
    acHelp: TAction;
    trContext: TTreeView;
    Panel3: TPanel;
    reOutput: TRichEdit;
    frmInput: TfrmInput;
    spltInput: TSplitter;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure acRunCmdExecute(Sender: TObject);
    procedure acExitExecute(Sender: TObject);
    procedure trContextCollapsing(Sender: TObject; Node: TTreeNode;
      var AllowCollapse: Boolean);
    procedure acHelpExecute(Sender: TObject);
    procedure trContextEdited(Sender: TObject; Node: TTreeNode; var S: String);
    procedure trContextEditing(Sender: TObject; Node: TTreeNode; var AllowEdit: Boolean);
    procedure acRunTestExecute(Sender: TObject);
  private
    { Private-Deklarationen }
    fOutput: TOutputRichEdit;
    procedure UpdateContext;
    procedure InputResized(Sender: TObject); 
  public
    { Public-Deklarationen }
    MathS: TMathSystem;
  end;

var
  fmPiMain: TfmPiMain;

implementation

uses
  ShellAPI,
  uMathIntf,
  uMathSelfTests,
  uFunctions, uFunctionsStatistics, uFunctionsGraphing, uFunctionsSymbolics, uMathDimensions;

const
  sProgramTitle = 'Pi Advanced Calculator';
  sProgramVersionStr = 'V7';

{$R *.dfm}

procedure TfmPiMain.FormCreate(Sender: TObject);
begin
  Application.Title:= sProgramTitle;
  Caption:= Format('%s   (%s)',[sProgramTitle, sProgramVersionStr]);
  fOutput:= TOutputRichEdit.Create;
  fOutput.Render:= reOutput;
  MathS:= TMathSystem.Create(fOutput);
  with MathS do begin
    RegisterPackage(TPackageTrig.Create);
    RegisterPackage(TPackageElementary.Create);
    RegisterPackage(TPackageNumerical.Create);
    RegisterPackage(TPackageLists.Create);
    RegisterPackage(TPackageData.Create);
    RegisterPackage(TPackageStatistics.Create);
    RegisterPackage(TPackageGraph.Create);
    RegisterPackage(TPackageSymbolics.Create);   
    RegisterPackage(TPackageDimensions.Create);   
  end;
  reOutput.Clear;
  frmInput.Clear;
  frmInput.OnRunCommand:= acRunCmdExecute;          
  frmInput.OnSizeForced:= InputResized;
end;

procedure TfmPiMain.FormDestroy(Sender: TObject);
begin
  FreeAndNil(MathS);
  FreeAndNil(fOutput);
end;

procedure TfmPiMain.UpdateContext;
var ct: TContext;
    j: integer;
    n: TTreeNode;
    s: string;
    x: IExpression;
    sc: IStringConvertible;
begin
  trContext.Items.BeginUpdate;
  try
    trContext.Items.Clear;
    ct:= TContext(MathS.Context.NativeObject);
    while ct<>nil do begin
      s:= ct.ContextName;
      if s='' then
        s:= 'Context';
      n:= trContext.Items.AddObject(nil, format('%s (%d)',[s,ct.Count]), ct);
      for j:= 0 to ct.Count-1 do begin
        x:= ct.Definition(ct.Name[j]); 
        if x.Represents(IStringConvertible, sc) then
          s:= sc.AsString(STR_FORMAT_OUTPUT)
        else
          s:= '<'+x.NativeObject.ClassName+'>';

        trContext.Items.AddChild(n, format('%s = %s', [ct.Name[j], s]));
      end;
      if not Assigned(ct.Parent) then
        break;
      ct:= TContext(ct.Parent.NativeObject);
    end;
    trContext.FullExpand;
  finally
    trContext.Items.EndUpdate;
  end;
end;

procedure TfmPiMain.FormShow(Sender: TObject);
begin
  UpdateContext;
  frmInput.SetFocus;
  InputResized(Self);
end;

procedure TfmPiMain.acRunCmdExecute(Sender: TObject);
var f: string;
begin
  try
    f:= frmInput.Text;
    frmInput.AddHistory(f);
    MathS.Output.Input(f);
    MathS.Run(f);
    frmInput.Text:= 'ans';
    frmInput.SelectAll;
  except
    on e: Exception do
      MathS.Output.Error('%s: %s',[e.ClassName, e.Message]);
  end;
  UpdateContext;
end;

procedure TfmPiMain.acExitExecute(Sender: TObject);
begin
  Close;
end;

procedure TfmPiMain.trContextEdited(Sender: TObject; Node: TTreeNode; var S: String);
begin
  S:= Node.Text;
end;

procedure TfmPiMain.trContextEditing(Sender: TObject; Node: TTreeNode; var AllowEdit: Boolean);
begin
  AllowEdit:= Assigned(Node.Parent);
end;

procedure TfmPiMain.trContextCollapsing(Sender: TObject; Node: TTreeNode;
  var AllowCollapse: Boolean);
begin
  AllowCollapse:= false;
end;

procedure TfmPiMain.acHelpExecute(Sender: TObject);
var
  fn: string;
begin
  fn:= ExtractFilePath(ParamStr(0))+'documentation.html';
  if not FileExists(fn) then begin
    if MessageDlg('Could not load "documentation.html". Launch from Web?', mtConfirmation, [mbYes, mbNo], 0) = mrYes then
      fn:= 'https://raw.github.com/martok/pi/master/bin/documentation.html'
    else
      exit;
  end;
  ShellExecute(Handle, 'open',PAnsiChar(fn),nil,nil, SW_SHOWNORMAL);
end;

procedure TfmPiMain.acRunTestExecute(Sender: TObject);
var
  Tester: TMathSysTest;
begin
  Tester:= TMathSysTest.Create(MathS);
  try
    Tester.Run;
  finally
    FreeAndNil(Tester);
  end;
end;

procedure TfmPiMain.InputResized(Sender: TObject);
begin
  spltInput.MinSize:= frmInput.Height;
end;

end.
