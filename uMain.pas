{-----------------------------------------------------------------------------
 The PI Advanced Calculator

 Graphical interface to TAU.
-----------------------------------------------------------------------------}
unit uMain;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, uMath, uMathOutputRichedit, ImgList, ActnList, StdCtrls, ComCtrls, ToolWin, ExtCtrls,
  uFrmInput, ButtonTabControl, uDockableForms;

type
  TfmPiMain = class(TForm)
    ActionList1: TActionList;
    acRunCmd: TAction;
    acExit: TAction;
    acRunTest: TAction;
    ilButtons: TImageList;
    acHelp: TAction;
    pnWorkspace: TPanel;
    Splitter1: TSplitter;
    Panel3: TPanel;
    spltInput: TSplitter;
    reOutput: TRichEdit;
    frmInput: TfrmInput;
    Panel1: TPanel;
    ToolBar1: TToolBar;
    ToolButton1: TToolButton;
    ToolButton5: TToolButton;
    ToolButton2: TToolButton;
    ToolButton3: TToolButton;
    ToolButton4: TToolButton;
    trContext: TTreeView;
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
    fPageControl: TDrawPageControl;
    fOutput: TOutputRichEdit;
    procedure UpdateContext;
    procedure UpdateTabView;
    function NewTab(const Title: String): TButtonTabSheet;
    procedure InputResized(Sender: TObject);
    procedure TabButtonClick(Sender: TObject; Button: TTabButton);
  public
    { Public-Deklarationen }
    MathS: TMathSystem;
    procedure InsertIntoTab(const dockable: IDockableForm);
  end;

var
  fmPiMain: TfmPiMain;

implementation

uses
  ShellAPI,
  VCLFixes,
  uMathIntf,
  uMathSelfTests,
  uFunctions, uFunctionsStatistics, uFunctionsGraphing, uFunctionsSymbolics, uMathDimensions,
  RTLConsts;

const
  sProgramTitle = 'Pi Advanced Calculator';
  sProgramVersionStr = 'V8';
  sTabInput = 'Input';

{$R *.dfm}

procedure TfmPiMain.FormCreate(Sender: TObject);
begin
  Application.Title:= sProgramTitle;
  Caption:= Format('%s   (%s)',[sProgramTitle, sProgramVersionStr]);
  UpdateTabView;
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
  FreeAndNil(fPageControl);
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
          s:= sc.AsString(STR_FORM_STANDARD)
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
    on e: Exception do begin
      // kernel exceptions are already logged at .Run, this are all others.
      MathS.Output.Error('Unhandled Error:',[]);
      MathS.Output.Error('%s: %s',[e.ClassName, e.Message]);
    end;
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

procedure TfmPiMain.UpdateTabView;
begin
  if Assigned(fPageControl) then begin
    if fPageControl.PageCount = 1 then begin
      pnWorkspace.Visible:= false;
      try
        pnWorkspace.Parent:= Self;
        FreeAndNil(fPageControl);
      finally
        pnWorkspace.Visible:= true;
      end;
    end;
  end;
end;

function TfmPiMain.NewTab(const Title: String): TButtonTabSheet;
var
  ts: TTabSheet;
begin
  if not Assigned(fPageControl) then begin    
    pnWorkspace.Visible:= false;
    try
      fPageControl:= TDrawPageControl.Create(Self);
      fPageControl.Align:= alClient;
      fPageControl.Parent:= Self;
      ts:= TTabSheet.Create(fPageControl);
      ts.PageControl:= fPageControl;
      ts.Caption:= sTabInput;
      pnWorkspace.Parent:= ts;
    finally
      pnWorkspace.Visible:= true;
    end;
  end;

  Result:= TButtonTabSheet.Create(fPageControl);  
  Result.PageControl:= fPageControl;
  Result.Caption:= Title;           
  Result.Buttons:= [tbRestore, tbClose];
  Result.OnTabButtonClick:= TabButtonClick;
  fPageControl.ActivePage:= Result;
end;

procedure TfmPiMain.TabButtonClick(Sender: TObject; Button: TTabButton);
var
  ts: TButtonTabSheet;
  f: TForm;
  d: IDockableForm;
begin
  ts:= Sender as TButtonTabSheet;
  case Button of   
    tbRestore: begin
      if (ts.ControlCount > 0) and (ts.Controls[0] is TForm) then begin
        if Supports(ts.Controls[0], IDockableForm, d) then
          d.Undock;  
        ts.PageControl:= nil;
        FreeAndNil(ts);
      end;
    end;
    tbClose: begin
      if (ts.ControlCount > 0) and (ts.Controls[0] is TForm) then begin
        f:= TForm(ts.Controls[0]);
        f.Hide;
        if Supports(f, IDockableForm, d) then
          d.Undock;
        f.Close;
      end;
      ts.PageControl:= nil;
      FreeAndNil(ts);
    end;
  end;
  UpdateTabView;
end;

procedure TfmPiMain.InsertIntoTab(const dockable: IDockableForm);
var
  ts: TButtonTabSheet;
begin
  ts:= NewTab(dockable.GetCaption);
  dockable.Dock(ts);
end;

end.
