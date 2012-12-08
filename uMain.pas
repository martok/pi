unit uMain;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, uMath, StdCtrls, ExtCtrls, ActnList, ToolWin, ComCtrls, ImgList,
  ShellAPI;

type
  TfmPiMain = class(TForm)
    Panel1: TPanel;
    Panel2: TPanel;
    Splitter1: TSplitter;
    cbInput: TComboBox;
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
    reOutput: TRichEdit;
    ToolButton5: TToolButton;
    acHelp: TAction;
    trContext: TTreeView;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure cbInputKeyPress(Sender: TObject; var Key: Char);
    procedure acRunCmdExecute(Sender: TObject);
    procedure acExitExecute(Sender: TObject);
    procedure acRunTestExecute(Sender: TObject);
    procedure trContextCollapsing(Sender: TObject; Node: TTreeNode;
      var AllowCollapse: Boolean);
    procedure acHelpExecute(Sender: TObject);
    procedure trContextEdited(Sender: TObject; Node: TTreeNode; var S: String);
    procedure trContextEditing(Sender: TObject; Node: TTreeNode; var AllowEdit: Boolean);
  private
    { Private-Deklarationen }
    procedure UpdateContext;
  public
    { Public-Deklarationen }
    MathS: TMathSystem;
  end;

var
  fmPiMain: TfmPiMain;

implementation

uses uTests;

const
  sProgramTitle = 'pi - Tiny Math Tool';
  sProgramVersionStr = 'V 5';

{$R *.dfm}

procedure TfmPiMain.FormCreate(Sender: TObject);
begin
  Application.Title:= sProgramTitle;
  Caption:= Format('%s   (%s)',[sProgramTitle, sProgramVersionStr]);
  MathS:= TMathSystem.Create;
  MathS.Output.Render:= reOutput;
  reOutput.Clear;
  cbInput.Clear;
end;

procedure TfmPiMain.FormDestroy(Sender: TObject);
begin
  FreeAndNil(MathS);
end;

procedure TfmPiMain.UpdateContext;
var ct:TContext;
    j: integer;
    n: TTreeNode;
    s: string;
begin
  trContext.Items.BeginUpdate;
  try
    trContext.Items.Clear;
    ct:= MathS.Context;
    while ct<>nil do begin
      s:= ct.ContextName;
      if s='' then
        s:= 'Context';
      n:= trContext.Items.AddObject(nil, format('%s (%d)',[s,ct.Count]), ct);
      for j:= 0 to ct.Count-1 do
        trContext.Items.AddChild(n, format('%s = %s', [ct.Name[j], ct.Definition(ct.Name[j]).StringForm]));
      ct:= ct.Parent;
    end;
    trContext.FullExpand;
  finally
    trContext.Items.EndUpdate;
  end;
end;

procedure TfmPiMain.FormShow(Sender: TObject);
begin
  UpdateContext;
  cbInput.SetFocus;
end;

procedure TfmPiMain.cbInputKeyPress(Sender: TObject; var Key: Char);
begin
  case Key of
    Chr(VK_RETURN): begin
      Key:= #0;
      acRunCmd.Execute;
    end;
    ',': begin
      // has the numpad comma been the reason?
      // better place would be KeyDown, but we can't change anything from there.
      if GetKeyState(VK_DECIMAL) < 0 then
        Key:= NeutralFormatSettings.DecimalSeparator;
    end;
  end;
end;

procedure TfmPiMain.acRunCmdExecute(Sender: TObject);
var f: string;
begin
  try
    f:= cbInput.Text;
    MathS.Output.Input(f);
    MathS.Run(f);
    cbInput.Text:= 'ans';
    cbInput.SelectAll;
    if cbInput.Items.IndexOf(f)>=0 then
      cbInput.Items.Delete(cbInput.Items.IndexOf(f));
    cbInput.Items.Insert(0,f);
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

procedure TfmPiMain.acRunTestExecute(Sender: TObject);
var testsys: TMathSysTest;
begin
  testsys:= TMathSysTest.Create(reOutput);
  try
    testsys.Run;
  finally
    testsys.Free;
  end;
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

end.
