unit uMain;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, uMath, StdCtrls, ExtCtrls, ActnList, ToolWin, ComCtrls, ImgList,
  TreeNT;

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
    trContext: TTreeNT;
    ToolButton5: TToolButton;
    acHelp: TAction;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure cbInputKeyPress(Sender: TObject; var Key: Char);
    procedure acRunCmdExecute(Sender: TObject);
    procedure acExitExecute(Sender: TObject);
    procedure acRunTestExecute(Sender: TObject);
    procedure trContextCollapsing(Sender: TObject; Node: TTreeNTNode;
      var AllowCollapse: Boolean);
    procedure acHelpExecute(Sender: TObject);
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
  sProgramVersionStr = 'V 1';

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
    n: TTreeNTNode;
    s: string;
begin
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

procedure TfmPiMain.trContextCollapsing(Sender: TObject; Node: TTreeNTNode;
  var AllowCollapse: Boolean);
begin
  AllowCollapse:= false;
end;

procedure TfmPiMain.acHelpExecute(Sender: TObject);
var
  ls: TStringList;
  i: integer;
  ol: integer;
begin
  ls:=TStringList.Create;
  try
    try
      ls.LoadFromFile(ExtractFilePath(ParamStr(0))+'readme.md');
    except
      MessageDlg('Could not load "readme.md". Is it in the application''s folder?', mtError, [mbOK], 0);
    end;
    reOutput.Lines.Add(sLineBreak+sLineBreak+sLineBreak);
    ol:= reOutput.SelStart;
    for i:= 0 to ls.Count-1 do begin
      reOutput.Lines.Add(Utf8ToAnsi(ls[i]));
    end;
    reOutput.Lines.Add(sLineBreak+sLineBreak+sLineBreak);
    reOutput.SelStart:= ol;
    PostMessage(reOutput.Handle, EM_SCROLLCARET, 0, 0);
  finally
    ls.Free;
  end;
end;

end.
