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
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure cbInputKeyPress(Sender: TObject; var Key: Char);
    procedure acRunCmdExecute(Sender: TObject);
    procedure acExitExecute(Sender: TObject);
    procedure acRunTestExecute(Sender: TObject);
    procedure trContextCollapsing(Sender: TObject; Node: TTreeNTNode;
      var AllowCollapse: Boolean);
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

{$R *.dfm}

procedure TfmPiMain.FormCreate(Sender: TObject);
begin
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
begin
  trContext.Items.Clear;
  ct:= MathS.Context;
  while ct<>nil do begin
    n:= trContext.Items.AddObject(nil, format('%s (%d)',[ct.ContextName,ct.Count]), ct);
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
  if Key=#13 then begin
    Key:= #0;
    acRunCmd.Execute;
  end;
end;

procedure TfmPiMain.acRunCmdExecute(Sender: TObject);
var f: string;
begin
  try
    f:= cbInput.Text;
    MathS.Output.Input(f);
    MathS.Run(f);
    cbInput.Text:= '';
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

end.
