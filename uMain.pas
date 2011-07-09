unit uMain;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, uMath, StdCtrls, ExtCtrls, ActnList, ToolWin, ComCtrls;

type
  TForm1 = class(TForm)
    Memo1: TMemo;
    Panel1: TPanel;
    Panel2: TPanel;
    lbContext: TListBox;
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
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure cbInputKeyPress(Sender: TObject; var Key: Char);
    procedure acRunCmdExecute(Sender: TObject);
    procedure acExitExecute(Sender: TObject);
    procedure acRunTestExecute(Sender: TObject);
  private
    { Private-Deklarationen }
    procedure UpdateContext;
  public
    { Public-Deklarationen }
    MathS: TMathSystem;
  end;

var
  Form1: TForm1;

implementation

uses uTests;

{$R *.dfm}

procedure TForm1.FormCreate(Sender: TObject);
begin
  MathS:= TMathSystem.Create;
  MathS.Output.Memo:= Memo1;
  Memo1.Clear;
  cbInput.Clear;
end;

procedure TForm1.FormDestroy(Sender: TObject);
begin
  FreeAndNil(MathS);
end;

procedure TForm1.UpdateContext;
var ct:TContext;
    i, j: integer;
begin
  lbContext.Clear;
  ct:= MathS.Context;
  i:= 1;
  while ct<>nil do begin
    lbContext.Items.Add('Context '+IntToStr(I));
    for j:= 0 to ct.Count-1 do
      lbContext.Items.Add('  '+ct.Name[j]+' = '+ct.Definition(ct.Name[j]).StringForm);
    ct:= ct.Parent;
    inc(i);
  end;
end;

procedure TForm1.FormShow(Sender: TObject);
begin
  UpdateContext;
  cbInput.SetFocus;
end;

procedure TForm1.cbInputKeyPress(Sender: TObject; var Key: Char);
begin
  if Key=#13 then begin
    Key:= #0;
    acRunCmd.Execute;
  end;
end;

procedure TForm1.acRunCmdExecute(Sender: TObject);
var f: string;
begin
  try
    f:= cbInput.Text;
    MathS.Output.LineOut('> '+f);
    MathS.Run(f);
    cbInput.Text:= '';
    if cbInput.Items.IndexOf(f)>=0 then
      cbInput.Items.Delete(cbInput.Items.IndexOf(f));
    cbInput.Items.Insert(0,f);
  except
    on e: Exception do
      MathS.Output.LineOut(e.ClassName+': '+e.Message);
  end;
  UpdateContext;
end;

procedure TForm1.acExitExecute(Sender: TObject);
begin
  Close;
end;

procedure TForm1.acRunTestExecute(Sender: TObject);
var testsys: TMathSysTest;
begin
  testsys:= TMathSysTest.Create(Memo1);
  try
    testsys.Run;
  finally
    testsys.Free;
  end;
end;

end.
