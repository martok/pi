unit uDockableForms;

interface

uses
  Windows, Controls, Forms, Messages;

type
  IDockableForm = interface
    ['{7799D9C9-BBEE-4321-8A69-690E40606BAF}']
    procedure Dock(Parent: TWinControl);
    procedure Undock;
    function GetCaption: string;
  end;

  TDockableForm = class(TForm, IDockableForm)
  private
  protected
    procedure WMSysCommand(var Message: TWMSysCommand); message WM_SYSCOMMAND;

  public
    function GetCaption: String;
    procedure FormDock(Parent: TWinControl);
    procedure IDockableForm.Dock = FormDock;
    procedure Undock;

    procedure ShowDockable;
  end;


implementation

uses
  uMain;

{ TDockableForm }

function TDockableForm.GetCaption: String;
begin
  Result:= Self.Caption;
end;

procedure TDockableForm.FormDock(Parent: TWinControl);
var
  v: Boolean;
begin
  v:= Visible;
  Hide;
  Align:= alClient;
  BorderStyle:= bsNone;
  Self.Parent:= Parent;
  Visible:= v;
end;

procedure TDockableForm.Undock;
var
  v: Boolean;
begin
  v:= Visible;
  Hide;
  Align:= alNone;
  BorderStyle:= bsSizeable;
  Self.Parent:= nil;
  Visible:= v;
end;

procedure TDockableForm.ShowDockable;
begin
  fmPiMain.InsertIntoTab(Self as IDockableForm);
  Show;
end;

procedure TDockableForm.WMSysCommand(var Message: TWMSysCommand);
begin
  if (Message.CmdType and $FFF0) = SC_MINIMIZE then begin
    fmPiMain.InsertIntoTab(Self as IDockableForm);
  end else
    inherited;
end;

end.
