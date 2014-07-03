unit uFrmInput;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms, 
  Dialogs, ExtCtrls, StdCtrls, Menus, Buttons;

type
  TfrmInput = class(TFrame)
    meInput: TMemo;
    Panel1: TPanel;
    pmHistory: TPopupMenu;
    miHistLast: TMenuItem;
    sbHistory: TSpeedButton;
    miClearHistory: TMenuItem;
    procedure meInputChange(Sender: TObject);
    procedure meInputKeyPress(Sender: TObject; var Key: Char);
    procedure pmHistoryPopup(Sender: TObject);
    procedure miHistoryClick(Sender: TObject);
    procedure sbHistoryClick(Sender: TObject);
    procedure miClearHistoryClick(Sender: TObject);
    procedure miHistMeasureItem(Sender: TObject; ACanvas: TCanvas;
      var Width, Height: Integer);
    procedure miHistDrawItem(Sender: TObject; ACanvas: TCanvas;
      ARect: TRect; Selected: Boolean);
    procedure meInputKeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
  private
    FHistory: TStringList;
    FOnRunCommand: TNotifyEvent;
    FOnSizeForced: TNotifyEvent;
    function GetText: string;
    procedure SetText(const Value: string);
  protected
    procedure CreateWnd; override;
    procedure AdjustHeight(const Lines: integer);
    procedure DoRunCommand;            
    procedure DoSizeForced;
    { Private-Deklarationen }
  public
    { Public-Deklarationen }
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Clear;
    property Text: string read GetText write SetText;
    procedure SelectAll;
    procedure SetFocus; override;
    procedure AddHistory(const Expr: string);

    property OnRunCommand: TNotifyEvent read FOnRunCommand write FOnRunCommand;
    property OnSizeForced: TNotifyEvent read FOnSizeForced write FOnSizeForced;
  end;

implementation

uses
  uMath, Types, Math;

{$R *.dfm}

{ TfrmInput }

procedure TfrmInput.AddHistory(const Expr: string);
var
  i: integer;
begin
  i:= FHistory.IndexOf(Expr);
  if i>=0 then
    FHistory.Delete(i);
  FHistory.Insert(0, Expr);
end;

procedure TfrmInput.AdjustHeight(const Lines: integer);
var
  tm: TTextMetric;
begin                                                   
  GetTextMetrics(GetDC(Handle), tm);
  Height:= tm.tmHeight * Lines + GetSystemMetrics(SM_CYBORDER) * 8;
  DoSizeForced;
end;

procedure TfrmInput.Clear;
begin
  SetText('');
end;

constructor TfrmInput.Create(AOwner: TComponent);
begin
  inherited;
  FHistory:= TStringList.Create;
end;

procedure TfrmInput.CreateWnd;
begin
  inherited;
  AdjustHeight(1);
  if Assigned(sbHistory) then begin
    sbHistory.Font.Name:= 'Marlett';
    sbHistory.Font.Charset:= SYMBOL_CHARSET;
    sbHistory.BoundsRect:= Rect(0, Panel1.ClientHeight-sbHistory.Height, Panel1.ClientWidth, Panel1.ClientHeight)
  end;
end;

destructor TfrmInput.Destroy;
begin
  FreeAndNil(FHistory);
  inherited;
end;

function TfrmInput.GetText: string;
begin
  Result:= meInput.Text;
end;

procedure TfrmInput.SelectAll;
begin
  if CanFocus then
    SetFocus;
  meInput.SelectAll;
end;

procedure TfrmInput.SetFocus;
begin
  inherited;
  meInput.SetFocus;
end;

procedure TfrmInput.SetText(const Value: string);
begin
  meInput.Text:= Value;
end;

procedure TfrmInput.meInputChange(Sender: TObject);
var
  ss,sl,ln: integer;
begin
  ln:= 1 + (Length(meInput.Text) - Length(StringReplace(meInput.Text, sLineBreak, '', [rfReplaceAll]))) div Length(sLineBreak);
  ss:= meInput.SelStart;
  sl:= meInput.SelLength;
  if ln < 1 then ln:= 1;
  if ln > 5 then begin
    ln:= 5;
    meInput.ScrollBars:= ssVertical;
  end else    
    meInput.ScrollBars:= ssNone;

  AdjustHeight(ln);
  meInput.SelStart:= ss;
  meInput.SelLength:=sl;
end;

procedure TfrmInput.meInputKeyPress(Sender: TObject; var Key: Char);
begin
  case Key of
    Chr(VK_RETURN): begin
      if GetKeyState(VK_SHIFT) >= 0 then begin
        Key:= #0;
        DoRunCommand;
      end;
    end;
    ',': begin
      // has the numpad comma been the reason?
      // better place would be KeyDown, but we can't change anything from there.
      if GetKeyState(VK_DECIMAL) < 0 then
        Key:= NeutralFormatSettings.DecimalSeparator;
    end;
  end;
end;

procedure TfrmInput.DoRunCommand;
begin
  if Assigned(FOnRunCommand) then
    FOnRunCommand(Self);
end;
       
procedure TfrmInput.DoSizeForced;
begin
  if Assigned(FOnSizeForced) then
    FOnSizeForced(Self);
end;

procedure TfrmInput.pmHistoryPopup(Sender: TObject);
var
  i: integer;
  mi: TMenuItem;
begin
  for i:= pmHistory.Items.IndexOf(miHistLast)-1 downto 0 do begin
    mi:= pmHistory.Items[i];
    pmHistory.Items.Delete(i);
    FreeAndNil(mi);
  end;

  for i:= FHistory.Count-1 downto 0 do begin
    mi:= NewItem(FHistory[i], 0, false, true, miHistoryClick, 0, '');
    mi.OnDrawItem:= miHistDrawItem;                                  
    mi.OnMeasureItem:= miHistMeasureItem;
    pmHistory.Items.Insert(0, mi);
    if i > 0 then
      pmHistory.Items.Insert(0, NewLine);
  end;
end;

procedure TfrmInput.miHistoryClick(Sender: TObject);
var
  mi: TMenuItem;
begin
  mi:= Sender as TMenuItem;
  SetText(mi.Caption);
end;

procedure TfrmInput.sbHistoryClick(Sender: TObject);
var
  p: TPoint;
begin
  p:= meInput.ClientToScreen(Point(0, meInput.Height));
  pmHistory.Popup(p.x,p.y);
end;

procedure TfrmInput.miClearHistoryClick(Sender: TObject);
begin
  FHistory.Clear;
end;


procedure TfrmInput.miHistMeasureItem(Sender: TObject; ACanvas: TCanvas; var Width, Height: Integer);
var
  mi: TMenuItem;
  r: TRect;
  s: string;
begin
  mi:= sender as TMenuItem;
  s:= mi.Caption;
  ACanvas.Font:= meInput.Font;
  DrawTextEx(ACanvas.Handle, PChar(s), Length(s), r, DT_CALCRECT, nil);
  InflateRect(r, 4, 4);
  Width:= Max(r.Right - r.Left, meInput.Width);
  Height:= r.Bottom - r.Top;
end;

procedure TfrmInput.miHistDrawItem(Sender: TObject; ACanvas: TCanvas; ARect: TRect; Selected: Boolean);
var
  mi: TMenuItem;
  s: string;
begin
  mi:= sender as TMenuItem;
  s:= mi.Caption;     
  ACanvas.Font:= meInput.Font;
  ACanvas.FillRect(ARect);
  if Selected then
    ACanvas.Font.Color:= clHighlightText;
  InflateRect(ARect, -4, -4);
  DrawTextEx(ACanvas.Handle, PChar(s), Length(s), ARect, DT_LEFT or DT_HIDEPREFIX, nil);
end;

procedure TfrmInput.meInputKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
begin
  if (Key=VK_DOWN) and (Shift=[ssALT]) then begin
    sbHistory.Click;
  end;
end;

end.
