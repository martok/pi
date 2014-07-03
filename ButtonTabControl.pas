unit ButtonTabControl;
{
 based on http://stackoverflow.com/questions/2201850/how-to-implement-a-close-button-for-a-ttabsheet-of-a-tpagecontrol
}

interface

uses
  Windows, Classes, Controls, Graphics, ComCtrls, Messages, Types;

type
  TTabButton = (tbNone, tbMinimize, tbRestore, tbMaximize, tbClose);
  TTabButtons = set of TTabButton;

  TTabButtonEvent = procedure(Sender: TObject; Button: TTabButton) of object;

  TButtonTabSheet = class(TTabSheet)
  private
    BTNSIZE: integer;  
    BTNSPACING: integer;
    FCaption: TCaption;
    FShowPushed: Boolean;
    FPushedBtn,
    FMouseoverBtn: TTabButton;
    FButtonRect: array[TTabButton] of TRect;     
    FButtons: TTabButtons;
    FButtonCount: integer;
    FOnTabButtonClick: TTabButtonEvent;
    procedure SetCaption(const Value: TCaption);
    procedure SetButtons(const Value: TTabButtons);
  protected
    procedure UpdateValues;
    procedure DrawButton(const Canvas: TCanvas; Rect: TRect; const Button: TTabButton; const Pushed, Hot: Boolean);

    procedure DoTabButton(Button: TTabButton);

    procedure WMSettingChange(var Message: TWMSettingChange); message WM_SETTINGCHANGE;

    procedure DrawTab(const Canvas: TCanvas; const Rect: TRect; const TabCaption: TPoint; Active: boolean); virtual;
    procedure TabMouseDown(Button: TMouseButton; Shift: TShiftState; X: Integer; Y: Integer); virtual;
    procedure TabMouseUp(Button: TMouseButton; Shift: TShiftState; X: Integer; Y: Integer); virtual;
    procedure TabMouseMove(Shift: TShiftState; X: Integer; Y: Integer); virtual;
    procedure TabMouseLeave; virtual;
  public
    constructor Create(AOwner: TComponent); override;
    property OnTabButtonClick: TTabButtonEvent read FOnTabButtonClick write FOnTabButtonClick;
    function ComputedSpacing: String;
  published
    property Caption: TCaption read FCaption write SetCaption;
    property Buttons: TTabButtons read FButtons write SetButtons;
  end;

  TDrawPageControl = class(TPageControl)
  private
  protected
    procedure DrawTab(TabIndex: Integer; const Rect: TRect; Active: Boolean); override;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X: Integer; Y: Integer); override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState; X: Integer; Y: Integer); override;
    procedure MouseMove(Shift: TShiftState; X: Integer; Y: Integer); override;
    procedure CMMouseleave(var Message: TMessage); message CM_MOUSELEAVE;
  public
    constructor Create(AOwner: TComponent); override;

  end;

implementation      

uses
  Themes, StrUtils;

const
  ButtonDrawStates : array [TTabButton] of Cardinal = (
     0,                  
     DFCS_CAPTIONMIN,
     DFCS_CAPTIONRESTORE,
     DFCS_CAPTIONMAX,
     DFCS_CAPTIONCLOSE
  );   
  ButtonThemeStates : array [TTabButton] of TThemedWindow = (
     twWindowDontCare,  
     twMinButtonNormal,
     twRestoreButtonNormal,
     twMaxButtonNormal,
     twCloseButtonNormal
  );

{ TDrawPageControl }

constructor TDrawPageControl.Create(AOwner: TComponent);
begin
  inherited;
  OwnerDraw:= true;
end;

procedure TDrawPageControl.DrawTab(TabIndex: Integer; const Rect: TRect; Active: Boolean);
var
  TabSheet:TButtonTabSheet;
  TabCaption: TPoint;
begin
  TabCaption.Y := Rect.Top + GetSystemMetrics(SM_CYDLGFRAME);

  if Active then begin
    TabCaption.X := Rect.Left + 2 * GetSystemMetrics(SM_CXDLGFRAME) + GetSystemMetrics(SM_CXBORDER);
  end else begin
    TabCaption.X := Rect.Left + GetSystemMetrics(SM_CXDLGFRAME);
  end;

  if Pages[TabIndex] is TButtonTabSheet then begin
    TabSheet:= Pages[TabIndex] as TButtonTabSheet;

    TabSheet.DrawTab(Canvas, Rect, TabCaption, Active);
  end else begin
    Canvas.FillRect(Rect);
    Canvas.TextOut(TabCaption.X, TabCaption.Y, Pages[TabIndex].Caption);
  end;
end;

procedure TDrawPageControl.MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var
  I: Integer;
begin
  inherited;

  I:= IndexOfTabAt(X, Y);

  if (I >= 0) and (Pages[I] is TButtonTabSheet) then begin
    TButtonTabSheet(Pages[I]).TabMouseDown(Button, Shift, X, Y);
    Repaint;
  end;
end;

procedure TDrawPageControl.MouseMove(Shift: TShiftState; X, Y: Integer);
var
  I, J: Integer;
begin
  inherited;

  I:= IndexOfTabAt(X, Y);

  for J:= 0 to PageCount-1 do
    if (I<>J) and (Pages[J] is TButtonTabSheet) then
      TButtonTabSheet(Pages[J]).TabMouseLeave;

  if (I >= 0) and (Pages[I] is TButtonTabSheet) then begin
    TButtonTabSheet(Pages[I]).TabMouseMove(Shift, X, Y);
    Repaint;
  end;
end;

procedure TDrawPageControl.MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var
  I: Integer;
begin
  inherited;

  I:= IndexOfTabAt(X, Y);

  if (I >= 0) and (Pages[I] is TButtonTabSheet) then begin
    TButtonTabSheet(Pages[I]).TabMouseUp(Button, Shift, X, Y);  
    Repaint;
  end;
end;

procedure TDrawPageControl.CMMouseleave(var Message: TMessage); 
var
  I: Integer;
begin
  for i:= 0 to PageCount-1 do 
    if Pages[i] is TButtonTabSheet then
      TButtonTabSheet(Pages[i]).TabMouseLeave;
  Repaint;
end;

{ TButtonTabSheet }  

constructor TButtonTabSheet.Create(AOwner: TComponent);
begin
  inherited;
  FPushedBtn:= tbNone;
  FMouseoverBtn:= tbNone;
  FShowPushed:= false;
  UpdateValues;
  Buttons:= [tbClose];
end;

function TButtonTabSheet.ComputedSpacing: String;
var
  ts: integer;
begin
  ts:= (BTNSIZE * FButtonCount) + BTNSPACING * (FButtonCount-1) + BTNSIZE div 2;
  if Assigned(PageControl) then begin
    PageControl.Canvas.Font:= PageControl.Font;
    Result:= DupeString(' ', ts div PageControl.Canvas.TextWidth(' ') + 1);
  end;
end;

procedure TButtonTabSheet.SetCaption(const Value: TCaption);
begin
  if FCaption = Value then Exit;
  FCaption:= Value;
  inherited Caption:= Value + ComputedSpacing;
end;

procedure TButtonTabSheet.SetButtons(const Value: TTabButtons);
var
  i: integer;
  b: TTabButton;
begin
  if FButtons = Value then Exit;
  FButtons := Value;
  i:= 0;
  for b:= Succ(Low(TTabButton)) to high(TTabButton) do
    if b in Value then
      inc(i);
  FButtonCount:= i;
  inherited Caption:= FCaption + ComputedSpacing;
  if Assigned(PageControl) then
    PageControl.Repaint;
end;
        
procedure TButtonTabSheet.DoTabButton(Button: TTabButton);
begin
  if Assigned(FOnTabButtonClick) then
    FOnTabButtonClick(Self, Button);
end;

procedure TButtonTabSheet.UpdateValues;
begin
  BTNSIZE := GetSystemMetrics(SM_CYSMICON) - 2 * GetSystemMetrics(SM_CYDLGFRAME);
  BTNSPACING := GetSystemMetrics(SM_CXBORDER);
  inherited Caption:= FCaption + ComputedSpacing;
end;

procedure TButtonTabSheet.WMSettingChange(var Message: TWMSettingChange);
begin
  UpdateValues;
end;

procedure TButtonTabSheet.DrawButton(const Canvas: TCanvas; Rect: TRect; const Button: TTabButton; const Pushed, Hot: Boolean);
var
  BtnDrawState: Cardinal;
  BtnDrawDetails: TThemedElementDetails;
  S: Byte;
begin
  if not ThemeServices.ThemesEnabled then begin
    BtnDrawState:= ButtonDrawStates[Button];
    if Pushed then
      BtnDrawState := BtnDrawState or DFCS_PUSHED
    else
    if Hot then
      BtnDrawState := BtnDrawState or DFCS_HOT;

    Windows.DrawFrameControl(Canvas.Handle, Rect, DFC_CAPTION, BtnDrawState);
  end else begin
    Dec(Rect.Left);

    if Pushed then
      S:= 2
    else if Hot then
      S:= 1
    else
      S:= 0;
    BtnDrawDetails:= ThemeServices.GetElementDetails(TThemedWindow(Ord(ButtonThemeStates[Button]) + S));

    ThemeServices.DrawElement(Canvas.Handle, BtnDrawDetails, Rect);
  end;
end;

procedure TButtonTabSheet.DrawTab(const Canvas: TCanvas; const Rect: TRect; const TabCaption: TPoint; Active: boolean);
var
  BtnRect: TRect;
  B: TTabButton;
begin
  BtnRect.Top := Rect.Top + GetSystemMetrics(SM_CYDLGFRAME);
  BtnRect.Right := Rect.Right - GetSystemMetrics(SM_CXDLGFRAME);

  BtnRect.Bottom := BtnRect.Top + BTNSIZE;

  Canvas.FillRect(Rect);
  Canvas.TextOut(TabCaption.X, TabCaption.Y, Caption);
  for B:= high(TTabButton) downto Succ(Low(TTabButton)) do begin
    if B in FButtons then begin
      BtnRect.Left := BtnRect.Right - BTNSIZE;
      FButtonRect[B]:= BtnRect;                 

      DrawButton(Canvas, BtnRect, B, (FPushedBtn=B) and FShowPushed, FMouseoverBtn = B);

      BtnRect.Right:= BtnRect.Left - BTNSPACING - 1;
    end else
      FButtonRect[B]:= Types.Rect(0,0,0,0);
  end;
end;

procedure TButtonTabSheet.TabMouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var
  B: TTabButton;
begin
  if Button = mbLeft then begin     
    for B:= high(TTabButton) downto Succ(Low(TTabButton)) do begin
      if (B in FButtons) and PtInRect(FButtonRect[B], Point(X, Y)) then begin
        FPushedBtn:= B;
        FShowPushed:= true;
        exit;
      end;
    end;
  end;
end;

procedure TButtonTabSheet.TabMouseMove(Shift: TShiftState; X, Y: Integer);   
var
  B: TTabButton;
begin
  if (ssLeft in Shift) and (FPushedBtn <> tbNone) then begin
    FShowPushed := PtInRect(FButtonRect[FPushedBtn], Point(X, Y));
  end;

  for B:= high(TTabButton) downto Succ(Low(TTabButton)) do begin
    if (B in FButtons) and PtInRect(FButtonRect[B], Point(X, Y)) then begin
      FMouseoverBtn:= B;
      break;
    end;
  end;
end;

procedure TButtonTabSheet.TabMouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  if (Button = mbLeft) and (FPushedBtn <> tbNone) then begin
    if PtInRect(FButtonRect[FPushedBtn], Point(X, Y)) then begin
      DoTabButton(FPushedBtn);
    end;
    FPushedBtn:= tbNone;
  end;
end;  

procedure TButtonTabSheet.TabMouseLeave;
begin
  FShowPushed:= false;
  FMouseoverBtn:= tbNone;
end;

end.
 