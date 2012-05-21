unit uGraphWindow;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms,
  Dialogs, uFunctionsGraphing, uMath;

type
  TScaleMode = (smLin, smLog);
  TGraphWindow = class(TForm)
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure FormPaint(Sender: TObject);
    procedure FormResize(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure FormMouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
  private
    { Private-Deklarationen }
    FShowLegend: boolean;
    FPlots: array of IValueObject;
    FXMin, FXMax, FYMin, FYMax: Number;
    FXScale, FYScale: TScaleMode;
  public
    { Public-Deklarationen }
    constructor CreateGraph(Plots: array of IValueObject);
    destructor Destroy; override;
    property XMin: Number read FXMin write FXMin;
    property XMax: Number read FXMax write FXMax;
    property YMin: Number read FYMin write FYMin;
    property YMax: Number read FYMax write FYMax;
    property XScale: TScaleMode read FXScale write FXScale;
    property YScale: TScaleMode read FYScale write FYScale;
  end;

  TScale = class
  protected
    FMinVal,
      FMaxVal: Single;
    FFrameMin,
      FFrameMax: integer;
    procedure Init; virtual;
  public
    constructor Create(AMinVal, AMaxVal: Single; AFrameMin, AFrameMax: integer);
    function Scale(Value: Number): integer; virtual; abstract;
    function Inverse(Pixel: integer): Number; virtual; abstract;
  end;

  TLinScale = class(TScale)
  private
    Fact: Double;
  protected
    procedure Init; override;
  public
    function Scale(Value: Number): integer; override;
    function Inverse(Pixel: integer): Number; override;
  end;
  TLogScale = class(TScale)
  private
    Fact: Double;
  protected
    procedure Init; override;
  public
    function Scale(Value: Number): integer; override;
    function Inverse(Pixel: integer): Number; override;
  end;

var
  GraphWindow: TGraphWindow;

implementation

uses Types, Math;

{$R *.dfm}

{ TScale }

constructor TScale.Create(AMinVal, AMaxVal: Single; AFrameMin, AFrameMax: integer);
begin
  inherited Create;
  FMinVal:= AMinVal;
  FMaxVal:= AMaxVal;
  FFrameMin:= AFrameMin;
  FFrameMax:= AFrameMax;
  Init;
end;

procedure TScale.Init;
begin
end;

{ TLinScale }

procedure TLinScale.Init;
begin
  Fact:= (FFrameMax - FFrameMin) / (FMaxVal - FMinVal);
end;

function TLinScale.Scale(Value: Number): integer;
begin
  Result:= round((Value - FMinVal) * Fact + FFrameMin);
end;

function TLinScale.Inverse(Pixel: integer): Number;
begin
  Result:= (Pixel - FFrameMin) / Fact + FMinVal;
end;

{ TLogScale }

procedure TLogScale.Init;
begin
  Fact:= (FFrameMax - FFrameMin) / (Log10(FMaxVal) - Log10(FMinVal));
end;

function TLogScale.Scale(Value: Number): integer;
begin
  if (Value < 0) or IsZero(Value) then
    Result:= MaxInt
  else
    Result:= round((Log10(Value) - Log10(FMinVal)) * Fact + FFrameMin);
end;

function TLogScale.Inverse(Pixel: integer): Number;
begin
  Result:= Power(10, (Pixel - FFrameMin) / Fact + Log10(FMinVal))
end;

{ TGraphWindow }

constructor TGraphWindow.CreateGraph(Plots: array of IValueObject);
var
  i: integer;
begin
  inherited Create(Application);
  SetLength(FPlots, Length(Plots));
  for i:= 0 to high(Plots) do
    FPlots[i]:= Plots[i];
end;

procedure TGraphWindow.FormCreate(Sender: TObject);
begin
  DoubleBuffered:= true;
  FShowLegend:= false;
end;

destructor TGraphWindow.Destroy;
var
  i: integer;
begin
  for i:= 0 to high(FPlots) do
    FPlots[i]:= nil;
  inherited;
end;

procedure TGraphWindow.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  Action:= caFree;
end;

procedure TGraphWindow.FormPaint(Sender: TObject);
var
  box: TRect;
  ax, ay: TScale;
  ob: TPlotBase;
  tup: IValueList;
  y: integer;
  xaxis, yaxis, a, aprev, n, nprev: Number;
  gap: boolean;
  ctx: TContext;
  clipr: HRGN;

  function AxisLabel(p: Number): string;
  begin
    Result:= FloatToStrF(p, ffGeneral, 5, 0, NeutralFormatSettings);
  end;

  procedure Axes;
  var
    l: string;
    ts: TSize;
  begin
    if FXMax < 0 then
      yaxis:= FXMax
    else if FXMin > 0 then
      yaxis:= FXMin
    else
      yaxis:= 0;

    if FYMax < 0 then
      xaxis:= FYMax
    else if FYMin > 0 then
      xaxis:= FYMin
    else
      xaxis:= 0;

    Canvas.Pen.Width:= 2;
    Canvas.Pen.Color:= clBlack;
    Canvas.MoveTo(ax.Scale(yaxis), box.Top);
    Canvas.LineTo(ax.Scale(yaxis), box.Bottom);
    Canvas.Brush.Style:= bsClear;
    a:= FYMin;
    while a <= FYMax do begin
      if not IsZero(a) then begin
        Canvas.MoveTo(ax.Scale(yaxis) - 4, ay.Scale(a));
        Canvas.LineTo(ax.Scale(yaxis) + 4, ay.Scale(a));
        l:= AxisLabel(a);
        ts:= Canvas.TextExtent(l);
        Canvas.TextOut(ax.Scale(yaxis) - ts.cx - 4, ay.Scale(a) - ts.cy div 2, l);
      end;
      a:= a + Power(10, Round(Log10((FYMax - FYMin) / 10)));
    end;

    Canvas.MoveTo(box.Left, ay.Scale(xaxis));
    Canvas.LineTo(box.Right, ay.Scale(xaxis));
    a:= FXMin;
    while a <= FXMax do begin
      if not IsZero(a) then begin
        Canvas.MoveTo(ax.Scale(a), ay.Scale(xaxis) - 4);
        Canvas.LineTo(ax.Scale(a), ay.Scale(xaxis) + 4);
        l:= AxisLabel(a);
        ts:= Canvas.TextExtent(l);
        Canvas.TextOut(ax.Scale(a) - ts.cx div 2, ay.Scale(xaxis) + 4, l);
      end;
      a:= a + Power(10, Round(Log10((FXMax - FXMin) / 10)));
    end;
  end;

  procedure Graphs;
  var
    i, x, j: integer;
  begin
    clipr:= CreateRectRgn(box.Left + 1, box.Top + 1, box.Right - 1, box.Bottom - 1);
    SelectClipRgn(Canvas.Handle, clipr);
    try
      for i:= 0 to high(FPlots) do begin
        ob:= TPlotBase(FPlots[i].GetObject);
        if ob is TPlot then begin
          Canvas.Pen.Width:= trunc(ob.Size);
          Canvas.Pen.Color:= ob.Color;
          ctx:= TContext.Create(ob.Context.System, ob.Context);
          try
            ctx.Silent:= true;
            gap:= true;
            nprev:= 0;
            aprev:= 0;
            for x:= box.Left to box.Right - 1 do begin
              a:= ax.Inverse(x);
              ctx.DefineValue(TPlot(ob).Variable, TValue.Create(a));
              n:= TPlot(ob).Expression.Evaluate(ctx).GetNumber;
              if ((ay is TLogScale) and ((n <= 0) or IsZero(n))) or
                (IsNan(n)) or
                (IsInfinite(n)) or
                ((abs((n - nprev + 1E-17) / (a - aprev + 1E-17)) > 1E5)) then
                gap:= true
              else begin
                if gap then
                  Canvas.MoveTo(ax.Scale(a), ay.Scale(n))
                else
                  Canvas.LineTo(ax.Scale(a), ay.Scale(n));
                gap:= false;
                nprev:= n;
                aprev:= a;
              end;
            end;
          finally
            FreeAndNil(ctx);
          end;
        end else if ob is TListPlot then begin
          for j:= 0 to TListPlot(ob).List.Length - 1 do begin
            tup:= TListPlot(ob).List.ListItem[j] as IValueList;
            n:= tup.ListItem[0].GetNumber;
            if ((ax is TLogScale) and ((n <= 0) or IsZero(n))) then
              continue;
            n:= tup.ListItem[1].GetNumber;
            if ((ay is TLogScale) and ((n <= 0) or IsZero(n))) then
              continue;
            case TListPlot(ob).Style of
              lsPoint: begin
                  Canvas.Brush.Color:= ob.Color;
                  Canvas.FillRect(Bounds(
                    ax.Scale(tup.ListItem[0].GetNumber) - ceil(ob.Size / 2),
                    ay.Scale(tup.ListItem[1].GetNumber) - ceil(ob.Size / 2),
                    trunc(ob.Size), trunc(ob.Size)));
                end;
              lsCross: begin
                  Canvas.Pen.Width:= 1;
                  Canvas.Pen.Color:= ob.Color;
                  x:= ax.Scale(tup.ListItem[0].GetNumber);
                  y:= ay.Scale(tup.ListItem[1].GetNumber);
                  Canvas.MoveTo(x, y - ceil(ob.Size));
                  Canvas.LineTo(x, y + ceil(ob.Size));
                  Canvas.MoveTo(x - ceil(ob.Size), y);
                  Canvas.LineTo(x + ceil(ob.Size), y);
                end;
              lsBar: begin
                  Canvas.Brush.Color:= ob.Color;
                  Canvas.Pen.Width:= 1;
                  Canvas.Pen.Color:= clBlack;
                  Canvas.Rectangle(Rect(
                    ax.Scale(tup.ListItem[0].GetNumber - ob.Size / 2),
                    ay.Scale(tup.ListItem[1].GetNumber),
                    ax.Scale(tup.ListItem[0].GetNumber + ob.Size / 2),
                    ay.Scale(0)));
                end;
            end;
          end;
        end;
      end;
    finally
      SelectClipRgn(Canvas.Handle, 0);
      DeleteObject(clipr);
    end;
  end;

  procedure Legend;
  var
    i, y: integer;
    leg: TRect;
    s: string;
    ts: TSize;
  begin
    leg:= box;
    InflateRect(leg, -20, -20);    
    Canvas.Pen.Color:= clBlack;
    Canvas.Pen.Width:= 1;
    Canvas.Brush.Color:= clWhite;
    Canvas.Brush.Style:= bsSolid;
    Canvas.Rectangle(leg);
    clipr:= CreateRectRgn(leg.Left + 1, leg.Top + 1, leg.Right - 1, leg.Bottom - 1);
    SelectClipRgn(Canvas.Handle, clipr);
    try
      y:= leg.Top+10;
      for i:= 0 to high(FPlots) do begin
        ob:= TPlotBase(FPlots[i].GetObject);
        Canvas.Pen.Color:= ob.Color;
        Canvas.Pen.Width:= 5;
        Canvas.MoveTo(leg.Left+5, y);
        Canvas.LineTo(leg.Left+25, y);
        if ob is TPlot then begin
          s:= TPlot(ob).Expression.StringForm;
        end else if ob is TListPlot then begin
          s:= Format('{%d Items}',[TListPlot(ob).List.Length]);
        end;
        ts:= Canvas.TextExtent(s);
        Canvas.TextOut(leg.Left+40, y - ts.cy div 2, s);
        inc(y, ts.cy + ts.cy div 2);
      end;
    finally
      SelectClipRgn(Canvas.Handle, 0);
      DeleteObject(clipr);
    end;
  end;

begin
  Canvas.Brush.Color:= clWhite;
  Canvas.FillRect(ClientRect);
  box:= Rect(10, 10, ClientWidth - 10, ClientHeight - 10);
  Canvas.Pen.Color:= clBlack;
  Canvas.Pen.Width:= 1;
  Canvas.Rectangle(box);
  if FXScale = smLog then
    ax:= TLogScale.Create(FXMin, FXMax, box.Left + 10, box.Right - 10)
  else
    ax:= TLinScale.Create(FXMin, FXMax, box.Left + 10, box.Right - 10);
  if FYScale = smLog then
    ay:= TLogScale.Create(FYMin, FYMax, box.Bottom - 10, box.Top + 10)
  else
    ay:= TLinScale.Create(FYMin, FYMax, box.Bottom - 10, box.Top + 10);
  try
    Graphs;
    Axes;
    if FShowLegend then
      Legend;
  finally
    FreeAndNil(ax);
    FreeAndNil(ay);
  end;
end;

procedure TGraphWindow.FormResize(Sender: TObject);
begin
  Refresh;
end;

procedure TGraphWindow.FormMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  FShowLegend:= true;
  Refresh;
  SetCaptureControl(Self);
end;

procedure TGraphWindow.FormMouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  ReleaseCapture;
  FShowLegend:= False;
  Refresh;
end;

end.

