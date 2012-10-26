unit uGraphWindow;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms,
  Dialogs, uFunctionsGraphing, uMath, Menus;

type
  TScaleMode = (smLin, smLog);
  TGraphWindow = class(TForm)
    pmGraph: TPopupMenu;
    miLegend: TMenuItem;
    miLegendNone: TMenuItem;
    N1: TMenuItem;
    miLegendTop: TMenuItem;
    miLegendBottom: TMenuItem;
    miCopyEMF: TMenuItem;
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure FormPaint(Sender: TObject);
    procedure FormResize(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure miLegendChange(Sender: TObject);
    procedure miCopyEMFClick(Sender: TObject);
  private
    { Private-Deklarationen }
    FPlots: array of IValueObject;
    FXMin, FXMax, FYMin, FYMax: Number;
    FXScale, FYScale: TScaleMode;
    Buffer: TBitmap;
    procedure PaintGraph(Canvas: TCanvas; DrawRect: TRect);
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

uses Types, Math, Clipbrd;

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
var
  sp: Number;
begin
  sp:= FMaxVal - FMinVal;
  if IsZero(sp) then
    sp:= 1;
  Fact:= (FFrameMax - FFrameMin) / sp;
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
  Buffer:= TBitmap.Create;
end;

destructor TGraphWindow.Destroy;
var
  i: integer;
begin
  Buffer.Free;
  for i:= 0 to high(FPlots) do
    FPlots[i]:= nil;
  inherited;
end;

procedure TGraphWindow.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  Action:= caFree;
end;

procedure TGraphWindow.FormPaint(Sender: TObject);
begin
  Canvas.Draw(0,0,Buffer);
end;

procedure TGraphWindow.FormResize(Sender: TObject);
begin
  Buffer.Width:= ClientWidth;
  Buffer.Height:= ClientHeight;
  PaintGraph(Buffer.Canvas, ClientRect);
  Refresh;
end;

procedure TGraphWindow.PaintGraph(Canvas: TCanvas; DrawRect: TRect);
var
  box: TRect;
  ax, ay: TScale;
  xaxis, yaxis, a, aprev, n, nprev: Number;
  gap: boolean;
  clipr: HRGN;

  function AxisLabel(p: Number): string;
  begin
    Result:= FloatToStrF(p, ffGeneral, 5, 0, NeutralFormatSettings);
  end;

  procedure Axes;
  var
    l: string;
    sp: Number;
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
    sp:= FYMax - FYMin;
    if IsZero(sp) then
      sp:= 1;
    a:= FYMin;
    while a <= FYMax do begin
      if not IsZero(a) then begin
        Canvas.MoveTo(ax.Scale(yaxis) - 4, ay.Scale(a));
        Canvas.LineTo(ax.Scale(yaxis) + 4, ay.Scale(a));
        l:= AxisLabel(a);
        ts:= Canvas.TextExtent(l);
        Canvas.TextOut(ax.Scale(yaxis) - ts.cx - 4, ay.Scale(a) - ts.cy div 2, l);
      end;
      a:= a + Power(10, Round(Log10(sp / 10)));
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

  procedure DrawFunction_Plot(ob: TPlot);
  var
    x: Integer;
    a: Number;
  begin
    Canvas.Pen.Width:= trunc(ob.Size);
    Canvas.Pen.Color:= ob.Color;
    gap:= true;
    nprev:= 0;
    aprev:= 0;
    for x:= box.Left to box.Right - 1 do begin
      a:= ax.Inverse(x);
      n:= ob.ValueAt(a);
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
  end;

  procedure DrawFunction_Histogram(ob: THistogram);
  var
    j: Integer;
    tup: IValueList;
    x,y: Number;
  begin
    for j:= 0 to ob.List.Length - 1 do begin
      tup:= ob.List.ListItem[j] as IValueList;
      x:= tup.ListItem[0].GetNumber;
      if IsNan(x) then continue;
      if ((ax is TLogScale) and ((x <= 0) or IsZero(x))) then
        continue;
      y:= tup.ListItem[1].GetNumber;
      if IsNan(y) then continue;
      if ((ay is TLogScale) and ((y <= 0) or IsZero(y))) then
        continue;
      Canvas.Brush.Color:= ob.Color;
      Canvas.Pen.Width:= 1;
      Canvas.Pen.Color:= clBlack;
      Canvas.Rectangle(Rect(
        ax.Scale(x - ob.Size / 2),
        ay.Scale(y),
        ax.Scale(x + ob.Size / 2),
        ay.Scale(0)));
    end;
  end;

  procedure DrawFunction_XYPlot(ob: TXYPlot);
  var
    j: Integer;
    x,y,px,py: Number;
    xx,yy: integer;

    function GetXY(indx: integer): boolean;
    var
      tup: IValueList;
    begin
      Result:= false;
      tup:= ob.List.ListItem[indx] as IValueList;
      x:= tup.ListItem[0].GetNumber;
      if IsNan(x) then exit;
      if ((ax is TLogScale) and ((x <= 0) or IsZero(x))) then
        exit;
      y:= tup.ListItem[1].GetNumber;
      if IsNan(y) then exit;
      if ((ay is TLogScale) and ((y <= 0) or IsZero(y))) then
        exit;
      Result:= true;
    end;
  begin
    px:= NaN;
    py:= NaN;
    if ob.Lines<>lsNone then
      for j:= 0 to ob.List.Length - 1 do begin
        if not GetXY(j) then continue;

        xx:= ax.Scale(x);
        yy:= ay.Scale(y);

        if IsNan(px) then
          canvas.MoveTo(xx,yy)
        else begin
          Canvas.MoveTo(ax.Scale(px),ay.Scale(py));
          case ob.Lines of
            lsStraight: begin
              Canvas.Pen.Color:= ob.Color;
              Canvas.LineTo(xx,yy);
            end;
            lsHoldX: begin
              Canvas.Pen.Color:= ob.Color;
              Canvas.LineTo(ax.Scale(px),yy);
              Canvas.LineTo(xx,yy);
            end;
            lsHoldY: begin
              Canvas.Pen.Color:= ob.Color;
              Canvas.LineTo(xx,ay.Scale(py));
              Canvas.LineTo(xx,yy);
            end;
            lsStepX: begin
              Canvas.Pen.Color:= ob.Color;
              Canvas.LineTo(ax.Scale(px),yy);
              Canvas.MoveTo(xx,yy);
            end;
            lsStepY: begin
              Canvas.Pen.Color:= ob.Color;
              Canvas.LineTo(xx,ay.Scale(py));
              Canvas.MoveTo(xx,yy);
            end;
          end;
        end;
        px:= x;
        py:= y;
      end;
    if ob.Points<>psNone then
      for j:= 0 to ob.List.Length - 1 do begin
        if not GetXY(j) then continue;

        xx:= ax.Scale(x);
        yy:= ay.Scale(y);

        case ob.Points of
          psDot: Canvas.Pixels[xx,yy]:= ob.Color;
          psCross: begin
            Canvas.Pen.Color:= ob.Color;
            Canvas.Pen.Style:= psSolid;
            Canvas.MoveTo(xx-trunc(ob.Size),yy-Trunc(ob.Size));
            Canvas.LineTo(xx+trunc(ob.Size+1),yy+Trunc(ob.Size+1));
            Canvas.MoveTo(xx+trunc(ob.Size),yy-Trunc(ob.Size));
            Canvas.LineTo(xx-trunc(ob.Size+1),yy+Trunc(ob.Size+1));
          end;
          psPlus: begin
            Canvas.Pen.Color:= ob.Color;
            Canvas.Pen.Style:= psSolid;
            Canvas.MoveTo(xx-trunc(ob.Size),yy);
            Canvas.LineTo(xx+trunc(ob.Size+1),yy);
            Canvas.MoveTo(xx,yy-Trunc(ob.Size));
            Canvas.LineTo(xx,yy+Trunc(ob.Size+1));
          end;
          psCircle: begin
            Canvas.Brush.Color:= ob.Color;
            Canvas.Pen.Color:= clBlack;
            Canvas.Pen.Style:= psSolid;
            Canvas.Ellipse(xx-trunc(ob.Size),yy-Trunc(ob.Size),
                           xx+trunc(ob.Size+1),yy+Trunc(ob.Size+1));
          end;
          psSquare: begin
            Canvas.Brush.Color:= ob.Color;
            Canvas.Pen.Color:= clBlack;
            Canvas.Pen.Style:= psSolid;
            Canvas.Rectangle(xx-trunc(ob.Size),yy-Trunc(ob.Size),
                           xx+trunc(ob.Size+1),yy+Trunc(ob.Size+1));
          end;
        end;
      end;
  end;

  procedure Graphs;
  var
    i: integer;
    ob: TPlotBase;
  begin
    clipr:= CreateRectRgn(box.Left + 1, box.Top + 1, box.Right - 1, box.Bottom - 1);
    SelectClipRgn(Canvas.Handle, clipr);
    try
      for i:= 0 to high(FPlots) do begin
        ob:= TPlotBase(FPlots[i].GetObject);
        if ob is TPlot then
          DrawFunction_Plot(TPlot(ob))
        else if ob is THistogram then
          DrawFunction_Histogram(THistogram(ob))
        else if ob is TXYPlot then
          DrawFunction_XYPlot(TXYPlot(ob));
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
    ob: TPlotBase;
  begin
    leg:= box;
    InflateRect(leg, -20, -20);
    if miLegendTop.Checked then
      leg.Bottom:= leg.Top+round(length(FPlots)*Canvas.TextHeight('Ij')*1.5)
    else
    if miLegendBottom.Checked then
      leg.Top:= leg.Bottom-round(length(FPlots)*Canvas.TextHeight('Ij')*1.5);

    Canvas.Pen.Color:= clBlack;
    Canvas.Pen.Width:= 1;
    Canvas.Brush.Style:= bsSolid;
    Canvas.Brush.Color:= clGray;
    Canvas.FillRect(Rect(leg.Left+5, leg.top+5, leg.Right+5, leg.Bottom+5));
    Canvas.Brush.Color:= clWhite;
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
        s:= TPlot(ob).GetLegend;
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
  box:= DrawRect;
  Canvas.FillRect(box);
  InflateRect(box, -10, -10);
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
    if not miLegendNone.Checked then
      Legend;
  finally
    FreeAndNil(ax);
    FreeAndNil(ay);
  end;
end;

procedure TGraphWindow.miLegendChange(Sender: TObject);
begin
  PaintGraph(Buffer.Canvas, ClientRect);
  Refresh;
end;

procedure TGraphWindow.miCopyEMFClick(Sender: TObject);
var
  emf: TMetafile;
  emfc: TMetafileCanvas;
begin
  emf:= TMetafile.Create;
  try
    emf.Enhanced:= true;
    emf.Width:= ClientWidth;
    emf.Height:= ClientHeight;
    emfc:= TMetafileCanvas.Create(emf, Canvas.Handle);
    try
      PaintGraph(emfc, ClientRect);
    finally
      emfc.Free;
    end;
    Clipboard.Assign(emf);
  finally
    emf.Free;
  end;
end;

end.

