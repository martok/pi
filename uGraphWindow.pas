{-----------------------------------------------------------------------------
 Package: Graphing

 Visuals
-----------------------------------------------------------------------------}
unit uGraphWindow;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms,
  Dialogs, uFunctionsGraphing, uMath, uMathIntf, uMathValues, Menus, ExtDlgs,
  uFPUSupport, uDockableForms;

type
  TScaleMode = (smLin, smLog);
  TInteractMode = (imNone, imRead, imZoom, imDrag);

  TGraphWindow = class(TDockableForm)
    pmGraph: TPopupMenu;
    miLegend: TMenuItem;
    miLegendNone: TMenuItem;
    N1: TMenuItem;
    miLegendTop: TMenuItem;
    miLegendBottom: TMenuItem;
    miCopyEMF: TMenuItem;
    N2: TMenuItem;
    miGrid: TMenuItem;
    miFineGrid: TMenuItem;
    miResetView: TMenuItem;
    N3: TMenuItem;
    miToolZoom: TMenuItem;
    miToolPan: TMenuItem;
    miToolRead: TMenuItem;
    sdGraph: TSavePictureDialog;
    miSaveEMF: TMenuItem;
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure FormPaint(Sender: TObject);
    procedure FormResize(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure miLegendChange(Sender: TObject);
    procedure miCopyEMFClick(Sender: TObject);
    procedure miGridClick(Sender: TObject);
    procedure miFineGridClick(Sender: TObject);
    procedure FormMouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure FormMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
    procedure FormMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure miResetViewClick(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure miToolZoomClick(Sender: TObject);
    procedure miToolPanClick(Sender: TObject);
    procedure miToolReadClick(Sender: TObject);
    procedure miSaveEMFClick(Sender: TObject);
  private
    { Private-Deklarationen }
    FPlots: array of IPlotObject;
    FXMin, FXMax, FYMin, FYMax: Number;
    FSXMin, FSXMax, FSYMin, FSYMax: Number;
    FXScale, FYScale: TScaleMode;
    Buffer: TBitmap;
    FBufferBox: TRect;
    FInteract: TInteractMode;
    FNextInteract: TInteractMode;
    FZoomRect: TRect;
    FYLabel: string;
    FXLabel: string;
    FTitle: string;
    procedure PaintGraph(Canvas: TCanvas; DrawRect: TRect);
  public
    { Public-Deklarationen }
    constructor CreateGraph(Plots: array of IPlotObject);
    destructor Destroy; override;
    property XMin: Number read FXMin write FXMin;
    property XMax: Number read FXMax write FXMax;
    property YMin: Number read FYMin write FYMin;
    property YMax: Number read FYMax write FYMax;
    property XScale: TScaleMode read FXScale write FXScale;
    property YScale: TScaleMode read FYScale write FYScale;
    property Title: string read FTitle write FTitle;
    property XLabel: string read FXLabel write FXLabel;
    property YLabel: string read FYLabel write FYLabel;
  end;

  TScale = class
  protected
    FMinVal,
      FMaxVal: Number;
    FFrameMin,
      FFrameMax: integer;
    procedure Init; virtual;
  public
    constructor Create(AMinVal, AMaxVal: Number; AFrameMin, AFrameMax: integer);
    class function FromMode(Mode: TScaleMode; AMinVal, AMaxVal: Number; AFrameMin, AFrameMax: integer):TScale;
    function Scale(Value: Number): integer; virtual; abstract;
    function Inverse(Pixel: integer): Number; virtual; abstract;
    function AxisLabel(p: Number): string; virtual;
  end;

  TLinScale = class(TScale)
  private
    Fact: Number;
  protected
    procedure Init; override;
  public
    function Scale(Value: Number): integer; override;
    function Inverse(Pixel: integer): Number; override;
  end;
  TLogScale = class(TScale)
  private
    Fact: Number;
  protected
    procedure Init; override;
  public
    function Scale(Value: Number): integer; override;
    function Inverse(Pixel: integer): Number; override;
  end;

var
  GraphWindow: TGraphWindow;

const
  COL_AXIS         : TColor = $404040;
  COL_GRID_MAJOR   : TColor = $808080;
  COL_GRID_MINOR   : TColor = $C0C0C0;

implementation

uses Types, Math, Clipbrd;

var
  Graphs_Counter :integer = 0;

{$R *.dfm}

procedure ArrowHead(Canvas: TCanvas; LineFrom, LineTo: TPoint; Width: integer; AngleRad: double);
var
  LineDir,len: double;
  Poly: array[0..2] of TPoint;
begin
  LineDir:= ArcTan2(LineTo.Y-LineFrom.Y, LineTo.X-LineFrom.X);
  len:= (Width/2) / Sin(AngleRad/2);
  Poly[0]:= LineTo;
  Poly[1]:= Point(round(LineTo.X - Cos(LineDir+AngleRad/2)*len), round(LineTo.Y - Sin(LineDir+AngleRad/2)*len));
  Poly[2]:= Point(round(LineTo.X - Cos(LineDir-AngleRad/2)*len), round(LineTo.Y - Sin(LineDir-AngleRad/2)*len));
  Canvas.Polygon(Poly);
end;

{ TScale }

function TScale.AxisLabel(p: Number): string;
begin
  Result:= FloatToStrF(p, ffGeneral, 9, 0, NeutralFormatSettings);
end;

constructor TScale.Create(AMinVal, AMaxVal: Number; AFrameMin, AFrameMax: integer);
begin
  inherited Create;
  FMinVal:= AMinVal;
  FMaxVal:= AMaxVal;
  FFrameMin:= AFrameMin;
  FFrameMax:= AFrameMax;
  Init;
end;

class function TScale.FromMode(Mode: TScaleMode; AMinVal, AMaxVal: Number;AFrameMin, AFrameMax: integer): TScale;
begin
  Result:= nil;
  case Mode of
    smLin: Result:= TLinScale.Create(AMinVal, AMaxVal,AFrameMin, AFrameMax);
    smLog: Result:= TLogScale.Create(AMinVal, AMaxVal,AFrameMin, AFrameMax);
  end;
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
  if fzero(sp) then
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
  if (Value < 0) or fzero(Value) then
    Result:= MaxInt
  else
    Result:= round((Log10(Value) - Log10(FMinVal)) * Fact + FFrameMin);
end;

function TLogScale.Inverse(Pixel: integer): Number;
begin
  Result:= Power(10, (Pixel - FFrameMin) / Fact + Log10(FMinVal))
end;

{ TGraphWindow }

constructor TGraphWindow.CreateGraph(Plots: array of IPlotObject);
var
  i: integer;
begin
  inherited Create(Application);
  inc(Graphs_Counter);
  Caption:= Format('Graph %d',[Graphs_Counter]);
  SetLength(FPlots, Length(Plots));
  for i:= 0 to high(Plots) do
    FPlots[i]:= Plots[i];
end;

procedure TGraphWindow.FormCreate(Sender: TObject);
begin
  DoubleBuffered:= true;
  Buffer:= TBitmap.Create;
  FBufferBox:= Rect(0,0,1,1);
  miGrid.Checked:= True;
  FSXMin:= NAN;
  FSXMax:= NAN;
  FSYMin:= NAN;
  FSYMax:= NAN;
  FInteract:= imNone;
  FNextInteract:= imZoom;
  miToolRead.Click;
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
var
  tr: TRect;
  ax,ay: TScale;
  vx,vy: Number;
  s: string;
  ts: TSize;
begin
  Canvas.Draw(0,0,Buffer);
  case FInteract of
    imRead: begin
      ax:= TScale.FromMode(FXScale, FXMin, FXMax, FBufferBox.Left + 10, FBufferBox.Right - 10);
      ay:= TScale.FromMode(FYScale, FYMin, FYMax, FBufferBox.Bottom - 10, FBufferBox.Top + 10);
      try
        //draw crosshairs
        Canvas.Pen.Style:= psDashDot;
        Canvas.Pen.Width:= 1;
        Canvas.Pen.Color:= clHighlight;
        Canvas.Brush.Style:= bsClear;
        Canvas.MoveTo(FZoomRect.Right, FBufferBox.Top);   Canvas.LineTo(FZoomRect.Right, FBufferBox.Bottom);
        Canvas.MoveTo(FBufferBox.Left, FZoomRect.Bottom); Canvas.LineTo(FBufferBox.Right, FZoomRect.Bottom);
        //format label
        vx:= ax.Inverse(FZoomRect.Right);
        vy:= ay.Inverse(FZoomRect.Bottom);
        s:= ax.AxisLabel(vx)+'  '+ay.AxisLabel(vy);
        ts:= Canvas.TextExtent(s);
        tr:= Bounds(FZoomRect.Right, FZoomRect.Bottom, ts.cx+6, ts.cy+4);
        //needs wrapping around?
        if tr.Right>FBufferBox.Right then
          OffsetRect(tr, -(tr.Right-tr.Left), 0);
        if tr.Bottom>FBufferBox.Bottom then
          OffsetRect(tr, 0, -(tr.Bottom-tr.Top));
        //draw framed text
        Canvas.Brush.Color:= clWhite;
        Canvas.Brush.Style:= bsSolid;
        Canvas.Pen.Color:= clBlack;
        Canvas.Pen.Style:= psSolid;
        Canvas.Rectangle(tr);
        Canvas.TextOut(tr.Left+3, tr.Top+2, s);
      finally
        FreeAndNil(ax);
        FreeAndNil(ay);
      end;
    end;
    imZoom: begin
      Canvas.Pen.Style:= psDashDot;
      Canvas.Pen.Width:= 1;
      Canvas.Pen.Color:= clHighlight;
      Canvas.Brush.Style:= bsClear;
      Canvas.Rectangle(FZoomRect);
    end;
    imDrag: begin
      Canvas.Pen.Style:= psDashDot;
      Canvas.Pen.Width:= 1;
      Canvas.Pen.Color:= clHighlight;
      Canvas.Brush.Style:= bsClear;
      Canvas.MoveTo(FZoomRect.Left, FZoomRect.Top);
      Canvas.LineTo(FZoomRect.Right, FZoomRect.Bottom);
      Canvas.Brush.Color:= Canvas.Pen.Color;
      Canvas.Brush.Style:= bsSolid;
      ArrowHead(Canvas,FZoomRect.TopLeft, FZoomRect.BottomRight, 8, 45*pi/180);
    end;
  end;
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
  xaxis, yaxis, aprev, n, nprev: Number;
  gap: boolean;
  clipr: HRGN;

  procedure SetFontRotate(degr: integer);
  var
    lf: TLogFont;
  begin
    GetObject(Canvas.Font.Handle, sizeof(lf), @lf);
    lf.lfEscapement:= degr;
    lf.lfOrientation:= degr;
    DeleteObject(Canvas.Font.Handle);
    Canvas.Font.Handle:= CreateFontIndirect(lf);
  end;

  procedure AxisLabels;
  var
    oldbox: TRect;
    te: TSize;
  begin
    Canvas.Brush.Style:= bsClear;
    Canvas.Font.Name:= 'Arial';
    if FTitle > '' then begin   
      oldBox:= box;
      Canvas.Font.Size:= 12;
      te:= Canvas.TextExtent(FTitle);      
      inc(box.Top, te.cy + 10);
      Canvas.TextRect(Rect(box.Left, oldbox.Top, box.Right, box.Top), (box.Right+box.Left - te.cx) div 2, oldbox.Top, FTitle);
    end;
    if (FXLabel > '') or (FYLabel > '') then begin 
      oldBox:= box;
      // calc new client box
      Canvas.Font.Size:= 10;
      te:= Canvas.TextExtent('jJ');
      if FXLabel > '' then
        dec(box.Bottom, te.cy + 5);
      if FYLabel > '' then
        inc(box.Left, te.cy + 5);
        
      // center labels outside box
      if FXLabel > '' then begin
        te:= Canvas.TextExtent(FXLabel);
        Canvas.TextRect(Rect(box.Left, box.Bottom, box.Right, oldbox.Bottom), (box.Right+box.Left - te.cx) div 2, box.Bottom + 5, FXLabel);
      end;     
      if FYLabel > '' then begin
        te:= Canvas.TextExtent(FYLabel);
        SetFontRotate(900);
        Canvas.TextRect(Rect(oldbox.Left, box.Top, box.Left, box.Bottom), oldbox.Left, (box.Bottom+box.Top-te.cx) div 2, FYLabel);
        SetFontRotate(0);
      end;
    end;
  end;

  procedure Axes;

    function RoundStep(st:Number): Number;
    var
      sn: TValueSign;
    begin
      sn:= Sign(st);
      st:= abs(st);
      Result:= sn * Max(Max(5*Power(10,Floor64(Log10(st/5))),
                            2*Power(10,Floor64(Log10(st/2)))),
                              Power(10,Floor64(Log10(st))));
    end;

    procedure LabelAxis(Hor: boolean; Value: Number);
    var
      l: string;
      ts: TSize;
      w: integer;
    begin
      if not fzero(Value) then begin
        if Hor then
          l:= ax.AxisLabel(Value)
        else
          l:= ay.AxisLabel(Value);
        ts:= Canvas.TextExtent(l);

        if Hor then begin
          if miGrid.Checked then
            Canvas.TextOut(ax.Scale(Value) + 4, ay.Scale(xaxis) + 1, l)
          else
            Canvas.TextOut(ax.Scale(Value) - ts.cx div 2, ay.Scale(xaxis) + 1, l);
        end else begin
          if ax.Scale(yaxis)-50 <= box.Left then
            w:= ax.Scale(yaxis) + 5
          else
            w:= ax.Scale(yaxis) - ts.cx - 4;
          if miGrid.Checked then
            Canvas.TextOut(w, ay.Scale(Value) - ts.cy + 2, l)
          else
            Canvas.TextOut(w, ay.Scale(Value) - ts.cy div 2, l);
        end;
      end;
    end;

    procedure TickMark(Hor:Boolean; Value: Number; Main: boolean);
    var
      w: integer;
    begin
      if not fzero(Value) then begin
        if Main then
          LabelAxis(Hor, Value);

        if (miGrid.Checked and Main) or (miFineGrid.Checked and not Main) then begin
          Canvas.Pen.Width:= 1;
          Canvas.Pen.Style:= Graphics.psDot;
          if Main then
            Canvas.Pen.Color:= COL_GRID_MAJOR
          else
            Canvas.Pen.Color:= COL_GRID_MINOR;
          if Hor then begin
            Canvas.MoveTo(ax.Scale(Value), box.Top);
            Canvas.LineTo(ax.Scale(Value), box.Bottom);
          end else begin
            Canvas.MoveTo(box.Left, ay.Scale(Value));
            Canvas.LineTo(box.Right, ay.Scale(Value));
          end;
        end;

        Canvas.Pen.Width:= 1;
        Canvas.Pen.Style:= psSolid;
        Canvas.Pen.Color:= COL_AXIS;
        if Main then
          w:= 4
        else
          w:= 3;
        if Hor then begin
          Canvas.MoveTo(ax.Scale(Value), ay.Scale(xaxis) - w);
          Canvas.LineTo(ax.Scale(Value), ay.Scale(xaxis) + 0);
        end else begin
          Canvas.MoveTo(ax.Scale(yaxis) - w, ay.Scale(Value));
          Canvas.LineTo(ax.Scale(yaxis) + w, ay.Scale(Value));
        end;
      end;
    end;

    procedure PaintAxis(Scale: TScale; Min, Max: Number; PixelHint: integer; TickH: boolean);
    const
      divs:array[0..3] of integer = (2,4,5,10);
    var
      rng,maindiv,subdiv: Number;
      a,b: Number;
      i: integer;
    begin
      rng:= (Max-Min)/2;
      rng:= Power(10, Ceil(Log10(rng)));
      for i:= high(divs) downto 0 do begin
        maindiv:= rng/divs[i];
        if abs(Scale.Scale(0)-Scale.Scale(maindiv)) > PixelHint then
          break;
      end;
      for i:= 0 to high(divs) do begin
        subdiv:= maindiv/divs[i];
        if abs(Scale.Scale(0)-Scale.Scale(subdiv)) <= PixelHint/2 then
          break;
      end;

      a:= Floor64(Min / maindiv) * maindiv;
      if (a > Min-subdiv/10) then
        TickMark(TickH, a, true);
      while a < (Max+subdiv/10) do begin
        b:= a + maindiv;
        if (b < Max+subdiv/10) and (b > Min-subdiv/10) then begin
          TickMark(TickH,b,true);
        end;

        a:= a + subdiv;
        while (a < b) and not fzero(a-b) and (a < Max+subdiv/10) do begin
          if (a > Min-subdiv/10) then
            TickMark(TickH,a,false);
          a:= a + subdiv;
        end;
        a:= b;
      end;
    end;

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

    Canvas.Pen.Width:= 1;
    Canvas.Pen.Color:= COL_AXIS;
    Canvas.Brush.Style:= bsClear;
    Canvas.Font.Name:= 'Arial';      
    Canvas.Font.Size:= 10;

    Canvas.MoveTo(ax.Scale(yaxis), box.Top);
    Canvas.LineTo(ax.Scale(yaxis), box.Bottom);
    Canvas.MoveTo(box.Left, ay.Scale(xaxis));
    Canvas.LineTo(box.Right, ay.Scale(xaxis));

    PaintAxis(ay, ay.FMinVal, ay.FMaxVal, 80, false);

    PaintAxis(ax, ax.FMinVal, ax.FMaxVal, 100, true);
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
    for x:= ax.FFrameMin to ax.FFrameMax - 1 do begin
      a:= ax.Inverse(x);
      n:= ob.ValueAt(a);
      if ((ay is TLogScale) and ((n <= 0) or fzero(n))) or
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
      tup:= ob.List.Item[j] as IValueList;
      x:= CastToFloat(tup.Item[0]);
      if IsNan(x) then continue;
      if ((ax is TLogScale) and ((x <= 0) or fzero(x))) then
        continue;
      y:= CastToFloat(tup.Item[1]);
      if IsNan(y) then continue;
      if ((ay is TLogScale) and ((y <= 0) or fzero(y))) then
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
      tup:= ob.List.Item[indx] as IValueList;
      x:= CastToFloat(tup.Item[0]);
      if IsNan(x) then exit;
      if ((ax is TLogScale) and ((x <= 0) or fzero(x))) then
        exit;
      y:= CastToFloat(tup.Item[1]);
      if IsNan(y) then exit;
      if ((ay is TLogScale) and ((y <= 0) or fzero(y))) then
        exit;
      Result:= true;
    end;
  begin
    px:= NaN;
    py:= NaN;
    if ob.Lines<>lsNone then
      for j:= 0 to ob.List.Length - 1 do begin
        if not GetXY(j) then begin
          px:= nan;
          py:= nan;
          continue;
        end;

        xx:= ax.Scale(x);
        yy:= ay.Scale(y);

        if IsNan(px) or IsNan(py) then
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
        ob:= TPlotBase(FPlots[i].NativeObject);
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
        ob:= TPlotBase(FPlots[i].NativeObject);
        Canvas.Pen.Color:= ob.Color;
        Canvas.Pen.Width:= 5;
        Canvas.MoveTo(leg.Left+5, y);
        Canvas.LineTo(leg.Left+25, y);
        s:= TPlot(ob).Caption;
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
  AxisLabels;
  Canvas.Pen.Color:= clBlack;
  Canvas.Pen.Width:= 1;
  Canvas.Rectangle(box);
  FBufferBox:= box;
  ax:= TScale.FromMode(FXScale, FXMin, FXMax, box.Left + 10, box.Right - 10);
  ay:= TScale.FromMode(FYScale, FYMin, FYMax, box.Bottom - 10, box.Top + 10);
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
  TMenuItem(Sender).Checked:= true;
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

procedure TGraphWindow.miSaveEMFClick(Sender: TObject);
var
  emf: TMetafile;
  emfc: TMetafileCanvas;
begin
  if sdGraph.Execute then begin
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
      emf.SaveToFile(sdGraph.FileName);
    finally
      emf.Free;
    end;
  end;
end;

procedure TGraphWindow.miGridClick(Sender: TObject);
begin
  miFineGrid.Enabled:= miGrid.Checked;
  miFineGrid.Checked:= miFineGrid.Checked and miGrid.Checked;
  PaintGraph(Buffer.Canvas, ClientRect);
  Refresh;
end;

procedure TGraphWindow.miFineGridClick(Sender: TObject);
begin
  PaintGraph(Buffer.Canvas, ClientRect);
  miGrid.Checked:= miGrid.Checked or miFineGrid.Checked;
  Refresh;
end;

procedure TGraphWindow.FormMouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  if FInteract = imNone then begin
    if Button = mbLeft then begin
      FInteract:= FNextInteract;
      FZoomRect.TopLeft:= Point(X,Y);
      FZoomRect.BottomRight:= FZoomRect.TopLeft;
      Refresh;
    end;
  end;
end;

procedure TGraphWindow.FormMouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var
  ax,ay: TScale;

  procedure ZoomIn;
  begin
    FZoomRect:= Rect(
      Min(FZoomRect.Left, FZoomRect.Right),
      Min(FZoomRect.Top, FZoomRect.Bottom),
      Max(FZoomRect.Left, FZoomRect.Right),
      Max(FZoomRect.Top, FZoomRect.Bottom)
    );
    FXMin:= ax.Inverse(FZoomRect.Left);
    FXMax:= ax.Inverse(FZoomRect.Right);
    FYMax:= ay.Inverse(FZoomRect.Top);
    FYMin:= ay.Inverse(FZoomRect.Bottom);
    PaintGraph(Buffer.Canvas, ClientRect);
  end;

  procedure Drag;
  var
    r: TRect;
  begin
    r:= Rect(
      ax.Scale(FXMin), ay.Scale(FYMax),
      ax.Scale(FXMax), ay.Scale(FYMin)
    );

    OffsetRect(r, FZoomRect.Left-FZoomRect.Right, FZoomRect.Top-FZoomRect.Bottom);

    FXMin:= ax.Inverse(r.Left);
    FXMax:= ax.Inverse(r.Right);
    FYMax:= ay.Inverse(r.Top);
    FYMin:= ay.Inverse(r.Bottom);
    PaintGraph(Buffer.Canvas, ClientRect);
  end;

begin
  ax:= TScale.FromMode(FXScale, FXMin, FXMax, FBufferBox.Left + 10, FBufferBox.Right - 10);
  ay:= TScale.FromMode(FYScale, FYMin, FYMax, FBufferBox.Bottom - 10, FBufferBox.Top + 10);
  try
    case FInteract of
      imRead: ;
      imZoom: begin
        FZoomRect.BottomRight:= Point(X,Y);
        if (FZoomRect.Left<>FZoomRect.Right) and (FZoomRect.Top<>FZoomRect.Bottom) then
          ZoomIn;
      end;
      imDrag: begin
        FZoomRect.BottomRight:= Point(X,Y);
        Drag;
      end;
    end;
  finally
    FreeAndNil(ax);
    FreeAndNil(ay);
  end;
  FInteract:= imNone;
  Refresh;
end;

procedure TGraphWindow.FormMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
begin
  case FInteract of
    imRead,
    imZoom,
    imDrag: begin
      FZoomRect.BottomRight:= Point(X,Y);
      Refresh;
    end;
  end;
end;

procedure TGraphWindow.FormShow(Sender: TObject);
begin
  if IsNaN(FSXMin) then begin
    FSXMin:= FXMin;
    FSXMax:= FXMax;
    FSYMin:= FYMin;
    FSYMax:= FYMax;
  end;
end;

procedure TGraphWindow.miResetViewClick(Sender: TObject);
begin
  FXMin:= FSXMin;
  FXMax:= FSXMax;
  FYMin:= FSYMin;
  FYMax:= FSYMax;
  PaintGraph(Buffer.Canvas, ClientRect);
  Refresh;
end;

procedure TGraphWindow.miToolReadClick(Sender: TObject);
begin
  TMenuItem(Sender).Checked:= true;
  FNextInteract:= imRead;
  Cursor:= crCross;
end;

procedure TGraphWindow.miToolZoomClick(Sender: TObject);
begin
  TMenuItem(Sender).Checked:= true;
  FNextInteract:= imZoom;
  Cursor:= crCross;
end;

procedure TGraphWindow.miToolPanClick(Sender: TObject);
begin
  TMenuItem(Sender).Checked:= true;
  FNextInteract:= imDrag;
  Cursor:= crHandPoint;
end;

end.

