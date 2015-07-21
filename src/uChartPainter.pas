unit uChartPainter;

interface

uses
  SysUtils, Types, Windows, Graphics, uMathIntf, uMath, Math, uFPUSupport, uChartScale,
  uFunctionsGraphing;

type
  TLegendPosition = (leNone, leTopLeft, leTopRight, leBottomLeft, leBottomRight);

  TPaintTarget = record
    // In
    Canvas: TCanvas;
    DrawRect: TRect;
    // Out
    ChartArea: TRect;
  end;

  TChartPainter = class
  private type
    PTickMarkInfo = ^TTickMarkInfo;
    TTickMarkInfo = record
      Canvas: TCanvas;
      Longest: integer;
      Bounds: TRect;
      Horz: boolean;
      OtherAxis: integer;
      XPosition: integer;
    end;
    TTickMarkCallback = procedure (Value: Number; Main: boolean; Axis: TScale; Info: PTickMarkInfo) of object;
  private
    FXScale: TScale;
    FYScale: TScale;
    FTitle: string;
    FXLabel: string;
    FYLabel: string;
    FFineGrid: boolean;
    FGrid: boolean;
    FLegend: TLegendPosition;
    procedure SetXScale(const Value: TScale);
    procedure SetYScale(const Value: TScale);
  protected
    procedure ClearBackground(C: TCanvas; var R: TRect);
    procedure ChartTitle(C: TCanvas; var R: TRect);
    procedure AxisLabelY(C: TCanvas; var R: TRect);  
    procedure AxisLabelX(C: TCanvas; var R: TRect); 
    procedure OutsideFrame(C: TCanvas; var R: TRect);
    procedure Axes(C: TCanvas; var R: TRect);
    procedure LabelAxis(Info: PTickMarkInfo; Value: Number);
    procedure Graphs(C: TCanvas; var R: TRect);
    procedure Legend(C: TCanvas; var R: TRect);

    function PrecomputeAxisDimensions(C: TCanvas): TSize;
    function PrecomputeXAxisMargin(C: TCanvas): Integer;
    function OutsideAxis(XAxis: boolean): integer;

    procedure TickMarkGenerator(Scale: TScale; PixelHint: integer; CB: TTickMarkCallback; Info: PTickMarkInfo);
    procedure TicksComputeMax(Value: Number; Main: boolean; Axis: TScale; Info: PTickMarkInfo);
    procedure TicksDrawMarks(Value: Number; Main: boolean; Axis: TScale; Info: PTickMarkInfo);
  public        
    Plots: TPlotsArray;

    constructor Create;
    destructor Destroy; override;

    property XScale: TScale read FXScale write SetXScale;
    property YScale: TScale read FYScale write SetYScale;
    property XLabel: string read FXLabel write FXLabel;
    property YLabel: string read FYLabel write FYLabel;

    property Title: string read FTitle write FTitle;

    property Grid: boolean read FGrid write FGrid;
    property FineGrid: boolean read FFineGrid write FFineGrid;

    property LegendPosition: TLegendPosition read FLegend write FLegend;

    procedure PaintGraph(var Target: TPaintTarget);
  end;

implementation

uses
  uMathValues;

const
  CCOLOR_BACKGROUND = clWhite;
  OUTER_PADDING     = 10;

  CFONT_TITLE       = 'Arial';
  CFONT_TITLE_S     = 12;
  TITLE_PADDING     = 10;

  CFONT_LABEL       = CFONT_TITLE;
  CFONT_ALABEL_S    = 10;
  LABEL_PADDING     = 5;

  CFONT_AXIS        = CFONT_TITLE;
  CFONT_AXIS_S      = 10;
  AXIS_PADDING      = 5;
                         
  TICKHINT_X        = 100;
  TICKHINT_Y        = 80;

  FRAME_WIDTH       = 2;    
  AXIS_WIDTH        = 1;
  GRID_WIDTH        = 1;
                            
  CFONT_LEGEND      = CFONT_TITLE;
  CFONT_LEGEND_S    = 10;
  LEGEND_MARGIN     = 10;
  LEGEND_PADDING    = 5;

  COL_FRAME         = $000000;
  COL_AXIS          = $404040;
  COL_GRID_MAJOR    = $808080;
  COL_GRID_MINOR    = $C0C0C0;
  COL_LEGEND_BG     = $FFFFFF;  
  COL_LEGEND_SHADOW = COL_GRID_MINOR;

procedure TextRectRotated(Canvas: TCanvas; Rect: TRect; X, Y, degr: Integer; const Text: string);
var
  ofn, rf: HFONT;
  lf: TLogFont;
begin
  GetObject(Canvas.Font.Handle, sizeof(lf), @lf);
  ofn:= Canvas.Font.Handle;
  try
    lf.lfEscapement:= degr;
    lf.lfOrientation:= degr;
    rf:= CreateFontIndirect(lf);
    try
      SelectObject(Canvas.Handle, rf);
      Canvas.TextRect(Rect, X, Y, Text);
    finally
      DeleteObject(rf);
    end;
  finally
    SelectObject(Canvas.Handle, ofn);
  end;
end;

{ TChartPainter }

constructor TChartPainter.Create;
begin
  inherited Create;
end;

destructor TChartPainter.Destroy;
begin
  FreeAndNil(FXScale);
  FreeAndNil(FYScale);
  inherited;
end;

procedure TChartPainter.Graphs(C: TCanvas; var R: TRect);
var
  dcs: integer;
  clipr: HRGN;
  i: integer;
  ob: TPlotBase;

  procedure DrawFunction_Plot(ob: TPlot);
  var
    x: Integer;
    a: Number;
    gap: boolean;
    nprev, aprev, n: Number;
  begin
    C.Pen.Width:= trunc(ob.Size);
    C.Pen.Color:= ob.Color;
    gap:= true;
    nprev:= 0;
    aprev:= 0;
    for x:= XScale.FrameMin to XScale.FrameMax - 1 do begin
      a:= XScale.Inverse(x);
      n:= ob.ValueAt(a);
      if ((YScale is TLogScale) and ((n <= 0) or fzero(n))) or
        (IsNan(n)) or
        (IsInfinite(n)) or
        ((abs((n - nprev + 1E-17) / (a - aprev + 1E-17)) > 1E5)) then
        gap:= true
      else begin
        if gap then
          C.MoveTo(XScale.Scale(a), YScale.Scale(n))
        else
          C.LineTo(XScale.Scale(a), YScale.Scale(n));
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
      if ((XScale is TLogScale) and ((x <= 0) or fzero(x))) then
        continue;
      y:= CastToFloat(tup.Item[1]);
      if IsNan(y) then continue;
      if ((YScale is TLogScale) and ((y <= 0) or fzero(y))) then
        continue;
      C.Brush.Color:= ob.Color;
      C.Pen.Width:= 1;
      C.Pen.Color:= clBlack;
      C.Rectangle(Rect(
        XScale.Scale(x - ob.Size / 2),
        YScale.Scale(y),
        XScale.Scale(x + ob.Size / 2),
        YScale.Scale(0)));
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
      if ((XScale is TLogScale) and ((x <= 0) or fzero(x))) then
        exit;
      y:= CastToFloat(tup.Item[1]);
      if IsNan(y) then exit;
      if ((YScale is TLogScale) and ((y <= 0) or fzero(y))) then
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

        xx:= XScale.Scale(x);
        yy:= YScale.Scale(y);

        if IsNan(px) or IsNan(py) then
          C.MoveTo(xx,yy)
        else begin
          C.MoveTo(XScale.Scale(px),YScale.Scale(py));
          case ob.Lines of
            lsStraight: begin
              C.Pen.Color:= ob.Color;
              C.LineTo(xx,yy);
            end;
            lsHoldX: begin
              C.Pen.Color:= ob.Color;
              C.LineTo(XScale.Scale(px),yy);
              C.LineTo(xx,yy);
            end;
            lsHoldY: begin
              C.Pen.Color:= ob.Color;
              C.LineTo(xx,YScale.Scale(py));
              C.LineTo(xx,yy);
            end;
            lsStepX: begin
              C.Pen.Color:= ob.Color;
              C.LineTo(XScale.Scale(px),yy);
              C.MoveTo(xx,yy);
            end;
            lsStepY: begin
              C.Pen.Color:= ob.Color;
              C.LineTo(xx,YScale.Scale(py));
              C.MoveTo(xx,yy);
            end;
          end;
        end;
        px:= x;
        py:= y;
      end;
    if ob.Points<>psNone then
      for j:= 0 to ob.List.Length - 1 do begin
        if not GetXY(j) then continue;

        xx:= XScale.Scale(x);
        yy:= YScale.Scale(y);

        case ob.Points of
          psDot: C.Pixels[xx,yy]:= ob.Color;
          psCross: begin
            C.Pen.Color:= ob.Color;
            C.Pen.Style:= psSolid;
            C.MoveTo(xx-trunc(ob.Size),yy-Trunc(ob.Size));
            C.LineTo(xx+trunc(ob.Size+1),yy+Trunc(ob.Size+1));
            C.MoveTo(xx+trunc(ob.Size),yy-Trunc(ob.Size));
            C.LineTo(xx-trunc(ob.Size+1),yy+Trunc(ob.Size+1));
          end;
          psPlus: begin
            C.Pen.Color:= ob.Color;
            C.Pen.Style:= psSolid;
            C.MoveTo(xx-trunc(ob.Size),yy);
            C.LineTo(xx+trunc(ob.Size+1),yy);
            C.MoveTo(xx,yy-Trunc(ob.Size));
            C.LineTo(xx,yy+Trunc(ob.Size+1));
          end;
          psCircle: begin
            C.Brush.Color:= ob.Color;
            C.Pen.Color:= clBlack;
            C.Pen.Style:= psSolid;
            C.Ellipse(xx-trunc(ob.Size),yy-Trunc(ob.Size),
                           xx+trunc(ob.Size+1),yy+Trunc(ob.Size+1));
          end;
          psSquare: begin
            C.Brush.Color:= ob.Color;
            C.Pen.Color:= clBlack;
            C.Pen.Style:= psSolid;
            C.Rectangle(xx-trunc(ob.Size),yy-Trunc(ob.Size),
                           xx+trunc(ob.Size+1),yy+Trunc(ob.Size+1));
          end;
        end;
      end;
  end;

begin
  clipr:= CreateRectRgn(R.Left + 1, R.Top + 1, R.Right - 1, R.Bottom - 1);
  dcs:= SaveDC(C.Handle);
  SelectClipRgn(C.Handle, clipr);
  try
    for i:= 0 to high(Plots) do begin
      ob:= TPlotBase(Plots[i].NativeObject);
      if ob is TPlot then
        DrawFunction_Plot(TPlot(ob))
      else if ob is THistogram then
        DrawFunction_Histogram(THistogram(ob))
      else if ob is TXYPlot then
        DrawFunction_XYPlot(TXYPlot(ob));
    end;
  finally
    RestoreDC(C.Handle, dcs);
    DeleteObject(clipr);
  end;
end;

procedure TChartPainter.Legend(C: TCanvas; var R: TRect);  
var
  i, y: integer;
  needed: TSize;
  leg,shadow: TRect;
  s: string;
  ts: TSize;
  lineHeight: integer;
  ob: TPlotBase; 
  clipr: HRGN;
begin
  if FLegend = leNone then
    exit;
                     
  C.Font.Name:= CFONT_LEGEND;
  C.Font.Size:= CFONT_LEGEND_S;
  // compute space needed
  lineHeight:= C.TextHeight('Ij');
  needed.cx:= 0;
  for i:= 0 to high(Plots) do begin
    ob:= TPlotBase(Plots[i].NativeObject);
    s:= TPlot(ob).Caption;
    ts:= C.TextExtent(s);
    needed.cx:= Max(ts.cx, needed.cx);
  end;         
  needed.cy:= round((Length(Plots)-1) * (lineHeight * 1.5) + lineHeight);
  // include padding and colored line
  inc(needed.cx, 2*LEGEND_PADDING + 35 + LEGEND_PADDING);
  inc(needed.cy, 2*LEGEND_PADDING);

  // outside rectangle
  leg:= R;
  InflateRect(leg, -LEGEND_MARGIN, -LEGEND_MARGIN);
  case FLegend of
    leTopLeft: begin
      leg.Right:= leg.Left + needed.cx;
      leg.Bottom:= leg.Top + needed.cy;
    end;
    leTopRight: begin
      leg.Left:= leg.Right - needed.cx;
      leg.Bottom:= leg.Top + needed.cy;
    end;  
    leBottomLeft: begin
      leg.Right:= leg.Left + needed.cx;
      leg.Top:= leg.Bottom - needed.cy;
    end;
    leBottomRight: begin
      leg.Left:= leg.Right - needed.cx;
      leg.Top:= leg.Bottom - needed.cy;
    end;
  end;

  // draw frame
  C.Pen.Color:= COL_FRAME;
  C.Pen.Width:= 1;
  C.Brush.Style:= bsSolid;
  C.Brush.Color:= COL_LEGEND_SHADOW;
  shadow:= Rect(leg.Left+LEGEND_PADDING, leg.top+LEGEND_PADDING, leg.Right+LEGEND_PADDING, leg.Bottom+LEGEND_PADDING);
  C.FillRect(shadow);
  C.Brush.Color:= COL_LEGEND_BG;
  C.Rectangle(leg);
  
  InflateRect(leg, -LEGEND_PADDING, -LEGEND_PADDING);
  clipr:= CreateRectRgn(leg.Left, leg.Top, leg.Right, leg.Bottom);
  SelectClipRgn(C.Handle, clipr);
  try
    y:= leg.Top + CFONT_LEGEND_S;
    for i:= 0 to high(Plots) do begin
      ob:= TPlotBase(Plots[i].NativeObject);
      C.Pen.Color:= ob.Color;
      C.Pen.Width:= 5;
      C.MoveTo(leg.Left+5, y);
      C.LineTo(leg.Left+20, y);
      s:= TPlot(ob).Caption;
      C.TextOut(leg.Left+35, y - lineHeight div 2, s);
      inc(y, round(lineHeight * 1.5));
    end;
  finally
    SelectClipRgn(C.Handle, 0);
    DeleteObject(clipr);
  end;
end;

procedure TChartPainter.Axes(C: TCanvas; var R: TRect);
var
  xaxis, yaxis: Number;
  TickInfo: TTickMarkInfo;
begin
  if FXScale.Max < 0 then
    yaxis:= FXScale.Max
  else if FXScale.Min > 0 then
    yaxis:= FXScale.Min
  else
    yaxis:= 0;

  if FYScale.Max < 0 then
    xaxis:= FYScale.Max
  else if FYScale.Min > 0 then
    xaxis:= FYScale.Min
  else
    xaxis:= 0;

  C.Pen.Width:= AXIS_WIDTH;
  C.Pen.Color:= COL_AXIS;
  C.Brush.Style:= bsClear;
  C.Font.Name:= CFONT_AXIS;
  C.Font.Size:= CFONT_AXIS_S;

  C.MoveTo(FXScale.Scale(yaxis), R.Top);
  C.LineTo(FXScale.Scale(yaxis), R.Bottom);
  C.MoveTo(R.Left, FYScale.Scale(xaxis));
  C.LineTo(R.Right, FYScale.Scale(xaxis));

  TickInfo.Canvas:= C;
  TickInfo.Bounds:= R;
  TickInfo.Horz:= false;
  TickInfo.OtherAxis:= FXScale.Scale(yaxis);
  TickInfo.XPosition:= OutsideAxis(false);
  TickMarkGenerator(FYScale, TICKHINT_Y, TicksDrawMarks, @TickInfo);    

  TickInfo.Horz:= true;   
  TickInfo.OtherAxis:= FYScale.Scale(xaxis);
  TickMarkGenerator(FXScale, TICKHINT_X, TicksDrawMarks, @TickInfo);
end;

procedure TChartPainter.SetXScale(const Value: TScale);
begin
  FreeAndNil(FXScale);
  FXScale := Value;
end;

procedure TChartPainter.SetYScale(const Value: TScale);
begin
  FreeAndNil(FYScale);
  FYScale := Value;
end;     

procedure TChartPainter.TickMarkGenerator(Scale: TScale; PixelHint: integer; CB: TTickMarkCallback; Info: PTickMarkInfo);
const
  divs:array[0..3] of integer = (2,4,5,10);
var
  rng,maindiv,subdiv: Number;
  a,b: Number;
  i: integer;
begin
  rng:= (Scale.Max-Scale.Min)/2;
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

  a:= Floor64(Scale.Min / maindiv) * maindiv;
  if (a > Scale.Min-subdiv/10) then
    CB(a, true, Scale, Info);
  while a < (Scale.Max+subdiv/10) do begin
    b:= a + maindiv;
    if (b < Scale.Max+subdiv/10) and (b > Scale.Min-subdiv/10) then begin
      CB(b, true, Scale, Info);
    end;

    a:= a + subdiv;
    while (a < b) and not fzero(a-b) and (a < Scale.Max+subdiv/10) do begin
      if (a > Scale.Min-subdiv/10) then
        CB(a, false, Scale, Info);
      a:= a + subdiv;
    end;
    a:= b;
  end;
end;

procedure TChartPainter.TicksComputeMax(Value: Number; Main: boolean; Axis: TScale; Info: PTickMarkInfo);
var
  te: TSize;
  s: string;
begin
  if Main then begin
    s:= Axis.AxisLabel(Value);
    te:= Info^.Canvas.TextExtent(s);
    Info^.Longest:= Max(Info^.Longest, te.cx);
  end;
end;

procedure TChartPainter.LabelAxis(Info: PTickMarkInfo; Value: Number);
var
  l: string;
  ts: TSize;
  w,a: integer;
  box: TRect;
begin
  if not fzero(Value) then begin
    if Info^.Horz then
      l:= FXScale.AxisLabel(Value)
    else
      l:= FYScale.AxisLabel(Value);
    ts:= Info^.Canvas.TextExtent(l);

    box:= Info^.Bounds;

    if Info^.Horz then begin
      a:= info^.OtherAxis + 1;
      if FGrid and (a <> box.Bottom) then
        w:= FXScale.Scale(Value) + 4
      else
        w:= FXScale.Scale(Value) - ts.cx div 2;
      w:= Min(Box.Right-ts.cx, Max(box.Left, w));
      Info^.Canvas.TextOut(w, a, l);
    end else begin
      case Info^.XPosition of
          -1: w:= info^.OtherAxis - ts.cx - 4;
        0, 1: w:= info^.OtherAxis + 5;
      end;
      if FGrid and (Info^.XPosition=0) then
        Info^.Canvas.TextOut(w, FYScale.Scale(Value) - ts.cy + 2, l)
      else
        Info^.Canvas.TextOut(w, FYScale.Scale(Value) - ts.cy div 2, l);
    end;
  end;
end;

procedure TChartPainter.TicksDrawMarks(Value: Number; Main: boolean; Axis: TScale; Info: PTickMarkInfo);
var
  w: integer;
  Canvas: TCanvas;
  box: TRect;
begin
  if not fzero(Value) then begin 
    Canvas:= Info^.Canvas;
    box:= Info^.Bounds;
    
    if Main then
      LabelAxis(Info, Value);

    // Draw Grid Lines
    if (FGrid and Main) or (FFineGrid and not Main) then begin
      Canvas.Pen.Width:= GRID_WIDTH;
      Canvas.Pen.Style:= Graphics.psDot;
      if Main then
        Canvas.Pen.Color:= COL_GRID_MAJOR
      else
        Canvas.Pen.Color:= COL_GRID_MINOR;
      if Info^.Horz then begin
        Canvas.MoveTo(FXScale.Scale(Value), box.Top);
        Canvas.LineTo(FXScale.Scale(Value), box.Bottom);
      end else begin
        Canvas.MoveTo(box.Left, FYScale.Scale(Value));
        Canvas.LineTo(box.Right, FYScale.Scale(Value));
      end;
    end;

    // Tick Marks
    Canvas.Pen.Width:= AXIS_WIDTH;
    Canvas.Pen.Style:= psSolid;
    Canvas.Pen.Color:= COL_AXIS;
    if Main then
      w:= 4
    else
      w:= 3;
    if Info^.Horz then begin
      Canvas.MoveTo(FXScale.Scale(Value), Info^.OtherAxis - w);
      Canvas.LineTo(FXScale.Scale(Value), Info^.OtherAxis + 0);
    end else begin
      Canvas.MoveTo(Info^.OtherAxis - w, FYScale.Scale(Value));
      Canvas.LineTo(Info^.OtherAxis + w, FYScale.Scale(Value));
    end;
  end;
end;

procedure TChartPainter.ClearBackground(C: TCanvas; var R: TRect);
begin
  C.Brush.Color:= CCOLOR_BACKGROUND;  
  C.Brush.Style:= bsSolid;
  C.FillRect(R);
  C.Brush.Style:= bsClear;
  InflateRect(R, -OUTER_PADDING, -OUTER_PADDING); 
end;

procedure TChartPainter.ChartTitle(C: TCanvas; var R: TRect);
var
  te: TSize;
  oldtop: integer;
begin
  if FTitle > '' then begin
    C.Font.Name:= CFONT_TITLE;
    C.Font.Size:= CFONT_TITLE_S;
    te:= C.TextExtent(FTitle);
    oldtop:= R.Top;
    inc(R.Top, te.cy + TITLE_PADDING);
    C.TextRect(Rect(R.Left, oldtop, R.Right, R.Top),
               (R.Right+R.Left - te.cx) div 2, oldtop,
               FTitle);
  end;
end;
      
procedure TChartPainter.AxisLabelX(C: TCanvas; var R: TRect);
var
  te: TSize;
  box: TRect;
begin
  if FXLabel > '' then begin
    C.Font.Name:= CFONT_LABEL;
    C.Font.Size:= CFONT_ALABEL_S;
    te:= C.TextExtent(FXLabel);

    box:= R;
    box.Top:= box.Bottom - te.cy;
    Dec(R.Bottom, te.cy + LABEL_PADDING);

    C.TextRect(box, (box.Right+box.Left - te.cx) div 2, box.Top, FXLabel);
  end;
end;

procedure TChartPainter.AxisLabelY(C: TCanvas; var R: TRect);
var
  te: TSize;
  box: TRect;
begin
  if FYLabel > '' then begin
    C.Font.Name:= CFONT_LABEL;
    C.Font.Size:= CFONT_ALABEL_S;
    te:= C.TextExtent(FYLabel);

    box:= R;
    case OutsideAxis(false) of
      -1, 0: begin
        box.Right:= box.Left + te.cy;
        Inc(R.Left, te.cy + LABEL_PADDING);
      end;
      1: begin
        box.Left:= box.Right - te.cy;
        Dec(R.Right, te.cy + LABEL_PADDING);
      end;
    end;
    TextRectRotated(C, box, box.Left, (box.Bottom+box.Top+te.cx) div 2, 900, FYLabel);
  end;
end;

procedure TChartPainter.OutsideFrame(C: TCanvas; var R: TRect);
begin
  C.Pen.Width:= FRAME_WIDTH;
  C.Pen.Color:= COL_FRAME;
  C.Rectangle(Rect(R.Left, R.Top, R.Right+1, R.Bottom+1));
end;

procedure TChartPainter.PaintGraph(var Target: TPaintTarget);
var
  xax_space, yaxpos: integer;
  TickInfo: TTickMarkInfo;
begin
  Target.ChartArea:= Target.DrawRect;

  if not Assigned(FXScale) or not Assigned(FYScale) then
    exit;

  ClearBackground(Target.Canvas, Target.ChartArea);

  ChartTitle(Target.Canvas, Target.ChartArea);
  // preliminary scale
  FXScale.FrameMin:= Target.ChartArea.Left;
  FXScale.FrameMax:= Target.ChartArea.Right;
  FYScale.FrameMin:= Target.ChartArea.Bottom;
  FYScale.FrameMax:= Target.ChartArea.Top;

  xax_space:= PrecomputeXAxisMargin(Target.Canvas);

  // store to axis for computation, but don't change ChartArea yet - X Label needs old rect
  FYScale.FrameMin:= Target.ChartArea.Bottom - xax_space;

  // Paint Y Label on correct side
  AxisLabelY(Target.Canvas, Target.ChartArea);

  // Compute Y Axis label width, if it is outside
  yaxpos:= OutsideAxis(false);
  if yaxpos <> 0 then begin
    // inc bottom by xax_space, do a full axis label run
    TickInfo.Canvas:= Target.Canvas;
    Target.Canvas.Font.Name:= CFONT_AXIS;
    Target.Canvas.Font.Size:= CFONT_AXIS_S;
    TickInfo.Longest:= 0;
    TickMarkGenerator(FYScale, TICKHINT_Y, TicksComputeMax, @TickInfo);

    case yaxpos of
      -1: Inc(Target.ChartArea.Left, TickInfo.Longest + AXIS_PADDING);
      +1: Dec(Target.ChartArea.Right, TickInfo.Longest + AXIS_PADDING);
    end;
  end;

  // Paint X Label now that we fixed Left/Right
  AxisLabelX(Target.Canvas, Target.ChartArea);

  // read back / set current
  Target.ChartArea.Bottom:= FYScale.FrameMin;
  FXScale.FrameMin:= Target.ChartArea.Left;
  FXScale.FrameMax:= Target.ChartArea.Right;
  FYScale.FrameMin:= Target.ChartArea.Bottom;
  FYScale.FrameMax:= Target.ChartArea.Top;

  // outer frame is fixed now    
  OutsideFrame(Target.Canvas, Target.ChartArea);

  Graphs(Target.Canvas, Target.ChartArea);
  Axes(Target.Canvas, Target.ChartArea);
  Legend(Target.Canvas, Target.ChartArea);
end;

{
Result: 0 = inside, -1: axis is smaller than Min, +1: axis is larger than Max 
}
function TChartPainter.OutsideAxis(XAxis: boolean): integer;
begin
  Result:= 0;
  if XAxis then begin   
    if FYScale.Min > 0 then
      Result:= -1
    else if FYScale.Max < 0 then
      Result:= +1
    else begin
      // can only be reached on non-log scales
      if (FYScale.Scale(0) > FYScale.FrameMin + 20) then
        Result:= -1
      else if (FYScale.Scale(0) < FYScale.FrameMax + 20) then
        Result:= +1;
    end;
  end else begin    
    if FXScale.Min >= 0 then
      Result:= -1
    else if FXScale.Max <= 0 then
      Result:= +1
    else begin              
      // can only be reached on non-log scales
      if (FXScale.Scale(0) < FXScale.FrameMin + 20) then
        Result:= -1
      else if (FXScale.Scale(0) > FXScale.FrameMax + 20) then
        Result:= +1;
    end;
  end;
end;

function TChartPainter.PrecomputeAxisDimensions(C: TCanvas): TSize;
var
  te: TSize;
begin
  Result.cx:= 0;
  Result.cy:= PrecomputeXAxisMargin(C);


  if FYLabel > '' then begin
    C.Font.Name:= CFONT_LABEL;
    C.Font.Size:= CFONT_ALABEL_S;
    te:= C.TextExtent(FYLabel);
    inc(Result.cx, te.cy); // will be rotated 90°
    inc(Result.cx, LABEL_PADDING);
  end;

  if OutsideAxis(false)<>0 then begin
    // TODO: guess width of formatted output
    inc(Result.cx, 50);  
    inc(Result.cx, AXIS_PADDING);
  end;
end;

function TChartPainter.PrecomputeXAxisMargin(C: TCanvas): Integer;
var
  te: TSize;
begin
  Result:= 0;

  if FXLabel > '' then begin
    C.Font.Name:= CFONT_LABEL;
    C.Font.Size:= CFONT_ALABEL_S;
    te:= C.TextExtent(FXLabel);
    inc(Result, te.cy);
    inc(Result, LABEL_PADDING);
  end;

  if OutsideAxis(true)<0 then begin
    C.Font.Name:= CFONT_AXIS;
    C.Font.Size:= CFONT_AXIS_S;
    te:= C.TextExtent(FXScale.AxisLabel(FXScale.Min));
    inc(Result, te.cy);
    inc(Result, AXIS_PADDING);
  end;
end;

end.
