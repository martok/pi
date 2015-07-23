{-----------------------------------------------------------------------------
 Package: Graphing

 Visuals
-----------------------------------------------------------------------------}
unit uGraphWindow;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms,
  Dialogs, uFunctionsGraphing, uMath, uMathIntf, uMathValues, Menus, ExtDlgs,
  uFPUSupport, uDockableForms, uChartScale, uChartPainter;

type
  TInteractMode = (imNone, imRead, imZoom, imDrag);

  TGraphWindow = class(TDockableForm)
    pmGraph: TPopupMenu;
    miLegend: TMenuItem;
    miLegendNone: TMenuItem;
    N1: TMenuItem;
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
    miSaveFile: TMenuItem;
    CopyAs1: TMenuItem;
    miCopyBitmap: TMenuItem;
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
    procedure miSaveFileClick(Sender: TObject);
    procedure miCopyBitmapClick(Sender: TObject);
  private
    { Private-Deklarationen }
    FPlots: TPlotsArray;
    FPainter: TChartPainter;
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
    //procedure PaintGraph__(Canvas: TCanvas; DrawRect: TRect);
    procedure RepaintBuffer;
    procedure InitPainter;
    procedure SetXMax(const Value: Number);
    procedure SetXMin(const Value: Number);
    procedure SetYMax(const Value: Number);
    procedure SetYMin(const Value: Number);
  public
    { Public-Deklarationen }
    constructor CreateGraph(Plots: array of IPlotObject);
    destructor Destroy; override;
    property XMin: Number read FXMin write SetXMin;
    property XMax: Number read FXMax write SetXMax;
    property YMin: Number read FYMin write SetYMin;
    property YMax: Number read FYMax write SetYMax;
    property XScale: TScaleMode read FXScale write FXScale;
    property YScale: TScaleMode read FYScale write FYScale;
    property Title: string read FTitle write FTitle;
    property XLabel: string read FXLabel write FXLabel;
    property YLabel: string read FYLabel write FYLabel;
  end;

var
  GraphWindow: TGraphWindow;

const
  COL_AXIS         : TColor = $404040;
  COL_GRID_MAJOR   : TColor = $808080;
  COL_GRID_MINOR   : TColor = $C0C0C0;

implementation

uses Types, Math, uChartExport;

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

  function NewRadioItem(const aCaption: string; const aTag, aGroup: integer; const aOnClick: TNotifyEvent): TMenuItem;
  begin
    Result:= Menus.NewItem(aCaption, 0, false, true, aOnClick, 0, '');
    Result.Tag:= aTag;
    Result.GroupIndex:= aGroup;
    Result.RadioItem:= true
  end;

begin
  DoubleBuffered:= true;
  Buffer:= TBitmap.Create;
  FBufferBox:= Rect(0,0,1,1);
  FPainter:= TChartPainter.Create;
  miGrid.Checked:= True;
  FSXMin:= NAN;
  FSXMax:= NAN;
  FSYMin:= NAN;
  FSYMax:= NAN;
  FInteract:= imNone;
  FNextInteract:= imZoom;
  miToolRead.Click;
  miLegend.Add(NewRadioItem('Top Left', ord(leTopLeft), 1, miLegendChange));
  miLegend.Add(NewRadioItem('Top Right', ord(leTopRight), 1, miLegendChange));
  miLegend.Add(NewRadioItem('Bottom Left', ord(leBottomLeft), 1, miLegendChange));
  miLegend.Add(NewRadioItem('Bottom Right', ord(leBottomRight), 1, miLegendChange));
end;

destructor TGraphWindow.Destroy;
var
  i: integer;
begin
  FreeAndNil(Buffer);
  for i:= 0 to high(FPlots) do
    FPlots[i]:= nil;
  FreeAndNil(FPainter);
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
      FPainter.XScale.FrameMin:= FBufferBox.Left;
      FPainter.XScale.FrameMax:= FBufferBox.Right;
      FPainter.YScale.FrameMin:= FBufferBox.Bottom;
      FPainter.YScale.FrameMax:= FBufferBox.Top;
      ax:= FPainter.XScale;
      ay:= FPainter.YScale;
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
  RepaintBuffer;
  Refresh;
end;  

procedure TGraphWindow.RepaintBuffer;
var
  tg: TPaintTarget;
begin
  tg.Canvas:= Buffer.Canvas;
  tg.DrawRect:= ClientRect;
  FPainter.PaintGraph(tg);
  FBufferBox:= tg.ChartArea;
end;

procedure TGraphWindow.SetXMax(const Value: Number);
begin
  if fsame(Value, FXMax) then exit;
  FXMax := Value;
  if Assigned(FPainter.XScale) then
    FPainter.XScale:= TScale.FromMode(FXScale, FXMin, FXMax, FPainter.XScale.FrameMin, FPainter.XScale.FrameMax);
end;

procedure TGraphWindow.SetXMin(const Value: Number);
begin
  if fsame(Value, FXMin) then exit;
  FXMin := Value;
  if Assigned(FPainter.XScale) then
    FPainter.XScale:= TScale.FromMode(FXScale, FXMin, FXMax, FPainter.XScale.FrameMin, FPainter.XScale.FrameMax);
end;

procedure TGraphWindow.SetYMax(const Value: Number);
begin
  if fsame(Value, FYMin) then exit;
  FYMax := Value;
  if Assigned(FPainter.YScale) then
    FPainter.YScale:= TScale.FromMode(FYScale, FYMin, FYMax, FPainter.YScale.FrameMin, FPainter.YScale.FrameMax);
end;

procedure TGraphWindow.SetYMin(const Value: Number);
begin
  if fsame(Value, FYMin) then exit;
  FYMin := Value;                                                                                                
  if Assigned(FPainter.YScale) then
    FPainter.YScale:= TScale.FromMode(FYScale, FYMin, FYMax, FPainter.YScale.FrameMin, FPainter.YScale.FrameMax);
end;

procedure TGraphWindow.miLegendChange(Sender: TObject);
begin
  TMenuItem(Sender).Checked:= true;
  FPainter.LegendPosition:= TLegendPosition(TMenuItem(Sender).Tag);
  RepaintBuffer;
  Refresh;
end;

procedure TGraphWindow.miCopyBitmapClick(Sender: TObject);
begin
  with TChartExportBMP.Create(ClientRect, Canvas) do try
    Paint(FPainter);
    ToClipboard;
  finally
    Free;
  end;
end;

procedure TGraphWindow.miCopyEMFClick(Sender: TObject);
begin
  with TChartExportEMF.Create(ClientRect, Canvas) do try
    Paint(FPainter);
    ToClipboard;
  finally
    Free;
  end;
end;

procedure TGraphWindow.miSaveFileClick(Sender: TObject);
var
  exp: TChartExport;
  lcx: string;
begin
  if sdGraph.Execute then begin
    lcx:= LowerCase(ExtractFileExt(sdGraph.FileName));
    if lcx = '.emf' then
      exp:= TChartExportEMF.Create(ClientRect, Canvas)
    else if lcx = '.bmp' then
      exp:= TChartExportBMP.Create(ClientRect, Canvas)
    else begin
      MessageDlg(Format('Unsupported file extension: %s', [lcx]), mtError, [mbOK],0);
      exit;
    end;

    try
      exp.Paint(FPainter);
      exp.ToFile(sdGraph.FileName);
    finally
      exp.Free;
    end;
  end;
end;

procedure TGraphWindow.miGridClick(Sender: TObject);
begin
  miFineGrid.Enabled:= miGrid.Checked;
  miFineGrid.Checked:= miFineGrid.Checked and miGrid.Checked;
  FPainter.Grid:= miGrid.Checked;
  FPainter.FineGrid:= miFineGrid.Checked;
  RepaintBuffer;
  Refresh;
end;

procedure TGraphWindow.miFineGridClick(Sender: TObject);
begin
  miGrid.Checked:= miGrid.Checked or miFineGrid.Checked;
  FPainter.FineGrid:= miFineGrid.Checked;
  RepaintBuffer;
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
    XMin:= ax.Inverse(FZoomRect.Left);
    XMax:= ax.Inverse(FZoomRect.Right);
    YMax:= ay.Inverse(FZoomRect.Top);
    YMin:= ay.Inverse(FZoomRect.Bottom);
    RepaintBuffer;
  end;

  procedure Drag;
  var
    r: TRect;
  begin
    r:= Rect(
      ax.Scale(XMin), ay.Scale(YMax),
      ax.Scale(XMax), ay.Scale(YMin)
    );

    OffsetRect(r, FZoomRect.Left-FZoomRect.Right, FZoomRect.Top-FZoomRect.Bottom);

    XMin:= ax.Inverse(r.Left);
    XMax:= ax.Inverse(r.Right);
    YMax:= ay.Inverse(r.Top);
    YMin:= ay.Inverse(r.Bottom);
    RepaintBuffer;
  end;

begin
  FPainter.XScale.FrameMin:= FBufferBox.Left;
  FPainter.XScale.FrameMax:= FBufferBox.Right;
  FPainter.YScale.FrameMin:= FBufferBox.Bottom;
  FPainter.YScale.FrameMax:= FBufferBox.Top;
  ax:= FPainter.XScale;
  ay:= FPainter.YScale;
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
    FSXMin:= XMin;
    FSXMax:= XMax;
    FSYMin:= YMin;
    FSYMax:= YMax;

    InitPainter;
  end;
end;

procedure TGraphWindow.InitPainter;
begin
  FPainter.Title:= FTitle;
  FPainter.XLabel:= FXLabel;
  FPainter.YLabel:= FYLabel;
  FPainter.XScale:= TScale.FromMode(FXScale, XMin, XMax, 0, ClientWidth);
  FPainter.YScale:= TScale.FromMode(FYScale, YMin, YMax, 0, ClientHeight);
  FPainter.Grid:= miGrid.Checked;
  FPainter.FineGrid:= miGrid.Checked and miFineGrid.Checked;
  FPainter.Plots:= FPlots;
end;

procedure TGraphWindow.miResetViewClick(Sender: TObject);
begin
  XMin:= FSXMin;
  XMax:= FSXMax;
  YMin:= FSYMin;
  YMax:= FSYMax;
  RepaintBuffer;
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

