unit uChartExport;

interface

uses
  SysUtils, Graphics, Types, uChartPainter, Clipbrd;

type
  TChartExport = class
  private
    fCreatedSize: TSize;  
    fRefCanvas: TCanvas;
    fGraphic: TGraphic;
  protected
    procedure CreateGraphic; virtual; abstract;
    procedure PaintOnCanvas(const aCanvas: TCanvas; const aChart: TChartPainter);
  public
    constructor Create(const aRect: TRect; const aRefCanvas: TCanvas);
    procedure Paint(const aChart: TChartPainter); virtual;
    procedure ToClipboard;
    procedure ToFile(const aFile: string);
    destructor Destroy; override;
  end;

  TChartExportEMF = class(TChartExport)
  protected
    procedure CreateGraphic; override;
  public
    procedure Paint(const aChart: TChartPainter); override;
  end;

  TChartExportBMP = class(TChartExport)
  protected
    procedure CreateGraphic; override;
  public
    procedure Paint(const aChart: TChartPainter); override;
  end;

implementation

{ TChartExport }

constructor TChartExport.Create(const aRect: TRect; const aRefCanvas: TCanvas);
begin
  inherited Create;
  fGraphic:= nil;
  fCreatedSize.cx:= aRect.Right - aRect.Left;
  fCreatedSize.cy:= aRect.Bottom - aRect.Top;
  fRefCanvas:= aRefCanvas;
  CreateGraphic;
end;

destructor TChartExport.Destroy;
begin
  FreeAndNil(fGraphic);
  inherited;
end;

procedure TChartExport.Paint(const aChart: TChartPainter);
begin
end;

procedure TChartExport.PaintOnCanvas(const aCanvas: TCanvas; const aChart: TChartPainter);
var
  tg: TPaintTarget;
begin
  tg.Canvas:= aCanvas;
  tg.DrawRect:= Bounds(0, 0, fCreatedSize.cx, fCreatedSize.cy);
  aChart.PaintGraph(tg);
end;

procedure TChartExport.ToClipboard;
begin
  Clipboard.Assign(fGraphic);
end;

procedure TChartExport.ToFile(const aFile: string);
begin
  fGraphic.SaveToFile(aFile);
end;

{ TChartExportEMF }

procedure TChartExportEMF.CreateGraphic;
var
  emf: TMetafile;
begin
  emf:= TMetafile.Create;
  emf.Enhanced:= true;
  fGraphic:= emf;
end;

procedure TChartExportEMF.Paint(const aChart: TChartPainter);
var
  emfc: TMetafileCanvas;
begin
  fGraphic.SetSize(fCreatedSize.cx, fCreatedSize.cy);

  emfc:= TMetafileCanvas.Create(TMetafile(fGraphic), fRefCanvas.Handle);
  try
    PaintOnCanvas(emfc, aChart);
  finally
    emfc.Free;
  end;
end;

{ TChartExportBMP }

procedure TChartExportBMP.CreateGraphic;
var
  bmp: TBitmap;
begin
  bmp:= TBitmap.Create;  
  fGraphic:= bmp;
end;

procedure TChartExportBMP.Paint(const aChart: TChartPainter);
begin
  fGraphic.SetSize(fCreatedSize.cx, fCreatedSize.cy);

  PaintOnCanvas(TBitmap(fGraphic).Canvas, aChart);
end;

end.
