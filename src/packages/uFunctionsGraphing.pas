{-----------------------------------------------------------------------------
 Package: Graphing
-----------------------------------------------------------------------------}
unit uFunctionsGraphing;

interface

uses SysUtils, Classes, Graphics, uMathIntf, uMathValues, uMath,
  uFPUSupport;

type
  TPlotRange = record
    XMin, XMax, YMin, YMax: Number;
  end;

  IPlotObject = interface(IExpressionAtom)['{C5255A30-8718-4A84-9569-3CF8FE47F5C3}']
  end;
  TPlotsArray = array of IPlotObject;
  TColorArray = array of TColor;

  TPlotBase = class(TE_Atom, IPlotObject)
  private
    FSize: Single;
    FColor: TColor;
    FCaption: String;
    FContext: IContext;
    FAutoColor: TColor;
    function GetCaption: string;
    function GetColor: TColor;
  protected
    procedure PlotOptions(D: TDynamicArguments); virtual;
  public
    constructor Create;
    destructor Destroy; override;
    function Clone(Deep: Boolean): IExpression; override;
    function GetRange: TPlotRange; virtual; abstract;
    function GetLegend: string; virtual; abstract;
    property Context: IContext read FContext;
    property Color: TColor read GetColor;
    property AutomaticColor: TColor read FAutoColor write FAutoColor;
    property Size: Single read FSize;
    property Caption: string read GetCaption;
  end;

  TPlot = class(TPlotBase)
  private
    FEx: IExpression;
    FVar: string;
    FMin, FMax: Number;
    FCacheRange: TPlotRange;
  protected
    procedure PlotOptions(D: TDynamicArguments); override;
  public
    constructor Create(Ex: IExpression; Vari: string);
    property Expression: IExpression read FEx;
    property Variable: string read FVar;
    function ValueAt(Param: Number): Number;
    function GetRange: TPlotRange; override;
    function GetLegend: String; override;
    procedure PreCalculate;
  end;

  THistogram = class(TPlotBase)
  private
    FList: IValueList;
  protected
    procedure PlotOptions(D: TDynamicArguments); override;
  public
    constructor Create(AList: IValueList);
    function GetRange: TPlotRange; override;
    function GetLegend: String; override;
    property List: IValueList read FList;
  end;

  TXYPointStyle = (psNone, psDot, psCross, psPlus, psCircle, psSquare);
  TXYLineStyle = (lsNone, lsStraight, lsHoldX, lsHoldY, lsStepX, lsStepY);

  TXYPlot = class(TPlotBase)
  private
    FList: IValueList;
    FPoints: TXYPointStyle;
    FLines: TXYLineStyle;
  protected
    procedure PlotOptions(D: TDynamicArguments); override;
  public
    constructor Create(AList: IValueList);
    function GetRange: TPlotRange; override;
    function GetLegend: String; override;
    property List: IValueList read FList;
    property Points: TXYPointStyle read FPoints;
    property Lines: TXYLineStyle read FLines;
  end;

  TPackageGraph = class(TFunctionPackage)
  private
    fColorScheme: string;
    function GetColorTable: TColorArray;
  protected
    procedure OnImport(const MS: TMathSystem); override;
    function OnPragma(const MS: TMathSystem; const Name: string; const NewValue: IExpression): IExpression; override;
  published
    function Plot_3_opt(Context: IContext; args: TExprList; Options: TDynamicArguments): IExpression;
    function Histogram_1_opt(Context: IContext; args: TExprList; Options: TDynamicArguments): IExpression;
    function XYPlot_1_opt(Context: IContext; args: TExprList; Options: TDynamicArguments): IExpression;
    function Show_1_opt(Context: IContext; args: TExprList; Options: TDynamicArguments): IExpression;
  end;

implementation

uses Math, uGraphWindow, TypInfo, uChartScale;

{ TPackageGraph }

procedure TPackageGraph.OnImport(const MS: TMathSystem);
begin
  fColorScheme:= 'default';
end;

function TPackageGraph.OnPragma(const MS: TMathSystem; const Name: string; const NewValue: IExpression): IExpression;
begin
  if AnsiSameText(Name, 'ColorScheme') then begin
    if NewValue <> nil then
      fColorScheme:= (NewValue as IValueString).Value;
    Result:= TValueString.Create(fColorScheme);
  end;
end;

function TPackageGraph.Plot_3_opt(Context: IContext; args: TExprList; Options: TDynamicArguments): IExpression;
var
  ex: IExpression;
  v: ISymbolReference;
  va: string;
  plot: TPlot;
  l: IValueList;
begin
  ex:= args[0];
  if not args[1].Represents(ISymbolReference, v) then
    raise EMathSysError.Create('Function Plot requires a variable reference');
  va:= v.Name;
  if not args[2].Evaluate(Context).Represents(IValueList, l) then
    raise EMathSysError.Create('Function Plot requires a plot range');

  plot:= TPlot.Create(ex, va);
  try
    plot.FMin:= CastToFloat(l.Item[0]);
    plot.FMax:= CastToFloat(l.Item[1]);
    plot.FContext:= TContext(Context.NativeObject).Bake;
    plot.PlotOptions(Options);
    plot.PreCalculate;
    Result:= Plot as IExpression;
  except
    FreeAndNil(Plot);
    raise;
  end;
end;

function TPackageGraph.GetColorTable: TColorArray;
const
  COLORMAP_DEFAULT: array[0..17] of TColor = (
     $0000ff,
     $00ffff,
     $00ff00,
     $ffff00,
     $ff0000,
     $ff00ff,
     $808a43,
     $6f1083,
     $69fdbb,
     $027f19,
     $427bed,
     $d85489,
     $c5d663,
     $0dbf8c,
     $916afa,
     $a82209,
     $fb9b36,
     $2c358a);

var
  i: integer;
begin
  if AnsiSameText(fColorScheme, 'random')  then begin
    SetLength(Result, 50);
    for i := 0 to high(Result) do
      Result[i]:= Random($FFFFFF);      
  end else begin
    SetLength(Result, Length(COLORMAP_DEFAULT));
    Move(COLORMAP_DEFAULT[0],Result[0],Sizeof(COLORMAP_DEFAULT));
  end;
end;

function TPackageGraph.Histogram_1_opt(Context: IContext; args: TExprList; Options: TDynamicArguments): IExpression;
var
  l: IValueList;
  plot: THistogram;
begin
  if not (args[0].Evaluate(Context).Represents(IValueList, l) and CheckForTuples(l, 2)) then
    raise EMathSysError.Create('Function ListPlot requires a list of 2-tuples');

  plot:= THistogram.Create(l);
  plot.FContext:= TContext(Context.NativeObject).Bake;
  try
    plot.PlotOptions(Options);
    Result:= plot as IExpression;
  except
    FreeAndNil(Plot);
    raise;
  end;
end;

function TPackageGraph.XYPlot_1_opt(Context: IContext; args: TExprList; Options: TDynamicArguments): IExpression;
var
  l: IValueList;
  plot: TXYPlot;
begin
  if not (args[0].Evaluate(Context).Represents(IValueList, l) and CheckForTuples(l, 2)) then
    raise EMathSysError.Create('Function XYPlot requires a list of 2-tuples');

  plot:= TXYPlot.Create(l);
  plot.FContext:= TContext(Context.NativeObject).Bake;
  try
    plot.PlotOptions(Options);
    Result:= Plot as IExpression;
  except
    FreeAndNil(Plot);
    raise;
  end;
end;

function TPackageGraph.Show_1_opt(Context: IContext; args: TExprList; Options: TDynamicArguments): IExpression;
var
  par: IExpression;
  pl: IValueList;
  ps: IValueString;
  plots: TPlotsArray;
  gr: TGraphWindow;
  pr,PaintRange: TPlotRange;
  vo: IPlotObject;
  i: integer;
  a, b, axes: string;
begin
  par:= args[0].Evaluate(Context);
  if par.Represents(IValueList, pl) then begin
    SetLength(Plots, pl.Length);
    for i:= 0 to high(plots) do
      if pl.Item[i].Represents(IPlotObject, vo) then begin
        plots[i]:= vo;
      end else
        raise EMathSysError.CreateFmt('Show: Element %d is not a plot object', [i]);
  end else
    if par.Represents(IPlotObject, vo) then begin
      SetLength(Plots, 1);
      plots[0]:= vo;
    end else
      raise EMathSysError.Create('Function Show requires a list of plot objects or a single plot object');

  gr:= TGraphWindow.CreateGraph(plots, GetColorTable);

  if Options.IsSet('Axes') then begin
    axes:= CastToString(Options.Value['Axes']);
    a:= Copy(LowerCase(axes), 1, 3);
    b:= Copy(LowerCase(axes), 4, 3);
    i:= GetEnumValue(TypeInfo(TScaleMode),'sm'+a);
    if i<0 then
      raise EMathSysError.Create('Invalid Axis Mode: '+a);
    gr.XScale:= TScaleMode(i);
    i:= GetEnumValue(TypeInfo(TScaleMode),'sm'+b);
    if i<0 then
      raise EMathSysError.Create('Invalid Axis Mode: '+b);
    gr.YScale:= TScaleMode(i);
  end else begin
    gr.XScale:= smLin;
    gr.YScale:= smLin;
  end;

  gr.XMin:= NAN;
  gr.XMax:= NAN;
  gr.YMin:= NAN;
  gr.YMax:= NAN;

  if Options.IsSet('XRange') and Supports(Options.Value['XRange'], IValueList, pl) then begin
    gr.XMin:= CastToFloat(pl.Item[0]);
    gr.XMax:= CastToFloat(pl.Item[1]);
  end;

  if Options.IsSet('YRange') and Supports(Options.Value['YRange'], IValueList, pl) then begin
    gr.YMin:= CastToFloat(pl.Item[0]);
    gr.YMax:= CastToFloat(pl.Item[1]);
  end;       
  if Options.IsSet('Title') and Supports(Options.Value['Title'], IValueString, ps) then begin
    gr.Title:= ps.Value;
  end;
  if Options.IsSet('XLabel') and Supports(Options.Value['XLabel'], IValueString, ps) then begin
    gr.XLabel:= ps.Value;
  end;
  if Options.IsSet('YLabel') and Supports(Options.Value['YLabel'], IValueString, ps) then begin
    gr.YLabel:= ps.Value;
  end;

  if IsNan(gr.XMin) or IsNan(gr.XMax) or IsNan(gr.YMin) or IsNan(gr.YMax) then begin
    PaintRange.XMin:= Infinity;
    PaintRange.YMin:= Infinity;
    PaintRange.XMax:= NegInfinity;
    PaintRange.YMax:= NegInfinity;
    for i:= 0 to high(Plots) do begin
      pr:= TPlot(plots[i].NativeObject).GetRange;
      if pr.XMin < PaintRange.XMin then PaintRange.XMin:= pr.XMin;
      if pr.YMin < PaintRange.YMin then PaintRange.YMin:= pr.YMin;
      if pr.XMax > PaintRange.XMax then PaintRange.XMax:= pr.XMax;
      if pr.YMax > PaintRange.YMax then PaintRange.YMax:= pr.YMax;
    end;
    if IsNan(gr.XMin) then gr.XMin:= PaintRange.XMin;
    if IsNan(gr.XMax) then gr.XMax:= PaintRange.XMax;
    if IsNan(gr.YMin) then gr.YMin:= PaintRange.YMin * 1.05;
    if IsNan(gr.YMax) then gr.YMax:= PaintRange.YMax * 1.05;
  end;

  if gr.XScale = smLog then begin
    if (gr.XMin <= 0) or fzero(gr.XMin) then begin
      gr.XMin:= 2000E-19;
      Context.Output.Hint('XRange Minimum %.3f <= 0, autocorrecting.', [gr.YMin]);
    end;
  end;

  if gr.YScale = smLog then begin
    if (gr.YMin <= 0) or fzero(gr.YMin) then begin
      gr.YMin:= 2000E-19;
      Context.Output.Hint('YRange Minimum %.3f <= 0, autocorrecting.', [gr.YMin]);
    end;
  end;

  gr.ShowDockable;
  Context.Output.Result('['+gr.Caption+']');
end;

{ TPlotBase }

function TPlotBase.Clone(Deep: Boolean): IExpression;
begin
  raise EMathTypeError.CreateFmt('Cannot modify object of class %s.',[ClassName]);
end;

constructor TPlotBase.Create;
begin
  inherited;
  FSize:= 1;
  FColor:= clNone;
  FAutoColor:= Random($FFFFFF);
  FCaption:= '';
end;

destructor TPlotBase.Destroy;
begin
  inherited;
end;

function TPlotBase.GetCaption: string;
begin
  if FCaption>'' then
    Result:= FCaption
  else
    Result:= GetLegend;
end;

function TPlotBase.GetColor: TColor;
begin
  if FColor<>clNone then
    Result:= FColor
  else
    Result:= FAutoColor;
end;

procedure TPlotBase.PlotOptions(D: TDynamicArguments);
begin
  if D.IsSet('Color') then
    FColor:= StringToColor(CastToString(D.Value['Color']));
  if D.IsSet('Size') then
    FSize:= CastToFloat(D.Value['Size']);
  if D.IsSet('Caption') then
    FCaption:= CastToString(D.Value['Caption']);
end;

{ TPlot }

constructor TPlot.Create(Ex: IExpression; Vari: string);
begin
  inherited Create;
  FCacheRange.XMin:= NaN;
  FEx:= Ex;
  FVar:= Vari;
  FMin:= -1;
  FMax:= 1;
end;

function TPlot.GetLegend: String;
begin
  if FEx<>nil then begin
    Result:= FEx.AsString(STR_FORM_STANDARD);
  end
  else
    Result:= '-';
end;

function TPlot.GetRange: TPlotRange;
begin
  Result:= FCacheRange;
end;

procedure TPlot.PlotOptions(D: TDynamicArguments);
begin
  inherited;
end;

procedure TPlot.PreCalculate;
const
  SAMPLES = 500;
var
  mi, ma, n, x, dx: Number;
begin
  FContext.SetSilent(true);

  FCacheRange.XMin:= FMin;
  FCacheRange.XMax:= FMax;
  mi:= Infinity;
  ma:= NegInfinity;
  x:= FMin;
  dx:= (FMax-FMin)/(SAMPLES-1);
  while (x < FMax) or fzero(x-FMax) do begin
    n:= ValueAt(x);
    if not IsNan(n) then begin
      if n > ma then ma:= n;
      if n < mi then mi:= n;
    end;
    x:= x + dx;
  end;
  FCacheRange.YMin:= mi;
  FCacheRange.YMax:= ma;
end;

function TPlot.ValueAt(Param: Number): Number;
var
  v: IValueNumber;
begin
  v:= TValueFactory.Float(Param);
  FContext.Define(Variable, v);
  Result:= EvaluateToFloat(FContext, FEx);
end;


{ THistogram }

constructor THistogram.Create(AList: IValueList);
var
  md,k: Number;
  v1,v2: IExpression;
  i: integer;
begin
  inherited Create;
  FList:= AList;
  md:= Infinity;
  for i:= 0 to FList.Length-2 do begin
    v1:= (FList.Item[i] as IValueList).Item[0];
    v2:= (FList.Item[i+1] as IValueList).Item[0];
    k:= CastToFloat(v2)-CastToFloat(v1);
    if k < md then
      md:= k;
  end;
  FSize:= md;
end;

function THistogram.GetRange: TPlotRange;
var
  i: integer;
  v: IExpression;
  n: Number;
begin
  Result.XMin:= Infinity;
  Result.YMin:= 0;
  Result.XMax:= NegInfinity;
  Result.YMax:= 0;
  for i:= 0 to FList.Length - 1 do begin
    v:= (FList.Item[i] as IValueList).Item[0];
    n:= CastToFloat(v);
    if Result.XMin>n then Result.XMin:= n;
    if Result.XMax<n then Result.XMax:= n;

    v:= (FList.Item[i] as IValueList).Item[1];
    n:= CastToFloat(v);
    if Result.YMin>n then Result.YMin:= n;
    if Result.YMax<n then Result.YMax:= n;
  end;
  Result.XMin:= Result.XMin - FSize / 2;
  Result.XMax:= Result.XMax + FSize / 2;
end;

function THistogram.GetLegend: String;
begin
  Result:= Format('{%d items}', [FList.Length]);
end;

procedure THistogram.PlotOptions(D: TDynamicArguments);
begin
  inherited;
end;

{ TXYPlot }

constructor TXYPlot.Create(AList: IValueList);
begin
  inherited Create;
  FList:= AList;
  FPoints:= psCross;
  FLines:= lsNone;
end;

function TXYPlot.GetLegend: String;
begin
  Result:= Format('{%d items}', [FList.Length]);
end;

function TXYPlot.GetRange: TPlotRange;
var
  i: integer;
  v: IExpression;
  n: Number;
begin
  Result.XMin:= Infinity;
  Result.YMin:= Infinity;
  Result.XMax:= NegInfinity;
  Result.YMax:= NegInfinity;
  for i:= 0 to FList.Length - 1 do begin
    v:= (FList.Item[i] as IValueList).Item[0];
    n:= CastToFloat(v);
    if IsNan(n) then
      continue;
    if Result.XMin>n then Result.XMin:= n;
    if Result.XMax<n then Result.XMax:= n;

    v:= (FList.Item[i] as IValueList).Item[1];
    n:= CastToFloat(v);
    if IsNan(n) then
      continue;
    if Result.YMin>n then Result.YMin:= n;
    if Result.YMax<n then Result.YMax:= n;
  end;
end;

procedure TXYPlot.PlotOptions(D: TDynamicArguments);
var
  i: integer;
begin
  inherited;
  if D.IsSet('points') then begin
    i:= GetEnumValue(TypeInfo(TXYPointStyle), 'ps' + CastToString(D.Value['points']));
    if i < 0 then
      raise EMathSysError.CreateFmt('Invalid Point Style name: %s', [CastToString(D.Value['points'])])
    else
      FPoints:= TXYPointStyle(i)
  end;
  if D.IsSet('lines') then begin
    i:= GetEnumValue(TypeInfo(TXYLineStyle), 'ls' + CastToString(D.Value['lines']));
    if i < 0 then
      raise EMathSysError.CreateFmt('Invalid Line Style name: %s', [CastToString(D.Value['lines'])])
    else
      FLines:= TXYLineStyle(i);
  end;
end;

end.

