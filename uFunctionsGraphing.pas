unit uFunctionsGraphing;

interface

uses SysUtils, Classes, Graphics, uMath;

type
  TPlotRange = record
    XMin, XMax, YMin, YMax: Number;
  end;

  TPlotBase = class
  private
    FSize: Single;
    FColor: TColor;
    FContext: TContext;
  protected
    procedure PlotOptions(D: TDynamicArguments); virtual;
  public
    constructor Create;
    destructor Destroy; override;
    function GetRange: TPlotRange; virtual; abstract;
    function GetLegend: string; virtual; abstract;
    property Context: TContext read FContext;
    property Color: TColor read FColor;
    property Size: Single read FSize;
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
  published
    function Plot_3_opt(Context: TContext; args: TExprList; Options: TDynamicArguments): IValue;
    function Histogram_1_opt(Context: TContext; args: TExprList; Options: TDynamicArguments): IValue;
    function XYPlot_1_opt(Context: TContext; args: TExprList; Options: TDynamicArguments): IValue;
    function Show_1_opt(Context: TContext; args: TExprList; Options: TDynamicArguments): IValue;
  end;

implementation

uses Math, uGraphWindow, TypInfo;

{ TPackageGraph }

function TPackageGraph.Plot_3_opt(Context: TContext; args: TExprList; Options: TDynamicArguments): IValue;
var
  ex, v: IExpression;
  va: string;
  plot: TPlot;
  l: IValueList;
begin
  ex:= args[0];
  v:= args[1];
  if v.GetClassType <> TE_ExprRef then
    raise EMathSysError.Create('Function Plot requires a variable reference');
  va:= TE_ExprRef(v.GetObject).Name;
  if not Supports(args[2].Evaluate(Context), IValueList, l) then
    raise EMathSysError.Create('Function Plot requires a plot range');

  plot:= TPlot.Create(ex, va);
  try
    plot.FMin:= l.ListItem[0].GetNumber;
    plot.FMax:= l.ListItem[1].GetNumber;
    plot.FContext:= Context.Bake;
    plot.PlotOptions(Options);
    plot.PreCalculate;
  except
    FreeAndNil(Plot);
    raise;
  end;
  Result:= TValueObject.Create(plot);
end;

function TPackageGraph.Histogram_1_opt(Context: TContext; args: TExprList; Options: TDynamicArguments): IValue;
var
  l: IValueList;
  plot: THistogram;
begin
  if not TValue.CheckForTuples(args[0].Evaluate(Context), 2) then
    raise EMathSysError.Create('Function ListPlot requires a list of 2-tuples');

  Supports(args[0].Evaluate(Context), IValueList, l);
  plot:= THistogram.Create(l);
  plot.FContext:= Context.Bake;
  try
    plot.PlotOptions(Options);
  except
    FreeAndNil(Plot);
    raise;
  end;
  Result:= TValueObject.Create(plot);
end;

function TPackageGraph.XYPlot_1_opt(Context: TContext; args: TExprList; Options: TDynamicArguments): IValue;
var
  l: IValueList;
  plot: TXYPlot;
begin
  if not TValue.CheckForTuples(args[0].Evaluate(Context), 2) then
    raise EMathSysError.Create('Function XYPlot requires a list of 2-tuples');

  Supports(args[0].Evaluate(Context), IValueList, l);
  plot:= TXYPlot.Create(l);
  plot.FContext:= Context.Bake;
  try
    plot.PlotOptions(Options);
  except
    FreeAndNil(Plot);
    raise;
  end;
  Result:= TValueObject.Create(plot);
end;

function TPackageGraph.Show_1_opt(Context: TContext; args: TExprList; Options: TDynamicArguments): IValue;
var
  par: IValue;
  pl: IValueList;
  plots: array of IValueObject;
  gr: TGraphWindow;
  pr,PaintRange: TPlotRange;
  vo: IValueObject;
  i: integer;
  a, b, axes: string;
begin
  par:= args[0].Evaluate(Context);
  if Supports(par, IValueList, pl) then begin
    SetLength(Plots, pl.Length);
    for i:= 0 to high(plots) do
      if Supports(pl.ListItem[i], IValueObject, vo) and (vo.GetObject is TPlotBase) then begin
        plots[i]:= vo;
      end else
        raise EMathSysError.CreateFmt('Show: Element %d is not a plot object', [i]);
  end else
    if Supports(par, IValueObject, vo) and (vo.GetObject is TPlotBase) then begin
      SetLength(Plots, 1);
      plots[0]:= vo;
    end else
      raise EMathSysError.Create('Function Show requires a list of plot objects or a single plot object');

  gr:= TGraphWindow.CreateGraph(plots);

  if Options.IsSet('Axes') then begin
    axes:= Options.Value['Axes'].GetString;
    a:= Copy(LowerCase(axes), 1, 3);
    b:= Copy(LowerCase(axes), 4, 3);
    if a = 'lin' then
      gr.XScale:= smLin
    else if a = 'log' then
      gr.XScale:= smLog;
    if b = 'lin' then
      gr.YScale:= smLin
    else if b = 'log' then
      gr.YScale:= smLog;
  end else begin
    gr.XScale:= smLin;
    gr.YScale:= smLin;
  end;

  gr.XMin:= NAN;
  gr.XMax:= NAN;
  gr.YMin:= NAN;
  gr.YMax:= NAN;

  if Options.IsSet('XRange') and Supports(Options.Value['XRange'], IValueList, pl) then begin
    gr.XMin:= pl.ListItem[0].GetNumber;
    gr.XMax:= pl.ListItem[1].GetNumber;
  end;

  if Options.IsSet('YRange') and Supports(Options.Value['YRange'], IValueList, pl) then begin
    gr.YMin:= pl.ListItem[0].GetNumber;
    gr.YMax:= pl.ListItem[1].GetNumber;
  end;

  if IsNan(gr.XMin) or IsNan(gr.XMax) or IsNan(gr.YMin) or IsNan(gr.YMax) then begin
    PaintRange.XMin:= Infinity;
    PaintRange.YMin:= Infinity;
    PaintRange.XMax:= NegInfinity;
    PaintRange.YMax:= NegInfinity;
    for i:= 0 to high(Plots) do begin
      pr:= TPlot(plots[i].GetObject).GetRange;
      if pr.XMin < PaintRange.XMin then PaintRange.XMin:= pr.XMin;
      if pr.YMin < PaintRange.YMin then PaintRange.YMin:= pr.YMin;
      if pr.XMax > PaintRange.XMax then PaintRange.XMax:= pr.XMax;
      if pr.YMax > PaintRange.YMax then PaintRange.YMax:= pr.YMax;
    end;
    if IsNan(gr.XMin) then gr.XMin:= PaintRange.XMin;
    if IsNan(gr.XMax) then gr.XMax:= PaintRange.XMax;
    if IsNan(gr.YMin) then gr.YMin:= PaintRange.YMin;
    if IsNan(gr.YMax) then gr.YMax:= PaintRange.YMax;
  end;

  if gr.XScale = smLog then begin
    if (gr.XMin <= 0) or IsZero(gr.XMin) then begin
      gr.XMin:= 2000E-19;
      Context.System.Output.Hint('XRange Minimum %.3f <= 0, autocorrecting.', [gr.YMin]);
    end;
  end;

  if gr.YScale = smLog then begin
    if (gr.YMin <= 0) or IsZero(gr.YMin) then begin
      gr.YMin:= 2000E-19;
      Context.System.Output.Hint('YRange Minimum %.3f <= 0, autocorrecting.', [gr.YMin]);
    end;
  end;

  gr.Show;
end;

{ TPlotBase }

constructor TPlotBase.Create;
begin
  inherited;
  FSize:= 1;
  FColor:= Random($FFFFFF);
end;

destructor TPlotBase.Destroy;
begin
  FreeAndNil(FContext);
  inherited;
end;

procedure TPlotBase.PlotOptions(D: TDynamicArguments);
begin
  if D.IsSet('Color') then
    FColor:= StringToColor(D.Value['Color'].GetString);
  if D.IsSet('Size') then
    FSize:= D.Value['Size'].GetNumber;
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
  if FEx<>nil then
    Result:= FEx.StringForm
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
  FContext.Silent:= true;

  FCacheRange.XMin:= FMin;
  FCacheRange.XMax:= FMax;
  mi:= Infinity;
  ma:= NegInfinity;
  x:= FMin;
  dx:= (FMax-FMin)/(SAMPLES-1);
  while (x < FMax) or IsZero(x-FMax) do begin
    n:= ValueAt(x);
    if n > ma then ma:= n;
    if n < mi then mi:= n;
    x:= x + dx;
  end;
  FCacheRange.YMin:= mi;
  FCacheRange.YMax:= ma;
end;

function TPlot.ValueAt(Param: Number): Number;
begin
  FContext.DefineValue(Variable, TValue.Create(Param));
  Result:= Expression.Evaluate(FContext).GetNumber;
end;


{ THistogram }

constructor THistogram.Create(AList: IValueList);
var
  md,k: Number;
  v1,v2: IValue;
  i: integer;
begin
  inherited Create;
  FList:= AList;
  md:= Infinity;
  for i:= 0 to FList.Length-2 do begin
    v1:= (FList.ListItem[i] as IValueList).ListItem[0];
    v2:= (FList.ListItem[i+1] as IValueList).ListItem[0];
    k:= v2.GetNumber-v1.GetNumber;
    if k < md then
      md:= k;
  end;
  FSize:= md;
end;

function THistogram.GetRange: TPlotRange;
var
  i: integer;
  v: IValue;
  n: Number;
begin
  Result.XMin:= Infinity;
  Result.YMin:= 0;
  Result.XMax:= NegInfinity;
  Result.YMax:= 0;
  for i:= 0 to FList.Length - 1 do begin
    v:= (FList.ListItem[i] as IValueList).ListItem[0];
    n:= v.GetNumber;
    if Result.XMin>n then Result.XMin:= n;
    if Result.XMax<n then Result.XMax:= n;

    v:= (FList.ListItem[i] as IValueList).ListItem[1];
    n:= v.GetNumber;
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
  v: IValue;
  n: Number;
begin
  Result.XMin:= Infinity;
  Result.YMin:= Infinity;
  Result.XMax:= NegInfinity;
  Result.YMax:= NegInfinity;
  for i:= 0 to FList.Length - 1 do begin
    v:= (FList.ListItem[i] as IValueList).ListItem[0];
    n:= v.GetNumber;
    if IsNan(n) then
      continue;
    if Result.XMin>n then Result.XMin:= n;
    if Result.XMax<n then Result.XMax:= n;

    v:= (FList.ListItem[i] as IValueList).ListItem[1];
    n:= v.GetNumber;
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
    i:= GetEnumValue(TypeInfo(TXYPointStyle), 'ps' + D.Value['points'].GetString);
    if i < 0 then
      raise EMathSysError.CreateFmt('Invalid Point Style name: %s', [D.Value['points'].GetString])
    else
      FPoints:= TXYPointStyle(i)
  end;
  if D.IsSet('lines') then begin
    i:= GetEnumValue(TypeInfo(TXYLineStyle), 'ls' + D.Value['lines'].GetString);
    if i < 0 then
      raise EMathSysError.CreateFmt('Invalid Line Style name: %s', [D.Value['lines'].GetString])
    else
      FLines:= TXYLineStyle(i);
  end;
end;

initialization
  TFunctionPackage.RegisterPackage(TPackageGraph);
end.

