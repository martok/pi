unit uFunctionsGraphing;

interface

uses SysUtils, Classes, Graphics, uMath;

type
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
    function GetMinX: Number; virtual; abstract;
    function GetMaxX: Number; virtual; abstract;
    property Context: TContext read FContext;
    property Color: TColor read FColor;
    property Size: Single read FSize;
  end;

  TPlot = class(TPlotBase)
  private
    FEx: IExpression;
    FVar: string;
    FMin, FMax: Number;
  protected
    procedure PlotOptions(D: TDynamicArguments); override;
  public
    constructor Create(Ex: IExpression; Vari: string);
    property Expression: IExpression read FEx;
    property Variable: string read FVar;
    function GetMinX: Number; override;
    function GetMaxX: Number; override;
  end;

  TListStyle = (lsPoint, lsCross, lsBar);
  TListPlot = class(TPlotBase)
  private
    FList: IValueList;
    FStyle: TListStyle;
  protected
    procedure PlotOptions(D: TDynamicArguments); override;
  public
    constructor Create(AList: IValueList);
    property List: IValueList read FList;
    property Style: TListStyle read FStyle;
    function GetMinX: Number; override;
    function GetMaxX: Number; override;
  end;

  TPackageGraph = class(TFunctionPackage)
  published
    function Plot_3_opt(Context: TContext; args: TExprList; Options: TDynamicArguments): IValue;
    function ListPlot_1_opt(Context: TContext; args: TExprList; Options: TDynamicArguments): IValue;
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
  plot.FMin:= l.ListItem[0].GetNumber;
  plot.FMax:= l.ListItem[1].GetNumber;
  plot.FContext:= Context.Bake;
  try
    plot.PlotOptions(Options);
  except
    FreeAndNil(Plot);
  end;
  Result:= TValueObject.Create(plot);
end;

function TPackageGraph.ListPlot_1_opt(Context: TContext; args: TExprList; Options: TDynamicArguments): IValue;
var
  l, l2: IValueList;
  plot: TListPlot;
begin
  if not Supports(args[0].Evaluate(Context), IValueList, l) then
    raise EMathSysError.Create('Function ListPlot requires a list of 2-tuples');
  if l.Length > 0 then
    if not Supports(l.ListItem[0], IValueList, l2) or (l2.Length <> 2) then
      raise EMathSysError.Create('Function ListPlot requires a list of 2-tuples');
  plot:= TListPlot.Create(l);
  plot.FContext:= Context.Bake;
  try
    plot.PlotOptions(Options);
  except
    FreeAndNil(Plot);
  end;
  Result:= TValueObject.Create(plot);
end;

function TPackageGraph.Show_1_opt(Context: TContext; args: TExprList; Options: TDynamicArguments): IValue;
var
  par: IValue;
  pl: IValueList;
  plots: array of IValueObject;
  gr: TGraphWindow;
  vo: IValueObject;
  i: integer;
  n, mi, ma: Number;
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

  if Options.IsSet('XRange') and Supports(Options.Value['XRange'], IValueList, pl) then begin
    gr.XMin:= pl.ListItem[0].GetNumber;
    gr.XMax:= pl.ListItem[1].GetNumber;
  end else begin
    mi:= MaxExtended;
    ma:= MinExtended;
    for i:= 0 to high(plots) do begin
      n:= TPlot(plots[i].GetObject).GetMinX;
      if n < mi then
        mi:= n;
      n:= TPlot(plots[i].GetObject).GetMaxX;
      if n > ma then
        ma:= n;
    end;
    gr.XMin:= mi;
    gr.XMax:= ma;
  end;

  if Options.IsSet('YRange') and Supports(Options.Value['YRange'], IValueList, pl) then begin
    gr.YMin:= pl.ListItem[0].GetNumber;
    gr.YMax:= pl.ListItem[1].GetNumber;
  end else begin
    if gr.YScale = smLog then begin
      gr.YMin:= 0.1;
      gr.YMax:= 10;
    end else begin
      gr.YMin:= -5;
      gr.YMax:= 5;
    end;
  end;

  if gr.XScale = smLog then begin
    if (gr.XMin <= 0) or IsZero(gr.XMin) then begin
      gr.XMin:= 2000E-19;
      Context.System.Output.Hint('XRange Minimum %f <= 0, autocorrecting.', [gr.YMin]);
    end;
  end;

  if gr.YScale = smLog then begin
    if (gr.YMin <= 0) or IsZero(gr.YMin) then begin
      gr.YMin:= 2000E-19;
      Context.System.Output.Hint('YRange Minimum %f <= 0, autocorrecting.', [gr.YMin]);
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
  FEx:= Ex;
  FVar:= Vari;
  FMin:= -1;
  FMax:= 1;
end;

function TPlot.GetMaxX: Number;
begin
  Result:= FMax;
end;

function TPlot.GetMinX: Number;
begin
  Result:= FMin;
end;

procedure TPlot.PlotOptions(D: TDynamicArguments);
begin
  inherited;
end;

{ TListPlot }

constructor TListPlot.Create(AList: IValueList);
begin
  inherited Create;
  FList:= AList;
  FStyle:= lsCross;
end;

function TListPlot.GetMaxX: Number;
var
  i: integer;
  v: IValue;
begin
  Result:= MinExtended;
  for i:= 0 to FList.Length - 1 do begin
    v:= (FList.ListItem[i] as IValueList).ListItem[0];
    if v.GetNumber > Result then
      Result:= v.GetNumber;
  end;
  Result:= Result + FSize / 2;
end;

function TListPlot.GetMinX: Number;
var
  i: integer;
  v: IValue;
begin
  Result:= MaxExtended;
  for i:= 0 to FList.Length - 1 do begin
    v:= (FList.ListItem[i] as IValueList).ListItem[0];
    if v.GetNumber < Result then
      Result:= v.GetNumber;
  end;
  Result:= Result - FSize / 2;
end;

procedure TListPlot.PlotOptions(D: TDynamicArguments);
var
  i: integer;
begin
  inherited;
  if D.IsSet('style') then begin
    i:= GetEnumValue(TypeInfo(TListStyle), 'ls' + D.Value['style'].GetString);
    if i < 0 then
      raise EMathSysError.CreateFmt('Invalid Style name: %s', [D.Value['style'].GetString])
    else
      FStyle:= TListStyle(i)
  end;
end;

initialization
  TFunctionPackage.RegisterPackage(TPackageGraph);
end.

