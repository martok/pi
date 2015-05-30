unit uChartScale;

interface

uses
  SysUtils, uMathIntf, uMath, Math, uFPUSupport;

type
  TScaleMode = (smLin, smLog);

  TScale = class
  private
    procedure SetFrameMax(const Value: integer);
    procedure SetFrameMin(const Value: integer);
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
    property FrameMin: integer read FFrameMin write SetFrameMin;
    property FrameMax: integer read FFrameMax write SetFrameMax;
    property Min: Number read FMinVal;
    property Max: Number read FMaxVal;
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

implementation


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

procedure TScale.SetFrameMin(const Value: integer);
begin                    
  if Value = FFrameMin then exit; 
  FFrameMin := Value;
  Init;
end;

procedure TScale.SetFrameMax(const Value: integer);
begin
  if Value = FFrameMax then exit;  
  FFrameMax := Value;
  Init;
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

end.
