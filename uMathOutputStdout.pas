unit uMathOutputStdout;

interface

uses
  SysUtils, ComCtrls, uMath, uMathIntf;

type
  TOutputStdOut = class(TIntfNoRefCount, IOutputProvider)
  private
    FRender: TRichEdit;
    procedure LineOut(const Line: string);
  public
    constructor Create;
    // IOutputProvider
    procedure Input(const Line: string);
    procedure Hint(const Line: string; Params: array of const);
    procedure Error(const Line: string; Params: array of const);
    procedure Result(const Line: string);
    procedure Clear;
  end;

implementation

uses
  StrUtils;


{ TOutputStdOut }

constructor TOutputStdOut.Create;
begin
  inherited;
end;

procedure TOutputStdOut.LineOut(const Line: string);
var
  s: String;
begin
  s:= StringReplace(Line, #160, '', [rfReplaceAll]);
  Writeln(s);
end;

procedure TOutputStdOut.Hint(const Line: string; Params: array of const);
begin
  LineOut('+ ' + Format(Line, Params));
end;

procedure TOutputStdOut.Error(const Line: string; Params: array of const);
begin
  LineOut('! ' + Format(Line, Params));
end;

procedure TOutputStdOut.Result(const Line: string);
begin
  LineOut('= ' + Line);
end;

procedure TOutputStdOut.Input(const Line: string);
begin
end;

procedure TOutputStdOut.Clear;
begin
  FRender.Clear;
end;



end.
