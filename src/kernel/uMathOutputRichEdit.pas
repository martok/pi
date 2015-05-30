{-----------------------------------------------------------------------------
 Output Provider for RichTextBox
-----------------------------------------------------------------------------}
unit uMathOutputRichEdit;

interface

uses
  SysUtils, ComCtrls, Graphics, Windows, Messages, uMath, uMathIntf;

type
  TOutputRichEdit = class(TIntfNoRefCount, IOutputProvider)
  private
    FRender: TRichEdit;
    procedure LineOut(const Line: string; Indent: integer; Color: TColor; Style: TFontStyles);
  public
    constructor Create;
    property Render: TRichEdit read FRender write FRender;
    // IOutputProvider
    procedure Input(const Line: string);
    procedure Hint(const Line: string; Params: array of const);
    procedure Error(const Line: string; Params: array of const);
    procedure Result(const Line: string);
    procedure Clear;
  end;

implementation


{ TOutputRichEdit }

constructor TOutputRichEdit.Create;
begin
  inherited;
  FRender:= nil;
end;

procedure TOutputRichEdit.LineOut(const Line: string; Indent: integer; Color: TColor; Style: TFontStyles);
var
  p: integer;
begin
  FRender.SelStart:= length(FRender.Text);
  FRender.SelLength:= 0;
  if Indent < 0 then begin
    FRender.Paragraph.FirstIndent:= 0;
    FRender.Paragraph.LeftIndent:= -Indent;
    FRender.Paragraph.TabCount:= 1;
    FRender.Paragraph.Tab[0]:= -Indent;
  end else begin
    FRender.Paragraph.FirstIndent:= Indent;
    FRender.Paragraph.LeftIndent:= 0;
    FRender.Paragraph.TabCount:= 0;
  end;
  FRender.SelAttributes.Color:= Color;
  FRender.SelAttributes.Style:= Style;
  FRender.Lines.Add(Line);
  p:= Pos(NeutralFormatSettings.ThousandSeparator, FRender.Text);
  while p > 0 do begin
    FRender.SelStart:= p - 1;
    FRender.SelLength:= 1;
    FRender.SelAttributes.Name:= 'Arial';
    FRender.SelText:= ' ';
    p:= Pos(NeutralFormatSettings.ThousandSeparator, FRender.Text);
  end;
  PostMessage(FRender.Handle, EM_SCROLLCARET, 0, 0);
end;

procedure TOutputRichEdit.Hint(const Line: string; Params: array of const);
begin
  LineOut(Format(Line, Params), 10, clNavy, [fsItalic]);
end;

procedure TOutputRichEdit.Error(const Line: string; Params: array of const);
begin
  LineOut(Format(Line, Params), 10, $009EFF, []);
end;

procedure TOutputRichEdit.Result(const Line: string);
begin
  LineOut('='#9 + Line, -10, FRender.Font.Color, []);
end;

procedure TOutputRichEdit.Input(const Line: string);
var
  s: string;
begin
  s:= '>'#9 + Line;
  if FRender.Lines.Count > 0 then
    s:= #13#10 + s;
  LineOut(s, -10, clDkGray, []);
end;

procedure TOutputRichEdit.Clear;
begin
  FRender.Clear;
end;



end.
