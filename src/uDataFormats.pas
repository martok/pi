unit uDataFormats;

interface

uses
  SysUtils, Classes, uMathIntf, uMath, uMathValues,
  uCCSVList;

type
  TFormatImportExport = class
  private
    FExtensions: TStringList;
    FFormatName: string;
  public
    constructor Create;
    destructor Destroy; override;
    property FormatName: string read FFormatName;
    function ExtensionKnown(Ext: String): boolean;
    // Return false means unsupported
    function Save(const Stream: TStream; const Subject: IExpression; const Options: TDynamicArguments):boolean; virtual;
    function Load(const Stream: TStream; out Subject: IExpression; const Options: TDynamicArguments):boolean; virtual;
  end;

  TFormatString = class(TFormatImportExport)
  public
    constructor Create;
    function Save(const Stream: TStream; const Subject: IExpression; const Options: TDynamicArguments): Boolean; override;
    function Load(const Stream: TStream; out Subject: IExpression; const Options: TDynamicArguments): Boolean; override;
  end;

  TFormatText = class(TFormatImportExport)
  public
    constructor Create;
    function Save(const Stream: TStream; const Subject: IExpression; const Options: TDynamicArguments): Boolean; override;
    function Load(const Stream: TStream; out Subject: IExpression; const Options: TDynamicArguments): Boolean; override;
  end;

  TFormatCSV = class(TFormatImportExport)
  private
    procedure TakeCSVParameters(Options: TDynamicArguments; list,
      line: TCSVStringList; var fs: TFormatSettings);
  public
    constructor Create;
    function Save(const Stream: TStream; const Subject: IExpression; const Options: TDynamicArguments): Boolean; override;
    function Load(const Stream: TStream; out Subject: IExpression; const Options: TDynamicArguments): Boolean; override;
  end;

implementation

{ TFormatImportExport }

constructor TFormatImportExport.Create;
begin
  inherited Create;
  FExtensions:= TStringList.Create;
  FExtensions.Sorted:= true;
  FExtensions.CaseSensitive:= false;
  FExtensions.Duplicates:= dupIgnore;
end;

destructor TFormatImportExport.Destroy;
begin
  FreeAndNil(FExtensions);
  inherited;
end;

function TFormatImportExport.ExtensionKnown(Ext: String): boolean;
begin
  Result:= FExtensions.IndexOf(Ext) >= 0;
end;

function TFormatImportExport.Load(const Stream: TStream; out Subject: IExpression; const Options: TDynamicArguments): boolean;
begin
  Result:= false;
end;

function TFormatImportExport.Save(const Stream: TStream; const Subject: IExpression; const Options: TDynamicArguments): boolean;
begin
  Result:= false;
end;

{ TFormatString }

constructor TFormatString.Create;
begin
  inherited Create;
  FFormatName:= 'String';
end;

function TFormatString.Load(const Stream: TStream; out Subject: IExpression; const Options: TDynamicArguments): Boolean;
var
  ss: TStringStream;
begin
  Result:= true;
  if Stream is TStringStream then
    Subject:= TValueString.Create(TStringStream(Stream).DataString)
  else begin
    ss:= TStringStream.Create('');
    try
      ss.CopyFrom(Stream,-1);
      Subject:= TValueString.Create(ss.DataString);
    finally
      FreeAndNil(ss);
    end;
  end;
end;

function TFormatString.Save(const Stream: TStream; const Subject: IExpression; const Options: TDynamicArguments): Boolean;
var
  s: string;
begin
  Result:= true;
  s:= Subject.AsString(STR_FORM_EXPORT);
  Stream.WriteBuffer(s[1], Length(s));
end;

{ TFormatTextFile }

constructor TFormatText.Create;
begin
  inherited Create;
  FFormatName:= 'Text';
  FExtensions.Add('.txt');
  FExtensions.Add('.log');
end;

function TFormatText.Load(const Stream: TStream; out Subject: IExpression; const Options: TDynamicArguments): Boolean;
var
  sl: TStringList;
  res: IValueList;
  i: integer;
begin
  Result:= true;
  sl:= TStringList.Create;
  try
    sl.LoadFromStream(Stream);

    res:= TValueList.Create;
    res.Length:= sl.Count;
    for i:= 0 to sl.Count - 1 do begin
      res.Item[i]:= TValueString.Create(sl[i]);
    end;

    Subject:= res as IExpression;
  finally
    FreeAndNil(sl);
  end;
end;

function TFormatText.Save(const Stream: TStream; const Subject: IExpression; const Options: TDynamicArguments): Boolean;
var
  sl: TStringList; 
  lst: IValueList;
  i: integer;
begin
  Result:= true;
  sl:= TStringList.Create;
  try
    if Subject.Represents(IValueList, lst) then begin
      for i := 0 to lst.Length - 1 do
        sl.Add(lst.Item[i].AsString(STR_FORM_EXPORT));
    end else
      sl.Add(Subject.AsString(STR_FORM_EXPORT));
    sl.SaveToStream(Stream);
  finally
    FreeAndNil(sl);
  end;
end;


{ TFormatCSV }

constructor TFormatCSV.Create;
begin
  inherited Create;
  FFormatName:= 'CSV';
  FExtensions.Add('.csv');
  FExtensions.Add('.log');
end;

procedure TFormatCSV.TakeCSVParameters(Options: TDynamicArguments; list,
  line: TCSVStringList; var fs: TFormatSettings);
var
  d: string;
begin
  line.Delimiter:= ';';
  line.QuoteChar:= '"';

  if Options.IsSet('Delimiter') then begin
    d:= CastToString(Options['Delimiter']);
    if d > '' then
      line.Delimiter:= d[1];
  end;
  if Options.IsSet('QuoteChar') then begin
    d:= CastToString(Options['QuoteChar']);
    if d > '' then
      line.QuoteChar:= d[1];
  end;
  if Options.IsSet('Decimal') then begin
    d:= CastToString(Options['Decimal']);
    if d > '' then
      fs.DecimalSeparator:= d[1];
  end;   
  line.ForceQuoteWords:= Options.IsSet('AlwaysQuote');
end;

function TFormatCSV.Load(const Stream: TStream; out Subject: IExpression; const Options: TDynamicArguments): Boolean;
var
  list, line: TCSVStringList;
  i, j, k, first, last: integer;
  res, row: IValueList;
  cn: IExpression;
  o_fs: TFormatSettings;
  skip, count: integer;
  ignempt: boolean;
begin
  Result:= true;

  list:= TCSVStringList.Create;
  line:= TCSVStringList.Create;
  o_fs:= NeutralFormatSettings;
  try
    TakeCSVParameters(Options, list, line, NeutralFormatSettings);

    list.LoadFromStream(Stream);

    if Options.IsSet('Skip') then
      skip:= CastToInteger(Options['Skip'])
    else
      skip:= 0;

    ignempt:= Options.IsSet('IgnoreEmpty');

    first:= skip;
    if Options.IsSet('Count') then begin
      count:= CastToInteger(Options['Count']);
      last:= first + count - 1;
      if last>List.Count-1 then
        last:= List.Count-1;
    end else
      last:= List.Count - 1;

    if last < 0 then
      last:= 0;

    res:= TValueList.Create;
    res.Length:= last - first + 1;
    for i:= first to last do begin
      line.StrictDelimitedText:= list[i];
      row:= TValueList.Create;
      k:= 0;
      for j:= 0 to line.Count - 1 do begin
        if (Line[j] = '') and ignempt then
          continue;
        cn:= TValueFactory.GuessFromString(Line[j], NeutralFormatSettings, #0);
        row.Length:= k + 1;
        row.Item[k]:= cn;
        inc(k);
      end;
      res.Item[i - first]:= row as IExpression;
    end;

    Subject:= res as IExpression;
  finally
    NeutralFormatSettings:= o_fs;
    FreeAndNil(list);
    FreeAndNil(line);
  end;
end;

function TFormatCSV.Save(const Stream: TStream; const Subject: IExpression; const Options: TDynamicArguments): Boolean;
var
  list, line: TCSVStringList;
  i, j: integer;
  items, row: IValueList;
  o_fs: TFormatSettings;

  function ToString(x: IExpression): string;
  begin
    Result:= x.AsString(STR_FORM_EXPORT);
  end;

begin
  Result:= true;

  if not Subject.Represents(IValueList, items) then
    raise EMathSysError.Create('Format CSV requires a list.');

  list:= TCSVStringList.Create;
  line:= TCSVStringList.Create;
  o_fs:= NeutralFormatSettings;
  try
    TakeCSVParameters(Options, list, line, NeutralFormatSettings);

    for i := 0 to items.Length - 1 do begin
      line.Clear;
      if items.Item[i].Represents(IValueList, row) then begin
        for j := 0 to row.Length - 1 do begin
          line.Add(ToString(row.Item[j]));
        end;
      end else
        line.Add(ToString(items.Item[i]));
      list.add(line.StrictDelimitedText);
    end;

    list.SaveToStream(Stream);
  finally
    NeutralFormatSettings:= o_fs;
    FreeAndNil(list);
    FreeAndNil(line);
  end;
end;

end.
