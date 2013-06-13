program tau;

{$APPTYPE CONSOLE}

uses
  SysUtils,
  uMath in 'uMath.pas',
  uMathIntf in 'uMathIntf.pas',
  uMathOutputStdout in 'uMathOutputStdout.pas',
  uMathValues in 'uMathValues.pas';

function GetInput: string;
var
  s: string;
begin
  Write('> ');
  Result:= '';
  Readln(s);
  while (s>'') and (s[length(s)]='\') do begin
    Result:= Result + Copy(s,1,length(s)-1);
    Readln(s);
  end;
  Result:= Result + s;
end;

var
  cmd: string;
  mk: TMathSystem;
begin
  mk:= TMathSystem.Create(TOutputStdOut.Create);
  try
    cmd:= GetInput;
    while cmd>'' do begin
      Writeln('');

      try
        mk.Run(cmd);
      except
        on e: Exception do
          mk.Output.Error(E.Message, []);
      end;

      Writeln;    
      cmd:= GetInput;
    end;
  finally
    FreeAndNil(mk);
  end;
end.
