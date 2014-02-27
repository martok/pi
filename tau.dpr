{-----------------------------------------------------------------------------
 Text-Mode interface to TAU.
-----------------------------------------------------------------------------}
program tau;

{$APPTYPE CONSOLE}

uses
  SysUtils,
  uMath in 'uMath.pas',
  uMathIntf in 'uMathIntf.pas',
  uMathOutputStdout in 'uMathOutputStdout.pas',
  uMathValues in 'uMathValues.pas',
  uMathDimensions in 'uMathDimensions.pas',  
  uMathConstants in 'uMathConstants.pas',
  uFunctions in 'uFunctions.pas'{,
  uFunctionsSymbolics in 'uFunctionsSymbolics.pas',
  uFunctionsStatistics in 'uFunctionsStatistics.pas',
  uExpressionMatcher in 'uExpressionMatcher.pas'};

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
    mk.RegisterPackage(TPackageDimensions.Create);
    mk.RegisterPackage(TPackageTrig.Create);
    mk.RegisterPackage(TPackageElementary.Create);
    mk.RegisterPackage(TPackageNumerical.Create);
    mk.RegisterPackage(TPackageLists.Create);
    mk.RegisterPackage(TPackageData.Create);
   { mk.RegisterPackage(TPackageStatistics.Create);
    mk.RegisterPackage(TPackageSymbolics.Create); }
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
