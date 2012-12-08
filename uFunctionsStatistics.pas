unit uFunctionsStatistics;

interface

uses SysUtils, Classes, Graphics, uMath, Math;

type
  TPackageStatistics = class(TFunctionPackage)
  published
    // http://en.wikipedia.org/wiki/Error_function
    function Erf_1(Context: TContext; args: TExprList): IValue;
    function ErfC_1(Context: TContext; args: TExprList): IValue;

    // f(x,my,sigma^2) = 1/(sigma sqrt(2pi)) exp(-(x-my)²/(2sigma²))
    function NormPDF_3(Context: TContext; args: TExprList): IValue;
    function StdNormPDF_1(Context: TContext; args: TExprList): IValue;  //phi
    // PHI(x) = 1/2 * (1 + erf(x/sqrt(2)) )
    function StdNormCDF_1(Context: TContext; args: TExprList): IValue;  //PHI

    // ^PHI http://en.wikipedia.org/wiki/Probit_function
    //      http://home.online.no/~pjacklam/notes/invnorm/
    function InvStdNormCDF_1(Context: TContext; args: TExprList): IValue;

    function StdNormRand_0(Context: TContext; args: TExprList): IValue;
  end;

implementation

function fac(const X: Int64): Int64;
var
  k: Cardinal;
begin
  Result:= 1;
  for k:= 2 to X do
    Result:= Result * k;
end;

function erf(const X: Number): Number;
//  Abramowitz & Stegun, Handbook of Mathematical Functions
//  maximum error: 3E-7
begin
  if IsZero(X) then
    Result:= 0
  else
  if X >=0 then begin
    Result:= 1 - 1 / Power(
      1 +
      (0.0705230784 * X) +
      (0.0422820123 * X*X) +
      (0.0092705272 * X*X*X) +
      (0.0001520143 * X*X*X*X) +
      (0.0002765672 * X*X*X*X*X) +
      (0.0000430638 * X*X*X*X*X*X)
    ,16)
  end else
    Result:= -erf(-X);
end;

function erfc(const X: Number): Number;
begin
  Result:= 1 - erf(X);
end;

function stdnormdist(const X: Number): Number;
begin
  Result:= 1 / sqrt(2*pi) * exp(-0.5*X*X);
end;

function invstdnormdist(const P: Number): Number;
// function obtained from P. J. Acklam, Delphi by Greg McCormick
// http://home.online.no/~pjacklam/notes/invnorm/index.html
//  maximum error: 1.15E-9

// Coefficients in rational approximations.
const a: array[1..6] of double=(
  -3.969683028665376e+01,
   2.209460984245205e+02,
  -2.759285104469687e+02,
   1.383577518672690e+02,
  -3.066479806614716e+01,
   2.506628277459239e+00 );

const b: array[1..5] of double =(
  -5.447609879822406e+01,
   1.615858368580409e+02,
  -1.556989798598866e+02,
   6.680131188771972e+01,
  -1.328068155288572e+01  );

const c: array[1..6] of double=(
  -7.784894002430293e-03,
  -3.223964580411365e-01,
  -2.400758277161838e+00,
  -2.549732539343734e+00,
   4.374664141464968e+00,
   2.938163982698783e+00  );

const d: array[1..4] of double=(
  7.784695709041462e-03,
  3.224671290700398e-01,
  2.445134137142996e+00,
  3.754408661907416e+00 );

//Define break-points.
const p_low = 0.02425;
const p_high = 1 - p_low;

var
 q,r,e,u: Number;
begin
  if (p <= 0) or (p >= 1) then
    Result:= NaN
  else begin
    if (p<p_low) then begin
      //Rational approximation for lower region.
      q := sqrt(-2*ln(p));
      Result := (((((c[1]*q+c[2])*q+c[3])*q+c[4])*q+c[5])*q+c[6]) /
                ((((d[1]*q+d[2])*q+d[3])*q+d[4])*q+1);
    end else
    if p <= p_high then begin
      //Rational approximation for central region.
      q := p - 0.5 ;
      r := q*q ;
      Result := (((((a[1]*r+a[2])*r+a[3])*r+a[4])*r+a[5])*r+a[6])*q /
                (((((b[1]*r+b[2])*r+b[3])*r+b[4])*r+b[5])*r+1) ;
    end else
    if p < 1  then begin
      //Rational approximation for upper region.
      q := sqrt(-2*ln(1-p));
      Result := -(((((c[1]*q+c[2])*q+c[3])*q+c[4])*q+c[5])*q+c[6]) /
                 ((((d[1]*q+d[2])*q+d[3])*q+d[4])*q+1);
    end;

    //lock on to full precision using Halley
    if not IsNan(Result)then begin
      e:= 0.5 * erfc(-Result/sqrt(2)) - P;
      u:= e * sqrt(2*pi) * exp(Result*Result/2);
      Result:= Result - u/(1 + Result*u/2);
    end;
  end;
end;


{ TPackageStatistics }

function TPackageStatistics.Erf_1(Context: TContext; args: TExprList): IValue;
begin
  Result:= TValue.Create(erf(args[0].Evaluate(Context).GetNumber));
end;

function TPackageStatistics.ErfC_1(Context: TContext; args: TExprList): IValue;
begin
  Result:= TValue.Create(erfc(args[0].Evaluate(Context).GetNumber));
end;

function TPackageStatistics.StdNormCDF_1(Context: TContext; args: TExprList): IValue;
begin
  Result:= TValue.Create(0.5 * (1 + erf(args[0].Evaluate(Context).GetNumber/sqrt(2))));
end;

function TPackageStatistics.StdNormPDF_1(Context: TContext; args: TExprList): IValue;
begin
  Result:= TValue.Create(stdnormdist(args[0].Evaluate(Context).GetNumber));
end;

function TPackageStatistics.NormPDF_3(Context: TContext; args: TExprList): IValue;
var
  x,s,u: Number;
begin
  x:= args[0].Evaluate(Context).GetNumber;
  u:= args[1].Evaluate(Context).GetNumber;
  s:= args[2].Evaluate(Context).GetNumber;
  Result:= TValue.Create(1/s * stdnormdist((x-u)/s));
end;

function TPackageStatistics.InvStdNormCDF_1(Context: TContext; args: TExprList): IValue;
begin
  Result:= TValue.Create(invstdnormdist(args[0].Evaluate(Context).GetNumber));
end;

function TPackageStatistics.StdNormRand_0(Context: TContext; args: TExprList): IValue;
begin
  Result:= TValue.Create(invstdnormdist(random));
end;

initialization
  TFunctionPackage.RegisterPackage(TPackageStatistics);
end.
