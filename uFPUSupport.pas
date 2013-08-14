unit uFPUSupport;

interface

type
  TExtRec  = packed record case boolean of
               true: (
                 m: Int64;       {significand/mantissa}
                 xp: word;       {biased exponent and sign}
               );
               false: (
                 e: Extended;
               )    
             end;


function fmod(x, y: Extended): Extended;
function fdiv(x, y: Extended): Extended;
function fpower(x, y: Extended): Extended;

function fzero(x: Extended): boolean;
function finf(x: Extended): boolean;
function ftruncable(x: Extended): boolean;

function fpu_maskexception(const aMask: Word): Word;
procedure fpu_clearexc;

const
  (*
  +--------------------------------------------+
  |X X X X   RC    PC  X  X  PM UM OM ZM DM IM |
  +--------------------------------------------+
   15-12   11-10  9-8  7  6   5  4  3  2  1  0
                   Control word
  *)
  FPU_PM = $20;
  FPU_UM = $10;
  FPU_OM = $08;
  FPU_ZM = $04;
  FPU_DM = $02;
  FPU_IM = $01;
  FPU_MASK_ALL = $3F;
  FPU_MASK_NONE = $FFFF and not FPU_MASK_ALL;



  //NUMBER_EPSILON = 1E-18;
  //normal numbers closest to 0, "predecessor" and "successor" of 0 in representable numbers 
  FPU_HEX_PRED0: TExtRec = (m: $4000000000000000; xp:$8000); //-2^-16383
  FPU_HEX_SUCC0: TExtRec = (m: $4000000000000000; xp:$0000); // 2^-16383

var
  FPU_PRED0: Extended absolute FPU_HEX_PRED0;
  FPU_SUCC0: Extended absolute FPU_HEX_SUCC0;



implementation

uses Math;

{
  Compute x mod y
  Condition:    x<>0
  Return:       remainder, sign copied from x
}
function fmod(x,y: Extended): Extended; assembler;
asm
       fld    [y]
       fld    [x]
  @@1: fprem
       fstsw  ax
       sahf
       jp     @@1
       fstp   st(1)
       fwait
end;
              
{
  Compute x / y
  Return:       NaN if any is NaN or 0/0
                +INF if 1/0
                -INF if -1/0
                result otherwise
}
function fdiv(x, y: Extended): Extended; assembler;
var
  fpucw,modcw: Word;
asm
  fstcw fpucw
  mov ax, fpucw
  or ax, $003F    //set PM, UM, OM, ZM, DM, IM
  mov modcw, ax
  fldcw modcw
  fld tbyte ptr [x]
  fld tbyte ptr [y]
  fdivp

  fstp tbyte ptr [Result]
  wait
  fclex
  fldcw fpucw
end;

{
  Compute x^y (or x ** y)
    based on Math.Power, but with more special cases covered
  Return:       result
}
function fpower(x, y: Extended): Extended;
begin
  if IsNan(x) or IsNan(y) then
    Result:= NaN
  else if fzero(y) then
    Result:= 1.0
  else begin
    if finf(x) then begin
      if y > 0 then
        Result:= Infinity           // inf^n  n > 0 -> IFN
      else                          // inf^0 = 1 already captured
        Result:= NegInfinity;       // inf^n  n < 0 -> 1/INF^n = 0
    end else
    if fzero(x) then begin
      if y > 0.0 then               // 0^n  n > 0 -> 0
        Result:= 0.0
      else if y < 0 then            // 0^n  n < 0 -> 1/0^n = INF
        Result:= Infinity
      else
        Result:= NaN;               // 0^0 -> indeterminate
    end else
    if fzero(frac(y)) and ftruncable(y) then begin
      if (x = 2.0) then             // 2^y  -> FPU can do that directly
        Result:= Math.Ldexp(1.0, Trunc(y))
      else
        Result := Math.IntPower(x, Trunc(y));
    end else
      Result := Exp(y * Ln(x))
  end;
end;

{
  Check if x is zero
  Return:       boolean if x is a denormal close to zero
}
function fzero(x: Extended): boolean;
begin
  Result:= (FPU_PRED0 < x) and (x < FPU_SUCC0);
end;       

{
  Check if x is infinite
  Return:       boolean if x is infinite
}
function finf(x: Extended): boolean;
begin
  with TExtRec(x) do begin
    Result := (xp and $7FFF=$7FFF) and (m=$8000000000000000);
  end;
end;

{
  Check if x is could be represented by trunc()
  Return:       boolean if x is in int64 range
}
function ftruncable(x: Extended): boolean;
begin
  Result:= (low(int64) <= x) and (x <= high(int64));
end;

{
  Write new FPU Exception mask
  Return:       old exception mask
}
function fpu_maskexception(const aMask: Word): Word; assembler;
var
  fpucw: Word;
asm
  fclex                           // throw away any signaled exceptions
  fstcw fpucw                     // retrieve current CW
  mov ax, fpucw
  mov Result, ax
  or Result, FPU_MASK_ALL         // return old exception mask
  and ax, FPU_MASK_NONE           // compute new CW
  or ax, aMask
  mov fpucw, ax
  fldcw fpucw                     // load desired CW
end;

{
  Clear FPU exception status
}
procedure fpu_clearexc; assembler;
asm
  fclex
end;

end.
