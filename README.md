pi - Matheparser/Taschenrechner
===============================

Offizielles Repository: [GitHub](https://github.com/martok/pi)



Syntax
------

Gleichungen eben ;)

Eine Besonderheit ist, dass mit dem DynamicAssignment-Operator (`:=` ) Ausdrücke
direkt ohne Auswertung gespeichert werden können. Diese kann man dann durch eine
spezielle Art des Funktionsaufrufs verwenden: Eckige Klammern führen eine Expression
mit neuem Kontext aus, der durch die Ausdrücke in der Klammer vorbereitet werden
kann (siehe Beispiel 3).

Operatoren
----------

*  `?`
   Describe. Gibt die StringForm seines Arguments aus
*  `^`
   Power. Berechnet x^y
*  `* / %`
   Multiplication, Division, Modulus
*  `+ -`
   Addition, Subtraction
*  `=`
   AssigmentStatic. Weist der linken Seite *den Wert* der rechten Seite zu
*  `:=`
   AssigmentDynamic. Weist der linken Seite *den Ausdruck* auf der rechten Seite zu
*  `,`
   ArgList. Stellt sowohl Parameterlisten als auch verkette Ausdrücke dar. Ergebnis ist immer das Ergebnis des letzten Ausdrucks. (Beispiele 4, 5)



Funktionen
----------

Numerisch:

*  `Abs(number)`
   Betrag
*  `Exp(number)`
   Exponential
*  `Ln(number)`
   Logarithmus zur Basis e
*  `Lg(number)`
   Logarithmus zur Basis 10
*  `Ld(number)`
   Logarithmus zur Basis 2
*  `Loga(base, number)`
   Logarithmus zu beliebiger Basis
*  `Fac(number)`
   Fakultät


Alle Winkelfunktionen erwarten/geben Radiant:

*  `Deg2Rad(number)`
   Grad in Radiant
*  `Rad2Deg(number)`
   Radiant in Grad


Winkelfunktionen:

*  `Sin(number)`
   Sinus
*  `Cos(number)`
   Cosinus
*  `Tan(number)`
   Tangens
*  `ArcSin(number)`
   Inverse zu Sin
*  `ArcCos(number)`
   Inverse zu Cos
*  `ArcTan(number)`
   Inverse zu Tan
*  `ArcTan(Y, X)`
   Delphi's [ArcTan2](http://docwiki.embarcadero.com/VCL/en/Math.ArcTan2): ArcTan im korrekten Quadranten
*  `Sinh(number)`
   Sinus Hyperbolicus
*  `Cosh(number)`
   Cosinus Hyperbolicus
*  `Tanh(number)`
   Tangens Hyperbolicus
*  `ArSinh(number)`
   Inverse zu SinH
*  `ArCosh(number)`
   Inverse zu CosH
*  `ArTanh(number)`
   Inverse zu TanH


Integer-Basiskonvertierung:

*  `ToBase(number, base)`
   Konvertiert Ganzzahlen in String-Repräsentation in Basis
*  `FromBase(string, base)`
   Konvertiert String-Repräsentation aus Basis in Zahl (nur Ganzzahlen)


Variablen&Context-Management:

*  `New() / New(string)`
   Erzeugt einen neuen (optional benannten) Kontext
*  `Drop()`
   Löscht den aktuell obersten Kontext
*  `Undef(string)`
   Entfernt eine Variable aus dem aktuellen Kontext

Beispiele
---------

### Einfache Berechnung

    > 1+2*3+4/5
    = 7.8

### Definition einer Expression

    > sinc:= sin(x)/x

### Aufruf dieser Expression

    > sinc[x=pi/3]
    = 0.826993343132688074

### Mehrere Berechnungen verketten

    > z=1,y=3*z,y+1
    = 4

### Subkontexte verwenden

    > sum:= x+y+z, sum[x=2, y=3, z=7]
    = 12

