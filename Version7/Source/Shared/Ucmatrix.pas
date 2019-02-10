unit Ucmatrix;

{
  ----------------------------------------------------------
  Copyright (c) 2008-2015, Electric Power Research Institute, Inc.
  All rights reserved.
  ----------------------------------------------------------
}

interface

uses
    UComplex;

{
   12-4-99 Added MvMultAccum
   2/4/03  Added Avg routines
}

type
    TcMatrix = class(TObject)

    PRIVATE
    { Private declarations }
        Norder: Integer;
        Values: pComplexArray;


    PUBLIC
    { Public declarations }
        InvertError: Integer;

        constructor CreateMatrix(N: Integer);
        destructor Destroy; OVERRIDE;
        procedure Invert;
        procedure Clear;  {Zero out matrix}
        procedure AddFrom(OtherMatrix: TcMatrix);
        procedure CopyFrom(OtherMatrix: TcMatrix);
        procedure SetElement(i, j: Integer; Value: Complex);
        procedure SetElemsym(i, j: Integer; Value: Complex);
        procedure AddElement(i, j: Integer; Value: Complex);
        procedure AddElemsym(i, j: Integer; Value: Complex);
        function GetElement(i, j: Integer): Complex;
        function GetErrorCode: Integer;
        function SumBlock(row1, row2, col1, col2: Integer): Complex;
        procedure MVmult(b, x: pComplexArray);  {b = Ax}
        procedure MVmultAccum(b, x: pComplexArray);  {b = Ax}
        function GetValuesArrayPtr(var Order: Integer): pComplexArray;
        procedure ZeroRow(iRow: Integer);
        procedure ZeroCol(iCol: Integer);
        function AvgDiagonal: Complex;   // Average of Diagonal Elements
        function AvgOffDiagonal: Complex;
        procedure MultByConst(x: Double);  // Multiply all elements by a constant

        function Kron(EliminationRow: Integer): TcMatrix;  // Perform Kron reduction on last row/col and return new matrix

        property Order: Integer READ Norder;

    end;

{--------------------------------------------------------------------------}


implementation

{$R-}  { Turn off range checking}
{--------------------------------------------------------------------------}

constructor TcMatrix.CreateMatrix(N: Integer);

var
    i: Integer;

begin

    try
        inherited Create;
        Norder := N;
        InvertError := 0;
        getmem(Values, Sizeof(Complex) * Norder * Norder);    {Allocate}
        for i := 1 to (Norder * Norder) do
            values^[i] := Cmplx(0.0, 0.0);

    except
        Destroy;
    end;

end;

 {--------------------------------------------------------------------------}
destructor TcMatrix.Destroy;

begin

    Freemem(Values, Sizeof(Complex) * Norder * Norder);
    inherited Destroy;
end;

{--------------------------------------------------------------------------}
procedure TcMatrix.Clear;
var
    i: Integer;
begin
    for i := 1 to (Norder * Norder) do
        values^[i] := Cmplx(0.0, 0.0);
end;

{--------------------------------------------------------------------------}
procedure TcMatrix.MvMult(b, x: pComplexArray);
var
    Sum: Complex;
    i, j: Integer;
begin

    for i := 1 to Norder do
    begin
        Sum := Cmplx(0.0, 0.0);
        for j := 1 to Norder do
        begin
            Caccum(Sum, cmul(Values^[((j - 1) * Norder + i)], x^[j]));
        end;
        b^[i] := Sum;
    end;

end;

 {--------------------------------------------------------------------------}
procedure TcMatrix.MvMultAccum(b, x: pComplexArray);
   // Same as MVMult except accumulates b
var
    Sum: Complex;
    i, j: Integer;
begin

    for i := 1 to Norder do
    begin
        Sum := Cmplx(0.0, 0.0);
        for j := 1 to Norder do
        begin
            Caccum(Sum, cmul(Values^[((j - 1) * Norder + i)], x^[j]));
        end;
        Caccum(b^[i], Sum);
    end;

end;

{--------------------------------------------------------------------------}
procedure TcMatrix.Invert;
type
    pIntArray = ^IntArray;
    IntArray = array [1..1] of Integer;

var
    j, k, L, LL, M, i: Integer;
    LT: pIntArray;
    RMY: Double;
    T1: Complex;
    A: pComplexArray;


    function Index(i, j: Integer): Integer;
    begin
        Index := (j - 1) * L + i;
    end;


begin

    L := Norder;
    InvertError := 0;

    A := Values;  {  Assign pointer to something we can use}

{Allocate LT}
//     LT:=nil;

    GetMem(LT, SizeOf(Integer) * L);
    if LT = NIL then
    begin
        InvertError := 1;
        Exit;
    end;

{Zero LT}
    for j := 1 to L do
        LT^[j] := 0;

    T1 := Cmplx(0.0, 0.0);
    K := 1;

{M Loop }

    for  M := 1 to L do
    begin
        for  LL := 1 to L do
        begin
            if LT^[LL] <> 1 then
            begin
                RMY := Cabs(A^[Index(LL, LL)]) - CAbs(T1);  {Will this work??}
                if RMY > 0.0 then
                begin
                    T1 := A^[Index(LL, LL)];
                    K := LL;
                end; {RMY}
            end; {IF LT}
        end; {LL}

{Error Check.  If RMY ends up zero, matrix is non-inversible}
        RMY := Cabs(T1);
        if RMY = 0.0 then
        begin
            InvertError := 2;
            Exit;
        end;

        T1 := Cmplx(0.0, 0.0);
        LT^[k] := 1;
        for i := 1 to L do
            if i <> k then
                for j := 1 to L do
                    if j <> k then
                        A^[Index(i, j)] :=
                            Csub(A^[Index(i, j)], Cdiv(Cmul(A^[Index(i, k)], A^[Index(k, j)]), A^[Index(k, k)]));

        A^[Index(k, k)] := Cnegate(Cinv(A^[Index(k, k)])); {Invert and negate k,k element}

        for  i := 1 to L do
            if i <> k then
            begin
                A^[Index(i, k)] := Cmul(A^[Index(i, k)], A^[Index(k, k)]);
                A^[Index(k, i)] := Cmul(A^[Index(k, i)], A^[Index(k, k)]);
            end;  {if}

    end; {M loop}

    for  j := 1 to L do
        for  k := 1 to L do
            A^[Index(j, k)] := Cnegate(A^[Index(j, k)]);

    FreeMem(LT, SizeOF(LT^[1]) * L);  {Dispose of LT}

end;

{--------------------------------------------------------------------------}
procedure TcMatrix.SetElement(i, j: Integer; Value: Complex);
begin
    Values^[((j - 1) * Norder + i)] := Value;
end;

{--------------------------------------------------------------------------}
procedure TcMatrix.AddElement(i, j: Integer; Value: Complex);
begin
    cAccum(Values^[((j - 1) * Norder + i)], Value);
end;

{--------------------------------------------------------------------------}
procedure TcMatrix.SetElemsym(i, j: Integer; Value: Complex);
begin
    Values^[((j - 1) * Norder + i)] := Value;
    if i <> j then
        Values^[((i - 1) * Norder + j)] := Value;  {ensure symmetry}
end;

   {--------------------------------------------------------------------------}
procedure TcMatrix.AddElemsym(i, j: Integer; Value: Complex);
begin
    cAccum(Values^[((j - 1) * Norder + i)], Value);
    if i <> j then
        cAccum(Values^[((i - 1) * Norder + j)], Value);  {ensure symmetry}
end;

{--------------------------------------------------------------------------}
function TcMatrix.GetElement(i, j: Integer): Complex;
begin
    Result := Values^[((j - 1) * Norder + i)];
end;

{--------------------------------------------------------------------------}
function TcMatrix.GetErrorCode: Integer;
begin
    Result := InvertError;
end;

{--------------------------------------------------------------------------}
function TcMatrix.SumBlock(row1, row2, col1, col2: Integer): Complex;
    { Sum all elements in a given block of the matrix}

var
    i, j, rowstart: Integer;
    Sum: Complex;

begin
    Sum := Cmplx(0.0, 0.0);

    for j := col1 to col2 do
    begin
        Rowstart := (j - 1) * Norder;
        for i := (rowstart + row1) to (rowstart + row2) do
            Sum := Cadd(Sum, Values^[i]);
    end;

    Result := Sum;

end;

{--------------------------------------------------------------------------}
procedure TcMatrix.CopyFrom(OtherMatrix: TcMatrix);
var
    i, j: Integer;
begin
    if Norder = OtherMatrix.Norder then
        for i := 1 to Norder do
        begin
            for j := 1 to Norder do
                SetElement(i, j, OtherMatrix.GetElement(i, j));
        end;
end;

{--------------------------------------------------------------------------}
procedure TcMatrix.AddFrom(OtherMatrix: TcMatrix);
var
    i, j: Integer;
begin
    if Norder = OtherMatrix.Norder then
        for i := 1 to Norder do
        begin
            for j := 1 to Norder do
                AddElement(i, j, OtherMatrix.GetElement(i, j));
        end;
end;

{--------------------------------------------------------------------------}
function TcMatrix.GetValuesArrayPtr(var Order: Integer): pComplexArray;
begin
    Result := Values;
    Order := Norder;
end;

{--------------------------------------------------------------------------}
procedure TcMatrix.ZeroRow(iRow: Integer);
var
    i, j: Integer;
    Zero: Complex;

begin
    Zero := Cmplx(0.0, 0.0);

    j := iRow;
    for i := 1 to Norder do
    begin
        Values^[j] := Zero;
        Inc(j, Norder);
    end;
end;

{--------------------------------------------------------------------------}
procedure TcMatrix.ZeroCol(iCol: Integer);

var
    i: Integer;
    Zero: Complex;
begin

    Zero := Cmplx(0.0, 0.0);
    for i := ((iCol - 1) * Norder + 1) to (iCol * Norder) do
    begin
        Values^[i] := Zero;
    end;
end;

function TcMatrix.AvgDiagonal: Complex;
var
    i: Integer;
begin

    Result := Cmplx(0.0, 0.0);
    for i := 1 to Norder do
    begin
        Caccum(Result, Values^[((i - 1) * Norder + i)]);
    end;

    if Norder > 0 then
        Result := CdivReal(Result, (Norder));

end;

function TcMatrix.AvgOffDiagonal: Complex;
// Average the upper triangle off diagonals
var
    i, j, Ntimes: Integer;
begin

    Result := Cmplx(0.0, 0.0);
    Ntimes := 0;
    for i := 1 to Norder do
        for j := i + 1 to Norder do
        begin
            Inc(Ntimes);
            Caccum(Result, Values^[((j - 1) * Norder + i)]);
        end;

    if Ntimes > 0 then
        Result := CdivReal(Result, (Ntimes));
end;

function TcMatrix.Kron(EliminationRow: Integer): TcMatrix;

{Do Kron reduction on present matrix and return a new one}
{Eliminates specified row/column}

var
    i, j, N: Integer;
    ii, jj: Integer;
    NNElement: Complex;
begin
    Result := NIL;   // Nil result means it failed
    if (Norder > 1) and (EliminationRow <= Norder) and (EliminationRow > 0) then
    begin

        Result := TCMatrix.CreateMatrix(Norder - 1);
        N := EliminationRow;
        NNElement := GetElement(N, N);

        ii := 0;
        for i := 1 to Norder do
            if i <> N then
            begin    // skip elimination row
                Inc(ii);
                jj := 0;
                for j := 1 to Norder do
                    if j <> N then
                    begin
                        Inc(jj);
                        Result.SetElement(ii, jj, CSub(GetElement(i, j), Cdiv(Cmul(GetElement(i, N), GetElement(N, j)), NNElement)));
                    end;
            end;

    end;

end;

procedure TcMatrix.MultByConst(x: Double);
var
    i: Integer;
begin
    for i := 1 to Norder * Norder do
    begin
        Values^[i] := CmulReal(Values^[i], x);
    end;
end;

end.
