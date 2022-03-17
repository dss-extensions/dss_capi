unit Ucmatrix;
{
  ----------------------------------------------------------
  Copyright (c) 2008-2015, Electric Power Research Institute, Inc.
  Copyright (c) 2018-2021, Paulo Meira
  All rights reserved.
  ----------------------------------------------------------
}
interface

uses
    UComplex, DSSUcomplex;

type
    PCMatrix = ^TCMatrix;
    
    TcMatrix = class(TObject)

    PRIVATE
        Norder: Integer;
        Values: pComplexArray;
        OwnsData: Boolean;

    PUBLIC
        InvertError: Integer;

        constructor CreateMatrix(N: Integer);
        constructor CreateMatrixInplace(N: Integer; pValues: pComplex);

        destructor Destroy; OVERRIDE;
        procedure Invert;
        procedure Negate;
        function IsZero: Boolean;
        function IsColRowZero(n: Integer): Boolean;
        procedure Clear; inline; // Zero out matrix
        procedure AddFrom(OtherMatrix: TcMatrix);
        procedure CopyFrom(OtherMatrix: TcMatrix);
        procedure SetElement(i, j: Integer; Value: Complex);
        procedure SetElemsym(i, j: Integer; Value: Complex);
        procedure AddElement(i, j: Integer; Value: Complex);
        procedure AddElemsym(i, j: Integer; Value: Complex);
        function GetElement(i, j: Integer): Complex;
        function GetErrorCode: Integer;
        procedure MVmult(b, x: pComplexArray); inline; {b = Ax}
        function GetValuesArrayPtr(var Order: Integer): pComplexArray;
        procedure ZeroRow(iRow: Integer);
        procedure ZeroCol(iCol: Integer);
        function AvgDiagonal: Complex;   // Average of Diagonal Elements
        function AvgOffDiagonal: Complex;
        function MtrxMult(B: TcMatrix): TcMatrix; // Multiply two square matrices of same order.  Result = A*B

        function Kron(EliminationRow: Integer): TcMatrix;  // Perform Kron reduction on last row/col and return new matrix

        property Order: Integer READ Norder;
    end;

implementation

uses 
    KLUSolve;

constructor TcMatrix.CreateMatrix(N: Integer);
begin
    try
        inherited Create;
        Norder := N;
        InvertError := 0;
        Values := Allocmem(Sizeof(Complex) * Norder * Norder); // alloc and fill with 0
        OwnsData := True;
    except
        Destroy;
    end;
end;

constructor TcMatrix.CreateMatrixInplace(N: Integer; pValues: pComplex);
begin
    try
        inherited Create;
        Norder := N;
        InvertError := 0;
        Values := pComplexArray(pValues); // assume zeroed
        OwnsData := False;
    except
        Destroy;
    end;
end;


destructor TcMatrix.Destroy;
begin
    if OwnsData then
        Freemem(Values, Sizeof(Complex) * Norder * Norder);
    inherited Destroy;
end;

procedure TcMatrix.Clear; inline;
begin
    FillByte(Values^, Sizeof(Complex) * Norder * Norder, 0);
end;

function TcMatrix.IsZero: Boolean; // This only check for exactly zero, no epsilon is used on purpose
var 
    i: integer;
    v: pComplex;
begin
    Result := True;
    v := @Values^[1];
    for i := 1 to Norder * Norder do
    begin
        if (v^.re <> 0) or (v^.im <> 0) then
        begin
            Result := False;
            Exit;
        end;
        inc(v);
    end;
end;

function TcMatrix.IsColRowZero(n: Integer): Boolean; // This only check for exactly zero, no epsilon is used on purpose
var 
    i, j: integer;
    e: Complex;
begin
    Result := True;
    
    i := n;
    
    for j := 1 to Norder do
    begin
        e := Values^[((j - 1) * Norder + i)];
        if (e.re <> 0) or (e.im <> 0) then
        begin
            Result := False;
            Exit;
        end;
        
        e := Values^[((i - 1) * Norder + j)];
        if (e.re <> 0) or (e.im <> 0) then
        begin
            Result := False;
            Exit;
        end;
    end;
end;

procedure TcMatrix.MvMult(b, x: pComplexArray); inline;
{$IFDEF DSS_CAPI_MVMULT}
begin
    KLUSolve.mvmult(Norder, b, values, x);
end;
{$ELSE}
var
    Sum: Complex;
    i, j: Integer;
begin
    for i := 1 to Norder do
    begin
        Sum := Cmplx(0.0, 0.0);
        for j := 1 to Norder do
        begin
            Sum += (Values^[((j - 1) * Norder + i)]) * x^[j]);
        end;
        b^[i] := Sum;
    end;
end;
{$ENDIF}

procedure TcMatrix.Negate;
var i: integer;
begin
    for i := 1 to Norder * Norder do
        Values^[i] := -Values^[i];
end;

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

    A := Values;  //  Assign pointer to something we can use

    // Allocate LT
    //     LT:=nil;
    GetMem(LT, SizeOf(Integer) * L);
    if LT = NIL then
    begin
        InvertError := 1;
        Exit;
    end;

    // Zero LT
    for j := 1 to L do
        LT^[j] := 0;

    T1 := Cmplx(0.0, 0.0);
    K := 1;

    // M Loop
    for  M := 1 to L do
    begin
        for  LL := 1 to L do
        begin
            if LT^[LL] <> 1 then
            begin
                RMY := Cabs(A^[Index(LL, LL)]) - CAbs(T1);  // Will this work??
                if RMY > 0.0 then
                begin
                    T1 := A^[Index(LL, LL)];
                    K := LL;
                end;
            end;
        end;

        // Error Check.  If RMY ends up zero, matrix is non-inversible
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
                            A^[Index(i, j)] - (A^[Index(i, k)] * A^[Index(k, j)]) / A^[Index(k, k)];

        A^[Index(k, k)] := -Cinv(A^[Index(k, k)]); // Invert and negate k,k element

        for  i := 1 to L do
            if i <> k then
            begin
                A^[Index(i, k)] := A^[Index(i, k)] * A^[Index(k, k)];
                A^[Index(k, i)] := A^[Index(k, i)] * A^[Index(k, k)];
            end;

    end; // M loop

    for  j := 1 to L do
        for  k := 1 to L do
            A^[Index(j, k)] := -A^[Index(j, k)];

    FreeMem(LT, SizeOF(LT^[1]) * L);
end;

procedure TcMatrix.SetElement(i, j: Integer; Value: Complex);
begin
    Values^[((j - 1) * Norder + i)] := Value;
end;

procedure TcMatrix.AddElement(i, j: Integer; Value: Complex);
begin
    Values^[((j - 1) * Norder + i)] += Value;
end;

procedure TcMatrix.SetElemsym(i, j: Integer; Value: Complex);
begin
    Values^[((j - 1) * Norder + i)] := Value;
    if i <> j then
        Values^[((i - 1) * Norder + j)] := Value; // ensure symmetry
end;
   
procedure TcMatrix.AddElemsym(i, j: Integer; Value: Complex);
begin
    Values^[((j - 1) * Norder + i)] += Value;
    if i <> j then
        Values^[((i - 1) * Norder + j)] += Value; // ensure symmetry
end;

function TcMatrix.GetElement(i, j: Integer): Complex;
begin
    Result := Values^[((j - 1) * Norder + i)];
end;

function TcMatrix.GetErrorCode: Integer;
begin
    Result := InvertError;
end;

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

function TcMatrix.GetValuesArrayPtr(var Order: Integer): pComplexArray;
begin
    Result := Values;
    Order := Norder;
end;

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
        Result += Values^[((i - 1) * Norder + i)];
    end;

    if Norder > 0 then
        Result := Result / Norder;
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
            Result += Values^[((j - 1) * Norder + i)];
        end;

    if Ntimes > 0 then
        Result := Result / Ntimes;
end;

function TcMatrix.Kron(EliminationRow: Integer): TcMatrix;
// Do Kron reduction on present matrix and return a new one
// Eliminates specified row/column
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
                        Result.SetElement(ii, jj, GetElement(i, j) - GetElement(i, N) * GetElement(N, j) / NNElement);
                    end;
            end;
    end;
end;

function TcMatrix.MtrxMult(B: TcMatrix): TCmatrix; //TODO: C++ version if better performance is useful here
// multiply two scquare matrices of same order
// C (result) = A*B
var
    i, j: Integer;
    cTemp1, cTemp2: pComplexArray;
begin
    Result := NIL;   // returns Nil pointer if illegal operation
    if B.Norder = Norder then
    begin
        Result := TcMatrix.CreateMatrix(Norder);
        cTemp1 := Allocmem(Sizeof(Complex) * Norder);   // Temp array to hold column
        cTemp2 := Allocmem(Sizeof(Complex) * Norder);   // Temp array
        for j := 1 to Norder do   // Column j
        begin
            for i := 1 to Norder do
                cTemp2^[i] := B.GetElement(i, j); // Row i
            MVmult(cTemp1, cTemp2);
            for i := 1 to Norder do
                Result.SetElement(i, j, cTemp1^[i]);
        end;
        Reallocmem(cTemp1, 0);    // Discard temp arrays
        Reallocmem(cTemp2, 0);
    end;
end;

end.
