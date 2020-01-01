unit CAPI_Parser;

{$inline on}

interface

uses
    CAPI_Utils;

function Parser_Get_CmdString(): PAnsiChar; CDECL;
procedure Parser_Set_CmdString(const Value: PAnsiChar); CDECL;
function Parser_Get_NextParam(): PAnsiChar; CDECL;
function Parser_Get_AutoIncrement(): Boolean; CDECL;
procedure Parser_Set_AutoIncrement(Value: Boolean); CDECL;
function Parser_Get_DblValue(): Double; CDECL;
function Parser_Get_IntValue(): Integer; CDECL;
function Parser_Get_StrValue(): PAnsiChar; CDECL;
function Parser_Get_WhiteSpace(): PAnsiChar; CDECL;
procedure Parser_Set_WhiteSpace(const Value: PAnsiChar); CDECL;
function Parser_Get_BeginQuote(): PAnsiChar; CDECL;
function Parser_Get_EndQuote(): PAnsiChar; CDECL;
procedure Parser_Set_BeginQuote(const Value: PAnsiChar); CDECL;
procedure Parser_Set_EndQuote(const Value: PAnsiChar); CDECL;
function Parser_Get_Delimiters(): PAnsiChar; CDECL;
procedure Parser_Set_Delimiters(const Value: PAnsiChar); CDECL;
procedure Parser_ResetDelimiters(); CDECL;
procedure Parser_Get_Vector(var ResultPtr: PDouble; ResultCount: PInteger; ExpectedSize: Integer); CDECL;
procedure Parser_Get_Vector_GR(ExpectedSize: Integer); CDECL;
procedure Parser_Get_Matrix(var ResultPtr: PDouble; ResultCount: PInteger; ExpectedOrder: Integer); CDECL;
procedure Parser_Get_Matrix_GR(ExpectedOrder: Integer); CDECL;
procedure Parser_Get_SymMatrix(var ResultPtr: PDouble; ResultCount: PInteger; ExpectedOrder: Integer); CDECL;
procedure Parser_Get_SymMatrix_GR(ExpectedOrder: Integer); CDECL;

implementation

uses
    CAPI_Constants,
    ParserDel,
    DSSClass;

function Parser_Get_CmdString(): PAnsiChar; CDECL;
begin
    Result := DSS_GetAsPAnsiChar(DSSPrime.ComParser.CmdString);
end;
//------------------------------------------------------------------------------
procedure Parser_Set_CmdString(const Value: PAnsiChar); CDECL;
begin
    DSSPrime.ComParser.CmdString := Value;
end;
//------------------------------------------------------------------------------
function Parser_Get_NextParam(): PAnsiChar; CDECL;
begin
    Result := DSS_GetAsPAnsiChar(DSSPrime.ComParser.NextParam);
end;
//------------------------------------------------------------------------------
function Parser_Get_AutoIncrement(): Boolean; CDECL;
begin
    Result := DSSPrime.ComParser.AutoIncrement;
end;
//------------------------------------------------------------------------------
procedure Parser_Set_AutoIncrement(Value: Boolean); CDECL;
begin
    DSSPrime.ComParser.AutoIncrement := Value;
end;
//------------------------------------------------------------------------------
function Parser_Get_DblValue(): Double; CDECL;
begin
    Result := DSSPrime.ComParser.DblValue;
end;
//------------------------------------------------------------------------------
function Parser_Get_IntValue(): Integer; CDECL;
begin
    Result := DSSPrime.ComParser.IntValue;
end;
//------------------------------------------------------------------------------
function Parser_Get_StrValue(): PAnsiChar; CDECL;
begin
    Result := DSS_GetAsPAnsiChar(DSSPrime.ComParser.StrValue);
end;
//------------------------------------------------------------------------------
function Parser_Get_WhiteSpace(): PAnsiChar; CDECL;
begin
    Result := DSS_GetAsPAnsiChar(DSSPrime.ComParser.Whitespace);
end;
//------------------------------------------------------------------------------
procedure Parser_Set_WhiteSpace(const Value: PAnsiChar); CDECL;
begin
    DSSPrime.ComParser.Whitespace := Value;
end;
//------------------------------------------------------------------------------
function Parser_Get_BeginQuote(): PAnsiChar; CDECL;
begin
    Result := DSS_GetAsPAnsiChar(DSSPrime.ComParser.BeginQuoteChars);
end;
//------------------------------------------------------------------------------
function Parser_Get_EndQuote(): PAnsiChar; CDECL;
begin
    Result := DSS_GetAsPAnsiChar(DSSPrime.ComParser.EndQuoteChars);
end;
//------------------------------------------------------------------------------
procedure Parser_Set_BeginQuote(const Value: PAnsiChar); CDECL;
begin
    DSSPrime.ComParser.BeginQuoteChars := Value;
end;
//------------------------------------------------------------------------------
procedure Parser_Set_EndQuote(const Value: PAnsiChar); CDECL;
begin
    DSSPrime.ComParser.EndQuoteChars := Value;
end;
//------------------------------------------------------------------------------
function Parser_Get_Delimiters(): PAnsiChar; CDECL;
begin
    Result := DSS_GetAsPAnsiChar(DSSPrime.ComParser.Delimiters);
end;
//------------------------------------------------------------------------------
procedure Parser_Set_Delimiters(const Value: PAnsiChar); CDECL;
begin
    DSSPrime.ComParser.Delimiters := Value;
end;
//------------------------------------------------------------------------------
procedure Parser_ResetDelimiters(); CDECL;
begin
    DSSPrime.ComParser.ResetDelims;
end;
//------------------------------------------------------------------------------
procedure Parser_Get_Vector(var ResultPtr: PDouble; ResultCount: PInteger; ExpectedSize: Integer); CDECL;
var
    Result: PDoubleArray;
    i, ActualSize: Integer;
    VectorBuffer: Array of Double;

begin
    SetLength(VectorBuffer, ExpectedSize);
    ActualSize := DSSPrime.ComParser.ParseAsVector(VectorBuffer);
    Result := DSS_RecreateArray_PDouble(ResultPtr, ResultCount, ActualSize);
    Move(VectorBuffer[0], ResultPtr[0], ActualSize * SizeOf(Double));
    SetLength(VectorBuffer, 0);
end;

procedure Parser_Get_Vector_GR(ExpectedSize: Integer); CDECL;
// Same as Parser_Get_Vector but uses global result (GR) pointers
begin
    Parser_Get_Vector(GR_DataPtr_PDouble, GR_CountPtr_PDouble, ExpectedSize)
end;

//------------------------------------------------------------------------------
procedure Parser_Get_Matrix(var ResultPtr: PDouble; ResultCount: PInteger; ExpectedOrder: Integer); CDECL;
var
    Result: PDoubleArray;
    i, MatrixSize: Integer;
    MatrixBuffer: Array of Double;
begin
    MatrixSize := ExpectedOrder * ExpectedOrder;
    SetLength(MatrixBuffer, MatrixSize);
    DSSPrime.ComParser.ParseAsMatrix(MatrixBuffer);
    Result := DSS_RecreateArray_PDouble(ResultPtr, ResultCount, MatrixSize);
    Move(MatrixBuffer[0], ResultPtr[0], MatrixSize * SizeOf(Double));
    SetLength(MatrixBuffer, 0);
end;

procedure Parser_Get_Matrix_GR(ExpectedOrder: Integer); CDECL;
// Same as Parser_Get_Matrix but uses global result (GR) pointers
begin
    Parser_Get_Matrix(GR_DataPtr_PDouble, GR_CountPtr_PDouble, ExpectedOrder)
end;

//------------------------------------------------------------------------------
procedure Parser_Get_SymMatrix(var ResultPtr: PDouble; ResultCount: PInteger; ExpectedOrder: Integer); CDECL;
var
    Result: PDoubleArray;
    i, MatrixSize: Integer;
    MatrixBuffer: Array of Double;
begin
    MatrixSize := ExpectedOrder * ExpectedOrder;
    SetLength(MatrixBuffer, MatrixSize);
    DSSPrime.ComParser.ParseAsSymMatrix(MatrixBuffer);
    Result := DSS_RecreateArray_PDouble(ResultPtr, ResultCount, MatrixSize);
    Move(MatrixBuffer[0], ResultPtr[0], MatrixSize * SizeOf(Double));
    SetLength(MatrixBuffer, 0);
end;

procedure Parser_Get_SymMatrix_GR(ExpectedOrder: Integer); CDECL;
// Same as Parser_Get_SymMatrix but uses global result (GR) pointers
begin
    Parser_Get_SymMatrix(GR_DataPtr_PDouble, GR_CountPtr_PDouble, ExpectedOrder)
end;
//------------------------------------------------------------------------------
end.
