unit CAPI_Parser;

interface

uses
    CAPI_Utils,
    CAPI_Types;

function Parser_Get_CmdString(): PAnsiChar; CDECL;
procedure Parser_Set_CmdString(const Value: PAnsiChar); CDECL;
function Parser_Get_NextParam(): PAnsiChar; CDECL;
function Parser_Get_AutoIncrement(): TAPIBoolean; CDECL;
procedure Parser_Set_AutoIncrement(Value: TAPIBoolean); CDECL;
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
procedure Parser_Get_Vector(var ResultPtr: PDouble; ResultCount: PAPISize; ExpectedSize: Integer); CDECL;
procedure Parser_Get_Vector_GR(ExpectedSize: Integer); CDECL;
procedure Parser_Get_Matrix(var ResultPtr: PDouble; ResultCount: PAPISize; ExpectedOrder: Integer); CDECL;
procedure Parser_Get_Matrix_GR(ExpectedOrder: Integer); CDECL;
procedure Parser_Get_SymMatrix(var ResultPtr: PDouble; ResultCount: PAPISize; ExpectedOrder: Integer); CDECL;
procedure Parser_Get_SymMatrix_GR(ExpectedOrder: Integer); CDECL;

implementation

uses
    CAPI_Constants,
    ParserDel,
    ArrayDef,
    DSSClass;

//------------------------------------------------------------------------------
function Parser_Get_CmdString(): PAnsiChar; CDECL;
begin
    Result := DSS_GetAsPAnsiChar(DSSPrime, DSSPrime.ComParser.CmdString);
end;
//------------------------------------------------------------------------------
procedure Parser_Set_CmdString(const Value: PAnsiChar); CDECL;
begin
    DSSPrime.ComParser.CmdString := Value;
end;
//------------------------------------------------------------------------------
function Parser_Get_NextParam(): PAnsiChar; CDECL;
begin
    Result := DSS_GetAsPAnsiChar(DSSPrime, DSSPrime.ComParser.NextParam);
end;
//------------------------------------------------------------------------------
function Parser_Get_AutoIncrement(): TAPIBoolean; CDECL;
begin
    Result := DSSPrime.ComParser.AutoIncrement;
end;
//------------------------------------------------------------------------------
procedure Parser_Set_AutoIncrement(Value: TAPIBoolean); CDECL;
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
    Result := DSS_GetAsPAnsiChar(DSSPrime, DSSPrime.ComParser.StrValue);
end;
//------------------------------------------------------------------------------
function Parser_Get_WhiteSpace(): PAnsiChar; CDECL;
begin
    Result := DSS_GetAsPAnsiChar(DSSPrime, DSSPrime.ComParser.Whitespace);
end;
//------------------------------------------------------------------------------
procedure Parser_Set_WhiteSpace(const Value: PAnsiChar); CDECL;
begin
    DSSPrime.ComParser.Whitespace := Value;
end;
//------------------------------------------------------------------------------
function Parser_Get_BeginQuote(): PAnsiChar; CDECL;
begin
    Result := DSS_GetAsPAnsiChar(DSSPrime, DSSPrime.ComParser.BeginQuoteChars);
end;
//------------------------------------------------------------------------------
function Parser_Get_EndQuote(): PAnsiChar; CDECL;
begin
    Result := DSS_GetAsPAnsiChar(DSSPrime, DSSPrime.ComParser.EndQuoteChars);
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
    Result := DSS_GetAsPAnsiChar(DSSPrime, DSSPrime.ComParser.Delimiters);
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
procedure Parser_Get_Vector(var ResultPtr: PDouble; ResultCount: PAPISize; ExpectedSize: Integer); CDECL;
var
    ActualSize: Integer;
begin
    DSS_RecreateArray_PDouble(ResultPtr, ResultCount, ExpectedSize);
    ActualSize := DSSPrime.ComParser.ParseAsVector(ResultCount^, ArrayDef.PDoubleArray(ResultPtr));
    ResultCount^ := ActualSize;
end;

procedure Parser_Get_Vector_GR(ExpectedSize: Integer); CDECL;
// Same as Parser_Get_Vector but uses global result (GR) pointers
begin
    Parser_Get_Vector(DSSPrime.GR_DataPtr_PDouble, DSSPrime.GR_Counts_PDouble, ExpectedSize)
end;

//------------------------------------------------------------------------------
procedure Parser_Get_Matrix(var ResultPtr: PDouble; ResultCount: PAPISize; ExpectedOrder: Integer); CDECL;
begin
    DSS_RecreateArray_PDouble(ResultPtr, ResultCount, ExpectedOrder * ExpectedOrder);
    DSSPrime.ComParser.ParseAsMatrix(ResultCount^, ArrayDef.PDoubleArray(ResultPtr));
end;

procedure Parser_Get_Matrix_GR(ExpectedOrder: Integer); CDECL;
// Same as Parser_Get_Matrix but uses global result (GR) pointers
begin
    Parser_Get_Matrix(DSSPrime.GR_DataPtr_PDouble, DSSPrime.GR_Counts_PDouble, ExpectedOrder)
end;

//------------------------------------------------------------------------------
procedure Parser_Get_SymMatrix(var ResultPtr: PDouble; ResultCount: PAPISize; ExpectedOrder: Integer); CDECL;
begin
    DSS_RecreateArray_PDouble(ResultPtr, ResultCount, ExpectedOrder * ExpectedOrder);
    DSSPrime.ComParser.ParseAsSymMatrix(ResultCount^, ArrayDef.PDoubleArray(ResultPtr));
end;

procedure Parser_Get_SymMatrix_GR(ExpectedOrder: Integer); CDECL;
// Same as Parser_Get_SymMatrix but uses global result (GR) pointers
begin
    Parser_Get_SymMatrix(DSSPrime.GR_DataPtr_PDouble, DSSPrime.GR_Counts_PDouble, ExpectedOrder)
end;

end.
