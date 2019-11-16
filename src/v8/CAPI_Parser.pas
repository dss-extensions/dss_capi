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
    ParserDel;

var
    ComParser: ParserDel.TParser;

function Parser_Get_CmdString_AnsiString(): Ansistring; inline;
begin
    Result := ComParser.CmdString;
end;

function Parser_Get_CmdString(): PAnsiChar; CDECL;
begin
    Result := DSS_GetAsPAnsiChar(Parser_Get_CmdString_AnsiString());
end;
//------------------------------------------------------------------------------
procedure Parser_Set_CmdString(const Value: PAnsiChar); CDECL;
begin
    ComParser.CmdString := Value;
end;
//------------------------------------------------------------------------------
function Parser_Get_NextParam_AnsiString(): Ansistring; inline;
begin
    Result := ComParser.NextParam;
end;

function Parser_Get_NextParam(): PAnsiChar; CDECL;
begin
    Result := DSS_GetAsPAnsiChar(Parser_Get_NextParam_AnsiString());
end;
//------------------------------------------------------------------------------
function Parser_Get_AutoIncrement(): Boolean; CDECL;
begin
    Result := ComParser.AutoIncrement;
end;
//------------------------------------------------------------------------------
procedure Parser_Set_AutoIncrement(Value: Boolean); CDECL;
begin
    ComParser.AutoIncrement := Value;
end;
//------------------------------------------------------------------------------
function Parser_Get_DblValue(): Double; CDECL;
begin
    Result := ComParser.DblValue;
end;
//------------------------------------------------------------------------------
function Parser_Get_IntValue(): Integer; CDECL;
begin
    Result := ComParser.IntValue;
end;
//------------------------------------------------------------------------------
function Parser_Get_StrValue_AnsiString(): Ansistring; inline;
begin
    Result := ComParser.StrValue;
end;

function Parser_Get_StrValue(): PAnsiChar; CDECL;
begin
    Result := DSS_GetAsPAnsiChar(Parser_Get_StrValue_AnsiString());
end;
//------------------------------------------------------------------------------
function Parser_Get_WhiteSpace_AnsiString(): Ansistring; inline;
begin
    Result := Comparser.Whitespace;
end;

function Parser_Get_WhiteSpace(): PAnsiChar; CDECL;
begin
    Result := DSS_GetAsPAnsiChar(Parser_Get_WhiteSpace_AnsiString());
end;
//------------------------------------------------------------------------------
procedure Parser_Set_WhiteSpace(const Value: PAnsiChar); CDECL;
begin
    ComParser.Whitespace := Value;
end;
//------------------------------------------------------------------------------
function Parser_Get_BeginQuote_AnsiString(): Ansistring; inline;
begin
    Result := ComParser.BeginQuoteChars;
end;

function Parser_Get_BeginQuote(): PAnsiChar; CDECL;
begin
    Result := DSS_GetAsPAnsiChar(Parser_Get_BeginQuote_AnsiString());
end;
//------------------------------------------------------------------------------
function Parser_Get_EndQuote_AnsiString(): Ansistring; inline;
begin
    Result := ComParser.EndQuoteChars;
end;

function Parser_Get_EndQuote(): PAnsiChar; CDECL;
begin
    Result := DSS_GetAsPAnsiChar(Parser_Get_EndQuote_AnsiString());
end;
//------------------------------------------------------------------------------
procedure Parser_Set_BeginQuote(const Value: PAnsiChar); CDECL;
begin
    ComParser.BeginQuoteChars := Value;
end;
//------------------------------------------------------------------------------
procedure Parser_Set_EndQuote(const Value: PAnsiChar); CDECL;
begin
    ComParser.EndQuoteChars := Value;
end;
//------------------------------------------------------------------------------
function Parser_Get_Delimiters_AnsiString(): Ansistring; inline;
begin
    Result := ComParser.Delimiters;
end;

function Parser_Get_Delimiters(): PAnsiChar; CDECL;
begin
    Result := DSS_GetAsPAnsiChar(Parser_Get_Delimiters_AnsiString());
end;
//------------------------------------------------------------------------------
procedure Parser_Set_Delimiters(const Value: PAnsiChar); CDECL;
begin
    ComParser.Delimiters := Value;
end;
//------------------------------------------------------------------------------
procedure Parser_ResetDelimiters(); CDECL;
begin
    ComParser.ResetDelims;
end;
//------------------------------------------------------------------------------
procedure Parser_Get_Vector(var ResultPtr: PDouble; ResultCount: PInteger; ExpectedSize: Integer); CDECL;
var
    Result: PDoubleArray;
    i, ActualSize: Integer;
    VectorBuffer: Array of Double;

begin
    SetLength(VectorBuffer, ExpectedSize);
    ActualSize := ComParser.ParseAsVector(VectorBuffer);
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
    ComParser.ParseAsMatrix(MatrixBuffer);
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
    ComParser.ParseAsSymMatrix(MatrixBuffer);
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
initialization
    ComParser := ParserDel.TParser.Create;  // create COM Parser object

end.
