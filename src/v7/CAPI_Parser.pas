UNIT CAPI_Parser;
{$inline on}

INTERFACE

USES ArrayDef, CAPI_Utils;

function Parser_Get_CmdString():PAnsiChar;cdecl;
procedure Parser_Set_CmdString(const Value: PAnsiChar);cdecl;
function Parser_Get_NextParam():PAnsiChar;cdecl;
function Parser_Get_AutoIncrement():WordBool;cdecl;
procedure Parser_Set_AutoIncrement(Value: WordBool);cdecl;
function Parser_Get_DblValue():Double;cdecl;
function Parser_Get_IntValue():Integer;cdecl;
function Parser_Get_StrValue():PAnsiChar;cdecl;
function Parser_Get_WhiteSpace():PAnsiChar;cdecl;
procedure Parser_Set_WhiteSpace(const Value: PAnsiChar);cdecl;
function Parser_Get_BeginQuote():PAnsiChar;cdecl;
function Parser_Get_EndQuote():PAnsiChar;cdecl;
procedure Parser_Set_BeginQuote(const Value: PAnsiChar);cdecl;
procedure Parser_Set_EndQuote(const Value: PAnsiChar);cdecl;
function Parser_Get_Delimiters():PAnsiChar;cdecl;
procedure Parser_Set_Delimiters(const Value: PAnsiChar);cdecl;
procedure Parser_ResetDelimiters();cdecl;
PROCEDURE Parser_Get_Vector(var ResultPtr: PDouble; ResultCount: PInteger; ExpectedSize: Integer);cdecl;
PROCEDURE Parser_Get_Vector_GR(ExpectedSize: Integer);cdecl;
PROCEDURE Parser_Get_Matrix(var ResultPtr: PDouble; ResultCount: PInteger; ExpectedOrder: Integer);cdecl;
PROCEDURE Parser_Get_Matrix_GR(ExpectedOrder: Integer);cdecl;
PROCEDURE Parser_Get_SymMatrix(var ResultPtr: PDouble; ResultCount: PInteger; ExpectedOrder: Integer);cdecl;
PROCEDURE Parser_Get_SymMatrix_GR(ExpectedOrder: Integer);cdecl;

IMPLEMENTATION

USES CAPI_Constants, ParserDel;


Var ComParser : ParserDel.TParser;            
            function Parser_Get_CmdString_AnsiString():AnsiString;inline;
begin
     Result := ComParser.CmdString;
end;

function Parser_Get_CmdString():PAnsiChar;cdecl;
begin
    Result := DSS_GetAsPAnsiChar(Parser_Get_CmdString_AnsiString());
end;
//------------------------------------------------------------------------------
procedure Parser_Set_CmdString(const Value: PAnsiChar);cdecl;
begin
     ComParser.CmdString := Value;
end;
//------------------------------------------------------------------------------
function Parser_Get_NextParam_AnsiString():AnsiString;inline;
begin
     Result := ComParser.NextParam;
end;

function Parser_Get_NextParam():PAnsiChar;cdecl;
begin
    Result := DSS_GetAsPAnsiChar(Parser_Get_NextParam_AnsiString());
end;
//------------------------------------------------------------------------------
function Parser_Get_AutoIncrement():WordBool;cdecl;
begin
     Result := ComParser.AutoIncrement;
end;
//------------------------------------------------------------------------------
procedure Parser_Set_AutoIncrement(Value: WordBool);cdecl;
begin
    ComParser.AutoIncrement := Value;
end;
//------------------------------------------------------------------------------
function Parser_Get_DblValue():Double;cdecl;
begin
   Result := ComParser.DblValue ;
end;
//------------------------------------------------------------------------------
function Parser_Get_IntValue():Integer;cdecl;
begin
    Result := ComParser.IntValue ;
end;
//------------------------------------------------------------------------------
function Parser_Get_StrValue_AnsiString():AnsiString;inline;
begin
    Result := ComParser.StrValue;
end;

function Parser_Get_StrValue():PAnsiChar;cdecl;
begin
    Result := DSS_GetAsPAnsiChar(Parser_Get_StrValue_AnsiString());
end;
//------------------------------------------------------------------------------
function Parser_Get_WhiteSpace_AnsiString():AnsiString;inline;
begin
    Result := Comparser.Whitespace;
end;

function Parser_Get_WhiteSpace():PAnsiChar;cdecl;
begin
    Result := DSS_GetAsPAnsiChar(Parser_Get_WhiteSpace_AnsiString());
end;
//------------------------------------------------------------------------------
procedure Parser_Set_WhiteSpace(const Value: PAnsiChar);cdecl;
begin
    ComParser.Whitespace := Value;
end;
//------------------------------------------------------------------------------
function Parser_Get_BeginQuote_AnsiString():AnsiString;inline;
begin
    Result := ComParser.BeginQuoteChars;
end;

function Parser_Get_BeginQuote():PAnsiChar;cdecl;
begin
    Result := DSS_GetAsPAnsiChar(Parser_Get_BeginQuote_AnsiString());
end;
//------------------------------------------------------------------------------
function Parser_Get_EndQuote_AnsiString():AnsiString;inline;
begin
     Result := ComParser.EndQuoteChars;
end;

function Parser_Get_EndQuote():PAnsiChar;cdecl;
begin
    Result := DSS_GetAsPAnsiChar(Parser_Get_EndQuote_AnsiString());
end;
//------------------------------------------------------------------------------
procedure Parser_Set_BeginQuote(const Value: PAnsiChar);cdecl;
begin
    ComParser.BeginQuoteChars := Value;
end;
//------------------------------------------------------------------------------
procedure Parser_Set_EndQuote(const Value: PAnsiChar);cdecl;
begin
     ComParser.EndQuoteChars := Value;
end;
//------------------------------------------------------------------------------
function Parser_Get_Delimiters_AnsiString():AnsiString;inline;
begin
     Result := ComParser.Delimiters ;
end;

function Parser_Get_Delimiters():PAnsiChar;cdecl;
begin
    Result := DSS_GetAsPAnsiChar(Parser_Get_Delimiters_AnsiString());
end;
//------------------------------------------------------------------------------
procedure Parser_Set_Delimiters(const Value: PAnsiChar);cdecl;
begin
     ComParser.Delimiters := Value;
end;
//------------------------------------------------------------------------------
procedure Parser_ResetDelimiters();cdecl;
begin
     ComParser.ResetDelims;
end;
//------------------------------------------------------------------------------
PROCEDURE Parser_Get_Vector(var ResultPtr: PDouble; ResultCount: PInteger; ExpectedSize: Integer);cdecl;
VAR
  Result: PDoubleArray;  i, ActualSize:Integer;
     VectorBuffer:ArrayDef.PDoubleArray;

begin
    VectorBuffer := Allocmem(SizeOf(VectorBuffer^[1])*ExpectedSize);
    ActualSize := ComParser.ParseAsVector(ExpectedSize, VectorBuffer);

    Result := DSS_RecreateArray_PDouble(ResultPtr, ResultCount, ((ActualSize-1)) + 1);
    For i := 0 to (ActualSize-1) Do Result[i] := VectorBuffer^[i+1];

    Reallocmem(VectorBuffer, 0);
end;
PROCEDURE Parser_Get_Vector_GR(ExpectedSize: Integer);cdecl;
// Same as Parser_Get_Vector but uses global result (GR) pointers
begin
   Parser_Get_Vector(GR_DataPtr_PDouble, GR_CountPtr_PDouble, ExpectedSize)
end;

//------------------------------------------------------------------------------
PROCEDURE Parser_Get_Matrix(var ResultPtr: PDouble; ResultCount: PInteger; ExpectedOrder: Integer);cdecl;
VAR
  Result: PDoubleArray;  i, MatrixSize:Integer;
     MatrixBuffer:ArrayDef.PDoubleArray;

begin
    MatrixSize := ExpectedOrder*ExpectedOrder;
    MatrixBuffer := Allocmem(SizeOf(MatrixBuffer^[1])*MatrixSize);
    ComParser.ParseAsMatrix(ExpectedOrder, MatrixBuffer);

    Result := DSS_RecreateArray_PDouble(ResultPtr, ResultCount, ((MatrixSize-1)) + 1);
    For i := 0 to (MatrixSize-1) Do Result[i] := MatrixBuffer^[i+1];

    Reallocmem(MatrixBuffer, 0);
end;
PROCEDURE Parser_Get_Matrix_GR(ExpectedOrder: Integer);cdecl;
// Same as Parser_Get_Matrix but uses global result (GR) pointers
begin
   Parser_Get_Matrix(GR_DataPtr_PDouble, GR_CountPtr_PDouble, ExpectedOrder)
end;

//------------------------------------------------------------------------------
PROCEDURE Parser_Get_SymMatrix(var ResultPtr: PDouble; ResultCount: PInteger; ExpectedOrder: Integer);cdecl;
VAR
  Result: PDoubleArray;  i, MatrixSize:Integer;
     MatrixBuffer:ArrayDef.PDoubleArray;

begin
    MatrixSize := ExpectedOrder*ExpectedOrder;
    MatrixBuffer := Allocmem(SizeOf(MatrixBuffer^[1])*MatrixSize);
    ComParser.ParseAsSymMatrix(ExpectedOrder, MatrixBuffer);

    Result := DSS_RecreateArray_PDouble(ResultPtr, ResultCount, ((MatrixSize-1)) + 1);
    For i := 0 to (MatrixSize-1) Do Result[i] := MatrixBuffer^[i+1];

    Reallocmem(MatrixBuffer, 0);

end;
PROCEDURE Parser_Get_SymMatrix_GR(ExpectedOrder: Integer);cdecl;
// Same as Parser_Get_SymMatrix but uses global result (GR) pointers
begin
   Parser_Get_SymMatrix(GR_DataPtr_PDouble, GR_CountPtr_PDouble, ExpectedOrder)
end;

//------------------------------------------------------------------------------
initialization
  ComParser := ParserDel.TParser.Create;  // create COM Parser object
            
END.
