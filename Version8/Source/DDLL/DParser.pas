unit DParser;

interface

function ParserI(mode: Longint; arg: Longint): Longint; CDECL;
function ParserF(mode: Longint; arg: Double): Double; CDECL;
function ParserS(mode: Longint; arg: pAnsiChar): pAnsiChar; CDECL;
procedure ParserV(mode: Longint; out arg: Variant); CDECL;

implementation

uses
    ComServ,
    ParserDel,
    Variants,
    ArrayDef;

var
    ComParser: ParserDel.TParser;

function ParserI(mode: Longint; arg: Longint): Longint; CDECL;
begin
    Result := 0;    // Default return value
    case mode of
        0:
        begin // Parser.IntValue
            Result := ComParser.IntValue;
        end;
        1:
        begin // Parser.ResetDelimiters
            ComParser.ResetDelims;
        end;
        2:
        begin  // Parser.Autoincrement read
            if ComParser.AutoIncrement then
                Result := 1;
        end;
        3:
        begin  // Parser.Autoincrement write
            if arg = 1 then
                ComParser.AutoIncrement := TRUE
            else
                ComParser.AutoIncrement := FALSE;
        end
    else
        Result := -1;
    end;
end;

//***************************Floating point type properties*********************
function ParserF(mode: Longint; arg: Double): Double; CDECL;
begin
    Result := 0.0; // Default return value
    case mode of
        0:
        begin  // Parser.DblValue
            Result := ComParser.DblValue;
        end
    else
        Result := -1.0;
    end;
end;

//***************************String type properties*****************************
function ParserS(mode: Longint; arg: pAnsiChar): pAnsiChar; CDECL;
begin
    Result := pAnsiChar(Ansistring('0')); // Default return value
    case mode of
        0:
        begin  // Parser.CmdString read
            Result := pAnsiChar(Ansistring(ComParser.CmdString));
        end;
        1:
        begin  // Parser.CmdString write
            ComParser.CmdString := Widestring(arg);
        end;
        2:
        begin  // Parser.NextParam
            Result := pAnsiChar(Ansistring(ComParser.NextParam));
        end;
        3:
        begin  // Parser.StrValue
            Result := pAnsiChar(Ansistring(ComParser.StrValue));
        end;
        4:
        begin  // Parser.WhiteSpace read
            Result := pAnsiChar(Ansistring(Comparser.Whitespace));
        end;
        5:
        begin  // Parser.WhiteSpace write
            ComParser.Whitespace := Widestring(arg);
        end;
        6:
        begin  // Parser.BeginQuote read
            Result := pAnsiChar(Ansistring(ComParser.BeginQuoteChars));
        end;
        7:
        begin  // Parser.BeginQuote write
            ComParser.BeginQuoteChars := Widestring(arg);
        end;
        8:
        begin  // Parser.EndQuote read
            Result := pAnsiChar(Ansistring(ComParser.EndQuoteChars));
        end;
        9:
        begin  // Parser.EndQuote write
            ComParser.EndQuoteChars := Widestring(arg);
        end;
        10:
        begin  // Parser.Delimiters read
            Result := pAnsiChar(Ansistring(ComParser.Delimiters));
        end;
        11:
        begin  // Parser.Delimiters write
            ComParser.Delimiters := Widestring(arg);
        end
    else
        Result := pAnsiChar(Ansistring('Error, parameter not valid'));
    end;
end;

//***************************Variant type properties****************************
procedure ParserV(mode: Longint; out arg: Variant); CDECL;

var
    i, ActualSize, MatrixSize: Integer;
    VectorBuffer: pDoubleArray;
    ExpectedSize, ExpectedOrder: Integer;
    MatrixBuffer: pDoubleArray;

begin
    case mode of
        0:
        begin  // Parser.Vector
            ExpectedSize := Integer(arg);
            VectorBuffer := Allocmem(SizeOf(VectorBuffer^[1]) * ExpectedSize);
            ActualSize := ComParser.ParseAsVector(ExpectedSize, VectorBuffer);
            arg := VarArrayCreate([0, (ActualSize - 1)], varDouble);
            for i := 0 to (ActualSize - 1) do
                arg[i] := VectorBuffer^[i + 1];
            Reallocmem(VectorBuffer, 0);
        end;
        1:
        begin  // Parser.Matrix
            ExpectedOrder := Integer(arg);
            MatrixSize := ExpectedOrder * ExpectedOrder;
            MatrixBuffer := Allocmem(SizeOf(MatrixBuffer^[1]) * MatrixSize);
            ComParser.ParseAsMatrix(ExpectedOrder, MatrixBuffer);

            arg := VarArrayCreate([0, (MatrixSize - 1)], varDouble);
            for i := 0 to (MatrixSize - 1) do
                arg[i] := MatrixBuffer^[i + 1];

            Reallocmem(MatrixBuffer, 0);
        end;
        2:
        begin  // Parser.SymMatrix
            ExpectedOrder := Integer(arg);
            MatrixSize := ExpectedOrder * ExpectedOrder;
            MatrixBuffer := Allocmem(SizeOf(MatrixBuffer^[1]) * MatrixSize);
            ComParser.ParseAsSymMatrix(ExpectedOrder, MatrixBuffer);

            arg := VarArrayCreate([0, (MatrixSize - 1)], varDouble);
            for i := 0 to (MatrixSize - 1) do
                arg[i] := MatrixBuffer^[i + 1];

            Reallocmem(MatrixBuffer, 0);
        end
    else
        arg[0] := 'Error, parameter not valid';
    end;
end;

end.
