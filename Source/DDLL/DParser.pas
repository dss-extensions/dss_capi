unit DParser;

interface

function ParserI(mode: Longint; arg: Longint): Longint; CDECL;
function ParserF(mode: Longint; arg: Double): Double; CDECL;
function ParserS(mode: Longint; arg: Pansichar): Pansichar; CDECL;
procedure ParserV(mode: Longint; var myPointer: Pointer; var myType, mySize: Longint); CDECL;

implementation

uses
{$IFNDEF FPC_DLL}
    ComServ,
    Dialogs,
{$ENDIF}
    ParserDel,
    Variants,
    ArrayDef,
    DSSGlobals,
    sysutils,
    ExceptionTrace;

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
function ParserS(mode: Longint; arg: Pansichar): Pansichar; CDECL;
begin
    Result := Pansichar(Ansistring('0')); // Default return value
    case mode of
        0:
        begin  // Parser.CmdString read
            Result := Pansichar(Ansistring(ComParser.CmdString));
        end;
        1:
        begin  // Parser.CmdString write
            ComParser.CmdString := String(arg);
        end;
        2:
        begin  // Parser.NextParam
            Result := Pansichar(Ansistring(ComParser.NextParam));
        end;
        3:
        begin  // Parser.StrValue
            Result := Pansichar(Ansistring(ComParser.StrValue));
        end;
        4:
        begin  // Parser.WhiteSpace read
            Result := Pansichar(Ansistring(Comparser.Whitespace));
        end;
        5:
        begin  // Parser.WhiteSpace write
            ComParser.Whitespace := String(arg);
        end;
        6:
        begin  // Parser.BeginQuote read
            Result := Pansichar(Ansistring(ComParser.BeginQuoteChars));
        end;
        7:
        begin  // Parser.BeginQuote write
            ComParser.BeginQuoteChars := String(arg);
        end;
        8:
        begin  // Parser.EndQuote read
            Result := Pansichar(Ansistring(ComParser.EndQuoteChars));
        end;
        9:
        begin  // Parser.EndQuote write
            ComParser.EndQuoteChars := String(arg);
        end;
        10:
        begin  // Parser.Delimiters read
            Result := Pansichar(Ansistring(ComParser.Delimiters));
        end;
        11:
        begin  // Parser.Delimiters write
            ComParser.Delimiters := String(arg);
        end
    else
        Result := Pansichar(Ansistring('Error, parameter not valid'));
    end;
end;

//***************************Variant type properties****************************
procedure ParserV(mode: Longint; var myPointer: Pointer; var myType, mySize: Longint); CDECL;

var
    i,
    ActualSize,
    ExpectedSize,
    ExpectedOrder,
    MatrixSize: Integer;
    VectorBuffer: pDoubleArray;
    MatrixBuffer: pDoubleArray;

begin
  {$IFDEF FPC_DLL}
    initialize(VectorBuffer);
    initialize(MatrixBuffer);
{$ENDIF}
    case mode of
        0:
        begin  // Parser.Vector
            myType := 2;        // Double
            setlength(myDBLArray, 1);
            myDBLArray[0] := 0;
            ExpectedSize := Integer(mySize);
            VectorBuffer := Allocmem(SizeOf(VectorBuffer^[1]) * ExpectedSize);
            ActualSize := ComParser.ParseAsVector(ExpectedSize, VectorBuffer);
            setlength(myDBLArray, ActualSize);
            for i := 0 to (ActualSize - 1) do
                myDBLArray[i] := VectorBuffer^[i + 1];
            Reallocmem(VectorBuffer, 0);
            myPointer := @(myDBLArray[0]);
            mySize := SizeOf(myDBLArray[0]) * Length(myDBLArray);
        end;
        1:
        begin  // Parser.Matrix
            myType := 2;        // Double
            setlength(myDBLArray, 1);
            ExpectedOrder := Integer(mySize);
            MatrixSize := ExpectedOrder * ExpectedOrder;
            MatrixBuffer := Allocmem(SizeOf(MatrixBuffer^[1]) * MatrixSize);
            ComParser.ParseAsMatrix(ExpectedOrder, MatrixBuffer);

            setlength(myDBLArray, MatrixSize);
            for i := 0 to (MatrixSize - 1) do
                myDBLArray[i] := MatrixBuffer^[i + 1];

            Reallocmem(MatrixBuffer, 0);
            myPointer := @(myDBLArray[0]);
            mySize := SizeOf(myDBLArray[0]) * Length(myDBLArray);
        end;
        2:
        begin  // Parser.SymMatrix
            myType := 2;        // Double
            setlength(myDBLArray, 1);
            ExpectedOrder := Integer(mySize);
            MatrixSize := ExpectedOrder * ExpectedOrder;
            MatrixBuffer := Allocmem(SizeOf(MatrixBuffer^[1]) * MatrixSize);
            ComParser.ParseAsSymMatrix(ExpectedOrder, MatrixBuffer);

            setlength(myDBLArray, MatrixSize);
            for i := 0 to (MatrixSize - 1) do
                myDBLArray[i] := MatrixBuffer^[i + 1];

            Reallocmem(MatrixBuffer, 0);
            myPointer := @(myDBLArray[0]);
            mySize := SizeOf(myDBLArray[0]) * Length(myDBLArray);
        end
    else
    begin
        myType := 4;        // String
        setlength(myStrArray, 0);
        WriteStr2Array('Error, parameter not recognized');
        myPointer := @(myStrArray[0]);
        mySize := Length(myStrArray);
    end;
    end;
end;

initialization
  {$IFDEF FPC_TRACE_INIT}
    writeln(format('init %s:%s', [{$I %FILE%}, {$I %LINE%}]));
{$ENDIF}
    try
        ComParser := ParserDel.TParser.Create;  // create COM Parser object
    except
        On E: Exception do
            DumpExceptionCallStack(E);
    end;
end.
