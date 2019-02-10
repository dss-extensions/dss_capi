unit ImplParser;

{
  ----------------------------------------------------------
  Copyright (c) 2008-2015, Electric Power Research Institute, Inc.
  All rights reserved.
  ----------------------------------------------------------
}
{$WARN SYMBOL_PLATFORM OFF}

interface

uses
    ComObj,
    ActiveX,
    OpenDSSengine_TLB,
    StdVcl;

type
    TParser = class(TAutoObject, IParser)
    PROTECTED
        function Get_CmdString: Widestring; SAFECALL;
        procedure Set_CmdString(const Value: Widestring); SAFECALL;
        function Get_NextParam: Widestring; SAFECALL;
        function Get_AutoIncrement: Wordbool; SAFECALL;
        procedure Set_AutoIncrement(Value: Wordbool); SAFECALL;
        function Get_DblValue: Double; SAFECALL;
        function Get_IntValue: Integer; SAFECALL;
        function Get_StrValue: Widestring; SAFECALL;
        function Get_WhiteSpace: Widestring; SAFECALL;
        procedure Set_WhiteSpace(const Value: Widestring); SAFECALL;
        function Get_BeginQuote: Widestring; SAFECALL;
        function Get_EndQuote: Widestring; SAFECALL;
        procedure Set_BeginQuote(const Value: Widestring); SAFECALL;
        procedure Set_EndQuote(const Value: Widestring); SAFECALL;
        function Get_Delimiters: Widestring; SAFECALL;
        procedure Set_Delimiters(const Value: Widestring); SAFECALL;
        procedure ResetDelimiters; SAFECALL;
        function Get_Vector(ExpectedSize: Integer): Olevariant; SAFECALL;
        function Get_Matrix(ExpectedOrder: Integer): Olevariant; SAFECALL;
        function Get_SymMatrix(ExpectedOrder: Integer): Olevariant; SAFECALL;

    end;

implementation

uses
    ComServ,
    ParserDel,
    Variants,
    ArrayDef;

var
    ComParser: ParserDel.TParser;

function TParser.Get_CmdString: Widestring;
begin
    Result := ComParser.CmdString;
end;

procedure TParser.Set_CmdString(const Value: Widestring);
begin
    ComParser.CmdString := Value;
end;

function TParser.Get_NextParam: Widestring;
begin
    Result := ComParser.NextParam;
end;

function TParser.Get_AutoIncrement: Wordbool;
begin
    Result := ComParser.AutoIncrement;
end;

procedure TParser.Set_AutoIncrement(Value: Wordbool);
begin
    ComParser.AutoIncrement := Value;
end;

function TParser.Get_DblValue: Double;
begin
    Result := ComParser.DblValue;
end;

function TParser.Get_IntValue: Integer;
begin
    Result := ComParser.IntValue;
end;

function TParser.Get_StrValue: Widestring;
begin
    Result := ComParser.StrValue;
end;

function TParser.Get_WhiteSpace: Widestring;
begin
    Result := Comparser.Whitespace;
end;

procedure TParser.Set_WhiteSpace(const Value: Widestring);
begin
    ComParser.Whitespace := Value;
end;

function TParser.Get_BeginQuote: Widestring;
begin
    Result := ComParser.BeginQuoteChars;
end;

function TParser.Get_EndQuote: Widestring;
begin
    Result := ComParser.EndQuoteChars;
end;

procedure TParser.Set_BeginQuote(const Value: Widestring);
begin
    ComParser.BeginQuoteChars := Value;
end;

procedure TParser.Set_EndQuote(const Value: Widestring);
begin
    ComParser.EndQuoteChars := Value;
end;

function TParser.Get_Delimiters: Widestring;
begin
    Result := ComParser.Delimiters;
end;

procedure TParser.Set_Delimiters(const Value: Widestring);
begin
    ComParser.Delimiters := Value;
end;

procedure TParser.ResetDelimiters;
begin
    ComParser.ResetDelims;
end;

function TParser.Get_Vector(ExpectedSize: Integer): Olevariant;
var
    i, ActualSize: Integer;
    VectorBuffer: pDoubleArray;

begin
    VectorBuffer := Allocmem(SizeOf(VectorBuffer^[1]) * ExpectedSize);
    ActualSize := ComParser.ParseAsVector(ExpectedSize, VectorBuffer);

    Result := VarArrayCreate([0, (ActualSize - 1)], varDouble);
    for i := 0 to (ActualSize - 1) do
        Result[i] := VectorBuffer^[i + 1];

    Reallocmem(VectorBuffer, 0);
end;

function TParser.Get_Matrix(ExpectedOrder: Integer): Olevariant;
var
    i, MatrixSize: Integer;
    MatrixBuffer: pDoubleArray;

begin
    MatrixSize := ExpectedOrder * ExpectedOrder;
    MatrixBuffer := Allocmem(SizeOf(MatrixBuffer^[1]) * MatrixSize);
    ComParser.ParseAsMatrix(ExpectedOrder, MatrixBuffer);

    Result := VarArrayCreate([0, (MatrixSize - 1)], varDouble);
    for i := 0 to (MatrixSize - 1) do
        Result[i] := MatrixBuffer^[i + 1];

    Reallocmem(MatrixBuffer, 0);
end;

function TParser.Get_SymMatrix(ExpectedOrder: Integer): Olevariant;
var
    i, MatrixSize: Integer;
    MatrixBuffer: pDoubleArray;

begin
    MatrixSize := ExpectedOrder * ExpectedOrder;
    MatrixBuffer := Allocmem(SizeOf(MatrixBuffer^[1]) * MatrixSize);
    ComParser.ParseAsSymMatrix(ExpectedOrder, MatrixBuffer);

    Result := VarArrayCreate([0, (MatrixSize - 1)], varDouble);
    for i := 0 to (MatrixSize - 1) do
        Result[i] := MatrixBuffer^[i + 1];

    Reallocmem(MatrixBuffer, 0);

end;

initialization
    TAutoObjectFactory.Create(ComServer, TParser, Class_Parser,
        ciInternal, tmApartment);

    ComParser := ParserDel.TParser.Create;  // create COM Parser object

end.
