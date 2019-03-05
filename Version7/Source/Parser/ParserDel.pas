unit ParserDel;

{
  ----------------------------------------------------------
  Copyright (c) 2008-2015, Electric Power Research Institute, Inc.
  All rights reserved.
  ----------------------------------------------------------
}
{
   Command Line Parser Class

   This Version is a Simple version for embedding in Delphi Programs.

   3/20/01  Added Quote char properties and strings
}

{$M+}

interface

uses
    Arraydef,
    classes,{controls,}
{$IFDEF FPC}
    CmdForms,
{$ELSE}
    DSSForms,
{$ENDIF}
    Sysutils,
    RPN,
    HashList;

type
    EParserProblem = class(Exception);

     {Class for keeping a list of variablel and associate values}
    TParserVar = class(Tobject)
    PRIVATE
        ActiveVariable: Cardinal;
        StringArraySize: Cardinal;
        FsizeIncrement: Cardinal;

        VarNames: THashList;
        VarValues: pStringArray;
        function get_value: String;
        procedure set_value(const Value: String);
        function Get_VarString(Idx: Cardinal): String;
    PUBLIC

        NumVariables: Cardinal;

        constructor Create(InitSize: Cardinal);
        destructor Destroy; OVERRIDE;

        function Add(const VarName, VarValue: String): Integer;      // returns number of variables
        function Lookup(const VarName: String): Integer;                  // returns index or 0
        property Value: String READ get_value WRITE set_value;
        property VarString[Idx: Cardinal]: String READ Get_VarString;

    end;

    TParser = class(TObject)
    PRIVATE
        CmdBuffer: String;
        FPosition: Integer;
        ParameterBuffer: String;
        TokenBuffer: String;
        DelimChars: String;
        WhiteSpaceChars: String;
        FBeginQuoteChars, FEndQuoteChars: String;
        LastDelimiter: Char;
        MatrixRowTerminator: Char;
        FAutoIncrement: Boolean;
        ConvertError: Boolean;
        IsQuotedString: Boolean;
        RPNCalculator: TRPNCalc;
        function Get_Remainder: String;
        procedure SetCmdString(const Value: String);
        function MakeString: String;
        function MakeInteger: Integer;
        function MakeDouble: Double;
        function GetNextParam: String;
        procedure SkipWhiteSpace(const LineBuffer: String; var LinePos: Integer);
        function IsWhiteSpace(ch: Char): Boolean;
        function IsDelimiter(const LineBuffer: String; var LinePos: Integer): Boolean;
        function IsDelimChar(ch: Char): Boolean;
        function IsCommentChar(const LineBuffer: String; var LinePos: Integer): Boolean;
        function GetToken(const LineBuffer: String; var LinePos: Integer): String;
        function InterpretRPNString(var Code: Integer): Double;
    PROTECTED

    PUBLIC
        constructor Create;
        destructor Destroy; OVERRIDE;
        property DblValue: Double READ MakeDouble;
        property IntValue: Integer READ MakeInteger;
        property StrValue: String READ MakeString;
        property Token: String READ TokenBuffer WRITE TokenBuffer;
        property Remainder: String READ Get_Remainder;
        property NextParam: String READ GetNextParam;
        function ParseAsBusName(var NumNodes: Integer; NodeArray: pIntegerArray): String;
        function ParseAsVector(ExpectedSize: Integer; VectorBuffer: pDoubleArray): Integer;
        function ParseAsMatrix(ExpectedOrder: Integer; MatrixBuffer: pDoubleArray): Integer;
        function ParseAsSymMatrix(ExpectedOrder: Integer; MatrixBuffer: pDoubleArray): Integer;
        procedure ResetDelims;   // resets delimiters to default
        procedure CheckforVar(var TokenBuffer: String);
    PUBLISHED
        property CmdString: String READ CmdBuffer WRITE SetCmdString;
        property Position: Integer READ FPosition WRITE FPosition; // to save and restore
        property Delimiters: String READ DelimChars WRITE DelimChars;
        property Whitespace: String READ WhiteSpaceChars WRITE WhiteSpaceChars;
        property BeginQuoteChars: String READ FBeginQuoteChars WRITE FBeginQuoteChars;
        property EndQuoteChars: String READ FEndQuoteChars WRITE FEndQuoteChars;
        property AutoIncrement: Boolean READ FAutoIncrement WRITE FAutoIncrement;
    end;

var
    Parser: TParser;
    ParserVars: TParserVar;


implementation

{$IFNDEF FPC}
uses
    Dialogs;

{$ENDIF}

const
    Commentchar = '!';
    VariableDelimiter = '@';  // first character of a variable

{=======================================================================================================================}

function ProcessRPNCommand(const TokenBuffer: String; RPN: TRPNCalc): Integer;

var
    S: String;
    Number: Double;

begin
    Result := 0;  // Error Code on conversion error


     {First Try to make a valid number. If that fails, check for RPN command}

    Val(TokenBuffer, Number, Result);
    if Result = 0 then
        RPN.X := Number  // Enters number in X register

    else
    begin    {Check for RPN command. }
        Result := 0; // reset error return
        S := LowerCase(TokenBuffer);
        with RPN do
            if CompareStr(S, '+') = 0 then
                Add
            else
            if CompareStr(S, '-') = 0 then
                Subtract
            else
            if CompareStr(S, '*') = 0 then
                multiply
            else
            if CompareStr(S, '/') = 0 then
                Divide
            else
            if CompareStr(S, 'sqrt') = 0 then
                Sqrt
            else
            if CompareStr(S, 'sqr') = 0 then
                Square
            else
            if CompareStr(S, '^') = 0 then
                YToTheXPower
            else
            if CompareStr(S, 'sin') = 0 then
                SinDeg
            else
            if CompareStr(S, 'cos') = 0 then
                CosDeg
            else
            if CompareStr(S, 'tan') = 0 then
                TanDeg
            else
            if CompareStr(S, 'asin') = 0 then
                aSinDeg
            else
            if CompareStr(S, 'acos') = 0 then
                aCosDeg
            else
            if CompareStr(S, 'atan') = 0 then
                aTanDeg
            else
            if CompareStr(S, 'atan2') = 0 then
                aTan2Deg
            else
            if CompareStr(S, 'swap') = 0 then
                SwapXY
            else
            if CompareStr(S, 'rollup') = 0 then
                RollUp
            else
            if CompareStr(S, 'rolldn') = 0 then
                RollDn
            else
            if CompareStr(S, 'ln') = 0 then
                Natlog
            else
            if CompareStr(S, 'pi') = 0 then
                EnterPi
            else
            if CompareStr(S, 'log10') = 0 then
                TenLog
            else
            if CompareStr(S, 'exp') = 0 then
                etothex
            else
            if CompareStr(S, 'inv') = 0 then
                inv
            else
            begin
                raise EParserProblem.Create('Invalid inline math entry: "' + TokenBuffer + '"');
                Result := 1;  // error
            end;
    end;

end;

{=======================================================================================================================}

function StriptoDotPos(Dotpos: Integer; var S: String): String;

{Strips off everything up to a period.}

begin

    if dotpos = 0 then
        Result := S;
    Result := Copy(S, 1, dotpos - 1);
end;

{=======================================================================================================================}

procedure TParser.CheckforVar(var TokenBuffer: String);
var
    VariableValue,
    VariableName: String;
    DotPos,
    CaratPos: Integer;

   {-------------------------------------}
    procedure ReplaceToDotPos(const S: String);
    begin
        if DotPos > 0 then
            TokenBuffer := S + Copy(TokenBuffer, Dotpos, Length(TokenBuffer) - DotPos + 1)
        else
            TokenBuffer := S;
    end;

   {-------------------------------------}

begin

   {Replace TokenBuffer with Variable value if first character is VariableDelimiter character}
    if Length(TokenBuffer) > 1 then
        if TokenBuffer[1] = VariableDelimiter then  // looking for '@'
        begin
            Dotpos := pos('.', TokenBuffer);
            CaratPos := pos('^', TokenBuffer);
            if CaratPos > 0 then
                DotPos := CaratPos;   // Carat takes precedence

            if Dotpos > 0 then
                VariableName := StripToDotPos(DotPos, TokenBuffer)
            else
                VariableName := TokenBuffer;

            if ParserVars.Lookup(VariableName) > 0 then
            begin
                VariableValue := ParserVars.Value; // Retrieve the value of the variable
                if VariableValue[1] = '{' then
                begin
                    ReplaceToDotPos(Copy(VariableValue, 2, length(VariableValue) - 2));    // get rid of closed brace added by parservar
                    IsQuotedString := TRUE;  // force RPN parser to handle
                end
                else
                    ReplaceToDotPos(VariableValue);
            end;
        end;
end;

{=======================================================================================================================}

constructor TParser.Create;
begin
    inherited Create;

    DelimChars := ',=';
    WhiteSpaceChars := ' ' + #9;   // blank + tab
    FBeginQuoteChars := '("''[{';
    FEndQuoteChars := ')"'']}';
    FPosition := 1;
    MatrixRowTerminator := '|';
    FAutoIncrement := FALSE;
    RPNCalculator := TRPNCalc.Create;


end;

{=======================================================================================================================}

destructor TParser.Destroy;
begin
    RPNCalculator.Free;

    inherited Destroy;
end;

{=======================================================================================================================}

procedure TParser.SetCmdString(const Value: String);
begin
    CmdBuffer := Value + ' '; // add some white space at end to get last param
    FPosition := 1;
    SkipWhiteSpace(CmdBuffer, FPosition);   // position at first non whitespace character
end;

{=======================================================================================================================}

procedure TParser.ResetDelims;
begin
    DelimChars := ',=';
    WhiteSpaceChars := ' ' + #9;
    MatrixRowTerminator := '|';
    FBeginQuoteChars := '("''[{';
    FEndQuoteChars := ')"'']}';
end;

{=======================================================================================================================}

function TParser.IsWhiteSpace(ch: Char): Boolean;
var
    i: Integer;
begin
    Result := FALSE;
    for i := 1 to Length(WhiteSpaceChars) do
    begin
        if ch = WhiteSpaceChars[i] then
        begin
            Result := TRUE;
            Exit;
        end;
    end;
end;


{=======================================================================================================================}

function TParser.IsDelimiter(const LineBuffer: String; var LinePos: Integer): Boolean;
var
    i: Integer;
    ch: Char;
begin

    Result := FALSE;

    if IsCommentChar(LineBuffer, LinePos) then
    begin
        Result := TRUE;
        LastDelimiter := CommentChar;
        Exit;
    end;

    ch := LineBuffer[LinePos];

    for i := 1 to Length(DelimChars) do
    begin
        if ch = DelimChars[i] then
        begin
            Result := TRUE;
            LastDelimiter := ch;
            Exit;
        end;
    end;

    for i := 1 to Length(WhiteSpaceChars) do
    begin
        if ch = WhiteSpaceChars[i] then
        begin
            Result := TRUE;
            LastDelimiter := ' ';  // to indicate stopped on white space
            Exit;
        end;
    end;

end;


{=======================================================================================================================}

function TParser.IsDelimChar(ch: Char): Boolean;
var
    i: Integer;
begin
    Result := FALSE;
    for i := 1 to Length(DelimChars) do
    begin
        if ch = DelimChars[i] then
        begin
            Result := TRUE;
            Exit;
        end;
    end;
end;

{=======================================================================================================================}

procedure TParser.SkipWhiteSpace(const LineBuffer: String; var LinePos: Integer);
begin
    while (LinePos < Length(LineBuffer)) and
        IsWhiteSpace(LineBuffer[LinePos]) do
        Inc(LinePos);
end;

{=======================================================================================================================}

function TParser.GetToken(const LineBuffer: String; var LinePos: Integer): String;
var
    TokenStart: Integer;
    CmdBufLength: Integer;
    QuoteIndex: Integer;  // value of quote character found


   {---------------- Local Function -----------------------}
    procedure ParseToEndChar(Endchar: Char);
    begin
        Inc(LinePos);
        TokenStart := LinePos;
        while (LinePos < CmdBufLength) and (LineBuffer[LinePos] <> EndChar) do
            Inc(LinePos);

        GetToken := Copy(LineBuffer, TokenStart, LinePos - TokenStart);
        if LinePos < CmdBufLength then
            Inc(LinePos);  // Increment past endchar
    end;

   {---------------- Local Function -----------------------}
    procedure ParseToEndQuote;
    begin
        ParseToEndChar(FEndQuoteChars[QuoteIndex]);
        IsQuotedString := TRUE;
    end;

   {---------------- Local Function -----------------------}
    function IsBeginQuote(ch: Char): Boolean;
    begin
        QuoteIndex := Pos(ch, FBeginQuoteChars);
        if QuoteIndex > 0 then
            Result := TRUE
        else
            Result := FALSE;
    end;

begin
    Result := '';   // if it doesn't find anything, return null string
    CmdBufLength := Length(LineBuffer);
    if LinePos <= CmdBufLength then
    begin

   {Handle Quotes and Parentheses around tokens}
        IsQuotedString := FALSE;
        if IsBeginQuote(LineBuffer[LinePos]) then
            ParseToEndQuote
        else    { Copy to next delimiter or whitespace}
        begin
            TokenStart := LinePos;
            while (LinePos < CmdBufLength) and not IsDelimiter(LineBuffer, LinePos) do
                Inc(LinePos);

            Result := Copy(LineBuffer, TokenStart, (LinePos - TokenStart));
        end;


    { Check for stop on comment }

    // if stop on comment, ignore rest of line.
        if LastDelimiter = CommentChar then
            LinePos := Length(LineBuffer) + 1
        else
        begin

      {Get Rid of Trailing White Space}
            if LastDelimiter = ' ' then
                SkipWhiteSpace(LineBuffer, LinePos);
            if IsDelimchar(LineBuffer[LinePos]) then
            begin
                LastDelimiter := LineBuffer[LinePos];
                Inc(LinePos);  // Move past terminating delimiter
            end;
            SkipWhiteSpace(LineBuffer, LinePos);
        end;
    end;
end;


{=======================================================================================================================}

function TParser.GetNextParam: String;

begin

    if FPosition <= Length(CmdBuffer) then
    begin
        LastDelimiter := ' ';
        TokenBuffer := GetToken(CmdBuffer, FPosition); // Get entire token and put in token Buffer
        if (LastDelimiter = '=') then
        begin
            Parameterbuffer := tokenBuffer;     // put first token in Parameterbuffer
            TokenBuffer := Gettoken(CmdBuffer, FPosition);   // get token value after the =
        end
        else
        begin
            ParameterBuffer := '';  //init to null string
        end;
    end
    else
    begin    // return null strings if none left
        ParameterBuffer := '';
        TokenBuffer := '';
    end;

    CheckForVar(TokenBuffer);

    Result := ParameterBuffer;

end;

{=======================================================================================================================}

function TParser.ParseAsBusName(var NumNodes: Integer; NodeArray: pIntegerArray): String;

{ Looking for "BusName.1.2.3" in the TokenBuffer
  Assumes NodeArray is big enough to hold the numbers}

var
    DotPos, NodeBufferPos: Integer;
    NodeBuffer, DelimSave, TokenSave: String;

begin
    if FAutoIncrement then
        GetNextParam;
    NumNodes := 0;
    DotPos := Pos('.', TokenBuffer);
    if DotPos = 0 then
        Result := TokenBuffer
    else
    begin
        Result := Trim(Copy(TokenBuffer, 1, DotPos - 1)); // Bus Name
        TokenSave := TokenBuffer;
      {now Get nodes}
        NodeBuffer := Copy(tokenBuffer, DotPos + 1, Length(tokenBuffer) - DotPos) + ' ';

        NodeBufferPos := 1;
        DelimSave := DelimChars;
        DelimChars := '.';
        TokenBuffer := GetToken(NodeBuffer, NodeBufferPos);
        try
            while Length(TokenBuffer) > 0 do
            begin
                inc(NumNodes);
                NodeArray^[NumNodes] := MakeInteger;
                if ConvertError then
                    NodeArray^[NumNodes] := -1;  // Indicate an error
                TokenBuffer := GetToken(NodeBuffer, NodeBufferPos);
            end;
        except
            On E: Exception do
                DSSMessageDlg('Node Buffer Too Small: ' + E.Message, TRUE);
        end;

        DelimChars := DelimSave;   //restore to original delimiters
        TokenBuffer := TokenSave;
    end;

end;

{=======================================================================================================================}

function TParser.ParseAsVector(ExpectedSize: Integer; VectorBuffer: pDoubleArray): Integer;
var
    ParseBufferPos, NumElements, i: Integer;
    ParseBuffer, DelimSave: String;

begin

    if FAutoIncrement then
        GetNextParam;

    NumElements := 0;
    Result := 0;  // return 0 if none found or error occurred
    try
        for i := 1 to ExpectedSize do
            VectorBuffer^[i] := 0.0;

     {now Get Vector values}
        ParseBuffer := TokenBuffer + ' ';

        ParseBufferPos := 1;
        DelimSave := DelimChars;
        DelimChars := DelimChars + MatrixRowTerminator;

        SkipWhiteSpace(ParseBuffer, ParseBufferPos);
        TokenBuffer := GetToken(ParseBuffer, ParseBufferPos);
        CheckForVar(TokenBuffer);
        while Length(TokenBuffer) > 0 do
        begin
            inc(NumElements);
            if NumElements <= ExpectedSize then
                VectorBuffer^[NumElements] := MakeDouble;
            if LastDelimiter = MatrixRowTerminator then
                BREAK;
            TokenBuffer := GetToken(ParseBuffer, ParseBufferPos);
            CheckForVar(TokenBuffer);
        end;

        Result := NumElements;

    except
        On E: Exception do
            DSSMessageDlg('Vector Buffer in ParseAsVector Probably Too Small: ' + E.Message, TRUE);
    end;


    DelimChars := DelimSave;   //restore to original delimiters
    TokenBuffer := copy(ParseBuffer, ParseBufferPos, Length(ParseBuffer));  // prepare for next trip

end;

{=======================================================================================================================}

function TParser.ParseAsMatrix(ExpectedOrder: Integer; MatrixBuffer: pDoubleArray): Integer;

var
    i, j, k, ElementsFound: Integer;
    RowBuf: pDoubleArray;

begin

    if FAutoIncrement then
        GetNextParam;

    RowBuf := NIL;

    try
        RowBuf := Allocmem(Sizeof(Double) * ExpectedOrder);

        for i := 1 to (ExpectedOrder * ExpectedOrder) do
            MatrixBuffer^[i] := 0.0;

        for i := 1 to ExpectedOrder do
        begin

            ElementsFound := ParseAsVector(ExpectedOrder, RowBuf);

         { Returns matrix in Column Order (Fortran order) }
            k := i;
            for j := 1 to ElementsFound do
            begin
                MatrixBuffer^[k] := RowBuf^[j];
                Inc(k, ExpectedOrder);
            end;

        end;

    except
        On E: Exception do
            DSSMessageDlg('Matrix Buffer in ParseAsMatrix Probably Too Small: ' + E.Message, TRUE);
    end;

    if Assigned(RowBuf) then
        FreeMem(RowBuf, (Sizeof(Double) * ExpectedOrder));
    result := ExpectedOrder;
end;

{=======================================================================================================================}

function TParser.ParseAsSymMatrix(ExpectedOrder: Integer; MatrixBuffer: pDoubleArray): Integer;

var
    i, j,
    ElementsFound: Integer;
    RowBuf: pDoubleArray;

   {---------------- Local Function -----------------------}
    function ElementIndex(ii, jj: Integer): Integer;
    begin
        Result := (jj - 1) * ExpectedOrder + ii;
    end;

begin


    if FAutoIncrement then
        GetNextParam;

    RowBuf := NIL;

    try
        RowBuf := Allocmem(Sizeof(Double) * ExpectedOrder);

        for i := 1 to (ExpectedOrder * ExpectedOrder) do
            MatrixBuffer^[i] := 0.0;

        for i := 1 to ExpectedOrder do
        begin

            ElementsFound := ParseAsVector(ExpectedOrder, RowBuf);

         { Returns matrix in Column Order (Fortran order) }
            for j := 1 to ElementsFound do
            begin
                MatrixBuffer^[ElementIndex(i, j)] := RowBuf^[j];
                if i <> j then
                    MatrixBuffer^[ElementIndex(j, i)] := RowBuf^[j];
            end;

        end;

    except
        On E: Exception do
            DSSMessageDlg('Matrix Buffer in ParseAsSymMatrix Probably Too Small: ' + E.Message, TRUE);
    end;

    if Assigned(RowBuf) then
        FreeMem(RowBuf, (Sizeof(Double) * ExpectedOrder));
    Result := ExpectedOrder;

end;


{=======================================================================================================================}

function TParser.MakeString: String;
begin
    if FAutoIncrement then
        GetNextParam;

    Result := TokenBuffer;
end;

{=======================================================================================================================}

function TParser.MakeInteger: Integer;
 // Hex integers must be preceeded by "$"
var
    Code: Integer;
    Temp: Double;
begin
    ConvertError := FALSE;
    if FAutoIncrement then
        GetNextParam;

    if Length(TokenBuffer) = 0 then
    begin
        Result := 0;
    end
    else
    begin
        if IsQuotedString then
        begin
            Temp := InterpretRPNString(Code);
            Result := Round(Temp);
        end
        else
            Val(TokenBuffer, Result, Code);  // Try direct conversion to integer

        if Code <> 0 then
        begin // on error for integer conversion
             // Try again with an double result in case value specified in decimal or some other technique
            Val(Tokenbuffer, Temp, Code);
            if Code <> 0 then
            begin
               // not needed with Raise ...  Result := 0;
                ConvertError := TRUE;
                raise EParserProblem.Create('Integer number conversion error for string: "' + TokenBuffer + '"');
            end
            else
                Result := Round(Temp);
            ;
        end;
    end;
end;

{=======================================================================================================================}

function TParser.MakeDouble: Double;
var
    Code: Integer;
begin
    if FAutoIncrement then
        GetNextParam;
    ConvertError := FALSE;
    if Length(TokenBuffer) = 0 then
        Result := 0.0
    else
    begin
        if IsQuotedString then
            Result := InterpretRPNString(Code)
        else
            Val(TokenBuffer, Result, Code);

        if Code <> 0 then
        begin
           // not needed with Raise ...  Result := 0.0;
            ConvertError := TRUE;
            raise EParserProblem.Create('Floating point number conversion error for string: "' + TokenBuffer + '"');
        end;
    end;

end;

{=======================================================================================================================}

function TParser.Get_Remainder: String;
begin
    Result := Copy(CmdBuffer, FPosition, Length(CmdBuffer) - FPosition + 1)
end;

{=======================================================================================================================}

function TParser.IsCommentChar(const LineBuffer: String; var LinePos: Integer): Boolean;

{Checks for CommentChar and '//'}

begin
    case LineBuffer[LinePos] of
        CommentChar:
            Result := TRUE;
        '/':
        begin
            if (Length(LineBuffer) > LinePos) and (LineBuffer[LinePos + 1] = '/') then
                Result := TRUE
            else
                Result := FALSE;
        end;
    else
        Result := FALSE;
    end;


end;

{=======================================================================================================================}

function TParser.InterpretRPNString(var Code: Integer): Double;
var
    ParseBufferPos: Integer;
    ParseBuffer: String;

begin

    Code := 0;
    ParseBuffer := TokenBuffer + ' ';
    ParseBufferPos := 1;

    SkipWhiteSpace(ParseBuffer, ParseBufferPos);
    TokenBuffer := GetToken(ParseBuffer, ParseBufferPos);
    CheckForVar(TokenBuffer);

    while Length(TokenBuffer) > 0 do
    begin

        Code := ProcessRPNCommand(TokenBuffer, RPNCalculator);
        if Code > 0 then
            Break;  // Stop on any floating point error

        TokenBuffer := GetToken(ParseBuffer, ParseBufferPos);
        CheckForVar(TokenBuffer);
    end;

    Result := RPNCalculator.X;

    TokenBuffer := copy(ParseBuffer, ParseBufferPos, Length(ParseBuffer));  // prepare for next trip

end;

{===================================== Variable Support =============================================================}

{ TParserVar }

procedure ReallocStr(var S: pStringArray; oldSize, NewSize: Integer);
// Make a bigger block to hold the pointers to the strings
var
    X: pStringArray;
begin
    X := Allocmem(NewSize);   // Zero fills new string pointer array (important!)
    if OldSize > 0 then
    begin
        Move(S^, X^, OldSize);
        Freemem(S, Oldsize);
    end;
    S := X;
end;

{=======================================================================}

function TParserVar.Add(const VarName, VarValue: String): Integer;
var
    idx: Cardinal;
    VarDefinition: String;

    function EncloseQuotes(const s: String): String;
    begin
        Result := '{' + s + '}';
    end;

begin

    // First, check to see if the varname already exists
    // if so, just change the value
    idx := Varnames.Find(lowercase(Varname));

    if idx = 0 then
    begin
        idx := VarNames.Add(VarName);  // Hashlist will take care of itself
        if idx > StringArraySize then
        begin
            // resize String array
            ReallocStr(VarValues, Sizeof(VarValues^[1]) * StringArraySize, Sizeof(VarValues^[1]) * (StringArraySize + FsizeIncrement));
            inc(StringArraySize, FsizeIncrement);
        end;
    end;

    {If a variable used in the definition of a variable, enclose in quotes.}

    if pos('@', VarValue) > 0 then
        VarDefinition := EncloseQuotes(VarValue)
    else
        VarDefinition := VarValue;

    VarValues^[idx] := VarDefinition;
    NumVariables := VarNames.ListSize;
    Result := idx;

end;

{=======================================================================}

constructor TParserVar.Create(InitSize: Cardinal);
begin

    VarNames := THashList.Create(InitSize);
    VarValues := AllocStringArray(InitSize);
    StringArraySize := InitSize;
    FsizeIncrement := InitSize;

     // Intrinsic Variables go here...
    ActiveVariable := VarNames.Add('@lastfile');
    VarValues^[ActiveVariable] := 'null';  // null value
    ActiveVariable := VarNames.Add('@lastexportfile');
    VarValues^[ActiveVariable] := 'null';  // null value
    ActiveVariable := VarNames.Add('@lastshowfile');
    VarValues^[ActiveVariable] := 'null';  // null value
    ActiveVariable := VarNames.Add('@lastplotfile');
    VarValues^[ActiveVariable] := 'null';  // null value
    ActiveVariable := VarNames.Add('@lastredirectfile');
    VarValues^[ActiveVariable] := 'null';  // null value
    ActiveVariable := VarNames.Add('@lastcompilefile');
    VarValues^[ActiveVariable] := 'null';  // null value
    ActiveVariable := VarNames.Add('@result');
    VarValues^[ActiveVariable] := 'null';  // null value

    NumVariables := Varnames.ListSize;

end;

{=======================================================================}

destructor TParserVar.Destroy;
begin
    VarNames.Free;
    FreeStringArray(VarValues, StringArraySize);

    inherited;
end;

{=======================================================================}

function TParserVar.get_value: String;
begin
    if ActiveVariable > 0 then
        Result := VarValues^[ActiveVariable]
    else
        Result := '';
end;

function TParserVar.Get_VarString(Idx: Cardinal): String;
    function TestEmpty(const s: String): String;
    begin
        if Length(s) = 0 then
            Result := 'null'
        else
            Result := S;
    end;

begin
    if (idx > 0) and (idx <= NumVariables) then
        Result := Format('%s. %s', [Varnames.Get(idx), TestEmpty(VarValues^[idx])])
    else
        Result := 'Variable index out of range';
end;

function TParserVar.Lookup(const VarName: String): Integer;
begin
    ActiveVariable := VarNames.Find(VarName);
    Result := ActiveVariable;
end;

procedure TParserVar.set_value(const Value: String);
begin
    if (ActiveVariable > 0) and (ActiveVariable <= NumVariables) then
        VarValues^[ActiveVariable] := Value;

end;

initialization

    // Variables
    ParserVars := TParserVar.Create(100);  // start with space for 100 variables

finalization

    ParserVars.Free;

end.
