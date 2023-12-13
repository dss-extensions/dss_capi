unit ParserDel;
// ----------------------------------------------------------
// Copyright (c) 2008-2015, Electric Power Research Institute, Inc.
// All rights reserved.
// ----------------------------------------------------------
interface

uses
    Arraydef,
    classes,
    Sysutils,
    RPN,
    HashList;

type
    EParserProblem = class(Exception);

    // Class for keeping a list of variablel and associate values
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

    TDSSParser = class(TObject)
    PRIVATE
        ParserVars: TParserVar; // reference to global parser vars
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
        procedure SkipWhiteSpace(const LineBuffer: String; var LinePos: Integer);
        function IsWhiteSpace(ch: Char): Boolean;
        function IsDelimiter(const LineBuffer: String; var LinePos: Integer): Boolean;
        function IsDelimChar(ch: Char): Boolean;
        function IsCommentChar(const LineBuffer: String; var LinePos: Integer): Boolean;
        function GetToken(const LineBuffer: String; var LinePos: Integer): String;
        function InterpretRPNString(var Code: Integer; requiredRPN: PBoolean = NIL): Double;
    PUBLIC
        DSSCtx: TObject;
        function MakeDouble(requiredRPN: PBoolean): Double; overload;
        function MakeDouble(): Double; overload;

        constructor Create(dssContext: TObject);
        destructor Destroy; OVERRIDE;
        property DblValue: Double READ MakeDouble;
        property IntValue: Integer READ MakeInteger;
        property StrValue: String READ MakeString;
        property Token: String READ TokenBuffer WRITE TokenBuffer;
        property Remainder: String READ Get_Remainder;
        function NextParam(): String;
        function ParseAsBusName(Param: String; var NumNodes: Integer; NodeArray: pIntegerArray): String;//TODO: make it a separate function
        function ParseAsVector(ExpectedSize: Integer; VectorBuffer: pDoubleArray; DoRound: Boolean=False): Integer;
        function ParseAsVector(var VectorBuffer: ArrayOfDouble; DoRound: Boolean=False): Integer;

        // TODO: remove, not used in the main code, only in the COM API, kinda useless for most common programming languages
        function ParseAsMatrix(ExpectedOrder: Integer; MatrixBuffer: pDoubleArray): Integer;

        function ParseAsSymMatrix(ExpectedOrder: Integer; MatrixBuffer: pDoubleArray; Stride: Integer = 1; Scale: Double = 1): Integer;
        function ParseAsSymMatrix(var MatrixBuffer: ArrayOfDouble; Stride: Integer = 1; Scale: Double = 1): Integer;
        procedure ResetDelims;   // resets delimiters to default
        function CheckforVar(var TokenBuffer_: String): Boolean;
        procedure SetVars(vars: TParserVar);

        property CmdString: String READ CmdBuffer WRITE SetCmdString;
        property Position: Integer READ FPosition WRITE FPosition; // to save and restore
        property Delimiters: String READ DelimChars WRITE DelimChars;
        property Whitespace: String READ WhiteSpaceChars WRITE WhiteSpaceChars;
        property BeginQuoteChars: String READ FBeginQuoteChars WRITE FBeginQuoteChars;
        property EndQuoteChars: String READ FEndQuoteChars WRITE FEndQuoteChars;
        property AutoIncrement: Boolean READ FAutoIncrement WRITE FAutoIncrement;
    end;

implementation

uses
    DSSClass,
    DSSHelper,
    DSSGlobals,
    Math;

const
    Commentchar = '!';
    VariableDelimiter = '@';  // first character of a variable


procedure ProcessRPNCommand(const TokenBuffer: String; RPN: TRPNCalc);
var
    S: String;
    Number: Double;
    ErrorCode: Integer = 0;
begin
    // First Try to make a valid number. If that fails, check for RPN command

    Val(TokenBuffer, Number, ErrorCode);
    if ErrorCode = 0 then
    begin
        RPN.X := Number;  // Enters number in X register
        Exit;
    end;

    // Check for RPN command.
    S := AnsiLowerCase(TokenBuffer);
    if CompareStr(S, '+') = 0 then
        RPN.Add()
    else
    if CompareStr(S, '-') = 0 then
        RPN.Subtract()
    else
    if CompareStr(S, '*') = 0 then
        RPN.Multiply()
    else
    if CompareStr(S, '/') = 0 then
        RPN.Divide()
    else
    if CompareStr(S, 'sqrt') = 0 then
        RPN.Sqrt()
    else
    if CompareStr(S, 'sqr') = 0 then
        RPN.Square()
    else
    if CompareStr(S, '^') = 0 then
        RPN.YToTheXPower()
    else
    if CompareStr(S, 'sin') = 0 then
        RPN.SinDeg()
    else
    if CompareStr(S, 'cos') = 0 then
        RPN.CosDeg()
    else
    if CompareStr(S, 'tan') = 0 then
        RPN.TanDeg()
    else
    if CompareStr(S, 'asin') = 0 then
        RPN.aSinDeg()
    else
    if CompareStr(S, 'acos') = 0 then
        RPN.aCosDeg()
    else
    if CompareStr(S, 'atan') = 0 then
        RPN.aTanDeg()
    else
    if CompareStr(S, 'atan2') = 0 then
        RPN.aTan2Deg()
    else
    if CompareStr(S, 'swap') = 0 then
        RPN.SwapXY()
    else
    if CompareStr(S, 'rollup') = 0 then
        RPN.RollUp()
    else
    if CompareStr(S, 'rolldn') = 0 then
        RPN.RollDn()
    else
    if CompareStr(S, 'ln') = 0 then
        RPN.Natlog()
    else
    if CompareStr(S, 'pi') = 0 then
        RPN.EnterPi()
    else
    if CompareStr(S, 'log10') = 0 then
        RPN.TenLog()
    else
    if CompareStr(S, 'exp') = 0 then
        RPN.etothex()
    else
    if CompareStr(S, 'inv') = 0 then
        RPN.inv()
    else
    begin
        raise EParserProblem.Create('Invalid inline math entry: "' + TokenBuffer + '"');
        // Result := 1;  // error -- REMOVED: never reached
    end;
end;

function StriptoDotPos(Dotpos: Integer; var S: String): String;
// Strips off everything up to a period.
begin
    if dotpos = 0 then
        Result := S;
    Result := Copy(S, 1, dotpos - 1);
end;

function TDSSParser.CheckforVar(var TokenBuffer_: String): Boolean;
var
    VariableValue,
    VariableName: String;
    DotPos,
    CaratPos: Integer;
    inputStr: String;

    procedure ReplaceToDotPos(const S: String);
    begin
        if DotPos > 0 then
            TokenBuffer_ := S + Copy(TokenBuffer_, Dotpos, Length(TokenBuffer_) - DotPos + 1)
        else
            TokenBuffer_ := S;
    end;
begin
    inputStr := TokenBuffer_;
    // Replace TokenBuffer_ with Variable value if first character is VariableDelimiter character
    if Length(TokenBuffer_) > 1 then
        if TokenBuffer_[1] = VariableDelimiter then  // looking for '@'
        begin
            Dotpos := pos('.', TokenBuffer_);
            CaratPos := pos('^', TokenBuffer_);
            if CaratPos > 0 then
                DotPos := CaratPos;   // Carat takes precedence

            if Dotpos > 0 then
                VariableName := StripToDotPos(DotPos, TokenBuffer_)
            else
                VariableName := TokenBuffer_;

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

    Result := (inputStr <> TokenBuffer_);
end;

constructor TDSSParser.Create(dssContext: TObject);
begin
    inherited Create;

    DSSCtx := dssContext;
    ParserVars := nil;
    DelimChars := ',=';
    WhiteSpaceChars := ' ' + #9;   // blank + tab
    FBeginQuoteChars := '("''[{';
    FEndQuoteChars := ')"'']}';
    FPosition := 1;
    MatrixRowTerminator := '|';
    FAutoIncrement := FALSE;
    RPNCalculator := TRPNCalc.Create;
end;

destructor TDSSParser.Destroy;
begin
    RPNCalculator.Free;

    inherited Destroy;
end;

procedure TDSSParser.SetCmdString(const Value: String);
begin
    CmdBuffer := Value + ' '; // add some white space at end to get last param
    FPosition := 1;
    SkipWhiteSpace(CmdBuffer, FPosition);   // position at first non whitespace character
end;

procedure TDSSParser.ResetDelims;
begin
    DelimChars := ',=';
    WhiteSpaceChars := ' ' + #9;
    MatrixRowTerminator := '|';
    FBeginQuoteChars := '("''[{';
    FEndQuoteChars := ')"'']}';
end;

function TDSSParser.IsWhiteSpace(ch: Char): Boolean;
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

function TDSSParser.IsDelimiter(const LineBuffer: String; var LinePos: Integer): Boolean;
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

function TDSSParser.IsDelimChar(ch: Char): Boolean;
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

procedure TDSSParser.SkipWhiteSpace(const LineBuffer: String; var LinePos: Integer);
begin
    while (LinePos < Length(LineBuffer)) and
        IsWhiteSpace(LineBuffer[LinePos]) do
        Inc(LinePos);
end;

function TDSSParser.GetToken(const LineBuffer: String; var LinePos: Integer): String;
var
    TokenStart: Integer;
    CmdBufLength: Integer;
    QuoteIndex: Integer;  // value of quote character found

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

    procedure ParseToEndQuote;
    begin
        ParseToEndChar(FEndQuoteChars[QuoteIndex]);
        IsQuotedString := TRUE;
    end;

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
        // Handle Quotes and Parentheses around tokens
        IsQuotedString := FALSE;
        if IsBeginQuote(LineBuffer[LinePos]) then
            ParseToEndQuote
        else // Copy to next delimiter or whitespace
        begin
            TokenStart := LinePos;
            while (LinePos < CmdBufLength) and not IsDelimiter(LineBuffer, LinePos) do
                Inc(LinePos);

            Result := Copy(LineBuffer, TokenStart, (LinePos - TokenStart));
        end;

        // Check for stop on comment 

        // if stop on comment, ignore rest of line.
        if LastDelimiter = CommentChar then
            LinePos := Length(LineBuffer) + 1
        else
        begin
            // Get Rid of Trailing White Space
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

function TDSSParser.NextParam(): String;
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

function TDSSParser.ParseAsBusName(Param: String; var NumNodes: Integer; NodeArray: pIntegerArray): String;
// Looking for "BusName.1.2.3" in the TokenBuffer
// Assumes NodeArray is big enough to hold the numbers
var
    DotPos, NodeBufferPos: Integer;
    NodeBuffer, DelimSave, TokenSave: String;
begin
    TokenBuffer := Param;
    if FAutoIncrement then
        NextParam();
    NumNodes := 0;
    DotPos := Pos('.', TokenBuffer);
    if DotPos = 0 then
        Result := TokenBuffer
    else
    begin
        Result := Trim(Copy(TokenBuffer, 1, DotPos - 1)); // Bus Name
        TokenSave := TokenBuffer;
        // now Get nodes
        NodeBuffer := Copy(tokenBuffer, DotPos + 1, Length(tokenBuffer) - DotPos) + ' ';

        NodeBufferPos := 1;
        DelimSave := DelimChars;
        DelimChars := '.';
        TokenBuffer := GetToken(NodeBuffer, NodeBufferPos);
        try
            while Length(TokenBuffer) > 0 do
            begin
                inc(NumNodes);
                NodeArray[NumNodes] := MakeInteger;
                if ConvertError then
                    NodeArray[NumNodes] := -1;  // Indicate an error
                TokenBuffer := GetToken(NodeBuffer, NodeBufferPos);
            end;
        except
            On E: Exception do
                TDSSContext(DSSCtx).MessageDlg('Node Buffer Too Small: ' + E.Message, TRUE);
        end;

        DelimChars := DelimSave;   //restore to original delimiters
        TokenBuffer := TokenSave;
    end;
end;

function TDSSParser.ParseAsVector(var VectorBuffer: ArrayOfDouble; DoRound: Boolean): Integer;
begin
    Result := ParseAsVector(Length(VectorBuffer), pDoubleArray(@VectorBuffer[0]), DoRound);
end;

function TDSSParser.ParseAsVector(ExpectedSize: Integer; VectorBuffer: pDoubleArray; DoRound: Boolean): Integer;
var
    ParseBufferPos, NumElements, i: Integer;
    ParseBuffer, DelimSave: String;
begin
    if FAutoIncrement then
        NextParam();

    NumElements := 0;
    Result := 0;  // return 0 if none found or error occurred
    try
        for i := 1 to ExpectedSize do
            VectorBuffer[i] := 0.0;

        // now Get Vector values
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
                VectorBuffer[NumElements] := MakeDouble;
            //TODO: warn about extra elements
            if LastDelimiter = MatrixRowTerminator then
                BREAK;
            TokenBuffer := GetToken(ParseBuffer, ParseBufferPos);
            CheckForVar(TokenBuffer);
        end;

        Result := NumElements;

    except
        On E: Exception do
            TDSSContext(DSSCtx).MessageDlg('Vector Buffer in ParseAsVector Probably Too Small: ' + E.Message, TRUE);
    end;

    DelimChars := DelimSave;   //restore to original delimiters
    TokenBuffer := copy(ParseBuffer, ParseBufferPos, Length(ParseBuffer));  // prepare for next trip
    if DoRound then
        for i := 1 to Math.Min(NumElements, ExpectedSize) do
            VectorBuffer[i] := Round(VectorBuffer[i]);
end;

function TDSSParser.ParseAsMatrix(ExpectedOrder: Integer; MatrixBuffer: pDoubleArray): Integer;
var
    i, j, k, ElementsFound: Integer;
    RowBuf: pDoubleArray;
begin
    Result := 0;
    if FAutoIncrement then
        NextParam();

    RowBuf := Allocmem(Sizeof(Double) * ExpectedOrder);
    for i := 1 to (ExpectedOrder * ExpectedOrder) do
        MatrixBuffer[i] := 0.0;

    try
        for i := 1 to ExpectedOrder do
        begin
            ElementsFound := ParseAsVector(ExpectedOrder, RowBuf);

            if ElementsFound > (ExpectedOrder * ExpectedOrder) then
            begin
                DoSimpleMsg(TDSSContext(DSSCtx), _('Matrix Buffer in ParseAsMatrix too small. Check your input data, especially dimensions and number of phases.'), 65533);
                Exit;
            end;

            // Returns matrix in Column Order (Fortran order)
            k := i;
            for j := 1 to ElementsFound do
            begin
                MatrixBuffer[k] := RowBuf[j];
                Inc(k, ExpectedOrder);
            end;
        end;
        Result := ExpectedOrder;
    finally
        if Assigned(RowBuf) then
            FreeMem(RowBuf, (Sizeof(Double) * ExpectedOrder));
    end;
end;

function TDSSParser.ParseAsSymMatrix(var MatrixBuffer: ArrayOfDouble; Stride: Integer; Scale: Double): Integer;
begin
    Result := ParseAsSymMatrix(Length(MatrixBuffer), pDoubleArray(@MatrixBuffer[0]), Stride);
end;

function TDSSParser.ParseAsSymMatrix(ExpectedOrder: Integer; MatrixBuffer: pDoubleArray; Stride: Integer; Scale: Double): Integer;
var
    i, j, subpos, maxpos,
    ElementsFound: Integer;
    RowBuf: pDoubleArray;
begin
    Result := 0;
    if FAutoIncrement then
        NextParam();

    RowBuf := Allocmem(Sizeof(Double) * ExpectedOrder);
    maxpos := ExpectedOrder * ExpectedOrder - 1;
    for i := 0 to (ExpectedOrder * ExpectedOrder) - 1 do
        MatrixBuffer[i * Stride + 1] := 0.0;

    try
        for i := 0 to (ExpectedOrder - 1) do
        begin
            ElementsFound := ParseAsVector(ExpectedOrder, RowBuf);

            for j := 0 to (ElementsFound - 1) do
            begin
                subpos := (j * ExpectedOrder + i);
                if subpos > maxpos then
                begin
                    DoSimpleMsg(TDSSContext(DSSCtx), _('Matrix Buffer in ParseAsSymMatrix too small. Check your input data, especially dimensions and number of phases.'), 65534);
                    Exit;
                end;

                MatrixBuffer[subpos * Stride + 1] := RowBuf[j + 1] * Scale;

                if i = j then
                    continue;

                subpos := (i * ExpectedOrder + j);
                if subpos > maxpos then
                begin
                    DoSimpleMsg(TDSSContext(DSSCtx), _('Matrix Buffer in ParseAsSymMatrix too small. Check your input data, especially dimensions and number of phases.'), 65534);
                    Exit;
                end;
                MatrixBuffer[subpos * Stride + 1] := RowBuf[j + 1] * Scale;
            end;
        end;
        Result := ExpectedOrder;
    finally
        FreeMem(RowBuf, (Sizeof(Double) * ExpectedOrder));
    end;
end;

function TDSSParser.MakeString: String;
begin
    if FAutoIncrement then
        NextParam();

    Result := TokenBuffer;
end;

function TDSSParser.MakeInteger: Integer;
 // Hex integers must be preceeded by "$"
var
    Code: Integer;
    Temp: Double;
begin
    ConvertError := FALSE;
    if FAutoIncrement then
        NextParam();

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
                raise EParserProblem.Create(Format('Integer number conversion error for string: "%s"', [TokenBuffer]));
            end
            else
                Result := Round(Temp);
        end;
    end;
end;

function TDSSParser.MakeDouble(): Double; overload;
begin
    Result := MakeDouble(NIL);
end;

function TDSSParser.MakeDouble(requiredRPN: PBoolean): Double; overload;
var
    Code: Integer;
begin
    if FAutoIncrement then
        NextParam();
    ConvertError := FALSE;
    if Length(TokenBuffer) = 0 then
        Result := 0.0
    else
    begin
        if IsQuotedString then
        begin
            Result := InterpretRPNString(Code, requiredRPN)
        end
        else
        begin
            if requiredRPN <> NIL then
                requiredRPN^ := false;
            Val(TokenBuffer, Result, Code);
        end;

        if Code <> 0 then
        begin
           // not needed with Raise ...  Result := 0.0;
            ConvertError := TRUE;
            raise EParserProblem.Create('Floating point number conversion error for string: "' + TokenBuffer + '"');
        end;
    end;
end;

function TDSSParser.Get_Remainder: String;
begin
    Result := Copy(CmdBuffer, FPosition, Length(CmdBuffer) - FPosition + 1)
end;

function TDSSParser.IsCommentChar(const LineBuffer: String; var LinePos: Integer): Boolean;
// Checks for CommentChar and '//'
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

function TDSSParser.InterpretRPNString(var Code: Integer; requiredRPN: PBoolean = NIL): Double;
var
    ParseBufferPos: Integer;
    ParseBuffer: String;
    cnt: Integer = 0;    
begin
    Code := 0;
    ParseBuffer := TokenBuffer + ' ';
    ParseBufferPos := 1;

    SkipWhiteSpace(ParseBuffer, ParseBufferPos);
    TokenBuffer := GetToken(ParseBuffer, ParseBufferPos);
    if CheckForVar(TokenBuffer) then
        inc(cnt);

    while Length(TokenBuffer) > 0 do
    begin
        ProcessRPNCommand(TokenBuffer, RPNCalculator);
        TokenBuffer := GetToken(ParseBuffer, ParseBufferPos);
        CheckForVar(TokenBuffer);
        inc(cnt);
    end;

    if (requiredRPN <> NIL) then
        requiredRPN^ := (cnt > 1);

    Result := RPNCalculator.X;

    TokenBuffer := copy(ParseBuffer, ParseBufferPos, Length(ParseBuffer));  // prepare for next trip
end;

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
    idx := VarNames.Find(Varname);

    if idx = 0 then
    begin
        idx := VarNames.Add(VarName);  // Hashlist will take care of itself
        if idx > StringArraySize then
        begin
            // resize String array
            ReallocStr(VarValues, Sizeof(VarValues[1]) * StringArraySize, Sizeof(VarValues[1]) * (StringArraySize + FsizeIncrement));
            inc(StringArraySize, FsizeIncrement);
        end;
    end;

    // If a variable used in the definition of a variable, enclose in quotes.
    if pos('@', VarValue) > 0 then
        VarDefinition := EncloseQuotes(VarValue)
    else
        VarDefinition := VarValue;

    VarValues[idx] := VarDefinition;
    NumVariables := VarNames.Count;
    Result := idx;
end;

constructor TParserVar.Create(InitSize: Cardinal);
begin
    VarNames := THashList.Create(InitSize);
    VarValues := AllocStringArray(InitSize);
    StringArraySize := InitSize;
    FsizeIncrement := InitSize;

     // Intrinsic Variables go here...
    ActiveVariable := VarNames.Add('@lastfile');
    VarValues[ActiveVariable] := 'null';  // null value
    ActiveVariable := VarNames.Add('@lastexportfile');
    VarValues[ActiveVariable] := 'null';  // null value
    ActiveVariable := VarNames.Add('@lastshowfile');
    VarValues[ActiveVariable] := 'null';  // null value
    ActiveVariable := VarNames.Add('@lastplotfile');
    VarValues[ActiveVariable] := 'null';  // null value
    ActiveVariable := VarNames.Add('@lastredirectfile');
    VarValues[ActiveVariable] := 'null';  // null value
    ActiveVariable := VarNames.Add('@lastcompilefile');
    VarValues[ActiveVariable] := 'null';  // null value
    ActiveVariable := VarNames.Add('@result');
    VarValues[ActiveVariable] := 'null';  // null value

    NumVariables := VarNames.Count;
end;

destructor TParserVar.Destroy;
begin
    VarNames.Free;
    FreeStringArray(VarValues, StringArraySize);

    inherited;
end;

function TParserVar.get_value: String;
begin
    if ActiveVariable > 0 then
        Result := VarValues[ActiveVariable]
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
        Result := Format('%s. %s', [VarNames.NameOfIndex(idx), TestEmpty(VarValues[idx])])
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
        VarValues[ActiveVariable] := Value;
end;

procedure TDSSParser.SetVars(vars: TParserVar);
begin
    ParserVars := vars;
end;

end.
