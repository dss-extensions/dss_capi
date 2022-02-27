
unit Utilities;

{
  ----------------------------------------------------------
  Copyright (c) 2008-2015, Electric Power Research Institute, Inc.
  All rights reserved.
  ----------------------------------------------------------
}

interface

uses
    ArrayDef,
    CktElement,
    PDElement,
    UComplex, DSSUcomplex,
    UcMatrix,
    DSSClass,
    Classes,
    Dynamics;

type
{$SCOPEDENUMS ON}
    TLSFileType = (
        PlainText = 0,
        Float64 = 1,
        Float32 = 2
    );
{$SCOPEDENUMS OFF}

const
    sCRLF: String = sLineBreak;
    clAqua: Integer = $FFFF00;
    clBlack: Integer = $000000;
    clBlue: Integer = $FF0000;
    clDkGray: Integer = $808080;
    clFuchsia: Integer = $FF00FF;
    clGray: Integer = $808080;
    clGreen: Integer = $008000;
    clLime: Integer = $00FF00;
    clLtGray: Integer = $C0C0C0;
    clMaroon: Integer = $000080;
    clNavy: Integer = $800000;
    clOlive: Integer = $008080;
    clPurple: Integer = $800080;
    clRed: Integer = $0000FF;
    clSilver: Integer = $C0C0C0;
    clTeal: Integer = $808000;
    clWhite: Integer = $FFFFFF;
    clYellow: Integer = $00FFFF;

function StrTOrF(const b: Boolean): String; inline;
function StrYOrN(const b: Boolean): String; inline;
function CompareTextShortest(const S1, S2: String): Integer;
procedure FireOffEditor(DSS: TDSSContext; FileNm: String);
procedure DoDOSCmd(DSS: TDSSContext; CmdString: String);
function StripExtension(const S: String): String;
function StripClassName(const S: String): String;  // Return only element name sans class.
function GetNodeString(const Busname: String): String;
function Pad(const S: String; Width: Integer): String;
function PadDots(const S: String; Width: Integer): String;
function PadTrunc(const S: String; Width: Integer): String;
function IntArrayToString(iarray: pIntegerArray; count: Integer): String;
function DblArrayToString(dblarray: pDoubleArray; count: Integer): String;
function CmplxArrayToString(cpxarray: pComplexArray; count: Integer): String;
function StringListToString(lst: TStringList): String;
function EncloseQuotes(const s: String): String;
procedure ShowMessageBeep(DSS: TDSSContext; const s: String);

// Parsing Utilities
procedure ParseObjectClassandName(DSS: TDSSContext; const FullObjName: String; var ClassName, ObjName: String);
procedure ParseIntArray(DSS: TDSSContext; var iarray: pIntegerArray; var count: Integer; const s: String);
function InterpretYesNo(const s: String): Boolean;
procedure InitDblArray(NumValues: Integer; Xarray: pDoubleArray; Value: Double);
function InterpretDblArray(DSS: TDSSContext; const s: String; MaxValues: Integer; ResultArray: pDoubleArray): Integer;
function InterpretIntArray(DSS: TDSSContext; const s: String; MaxValues: Integer; ResultArray: pIntegerArray): Integer;
procedure InterpretTStringListArray(DSS: TDSSContext; const s: String; var ResultList: TStringList);
function InterpretTimeStepSize(DSS: TDSSContext; const s: String): Double;
function InterpretColorName(DSS: TDSSContext; const s: String): Integer;
function ConstructElemName(DSS: TDSSContext; const Param: String): String;

function GetDSSArray_Real(n: Integer; dbls: pDoubleArray; scale: Double = 1.0): String;
function GetDSSArray_Single(n: Integer; sngs: pSingleArray): String;
function GetDSSArray_Integer(n: Integer; ints: pIntegerArray): String;
function GetOCPDeviceType(pElem: TDSSCktElement): Integer;
function GetOCPDeviceTypeString(icode: Integer): String;
// Addition to deal with Memory mapped data
function InterpretDblArrayMMF(DSS: TDSSContext; mmPtr: pByte; FileType: TLSFileType; Column, INDEX, DataSize: Integer): double;
 

// misc functions
function DoExecutiveCommand(DSS: TDSSContext; const s: String): Integer;
function GetCktElementIndex(DSS: TDSSContext; const FullObjName: String): Integer;
function IsShuntElement(const Elem: TDSSCktElement): Boolean;
function IsLineElement(const Elem: TDSSCktElement): Boolean;
function IsTransformerElement(const Elem: TDSSCktElement): Boolean;
function CheckParallel(const Line1, Line2: TDSSCktElement): Boolean;
function AllTerminalsClosed(ThisElement: TDSSCktElement): Boolean;
function Str_Real(const Value: Double; NumDecimals: Integer): String;
procedure DumpAllDSSCommands(DSS: TDSSContext; var Filename: String);
procedure DumpAllocationFactors(DSS: TDSSContext; var Filename: String);
procedure DumpComplexMatrix(DSS: TDSSContext; F: TFileStream; AMatrix: TcMatrix);
function NearestBasekV(DSS: TDSSContext; kV: Double): Double;
function DoResetFaults(DSS: TDSSContext): Integer;
function DoResetControls(DSS: TDSSContext): Integer;
procedure DoResetKeepList(DSS: TDSSContext);
function GetNodeNum(DSS: TDSSContext; NodeRef: Integer): Integer;
function MaxdblArrayValue(npts: Integer; dbls: pDoubleArray): Double;
function iMaxAbsdblArrayValue(npts: Integer; dbls: pDoubleArray): Integer;
function iMaxAbssngArrayValue(npts: Integer; sngs: pSingleArray): Integer;
function QuadSolver(const a, b, c: Double): Double; // returns largest of two answers


// Save Function Helper
function WriteClassFile(DSS: TDSSContext; const DSS_Class: TDSSClass; FileName: String; IsCktElement: Boolean): Boolean;
function WriteVsourceClassFile(DSS: TDSSContext; const DSS_Class: TDSSClass; IsCktElement: Boolean): Boolean;
procedure WriteActiveDSSObject(DSS: TDSSContext; F: TFileStream; const NeworEdit: String);
function checkforblanks(const S: String): String;
function RewriteAlignedFile(DSS: TDSSContext; const Filename: String): Boolean;

// Event Log
procedure ClearEventLog(DSS: TDSSContext);
procedure AppendToEventLog(DSS: TDSSContext; const opdev: String; const action: String);
procedure LogThisEvent(DSS: TDSSContext; const EventName: String);

procedure ClearErrorLog(DSS: TDSSContext);

// Routines for doing common things to complex numbers
procedure RotatePhasorDeg(var Phasor: Complex; const h, AngleDeg: Double);
procedure RotatePhasorRad(var Phasor: Complex; const h, AngleRad: Double);
procedure ConvertComplexArrayToPolar(const Buffer: pComplexArray; N: Integer);
procedure ConvertComplexArrayToPowerandPF(const Buffer: pComplexArray; N: Integer);
function Residual(p: Pointer; Nph: Integer): Complex;
function ResidualPolar(p: Pointer; Nph: Integer): Complex;
function Powerfactor(const S: Complex): Double;
procedure CmulArray(pc: pcomplexarray; Multiplier: Double; size: Integer);  // Multiply a complex array times a double

// Support for going in and out of Dynamics Mode and Harmonics Mode
procedure CalcInitialMachineStates(DSS: TDSSContext);
procedure InvalidateAllPCELEMENTS(DSS: TDSSContext);
function InitializeForHarmonics(DSS: TDSSContext): Boolean;
function SavePresentVoltages(DSS: TDSSContext): Boolean;
function RetrieveSavedVoltages(DSS: TDSSContext): Boolean;

function GetMaxPUVoltage(DSS: TDSSContext): Double;
function GetMinPUVoltage(DSS: TDSSContext; IgnoreNeutrals: Boolean): Double;
function GetTotalPowerFromSources(DSS: TDSSContext): Complex;
function GetMaxCktElementSize(DSS: TDSSContext): Integer;
function GetUniqueNodeNumber(DSS: TDSSContext; const sBusName: String; StartNode: Integer): Integer;

// TraceBack Functions
function IsPathBetween(FromLine, ToLine: TPDElement): Boolean;
procedure TraceAndEdit(DSS: TDSSContext; FromLine, ToLine: TPDElement; NPhases: Integer; EditStr: String);
procedure GoForwardAndRephase(DSS: TDSSContext; FromLine: TPDElement; const PhaseString, EditStr, ScriptFileName: String; TransStop: Boolean);

procedure MakeDistributedGenerators(DSS: TDSSContext; kW, PF: Double; How: String; Skip: Integer; Fname: String; DoGenerators: Boolean);

procedure Obfuscate(DSS: TDSSContext);

function AdjustInputFilePath(DSS: TDSSContext; param: String): String;
procedure FSWriteLn(F: TFileStream; Ss: Array of String); inline; overload;
procedure FSWriteLn(F: TFileStream; S: String = ''); inline; overload;
procedure FSWriteLn(F: TFileStream; S: String; S2: String); inline; overload;
procedure FSWriteLn(F: TFileStream; S: String; S2: String; S3: String); inline; overload;
procedure FSWrite(F: TFileStream; S: String); inline; overload;
procedure FSWrite(F: TFileStream; S: String; S2: String); inline; overload;
procedure FSWrite(F: TFileStream; S: String; S2: String; S3: String); inline; overload;

procedure FSReadln(F: TStream; out S: String);
procedure FSFlush(F: TFileStream);

function SliceProps(props: pStringArray; count: Integer): ArrayOfString; // The built-in Slice was causing issues on ARM64

procedure DoSngFile(DSS: TDSSContext; var pA, pB: PDoubleArray; var NumPoints: Integer; OnlyLoadB: Boolean; const FileName: String; const ClassName: String; RoundA: Boolean = False);
procedure DoDblFile(DSS: TDSSContext; var pA, pB: PDoubleArray; var NumPoints: Integer; OnlyLoadB: Boolean; const FileName: String; const ClassName: String; RoundA: Boolean = False);
procedure DoCSVFile(DSS: TDSSContext; var pA, pB: PDoubleArray; var NumPoints: Integer; OnlyLoadB: Boolean; const FileName: String; const ClassName: String; RoundA: Boolean = False);

procedure DelFilesFromDir(Directory: String; FileMask: String = '*'; DelSubDirs: Boolean = True);

implementation

uses
{$IFDEF WINDOWS}
    Windows,
    ShellApi,
{$ELSE}
    BaseUnix,
    Unix,
{$ENDIF}
    Process,
    CmdForms,
    SysUtils,
    DSSClassDefs,
    DSSGlobals,
    Executive,
    ExecCommands,
    ExecOptions,
    Solution,
    DSSObject,
    math,
    ParserDel,
    Capacitor,
    Reactor,
    Generator,
    Load,
    Line,
    Fault,
    HashList,
    LoadShape,
    EnergyMeter,
    PCElement,
    ControlElem,
    DSSHelper,
    StrUtils;

const
    padString: String = '                                                  '; //50 blanks
    paddotsString: String = ' .................................................'; //50 dots

function StrTOrF(const b: Boolean): String; inline;
begin
    if b then
        Result := 'true'
    else
        Result := 'false'
end;

function StrYOrN(const b: Boolean): String; inline;
begin
    if b then
        Result := 'Yes'
    else
        Result := 'No'
end;

function CompareTextShortest(const S1, S2: String): Integer;
var
    Teststr: String;
begin
    if Length(S1) < Length(S2) then
    begin
        TestStr := Copy(S2, 1, Length(S1));
        Result := CompareText(TestStr, S1);
    end
    else
    begin
        TestStr := Copy(S1, 1, Length(S2));
        Result := CompareText(TestStr, S2);
    end;
end;

function Pad(const S: String; Width: Integer): String;
// Pad out a string with blanks to Width characters
begin
    Result := Copy(S, 1, Length(S)) + Copy(padString, 1, (Width - Length(S)));
  // For i := 1 to Width-Length(S) DO Result := Result + ' ';
end;

function PadDots(const S: String; Width: Integer): String;
// Pad out a string with dots to Width characters
begin
    Result := Copy(S, 1, Length(S)) + Copy(paddotsString, 1, (Width - Length(S)));
end;

function PadTrunc(const S: String; Width: Integer): String;
// Pad out a string with blanks to Width characters or truncate to Width Chars
begin
    Result := Copy(Pad(S, Width), 1, Width);
end;

function StripExtension(const S: String): String;
// Strips off everything up to a period.
var
    dotpos: Integer;

begin
    dotpos := pos('.', S) - 1;
    if dotpos = (-1) then
        dotpos := Length(S);
    Result := Copy(S, 1, dotpos);
end;

function StripClassName(const S: String): String;
// Returns everything past the first period
var
    dotpos: Integer;

begin
    dotpos := pos('.', S);
    Result := Copy(S, dotpos + 1, Length(S));
end;

{$IFDEF WINDOWS}
procedure FireOffEditor(DSS: TDSSContext; FileNm: String);
var
    retval: Word;
begin
    if not DSS_CAPI_ALLOW_EDITOR then
        Exit; // just ignore if Show is not allowed

    if (@DSSPrime.DSSMessageCallback) <> NIL then
        if 0 = DSSPrime.DSSMessageCallback(DSSPrime, PChar(FileNm), ord(DSSMessageType.FireOffEditor)) then
            Exit;

    try
        if FileExists(FileNm) then
        begin
            retval := ShellExecute(0, NIL, Pchar(encloseQuotes(DefaultEditor)), Pchar(encloseQuotes(FileNm)), NIL, SW_SHOW);
            SetLastResultFile(DSS, FileNm);

            case Retval of
                0:
                    DoSimpleMsg(DSS, _('System out of memory. Cannot start Editor.'), 700);
                ERROR_BAD_FORMAT:
                    DoSimpleMsg(DSS, _('Editor File is Invalid.'), 701);
                ERROR_FILE_NOT_FOUND:
                    DoSimpleMsg(DSS, 'Editor "%s" not found. Did you set a complete path name?', [DefaultEditor], 702);
                ERROR_PATH_NOT_FOUND:
                    DoSimpleMsg(DSS, 'Path for Editor "%s" not found.', [DefaultEditor], 703);
            end;
        end;
    except
        On E: Exception do
            DoErrorMsg(DSS, 'FireOffEditor.', E.Message,
                _('Default Editor correctly specified???'), 704);
    end;
end;


procedure DoDOSCmd(DSS: TDSSContext; CmdString: String);
var
    Handle: Word;
begin
    try
        Handle := 0;
        ShellExecute(Handle, 'open', Pchar('cmd.exe'), Pchar(CmdString), NIL, SW_SHOW);

    except
        On E: Exception do
            DoSimpleMsg(DSS, 'DoDOSCmd Error:%s. Error in Command "%s"', [E.Message, CmdString], 704);
    end;
end;
{$ELSE}
procedure FireOffEditor(DSS: TDSSContext; FileNm: String);
var
    s: String;
    gotError: Boolean;
    msg: String;
begin
    if not DSS_CAPI_ALLOW_EDITOR then
        Exit; // just ignore if Show is not allowed

    if (@DSSPrime.DSSMessageCallback) <> NIL then
        if 0 = DSSPrime.DSSMessageCallback(DSSPrime, PChar(FileNm), ord(DSSMessageType.FireOffEditor)) then
            Exit;

    gotError := FALSE;
    msg := 'Unknown error in process.';
    try
        if FileExists(FileNm) then
        begin
            gotError := not RunCommand('/bin/bash', ['-c', DefaultEditor + ' ' + FileNm], s);
        end;
    except
        On E: Exception do
        begin
            gotError := TRUE;
            msg := E.Message;
        end;
    end;
    if gotError then
        DoErrorMsg(DSS, 'FireOffEditor.', msg, _('Editor could not be started. Is the editor correctly specified?'), 704);
end;

procedure DoDOSCmd(DSS: TDSSContext; CmdString: String);
var //Handle:Word;
    s: String;
    gotError: Boolean;
    msg: String;
begin
    gotError := FALSE;
    msg := _('Unknown error in command.');
    try
        gotError := not RunCommand('/bin/bash', ['-c', CmdString], s);
    except
        On E: Exception do
        begin
            gotError := TRUE;
            msg := E.Message;
        end;
    end;
    if gotError then
        DoSimpleMsg(DSS, 'DoDOSCmd Error:%s. Error in Command "%s"', [msg, CmdString], 704);
end;
{$ENDIF}

function IntArrayToString(iarray: pIntegerArray; count: Integer): String;
// Put array values in parentheses separated by commas.
var
    i: Integer;
begin
    Result := '[NULL]';
    if count > 0 then
    begin
        Result := '[';
        for i := 1 to count do
        begin
            Result := Result + IntToStr(iarray^[i]);
            if i <> count then
                Result := Result + ', ';
        end;
        Result := Result + ']';
    end;
end;

function DblArrayToString(dblarray: pDoubleArray; count: Integer): String;
// Put array values in brackets separated by commas.
var
    i: Integer;
begin
    Result := '[NULL]';
    if count > 0 then
    begin
        Result := Format('[%.10g', [dblarray^[1]]);
        for i := 2 to count do
            Result := Result + Format(', %.10g', [dblarray^[i]]);
        Result := Result + ']';
    end;
end;

function CmplxArrayToString(cpxarray: pComplexArray; count: Integer): String;
// Put array values in brackets separated by commas.
var
    i: Integer;
begin
    Result := '[NULL]';
    if count > 0 then
    begin
        Result := Format('[%.10g +j %.10g', [cpxarray^[1].re, cpxarray^[1].im]);
        for i := 2 to count do
            Result := Result + Format(', %.10g +j %.10g', [cpxarray^[i].re, cpxarray^[i].im]);
        Result := Result + ']';
    end;
end;

function StringListToString(lst: TStringList): String;
var
    i: Integer;
begin
    if (lst = NIL) or (lst.Count = 0) then
    begin
        Result := '';
        Exit;
    end;
    Result := '[' + lst.Strings[0];
    for i := 1 to lst.Count - 1 do
    begin
        Result := Result + ', ' + lst.Strings[i];
    end;
    Result := Result + ']';
end;

function EncloseQuotes(const s: String): String;
begin
    Result := '"' + s + '"';
end;

function InterpretYesNo(const s: String): Boolean;
//' Interpret Yes / no properties  - can also be True/False
begin
    case LowerCase(S[1]) of
        'y', 't':
            Result := TRUE;
        'n', 'f':
            Result := FALSE;
    else
        Result := FALSE;
    end;
end;

procedure InitDblArray(NumValues: Integer; Xarray: pDoubleArray; Value: Double);
var
    i: Integer;
// Set all elements of a double array
begin
    for i := 1 to NumValues do
        Xarray^[i] := Value;
end;

function InterpretDblArray(DSS: TDSSContext; const s: String; MaxValues: Integer; ResultArray: pDoubleArray): Integer;
//  Get numeric values from an array specified either as a list on numbers or a text file spec.
//  ResultArray must be allocated to MaxValues by calling routine.
//
//  9/7/2011 Modified to allow multi-column CSV files and result file
//
//  CSV File my have one value per line or multiple columns and a header row.
//  Example:
//          ... mult=[file = myfilename, column=2, header=yes]
//          ... mult=[file = %result%, column=2, header=yes]    // last result file
//
//          file= must be first
//          the %result% variable implies the last result file
//
//          or
//
//          Use the Array=@lastfile variable syntax and the parser will automativally replace with last file name
var
    ParmName,
    Param: String;
    MStream: TMemoryStream;
    F: TStream = NIL; // input
    i: Integer;
    // Temp: Single;
    CSVFileName: String;
    CSVColumn: Integer;
    CSVHeader: Boolean;
    InputLIne: String;
    iskip: Integer;
    sngArray: ArrayDef.PSingleArray;

begin
    DSS.AuxParser.CmdString := S;
    ParmName := DSS.AuxParser.NextParam;
    Param := DSS.AuxParser.StrValue;
    Result := MaxValues; // Default Return Value;

    // Syntax can be either a list of numeric values or a file specification:  File= ...

    if CompareText(Parmname, 'file') = 0 then
    begin
        // Default values
        if compareText(param, '%result%') = 0 then
            CSVFileName := DSS.LastResultFile
        else
            CSVFileName := Param;

        try
            F := DSS.GetROFileStream(CSVFileName);
        except
            DoSimpleMsg(DSS, 'CSV file "%s" could not be opened', [CSVFileName], 70401);
            Exit;
        end;

        // Default options
        CSVColumn := 1;
        CSVHeader := FALSE;

        // Look for other options  (may be in either order)
        ParmName := DSS.AuxParser.NextParam;
        Param := DSS.AuxParser.StrValue;
        while Length(Param) > 0 do
        begin
            if CompareTextShortest(ParmName, 'column') = 0 then
                CSVColumn := DSS.AuxParser.IntValue;
            if CompareTextShortest(ParmName, 'header') = 0 then
                CSVHeader := InterpretYesNo(param);
            ParmName := DSS.AuxParser.NextParam;
            Param := DSS.AuxParser.StrValue;
        end;

        // load the list from a file

        try
            if CSVHeader then
                FSReadln(F, InputLIne);  // skip the header row

            for i := 1 to MaxValues do
            begin
                try
                    if (F.Position + 1) < F.Size then
                    begin
                        FSReadln(F, InputLIne);
                        DSS.AuxParser.CmdString := InputLine;
                        for iskip := 1 to CSVColumn do
                            ParmName := DSS.AuxParser.NextParam;
                        ResultArray^[i] := DSS.AuxParser.dblValue;
                    end
                    else
                    begin
                        Result := i - 1;  // This will be different if less found;
                        Break;
                    end;
                except
                    On E: Exception do
                    begin
                        DoSimpleMsg(DSS, 'Error reading %d-th numeric array value from file: "%s" Error is:', [i, Param, E.message], 705);
                        Result := i - 1;
                        Break;
                    end;
                end;

            end;

        finally

            FreeAndNil(F);

        end;
    end
    else if (Length(Parmname) > 0) and (CompareTextShortest(Parmname, 'dblfile') = 0) then
    begin
         // load the list from a file of doubles (no checking done on type of data)
        try
            F := DSS.GetROFileStream(Param);
        except
            DoSimpleMsg(DSS, 'File of doubles "%s" could not be opened.', [Param], 70501);
            Exit;
        end;
        Result := Min(Maxvalues, F.Size div sizeof(ResultArray^[1]));  // no. of doubles
        F.ReadBuffer(ResultArray^[1], SizeOf(ResultArray^[1]) * Result);
        F.Free;
    end
    else if (Length(Parmname) > 0) and (CompareTextShortest(Parmname, 'sngfile') = 0) then
    begin
        // load the list from a file of singles (no checking done on type of data)
        try
            F := DSS.GetROFileStream(Param)
        except
            DoSimpleMsg(DSS, 'File of Singles "%s" could not be opened.', [Param], 70502);
            Exit;
        end;
       
        MStream := TMemoryStream.Create;
        MStream.LoadFromStream(F);
        F.Free;
        sngArray := ArrayDef.PSingleArray(MStream.Memory);
        // Now move the singles from the file into the destination array
        Result := Min(Maxvalues, MStream.Size div sizeof(Single));  // no. of singles
        for i := 1 to Result do
        begin
            ResultArray^[i] := sngArray[i];  // Single to Double
        end;
        MStream.Free;
    end
    else
    begin  // Parse list of values off input string
         // Parse Values of array list
        for i := 1 to MaxValues do
        begin
            ResultArray^[i] := DSS.AuxParser.DblValue;    // Fills array with zeros if we run out of numbers
            DSS.AuxParser.NextParam;
        end;
    end;
end;

function InterpretDblArrayMMF(DSS: TDSSContext; mmPtr: pByte; FileType: TLSFileType; Column, INDEX, DataSize: Integer): double;
//  Gets the value for the value at INDEX within the file mapped (mmPtr)
//  Considers the flags FileType, Column for locating the data within the file
//  FileType :
//    0 - normal file (ANSI char)
//    1 - dblfile
//    2 - sngfile
var
   DBLByteArray: array[0..7] of byte;
   SGLByteArray: array[0..3] of byte;
   // InputLIne, 
   content: String;
   byteValue : Byte;
   OffSet, i, j : Integer;
Begin
    Result := 1.0; // Default Return Value;
    OffSet := (INDEX - 1) * DataSize;

    if FileType = TLSFileType.PlainText then  // Normal file (CSV, txt, ASCII based file)
    begin
        content := '';
        byteValue := 0;
        i := OffSet;
        if mmPtr[i] = $0A then 
            inc(i); // in case we are at the end of the previous line
            
        j := 0;
        while byteValue <> $0A do
        begin
            byteValue := mmPtr[i];
            // Concatenates avoiding special chars (EOL)
            if (byteValue >= 46) and (byteValue < 58) then 
                content := content + AnsiChar(byteValue);

            if byteValue = 44 then // a comma char was found
            begin               // If we are at the column, exit, otherwise, keep looking
                inc(j);         // discarding the previous number (not needed anyway)
                if j = Column then 
                    break
                else 
                    content := '';
            end;
            inc(i);
        end;
        try
            // checks if the extraction was OK, othwerwise, forces the default value
            if content = '' then 
                content := '1.0';
            Result := strtofloat(content);
        except
            on E:Exception Do
            begin
                DoSimpleMsg(DSS, 'Error reading %d-th numeric array value. Error is:', [i, E.message], 785);
                Result := i - 1;
            end;
        end;
        Exit;
    end;

    if (FileType = TLSFileType.Float64) then // DBL files
    begin
        // load the list from a file of doubles (no checking done on type of data)
        for i := 0 to (DataSize - 1) do 
            DBLByteArray[i] :=  mmPtr[i + OffSet]; // Load data into the temporary buffer
        Result := double(DBLByteArray); // returns the number (double)
        Exit;
    end;

    if (FileType = TLSFileType.Float32) then // SGL files
    begin
        // load the list from a file of doubles (no checking done on type of data)
        for i := 0 to (DataSize - 1) do 
            SGLByteArray[i] := mmPtr[i + OffSet]; // Load data into the temporary buffer
        Result := single(SGLByteArray); // returns the number formatted as double
        Exit;
    end;
End;

function InterpretIntArray(DSS: TDSSContext; const s: String; MaxValues: Integer; ResultArray: pIntegerArray): Integer;
//  Get numeric values from an array specified either as a list on numbers or a text file spec.
//  ResultArray must be allocated to MaxValues by calling routine.
//  File is assumed to have one value per line.
var
    ParmName,
    Param: String;
    F: TStream = nil;
    i: Integer;
    line: String;
begin
    DSS.AuxParser.CmdString := S;
    ParmName := DSS.AuxParser.NextParam;
    Param := DSS.AuxParser.StrValue;
    Result := Maxvalues;  // Default return value

    // Syntax can be either a list of numeric values or a file specification:  File= ...

    if CompareText(Parmname, 'file') = 0 then
    begin
         // load the list from a file
        try
            F := DSS.GetROFileStream(Param);
            for i := 1 to MaxValues do
            begin
                if (F.Position + 1) < F.Size then
                begin
                    FSReadln(F, line);
                    ResultArray^[i] := StrToInt(line);
                end
                else
                begin
                    Result := i - 1;
                    Break;
                end;
            end;
            FreeAndNil(F);

        except
            On E: Exception do
                DoSimpleMsg(DSS, 'Error trying to read numeric array values from file "%s". Error is: %s', [Param, E.Message], 706);
        end;
    end
    else
    begin  // Parse list of values off input string

         // Parse Values of array list
        for i := 1 to MaxValues do
        begin
            ResultArray^[i] := DSS.AuxParser.IntValue;    // Fills array with zeros if we run out of numbers
            DSS.AuxParser.NextParam;
        end;
    end;
end;

function InterpretTimeStepSize(DSS: TDSSContext; const s: String): Double;
// Return stepsize in seconds
var
    Code: Integer;
    ch: Char;
    s2: String;

begin
    // Try to convert and see if we get an error
    val(s, Result, Code);
    if Code = 0 then
        Exit;  // Only a number was specified, so must be seconds

    // Error occurred so must have a units specifier
    ch := s[Length(s)];  // get last character
    s2 := copy(s, 1, Length(s) - 1);
    Val(S2, Result, Code);
    if Code > 0 then
    begin   // check for error
        Result := DSS.ActiveCircuit.solution.DynaVars.h; // Don't change it
        DoSimpleMsg(DSS, 'Error in specification of StepSize: %s', [s], 99933);
        Exit;
    end;
    case ch of
        'h':
            Result := Result * 3600.0;
        'm':
            Result := Result * 60.0;
        's': ; // Do nothing
    else
        Result := DSS.ActiveCircuit.solution.DynaVars.h; // Don't change it
        DoSimpleMsg(DSS, 'Error in specification of StepSize: "%s". Units can only be h, m, or s (single char only)', [s], 99934);
    end;
end;

procedure InterpretTStringListArray(DSS: TDSSContext; const s: String; var ResultList: TStringList);
// Get string values from an array specified either as a list on strings or a text file spec.
// ResultArray is allocated as needed.
// File is assumed to have one value per line.
var
    ParmName,
    Param,
    NextParam: String;
    F: TStream = nil;
begin
    //  Throw Away any Previous Allocation
    if ResultList = NIL then
        ResultList := TStringList.Create()
    else
        ResultList.Clear;

    DSS.AuxParser.CmdString := S;
    ParmName := DSS.AuxParser.NextParam;
    Param := DSS.AuxParser.StrValue;

    // Syntax can be either a list of string values or a file specification:  File= ...
    if CompareText(Parmname, 'file') = 0 then
    begin
         // load the list from a file
        try
            F := DSS.GetROFileStream(Param);
            while (F.Position + 1) < F.Size do
            begin
                FSReadln(F, Param);
                DSS.AuxParser.CmdString := Param;
                ParmName := DSS.AuxParser.NextParam;
                NextParam := DSS.AuxParser.StrValue;
                if Length(NextParam) > 0 then
                begin     // Ignore Blank Lines in File
                    ResultList.Add(NextParam);
                end;
            end;
            FreeAndNil(F);

        except
            On E: Exception do
                DoSimpleMsg(DSS, 'Error trying to read lines from a file. Error is: %s', [E.message], 708);
        end;
    end
    else
    begin  // Parse list of values off input string
         // Parse Values of array list
        while Param <> '' do
        begin
            ResultList.add(Param);
            ParmName := DSS.AuxParser.NextParam;
            Param := DSS.AuxParser.StrValue;
        end;
    end;
end;

procedure ParseObjectClassandName(DSS: TDSSContext; const FullObjName: String; var ClassName, ObjName: String);
var
    dotpos: Integer;
begin
    // Split off Obj class and name
    dotpos := Pos('.', FullObjName);
    case dotpos of
        0:
        begin
            ObjName := Copy(FullObjName, 1, Length(FullObjName));  // assume it is all objname; class defaults
            ClassName := '';
        end;
    else
    begin
        ClassName := Copy(FullObjName, 1, dotpos - 1);
        ObjName := Copy(FullObjName, dotpos + 1, Length(FullObjName));
    end;
    end;

    // Check object name in case it is a variable
    DSS.Parser.CheckforVar(ObjName);
end;

procedure ParseIntArray(DSS: TDSSContext; var iarray: pIntegerArray; var count: Integer; const s: String);
var
    param: String;
    i: Integer;
begin
    // Parse the line once to get the count of tokens on string, S
    DSS.AuxParser.cmdString := S;
    Count := 0;
    repeat
        DSS.AuxParser.NextParam;
        Param := DSS.AuxParser.StrValue;
        if Length(Param) > 0 then
            Inc(Count);
    until Length(Param) = 0;

    // reallocate iarray  to new size
    ReallocMem(iarray, sizeof(iarray^[1]) * count);

    // Parse again for real
    DSS.AuxParser.cmdString := S;
    for i := 1 to Count do
    begin
        DSS.AuxParser.NextParam;
        iarray^[i] := DSS.AuxParser.IntValue;
    end;
end;

function IsShuntElement(const Elem: TDSSCktElement): Boolean;
begin
    case (Elem.DSSObjType and CLASSMASK) of
        CAP_ELEMENT:
            Result := TCapacitorObj(Elem).IsShunt;
        REACTOR_ELEMENT:
            Result := TReactorObj(Elem).IsShunt;
    else
        Result := FALSE;
    end;
end;

function IsLineElement(const Elem: TDSSCktElement): Boolean;
begin
    Result := ((Elem.DSSObjType and CLASSMASK) = LINE_ELEMENT)
end;

function IsTransformerElement(const Elem: TDSSCktElement): Boolean;
begin
    Result := ((Elem.DSSObjType and CLASSMASK) = XFMR_ELEMENT)
end;

function GetCktElementIndex(DSS: TDSSContext; const FullObjName: String): Integer; //TODO: merge with TDSSCircuit.SetElementActive
// Given the full object name, return the index to the circuit element in the
// active circuit.  Use full name if given. Else assume last class referenced.
var
    DevIndex: Integer;
    DevClassIndex: Integer;
    DevType,
    DevName: String;
    DevCls: TDSSClass;
    element: TDSSCktElement;
begin
    Result := 0;
    ParseObjectClassandName(DSS, FullObjName, DevType, DevName);
    DevClassIndex := DSS.ClassNames.Find(DevType);
    if DevClassIndex = 0 then 
        DevClassIndex := DSS.LastClassReferenced;
    DevCls := DSS.DSSClassList.At(DevClassIndex);

    if DevName = '' then
        Exit;
    
    if not DSS.ActiveCircuit.DuplicatesAllowed then
    begin
        element := TDSSCktElement(DevCls.Find(DevName, False));
        if element <> NIL then
        begin
            Result := element.Handle;
            Exit;
        end;
    end
    else
    begin
        Devindex := DSS.ActiveCircuit.DeviceList.Find(DevName);
        while DevIndex > 0 do 
        begin
            if TDSSCktElement(DSS.ActiveCircuit.CktElements.At(Devindex)).ParentClass = DevCls then   // we got a match
            begin
                Result := DevIndex;
                Exit;
            end;
            Devindex := DSS.ActiveCircuit.Devicelist.FindNext;   // Could be duplicates
        end;
    end;
end;

function Str_Real(const Value: Double; NumDecimals: Integer): String;
begin
    try
        Result := FloatToStrF(Value, ffFixed, 0, NumDecimals);
    except
        Result := '*****';
    end;
end;

function ReplaceCRLF(const S: String): String;
begin
    // Replace CRLF with a \n character sequence
    Result := StringReplace(S, CRLF, '\n', [rfReplaceAll]);
end;

procedure DumpAllocationFactors(DSS: TDSSContext; var FileName: String);
var
    F: TFileStream = nil;
    pLoad: TLoadObj;
begin
    try
        F := TFileStream.Create(FileName, fmCreate);
    except
        On E: Exception do
        begin
            DoErrorMsg(DSS, 
                Format(_('Error opening "%s" for writing.'), [FileName]),
                E.Message, 
                _('File protected or other file error.'), 709);
            FreeAndNil(F);
            Exit;
        end;
    end;

    with DSS.ActiveCircuit do
    begin
        pLoad := Loads.First;
        while pLoad <> NIL do
        begin
            case pLoad.LoadSpecType of
                TLoadSpec.ConnectedkVA_PF:
                    FSWriteln(F, 'Load.' + pLoad.Name + '.AllocationFactor=' + Format('%-.5g', [pLoad.kVAAllocationFactor]));
                TLoadSpec.kwh_PF:
                    FSWriteln(F, 'Load.' + pLoad.Name + '.CFactor=' + Format('%-.5g', [pLoad.CFactor]));
            end;
            pLoad := Loads.Next;
        end;
    end;

    FreeAndNil(F);

    DSS.GlobalResult := FileName;
end;

procedure DumpAllDSSCommands(DSS: TDSSContext; var FileName: String);
var
    F: TFileStream = nil;
    pClass: TDSSClass;
    i: Integer;
    sout: String;
begin
    try
        FileName := DSS.OutputDirectory + 'DSSCommandsDump.txt';
        F := TFileStream.Create(FileName, fmCreate);
    except
        On E: Exception do
        begin
            DoErrorMsg(DSS, 
                Format(_('Error opening "%s" for writing.'), [FileName]), 
                E.Message, 
                _('Disk protected or other file error'), 710);
            FreeAndNil(F);
            Exit;
        end;
    end;

    // dump Executive commands
    FSWriteln(F, '[execcommands]');
    for i := 1 to NumExecCommands do
    begin
        WriteStr(sout, i: 0, ', "', Execcommand[i], '", "', ReplaceCRLF(DSSHelp('Command.' + ExecCommand[i])), '"');
        FSWriteln(F, sout);
    end;

    // Dump Executive Options
    FSWriteln(F, '[execoptions]');
    for i := 1 to NumExecOptions do
    begin
        WriteStr(sout, i: 0, ', "', ExecOption[i], '", "', ReplaceCRLF(DSSHelp('Executive.' + ExecOption[i])), '"');
        FSWriteln(F, sout);
    end;

   // Dump All present DSSClasses
   pClass := DSS.DSSClassList.First;
   while pClass <> NIL do
   begin
       FSWriteln(F, '[' + pClass.name + ']');
       for i := 1 to pClass.NumProperties do
       begin
           WriteStr(sout, i: 0, ', "', pClass.PropertyName^[i], '", "', ReplaceCRLF(pClass.GetPropertyHelp(i)), '"');
           FSWriteln(F, sout);
       end;
       pClass := DSS.DSSClassList.Next;
   end;
    FreeAndNil(F);
end;

function NearestBasekV(DSS: TDSSContext; kV: Double): Double;
// Find closest base voltage
var
    TestkV: Double;
    Count: Integer;
    Diff,
    MinDiff: Double;
begin
    Count := 1;
    TestkV := DSS.ActiveCircuit.LegalVoltageBases^[1];
    Result := TestkV;
    MinDiff := 1.0E50;  // Big whompin number

    while TestkV <> 0.0 do
    begin
        Diff := Abs(1.0 - kV / TestkV);     // Get Per unit difference
        if Diff < MinDiff then
        begin
            MinDiff := Diff;
            Result := TestkV;
        end;

        Inc(Count);
        TestkV := DSS.ActiveCircuit.LegalVoltageBases^[Count];
    end;
end;

function SavePresentVoltages(DSS: TDSSContext): Boolean;
var
    F: TFileStream = nil;
    i: Integer;
    dNumNodes: Double;
begin
    Result := TRUE;
    try
        F := TFileStream.Create(DSS.OutputDirectory + DSS.CircuitName_ + 'SavedVoltages.dbl', fmCreate);
        
    except
        On E: Exception do
        begin
            DoSimpleMsg(DSS, 'Error opening/creating file to save voltages: %s', [E.message], 711);
            Result := FALSE;
            Exit;
        end;
    end;

    try
        with DSS.ActiveCircuit, Solution do
        begin
            dNumNodes := NumNodes;
            F.WriteBuffer(dNumNodes, sizeOf(Double));
            for i := 1 to NumNodes do
            begin
                F.WriteBuffer(NodeV^[i].re, sizeOf(Double));
                F.WriteBuffer(NodeV^[i].im, sizeOf(Double));
            end;
        end;

        FreeAndNil(F);

    except
        On E: Exception do
        begin
            DoSimpleMsg(DSS, 'Error writing file to save voltages: %s', [E.message], 712);
            Result := FALSE;
        end;
    end;
end;

function RetrieveSavedVoltages(DSS: TDSSContext): Boolean;
var
    F: TFileStream = nil;
    i: Integer;
    dNumNodes: Double;
begin
    Result := TRUE;
    try
        F := TFileStream.Create(DSS.OutputDirectory + DSS.CircuitName_ + 'SavedVoltages.dbl', fmOpenRead or fmShareDenyWrite);

    except
        On E: Exception do
        begin
            DoSimpleMsg(DSS, 'Error opening file to retrieve saved voltages: %s', [E.message], 713);
            Result := FALSE;
            Exit;
        end;
    end;

    try
        with DSS.ActiveCircuit, Solution do
        begin
            F.ReadBuffer(dNumNodes, sizeof(Double));
            if NumNodes = Round(dNumNodes) then
                for i := 1 to NumNodes do
                begin
                    F.ReadBuffer(NodeV^[i].re, sizeof(Double));
                    F.ReadBuffer(NodeV^[i].im, sizeof(Double));
                end
            else
            begin
                DoSimpleMsg(DSS, _('Saved results do not match present circuit. Aborting.'), 714);
                Result := FALSE;
            end;
        end;

        FreeAndNil(F);

    except
        On E: Exception do
        begin
            DoSimpleMsg(DSS, 'Error reading file to retrieve saved voltages: %s', [E.message], 715);
            Result := FALSE;
        end;
    end;
end;

function InitializeForHarmonics(DSS: TDSSContext): Boolean;
// Initialize PCELEMENT base values for harmonics analysis
var
    pcElem: TPCElement;
begin
    if SavePresentVoltages(DSS)   // Zap voltage vector to disk
    then
        with DSS.ActiveCircuit do
        begin
            // Go through all PC Elements
            pcElem := PCElements.First;
            while pcElem <> NIL do
            begin
                if pcElem.Enabled then
                    pcElem.InitHarmonics;   // Virtual function
                pcElem := PCElements.Next;
            end;
            Result := TRUE;
        end
    else
        Result := FALSE;
end;

procedure CalcInitialMachineStates(DSS: TDSSContext);
var
    pcelem: TPCElement;
begin
    // Do All PC Elements
    // If state variables not defined for a PC class, does nothing
    with DSS.ActiveCircuit do
    begin
        pcelem := PCElements.First;

        while pcelem <> NIL do
        begin
            if pcelem.Enabled then
                pcelem.InitStateVars;
            pcelem := PCElements.Next;
        end;
    end;
end;

procedure InvalidateAllPCELEMENTS(DSS: TDSSContext);
var
    pcelem: TPCElement;
begin
    // Invalidate All PC Elements; Any could be a machine
    with DSS.ActiveCircuit do
    begin
        pcelem := PCElements.First;

        while pcelem <> NIL do
        begin
            if pcelem.Enabled then
                pcelem.YPrimInvalid := TRUE;
            pcelem := PCElements.Next;
        end;
    end;
end;

function DoResetFaults(DSS: TDSSContext): Integer;
var
    pFault: TFaultOBj;

begin
    Result := 0;
    with DSS.ActiveCircuit do
    begin
        pFault := TFaultObj(Faults.First);
        while pFault <> NIL do
        begin
            pFault.Reset;
            pFault := TFaultObj(Faults.Next);
        end;
    end;
end;

function DoResetControls(DSS: TDSSContext): Integer;
var
    ControlDevice: TControlElem;
begin
    Result := 0;
    with DSS.ActiveCircuit do
    begin
        ControlDevice := DSSControls.First;
        while ControlDevice <> NIL do
        begin
            if ControlDevice.Enabled then
                ControlDevice.Reset;
            ControlDevice := DSSControls.Next;
        end;
    end;
end;

function GetNodeNum(DSS: TDSSContext; NodeRef: Integer): Integer;
begin
    if NodeRef = 0 then
        Result := 0
    else
        Result := DSS.ActiveCircuit.MapNodeToBus^[NodeRef].NodeNum
end;

procedure RotatePhasorDeg(var Phasor: Complex; const h, AngleDeg: Double);
// rotate a phasor by an angle and harmonic
begin
    Phasor := Phasor * pdegtocomplex(1.0, h * AngleDeg);
end;

procedure RotatePhasorRad(var Phasor: Complex; const h, AngleRad: Double);
// rotate a phasor by an angle and harmonic
begin
    Phasor := Phasor * pclx(1.0, h * AngleRad);
end;

procedure ConvertComplexArrayToPowerandPF(const Buffer: pComplexArray; N: Integer);
// Creates continous PF function from 1 to 2 where 1-2 range is leading (opposite sign)
var
    Mag, PF: Double;
    i: Integer;

    function PFSign(const S: Complex): Double;
    begin
        if S.re * S.im < 0.0 then
            Result := -1.0
        else
            Result := 1.0;
    end;

begin
    // Assume we get P + jQ
    for i := 1 to N do
    begin
        Mag := Cabs(Buffer^[i]);
        if Mag > 0.0 then
        begin
            PF := PFSign(Buffer^[i]) * Abs(Buffer^[i].Re) / Mag;
            if PF < 0.0 then
                PF := 2.0 - abs(PF);
        end
        else
            PF := 1.0;  // for zero power
        Buffer^[i].im := PF;
    end;
end;

procedure ConvertComplexArrayToPolar(const Buffer: pComplexArray; N: Integer);
var
    X: Polar;
    i: Integer;
begin
    for i := 1 to N do
    begin
        x := CtoPolarDeg(Buffer^[i]);
        with Buffer^[i], x do
        begin
            re := Mag;
            im := Ang;
        end;
    end;
end;

function Residual(p: Pointer; Nph: Integer): Complex;
// Assume p points to complex array
// compute residual of the number of phases specified and convert to polar
var
    pc: pComplexArray;
    i: Integer;

begin
    pc := p;
    Result := CZERO;
    for i := 1 to Nph do
        Result += pc^[i];
end;

function ResidualPolar(p: Pointer; Nph: Integer): Complex;
// Assume p points to complex array
// compute residual of the number of phases specified and convert to polar
var
    x: Complex;
begin
    x := Residual(p, Nph);
    Result.re := Cabs(x);
    Result.im := Cdang(x);
end;

function Powerfactor(const S: Complex): Double;

    function Sign(x: Double): Double;
    begin
        if x < 0.0 then
            result := -1.0
        else
            result := 1.0;
    end;

begin
    if (S.re <> 0.0) and (S.im <> 0.0) then
        Result := Sign(S.re * S.im) * Abs(S.re) / Cabs(S)
    else
        Result := 1.0;
end;

procedure ClearEventLog;
begin
    try
        DSS.EventStrings.Clear;
    except
        On E: Exception do
            DoSimpleMsg(DSS, 'Exception clearing event log: %s, @EventStrings=%p', [E.Message, @DSS.EventStrings], 7151);
    end;
end;

procedure ClearErrorLog(DSS: TDSSContext);
begin
    try
        DSS.ErrorStrings.Clear;
    except
        On E: Exception do
            DoSimpleMsg(DSS, 'Exception clearing error log: %s, @EventStrings=%p', [E.Message, @DSS.EventStrings], 71511);
    end;
end;

procedure LogThisEvent(DSS: TDSSContext; const EventName: String);
begin
    with DSS.ActiveCircuit.Solution do
        DSS.EventStrings.Add(Format('Hour=%d, Sec=%-.8g, Iteration=%d, ControlIter=%d, Event=%s',
            [DynaVars.intHour, Dynavars.t, iteration, ControlIteration, EventName]));
end;

procedure AppendToEventLog(DSS: TDSSContext; const opdev: String; const action: String);
var
    S: String;
begin
    with  DSS.ActiveCircuit.Solution do
        S := Format('Hour=%d, Sec=%-.5g, ControlIter=%d, Element=%s, Action=%s',
            [DynaVars.intHour, Dynavars.t, ControlIteration, OpDev, Uppercase(action)]);
    DSS.EventStrings.Add(S);
end;


procedure DumpComplexMatrix(DSS: TDSSContext; F: TFileStream; AMatrix: TcMatrix);
var
    i, j: Integer;
begin
    try
        if AMatrix <> NIL then
        begin
            FSWriteln(F, '!(Real part)');
            with AMatrix do
            begin
                for i := 1 to Order do
                begin
                    FSWrite(F, '! ');
                    for j := 1 to i do
                        FSWrite(F, Format('%g ', [GetElement(i, j).re]));
                    FSWriteln(F);
                end;
                FSWriteln(F, '!(Imaginary part) = ');
                for i := 1 to Order do
                begin
                    FSWrite(F, '! ');
                    for j := 1 to i do
                        FSWrite(F, Format('%g ', [GetElement(i, j).im]));
                    ;
                    FSWriteln(F);
                end;
            end;
        end;
    except
        On E: Exception do
        begin
            DoSimpleMsg(DSS, 'Write aborted. Error in Dump Complex Matrix: %s', [E.message], 716);
        end;

    end;
end;

function AllTerminalsClosed(ThisElement: TDSSCktElement): Boolean;
// check all conductors of this element to see IF it is closed.
// Make sure at least one phase on each terminal is closed.
var
    i, j: Integer;
begin
    Result := FALSE;
    for i := 1 to ThisElement.Nterms do
    begin
        Result := FALSE;
        ThisElement.ActiveTerminalIdx := i;
        for j := 1 to ThisElement.NPhases do
            if ThisElement.Closed[j] then
            begin
                Result := TRUE;
                Break;
            end;
        if not Result then
            Exit;  // didn't find a closed phase on this terminal
    end;
end;

function WriteVsourceClassFile(DSS: TDSSContext; const DSS_Class: TDSSClass; IsCktElement: Boolean): Boolean;
// Special Function to write the Vsource class and change the DSS command of the first Source
// so that there is no problem with duplication when the circuit is subsequently created
var
    F: TFileStream = nil;
    ClassName: String;
begin
    Result := TRUE;
    if DSS_Class.ElementCount = 0 then
        Exit;
    try
        ClassName := DSS_Class.Name;
        F := TFileStream.Create(DSS.CurrentDSSDir + ClassName + '.dss', fmCreate);
        DSS.SavedFileList.Add(ClassName + '.dss');
        DSS_Class.First;   // Sets DSS.ActiveDSSObject
        WriteActiveDSSObject(DSS, F, 'Edit'); // Write First Vsource out as an Edit
        while DSS_Class.Next > 0 do
        begin
            // Skip Cktelements that have been checked before and written out by
            // something else
            if Flg.HasBeenSaved in TDSSCktElement(DSS.ActiveDSSObject).Flags then
                Continue;
            // Skip disabled circuit elements; write all general DSS objects
            WriteActiveDSSObject(DSS, F, 'New');    // sets HasBeenSaved := TRUE
        end;
        DSS_Class.Saved := TRUE;

    except
        On E: Exception do
        begin
            DoSimpleMsg(DSS, 'WriteClassFile Error: %s', [E.Message], 717);
            Result := FALSE;
        end;
    end;
    FreeAndNil(F);
end;

function WriteClassFile(DSS: TDSSContext; const DSS_Class: TDSSClass; FileName: String; IsCktElement: Boolean): Boolean;
var
    F: TFileStream = nil;
    ClassName: String;
    Nrecords: Integer;
    ParClass: TDssClass;
begin
    Result := TRUE;

    if DSS_Class.ElementCount = 0 then
        Exit;

    try
        ClassName := DSS_Class.Name;
        if Length(FileName) = 0 then
            FileName := DSS.CurrentDSSDir + ClassName + '.dss';   // default file name
        F := TFileStream.Create(FileName, fmCreate);

        Nrecords := 0;

        DSS_Class.First;   // Sets ActiveDSSObject
        repeat
            // Skip Cktelements that have been checked before and written out by
            // something else
            if IsCktElement then
                with TDSSCktElement(DSS.ActiveDSSObject) do
                    if (Flg.HasBeenSaved in Flags) or (not Enabled) then
                        Continue;

            ParClass :=  DSS.ActiveDSSObject.ParentClass;
            if LowerCase(ParClass.Name) = 'loadshape' then
                if not TLoadShapeObj(DSS.ActiveDSSObject).Enabled then
                    continue;
            WriteActiveDSSObject(DSS, F, 'New');  // sets HasBeenSaved := TRUE
            Inc(Nrecords); // count the actual records

        until DSS_Class.Next = 0;

        FreeAndNil(F);

        if Nrecords > 0 then
            DSS.SavedFileList.Add(FileName)
        else
            DeleteFile(FileName);

        DSS_Class.Saved := TRUE;

    except
        On E: Exception do
        begin
            DoSimpleMsg(DSS, 'WriteClassFile Error: %s', [E.Message], 718);
            Result := FALSE;
        end;
    end;
    
    FreeAndNil(F);
end;

function checkforblanks(const S: String): String;
// Checks for blanks in the name and puts quotes around it
begin
    Result := s;
    if Pos(' ', S) > 0 then
        if (S[1] <> '(') and (S[1] <> '[') and (S[1] <> '{') and (S[1] <> '"') and (S[1] <> '''') then  // Ignore if already quoted
            Result := '"' + S + '"';
end;

procedure WriteActiveDSSObject(DSS: TDSSContext; F: TFileStream; const NeworEdit: String);
begin
    //  FSWrite(F, NeworEdit, ' "', DSS.ActiveDSSObject.FullName,'"');
    FSWrite(F, Format('%s "%s"', [NeworEdit, DSS.ActiveDSSObject.FullName]));

    DSS.ActiveDSSObject.SaveWrite(F);

    // Handle disabled circuit elements;   Modified to allow applets to save disabled elements 12-28-06
    if (DSS.ActiveDSSObject.DSSObjType and ClassMask) <> DSS_Object then
        if not TDSSCktElement(DSS.ActiveDSSObject).Enabled then
            FSWrite(F, ' ENABLED=NO');
    FSWriteln(F); // Terminate line

    Include(DSS.ActiveDSSObject.Flags, Flg.HasBeenSaved);
end;

procedure DoResetKeepList(DSS: TDSSContext);
var
    i: Integer;
begin
    with DSS.ActiveCircuit do
        for i := 1 to NumBuses do
            Buses^[i].Keep := FALSE;
end;

function ExtractComment(const s: String): String;
begin
    Result := copy(s, pos('!', s), Length(s));
end;

function RewriteAlignedFile(DSS: TDSSContext; const Filename: String): Boolean;
var
    Fin: TStream = nil;
    Fout: TFileStream = nil;
    SaveDelims, Line, Field, AlignedFile: String;
    FieldLength: pIntegerArray;
    ArraySize, FieldLen, FieldNum: Integer;
begin
    Result := TRUE;

    try
        Fin := DSS.GetROFileStream(FileName);
    except
        On E: Exception do
        begin
            DoSimpleMsg(DSS, 'Error opening file "%s": %s', [Filename, E.message], 719);
            Result := FALSE;
            Exit
        end;
    end;

    try
        AlignedFile := ExtractFilePath(FileName) + 'Aligned_' + ExtractFileName(FileName);
        Fout := TFileStream.Create(AlignedFile, fmCreate);
    except
        On E: Exception do
        begin
            DoSimpleMsg(DSS, 'Error opening file "%s": %s', [AlignedFile, E.message], 720);
            FreeAndNil(Fin);
            Result := FALSE;
            Exit;
        end;
    end;

    SaveDelims := DSS.AuxParser.Delimiters;
    DSS.AuxParser.Delimiters := ',';
    ArraySize := 10;
    FieldLength := Allocmem(Sizeof(Integer) * ArraySize);

    try
        // Scan once to set field lengths
        while (Fin.Position + 1) < Fin.Size do
        begin
            FSReadln(Fin, line);
            DSS.AuxParser.CmdString := Line;  // Load the parser
            FieldNum := 0;
            repeat
                DSS.AuxParser.NextParam;
                Field := DSS.AuxParser.StrValue;
                FieldLen := Length(Field);
                if pos(' ', Field) > 0 then
                    FieldLen := FieldLen + 2;
                if FieldLen > 0 then
                begin
                    Inc(FieldNum);
                    if FieldNum > ArraySize then
                    begin
                        ArraySize := FieldNum;
                        Reallocmem(FieldLength, Sizeof(FieldLength^[1]) * ArraySize);
                        FieldLength^[FieldNum] := FieldLen;
                    end
                    else
                    if FieldLen > FieldLength^[Fieldnum] then
                        FieldLength^[FieldNum] := FieldLen;

                end;
            until FieldLen = 0;

        end;

        // Now go back and re-read while writing the new file
        Fin.Seek(0, soBeginning);

        if (Fin.Position + 1) < Fin.Size then
        begin
            FSReadln(Fin, Line);
            DSS.AuxParser.CmdString := Line;  // Load the parser
            FieldNum := 0;
            repeat
                DSS.AuxParser.NextParam;
                Field := DSS.AuxParser.StrValue;
                if pos(' ', Field) > 0 then
                    Field := '"' + Field + '"';  // add quotes if a space in field
                FieldLen := Length(Field);
                if FieldLen > 0 then
                begin
                    Inc(FieldNum);
                    FSWrite(Fout, Pad(Field, FieldLength^[FieldNum] + 1));
                end;
            until FieldLen = 0;

            if (pos('!', Line) > 0) then
                FSWrite(Fout, ExtractComment(Line));

            FSWriteln(Fout);
        end;
    finally     // Make sure we do this stuff ...
        FreeAndNil(Fin);
        FreeAndNil(Fout);

        Reallocmem(FieldLength, 0);
        DSS.AuxParser.Delimiters := SaveDelims;

    end;

    DSS.GlobalResult := AlignedFile;
end;

function DoExecutiveCommand(DSS: TDSSContext; const s: String): Integer;
begin
    DSS.DSSExecutive.command := S;
    Result := DSS.DSSExecutive.Error;
end;

function CheckParallel(const Line1, Line2: TDSSCktElement): Boolean;
// Check to see if two lines are in parallel
begin
    Result := FALSE;
    if Line1.Terminals[0].BusRef = Line2.Terminals[0].BusRef then
        if Line1.Terminals[1].BusRef = Line2.Terminals[1].BusRef then
        begin
            Result := TRUE;
            Exit;
        end;
    if Line1.Terminals[1].BusRef = Line2.Terminals[0].BusRef then
        if Line1.Terminals[0].BusRef = Line2.Terminals[1].BusRef then
        begin
            Result := TRUE;
            Exit;
        end;
end;

function GetMaxPUVoltage(DSS: TDSSContext): Double;
var
    i, j, nref: Integer;
begin
    Result := -1.0;
    with DSS.ActiveCircuit do
    begin
        for i := 1 to NumBuses do
        begin
            if buses^[i].kVBase > 0.0 then
            begin
                for j := 1 to Buses^[i].NumNodesThisBus do
                begin
                    Nref := Buses^[i].GetRef(j);
                    if Nref > 0 then
                        Result := Max(Result, Cabs(Solution.NodeV^[nref]) / Buses^[i].kvbase);
                end;
            end;
        end;
        Result := Result * 0.001;
    end;
end;

function GetMinPUVoltage(DSS: TDSSContext; IgnoreNeutrals: Boolean): Double;
var
    i, j, nref: Integer;
    MinFound: Boolean;
    Vmagpu: Double;
begin
    Result := 1.0e50; // start with big number
    MinFound := FALSE;

    with DSS.ActiveCircuit do
    begin
        for i := 1 to NumBuses do
            with buses^[i] do
                if kVBase > 0.0 then
                begin
                    for j := 1 to NumNodesThisBus do
                    begin
                        Nref := GetRef(j);
                        if Nref > 0 then
                        begin
                            Vmagpu := Cabs(Solution.NodeV^[nref]) / kvbase;
                            if IgnoreNeutrals then
                            begin
                                if (Vmagpu > 100.0) then
                                begin  // 0.1 pu
                                    Result := Min(Result, Vmagpu);   // only check buses greater than 10%
                                    MinFound := TRUE;
                                end;
                            end
                            else
                            begin
                                Result := Min(Result, Vmagpu);
                                MinFound := TRUE;
                            end;
                        end;
                    end;
                end;
        Result := Result * 0.001;
    end;

    if not MinFound then
        Result := -1.0;
end;

function GetTotalPowerFromSources(DSS: TDSSContext): Complex;
var
    CktElem: TDSSCktElement;
begin
    Result := CZERO;
    cktElem := DSS.ActiveCircuit.Sources.First;
    while CktElem <> NIL do
    begin
        Result -= CktElem.power[1];
        cktElem := DSS.ActiveCircuit.Sources.Next;
    end;
end;

procedure WriteUniformGenerators(DSS: TDSSContext; F: TFileStream; kW, PF: Double; DoGenerators: Boolean);
// Distribute the generators uniformly amongst the feeder nodes that have loads
var
    kWeach: Double;
    LoadClass: TDSSClass;
    pLoad: TLoadObj;
    Count, i: Integer;

begin
    LoadClass := GetDSSClassPtr(DSS, 'load');
    Count := LoadClass.ElementList.Count;

    kWEach := kW / Max(1.0, round(Count));
    if DSS.ActiveCircuit.PositiveSequence then
        kWEach := kWeach / 3.0;

    for i := 1 to Count do
    begin
        pLoad := TLoadObj(LoadClass.ElementList.Get(i));
        if pLoad.Enabled then
        begin
            if DoGenerators then
                FSWrite(F, Format('new generator.DG_%d  bus1=%s', [i, pLoad.GetBus(1)]))
            else
                FSWrite(F, Format('new load.DL_%d  bus1=%s', [i, pLoad.GetBus(1)]));
            with DSS.ActiveCircuit do
            begin
                FSWrite(F, Format(' phases=%d kV=%-g', [pLoad.NPhases, pLoad.kVLoadBase]));
                FSWrite(F, Format(' kW=%-g', [kWeach]));
                FSWrite(F, Format(' PF=%-.3g', [PF]));
            end;
            FSWrite(F, ' model=1');
            FSWriteln(F);
        end;
    end;
end;

procedure WriteRandomGenerators(DSS: TDSSContext; F: TFileStream; kW, PF: Double; DoGenerators: Boolean);
// Distribute Generators randomly to loaded buses
var
    kWeach: Double;
    LoadClass: TDSSClass;
    pLoad: TLoadObj;
    Count, i, LoadCount: Integer;

begin
    LoadClass := GetDSSClassPtr(DSS, 'load');
    Count := LoadClass.ElementList.Count;
    // Count enabled loads
    LoadCount := 0;
    for i := 1 to Count do
    begin
        pLoad := TLoadObj(LoadClass.ElementList.Get(i));
        if pLoad.Enabled then
            inc(LoadCount);
    end;


    kWEach := kW / LoadCount;  // median sized generator
    if DSS.ActiveCircuit.PositiveSequence then
        kWEach := kWEach / 3.0;

    randomize;

    // Place random sizes on load buses so that total is approximately what was spec'd
    for i := 1 to Count do
    begin
        pLoad := TLoadObj(LoadClass.ElementList.Get(i));
        if pLoad.Enabled then
        begin
            if DoGenerators then
                FSWrite(F, Format('new generator.DG_%d  bus1=%s', [i, pLoad.GetBus(1)]))
            else
                FSWrite(F, Format('new load.DL_%d  bus1=%s', [i, pLoad.GetBus(1)]));
            with DSS.ActiveCircuit do
            begin
                FSWrite(F, Format(' phases=%d kV=%-g', [pLoad.NPhases, pLoad.kVLoadBase]));
                FSWrite(F, Format(' kW=%-g', [kWeach * random * 2.0]));
                FSWrite(F, Format(' PF=%-.3g', [PF]));
            end;
            FSWrite(F, ' model=1');
            FSWriteln(F);
        end;
    end;
end;

procedure WriteEveryOtherGenerators(DSS: TDSSContext; F: TFileStream; kW, PF: Double; Skip: Integer; DoGenerators: Boolean);
// distribute generators on every other load, skipping the number specified

// Distribute the generator Proportional to load
var
    kWeach, TotalkW: Double;
    LoadClass: TDSSClass;
    pLoad: TLoadObj;
    Count, i, skipcount: Integer;

begin
    LoadClass := GetDSSClassPtr(DSS, 'load');
    Count := LoadClass.ElementList.Count;
    // Add up the rated load in the enabled loads where gens will be placed
    TotalkW := 0.0;
    Skipcount := Skip;
    for i := 1 to Count do
    begin
        pLoad := TLoadObj(LoadClass.ElementList.Get(i));
        if pLoad.Enabled then
            // Do not count skipped loads
            if Skipcount = 0 then
            begin
                TotalkW := TotalkW + pLoad.kWBase;  // will be right value if pos seq, too
                SkipCount := Skip;  // start counter over again
            end
            else
                Dec(SkipCount);
    end;

    if DSS.ActiveCircuit.PositiveSequence then
        kWeach := kW / TotalkW / 3.0
    else
        kWeach := kW / TotalkW;

    SkipCount := Skip;
    for i := 1 to Count do
    begin
        pLoad := TLoadObj(LoadClass.ElementList.Get(i));
        if pLoad.Enabled then
            if SkipCount = 0 then
            begin
                if DoGenerators then
                    FSWrite(F, Format('new generator.DG_%d  bus1=%s', [i, pLoad.GetBus(1)]))
                else
                    FSWrite(F, Format('new load.DL_%d  bus1=%s', [i, pLoad.GetBus(1)]));
                with DSS.ActiveCircuit do
                begin
                    FSWrite(F, Format(' phases=%d kV=%-g', [pLoad.NPhases, pLoad.kVLoadBase]));
                    FSWrite(F, Format(' kW=%-g ', [kWeach * pLoad.kWBase]));
                    FSWrite(F, Format(' PF=%-.3g', [PF]));
                end;
                FSWrite(F, ' model=1');
                FSWriteln(F);
                SkipCount := Skip;
            end
            else
                Dec(SkipCount);
    end;
end;

procedure WriteProportionalGenerators(DSS: TDSSContext; F: TFileStream; kW, PF: Double; DoGenerators: Boolean);
// Distribute the generator Proportional to load
var
    kWeach,
    TotalkW: Double;
    LoadClass: TDSSClass;
    pLoad: TLoadObj;
    Count, i: Integer;
begin
    LoadClass := GetDSSClassPtr(DSS, 'load');
    Count := LoadClass.ElementList.Count;
    // Add up the rated load in the enabled loads
    TotalkW := 0.0;
    for i := 1 to Count do
    begin
        pLoad := TLoadObj(LoadClass.ElementList.Get(i));
        if pLoad.Enabled then
        begin
            TotalkW := TotalkW + pLoad.kWBase;  // will be right value if pos seq, too
        end;
    end;

    if DSS.ActiveCircuit.PositiveSequence then
        kWeach := kW / TotalkW / 3.0
    else
        kWeach := kW / TotalkW;

    for i := 1 to Count do
    begin
        pLoad := TLoadObj(LoadClass.ElementList.Get(i));
        if pLoad.Enabled then
        begin
            if DoGenerators then
                FSWrite(F, Format('new generator.DG_%d  bus1=%s', [i, pLoad.GetBus(1)]))
            else
                FSWrite(F, Format('new load.DL_%d  bus1=%s', [i, pLoad.GetBus(1)]));
            with DSS.ActiveCircuit do
            begin
                FSWrite(F, Format(' phases=%d kV=%-g', [pLoad.NPhases, pLoad.kVLoadBase]));
                FSWrite(F, Format(' kW=%-g', [kWeach * pLoad.kWBase]));
                FSWrite(F, Format(' PF=%-.3g', [PF]));
            end;
            FSWrite(F, ' model=1');
            FSWriteln(F);
        end;
    end;
end;


procedure MakeDistributedGenerators(DSS: TDSSContext; kW, PF: Double; How: String; Skip: Integer; Fname: String; DoGenerators: Boolean);
var
    F: TFileStream = nil;
    WhatStr: String;

begin
    // Write outputfile and then redirect command parser to it.
    try
        if FileExists(Fname) then
        begin
            // Since we don't have a warning mechanism, it's better to abort
            // and let the user remove the existing file first
            DoSimpleMsg(DSS, 'File "%s" was about to be overwritten. Rename/remove the existing file and try again.', [Fname], 721);
            Exit;
        end;
        F := TFileStream.Create(Fname, fmCreate);
    except
        On E: Exception do
        begin
            DoSimpleMsg(DSS, 'Error opening "%s" for writing. Aborting.', [Fname], 722);
            Exit;
        end;
    end;

    try
        if DoGenerators then
            WhatStr := 'Generators'
        else
            WhatStr := 'Loads';

        FSWriteln(F, '! Created with Distribute Command:');
        FSWriteln(f, Format('! Distribute kW=%-.6g PF=%-.6g How=%s Skip=%d  file=%s  what=%s', [kW, PF, How, Skip, Fname, WhatStr]));
        FSWriteln(F);
        // FSWriteln(F, 'Set allowduplicates=yes');
        if Length(How) = 0 then
            How := 'P';
        case Uppercase(How)[1] of
            'U':
                WriteUniformGenerators(DSS, F, kW, PF, DoGenerators);
            'R':
                WriteRandomGenerators(DSS, F, kW, PF, DoGenerators);
            'S':
                WriteEveryOtherGenerators(DSS, F, kW, PF, Skip, DoGenerators);
        else
            WriteProportionalGenerators(DSS, F, kW, PF, DoGenerators);
        end;
        DSS.GlobalResult := Fname;
    finally
        // FSWriteln(F, 'Set allowduplicates=no');
        FreeAndNil(F);
        SetLastResultFile(DSS, Fname);
    end;
end;

function GetDSSArray_Real(n: Integer; dbls: pDoubleArray; scale: Double): String;
var
    i: Integer;
begin
    if dbls = NIL then
    begin
        Result := '';
        Exit;
    end;
    Result := '[';

    if scale = 1 then
    begin
        for i := 1 to n do
            Result := Result + Format(' %-.6g', [dbls^[i]])
    end
    else
    begin
        for i := 1 to n do
            Result := Result + Format(' %-.6g', [dbls^[i] / scale]);
    end;

    Result := Result + ']';
end;

function GetDSSArray_Single(n: Integer; sngs: pSingleArray): String;
var
    i: Integer;
    tmp: Double;
begin
    if sngs = NIL then
    begin
        Result := '';
        Exit;
    end;
    Result := '[';
    for i := 1 to n do
    begin
        tmp := sngs^[i];
        Result := Result + Format(' %-.6g', [tmp]);
    end;
    Result := Result + ']';
end;

function GetDSSArray_Integer(n: Integer; ints: pIntegerArray): String;
var
    i: Integer;
begin
    if ints = NIL then
    begin
        Result := '';
        Exit;
    end;
    Result := '[';
    for i := 1 to n do
        Result := Result + Format(' %-.d', [ints^[i]]);
    Result := Result + ']';
end;

procedure CmulArray(pc: pcomplexarray; Multiplier: Double; size: Integer);  // Multiply a complex array times a double
var
    i: Integer;
begin
    for i := 1 to size do
        pc[i] := pc[i] * Multiplier;
end;

function GetMaxCktElementSize(DSS: TDSSContext): Integer;
var
    i: Integer;

begin
    Result := 0;

    with DSS.ActiveCircuit do
        for i := 1 to NumDevices do
            Result := max(result, TDSSCktElement(CktElements.Get(i)).Yorder);
end;

function GetUniqueNodeNumber(DSS: TDSSContext; const sBusName: String; StartNode: Integer): Integer;
// To help avoid collisions of neutral numbers, this function returns a node number that is not being used,
// Starting at the StartNode value
var
    iBusidx: Integer;
begin
    Result := StartNode;
    iBusidx := DSS.ActiveCircuit.Buslist.Find(sBusName);
    if iBusidx > 0 then
        while DSS.ActiveCircuit.Buses^[iBusidx].FindIdx(Result) <> 0 do
            Inc(Result);
    DSS.ActiveCircuit.Buses^[iBusidx].Add(DSS.ActiveCircuit, result);  // add it to the list so next call will be unique
end;

procedure ShowMessageBeep(DSS: TDSSContext; const s: String);
begin
    Beep;
    DSSInfoMessageDlg(s);
end;

function IsPathBetween(FromLine, ToLine: TPDElement): Boolean;
var
    PDElem: TPDelement;
begin
    PDElem := FromLine;
    Result := FALSE;
    while PDElem <> NIL do
    begin
        if PDElem = ToLine then
        begin
            Result := TRUE;
            Exit;
        end;
        PDElem := PDElem.ParentPDElement;
    end;
end;

procedure TraceAndEdit(DSS: TDSSContext; FromLine, ToLine: TPDElement; NPhases: Integer; EditStr: String);
// Trace back up a tree and execute an edit command string
var
    pLine: TPDElement;
begin
    pLine := FromLine;
    while pLine <> NIL do
    begin
        if (pLine.NPhases = NPhases) or (Nphases = 0) then
        begin
            DSS.Parser.CmdString := EditStr;
            pLine.Edit(DSS.Parser);   // Uses Parser
        end;
        if pLine = ToLine then
            Break;
        pLine := pLine.ParentPDElement;
    end;
end;

procedure GoForwardAndRephase(DSS: TDSSContext; FromLine: TPDElement; const PhaseString, EditStr, ScriptFileName: String; TransStop: Boolean);
// Trace forward down a tree and Generate a script file to change the phase
var
    pPDelem: TPDElement;
    pShuntObject: TDSSCktElement;
    pMeter: TEnergyMeterObj;
    i: Integer;
    S: String;
    Fout: TFileStream = nil;
    FileName: String;
    XfmrLevel: Integer;

begin
    pMeter := FromLine.MeterObj as TEnergyMeterObj;

    // Search for starting line in branchlist
    pPDelem := pMeter.BranchList.first;
    while pPDelem <> NIL do
    begin
        if (FromLine = pPDelem) then
        begin
            Break;
        end;
        pPDelem := pMeter.BranchList.GoForward;
    end;

    // Error check
    if pPDelem = NIL then
    begin
        DoSimpleMsg(DSS, '"%s" not found in Meter Zone.', [FromLine.FullName], 723);
        Exit;
    end;

    try
        FileName := DSS.OutputDirectory + DSS.CircuitName_ + ScriptFileName;
        DSS.GlobalResult := FileName;

        Fout := TFileStream.Create(filename, fmCreate);    

        pMeter.BranchList.StartHere;
        pPDelem := pMeter.BranchList.GoForward;

        while pPDelem <> NIL do
        begin
            S := 'edit "' + pPDelem.FullName + '"';
            // ----------------LINES---------------------------------------------------
            if IsLineElement(pPDelem) then
            begin
                for i := 1 to pPDElem.NTerms do
                begin
                    S := S + Format(' Bus%d=%s%s', [i, StripExtension(pPDelem.GetBus(i)), PhaseString]);
                    //  Parser.CmdString := Format('Bus$d=%s%s',[i, StripExtension(pPDelem.GetBus(i)), PhaseString]);
                    //  pPDelem.Edit;
                end;

                // When we're done with that, we'll send the Edit string
                if Length(EditStr) > 0 then
                begin
                    S := S + '  ' + EditStr;
                //  Parser.CmdString := EditStr;
                //  pPDelem.Edit;   // Uses Parser
                end;

                FSWriteln(Fout, S);

                // Now get all shunt objects connected to this branch
                pShuntObject := pMeter.BranchList.FirstObject;
                while pShuntObject <> NIL do
                begin
                    // 1st Terminal Only
                    i := 1;
                    S := 'edit "' + pShuntObject.FullName + '"';
                    S := S + Format(' Bus%d=%s%s', [i, StripExtension(pShuntObject.GetBus(i)), PhaseString]);
                    if Length(EditStr) > 0 then
                        S := S + '  ' + EditStr;
                    FSWriteln(Fout, S);
                    //  Parser.CmdString := Format('Bus$d=%s%s',[i, StripExtension(pShuntObject.GetBus(1)), PhaseString]);
                    //  pShuntObject.Edit;
                    pShuntObject := pMeter.BranchList.NextObject
                end;
                pPDelem := pMeter.BranchList.GoForward;
            end // IsLine
            else
            // ----------------TRANSFORMERS---------------------------------------------------
            if IsTransformerElement(pPDELEM) then
            begin
                //  We'll stop at transformers and change only the primary winding.
                //  Then we'll cycle forward until the lexical level is less or we're done
                XFmrLevel := pMeter.BranchList.Level;
                S := S + Format(' wdg=1 Bus=%s%s  %s', [StripExtension(pPDelem.GetBus(1)), PhaseString, EditStr]);
                if not TransStop then
                    S := S + Format(' wdg=2 Bus=%s%s  %s', [StripExtension(pPDelem.GetBus(2)), PhaseString, EditStr]);
                FSWriteln(Fout, S);

                // By default Go forward in the tree until we bounce back up to a line section above the transformer
                if TransStop then
                begin
                    repeat
                        pPDelem := pMeter.BranchList.GoForward;
                    until (pPDelem = NIL) or (pMeter.BranchList.Level <= XfmrLevel);
                end
                else
                    pPDelem := pMeter.BranchList.GoForward; // Then we get lines and loads beyond transformer
            end;
        end;
    finally
        FreeAndNil(Fout);
        FireOffEditor(DSS, FileName);
    end;
end;

function MaxdblArrayValue(npts: Integer; dbls: pDoubleArray): Double;
// returns max value of an array of doubles
var
    i: Integer;
begin
    Result := 0.0;
    if npts = 0 then
        exit;

    Result := dbls^[1];
    for i := 2 to npts do
        Result := max(Result, dbls^[i]);
end;

function iMaxAbsdblArrayValue(npts: Integer; dbls: pDoubleArray): Integer;
// Returns index of max array value  in abs value
var
    i: Integer;
    MaxValue: Double;
begin
    Result := 0;
    if npts = 0 then
        exit;

    Result := 1;
    MaxValue := abs(dbls^[1]);
    for i := 2 to npts do
        if abs(dbls^[i]) > Maxvalue then
        begin
            Maxvalue := abs(dbls^[i]);
            Result := i;   // save index
        end;
end;

function iMaxAbssngArrayValue(npts: Integer; sngs: pSingleArray): Integer;
// Returns index of max array value  in abs value
var
    i: Integer;
    MaxValue: Single;
begin
    Result := 0;
    if npts = 0 then
        exit;

    Result := 1;
    MaxValue := abs(sngs^[1]);
    for i := 2 to npts do
        if abs(sngs^[i]) > Maxvalue then
        begin
            Maxvalue := abs(sngs^[i]);
            Result := i;   // save index
        end;
end;

function InterpretColorName(DSS: TDSSContext; const s: String): Integer;
begin
    Result := clBlue;  // default color
    try
        if CompareTextShortest(S, 'black') = 0 then
            Result := clBlack
        else
        if CompareTextShortest(S, 'Maroon') = 0 then
            Result := clMaroon
        else
        if CompareTextShortest(S, 'Green') = 0 then
            Result := clGreen
        else
        if CompareTextShortest(S, 'Olive') = 0 then
            Result := clOlive
        else
        if CompareTextShortest(S, 'Navy') = 0 then
            Result := clNavy
        else
        if CompareTextShortest(S, 'Purple') = 0 then
            Result := clPurple
        else
        if CompareTextShortest(S, 'Teal') = 0 then
            Result := clTeal
        else
        if CompareTextShortest(S, 'Gray') = 0 then
            Result := clGray
        else
        if CompareTextShortest(S, 'Silver') = 0 then
            Result := clSilver
        else
        if CompareTextShortest(S, 'Red') = 0 then
            Result := clRed
        else
        if CompareTextShortest(S, 'Lime') = 0 then
            Result := clLime
        else
        if CompareTextShortest(S, 'Yellow') = 0 then
            Result := clYellow
        else
        if CompareTextShortest(S, 'Blue') = 0 then
            Result := clBlue
        else
        if CompareTextShortest(S, 'Fuchsia') = 0 then
            Result := clFuchsia
        else
        if CompareTextShortest(S, 'Aqua') = 0 then
            Result := clAqua
        else
        if CompareTextShortest(S, 'LtGray') = 0 then
            Result := clLtGray
        else
        if CompareTextShortest(S, 'DkGray') = 0 then
            Result := clDkGray
        else
        if CompareTextShortest(S, 'White') = 0 then
            Result := clWhite
        else
            Result := StrToInt(S);
    except
        On E: Exception do
            DoSimpleMsg(DSS, 'Invalid Color Specification: "%s".', [S], 724);
    end;
end;

function MakeNewCktElemName(DSS: TDSSContext; const oldname: String): String;
begin
    SetObject(DSS, OldName);  // set object active
    with DSS.ActiveDSSObject do
        Result := Format('%s.%s%d', [ParentClass.Name, copy(ParentClass.Name, 1, 4), ClassIndex]);
end;

function ConstructElemName(DSS: TDSSContext; const Param: String): String;
// Construct an element name, sustituting @var values if any
var
    FClassName, FObjName: String;
begin
    ParseObjectClassandName(DSS, lowercase(param), FClassName, FObjName);  // insert @var test
    result := Format('%s.%s', [FClassName, FObjName]);
end;

procedure Obfuscate(DSS: TDSSContext);
// Rename Buses and element names to generic names to remove identifiable names
var
    i, bref: Integer;
    dotpos: Integer;
    DevListSize: Integer;
    TempBusList: TBusHashListType;
    pCktElem: TDSSCktElement;
    pCtrlElem: TDSSCktElement;
    S, Nodes: String;
    OldBusName: String;
    NewBusName: String;
    Baseclass: Integer;
    ElemClass: Integer;
    ControlUpDateStrings: TstringList;
    ControlUpDatePtrs: Tlist;

    procedure RenameCktElem(pelem: TDSSCktElement); // local proc
    begin
        with pelem do
        begin
            Name := Format('%s%d', [copy(ParentClass.Name, 1, 4), ClassIndex]);
            DSS.ActiveCircuit.DeviceList.Add(Name); // Make a new device list corresponding to the CktElements List
            Include(pelem.Flags, Flg.Checked);
        end;
    end;

begin
    // Make sure buslist exists

    if DSS.ActiveCircuit = NIL then
        Exit;
    if DSS.ActiveCircuit.BusList.Count <= 0 then
        Exit;
    with DSS.ActiveCircuit do
    begin
        TempBusList := TBusHashListType.Create(BusList.Count);

        // Rename Buses
        for i := 1 to BusList.Count do
            TempBusList.Add(Format('B_%d', [i]));

        BusList.Free;
        BusList := TempBusList; // Reassign

        // Rename the bus names in each circuit element before renaming the elements
        pCktElem := Cktelements.First;
        while pCktElem <> NIL do
        begin
            BaseClass := (pCktElem.DSSObjType and BASECLASSMASK);
            if (BaseClass = PC_ELEMENT) or (BaseClass = PD_ELEMENT) then
            begin
                S := '';
                for i := 1 to pCktElem.NTerms do
                begin
                    OldBusName := pCktElem.GetBus(i);
                    dotpos := pos('.', OldBusName);
                    if dotpos = 0 then
                        Nodes := ''
                    else
                        Nodes := Copy(OldBusName, dotpos, length(OldBusName));    // preserve node designations if any
                    bref := pCktElem.terminals[i - 1].BusRef;
                    NewBusName := Format('B_%d%s', [bref, Nodes]);
                    //Check for Transformer because that will be an exception
                    case (pCktElem.DSSObjType and CLASSMASK) of
                        XFMR_ELEMENT:
                            S := S + Format('Wdg=%d Bus=%s ', [i, NewBusName]);
                    else
                        S := S + Format('Bus%d=%s ', [i, NewBusName]);
                    end;
                end;
                DSS.Parser.CmdString := S;
                pCktElem.Edit(DSS.Parser);
            end;
            pCktElem := CktElements.Next;
        end;

        // Rename the circuit elements to generic values
        // Have to catch the control elements and edit some of their parameters

        // first, make scripts to change the monitored element names in the controls to what they will be
        ControlUpDateStrings := TStringList.Create;
        ControlUpDatePtrs := TList.Create;

        pCktElem := Cktelements.First;
        while pCktElem <> NIL do
        begin
            case (pCktElem.DSSObjType and CLASSMASK) of
                CAP_CONTROL:
                begin
                    S := Format('Element=%s ', [MakeNewCktElemName(DSS, pCktElem.GetPropertyValue(1))]);
                    ControlUpDateStrings.Add(S + Format('Capacitor=%s ', [Copy(MakeNewCktElemName(DSS, 'capacitor.' + pCktElem.GetPropertyValue(3)), 11, 100)]));
                    ControlUpDatePtrs.Add(pCktElem);
                end;
                REG_CONTROL: ;   // handled below
                RELAY_CONTROL:
                begin
                    S := Format('MonitoredObj=%s ', [MakeNewCktElemName(DSS, pCktElem.GetPropertyValue(1))]);
                    ControlUpDateStrings.Add(S + Format('SwitchedObj=%s ', [MakeNewCktElemName(DSS, pCktElem.GetPropertyValue(3))]));
                    ControlUpDatePtrs.Add(pCktElem);
                end;
                RECLOSER_CONTROL:
                begin
                    S := Format('MonitoredObj=%s ', [MakeNewCktElemName(DSS, pCktElem.GetPropertyValue(1))]);
                    ControlUpDateStrings.Add(S + Format('SwitchedObj=%s ', [MakeNewCktElemName(DSS, pCktElem.GetPropertyValue(3))]));
                    ControlUpDatePtrs.Add(pCktElem);
                end;
                FUSE_CONTROL:
                begin
                    S := Format('MonitoredObj=%s ', [MakeNewCktElemName(DSS, pCktElem.GetPropertyValue(1))]);
                    ControlUpDateStrings.Add(S + Format('SwitchedObj=%s ', [MakeNewCktElemName(DSS, pCktElem.GetPropertyValue(3))]));
                    ControlUpDatePtrs.Add(pCktElem);
                end;
                GEN_CONTROL:
                begin
                    ControlUpDateStrings.Add(Format('Element=%s ', [MakeNewCktElemName(DSS, pCktElem.GetPropertyValue(1))]));
                    ControlUpDatePtrs.Add(pCktElem);
                end;
                STORAGE_CONTROL:
                begin
                    ControlUpDateStrings.Add(Format('Element=%s ', [MakeNewCktElemName(DSS, pCktElem.GetPropertyValue(1))]));
                    ControlUpDatePtrs.Add(pCktElem);
                end;
                SWT_CONTROL:
                begin
                    ControlUpDateStrings.Add(Format('SwitchedObj=%s ', [MakeNewCktElemName(DSS, pCktElem.GetPropertyValue(1))]));
                    ControlUpDatePtrs.Add(pCktElem);
                end;
            end;

            pCktElem := Cktelements.Next;
        end;

        pCktElem := Cktelements.First;
        while pCktElem <> NIL do
        begin
            Exclude(pCktElem.Flags, Flg.Checked);     // Initialize to not checked
            pCktElem := Cktelements.Next;
        end;

        DevListSize := DeviceList.Count;
        DeviceList.Free;
        DeviceList := THashList.Create(DevListSize);

        pCktElem := Cktelements.First;
        while pCktElem <> NIL do
        begin
            if not (Flg.Checked in pCktElem.Flags) then
            begin
                ElemClass := (pCktElem.DSSObjType and CLASSMASK);
                RenameCktElem(pCktElem);
                case ElemClass of
                    XFMR_ELEMENT:
                        if Flg.HasControl in pCktElem.Flags then
                        begin
                            pCtrlElem := pCktElem.ControlElementList.First;
                            while assigned(pCtrlElem) do
                            begin
                                if (pCtrlElem.DSSObjType and CLASSMASK) = REG_CONTROL then
                                begin
                                    DSS.Parser.CmdString := Format('Transformer=%s', [pCktElem.Name]);
                                    pCtrlElem.Edit(DSS.Parser);
                                end;
                                pCtrlElem := pCktElem.ControlElementList.Next;
                            end;
                        end;
                else
                    // nada
                end;
            end;
            pCktElem := Cktelements.Next;
        end;

        // Run the control update scripts now that everything is renamed
        for i := 0 to ControlUpDatePtrs.Count - 1 do
        begin
            pCktElem := ControlUpDatePtrs.Items[i];
            DSS.Parser.CmdString := ControlUpDateStrings.Strings[i];
            pCktElem.Edit(DSS.Parser);
        end;

        ControlUpDateStrings.Free;
        ControlUpDatePtrs.Free;
    end;  // With
end;

function QuadSolver(const a, b, c: Double): Double; // returns largest of two answers
var
    Ans1, Ans2, MidTerm, a2: Double;
begin
    Result := 0.0;   // default return
    if a = 0.0 then
    begin
        if b <> 0.0 then
            Result := -c / b;
    end
    else
    begin
        MidTerm := sqrt(b * b - 4.0 * a * c);
        a2 := 2.0 * a;
        Ans1 := (-b + MidTerm) / a2;
        Ans2 := (-b - MidTerm) / a2;
        // return most positive number
        if (Ans1 > Ans2) then
            Result := Ans1
        else
            Result := Ans2;
    end;
end;

function GetOCPDeviceType(pElem: TDSSCktElement): Integer;
var
    i: Integer;
    pCktElement: TDSSCktElement;
begin
    Result := 0;
    i := 1;
    repeat
        pCktElement := pElem.ControlElementList.Get(i);
        if pCktElement <> NIL then
            case (pCktElement.DSSObjType and CLASSMASK) of

                FUSE_CONTROL:
                    Result := 1;
                RECLOSER_CONTROL:
                    Result := 2;
                RELAY_CONTROL:
                    Result := 3;

            end;
        inc(i);
    until (i > pElem.ControlElementList.Count) or (Result > 0);
end;

function GetOCPDeviceTypeString(icode: Integer): String;
begin
    case iCode of
        1:
            Result := 'FUSE';
        2:
            Result := 'RECLOSER';
        3:
            Result := 'RELAY';
    else
        Result := 'Unknown';
    end;
end;

function GetNodeString(const Busname: String): String;
var
    dotpos: Integer;
begin
    dotpos := pos('.', BusName);
    if dotpos = 0 then
        Result := ''
    else
        Result := Copy(BusName, dotpos, length(BusName));    // preserve node designations if any
end;

function AdjustInputFilePath(DSS: TDSSContext; param: String): String;
begin
{$IFDEF WINDOWS}        
    param := ReplaceStr(param, '/', PathDelim);
{$ELSE}
    param := ReplaceStr(param, '\', PathDelim);
{$ENDIF}
    if (not DSS_CAPI_ALLOW_CHANGE_DIR) and FileExists(DSS.CurrentDSSDir + Param) then // and (not FileExists(Param)) 
        Result := DSS.CurrentDSSDir + Param
    else
        Result := Param;
end;

procedure FSWrite(F: TFileStream; S: String); inline; overload;
begin
    F.WriteBuffer(S[1], Length(S));
end;

procedure FSWrite(F: TFileStream; S: String; S2: String); inline; overload;
begin
    F.WriteBuffer(S[1], Length(S));
    F.WriteBuffer(S2[1], Length(S2));
end;

procedure FSWrite(F: TFileStream; S: String; S2: String; S3: String); inline; overload;
begin
    F.WriteBuffer(S[1], Length(S));
    F.WriteBuffer(S2[1], Length(S2));
    F.WriteBuffer(S3[1], Length(S3));
end;

procedure FSWriteLn(F: TFileStream; S: String = ''); inline; overload;
begin
    F.WriteBuffer(S[1], Length(S));
    F.WriteBuffer(sCRLF[1], Length(sCRLF));
end;

procedure FSWriteLn(F: TFileStream; S: String; S2: String); inline; overload;
begin
    F.WriteBuffer(S[1], Length(S));
    F.WriteBuffer(S2[1], Length(S2));
    F.WriteBuffer(sCRLF[1], Length(sCRLF));
end;

procedure FSWriteLn(F: TFileStream; S: String; S2: String; S3: String); inline; overload;
begin
    F.WriteBuffer(S[1], Length(S));
    F.WriteBuffer(S2[1], Length(S2));
    F.WriteBuffer(S3[1], Length(S3));
    F.WriteBuffer(sCRLF[1], Length(sCRLF));
end;

procedure FSWriteLn(F: TFileStream; Ss: Array of String); inline; overload;
var 
    i: integer;
begin
    for i := 0 to Length(Ss) - 1 do
    begin
        F.WriteBuffer(Ss[i][1], Length(Ss[i]));
        F.WriteBuffer(sCRLF[1], Length(sCRLF));
    end;
end;

procedure FSReadln(F: TStream; out S: String); // TODO: optimize?
var
    ch: AnsiChar; 
begin
    S := '';
    repeat
        if F.Read(ch, SizeOf(AnsiChar)) <> SizeOf(AnsiChar) then 
            break;
        
        S := S + ch;
    until ch = #10;

    //TODO: Do we need to handle files with classic Mac OS line endings too?
    if (Length(S) >= 1) and (S[Length(S)] = #10) then
    begin
        if (Length(S) >= 2) and (S[Length(S) - 1] = #13) then
            SetLength(S, Length(S) - 2)
        else
            SetLength(S, Length(S) - 1)
    end;
end;

procedure FSFlush(F: TFileStream);
begin
{$IFDEF WINDOWS}
    FlushFileBuffers(F.Handle);
{$ELSE}
    FPFSync(F.Handle);
{$ENDIF}
end;

function SliceProps(props: pStringArray; count: Integer): ArrayOfString; // The built-in Slice was causing issues on ARM64
var
    i: Integer;
begin
    SetLength(Result, count);
    for i := 1 to count do
        Result[i - 1] := props[i];
end;

procedure DoSngFile(DSS: TDSSContext; var pA, pB: PDoubleArray; var NumPoints: Integer; OnlyLoadB: Boolean; const FileName: String; const ClassName: String; RoundA: Boolean);
var
    F: TStream = nil;
    sA, 
    sB: Single;
    i: Integer;
begin
    try
        try
            F := DSS.GetROFileStream(FileName);
        except
            DoSimpleMsg(DSS, 'Error opening file: "%s"', [FileName], 615);
            FreeAndNil(F);
            Exit;
        end;

        ReAllocmem(pB, Sizeof(Double) * NumPoints);
        i := 0;
        if not OnlyLoadB then
        begin
            ReAllocmem(pA, Sizeof(Double) * NumPoints);
            while ((F.Position + 1) < F.Size) and (i < NumPoints) do
            begin
                Inc(i);

                if F.Read(sA, SizeOf(sA)) <> SizeOf(sA) then 
                    Break;

                pA[i] := sA;

                if F.Read(sB, SizeOf(sB)) <> SizeOf(sB) then 
                    Break;

                pB[i] := sB;
            end;
        end
        else
        begin
            while ((F.Position + 1) < F.Size) and (i < NumPoints) do
            begin
                Inc(i);

                if F.Read(sB, SizeOf(sB)) <> SizeOf(sB) then 
                    Break;

                pB[i] := sB;
            end;
        end;

        FreeAndNil(F);
        if i <> NumPoints then
            NumPoints := i;

        if RoundA then
            for i := 1 to NumPoints do
                pA[i] := Round(pA[i]);

    except
        DoSimpleMsg(DSS, 'Error Processing binary (single) %s File: "%s"', [ClassName, FileName], 616);
        FreeAndNil(F);
        Exit;
    end;
end;

procedure DoDblFile(DSS: TDSSContext; var pA, pB: PDoubleArray; var NumPoints: Integer; OnlyLoadB: Boolean; const FileName: String; const ClassName: String; RoundA: Boolean);
var
    F: TStream = nil;
    i: Integer;
begin
    try
        try
            F := DSS.GetROFileStream(FileName);
        except
            DoSimpleMsg(DSS, 'Error opening file: "%s"', [FileName], 615);
            FreeAndNil(F);
            Exit;
        end;

        ReAllocmem(pB, Sizeof(Double) * NumPoints);
        i := 0;
        if not OnlyLoadB then
        begin
            ReAllocmem(pA, Sizeof(Double) * NumPoints);
            while ((F.Position + 1) < F.Size) and (i < NumPoints) do
            begin
                Inc(i);

                if F.Read(pA[i], SizeOf(Double)) <> SizeOf(Double) then 
                    Break;

                if F.Read(pB[i], SizeOf(Double)) <> SizeOf(Double) then 
                    Break;
            end;
        end
        else
        begin
            while ((F.Position + 1) < F.Size) and (i < NumPoints) do
            begin
                Inc(i);

                if F.Read(pB[i], SizeOf(Double)) <> SizeOf(Double) then 
                    Break;
            end;
        end;

        FreeAndNil(F);
        if i <> NumPoints then
            NumPoints := i;

        if RoundA then
            for i := 1 to NumPoints do
                pA[i] := Round(pA[i]);

    except
        DoSimpleMsg(DSS, 'Error Processing binary (double) %s File: "%s"', [ClassName, FileName], 616);
        FreeAndNil(F);
        Exit;
    end;
end;

procedure DoCSVFile(DSS: TDSSContext; var pA, pB: PDoubleArray; var NumPoints: Integer; OnlyLoadB: Boolean; const FileName: String; const ClassName: String; RoundA: Boolean);
var
    F: TStream = nil;
    i: Integer;
    s: String;
begin
    try
        F := DSS.GetROFileStream(FileName);
    except
        DoSimpleMsg(DSS, 'Error opening file: "%s"', [FileName], 58613);
        FreeAndNil(F);
        Exit;
    end;
    try
        ReAllocmem(pB, Sizeof(Double) * NumPoints);
        i := 0;
        if not OnlyLoadB then
        begin
            ReAllocmem(pA, Sizeof(Double) * NumPoints);
            while ((F.Position + 1) < F.Size) and (i < NumPoints) do
            begin
                Inc(i);
                FSReadln(F, s); // read entire line and parse with AuxParser
                // AuxParser allows commas or white space
                with DSS.AuxParser do
                begin
                    CmdString := s;
                    NextParam;
                    pA[i] := DblValue;
                    NextParam;
                    pB[i] := DblValue;
                end;
            end;
        end
        else
        begin
            while ((F.Position + 1) < F.Size) and (i < NumPoints) do
            begin
                Inc(i);
                FSReadln(F, s); // read entire line and parse with AuxParser
                // AuxParser allows commas or white space
                with DSS.AuxParser do
                begin
                    CmdString := s;
                    NextParam;
                    pB[i] := DblValue;
                end;
            end;
        end;

        FreeAndNil(F);
        NumPoints := i;

    except
        On E: Exception do
        begin
            DoSimpleMsg(DSS, 'Error Processing CSV File: "%s". %s', [FileName, E.Message], 58614);
            FreeAndNil(F);
            Exit;
        end;
    end;
end;


// Routine created to empty a recently created folder
{$IFDEF MSWINDOWS}
procedure DelFilesFromDir(Directory, FileMask: string; DelSubDirs: Boolean);
var
  SourceLst: string;
  FOS: TSHFileOpStruct;
begin
  FillChar(FOS, SizeOf(FOS), 0);
  FOS.wFunc := FO_DELETE;
  SourceLst := Directory + PathDelim + FileMask + #0;
  FOS.pFrom := PChar(SourceLst);
  if not DelSubDirs then
    FOS.fFlags := FOS.fFlags OR FOF_FILESONLY;
  // Remove the next line if you want a confirmation dialog box
  FOS.fFlags := FOS.fFlags OR FOF_NOCONFIRMATION;
  // Add the next line for a "silent operation" (no progress box)
  FOS.fFlags := FOS.fFlags OR FOF_SILENT;
  SHFileOperation(FOS);
end;
{$ENDIF}
{$IFDEF UNIX}
procedure DeltreeDir(Directory: String);
var
    Info: TSearchRec;
begin
    if FindFirst(Directory + PathDelim + '*', faAnyFile and faDirectory, Info) = 0 then
    begin
        repeat
            with Info do
            begin
                if (name = '.') or (name = '..') then
                    continue;
                if (Attr and faDirectory) = faDirectory then
                begin
                    DeltreeDir(Directory + PathDelim + Name)
                end
                else
                begin
                    DeleteFile(Directory + PathDelim + Name);
                end;
            end;
        until FindNext(info) <> 0;
    end;
    rmdir(Directory);
end;

procedure DelFilesFromDir(Directory, FileMask: String; DelSubDirs: Boolean);
var
    Info: TSearchRec;
    flags: Longint;
begin
    if DelSubDirs then
        flags := faAnyFile and faDirectory
    else
        flags := faAnyFile;

    if FindFirst(Directory + PathDelim + FileMask, flags, Info) = 0 then
    begin
        repeat
            with Info do
            begin
                if (name = '.') or (name = '..') then
                    continue;
                if (Attr and faDirectory) = faDirectory then
                begin
                    try
                        DeltreeDir(Directory + PathDelim + Name)
                    except
                        DSSMessageDlg('Could not remove directory ' + Directory + PathDelim + Name, True);
                    end;
                end
                else
                begin
                    DeleteFile(Directory + PathDelim + Name);
                end;
            end;
        until FindNext(info) <> 0;
    end;
end;{$ENDIF}

end.
