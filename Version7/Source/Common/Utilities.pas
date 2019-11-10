unit Utilities;

{
  ----------------------------------------------------------
  Copyright (c) 2008-2015, Electric Power Research Institute, Inc.
  All rights reserved.
  ----------------------------------------------------------
}


{
  12-18-2002 RCD Converted Eventlog to in-memory rather than file
}

interface

uses
    ArrayDef,
    CktElement,
    PDElement,
    UComplex,
    UcMatrix,
    DSSClass,
    Dynamics,
    Classes{, StdCtrls};

function CompareTextShortest(const S1, S2: String): Integer;
procedure FireOffEditor(DSS: TDSS; FileNm: String);
procedure DoDOSCmd(DSS: TDSS; CmdString: String);
function StripExtension(const S: String): String;
function StripClassName(const S: String): String;  // Return only element name sans class.
function GetNodeString(const Busname: String): String;
function Pad(const S: String; Width: Integer): String;
function PadDots(const S: String; Width: Integer): String;
function PadTrunc(const S: String; Width: Integer): String;
function IntArrayToString(iarray: pIntegerArray; count: Integer): String;
function DblArrayToString(dblarray: pDoubleArray; count: Integer): String;
function CmplxArrayToString(cpxarray: pComplexArray; count: Integer): String;
function EncloseQuotes(const s: String): String;
procedure ShowMessageBeep(DSS: TDSS; const s: String);
function FullName(pElem: TDSSCktElement): String;

{Parsing Utilities}
procedure ParseObjectClassandName(DSS: TDSS; const FullObjName: String; var ClassName, ObjName: String);
procedure ParseIntArray(DSS: TDSS; var iarray: pIntegerArray; var count: Integer; const s: String);
function InterpretSolveMode(const s: String): TSolveMode;
function InterpretControlMode(const s: String): Integer;
function InterpretLoadModel(DSS: TDSS; const s: String): Integer;
function InterpretYesNo(const s: String): Boolean;
function InterpretRandom(const s: String): Integer;
function InterpretAddType(const s: String): Integer;
function InterpretConnection(const s: String): Integer;
function InterpretSolveAlg(const s: String): Integer;
function InterpretCktModel(const s: String): Boolean;
procedure InitDblArray(NumValues: Integer; Xarray: pDoubleArray; Value: Double);
procedure InitIntArray(NumValues: Integer; Xarray: pIntegerArray; Value: Integer);
function InterpretDblArray(DSS: TDSS; const s: String; MaxValues: Integer; ResultArray: pDoubleArray): Integer;
function InterpretIntArray(DSS: TDSS; const s: String; MaxValues: Integer; ResultArray: pIntegerArray): Integer;
procedure InterpretAndAllocStrArray(DSS: TDSS; const s: String; var Size: Integer; var ResultArray: pStringArray);
procedure InterpretTStringListArray(DSS: TDSS; const s: String; var ResultList: TStringList);
function InterpretTimeStepSize(DSS: TDSS; const s: String): Double;
function InterpretLoadShapeClass(const s: String): Integer;
function InterpretEarthModel(const s: String): Integer;
function InterpretColorName(DSS: TDSS; const s: String): Integer;
function InterpretComplex(DSS: TDSS; const s: String): Complex;
function ConstructElemName(DSS: TDSS; const Param: String): String;
function InterpretCoreType(const str: String): Integer;

function GetSolutionModeID(DSS: TDSS): String;
function GetSolutionModeIDName(idx: TSolveMode): String;
function GetControlModeID(DSS: TDSS): String;
function GetRandomModeID(DSS: TDSS): String;
function GetLoadModel(DSS: TDSS): String;
function GetActiveLoadShapeClass(DSS: TDSS): String;
function GetDSSArray_Real(n: Integer; dbls: pDoubleArray): String;
function GetDSSArray_Integer(n: Integer; ints: pIntegerArray): String;
function GetEarthModel(n: Integer): String;
function GetOCPDeviceType(pElem: TDSSCktElement): Integer;
function GetOCPDeviceTypeString(icode: Integer): String;


{misc functions}
function DoExecutiveCommand(DSS: TDSS; const s: String): Integer;
function GetCktElementIndex(DSS: TDSS; const FullObjName: String): Integer;
function IsShuntElement(const Elem: TDSSCktElement): Boolean;
function IsLineElement(const Elem: TDSSCktElement): Boolean;
function IsTransformerElement(const Elem: TDSSCktElement): Boolean;
// --- moved to ReduceAlgs 2/13/19 Function  IsStubLine(const Elem:TDSSCktElement):Boolean;
function CheckParallel(const Line1, Line2: TDSSCktElement): Boolean;
function AllTerminalsClosed(ThisElement: TDSSCktElement): Boolean;
function Str_Real(const Value: Double; NumDecimals: Integer): String;
procedure DumpAllDSSCommands(DSS: TDSS; var Filename: String);
procedure DumpAllocationFactors(DSS: TDSS; var Filename: String);
procedure DumpComplexMatrix(DSS: TDSS; var F: TextFile; AMatrix: TcMatrix);
function NearestBasekV(DSS: TDSS; kV: Double): Double;
function PresentTimeInSec(DSS: TDSS): Double;
function DoResetFaults(DSS: TDSS): Integer;
function DoResetControls(DSS: TDSS): Integer;
procedure DoResetKeepList(DSS: TDSS);
function GetNodeNum(DSS: TDSS; NodeRef: Integer): Integer;
procedure InitStringToNull(var S: String);
function CmulReal_im(const a: Complex; const Mult: Double): Complex;  // Multiply only imaginary part by a real
//FUNCTION IsValidNumericField(const NumberField:TEdit):Boolean;
function MaxdblArrayValue(npts: Integer; dbls: pDoubleArray): Double;
function iMaxAbsdblArrayValue(npts: Integer; dbls: pDoubleArray): Integer;
function QuadSolver(const a, b, c: Double): Double; // returns largest of two answers


{Save Function Helper}
function WriteClassFile(DSS: TDSS; const DSS_Class: TDSSClass; FileName: String; IsCktElement: Boolean): Boolean;
function WriteVsourceClassFile(DSS: TDSS; const DSS_Class: TDSSClass; IsCktElement: Boolean): Boolean;
procedure WriteActiveDSSObject(DSS: TDSS; var F: TextFile; const NeworEdit: String);
function checkforblanks(const S: String): String;
function RewriteAlignedFile(DSS: TDSS; const Filename: String): Boolean;

{Event Log}
procedure ClearEventLog(DSS: TDSS);
procedure AppendToEventLog(DSS: TDSS; const opdev: String; const action: String);
procedure LogThisEvent(DSS: TDSS; const EventName: String);

procedure ClearErrorLog(DSS: TDSS);

{Routines for doing common things to complex numbers}
procedure RotatePhasorDeg(var Phasor: Complex; const h, AngleDeg: Double);
procedure RotatePhasorRad(var Phasor: Complex; const h, AngleRad: Double);
procedure ConvertComplexArrayToPolar(const Buffer: pComplexArray; N: Integer);
procedure ConvertComplexArrayToPowerandPF(const Buffer: pComplexArray; N: Integer);
function Residual(p: Pointer; Nph: Integer): Complex;
function ResidualPolar(p: Pointer; Nph: Integer): Complex;
function Powerfactor(const S: Complex): Double;
function ConvertPFToPFRange2(const value: Double): Double;
function ConvertPFRange2ToPF(const value: Double): Double;
procedure CmulArray(pc: pcomplexarray; Multiplier: Double; size: Integer);  // Multiply a complex array times a double

{Support for going in and out of Dynamics Mode and Harmonics Mode}
procedure CalcInitialMachineStates(DSS: TDSS);
procedure InvalidateAllPCELEMENTS(DSS: TDSS);
function InitializeForHarmonics(DSS: TDSS): Boolean;
function SavePresentVoltages(DSS: TDSS): Boolean;
function RetrieveSavedVoltages(DSS: TDSS): Boolean;

function GetMaxPUVoltage(DSS: TDSS): Double;
function GetMinPUVoltage(DSS: TDSS; IgnoreNeutrals: Boolean): Double;
function GetTotalPowerFromSources(DSS: TDSS): Complex;
function GetMaxCktElementSize(DSS: TDSS): Integer;
function GetUniqueNodeNumber(DSS: TDSS; const sBusName: String; StartNode: Integer): Integer;

{TraceBack Functions}
function IsPathBetween(FromLine, ToLine: TPDElement): Boolean;
procedure TraceAndEdit(DSS: TDSS; FromLine, ToLine: TPDElement; NPhases: Integer; EditStr: String);
procedure GoForwardAndRephase(DSS: TDSS; FromLine: TPDElement; const PhaseString, EditStr, ScriptFileName: String; TransStop: Boolean);

procedure MakeDistributedGenerators(DSS: TDSS; kW, PF: Double; How: String; Skip: Integer; Fname: String; DoGenerators: Boolean);

procedure Obfuscate(DSS: TDSS);


{Feeder Utilities} // not currently used
// procedure EnableFeeders;
// procedure DisableFeeders;
// procedure InitializeFeeders;
// procedure ForwardSweepAllFeeders;
// procedure BackwardSweepAllFeeders;


implementation

uses
{$IFDEF FPC}
    Process,
    CmdForms,
{$ELSE}
    Windows,
    ShellAPI,
    Dialogs,
    Graphics,
    DSSForms,
{$ENDIF}
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
    Feeder,
    HashList,
    EnergyMeter,
    PCElement,
    ControlElem,
    DSSHelper;

const
    ZERONULL: Integer = 0;
    padString: String = '                                                  '; //50 blanks
    paddotsString: String = ' .................................................'; //50 dots


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

// = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = =
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
// = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = =
function PadTrunc(const S: String; Width: Integer): String;
// Pad out a string with blanks to Width characters or truncate to Width Chars
begin
    Result := Copy(Pad(S, Width), 1, Width);
end;

// = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = =
function FullName(pElem: TDSSCktElement): String;
begin
    Result := EncloseQuotes(pElem.DSSClassName + '.' + UpperCase(pElem.Name));
end;

// = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = =
function StripExtension(const S: String): String;

{Strips off everything up to a period.}

var
    dotpos: Integer;

begin
    dotpos := pos('.', S) - 1;
    if dotpos = (-1) then
        dotpos := Length(S);
    Result := Copy(S, 1, dotpos);
end;

// = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = =
function StripClassName(const S: String): String;
{Returns everything past the first period}

var
    dotpos: Integer;

begin
    dotpos := pos('.', S);
    Result := Copy(S, dotpos + 1, Length(S));
end;

{$IFDEF FPC}
procedure FireOffEditor(DSS: TDSS; FileNm: String);
var
    s: String;
    gotError: Boolean;
    msg: String;
begin
{$IFDEF DSS_CAPI}
    if not DSS_CAPI_ALLOW_EDITOR then
        Exit; // just ignore if Show is not allowed
{$ENDIF}

    gotError := FALSE;
    msg := 'Unknown error in process.';
    try
        if FileExists(FileNm) then
        begin
{$IF (defined(Windows) or defined(MSWindows))}
            gotError := not RunCommand(DefaultEditor, [FileNm], s);
{$ELSE}
            gotError := not RunCommand('/bin/bash', ['-c', DefaultEditor + ' ' + FileNm], s);
{$ENDIF}
        end;
    except
        On E: Exception do
        begin
            gotError := TRUE;
            msg := E.Message;
        end;
    end;
    if gotError then
        DoErrorMsg(DSS, 'FireOffEditor.', msg, 'Editor could not be started. Is the editor correctly specified?', 704);
end;

procedure DoDOSCmd(DSS: TDSS; CmdString: String);
var //Handle:Word;
    s: String;
    gotError: Boolean;
    msg: String;
begin
    gotError := FALSE;
    msg := 'Unknown error in command.';
    try
{$IF (defined(Windows) or defined(MSWindows))}
        gotError := not RunCommand('cmd', ['/c', CmdString], s);
{$ELSE}
        gotError := not RunCommand('/bin/bash', ['-c', CmdString], s);
{$ENDIF}
    except
        On E: Exception do
        begin
            gotError := TRUE;
            msg := E.Message;
        end;
    end;
    if gotError then
        DoSimpleMsg(DSS, Format('DoDOSCmd Error:%s. Error in Command "%s"', [msg, CmdString]), 704);
end;

{$ELSE}
// = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = =
procedure FireOffEditor(DSS: TDSS; FileNm: String);
var
    retval: Word;
begin
    try
        if FileExists(FileNm) then
        begin
            retval := ShellExecute(0, NIL, Pchar(encloseQuotes(DefaultEditor)), Pchar(encloseQuotes(FileNm)), NIL, SW_SHOW);
            SetLastResultFile(DSS, FileNm);

            case Retval of
                0:
                    DoSimpleMsg(DSS, 'System out of memory. Cannot start Editor.', 700);
                ERROR_BAD_FORMAT:
                    DoSimpleMsg(DSS, 'Editor File is Invalid.', 701);
                ERROR_FILE_NOT_FOUND:
                    DoSimpleMsg(DSS, 'Editor "' + DefaultEditor + '"  Not Found.' + CRLF + 'Did you set complete path name?', 702);
                ERROR_PATH_NOT_FOUND:
                    DoSimpleMsg(DSS, 'Path for Editor "' + DefaultEditor + '" Not Found.', 703);
            end;
        end;
    except
        On E: Exception do
            DoErrorMsg(DSS, 'FireOffEditor.', E.Message,
                'Default Editor correctly specified???', 704);
    end;
end;


// = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = =
procedure DoDOSCmd(CmdString: String);
var
    Handle: Word;
begin
    try
        Handle := 0;
        ShellExecute(Handle, 'open', Pchar('cmd.exe'), Pchar(CmdString), NIL, SW_SHOW);

    except
        On E: Exception do
            DoSimpleMsg(DSS, Format('DoDOSCmd Error:%s. Error in Command "%s"', [E.Message, CmdString]), 704);
    end;
end;

{$ENDIF}

// = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = =
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

// = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = =
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

// = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = =
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


function EncloseQuotes(const s: String): String;
begin
    Result := '"' + s + '"';
end;


//----------------------------------------------------------------------------
function InterpretSolveMode(const s: String): TSolveMode;

// interpret solution mode
// could be "nominal" "daily"  "yearly" "montecarlo" "dutycycle"  "loadduration" "peakdays" , etc.

var
    SLC: String;
begin
    SLC := lowercase(s);

    case SLC[1] of
        's':
            Result := TSolveMode.SNAPSHOT;
        'd':
            case SLC[2] of
                'u':
                    Result := TSolveMode.DUTYCYCLE;
                'i':
                    Result := TSolveMode.DIRECT;
                'y':
                    Result := TSolveMode.DYNAMICMODE;
            else
                Result := TSolveMode.DAILYMODE;
            end;
        'f':
            Result := TSolveMode.FAULTSTUDY;
        'h':
            case SLC[9] of                   //Modification added by Davis Montenegro 25/06/2014
                't':
                    Result := TSolveMode.HARMONICMODET;     // For adding the harmoncis mode in time domain
            else
                Result := TSolveMode.HARMONICMODE;
            end;
        'y':
            Result := TSolveMode.YEARLYMODE;
        'm':
            case SLC[2] of
                '1':
                    Result := TSolveMode.MONTECARLO1;
                '2':
                    Result := TSolveMode.MONTECARLO2;
                '3':
                    Result := TSolveMode.MONTECARLO3;
                'f':
                    Result := TSolveMode.MONTEFAULT;
            else
                Result := TSolveMode.MONTECARLO1;
            end;
        'p':
            Result := TSolveMode.PEAKDAY;
        'a':
            Result := TSolveMode.AUTOADDFLAG;
        'l':
            case SLC[2] of
                'd':
                    case SLC[3] of
                        '1':
                            Result := TSolveMode.LOADDURATION1;
                        '2':
                            Result := TSolveMode.LOADDURATION2;
                    else
                        Result := TSolveMode.LOADDURATION1;
                    end;
            else
                Result := TSolveMode.LOADDURATION1;
            end;
        't':
            Result := TSolveMode.GENERALTIME;

    else
        Result := TSolveMode.SNAPSHOT;
    end;


end;

//----------------------------------------------------------------------------
function InterpretControlMode(const s: String): Integer;

// interpret solution control mode

var
    SLC: String;
begin
    SLC := lowercase(s);

    case SLC[1] of
        'o':
            Result := CONTROLSOFF;
        'e':
            Result := EVENTDRIVEN;    // "event"
        't':
            Result := TIMEDRIVEN;     // "time"
        'm':
            Result := MULTIRATE;     // "MultiRate"
    else
        Result := CTRLSTATIC;
    end;


end;
//----------------------------------------------------------------------------
function InterpretLoadModel(DSS: TDSS; const s: String): Integer;
var
    S2: String;
begin
    S2 := LowerCase(S);
    case S2[1] of
        'a':
            Result := ADMITTANCE;
        'p':
            Result := POWERFLOW;
    else
        Result := ADMITTANCE;
    end;
{ If this represents a change, invalidate all the PC Yprims}
    if Result <> DSS.ActiveCircuit.Solution.LoadModel then
        DSS.ActiveCircuit.InvalidateAllPCElements;

end;

//----------------------------------------------------------------------------
function InterpretYesNo(const s: String): Boolean;

//' Interpret Yes / no properties  - can also be True/False
var
    S2: Char;
begin
    S2 := LowerCase(S)[1];
    case S2 of
        'y', 't':
            Result := TRUE;
        'n', 'f':
            Result := FALSE;
    else
        Result := FALSE;
    end;

end;

//----------------------------------------------------------------------------
function InterpretRandom(const s: String): Integer;

// interpret the type of random variation in the load
// none|gaussian|uniform |lognormal

var
    SLC: String;
begin
    SLC := lowercase(s);

    case SLC[1] of
        'g':
            Result := GAUSSIAN;  //gaussian
        'u':
            Result := UNIFORM;  //uniform
        'l':
            Result := LOGNORMAL; // Log-Normal
    else
        Result := 0;  // no variation for any other entry
    end

end;


//----------------------------------------------------------------------------
function InterpretAddType(const s: String): Integer;
// type of device to automatically add. Default is capacitor

var
    SLC: String;
begin
    SLC := lowercase(s);

    case SLC[1] of
        'g':
            Result := GENADD;
    else
        Result := CAPADD;
    end

end;

//----------------------------------------------------------------------------
function InterpretConnection(const s: String): Integer;
{ Accepts  (Case insensitive)
    delta or LL    Result=1       
    Y, wye, or LN  Result=0
}
begin
    Result := 0;
    case lowercase(S)[1] of
        'y', 'w':
            Result := 0;  {Wye}
        'd':
            Result := 1;  {Delta or line-Line}
        'l':
            case lowercase(s)[2] of
                'n':
                    Result := 0;
                'l':
                    Result := 1;
            end;
    end;
end;

//----------------------------------------------------------------------------
function InterpretSolveAlg(const s: String): Integer;

var
    SLC: String;

begin
    SLC := copy(lowercase(s), 1, 2);

    if CompareText(SLC, 'ne') = 0 then
        Result := NEWTONSOLVE
    else
        Result := NORMALSOLVE;

end;


//----------------------------------------------------------------------------
function InterpretCktModel(const s: String): Boolean;

{Returns True if Positive Sequence}

begin

    case s[1] of
        'p', 'P':
            Result := TRUE
    else
        Result := FALSE
    end;

end;

//----------------------------------------------------------------------------
function InterpretComplex(DSS: TDSS; const s: String): Complex;

// interpret first two entries as complex numbers

var
    ParmName: String;

begin
    DSS.AuxParser.CmdString := S;
    ParmName := DSS.AuxParser.NextParam;
    Result.re := DSS.AuxParser.dblvalue;
    ParmName := DSS.AuxParser.NextParam;
    Result.im := DSS.AuxParser.dblvalue;
end;

//----------------------------------------------------------------------------

procedure InitDblArray(NumValues: Integer; Xarray: pDoubleArray; Value: Double);
var
    i: Integer;
{Set all elements of a double array}
begin
    for i := 1 to NumValues do
        Xarray^[i] := Value;
end;

//----------------------------------------------------------------------------

procedure InitIntArray(NumValues: Integer; Xarray: pIntegerArray; Value: Integer);
var
    i: Integer;
{Set all elements of a Integer array}
begin
    for i := 1 to NumValues do
        Xarray^[i] := Value;
end;

//----------------------------------------------------------------------------
function InterpretDblArray(DSS: TDSS; const s: String; MaxValues: Integer; ResultArray: pDoubleArray): Integer;

{ Get numeric values from an array specified either as a list on numbers or a text file spec.
  ResultArray must be allocated to MaxValues by calling routine.

  9/7/2011 Modified to allow multi-column CSV files and result file

  CSV File my have one value per line or multiple columns and a header row.
  Example:
          ... mult=[file = myfilename, column=2, header=yes]
          ... mult=[file = %result%, column=2, header=yes]    // last result file

          file= must be first
          the %result% variable implies the last result file

          or

          Use the Array=@lastfile variable syntax and the parser will automativally replace with last file name

}

var
    ParmName,
    Param: String;
    F: Textfile;
    MyStream: TMemoryStream;
    i: Integer;
    Temp: Single;
    CSVFileName: String;
    CSVColumn: Integer;
    CSVHeader: Boolean;
    InputLIne: String;
    iskip: Integer;

begin

    DSS.AuxParser.CmdString := S;
    ParmName := DSS.AuxParser.NextParam;
    Param := DSS.AuxParser.StrValue;
    Result := MaxValues; // Default Return Value;

     {Syntax can be either a list of numeric values or a file specification:  File= ...}

    if CompareText(Parmname, 'file') = 0 then
    begin
         {Default values}
        if compareText(param, '%result%') = 0 then
            CSVFileName := DSS.LastResultFile
        else
            CSVFileName := Param;
        if not FileExists(CSVFileName) then
        begin
            DoSimpleMsg(DSS, Format('CSV file "%s" does not exist', [CSVFileName]), 70401);
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
            AssignFile(F, CSVFileName);
            Reset(F);

            if CSVHeader then
                Readln(F, InputLIne);  // skip the header row

            for i := 1 to MaxValues do
            begin

                try
                    if not EOF(F) then
                    begin
                        Readln(F, InputLIne);
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
                        DoSimpleMsg(DSS, Format('Error reading %d-th numeric array value from file: "%s" Error is:', [i, Param, E.message]), 705);
                        Result := i - 1;
                        Break;
                    end;
                end;

            end;

        finally

            CloseFile(F);

        end;
    end

    else
    if (Length(Parmname) > 0) and (CompareTextShortest(Parmname, 'dblfile') = 0) then
    begin
         // load the list from a file of doubles (no checking done on type of data)
        MyStream := TMemoryStream.Create;

        if FileExists(Param) then
        begin
            MyStream.LoadFromFile(Param);
            // Now move the doubles from the file into the destination array
            Result := Min(Maxvalues, MyStream.Size div sizeof(ResultArray^[1]));  // no. of doubles
            MyStream.ReadBuffer(ResultArray^[1], SizeOf(ResultArray^[1]) * Result);
        end
        else
            DoSimpleMsg(DSS, Format('File of doubles "%s" not found.', [Param]), 70501);
        MyStream.Free;
    end

    else
    if (Length(Parmname) > 0) and (CompareTextShortest(Parmname, 'sngfile') = 0) then
    begin
         // load the list from a file of singles (no checking done on type of data)
        MyStream := TMemoryStream.Create;

        if FileExists(Param) then
        begin
            MyStream.LoadFromFile(Param);
            // Now move the singles from the file into the destination array
            Result := Min(Maxvalues, MyStream.Size div sizeof(Single));  // no. of singles
            for i := 1 to Result do
            begin
                MyStream.Read(Temp, Sizeof(Single));
                ResultArray^[i] := Temp;  // Single to Double
            end;
        end
        else
            DoSimpleMsg(DSS, Format('File of Singles "%s" not found.', [Param]), 70502);
        MyStream.Free;

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

function InterpretIntArray(DSS: TDSS; const s: String; MaxValues: Integer; ResultArray: pIntegerArray): Integer;

{ Get numeric values from an array specified either as a list on numbers or a text file spec.
  ResultArray must be allocated to MaxValues by calling routine.
  File is assumed to have one value per line.}

var
    ParmName,
    Param: String;
    F: Textfile;
    i: Integer;

begin
    DSS.AuxParser.CmdString := S;
    ParmName := DSS.AuxParser.NextParam;
    Param := DSS.AuxParser.StrValue;
    Result := Maxvalues;  // Default return value

     {Syntax can be either a list of numeric values or a file specification:  File= ...}

    if CompareText(Parmname, 'file') = 0 then
    begin
         // load the list from a file
        try
            AssignFile(F, Param);
            Reset(F);
            for i := 1 to MaxValues do
            begin
                if not EOF(F) then
                    Readln(F, ResultArray^[i])
                else
                begin
                    Result := i - 1;
                    Break;
                end;
            end;
            CloseFile(F);

        except
            On E: Exception do
                DoSimpleMsg(DSS, 'Error trying to read numeric array values from file: "' + Param + '"  Error is: ' + E.message, 706);
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

function InterpretTimeStepSize(DSS: TDSS; const s: String): Double;
{Return stepsize in seconds}
var
    Code: Integer;
    ch: Char;
    s2: String;

begin
     {Try to convert and see if we get an error}
    val(s, Result, Code);
    if Code = 0 then
        Exit;  // Only a number was specified, so must be seconds

     {Error occurred so must have a units specifier}
    ch := s[Length(s)];  // get last character
    s2 := copy(s, 1, Length(s) - 1);
    Val(S2, Result, Code);
    if Code > 0 then
    begin   {check for error}
        Result := DSS.ActiveCircuit.solution.DynaVars.h; // Don't change it
        DoSimpleMsg(DSS, 'Error in specification of StepSize: ' + s, 99933);
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
        DoSimpleMsg(DSS, 'Error in specification of StepSize: "' + s + '" Units can only be h, m, or s (single char only) ', 99934);
    end;

end;


//----------------------------------------------------------------------------
procedure InitStringToNull(var S: String);

begin
    Move(ZeroNull, S, 4);
end;

//----------------------------------------------------------------------------
procedure InterpretAndAllocStrArray(DSS: TDSS; const s: String; var Size: Integer; var ResultArray: pStringArray);

{ Get string values from an array specified either as a list on strings or a text file spec.
  ResultArray is allocated as needed.
  File is assumed to have one value per line.}

var
    ParmName,
    Param: String;
    F: Textfile;
    MaxSize: Integer;


    procedure ReallocStringArray;
    var
        j: Integer;
    begin
        Reallocmem(ResultArray, Sizeof(ResultArray^[1]) * MaxSize);
        for j := Size + 1 to MaxSize do
            InitStringToNull(ResultArray^[j]);    // Init string values
    end;

    procedure BumpUpStringArray;
    begin
        Inc(MaxSize, 100);
        ReallocStringArray;
    end;

    procedure FreeStringArray;
    var
        j: Integer;
    begin
        if Assigned(ResultArray) then
        begin
            for j := 1 to Size do
            begin
                ResultArray^[j] := '';
            end;
            ReallocMem(ResultArray, 0);
        end;
    end;

begin

     //  Throw Away any Previous Allocation
    FreeStringArray;

     // Now Reallocate
    MaxSize := 100;  // initialize
    Size := 0;
    ReAllocStringArray;

    DSS.AuxParser.CmdString := S;
    ParmName := DSS.AuxParser.NextParam;
    Param := DSS.AuxParser.StrValue;

     {Syntax can be either a list of string values or a file specification:  File= ...}

    if CompareText(Parmname, 'file') = 0 then
    begin
         // load the list from a file

        try
            AssignFile(F, Param);
            Reset(F);
            while not EOF(F) do
            begin
                Readln(F, Param);
                if Param <> '' then
                begin     // Ignore Blank Lines in File
                    Inc(Size);
                    if Size > Maxsize then
                        BumpUpStringArray;
                    ResultArray^[Size] := Param;
                end;
            end;
            CloseFile(F);

        except
            On E: Exception do
                DoSimpleMsg(DSS, 'Error trying to read numeric array values from a file. Error is: ' + E.message, 707);
        end;


    end
    else
    begin  // Parse list of values off input string

         // Parse Values of array list
        while Param <> '' do
        begin
            Inc(Size);
            if Size > Maxsize then
                BumpUpStringArray;
            ResultArray^[Size] := Param;
            ParmName := DSS.AuxParser.NextParam;
            Param := DSS.AuxParser.StrValue;
        end;
    end;

    MaxSize := Size;   // Get rid of Excess Allocation
    ReallocStringArray;

end;

//----------------------------------------------------------------------------
procedure InterpretTStringListArray(DSS: TDSS; const s: String; var ResultList: TStringList);

{ Get string values from an array specified either as a list on strings or a text file spec.
  ResultArray is allocated as needed.
  File is assumed to have one value per line.}

var
    ParmName,
    Param,
    NextParam: String;
    F: Textfile;


begin

     //  Throw Away any Previous Allocation
    ResultList.Clear;


    DSS.AuxParser.CmdString := S;
    ParmName := DSS.AuxParser.NextParam;
    Param := DSS.AuxParser.StrValue;

     {Syntax can be either a list of string values or a file specification:  File= ...}

    if CompareText(Parmname, 'file') = 0 then
    begin
         // load the list from a file

        try
            AssignFile(F, Param);
            Reset(F);
            while not EOF(F) do
            begin
                Readln(F, Param);
                DSS.AuxParser.CmdString := Param;
                ParmName := DSS.AuxParser.NextParam;
                NextParam := DSS.AuxParser.StrValue;
                if Length(NextParam) > 0 then
                begin     // Ignore Blank Lines in File
                    ResultList.Add(NextParam);
                end;
            end;
            CloseFile(F);

        except
            On E: Exception do
                DoSimpleMsg(DSS, 'Error trying to read numeric array values from a file. Error is: ' + E.message, 708);
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

function InterpretCoreType(const str: String): Integer;
begin
    case str[1] of
        '1':
            Result := 1;  // 1-phase
        '3':
            Result := 3;  // 3-Leg
        '5':
            Result := 5;  // 5-Leg
    else
        Result := 0; // default to shell
    end;
end;

//----------------------------------------------------------------------------
procedure ParseObjectClassandName(DSS: TDSS; const FullObjName: String; var ClassName, ObjName: String);

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

function GetSolutionModeIDName(idx: TSolveMode): String;
begin

    case idx of

        TSolveMode.SNAPSHOT:
            Result := 'Snap';
        TSolveMode.DAILYMODE:
            Result := 'Daily';
        TSolveMode.YEARLYMODE:
            Result := 'Yearly';
        TSolveMode.MONTECARLO1:
            Result := 'M1';
        TSolveMode.MONTECARLO2:
            Result := 'M2';
        TSolveMode.MONTECARLO3:
            Result := 'M3';
        TSolveMode.LOADDURATION1:
            Result := 'LD1';
        TSolveMode.LOADDURATION2:
            Result := 'LD2';
        TSolveMode.PEAKDAY:
            Result := 'Peakday';
        TSolveMode.DUTYCYCLE:
            Result := 'DUtycycle';
        TSolveMode.DIRECT:
            Result := 'DIrect';
        TSolveMode.DYNAMICMODE:
            Result := 'DYnamic';
        TSolveMode.MONTEFAULT:
            Result := 'MF';
        TSolveMode.FAULTSTUDY:
            Result := 'Faultstudy';
        TSolveMode.AUTOADDFLAG:
            Result := 'Autoadd';
        TSolveMode.HARMONICMODE:
            Result := 'Harmonic';
        TSolveMode.HARMONICMODET:
            Result := 'HarmonicT';
        TSolveMode.GENERALTIME:
            Result := 'Time';
    else
        Result := 'UNKNOWN'
    end;

end;

function GetSolutionModeID(DSS: TDSS): String;

begin
    Result := 'UNKNOWN';
    if DSS.ActiveCircuit <> NIL then
        Result := GetSolutionModeIDName(DSS.ActiveCircuit.Solution.mode);
end;

function GetControlModeID(DSS: TDSS): String;

begin
    Result := 'Unknown';
    if DSS.ActiveCircuit <> NIL then
        case DSS.ActiveCircuit.Solution.Controlmode of
            CTRLSTATIC:
                Result := 'STATIC';
            EVENTDRIVEN:
                Result := 'EVENT';
            TIMEDRIVEN:
                Result := 'TIME';
            MULTIRATE:
                Result := 'MULTIRATE';
            CONTROLSOFF:
                Result := 'OFF';
        else
            Result := 'UNKNOWN'
        end;

end;

function GetRandomModeID(DSS: TDSS): String;

begin
    Result := 'Unknown';
    if DSS.ActiveCircuit <> NIL then
        case DSS.ActiveCircuit.Solution.RandomType of

            0:
                Result := 'None';
            GAUSSIAN:
                Result := 'Gaussian';
            UNIFORM:
                Result := 'Uniform';
            LOGNORMAL:
                Result := 'LogNormal';
        else
            Result := 'Unknown'
        end;

end;

function GetLoadModel(DSS: TDSS): String;
begin

    case DSS.ActiveCircuit.solution.LoadModel of
        ADMITTANCE:
            Result := 'Admittance';
    else
        Result := 'PowerFlow'
    end;


end;

procedure ParseIntArray(DSS: TDSS; var iarray: pIntegerArray; var count: Integer; const s: String);

var
    paramName: String;
    param: String;
    i: Integer;

begin

// Parse the line once to get the count of tokens on string, S
    DSS.AuxParser.cmdString := S;
    Count := 0;
    repeat
        ParamName := DSS.AuxParser.NextParam;
        Param := DSS.AuxParser.StrValue;
        if Length(Param) > 0 then
            Inc(Count);
    until Length(Param) = 0;

//  reallocate iarray  to new size
    ReallocMem(iarray, sizeof(iarray^[1]) * count);

// Parse again for real
    DSS.AuxParser.cmdString := S;
    for i := 1 to Count do
    begin
        ParamName := DSS.AuxParser.NextParam;
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

    if ((Elem.DSSObjType and CLASSMASK) = LINE_ELEMENT) then
        Result := TRUE
    else
        Result := FALSE;

end;


function IsTransformerElement(const Elem: TDSSCktElement): Boolean;
begin

    if ((Elem.DSSObjType and CLASSMASK) = XFMR_ELEMENT) then
        Result := TRUE
    else
        Result := FALSE;

end;


//----------------------------------------------------------------------------
function GetCktElementIndex(DSS: TDSS; const FullObjName: String): Integer;

// Given the full object name, return the index to the circuit element in the
// active circuit.  Use full name if given. Else assume last class referenced.

var
    DevClassIndex, DevIndex: Integer;
    DevClassName, DevName: String;

begin
    Result := 0; // Default return value
    ParseObjectClassandName(DSS, FullObjName, DevClassName, DevName);
    DevClassIndex := DSS.ClassNames.Find(DevClassName);
    if DevClassIndex = 0 then
        DevClassIndex := DSS.LastClassReferenced;

     // Since there could be devices of the same name of different classes,
     // loop until we find one of the correct class
    with DSS.ActiveCircuit do
    begin
        Devindex := DeviceList.Find(DevName);
        while DevIndex > 0 do
        begin
            if DeviceRef^[Devindex].CktElementClass = DevClassIndex then   // we got a match
            begin
                Result := DevIndex;
                Exit;
            end;
            Devindex := Devicelist.FindNext;
        end;
    end;

end;

//----------------------------------------------------------------------------
function Str_Real(const Value: Double; NumDecimals: Integer): String;
begin
    try
//         Str(Value:0:NumDecimals, Result);
        Result := FloatToStrF(Value, ffFixed, 0, NumDecimals);
    except
        Result := '*****';
    end;

end;


// - - - - - --------------------------------------------------
function ReplaceCRLF(const S: String): String;
begin
    {Replace CRLF with a \n character sequence}
    Result := StringReplace(S, CRLF, '\n', [rfReplaceAll]);
end;

// - - - - - --------------------------------------------------
procedure DumpAllocationFactors(DSS: TDSS; var FileName: String);

var
    F: TextFile;
    pLoad: TLoadObj;

begin

    try
        AssignFile(F, FileName);
        Rewrite(F);
    except
        On E: Exception do
        begin
            DoErrorMsg(DSS, 'Error opening ' + FileName + ' for writing.', E.Message, ' File protected or other file error.', 709);
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
                    Writeln(F, 'Load.' + pLoad.Name + '.AllocationFactor=', Format('%-.5g', [pLoad.kVAAllocationFactor]));
                TLoadSpec.kwh_PF:
                    Writeln(F, 'Load.' + pLoad.Name + '.CFactor=', Format('%-.5g', [pLoad.CFactor]));
            end;
            pLoad := Loads.Next;
        end; {While}
    end; {With}

    CloseFile(F);

    DSS.GlobalResult := FileName;

end;


// - - - - - --------------------------------------------------
procedure DumpAllDSSCommands(DSS: TDSS; var FileName: String);

var
    F: TextFile;
    pClass: TDSSClass;
    i: Integer;

begin

    try
        FileName := DSS.OutputDirectory + 'DSSCommandsDump.Txt';
        AssignFile(F, FileName);
        Rewrite(F);
    except
        On E: Exception do
        begin
            DoErrorMsg(DSS, 'Error opening ' + FileName + ' for writing.', E.Message, 'Disk protected or other file error', 710);
            Exit;
        end;
    end;

  // dump Executive commands
    Writeln(F, '[execcommands]');
    for i := 1 to NumExecCommands do
    begin
        Writeln(F, i: 0, ', "', Execcommand[i], '", "',
            ReplaceCRLF(CommandHelp[i]), '"');
    end;

  // Dump Executive Options
    Writeln(F, '[execoptions]');
    for i := 1 to NumExecOptions do
    begin
        Writeln(F, i: 0, ', "', ExecOption[i], '", "',
            ReplaceCRLF(OptionHelp[i]), '"');
    end;

  // Dump All presend DSSClasses
    pClass := DSS.DSSClassList.First;
    while pClass <> NIL do
    begin
        Writeln(F, '[', pClass.name, ']');
        for i := 1 to pClass.NumProperties do
        begin
            Writeln(F, i: 0, ', "', pClass.PropertyName^[i], '", "',
                ReplaceCRLF(pClass.PropertyHelp^[i]), '"');
        end;
        pClass := DSS.DSSClassList.Next;
    end;


    CloseFile(F);


end;

//----------------------------------------------------------------------------
function NearestBasekV(DSS: TDSS; kV: Double): Double;

{Find closest base voltage}

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

//----------------------------------------------------------------------------
function SavePresentVoltages(DSS: TDSS): Boolean;

var
    F: file of Double;
    i: Integer;
    dNumNodes: Double;
begin
    Result := TRUE;
    try
        Assignfile(F, DSS.OutputDirectory + DSS.CircuitName_ + 'SavedVoltages.dbl');
        Rewrite(F);

    except
        On E: Exception do
        begin
            DoSimpleMsg(DSS, 'Error opening/creating file to save voltages: ' + E.message, 711);
            Result := FALSE;
            Exit;
        end;
    end;

    try
        with DSS.ActiveCircuit, Solution do
        begin
            dNumNodes := NumNodes;
            Write(F, dNumNodes);
            for i := 1 to NumNodes do
                Write(F, NodeV^[i].re, NodeV^[i].im);
        end;

        CloseFile(F);

    except
        On E: Exception do
        begin
            DoSimpleMsg(DSS, 'Error writing file to save voltages: ' + E.message, 712);
            Result := FALSE;
        end;
    end;


end;

//----------------------------------------------------------------------------
function RetrieveSavedVoltages(DSS: TDSS): Boolean;

var
    F: file of Double;
    i: Integer;
    dNumNodes: Double;
begin

    Result := TRUE;
    try
        Assignfile(F, DSS.OutputDirectory + DSS.CircuitName_ + 'SavedVoltages.dbl');
        Reset(F);

    except
        On E: Exception do
        begin
            DoSimpleMsg(DSS, 'Error opening file to retrieve saved voltages: ' + E.message, 713);
            Result := FALSE;
            Exit;
        end;
    end;

    try
        with DSS.ActiveCircuit, Solution do
        begin
            Read(F, dNumNodes);
            if NumNodes = Round(dNumNodes) then
                for i := 1 to NumNodes do
                    Read(F, NodeV^[i].re, NodeV^[i].im)
            else
            begin
                DoSimpleMsg(DSS, 'Saved results do not match present circuit. Aborting.', 714);
                Result := FALSE;
            end;
        end;

        CloseFile(F);

    except
        On E: Exception do
        begin
            DoSimpleMsg(DSS, 'Error reading file to retrieve saved voltages: ' + E.message, 715);
            Result := FALSE;
        end;
    end;


end;

//----------------------------------------------------------------------------
function InitializeForHarmonics(DSS: TDSS): Boolean;

{Intialize PCELEMENT base values for harmonics analysis}

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
        end {With}
    else
        Result := FALSE;

end;


//----------------------------------------------------------------------------
procedure CalcInitialMachineStates(DSS: TDSS);

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

//----------------------------------------------------------------------------
procedure InvalidateAllPCELEMENTS(DSS: TDSS);

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

function PresentTimeInSec(DSS: TDSS): Double;

begin
    with DSS.ActiveCircuit.Solution do
        Result := Dynavars.t + DynaVars.intHour * 3600.0;
end;


//----------------------------------------------------------------------------
function DoResetFaults(DSS: TDSS): Integer;
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
    end;  {End With}
end;


//----------------------------------------------------------------------------
function DoResetControls(DSS: TDSS): Integer;
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
    end;  {End With}
end;

//----------------------------------------------------------------------------
function GetNodeNum(DSS: TDSS; NodeRef: Integer): Integer;
begin
    if NodeRef = 0 then
        Result := 0
    else
        Result := DSS.ActiveCircuit.MapNodeToBus^[NodeRef].NodeNum
end;


//----------------------------------------------------------------------------
procedure RotatePhasorDeg(var Phasor: Complex; const h, AngleDeg: Double);

// rotate a phasor by an angle and harmonic

begin

    Phasor := Cmul(Phasor, pdegtocomplex(1.0, h * AngleDeg));

end;

procedure RotatePhasorRad(var Phasor: Complex; const h, AngleRad: Double);

// rotate a phasor by an angle and harmonic

begin

    Phasor := Cmul(Phasor, pclx(1.0, h * AngleRad));

end;

//----------------------------------------------------------------------------
procedure ConvertComplexArrayToPowerandPF(const Buffer: pComplexArray; N: Integer);

{Creates continous PF function from 1 to 2 where 1-2 range is leading (opposite sign)}
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

{Assume we get P + jQ}

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


//----------------------------------------------------------------------------
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

//----------------------------------------------------------------------------
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
        Caccum(Result, pc^[i]);
end;

//----------------------------------------------------------------------------
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

function ConvertPFToPFRange2(const value: Double): Double;
{Convert PF from +/- 1 to 0..2 Where 1..2 is leading}
begin
    if value < 0.0 then
        Result := 2.0 + Value
    else
        Result := Value;
end;

function ConvertPFRange2ToPF(const value: Double): Double;

begin
    if value > 1.0 then
        Result := value - 2.0
    else
        Result := Value;
end;

procedure ClearEventLog;

begin
    try
{****  WriteDLLDebugFile(Format('ClearEventLog: EventStrings= %p', [@EventStrings])); }
        DSS.EventStrings.Clear;
    except
        On E: Exception do
            DoSimpleMsg(DSS, Format('Exception clearing event log: %s, @EventStrings=%p', [E.Message, @DSS.EventStrings]), 7151);
    end;
end;

procedure ClearErrorLog(DSS: TDSS);

begin
    try
{****  WriteDLLDebugFile(Format('ClearEventLog: EventStrings= %p', [@EventStrings])); }
        DSS.ErrorStrings.Clear;
    except
        On E: Exception do
            DoSimpleMsg(DSS, Format('Exception clearing error log: %s, @EventStrings=%p', [E.Message, @DSS.EventStrings]), 71511);
    end;
end;

procedure LogThisEvent(DSS: TDSS; const EventName: String);

begin
    {****  WriteDLLDebugFile(Format('LogThisEvent: EventStrings= %p', [@EventStrings])); }
    with DSS.ActiveCircuit.Solution do
        DSS.EventStrings.Add(Format('Hour=%d, Sec=%-.8g, Iteration=%d, ControlIter=%d, Event=%s',
            [DynaVars.intHour, Dynavars.t, iteration, ControlIteration, EventName]));

     //     'Time=' + TimeToStr(Time)+': '+EventName);
 {****  ShowMessageForm(EventStrings); }
end;

procedure AppendToEventLog(DSS: TDSS; const opdev: String; const action: String);
var
    S: String;

begin
  {****  WriteDLLDebugFile(Format('LogThisEvent: EventStrings= %p', [@EventStrings])); }
    with  DSS.ActiveCircuit.Solution do
        S := Format('Hour=%d, Sec=%-.5g, ControlIter=%d, Element=%s, Action=%s',
            [DynaVars.intHour, Dynavars.t, ControlIteration, OpDev, Uppercase(action)]);
    DSS.EventStrings.Add(S);
  {****  ShowMessageForm(EventStrings); }
end;


procedure DumpComplexMatrix(DSS: TDSS; var F: TextFile; AMatrix: TcMatrix);

var
    i, j: Integer;

begin
    try
        if AMatrix <> NIL then
        begin
            Writeln(F, '!(Real part)');
            with AMatrix do
            begin
                for i := 1 to Order do
                begin
                    Write(F, '! ');
                    for j := 1 to i do
                        Write(F, Format('%g ', [GetElement(i, j).re]));
                    Writeln(F);
                end;
                Writeln(F, '!(Imaginary part) = ');
                for i := 1 to Order do
                begin
                    Write(F, '! ');
                    for j := 1 to i do
                        Write(F, Format('%g ', [GetElement(i, j).im]));
                    ;
                    Writeln(F);
                end;
            end;
        end;
    except
        On E: Exception do
        begin
            DoSimpleMsg(DSS, 'Error in Dump Complex Matrix: ' + E.message + '  Write aborted.', 716);
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


function WriteVsourceClassFile(DSS: TDSS; const DSS_Class: TDSSClass; IsCktElement: Boolean): Boolean;
{Special Function to write the Vsource class and change the DSS command of the first Source
 so that there is no problem with duplication when the circuit is subsequently created}

var
    F: TextFile;
    ClassName: String;
begin
    Result := TRUE;
    if DSS_Class.ElementCount = 0 then
        Exit;
    try
        ClassName := DSS_Class.Name;
        AssignFile(F, ClassName + '.dss');
        Rewrite(F);
        DSS.SavedFileList.Add(ClassName + '.dss');
        DSS_Class.First;   // Sets DSS.ActiveDSSObject
        WriteActiveDSSObject(DSS, F, 'Edit'); // Write First Vsource out as an Edit
        while DSS_Class.Next > 0 do
        begin
       // Skip Cktelements that have been checked before and written out by
       // something else
            if TDSSCktElement(DSS.ActiveDSSObject).HasBeenSaved then
                Continue;
       // Skip disabled circuit elements; write all general DSS objects
            WriteActiveDSSObject(DSS, F, 'New');    // sets HasBeenSaved := TRUE
        end;
        CloseFile(F);
        DSS_Class.Saved := TRUE;

    except
        On E: Exception do
        begin
            DoSimpleMsg(DSS, 'WriteClassFile Error: ' + E.Message, 717);
            Result := FALSE;
        end;
    end;

end;

function WriteClassFile(DSS: TDSS; const DSS_Class: TDSSClass; FileName: String; IsCktElement: Boolean): Boolean;

var
    F: TextFile;
    ClassName: String;
    Nrecords: Integer;


begin

    Result := TRUE;

    if DSS_Class.ElementCount = 0 then
        Exit;

    try
        ClassName := DSS_Class.Name;
        if Length(FileName) = 0 then
            FileName := ClassName + '.DSS';   // default file name
        AssignFile(F, FileName);
        Rewrite(F);

        Nrecords := 0;

        DSS_Class.First;   // Sets DSS.ActiveDSSObject
        repeat

       // Skip Cktelements that have been checked before and written out by
       // something else
            if IsCktElement then
                with TDSSCktElement(DSS.ActiveDSSObject) do
                    if HasBeenSaved or (not Enabled) then
                        Continue;

            WriteActiveDSSObject(DSS, F, 'New');  // sets HasBeenSaved := TRUE
            Inc(Nrecords); // count the actual records

        until DSS_Class.Next = 0;

        CloseFile(F);

        if Nrecords > 0 then
            DSS.SavedFileList.Add(FileName)
        else
            DeleteFile(FileName);

        DSS_Class.Saved := TRUE;

    except
        On E: Exception do
        begin
            DoSimpleMsg(DSS, 'WriteClassFile Error: ' + E.Message, 718);
            Result := FALSE;
        end;
    end;

end;

function checkforblanks(const S: String): String;
{Checks for blanks in the name and puts quotes around it}
begin
    Result := s;
    if Pos(' ', S) > 0 then
        if S[1] <> '(' then  // Ignore if already quoted
            if S[1] <> '[' then  // Ignore if already quoted
                if S[1] <> '{' then  // Ignore if already quoted
                    Result := '"' + S + '"';
end;


procedure WriteActiveDSSObject(DSS: TDSS; var F: TextFile; const NeworEdit: String);

var
    ParClass: TDssClass;


begin
    ParClass := DSS.ActiveDSSObject.ParentClass;
  //  Write(F, NeworEdit, ' "', ParClass.Name + '.' + DSS.ActiveDSSObject.Name,'"');
    Write(F, Format('%s "%s.%s"', [NeworEdit, ParClass.Name, DSS.ActiveDSSObject.Name]));

    DSS.ActiveDSSObject.SaveWrite(F);


   // Handle disabled circuit elements;   Modified to allow applets to save disabled elements 12-28-06
    if (DSS.ActiveDSSObject.DSSObjType and ClassMask) <> DSS_Object then
        if not TDSSCktElement(DSS.ActiveDSSObject).Enabled then
            Write(F, ' ENABLED=NO');
    Writeln(F); // Terminate line

    DSS.ActiveDSSObject.HasBeenSaved := TRUE;

end;

procedure DoResetKeepList(DSS: TDSS);

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

function RewriteAlignedFile(DSS: TDSS; const Filename: String): Boolean;

var
    Fin, Fout: TextFile;
    SaveDelims, Line, Field, AlignedFile: String;
    FieldLength: pIntegerArray;
    ArraySize, FieldLen, FieldNum: Integer;

begin
    Result := TRUE;

    try
        AssignFile(Fin, FileName);
        Reset(Fin);
    except
        On E: Exception do
        begin
            DoSimpleMsg(DSS, 'Error opening file: ' + Filename + ', ' + E.message, 719);
            Result := FALSE;
            Exit
        end;
    end;

    try
        AlignedFile := ExtractFilePath(FileName) + 'Aligned_' + ExtractFileName(FileName);
        AssignFile(Fout, AlignedFile);
        Rewrite(Fout);
    except
        On E: Exception do
        begin
            DoSimpleMsg(DSS, 'Error opening file: ' + AlignedFile + ', ' + E.message, 720);
            CloseFile(Fin);
            Result := FALSE;
            Exit;
        end;
    end;

    SaveDelims := DSS.AuxParser.Delimiters;
    DSS.AuxParser.Delimiters := ',';
    ArraySize := 10;
    FieldLength := Allocmem(Sizeof(Integer) * ArraySize);

    try
   {Scan once to set field lengths}
        while not Eof(Fin) do
        begin
            Readln(Fin, line);
            DSS.AuxParser.CmdString := Line;  // Load the parsr
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

   {Now go back and re-read while writing the new file}
        Reset(Fin);

        while not EOF(Fin) do
        begin
            Readln(Fin, Line);
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
                    Write(Fout, Pad(Field, FieldLength^[FieldNum] + 1));
                end;
            until FieldLen = 0;

            if (pos('!', Line) > 0) then
                Write(Fout, ExtractComment(Line));

            Writeln(Fout);
        end;

    finally     {Make sure we do this stuff ...}

        Closefile(Fin);
        CloseFile(Fout);

        Reallocmem(FieldLength, 0);
        DSS.AuxParser.Delimiters := SaveDelims;

    end;

    DSS.GlobalResult := AlignedFile;

end;

function DoExecutiveCommand(DSS: TDSS; const s: String): Integer;

begin
    DSS.DSSExecutive.command := S;
    Result := DSS.DSSExecutive.Error;
end;


function CheckParallel(const Line1, Line2: TDSSCktElement): Boolean;
  {Check to see if two lines are in parallel}
begin
    Result := FALSE;
    if Line1.Terminals^[1].BusRef = Line2.Terminals^[1].BusRef then
        if Line1.Terminals^[2].BusRef = Line2.Terminals^[2].BusRef then
        begin
            Result := TRUE;
            Exit;
        end;
    if Line1.Terminals^[2].BusRef = Line2.Terminals^[1].BusRef then
        if Line1.Terminals^[1].BusRef = Line2.Terminals^[2].BusRef then
        begin
            Result := TRUE;
            Exit;
        end;
end;

function GetMaxPUVoltage(DSS: TDSS): Double;
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

function GetMinPUVoltage(DSS: TDSS; IgnoreNeutrals: Boolean): Double;
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

function GetTotalPowerFromSources(DSS: TDSS): Complex;

var
    CktElem: TDSSCktElement;

begin
    Result := CZERO;
    cktElem := DSS.ActiveCircuit.Sources.First;
    while CktElem <> NIL do
    begin
     //----CktElem.ActiveTerminalIdx := 1;
        Caccum(Result, Cnegate(CktElem.power[1]));
        cktElem := DSS.ActiveCircuit.Sources.Next;
    end;
end;

procedure WriteUniformGenerators(DSS: TDSS; var F: TextFile; kW, PF: Double; DoGenerators: Boolean);
 { Distribute the generators uniformly amongst the feeder nodes that have loads}

var
    kWeach: Double;
    LoadClass: TDSSClass;
    pLoad: TLoadObj;
    Count, i: Integer;

begin
    LoadClass := GetDSSClassPtr(DSS, 'load');
    Count := LoadClass.ElementList.ListSize;

    kWEach := kW / Max(1.0, round(Count));
    if DSS.ActiveCircuit.PositiveSequence then
        kWEach := kWeach / 3.0;

    for i := 1 to Count do
    begin
        pLoad := TLoadObj(LoadClass.ElementList.Get(i));
        if pLoad.Enabled then
        begin
            if DoGenerators then
                Write(F, Format('new generator.DG_%d  bus1=%s', [i, pLoad.GetBus(1)]))
            else
                Write(F, Format('new load.DL_%d  bus1=%s', [i, pLoad.GetBus(1)]));
            with DSS.ActiveCircuit do
            begin
                Write(F, Format(' phases=%d kV=%-g', [pLoad.NPhases, pLoad.kVLoadBase]));
                Write(F, Format(' kW=%-g', [kWeach]));
                Write(F, Format(' PF=%-.3g', [PF]));
            end;
            Write(F, ' model=1');
            Writeln(F);
        end;
    end;
end;

procedure WriteRandomGenerators(DSS: TDSS; var F: TextFile; kW, PF: Double; DoGenerators: Boolean);
{Distribute Generators randomly to loaded buses}

var
    kWeach: Double;
    LoadClass: TDSSClass;
    pLoad: TLoadObj;
    Count, i, LoadCount: Integer;

begin
    LoadClass := GetDSSClassPtr(DSS, 'load');
    Count := LoadClass.ElementList.ListSize;
   {Count enabled loads}
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

   {Place random sizes on load buses so that total is approximately what was spec'd}
    for i := 1 to Count do
    begin
        pLoad := TLoadObj(LoadClass.ElementList.Get(i));
        if pLoad.Enabled then
        begin
            if DoGenerators then
                Write(F, Format('new generator.DG_%d  bus1=%s', [i, pLoad.GetBus(1)]))
            else
                Write(F, Format('new load.DL_%d  bus1=%s', [i, pLoad.GetBus(1)]));
            with DSS.ActiveCircuit do
            begin
                Write(F, Format(' phases=%d kV=%-g', [pLoad.NPhases, pLoad.kVLoadBase]));
                Write(F, Format(' kW=%-g', [kWeach * random * 2.0]));
                Write(F, Format(' PF=%-.3g', [PF]));
            end;
            Write(F, ' model=1');
            Writeln(F);
        end;
    end;


end;

procedure WriteEveryOtherGenerators(DSS: TDSS; var F: TextFile; kW, PF: Double; Skip: Integer; DoGenerators: Boolean);

{distribute generators on every other load, skipping the number specified}

{Distribute the generator Proportional to load}


var
    kWeach, TotalkW: Double;
    LoadClass: TDSSClass;
    pLoad: TLoadObj;
    Count, i, skipcount: Integer;

begin

    LoadClass := GetDSSClassPtr(DSS, 'load');
    Count := LoadClass.ElementList.ListSize;
   {Add up the rated load in the enabled loads where gens will be placed}
    TotalkW := 0.0;
    Skipcount := Skip;
    for i := 1 to Count do
    begin
        pLoad := TLoadObj(LoadClass.ElementList.Get(i));
        if pLoad.Enabled then
        {Do not count skipped loads}
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
                    Write(F, Format('new generator.DG_%d  bus1=%s', [i, pLoad.GetBus(1)]))
                else
                    Write(F, Format('new load.DL_%d  bus1=%s', [i, pLoad.GetBus(1)]));
                with DSS.ActiveCircuit do
                begin
                    Write(F, Format(' phases=%d kV=%-g', [pLoad.NPhases, pLoad.kVLoadBase]));
                    Write(F, Format(' kW=%-g ', [kWeach * pLoad.kWBase]));
                    Write(F, Format(' PF=%-.3g', [PF]));
                end;
                Write(F, ' model=1');
                Writeln(F);
                SkipCount := Skip;
            end
            else
                Dec(SkipCount);
    end;

end;

procedure WriteProportionalGenerators(DSS: TDSS; var F: TextFile; kW, PF: Double; DoGenerators: Boolean);

{Distribute the generator Proportional to load}


var
    kWeach,
    TotalkW: Double;
    LoadClass: TDSSClass;
    pLoad: TLoadObj;
    Count, i: Integer;

begin
    LoadClass := GetDSSClassPtr(DSS, 'load');
    Count := LoadClass.ElementList.ListSize;
   {Add up the rated load in the enabled loads}
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
                Write(F, Format('new generator.DG_%d  bus1=%s', [i, pLoad.GetBus(1)]))
            else
                Write(F, Format('new load.DL_%d  bus1=%s', [i, pLoad.GetBus(1)]));
            with DSS.ActiveCircuit do
            begin
                Write(F, Format(' phases=%d kV=%-g', [pLoad.NPhases, pLoad.kVLoadBase]));
                Write(F, Format(' kW=%-g', [kWeach * pLoad.kWBase]));
                Write(F, Format(' PF=%-.3g', [PF]));
            end;
            Write(F, ' model=1');
            Writeln(F);
        end;
    end;

end;


procedure MakeDistributedGenerators(DSS: TDSS; kW, PF: Double; How: String; Skip: Integer; Fname: String; DoGenerators: Boolean);

var
    F: TextFile;
    WhatStr: String;

begin
    {Write outputfile and then redirect command parser to it.}

    try
        if FileExists(Fname) then
            DoSimpleMsg(DSS, 'File "' + Fname + '" is about to be overwritten. Rename it now before continuing if you wish to keep it.', 721);
        AssignFile(F, Fname);
        Rewrite(F);
    except
        On E: Exception do
        begin
            DoSimpleMsg(DSS, 'Error opening "' + Fname + '" for writing. Aborting.', 722);
            Exit;
        end;
    end;

    try
        if DoGenerators then
            WhatStr := 'Generators'
        else
            WhatStr := 'Loads';

        Writeln(F, '! Created with Distribute Command:');
        Writeln(f, Format('! Distribute kW=%-.6g PF=%-.6g How=%s Skip=%d  file=%s  what=%s', [kW, PF, How, Skip, Fname, WhatStr]));
        Writeln(F);
     // Writeln(F, 'Set allowduplicates=yes');
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
   // Writeln(F, 'Set allowduplicates=no');
        CloseFile(F);
        SetLastResultFile(DSS, Fname);
    end;

end;

{Feeder Utilities}
// procedure EnableFeeders(DSS: TDSS);
// 
// var
//     pMeter: TEnergyMeterObj;
// 
//     // Let EnergyMeter Objects control re-enabling of Feeders
//     // Feeder could have been dumped in meantime by setting Feeder=False in EnergyMeter
// 
// begin
//     with DSS.ActiveCircuit do
//     begin
//         pMeter := EnergyMeters.First;
//         while pMeter <> NIL do
//         begin
//             pMeter.EnableFeeder; // also sets CktElement feeder flags true   if a valid feeder
//             pMeter := EnergyMeters.Next;
//         end;
//     end;
// end;
// 
// procedure DisableFeeders;
// 
// var
//     pFeeder: TFeederObj;
// 
// begin
//     with DSS.ActiveCircuit do
//     begin
//         pFeeder := Feeders.First;
//         while pFeeder <> NIL do
//         begin
//             pFeeder.Enabled := FALSE;
//             pFeeder.SetCktElementFeederFlags(FALSE);
//             pFeeder := Feeders.Next;
//         end;
//     end;
// 
// 
// end;
// 
// procedure InitializeFeeders;
// // Var i:Integer;
// begin
//     (*    Do Nothing for now
//     With DSS.ActiveCircuit Do
//     For i := 1 to Feeders.ListSize Do Begin
//         If Not SolutionAbort Then TFeederObj(Feeders.Get(i)).InitForSweep;
//     End;
//     *)
// end;
// 
// procedure ForwardSweepAllFeeders;
// // Var i:Integer;
// begin
//     (*    Do Nothing for now
//     With DSS.ActiveCircuit Do
//     For i := 1 to Feeders.ListSize Do Begin
//         If Not SolutionAbort Then TFeederObj(Feeders.Get(i)).ForwardSweep;
//     End;
// *)
// end;
// 
// procedure BackwardSweepAllFeeders;
// // Var i:Integer;
// begin
//     (*    Do Nothing for now
//     With DSS.ActiveCircuit Do
//     For i := 1 to Feeders.ListSize Do Begin
//         If Not SolutionAbort Then TFeederObj(Feeders.Get(i)).BackwardSweep;
//     End;
//     *)
// end;

function GetDSSArray_Real(n: Integer; dbls: pDoubleArray): String;
var
    i: Integer;

begin
    Result := '[';
    for i := 1 to n do
        Result := Result + Format(' %-.6g', [dbls^[i]]);
    Result := Result + ']';
end;

function GetDSSArray_Integer(n: Integer; ints: pIntegerArray): String;

var
    i: Integer;

begin
    Result := '[';
    for i := 1 to n do
        Result := Result + Format(' %-.d', [ints^[i]]);
    Result := Result + ']';
end;

function CmulReal_im(const a: Complex; const Mult: Double): Complex;  // Multiply only imaginary part by a real

begin
    Result := cmplx(a.re, a.im * mult);
end;

procedure CmulArray(pc: pcomplexarray; Multiplier: Double; size: Integer);  // Multiply a complex array times a double
var
    i: Integer;
begin
    for i := 1 to size do
        pc^[i] := CMulReal(pc^[i], Multiplier);
end;

function GetMaxCktElementSize(DSS: TDSS): Integer;
var
    i: Integer;

begin
    Result := 0;

    with DSS.ActiveCircuit do
        for i := 1 to NumDevices do
            Result := max(result, TDSSCktElement(CktElements.Get(i)).Yorder);

end;

(*
FUNCTION IsValidNumericField(const NumberField:TEdit):Boolean;

Var
   Code  :Integer;
   Value :Double;

Begin
     Result := TRUE;
     Val(NumberField.Text, Value, Code);
     If Code>0 Then Begin
         Beep;
         NumberField.SetFocus;
         Result := FALSE;
     End;
End;
*)
function GetUniqueNodeNumber(DSS: TDSS; const sBusName: String; StartNode: Integer): Integer;
{To help avoid collisions of neutral numbers, this function returns a node number that is not being used,
 Starting at the StartNode value}
var
    iBusidx: Integer;
begin
    Result := StartNode;
    iBusidx := DSS.ActiveCircuit.Buslist.Find(sBusName);
    if iBusidx > 0 then
        while DSS.ActiveCircuit.Buses^[iBusidx].FindIdx(Result) <> 0 do
            Inc(Result);
    DSS.ActiveCircuit.Buses^[iBusidx].Add(result);  // add it to the list so next call will be unique
end;

procedure ShowMessageBeep(DSS: TDSS; const s: String);
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

procedure TraceAndEdit(DSS: TDSS; FromLine, ToLine: TPDElement; NPhases: Integer; EditStr: String);
{Trace back up a tree and execute an edit command string}
var
    pLine: TPDElement;
begin
    pLine := FromLine;
    while pLine <> NIL do
    begin
        if (pLine.NPhases = NPhases) or (Nphases = 0) then
        begin
            DSS.Parser.CmdString := EditStr;
            pLine.Edit;   // Uses Parser
        end;
        if pLine = ToLine then
            Break;
        pLine := pLine.ParentPDElement;
    end;
end;

procedure GoForwardAndRephase(DSS: TDSS; FromLine: TPDElement; const PhaseString, EditStr, ScriptFileName: String; TransStop: Boolean);
{Trace forward down a tree and Generate a script file to change the phase}
var
    pPDelem: TPDElement;
    pShuntObject: TDSSCktElement;
    pMeter: TEnergyMeterObj;
    i: Integer;
    S: String;
    Fout: Textfile;
    FileName: String;
    XfmrLevel: Integer;

begin

    pMeter := FromLine.MeterObj as TEnergyMeterObj;

   {Search for starting line in branchlist}
    pPDelem := pMeter.BranchList.first;
    while pPDelem <> NIL do
    begin
        if (FromLine = pPDelem) then
        begin
            Break;
        end;
        pPDelem := pMeter.BranchList.GoForward;
    end;

   {Error check}
    if pPDelem = NIL then
    begin
        DoSimpleMsg(DSS, FromLine.ParentClass.Name + '.' + FromLine.Name + ' Not found in Meter Zone.', 723);
        Exit;
    end;

    try
        FileName := DSS.OutputDirectory + DSS.CircuitName_ + ScriptFileName;
        DSS.GlobalResult := FileName;

        Assignfile(Fout, fileName);
        Rewrite(Fout);

        pMeter.BranchList.StartHere;
        pPDelem := pMeter.BranchList.GoForward;

        while pPDelem <> NIL do
        begin
            S := 'edit ' + pPDelem.ParentClass.Name + '.' + pPDElem.Name;

{----------------LINES---------------------------------------------------}

            if IsLineElement(pPDelem) then
            begin

                for i := 1 to pPDElem.NTerms do
                begin
                    S := S + Format(' Bus%d=%s%s', [i, StripExtension(pPDelem.GetBus(i)), PhaseString]);
           //  Parser.CmdString := Format('Bus$d=%s%s',[i, StripExtension(pPDelem.GetBus(i)), PhaseString]);
           //  pPDelem.Edit;
                end;

         {When we're done with that, we'll send the Edit string}
                if Length(EditStr) > 0 then
                begin
                    S := S + '  ' + EditStr;
           //  Parser.CmdString := EditStr;
           //  pPDelem.Edit;   // Uses Parser
                end;

                Writeln(Fout, S);

         {Now get all shunt objects connected to this branch}
                pShuntObject := pMeter.BranchList.FirstObject;
                while pShuntObject <> NIL do
                begin
               {1st Terminal Only}
                    i := 1;
                    S := 'edit ' + pShuntObject.ParentClass.Name + '.' + pShuntObject.Name;
                    S := S + Format(' Bus%d=%s%s', [i, StripExtension(pShuntObject.GetBus(i)), PhaseString]);
                    if Length(EditStr) > 0 then
                        S := S + '  ' + EditStr;
                    Writeln(Fout, S);
             //  Parser.CmdString := Format('Bus$d=%s%s',[i, StripExtension(pShuntObject.GetBus(1)), PhaseString]);
             //  pShuntObject.Edit;
                    pShuntObject := pMeter.BranchList.NextObject
                end;
                pPDelem := pMeter.BranchList.GoForward;
            end    {IsLine}

{----------------TRANSFORMERS---------------------------------------------------}

            else
            if IsTransformerElement(pPDELEM) then
            begin

     {
       We'll stop at transformers and change only the primary winding.
       Then we'll cycle forward until the lexical level is less or we're done
     }
                XFmrLevel := pMeter.BranchList.Level;
                S := S + Format(' wdg=1 Bus=%s%s  %s', [StripExtension(pPDelem.GetBus(1)), PhaseString, EditStr]);
                if not TransStop then
                    S := S + Format(' wdg=2 Bus=%s%s  %s', [StripExtension(pPDelem.GetBus(2)), PhaseString, EditStr]);
                Writeln(Fout, S);

     {Be default Go forward in the tree until we bounce back up to a line section above the transformer}
                if TransStop then
                begin
                    repeat
                        pPDelem := pMeter.BranchList.GoForward;
                    until (pPDelem = NIL) or (pMeter.BranchList.Level <= XfmrLevel);
                end
                else
                    pPDelem := pMeter.BranchList.GoForward;  {Then we get lines and loads beyond transformer}
            end;

        end;

    finally

        Closefile(Fout);
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

function InterpretLoadShapeClass(const s: String): Integer;
var
    ss: String;
begin
    ss := lowercase(s);
    Result := USENONE;

    case ss[1] of
        'd':
            case ss[2] of
                'a':
                    Result := USEDAILY;
                'u':
                    Result := USEDUTY;
            end;
        'y':
            Result := USEYEARLY;
        'n':
            Result := USENONE;
    end;

end;

function InterpretEarthModel(const s: String): Integer;

var
    ss: String;
begin
    ss := lowercase(s);
    Result := SIMPLECARSON;
    case ss[1] of
        'c':
            Result := SIMPLECARSON;
        'f':
            Result := FULLCARSON;
        'd':
            Result := DERI;
    end;

end;

function GetActiveLoadShapeClass(DSS: TDSS): String;

begin
    case DSS.ActiveCircuit.ActiveLoadShapeClass of
        USEDAILY:
            Result := 'Daily';
        USEYEARLY:
            Result := 'Yearly';
        USEDUTY:
            Result := 'Duty';
        USENONE:
            Result := 'None';
    end;

end;

function GetEarthModel(n: Integer): String;

begin
    case n of
        SIMPLECARSON:
            Result := 'Carson';
        FULLCARSON:
            Result := 'FullCarson';
        DERI:
            Result := 'Deri';
    end;

end;


function InterpretColorName(DSS: TDSS; const s: String): Integer;

begin
{$IFDEF FPC}
    Result := 0; // RGB for black
{$ELSE}
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
            DoSimpleMsg(DSS, 'Invalid Color Specification: "' + S + '".', 724);
    end;
{$ENDIF}
end;

function MakeNewCktElemName(DSS: TDSS; const oldname: String): String;
begin
    SetObject(DSS, OldName);  // set opject active
    with DSS.ActiveDSSObject do
        Result := Format('%s.%s%d', [ParentClass.Name, copy(ParentClass.Name, 1, 4), ClassIndex]);
end;

function ConstructElemName(DSS: TDSS; const Param: String): String;
{Construct an element name, sustituting @var values if any}

var
    FClassName, FObjName: String;

begin

    ParseObjectClassandName(DSS, lowercase(param), FClassName, FObjName);  // insert @var test
    result := Format('%s.%s', [FClassName, FObjName]);

end;

procedure Obfuscate(DSS: TDSS);
{Rename Buses and element names to generic names to remove identifiable names}
var
    i, bref: Integer;
    dotpos: Integer;
    DevListSize: Integer;
    TempBusList: THashList;
    pCktElem: TDSSCktElement;
    pCtrlElem: TDSSCktElement;
    S, Nodes: String;
    OldBusName: String;
    NewBusName: String;
    Baseclass: Integer;
    ElemClass: Integer;
    ControlUpDateStrings: TstringList;
    ControlUpDatePtrs: Tlist;

   {-------------------------------------------------------------}
    procedure RenameCktElem(pelem: TDSSCktElement); // local proc
    begin
        with pelem do
        begin
            Name := Format('%s%d', [copy(ParentClass.Name, 1, 4), ClassIndex]);
            DSS.ActiveCircuit.DeviceList.Add(Name); // Make a new device list corresponding to the CktElements List
            pelem.Checked := TRUE;
        end;
    end;

   {-------------------------------------------------------------}

begin

{Make sure buslist exists}

    if DSS.ActiveCircuit = NIL then
        Exit;
    if DSS.ActiveCircuit.BusList.ListSize <= 0 then
        Exit;
    with DSS.ActiveCircuit do
    begin
        TempBusList := THashList.Create(BusList.ListSize);

    {Rename Buses}
        for i := 1 to BusList.ListSize do
            TempBusList.Add(Format('B_%d', [i]));

        BusList.Free;
        BusList := TempBusList; // Reassign

    {Rename the bus names in each circuit element before renaming the elements}
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
                    bref := pCktElem.terminals^[i].BusRef;
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
                pCktElem.Edit;
            end;
            pCktElem := CktElements.Next;
        end;

    {Rename the circuit elements to generic values}
    {Have to catch the control elements and edit some of their parameters}

    {first, make scripts to change the monitored element names in the controls to what they will be}
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
            pCktElem.Checked := FALSE;     // Initialize to not checked
            pCktElem := Cktelements.Next;
        end;

        DevListSize := DeviceList.ListSize;
        DeviceList.Free;
        DeviceList := THashList.Create(DevListSize);

        pCktElem := Cktelements.First;
        while pCktElem <> NIL do
        begin
            if not pCktElem.Checked then
            begin
                ElemClass := (pCktElem.DSSObjType and CLASSMASK);
                RenameCktElem(pCktElem);
                case ElemClass of
                    XFMR_ELEMENT:
                        if pCktElem.HasControl then
                        begin
                            pCtrlElem := pCktElem.ControlElementList.First;
                            while assigned(pCtrlElem) do
                            begin
                                if (pCtrlElem.DSSObjType and CLASSMASK) = REG_CONTROL then
                                begin
                                    DSS.Parser.CmdString := Format('Transformer=%s', [pCktElem.Name]);
                                    pCtrlElem.Edit;
                                end;
                                pCtrlElem := pCktElem.ControlElementList.Next;
                            end;
                        end;
                else
                {nada}
                end;
            end;
            pCktElem := Cktelements.Next;
        end;


    {Run the control update scripts now that everything is renamed}

        for i := 0 to ControlUpDatePtrs.Count - 1 do
        begin
            pCktElem := ControlUpDatePtrs.Items[i];
            DSS.Parser.CmdString := ControlUpDateStrings.Strings[i];
            pCktElem.Edit;
        end;

        ControlUpDateStrings.Free;
        ControlUpDatePtrs.Free;

    end;  {With}

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

{-------------------------------------------------------------------------------}
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
    until (i > pElem.ControlElementList.listSize) or (Result > 0);
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

initialization

finalization


end.
