unit DSSCallBackRoutines;

{
    ----------------------------------------------------------
  Copyright (c) 2008-2015, Electric Power Research Institute, Inc.
  All rights reserved.
  ----------------------------------------------------------
}

interface

uses
    ArrayDef,
    UComplex, DSSUcomplex;

TYPE

  // NOTE: Maxlen argument is to better accommodate Fortran strings.  VB also
  //       Caller must allocate space for pchar values       
    pDSSCallBacks = ^TDSSCallBacks;  {Pointer to callback structure}
    TDSSCallBacks = {$IFNDEF DSS_CAPI_NO_PACKED_RECORDS}Packed{$ENDIF} Record

        MsgCallBack: Procedure (S : pAnsiChar; Maxlen:UInt32);Stdcall; {Make use of DSS Message handling}

        // Routines for using DSS Parser.  This allows you to write models that accept
        // syntax like other DSS scripts.
        GetIntValue: Procedure(var i : Int32);Stdcall; {Get next param as an Int32}
        GetDblValue: Procedure(var x : Double); Stdcall;  {Get next param as a double}
        GetStrValue: Procedure(s : pAnsiChar; maxlen : UInt32); Stdcall;
        //Get next param as a string <= maxlen characters  (UInt32 = 32-bit unsigned)
        //caller must allocate space for s (Maxlen chars)
        LoadParser:  Procedure(S : pAnsiChar; maxlen : UInt32); Stdcall; // Copies a string into a special instance of the DSS parser
        NextParam:   Function (ParamName : pAnsiChar; Maxlen : UInt32):Int32; Stdcall;
        //  Advance to the next parameter and
        //  Get name of the param just retrieved, if one was given.
        //  Returns length of parameter found.  If 0, then end of string.
        //  This is to handle the syntax "paramname=paramvalue" commonly used in DSS scripts
        //  Copies the string to the location specified by s up to maxlen characters.
        //  Caller must allocate space (Maxlen chars)

        DoDSSCommand:             Procedure(S : pAnsiChar; Maxlen : UInt32); StdCall;
        GetActiveElementBusNames: Procedure(Name1 : pAnsiChar; Len1 : UInt32; Name2 : pAnsiChar; Len2 : UInt32); StdCall;
        GetActiveElementVoltages: Procedure(Var NumVoltages : Int32; V : pComplexArray); StdCall;
        GetActiveElementCurrents: Procedure(Var NumCurrents : Int32; Curr : pComplexArray); StdCall;
        GetActiveElementLosses:   Procedure(Var TotalLosses, LoadLosses, NoLoadLosses : Complex); StdCall;
        GetActiveElementPower:    Procedure(Terminal : Int32; Var TotalPower : Complex); StdCall;
        GetActiveElementNumCust:  Procedure(Var NumCust, TotalCust : Int32); StdCall;
        GetActiveElementNodeRef:  Procedure(Maxsize : Int32; NodeReferenceArray : pIntegerArray);  StdCall;// calling program must allocate
        GetActiveElementBusRef:   Function(Terminal : Int32) : Int32;  StdCall;
        GetActiveElementTerminalInfo: Procedure(Var NumTerminals, NumConds, NumPhases : Int32); StdCall;
        GetPtrToSystemVarray:     Procedure(var V : Pointer; var iNumNodes : Int32); StdCall; // Returns pointer to Solution.V and size
        GetActiveElementIndex:    Function() : Int32; StdCall;
        IsActiveElementEnabled:   Function() : Boolean; StdCall;
        IsBusCoordinateDefined:   Function(BusRef : Int32) : Boolean; StdCall;
        GetBusCoordinate:         Procedure(BusRef : Int32; Var X, Y : Double); StdCall;
        GetBuskVBase:             Function(BusRef : Int32) : Double; StdCall;
        GetBusDistFromMeter:      Function(BusRef : Int32) : Double; StdCall;

        GetDynamicsStruct:        Procedure(var pDynamicsStruct : Pointer); StdCall;  // Returns pointer to dynamics variables structure
        GetStepSize:              Function() : Double; StdCall;  // Return just 'h' from dynamics record
        GetTimeSec:               Function() : Double; StdCall; // returns t in sec from top of hour
        GetTimeHr:                Function() : Double; StdCall; // returns time as a double in hours

        GetPublicDataPtr:         Procedure(var pPublicData : Pointer; Var PublicDataBytes : Int32); StdCall;
        GetActiveElementName:     Function(FullName : pAnsiChar; MaxNameLen : UInt32) : Int32; StdCall;
        GetActiveElementPtr:      Function() : Pointer; StdCall;  // Returns pointer to active circuit element
        ControlQueuePush:         Function(Const Hour:Int32; Const Sec:Double; Const Code, ProxyHdl:Int32; Owner:Pointer):Int32; StdCall;
        GetResultStr:             Procedure(S : pAnsiChar; Maxlen : UInt32); StdCall;
    end;

var
    CallBackRoutines: TDSSCallBacks;

procedure DoSimpleMsgCallback(S: pAnsiChar; maxlen: Cardinal); STDCALL; // Call back for user-written models

implementation

uses
    ParserDel,
    DSSGlobals,
    Executive,
    SysUtils,
    CktElement,
    Math,
    PDElement,
    DSSClass,
    DSSHelper;

var
    CallBackParser: TDSSParser;
    CB_ParamName,
    CB_Param: String;

{====================================================================================================================}

procedure DoSimpleMsgCallback(S: pAnsiChar; maxlen: Cardinal); STDCALL; // Call back for user-written models

begin
    DoSimpleMsg(DSSPrime, String(s), 9000);
end;

   {These routines should work well with Fortran as well as C and VB}

{====================================================================================================================}

procedure ParserLoad(S: pAnsiChar; Maxlen: Cardinal); STDCALL;

begin
    CallBackParser.CmdString := String(S);
end;

{====================================================================================================================}

procedure ParserIntValue(var i: Integer); STDCALL;

begin
    with CallBackParser do
    begin
        i := IntValue;
    end;
end;


{====================================================================================================================}

procedure ParserDblValue(var x: Double); STDCALL;

begin
    with CallBackParser do
    begin
        x := DblValue;
    end;
end;

{====================================================================================================================}

procedure ParserStrValue(s: pAnsiChar; Maxlen: Cardinal); STDCALL;

{Copies null-terminated string into location pointed to by S up to the max chars specified}

begin
    with CallBackParser do
    begin
        StrlCopy(s, pAnsiChar(Ansistring(CB_Param)), Maxlen);
    end;
end;


{====================================================================================================================}

function ParserNextParam(ParamName: pAnsiChar; Maxlen: Cardinal): Integer; STDCALL;
begin
    with CallBackParser do
    begin
        CB_ParamName := NextParam;
        CB_Param := StrValue;
    end;
    StrlCopy(ParamName, pAnsiChar(Ansistring(CB_ParamName)), Maxlen); // Copies up to Maxlen
    Result := Length(CB_Param);
end;

{====================================================================================================================}

procedure DoDSSCommandCallBack(S: pAnsiChar; Maxlen: Cardinal); STDCALL;
begin
    DSSPrime.SolutionAbort := FALSE;
    DSSPrime.DSSExecutive.Command := String(S);
end;

{====================================================================================================================}

procedure GetActiveElementBusNamesCallBack(Name1: pAnsiChar; Len1: Cardinal; Name2: pAnsiChar; Len2: Cardinal); STDCALL;
  {Get first two bus names of active Circuit Element for labeling graphs, etc.}
  {Coordinate must be defined else returns null string}
var
    CktElement: TDSSCktElement;
    BusIdx: Integer;
begin
    StrlCopy(Name1, pAnsiChar(''), Len1);  // Initialize to null
    StrlCopy(Name2, pAnsiChar(''), Len2);
    if DSSPrime.ActiveCircuit <> NIL then
    begin
        CktElement := DSSPrime.ActiveCircuit.Activecktelement;
        if CktElement <> NIL then
        begin
     {First bus}
            BusIdx := CktElement.Terminals[0].busref;
            if BusIdx > 0 then
                with DSSPrime.ActiveCircuit.Buses^[BusIdx] do
                    if CoordDefined then
                        StrlCopy(Name1, pAnsiChar(Ansistring(DSSPrime.ActiveCircuit.BusList.NameOfIndex(Busidx))), Len1);
      {Second bus}
            BusIdx := CktElement.Terminals[1].busref;
            if BusIdx > 0 then
                with DSSPrime.ActiveCircuit.Buses^[BusIdx] do
                    if CoordDefined then
                        StrlCopy(Name2, pAnsiChar(Ansistring(DSSPrime.ActiveCircuit.BusList.NameOfIndex(Busidx))), Len2);
        end; {If CktElement}
    end;  {If ActiveCircuit}
end;

{====================================================================================================================}

procedure GetActiveElementVoltagesCallBack(var NumVoltages: Integer; V: pComplexArray); STDCALL;
{NumVoltages is size of the V buffer}
var
    i: Integer;
begin
    if Assigned(DSSPrime.ActiveCircuit.ActiveCktElement) then
        with DSSPrime.ActiveCircuit do
            with ActiveCktElement do
            begin
                NumVoltages := Min(Yorder, NumVoltages);  // reset buffer size
                for i := 1 to NumVoltages do
                    V^[i] := Solution.NodeV^[NodeRef^[i]];
            end;
end;

{====================================================================================================================}

procedure GetActiveElementCurrentsCallBack(var NumCurrents: Integer; Curr: pComplexArray); STDCALL;
var
    i: Integer;
begin
    if Assigned(DSSPrime.ActiveCircuit.ActiveCktElement) then
        with DSSPrime.ActiveCircuit do
            with ActiveCktElement do
            begin
                ComputeIterminal;
                NumCurrents := Min(Yorder, NumCurrents); // Reset to actual number of elements returned
                for i := 1 to NumCurrents do
                    Curr^[i] := ITerminal^[i];
            end;
end;

{====================================================================================================================}

procedure GetActiveElementLossesCallBack(var TotalLosses, LoadLosses, NoLoadLosses: Complex); STDCALL;
begin
    TotalLosses := CZERO;
    LoadLosses := CZERO;
    NoLoadLosses := CZERO;
    if Assigned(DSSPrime.ActiveCircuit.ActiveCktElement) then
        with DSSPrime.ActiveCircuit do
            with ActiveCktElement do
            begin
                GetLosses(TotalLosses, LoadLosses, NoLoadLosses);
            end;
end;

{====================================================================================================================}

procedure GetActiveElementPowerCallBack(Terminal: Integer; var TotalPower: Complex); STDCALL;
begin
    TotalPower := CZERO;
    if Assigned(DSSPrime.ActiveCircuit.ActiveCktElement) then
        with DSSPrime.ActiveCircuit do
            with ActiveCktElement do
            begin
             //----ActiveTerminalIdx := Terminal;
                TotalPower := Power[Terminal];
            end;
end;

{====================================================================================================================}

procedure GetActiveElementNumCustCallBack(var Numcust, TotalCust: Integer); STDCALL;

var
    pDElem: TPDElement;

begin
    NumCust := 0;
    TotalCust := 0;
    if Assigned(DSSPrime.ActiveCircuit.ActiveCktElement) then
        if DSSPrime.ActiveCircuit.ActiveCktElement is TPDElement then
        begin
            pDElem := DSSPrime.ActiveCircuit.ActiveCktElement as TPDElement;
            NumCust := pDElem.BranchNumCustomers;
            TotalCust := pDElem.BranchTotalCustomers;
        end;
end;

{====================================================================================================================}

procedure GetActiveElementNodeRefCallBack(Maxsize: Integer; NodeReferenceArray: pIntegerArray); STDCALL;// calling program must allocate
var
    i: Integer;
begin
    if Assigned(DSSPrime.ActiveCircuit.ActiveCktElement) then
        with DSSPrime.ActiveCircuit do
            with ActiveCktElement do
            begin
                for i := 1 to Min(Yorder, Maxsize) do
                    NodeReferenceArray^[i] := NodeRef^[i];
            end;
end;

{====================================================================================================================}

function GetActiveElementBusRefCallBack(Terminal: Integer): Integer; STDCALL;
begin
    Result := 0;
    if Assigned(DSSPrime.ActiveCircuit.ActiveCktElement) then
        with DSSPrime.ActiveCircuit do
            with ActiveCktElement do
            begin
                Result := Terminals[Terminal - 1].BusRef;
            end;
end;

{====================================================================================================================}

procedure GetActiveElementTerminalInfoCallBack(var NumTerminals, NumConds, NumPhases: Integer); STDCALL;
begin
    if Assigned(DSSPrime.ActiveCircuit.ActiveCktElement) then
        with DSSPrime.ActiveCircuit do
            with ActiveCktElement do
            begin
                NumTerminals := Nterms;
                NumConds := Nconds;
                NumPhases := NPhases;
            end;
end;

{====================================================================================================================}

procedure GetPtrToSystemVarrayCallBack(var V: Pointer; var iNumNodes: Integer); STDCALL; // Returns pointer to Solution.V and size
begin
    if Assigned(DSSPrime.ActiveCircuit.ActiveCktElement) then
        with DSSPrime.ActiveCircuit do
            with ActiveCktElement do
            begin
                V := Solution.NodeV;  // Return Pointer to Node Voltage array
                iNumNodes := NumNodes;
            end;
end;


{====================================================================================================================}

function GetActiveElementIndexCallBack: Integer; STDCALL;
    {Usually just checking to see if this result >0}
begin
    Result := 0;
    if Assigned(DSSPrime.ActiveCircuit) then
        if Assigned(DSSPrime.ActiveCircuit.ActiveCktElement) then
            Result := DSSPrime.ActiveCircuit.ActiveCktElement.ClassIndex;
end;

{====================================================================================================================}

function IsActiveElementEnabledCallBack: Boolean; STDCALL;

begin
    Result := FALSE;
    if Assigned(DSSPrime.ActiveCircuit) then
        if Assigned(DSSPrime.ActiveCircuit.ActiveCktElement) then
            Result := DSSPrime.ActiveCircuit.ActiveCktElement.Enabled;
end;

{====================================================================================================================}

function IsBusCoordinateDefinedCallback(BusRef: Integer): Boolean; STDCALL;
begin
    Result := FALSE;
    if Assigned(DSSPrime.ActiveCircuit) and (busRef > 0) then
        Result := DSSPrime.ActiveCircuit.Buses^[BusRef].CoordDefined;
end;

{====================================================================================================================}
procedure GetBusCoordinateCallback(BusRef: Integer; var X, Y: Double); STDCALL;
begin
    X := 0.0;
    Y := 0.0;
    if Assigned(DSSPrime.ActiveCircuit) and (busRef > 0) then
    begin
        X := DSSPrime.ActiveCircuit.Buses^[BusRef].X;
        Y := DSSPrime.ActiveCircuit.Buses^[BusRef].Y;
    end;
end;

{====================================================================================================================}
function GetBuskVBaseCallback(BusRef: Integer): Double; STDCALL;
begin
    Result := 0.0;
    if Assigned(DSSPrime.ActiveCircuit) and (busRef > 0) then
    begin
        Result := DSSPrime.ActiveCircuit.Buses^[BusRef].kVBase;
    end;
end;

{====================================================================================================================}
function GetBusDistFromMeterCallback(BusRef: Integer): Double; STDCALL;
begin
    Result := 0.0;
    if Assigned(DSSPrime.ActiveCircuit) and (busRef > 0) then
    begin
        Result := DSSPrime.ActiveCircuit.Buses^[BusRef].DistFromMeter;
    end;
end;

{====================================================================================================================}
procedure GetDynamicsStructCallBack(var DynamicsStruct: Pointer); STDCALL;
begin
    if Assigned(DSSPrime.ActiveCircuit) then
    begin
        DynamicsStruct := @DSSPrime.ActiveCircuit.Solution.DynaVars;
    end;
end;

{====================================================================================================================}
function GetStepSizeCallBack: Double; STDCALL;
begin
    Result := 0.0;
    if Assigned(DSSPrime.ActiveCircuit) then
    begin
        Result := DSSPrime.ActiveCircuit.Solution.DynaVars.h;
    end;
end;

{====================================================================================================================}
function GetTimeSecCallBack: Double; STDCALL;
begin
    Result := 0.0;
    if Assigned(DSSPrime.ActiveCircuit) then
    begin
        Result := DSSPrime.ActiveCircuit.Solution.DynaVars.t;
    end;
end;

{====================================================================================================================}
function GetTimeHrCallBack: Double; STDCALL;
begin
    Result := 0.0;
    if Assigned(DSSPrime.ActiveCircuit) then
    begin
        Result := DSSPrime.ActiveCircuit.Solution.DynaVars.dblHour;
    end;
end;

{====================================================================================================================}

procedure GetPublicDataPtrCallBack(var pPublicData: Pointer; var PublicDataBytes: Integer); STDCALL;

begin
    if Assigned(DSSPrime.ActiveCircuit.ActiveCktElement) then
        with DSSPrime.ActiveCircuit do
            with ActiveCktElement do
            begin
                pPublicData := PublicDataStruct;
                PublicDataBytes := PublicDataSize;
            end;
end;

function GetActiveElementNameCallBack(ElFullName: pAnsiChar; Maxlen: Cardinal): Integer; STDCALL;
// Maxlen is num of chars the calling program allocates for the string
var
    S: String;
begin
    Result := 0;
    if Assigned(DSSPrime.ActiveCircuit.ActiveCktElement) then
        with DSSPrime.ActiveCircuit do
            with ActiveCktElement do
            begin
                S := FullName;
                StrlCopy(ElFullName, pAnsiChar(Ansistring(S)), Maxlen);
                Result := Length(ElFullName);
            end;
end;

function GetActiveElementPtrCallBack(): Pointer; STDCALL;  // Returns pointer to active circuit element
begin
    Result := Pointer(DSSPrime.ActiveCircuit.ActiveCktElement);
end;

function ControlQueuePushCallBack(const Hour: Integer; const Sec: Double; const Code, ProxyHdl: Integer; Owner: Pointer): Integer; STDCALL;
begin
    Result := DSSPrime.ActiveCircuit.ControlQueue.Push(Hour, Sec, Code, ProxyHdl, Owner);
end;

procedure GetResultStrCallBack(S: pAnsiChar; Maxlen: Cardinal); STDCALL;
begin
    StrlCopy(S, pAnsiChar(Ansistring(DSSPrime.GlobalResult)), Maxlen);
end;

{====================================================================================================================}

initialization

{Initialize Function Interface variables for user-Written Callbacks}

    with CallBackRoutines do
    begin
        MsgCallBack := DoSimpleMsgCallback; // for user-written callbacks
        GetIntValue := ParserIntValue;
        GetDblValue := ParserDblValue;
        GetStrValue := ParserStrValue;
        LoadParser := ParserLoad;
        NextParam := ParserNextParam;
        DoDSSCommand := DoDSSCommandCallBack;
        GetActiveElementBusNames := GetActiveElementBusNamesCallBack;
        GetActiveElementVoltages := GetActiveElementVoltagesCallBack;
        GetActiveElementCurrents := GetActiveElementCurrentsCallBack;
        GetActiveElementLosses := GetActiveElementLossesCallBack;
        GetActiveElementPower := GetActiveElementPowerCallBack;
        GetActiveElementNumCust := GetActiveElementNumCustCallBack;
        GetActiveElementNodeRef := GetActiveElementNodeRefCallBack;
        GetActiveElementBusRef := GetActiveElementBusRefCallBack;
        GetActiveElementTerminalInfo := GetActiveElementTerminalInfoCallBack;
        GetPtrToSystemVarray := GetPtrToSystemVarrayCallBack;
        GetActiveElementIndex := GetActiveElementIndexCallBack;
        IsActiveElementEnabled := IsActiveElementEnabledCallBack;
        IsBusCoordinateDefined := IsBusCoordinateDefinedCallBack;
        GetBusCoordinate := GetBusCoordinateCallBack;
        GetBuskVBase := GetBuskVBaseCallBack;
        GetBusDistFromMeter := GetBusDistFromMeterCallback;

         // Added 4-9-2012
        GetDynamicsStruct := GetDynamicsStructCallBack;
        GetStepSize := GetStepSizeCallBack;
        GetTimeSec := GetTimeSecCallBack;
        GetTimeHr := GetTimeHrCallBack;

        GetPublicDataPtr := GetPublicDataPtrCallBack;
        GetActiveElementName := GetActiveElementNameCallBack;
        GetActiveElementPtr := GetActiveElementPtrCallBack;
        ControlQueuePush := ControlQueuePushCallBack;
        GetResultStr := GetResultStrCallBack;
    end;

    CallBackParser := TDSSParser.Create;

{====================================================================================================================}

finalization

    CallBackParser.Free;

end.
