unit DSSCallBackRoutines;

// ----------------------------------------------------------
// Copyright (c) 2008-2015, Electric Power Research Institute, Inc.
// All rights reserved.
// ----------------------------------------------------------

interface

uses
    ArrayDef,
    UComplex, DSSUcomplex;

TYPE

  // NOTE: Maxlen argument is to better accommodate Fortran strings.  VB also
  //       Caller must allocate space for pchar values       
    pDSSCallBacks = ^TDSSCallBacks;  // Pointer to callback structure
    TDSSCallBacks = {$IFNDEF DSS_CAPI_NO_PACKED_RECORDS}Packed{$ENDIF} Record

        MsgCallBack: Procedure (S : pAnsiChar; Maxlen:UInt32);Stdcall; // Make use of DSS Message handling

        // Routines for using DSS Parser.  This allows you to write models that accept
        // syntax like other DSS scripts.
        GetIntValue: Procedure(var i : Int32);Stdcall; // Get next param as an Int32
        GetDblValue: Procedure(var x : Double); Stdcall;  // Get next param as a double
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
    DSSHelper,
    Bus,
    Circuit;

var
    CallBackParser: TDSSParser;
    CB_ParamName,
    CB_Param: String;


procedure DoSimpleMsgCallback(S: pAnsiChar; maxlen: Cardinal); STDCALL; // Call back for user-written models

begin
    DoSimpleMsg(DSSPrime, String(s), 9000);
end;

// These routines should work well with Fortran as well as C and VB

procedure ParserLoad(S: pAnsiChar; Maxlen: Cardinal); STDCALL;

begin
    CallBackParser.DSSCtx := DSSPrime;
    CallBackParser.CmdString := String(S);
end;


procedure ParserIntValue(var i: Integer); STDCALL;

begin
    CallBackParser.DSSCtx := DSSPrime;
    i := CallBackParser.IntValue;
end;



procedure ParserDblValue(var x: Double); STDCALL;

begin
    CallBackParser.DSSCtx := DSSPrime;
    x := CallBackParser.DblValue;
end;


procedure ParserStrValue(s: pAnsiChar; Maxlen: Cardinal); STDCALL;

// Copies null-terminated string into location pointed to by S up to the max chars specified

begin
    CallBackParser.DSSCtx := DSSPrime;
    SetLength(CB_Param, Maxlen);
    StrlCopy(s, pAnsiChar(Ansistring(CB_Param)), Maxlen);
end;



function ParserNextParam(ParamName: pAnsiChar; Maxlen: Cardinal): Integer; STDCALL;
begin
    CallBackParser.DSSCtx := DSSPrime;
    CB_ParamName := CallBackParser.NextParam;
    CB_Param := CallBackParser.StrValue;
    StrlCopy(ParamName, pAnsiChar(Ansistring(CB_ParamName)), Maxlen); // Copies up to Maxlen
    Result := Length(CB_Param);
end;


procedure DoDSSCommandCallBack(S: pAnsiChar; Maxlen: Cardinal); STDCALL;
begin
    DSSPrime.SolutionAbort := FALSE;
    DSSPrime.DSSExecutive.Command := String(S);
end;


procedure GetActiveElementBusNamesCallBack(Name1: pAnsiChar; Len1: Cardinal; Name2: pAnsiChar; Len2: Cardinal); STDCALL;
  // Get first two bus names of active Circuit Element for labeling graphs, etc.
  // Coordinate must be defined else returns null string
var
    CktElement: TDSSCktElement;
    BusIdx: Integer;
    bus: TDSSBus;
begin
    StrlCopy(Name1, pAnsiChar(''), Len1);  // Initialize to null
    StrlCopy(Name2, pAnsiChar(''), Len2);
    if DSSPrime.ActiveCircuit = NIL then
        Exit;

    CktElement := DSSPrime.ActiveCircuit.Activecktelement;
    if CktElement = NIL then
        Exit;

    // First bus
    BusIdx := CktElement.Terminals[0].busref;
    if BusIdx > 0 then
    begin
        bus := DSSPrime.ActiveCircuit.Buses[BusIdx];
        if bus.CoordDefined then
            StrlCopy(Name1, pAnsiChar(Ansistring(DSSPrime.ActiveCircuit.BusList.NameOfIndex(Busidx))), Len1);
    end;
    // Second bus
    BusIdx := CktElement.Terminals[1].busref;
    if BusIdx > 0 then
    begin
        bus := DSSPrime.ActiveCircuit.Buses[BusIdx];
        if bus.CoordDefined then
            StrlCopy(Name2, pAnsiChar(Ansistring(DSSPrime.ActiveCircuit.BusList.NameOfIndex(Busidx))), Len2);
    end;
end;


procedure GetActiveElementVoltagesCallBack(var NumVoltages: Integer; V: pComplexArray); STDCALL;
// NumVoltages is size of the V buffer
var
    i: Integer;
    elem: TDSSCktElement;
    circ: TDSSCircuit;
begin
    if DSSPrime.ActiveCircuit.ActiveCktElement = NIL then
        Exit;
    elem := DSSPrime.ActiveCircuit.ActiveCktElement;
    circ := DSSPrime.ActiveCircuit;
    NumVoltages := Min(elem.Yorder, NumVoltages);  // reset buffer size
    for i := 1 to NumVoltages do
        V[i] := circ.Solution.NodeV[elem.NodeRef[i]];
end;


procedure GetActiveElementCurrentsCallBack(var NumCurrents: Integer; Curr: pComplexArray); STDCALL;
var
    i: Integer;
    elem: TDSSCktElement;
begin
    if DSSPrime.ActiveCircuit.ActiveCktElement = NIL then
        Exit;
    elem := DSSPrime.ActiveCircuit.ActiveCktElement;
    elem.ComputeIterminal();
    NumCurrents := Min(elem.Yorder, NumCurrents); // Reset to actual number of elements returned
    for i := 1 to NumCurrents do
        Curr[i] := elem.ITerminal[i];
end;


procedure GetActiveElementLossesCallBack(var TotalLosses, LoadLosses, NoLoadLosses: Complex); STDCALL;
begin
    TotalLosses := 0;
    LoadLosses := 0;
    NoLoadLosses := 0;
    if DSSPrime.ActiveCircuit.ActiveCktElement = NIL then
        Exit;

    DSSPrime.ActiveCircuit.ActiveCktElement.GetLosses(TotalLosses, LoadLosses, NoLoadLosses);
end;


procedure GetActiveElementPowerCallBack(Terminal: Integer; var TotalPower: Complex); STDCALL;
begin
    TotalPower := 0;
    if DSSPrime.ActiveCircuit.ActiveCktElement = NIL then
        Exit;
    //----ActiveTerminalIdx := Terminal;
    TotalPower := DSSPrime.ActiveCircuit.ActiveCktElement.Power[Terminal];
end;


procedure GetActiveElementNumCustCallBack(var Numcust, TotalCust: Integer); STDCALL;

var
    pDElem: TPDElement;

begin
    NumCust := 0;
    TotalCust := 0;
    if DSSPrime.ActiveCircuit.ActiveCktElement = NIL then
        Exit;

    if DSSPrime.ActiveCircuit.ActiveCktElement is TPDElement then
    begin
        pDElem := DSSPrime.ActiveCircuit.ActiveCktElement as TPDElement;
        NumCust := pDElem.BranchNumCustomers;
        TotalCust := pDElem.BranchTotalCustomers;
    end;
end;


procedure GetActiveElementNodeRefCallBack(Maxsize: Integer; NodeReferenceArray: pIntegerArray); STDCALL;// calling program must allocate
var
    i: Integer;
    elem: TDSSCktElement;
begin
    elem := DSSPrime.ActiveCircuit.ActiveCktElement;
    if elem = NIL then
        Exit;

    for i := 1 to Min(elem.Yorder, Maxsize) do
        NodeReferenceArray[i] := elem.NodeRef[i];
end;


function GetActiveElementBusRefCallBack(Terminal: Integer): Integer; STDCALL;
var
    elem: TDSSCktElement;
begin
    Result := 0;
    elem := DSSPrime.ActiveCircuit.ActiveCktElement;
    if elem = NIL then
        Exit;
    Result := elem.Terminals[Terminal - 1].BusRef;
end;


procedure GetActiveElementTerminalInfoCallBack(var NumTerminals, NumConds, NumPhases: Integer); STDCALL;
var
    elem: TDSSCktElement;
begin
    elem := DSSPrime.ActiveCircuit.ActiveCktElement;
    if elem = NIL then
        Exit;

    NumTerminals := elem.Nterms;
    NumConds := elem.Nconds;
    NumPhases := elem.NPhases;
end;


procedure GetPtrToSystemVarrayCallBack(var V: Pointer; var iNumNodes: Integer); STDCALL; // Returns pointer to Solution.V and size
begin
    V := DSSPrime.ActiveCircuit.Solution.NodeV;  // Return Pointer to Node Voltage array
    iNumNodes := DSSPrime.ActiveCircuit.NumNodes;
end;



function GetActiveElementIndexCallBack: Integer; STDCALL;
// Usually just checking to see if this result >0
var
    elem: TDSSCktElement;
begin
    Result := 0;
    elem := DSSPrime.ActiveCircuit.ActiveCktElement;
    if elem = NIL then
        Exit;
    Result := elem.ClassIndex;
end;


function IsActiveElementEnabledCallBack: Boolean; STDCALL;

var
    elem: TDSSCktElement;
begin
    Result := FALSE;
    elem := DSSPrime.ActiveCircuit.ActiveCktElement;
    if elem = NIL then
        Exit;
    Result := elem.Enabled;
end;


function IsBusCoordinateDefinedCallback(BusRef: Integer): Boolean; STDCALL;
begin
    Result := FALSE;
    if Assigned(DSSPrime.ActiveCircuit) and (busRef > 0) then
        Result := DSSPrime.ActiveCircuit.Buses[BusRef].CoordDefined;
end;

procedure GetBusCoordinateCallback(BusRef: Integer; var X, Y: Double); STDCALL;
begin
    X := 0.0;
    Y := 0.0;
    if Assigned(DSSPrime.ActiveCircuit) and (busRef > 0) then
    begin
        X := DSSPrime.ActiveCircuit.Buses[BusRef].X;
        Y := DSSPrime.ActiveCircuit.Buses[BusRef].Y;
    end;
end;

function GetBuskVBaseCallback(BusRef: Integer): Double; STDCALL;
begin
    Result := 0.0;
    if Assigned(DSSPrime.ActiveCircuit) and (busRef > 0) then
    begin
        Result := DSSPrime.ActiveCircuit.Buses[BusRef].kVBase;
    end;
end;

function GetBusDistFromMeterCallback(BusRef: Integer): Double; STDCALL;
begin
    Result := 0.0;
    if Assigned(DSSPrime.ActiveCircuit) and (busRef > 0) then
    begin
        Result := DSSPrime.ActiveCircuit.Buses[BusRef].DistFromMeter;
    end;
end;

procedure GetDynamicsStructCallBack(var DynamicsStruct: Pointer); STDCALL;
begin
    if Assigned(DSSPrime.ActiveCircuit) then
    begin
        DynamicsStruct := @DSSPrime.ActiveCircuit.Solution.DynaVars;
    end;
end;

function GetStepSizeCallBack: Double; STDCALL;
begin
    Result := 0.0;
    if Assigned(DSSPrime.ActiveCircuit) then
    begin
        Result := DSSPrime.ActiveCircuit.Solution.DynaVars.h;
    end;
end;

function GetTimeSecCallBack: Double; STDCALL;
begin
    Result := 0.0;
    if DSSPrime.ActiveCircuit = NIL then
        Exit; //TODO: This isn't checked in the other functions :|
    Result := DSSPrime.ActiveCircuit.Solution.DynaVars.t;
end;

function GetTimeHrCallBack: Double; STDCALL;
begin
    Result := 0.0;
    if DSSPrime.ActiveCircuit = NIL then
        Exit; //TODO: This isn't checked in the other functions :|
    Result := DSSPrime.ActiveCircuit.Solution.DynaVars.dblHour;
end;


procedure GetPublicDataPtrCallBack(var pPublicData: Pointer; var PublicDataBytes: Integer); STDCALL;
var
    elem: TDSSCktElement;
begin
    elem := DSSPrime.ActiveCircuit.ActiveCktElement;
    if elem = NIL then
        Exit;

    pPublicData := elem.PublicDataStruct;
    PublicDataBytes := elem.PublicDataSize;
end;

function GetActiveElementNameCallBack(ElFullName: pAnsiChar; Maxlen: Cardinal): Integer; STDCALL;
// Maxlen is num of chars the calling program allocates for the string
var
    S: String;
    elem: TDSSCktElement;
begin
    Result := 0;
    elem := DSSPrime.ActiveCircuit.ActiveCktElement;
    if elem = NIL then
        Exit;

    S := elem.FullName;
    StrlCopy(ElFullName, pAnsiChar(Ansistring(S)), Maxlen);
    Result := Length(ElFullName);
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


initialization
    // Initialize Function Interface variables for user-Written Callbacks
    CallBackRoutines.MsgCallBack := DoSimpleMsgCallback; // for user-written callbacks
    CallBackRoutines.GetIntValue := ParserIntValue;
    CallBackRoutines.GetDblValue := ParserDblValue;
    CallBackRoutines.GetStrValue := ParserStrValue;
    CallBackRoutines.LoadParser := ParserLoad;
    CallBackRoutines.NextParam := ParserNextParam;
    CallBackRoutines.DoDSSCommand := DoDSSCommandCallBack;
    CallBackRoutines.GetActiveElementBusNames := GetActiveElementBusNamesCallBack;
    CallBackRoutines.GetActiveElementVoltages := GetActiveElementVoltagesCallBack;
    CallBackRoutines.GetActiveElementCurrents := GetActiveElementCurrentsCallBack;
    CallBackRoutines.GetActiveElementLosses := GetActiveElementLossesCallBack;
    CallBackRoutines.GetActiveElementPower := GetActiveElementPowerCallBack;
    CallBackRoutines.GetActiveElementNumCust := GetActiveElementNumCustCallBack;
    CallBackRoutines.GetActiveElementNodeRef := GetActiveElementNodeRefCallBack;
    CallBackRoutines.GetActiveElementBusRef := GetActiveElementBusRefCallBack;
    CallBackRoutines.GetActiveElementTerminalInfo := GetActiveElementTerminalInfoCallBack;
    CallBackRoutines.GetPtrToSystemVarray := GetPtrToSystemVarrayCallBack;
    CallBackRoutines.GetActiveElementIndex := GetActiveElementIndexCallBack;
    CallBackRoutines.IsActiveElementEnabled := IsActiveElementEnabledCallBack;
    CallBackRoutines.IsBusCoordinateDefined := IsBusCoordinateDefinedCallBack;
    CallBackRoutines.GetBusCoordinate := GetBusCoordinateCallBack;
    CallBackRoutines.GetBuskVBase := GetBuskVBaseCallBack;
    CallBackRoutines.GetBusDistFromMeter := GetBusDistFromMeterCallback;

        // Added 4-9-2012
    CallBackRoutines.GetDynamicsStruct := GetDynamicsStructCallBack;
    CallBackRoutines.GetStepSize := GetStepSizeCallBack;
    CallBackRoutines.GetTimeSec := GetTimeSecCallBack;
    CallBackRoutines.GetTimeHr := GetTimeHrCallBack;

    CallBackRoutines.GetPublicDataPtr := GetPublicDataPtrCallBack;
    CallBackRoutines.GetActiveElementName := GetActiveElementNameCallBack;
    CallBackRoutines.GetActiveElementPtr := GetActiveElementPtrCallBack;
    CallBackRoutines.ControlQueuePush := ControlQueuePushCallBack;
    CallBackRoutines.GetResultStr := GetResultStrCallBack;

    CallBackParser := TDSSParser.Create(NIL); //TODO: this is bad. Maybe we should restrict user-models.


finalization

    CallBackParser.Free;

end.
