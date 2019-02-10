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
    uComplex;

{$INCLUDE DSSCallBackStructDef.pas}


var
    CallBackRoutines: TDSSCallBacks;

procedure DoSimpleMsgCallback(S: pAnsiChar; maxlen: Cardinal); STDCALL; // Call back for user-written models

implementation

uses
    ParserDel,
    DSSGlobals,
    Executive,
{$IFNDEF FPC}
    AnsiStrings,
{$ELSE}
    SysUtils,
{$ENDIF}
    CktElement,
    Math,
    PDElement;

var
    CallBackParser: TParser;
    CB_ParamName,
    CB_Param: String;

{====================================================================================================================}

procedure DoSimpleMsgCallback(S: pAnsiChar; maxlen: Cardinal); STDCALL; // Call back for user-written models

begin
    DoSimpleMsg(String(s), 9000);
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
    SolutionAbort := FALSE;
    DSSExecutive.Command := String(S);
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
    if ActiveCircuit <> NIL then
    begin
        CktElement := ActiveCircuit.Activecktelement;
        if CktElement <> NIL then
        begin
     {First bus}
            BusIdx := CktElement.Terminals^[1].busref;
            if BusIdx > 0 then
                with  ActiveCircuit.Buses^[BusIdx] do
                    if CoordDefined then
                        StrlCopy(Name1, pAnsiChar(Ansistring(ActiveCircuit.BusList.Get(Busidx))), Len1);
      {Second bus}
            BusIdx := CktElement.Terminals^[2].busref;
            if BusIdx > 0 then
                with  ActiveCircuit.Buses^[BusIdx] do
                    if CoordDefined then
                        StrlCopy(Name2, pAnsiChar(Ansistring(ActiveCircuit.BusList.Get(Busidx))), Len2);
        end; {If CktElement}
    end;  {If ActiveCircuit}
end;

{====================================================================================================================}

procedure GetActiveElementVoltagesCallBack(var NumVoltages: Integer; V: pComplexArray); STDCALL;
{NumVoltages is size of the V buffer}
var
    i: Integer;
begin
    if Assigned(ActiveCircuit.ActiveCktElement) then
        with ActiveCircuit do
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
    if Assigned(ActiveCircuit.ActiveCktElement) then
        with ActiveCircuit do
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
    if Assigned(ActiveCircuit.ActiveCktElement) then
        with ActiveCircuit do
            with ActiveCktElement do
            begin
                GetLosses(TotalLosses, LoadLosses, NoLoadLosses);
            end;
end;

{====================================================================================================================}

procedure GetActiveElementPowerCallBack(Terminal: Integer; var TotalPower: Complex); STDCALL;
begin
    TotalPower := CZERO;
    if Assigned(ActiveCircuit.ActiveCktElement) then
        with ActiveCircuit do
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
    if Assigned(ActiveCircuit.ActiveCktElement) then
        if ActiveCircuit.ActiveCktElement is TPDElement then
        begin
            pDElem := ActiveCircuit.ActiveCktElement as TPDElement;
            NumCust := pDElem.BranchNumCustomers;
            TotalCust := pDElem.BranchTotalCustomers;
        end;
end;

{====================================================================================================================}

procedure GetActiveElementNodeRefCallBack(Maxsize: Integer; NodeReferenceArray: pIntegerArray); STDCALL;// calling program must allocate
var
    i: Integer;
begin
    if Assigned(ActiveCircuit.ActiveCktElement) then
        with ActiveCircuit do
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
    if Assigned(ActiveCircuit.ActiveCktElement) then
        with ActiveCircuit do
            with ActiveCktElement do
            begin
                Result := Terminals^[Terminal].BusRef;
            end;
end;

{====================================================================================================================}

procedure GetActiveElementTerminalInfoCallBack(var NumTerminals, NumConds, NumPhases: Integer); STDCALL;
begin
    if Assigned(ActiveCircuit.ActiveCktElement) then
        with ActiveCircuit do
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
    if Assigned(ActiveCircuit.ActiveCktElement) then
        with ActiveCircuit do
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
    if Assigned(ActiveCircuit) then
        if Assigned(ActiveCircuit.ActiveCktElement) then
            Result := ActiveCircuit.ActiveCktElement.ClassIndex;
end;

{====================================================================================================================}

function IsActiveElementEnabledCallBack: Boolean; STDCALL;

begin
    Result := FALSE;
    if Assigned(ActiveCircuit) then
        if Assigned(ActiveCircuit.ActiveCktElement) then
            Result := ActiveCircuit.ActiveCktElement.Enabled;
end;

{====================================================================================================================}

function IsBusCoordinateDefinedCallback(BusRef: Integer): Boolean; STDCALL;
begin
    Result := FALSE;
    if Assigned(ActiveCircuit) and (busRef > 0) then
        Result := ActiveCircuit.Buses^[BusRef].CoordDefined;
end;

{====================================================================================================================}
procedure GetBusCoordinateCallback(BusRef: Integer; var X, Y: Double); STDCALL;
begin
    X := 0.0;
    Y := 0.0;
    if Assigned(ActiveCircuit) and (busRef > 0) then
    begin
        X := ActiveCircuit.Buses^[BusRef].X;
        Y := ActiveCircuit.Buses^[BusRef].Y;
    end;
end;

{====================================================================================================================}
function GetBuskVBaseCallback(BusRef: Integer): Double; STDCALL;
begin
    Result := 0.0;
    if Assigned(ActiveCircuit) and (busRef > 0) then
    begin
        Result := ActiveCircuit.Buses^[BusRef].kVBase;
    end;
end;

{====================================================================================================================}
function GetBusDistFromMeterCallback(BusRef: Integer): Double; STDCALL;
begin
    Result := 0.0;
    if Assigned(ActiveCircuit) and (busRef > 0) then
    begin
        Result := ActiveCircuit.Buses^[BusRef].DistFromMeter;
    end;
end;

{====================================================================================================================}
procedure GetDynamicsStructCallBack(var DynamicsStruct: Pointer); STDCALL;
begin
    if Assigned(ActiveCircuit) then
    begin
        DynamicsStruct := @ActiveCircuit.Solution.DynaVars;
    end;

end;

{====================================================================================================================}
function GetStepSizeCallBack: Double; STDCALL;
begin
    Result := 0.0;
    if Assigned(ActiveCircuit) then
    begin
        Result := ActiveCircuit.Solution.DynaVars.h;
    end;
end;

{====================================================================================================================}
function GetTimeSecCallBack: Double; STDCALL;
begin
    Result := 0.0;
    if Assigned(ActiveCircuit) then
    begin
        Result := ActiveCircuit.Solution.DynaVars.t;
    end;

end;

{====================================================================================================================}
function GetTimeHrCallBack: Double; STDCALL;
begin
    Result := 0.0;
    if Assigned(ActiveCircuit) then
    begin
        Result := ActiveCircuit.Solution.DynaVars.dblHour;
    end;
end;

{====================================================================================================================}

procedure GetPublicDataPtrCallBack(var pPublicData: Pointer; var PublicDataBytes: Integer); STDCALL;

begin

    if Assigned(ActiveCircuit.ActiveCktElement) then
        with ActiveCircuit do
            with ActiveCktElement do
            begin
                pPublicData := PublicDataStruct;
                PublicDataBytes := PublicDataSize;
            end;

end;

function GetActiveElementNameCallBack(FullName: pAnsiChar; Maxlen: Cardinal): Integer; STDCALL;
{Maxlen is num of chars the calling program allocates for the string}

var
    S: String;
begin
    Result := 0;
    if Assigned(ActiveCircuit.ActiveCktElement) then
        with ActiveCircuit do
            with ActiveCktElement do
            begin
                S := ParentClass.Name + '.' + Name;

                StrlCopy(FullName, pAnsiChar(Ansistring(S)), Maxlen);
                Result := Length(FullName);
            end;
end;

function GetActiveElementPtrCallBack(): Pointer; STDCALL;  // Returns pointer to active circuit element
begin
    Result := Pointer(ActiveCircuit.ActiveCktElement);
end;

function ControlQueuePushCallBack(const Hour: Integer; const Sec: Double; const Code, ProxyHdl: Integer; Owner: Pointer): Integer; STDCALL;
begin
    Result := ActiveCircuit.ControlQueue.Push(Hour, Sec, Code, ProxyHdl, Owner);
end;

procedure GetResultStrCallBack(S: pAnsiChar; Maxlen: Cardinal); STDCALL;
begin
    StrlCopy(S, pAnsiChar(Ansistring(GlobalResult)), Maxlen);
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

    CallBackParser := TParser.Create;

{====================================================================================================================}

finalization

    CallBackParser.Free;

end.
