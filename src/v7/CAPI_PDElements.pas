unit CAPI_PDElements;

{$inline on}

interface

uses
    CAPI_Utils;

function PDElements_Get_Count(): Integer; CDECL;
function PDElements_Get_FaultRate(): Double; CDECL;
function PDElements_Get_First(): Integer; CDECL;
function PDElements_Get_IsShunt(): Wordbool; CDECL;
function PDElements_Get_Next(): Integer; CDECL;
function PDElements_Get_pctPermanent(): Double; CDECL;
procedure PDElements_Set_FaultRate(Value: Double); CDECL;
procedure PDElements_Set_pctPermanent(Value: Double); CDECL;
function PDElements_Get_Name(): PAnsiChar; CDECL;
procedure PDElements_Set_Name(const Value: PAnsiChar); CDECL;
function PDElements_Get_AccumulatedL(): Double; CDECL;
function PDElements_Get_Lambda(): Double; CDECL;
function PDElements_Get_Numcustomers(): Integer; CDECL;
function PDElements_Get_ParentPDElement(): Integer; CDECL;
function PDElements_Get_RepairTime(): Double; CDECL;
function PDElements_Get_Totalcustomers(): Integer; CDECL;
function PDElements_Get_FromTerminal(): Integer; CDECL;
function PDElements_Get_TotalMiles(): Double; CDECL;
function PDElements_Get_SectionID(): Integer; CDECL;
procedure PDElements_Set_RepairTime(Value: Double); CDECL;

procedure PDElements_Get_AllNames(var ResultPtr: PPAnsiChar; ResultCount: PInteger); CDECL;
procedure PDElements_Get_AllNames_GR(); CDECL;
procedure PDElements_Get_AllMaxCurrents(var ResultPtr: PDouble; ResultCount: PInteger); CDECL;
procedure PDElements_Get_AllMaxCurrents_GR(); CDECL;
procedure PDElements_Get_AllPctNorm(var ResultPtr: PDouble; ResultCount: PInteger); CDECL;
procedure PDElements_Get_AllPctNorm_GR(); CDECL;
procedure PDElements_Get_AllPctEmerg(var ResultPtr: PDouble; ResultCount: PInteger); CDECL;
procedure PDElements_Get_AllPctEmerg_GR(); CDECL;

implementation

uses
    CAPI_Constants,
    DSSGlobals,
    PDElement,
    PDClass,
    SysUtils,
    PointerList,
    Bus,
    XYCurve,
    ucomplex,
    ArrayDef,
    Utilities;
    
type
    PDoubleArray = CAPI_Utils.PDoubleArray;

function PDElements_Get_Count(): Integer; CDECL;
begin
    Result := 0;
    if ActiveCircuit = NIL then
        Exit;
    
    Result := ActiveCircuit.PDElements.ListSize;
end;
//------------------------------------------------------------------------------
function PDElements_Get_FaultRate(): Double; CDECL;
var
    ActivePDElement: TPDElement;
begin
    Result := 0.0;
    if ActiveCircuit = NIL then
        Exit;
        
    with ActiveCircuit do
        if ActiveCktElement is TPDElement then
        begin
            ActivePDElement := ActiveCktelement as TPDElement;
            Result := ActivePDElement.Faultrate;
        end;
end;
//------------------------------------------------------------------------------
function PDElements_Get_First(): Integer; CDECL;
var
    ActivePDElement: TPDElement;
begin
    Result := 0;
    if ActiveCircuit = NIL then 
        Exit;
        
    with ActiveCircuit do
    begin
        ActivePDElement := PDElements.First;
        if ActivePDElement <> NIL then
        begin
            repeat
                if ActivePDElement.enabled then
                begin
                    Result := 1;
                    ActiveCktElement := ActivePDElement;
                end
                else
                    ActivePDElement := PDElements.Next;
            until (Result = 1) or (ActivePDELement = NIL);
        end;
    end;
end;
//------------------------------------------------------------------------------
function PDElements_Get_IsShunt(): Wordbool; CDECL;
var
    ActivePDElement: TPDElement;
begin
    Result := FALSE;
    if ActiveCircuit = NIL then
        Exit;
        
    with ActiveCircuit do
        if ActiveCktElement is TPDElement then
        begin
            ActivePDElement := ActiveCktelement as TPDElement;
            Result := ActivePDElement.IsShunt;
        end;
end;
//------------------------------------------------------------------------------
function PDElements_Get_Next(): Integer; CDECL;
var
    ActivePDElement: TPDElement;
begin
    Result := 0;
    if ActiveCircuit = NIL then
        Exit;
        
    with ActiveCircuit do
    begin
        ActivePDElement := PDElements.Next;
        if ActivePDElement <> NIL then
        begin
            repeat
                if ActivePDElement.enabled then
                begin
                    Result := 1;
                    ActiveCktElement := ActivePDElement;
                end
                else
                    ActivePDElement := PDElements.Next;
            until (Result = 1) or (ActivePDELement = NIL);
        end;
    end;
end;
//------------------------------------------------------------------------------
function PDElements_Get_pctPermanent(): Double; CDECL;
var
    ActivePDElement: TPDElement;
begin
    Result := 0.0;
    if ActiveCircuit = NIL then
        Exit;
        
    with ActiveCircuit do
        if ActiveCktElement is TPDElement then
        begin
            ActivePDElement := ActiveCktelement as TPDElement;
            Result := ActivePDElement.PctPerm;
        end;
end;
//------------------------------------------------------------------------------
procedure PDElements_Set_FaultRate(Value: Double); CDECL;
var
    ActivePDElement: TPDElement;
begin
    if ActiveCircuit = NIL then
        Exit;
        
    with ActiveCircuit do
        if ActiveCktElement is TPDElement then
        begin
            ActivePDElement := ActiveCktelement as TPDElement;
            ActivePDElement.FaultRate := Value;
        end;
end;
//------------------------------------------------------------------------------
procedure PDElements_Set_pctPermanent(Value: Double); CDECL;
var
    ActivePDElement: TPDElement;
begin
    if ActiveCircuit = NIL then
        Exit;
        
    with ActiveCircuit do
        if ActiveCktElement is TPDElement then
        begin
            ActivePDElement := ActiveCktelement as TPDElement;
            ActivePDElement.PctPerm := Value;
        end;
end;
//------------------------------------------------------------------------------
function PDElements_Get_Name_AnsiString(): Ansistring; inline;
var
    ActivePDElement: TPDElement;
begin
    Result := '';   // return null if not a PD element
    if ActiveCircuit = NIL then
        Exit;
        
    with ActiveCircuit do
        if ActiveCktElement is TPDElement then
        begin
            ActivePDElement := ActiveCktelement as TPDElement;
            with ActivePDElement do
                Result := Format('%s.%s', [Parentclass.Name, Name]);  // full name
        end;
end;

function PDElements_Get_Name(): PAnsiChar; CDECL;
begin
    Result := DSS_GetAsPAnsiChar(PDElements_Get_Name_AnsiString());
end;
//------------------------------------------------------------------------------
procedure PDElements_Set_Name(const Value: PAnsiChar); CDECL; //TODO: rewrite to use a hashmap?
var
    ActivePDElement: TPDElement;
    TestString: String;
begin
    if ActiveCircuit = NIL then
        Exit;
        
    with ActiveCircuit do
    begin
        TestString := Value;
        // Search through list of PD Elements until we find this one
        ActivePDElement := PDElements.First;
        while Assigned(ActivePDElement) do
            with ActivePDelement do
            begin
                if (CompareText(TestString, Format('%s.%s', [Parentclass.Name, Name])) = 0) then
                begin
                    ActiveCktElement := ActivePDElement;
                    Break;
                end;
                ActivePDElement := PDElements.Next;
            end;
    end;
end;
//------------------------------------------------------------------------------
function PDElements_Get_AccumulatedL(): Double; CDECL;
var
    ActivePDElement: TPDElement;
begin
    Result := 0.0;
    if ActiveCircuit = NIL then
        Exit;
        
    with ActiveCircuit do
        if ActiveCktElement is TPDElement then
        begin
            ActivePDElement := ActiveCktelement as TPDElement;
            Result := ActivePDElement.AccumulatedBrFltRate;
        end;
end;
//------------------------------------------------------------------------------
function PDElements_Get_Lambda(): Double; CDECL;
var
    ActivePDElement: TPDElement;
begin
    Result := 0.0;
    if ActiveCircuit = NIL then
        Exit;
        
    with ActiveCircuit do
        if ActiveCktElement is TPDElement then
        begin
            ActivePDElement := ActiveCktelement as TPDElement;
            Result := ActivePDElement.BranchFltRate;
        end;
end;
//------------------------------------------------------------------------------
function PDElements_Get_Numcustomers(): Integer; CDECL;
var
    ActivePDElement: TPDElement;
begin
    Result := 0;
    if ActiveCircuit = NIL then
        Exit;
        
    with ActiveCircuit do
        if ActiveCktElement is TPDElement then
        begin
            ActivePDElement := ActiveCktelement as TPDElement;
            Result := ActivePDElement.BranchNumCustomers;
        end;
end;
//------------------------------------------------------------------------------
function PDElements_Get_ParentPDElement(): Integer; CDECL;
var
    ActivePDElement: TPDElement;
begin
    Result := 0;
    if ActiveCircuit = NIL then
        Exit;
        
    with ActiveCircuit do
        if ActiveCktElement is TPDElement then
        begin
            ActivePDElement := ActiveCktelement as TPDElement;
            if ActivePDElement.ParentPDElement <> NIL then    // leaves ActiveCktElement as is
            begin
                ActiveCktElement := ActivePDElement.ParentPDElement;
                Result := ActivecktElement.ClassIndex;
            end;
        end;
end;
//------------------------------------------------------------------------------
function PDElements_Get_RepairTime(): Double; CDECL;
var
    ActivePDElement: TPDElement;
begin
    Result := 0.0;
    if ActiveCircuit = NIL then
        Exit;
        
    with ActiveCircuit do
        if ActiveCktElement is TPDElement then
        begin
            ActivePDElement := ActiveCktelement as TPDElement;
            Result := ActivePDElement.HrsToRepair;
        end;
end;
//------------------------------------------------------------------------------
function PDElements_Get_Totalcustomers(): Integer; CDECL;
var
    ActivePDElement: TPDElement;
begin
    Result := 0;
    if ActiveCircuit = NIL then
        Exit;
        
    with ActiveCircuit do
        if ActiveCktElement is TPDElement then
        begin
            ActivePDElement := ActiveCktelement as TPDElement;
            Result := ActivePDElement.BranchTotalCustomers;
        end;
end;
//------------------------------------------------------------------------------
function PDElements_Get_FromTerminal(): Integer; CDECL;
var
    ActivePDElement: TPDElement;
begin
    Result := 0;
    if ActiveCircuit = NIL then
        Exit;
        
    with ActiveCircuit do
        if ActiveCktElement is TPDElement then
        begin
            ActivePDElement := ActiveCktelement as TPDElement;
            Result := ActivePDElement.FromTerminal;
        end;
end;
//------------------------------------------------------------------------------
function PDElements_Get_TotalMiles(): Double; CDECL;
// Total miles of line from here on down to the end of the feeder
var
    ActivePDElement: TPDElement;
begin
    Result := 0.0;
    if ActiveCircuit = NIL then
        Exit;
        
    with ActiveCircuit do
        if ActiveCktElement is TPDElement then
        begin
            ActivePDElement := ActiveCktelement as TPDElement;
            Result := ActivePDElement.AccumulatedMilesDownStream;
        end;
end;
//------------------------------------------------------------------------------
function PDElements_Get_SectionID(): Integer; CDECL;
var
    ActivePDElement: TPDElement;
begin
    Result := 0;
    if ActiveCircuit = NIL then
        Exit;
        
    with ActiveCircuit do
        if ActiveCktElement is TPDElement then
        begin
            ActivePDElement := ActiveCktelement as TPDElement;
            Result := ActivePDElement.BranchSectionID;
        end;
end;
//------------------------------------------------------------------------------
procedure PDElements_Set_RepairTime(Value: Double); CDECL;
var
    ActivePDElement: TPDElement;
begin
    if ActiveCircuit = NIL then
        Exit;
        
    with ActiveCircuit do
        if ActiveCktElement is TPDElement then
        begin
            ActivePDElement := ActiveCktelement as TPDElement;
            ActivePDElement.HrsToRepair := Value;
        end;
end;
//------------------------------------------------------------------------------
procedure PDElements_Get_AllNames(var ResultPtr: PPAnsiChar; ResultCount: PInteger); CDECL;
var
    Result: PPAnsiCharArray;
    idx_before, k: Integer;
    elem: TPDElement;
    pList: TPointerList;
begin
    Result := DSS_RecreateArray_PPAnsiChar(ResultPtr, ResultCount, 1);
    Result[0] := DSS_CopyStringAsPChar('NONE');
    if ActiveCircuit = NIL then
        Exit;

    pList := ActiveCircuit.PDElements;
    if pList.ListSize <= 0 then
        Exit;
        
    DSS_RecreateArray_PPAnsiChar(Result, ResultPtr, ResultCount, pList.ListSize);
    idx_before := pList.ActiveIndex;
    k := 0;
    elem := pList.First;
    while elem <> NIL do
    begin
        Result[k] := DSS_CopyStringAsPChar(elem.DSSClassName + '.' + elem.Name);
        Inc(k);
        elem := pList.Next;
    end;
    if (idx_before > 0) and (idx_before <= pList.ListSize) then
        pList.Get(idx_before);
end;

procedure PDElements_Get_AllNames_GR(); CDECL;
// Same as PDElements_Get_AllNames but uses global result (GR) pointers
begin
    PDElements_Get_AllNames(GR_DataPtr_PPAnsiChar, GR_CountPtr_PPAnsiChar);
end;
//------------------------------------------------------------------------------
function _PDElements_Get_pctCapacity_for(const What: integer; pElem: TPDElement; cBuffer: pComplexArray): Double; inline;
var
    RatingIdx,
    i: Integer;
    EmergAmps,
    NormAmps,
    Currmag,
    MaxCurrent: Double;
    RSignal: TXYCurveObj;
begin
    Result := 0;
    MaxCurrent := 0.0;
    for  i := 1 to pElem.Nphases do
    begin
        Currmag := Cabs(Cbuffer^[i]);
        if Currmag > MaxCurrent then
            MaxCurrent := Currmag;
    end;
    if What = 0 then
    begin
        Result := MaxCurrent;
        Exit;
    end;
    
    NormAmps := pElem.NormAmps;
    EmergAmps := pElem.EmergAmps;

    if SeasonalRating then
    begin
        if SeasonSignal <> '' then
        begin
            RSignal := XYCurveClass.Find(SeasonSignal);
            if RSignal <> NIL then
            begin
                RatingIdx := trunc(RSignal.GetYValue(ActiveCircuit.Solution.DynaVars.intHour));
                // Brings the seasonal ratings for the PDElement
                if (RatingIdx <= PElem.NumAmpRatings) and (PElem.NumAmpRatings > 1) then
                begin
                    NormAmps := pElem.AmpRatings[RatingIdx];
                    EmergAmps := pElem.AmpRatings[RatingIdx];
                end;
            end
            else
                SeasonalRating := FALSE;   // The XYCurve defined doesn't exist
        end
        else
            SeasonalRating := FALSE;    // The user didn't define the seasonal signal
    end;
    
    case What of
        1: if NormAmps <> 0 then Result := 100 * MaxCurrent / NormAmps;
        2: if EmergAmps <> 0 then Result := 100 * MaxCurrent / EmergAmps;
    end;
end;

procedure _PDElements_Get_x(var ResultPtr: PDouble; ResultCount: PInteger; const What: integer);
// Internal helper function to calculate for all PDElements
// MaxCurrent (0), CapacityNorm (1), CapacityEmerg (2), Power (3)
var
    Result: CAPI_Utils.PDoubleArray;
    k, idx_before: Integer;
    pElem: TPDElement;
    pList: TPointerList;
    LocalPower: Complex;
    cBuffer: pComplexArray;
begin
    cBuffer := NIL;
    if (ActiveCircuit = NIL) or (ActiveCircuit.PDElements.ListSize <= 0) then 
    begin
        Result := DSS_RecreateArray_PDouble(ResultPtr, ResultCount, 1);
        Result[0] := -1;
        Exit;
    end;

    pList := ActiveCircuit.PDElements;
    idx_before := pList.ActiveIndex;
    k := 0;
    pElem := pList.First;
    case What of
    3: 
        begin
            Result := DSS_RecreateArray_PDouble(ResultPtr, ResultCount, pList.ListSize * 2); // complex
            while pElem <> NIL do
            begin
                LocalPower := pElem.Power[1];
                Result[k] := Localpower.re * 0.001;
                Result[k + 1] := Localpower.im * 0.001;
                Inc(k, 2);
                pElem := pList.Next;
            end;
        end;
    0, 1, 2:
        try
            Getmem(cBuffer, sizeof(Complex) * GetMaxCktElementSize);
            Result := DSS_RecreateArray_PDouble(ResultPtr, ResultCount, pList.ListSize); // real
            while pElem <> NIL do
            begin
                pElem.GetCurrents(cBuffer);
                Result[k] := _PDElements_Get_pctCapacity_for(What, pElem, cBuffer);
                Inc(k);
                pElem := pList.Next;
            end;
        finally
            if Assigned(cBuffer) then
                Freemem(cBuffer);
        end;
    end;
    
    if (idx_before > 0) and (idx_before <= pList.ListSize) then
        pList.Get(idx_before);
end;

//------------------------------------------------------------------------------
procedure PDElements_Get_AllMaxCurrents(var ResultPtr: PDouble; ResultCount: PInteger); CDECL;
begin
    _PDElements_Get_x(ResultPtr, ResultCount, 0);
end;

procedure PDElements_Get_AllMaxCurrents_GR(); CDECL;
// Same as PDElements_Get_AllMaxCurrents but uses global result (GR) pointers
begin
    PDElements_Get_AllMaxCurrents(GR_DataPtr_PDouble, GR_CountPtr_PDouble)
end;

//------------------------------------------------------------------------------
procedure PDElements_Get_AllPctNorm(var ResultPtr: PDouble; ResultCount: PInteger); CDECL;
begin
    _PDElements_Get_x(ResultPtr, ResultCount, 1);
end;

procedure PDElements_Get_AllPctNorm_GR(); CDECL;
// Same as PDElements_Get_AllPctNorm but uses global result (GR) pointers
begin
    PDElements_Get_AllPctNorm(GR_DataPtr_PDouble, GR_CountPtr_PDouble)
end;

//------------------------------------------------------------------------------
procedure PDElements_Get_AllPctEmerg(var ResultPtr: PDouble; ResultCount: PInteger); CDECL;
begin
    _PDElements_Get_x(ResultPtr, ResultCount, 2);
end;

procedure PDElements_Get_AllPctEmerg_GR(); CDECL;
// Same as PDElements_Get_AllPctEmerg but uses global result (GR) pointers
begin
    PDElements_Get_AllPctEmerg(GR_DataPtr_PDouble, GR_CountPtr_PDouble)
end;

//------------------------------------------------------------------------------
end.
