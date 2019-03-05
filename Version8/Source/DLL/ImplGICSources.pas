unit ImplGICSources;

{$WARN SYMBOL_PLATFORM OFF}

interface

uses
    ComObj,
    ActiveX,
    OpenDSSengine_TLB,
    StdVcl;

type
    TGICSources = class(TAutoObject, IGICSources)
    PROTECTED
        function Get_AllNames: Olevariant; SAFECALL;
        function Get_Bus1: Widestring; SAFECALL;
        function Get_Bus2: Widestring; SAFECALL;
        function Get_Name: Widestring; SAFECALL;
        procedure Set_Name(const Value: Widestring); SAFECALL;
        function Get_Phases: Integer; SAFECALL;
        procedure Set_Phases(Value: Integer); SAFECALL;
        function Get_EN: Double; SAFECALL;
        procedure Set_EN(Value: Double); SAFECALL;
        function Get_EE: Double; SAFECALL;
        procedure Set_EE(Value: Double); SAFECALL;
        function Get_Lat1: Double; SAFECALL;
        procedure Set_Lat1(Value: Double); SAFECALL;
        function Get_Lat2: Double; SAFECALL;
        procedure Set_Lat2(Value: Double); SAFECALL;
        function Get_Lon1: Double; SAFECALL;
        procedure Set_Lon1(Value: Double); SAFECALL;
        function Get_Lon2: Double; SAFECALL;
        procedure Set_Lon2(Value: Double); SAFECALL;
        function Get_Volts: Double; SAFECALL;
        procedure Set_Volts(Value: Double); SAFECALL;
        function Get_Count: Integer; SAFECALL;
        function Get_First: Integer; SAFECALL;
        function Get_Next: Integer; SAFECALL;

    end;

implementation

uses
    ComServ,
    Variants,
    DSSGlobals,
    GICSource,
    PointerList,
    CktElement;

function TGICSources.Get_AllNames: Olevariant;
var
    GICElem: TGICSourceObj;
    ElementList: Tpointerlist;
    k: Integer;

begin
    Result := VarArrayCreate([0, 0], varOleStr);
    Result[0] := 'NONE';
    if ActiveCircuit[ActiveActor] <> NIL then
    begin
        ElementList := GICsourceClass.ElementList;
        if ElementList.ListSize > 0 then
        begin
            VarArrayRedim(Result, ElementList.ListSize - 1);
            k := 0;
            GICElem := ElementList.First;
            while GICElem <> NIL do
            begin
                Result[k] := GICElem.Name;
                Inc(k);
                GICElem := ElementList.Next;
            end;
        end;
    end;

end;

function TGICSources.Get_Bus1: Widestring;
begin
    Result := '';
    if ActiveCircuit[ActiveActor] <> NIL then
    begin
        Result := ActiveCircuit[ActiveActor].ActiveCktElement.GetBus(1);
    end
end;

function TGICSources.Get_Bus2: Widestring;
begin
    Result := '';
    if ActiveCircuit[ActiveActor] <> NIL then
    begin
        Result := ActiveCircuit[ActiveActor].ActiveCktElement.GetBus(2);
    end
end;

function TGICSources.Get_Name: Widestring;

begin
    Result := '';  // signify no name
    if ActiveCircuit[ActiveActor] <> NIL then
    begin
        Result := ActiveCircuit[ActiveActor].ActiveCktElement.Name;
    end;

end;

procedure TGICSources.Set_Name(const Value: Widestring);
// Set element active by name
// Becomes the active circuit element

begin
    if ActiveCircuit[ActiveActor] <> NIL then
    begin
        if GICsourceClass.SetActive(Value) then
        begin
            ActiveCircuit[ActiveActor].ActiveCktElement := GICsourceClass.ElementList.Active;
        end
        else
        begin
            DoSimpleMsg('Vsource "' + Value + '" Not Found in Active Circuit.', 77003);
        end;
    end;

end;

function TGICSources.Get_Phases: Integer;
var
    elem: TGICsourceObj;
begin
    Result := 0;
    elem := GICSourceClass.ElementList.Active;
    if elem <> NIL then
    begin
        Result := elem.nphases;
    end;
end;

procedure TGICSources.Set_Phases(Value: Integer);
var
    elem: TGICsourceObj;
begin
    elem := GICSourceClass.ElementList.Active;
    if elem <> NIL then
    begin
        elem.nphases := Value;
        Elem.NConds := Value;  // Force reallocation of terminal info
    end;
end;

function TGICSources.Get_EN: Double;
var
    elem: TGICsourceObj;
begin
    Result := 0.0;
    elem := GICSourceClass.ElementList.Active;
    if elem <> NIL then
        Result := elem.ENorth;
end;

procedure TGICSources.Set_EN(Value: Double);
var
    elem: TGICsourceObj;
begin
    elem := GICSourceClass.ElementList.Active;
    if elem <> NIL then
        elem.ENorth := Value;
end;

function TGICSources.Get_EE: Double;
var
    elem: TGICsourceObj;
begin
    Result := 0.0;
    elem := GICSourceClass.ElementList.Active;
    if elem <> NIL then
        Result := elem.EEast;
end;

procedure TGICSources.Set_EE(Value: Double);
var
    elem: TGICsourceObj;
begin
    elem := GICSourceClass.ElementList.Active;
    if elem <> NIL then
        elem.EEast := Value;
end;

function TGICSources.Get_Lat1: Double;
var
    elem: TGICsourceObj;
begin
    Result := 0.0;
    elem := GICSourceClass.ElementList.Active;
    if elem <> NIL then
        Result := elem.Lat1;

end;

procedure TGICSources.Set_Lat1(Value: Double);
var
    elem: TGICsourceObj;
begin
    elem := GICSourceClass.ElementList.Active;
    if elem <> NIL then
    begin
        elem.Lat1 := Value;
        elem.VoltsSpecified := FALSE;
    end;
end;

function TGICSources.Get_Lat2: Double;
var
    elem: TGICsourceObj;
begin
    Result := 0.0;
    elem := GICSourceClass.ElementList.Active;
    if elem <> NIL then
        Result := elem.Lat2;

end;

procedure TGICSources.Set_Lat2(Value: Double);
var
    elem: TGICsourceObj;
begin
    elem := GICSourceClass.ElementList.Active;
    if elem <> NIL then
    begin
        elem.Lat2 := Value;
        elem.VoltsSpecified := FALSE;
    end;

end;

function TGICSources.Get_Lon1: Double;
var
    elem: TGICsourceObj;
begin
    Result := 0.0;
    elem := GICSourceClass.ElementList.Active;
    if elem <> NIL then
        Result := elem.Lon1;

end;

procedure TGICSources.Set_Lon1(Value: Double);
var
    elem: TGICsourceObj;
begin
    elem := GICSourceClass.ElementList.Active;
    if elem <> NIL then
    begin
        elem.Lon1 := Value;
        elem.VoltsSpecified := FALSE;
    end;

end;

function TGICSources.Get_Lon2: Double;
var
    elem: TGICsourceObj;
begin
    Result := 0.0;
    elem := GICSourceClass.ElementList.Active;
    if elem <> NIL then
        Result := elem.Lon2;

end;

procedure TGICSources.Set_Lon2(Value: Double);
var
    elem: TGICsourceObj;
begin
    elem := GICSourceClass.ElementList.Active;
    if elem <> NIL then
    begin
        elem.Lon2 := Value;
        elem.VoltsSpecified := FALSE;
    end;

end;

function TGICSources.Get_Volts: Double;
var
    elem: TGICsourceObj;
begin
    Result := 0.0;
    elem := GICSourceClass.ElementList.Active;
    if elem <> NIL then
        Result := elem.Volts;
end;

procedure TGICSources.Set_Volts(Value: Double);
var
    elem: TGICsourceObj;
begin
    elem := GICSourceClass.ElementList.Active;
    if elem <> NIL then
    begin
        elem.Volts := Value;
        elem.VoltsSpecified := TRUE;
    end;
end;

function TGICSources.Get_Count: Integer;
begin
    Result := 0;
    if ActiveCircuit[ActiveActor] <> NIL then
        Result := GICSourceClass.ElementList.ListSize;
end;

function TGICSources.Get_First: Integer;
var
    pElem: TGICsourceObj;
begin
    Result := 0;
    if ActiveCircuit[ActiveActor] <> NIL then
    begin
        pElem := GICSourceClass.ElementList.First;
        if pElem <> NIL then
            repeat
                if pElem.Enabled then
                begin
                    ActiveCircuit[ActiveActor].ActiveCktElement := pElem;
                    Result := 1;
                end
                else
                    pElem := GICSourceClass.ElementList.Next;
            until (Result = 1) or (pElem = NIL);
    end;
end;

function TGICSources.Get_Next: Integer;
var
    pElem: TGICsourceObj;
begin
    Result := 0;
    if ActiveCircuit[ActiveActor] <> NIL then
    begin
        pElem := GICSourceClass.ElementList.Next;
        if pElem <> NIL then
            repeat
                if pElem.Enabled then
                begin
                    ActiveCircuit[ActiveActor].ActiveCktElement := pElem;
                    Result := GICSourceClass.ElementList.ActiveIndex;
                end
                else
                    pElem := GICSourceClass.ElementList.Next;
            until (Result > 0) or (pElem = NIL);
    end;
end;

initialization
    TAutoObjectFactory.Create(ComServer, TGICSources, Class_GICSources,
        ciInternal, tmApartment);
end.
