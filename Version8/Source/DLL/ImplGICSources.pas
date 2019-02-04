unit ImplGICSources;

{$WARN SYMBOL_PLATFORM OFF}

interface

uses
  ComObj, ActiveX, OpenDSSengine_TLB, StdVcl;

type
  TGICSources = class(TAutoObject, IGICSources)
  protected
    function Get_AllNames: OleVariant; safecall;
    function Get_Bus1: WideString; safecall;
    function Get_Bus2: WideString; safecall;
    function Get_Name: WideString; safecall;
    procedure Set_Name(const Value: WideString); safecall;
    function Get_Phases: Integer; safecall;
    procedure Set_Phases(Value: Integer); safecall;
    function Get_EN: Double; safecall;
    procedure Set_EN(Value: Double); safecall;
    function Get_EE: Double; safecall;
    procedure Set_EE(Value: Double); safecall;
    function Get_Lat1: Double; safecall;
    procedure Set_Lat1(Value: Double); safecall;
    function Get_Lat2: Double; safecall;
    procedure Set_Lat2(Value: Double); safecall;
    function Get_Lon1: Double; safecall;
    procedure Set_Lon1(Value: Double); safecall;
    function Get_Lon2: Double; safecall;
    procedure Set_Lon2(Value: Double); safecall;
    function Get_Volts: Double; safecall;
    procedure Set_Volts(Value: Double); safecall;
    function Get_Count: Integer; safecall;
    function Get_First: Integer; safecall;
    function Get_Next: Integer; safecall;

  end;

implementation

uses ComServ, Variants, DSSGlobals, GICSource, PointerList, CktElement;

function TGICSources.Get_AllNames: OleVariant;
Var
  GICElem      :TGICSourceObj;
  ElementList  :Tpointerlist;
  k:Integer;

Begin
    Result := VarArrayCreate([0, 0], varOleStr);
    Result[0] := 'NONE';
    IF ActiveCircuit[ActiveActor] <> Nil THEN
    Begin
       ElementList := GICsourceClass.ElementList;
       If ElementList.ListSize>0 Then
       Begin
         VarArrayRedim(Result, ElementList.ListSize-1);
         k:=0;
         GICElem := ElementList.First;
         WHILE GICElem<>Nil DO
         Begin
            Result[k] := GICElem.Name;
            Inc(k);
            GICElem := ElementList.Next;
         End;
       End;
    End;

end;

function TGICSources.Get_Bus1: WideString;
begin
    Result := '';
    IF ActiveCircuit[ActiveActor] <> NIL
    THEN Begin
         Result := ActiveCircuit[ActiveActor].ActiveCktElement.GetBus(1);
    End
end;

function TGICSources.Get_Bus2: WideString;
begin
    Result := '';
    IF ActiveCircuit[ActiveActor] <> NIL
    THEN Begin
         Result := ActiveCircuit[ActiveActor].ActiveCktElement.GetBus(2);
    End
end;

function TGICSources.Get_Name: WideString;

Begin
   Result := '';  // signify no name
   If ActiveCircuit[ActiveActor] <> Nil Then
   Begin
          Result := ActiveCircuit[ActiveActor].ActiveCktElement.Name;
   End;

end;

procedure TGICSources.Set_Name(const Value: WideString);
// Set element active by name
// Becomes the active circuit element

begin
     If ActiveCircuit[ActiveActor] <> Nil Then
     Begin
          If GICsourceClass.SetActive(Value) Then
          Begin
               ActiveCircuit[ActiveActor].ActiveCktElement := GICsourceClass.ElementList.Active ;
          End
          Else Begin
              DoSimpleMsg('Vsource "'+ Value +'" Not Found in Active Circuit.', 77003);
          End;
     End;

end;

function TGICSources.Get_Phases: Integer;
var
  elem: TGICsourceObj;
begin
  Result := 0;
  elem := GICSourceClass.ElementList.Active ;
  if elem <> nil then Begin
    Result := elem.nphases  ;
  End;
end;

procedure TGICSources.Set_Phases(Value: Integer);
var
  elem: TGICsourceObj;
begin
  elem := GICSourceClass.ElementList.Active ;
  if elem <> nil then Begin
    elem.nphases := Value ;
    Elem.NConds := Value;  // Force reallocation of terminal info
  End;
end;

function TGICSources.Get_EN: Double;
var
  elem: TGICsourceObj;
begin
  Result := 0.0;
  elem := GICSourceClass.ElementList.Active ;
  if elem <> nil then Result := elem.ENorth;
end;

procedure TGICSources.Set_EN(Value: Double);
var
  elem: TGICsourceObj;
begin
  elem := GICSourceClass.ElementList.Active ;
  if elem <> nil then elem.ENorth := Value ;
end;

function TGICSources.Get_EE: Double;
var
  elem: TGICsourceObj;
begin
  Result := 0.0;
  elem := GICSourceClass.ElementList.Active ;
  if elem <> nil then Result := elem.EEast;
end;

procedure TGICSources.Set_EE(Value: Double);
var
  elem: TGICsourceObj;
begin
  elem := GICSourceClass.ElementList.Active ;
  if elem <> nil then elem.EEast := Value ;
end;

function TGICSources.Get_Lat1: Double;
var
  elem: TGICsourceObj;
begin
  Result := 0.0;
  elem := GICSourceClass.ElementList.Active ;
  if elem <> nil then Result := elem.Lat1;

end;

procedure TGICSources.Set_Lat1(Value: Double);
var
  elem: TGICsourceObj;
begin
  elem := GICSourceClass.ElementList.Active ;
  if elem <> nil then Begin
     elem.Lat1 := Value ;
     elem.VoltsSpecified := FALSE;
  End;
end;

function TGICSources.Get_Lat2: Double;
var
  elem: TGICsourceObj;
begin
  Result := 0.0;
  elem := GICSourceClass.ElementList.Active ;
  if elem <> nil then Result := elem.Lat2;

end;

procedure TGICSources.Set_Lat2(Value: Double);
var
  elem: TGICsourceObj;
begin
  elem := GICSourceClass.ElementList.Active ;
  if elem <> nil then Begin
     elem.Lat2 := Value ;
     elem.VoltsSpecified := FALSE;
  End;

end;

function TGICSources.Get_Lon1: Double;
var
  elem: TGICsourceObj;
begin
  Result := 0.0;
  elem := GICSourceClass.ElementList.Active ;
  if elem <> nil then Result := elem.Lon1;

end;

procedure TGICSources.Set_Lon1(Value: Double);
var
  elem: TGICsourceObj;
begin
  elem := GICSourceClass.ElementList.Active ;
  if elem <> nil then Begin
     elem.Lon1 := Value ;
     elem.VoltsSpecified := FALSE;
  End;

end;

function TGICSources.Get_Lon2: Double;
var
  elem: TGICsourceObj;
begin
  Result := 0.0;
  elem := GICSourceClass.ElementList.Active ;
  if elem <> nil then Result := elem.Lon2;

end;

procedure TGICSources.Set_Lon2(Value: Double);
var
  elem: TGICsourceObj;
begin
  elem := GICSourceClass.ElementList.Active ;
  if elem <> nil then Begin
     elem.Lon2 := Value ;
     elem.VoltsSpecified := FALSE;
  End;

end;

function TGICSources.Get_Volts: Double;
var
  elem: TGICsourceObj;
begin
  Result := 0.0;
  elem := GICSourceClass.ElementList.Active ;
  if elem <> nil then Result := elem.Volts;
end;

procedure TGICSources.Set_Volts(Value: Double);
var
  elem: TGICsourceObj;
begin
  elem := GICSourceClass.ElementList.Active ;
  if elem <> nil then Begin
     elem.Volts := Value ;
     elem.VoltsSpecified := TRUE;
  End;
end;

function TGICSources.Get_Count: Integer;
Begin
     Result := 0;
     If ActiveCircuit[ActiveActor] <> Nil Then
        Result := GICSourceClass.ElementList.ListSize;
end;

function TGICSources.Get_First: Integer;
Var
   pElem : TGICsourceObj;
begin
     Result := 0;
     If ActiveCircuit[ActiveActor] <> Nil Then
     Begin
        pElem := GICSourceClass.ElementList.First;
        If pElem <> Nil Then
        Repeat
          If pElem.Enabled Then Begin
              ActiveCircuit[ActiveActor].ActiveCktElement := pElem;
              Result := 1;
          End
          Else pElem := GICSourceClass.ElementList.Next;
        Until (Result = 1) or (pElem = nil);
     End;
end;

function TGICSources.Get_Next: Integer;
Var
   pElem : TGICsourceObj;
begin
     Result := 0;
     If ActiveCircuit[ActiveActor] <> Nil Then
     Begin
        pElem := GICSourceClass.ElementList.Next;
        If pElem <> Nil Then
        Repeat
          If pElem.Enabled Then Begin
              ActiveCircuit[ActiveActor].ActiveCktElement := pElem;
              Result := GICSourceClass.ElementList.ActiveIndex;
          End
          Else pElem := GICSourceClass.ElementList.Next;
        Until (Result > 0) or (pElem = nil);
     End;
end;

initialization
  TAutoObjectFactory.Create(ComServer, TGICSources, Class_GICSources,
    ciInternal, tmApartment);
end.
