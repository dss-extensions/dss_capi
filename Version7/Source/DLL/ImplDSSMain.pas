unit ImplDSSMain;

interface

uses
    ComObj,
    ActiveX,
    DSS_TLB;

type
    TDSSMain = class(TAutoObject, IDSSMain)
    PROTECTED
        function Get_ActiveCircuit: Widestring; SAFECALL;
        function Get_Circuits(Index: Olevariant): ICircuit; SAFECALL;
        function Get_NumCircuits: Integer; SAFECALL;
        procedure Set_ActiveCircuit(const Value: Widestring); SAFECALL;
    end;

implementation

uses
    ComServ,
    DSSGlobals,
    Executive,
    sysUtils;

function TDSSMain.Get_ActiveCircuit: Widestring;
begin
    Result := ActiveCircuit.Name;
end;

function TDSSMain.Get_Circuits(Index: Olevariant): ICircuit;
begin

    case (VarType(Index) and varTypeMask) of
        VarInteger:
        begin

            if (Circuits.ListSize >= Integer(Index)) and (Integer(index) > 0) then
                ActiveCircuit := Circuits.Get(Integer(Index))
            else
                DoSimpleMsg('Circuit index requested (' + IntToStr(Index) + ') is invalid');

        end;
        VarOleStr:
        begin
            DSSExecutive.SetActiveCircuit(String(index));
        end;
    end;


end;

function TDSSMain.Get_NumCircuits: Integer;
begin
    Result := NumCircuits;
end;

procedure TDSSMain.Set_ActiveCircuit(const Value: Widestring);
begin
    DSSExecutive.SetActiveCircuit(String(Value));
end;

initialization
    TAutoObjectFactory.Create(ComServer, TDSSMain, Class_DSSMain, ciMultiInstance);
end.
