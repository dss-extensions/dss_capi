unit ImplTransformers;

{
  ----------------------------------------------------------
  Copyright (c) 2008-2015, Electric Power Research Institute, Inc.
  All rights reserved.
  ----------------------------------------------------------
}
{$WARN SYMBOL_PLATFORM OFF}

interface

uses
    ComObj,
    ActiveX,
    OpenDSSengine_TLB,
    StdVcl;

type
    TTransformers = class(TAutoObject, ITransformers)
    PROTECTED
        function Get_AllNames: Olevariant; SAFECALL;
        function Get_First: Integer; SAFECALL;
        function Get_IsDelta: Wordbool; SAFECALL;
        function Get_kV: Double; SAFECALL;
        function Get_kVA: Double; SAFECALL;
        function Get_MaxTap: Double; SAFECALL;
        function Get_MinTap: Double; SAFECALL;
        function Get_Name: Widestring; SAFECALL;
        function Get_Next: Integer; SAFECALL;
        function Get_NumTaps: Integer; SAFECALL;
        function Get_NumWindings: Integer; SAFECALL;
        function Get_R: Double; SAFECALL;
        function Get_Rneut: Double; SAFECALL;
        function Get_Tap: Double; SAFECALL;
        function Get_Wdg: Integer; SAFECALL;
        function Get_XfmrCode: Widestring; SAFECALL;
        function Get_Xhl: Double; SAFECALL;
        function Get_Xht: Double; SAFECALL;
        function Get_Xlt: Double; SAFECALL;
        function Get_Xneut: Double; SAFECALL;
        procedure Set_IsDelta(Value: Wordbool); SAFECALL;
        procedure Set_kV(Value: Double); SAFECALL;
        procedure Set_kVA(Value: Double); SAFECALL;
        procedure Set_MaxTap(Value: Double); SAFECALL;
        procedure Set_MinTap(Value: Double); SAFECALL;
        procedure Set_Name(const Value: Widestring); SAFECALL;
        procedure Set_NumTaps(Value: Integer); SAFECALL;
        procedure Set_NumWindings(Value: Integer); SAFECALL;
        procedure Set_R(Value: Double); SAFECALL;
        procedure Set_Rneut(Value: Double); SAFECALL;
        procedure Set_Tap(Value: Double); SAFECALL;
        procedure Set_Wdg(Value: Integer); SAFECALL;
        procedure Set_XfmrCode(const Value: Widestring); SAFECALL;
        procedure Set_Xhl(Value: Double); SAFECALL;
        procedure Set_Xht(Value: Double); SAFECALL;
        procedure Set_Xlt(Value: Double); SAFECALL;
        procedure Set_Xneut(Value: Double); SAFECALL;
        function Get_Count: Integer; SAFECALL;
        function Get_WdgVoltages: Olevariant; SAFECALL;
        function Get_WdgCurrents: Olevariant; SAFECALL;
        function Get_strWdgCurrents: Widestring; SAFECALL;
        function Get_CoreType: Integer; SAFECALL;
        procedure Set_CoreType(Value: Integer); SAFECALL;
        function Get_RdcOhms: Double; SAFECALL;
        procedure Set_RdcOhms(Value: Double); SAFECALL;
    end;

implementation

uses
    ComServ,
    DSSGlobals,
    Executive,
    Transformer,
    Variants,
    SysUtils,
    PointerList,
    ucomplex;

function ActiveTransformer: TTransfObj;
begin
    Result := NIL;
    if ActiveCircuit <> NIL then
        Result := ActiveCircuit.Transformers.Active;
end;

// assuming the active winding has already been set
procedure Set_Parameter(const parm: String; const val: String);
var
    cmd: String;
begin
    if not Assigned(ActiveCircuit) then
        exit;
    SolutionAbort := FALSE;  // Reset for commands entered from outside
    cmd := Format('transformer.%s.%s=%s', [ActiveTransformer.Name, parm, val]);
    DSSExecutive.Command := cmd;
end;

function TTransformers.Get_AllNames: Olevariant;
var
    elem: TTransfObj;
    lst: TPointerList;
    k: Integer;
begin
    Result := VarArrayCreate([0, 0], varOleStr);
    Result[0] := 'NONE';
    if ActiveCircuit <> NIL then
        with ActiveCircuit do
            if Transformers.ListSize > 0 then
            begin
                lst := Transformers;
                VarArrayRedim(Result, lst.ListSize - 1);
                k := 0;
                elem := lst.First;
                while elem <> NIL do
                begin
                    Result[k] := elem.Name;
                    Inc(k);
                    elem := lst.Next;
                end;
            end;
end;

function TTransformers.Get_First: Integer;
var
    elem: TTransfObj;
    lst: TPointerList;
begin
    Result := 0;
    if ActiveCircuit <> NIL then
    begin
        lst := ActiveCircuit.Transformers;
        elem := lst.First;
        if elem <> NIL then
        begin
            repeat
                if elem.Enabled then
                begin
                    ActiveCircuit.ActiveCktElement := elem;
                    Result := 1;
                end
                else
                    elem := lst.Next;
            until (Result = 1) or (elem = NIL);
        end;
    end;
end;

function TTransformers.Get_IsDelta: Wordbool;
var
    elem: TTransfObj;
begin
    Result := FALSE;
    elem := ActiveTransformer;
    if elem <> NIL then
        if elem.WdgConnection[elem.ActiveWinding] > 0 then
            Result := TRUE;
end;

function TTransformers.Get_kV: Double;
var
    elem: TTransfObj;
begin
    Result := 0.0;
    elem := ActiveTransformer;
    if elem <> NIL then
        Result := elem.Winding^[elem.ActiveWinding].kvll;
end;

function TTransformers.Get_kVA: Double;
var
    elem: TTransfObj;
begin
    Result := 0.0;
    elem := ActiveTransformer;
    if elem <> NIL then
        Result := elem.WdgKVA[elem.ActiveWinding];
end;

function TTransformers.Get_MaxTap: Double;
var
    elem: TTransfObj;
begin
    Result := 0.0;
    elem := ActiveTransformer;
    if elem <> NIL then
        Result := elem.Maxtap[elem.ActiveWinding];
end;

function TTransformers.Get_MinTap: Double;
var
    elem: TTransfObj;
begin
    Result := 0.0;
    elem := ActiveTransformer;
    if elem <> NIL then
        Result := elem.Mintap[elem.ActiveWinding];
end;

function TTransformers.Get_Name: Widestring;
var
    elem: TTransfObj;
begin
    Result := '';
    if ActiveCircuit <> NIL then
    begin
        elem := ActiveCircuit.Transformers.Active;
        if elem <> NIL then
            Result := elem.Name;
    end;
end;

function TTransformers.Get_Next: Integer;
var
    elem: TTransfObj;
    lst: TPointerList;
begin
    Result := 0;
    if ActiveCircuit <> NIL then
    begin
        lst := ActiveCircuit.Transformers;
        elem := lst.Next;
        if elem <> NIL then
        begin
            repeat
                if elem.Enabled then
                begin
                    ActiveCircuit.ActiveCktElement := elem;
                    Result := lst.ActiveIndex;
                end
                else
                    elem := lst.Next;
            until (Result > 0) or (elem = NIL);
        end
    end;
end;

function TTransformers.Get_NumTaps: Integer;
var
    elem: TTransfObj;
begin
    Result := 0;
    elem := ActiveTransformer;
    if elem <> NIL then
        Result := elem.NumTaps[elem.ActiveWinding];
end;

function TTransformers.Get_NumWindings: Integer;
var
    elem: TTransfObj;
begin
    Result := 0;
    elem := ActiveTransformer;
    if elem <> NIL then
        Result := elem.NumberOfWindings;
end;

function TTransformers.Get_R: Double;
var
    elem: TTransfObj;
begin
    Result := 0.0;
    elem := ActiveTransformer;
    if elem <> NIL then
        Result := elem.WdgResistance[elem.ActiveWinding];
end;

function TTransformers.Get_Rneut: Double;
var
    elem: TTransfObj;
begin
    Result := 0.0;
    elem := ActiveTransformer;
    if elem <> NIL then
        Result := elem.WdgRneutral[elem.ActiveWinding];
end;

function TTransformers.Get_Tap: Double;
var
    elem: TTransfObj;
begin
    Result := 0.0;
    elem := ActiveTransformer;
    if elem <> NIL then
        Result := elem.PresentTap[elem.ActiveWinding];
end;

function TTransformers.Get_Wdg: Integer;
var
    elem: TTransfObj;
begin
    Result := 0;
    elem := ActiveTransformer;
    if elem <> NIL then
        Result := elem.ActiveWinding;
end;

function TTransformers.Get_XfmrCode: Widestring;
var
    elem: TTransfObj;
begin
    Result := '';
    elem := ActiveTransformer;
    if elem <> NIL then
        Result := elem.XfmrCode;
end;

function TTransformers.Get_Xhl: Double;
var
    elem: TTransfObj;
begin
    Result := 0.0;
    elem := ActiveTransformer;
    if elem <> NIL then
        Result := elem.XhlVal;
end;

function TTransformers.Get_Xht: Double;
var
    elem: TTransfObj;
begin
    Result := 0.0;
    elem := ActiveTransformer;
    if elem <> NIL then
        Result := elem.XhtVal;
end;

function TTransformers.Get_Xlt: Double;
var
    elem: TTransfObj;
begin
    Result := 0.0;
    elem := ActiveTransformer;
    if elem <> NIL then
        Result := elem.XltVal;
end;

function TTransformers.Get_Xneut: Double;
var
    elem: TTransfObj;
begin
    Result := 0.0;
    elem := ActiveTransformer;
    if elem <> NIL then
        Result := elem.WdgXneutral[elem.ActiveWinding];
end;

procedure TTransformers.Set_IsDelta(Value: Wordbool);
begin
    if Value = TRUE then
        Set_Parameter('Conn', 'Delta')
    else
        Set_Parameter('Conn', 'Wye')
end;

procedure TTransformers.Set_kV(Value: Double);
begin
    Set_Parameter('kv', FloatToStr(Value));
end;

procedure TTransformers.Set_kVA(Value: Double);
begin
    Set_Parameter('kva', FloatToStr(Value));
end;

procedure TTransformers.Set_MaxTap(Value: Double);
begin
    Set_Parameter('MaxTap', FloatToStr(Value));
end;

procedure TTransformers.Set_MinTap(Value: Double);
begin
    Set_Parameter('MinTap', FloatToStr(Value));
end;

procedure TTransformers.Set_Name(const Value: Widestring);
var
    ActiveSave: Integer;
    S: String;
    Found: Boolean;
    elem: TTransfObj;
    lst: TPointerList;
begin
    if ActiveCircuit <> NIL then
    begin
        lst := ActiveCircuit.Transformers;
        S := Value;  // Convert to Pascal String
        Found := FALSE;
        ActiveSave := lst.ActiveIndex;
        elem := lst.First;
        while elem <> NIL do
        begin
            if (CompareText(elem.Name, S) = 0) then
            begin
                ActiveCircuit.ActiveCktElement := elem;
                Found := TRUE;
                Break;
            end;
            elem := lst.Next;
        end;
        if not Found then
        begin
            DoSimpleMsg('Transformer "' + S + '" Not Found in Active Circuit.', 5003);
            elem := lst.Get(ActiveSave);    // Restore active Load
            ActiveCircuit.ActiveCktElement := elem;
        end;
    end;
end;

procedure TTransformers.Set_NumTaps(Value: Integer);
begin
    Set_Parameter('NumTaps', IntToStr(Value));
end;

procedure TTransformers.Set_NumWindings(Value: Integer);
var
    elem: TTransfObj;
begin
    elem := ActiveTransformer;
    if elem <> NIL then
        elem.SetNumWindings(Value);
end;

procedure TTransformers.Set_R(Value: Double);
begin
    Set_Parameter('%R', FloatToStr(Value));
end;

procedure TTransformers.Set_Rneut(Value: Double);
begin
    Set_Parameter('Rneut', FloatToStr(Value));
end;

procedure TTransformers.Set_Tap(Value: Double);
begin
    Set_Parameter('Tap', FloatToStr(Value));
end;

procedure TTransformers.Set_Wdg(Value: Integer);
var
    elem: TTransfObj;
begin
    elem := ActiveTransformer;
    if elem <> NIL then
        if (value > 0) and (value <= elem.NumberOfWindings) then
            elem.ActiveWinding := Value;
end;

procedure TTransformers.Set_XfmrCode(const Value: Widestring);
begin
    Set_Parameter('XfmrCode', Value);
end;

procedure TTransformers.Set_Xhl(Value: Double);
begin
    Set_Parameter('Xhl', FloatToStr(Value));
end;

procedure TTransformers.Set_Xht(Value: Double);
begin
    Set_Parameter('Xht', FloatToStr(Value));
end;

procedure TTransformers.Set_Xlt(Value: Double);
begin
    Set_Parameter('Xlt', FloatToStr(Value));
end;

procedure TTransformers.Set_Xneut(Value: Double);
begin
    Set_Parameter('Xneut', FloatToStr(Value));
end;

function TTransformers.Get_Count: Integer;
begin
    if Assigned(ActiveCircuit) then
        Result := ActiveCircuit.Transformers.ListSize;
end;

function TTransformers.Get_WdgVoltages: Olevariant;
var
    elem: TTransfObj;
    TempVoltageBuffer: pComplexArray;
    i,
    iV: Integer;
begin

    elem := ActiveTransformer;
    if elem <> NIL then
    begin
        if (elem.ActiveWinding > 0) and (elem.ActiveWinding <= elem.NumberOfWindings) then
        begin
            Result := VarArrayCreate([0, 2 * elem.nphases - 1], varDouble);
            TempVoltageBuffer := AllocMem(Sizeof(Complex) * elem.nphases);
            elem.GetWindingVoltages(elem.ActiveWinding, TempVoltageBuffer);
            iV := 0;
            for i := 1 to elem.Nphases do
            begin
                Result[iV] := TempVoltageBuffer^[i].re;
                Inc(iV);
                Result[iV] := TempVoltageBuffer^[i].im;
                Inc(iV);
            end;

            Reallocmem(TempVoltageBuffer, 0);
        end
        else
            Result := VarArrayCreate([0, 0], varDouble);
        ;

    end
    else
        Result := VarArrayCreate([0, 0], varDouble);

end;

function TTransformers.Get_WdgCurrents: Olevariant;
var
    elem: TTransfObj;
    TempCurrentBuffer: pComplexArray;
    NumCurrents,
    i,
    iV: Integer;
begin

    elem := ActiveTransformer;
    if elem <> NIL then
    begin
        NumCurrents := 2 * elem.NPhases * elem.NumberOfWindings; // 2 currents per winding
        Result := VarArrayCreate([0, 2 * NumCurrents - 1], varDouble);
        TempCurrentBuffer := AllocMem(Sizeof(Complex) * NumCurrents);
        ;
        elem.GetAllWindingCurrents(TempCurrentBuffer);
        iV := 0;
        for i := 1 to NumCurrents do
        begin
            Result[iV] := TempCurrentBuffer^[i].re;
            Inc(iV);
            Result[iV] := TempCurrentBuffer^[i].im;
            Inc(iV);
        end;

        Reallocmem(TempCurrentBuffer, 0);

    end
    else
        Result := VarArrayCreate([0, 0], varDouble);

end;

function TTransformers.Get_strWdgCurrents: Widestring;
var
    elem: TTransfObj;

begin
    elem := ActiveTransformer;
    if elem <> NIL then
    begin
        Result := elem.GetWindingCurrentsResult;
    end;
end;

function TTransformers.Get_CoreType: Integer;
var
    elem: TTransfObj;
begin
    Result := 0;  // default = shell
    elem := ActiveTransformer;
    if elem <> NIL then
        Result := elem.CoreType;

end;

procedure TTransformers.Set_CoreType(Value: Integer);
var
    elem: TTransfObj;
begin
    elem := ActiveTransformer;
    if elem <> NIL then
    begin
        elem.CoreType := Value;
        case Value of
            1:
                elem.strCoreType := '1-phase';
            3:
                elem.strCoreType := '3-leg';
            5:
                elem.strCoreType := '5-leg';
        else
            elem.strCoreType := 'shell';
        end;
    end;

end;

function TTransformers.Get_RdcOhms: Double;
var
    elem: TTransfObj;
begin
    Result := 0.0;
    elem := ActiveTransformer;
    if elem <> NIL then
        Result := elem.WdgRdc[elem.ActiveWinding];
end;

procedure TTransformers.Set_RdcOhms(Value: Double);
begin
    Set_Parameter('RdcOhms', FloatToStr(Value));
end;

initialization
    TAutoObjectFactory.Create(ComServer, TTransformers, Class_Transformers,
        ciInternal, tmApartment);
end.
