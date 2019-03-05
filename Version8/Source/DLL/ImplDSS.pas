unit ImplDSS;

{
  ----------------------------------------------------------
  Copyright (c) 2008-2015, Electric Power Research Institute, Inc.
  All rights reserved.
  ----------------------------------------------------------
}

{This is the class that gets registered}

interface

uses
    ComObj,
    ActiveX,
    OpenDSSEngine_TLB,
    StdVcl;

type
    TDSS = class(TAutoObject, IDSS)
    PROTECTED
        function Get_ActiveCircuit: ICircuit; SAFECALL;
        function Get_Circuits(Idx: Olevariant): ICircuit; SAFECALL;
        function Get_NumCircuits: Integer; SAFECALL;
        function Get_Error: IError; SAFECALL;
        function Get_Text: IText; SAFECALL;
        function NewCircuit(const Name: Widestring): ICircuit; SAFECALL;
        procedure ClearAll; SAFECALL;
        procedure ShowPanel; SAFECALL;
        function Get_Version: Widestring; SAFECALL;
        function Start(code: Integer): Wordbool; SAFECALL;
        function Get_DSSProgress: IDSSProgress; SAFECALL;
        function Get_Classes: Olevariant; SAFECALL;
        function Get_UserClasses: Olevariant; SAFECALL;
        function Get_NumClasses: Integer; SAFECALL;
        function Get_NumUserClasses: Integer; SAFECALL;
        function Get_AllowForms: Wordbool; SAFECALL;
        function Get_DataPath: Widestring; SAFECALL;
        procedure Set_DataPath(const Value: Widestring); SAFECALL;
        procedure Reset; SAFECALL;
        procedure Set_AllowForms(Value: Wordbool); SAFECALL;
        function Get_DefaultEditor: Widestring; SAFECALL;
        function Get_ActiveClass: IActiveClass; SAFECALL;
        function SetActiveClass(const ClassName: Widestring): Integer; SAFECALL;
        function Get_Executive: IDSS_Executive; SAFECALL;
        function Get_Events: IDSSEvents; SAFECALL;
        function Get_CmathLib: ICmathLib; SAFECALL;
        function Get_Parser: IParser; SAFECALL;
        function Get_DSSim_Coms: IDSSimComs; SAFECALL;
    end;

implementation

uses
    ComServ,
    DSSClassDefs,
    DSSGlobals,
    DSSForms,
    Forms,
    ScriptFormNormal,
    DSSClass,
    ImplGlobals,
    Exechelper,
    sysUtils,
    Executive,
    Variants;

function TDSS.Get_ActiveCircuit: ICircuit;
begin
    Result := FCircuit as ICircuit;
end;

function TDSS.Get_Circuits(Idx: Olevariant): ICircuit;
var
    i: Integer;
    S: String;
begin

    case (VarType(Idx) and varTypeMask) of
        varSmallint, VarInteger:
        begin
            i := Idx;
            if (Circuits.ListSize > i) and (i >= 0) then
                ActiveCircuit[ActiveActor] := Circuits.Get(i + 1)
            else
                DoSimpleMsg('Circuit index requested (' + IntToStr(i) + ') is invalid', 5009);

        end;
        VarOleStr:
        begin
            S := Idx;
            SetActiveCircuit(s);
        end;
    else
        DoSimpleMsg('Illegal Var Type Passed to Circuits Interface: ' + Format('$%x', [VarType(Idx)]), 5010);

    end;

    Result := FCircuit as ICircuit;  // Return interface that operates on active circuit
end;

function TDSS.Get_NumCircuits: Integer;
begin
    Result := ActiveCircuit[ActiveActor].NumCircuits;
end;


function TDSS.Get_Error: IError;
begin
    Result := FError as IError;
end;

function TDSS.Get_Text: IText;
begin
    Result := FText as IText;
end;


function TDSS.NewCircuit(const Name: Widestring): ICircuit;
begin

    MakeNewCircuit(Name);

    Result := FCircuit as ICircuit;

end;

procedure TDSS.ClearAll;
begin
    DoClearCmd;
end;

procedure TDSS.ShowPanel;
begin

//    ShowControlPanel; // in DSSForms
    if not Assigned(MainEditFormNormal) then
    begin
        MainEditFormNormal := TMainEditFormnormal.Create(NIL);
        MainEditFormNormal.Caption := 'OpenDSS Script Form';
        MainEditFormNormal.isMainWindow := TRUE;
    end;

    MainEditFormNormal.Show;

end;

function TDSS.Get_Version: Widestring;
begin
    Result := VersionString + '; License Status: Open ';
end;

function TDSS.Start(code: Integer): Wordbool;
{Place any start code here}
begin
    Result := TRUE;
end;

function TDSS.Get_DSSProgress: IDSSProgress;
begin
    Result := FDSSProgress as IDSSProgress;
end;

function TDSS.Get_Classes: Olevariant;
var
    i, k: Integer;

begin

    Result := VarArrayCreate([0, NumIntrinsicClasses - 1], varOleStr);
    k := 0;
    for i := 1 to NumIntrinsicClasses do
    begin
        Result[k] := TDSSClass(DssClassList[ActiveActor].Get(i)).Name;
        Inc(k);
    end;

end;

function TDSS.Get_UserClasses: Olevariant;
var
    i, k: Integer;

begin
    if NumUserClasses > 0 then
    begin
        Result := VarArrayCreate([0, NumUserClasses - 1], varOleStr);
        k := 0;
        for i := NumIntrinsicClasses + 1 to DSSClassList[ActiveActor].ListSize do
        begin
            Result[k] := TDSSClass(DssClassList[ActiveActor].Get(i)).Name;
            Inc(k);
        end;
    end
    else
        Result := VarArrayCreate([0, 0], varOleStr);
end;

function TDSS.Get_NumClasses: Integer;
begin

    Result := NumIntrinsicClasses;

end;

function TDSS.Get_NumUserClasses: Integer;
begin
    Result := NumUserClasses;
end;

function TDSS.Get_AllowForms: Wordbool;
begin
    Result := not NoFormsAllowed;
end;


procedure TDSS.Set_AllowForms(Value: Wordbool);
begin
    if not Value then
        NoFormsAllowed := not Value;  // Only set to False
    if NoFormsAllowed then
        CloseDownForms;  // DSSForms

end;

function TDSS.Get_DataPath: Widestring;
begin
    Result := DataDirectory[ActiveActor];
end;

procedure TDSS.Set_DataPath(const Value: Widestring);
begin
    SetDataPath(Value);
end;


procedure TDSS.Reset;
begin
        {Put any code here necessary to reset for specific systems};
end;


function TDSS.Get_DefaultEditor: Widestring;
begin
    Result := DSSGlobals.DefaultEditor;
end;

function TDSS.Get_ActiveClass: IActiveClass;
begin
    Result := FActiveClass as IActiveClass;
end;

function TDSS.SetActiveClass(const ClassName: Widestring): Integer;
var
    DevClassIndex: Integer;

begin
    Result := 0;
    DevClassIndex := ClassNames[ActiveActor].Find(ClassName);
    if DevClassIndex = 0 then
    begin
        DoSimplemsg('Error: Class ' + ClassName + ' not found.', 5016);
        Exit;
    end;

    LastClassReferenced[ActiveActor] := DevClassIndex;
    ActiveDSSClass[ActiveActor] := DSSClassList[ActiveActor].Get(LastClassReferenced[ActiveActor]);
    Result := LastClassReferenced[ActiveActor];

end;


function TDSS.Get_Executive: IDSS_Executive;
begin
    Result := FDSS_Executive as IDSS_Executive;
end;

function TDSS.Get_Events: IDSSEvents;
begin
    Result := FEvents as IDSSEvents;
end;

function TDSS.Get_DSSim_Coms: IDSSimComs;
begin
    Result := FDSSim_Coms as IDSSimComs;
end;

function TDSS.Get_CmathLib: ICmathLib;
begin
    Result := FCmathlib as ICmathLib;
end;

function TDSS.Get_Parser: IParser;
begin
    Result := Fparser as IParser;
end;

initialization

{This is the only class that gets registered "OpenDSSengine.DSS" The rest are all ciInternal}
    TAutoObjectFactory.Create(ComServer, TDSS, Class_DSS, ciMultiInstance, tmApartment);
    IsMultiThread := TRUE;

end.
