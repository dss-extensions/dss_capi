unit CAPI_Obj;

// Copyright (c) 2020-2023, DSS C-API contributors
// Copyright (c) 2020-2023, Paulo Meira
// All rights reserved.
//
// Redistribution and use in source and binary forms, with or without
// modification, are permitted provided that the following conditions are met:
//
// * Redistributions of source code must retain the above copyright notice, this
//   list of conditions and the following disclaimer.
//
// * Redistributions in binary form must reproduce the above copyright notice,
//   this list of conditions and the following disclaimer in the documentation
//   and/or other materials provided with the distribution.
//
// * Neither the name of the copyright holder nor the names of its
//   contributors may be used to endorse or promote products derived from
//   this software without specific prior written permission.
//
// THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS"
// AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
// IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
// DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE
// FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
// DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR
// SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER
// CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY,
// OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
// OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.

interface

uses
    CAPI_Utils,
    CAPI_Types,
    DSSObject,
    fpjson,
    Circuit,
    DSSClass;

type
    dss_obj_float64_function_t = function (obj: Pointer): Double; CDECL;
    dss_obj_int32_function_t = function (obj: Pointer): Integer; CDECL;
    dss_obj_float64_int32_function_t = function (obj: Pointer; funcArg: Integer): Double; CDECL;
    // dss_obj_int32_int32_function_t = function (obj: Pointer): Integer; CDECL;

{$SCOPEDENUMS ON}
{$PUSH}
{$Z4} // keep enums as int32 values
    BatchOp = (
        SetValues = 0,
        Multiply = 1,
        Increment = 2,
        Divide = 3
    );

    ExtraClassIDs = (
        PDElements = -4,
        PCElements = -3,
        CktElements = -2,
        DSSObjs = -1
    );
{$POP}    
{$SCOPEDENUMS OFF}

// The classic API keeps the string buffer in the global state,
// but since this new API wants to avoid that, users must dispose
// the string copies themselves.
//TODO: consider using the same API as numeric arrays for string
procedure DSS_Dispose_String(S: PAnsiChar); CDECL;

function Obj_New(DSS: TDSSContext; ClsIdx: Integer; Name: PAnsiChar; Activate: TAltAPIBoolean; BeginEdit: TAltAPIBoolean): Pointer; CDECL;
function Obj_GetHandleByName(DSS: TDSSContext; ClsIdx: Integer; Name: PAnsiChar): Pointer; CDECL;
function Obj_GetHandleByIdx(DSS: TDSSContext; ClsIdx: Integer; Idx: Integer): Pointer; CDECL;

function Obj_GetName(obj: TDSSObject): PAnsiChar; CDECL;
function Obj_GetFullName(obj: TDSSObject): PAnsiChar; CDECL;
function Obj_GetNumProperties(obj: TDSSObject): Integer; CDECL;
function Obj_GetCount(DSS: TDSSContext; ClsIdx: Integer): Integer; CDECL;
function Obj_GetListPointer(DSS: TDSSContext; ClsIdx: Integer): PPointer; CDECL;
function Obj_GetIdx(obj: TDSSObject): Integer; CDECL;
function Obj_GetClassName(obj: TDSSObject): PAnsiChar; CDECL;
function Obj_GetClassIdx(obj: TDSSObject): Integer; CDECL;
function Obj_PropertySideEffects(obj: TDSSObject; Index: Integer; PreviousInt: Integer; setterFlags: TDSSPropertySetterFlags): TAltAPIBoolean; CDECL;
procedure Obj_BeginEdit(obj: TDSSObject); CDECL;
procedure Obj_EndEdit(obj: TDSSObject; NumChanges: Integer); CDECL;
procedure Obj_Activate(obj: TDSSObject; AllLists: TAltAPIBoolean); CDECL;
function Obj_GetPropSeqPtr(obj: TDSSObject): PInteger; CDECL;

function Obj_GetFloat64(obj: TDSSObject; Index: Integer): Double; CDECL;
function Obj_GetInt32(obj: TDSSObject; Index: Integer): Integer; CDECL;
function Obj_GetString(obj: TDSSObject; Index: Integer): PAnsiChar; CDECL;
function Obj_GetObject(obj: TDSSObject; Index: Integer): Pointer; CDECL;
function Obj_GetAsString(obj: TDSSObject; Index: Integer): PAnsiChar; CDECL;
procedure Obj_GetFloat64Array(var ResultPtr: PDouble; ResultCount: PAPISize; obj: TDSSObject; Index: Integer); CDECL;
procedure Obj_GetInt32Array(var ResultPtr: PInteger; ResultCount: PAPISize; obj: TDSSObject; Index: Integer); CDECL;
procedure Obj_GetStringArray(var ResultPtr: PPAnsiChar; ResultCount: PAPISize; obj: TDSSObject; Index: Integer); CDECL;
procedure Obj_GetObjectArray(var ResultPtr: PPointer; ResultCount: PAPISize; obj: TDSSObject; Index: Integer); CDECL;
function Obj_ToJSON(obj: TDSSObject; joptions: Integer): PAnsiChar; CDECL;

procedure Obj_SetAsString(obj: TDSSObject; Index: Integer; Value: PAnsiChar; setterFlags: TDSSPropertySetterFlags); CDECL;
procedure Obj_SetFloat64(obj: TDSSObject; Index: Integer; Value: Double; setterFlags: TDSSPropertySetterFlags); CDECL;
procedure Obj_SetInt32(obj: TDSSObject; Index: Integer; Value: Integer; setterFlags: TDSSPropertySetterFlags); CDECL;
procedure Obj_SetString(obj: TDSSObject; Index: Integer; Value: PAnsiChar; setterFlags: TDSSPropertySetterFlags); CDECL;
procedure Obj_SetObject(obj: TDSSObject; Index: Integer; Value: TDSSObject; setterFlags: TDSSPropertySetterFlags); CDECL;

procedure Obj_SetFloat64Array(obj: TDSSObject; Index: Integer; Value: PDouble; ValueCount: Integer; setterFlags: TDSSPropertySetterFlags); CDECL;
procedure Obj_SetInt32Array(obj: TDSSObject; Index: Integer; Value: PInteger; ValueCount: Integer; setterFlags: TDSSPropertySetterFlags); CDECL;
procedure Obj_SetStringArray(obj: TDSSObject; Index: Integer; Value: PPAnsiChar; ValueCount: Integer; setterFlags: TDSSPropertySetterFlags); CDECL;
procedure Obj_SetObjectArray(obj: TDSSObject; Index: Integer; Value: TDSSObjectPtr; ValueCount: Integer; setterFlags: TDSSPropertySetterFlags); CDECL;

// internal functions
function Obj_ToJSON_(obj: TDSSObject; joptions: Integer): String;
function Obj_ToJSONData(obj: TDSSObject; joptions: Integer): TJSONData;

// Batch: creation and state setup
procedure Batch_CreateFromNew(DSS: TDSSContext; var ResultPtr: TDSSObjectPtr; ResultCount: PAPISize; ClsIdx: Integer; Names: PPAnsiChar; Count: Integer; BeginEdit: TAltAPIBoolean); CDECL;
procedure Batch_Dispose(batch: Pointer); CDECL;
procedure Batch_BeginEdit(batch: TDSSObjectPtr; batchSize: Integer); CDECL;
procedure Batch_EndEdit(batch: TDSSObjectPtr; batchSize: Integer; NumEdits: Integer); CDECL;
procedure Batch_GetPropSeq(var ResultPtr: PInteger; ResultCount: PAPISize; batch: TDSSObjectPtr; batchSize: Integer); CDECL;

// Batch -- using class and property indices
procedure Batch_CreateByClass(DSS: TDSSContext; var ResultPtr: TDSSObjectPtr; ResultCount: PAPISize; clsIdx: Integer); CDECL;
procedure Batch_CreateByRegExp(DSS: TDSSContext; var ResultPtr: TDSSObjectPtr; ResultCount: PAPISize; clsIdx: Integer; re: PAnsiChar); CDECL;
procedure Batch_CreateByIndex(DSS: TDSSContext; var ResultPtr: TDSSObjectPtr; ResultCount: PAPISize; ClsIdx: Integer; Value: PInteger; ValueCount: Integer); CDECL;
procedure Batch_CreateByInt32Property(DSS: TDSSContext; var ResultPtr: TDSSObjectPtr; ResultCount: PAPISize; ClsIdx: Integer; propidx: Integer; value: Integer); CDECL;
procedure Batch_CreateByFloat64PropertyRange(DSS: TDSSContext; var ResultPtr: TDSSObjectPtr; ResultCount: PAPISize; ClsIdx: Integer; propidx: Integer; valueMin: Double; valueMax: Double); CDECL;
procedure Batch_FilterByInt32Property(DSS: TDSSContext; var ResultPtr: TDSSObjectPtr; ResultCount: PAPISize; batch: TDSSObjectPtr; batchSize: Integer; propidx: Integer; value: Integer); CDECL;
procedure Batch_FilterByFloat64PropertyRange(DSS: TDSSContext; var ResultPtr: TDSSObjectPtr; ResultCount: PAPISize; batch: TDSSObjectPtr; batchSize: Integer; propidx: Integer; valueMin: Double; valueMax: Double); CDECL;


function Batch_ToJSON(batch: TDSSObjectPtr; batchSize: Integer; joptions: Integer): PAnsiChar; CDECL;

procedure Batch_GetFloat64(var ResultPtr: PDouble; ResultCount: PAPISize; batch: TDSSObjectPtr; batchSize: Integer; Index: Integer); CDECL;
procedure Batch_GetInt32(var ResultPtr: PInteger; ResultCount: PAPISize; batch: TDSSObjectPtr; batchSize: Integer; Index: Integer); CDECL;
procedure Batch_GetString(var ResultPtr: PPAnsiChar; ResultCount: PAPISize; batch: TDSSObjectPtr; batchSize: Integer; Index: Integer); CDECL;
procedure Batch_GetAsString(var ResultPtr: PPAnsiChar; ResultCount: PAPISize; batch: TDSSObjectPtr; batchSize: Integer; Index: Integer); CDECL;
procedure Batch_GetObject(var ResultPtr: PPointer; ResultCount: PAPISize; batch: TDSSObjectPtr; batchSize: Integer; Index: Integer); CDECL;
procedure Batch_GetFloat64FromFunc(var ResultPtr: PDouble; ResultCount: PAPISize; batch: TDSSObjectPtr; batchSize: Integer; func: dss_obj_float64_function_t); CDECL;
procedure Batch_GetFloat64FromFunc2(var ResultPtr: PDouble; ResultCount: PAPISize; batch: TDSSObjectPtr; batchSize: Integer; func: dss_obj_float64_int32_function_t; funcArg: Integer); CDECL;
procedure Batch_GetInt32FromFunc(var ResultPtr: PInteger; ResultCount: PAPISize; batch: TDSSObjectPtr; batchSize: Integer; func: dss_obj_int32_function_t); CDECL;

// procedure Batch_SetAsString(batch: TDSSObjectPtr; batchSize: Integer; Index: Integer; Value: PAnsiChar); CDECL;
procedure Batch_Float64(batch: TDSSObjectPtr; batchSize: Integer; Index: Integer; Operation: BatchOp; Value: Double; setterFlags: TDSSPropertySetterFlags); CDECL;
procedure Batch_Float64Array(batch: TDSSObjectPtr; batchSize: Integer; Index: Integer; Operation: BatchOp; Value: PDouble; setterFlags: TDSSPropertySetterFlags); CDECL;

procedure Batch_Int32(batch: TDSSObjectPtr; batchSize: Integer; Index: Integer; Operation: BatchOp; Value: Integer; setterFlags: TDSSPropertySetterFlags); CDECL;
procedure Batch_Int32Array(batch: TDSSObjectPtr; batchSize: Integer; Index: Integer; Operation: BatchOp; Value: PInteger; setterFlags: TDSSPropertySetterFlags); CDECL;

procedure Batch_SetString(batch: TDSSObjectPtr; batchSize: Integer; Index: Integer; Value: PAnsiChar; setterFlags: TDSSPropertySetterFlags); CDECL;
procedure Batch_SetObject(batch: TDSSObjectPtr; batchSize: Integer; Index: Integer; Value: TDSSObject; setterFlags: TDSSPropertySetterFlags); CDECL;

procedure Batch_SetFloat64Array(batch: TDSSObjectPtr; batchSize: Integer; Index: Integer; Value: PDouble; setterFlags: TDSSPropertySetterFlags); CDECL;
procedure Batch_SetInt32Array(batch: TDSSObjectPtr; batchSize: Integer; Index: Integer; Value: PInteger; setterFlags: TDSSPropertySetterFlags); CDECL;
procedure Batch_SetStringArray(batch: TDSSObjectPtr; batchSize: Integer; Index: Integer; Value: PPAnsiChar; setterFlags: TDSSPropertySetterFlags); CDECL;
procedure Batch_SetObjectArray(batch: TDSSObjectPtr; batchSize: Integer; Index: Integer; Value: TDSSObjectPtr; setterFlags: TDSSPropertySetterFlags); CDECL;

// Batch -- using class and property names
procedure Batch_CreateFromNewS(DSS: TDSSContext; var ResultPtr: TDSSObjectPtr; ResultCount: PAPISize; clsName: String; Names: PPAnsiChar; Count: Integer; BeginEdit: TAltAPIBoolean); CDECL;
procedure Batch_CreateByClassS(DSS: TDSSContext; var ResultPtr: TDSSObjectPtr; ResultCount: PAPISize; clsName: PAnsiChar); CDECL;
procedure Batch_CreateByRegExpS(DSS: TDSSContext; var ResultPtr: TDSSObjectPtr; ResultCount: PAPISize; clsname: PAnsiChar; re: PAnsiChar); CDECL;
procedure Batch_CreateByIndexS(DSS: TDSSContext; var ResultPtr: TDSSObjectPtr; ResultCount: PAPISize; clsname: PAnsiChar; Value: PInteger; ValueCount: Integer); CDECL;
procedure Batch_CreateByInt32PropertyS(DSS: TDSSContext; var ResultPtr: TDSSObjectPtr; ResultCount: PAPISize; clsname: PAnsiChar; propname: PAnsiChar; value: Integer); CDECL;
procedure Batch_CreateByFloat64PropertyRangeS(DSS: TDSSContext; var ResultPtr: TDSSObjectPtr; ResultCount: PAPISize; clsname: PAnsiChar; propname: PAnsiChar; valueMin: Double; valueMax: Double); CDECL;

procedure Batch_GetFloat64S(var ResultPtr: PDouble; ResultCount: PAPISize; batch: TDSSObjectPtr; batchSize: Integer; Name: PChar); CDECL;
procedure Batch_GetInt32S(var ResultPtr: PInteger; ResultCount: PAPISize; batch: TDSSObjectPtr; batchSize: Integer; Name: PChar); CDECL;
procedure Batch_GetStringS(var ResultPtr: PPAnsiChar; ResultCount: PAPISize; batch: TDSSObjectPtr; batchSize: Integer; Name: PChar); CDECL;
procedure Batch_GetAsStringS(var ResultPtr: PPAnsiChar; ResultCount: PAPISize; batch: TDSSObjectPtr; batchSize: Integer; Name: PChar); CDECL;
procedure Batch_GetObjectS(var ResultPtr: PPointer; ResultCount: PAPISize; batch: TDSSObjectPtr; batchSize: Integer; Name: PChar); CDECL;

// procedure Batch_SetAsStringS(batch: TDSSObjectPtr; batchSize: Integer; Name: PChar; Value: PAnsiChar); CDECL;
procedure Batch_Float64S(batch: TDSSObjectPtr; batchSize: Integer; Name: PChar; Operation: BatchOp; Value: Double; setterFlags: TDSSPropertySetterFlags); CDECL;
procedure Batch_Int32S(batch: TDSSObjectPtr; batchSize: Integer; Name: PChar; Operation: BatchOp; Value: Integer; setterFlags: TDSSPropertySetterFlags); CDECL;
procedure Batch_SetStringS(batch: TDSSObjectPtr; batchSize: Integer; Name: PChar; Value: PAnsiChar; setterFlags: TDSSPropertySetterFlags); CDECL;
procedure Batch_SetObjectS(batch: TDSSObjectPtr; batchSize: Integer; Name: PChar; Value: TDSSObject; setterFlags: TDSSPropertySetterFlags); CDECL;

procedure Batch_Float64ArrayS(batch: TDSSObjectPtr; batchSize: Integer; Name: PChar; Operation: BatchOp; Value: PDouble; setterFlags: TDSSPropertySetterFlags); CDECL;
procedure Batch_SetFloat64ArrayS(batch: TDSSObjectPtr; batchSize: Integer; Name: PChar; Value: PDouble; setterFlags: TDSSPropertySetterFlags); CDECL;
procedure Batch_Int32ArrayS(batch: TDSSObjectPtr; batchSize: Integer; Name: PChar; Operation: BatchOp; Value: PInteger; setterFlags: TDSSPropertySetterFlags); CDECL;
procedure Batch_SetInt32ArrayS(batch: TDSSObjectPtr; batchSize: Integer; Name: PChar; Value: PInteger; setterFlags: TDSSPropertySetterFlags); CDECL;
procedure Batch_SetStringArrayS(batch: TDSSObjectPtr; batchSize: Integer; Name: PChar; Value: PPAnsiChar; setterFlags: TDSSPropertySetterFlags); CDECL;
procedure Batch_SetObjectArrayS(batch: TDSSObjectPtr; batchSize: Integer; Name: PChar; Value: TDSSObjectPtr; setterFlags: TDSSPropertySetterFlags); CDECL;

// JSON functions, internal
function Obj_Circuit_ToJSON_(ckt: TDSSCircuit; joptions: Integer): PAnsiChar;
procedure Obj_Circuit_FromJSON_(DSS: TDSSContext; jckt: TJSONObject; joptions: Integer);

implementation

uses
    CAPI_metadata,
    CAPI_Alt,
    CAPI_Schema,
    StrUtils,
    Utilities,
    RegExpr,
    DSSGlobals,
    SysUtils,
    CktElement,
    DSSPointerList,
    DSSClassDefs,
    DSSHelper,
    DSSObjectHelper,
    TypInfo,
    ArrayDef,
    Math,
    UComplex,
    Classes,
    jsonparser,
    Bus,
    DateUtils,
    DynEqPCE;

procedure DSS_Dispose_String(S: PAnsiChar); CDECL;
begin
    FreeMem(S);
end;

function obj_NewFromClass(DSS: TDSSContext; Cls: TDSSClass; Name: String; Activate: TAltAPIBoolean; BeginEdit: TAltAPIBoolean): Pointer;
var
    Obj: TDSSObject;
begin
    Result := NIL;
    if DSS = NIL then DSS := DSSPrime;
    Obj := Cls.NewObject(Name, Activate);
    if Obj = NIL then
        Exit;

    if BeginEdit then
        Cls.BeginEdit(Obj, False);

    if Cls.DSSClassType = DSS_OBJECT then
        DSS.DSSObjs.Add(Obj)
    else
        DSS.ActiveCircuit.AddCktElement(TDSSCktElement(Obj));

    Result := Obj;
end;


function Obj_New(DSS: TDSSContext; ClsIdx: Integer; Name: PAnsiChar; Activate: TAltAPIBoolean; BeginEdit: TAltAPIBoolean): Pointer; CDECL;
var
    Cls: TDSSClass;
    checkDups: Boolean;
begin
    Result := NIL;
    if DSS = NIL then DSS := DSSPrime;
    Cls := DSS.DSSClassList.At(ClsIdx);
    if (Cls = NIL) or (Cls.RequiresCircuit and InvalidCircuit(DSS)) then
        Exit;

    checkDups := (cls.DSSClassType <> DSS_OBJECT) and (not DSS.ActiveCircuit.DuplicatesAllowed);

    if checkDups then
    begin
        if cls.Find(Name, true) <> NIL then
        begin
            DoSimpleMsg(DSS, 'Warning: Duplicate new element definition: "%s.%s". Element being redefined.', [Cls.Name, Name], 266);
            Exit;
        end;
    end;
    Result := obj_NewFromClass(DSS, cls, name, Activate, BeginEdit);
end;

function Obj_GetHandleByName(DSS: TDSSContext; ClsIdx: Integer; Name: PAnsiChar): Pointer; CDECL;
var
    Cls: TDSSClass;
begin
    Result := NIL;
    if DSS = NIL then DSS := DSSPrime;
    Cls := DSS.DSSClassList.At(ClsIdx);
    if Cls = NIL then
        Exit;

    Result := Cls.Find(Name, False);
end;

function Obj_GetHandleByIdx(DSS: TDSSContext; ClsIdx: Integer; Idx: Integer): Pointer; CDECL;
var
    Cls: TDSSClass;
begin
    Result := NIL;
    if DSS = NIL then DSS := DSSPrime;
    Cls := DSS.DSSClassList.At(ClsIdx);
    if Cls = NIL then
        Exit;

    Result := Cls.ElementList.At(Idx);
end;

procedure Obj_BeginEdit(obj: TDSSObject); CDECL;
begin
    obj.ParentClass.BeginEdit(obj, False);
end;

procedure Obj_EndEdit(obj: TDSSObject; NumChanges: Integer); CDECL;
begin
    obj.ParentClass.EndEdit(obj, NumChanges);
end;

procedure activateOnList(Obj: TDSSObject; List: TDSSPointerList);
var
    prev: Integer;
    p: TDSSObject;
begin
    if List.Active = Obj then
        Exit;

    prev := List.ActiveIndex;
    p := List.First();
    while p <> NIL do
    begin
        if List.Active = Obj then
            Exit;

        p := List.Next();
    end;

    // Restore previous position if not found
    List.Get(prev);
end;

procedure Obj_Activate(obj: TDSSObject; AllLists: TAltAPIBoolean); CDECL;
begin
    if obj is TDSSCktElement then
        obj.DSS.ActiveCircuit.ActiveCktElement := TDSSCktElement(obj)
    else
        obj.DSS.ActiveDSSObject := obj;

    obj.ParentClass.ElementList.Get(obj.ClassIndex);

    if not AllLists then
        Exit;

    // Adapted from TDSSCircuit.AddCktElement
    with obj.DSS.ActiveCircuit do
    begin
        // Update lists of PC and PD elements
        case (Obj.DSSObjType and BaseClassMask) of
            PD_ELEMENT:
                activateOnList(obj, PDElements);
            PC_ELEMENT:
                activateOnList(obj, PCElements);
            CTRL_ELEMENT:
                activateOnList(obj, DSSControls);
            METER_ELEMENT:
                activateOnList(obj, MeterElements);
        end;

        // Update lists of special elements and generic types

        //TODO: note that most of these lists are kind of redundant
        //      with our current implementation

        case (Obj.DSSObjType and CLASSMASK) of
            MON_ELEMENT:
                activateOnList(obj, Monitors);
            ENERGY_METER:
                activateOnList(obj, EnergyMeters);
            SENSOR_ELEMENT:
                activateOnList(obj, Sensors);
            GEN_ELEMENT:
                activateOnList(obj, Generators);
            SOURCE:
                activateOnList(obj, Sources);
            CAP_CONTROL:
                activateOnList(obj, CapControls);
            SWT_CONTROL:
                activateOnList(obj, SwtControls);
            REG_CONTROL:
                activateOnList(obj, RegControls);
            LOAD_ELEMENT:
                activateOnList(obj, Loads);
            CAP_ELEMENT:
                activateOnList(obj, ShuntCapacitors);
            REACTOR_ELEMENT:
                activateOnList(obj, Reactors);
            RELAY_CONTROL:
                activateOnList(obj, Relays);
            FUSE_CONTROL:
                activateOnList(obj, Fuses);
            RECLOSER_CONTROL:
                activateOnList(obj, Reclosers);

            // Keep Lines, Transformer, and Lines and Faults in PDElements and separate lists
            // so we can find them quickly.
            AUTOTRANS_ELEMENT:
                activateOnList(obj, AutoTransformers);
            XFMR_ELEMENT:
                activateOnList(obj, Transformers);
            LINE_ELEMENT:
                activateOnList(obj, Lines);
            FAULTOBJECT:
                activateOnList(obj, Faults);

            STORAGE_ELEMENT:
                activateOnList(obj, StorageElements);
            PVSYSTEM_ELEMENT:
                activateOnList(obj, PVSystems);
            INV_CONTROL:
                activateOnList(obj, InvControls);
            EXP_CONTROL:
                activateOnList(obj, ExpControls);
        end;
    end;
end;

function Obj_GetPropSeqPtr(obj: TDSSObject): PInteger; CDECL;
begin
    Result := PInteger(obj.PrpSequence);
end;

function Obj_GetName(obj: TDSSObject): PAnsiChar; CDECL;
begin
    Result := PAnsiChar(obj.Name);
end;

function Obj_GetFullName(obj: TDSSObject): PAnsiChar; CDECL;
begin
    Result := DSS_CopyStringAsPChar(obj.FullName);
end;

function Obj_GetNumProperties(obj: TDSSObject): Integer; CDECL;
begin
    Result := obj.ParentClass.NumProperties;
end;

function Obj_GetListPointer(DSS: TDSSContext; ClsIdx: Integer): PPointer; CDECL;
var
    Cls: TDSSClass;
begin
    Result := NIL;
    if DSS = NIL then DSS := DSSPrime;

    if ClsIdx <= 0 then
    begin
        if (ClsIdx <> ord(ExtraClassIDs.DSSObjs)) and InvalidCircuit(DSS) then
            Exit;
        
        case ClsIdx of
            ord(ExtraClassIDs.DSSObjs):
                Result := PPointer(DSS.DSSObjs.InternalPointer);
            ord(ExtraClassIDs.CktElements):
                Result := PPointer(DSS.ActiveCircuit.CktElements.InternalPointer);
            ord(ExtraClassIDs.PCElements):
                Result := PPointer(DSS.ActiveCircuit.PCElements.InternalPointer);
            ord(ExtraClassIDs.PDElements):
                Result := PPointer(DSS.ActiveCircuit.PDElements.InternalPointer);
        else
            DoSimpleMsg(DSS, 'Class index is not a valid DSS class or convenience extra class.', [ClsIdx], 5022);
        end;
        Exit;
    end;

    Cls := DSS.DSSClassList.At(ClsIdx);
    if Cls = NIL then
        Exit;

    Result := PPointer(Cls.ElementList.InternalPointer);
end;

function Obj_GetCount(DSS: TDSSContext; ClsIdx: Integer): Integer; CDECL;
var
    Cls: TDSSClass;
begin
    Result := 0;
    if DSS = NIL then DSS := DSSPrime;

    if ClsIdx <= 0 then
    begin
        if (ClsIdx <> ord(ExtraClassIDs.DSSObjs)) and InvalidCircuit(DSS) then
            Exit;

        case ClsIdx of
            ord(ExtraClassIDs.DSSObjs):
                Result := DSS.DSSObjs.Count;
            ord(ExtraClassIDs.CktElements):
                Result := DSS.ActiveCircuit.CktElements.Count;
            ord(ExtraClassIDs.PCElements):
                Result := DSS.ActiveCircuit.PCElements.Count;
            ord(ExtraClassIDs.PDElements):
                Result := DSS.ActiveCircuit.PDElements.Count;
        else
            DoSimpleMsg(DSS, 'Class index is not a valid DSS class or convenience extra class.', [ClsIdx], 5022);
        end;
        Exit;
    end;

    Cls := DSS.DSSClassList.At(ClsIdx);
    if Cls = NIL then
        Exit;

    Result := Cls.ElementList.Count
end;

function Obj_GetIdx(obj: TDSSObject): Integer; CDECL;
begin
    Result := obj.ClassIndex;
end;

function Obj_GetClassName(obj: TDSSObject): PAnsiChar; CDECL;
begin
    Result := PChar(obj.ParentClass.Name);
end;

function Obj_GetClassIdx(obj: TDSSObject): Integer; CDECL;
begin
    Result := obj.ParentClass.DSSClassIndex;
end;

function Obj_PropertySideEffects(obj: TDSSObject; Index: Integer; PreviousInt: Integer; setterFlags: TDSSPropertySetterFlags): TAltAPIBoolean; CDECL;
begin
    Result := True;
    try
        obj.PropertySideEffects(Index, PreviousInt, setterFlags);
    except
        Result := False;
    end;
end;

function Obj_GetFloat64(obj: TDSSObject; Index: Integer): Double; CDECL;
begin
    Result := obj.GetDouble(Index);
end;

function Obj_GetInt32(obj: TDSSObject; Index: Integer): Integer; CDECL;
begin
    Result := obj.GetInteger(Index);
end;

function Obj_GetString(obj: TDSSObject; Index: Integer): PAnsiChar; CDECL;
begin
    Result := DSS_CopyStringAsPChar(obj.GetString(Index));
end;

function Obj_GetAsString(obj: TDSSObject; Index: Integer): PAnsiChar; CDECL;
begin
    Result := DSS_CopyStringAsPChar(obj.GetPropertyValue(Index));
end;

function Obj_GetObject(obj: TDSSObject; Index: Integer): Pointer; CDECL;
begin
    Result := obj.GetObject(Index);
end;

procedure Obj_SetFloat64(obj: TDSSObject; Index: Integer; Value: Double; setterFlags: TDSSPropertySetterFlags); CDECL;
begin
    obj.SetDouble(Index, Value, setterFlags);
end;

procedure Obj_SetInt32(obj: TDSSObject; Index: Integer; Value: Integer; setterFlags: TDSSPropertySetterFlags); CDECL;
begin
    obj.SetInteger(Index, Value, setterFlags);
end;

procedure Obj_SetString(obj: TDSSObject; Index: Integer; Value: PAnsiChar; setterFlags: TDSSPropertySetterFlags); CDECL;
begin
    obj.SetString(Index, Value, setterFlags);
end;

procedure Obj_SetAsString(obj: TDSSObject; Index: Integer; Value: PAnsiChar; setterFlags: TDSSPropertySetterFlags); CDECL;
begin
    obj.ParsePropertyValue(Index, Value, setterFlags);
end;

procedure Obj_SetObject(obj: TDSSObject; Index: Integer; Value: TDSSObject; setterFlags: TDSSPropertySetterFlags); CDECL;
begin
    obj.SetObject(Index, Value, setterFlags);
end;

procedure Obj_SetFloat64Array(obj: TDSSObject; Index: Integer; Value: PDouble; ValueCount: Integer; setterFlags: TDSSPropertySetterFlags); CDECL;
begin
    obj.SetDoubles(Index, Value, ValueCount, setterFlags);
end;

procedure Obj_SetInt32Array(obj: TDSSObject; Index: Integer; Value: PInteger; ValueCount: Integer; setterFlags: TDSSPropertySetterFlags); CDECL;
begin
    obj.SetIntegers(Index, Value, ValueCount, setterFlags);
end;

procedure Obj_SetStringArray(obj: TDSSObject; Index: Integer; Value: PPAnsiChar; ValueCount: Integer; setterFlags: TDSSPropertySetterFlags); CDECL;
begin
    obj.SetStrings(Index, Value, ValueCount, setterFlags);
end;

procedure Obj_SetObjectArray(obj: TDSSObject; Index: Integer; Value: TDSSObjectPtr; ValueCount: Integer; setterFlags: TDSSPropertySetterFlags); CDECL;
begin
    obj.SetObjects(Index, Value, ValueCount, setterFlags);
end;

procedure Obj_GetFloat64Array(var ResultPtr: PDouble; ResultCount: PAPISize; obj: TDSSObject; Index: Integer); CDECL;
begin
    obj.GetDoubles(Index, ResultPtr, ResultCount);
end;

procedure Obj_GetInt32Array(var ResultPtr: PInteger; ResultCount: PAPISize; obj: TDSSObject; Index: Integer); CDECL;
begin
    obj.GetIntegers(Index, ResultPtr, ResultCount);
end;

procedure Obj_GetStringArray(var ResultPtr: PPAnsiChar; ResultCount: PAPISize; obj: TDSSObject; Index: Integer); CDECL;
begin
    obj.GetStrings(Index, ResultPtr, ResultCount);
end;

procedure Obj_GetObjectArray(var ResultPtr: PPointer; ResultCount: PAPISize; obj: TDSSObject; Index: Integer); CDECL;
begin
    obj.GetObjects(Index, ResultPtr, ResultCount);
end;

function Obj_ToJSONData(obj: TDSSObject; joptions: Integer): TJSONData;
var
    iProp, iPropNext, iPropNext2: Integer;
    jvalue: TJSONData = NIL;
    cls: TDSSClass;
    done: array of Boolean;
    resObj: TJSONObject;
    pnames: pStringArray;
    dynObj: TDynEqPCE;
begin
    Result := NIL;
    if obj = NIL then
        Exit;

    cls := obj.ParentClass;

    if (joptions and Integer(DSSJSONOptions.LowercaseKeys)) = 0 then
    begin
        pnames := cls.PropertyNameJSON;
    end
    else
    begin
        pnames := cls.PropertyNameLowercase;
    end;

    if (joptions and Integer(DSSJSONOptions.IncludeDSSClass)) <> 0 then
    begin
        if (joptions and Integer(DSSJSONOptions.LowercaseKeys)) = 0 then
            Result := TJSONObject.Create(['DSSClass', cls.Name, 'Name', obj.Name])
        else
            Result := TJSONObject.Create(['dssclass', cls.Name, 'Name', obj.Name]);
    end
    else
        Result := TJSONObject.Create(['Name', obj.Name]);

    SetLength(done, cls.NumProperties);

    resObj := Result as TJSONObject;

    if (joptions and Integer(DSSJSONOptions.Full)) = 0 then
    begin
        // Clear done status
        SetLength(done, 0);
        SetLength(done, cls.NumProperties);

        // Return only filled properties, but adjust some odd ones
        iPropNext := obj.GetNextPropertySet(-9999999);
        iPropNext2 := 0;
        while iPropNext > 0 do
        begin
            iProp := iPropNext;
            if iPropNext2 <> 0 then
            begin
                iPropNext := iPropNext2; // try to keep the previous ordering
                iPropNext2 := 0;
            end
            else
            begin
                iPropNext := obj.GetNextPropertySet(iProp);
            end;
            if done[iProp] then
                continue;

            done[iProp] := True;

            // If redundant and array-related, prefer the original property.
            // We use the singular-named property (e.g. kV instead of kVs) since not
            // all plural forms are exposed by OpenDSS properties that implement
            // array-valued quantities.
            // These don't expose array versions (they're either DoubleOnStructArrayProperty or IntegerOnStructArrayProperty):
            // - Transformer: MaxTap, MinTap, RdcOhms, NumTaps, Rneut, Xneut
            // - AutoTrans: MaxTap, MinTap, RdcOhms, NumTaps
            // - XfmrCode: MaxTap, MinTap, RdcOhms, NumTaps, Rneut, Xneut
            if (TPropertyFlag.Redundant in cls.PropertyFlags[iProp]) and
                (cls.PropertyRedundantWith[iProp] <> 0) and (
                    (TPropertyFlag.OnArray in cls.PropertyFlags[iProp]) or
                    (cls.PropertyType[iProp] in [
                        TPropertyType.BusesOnStructArrayProperty,
                        TPropertyType.MappedStringEnumArrayOnStructArrayProperty,
                        TPropertyType.MappedStringEnumArrayProperty,
                        TPropertyType.DoubleArrayOnStructArrayProperty,
                        TPropertyType.DSSObjectReferenceArrayProperty,
                        TPropertyType.IntegerArrayProperty,
                        TPropertyType.DoubleFArrayProperty,
                        TPropertyType.DoubleVArrayProperty,
                        TPropertyType.DoubleDArrayProperty,
                        TPropertyType.DoubleArrayProperty,
                        TPropertyType.DoubleProperty
                        //TODO: any other?
                    ])
                ) then
            begin
                // Set iPropNext to allow multiple levels of redundancy
                iPropNext2 := iPropNext;
                iPropNext := cls.PropertyRedundantWith[iProp];
                continue;
            end;

            // skip "Like", substructure (winding, wire) index or suppressed props
            if ((cls.PropertyType[iProp] = TPropertyType.MakeLikeProperty) or 
                (TPropertyFlag.SuppressJSON in cls.PropertyFlags[iProp]) and (not (TPropertyFlag.Redundant in cls.PropertyFlags[iProp]))) or
                (TPropertyFlag.AltIndex in cls.PropertyFlags[iProp]) or
                (TPropertyFlag.IntegerStructIndex in cls.PropertyFlags[iProp]) then
                continue;

            if cls.GetObjPropertyJSONValue(Pointer(obj), iProp, joptions, jvalue, True) then
                resObj.Add(pnames[iProp], jvalue);
        end;
    end
    else
    begin
        // Return all properties
        for iProp := 1 to cls.NumProperties do
        begin
            if ((Integer(DSSJSONOptions.SkipRedundant) and joptions) <> 0) and (TPropertyFlag.Redundant in cls.PropertyFlags[iProp]) then
                continue;
            // skip substructure (winding, wire) index or suppressed props
            if (TPropertyFlag.SuppressJSON in cls.PropertyFlags[iProp]) or
                (TPropertyFlag.AltIndex in cls.PropertyFlags[iProp]) or
                (TPropertyFlag.IntegerStructIndex in cls.PropertyFlags[iProp]) then
                continue;

            if cls.GetObjPropertyJSONValue(Pointer(obj), iProp, joptions, jvalue, True) then
                resObj.Add(pnames[iProp], jvalue);
        end;
    end;
    if not (obj is TDynEqPCE) then
        Exit;
    
    dynObj := obj as TDynEqPCE;
    if dynObj.UserDynInit = NIL then
        Exit;

    resObj.Add('DynInit', dynObj.UserDynInit.Clone());
end;

function Obj_ToJSON_(obj: TDSSObject; joptions: Integer): String;
var
    json: TJSONData = NIL;
begin
    Result := '';
    if obj = NIL then
        Exit;

    try
        json := Obj_ToJSONData(obj, joptions);
        if json <> NIL then
        begin
            if (Integer(DSSJSONOptions.Pretty) and joptions) <> 0 then
                Result := json.FormatJSON([], 2)
            else
                Result := json.FormatJSON([foSingleLineArray, foSingleLineObject, foskipWhiteSpace], 0);
        end;
    except
        on E: Exception do
            obj.DoSimpleMsg('Error converting object data to JSON: %s', [E.message], 5020);
    end;
    FreeAndNil(json);
end;

function Obj_ToJSON(obj: TDSSObject; joptions: Integer): PAnsiChar; CDECL;
begin
    Result := DSS_CopyStringAsPChar(Obj_ToJSON_(obj, joptions))
end;

//------------------------------------------------------------------------------

procedure Batch_Dispose(batch: Pointer); CDECL;
begin
    FreeMem(batch);
end;

procedure Batch_BeginEdit(batch: TDSSObjectPtr; batchSize: Integer); CDECL;
var
    i: Integer;
    cls: TDSSClass;
begin
    if (batch = NIL) or (batch^ = NIL) or (batchSize <= 0) then
        Exit;

    cls := batch^.ParentClass;
    for i := 1 to batchSize do
    begin
        cls.BeginEdit(batch^, False);
        inc(batch);
    end;
end;

procedure Batch_EndEdit(batch: TDSSObjectPtr; batchSize: Integer; NumEdits: Integer); CDECL;
var
    i: Integer;
    cls: TDSSClass;
begin
    if (batch = NIL) or (batch^ = NIL) or (batchSize <= 0) then
        Exit;

    cls := batch^.ParentClass;
    for i := 1 to batchSize do
    begin
        cls.EndEdit(batch^, NumEdits);
        inc(batch);
    end;
end;

procedure ensureBatchSize(maxSize: Integer; var ResultPtr: TDSSObjectPtr; ResultCount: PAPISize);
begin
    if (ResultPtr <> NIL) and (ResultCount[1] >= maxSize) then
    begin
        ResultCount[0] := 0;
        Exit;
    end;

    if ResultPtr <> NIL then
        Batch_Dispose(ResultPtr);

    ResultPtr := AllocMem(SizeOf(Pointer) * maxSize);
    ResultCount[0] := 0;
    ResultCount[1] := maxSize;
end;

procedure Batch_GetPropSeq(var ResultPtr: PInteger; ResultCount: PAPISize; batch: TDSSObjectPtr; batchSize: Integer); CDECL;
var
    cls: TDSSClass;
    // propOffset: PtrUint;
    presult: PInteger;
    i, N: Integer;
    // propFlags: TPropertyFlags;
begin
    ResultCount[0] := 0;
    if (batch = NIL) or (batch^ = NIL) or (batchSize = 0) then
    begin
        Exit;
    end;
    cls := batch^.ParentClass;
    N := cls.NumProperties + 1;
    DSS_RecreateArray_PInteger(ResultPtr, ResultCount, batchSize * N, N, batchSize);
    presult := ResultPtr;
    for i := 1 to batchSize do
    begin
        Move(batch^.PrpSequence^, presult^, N * SizeOf(Integer));
        inc(batch);
        inc(presult, N);
    end;
end;

procedure Batch_CreateFromNew(DSS: TDSSContext; var ResultPtr: TDSSObjectPtr; ResultCount: PAPISize; ClsIdx: Integer; Names: PPAnsiChar; Count: Integer; BeginEdit: TAltAPIBoolean); CDECL;
var
    // Obj: TDSSObject;
    Cls: TDSSClass;
    outptr: TDSSObjectPtr;
    i: Integer;
    Name, prefix: String;
    checkDups: Boolean;
begin
    if DSS = NIL then DSS := DSSPrime;
    Cls := DSS.DSSClassList.At(ClsIdx);
    if (Cls = NIL) or (Cls.RequiresCircuit and InvalidCircuit(DSS)) then
        Exit;

    checkDups := (cls.DSSClassType <> DSS_OBJECT) and (not DSS.ActiveCircuit.DuplicatesAllowed);
    ensureBatchSize(Count, ResultPtr, ResultCount);
    outptr := ResultPtr;

    if Names = NIL then
    begin
        // Use a random batch prefix to avoid collisions; we won't check for existing objects here
        prefix := Format('%09d_', [Random(1000000000)]);
        for i := 1 to Count do
        begin
            outptr^ := Cls.NewObject(Format('%s_%d', [prefix, i]), False);
            if Cls.DSSClassType = DSS_OBJECT then
                DSS.DSSObjs.Add(outptr^)
            else
                DSS.ActiveCircuit.AddCktElement(TDSSCktElement(outptr^));

            inc(outptr);
        end;
    end
    else
    begin
        for i := 1 to Count do
        begin
            Name := Names^;
            if checkDups and (cls.Find(Name, true) <> NIL) then
            begin
                DoSimpleMsg(DSS, 'Warning: Duplicate new element definition: "%s.%s". Element being redefined.', [Cls.Name, Name], 266);
                Exit;
            end;
            outptr^ := Cls.NewObject(Name, False);
            if Cls.DSSClassType = DSS_OBJECT then
                DSS.DSSObjs.Add(outptr^)
            else
                DSS.ActiveCircuit.AddCktElement(TDSSCktElement(outptr^));

            inc(outptr);
            inc(Names);
        end;
    end;

    ResultCount^ := Count;
    if not BeginEdit then
        Exit;

    outptr := ResultPtr;
    for i := 1 to Count do
    begin
        Cls.BeginEdit(outptr^, False);
        inc(outptr);
    end;
end;

procedure Batch_CreateByRegExp(DSS: TDSSContext; var ResultPtr: TDSSObjectPtr; ResultCount: PAPISize; clsIdx: Integer; re: PAnsiChar); CDECL;
var
    cls: TDSSClass;
    rex: TRegExpr = NIL;
    objlist: TDSSObjectPtr;
    outptr: TDSSObjectPtr;
    res: String;
    i: Integer;
begin
    if DSS = NIL then DSS := DSSPrime;
    cls := DSS.DSSClassList.At(clsIdx);
    if cls = NIL then
        Exit;
    ensureBatchSize(cls.ElementList.Count, ResultPtr, ResultCount);
    objlist := TDSSObjectPtr(cls.ElementList.InternalPointer);
    outptr := ResultPtr;
    try
        res := re;
        rex := TRegExpr.Create();
        rex.ModifierI := True;
        rex.Expression:= res;
        ResultCount[0] := 0;
        for i := 1 to cls.ElementList.Count do
        begin
            if rex.Exec(objlist^.Name) then
            begin
                outptr^:= objlist^;
                inc(outptr);
                inc(ResultCount[0]);
            end;
            inc(objlist);
        end;
    except
        on E: Exception do
        begin
            DoSimpleMsg(DSS, 'Error processing regular expression: %s', [E.Message], 20231029);    
            ResultCount[0] := 0;
        end;
    end;
    FreeAndNil(rex);
end;


procedure Batch_CreateByClass(DSS: TDSSContext; var ResultPtr: TDSSObjectPtr; ResultCount: PAPISize; clsIdx: Integer); CDECL;
var
    cls: TDSSClass;
    objlist: TDSSObjectPtr;
    outptr: TDSSObjectPtr;
    i: Integer;
begin
    if DSS = NIL then DSS := DSSPrime;
    cls := DSS.DSSClassList.At(clsIdx);
    if cls = NIL then
        Exit;
    ensureBatchSize(cls.ElementList.Count, ResultPtr, ResultCount);
    ResultCount[0] := cls.ElementList.Count;
    objlist := TDSSObjectPtr(cls.ElementList.InternalPointer);
    outptr := ResultPtr;
    for i := 1 to cls.ElementList.Count do
    begin
        outptr^:= objlist^;
        inc(outptr);
        inc(objlist);
    end;
end;

procedure Batch_CreateByIndex(DSS: TDSSContext; var ResultPtr: TDSSObjectPtr; ResultCount: PAPISize; ClsIdx: Integer; Value: PInteger; ValueCount: Integer); CDECL;
var
    cls: TDSSClass;
    list: TDSSPointerList;
    outptr: TDSSObjectPtr;
    i: Integer;
begin
    if DSS = NIL then DSS := DSSPrime;
    cls := DSS.DSSClassList.At(clsIdx);
    if cls = NIL then
        Exit;
    list := cls.ElementList;
    ensureBatchSize(list.Count, ResultPtr, ResultCount);
    outptr := ResultPtr;
    for i := 1 to ValueCount do
    begin
        if (Value^ > 0) and (Value^ <= list.Count) then
        begin
            outptr^ := list.At(Value^);
            inc(outptr);
            inc(ResultCount[0]);
        end;
        inc(Value);
    end;
end;

procedure Batch_CreateByInt32Property(DSS: TDSSContext; var ResultPtr: TDSSObjectPtr; ResultCount: PAPISize; ClsIdx: Integer; propidx: Integer; value: Integer); CDECL;
var
    cls: TDSSClass;
begin
    if DSS = NIL then DSS := DSSPrime;
    cls := DSS.DSSClassList.At(clsIdx);
    if cls = NIL then
    begin
        Exit;
    end;
    Batch_FilterByInt32Property(DSS, ResultPtr, ResultCount, TDSSObjectPtr(cls.ElementList.InternalPointer), cls.ElementList.Count, propidx, value);
end;

procedure Batch_FilterByInt32Property(DSS: TDSSContext; var ResultPtr: TDSSObjectPtr; ResultCount: PAPISize; batch: TDSSObjectPtr; batchSize: Integer; propidx: Integer; value: Integer); CDECL;
var
    cls: TDSSClass;
    objlist: TDSSObjectPtr;
    outptr: TDSSObjectPtr;
    propOffset: PtrUint;
    i: Integer;
    propFlags: TPropertyFlags;
    ptype: TPropertyType;
begin
    if DSS = NIL then DSS := DSSPrime;
    ResultCount[0] := 0;
    if (batch = NIL) or (batchSize = 0) then
        Exit;

    cls := batch^.ParentClass;
    if cls = NIL then
    begin
        Exit;
    end;

    ptype := cls.PropertyType[propidx];
    if not (ptype in [
        TPropertyType.IntegerProperty,
        TPropertyType.MappedIntEnumProperty,
        TPropertyType.MappedStringEnumProperty,
        TPropertyType.BooleanProperty,
        TPropertyType.IntegerOnStructArrayProperty,
        TPropertyType.EnabledProperty
    ]) then
    begin
        Exit;
    end;
    if (ptype in [TPropertyType.BooleanProperty, TPropertyType.EnabledProperty]) or (ptype = TPropertyType.EnabledProperty) then
    begin
        value := Integer(LongBool(value <> 0));
    end;

    propFlags := cls.PropertyFlags[propIdx];
    propOffset := cls.PropertyOffset[propIdx];
    objlist := batch;
    ensureBatchSize(batchSize, ResultPtr, ResultCount);
    outptr := ResultPtr;
    if (ptype in [
        TPropertyType.IntegerProperty,
        TPropertyType.MappedIntEnumProperty,
        TPropertyType.MappedStringEnumProperty,
        TPropertyType.BooleanProperty]) and
        (not (TPropertyFlag.CustomGet in propFlags)) and
        (not (TPropertyFlag.ReadByFunction in propFlags)) and
        (not (TPropertyFlag.ScaledByFunction in propFlags)) then
    begin
        // 40-50% faster than calling the function
        for i := 1 to batchSize do
        begin
            if (PInteger(PtrUint(objlist^) + propoffset))^ = value then
            begin
                outptr^ := objlist^;
                inc(outptr);
                inc(ResultCount[0]);
            end;
            inc(objlist);
        end;
        Exit;
    end;

    for i := 1 to batchSize do
    begin
        if cls.GetObjInteger(objList^, propIdx) = value then
        begin
            outptr^ := objlist^;
            inc(outptr);
            inc(ResultCount[0]);
        end;
        inc(objlist);
    end;
end;

procedure Batch_CreateByFloat64PropertyRange(DSS: TDSSContext; var ResultPtr: TDSSObjectPtr; ResultCount: PAPISize; ClsIdx: Integer; propidx: Integer; valueMin: Double; valueMax: Double); CDECL;
var
    cls: TDSSClass;
begin
    if DSS = NIL then DSS := DSSPrime;
    cls := DSS.DSSClassList.At(clsIdx);
    if cls = NIL then
    begin
        Exit;
    end;
    Batch_FilterByFloat64PropertyRange(DSS, ResultPtr, ResultCount, TDSSObjectPtr(cls.ElementList.InternalPointer), cls.ElementList.Count, propidx, valueMin, valueMax);
end;

procedure Batch_FilterByFloat64PropertyRange(DSS: TDSSContext; var ResultPtr: TDSSObjectPtr; ResultCount: PAPISize; batch: TDSSObjectPtr; batchSize: Integer; propidx: Integer; valueMin: Double; valueMax: Double); CDECL;
var
    cls: TDSSClass;
    objlist: TDSSObjectPtr;
    outptr: TDSSObjectPtr;
    propOffset: PtrUint;
    i: Integer;
    propFlags: TPropertyFlags;
    ptype: TPropertyType;
    v: Double;
begin
    if DSS = NIL then DSS := DSSPrime;
    ResultCount[0] := 0;
    if (batch = NIL) or (batchSize = 0) then
        Exit;

    cls := batch^.ParentClass;
    if cls = NIL then
    begin
        Exit;
    end;

    ptype := cls.PropertyType[propidx];
    if not (ptype in [
        TPropertyType.DoubleProperty,
        TPropertyType.DoubleOnStructArrayProperty,
        TPropertyType.DoubleOnArrayProperty
    ]) then
    begin
        Exit;
    end;

    propFlags := cls.PropertyFlags[propIdx];
    propOffset := cls.PropertyOffset[propIdx];
    objlist := batch;
    ensureBatchSize(batchSize, ResultPtr, ResultCount);
    outptr := ResultPtr;
    if (ptype = TPropertyType.DoubleProperty) and
        (propFlags = []) and
        (cls.PropertyScale[propIdx] = 1) then
    begin
        for i := 1 to batchSize do
        begin
            v := PDouble(PtrUint(objlist^) + propoffset)^;
            if (v >= valueMin) and (v <= valueMax) then
            begin
                outptr^ := objlist^;
                inc(outptr);
                inc(ResultCount[0]);
            end;
            inc(objlist);
        end;
        Exit;
    end;

    for i := 1 to batchSize do
    begin
        v := cls.GetObjDouble(objList^, propIdx);
        if (v >= valueMin) and (v <= valueMax) then
        begin
            outptr^ := objlist^;
            inc(outptr);
            inc(ResultCount[0]);
        end;
        inc(objlist);
    end;
end;

function Batch_ToJSON(batch: TDSSObjectPtr; batchSize: Integer; joptions: Integer): PAnsiChar; CDECL;
var
    json: TJSONArray = NIL;
    i: Integer;
    exportDefaultObjs: Boolean;
begin
    exportDefaultObjs := (joptions and Integer(DSSJSONOptions.IncludeDefaultObjs)) <> 0;
    Result := NIL;
    if (batch = NIL) or (batch^ = NIL) then
        Exit;

    if (batchSize = 0) then
    begin
        Result := DSS_CopyStringAsPChar('[]');
        Exit;
    end;

    try
        json := TJSONArray.Create([]);
        if ((joptions and Integer(DSSJSONOptions.ExcludeDisabled)) = 0) or not (batch^ is TDSSCktElement) then
        begin
            for i := 1 to batchSize do
            begin
                if (not (Flg.DefaultAndUnedited in batch^.Flags)) or exportDefaultObjs then
                    json.Add(Obj_ToJSONData(batch^, joptions));

                inc(batch);
            end;
        end
        else
        begin
            for i := 1 to batchSize do
            begin
                // NOTE: Default objects are circuit elements, so we can skip the check here.
                // if ((not (Flg.DefaultAndUnedited in batch^.Flags)) or exportDefaultObjs) and
                if TDSSCktElement(batch^).Enabled then
                    json.Add(Obj_ToJSONData(batch^, joptions));

                inc(batch);
            end;
        end;
        if json <> NIL then
        begin
            if (Integer(DSSJSONOptions.Pretty) and joptions) <> 0 then
                Result := DSS_CopyStringAsPChar(json.FormatJSON([], 2))
            else
                Result := DSS_CopyStringAsPChar(json.FormatJSON([foSingleLineArray, foSingleLineObject, foskipWhiteSpace], 0));
        end;
    except
        on E: Exception do
            batch^.DoSimpleMsg('Error converting batch data to JSON: %s', [E.message], 5020);
    end;
    FreeAndNil(json);
end;

procedure Batch_GetFloat64(var ResultPtr: PDouble; ResultCount: PAPISize; batch: TDSSObjectPtr; batchSize: Integer; Index: Integer); CDECL;
var
    cls: TDSSClass;
    // propOffset: PtrUint;
    presult: PDouble;
    i: Integer;
begin
    ResultCount[0] := 0;
    if (batch = NIL) or (batch^ = NIL) or (batchSize = 0) then
        Exit;

    cls := batch^.ParentClass;

    // propOffset := cls.PropertyOffset[propIdx];
    DSS_RecreateArray_PDouble(ResultPtr, ResultCount, batchSize);
    presult := ResultPtr;

    if not (cls.PropertyType[Index] in [
        TPropertyType.DoubleProperty,
        TPropertyType.DoubleOnArrayProperty,
        TPropertyType.DoubleOnStructArrayProperty
    ]) then
        Exit;

    for i := 1 to batchSize do
    begin
        //p^ := PDouble(PtrUint(batch^) + propOffset))^; // TODO: benchmark
        presult^ := cls.GetObjDouble(batch^, Index);
        inc(batch);
        inc(presult);
    end;
end;

procedure Batch_GetFloat64FromFunc(var ResultPtr: PDouble; ResultCount: PAPISize; batch: TDSSObjectPtr; batchSize: Integer; func: dss_obj_float64_function_t); CDECL;
var
    presult: PDouble;
    i: Integer;
begin
    ResultCount[0] := 0;
    if (batch = NIL) or (batch^ = NIL) or (batchSize = 0) or ((@func) = NIL) then
        Exit;

    DSS_RecreateArray_PDouble(ResultPtr, ResultCount, batchSize);
    presult := ResultPtr;

    for i := 1 to batchSize do
    begin
        presult^ := func(batch^);
        inc(batch);
        inc(presult);
    end;
end;

procedure Batch_GetFloat64FromFunc2(var ResultPtr: PDouble; ResultCount: PAPISize; batch: TDSSObjectPtr; batchSize: Integer; func: dss_obj_float64_int32_function_t; funcArg: Integer); CDECL;
var
    presult: PDouble;
    i: Integer;
begin
    ResultCount[0] := 0;
    if (batch = NIL) or (batch^ = NIL) or (batchSize = 0) or ((@func) = NIL) then
        Exit;

    DSS_RecreateArray_PDouble(ResultPtr, ResultCount, batchSize);
    presult := ResultPtr;

    for i := 1 to batchSize do
    begin
        presult^ := func(batch^, funcArg);
        inc(batch);
        inc(presult);
    end;
end;

procedure Batch_GetInt32(var ResultPtr: PInteger; ResultCount: PAPISize; batch: TDSSObjectPtr; batchSize: Integer; Index: Integer); CDECL;
var
    cls: TDSSClass;
    propOffset: PtrUint;
    presult: PInteger;
    i: Integer;
    propFlags: TPropertyFlags;
begin
    ResultCount[0] := 0;
    if (batch = NIL) or (batch^ = NIL) or (batchSize = 0) then
    begin
        Exit;
    end;

    cls := batch^.ParentClass;
    propFlags := cls.PropertyFlags[Index];
    propOffset := cls.PropertyOffset[Index];

    DSS_RecreateArray_PInteger(ResultPtr, ResultCount, batchSize);
    presult := ResultPtr;

    if not (cls.PropertyType[Index] in [
        TPropertyType.IntegerProperty,
        TPropertyType.MappedIntEnumProperty,
        TPropertyType.MappedStringEnumProperty,
        TPropertyType.BooleanProperty,
        TPropertyType.EnabledProperty,
        TPropertyType.IntegerOnStructArrayProperty
    ]) then
    begin
        Exit;
    end;

    if (cls.PropertyType[Index] in [
        TPropertyType.IntegerProperty,
        TPropertyType.MappedIntEnumProperty,
        TPropertyType.MappedStringEnumProperty,
        TPropertyType.BooleanProperty]) and
        (not (TPropertyFlag.CustomGet in propFlags)) and
        (not (TPropertyFlag.ReadByFunction in propFlags)) and
        (not (TPropertyFlag.ScaledByFunction in propFlags)) then
    begin
        for i := 1 to batchSize do
        begin
            presult^ := (PInteger(PtrUint(batch^) + propOffset))^;
            inc(batch);
            inc(presult);
        end;
        Exit;
    end;

    for i := 1 to batchSize do
    begin
        presult^ := cls.GetObjInteger(batch^, Index);
        inc(batch);
        inc(presult);
    end;
end;

procedure Batch_GetInt32FromFunc(var ResultPtr: PInteger; ResultCount: PAPISize; batch: TDSSObjectPtr; batchSize: Integer; func: dss_obj_int32_function_t); CDECL;
var
    presult: PInteger;
    i: Integer;
begin
    ResultCount[0] := 0;
    if (batch = NIL) or (batch^ = NIL) or (batchSize = 0) or ((@func) = NIL) then
    begin
        Exit;
    end;
    DSS_RecreateArray_PInteger(ResultPtr, ResultCount, batchSize);
    presult := ResultPtr;
    for i := 1 to batchSize do
    begin
        presult^ := func(batch^);
        inc(batch);
        inc(presult);
    end;
end;

procedure Batch_GetString(var ResultPtr: PPAnsiChar; ResultCount: PAPISize; batch: TDSSObjectPtr; batchSize: Integer; Index: Integer); CDECL;
var
    cls: TDSSClass;
    // propOffset: PtrUint;
    presult: PPAnsiChar;
    i: Integer;
begin
    ResultCount[0] := 0;
    if (batch = NIL) or (batch^ = NIL) or (batchSize = 0) then
        Exit;

    cls := batch^.ParentClass;

    // propOffset := cls.PropertyOffset[propIdx];
    DSS_RecreateArray_PPAnsiChar(ResultPtr, ResultCount, batchSize);
    presult := ResultPtr;

    if not (cls.PropertyType[Index] in [
        TPropertyType.StringSilentROFunctionProperty,
        TPropertyType.StringProperty,
        TPropertyType.BusProperty,
        TPropertyType.BusOnStructArrayProperty,
        TPropertyType.MappedStringEnumProperty,
        TPropertyType.DSSObjectReferenceProperty
    ]) then
        Exit;

    for i := 1 to batchSize do
    begin
        //p^ := DSS_CopyStringAsPChar(PString(PtrUint(batch^) + propOffset))^); // TODO: benchmark
        presult^ := DSS_CopyStringAsPChar(cls.GetObjString(batch^, Index));
        inc(batch);
        inc(presult);
    end;
end;

procedure Batch_GetAsString(var ResultPtr: PPAnsiChar; ResultCount: PAPISize; batch: TDSSObjectPtr; batchSize: Integer; Index: Integer); CDECL;
var
    cls: TDSSClass;
    // propOffset: PtrUint;
    presult: PPAnsiChar;
    i: Integer;
    s: String;
begin
    ResultCount[0] := 0;
    if (batch = NIL) or (batch^ = NIL) or (batchSize = 0) then
        Exit;

    cls := batch^.ParentClass;
    DSS_RecreateArray_PPAnsiChar(ResultPtr, ResultCount, batchSize);
    presult := ResultPtr;
    for i := 1 to batchSize do
    begin
        cls.GetObjPropertyValue(batch^, Index, s);
        presult^ := DSS_CopyStringAsPChar(s);
        inc(batch);
        inc(presult);
    end;
end;

procedure Batch_GetObject(var ResultPtr: PPointer; ResultCount: PAPISize; batch: TDSSObjectPtr; batchSize: Integer; Index: Integer); CDECL;
var
    cls: TDSSClass;
    // propOffset: PtrUint;
    presult: TDSSObjectPtr;
    i: Integer;
begin
    ResultCount[0] := 0;
    if (batch = NIL) or (batch^ = NIL) or (batchSize = 0) then
        Exit;

    cls := batch^.ParentClass;

    // propOffset := cls.PropertyOffset[propIdx];
    DSS_CreateArray_PPointer(ResultPtr, ResultCount, batchSize);
    presult := TDSSObjectPtr(ResultPtr);

    if not (cls.PropertyType[Index] in [
        TPropertyType.DSSObjectReferenceProperty,
        TPropertyType.DSSObjectReferenceArrayProperty
    ]) then
        Exit;

    for i := 1 to batchSize do
    begin
        //p^ := TDSSObjectPtr(PtrUint(batch^) + propOffset))^; // TODO: benchmark
        presult^ := cls.GetObjObject(batch^, Index);
        inc(batch);
        inc(presult);
    end;
end;

procedure Batch_Float64(batch: TDSSObjectPtr; batchSize: Integer; Index: Integer; Operation: BatchOp; Value: Double; setterFlags: TDSSPropertySetterFlags); CDECL;
var
    cls: TDSSClass;
    propOffset: PtrUint;
    i: Integer;
    prev: Double;
    doublePtr: PDouble;
    propFlags: TPropertyFlags;
    singleEdit: Boolean;
begin
    if (batch = NIL) or (batch^ = NIL) or (batchSize = 0) then
        Exit;

    cls := batch^.ParentClass;
    propFlags := cls.PropertyFlags[Index];
    propOffset := cls.PropertyOffset[Index];

    if not (cls.PropertyType[Index] in [
        TPropertyType.DoubleProperty,
        TPropertyType.DoubleOnStructArrayProperty,
        TPropertyType.DoubleOnArrayProperty
    ]) then
        Exit;

    if (cls.PropertyType[Index] = TPropertyType.DoubleProperty) and
        (propFlags = []) and
        (cls.PropertyScale[Index] = 1) then
    begin
        case Operation of
            BatchOp.Multiply:
                for i := 1 to batchSize do
                begin
                    singleEdit := not (Flg.EditingActive in batch^.Flags);
                    if singleEdit then
                        cls.BeginEdit(batch^, False);

                    doublePtr := (PDouble(PtrUint(batch^) + propOffset));
                    prev := doubleptr^;
                    doublePtr^ := doublePtr^ * Value;
                    batch^.SetAsNextSeq(Index);
                    batch^.PropertySideEffects(Index, Round(prev), setterFlags);

                    if singleEdit then
                        cls.EndEdit(batch^, 1);
                    inc(batch);
                end;
            BatchOp.Divide:
                for i := 1 to batchSize do
                begin
                    singleEdit := not (Flg.EditingActive in batch^.Flags);
                    if singleEdit then
                        cls.BeginEdit(batch^, False);

                    doublePtr := (PDouble(PtrUint(batch^) + propOffset));
                    prev := doubleptr^;
                    doublePtr^ := doublePtr^ / Value;
                    batch^.SetAsNextSeq(Index);
                    batch^.PropertySideEffects(Index, Round(prev), setterFlags);

                    if singleEdit then
                        cls.EndEdit(batch^, 1);
                    inc(batch);
                end;
            BatchOp.Increment:
                for i := 1 to batchSize do
                begin
                    singleEdit := not (Flg.EditingActive in batch^.Flags);
                    if singleEdit then
                        cls.BeginEdit(batch^, False);

                    doublePtr := (PDouble(PtrUint(batch^) + propOffset));
                    prev := doubleptr^;
                    doublePtr^ := doublePtr^ + Value;
                    batch^.SetAsNextSeq(Index);
                    batch^.PropertySideEffects(Index, Round(prev), setterFlags);

                    if singleEdit then
                        cls.EndEdit(batch^, 1);
                    inc(batch);
                end;
        else
            for i := 1 to batchSize do
            begin
                singleEdit := not (Flg.EditingActive in batch^.Flags);
                if singleEdit then
                    cls.BeginEdit(batch^, False);

                doublePtr := (PDouble(PtrUint(batch^) + propOffset));
                prev := doubleptr^;
                doublePtr^ := Value;
                batch^.SetAsNextSeq(Index);
                batch^.PropertySideEffects(Index, Round(prev), setterFlags);

                if singleEdit then
                    cls.EndEdit(batch^, 1);
                inc(batch);
            end;
        end;

        Exit;
    end;

    case Operation of
        BatchOp.Multiply:
            for i := 1 to batchSize do
            begin
                batch^.SetDouble(Index, Value * cls.GetObjDouble(batch^, Index), setterFlags);
                inc(batch);
            end;
        BatchOp.Divide:
            for i := 1 to batchSize do
            begin
                batch^.SetDouble(Index, cls.GetObjDouble(batch^, Index) / Value, setterFlags);
                inc(batch);
            end;
        BatchOp.Increment:
            for i := 1 to batchSize do
            begin
                batch^.SetDouble(Index, Value + cls.GetObjDouble(batch^, Index), setterFlags);
                inc(batch);
            end;
    else
        for i := 1 to batchSize do
        begin
            batch^.SetDouble(Index, Value, setterFlags);
            inc(batch);
        end;
    end;
end;

procedure Batch_Float64Array(batch: TDSSObjectPtr; batchSize: Integer; Index: Integer; Operation: BatchOp; Value: PDouble; setterFlags: TDSSPropertySetterFlags); CDECL;
var
    cls: TDSSClass;
    propOffset: PtrUint;
    i: Integer;
    prev: Double;
    doublePtr: PDouble;
    propFlags: TPropertyFlags;
    singleEdit: Boolean;
    allowNA: Boolean;
begin
    if (batch = NIL) or (batch^ = NIL) or (batchSize = 0) then
        Exit;

    allowNA := not (TDSSPropertySetterFlag.SkipNA in setterFlags);
    cls := batch^.ParentClass;
    propFlags := cls.PropertyFlags[Index];
    propOffset := cls.PropertyOffset[Index];

    if not (cls.PropertyType[Index] in [
        TPropertyType.DoubleProperty,
        TPropertyType.DoubleOnStructArrayProperty,
        TPropertyType.DoubleOnArrayProperty
    ]) then
        Exit;

    if (cls.PropertyType[Index] = TPropertyType.DoubleProperty) and
        (propFlags = []) and
        (cls.PropertyScale[Index] = 1) then
    begin
        case Operation of
            BatchOp.Multiply:
                for i := 1 to batchSize do
                begin
                    if (allowNA) or (not IsNaN(Value^)) then
                    begin
                        singleEdit := not (Flg.EditingActive in batch^.Flags);
                        if singleEdit then
                            cls.BeginEdit(batch^, False);

                        doublePtr := (PDouble(PtrUint(batch^) + propOffset));
                        prev := doubleptr^;
                        doublePtr^ := doublePtr^ * Value^;
                        batch^.SetAsNextSeq(Index);
                        batch^.PropertySideEffects(Index, Round(prev), setterFlags);

                        if singleEdit then
                            cls.EndEdit(batch^, 1);
                    end;
                    inc(batch);
                    inc(Value);
                end;
            BatchOp.Divide:
                for i := 1 to batchSize do
                begin
                    if (allowNA) or (not IsNaN(Value^)) then
                    begin
                        singleEdit := not (Flg.EditingActive in batch^.Flags);
                        if singleEdit then
                            cls.BeginEdit(batch^, False);

                        doublePtr := (PDouble(PtrUint(batch^) + propOffset));
                        prev := doubleptr^;
                        doublePtr^ := doublePtr^ / Value^;
                        batch^.SetAsNextSeq(Index);
                        batch^.PropertySideEffects(Index, Round(prev), setterFlags);

                        if singleEdit then
                            cls.EndEdit(batch^, 1);
                    end;
                    inc(batch);
                    inc(Value);
                end;
            BatchOp.Increment:
                for i := 1 to batchSize do
                begin
                    if (allowNA) or (not IsNaN(Value^)) then
                    begin
                        singleEdit := not (Flg.EditingActive in batch^.Flags);
                        if singleEdit then
                            cls.BeginEdit(batch^, False);

                        doublePtr := (PDouble(PtrUint(batch^) + propOffset));
                        prev := doubleptr^;
                        doublePtr^ := doublePtr^ + Value^;
                        batch^.SetAsNextSeq(Index);
                        batch^.PropertySideEffects(Index, Round(prev), setterFlags);

                        if singleEdit then
                            cls.EndEdit(batch^, 1);
                    end;
                    inc(batch);
                    inc(Value);
                end;
            BatchOp.SetValues:
                for i := 1 to batchSize do
                begin
                    if (allowNA) or (not IsNaN(Value^)) then
                    begin
                        singleEdit := not (Flg.EditingActive in batch^.Flags);
                        if singleEdit then
                            cls.BeginEdit(batch^, False);

                        doublePtr := PDouble(PtrUint(batch^) + propOffset);
                        prev := doubleptr^;
                        doublePtr^ := Value^;
                        batch^.SetAsNextSeq(Index);
                        batch^.PropertySideEffects(Index, Round(prev), setterFlags);

                        if singleEdit then
                            cls.EndEdit(batch^, 1);
                    end;
                    inc(batch);
                    inc(Value);
                end;
        end;

        Exit;
    end;

    case Operation of
        BatchOp.Multiply:
            for i := 1 to batchSize do
            begin
                if (allowNA) or (not IsNaN(Value^)) then
                    batch^.SetDouble(Index, Value^ * cls.GetObjDouble(batch^, Index), setterFlags);
                inc(batch);
                inc(Value);
            end;
        BatchOp.Divide:
            for i := 1 to batchSize do
            begin
                if (allowNA) or (not IsNaN(Value^)) then
                    batch^.SetDouble(Index, cls.GetObjDouble(batch^, Index) / Value^, setterFlags);
                inc(batch);
                inc(Value);
            end;
        BatchOp.Increment:
            for i := 1 to batchSize do
            begin
                if (allowNA) or (not IsNaN(Value^)) then
                    batch^.SetDouble(Index, Value^ + cls.GetObjDouble(batch^, Index), setterFlags);
                inc(batch);
                inc(Value);
            end;
    else
        for i := 1 to batchSize do
        begin
            if (allowNA) or (not IsNaN(Value^)) then
                batch^.SetDouble(Index, Value^, setterFlags);
            inc(batch);
            inc(Value);
        end;
    end;
end;

procedure Batch_Int32(batch: TDSSObjectPtr; batchSize: Integer; Index: Integer; Operation: BatchOp; Value: Integer; setterFlags: TDSSPropertySetterFlags); CDECL;
var
    cls: TDSSClass;
    // propOffset: PtrUint;
    i: Integer;
    // prev: Integer;
    // intptr: PInteger;
    // propFlags: TPropertyFlags;
    ptype: TPropertyType;
begin
    if (batch = NIL) or (batch^ = NIL) or (batchSize = 0) then
        Exit;

    cls := batch^.ParentClass;
    // propFlags := cls.PropertyFlags[Index];
    // propOffset := cls.PropertyOffset[Index];

    ptype := cls.PropertyType[Index];
    if not (ptype in [
        TPropertyType.IntegerProperty,
        TPropertyType.MappedIntEnumProperty,
        TPropertyType.MappedStringEnumProperty,
        TPropertyType.BooleanProperty,
        TPropertyType.EnabledProperty,
        TPropertyType.IntegerOnStructArrayProperty
    ]) then
        Exit;

    if (ptype in [TPropertyType.BooleanProperty, TPropertyType.EnabledProperty, TPropertyType.BooleanActionProperty]) and not (Operation in [BatchOp.Increment]) then
    begin
        Value := Integer(LongBool(value <> 0));
    end;

    // if (cls.PropertyType[Index] in [
    //     TPropertyType.IntegerProperty,
    //     TPropertyType.MappedIntEnumProperty,
    //     TPropertyType.MappedStringEnumProperty,
    //     TPropertyType.BooleanProperty]) and
    //     (not (TPropertyFlag.CustomSet in propFlags)) and
    //     (not (TPropertyFlag.WriteByFunction in propFlags)) and
    //     (not (TPropertyFlag.ScaledByFunction in propFlags)) then
    // begin
    //     for i := 1 to batchSize do
    //     begin
    //         intptr := (PInteger(PtrUint(batch^) + propoffset));
    //         prev := intptr^;
    //         intptr^ := Value;

    //         inc(batch);
    //     end;
    //     Exit;
    // end;

    case Operation of
        BatchOp.Multiply:
            for i := 1 to batchSize do
            begin
                batch^.SetInteger(Index, Value * cls.GetObjInteger(batch^, Index), setterFlags);
                inc(batch);
            end;
        BatchOp.Divide:
            for i := 1 to batchSize do
            begin
                batch^.SetInteger(Index, cls.GetObjInteger(batch^, Index) div Value, setterFlags);
                inc(batch);
            end;
        BatchOp.Increment:
            for i := 1 to batchSize do
            begin
                batch^.SetInteger(Index, Value + cls.GetObjInteger(batch^, Index), setterFlags);
                inc(batch);
            end;
    else
        for i := 1 to batchSize do
        begin
            batch^.SetInteger(Index, Value, setterFlags);
            inc(batch);
        end;
    end;
end;

procedure Batch_SetString(batch: TDSSObjectPtr; batchSize: Integer; Index: Integer; Value: PAnsiChar; setterFlags: TDSSPropertySetterFlags); CDECL;
var
    cls: TDSSClass;
    // propOffset: PtrUint;
    i: Integer;
    // propFlags: TPropertyFlags;
    sValue: String;
begin
    if (batch = NIL) or (batch^ = NIL) or (batchSize = 0) then
        Exit;

    cls := batch^.ParentClass;
    // propFlags := cls.PropertyFlags[Index];
    // propOffset := cls.PropertyOffset[Index];

    if not (cls.PropertyType[Index] in [
        // TPropertyType.StringEnumActionProperty, --TODO? Not in SetObjString
        //TPropertyType.MappedStringEnumOnStructArrayProperty, --TODO? Not in SetObjString
        TPropertyType.StringProperty,
        TPropertyType.BusProperty,
        TPropertyType.MappedStringEnumProperty,
        TPropertyType.BusOnStructArrayProperty,
        TPropertyType.DSSObjectReferenceProperty
    ]) then
        Exit;

    sValue := Value;
    for i := 1 to batchSize do
    begin
        batch^.SetString(Index, sValue, setterFlags);
        inc(batch);
    end;
end;

// procedure Batch_SetAsString(batch: TDSSObjectPtr; batchSize: Integer; Index: Integer; Value: PAnsiChar); CDECL;
// var
    // cls: TDSSClass;
    // i: Integer;
    // propFlags: TPropertyFlags;
    // sValue: String;
// begin
    // if (batch = NIL) or (batch^ = NIL) or (batchSize = 0) then
        // Exit;
//
    // cls := batch^.ParentClass;
    // propFlags := cls.PropertyFlags[Index];
    // propOffset := cls.PropertyOffset[Index];
    // sValue := Value;
    // for i := 1 to batchSize do
    // begin
        // batch^.ParsePropertyValue(Index, Value);
        // inc(batch);
    // end;
// end;

procedure Batch_SetObject(batch: TDSSObjectPtr; batchSize: Integer; Index: Integer; Value: TDSSObject; setterFlags: TDSSPropertySetterFlags); CDECL;
var
    cls: TDSSClass;
    // propOffset: PtrUint;
    i: Integer;
    // propFlags: TPropertyFlags;
begin
    if (batch = NIL) or (batch^ = NIL) or (batchSize = 0) then
        Exit;

    cls := batch^.ParentClass;
    // propFlags := cls.PropertyFlags[Index];
    // propOffset := cls.PropertyOffset[Index];

    if cls.PropertyType[Index] <> TPropertyType.DSSObjectReferenceProperty then
        Exit;

    for i := 1 to batchSize do
    begin
        batch^.SetObject(Index, Value, setterFlags);
        inc(batch);
    end;
end;

procedure Batch_SetFloat64Array(batch: TDSSObjectPtr; batchSize: Integer; Index: Integer; Value: PDouble; setterFlags: TDSSPropertySetterFlags); CDECL;
begin
    Batch_Float64Array(batch, batchSize, Index, BatchOp.SetValues, Value, setterFlags);
end;

procedure Batch_Int32Array(batch: TDSSObjectPtr; batchSize: Integer; Index: Integer; Operation: BatchOp; Value: PInteger; setterFlags: TDSSPropertySetterFlags); CDECL;
var
    cls: TDSSClass;
    propOffset: PtrUint;
    i: Integer;
    prev: Integer;
    intptr: PInteger;
    propFlags: TPropertyFlags;
    ptype: TPropertyType;
    ValueCursor: PInteger;
    singleEdit: Boolean;
    allowNA: Boolean;
begin
    if (batch = NIL) or (batch^ = NIL) or (batchSize = 0) or (Value = NIL) then
        Exit;

    allowNA := not (TDSSPropertySetterFlag.SkipNA in setterFlags);
    cls := batch^.ParentClass;
    propFlags := cls.PropertyFlags[Index];
    propOffset := cls.PropertyOffset[Index];

    ptype := cls.PropertyType[Index];
    if not (ptype in [
        TPropertyType.IntegerProperty,
        TPropertyType.MappedIntEnumProperty,
        TPropertyType.MappedStringEnumProperty,
        TPropertyType.BooleanProperty,
        TPropertyType.EnabledProperty,
        TPropertyType.IntegerOnStructArrayProperty
    ]) then
        Exit;

    if (ptype in [TPropertyType.BooleanProperty, TPropertyType.EnabledProperty, TPropertyType.BooleanActionProperty]) and not (Operation in [BatchOp.Increment]) then
    begin
        ValueCursor := Value;
        for i := 0 to batchSize - 1 do
        begin
            ValueCursor^ := Integer(LongBool(ValueCursor^ <> 0));
            inc(ValueCursor);
        end;
    end;

    // if (cls.PropertyType[Index] in [
    //     TPropertyType.IntegerProperty,
    //     TPropertyType.MappedIntEnumProperty,
    //     TPropertyType.MappedStringEnumProperty,
    //     TPropertyType.BooleanProperty]) and
    //     (not (TPropertyFlag.CustomSet in propFlags)) and
    //     (not (TPropertyFlag.WriteByFunction in propFlags)) and
    //     (not (TPropertyFlag.ScaledByFunction in propFlags)) then
    // begin
    //     for i := 1 to batchSize do
    //     begin
    //         intptr := (PInteger(PtrUint(batch^) + propoffset));
    //         prev := intptr^;
    //         intptr^ := Value^;

    //         inc(batch);
    //         inc(Value);
    //     end;
    //     Exit;
    // end;

    case Operation of
        BatchOp.Multiply:
            for i := 1 to batchSize do
            begin
                if (allowNA) or (Value^ <> $7fffffff) then
                    batch^.SetInteger(Index, Value^ * cls.GetObjInteger(batch^, Index), setterFlags);
                inc(batch);
                inc(Value);
            end;
        BatchOp.Divide:
            for i := 1 to batchSize do
            begin
                if (allowNA) or (Value^ <> $7fffffff) then
                    batch^.SetInteger(Index, cls.GetObjInteger(batch^, Index) div Value^, setterFlags);
                inc(batch);
                inc(Value);
            end;
        BatchOp.Increment:
            for i := 1 to batchSize do
            begin
                if (allowNA) or (Value^ <> $7fffffff) then
                    batch^.SetInteger(Index, Value^ + cls.GetObjInteger(batch^, Index), setterFlags);
                inc(batch);
                inc(Value);
            end;
        BatchOp.SetValues:
        begin
            if (cls.PropertyType[Index] <> TPropertyType.IntegerOnStructArrayProperty) and
                (propFlags = []) and
                (cls.PropertyScale[Index] = 1) then
            begin
                // Faster path
                for i := 1 to batchSize do
                begin
                    if (allowNA) or (Value^ <> $7fffffff) then
                    begin
                        // check for each element, in case the element is being edited somewhere else
                        singleEdit := not (Flg.EditingActive in batch^.Flags);
                        if singleEdit then
                            cls.BeginEdit(batch^, False);

                        intPtr := PInteger(PtrUint(batch^) + propOffset);
                        prev := intPtr^;
                        intPtr^ := Value^;
                        batch^.PropertySideEffects(Index, prev, setterFlags);

                        if singleEdit then
                            cls.EndEdit(batch^, 1);
                    end;
                    inc(batch);
                    inc(Value);
                end;
                Exit;
            end;

            for i := 1 to batchSize do
            begin
                if (allowNA) or (Value^ <> $7fffffff) then
                    batch^.SetInteger(Index, Value^, setterFlags);
                inc(batch);
                inc(Value);
            end;
        end;
    end;
end;

procedure Batch_SetInt32Array(batch: TDSSObjectPtr; batchSize: Integer; Index: Integer; Value: PInteger; setterFlags: TDSSPropertySetterFlags); CDECL;
begin
    Batch_Int32Array(batch, batchSize, Index, BatchOp.SetValues, Value, setterFlags);
end;

procedure Batch_SetStringArray(batch: TDSSObjectPtr; batchSize: Integer; Index: Integer; Value: PPAnsiChar; setterFlags: TDSSPropertySetterFlags); CDECL;
var
    cls: TDSSClass;
    // propOffset: PtrUint;
    i: Integer;
    // propFlags: TPropertyFlags;
    allowNA: Boolean;
begin
    if (batch = NIL) or (batch^ = NIL) or (batchSize = 0) then
        Exit;

    cls := batch^.ParentClass;
    // propFlags := cls.PropertyFlags[Index];
    // propOffset := cls.PropertyOffset[Index];

    if not (cls.PropertyType[Index] in [
        TPropertyType.StringEnumActionProperty,
        TPropertyType.StringProperty,
        TPropertyType.BusProperty,
        TPropertyType.MappedStringEnumProperty,
        TPropertyType.BusOnStructArrayProperty,
        TPropertyType.DSSObjectReferenceProperty
    ]) then
        Exit;

    allowNA := not (TDSSPropertySetterFlag.SkipNA in setterFlags);

    for i := 1 to batchSize do
    begin
        if (allowNA) or (Value^ <> NIL) then
            batch^.SetString(Index, Value^, setterFlags);
        inc(batch);
        inc(Value)
    end;
end;

procedure Batch_SetObjectArray(batch: TDSSObjectPtr; batchSize: Integer; Index: Integer; Value: TDSSObjectPtr; setterFlags: TDSSPropertySetterFlags); CDECL;
var
    cls: TDSSClass;
    // propOffset: PtrUint;
    i: Integer;
    // propFlags: TPropertyFlags;
begin
    if (batch = NIL) or (batch^ = NIL) or (batchSize = 0) then
        Exit;

    if Value = NIL then
    begin
        Batch_SetObject(batch, batchSize, Index, NIL, setterFlags);
        Exit;
    end;

    cls := batch^.ParentClass;
    // propFlags := cls.PropertyFlags[Index];
    // propOffset := cls.PropertyOffset[Index];

    if cls.PropertyType[Index] <> TPropertyType.DSSObjectReferenceProperty then
        Exit;

    for i := 1 to batchSize do
    begin
        batch^.SetObject(Index, Value^, setterFlags);
        inc(batch);
        inc(Value)
    end;
end;

procedure Batch_CreateFromNewS(DSS: TDSSContext; var ResultPtr: TDSSObjectPtr; ResultCount: PAPISize; clsName: String; Names: PPAnsiChar; Count: Integer; BeginEdit: TAltAPIBoolean); CDECL;
var
    clsIdx: Integer;
begin
    if DSS = NIL then DSS := DSSPrime;
    clsIdx := DSS.ClassNames.Find(clsname);
    if clsIdx = 0 then
        Exit;
    Batch_CreateFromNew(DSS, ResultPtr, ResultCount, clsIdx, Names, Count, BeginEdit);
end;

procedure Batch_CreateByClassS(DSS: TDSSContext; var ResultPtr: TDSSObjectPtr; ResultCount: PAPISize; clsName: PAnsiChar); CDECL;
var
    clsIdx: Integer;
begin
    if DSS = NIL then DSS := DSSPrime;
    clsIdx := DSS.ClassNames.Find(clsname);
    if clsIdx = 0 then
        Exit;
    Batch_CreateByClass(DSS, ResultPtr, ResultCount, clsIdx);
end;

procedure Batch_CreateByRegExpS(DSS: TDSSContext; var ResultPtr: TDSSObjectPtr; ResultCount: PAPISize; clsname: PAnsiChar; re: PAnsiChar); CDECL;
var
    clsIdx: Integer;
begin
    if DSS = NIL then DSS := DSSPrime;
    clsIdx := DSS.ClassNames.Find(clsname);
    if clsIdx = 0 then
        Exit;
    Batch_CreateByRegExp(DSS, ResultPtr, ResultCount, clsIdx, re);
end;

procedure Batch_CreateByIndexS(DSS: TDSSContext; var ResultPtr: TDSSObjectPtr; ResultCount: PAPISize; clsname: PAnsiChar; Value: PInteger; ValueCount: Integer); CDECL;
var
    clsIdx: Integer;
begin
    if DSS = NIL then DSS := DSSPrime;
    clsIdx := DSS.ClassNames.Find(clsname);
    if clsIdx = 0 then
    begin
        //writeln('cls not found, aborting.');
        Exit;
    end;
    Batch_CreateByIndex(DSS, ResultPtr, ResultCount, clsIdx, Value, ValueCount);
end;

procedure Batch_CreateByInt32PropertyS(DSS: TDSSContext; var ResultPtr: TDSSObjectPtr; ResultCount: PAPISize; clsname: PAnsiChar; propname: PAnsiChar; value: Integer); CDECL;
var
    i, clsIdx: Integer;
    propIdx: Integer = 0;
    cls: TDSSClass;
    spropname: String;
begin
    if DSS = NIL then DSS := DSSPrime;
    clsIdx := DSS.ClassNames.Find(clsname);
    //WriteLn('clsIdx = ', clsIdx);
    if clsIdx = 0 then
        Exit;

    cls := DSS.DSSClassList.At(clsIdx);
    spropname := propname;
    for i := 1 to cls.NumProperties do
    begin
        if CompareText(spropname, cls.PropertyName[i]) = 0 then
        begin
            propIdx := i;
            break;
        end;
    end;
    if propIdx = 0 then
        Exit;

    //WriteLn('propIdx = ', propIdx);
    Batch_CreateByInt32Property(DSS, ResultPtr, ResultCount, clsIdx, propidx, value);
end;

procedure Batch_CreateByFloat64PropertyRangeS(DSS: TDSSContext; var ResultPtr: TDSSObjectPtr; ResultCount: PAPISize; clsname: PAnsiChar; propname: PAnsiChar; valueMin: Double; valueMax: Double); CDECL;
var
    i, clsIdx: Integer;
    propIdx: Integer = 0;
    cls: TDSSClass;
    spropname: String;
begin
    if DSS = NIL then DSS := DSSPrime;
    clsIdx := DSS.ClassNames.Find(clsname);
    if clsIdx = 0 then
        Exit;

    cls := DSS.DSSClassList.At(clsIdx);
    spropname := propname;
    for i := 1 to cls.NumProperties do
    begin
        if CompareText(spropname, cls.PropertyName[i]) = 0 then
        begin
            propIdx := i;
            break;
        end;
    end;
    if propIdx = 0 then
        Exit;

    Batch_CreateByFloat64PropertyRange(DSS, ResultPtr, ResultCount, clsIdx, propidx, valueMin, valueMax);
end;

//------------------------------------------------------------------------------

function GetPropIndex(batch: TDSSObjectPtr; Name: String; out propIdx: Integer): Boolean;
var
    i: Integer;
    cls: TDSSClass;
begin
    cls := batch^.ParentClass;
    for i := 1 to cls.NumProperties do
    begin
        if CompareText(Name, cls.PropertyName[i]) = 0 then
        begin
            propIdx := i;
            Result := True;
            Exit;
        end;
    end;
    Result := False;
end;

procedure Batch_GetFloat64S(var ResultPtr: PDouble; ResultCount: PAPISize; batch: TDSSObjectPtr; batchSize: Integer; Name: PChar); CDECL;
var
    propIdx: Integer;
begin
    if (batch = NIL) or (batch^ = NIL) or (batchSize <= 0) then
        Exit;

    if not GetPropIndex(batch, Name, propIdx) then
        Exit;

    Batch_GetFloat64(ResultPtr, ResultCount, batch, batchSize, propIdx);
end;

procedure Batch_GetInt32S(var ResultPtr: PInteger; ResultCount: PAPISize; batch: TDSSObjectPtr; batchSize: Integer; Name: PChar); CDECL;
var
    propIdx: Integer;
begin
    if (batch = NIL) or (batch^ = NIL) or (batchSize <= 0) then
        Exit;

    if not GetPropIndex(batch, Name, propIdx) then
        Exit;

    Batch_GetInt32(ResultPtr, ResultCount, batch, batchSize, propIdx);
end;

procedure Batch_GetStringS(var ResultPtr: PPAnsiChar; ResultCount: PAPISize; batch: TDSSObjectPtr; batchSize: Integer; Name: PChar); CDECL;
var
    propIdx: Integer;
begin
    if (batch = NIL) or (batch^ = NIL) or (batchSize <= 0) then
        Exit;

    if not GetPropIndex(batch, Name, propIdx) then
        Exit;

    Batch_GetString(ResultPtr, ResultCount, batch, batchSize, propIdx);
end;

procedure Batch_GetAsStringS(var ResultPtr: PPAnsiChar; ResultCount: PAPISize; batch: TDSSObjectPtr; batchSize: Integer; Name: PChar); CDECL;
var
    propIdx: Integer;
begin
    if (batch = NIL) or (batch^ = NIL) or (batchSize <= 0) then
        Exit;

    if not GetPropIndex(batch, Name, propIdx) then
        Exit;

    Batch_GetAsString(ResultPtr, ResultCount, batch, batchSize, propIdx);
end;

procedure Batch_GetObjectS(var ResultPtr: PPointer; ResultCount: PAPISize; batch: TDSSObjectPtr; batchSize: Integer; Name: PChar); CDECL;
var
    propIdx: Integer;
begin
    if (batch = NIL) or (batch^ = NIL) or (batchSize <= 0) then
        Exit;

    if not GetPropIndex(batch, Name, propIdx) then
        Exit;

    Batch_GetObject(ResultPtr, ResultCount, batch, batchSize, propIdx);
end;

// procedure Batch_SetAsStringS(batch: TDSSObjectPtr; batchSize: Integer; Name: PChar; Value: PAnsiChar); CDECL;
// var
//     propIdx: Integer;
// begin
//     if (batch = NIL) or (batch^ = NIL) or (batchSize <= 0) then
//         Exit;

//     if not GetPropIndex(batch, Name, propIdx) then
//         Exit;

//     Batch_SetAsString(batch, batchSize, propIdx, Value);
// end;

procedure Batch_Float64S(batch: TDSSObjectPtr; batchSize: Integer; Name: PChar; Operation: BatchOp; Value: Double; setterFlags: TDSSPropertySetterFlags); CDECL;
var
    propIdx: Integer;
begin
    if (batch = NIL) or (batch^ = NIL) or (batchSize <= 0) then
        Exit;

    if not GetPropIndex(batch, Name, propIdx) then
        Exit;

    Batch_Float64(batch, batchSize, propIdx, Operation, Value, setterFlags);
end;

procedure Batch_Int32S(batch: TDSSObjectPtr; batchSize: Integer; Name: PChar; Operation: BatchOp; Value: Integer; setterFlags: TDSSPropertySetterFlags); CDECL;
var
    propIdx: Integer;
begin
    if (batch = NIL) or (batch^ = NIL) or (batchSize <= 0) then
        Exit;

    if not GetPropIndex(batch, Name, propIdx) then
        Exit;

    Batch_Int32(batch, batchSize, propIdx, Operation, Value, setterFlags);
end;

procedure Batch_SetStringS(batch: TDSSObjectPtr; batchSize: Integer; Name: PChar; Value: PAnsiChar; setterFlags: TDSSPropertySetterFlags); CDECL;
var
    propIdx: Integer;
begin
    if (batch = NIL) or (batch^ = NIL) or (batchSize <= 0) then
        Exit;

    if not GetPropIndex(batch, Name, propIdx) then
        Exit;

    Batch_SetString(batch, batchSize, propIdx, Value, setterFlags);
end;

procedure Batch_SetObjectS(batch: TDSSObjectPtr; batchSize: Integer; Name: PChar; Value: TDSSObject; setterFlags: TDSSPropertySetterFlags); CDECL;
var
    propIdx: Integer;
begin
    if (batch = NIL) or (batch^ = NIL) or (batchSize <= 0) then
        Exit;

    if not GetPropIndex(batch, Name, propIdx) then
        Exit;

    Batch_SetObject(batch, batchSize, propIdx, Value, setterFlags);
end;

procedure Batch_Float64ArrayS(batch: TDSSObjectPtr; batchSize: Integer; Name: PChar; Operation: BatchOp; Value: PDouble; setterFlags: TDSSPropertySetterFlags); CDECL;
var
    propIdx: Integer;
begin
    if (batch = NIL) or (batch^ = NIL) or (batchSize <= 0) or (Value = NIL) then
        Exit;

    if not GetPropIndex(batch, Name, propIdx) then
        Exit;

    Batch_Float64Array(batch, batchSize, propIdx, Operation, Value, setterFlags);
end;

procedure Batch_SetFloat64ArrayS(batch: TDSSObjectPtr; batchSize: Integer; Name: PChar; Value: PDouble; setterFlags: TDSSPropertySetterFlags); CDECL;
begin
    Batch_Float64ArrayS(batch, batchSize, Name, BatchOp.SetValues, Value, setterFlags);
end;

procedure Batch_Int32ArrayS(batch: TDSSObjectPtr; batchSize: Integer; Name: PChar; Operation: BatchOp; Value: PInteger; setterFlags: TDSSPropertySetterFlags); CDECL;
var
    propIdx: Integer;
begin
    if (batch = NIL) or (batch^ = NIL) or (batchSize <= 0) or (Value = NIL) then
        Exit;

    if not GetPropIndex(batch, Name, propIdx) then
        Exit;

    Batch_Int32Array(batch, batchSize, propIdx, Operation, Value, setterFlags);
end;

procedure Batch_SetInt32ArrayS(batch: TDSSObjectPtr; batchSize: Integer; Name: PChar; Value: PInteger; setterFlags: TDSSPropertySetterFlags); CDECL;
begin
    Batch_Int32ArrayS(batch, batchSize, Name, BatchOp.SetValues, Value, setterFlags);
end;

procedure Batch_SetStringArrayS(batch: TDSSObjectPtr; batchSize: Integer; Name: PChar; Value: PPAnsiChar; setterFlags: TDSSPropertySetterFlags); CDECL;
var
    propIdx: Integer;
begin
    if (batch = NIL) or (batch^ = NIL) or (batchSize <= 0) then
        Exit;

    if not GetPropIndex(batch, Name, propIdx) then
        Exit;

    Batch_SetStringArray(batch, batchSize, propIdx, Value, setterFlags);
end;

procedure Batch_SetObjectArrayS(batch: TDSSObjectPtr; batchSize: Integer; Name: PChar; Value: TDSSObjectPtr; setterFlags: TDSSPropertySetterFlags); CDECL;
var
    propIdx: Integer;
begin
    if (batch = NIL) or (batch^ = NIL) or (batchSize <= 0) then
        Exit;

    if not GetPropIndex(batch, Name, propIdx) then
        Exit;

    Batch_SetObjectArray(batch, batchSize, propIdx, Value, setterFlags);
end;

//------------------------------------------------------------------------------
procedure saveOpenTerminalsJSON(ckt: TDSSCircuit; var cmds: TJSONArray);
// Equivalent of TDSSCircuit.SaveOpenTerminals
var
    elem: TDSSCktElement;
    name: String;
    i, termIdx, numCondOpen: Integer;
begin
    for elem in ckt.CktElements do
    begin
        if elem.AllConductorsClosed() then
            continue;

        name := CheckForBlanks(elem.FullName);
        for termIdx := 0 to elem.NTerms - 1 do
        begin
            numCondOpen := 0;
            for i := 0 to elem.NConds - 1 do 
            begin
                if not elem.Terminals[termIdx].ConductorsClosed[i] then
                    numCondOpen += 1;
            end;
            if numCondOpen = 0 then
                continue;

            if numCondOpen = elem.NConds then
            begin   
                // Open all conductors in the terminal, easy path
                cmds.Add(Format('Open %s %d', [name, termIdx + 1]));
                continue;
            end;

            // Open specific conductors
            for i := 0 to elem.NConds - 1 do 
            begin
                if elem.Terminals[termIdx].ConductorsClosed[i] then
                    continue;

                cmds.Add(Format('Open %s %d %d', [name, termIdx + 1, i + 1]));
            end;
        end;
    end;
end;

function Obj_Circuit_ToJSON_(ckt: TDSSCircuit; joptions: Integer): PAnsiChar;
var
    circ: TJSONObject = NIL;
    busArray: TJSONArray = NIL;
    clsArray: TJSONArray = NIL;
    cmds: TJSONArray = NIL;
    // vsrc: TVSourceObj = NIL;
    cls: TDSSClass;
    obj: TDSSObject;
    first: Boolean;
    // vsrccls: TDSSClass;
    bus: TDSSBus;
    i: Integer;
    exportDefaultObjs: Boolean;
    DSS: TDSSContext;
begin
    DSS := ckt.DSS;
    exportDefaultObjs := (joptions and Integer(DSSJSONOptions.IncludeDefaultObjs)) <> 0;
    Result := NIL;
    try
        cmds := TJSONArray.Create();
        if (joptions and Integer(DSSJSONOptions.SkipTimestamp)) = 0 then
            cmds.Add(Format('! Last saved by AltDSS/%s on %s',  [VersionString, DateToISO8601(Now())]));
        if ckt.PositiveSequence then
            cmds.Add(Format('Set CktModel=%s', [ckt.DSS.CktModelEnum.OrdinalToString(Integer(ckt.PositiveSequence))]));
        if ckt.DuplicatesAllowed then
            cmds.Add('Set AllowDuplicates=True');
        if ckt.LongLineCorrection then
            cmds.Add('Set LongLineCorrection=True');

        cmds.Add('Set EarthModel=' + ckt.DSS.EarthModelEnum.OrdinalToString(ckt.DSS.DefaultEarthModel));
        cmds.Add('Set VoltageBases=' + GetDSSArray(ckt.LegalVoltageBases));
        if (joptions and Integer(DSSJSONOptions.SkipBuses)) = 0 then
        begin
            busArray := TJSONArray.Create();
            for i := 1 to ckt.NumBuses do
            begin
                busArray.Add(alt_Bus_ToJSON_(ckt.DSS, ckt.Buses[i], joptions));
            end;
        end;

        circ := TJSONObject.Create([
            '$schema', ALTDSS_SCHEMA_ID,
            'Name', ckt.Name,
            'DefaultBaseFreq', ckt.DSS.DefaultBaseFreq,
            'PreCommands', cmds
            // MakeBusList as a PostCommand is implicit
        ]);
        if busArray <> NIL then
        begin
            circ.Add('Bus', busArray);
            busArray := NIL;
        end;

        // This will be automated later
        cmds := TJSONArray.Create();
        // cmds.Add('Set Mode=' + DSS.SolveModeEnum.OrdinalToString(ord(ckt.Solution.mode)));
        cmds.Add('Set ControlMode=' + DSS.ControlModeEnum.OrdinalToString(ckt.Solution.Controlmode));
        cmds.Add('Set Random=' + DSS.RandomModeEnum.OrdinalToString(ckt.Solution.RandomType));
        cmds.Add('Set frequency=' + Format('%-g', [ckt.Solution.Frequency]));
        cmds.Add('Set stepsize=' + Format('%-g', [ckt.Solution.DynaVars.h]));
        cmds.Add('Set number=' + IntToStr(ckt.Solution.NumberOfTimes));
        cmds.Add('Set tolerance=' + Format('%-g', [ckt.Solution.ConvergenceTolerance]));
        cmds.Add('Set maxiterations=' + IntToStr(ckt.Solution.MaxIterations));
        cmds.Add('Set miniterations=' + IntToStr(ckt.Solution.MinIterations));
        cmds.Add('Set loadmodel=' + DSS.DefaultLoadModelEnum.OrdinalToString(ckt.Solution.LoadModel));
        cmds.Add('Set loadmult=' + Format('%-g', [ckt.LoadMultiplier]));
        cmds.Add('Set Normvminpu=' + Format('%-g', [ckt.NormalMinVolts]));
        cmds.Add('Set Normvmaxpu=' + Format('%-g', [ckt.NormalMaxVolts]));
        cmds.Add('Set Emergvminpu=' + Format('%-g', [ckt.EmergMinVolts]));
        cmds.Add('Set Emergvmaxpu=' + Format('%-g', [ckt.EmergMaxVolts]));
        cmds.Add('Set %mean=' + Format('%-.4g', [ckt.DefaultDailyShapeObj.Mean * 100.0]));
        cmds.Add('Set %stddev=' + Format('%-.4g', [ckt.DefaultDailyShapeObj.StdDev * 100.0]));
        cmds.Add('Set LDCurve=' + NameIfNotNil(ckt.LoadDurCurveObj));
        cmds.Add('Set %growth=' + Format('%-.4g', [((ckt.DefaultGrowthRate - 1.0) * 100.0)]));  // default growth rate
        cmds.Add('Set genkw=' + Format('%-g', [ckt.AutoAddObj.GenkW]));
        cmds.Add('Set genpf=' + Format('%-g', [ckt.AutoAddObj.GenPF]));
        cmds.Add('Set capkvar=' + Format('%-g', [ckt.AutoAddObj.Capkvar]));
        cmds.Add('Set addtype=' + DSS.AddTypeEnum.OrdinalToString(DSS.ActiveCircuit.AutoAddObj.AddType));
        cmds.Add('Set zonelock=' + StrYorN(ckt.ZonesLocked));
        cmds.Add(Format('Set ueweight=%8.2f', [ckt.UEWeight]));
        cmds.Add(Format('Set lossweight=%8.2f', [ckt.LossWeight]));
        cmds.Add('Set ueregs=' + IntArraytoString(ckt.UEregs));
        cmds.Add('Set lossregs=' + IntArraytoString(ckt.Lossregs));
        cmds.Add('Set algorithm=' + DSS.SolveAlgEnum.OrdinalToString(ckt.Solution.Algorithm));
        cmds.Add('Set Trapezoidal=' + StrYorN(ckt.TrapezoidalIntegration));
        cmds.Add('Set genmult=' + Format('%-g', [ckt.GenMultiplier]));
        cmds.Add('Set Basefrequency=' + Format('%-g', [ckt.Fundamental]));
        if ckt.Solution.DoAllHarmonics then
            cmds.Add('Set harmonics=ALL')
        else
            cmds.Add('Set harmonics=' + GetDSSArray(ckt.Solution.HarmonicList));
        cmds.Add('Set maxcontroliter=' + IntToStr(ckt.Solution.MaxControlIterations));

        saveOpenTerminalsJSON(ckt, cmds);

        circ.Add('PostCommands', cmds);
        cmds := NIL;

        // vsrccls := ckt.DSS.VSourceClass;
        for cls in ckt.DSS.DSSClassList do
        begin
            clsArray := TJSONArray.Create();
            // if cls = vsrccls then
            // begin
            //     first := True;
            //     for obj in cls do
            //     begin
            //         if first then
            //         begin
            //             vsrc := TSourceObj(obj);
            //             first := false;
            //             continue;
            //         end;
            //         clsArray.Add(Obj_ToJSONData(obj, joptions));
            //     end;
            //     circ.Add('circuitVsource', Obj_ToJSONData(vsrc, joptions));
            // end;
            // else
            // begin
            if exportDefaultObjs then
            begin
                for obj in cls do
                begin
                    clsArray.Add(Obj_ToJSONData(obj, joptions));
                end;
            end
            else
            begin
                for obj in cls do
                begin
                    if not (Flg.DefaultAndUnedited in obj.Flags) then
                        clsArray.Add(Obj_ToJSONData(obj, joptions));
                end;
            end;
            // end;
            if clsArray.Count <> 0 then
                circ.Add(cls.Name, clsArray)
            else
                clsArray.Free();
            clsArray := NIL;
        end;

        Result := DSS_GetAsPAnsiChar(ckt.DSS, circ.FormatJSON());
    except
        on E: Exception do
            DoSimpleMsg(ckt.DSS, 'Error converting data to JSON: %s', [E.message], 5020);
    end;

    if cmds <> NIL then
        cmds.Free();
    if circ <> NIL then
        circ.Free();
    if busArray <> NIL then
        busArray.Free();
    if clsArray <> NIL then
        clsArray.Free();
    // if vsrc <> NIL then
    //     vsrc.Free();
end;

procedure loadClassFromJSON(DSS: TDSSContext; cls: TDSSClass; jcls: TJSONData; joptions: Integer);
// 1. Load from array, or
// 2. Load array from file, then 1, or
// 3. Incrementally load from JSONLines
var
    obj: TJSONObject;
    data: TJSONData = NIL;
    arr: TJSONArray = NIL;
    arrItem: TJSONData = NIL;
    jsonFilePath: TJSONString = NIL;
    jsonLinesFilePath: TJSONString = NIL;
    F: TStream = NIL;
    lineNum: Integer;
    specialFirst: Boolean;
    line: String;
    dupsAllowed: Boolean;
    localError: Boolean = false;
    typeStr: String;
    elementName: String = '';

    procedure loadSingleObj(o: TJSONObject; num: Integer);
    var
        dssObj: TDSSObject;
        nameData: TJSONData = NIL;
        name: String;
        extraOptions: Integer = 0;
    begin
        try
            nameData := o.Extract('Name');
            if nameData = NIL then
                nameData := o.Extract('name');

            if nameData = NIL then
                raise Exception.Create(Format('JSON/%s: missing "Name" from item %d.', [cls.Name, num]));

            name := nameData.Value;
            elementName := name; // context for potential expections
            if not specialFirst then
            begin
                if dupsAllowed then
                begin
                    dssObj := obj_NewFromClass(DSS, cls, name, false, true);
                end
                else
                begin // Check to see if we can set it active first
                    dssObj := cls.Find(Name, true);
                    if dssObj = NIL then
                    begin
                        dssObj := obj_NewFromClass(DSS, cls, name, false, true);
                    end
                    else
                    begin
                        if (cls.DSSClassType <> DSS_OBJECT) then 
                        begin
                            // Allow redefining/editing default objects to mirror the DSS scripting behavior.
                            // Error out for everything else.
                            DoSimpleMsg(DSS, 'Duplicate new element definition: "%s.%s".', [DSS.ActiveDSSClass.Name, Name], 266);
                            Exit;
                        end;
                        extraOptions := ord(DSSJSONOptions.Edit);
                    end;
                end;
            end
            else
            begin
                specialFirst := false;
                dssObj := cls.ElementList.Get(1);
            end;
            if not cls.FillObjFromJSON(dssObj, o, joptions or extraOptions, []) then
                raise Exception.Create(Format('JSON/%s/%s: error processing item.', [cls.Name, name]));
        finally
            if nameData <> NIL then
                nameData.Free();
        end;
    end;
begin
    specialFirst := (cls = DSS.VSourceClass);
    dupsAllowed := DSS.ActiveCircuit.DuplicatesAllowed and (cls.DSSClassType <> DSS_OBJECT);
    try
        if jcls is TJSONObject then
        begin
            obj := TJSONObject(jcls);
            if obj.Find('JSONFile', jsonFilePath) then
            begin
                F := DSS.GetInputStreamEx(jsonFilePath.Value);
                data := GetJSON(F);
                if not (data is TJSONArray) then
                    raise Exception.Create(Format('JSON/%s: unexpected format in file "%s".', [cls.Name, jsonFilePath.Value]));
                arr := TJSONArray(data);
                // continue as if the array was built-in
            end
            else
            if obj.Find('JSONLinesFile', jsonLinesFilePath) then
            begin
                F := DSS.GetInputStreamEx(jsonLinesFilePath.Value);
                lineNum := 1;
                while (F.Position + 1) < F.Size do
                begin
                    FSReadln(F, line);
                    data := GetJSON(line);
                    if not (data is TJSONObject) then
                        raise Exception.Create(Format('JSON/%s: unexpected format in file "%s", line %d.', [cls.Name, jsonLinesFilePath.Value, lineNum]));

                    loadSingleObj(TJSONObject(data), lineNum);
                    FreeAndNil(data);
                    if DSS.ErrorNumber <> 0 then
                        break;
                    lineNum += 1;
                end;
                Exit;
            end;
        end
        else
        if jcls is TJSONArray then
        begin
            arr := TJSONArray(jcls);
        end;

        if arr = NIL then
        begin
            WriteStr(typeStr, jcls.JSONtype);
            typeStr := Copy(typeStr, 3, length(typeStr));
            raise Exception.Create(Format('JSON/%s: unexpected format (%s) for class container.', [cls.Name, typeStr])); 
        end;

        for lineNum := 0 to arr.Count - 1 do
        begin
            arrItem := arr.Items[lineNum];
            if not (arrItem is TJSONObject) then
                raise Exception.Create(Format('JSON/%s: unexpected format for object number %d.', [cls.Name, lineNum])); 
            loadSingleObj(arrItem as TJSONObject, lineNum);
            if DSS.ErrorNumber <> 0 then
                break;
        end;

    except
        on E: Exception do
        begin
            line := '';
            if arrItem <> NIL then
                line := arrItem.FormatJSON([foSingleLineObject, foSingleLineArray]);

            if DSS.ErrorNumber = 0 then
            begin
                if Length(line) <> 0 then
                begin
                    if Length(elementName) <> 0 then
                    begin
                        line := Format('Element: %s.%s. Context follows: `%s`', [cls.Name, elementName, line]);
                    end
                    else
                    begin
                        line := 'Context follows: `' + line + '`';
                    end;
                end;
                DoSimpleMsg(DSS, 'Error loading %s record from JSON: %s %s', [cls.Name, E.message, line], 5021);
                localError := true;
            end;
        end;
    end;

    if (not localError) and (DSS.ErrorNumber <> 0) then
    begin
        line := '';
        if arrItem <> NIL then
        begin
            if Length(line) <> 0 then
            begin
                if Length(elementName) <> 0 then
                begin
                    line := Format('Element: %s.%s. Context follows: `%s`', [
                        cls.Name, 
                        elementName, 
                        arrItem.FormatJSON([foSingleLineObject, foSingleLineArray])
                    ]);
                end
                else
                begin
                    line := 'Context follows: `' + arrItem.FormatJSON([foSingleLineObject, foSingleLineArray]) + '`';
                end;
            end;
        end;
        DoSimpleMsg(DSS, 'Error loading %s record from JSON (#%d): %s %s', 
            [cls.Name, DSS.ErrorNumber, DSS.LastErrorMessage, line], 5021
        );
    end;

    FreeAndNil(F);
    FreeAndNil(data);
end;

procedure busFromJSON(DSS: TDSSContext; obj: TJSONObject; arrayIdx: Integer);
var
    busIdx: Integer;
    bus: TDSSBus;
    name: String;
    numVal: TJSONNumber;
    boolVal: TJSONBoolean;
    kVDone: Boolean = false;
begin
    busIdx := DSS.ActiveCircuit.BusList.Find(obj['Name'].AsString);
    if (busIdx = 0) then
        Exit; // TODO: error?

    bus := DSS.ActiveCircuit.Buses[busIdx];
    
    if obj.Find('X', numVal) then
    begin
        bus.CoordDefined := true;
        bus.X := numVal.AsFloat;
    end;
    if obj.Find('Y', numVal) then
    begin
        bus.CoordDefined := true;
        bus.Y := numVal.AsFloat;
    end;
    if obj.Find('Keep', boolVal) then
    begin
        bus.keep := boolVal.AsBoolean;
    end;
    if obj.Find('kVLN', numVal) then
    begin
        bus.kVBase := numVal.AsFloat;
        kVDone := true;
    end;
    if obj.Find('kVLL', numVal) then
    begin
        if not kVDone then
            bus.kVBase := numVal.AsFloat / SQRT3
        else
            raise Exception.Create(_('Both "kVLN" and "kVLL" were specified.'));
    end;
end;

procedure Obj_Circuit_FromJSON_(DSS: TDSSContext; jckt: TJSONObject; joptions: Integer);
var
    ckt: TDSSCircuit;
    tmp: TJSONData;
    cls: TDSSClass;
    items, item: TJSONArray;
    i: Integer;
begin
    DSS.DSSExecutive.Clear();
    tmp := jckt.Find('DefaultBaseFreq');
    if tmp <> NIL then
        DSS.DefaultBaseFreq := tmp.AsFloat;

    MakeNewCircuit(DSS, jckt.Get('Name', 'untitled'));
    tmp := jckt.Find('PreCommands');
    if (tmp <> NIL) then
    begin
        if not (tmp is TJSONArray) then
            raise Exception.Create('"PreCommands" must be an array of strings, if provided.');

        items := tmp as TJSONArray;
        for i := 0 to items.Count - 1 do
        begin
            DSS.DSSExecutive.ParseCommand(items.Items[i].AsString, i + 1);
            if DSS.ErrorNumber <> 0 then
                Exit;
        end;
    end;

    for cls in DSS.DSSClassList do
    begin
        tmp := jckt.Find(cls.Name);
        if tmp = NIL then
            continue;

        loadClassFromJSON(DSS, cls, tmp, joptions);
        if DSS.ErrorNumber <> 0 then
            Exit;
    end;

    // "MakeBusList"
    if DSS.ActiveCircuit.BusNameRedefined then
        DSS.ActiveCircuit.ReprocessBusDefs();

    tmp := jckt.Find('Bus'); 
    if (tmp <> NIL) then // Providing buses is optional
    begin
        if not (tmp is TJSONArray) then
            raise Exception.Create('"Bus" must be an array of bus objects, if provided.');

        items := tmp as TJSONArray;
        for i := 0 to items.Count - 1 do
        begin
            tmp := items.Items[i];
            if not (tmp is TJSONObject) then
                raise Exception.Create(Format(_('"Bus[%d]" must be a bus object.'), [i]));

            busFromJSON(DSS, tmp as TJSONObject, i);
        end;
    end;
    tmp := jckt.Find('PostCommands');
    if (tmp <> NIL) then
    begin
        if not (tmp is TJSONArray) then
            raise Exception.Create('"PostCommands" must be an array of strings, if provided.');

        items := tmp as TJSONArray;
        for i := 0 to items.Count - 1 do
        begin
            DSS.DSSExecutive.ParseCommand(items.Items[i].AsString, i + 1);
            if DSS.ErrorNumber <> 0 then
                Exit;
        end;
    end;

end;
//------------------------------------------------------------------------------
end.
