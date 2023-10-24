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
    Circuit;

type
    dss_obj_float64_function_t = function (obj: Pointer): Double; CDECL;    
    dss_obj_int32_function_t = function (obj: Pointer): Integer; CDECL;    

//TODO: decise if we want to expose the metadata (property index, name and type) now or later

// The classic API keeps the string buffer in the global state,
// but since this new API wants to avoid that, users must dispose
// the string copies themselves.
//TODO: consider using the same API as numeric arrays for string
procedure DSS_Dispose_String(S: PAnsiChar); CDECL;

function Obj_New(DSS: TDSSContext; ClsIdx: Integer; Name: PAnsiChar; Activate: TAPIBoolean; BeginEdit: TAPIBoolean): Pointer; CDECL;
function Obj_GetHandleByName(DSS: TDSSContext; ClsIdx: Integer; Name: PAnsiChar): Pointer; CDECL;
function Obj_GetHandleByIdx(DSS: TDSSContext; ClsIdx: Integer; Idx: Integer): Pointer; CDECL;

function Obj_GetName(Handle: Pointer): PAnsiChar; CDECL;
function Obj_GetNumProperties(Handle: Pointer): Integer; CDECL;
function Obj_GetCount(DSS: TDSSContext; ClsIdx: Integer): Integer; CDECL;
function Obj_GetListPointer(DSS: TDSSContext; ClsIdx: Integer): PPointer; CDECL;
function Obj_GetIdx(Handle: Pointer): Integer; CDECL;
function Obj_GetClassName(Handle: Pointer): PAnsiChar; CDECL;
function Obj_GetClassIdx(Handle: Pointer): Integer; CDECL;
function Obj_PropertySideEffects(Handle: Pointer; Index: Integer; PreviousInt: Integer): TAPIBoolean; CDECL;
procedure Obj_BeginEdit(Handle: Pointer); CDECL;
procedure Obj_EndEdit(Handle: Pointer; NumChanges: Integer); CDECL;
procedure Obj_Activate(Handle: Pointer; AllLists: TAPIBoolean); CDECL;
function Obj_GetPropSeqPtr(Handle: Pointer): PInteger; CDECL;

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

procedure Obj_SetAsString(obj: TDSSObject; Index: Integer; Value: PAnsiChar); CDECL;
procedure Obj_SetFloat64(obj: TDSSObject; Index: Integer; Value: Double); CDECL;
procedure Obj_SetInt32(obj: TDSSObject; Index: Integer; Value: Integer); CDECL;
procedure Obj_SetString(obj: TDSSObject; Index: Integer; Value: PAnsiChar); CDECL;
procedure Obj_SetObject(obj: TDSSObject; Index: Integer; Value: TDSSObject); CDECL;

procedure Obj_SetFloat64Array(obj: TDSSObject; Index: Integer; Value: PDouble; ValueCount: Integer); CDECL;
procedure Obj_SetInt32Array(obj: TDSSObject; Index: Integer; Value: PInteger; ValueCount: Integer); CDECL;
procedure Obj_SetStringArray(obj: TDSSObject; Index: Integer; Value: PPAnsiChar; ValueCount: Integer); CDECL;
procedure Obj_SetObjectArray(obj: TDSSObject; Index: Integer; Value: TDSSObjectPtr; ValueCount: Integer); CDECL;

// internal functions
function Obj_ToJSON_(obj: TDSSObject; joptions: Integer): String;
function Obj_ToJSONData(obj: TDSSObject; joptions: Integer): TJSONData;

// Batch: creation and state setup
procedure Batch_CreateFromNew(DSS: TDSSContext; var ResultPtr: TDSSObjectPtr; ResultCount: PAPISize; ClsIdx: Integer; Names: PPAnsiChar; Count: Integer; BeginEdit: TAPIBoolean); CDECL;
procedure Batch_Dispose(batch: Pointer); CDECL;
procedure Batch_BeginEdit(batch: TDSSObjectPtr; batchSize: Integer); CDECL;
procedure Batch_EndEdit(batch: TDSSObjectPtr; batchSize: Integer; NumEdits: Integer); CDECL;
procedure Batch_GetPropSeq(var ResultPtr: PInteger; ResultCount: PAPISize; batch: TDSSObjectPtr; batchSize: Integer); CDECL;

// Batch -- using class and property indices
procedure Batch_CreateByClass(DSS: TDSSContext; var ResultPtr: TDSSObjectPtr; ResultCount: PAPISize; clsIdx: Integer); CDECL;
procedure Batch_CreateByRegExp(DSS: TDSSContext; var ResultPtr: TDSSObjectPtr; ResultCount: PAPISize; clsIdx: Integer; re: PAnsiChar); CDECL;
procedure Batch_CreateByIndex(DSS: TDSSContext; var ResultPtr: TDSSObjectPtr; ResultCount: PAPISize; ClsIdx: Integer; Value: PInteger; ValueCount: Integer); CDECL;
procedure Batch_CreateByInt32Property(DSS: TDSSContext; var ResultPtr: TDSSObjectPtr; ResultCount: PAPISize; ClsIdx: Integer; propidx: Integer; value: Integer); CDECL;

function Batch_ToJSON(batch: TDSSObjectPtr; batchSize: Integer; joptions: Integer): PAnsiChar; CDECL;

procedure Batch_GetFloat64(var ResultPtr: PDouble; ResultCount: PAPISize; batch: TDSSObjectPtr; batchSize: Integer; Index: Integer); CDECL;
procedure Batch_GetInt32(var ResultPtr: PInteger; ResultCount: PAPISize; batch: TDSSObjectPtr; batchSize: Integer; Index: Integer); CDECL;
procedure Batch_GetString(var ResultPtr: PPAnsiChar; ResultCount: PAPISize; batch: TDSSObjectPtr; batchSize: Integer; Index: Integer); CDECL;
procedure Batch_GetAsString(var ResultPtr: PPAnsiChar; ResultCount: PAPISize; batch: TDSSObjectPtr; batchSize: Integer; Index: Integer); CDECL;
procedure Batch_GetObject(var ResultPtr: PPointer; ResultCount: PAPISize; batch: TDSSObjectPtr; batchSize: Integer; Index: Integer); CDECL;
procedure Batch_GetFloat64FromFunc(var ResultPtr: PDouble; ResultCount: PAPISize; batch: TDSSObjectPtr; batchSize: Integer; func: dss_obj_float64_function_t); CDECL;
procedure Batch_GetInt32FromFunc(var ResultPtr: PInteger; ResultCount: PAPISize; batch: TDSSObjectPtr; batchSize: Integer; func: dss_obj_int32_function_t); CDECL;

// procedure Batch_SetAsString(batch: TDSSObjectPtr; batchSize: Integer; Index: Integer; Value: PAnsiChar); CDECL;
procedure Batch_Float64(batch: TDSSObjectPtr; batchSize: Integer; Index: Integer; Operation: Integer; Value: Double); CDECL;
procedure Batch_Int32(batch: TDSSObjectPtr; batchSize: Integer; Index: Integer; Operation: Integer; Value: Integer); CDECL;
procedure Batch_SetString(batch: TDSSObjectPtr; batchSize: Integer; Index: Integer; Value: PAnsiChar); CDECL;
procedure Batch_SetObject(batch: TDSSObjectPtr; batchSize: Integer; Index: Integer; Value: TDSSObject); CDECL;

procedure Batch_SetFloat64Array(batch: TDSSObjectPtr; batchSize: Integer; Index: Integer; Value: PDouble); CDECL;
procedure Batch_SetInt32Array(batch: TDSSObjectPtr; batchSize: Integer; Index: Integer; Value: PInteger); CDECL;
procedure Batch_SetStringArray(batch: TDSSObjectPtr; batchSize: Integer; Index: Integer; Value: PPAnsiChar); CDECL;
procedure Batch_SetObjectArray(batch: TDSSObjectPtr; batchSize: Integer; Index: Integer; Value: TDSSObjectPtr); CDECL;

// Batch -- using class and property names
procedure Batch_CreateFromNewS(DSS: TDSSContext; var ResultPtr: TDSSObjectPtr; ResultCount: PAPISize; clsName: String; Names: PPAnsiChar; Count: Integer; BeginEdit: TAPIBoolean); CDECL;
procedure Batch_CreateByClassS(DSS: TDSSContext; var ResultPtr: TDSSObjectPtr; ResultCount: PAPISize; clsName: PAnsiChar); CDECL;
procedure Batch_CreateByRegExpS(DSS: TDSSContext; var ResultPtr: TDSSObjectPtr; ResultCount: PAPISize; clsname: PAnsiChar; re: PAnsiChar); CDECL;
procedure Batch_CreateByIndexS(DSS: TDSSContext; var ResultPtr: TDSSObjectPtr; ResultCount: PAPISize; clsname: PAnsiChar; Value: PInteger; ValueCount: Integer); CDECL;
procedure Batch_CreateByInt32PropertyS(DSS: TDSSContext; var ResultPtr: TDSSObjectPtr; ResultCount: PAPISize; clsname: PAnsiChar; propname: PAnsiChar; value: Integer); CDECL;

procedure Batch_GetFloat64S(var ResultPtr: PDouble; ResultCount: PAPISize; batch: TDSSObjectPtr; batchSize: Integer; Name: PChar); CDECL;
procedure Batch_GetInt32S(var ResultPtr: PInteger; ResultCount: PAPISize; batch: TDSSObjectPtr; batchSize: Integer; Name: PChar); CDECL;
procedure Batch_GetStringS(var ResultPtr: PPAnsiChar; ResultCount: PAPISize; batch: TDSSObjectPtr; batchSize: Integer; Name: PChar); CDECL;
procedure Batch_GetAsStringS(var ResultPtr: PPAnsiChar; ResultCount: PAPISize; batch: TDSSObjectPtr; batchSize: Integer; Name: PChar); CDECL;
procedure Batch_GetObjectS(var ResultPtr: PPointer; ResultCount: PAPISize; batch: TDSSObjectPtr; batchSize: Integer; Name: PChar); CDECL;

// procedure Batch_SetAsStringS(batch: TDSSObjectPtr; batchSize: Integer; Name: PChar; Value: PAnsiChar); CDECL;
procedure Batch_Float64S(batch: TDSSObjectPtr; batchSize: Integer; Name: PChar; Operation: Integer; Value: Double); CDECL;
procedure Batch_Int32S(batch: TDSSObjectPtr; batchSize: Integer; Name: PChar; Operation: Integer; Value: Integer); CDECL;
procedure Batch_SetStringS(batch: TDSSObjectPtr; batchSize: Integer; Name: PChar; Value: PAnsiChar); CDECL;
procedure Batch_SetObjectS(batch: TDSSObjectPtr; batchSize: Integer; Name: PChar; Value: TDSSObject); CDECL;

procedure Batch_SetFloat64ArrayS(batch: TDSSObjectPtr; batchSize: Integer; Name: PChar; Value: PDouble); CDECL;
procedure Batch_SetInt32ArrayS(batch: TDSSObjectPtr; batchSize: Integer; Name: PChar; Value: PInteger); CDECL;
procedure Batch_SetStringArrayS(batch: TDSSObjectPtr; batchSize: Integer; Name: PChar; Value: PPAnsiChar); CDECL;
procedure Batch_SetObjectArrayS(batch: TDSSObjectPtr; batchSize: Integer; Name: PChar; Value: TDSSObjectPtr); CDECL;

// JSON functions, internal
function Obj_Circuit_ToJSON_(ckt: TDSSCircuit; joptions: Integer): PAnsiChar;
procedure Obj_Circuit_FromJSON_(DSS: TDSSContext; jckt: TJSONObject; joptions: Integer);

implementation

uses
    CAPI_metadata,
    StrUtils,
    Utilities,
    RegExpr,
    DSSGlobals,
    SysUtils,
    CktElement,
    DSSClass,
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
    DateUtils;

const
    // TODO: enum?
    Batch_Set = 0;
    Batch_Multiply = 1;
    Batch_Increment = 2;

procedure DSS_Dispose_String(S: PAnsiChar); CDECL;
begin
    FreeMem(S);
end;

function Obj_New(DSS: TDSSContext; ClsIdx: Integer; Name: PAnsiChar; Activate: TAPIBoolean; BeginEdit: TAPIBoolean): Pointer; CDECL;
var
    Obj: TDSSObject;
    Cls: TDSSClass;
begin
    Result := NIL;
    if DSS = NIL then DSS := DSSPrime;
    Cls := DSS.DSSClassList.At(ClsIdx);
    if Cls = NIL then
        Exit;

    Obj := Cls.NewObject(Name, Activate);
    if BeginEdit then
        Cls.BeginEdit(Obj, False);

    if Cls.DSSClassType = DSS_OBJECT then
        DSS.DSSObjs.Add(Obj)
    else
        DSS.ActiveCircuit.AddCktElement(TDSSCktElement(Obj));

    Result := Obj;
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

procedure Obj_BeginEdit(Handle: Pointer); CDECL;
begin
    TDSSObject(Handle).ParentClass.BeginEdit(Handle, False);
end;

procedure Obj_EndEdit(Handle: Pointer; NumChanges: Integer); CDECL;
begin
    TDSSObject(Handle).ParentClass.EndEdit(Handle, NumChanges);
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

procedure Obj_Activate(Handle: Pointer; AllLists: TAPIBoolean); CDECL;
var
    obj: TDSSObject;
begin
    obj := TDSSObject(Handle);

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

function Obj_GetPropSeqPtr(Handle: Pointer): PInteger; CDECL;
var
    obj: TDSSObject;
begin
    obj := TDSSObject(Handle);
    Result := PInteger(obj.PrpSequence);
end;

function Obj_GetName(Handle: Pointer): PAnsiChar; CDECL;
begin
    Result := PChar(TDSSObject(Handle).Name);
end;

function Obj_GetNumProperties(Handle: Pointer): Integer; CDECL;
begin
    Result := TDSSObject(Handle).ParentClass.NumProperties;
end;

function Obj_GetListPointer(DSS: TDSSContext; ClsIdx: Integer): PPointer; CDECL;
var
    Cls: TDSSClass;
begin
    Result := NIL;
    if DSS = NIL then DSS := DSSPrime;
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
    Cls := DSS.DSSClassList.At(ClsIdx);
    if Cls = NIL then
        Exit;

    Result := Cls.ElementList.Count
end;

function Obj_GetIdx(Handle: Pointer): Integer; CDECL;
begin
    Result := TDSSObject(Handle).ClassIndex;
end;

function Obj_GetClassName(Handle: Pointer): PAnsiChar; CDECL;
begin
    Result := PChar(TDSSObject(Handle).ParentClass.Name);
end;

function Obj_GetClassIdx(Handle: Pointer): Integer; CDECL;
begin
    Result := TDSSObject(Handle).ParentClass.DSSClassIndex;
end;

function Obj_PropertySideEffects(Handle: Pointer; Index: Integer; PreviousInt: Integer): TAPIBoolean; CDECL;
begin
    Result := True;
    try
        TDSSObject(Handle).PropertySideEffects(Index, PreviousInt);
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

procedure Obj_SetFloat64(obj: TDSSObject; Index: Integer; Value: Double); CDECL;
begin
    obj.SetDouble(Index, Value);
end;

procedure Obj_SetInt32(obj: TDSSObject; Index: Integer; Value: Integer); CDECL;
begin
    obj.SetInteger(Index, Value);
end;

procedure Obj_SetString(obj: TDSSObject; Index: Integer; Value: PAnsiChar); CDECL;
begin
    obj.SetString(Index, Value);
end;

procedure Obj_SetAsString(obj: TDSSObject; Index: Integer; Value: PAnsiChar); CDECL;
begin
    obj.ParsePropertyValue(Index, Value);
end;

procedure Obj_SetObject(obj: TDSSObject; Index: Integer; Value: TDSSObject); CDECL;
begin
    obj.SetObject(Index, Value);
end;

procedure Obj_SetFloat64Array(obj: TDSSObject; Index: Integer; Value: PDouble; ValueCount: Integer); CDECL;
begin
    obj.SetDoubles(Index, Value, ValueCount);
end;

procedure Obj_SetInt32Array(obj: TDSSObject; Index: Integer; Value: PInteger; ValueCount: Integer); CDECL;
begin
    obj.SetIntegers(Index, Value, ValueCount);
end;

procedure Obj_SetStringArray(obj: TDSSObject; Index: Integer; Value: PPAnsiChar; ValueCount: Integer); CDECL;
begin
    obj.SetStrings(Index, Value, ValueCount);
end;

procedure Obj_SetObjectArray(obj: TDSSObject; Index: Integer; Value: TDSSObjectPtr; ValueCount: Integer); CDECL;
begin
    obj.SetObjects(Index, Value, ValueCount);
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
    iProp, iPropNext: Integer;
    jvalue: TJSONData = NIL;
    cls: TDSSClass;
    done: array of Boolean;
    resObj: TJSONObject;
    pnames: pStringArray;
begin
    Result := NIL;
    if obj = NIL then
        Exit;

    cls := obj.ParentClass;

    if (joptions and Integer(DSSJSONOptions.LowercaseKeys)) = 0 then
    begin
        pnames := cls.PropertyName;
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
        while iPropNext > 0 do
        begin
            iProp := iPropNext;
            iPropNext := obj.GetNextPropertySet(iProp);

            if done[iProp] then
                continue;

            done[iProp] := True;

            // skip substructure (winding, wire) index or suppressed props
            if (TPropertyFlag.SuppressJSON in cls.PropertyFlags[iProp]) or 
                (TPropertyFlag.AltIndex in cls.PropertyFlags[iProp]) or 
                (TPropertyFlag.IntegerStructIndex in cls.PropertyFlags[iProp]) then
                continue;

            // If redundant and array-related, prefer the original property.
            // We use the singular-named property (e.g. kV instead of kVs) since not
            // all plural forms are exposed by OpenDSS properties that implement
            // array-valued quantities.
            // These don't expose array versions (they're either DoubleOnStructArrayProperty or IntegerOnStructArrayProperty):
            // - Transformer: MaxTap, MinTap, RdcOhms, NumTaps, Rneut, Xneut
            // - AutoTrans: MaxTap, MinTap, RdcOhms, NumTaps
            // - XfmrCode: MaxTap, MinTap, RdcOhms, NumTaps, Rneut, Xneut
            //TODO: validate Line in JSON
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
                        TPropertyType.DoubleArrayProperty
                    ])
                ) then
            begin
                iProp := cls.PropertyRedundantWith[iProp];
                if done[iProp] then
                    continue;

                done[iProp] := True;
            end;

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
    finally
        FreeAndNil(json);
    end;
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
    if (batch = NIL) or (batch^ = NIL) then
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

procedure Batch_CreateFromNew(DSS: TDSSContext; var ResultPtr: TDSSObjectPtr; ResultCount: PAPISize; ClsIdx: Integer; Names: PPAnsiChar; Count: Integer; BeginEdit: TAPIBoolean); CDECL;
var
    // Obj: TDSSObject;
    Cls: TDSSClass;
    outptr: TDSSObjectPtr;
    i: Integer;
    prefix: String;
begin
    if DSS = NIL then DSS := DSSPrime;
    Cls := DSS.DSSClassList.At(ClsIdx);
    if Cls = NIL then
        Exit;
    ensureBatchSize(Count, ResultPtr, ResultCount);    
    outptr := ResultPtr;
    
    if Names = NIL then
    begin
        // Use a random batch prefix to avoid collisions
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
            outptr^ := Cls.NewObject(Names^, False);
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
        rex := TRegExpr.Create();
        rex.ModifierI := True;
        rex.Expression:= re;
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
    finally
        FreeAndNil(rex);
    end;
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
    objlist: TDSSObjectPtr;
    outptr: TDSSObjectPtr;
    propOffset: PtrUint;
    i: Integer;
    propFlags: TPropertyFlags;
    ptype: TPropertyType;
begin
    if DSS = NIL then DSS := DSSPrime;
    cls := DSS.DSSClassList.At(clsIdx);
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
    objlist := TDSSObjectPtr(cls.ElementList.InternalPointer);
    ensureBatchSize(cls.ElementList.Count, ResultPtr, ResultCount);
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
        for i := 1 to cls.ElementList.Count do
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

    for i := 1 to cls.ElementList.Count do
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

function Batch_ToJSON(batch: TDSSObjectPtr; batchSize: Integer; joptions: Integer): PAnsiChar; CDECL;
var
    json: TJSONArray = NIL;
    i: Integer;
begin
    Result := NIL;
    try
        json := TJSONArray.Create([]);
        if ((joptions and Integer(DSSJSONOptions.ExcludeDisabled)) = 0) or not (batch^ is TDSSCktElement) then
        begin
            for i := 1 to batchSize do
            begin
                json.Add(Obj_ToJSONData(batch^, joptions));
                inc(batch);
            end;
        end
        else
        begin
            for i := 1 to batchSize do
            begin
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
    finally
        FreeAndNil(json);
    end;
end;

procedure Batch_GetFloat64(var ResultPtr: PDouble; ResultCount: PAPISize; batch: TDSSObjectPtr; batchSize: Integer; Index: Integer); CDECL;
var
    cls: TDSSClass;
    // propOffset: PtrUint;
    presult: PDouble;
    i: Integer;
begin
    if (batch = NIL) or (batch^ = NIL) then
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
    if (batch = NIL) or (batch^ = NIL) or ((@func) = NIL) then
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

procedure Batch_GetInt32(var ResultPtr: PInteger; ResultCount: PAPISize; batch: TDSSObjectPtr; batchSize: Integer; Index: Integer); CDECL;
var
    cls: TDSSClass;
    propOffset: PtrUint;
    presult: PInteger;
    i: Integer;
    propFlags: TPropertyFlags;
begin
    if (batch = NIL) or (batch^ = NIL) then
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
    if (batch = NIL) or (batch^ = NIL) or ((@func) = NIL) then
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
    if (batch = NIL) or (batch^ = NIL) then
        Exit;

    cls := batch^.ParentClass;

    // propOffset := cls.PropertyOffset[propIdx];
    DSS_RecreateArray_PPAnsiChar(ResultPtr, ResultCount, batchSize);
    presult := ResultPtr;
    
    if not (cls.PropertyType[Index] in [
        TPropertyType.StringSilentROFunctionProperty,
        TPropertyType.StringProperty,
        TPropertyType.BusProperty,
        // TPropertyType.StringOnArrayProperty,
        // TPropertyType.StringOnStructArrayProperty,
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
    if (batch = NIL) or (batch^ = NIL) then
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
    if (batch = NIL) or (batch^ = NIL) then
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

procedure Batch_Float64(batch: TDSSObjectPtr; batchSize: Integer; Index: Integer; Operation: Integer; Value: Double); CDECL;
var
    cls: TDSSClass;
    propOffset: PtrUint;
    i: Integer;
    prev: Double;
    doublePtr: PDouble;
    propFlags: TPropertyFlags;
    singleEdit: Boolean;
begin
    if (batch = NIL) or (batch^ = NIL) then
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
            Batch_Multiply:
                for i := 1 to batchSize do
                begin
                    singleEdit := not (Flg.EditingActive in batch^.Flags);
                    if singleEdit then
                        cls.BeginEdit(batch^, False);

                    doublePtr := (PDouble(PtrUint(batch^) + propOffset));
                    prev := doubleptr^;
                    doublePtr^ := doublePtr^ * Value;
                    batch^.SetAsNextSeq(Index);
                    batch^.PropertySideEffects(Index, Round(prev));

                    if singleEdit then
                        cls.EndEdit(batch^, 1);
                    inc(batch);
                end;
            Batch_Increment:
                for i := 1 to batchSize do
                begin
                    singleEdit := not (Flg.EditingActive in batch^.Flags);
                    if singleEdit then
                        cls.BeginEdit(batch^, False);

                    doublePtr := (PDouble(PtrUint(batch^) + propOffset));
                    prev := doubleptr^;
                    doublePtr^ := doublePtr^ + Value;
                    batch^.SetAsNextSeq(Index);
                    batch^.PropertySideEffects(Index, Round(prev));

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
                batch^.PropertySideEffects(Index, Round(prev));

                if singleEdit then
                    cls.EndEdit(batch^, 1);
                inc(batch);
            end;
        end;

        Exit;
    end;

    case Operation of
        Batch_Multiply:
            for i := 1 to batchSize do
            begin
                batch^.SetDouble(Index, Value * cls.GetObjDouble(batch^, Index));
                inc(batch);
            end;
        Batch_Increment:
            for i := 1 to batchSize do
            begin
                batch^.SetDouble(Index, Value + cls.GetObjDouble(batch^, Index));
                inc(batch);
            end;
    else
        for i := 1 to batchSize do
        begin
            batch^.SetDouble(Index, Value);
            inc(batch);
        end;
    end;
end;

procedure Batch_Int32(batch: TDSSObjectPtr; batchSize: Integer; Index: Integer; Operation: Integer; Value: Integer); CDECL;
var
    cls: TDSSClass;
    // propOffset: PtrUint;
    i: Integer;
    // prev: Integer;
    // intptr: PInteger;
    // propFlags: TPropertyFlags;
    ptype: TPropertyType;
begin
    if (batch = NIL) or (batch^ = NIL) then
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

    if (ptype in [TPropertyType.BooleanProperty, TPropertyType.EnabledProperty, TPropertyType.BooleanActionProperty]) and not (Operation in [Batch_Increment]) then
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
        Batch_Multiply:
            for i := 1 to batchSize do
            begin
                batch^.SetInteger(Index, Value * cls.GetObjInteger(batch^, Index));
                inc(batch);
            end;
        Batch_Increment:
            for i := 1 to batchSize do
            begin
                batch^.SetInteger(Index, Value + cls.GetObjInteger(batch^, Index));
                inc(batch);
            end;
    else
        for i := 1 to batchSize do
        begin
            batch^.SetInteger(Index, Value);
            inc(batch);
        end;
    end;
end;

procedure Batch_SetString(batch: TDSSObjectPtr; batchSize: Integer; Index: Integer; Value: PAnsiChar); CDECL;
var
    cls: TDSSClass;
    // propOffset: PtrUint;
    i: Integer;
    // propFlags: TPropertyFlags;
    sValue: String;
begin
    if (batch = NIL) or (batch^ = NIL) then
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
        // TPropertyType.StringOnArrayProperty,
        // TPropertyType.StringOnStructArrayProperty,
        TPropertyType.BusOnStructArrayProperty,
        TPropertyType.DSSObjectReferenceProperty
    ]) then
        Exit;

    sValue := Value;
    for i := 1 to batchSize do
    begin
        batch^.SetString(Index, sValue);
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
    // if (batch = NIL) or (batch^ = NIL) then
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

procedure Batch_SetObject(batch: TDSSObjectPtr; batchSize: Integer; Index: Integer; Value: TDSSObject); CDECL;
var
    cls: TDSSClass;
    // propOffset: PtrUint;
    i: Integer;
    // propFlags: TPropertyFlags;
begin
    if (batch = NIL) or (batch^ = NIL) then
        Exit;

    cls := batch^.ParentClass;
    // propFlags := cls.PropertyFlags[Index];
    // propOffset := cls.PropertyOffset[Index];

    if cls.PropertyType[Index] <> TPropertyType.DSSObjectReferenceProperty then
        Exit;

    for i := 1 to batchSize do
    begin
        batch^.SetObject(Index, Value);
        inc(batch);
    end;
end;

procedure Batch_SetFloat64Array(batch: TDSSObjectPtr; batchSize: Integer; Index: Integer; Value: PDouble); CDECL;
var
    cls: TDSSClass;
    propOffset: PtrUint;
    i: Integer;
    prev: Double;
    doublePtr: PDouble;
    propFlags: TPropertyFlags;
    singleEdit: Boolean;
begin
    if (batch = NIL) or (batch^ = NIL) then
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
        for i := 1 to batchSize do
        begin
            singleEdit := not (Flg.EditingActive in batch^.Flags);
            if singleEdit then
                cls.BeginEdit(batch^, False);

            doublePtr := PDouble(PtrUint(batch^) + propOffset);
            prev := doubleptr^;
            doublePtr^ := Value^;
            batch^.PropertySideEffects(Index, Round(prev));

            if singleEdit then
                cls.EndEdit(batch^, 1);
            inc(batch);
            inc(Value);
        end;
        Exit;
    end;

    for i := 1 to batchSize do
    begin
        batch^.SetDouble(Index, Value^);
        inc(batch);
        inc(Value)
    end;
end;

procedure Batch_SetInt32Array(batch: TDSSObjectPtr; batchSize: Integer; Index: Integer; Value: PInteger); CDECL;
var
    cls: TDSSClass;
    propOffset: PtrUint;
    i: Integer;
    prev: Integer;
    intPtr: PInteger;
    propFlags: TPropertyFlags;
    singleEdit: Boolean;
begin
    if (batch = NIL) or (batch^ = NIL) then
        Exit;

    cls := batch^.ParentClass;
    propFlags := cls.PropertyFlags[Index];
    propOffset := cls.PropertyOffset[Index];

    if not (cls.PropertyType[Index] in [
        TPropertyType.IntegerProperty,
        TPropertyType.MappedIntEnumProperty,
        TPropertyType.MappedStringEnumProperty,
        TPropertyType.BooleanProperty,
        TPropertyType.IntegerOnStructArrayProperty
    ]) then
        Exit;

    if (cls.PropertyType[Index] <> TPropertyType.IntegerOnStructArrayProperty) and 
        (propFlags = []) and
        (cls.PropertyScale[Index] = 1) then
    begin
        for i := 1 to batchSize do
        begin
            singleEdit := not (Flg.EditingActive in batch^.Flags);
            if singleEdit then
                cls.BeginEdit(batch^, False);

            intPtr := PInteger(PtrUint(batch^) + propOffset);
            prev := intPtr^;
            intPtr^ := Value^;
            batch^.PropertySideEffects(Index, prev);

            if singleEdit then
                cls.EndEdit(batch^, 1);
            inc(batch);
            inc(Value);
        end;
        Exit;
    end;

    for i := 1 to batchSize do
    begin
        batch^.SetInteger(Index, Value^);
        inc(batch);
        inc(Value)
    end;
end;

procedure Batch_SetStringArray(batch: TDSSObjectPtr; batchSize: Integer; Index: Integer; Value: PPAnsiChar); CDECL;
var
    cls: TDSSClass;
    // propOffset: PtrUint;
    i: Integer;
    // propFlags: TPropertyFlags;
begin
    if (batch = NIL) or (batch^ = NIL) then
        Exit;

    cls := batch^.ParentClass;
    // propFlags := cls.PropertyFlags[Index];
    // propOffset := cls.PropertyOffset[Index];
    
    if not (cls.PropertyType[Index] in [
        // TPropertyType.StringEnumActionProperty, --TODO? Not in SetObjString
        // TPropertyType.MappedStringEnumOnStructArrayProperty, --TODO? Not in SetObjString
        TPropertyType.StringProperty,
        TPropertyType.BusProperty,
        TPropertyType.MappedStringEnumProperty,
        // TPropertyType.StringOnArrayProperty,
        // TPropertyType.StringOnStructArrayProperty,
        TPropertyType.BusOnStructArrayProperty,
        TPropertyType.DSSObjectReferenceProperty
    ]) then
        Exit;

    for i := 1 to batchSize do
    begin
        batch^.SetString(Index, Value^);
        inc(batch);
        inc(Value)
    end;
end;

procedure Batch_SetObjectArray(batch: TDSSObjectPtr; batchSize: Integer; Index: Integer; Value: TDSSObjectPtr); CDECL;
var
    cls: TDSSClass;
    // propOffset: PtrUint;
    i: Integer;
    // propFlags: TPropertyFlags;
begin
    if (batch = NIL) or (batch^ = NIL) then
        Exit;

    if Value = NIL then
    begin
        Batch_SetObject(batch, batchSize, Index, NIL);
        Exit;
    end;

    cls := batch^.ParentClass;
    // propFlags := cls.PropertyFlags[Index];
    // propOffset := cls.PropertyOffset[Index];

    if cls.PropertyType[Index] <> TPropertyType.DSSObjectReferenceProperty then
        Exit;

    for i := 1 to batchSize do
    begin
        batch^.SetObject(Index, Value^);
        inc(batch);
        inc(Value)
    end;
end;

procedure Batch_CreateFromNewS(DSS: TDSSContext; var ResultPtr: TDSSObjectPtr; ResultCount: PAPISize; clsName: String; Names: PPAnsiChar; Count: Integer; BeginEdit: TAPIBoolean); CDECL;
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

procedure Batch_Float64S(batch: TDSSObjectPtr; batchSize: Integer; Name: PChar; Operation: Integer; Value: Double); CDECL;
var
    propIdx: Integer;
begin
    if (batch = NIL) or (batch^ = NIL) or (batchSize <= 0) then
        Exit;

    if not GetPropIndex(batch, Name, propIdx) then
        Exit;

    Batch_Float64(batch, batchSize, propIdx, Operation, Value);
end;

procedure Batch_Int32S(batch: TDSSObjectPtr; batchSize: Integer; Name: PChar; Operation: Integer; Value: Integer); CDECL;
var
    propIdx: Integer;
begin
    if (batch = NIL) or (batch^ = NIL) or (batchSize <= 0) then
        Exit;

    if not GetPropIndex(batch, Name, propIdx) then
        Exit;

    Batch_Int32(batch, batchSize, propIdx, Operation, Value);
end;

procedure Batch_SetStringS(batch: TDSSObjectPtr; batchSize: Integer; Name: PChar; Value: PAnsiChar); CDECL;
var
    propIdx: Integer;
begin
    if (batch = NIL) or (batch^ = NIL) or (batchSize <= 0) then
        Exit;

    if not GetPropIndex(batch, Name, propIdx) then
        Exit;

    Batch_SetString(batch, batchSize, propIdx, Value);
end;

procedure Batch_SetObjectS(batch: TDSSObjectPtr; batchSize: Integer; Name: PChar; Value: TDSSObject); CDECL;
var
    propIdx: Integer;
begin
    if (batch = NIL) or (batch^ = NIL) or (batchSize <= 0) then
        Exit;

    if not GetPropIndex(batch, Name, propIdx) then
        Exit;

    Batch_SetObject(batch, batchSize, propIdx, Value);
end;


procedure Batch_SetFloat64ArrayS(batch: TDSSObjectPtr; batchSize: Integer; Name: PChar; Value: PDouble); CDECL;
var
    propIdx: Integer;
begin
    if (batch = NIL) or (batch^ = NIL) or (batchSize <= 0) then
        Exit;

    if not GetPropIndex(batch, Name, propIdx) then
        Exit;

    Batch_SetFloat64Array(batch, batchSize, propIdx, Value);
end;

procedure Batch_SetInt32ArrayS(batch: TDSSObjectPtr; batchSize: Integer; Name: PChar; Value: PInteger); CDECL;
var
    propIdx: Integer;
begin
    if (batch = NIL) or (batch^ = NIL) or (batchSize <= 0) then
        Exit;

    if not GetPropIndex(batch, Name, propIdx) then
        Exit;

    Batch_SetInt32Array(batch, batchSize, propIdx, Value);
end;

procedure Batch_SetStringArrayS(batch: TDSSObjectPtr; batchSize: Integer; Name: PChar; Value: PPAnsiChar); CDECL;
var
    propIdx: Integer;
begin
    if (batch = NIL) or (batch^ = NIL) or (batchSize <= 0) then
        Exit;

    if not GetPropIndex(batch, Name, propIdx) then
        Exit;

    Batch_SetStringArray(batch, batchSize, propIdx, Value);
end;

procedure Batch_SetObjectArrayS(batch: TDSSObjectPtr; batchSize: Integer; Name: PChar; Value: TDSSObjectPtr); CDECL;
var
    propIdx: Integer;
begin
    if (batch = NIL) or (batch^ = NIL) or (batchSize <= 0) then
        Exit;

    if not GetPropIndex(batch, Name, propIdx) then
        Exit;

    Batch_SetObjectArray(batch, batchSize, propIdx, Value);
end;

//------------------------------------------------------------------------------
function Obj_Circuit_ToJSON_(ckt: TDSSCircuit; joptions: Integer): PAnsiChar;
var
    circ: TJSONObject = NIL;
    busArray: TJSONArray = NIL;
    busObj: TJSONObject = NIL;
    clsArray: TJSONArray = NIL;
    cmds: TJSONArray = NIL;
    // vsrc: TVSourceObj = NIL;
    cls: TDSSClass;
    obj: TDSSObject;
    first: Boolean;
    // vsrccls: TDSSClass;
    bus: TDSSBus;
    i: Integer;
begin
    Result := NIL;
    try
        cmds := TJSONArray.Create();
        cmds.Add(Format('! Last saved by AltDSS/%s on %s',  [VersionString, DateToISO8601(Now())]));
        if ckt.PositiveSequence then
            cmds.Add(Format('Set CktModel=%s', [ckt.DSS.CktModelEnum.OrdinalToString(Integer(ckt.PositiveSequence))]));
        if ckt.DuplicatesAllowed then
            cmds.Add('Set AllowDuplicates=True');
        if ckt.LongLineCorrection then
            cmds.Add('Set LongLineCorrection=True');

        busArray := TJSONArray.Create();
        for i := 1 to ckt.NumBuses do
        begin
            bus := ckt.Buses[i];
            busObj := TJSONObject.Create(['Name', bus.Name]);
            if bus.CoordDefined then
            begin
                busObj.Add('X', bus.x);
                busObj.Add('Y', bus.y);
            end;
            if bus.kVBase <> 0 then
                busObj.Add('', bus.kVBase);
            if bus.Keep then
                busObj.Add('Keep', true);
            busArray.Add(busObj);
            busObj := NIL;
        end;
        circ := TJSONObject.Create([
            'Name', ckt.Name,
            'DefaultBaseFreq', ckt.DSS.DefaultBaseFreq,
            'PreCommands', cmds, 
            // MakeBusList as a PostCommand is implicit
            'Bus', busArray
        ]);
        cmds := NIL;
        busArray := NIL;

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
            for obj in cls do
            begin
                clsArray.Add(Obj_ToJSONData(obj, joptions));
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
        DoSimpleMsg(ckt.DSS, 'Error converting data to JSON: %s', [E.message], 20230918);
    end;

    if cmds <> NIL then
        cmds.Free();
    if circ <> NIL then
        circ.Free();
    if busArray <> NIL then
        busArray.Free();
    if busObj <> NIL then
        busObj.Free();
    if clsArray <> NIL then
        clsArray.Free();
    // if vsrc <> NIL then
    //     vsrc.Free();
end;

function processArrayOrFilePath(DSS: TDSSContext; data: TJSONData): ArrayOfDouble;
var
    arr: TJSONArray = NIL;
    obj: TJSONObject = NIL;
    i: Integer;
    col: Integer = 1;
    header: Boolean = false;
    fn: TJSONString = NIL;
begin
    if data is TJSONArray then
    begin
        // Easy path
        arr := TJSONArray(data);
        SetLength(Result, arr.Count);
        for i := 0 to High(Result) do
            Result[i] := arr.Floats[i];
        Exit;
    end;
    obj := obj as TJSONOBject;
    if obj = NIL then
        raise Exception.Create('Array is not correctly specified');

    obj.Find('DblFile', fn);
    if fn <> NIL then
    
    obj.Find('SngFile', fn);
    if fn <> NIL then
    
    
    obj.Find('CSVFile', fn);
    if fn = NIL then
        raise Exception.Create('Array is not correctly specified');

    col := obj.Get('Column', col);
    header := obj.Get('Header', header);

end;

procedure loadClassFromJSON(DSS: TDSSContext; cls: TDSSClass; jcls: TJSONData; joptions: Integer);
// 1. Load from array, or
// 2. Load array from file, then 1, or
// 3. Incrementally load from JSONLines
var
    obj: TJSONObject;
    data: TJSONData = NIL;
    arr: TJSONArray;
    arrItem: TJSONData;
    jsonFilePath: TJSONString = NIL;
    jsonLinesFilePath: TJSONString = NIL;
    F: TStream = NIL;
    lineNum: Integer;
    specialFirst: Boolean;
    line: String;

    procedure loadSingleObj(o: TJSONObject);
    begin
        if specialFirst then
        begin
            WriteLn('"SPECIAL loading first" ', cls.Name, '.', o.Get('name', 'MISSING_NAME'));
            specialFirst := false;
        end;

        WriteLn('"Loading" ', cls.Name, '.', o.Get('name', 'MISSING_NAME'));
    end;
begin
    specialFirst := (cls = DSS.VSourceClass);
    try
        if jcls is TJSONObject then
        begin
            obj := TJSONObject(jcls);
            obj.Find('JSONFile', jsonFilePath);
            obj.Find('JSONLinesFile', jsonLinesFilePath);
            if jsonFilePath <> NIL then
            begin
                F := DSS.GetInputStreamEx(jsonFilePath.Value);
                data := GetJSON(F);
                if not (data is TJSONArray) then
                    raise Exception.Create(Format('JSON/%s: unexpected format in file "%s".', [cls.Name, jsonFilePath.Value]));
                arr := TJSONArray(data);
                // continue as if the array as built-in
            end
            else
            if jsonLinesFilePath <> NIL then
            begin
                F := DSS.GetInputStreamEx(jsonLinesFilePath.Value);
                lineNum := 1;
                while (F.Position + 1) < F.Size do
                begin
                    FSReadln(F, line);
                    data := GetJSON(line);
                    if not (data is TJSONObject) then
                        raise Exception.Create(Format('JSON/%s: unexpected format in file "%s", line %d.', [cls.Name, jsonLinesFilePath.Value, lineNum]));

                    loadSingleObj(TJSONObject(data));
                    FreeAndNil(data);
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

        for lineNum := 0 to arr.Count - 1 do
        begin
            arrItem := arr.Items[lineNum];
            if not (arrItem is TJSONObject) then
                raise Exception.Create(Format('JSON/%s: unexpected format for object number %d.', [cls.Name, lineNum]));
            loadSingleObj(TJSONObject(arrItem));
        end;

    finally
        FreeAndNil(F);
        FreeAndNil(data);
    end;
end;

procedure Obj_Circuit_FromJSON_(DSS: TDSSContext; jckt: TJSONObject; joptions: Integer);
var
    ckt: TDSSCircuit;
    tmp: TJSONData;
    cls: TDSSClass;
    cmds: TJSONArray;
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

        cmds := tmp as TJSONArray;
        for i := 0 to cmds.Count - 1 do
        begin
            DSSPrime.DSSExecutive.ParseCommand(cmds.Items[i].AsString);
        end;
    end;

    for cls in ckt.DSS.DSSClassList do
    begin
        tmp := jckt.Find(cls.Name);
        if tmp = NIL then
            continue;

        loadClassFromJSON(DSS, cls, tmp,  joptions);
    end;

end;
//------------------------------------------------------------------------------

end.
