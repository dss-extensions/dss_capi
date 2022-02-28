unit CAPI_Obj;

// Copyright (c) 2020-2022, DSS C-API contributors
// Copyright (c) 2020-2022, Paulo Meira
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
    DSSObject;

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
function Obj_GetIdx(Handle: Pointer): Integer; CDECL;
function Obj_GetClassName(Handle: Pointer): PAnsiChar; CDECL;
function Obj_GetClassIdx(Handle: Pointer): Integer; CDECL;
function Obj_PropertySideEffects(Handle: Pointer; Index: Integer; PreviousInt: Integer): TAPIBoolean; CDECL;
procedure Obj_BeginEdit(Handle: Pointer); CDECL;
procedure Obj_EndEdit(Handle: Pointer; NumChanges: Integer); CDECL;
procedure Obj_Activate(Handle: Pointer; AllLists: TAPIBoolean); CDECL;

function Obj_GetFloat64(obj: TDSSObject; Index: Integer): Double; CDECL;
function Obj_GetInt32(obj: TDSSObject; Index: Integer): Integer; CDECL;
function Obj_GetString(obj: TDSSObject; Index: Integer): PAnsiChar; CDECL;
function Obj_GetObject(obj: TDSSObject; Index: Integer): Pointer; CDECL;
function Obj_GetAsString(obj: TDSSObject; Index: Integer): PAnsiChar; CDECL;
procedure Obj_GetFloat64Array(var ResultPtr: PDouble; ResultCount: PAPISize; obj: TDSSObject; Index: Integer); CDECL;
procedure Obj_GetInt32Array(var ResultPtr: PInteger; ResultCount: PAPISize; obj: TDSSObject; Index: Integer); CDECL;
procedure Obj_GetStringArray(var ResultPtr: PPAnsiChar; ResultCount: PAPISize; obj: TDSSObject; Index: Integer); CDECL;
procedure Obj_GetObjectArray(var ResultPtr: PPointer; ResultCount: PAPISize; obj: TDSSObject; Index: Integer); CDECL;

procedure Obj_SetAsString(obj: TDSSObject; Index: Integer; Value: PAnsiChar); CDECL;
procedure Obj_SetFloat64(obj: TDSSObject; Index: Integer; Value: Double); CDECL;
procedure Obj_SetInt32(obj: TDSSObject; Index: Integer; Value: Integer); CDECL;
procedure Obj_SetString(obj: TDSSObject; Index: Integer; Value: PAnsiChar); CDECL;
procedure Obj_SetObject(obj: TDSSObject; Index: Integer; Value: TDSSObject); CDECL;

procedure Obj_SetFloat64Array(obj: TDSSObject; Index: Integer; Value: PDouble; ValueCount: Integer); CDECL;
procedure Obj_SetInt32Array(obj: TDSSObject; Index: Integer; Value: PInteger; ValueCount: Integer); CDECL;
procedure Obj_SetStringArray(obj: TDSSObject; Index: Integer; Value: PPAnsiChar; ValueCount: Integer); CDECL;
procedure Obj_SetObjectArray(obj: TDSSObject; Index: Integer; Value: TDSSObjectPtr; ValueCount: Integer); CDECL;


// Batch: creation and state setup
procedure Batch_CreateFromNew(DSS: TDSSContext; var ResultPtr: TDSSObjectPtr; ResultCount: PAPISize; ClsIdx: Integer; Names: PPAnsiChar; Count: Integer; BeginEdit: TAPIBoolean); CDECL;
procedure Batch_Dispose(batch: Pointer); CDECL;
procedure Batch_BeginEdit(batch: TDSSObjectPtr; batchSize: Integer);
procedure Batch_EndEdit(batch: TDSSObjectPtr; batchSize: Integer; NumEdits: Integer);

// Batch -- using class and property indices
procedure Batch_CreateByClass(DSS: TDSSContext; var ResultPtr: TDSSObjectPtr; ResultCount: PAPISize; clsIdx: Integer); CDECL;
procedure Batch_CreateByRegExp(DSS: TDSSContext; var ResultPtr: TDSSObjectPtr; ResultCount: PAPISize; clsIdx: Integer; re: PAnsiChar); CDECL;
procedure Batch_CreateByIndex(DSS: TDSSContext; var ResultPtr: TDSSObjectPtr; ResultCount: PAPISize; ClsIdx: Integer; Value: PInteger; ValueCount: Integer); CDECL;
procedure Batch_CreateByInt32Property(DSS: TDSSContext; var ResultPtr: TDSSObjectPtr; ResultCount: PAPISize; ClsIdx: Integer; propidx: Integer; value: Integer); CDECL;

procedure Batch_GetFloat64(var ResultPtr: PDouble; ResultCount: PAPISize; batch: TDSSObjectPtr; batchSize: Integer; Index: Integer); CDECL;
procedure Batch_GetInt32(var ResultPtr: PInteger; ResultCount: PAPISize; batch: TDSSObjectPtr; batchSize: Integer; Index: Integer); CDECL;
procedure Batch_GetString(var ResultPtr: PPAnsiChar; ResultCount: PAPISize; batch: TDSSObjectPtr; batchSize: Integer; Index: Integer); CDECL;
procedure Batch_GetAsString(var ResultPtr: PPAnsiChar; ResultCount: PAPISize; batch: TDSSObjectPtr; batchSize: Integer; Index: Integer); CDECL;
procedure Batch_GetObject(var ResultPtr: PPointer; ResultCount: PAPISize; batch: TDSSObjectPtr; batchSize: Integer; Index: Integer); CDECL;

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



implementation

uses
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
    DSSObjectHelper;

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
    Cls := DSS.DSSClassList.At(ClsIdx);
    if Cls = NIL then
        Exit;

    Obj := Cls.NewObject(Name, Activate);
    if BeginEdit then
        Cls.BeginEdit(Obj, False);

    Result := Obj;
end;

function Obj_GetHandleByName(DSS: TDSSContext; ClsIdx: Integer; Name: PAnsiChar): Pointer; CDECL;
var
    Cls: TDSSClass;
begin
    Result := NIL;
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
    p := List.First;
    while p <> NIL do
    begin
        if List.Active = Obj then
            Exit;

        p := List.Next;
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

function Obj_GetName(Handle: Pointer): PAnsiChar; CDECL;
begin
    Result := DSS_CopyStringAsPChar(TDSSObject(Handle).Name);
end;

function Obj_GetIdx(Handle: Pointer): Integer; CDECL;
begin
    Result := TDSSObject(Handle).ClassIndex;
end;

function Obj_GetClassName(Handle: Pointer): PAnsiChar; CDECL;
begin
    Result := DSS_CopyStringAsPChar(TDSSObject(Handle).ParentClass.Name);
end;

function Obj_GetClassIdx(Handle: Pointer): Integer; CDECL;
begin
    Result := TDSSObject(Handle).ParentClass.DSSClassType;
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


//------------------------------------------------------------------------------

procedure Batch_Dispose(batch: Pointer); CDECL;
begin
    FreeMem(batch);
end;

procedure Batch_BeginEdit(batch: TDSSObjectPtr; batchSize: Integer);
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

procedure Batch_EndEdit(batch: TDSSObjectPtr; batchSize: Integer; NumEdits: Integer);
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

procedure Batch_CreateFromNew(DSS: TDSSContext; var ResultPtr: TDSSObjectPtr; ResultCount: PAPISize; ClsIdx: Integer; Names: PPAnsiChar; Count: Integer; BeginEdit: TAPIBoolean); CDECL;
var
    Obj: TDSSObject;
    Cls: TDSSClass;
    outptr: TDSSObjectPtr;
    i: Integer;
    prefix: String;
begin
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
            inc(outptr);
        end;
    end
    else
    begin
        for i := 1 to Count do
        begin
            outptr^ := Cls.NewObject(Names^, False);
            inc(outptr);
            inc(Names);
        end;
    end;

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
    flags: TPropertyFlags;
begin
    cls := DSS.DSSClassList.At(clsIdx);
    if cls = NIL then
    begin
        Exit;
    end;
    
    if not (cls.PropertyType[propidx] in [
        TPropertyType.IntegerProperty,
        TPropertyType.MappedIntEnumProperty,
        TPropertyType.MappedStringEnumProperty,
        TPropertyType.BooleanProperty,
        TPropertyType.IntegerOnStructArrayProperty
    ]) then
    begin
        Exit;
    end;

    flags := cls.PropertyFlags[propIdx];
    propOffset := cls.PropertyOffset[propIdx];
    objlist := TDSSObjectPtr(cls.ElementList.InternalPointer);
    ensureBatchSize(cls.ElementList.Count, ResultPtr, ResultCount);
    outptr := ResultPtr;
    if (cls.PropertyType[propidx] in [
        TPropertyType.IntegerProperty,
        TPropertyType.MappedIntEnumProperty,
        TPropertyType.MappedStringEnumProperty,
        TPropertyType.BooleanProperty]) and 
        (not (TPropertyFlag.CustomGet in flags)) and
        (not (TPropertyFlag.ReadByFunction in flags)) and
        (not (TPropertyFlag.ScaledByFunction in flags)) then
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

procedure Batch_GetInt32(var ResultPtr: PInteger; ResultCount: PAPISize; batch: TDSSObjectPtr; batchSize: Integer; Index: Integer); CDECL;
var
    cls: TDSSClass;
    propOffset: PtrUint;
    presult: PInteger;
    i: Integer;
    flags: TPropertyFlags;
begin
    if (batch = NIL) or (batch^ = NIL) then
    begin
        Exit;
    end;

    cls := batch^.ParentClass;
    flags := cls.PropertyFlags[Index];
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
        (not (TPropertyFlag.CustomGet in flags)) and
        (not (TPropertyFlag.ReadByFunction in flags)) and
        (not (TPropertyFlag.ScaledByFunction in flags)) then
    begin
        for i := 1 to batchSize do
        begin
            presult^ := (PInteger(PtrUint(batch^) + propoffset))^;
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
        TPropertyType.StringOnArrayProperty,
        TPropertyType.StringOnStructArrayProperty,
        TPropertyType.BusOnStructArrayProperty
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
    flags: TPropertyFlags;
begin
    if (batch = NIL) or (batch^ = NIL) then
        Exit;

    cls := batch^.ParentClass;
    flags := cls.PropertyFlags[Index];
    propOffset := cls.PropertyOffset[Index];

    if not (cls.PropertyType[Index] in [
        TPropertyType.DoubleProperty,
        TPropertyType.DoubleOnStructArrayProperty,
        TPropertyType.DoubleOnArrayProperty
    ]) then
        Exit;

    if (cls.PropertyType[Index] = TPropertyType.DoubleProperty) and 
        (flags = []) and
        (not cls.PropertyInverse[Index]) and
        (cls.PropertyScale[Index] = 1) then
    begin
        case Operation of
            Batch_Multiply:
                for i := 1 to batchSize do
                begin
                    doublePtr := (PDouble(PtrUint(batch^) + propOffset));
                    prev := doubleptr^;
                    doublePtr^ := doublePtr^ * Value;
                    batch^.PropertySideEffects(Index, Round(prev));
                    inc(batch);
                end;
            Batch_Increment:
                for i := 1 to batchSize do
                begin
                    doublePtr := (PDouble(PtrUint(batch^) + propOffset));
                    prev := doubleptr^;
                    doublePtr^ := doublePtr^ + Value;
                    batch^.PropertySideEffects(Index, Round(prev));
                    inc(batch);
                end;
        else
            for i := 1 to batchSize do
            begin
                doublePtr := (PDouble(PtrUint(batch^) + propOffset));
                prev := doubleptr^;
                doublePtr^ := Value;
                batch^.PropertySideEffects(Index, Round(prev));
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
    propOffset: PtrUint;
    i: Integer;
    // prev: Integer;
    // intptr: PInteger;
    flags: TPropertyFlags;
begin
    if (batch = NIL) or (batch^ = NIL) then
        Exit;

    cls := batch^.ParentClass;
    flags := cls.PropertyFlags[Index];
    propOffset := cls.PropertyOffset[Index];

    if not (cls.PropertyType[Index] in [
        TPropertyType.IntegerProperty,
        TPropertyType.MappedIntEnumProperty,
        TPropertyType.MappedStringEnumProperty,
        TPropertyType.BooleanProperty,
        TPropertyType.EnabledProperty,
        TPropertyType.IntegerOnStructArrayProperty
    ]) then
        Exit;

    // if (cls.PropertyType[Index] in [
    //     TPropertyType.IntegerProperty,
    //     TPropertyType.MappedIntEnumProperty,
    //     TPropertyType.MappedStringEnumProperty,
    //     TPropertyType.BooleanProperty]) and 
    //     (not (TPropertyFlag.CustomSet in flags)) and
    //     (not (TPropertyFlag.WriteByFunction in flags)) and
    //     (not (TPropertyFlag.ScaledByFunction in flags)) then
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
    propOffset: PtrUint;
    i: Integer;
    flags: TPropertyFlags;
    sValue: String;
begin
    if (batch = NIL) or (batch^ = NIL) then
        Exit;

    cls := batch^.ParentClass;
    flags := cls.PropertyFlags[Index];
    propOffset := cls.PropertyOffset[Index];
    
    if not (cls.PropertyType[Index] in [
        // TPropertyType.StringEnumActionProperty, --TODO? Not in SetObjString
        //TPropertyType.MappedStringEnumOnStructArrayProperty, --TODO? Not in SetObjString
        TPropertyType.StringProperty,
        TPropertyType.BusProperty,
        TPropertyType.MappedStringEnumProperty,
        TPropertyType.StringOnArrayProperty,
        TPropertyType.StringOnStructArrayProperty,
        TPropertyType.BusOnStructArrayProperty
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
    // flags: TPropertyFlags;
    // sValue: String;
// begin
    // if (batch = NIL) or (batch^ = NIL) then
        // Exit;
// 
    // cls := batch^.ParentClass;
    // flags := cls.PropertyFlags[Index];
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
    propOffset: PtrUint;
    i: Integer;
    flags: TPropertyFlags;
begin
    if (batch = NIL) or (batch^ = NIL) then
        Exit;

    cls := batch^.ParentClass;
    flags := cls.PropertyFlags[Index];
    propOffset := cls.PropertyOffset[Index];

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
    flags: TPropertyFlags;
begin
    if (batch = NIL) or (batch^ = NIL) then
        Exit;

    cls := batch^.ParentClass;
    flags := cls.PropertyFlags[Index];
    propOffset := cls.PropertyOffset[Index];

    if not (cls.PropertyType[Index] in [
        TPropertyType.DoubleProperty,
        TPropertyType.DoubleOnStructArrayProperty,
        TPropertyType.DoubleOnArrayProperty
    ]) then
        Exit;

    if (cls.PropertyType[Index] = TPropertyType.DoubleProperty) and 
        (flags = []) and
        (not cls.PropertyInverse[Index]) and
        (cls.PropertyScale[Index] = 1) then
    begin
        for i := 1 to batchSize do
        begin
            doublePtr := PDouble(PtrUint(batch^) + propOffset);
            prev := doubleptr^;
            doublePtr^ := Value^;
            batch^.PropertySideEffects(Index, Round(prev));
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
    flags: TPropertyFlags;
begin
    if (batch = NIL) or (batch^ = NIL) then
        Exit;

    cls := batch^.ParentClass;
    flags := cls.PropertyFlags[Index];
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
        (flags = []) and
        (not cls.PropertyInverse[Index]) and
        (cls.PropertyScale[Index] = 1) then
    begin
        for i := 1 to batchSize do
        begin
            intPtr := PInteger(PtrUint(batch^) + propOffset);
            prev := intPtr^;
            intPtr^ := Value^;
            batch^.PropertySideEffects(Index, prev);
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
    propOffset: PtrUint;
    i: Integer;
    flags: TPropertyFlags;
begin
    if (batch = NIL) or (batch^ = NIL) then
        Exit;

    cls := batch^.ParentClass;
    flags := cls.PropertyFlags[Index];
    propOffset := cls.PropertyOffset[Index];
    
    if not (cls.PropertyType[Index] in [
        // TPropertyType.StringEnumActionProperty, --TODO? Not in SetObjString
        //TPropertyType.MappedStringEnumOnStructArrayProperty, --TODO? Not in SetObjString
        TPropertyType.StringProperty,
        TPropertyType.BusProperty,
        TPropertyType.MappedStringEnumProperty,
        TPropertyType.StringOnArrayProperty,
        TPropertyType.StringOnStructArrayProperty,
        TPropertyType.BusOnStructArrayProperty
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
    propOffset: PtrUint;
    i: Integer;
    flags: TPropertyFlags;
begin
    if (batch = NIL) or (batch^ = NIL) then
        Exit;

    cls := batch^.ParentClass;
    flags := cls.PropertyFlags[Index];
    propOffset := cls.PropertyOffset[Index];

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
    clsIdx := DSS.ClassNames.Find(clsname);
    if clsIdx = 0 then
        Exit;
    Batch_CreateFromNew(DSS, ResultPtr, ResultCount, clsIdx, Names, Count, BeginEdit);
end;

procedure Batch_CreateByClassS(DSS: TDSSContext; var ResultPtr: TDSSObjectPtr; ResultCount: PAPISize; clsName: PAnsiChar); CDECL;
var
    clsIdx: Integer;
begin
    clsIdx := DSS.ClassNames.Find(clsname);
    if clsIdx = 0 then
        Exit;
    Batch_CreateByClass(DSS, ResultPtr, ResultCount, clsIdx);
end;

procedure Batch_CreateByRegExpS(DSS: TDSSContext; var ResultPtr: TDSSObjectPtr; ResultCount: PAPISize; clsname: PAnsiChar; re: PAnsiChar); CDECL;
var
    clsIdx: Integer;
begin
    clsIdx := DSS.ClassNames.Find(clsname);
    if clsIdx = 0 then
        Exit;
    Batch_CreateByRegExp(DSS, ResultPtr, ResultCount, clsIdx, re);
end;

procedure Batch_CreateByIndexS(DSS: TDSSContext; var ResultPtr: TDSSObjectPtr; ResultCount: PAPISize; clsname: PAnsiChar; Value: PInteger; ValueCount: Integer); CDECL;
var
    clsIdx: Integer;
begin
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

end.