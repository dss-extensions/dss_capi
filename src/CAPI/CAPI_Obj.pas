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

//TODO: decise if we want to expose the metadata (property index, name and type) now or later

// The classic API keeps the string buffer in the global state,
// but since this new API wants to avoid that, users must dispose
// the string copies themselves.
//TODO: consider using the same API as numeric arrays for string
procedure DSS_Dispose_String(S: PAnsiChar); CDECL;

function DSS_ExtractSchema(DSS: TDSSContext): PAnsiChar; CDECL;

function Obj_New(DSS: TDSSContext; ClsIdx: Integer; Name: PAnsiChar; Activate: TAPIBoolean; BeginEdit: TAPIBoolean): Pointer; CDECL;
function Obj_GetHandleByName(DSS: TDSSContext; ClsIdx: Integer; Name: PAnsiChar): Pointer; CDECL;
function Obj_GetHandleByIdx(DSS: TDSSContext; ClsIdx: Integer; Idx: Integer): Pointer; CDECL;

function Obj_GetName(Handle: Pointer): PAnsiChar; CDECL;
function Obj_GetNumProperties(Handle: Pointer): Integer; CDECL;
function Obj_GetCount(DSS: TDSSContext; ClsIdx: Integer): Integer; CDECL;
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
    HashList,    
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

function flagsToArray(flags: TPropertyFlags): TJSONArray;
begin
    Result := TJSONArray.Create();
    if TPropertyFlag.CustomSet in flags then Result.Add('CustomSet');
    if TPropertyFlag.CustomSetRaw in flags then Result.Add('CustomSetRaw');
    if TPropertyFlag.CustomGet in flags then Result.Add('CustomGet');
    if TPropertyFlag.IsFilename in flags then Result.Add('IsFilename');
    if TPropertyFlag.IgnoreInvalid in flags then Result.Add('IgnoreInvalid');
    if TPropertyFlag.NonPositive in flags then Result.Add('NonPositive');
    if TPropertyFlag.NonNegative in flags then Result.Add('NonNegative');
    if TPropertyFlag.NonZero in flags then Result.Add('NonZero');
    if TPropertyFlag.Transform_Abs in flags then Result.Add('Transform_Abs');
    if TPropertyFlag.Transform_LowerCase in flags then Result.Add('Transform_LowerCase');
    if TPropertyFlag.ScaledByFunction in flags then Result.Add('ScaledByFunction');
    if TPropertyFlag.WriteByFunction in flags then Result.Add('WriteByFunction');
    if TPropertyFlag.ReadByFunction in flags then Result.Add('ReadByFunction');
    if TPropertyFlag.RealPart in flags then Result.Add('RealPart');
    if TPropertyFlag.ImagPart in flags then Result.Add('ImagPart');
    if TPropertyFlag.GreaterThanOne in flags then Result.Add('GreaterThanOne');
    if TPropertyFlag.IntegerStructIndex in flags then Result.Add('IntegerStructIndex');
    if TPropertyFlag.OnArray in flags then Result.Add('OnArray');
    if TPropertyFlag.IntervalUnits in flags then Result.Add('IntervalUnits');
    if TPropertyFlag.AltIndex in flags then Result.Add('AltIndex');
    if TPropertyFlag.SizeIsFunction in flags then Result.Add('SizeIsFunction');
    if TPropertyFlag.SilentReadOnly in flags then Result.Add('SilentReadOnly');
    if TPropertyFlag.ConditionalReadOnly in flags then Result.Add('ConditionalReadOnly');
    if TPropertyFlag.IntegerToDouble in flags then Result.Add('IntegerToDouble');
    if TPropertyFlag.CheckForVar in flags then Result.Add('CheckForVar');
    if TPropertyFlag.AllowNone in flags then Result.Add('AllowNone');
    if TPropertyFlag.ArrayMaxSize in flags then Result.Add('ArrayMaxSize');
    if TPropertyFlag.ValueOffset in flags then Result.Add('ValueOffset');
    if TPropertyFlag.Redundant in flags then Result.Add('Redundant');
    if TPropertyFlag.Unused in flags then Result.Add('Unused');
    if TPropertyFlag.ConditionalValue in flags then Result.Add('ConditionalValue');
    if TPropertyFlag.FullNameAsArray in flags then Result.Add('FullNameAsArray');
    if TPropertyFlag.FullNameAsJSONArray in flags then Result.Add('FullNameAsJSONArray');
    if TPropertyFlag.Util in flags then Result.Add('Util');
    if TPropertyFlag.Deprecated in flags then Result.Add('Deprecated');
    if TPropertyFlag.InverseValue in flags then Result.Add('InverseValue');
    if TPropertyFlag.SuppressJSON in flags then Result.Add('SuppressJSON');
end;

function prepareEnum(e: TDSSEnum; enumIds: TClassNamesHashListType): TJSONObject;
var
    names, values: TJSONArray;
    i: Integer;
begin
    names := TJSONArray.Create();
    values := TJSONArray.Create();
    for i := 0 to High(e.Names) do
    begin
        names.Add(e.Names[i]);
        values.Add(e.Ordinals[i]);
    end;
    enumIds.Add(e.Name);
    Result := TJSONObject.Create([
        'name', e.Name,
        'id', Integer(enumIds.Count),
        'names', names,
        'values', values,
        'sequential', e.Sequential,
        'hybrid', e.Hybrid,
        'useFirstFound', e.UseFirstFound,
        'allowLonger', e.AllowLonger
    ]);
end;

function prepareEnumJsonSchema(e: TDSSEnum; enumIds: TClassNamesHashListType; prefixPath: String = ''): TJSONObject;
var
    names, values: TJSONArray;
    mapping: TJSONArray;
    i: Integer;
begin
    names := TJSONArray.Create();
    values := TJSONArray.Create();
    mapping := TJSONArray.Create();
    for i := 0 to High(e.AltNames) do
    begin
        if e.AltNames[i] = '' then
            continue;

        if e.AltNamesValid then
            names.Add(e.AltNames[i])
        else
            names.Add(e.Names[i]);

        values.Add(e.Ordinals[i]);
        mapping.Add(TJSONArray.Create([e.AltNames[i], e.Names[i], e.Ordinals[i]]));
    end;
    enumIds.Add(prefixPath + e.JSONName);
    if e.JSONUseNumbers then
    begin
        Result := TJSONObject.Create([
            'title', e.Name,
            'type', 'integer',
            // 'format', 'int32',
            'enum', values,
            '$dssFullEnum', mapping
        ]);
        Exit;
    end;

    if e.Hybrid then
    begin
        Result := TJSONObject.Create(['title', e.Name, 'oneOf', TJSONArray.Create([
            TJSONObject.Create(['type', 'string', 'enum', names]),
            // TJSONObject.Create(['type', 'integer', 'format', 'int32', 'minimum', 1])
            TJSONObject.Create(['type', 'integer', 'minimum', 1])
        ]), '$dssFullEnum', mapping]);
        Exit;
    end;

    Result := TJSONObject.Create([
        'title', e.Name,
        'type', 'string',
        'enum', names,
        '$dssFullEnum', mapping
    ]);
end;


function extractUnits(flags: TPropertyFlags): String;
begin
    if TPropertyFlag.Units_Hz in flags then
    begin
        Result := 'Hz';
        Exit;
    end;
    if TPropertyFlag.Units_pu_Voltage in flags then
    begin
        Result := 'pu (voltage)';
        Exit;
    end;
    if TPropertyFlag.Units_pu_Current in flags then
    begin
        Result := 'pu (current)';
        Exit;
    end;
    if TPropertyFlag.Units_pu_Power in flags then
    begin
        Result := 'pu (power)';
        Exit;
    end;
    if TPropertyFlag.Units_pu_Impedance in flags then
    begin
        Result := 'pu (impedance)';
        Exit;
    end;
    if TPropertyFlag.Units_ohmMeter in flags then
    begin
        Result := 'Ωm';
        Exit;
    end;
    if TPropertyFlag.Units_ohm in flags then
    begin
        Result := 'Ω';
        Exit;
    end;
    if TPropertyFlag.Units_ohm_per_length in flags then
    begin
        Result := 'Ω/[length_unit]';
        Exit;
    end;
    if TPropertyFlag.Units_nF_per_length in flags then
    begin
        Result := 'nF/[length_unit]';
        Exit;
    end;
    if TPropertyFlag.Units_uF in flags then
    begin
        Result := 'μF';
        Exit;
    end;
    if TPropertyFlag.Units_mH in flags then
    begin
        Result := 'mH';
        Exit;
    end;
    if TPropertyFlag.Units_uS_per_length in flags then
    begin
        Result := 'μS/[length_unit]';
        Exit;
    end;
    if TPropertyFlag.Units_s in flags then
    begin
        Result := 's';
        Exit;
    end;
    if TPropertyFlag.Units_hour in flags then
    begin
        Result := 'hour';
        Exit;
    end;
    if TPropertyFlag.Units_ToD_hour in flags then
    begin
        Result := 'ToD-hour';
        Exit;
    end;
    if TPropertyFlag.Units_minute in flags then
    begin
        Result := 'minute';
        Exit;
    end;
    if TPropertyFlag.Units_V in flags then
    begin
        Result := 'V';
        Exit;
    end;
    if TPropertyFlag.Units_W in flags then
    begin
        Result := 'W';
        Exit;
    end;
    if TPropertyFlag.Units_kW in flags then
    begin
        Result := 'kW';
        Exit;
    end;
    if TPropertyFlag.Units_kvar in flags then
    begin
        Result := 'kvar';
        Exit;
    end;
    if TPropertyFlag.Units_kVA in flags then
    begin
        Result := 'kVA';
        Exit;
    end;
    if TPropertyFlag.Units_MVA in flags then
    begin
        Result := 'MVA';
        Exit;
    end;
    if TPropertyFlag.Units_kWh in flags then
    begin
        Result := 'kWh';
        Exit;
    end;
    if TPropertyFlag.Units_V_per_km in flags then
    begin
        Result := 'V/km';
        Exit;
    end;
    if TPropertyFlag.Units_deg in flags then
    begin
        Result := '°';
        Exit;
    end;
    if TPropertyFlag.Units_degC in flags then
    begin
        Result := '°C';
        Exit;
    end;
    if TPropertyFlag.Units_A in flags then
    begin
        Result := 'A';
        Exit;
    end;
    if TPropertyFlag.Units_kV in flags then
    begin
        Result := 'kV';
        Exit;
    end;
    Result := '';
end;

function getLengthPropertyName(cls: TDSSClass; sizedPropIndex: Integer): String;
var
    propIndex: Integer;
    propOffset: PtrInt;
begin
    Result := '';
    propOffset := cls.PropertyOffset2[sizedPropIndex];
    if TPropertyFlag.IndirectCount in cls.PropertyFlags[sizedPropIndex] then
    begin
        propOffset := cls.PropertyOffset3[sizedPropIndex];
        for propIndex := 1 to cls.NumProperties do
        begin
            if (cls.PropertyType[propIndex] = TPropertyType.StringListProperty) and (cls.PropertyOffset[propIndex] = propOffset) then
            begin
                Result := cls.PropertyNameJSON[propIndex];
                Exit;
            end;
        end;
        Exit;
    end
    else
    if (TPropertyFlag.GlobalCount in cls.PropertyFlags[sizedPropIndex]) or 
        (TPropertyFlag.OnArray in cls.PropertyFlags[sizedPropIndex]) or (cls.PropertyType[sizedPropIndex] in [
        TPropertyType.BusOnStructArrayProperty,
        TPropertyType.BusesOnStructArrayProperty,
        TPropertyType.DoubleOnArrayProperty, 
        TPropertyType.IntegerOnStructArrayProperty,
        TPropertyType.DoubleOnStructArrayProperty,
        TPropertyType.MappedStringEnumOnStructArrayProperty]) then
    begin
        propOffset := cls.PropertyStructArrayCountOffset;
    end;
    Result := '';

    for propIndex := 1 to cls.NumProperties do
    begin
        if (cls.PropertyType[propIndex] = TPropertyType.IntegerProperty) and (cls.PropertyOffset[propIndex] = propOffset) then
        begin
            Result := cls.PropertyNameJSON[propIndex];
            Exit;
        end;
    end;
end;

function getIteratorPropertyName(cls: TDSSClass; sizedPropIndex: Integer): String;
var
    propIndex: Integer;
    propOffset: PtrInt;
begin
    Result := '';
    if (TPropertyFlag.OnArray in cls.PropertyFlags[sizedPropIndex]) or (cls.PropertyType[sizedPropIndex] in [
        TPropertyType.BusOnStructArrayProperty,
        TPropertyType.BusesOnStructArrayProperty,
        TPropertyType.IntegerOnStructArrayProperty,
        TPropertyType.DoubleOnStructArrayProperty,
        TPropertyType.MappedStringEnumOnStructArrayProperty]) then
    begin
        propOffset := cls.PropertyStructArrayIndexOffset;
    end
    else 
    if cls.PropertyType[sizedPropIndex] = TPropertyType.DoubleOnArrayProperty then
    begin
        propOffset := cls.PropertyOffset2[sizedPropIndex];
    end
    else
    begin
        Exit;
    end;
    for propIndex := 1 to cls.NumProperties do
    begin
        if (cls.PropertyType[propIndex] = TPropertyType.IntegerProperty) and (cls.PropertyOffset[propIndex] = propOffset) then
        begin
            Result := cls.PropertyNameJSON[propIndex];
            Exit;
        end;
    end;
end;

function indexOfIn(propIndex: Integer; const AltPropertyOrder: ArrayOfInteger): Integer;
var
    i: Integer;
begin
    Result := -1;
    for i := 1 to High(AltPropertyOrder) do
    begin
        if AltPropertyOrder[i] = propIndex then
        begin
            Result := i;
            Exit;
        end;
    end;
end;

function prepareClassJsonSchema(clsidx: Integer; cls: TDSSClass; enumIds: TClassNamesHashListType): TJSONObject;
const 
    PropertyTypeJson: array[TPropertyType] of string = (
        'number', // DoubleProperty
        'boolean', // EnabledProperty
        'string', // MakeLikeProperty
        'boolean', // BooleanActionProperty
        '-', // StringEnumActionProperty

        'numberArray', // DoubleOnArrayProperty
        'numberArray', // DoubleOnStructArrayProperty
        'string', // StringSilentROFunctionProperty

        '#/$defs/ArrayOrFilePath', // DoubleArrayProperty
        '#/$defs/ArrayOrFilePath', // DoubleDArrayProperty, // -> For dynamic arrays
        'numberArray', // DoubleVArrayProperty, // -> Use ParseAsVector
        'numberArray', // DoubleFArrayProperty, // -> For fixed-size arrays, with size in offset2
        'numberArray', // ComplexPartSymMatrixProperty,
        'numberArray', //'symMatrix', // DoubleSymMatrixProperty,

        'integerArray', // IntegerArrayProperty, // Capacitor
        'stringArray', // StringListProperty, //TODO: maybe replace later with DSSObjectReferenceArrayProperty in lots of instances
        '-', // DSSObjectReferenceProperty,
        'stringArray', //DSSObjectReferenceArrayProperty, // Line, LineGeometry

        'numberArray', //DoubleArrayOnStructArrayProperty, // AutoTrans, Transformer, XfmrCode
        
        'integer', // IntegerProperty,
        'string', // StringProperty,
        '#/$defs/Complex', // ComplexProperty,
        'boolean', // BooleanProperty,
        '#/$defs/BusConnection', // BusProperty,
        '#/$defs/Complex', // ComplexPartsProperty,

        '-', // MappedStringEnumProperty, // Lots of classes
        '-', // MappedIntEnumProperty, // Load, Generator, InvControl
        
        '-', // 'stringArray', // MappedStringEnumArrayProperty, // Fuse
        '-', // 'string', // MappedStringEnumOnStructArrayProperty, // AutoTrans, Transformer, XfmrCode
        '-', // 'stringArray', // MappedStringEnumArrayOnStructArrayProperty, // AutoTrans, Transformer, XfmrCode

        'integerArray', // IntegerOnStructArrayProperty, // AutoTrans, Transformer, XfmrCode

        '#/$defs/BusConnection', // BusOnStructArrayProperty, // AutoTrans, Transformer
        '#/$defs/BusConnectionArray', // BusesOnStructArrayProperty, // AutoTrans, Transformer

        'removed' //DeprecatedAndRemoved
    );
var 
    propIndex, propIndex_: Integer;
    propNameJSON: String;
    flags: TPropertyFlags;
    props: TJSONObject;
    parents: TJSONArray;
    localEnums: TJSONObject;
    aenum: TDSSENum;
    enumPath, stype: String;
    jtype, jtype_single: String;
    poffset2: PtrInt;
    param2: TJSONData;
    prop: TJSONObject;
    subprop: TJSONObject;
    ptype: TPropertyType;
    requiredProps: TJSONArray;

    onArray: Boolean;
    lengthProp, iteratorProp: String;

    units: String;

    // For object references
    other: TDSSObject = NIL;
    otherName, pattern: String;
    clsParent: String;
    ipattern: Integer;
    allowedClasses: Array of String;
    obj: TDSSObject = NIL;

    // For handling default values
    j: Integer;
    defaultF64: Double;
    defaultI32: Integer;
    defaultStr: String;
    defaultC128: Complex;
    readOnly, noDefault: Boolean;
    defaultF64A: PDouble = NIL;
    defaultI32A: PInteger = NIL;
    defaultStrA: PPAnsiChar = NIL;
    defaultArray: TJSONArray = NIL;
    aDim: Array[0..3] of TAPISize;

    // For handling spec sets
    toRemove: TStringList = NIL;
    oneOf: TJSONArray = NIL;
    specSet: TJSONObject = NIL;
    propName: String;
    propJSON: Array of TJSONObject;
    requiredInSpec: TJSONArray = NIL;

    zorder: Integer;
begin
    SetLength(propJSON, cls.NumProperties + 1);

    props := TJSONObject.Create();
    localEnums := TJSONObject.Create();
    parents := TJSONArray.Create();

    props.Add('Name', TJSONObject.Create([
        'title', 'Name',
        'type', 'string',
        'minLength', 1,
        'maxLength', 255,
        '$dssPropertyOrder', 0,
        '$dssPropertyIndex', 0
    ]));
    requiredProps := TJSONArray.Create(['Name']);

    with cls do
    begin
        obj := cls.NewObject('SAMPLE_FOR_DEFAULTS', false);

        for clsParent in ClassParents do
            parents.Add(clsParent);

        for propIndex_ := 1 to NumProperties do
        begin
            propIndex := propIndex_;
            propName := PropertyName[propIndex];
            propNameJSON := PropertyNameJSON[propIndex];

            ptype := PropertyType[propIndex];
            flags := PropertyFlags[propIndex];
            if (TPropertyFlag.SuppressJSON in flags) or 
                (TPropertyFlag.AltIndex in flags) or 
                (TPropertyFlag.IntegerStructIndex in flags) or
                (TPropertyFlag.Redundant in flags) or
                (TPropertyType.DeprecatedAndRemoved = ptype) then
                // Skip redundant/removed
                continue;

            if (PropertyArrayAlternative[propIndex] <> 0) then
            begin
                // Redirect to the alternative property, but keep the name!
                // This skips the redundant check on purpose.
                propIndex := PropertyArrayAlternative[propIndex];
                ptype := PropertyType[propIndex];
                flags := PropertyFlags[propIndex];
            end;
            zorder := indexOfIn(propIndex, AltPropertyOrder);

            units := extractUnits(flags);

            readOnly := (TPropertyFlag.SilentReadOnly in flags) or (TPropertyType.StringSilentROFunctionProperty = ptype);
            noDefault := (TPropertyFlag.NoDefault in flags) or (TPropertyFlag.DynamicDefault in flags);
            Str(ptype, stype);
            stype := Copy(stype, 1, Length(stype) - Length('Property'));
            jtype := PropertyTypeJson[ptype];
            prop := TJSONObject.Create();
            jtype_single := jtype;

            onArray := (('-' = jtype) and AnsiEndsStr('Array', stype)) or AnsiEndsStr('Array', jtype) or (jtype = '#/$defs/ArrayOrFilePath') or (jtype = '#/$defs/BusConnectionArray') or (TPropertyFlag.OnArray in flags);
            // WriteLn('ONARRAY FOR ', cls.Name, '.', propName, '>', cls.PropertyName[propIndex_], ' ->>> ', onArray);SysFlushStdIO();
            if onArray and (jtype <> '-') then
            begin
                if jtype = '#/$defs/ArrayOrFilePath' then
                    jtype_single := 'number'
                else if jtype = '#/$defs/BusConnectionArray' then
                begin
                    jtype_single := '#/$defs/BusConnection';
                    jtype := 'array';
                end
                else if (TPropertyFlag.OnArray in flags) then
                begin
                    jtype := 'array';
                end
                else
                begin
                    jtype_single := Copy(jtype, 1, Length(jtype) - Length('Array'));
                    jtype := 'array';
                end;
                
                if jtype[1] = '#' then
                    prop.Add('$ref', jtype)
                else
                    prop.Add('type', jtype);

                if jtype = 'array' then
                begin
                    if jtype_single[1] = '#' then
                        subprop := TJSONObject.Create(['$ref', jtype_single])
                    else
                        subprop := TJSONObject.Create(['type', jtype_single]);

                    ;//else
                        //WriteLn('TODO: handle array subtype: ', jtype_single);

                    if PropertyTrapZero[propIndex] <> 0 then
                        subprop.Add('exclusiveMinimum', 0);

                    prop.Add('items', subprop);
                end;

                // // > Handle defaults and formats
                if jtype_single = 'number' then
                begin
                    // subprop.Add('format', 'float64');
                    if not (readOnly or noDefault) then
                    begin
                        aDim[0] := 0;
                        aDim[1] := 0;
                        obj.GetDoubles(propIndex, defaultF64A, aDim);
                        defaultF64 := NaN;
                        if defaultF64A <> NIL then
                        begin
                            for j := 0 to aDim[0] - 1 do
                            begin
                                defaultF64 := defaultF64A[j];
                                if (IsNaN(defaultF64) or IsInfinite(defaultF64)) then
                                    break;
                            end;
                        end;
                        
                        if not (IsNaN(defaultF64) or IsInfinite(defaultF64)) then
                        begin
                            defaultArray := TJSONArray.Create();
                            for j := 0 to aDim[0] - 1 do
                            begin
                                defaultArray.Add(defaultF64A[j]);
                            end;
                            prop.Add('default', defaultArray);
                            defaultArray := NIL;
                        end;
                        DSS_Dispose_PDouble(defaultF64A);
                        defaultF64A := NIL;
                    end;
                end
                else if jtype_single = 'integer' then
                begin
                    // subprop.Add('format', 'int32');
                    if not (readOnly or noDefault) then
                    begin
                        aDim[0] := 0;
                        aDim[1] := 0;
                        obj.GetIntegers(propIndex, defaultI32A, aDim);
                        if (aDim[0] <> 0) and (defaultI32A <> NIL) then
                        begin
                            defaultArray := TJSONArray.Create();
                            for j := 0 to aDim[0] - 1 do
                            begin
                                defaultArray.Add(defaultI32A[j]);
                            end;
                            prop.Add('default', defaultArray);
                            defaultArray := NIL;
                        end;
                        DSS_Dispose_PInteger(defaultI32A);
                        defaultI32A := NIL;
                    end;
                end
                else if jtype_single = 'boolean' then
                begin
                    // This shouldn't exist right now.
                    WriteLn('ERROR/JSON-Schema: support for array of booleans is not implemented');
                end
                //     if not (readOnly or AnsiEndsStr('Array', stype) or noDefault) then
                //     begin
                //         defaultI32 := obj.GetInteger(propIndex);
                //         prop.Add('default', defaultI32 <> 0);
                //     end;
                // end
                else if (jtype_single = 'string') or (jtype_single = '#/$defs/BusConnection') then
                begin
                    if not (readOnly or noDefault) then
                    begin
                        aDim[0] := 0;
                        aDim[1] := 0;
                        obj.GetStrings(propIndex, defaultStrA, aDim);
                        if (aDim[0] <> 0) and (defaultStrA <> NIL) then
                        begin
                            defaultArray := TJSONArray.Create();
                            for j := 0 to aDim[0] - 1 do
                            begin
                                if defaultStrA[j] <> NIL then
                                    defaultArray.Add(String(defaultStrA[j]))
                                else
                                    defaultArray.Add(TJSONNull.Create());
                            end;
                            prop.Add('default', defaultArray);
                        end;
                        DSS_Dispose_PPAnsiChar(defaultStrA, aDim[1]);
                        defaultStrA := NIL;
                        defaultArray := NIL;
                    end;
                end;
                // // < Handle defaults
            end
            else
            if jtype <> '-' then
            begin
                if jtype[1] = '#' then
                    prop.Add('$ref', jtype)
                else
                    prop.Add('type', jtype);

                if jtype = 'number' then
                begin
                    // prop.Add('format', 'float64');
                    if not (readOnly or AnsiEndsStr('Array', stype) or noDefault) then
                    begin
                        defaultF64 := obj.GetDouble(propIndex);
                        if not (IsNaN(defaultF64) or IsInfinite(defaultF64)) then
                            prop.Add('default', defaultF64);
                    end;
                end
                else if jtype = 'integer' then
                begin
                    // prop.Add('format', 'int32');
                    if not (readOnly or AnsiEndsStr('Array', stype) or noDefault) then
                    begin
                        defaultI32 := obj.GetInteger(propIndex);
                        prop.Add('default', defaultI32);
                    end;
                end
                else if jtype = 'boolean' then
                begin
                    if not (readOnly or AnsiEndsStr('Array', stype) or noDefault) then
                    begin
                        defaultI32 := obj.GetInteger(propIndex);
                        prop.Add('default', defaultI32 <> 0);
                    end;
                end
                else if (jtype = 'string') or (jtype = '#/$defs/BusConnection') then
                begin
                    if not (readOnly or AnsiEndsStr('Array', stype) or noDefault) then
                    begin
                        defaultStr := obj.GetString(propIndex);
                        if (defaultStr <> '') and not (AnsiStartsStr('sample_for_defaults', defaultStr)) then
                            prop.Add('default', defaultStr);
                    end;
                end
                else if jtype = '#/$defs/Complex' then
                begin
                    if not (readOnly or AnsiEndsStr('Array', stype)) then
                    begin
                        defaultC128 := obj.GetComplex(propIndex);
                        if not (IsNaN(defaultC128.Re) or IsInfinite(defaultC128.Re) or 
                                IsNaN(defaultC128.Im) or IsInfinite(defaultC128.Im)) then
                            prop.Add('default', TJSONArray.Create([defaultC128.Re, defaultC128.Im]));
                    end;
                end;
                
                if (PropertyName[propIndex] = 'DynOut') or (PropertyName[propIndex] = 'WdgCurrents') then
                begin
                    prop.Add('$comment', 'TODO: use array instead of string');
                end;


                if TPropertyFlag.NonNegative in flags then
                begin
                    if (PropertyTrapZero[propIndex] <> 0) or (TPropertyFlag.NonZero in flags) then
                        prop.Add('exclusiveMinimum', 0)
                    else
                        prop.Add('minimum', 0);
                end
                //TODO
                else if (PropertyTrapZero[propIndex] <> 0) or (TPropertyFlag.NonZero in flags) then
                    prop.Add('exclusiveMinimum', 0);
            end;


            poffset2 := PropertyOffset2[propIndex];
            if PropertyType[propIndex] in [
                TPropertyType.StringEnumActionProperty,
                TPropertyType.MappedStringEnumProperty,
                TPropertyType.MappedIntEnumProperty,
                TPropertyType.MappedStringEnumArrayProperty,
                TPropertyType.MappedStringEnumOnStructArrayProperty,
                TPropertyType.MappedStringEnumArrayOnStructArrayProperty
            ] then
            begin
                aenum := TDSSEnum(Pointer(PropertyOffset2[propIndex]));
                if (enumIds.Find(aenum.JSONName) = 0) and (enumIds.Find(cls.Name + '/$defs/' + aenum.JSONName) = 0) then
                    localEnums.Add(aenum.JSONName, prepareEnumJsonSchema(aenum, enumIds, cls.Name + '/$defs/'));
                
                if enumIds.Find(aenum.JSONName) <> 0 then
                begin
                    enumPath := '#/$defs/' + aenum.JSONName;
                    param2 := CreateJSON(enumIds.Find(aenum.JSONName));
                end
                else
                begin
                    enumPath := '#/$defs/' + cls.Name + '/$defs/' + aenum.JSONName;
                    // param2 := CreateJSON(enumIds.Find(cls.Name + '/$defs/' + aenum.JSONName));
                end;

                if not onArray then
                begin
                    prop.Add('$ref', enumPath);
                    case PropertyType[propIndex] of 
                        TPropertyType.MappedIntEnumProperty:
                        begin
                            defaultI32 := obj.GetInteger(propIndex);
                            if (defaultI32 >= aenum.MinOrdinal) and (defaultI32 <= aenum.MaxOrdinal) then
                                prop.Add('default', defaultI32);
                        end;
                        TPropertyType.MappedStringEnumProperty:
                        begin
                            defaultI32 := obj.GetInteger(propIndex);
                            prop.Add('default', aenum.OrdinalToJSONValue(defaultI32));
                        end;
                    end;
                    //TODO: default for the others
                end
                else
                begin
                    prop.Add('type', 'array');
                    prop.Add('items', TJSONObject.Create(['$ref', enumPath]));
                    if not (readOnly or noDefault) then
                    begin
                        aDim[0] := 0;
                        aDim[1] := 0;
                        obj.GetIntegers(propIndex, defaultI32A, aDim);
                        if (aDim[0] <> 0) and (defaultI32A <> NIL) then
                        begin
                            defaultArray := TJSONArray.Create();
                            for j := 0 to aDim[0] - 1 do
                            begin
                                defaultArray.Add(aenum.OrdinalToJSONValue(defaultI32A[j]));
                            end;
                            prop.Add('default', defaultArray);
                            defaultArray := NIL;
                        end;
                        DSS_Dispose_PInteger(defaultI32A);
                        defaultI32A := NIL;
                    end;

                end;
            end
            else if PropertyType[propIndex] = TPropertyType.DSSObjectReferenceProperty then
            begin
                other := obj.GetObject(propIndex);
                if poffset2 = 0 then
                begin
                    prop.Add('type', 'string');
                    if TPropertyFlag.PDElement in flags then 
                        otherName := 'PDElement'
                    else
                        otherName := 'CktElement';
                    
                    if other <> NIL then
                        prop.Add('default', other.FullName);
                    
                    // prop.Add('$comment', Format('A validation regex for this would be long (%s)', [otherName]));
                end
                else
                begin
                    SetLength(allowedClasses, 1);
                    if TDSSClass(poffset2) is TProxyClass then
                        allowedClasses := TProxyClass(poffset2).TargetClassNames
                    else
                        allowedClasses[0] := TDSSClass(poffset2).Name;
                    
                    // param2 := CreateJSON(otherName);
                    
                    // Use a dumb workaround to build a pattern, since not everything handles (?i)
                    // pattern := '^';
                    // for otherName in allowedClasses do
                    // begin
                    //     if Length(pattern) > 1 then
                    //         pattern += '|('
                    //     else
                    //         pattern += '(';

                    //     for ipattern := 1 to Length(otherName) do
                    //     begin
                    //         if (AnsiLowerCase(otherName[ipattern]) <> AnsiUpperCase(otherName[ipattern])) then
                    //             pattern += Format('[%s%s]', [AnsiLowerCase(otherName[ipattern]), AnsiUpperCase(otherName[ipattern])])
                    //         else
                    //             pattern += otherName[ipattern];
                    //     end;
                    //     pattern += ')';
                    // end;
                    // pattern += '\..*$';

                    // pattern := '^#/';
                    // for otherName in allowedClasses do
                    // begin
                    //     if Length(pattern) > 3 then
                    //         pattern += '|';

                    //     pattern += '(';
                    //     pattern += otherName;
                    //     pattern += ')';
                    // end;
                    // pattern += '/.*$';

                    prop.Add('type', 'string');
                    prop.Add('minLength', 1);
                    prop.Add('maxLength', 255);
                    if other <> NIL then
                    begin
                        if Length(allowedClasses) = 1 then
                            prop.Add('default', other.Name)
                        else
                            prop.Add('default', other.FullName);
                    end;
                    // prop.Add('format', 'json-pointer');
                    //prop.Add('pattern', pattern);
                    //prop.Add('pattern', pattern);
                end;                    
            end
            else if PropertyType[propIndex] = TPropertyType.DSSObjectReferenceArrayProperty then
            begin
                if poffset2 = 0 then
                begin
                    if TPropertyFlag.PDElement in flags then 
                        param2 := CreateJSON('PDElement')
                    else
                        param2 := CreateJSON('CktElement')
                end
                else
                    param2 := CreateJSON(TDSSClass(poffset2).Name);
            end
            else
                param2 := CreateJSON(poffset2);


            if onArray then
            begin
                if ptype = TPropertyType.DoubleFArrayProperty then
                begin
                    prop.Add('minItems', PropertyOffset2[propIndex]);
                    prop.Add('maxItems', PropertyOffset2[propIndex]);
                end
                else
                begin
                    lengthProp := getLengthPropertyName(cls, propIndex);
                    if (propIndex_ <> propIndex) and (lengthProp = '') then
                        lengthProp := getLengthPropertyName(cls, propIndex_);

                    // WriteLn('LENGTH PROP FOR ', cls.Name, '.', propName, '>', cls.PropertyName[propIndex_], ' ->>> ', lengthProp);SysFlushStdIO();
                    if (lengthProp <> '') and not (TPropertyFlag.SizeIsFunction in flags) then
                    begin
                        if ANSIContainsStr(stype, 'Matrix') then
                        begin
                            prop.Add('$dssShape', TJSONArray.Create([lengthProp, lengthProp]));
                        end
                        else
                        begin
                            iteratorProp := getIteratorPropertyName(cls, propIndex);
                            if (propIndex_ <> propIndex) and (iteratorProp = '') then
                                iteratorProp := getIteratorPropertyName(cls, propIndex_);

                            prop.Add('$dssLength', lengthProp);
                            if iteratorProp <> '' then
                            begin
                                prop.Add('$dssIterator', iteratorProp);
                            end;
                        end;
                    end;
                    if propIndex <> propIndex_ then
                    begin
                        // This should give us eanough info to export the data in the two alternative representations
                        prop.Add('$dssScalarProperty', cls.PropertyNameJSON[propIndex_]);
                        prop.Add('$dssArrayProperty', cls.PropertyNameJSON[propIndex]);
                    end;
                end;
            end;
            // prop.Add('dssIndex', i);
            // prop.Add('dssSourceClass', PropertySource[i]);
            // prop.Add('dssType', stype);
            // prop.Add('dssParams', TJSONArray.Create([PropertyOffset[i], param2, PropertyOffset3[i]]));
            // prop.Add('dssFlags', flagsToArray(flags));

            // if PropertyScale[i] <> 1 then
            // begin
            //     prop.Add('dssScale', PropertyScale[i]);
            // end;
            // if PropertyValueOffset[i] <> 0 then
            // begin
            //     prop.Add('dssValueOffset', PropertyValueOffset[i]);
            // end;
            // if PropertyTrapZero[i] <> 0 then
            // begin
            //     prop.Add('dssTrapZero', PropertyTrapZero[i]);
            // end;

            if DSSPropertyHelp <> NIL then
                prop.Add('description', GetPropertyHelp(propIndex)); // TODO: if array option, use that instead
            // if TPropertyFlag.Redundant in flags then
            //     prop.Add('dssRedundantWith', PropertyRedundantWith[i]);
            // if PropertyArrayAlternative[i] <> 0 then
            //     prop.Add('dssArrayAlternative', PropertyArrayAlternative[i]);
            if (ptype = TPropertyType.DeprecatedAndRemoved) or (TPropertyFlag.Deprecated in flags) then
            begin
                prop.Add('deprecated', true);
                // prop.Add('dssDeprecationMessage', PropertyDeprecatedMessage[i]);
            end;
            if (ptype in [TPropertyType.BooleanActionProperty, TPropertyType.StringEnumActionProperty, TPropertyType.MakeLikeProperty])  then
            begin
                prop.Add('writeOnly', true);
                // if ptype = TPropertyType.MakeLikeProperty then
                //     prop.Add('$dssPropertyOrder', -1000) // the first after NAME
                // else if TPropertyFlag.Ordering_First in flags then
                //     prop.Add('$dssPropertyOrder', -999) // right after LIKE
                // else
                //     prop.Add('$dssPropertyOrder', 999); // ALWAYS the last ones
            end;
            // else
            // begin
            //     if TPropertyFlag.Ordering_First in flags then
            //         prop.Add('$dssPropertyOrder', -999); // right after LIKE
            // end;

            if readOnly then
            begin
                prop.Add('readOnly', true);
            end;
            if units <> '' then
            begin
                if units = 'ToD-hour' then
                begin
                    units := 'hour';
                    prop.Add('minimum', 0);
                    prop.Add('exclusiveMaximum', 24);
                end;
                prop.Add('units', units);
            end;

            prop.Add('title', propName);
            if TPropertyFlag.IsFilename in flags then
            begin
                prop.Add('format', 'file-path');
                lengthProp := getLengthPropertyName(cls, propIndex);
                if lengthProp <> '' then
                    prop.Add('$dssLength', lengthProp);
            end;

            prop.Add('$dssPropertyIndex', propIndex_);
            prop.Add('$dssPropertyOrder', zorder); // Use the value already prepared during the class initialization
            if TPropertyFlag.ValueOffset in flags then
                prop.Add('$dssValueOffset', PropertyValueOffset[propIndex]);

            propJSON[propIndex] := prop;
            props.Add(propNameJSON, prop);
            if (TPropertyFlag.Required in flags) then
                requiredProps.Add(propNameJSON);

        end;

        if Length(SpecSets) > 0 then
        begin
            oneOf := TJSONArray.Create();
            toRemove := TStringList.Create();
            toRemove.Sorted := true;

            for j := 0 to High(SpecSets) do
            begin
                specSet := TJSONObject.Create();
                requiredInSpec := TJSONArray.Create();                
                for propIndex in SpecSets[j] do
                begin
                    if propJSON[propIndex] = NIL then
                    begin
                        specSet.Free();
                        specSet := NIL;
                        break;
                    end;
                    specSet.Add(PropertyNameJSON[propIndex], propJSON[propIndex].Clone());
                    if toRemove.IndexOf(PropertyNameJSON[propIndex]) < 0 then
                    begin
                        toRemove.Add(PropertyNameJSON[propIndex]);
                    end;

                    if (TPropertyFlag.RequiredInSpecSet in PropertyFlags[propIndex]) then
                        requiredInSpec.Add(PropertyNameJSON[propIndex]);
                end;
                if specSet = NIL then
                    continue;

                if (specSet.Count > 0) or (specSet.Count = Length(SpecSets[j])) then
                begin
                    if requiredInSpec.Count > 0 then
                        oneOf.Add(TJSONObject.Create([
                            'title', SpecSetNames[j],
                            'type', 'object',
                            'properties', specSet,
                            'required', requiredInSpec
                        ]))
                    else
                    begin
                        oneOf.Add(TJSONObject.Create([
                            'title', SpecSetNames[j],
                            'type', 'object',
                            'properties', specSet
                        ]));
                        requiredInSpec.Free();
                    end;
                end
                else
                    specSet.Free();
            end;
            for propName in toRemove do
            begin
                propIndex := props.IndexOfName(propName);
                if propIndex < 0 then
                begin
                    WriteLn('ERROR: propIndex not found when trying to remove repeated schema property.');
                    continue;
                end;
                props.Delete(propIndex);
            end;

            toRemove.Free();
        end;

        Result := TJSONObject.Create([
            'title', cls.Class_Name,
            'type', 'object',
            // 'dssIndex', clsidx,
            // 'dssParents', parents,
            // 'dssStructArrayIndexOffset1', CreateJSON(PropertyStructArrayIndexOffset),
            // 'dssStructArrayIndexOffset2', CreateJSON(PropertyStructArrayIndexOffset2),
            'properties', props
        ]);

        if (oneOf <> NIL) and (oneOf.Count <> 0) then
            Result.Add('oneOf', oneOf);

        if (requiredProps.Count <> 0) then
            Result.Add('required', requiredProps);

        if localEnums.Count <> 0 then
            Result.Add('$defs', localEnums);
    end;
end;


function prepareClassSchema(clsidx: Integer; cls: TDSSClass; enumIds: TClassNamesHashListType): TJSONObject;
var 
    i: Integer;
    props: TJSONArray;
    parents, localEnums: TJSONArray;
    aenum: TDSSENum;
    stype: String;
    poffset2: PtrInt;
    param2: TJSONData;
    prop: TJSONObject;
begin
    props := TJSONArray.Create();
    localEnums := TJSONArray.Create();
    parents := TJSONArray.Create();

    with cls do
    begin
        for i := 1 to ClassParents.Count do
            parents.Add(ClassParents.Strings[i - 1]);

        for i := 1 to NumProperties do
        begin
            Str(PropertyType[i], stype);
            stype := Copy(stype, 1, Length(stype) - Length('Property'));

            poffset2 := PropertyOffset2[i];
            if PropertyType[i] in [
                TPropertyType.StringEnumActionProperty,
                TPropertyType.MappedStringEnumProperty,
                TPropertyType.MappedIntEnumProperty,
                TPropertyType.MappedStringEnumArrayProperty,
                TPropertyType.MappedStringEnumOnStructArrayProperty,
                TPropertyType.MappedStringEnumArrayOnStructArrayProperty
            ] then
            begin
                aenum := TDSSEnum(Pointer(PropertyOffset2[i]));
                if enumIds.Find(aenum.Name) = 0 then
                    localEnums.Add(prepareEnum(aenum, enumIds));
                
                param2 := CreateJSON(enumIds.Find(aenum.Name));
            end
            else if PropertyType[i] = TPropertyType.DSSObjectReferenceProperty then
            begin
                if poffset2 = 0 then
                begin
                    if TPropertyFlag.PDElement in PropertyFlags[i] then 
                        param2 := CreateJSON('PDElement')
                    else
                        param2 := CreateJSON('CktElement')
                end
                else
                    param2 := CreateJSON(TDSSClass(poffset2).Name);
            end
            else if PropertyType[i] = TPropertyType.DSSObjectReferenceArrayProperty then
            begin
                if poffset2 = 0 then
                begin
                    if TPropertyFlag.PDElement in PropertyFlags[i] then 
                        param2 := CreateJSON('PDElement')
                    else
                        param2 := CreateJSON('CktElement')
                end
                else
                    param2 := CreateJSON(TDSSClass(poffset2).Name);
            end
            else
                param2 := CreateJSON(poffset2);

            prop := TJSONObject.Create([
                'name', PropertyName[i],
                'altName', PropertyNameJSON[i],
                'index', i,
                'sourceClass', PropertySource[i],
                'scale', PropertyScale[i],
                'valueOffset', PropertyValueOffset[i],
                'trapZero', PropertyTrapZero[i],
                'type', stype,
                'params', TJSONArray.Create([PropertyOffset[i], param2, PropertyOffset3[i]]),
                'flags', flagsToArray(PropertyFlags[i])
            ]);
            if DSSPropertyHelp <> NIL then
                prop.Add('description', GetPropertyHelp(i));
            if TPropertyFlag.Redundant in PropertyFlags[i] then
                prop.Add('redundantWith', PropertyRedundantWith[i]);
            if PropertyArrayAlternative[i] <> 0 then
                prop.Add('arrayAlternative', PropertyArrayAlternative[i]);
            if (PropertyType[i] = TPropertyType.DeprecatedAndRemoved) or (TPropertyFlag.Deprecated in PropertyFlags[i]) then
                prop.Add('deprecationMessage', PropertyDeprecatedMessage[i]);

            props.Add(prop);
        end;

        Result := TJSONObject.Create([
            'name', cls.Class_Name,
            'index', clsidx,
            'parents', parents,
            'structArrayIndexOffset1', CreateJSON(PropertyStructArrayIndexOffset),
            'structArrayIndexOffset2', CreateJSON(PropertyStructArrayIndexOffset2),
            'properties', props,
            'classEnums', localEnums
        ]);
    end;
end;

function DSS_ExtractJSONSchema(DSS: TDSSContext): PAnsiChar; CDECL;
var
    schema, clsSchema, clsArrayDef: TJSONObject;
    globalDefs: TJSONObject;
    dssEnum: TDSSEnum;
    enumIds: TClassNamesHashListType;
    circuitProperties: TJSONObject;
    i: Integer;
    cls: TDSSClass;
begin
    if DSS = NIL then DSS := DSSPrime;
    DSS.DSSExecutive.ParseCommand('clear');
    DSS.DSSExecutive.ParseCommand('new circuit.defaults');

    Result := NIL;
    globalDefs := TJSONObject.Create();
    globalDefs.Add(
        'Complex',
        TJSONObject.Create([
            'type', 'array',
            // 'items', TJSONObject.Create(['type', 'number', 'format', 'float64']),
            'items', TJSONObject.Create(['type', 'number']),
            'minItems', 2,
            'maxItems', 2,
            '$comment', 'A **rectangular** complex number represented as an array of two floating-point numbers, real and imaginary parts.'
        ])
    );
    globalDefs.Add(
        'PComplex',
        TJSONObject.Create([
            'type', 'array',
            'items', TJSONObject.Create(['type', 'number']),
            'minItems', 2,
            'maxItems', 2,
            '$comment', 'A **polar** complex number represented as an array of two floating-point numbers, magnitude and angle parts. Angle in degrees.'
        ])
    );
    globalDefs.Add(
        'ArrayOrFilePath',
        TJSONObject.Create([
            'oneOf', TJSONArray.Create([
                TJSONObject.Create(['title', 'FloatArray', 'type', 'array', 'items', TJSONObject.Create(['type', 'number'])]),
                TJSONObject.Create([
                    'title', 'FloatArrayFromCSV',
                    'type', 'object', 
                    'properties', TJSONObject.Create([
                        'CSVFile', TJSONObject.Create(['type', 'string', 'format', 'file-path']),
                        'column', TJSONObject.Create(['type', 'integer', 'default', 1, 'exclusiveMinimum', 0]),
                        'header', TJSONObject.Create(['type', 'boolean', 'default', false])
                    ]),
                    'required', TJSONArray.Create(['CSVFile'])
                ]),
                TJSONObject.Create([
                    'title', 'FloatArrayFromDbl',
                    'type', 'object', 
                    'properties', TJSONObject.Create([
                        'DblFile', TJSONObject.Create(['type', 'string', 'format', 'file-path'])
                    ]),
                    'required', TJSONArray.Create(['DblFile'])
                ]),
                TJSONObject.Create([
                    'title', 'FloatArrayFromSng',
                    'type', 'object', 
                    'properties', TJSONObject.Create([
                        'SngFile', TJSONObject.Create(['type', 'string', 'format', 'file-path'])
                    ]),
                    'required', TJSONArray.Create(['SngFile'])
                ])
            ])
        ])
    );

    globalDefs.Add(
        'JSONFilePath',
        TJSONObject.Create([
            'title', 'JSONFilePath',
            'type', 'object',
            'properties', TJSONObject.Create([
                'JSONFile', TJSONObject.Create([
                    'type', 'string',
                    'format', 'file-path'
                ])
            ]),
            'required', TJSONArray.Create(['JSONFile'])
    ]));
    globalDefs.Add(
        'JSONLinesFilePath',
        TJSONObject.Create([
            'title', 'JSONLinesFilePath',
            'type', 'object',
            'properties', TJSONObject.Create([
                'JSONLinesFile', TJSONObject.Create([
                    'type', 'string',
                    'format', 'file-path'
                ])
            ]),
            'required', TJSONArray.Create(['JSONLinesFile'])
    ]));

    globalDefs.Add(
        'Bus',
        TJSONObject.Create([
            'type', 'object',
            'properties', TJSONObject.Create([
                'Name', TJSONObject.Create([
                    'title', 'Name',
                    'type', 'string',
                    'minLength', 1,
                    'maxLength', 255
                ]),
                'X', TJSONObject.Create([
                    'title', 'X',
                    'type', 'number'
                    // , 'format', 'float64'
                ]),
                'Y', TJSONObject.Create([
                    'title', 'Y',
                    'type', 'number'
                    // , 'format', 'float64'
                ]),
                'kVLN', TJSONObject.Create([
                    'title', 'kVLN',
                    'type', 'number',
                    // 'format', 'float64',
                    'exclusiveMinimum', 0
                ]),
                'kVLL', TJSONObject.Create([
                    'title', 'kVLL',
                    'type', 'number',
                    // 'format', 'float64',
                    'exclusiveMinimum', 0
                ]),
                'Keep', TJSONObject.Create([
                    'title', 'Keep',
                    'type', 'boolean',
                    'default', false
                ])
            ]),
            'anyOf', TJSONArray.Create([
                TJSONObject.Create([
                    'required', TJSONArray.Create(['kVLN']),
                    'not', TJSONObject.Create(['required', TJSONArray.Create(['kVLL'])])
                ]),
                TJSONObject.Create([
                    'required', TJSONArray.Create(['kVLL']),
                    'not', TJSONObject.Create(['required', TJSONArray.Create(['kVLN'])])
                ]),
                TJSONObject.Create([
                    'not', TJSONObject.Create(['required', TJSONArray.Create(['kVLN', 'kVLL'])])
                ])
            ]),
            'required', TJSONArray.Create(['Name'])
        ])
    );
    globalDefs.Add(
        'BusConnection',
        TJSONObject.Create([
            'type', 'string',
            'pattern', '[^.]+(\.[0-9]+)*'
        ])
    );

    enumIds := TClassNamesHashListType.Create(100);

    circuitProperties := TJSONObject.Create([
        'Name', TJSONObject.Create(['title', 'Name', 'type', 'string', 'minLength', 1, 'maxLength', 255]),
        'DefaultBaseFreq', TJSONObject.Create(['title', 'DefaultBaseFreq', 'type', 'float', 'exclusiveMinimum', 0, '$comment', 'Dynamic default.']),
        'PreCommands', TJSONObject.Create(['type', 'array', 'items', TJSONObject.Create(['type', 'string'])]),
        'PostCommands', TJSONObject.Create(['type', 'array', 'items', TJSONObject.Create(['type', 'string'])]),
        'Bus', TJSONObject.Create([
            'type', 'array', 
            'items', TJSONObject.Create(['$ref', '#/$defs/Bus']),
            'default', TJSONArray.Create()
        ])
        // 'circuitVsource', TJSONObject.Create(['$ref', '#/$defs/Vsource'])
    ]);

    for dssEnum in DSS.Enums do
        globalDefs.Add(dssEnum.JSONName, prepareEnumJsonSchema(dssEnum, enumIds));

    for cls in DSS.DSSClassList do
    begin
        globalDefs.Add(cls.Name, prepareClassJsonSchema(i, DSS.DSSClassList.At(i), enumIds));
        clsArrayDef := TJSONObject.Create([
            'title', cls.Name + 'List',
            'type', 'array',
            'items', TJSONObject.Create(['$ref', Format('#/$defs/%s', [cls.Name])])
        ]);
        clsSchema := TJSONObject.Create([
            'default', TJSONArray.Create(),
            'oneOf', TJSONArray.Create([
                clsArrayDef,
                TJSONObject.Create(['$ref', '#/$defs/JSONFilePath']),
                TJSONObject.Create(['$ref', '#/$defs/JSONLinesFilePath'])
            ])]);
        if cls = DSS.VSourceClass then
        begin
            clsArrayDef.Add('minLength', 1);
        end;
        circuitProperties.Add(cls.Name, clsSchema);
    end;

    schema := TJSONObject.Create([
        '$schema', 'https://json-schema.org/draft/2020-12/schema',
        '$id', 'https://dss-extensions.org/altdss-flat.schema.json',
        // 'version', DSS_CAPI_VERSION,
        // 'commit', DSS_CAPI_REV,
        '$defs', globalDefs,
        'type', 'object',
        'properties', circuitProperties,
        'required', TJSONArray.Create(['Vsource'])
    ]);
    Result := DSS_GetAsPAnsiChar(DSS, schema.FormatJSON());
    //Result := DSS_GetAsPAnsiChar(DSS, '-');
    schema.Free();
    enumIds.Free();
end;
function DSS_ExtractSchema(DSS: TDSSContext): PAnsiChar; CDECL;
// - Enums are mapped to a sequential integer id
// - Object references are translated to (class) names
var
    schema: TJSONObject;
    classes: TJSONArray;
    enums: TJSONArray;
    enumIds: TClassNamesHashListType;
    i: Integer;
begin
    if DSS = NIL then DSS := DSSPrime;

    if (SysUtils.GetEnvironmentVariable('DSS_EXTENSIONS_EXPERIMENTAL_JSON_SCHEMA') = '1') then
    begin
        Result := DSS_ExtractJSONSchema(DSS);
        Exit;
    end;

    Result := NIL;
    classes := TJSONArray.Create();
    enums := TJSONArray.Create();
    enumIds := TClassNamesHashListType.Create(100);

    for i := 1 to DSS.Enums.Count do
        enums.Add(prepareEnum(TDSSEnum(DSS.Enums[i - 1]), enumIds));

    for i := 1 to DSS.DSSClassList.Count do
        classes.Add(prepareClassSchema(i, DSS.DSSClassList.At(i), enumIds));

    schema := TJSONObject.Create([
        'version', DSS_CAPI_VERSION,
        'commit', DSS_CAPI_REV,
        'classes', classes,
        'globalEnums', enums
        // TODO: options and commands when redone
    ]);
    Result := DSS_GetAsPAnsiChar(DSS, schema.FormatJSON());
    schema.Free();
    enumIds.Free();
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
