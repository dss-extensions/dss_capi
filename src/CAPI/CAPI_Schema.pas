unit CAPI_Schema;

interface

uses
    CAPI_Utils,
    CAPI_Types,
    DSSObject,
    fpjson;

function DSS_ExtractSchema(DSS: TDSSContext): PAnsiChar; CDECL;

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

function prepareClassJsonSchema(cls: TDSSClass; enumIds: TClassNamesHashListType): TJSONObject;
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
        '#/$defs/StringArrayOrFilePath', // StringListProperty, //TODO: maybe replace later with DSSObjectReferenceArrayProperty in lots of instances
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
    propJSON: Array of TJSONObject = NIL;
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
                        'Column', TJSONObject.Create(['type', 'integer', 'default', 1, 'exclusiveMinimum', 0]),
                        'Header', TJSONObject.Create(['type', 'boolean', 'default', false])
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
        'StringArrayOrFilePath',
        TJSONObject.Create([
            'oneOf', TJSONArray.Create([
                TJSONObject.Create(['title', 'StringArray', 'type', 'array', 'items', TJSONObject.Create(['type', 'string', 'minLength', 1])]),
                TJSONObject.Create([
                    'title', 'StringArrayFromFile',
                    'type', 'object', 
                    'properties', TJSONObject.Create([
                        'File', TJSONObject.Create(['type', 'string', 'format', 'file-path'])
                    ]),
                    'required', TJSONArray.Create(['File'])
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
        globalDefs.Add(cls.Name, prepareClassJsonSchema(cls, enumIds));
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

end.