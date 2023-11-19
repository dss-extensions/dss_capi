unit DSSObjectHelper;

interface

uses 
    Classes,
    DSSObject,
    DSSClass,
    Circuit,
    ArrayDef,
    CAPI_Types,
    fpjson,
    UComplex, DSSUcomplex;

type
{$SCOPEDENUMS ON}
    DSSJSONOptions = (
        Full = 1 shl 0,
        SkipRedundant = 1 shl 1,
        EnumAsInt = 1 shl 2,
        FullNames = 1 shl 3,
        Pretty = 1 shl 4, 
        ExcludeDisabled = 1 shl 5,
        IncludeDSSClass = 1 shl 6,
        LowercaseKeys = 1 shl 7,
        State = 1 shl 8, //TODO: power flow state, state variables for the given element, if applies
        Debug = 1 shl 9 // TODO
    );
{$SCOPEDENUMS OFF}
    
    ArrayOfDSSObject = Array of TDSSObject;

    TDSSClassHelper = class helper for TDSSClass
    private
        function GetCircuit: TDSSCircuit; inline;
    protected
        property ActiveCircuit: TDSSCircuit read GetCircuit;
    public
        procedure AddProperties_Double(props: Array of Integer; ptrs: Array of PDouble);
        procedure AddProperties_Object(props: Array of Integer; ptrs: Array of TDSSObjectPtr; clss: Array of TDSSClass);

        function ParseObjPropertyValue(Obj: Pointer; Index: Integer; const Value: String; out prevInt: Integer): Boolean;
        function GetObjPropertyValue(obj: Pointer; Index: Integer; out PropStr: String): Boolean;
        function GetObjPropertyJSONValue(obj: Pointer; Index: Integer; joptions: Integer; var val: TJSONData; preferArray: Boolean = False): Boolean;
        function SetObjPropertyJSONValue(obj: Pointer; Index: Integer; joptions: Integer; val: TJSONData): Boolean;

        //TODO: add error as result for the 16 following functions

        procedure SetObjDouble(ptr: Pointer; Index: Integer; Value: Double);
        procedure SetObjInteger(ptr: Pointer; Index: Integer; Value: Integer; prevInt: PInteger);
        procedure SetObjString(ptr: Pointer; Index: Integer; Value: String);
        procedure SetObjObject(ptr: Pointer; Index: Integer; Value: TDSSObject);

        procedure SetObjDoubles(ptr: Pointer; Index: Integer; Value: PDouble; ValueCount: Integer);
        procedure SetObjIntegers(ptr: Pointer; Index: Integer; Value: PInteger; ValueCount: Integer);
        procedure SetObjStrings(ptr: Pointer; Index: Integer; Value: PPAnsiChar; ValueCount: Integer);
        procedure SetObjObjects(ptr: Pointer; Index: Integer; Value: TDSSObjectPtr; ValueCount: Integer);

        function GetObjDouble(Obj: Pointer; Index: Integer): Double;
        function GetObjInteger(Obj: Pointer; Index: Integer): Integer;
        function GetObjString(Obj: Pointer; Index: Integer): String;
        function GetObjObject(Obj: Pointer; Index: Integer): TDSSObject;

        procedure GetObjDoubles(Obj: Pointer; Index: Integer; var ResultPtr: PDouble; ResultCount: PAPISize);
        procedure GetObjIntegers(Obj: Pointer; Index: Integer; var ResultPtr: PInteger; ResultCount: PAPISize);
        procedure GetObjStrings(Obj: Pointer; Index: Integer; var ResultPtr: PPAnsiChar; ResultCount: PAPISize);
        procedure GetObjObjects(Obj: Pointer; Index: Integer; var ResultPtr: PPointer; ResultCount: PAPISize);

        function FillObjFromJSON(obj: Pointer; json: TJSONObject; joptions: Integer): Boolean;
    end;

    TDSSObjectHelper = class helper for TDSSObject
    private
        function GetCircuit: TDSSCircuit; inline;
    protected
        property ActiveCircuit: TDSSCircuit read GetCircuit;
    public
        property Circuit: TDSSCircuit read GetCircuit;

        function AdjustInputFilePath(const Value: String): String;

        // Set[Property|Double|Integer|...] calls BeginEdit and EndEdit, for convenience, if not already in an active edit
        function ParsePropertyValue(Index: Integer; Value: String): Boolean;
        function SetDouble(Index: Integer; Value: Double): Boolean;
        function SetInteger(Index: Integer; Value: Integer): Boolean;
        function SetString(Index: Integer; Value: String): Boolean;
        function SetObject(Index: Integer; Value: TDSSObject): Boolean;
        function SetDoubles(Index: Integer; Value: ArrayOfDouble): Boolean; overload;
        function SetDoubles(Index: Integer; Value: PDouble; ValueCount: Integer): Boolean; overload;
        function SetIntegers(Index: Integer; Value: ArrayOfInteger): Boolean; overload;
        function SetIntegers(Index: Integer; Value: PInteger; ValueCount: Integer): Boolean; overload;
        function SetStrings(Index: Integer; Value: ArrayOfString): Boolean; overload;
        function SetObjects(Index: Integer; Value: ArrayOfDSSObject): Boolean; overload;
        function SetObjects(Index: Integer; Value: TDSSObjectPtr; ValueCount: Integer): Boolean; overload;
        function SetStrings(Index: Integer; Value: PPAnsiChar; ValueCount: Integer): Boolean; overload;

        function GetDouble(Index: Integer): Double;
        function GetInteger(Index: Integer): Integer;
        function GetString(Index: Integer): String;
        function GetObject(Index: Integer): TDSSObject;

        procedure GetDoubles(Index: Integer; var ResultPtr: PDouble; ResultCount: PAPISize);
        procedure GetIntegers(Index: Integer; var ResultPtr: PInteger; ResultCount: PAPISize);
        procedure GetStrings(Index: Integer; var ResultPtr: PPAnsiChar; ResultCount: PAPISize);
        procedure GetObjects(Index: Integer; var ResultPtr: PPointer; ResultCount: PAPISize);

        function GetComplex(Index: Integer): Complex; // wraps GetDoubles

        procedure BeginEdit(Activate: Boolean);
        procedure EndEdit(NumChanges: Integer);

        function PrpSpecified(idx: Integer): Boolean; inline;
    end;

implementation

uses
    DSSGlobals,
    DSSHelper,
    CktElement,
    PDElement,
    Utilities,
    SysUtils,
    UcMatrix,
    ParserDel,
    Math,
    CAPI_Utils;

type
    PLongBool = ^LongBool;
    PPDouble = ^PDouble;
    PPString= ^PString;
    PPByte = ^PByte;
    TDSSObjectPtrPtr = ^TDSSObjectPtr;
    PStringList = ^TStringList;

function TDSSClassHelper.GetCircuit: TDSSCircuit;
begin
    Result := DSS.ActiveCircuit;
end;


function constructElemName(DSS: TDSSContext; const Param: String): String;
// Construct an element name, sustituting @var values if any
var
    FClassName, FObjName: String;
begin
    ParseObjectClassandName(DSS, AnsiLowerCase(param), FClassName, FObjName);  // insert @var test
    result := Format('%s.%s', [FClassName, FObjName]);
end;

function TDSSClassHelper.ParseObjPropertyValue(Obj: Pointer; Index: Integer; const Value: String; out prevInt: Integer): Boolean;
// This handles most of the parsing and passes the processed values to 
// the specific functions (e.g. SetObjInteger) if possible, to reduce code duplication.
var
    doubleVal: Double;
    doubleVals: Array of Double;
    doublePtr: PDouble;
    integerPtr, positionPtr: PInteger;
    complexPtr: PComplex;
    complexVal: Complex;
    i, intVal: Integer;
    dataPtr: Pointer;
    ptype: TPropertyType;
    otherObjPtr: TDSSObjectPtr;
    otherObj: TDSSObject;
    cls: TDSSClass;
    scale: Double;
    stringList: TStringList;
    stringListPtr: PStringList;
    flags: TPropertyFlags;
    DataStr: String;
    enumInfo: TDSSEnum;

    OrderFound, Norder, maxSize: Integer;
    mat, matbak: TCmatrix;
    darray: PDoubleArray;
    iarray: pIntegerArray;

    errCode: Word;

    ElemName: String;

    PropParser: TDSSParser;
    objs: Array of TDSSObject;

    function GetDouble(Value: String): Double;
    begin
        Val(Value, Result, errCode);
        if errCode <> 0 then
        begin
            PropParser.CmdString := '(' + Value + ')';
            PropParser.NextParam();
            Result := PropParser.DblValue;
        end;
    end;
    function GetInteger(Value: String): Integer;
    begin
        Val(Value, Result, errCode);
        if errCode <> 0 then
        begin
            PropParser.CmdString := '(' + Value + ')';
            PropParser.NextParam();
            Result := PropParser.IntValue;
        end;
    end;
    function GetComplex(const s: String): Complex;
    // moved from Utilities -- previously InterpretComplex
    begin
        PropParser.CmdString := S;
        PropParser.NextParam();
        Result.re := PropParser.dblvalue;
        PropParser.NextParam();
        Result.im := PropParser.dblvalue;
    end;
begin
    Result := False;
    
    if (Index < 0) or (Index > NumProperties) or
        (PropertyOffset[Index] = -1) then
        Exit;
    flags := PropertyFlags[Index];

    if TPropertyFlag.CustomSetRaw in flags then
    begin
        TDSSObject(obj).CustomSetRaw(Index, Value);
        Result := True;
        Exit;
    end;

    PropParser := DSS.PropParser;

    ptype := PropertyType[Index];
    case ptype of
        TPropertyType.DeprecatedAndRemoved:
        begin
            DoSimpleMsg(
                '%s.%s: %s', 
                [TDSSObject(obj).FullName, PropertyName[Index], _(PropertyDeprecatedMessage[Index])],
                2020030);

            Result := False;
            Exit;
        end;
        TPropertyType.StringSilentROFunctionProperty:
        //TPropertyType.DoubleArraySilentROFunctionProperty:
        begin
            //TODO: error message, optionally
            Result := True;
        end;
        TPropertyType.DoubleOnArrayProperty,
        TPropertyType.DoubleOnStructArrayProperty,
        TPropertyType.DoubleProperty:
        begin
            if flags = [] then
            begin
                // Most properties don't have any flags set, just skip the checks
                SetObjDouble(Obj, Index, GetDouble(Value));
                Result := True;
                Exit;
            end;
            if TPropertyFlag.SilentReadOnly in flags then
            begin
                Result := True;
                Exit;
            end;

            if TPropertyFlag.IntervalUnits in flags then
            begin
                Val(Value, doubleVal, errCode);
                if errCode <> 0 then
                begin
                    Val(Copy(Value, 1, Length(Value) - 1), doubleVal, errCode);
                    if errCode <> 0 then
                    begin
                        DoSimpleMsg(
                            '%s.%s: Error in specification, invalid value: "%s". Units can only be h, m, or s (single char only). If omitted, "s" is assumed.', 
                            [TDSSObject(obj).FullName, PropertyName[Index], Value],
                            2020034);
                        Exit;
                    end;
                    
                    case Value[High(Value)] of
                        'h':
                            doubleVal := doubleVal * 3600;
                        'm':
                            doubleVal := doubleVal * 60;
                        's':
                            ;
                    else
                        begin
                            DoSimpleMsg(
                                '%s.%s: Error in specification, invalid value: "%s". Units can only be h, m, or s (single char only). If omitted, "s" is assumed.', 
                                [TDSSObject(obj).FullName, PropertyName[Index], Value],
                                2020035);
                            Exit;
                        end;
                    end;
                end;
            end
            else
                doubleVal := GetDouble(Value);

            SetObjDouble(Obj, index, doubleVal);
            Result := True;
        end;
        TPropertyType.MappedStringEnumOnStructArrayProperty:
        begin
            SetObjInteger(Obj, Index, TDSSEnum(Pointer(PropertyOffset2[Index])).StringToOrdinal(AnsiLowerCase(Value)), @prevInt);
            Result := True;
        end;
        TPropertyType.StringEnumActionProperty:
        begin
            SetObjInteger(Obj, Index, TDSSEnum(Pointer(PropertyOffset2[Index])).StringToOrdinal(AnsiLowerCase(Value)), @prevInt);
            Result := True;
        end;
        TPropertyType.IntegerOnStructArrayProperty,
        TPropertyType.IntegerProperty:
        begin
            if TPropertyFlag.IntervalUnits in flags then
            begin
                Val(Value, intVal, errCode);
                if errCode <> 0 then
                begin
                    Val(Copy(Value, 1, Length(Value) - 1), intVal, errCode);
                    if errCode <> 0 then
                    begin
                        DoSimpleMsg(
                            '%s.%s: Error in specification, invalid value: "%s". Units can only be h, m, or s (single char only). If omitted, "s" is assumed.', 
                            [TDSSObject(obj).FullName, PropertyName[Index], Value],
                        2020034);
                        Exit;
                    end;
                    
                    case Value[High(Value)] of
                        'h':
                            intVal := intVal * 3600;
                        'm':
                            intVal := intVal * 60;
                        's':
                            ;
                    else
                        begin
                            DoSimpleMsg(
                                '%s.%s: Error in specification, invalid value: "%s". Units can only be h, m, or s (single char only). If omitted, "s" is assumed.', 
                                [TDSSObject(obj).FullName, PropertyName[Index], Value],
                            2020035);

                            Exit;
                        end;
                    end;
                end;
            end
            else
                intVal := GetInteger(Value);

            SetObjInteger(Obj, Index, intVal, @prevInt);
            Result := True;
        end;
        TPropertyType.MappedStringEnumProperty:
            begin
            if (TPropertyFlag.ConditionalReadOnly in flags) and (PLongBool(PByte(obj) + PropertyOffset3[Index])^) then
            begin
                Result := True;
                Exit;
            end;
            SetObjInteger(Obj, Index, TDSSEnum(Pointer(PropertyOffset2[Index])).StringToOrdinal(AnsiLowerCase(Value)), @prevInt);
            Result := True;
        end;
        TPropertyType.MappedIntEnumProperty:
        begin
            enumInfo := TDSSEnum(PropertyOffset2[Index]);
            intVal := GetInteger(Value);
            if not (enumInfo.IsOrdinalValid(intVal)) then
            begin
                DoSimpleMsg(
                    Format('%s.%s: "%s" is not a valid value.',
                        [TDSSObject(obj).FullName, PropertyName[Index], Value]
                    ), 401);
                DoSimpleMsg('Invalid value (%d).', [intVal], 5004);
                Exit;
            end;
            SetObjInteger(Obj, Index, intVal, @prevInt);
            Result := True;
        end;
        TPropertyType.EnabledProperty,
        TPropertyType.BooleanActionProperty,
        TPropertyType.BooleanProperty:
        begin
            SetObjInteger(Obj, Index, Integer(InterpretYesNo(Value)), @prevInt);
            Result := True;
        end;
        TPropertyType.StringListProperty:
        begin
            if TPropertyFlag.WriteByFunction in flags then
                stringList := TStringList.Create()
            else
            begin
                stringListPtr := PStringList(PByte(obj) + PropertyOffset[Index]);
                stringList := stringListPtr^;
            end;
            InterpretTStringListArray(DSS, Value, stringList, TPropertyFlag.Transform_LowerCase in flags);
            if TPropertyFlag.WriteByFunction in flags then
                TWriteStringListPropertyFunction(Pointer(PropertyWriteFunction[Index]))(obj, stringList);

            Result := True;
        end;
        TPropertyType.ComplexProperty:
        begin
            complexPtr := PComplex(PByte(obj) + PropertyOffset[Index]);
            complexPtr^ := GetComplex(Value);
            Result := True;
        end;
        TPropertyType.ComplexPartsProperty:
        begin
            complexVal := GetComplex(Value);
            doublePtr := PDouble(PByte(obj) + PropertyOffset[Index]);
            doublePtr^ := complexVal.re;
            doublePtr := PDouble(PByte(obj) + PropertyOffset2[Index]);
            doublePtr^ := complexVal.im;
            Result := True;
        end;
        TPropertyType.BusProperty,
        TPropertyType.BusOnStructArrayProperty,
        TPropertyType.StringProperty,
        TPropertyType.MakeLikeProperty:
        begin
            SetObjString(Obj, Index, Value);
            Result := True;
        end;
        TPropertyType.BusesOnStructArrayProperty:
        begin
           // Number of items
            intVal := PInteger(PByte(obj) + PropertyOffset[Index])^;
            // Current position
            positionPtr := PInteger(PByte(obj) + PropertyStructArrayIndexOffset);

            PropParser.CmdString := Value;  // Load up Parser

            // Loop for no more than the expected number of items;  Ignore omitted values
            for i := 1 to intVal do
            begin
                PropParser.NextParam(); // ignore any parameter name  not expecting any
                DataStr := PropParser.StrValue;
                if Length(DataStr) > 0 then
                    TDSSCktElement(obj).SetBus(i, DataStr);
            end;
            positionPtr^ := intVal; // match the effective behavior of the original code

            Result := True;
        end;
        TPropertyType.IntegerArrayProperty:
        begin
            integerPtr := PInteger(PByte(obj) + PropertyOffset2[Index]);
            dataPtr := PByte(obj) + PropertyOffset[Index];
            if PPInteger(dataPtr)^ = NIL then
            begin
                // If not initialized, allocate here.
                // Note that this should not be used with dynamic arrays
                ReAllocmem(PPInteger(dataPtr)^, Sizeof(Integer) * integerPtr^);
            end;
            integerPtr^ := InterpretIntArray(DSS, 
                Value, 
                integerPtr^, 
                pIntegerArray(PPInteger(dataPtr)^)
            );
            Result := True;
        end;
        TPropertyType.MappedStringEnumArrayProperty:
        begin
            if (TPropertyFlag.SizeIsFunction in flags) then
                maxSize := TIntegerPropertyFunction(Pointer(PropertyOffset3[Index]))(obj)
            else
                maxSize := PropertyOffset3[Index];

            integerPtr := PPInteger(PByte(obj) + PropertyOffset[Index])^;

            PropParser.CmdString := Value;  // Load up Parser
            for i := 1 to maxSize do
            begin
                PropParser.NextParam(); // ignore any parameter name  not expecting any
                DataStr := PropParser.StrValue;

                if Length(DataStr) > 0 then
                    integerPtr^ := TDSSEnum(Pointer(PropertyOffset2[Index])).StringToOrdinal(PropParser.StrValue);

                Inc(integerPtr);
            end;
            Result := True;
        end;
        TPropertyType.MappedStringEnumArrayOnStructArrayProperty:
        begin
            // Number of items
            intVal := PInteger(PByte(obj) + PropertyStructArrayCountOffset)^;

            // Current position
            positionPtr := PInteger(PByte(obj) + PropertyStructArrayIndexOffset);

            // Pointer to the first of the target fields
            integerPtr := PInteger(
                PPByte(PByte(obj) + PropertyStructArrayOffset)^ +
                PropertyOffset[Index]
            );

            PropParser.CmdString := Value;  // Load up Parser
            // Loop for no more than the expected number of items;  Ignore omitted values
            for i := 1 to intVal do
            begin
                PropParser.NextParam(); // ignore any parameter name  not expecting any
                DataStr := PropParser.StrValue;

                if Length(DataStr) > 0 then
                    integerPtr^ := TDSSEnum(Pointer(PropertyOffset2[Index])).StringToOrdinal(PropParser.StrValue);

                // Move to the next position
                integerPtr := PInteger(ptruint(integerPtr) + PropertyStructArrayStep);
            end;

            positionPtr^ := intVal; // match the effective behavior of the original code

            Result := True;
        end;
        TPropertyType.DoubleSymMatrixProperty:
        begin
            scale := PropertyScale[Index];
            Norder := PInteger(PByte(obj) + PropertyOffset3[Index])^; // e.g. Fnphases
            dataPtr := PByte(obj) + PropertyOffset[Index];
            darray := Allocmem(Sizeof(Double) * Norder * Norder);
            PropParser.Token := Value;
            OrderFound := PropParser.ParseAsSymMatrix(Norder, darray, 1, scale);
            if OrderFound = Norder then
            begin
                // Replace matrix with the new pointer
                ReAllocMem(PPDouble(dataPtr)^, 0);
                PPDouble(dataPtr)^ := Pointer(darray);
            end
            else
            begin
                //TODO: Error message
                // No changes, just dispose the temporary copy
                FreeMem(darray);
            end;
            Result := (DSS.ErrorNumber = 0);
        end;
        TPropertyType.ComplexPartSymMatrixProperty:
        begin
            if TPropertyFlag.ScaledByFunction in flags then
                scale := TPropertyScaleFunction(Pointer(PropertyOffset2[Index]))(obj, False) // False = Setter scale
            else
                scale := PropertyScale[Index];

            mat := PCMatrix(Pointer(PByte(obj) + PropertyOffset[Index]))^;
            doublePtr := PDouble(mat.GetValuesArrayPtr(Norder));
            if TPropertyFlag.ImagPart in flags then
                Inc(doublePtr);

            matbak := TCMatrix.CreateMatrix(Norder);
            matbak.CopyFrom(mat);

            PropParser.Token := Value;
            OrderFound := PropParser.ParseAsSymMatrix(Norder, PDoubleArray(doublePtr), 2, scale);
            if OrderFound <> Norder then
            begin
                //TODO: Proper error message
                // No changes, restore the original copy
                mat.CopyFrom(matbak);
            end;
            matbak.Free;
            Result := (DSS.ErrorNumber = 0);
        end;

        TPropertyType.DoubleArrayProperty,
        TPropertyType.DoubleDArrayProperty,
        TPropertyType.DoubleFArrayProperty,
        TPropertyType.DoubleVArrayProperty:
        begin
            integerPtr := NIL;
            if TPropertyFlag.SizeIsFunction in flags then
            begin
                intVal := TIntegerPropertyFunction(Pointer(PropertyOffset3[Index]))(obj);
                integerPtr := @intVal;
            end
            else
            if (ptype <> TPropertyType.DoubleFArrayProperty) then
                integerPtr := PInteger(PByte(obj) + PropertyOffset2[Index]); // Size pointer

            if (TPropertyFlag.AllowNone in flags) and (Length(Value) = 4) and (Comparetext(Value, 'NONE') = 0) then
            begin
                if integerPtr <> NIL then
                    integerPtr^ := 0;
                Result := True;
                Exit;
            end;

            if TPropertyFlag.WriteByFunction in flags then
            begin
                SetLength(doubleVals, integerPtr^);
                dataPtr := @doubleVals[0];
            end
            else
                dataPtr := PByte(obj) + PropertyOffset[Index];

            if ((ptype = TPropertyType.DoubleArrayProperty) or (ptype = TPropertyType.DoubleVArrayProperty)) 
                and (PPDouble(dataPtr)^ = NIL) then
            begin
                // If not initialized, allocate here.
                // Note that this should not be used with dynamic arrays
                ReAllocmem(PPDouble(dataPtr)^, Sizeof(Double) * integerPtr^);
            end;

            case ptype of
                TPropertyType.DoubleArrayProperty,
                TPropertyType.DoubleDArrayProperty:
                begin
                    if TPropertyFlag.ArrayMaxSize in flags then
                        maxSize := PropertyOffset3[Index]
                    else
                        maxSize := integerPtr^;

                    if not (TPropertyFlag.IntegerToDouble in flags) then
                        integerPtr^ := InterpretDblArray(DSS,
                            Value, 
                            maxSize, 
                            pDoubleArray(PPDouble(dataPtr)^)
                        )
                    else
                    begin
                        iarray := Allocmem(Sizeof(Integer) * integerPtr^);
                        integerPtr^ := InterpretIntArray(DSS,
                            Value, 
                            maxSize, 
                            iarray
                        );
                        darray := pDoubleArray(PPDouble(dataPtr)^);
                        for i := 1 to integerPtr^ do
                            darray[i] := iarray[i];

                        ReallocMem(iarray, 0);
                    end;
                end;
                TPropertyType.DoubleVArrayProperty:
                begin
                    if TPropertyFlag.ArrayMaxSize in flags then
                    begin
                        maxSize := PropertyOffset3[Index];
                        PropParser.Token := Value;
                        integerPtr^ := PropParser.ParseAsVector(maxSize, pDoubleArray(PPDouble(dataPtr)^));
                    end
                    else
                    begin
                        PropParser.Token := Value;
                        PropParser.ParseAsVector(integerPtr^, pDoubleArray(PPDouble(dataPtr)^));                        
                    end;
                end;
                TPropertyType.DoubleFArrayProperty:
                begin
                    PropParser.Token := Value;
                    prevInt := PropParser.ParseAsVector(PropertyOffset2[Index], pDoubleArray(PDouble(dataPtr)));
                    if prevInt <> PropertyOffset2[Index] then
                    begin
                        //TODO: error/warn if wrong number of values specified? (Only for some properties)
                    end;
                end;
            end;

            Result := True;
            scale := PropertyScale[Index];

            if scale <> 1 then
            begin
                doublePtr := PPDouble(dataPtr)^;
                for i := 1 to integerPtr^ do
                begin
                    doublePtr^ := doublePtr^ * scale;
                    Inc(doublePtr);
                end;
            end;

            if TPropertyFlag.WriteByFunction in flags then
            begin
                SetLength(doubleVals, integerPtr^);
                TWriteDoublesPropertyFunction(Pointer(PropertyWriteFunction[Index]))(obj, @doubleVals[0], Length(doubleVals))
            end
        end;
        TPropertyType.DoubleArrayOnStructArrayProperty:
        begin
            // Number of items
            intVal := PInteger(PByte(obj) + PropertyOffset2[Index])^;

            // Current position
            positionPtr := PInteger(PByte(obj) + PropertyStructArrayIndexOffset);

            // Pointer to the first of the target fields
            doublePtr := PDouble(
                PPByte(PByte(obj) + PropertyStructArrayOffset)^ +
                PropertyOffset[Index]
            );

            scale := PropertyScale[Index];

            PropParser.CmdString := Value;  // Load up Parser
            // Loop for no more than the expected number of items;  Ignore omitted values
            for i := 1 to intVal do
            begin
                PropParser.NextParam(); // ignore any parameter name  not expecting any
                DataStr := PropParser.StrValue;

                if Length(DataStr) > 0 then
                    doublePtr^ := PropParser.Dblvalue * scale;

                // Move to the next position
                doublePtr := PDouble(ptruint(doublePtr) + PropertyStructArrayStep);
            end;

            positionPtr^ := intVal; // match the effective behavior of the original code

            Result := True;
        end;
        TPropertyType.DSSObjectReferenceProperty:
        begin
            if not (TPropertyFlag.OnArray in flags) then
                otherObjPtr := TDSSObjectPtr((PtrUint(obj) + PtrUint(PropertyOffset[Index])))
            else
            begin
                otherObjPtr := TDSSObjectPtr(PPByte(PtrUint(obj) + PropertyOffset[Index])^);
                inc(otherObjPtr, PInteger(PtrUint(obj) + PropertyStructArrayIndexOffset)^ - 1);
            end;

            cls := Pointer(PropertyOffset2[Index]);
            ElemName := Value;
            if cls <> NIL then
            begin
                if TPropertyFlag.CheckForVar in flags then
                    PropParser.CheckforVar(ElemName);

                if Length(Value) = 0 then //TODO: allow "none" to clear it too?
                    otherObj := NIL
                else
                    otherObj := cls.Find(ElemName); //TODO: add False?

                if otherObj = NIL then
                begin
                    DoSimpleMsg(
                        Format('%s.%s: %s object "%s" not found.',
                            [TDSSObject(obj).FullName, PropertyName[Index], cls.Name, Value]
                        ), 401);
                    //TODO: stop?
                end;
            end
            else
            begin
                ElemName := constructElemName(DSS, AnsiLowerCase(Value));  // substitute @var value if any
                intVal := GetCktElementIndex(DSS, ElemName);
                otherObj := NIL;
                if intVal > 0 then
                    otherObj := DSS.ActiveCircuit.CktElements.Get(intVal);

                if otherObj = NIL then
                begin
                    DoSimpleMsg(
                        Format('%s.%s: CktElement "%s" not found.',
                            [TDSSObject(obj).FullName, PropertyName[Index], Value]
                        ), 402);
                    //TODO: stop?
                end
                else if (TPropertyFlag.PDElement in flags) and not (otherObj is TPDElement) then
                begin
                    DoSimpleMsg(
                        Format('%s.%s: "%s" is not a PDElement.',
                            [TDSSObject(obj).FullName, PropertyName[Index], otherObj.FullName]
                        ), 405);
                    //TODO: stop?
                end;
            end;

            //TODO: add validation -- e.g. PD element for EnergyMeter
            if (TPropertyFlag.WriteByFunction in flags) then
                TWriteObjRefPropertyFunction(Pointer(PropertyWriteFunction[Index]))(obj, otherObj)
            else
                otherObjPtr^ := otherObj;

            Result := True;
        end;
        TPropertyType.DSSObjectReferenceArrayProperty:
        begin
            PropParser.CmdString := Value;

            // Class of the objects
            cls := Pointer(PropertyOffset2[Index]);

            if TPropertyFlag.WriteByFunction in flags then
            begin
                SetLength(objs, 0);
                PropParser.NextParam();
                if cls <> NIL then
                begin
                    while Length(PropParser.StrValue) > 0 do
                    begin
                        ElemName := PropParser.StrValue;
                        if TPropertyFlag.CheckForVar in flags then
                            PropParser.CheckforVar(ElemName);

                        otherObj := cls.Find(ElemName, False);
                        if otherObj = NIL then
                        begin
                            DoSimpleMsg(
                                Format('%s.%s: %s object "%s" not found.',
                                    [TDSSObject(obj).FullName, PropertyName[Index], cls.Name, PropParser.StrValue]
                                ), 403);
                            Exit;
                        end;
                        SetLength(objs, Length(objs) + 1);
                        objs[High(objs)] := otherObj;
                        PropParser.NextParam();
                    end;
                end
                else
                begin
                    while Length(PropParser.StrValue) > 0 do
                    begin
                        ElemName := constructElemName(DSS, AnsiLowerCase(PropParser.StrValue));
                        intVal := GetCktElementIndex(DSS, ElemName);
                        otherObj := NIL;
                        if intVal > 0 then
                            otherObj := DSS.ActiveCircuit.CktElements.Get(intVal);

                        if otherObj = NIL then
                        begin
                            DoSimpleMsg(
                                Format('%s.%s: object "%s" not found.',
                                    [TDSSObject(obj).FullName, PropertyName[Index], ElemName]
                                ), 403);
                            Exit;
                        end;
                        SetLength(objs, Length(objs) + 1);
                        objs[High(objs)] := otherObj;
                        PropParser.NextParam();
                    end;
                end;
                TWriteObjRefsPropertyFunction(Pointer(PropertyWriteFunction[Index]))(obj, PPointer(@objs[0]), Length(objs));
                Result := True;
                Exit;
            end;

            // Number of items
            intVal := PInteger(PByte(obj) + PropertyStructArrayCountOffset)^;

            if intVal < 1 then
            begin
                DoSimpleMsg(
                    Format('%s.%s: No objects are expected! Check if the order of property assignments is correct.',
                        [TDSSObject(obj).FullName, PropertyName[Index]]
                    ), 402);
                Exit;
            end;

            // Current position
            positionPtr := NIL;
            if (PropertyStructArrayIndexOffset2 <> 0) or (PropertyStructArrayIndexOffset <> 0)  then
            begin
                if TPropertyFlag.AltIndex in flags then
                    positionPtr := PInteger(PByte(obj) + PropertyStructArrayIndexOffset2)
                else
                    positionPtr := PInteger(PByte(obj) + PropertyStructArrayIndexOffset);
            end;

            // Start of array
            otherObjPtr := TDSSObjectPtrPtr((PtrUint(obj) + PtrUint(PropertyOffset[Index])))^;

            // TODO: if cls = NIL,..
            i := 0;
            for i := 1 to intVal do
            begin
                PropParser.NextParam();
                if Length(PropParser.StrValue) = 0 then
                    break;

                otherObj := cls.Find(PropParser.StrValue, False);
                if otherObj = NIL then
                begin
                    DoSimpleMsg(
                        Format('%s.%s: %s object "%s" not found.',
                            [TDSSObject(obj).FullName, PropertyName[Index], cls.Name, PropParser.StrValue]
                        ), 403);
                    //TODO: stop?
                end
                else
                    otherObjPtr^ := otherObj;

                Inc(otherObjPtr);
            end;

            if positionPtr <> NIL then
                positionPtr^ := i;

            Result := True;
        end;
    end;
end;

function FloatToStrEx(v: Double): String; inline;
begin
    if IsNaN(v) then
        Result := '----'
    else
        Result := FloatToStr(v);
end;

function GetDSSArray_JSON(n: Integer; ints: pIntegerArray; step: Integer = 4): TJSONData; overload;
var
    i: Integer;
    resArray: TJSONArray;
begin
    if ints = NIL then
    begin
        Result := TJSONNull.Create();
        Exit;
    end;
    resArray := TJSONArray.Create([]);
    for i := 0 to n-1 do
        resArray.Add(PInteger(PByte(ints) + i * step)^);

    Result := resArray;
end;

function GetDSSArray_JSON(n: Integer; dbls: pDoubleArray; scale: Double; step: Integer = 8): TJSONData; overload;
var
    i: Integer;
    resArray: TJSONArray;
begin
    if dbls = NIL then
    begin
        Result := TJSONNull.Create();
        Exit;
    end;
    resArray := TJSONArray.Create([]);
    if scale = 1 then
    begin
        for i := 0 to n-1 do
        begin
            resArray.Add(PDouble(PByte(dbls) + i * step)^);
        end
    end
    else
    begin
        for i := 0 to n-1 do
        begin
            resArray.Add(PDouble(PByte(dbls) + i * step)^ / scale);
        end;
    end;
    Result := resArray;
end;

function TDSSClassHelper.GetObjPropertyJSONValue(obj: Pointer; Index: Integer; joptions: Integer; var val: TJSONData; preferArray: Boolean): Boolean;
// Lots of code reused here from TDSSClassHelper.GetObjPropertyValue below.
// When ported to C++, we should address this duplication.
var
    c: PComplex;
    otherObj: TDSSObject;
    otherObjPtr, otherObjPtr0: TDSSObjectPtr;
    dval, scale: Double;
    stringList: TStringList;
    i, j, intVal, Norder, count, step: Integer;
    doublePtr: PDouble;
    integerPtr: PInteger;
    darray: PDoubleArray;
    mat: TCMatrix;
    ValueCount: Array[0..3] of TAPISize;
    valArray, valArrayItem: TJSONArray;
    ptype: TPropertyType;
    jsonArray: TJSONArray = NIL;
begin
    if preferArray and (PropertyArrayAlternative[Index] <> 0) then
    begin
        // This should handle most properties that have redundant element-in-array versions.
        // For the few remaining of types below, we check later in the function:
        // DoubleOnArrayProperty, 
        // DoubleOnStructArrayProperty,
        // IntegerOnStructArrayProperty,
        // DSSObjectReferenceProperty + OnArray flag
        Result := GetObjPropertyJSONValue(obj, PropertyArrayAlternative[Index], joptions, val, True);
        Exit;
    end;

    if not ((Index > 0) and (Index <= NumProperties) and (PropertyOffset[Index] <> -1)) then
    begin
        Result := False;
        Exit;
    end;

    Result := True;
    ptype := PropertyType[Index];
    case ptype of
        TPropertyType.DoubleProperty,
        TPropertyType.DoubleOnArrayProperty,
        TPropertyType.DoubleOnStructArrayProperty:
        begin
            if preferArray and (ptype in [TPropertyType.DoubleOnArrayProperty, TPropertyType.DoubleOnStructArrayProperty]) then
            begin
                if TPropertyFlag.SizeIsFunction in PropertyFlags[Index] then
                begin
                    Norder := TIntegerPropertyFunction(Pointer(PropertyOffset3[Index]))(obj)
                end
                else
                    Norder := PInteger(PByte(obj) + PropertyStructArrayCountOffset)^;

                if ptype = TPropertyType.DoubleOnArrayProperty then
                begin
                    darray := pDoubleArray(PPDouble(PByte(obj) + PropertyOffset[Index])^);
                    step := SizeOf(Double);
                end
                else
                begin
                    // DoubleOnStructArrayProperty
                    darray := pDoubleArray(
                            PPByte(PByte(obj) + PropertyStructArrayOffset)^ + // Pointer to the pointer array
                            PropertyOffset[Index] // base field
                    );
                    step := PropertyStructArrayStep;
                end;
                val := GetDSSArray_JSON(Norder, darray, PropertyScale[Index], step);
                Exit;
            end;

            // if (not TPropertyFlag.... in flags) then
            dval := GetObjDouble(obj, Index);
            if IsNaN(dval) or IsInfinite(dval) then
                val := TJSONNull.Create()
            else
                val := TJSONFloatNumber.Create(GetObjDouble(obj, Index));
            // else
            Exit;
        end;
        TPropertyType.IntegerOnStructArrayProperty,
        TPropertyType.MappedIntEnumProperty,
        TPropertyType.IntegerProperty:
        begin        
            if preferArray and (ptype = TPropertyType.IntegerOnStructArrayProperty) then
            begin
                if TPropertyFlag.SizeIsFunction in PropertyFlags[Index] then
                begin
                    Norder := TIntegerPropertyFunction(Pointer(PropertyOffset3[Index]))(obj)
                end
                else
                    Norder := PInteger(PByte(obj) + PropertyStructArrayCountOffset)^;

                val := GetDSSArray_JSON(
                    Norder, 
                    pIntegerArray(
                        PPByte(PByte(obj) + PropertyStructArrayOffset)^ + // Pointer to the pointer array
                        PropertyOffset[Index] // base field
                    ),
                    PropertyStructArrayStep
                );
                Exit;
            end
            else
            begin
                val := TJSONIntegerNumber.Create(GetObjInteger(obj, Index));
            end;
            Exit;
        end;
        TPropertyType.BooleanActionProperty,
        TPropertyType.EnabledProperty,
        TPropertyType.BooleanProperty:
        begin        
            val := TJSONBoolean.Create(GetObjInteger(obj, Index) <> 0);
            Exit;
        end;

        TPropertyType.ComplexProperty:
        begin
            c := PComplex(PByte(obj) + PropertyOffset[Index]);
            val := TJSONArray.Create([c.re, c.im]);
            Exit;
        end;

        TPropertyType.ComplexPartsProperty:
        begin
            c := PComplex(PByte(obj) + PropertyOffset[Index]);
            val := TJSONArray.Create([PDouble(PByte(obj) + PropertyOffset[Index])^, PDouble(PByte(obj) + PropertyOffset2[Index])^]);
            Exit;
        end;

        TPropertyType.BusProperty,
        TPropertyType.BusOnStructArrayProperty,
        TPropertyType.StringSilentROFunctionProperty,
        TPropertyType.StringEnumActionProperty,
        TPropertyType.StringProperty,
        TPropertyType.MakeLikeProperty,
        TPropertyType.MappedStringEnumOnStructArrayProperty,
        TPropertyType.MappedStringEnumProperty:
        begin
            if ((joptions and Integer(DSSJSONOptions.EnumAsInt) = 0) or (not (ptype in [
                TPropertyType.MappedStringEnumOnStructArrayProperty, 
                TPropertyType.MappedStringEnumProperty
            ]))) then
            begin
                val := TJSONString.Create(GetObjString(obj, Index));
                Exit;
            end
            else
            begin
                val := TJSONIntegerNumber.Create(GetObjInteger(obj, Index));
                Exit;
            end;
        end;
        TPropertyType.DSSObjectReferenceProperty:
        begin
            if TPropertyFlag.OnArray in PropertyFlags[Index] then
            begin
                Norder := PInteger(PByte(obj) + PropertyStructArrayCountOffset)^;
                otherObjPtr := TDSSObjectPtrPtr(PByte(obj) + PropertyOffset[Index])^;
                jsonArray := TJSONArray.Create();
                val := jsonArray;
                if (TPropertyFlag.FullNameAsArray in PropertyFlags[Index]) then
                begin
                    for i := 1 to Norder do
                    begin
                        jsonArray.Add(otherObjPtr^.FullName);
                        inc(otherObjPtr);
                    end;
                end
                else
                begin
                    for i := 1 to Norder do
                    begin
                        jsonArray.Add(otherObjPtr^.Name);
                        inc(otherObjPtr);
                    end;
                end;
                Exit;
            end;

            otherObj := GetObjObject(obj, Index);
            if otherObj = NIL then
                val := TJSONNull.Create()
            else if (joptions and Integer(DSSJSONOptions.FullNames)) <> 0 then
            begin
                val := TJSONString.Create(otherObj.FullName)
            end
            else
                val := TJSONString.Create(GetObjString(obj, Index));
            Exit;
        end;
        TPropertyType.DoubleArrayProperty,
        TPropertyType.DoubleDArrayProperty,
        TPropertyType.DoubleVArrayProperty:
        begin
            if TPropertyFlag.SizeIsFunction in PropertyFlags[Index] then
            begin
                Norder := TIntegerPropertyFunction(Pointer(PropertyOffset3[Index]))(obj)
            end
            else
                Norder := PInteger(PByte(obj) + PropertyOffset2[Index])^;

            if (TPropertyFlag.AllowNone in PropertyFlags[Index]) and (Norder = 0) then
            begin
                val := TJSONNull.Create();
                Exit;
            end;

            if TPropertyFlag.ReadByFunction in PropertyFlags[Index] then
            begin
                ValueCount[0] := 0;
                ValueCount[1] := 0;
                if DSS_EXTENSIONS_ARRAY_DIMS then
                begin
                    ValueCount[2] := 0;
                    ValueCount[3] := 0;
                end;
                doublePtr := NIL;
                TDoublesPropertyFunction(Pointer(PropertyReadFunction[Index]))(obj, doublePtr, @ValueCount[0]);
                val := GetDSSArray_JSON(
                    ValueCount[0],
                    pDoubleArray(doublePtr),
                    PropertyScale[Index]
                );
                DSS_Dispose_PDouble(doublePtr);
                Exit;
            end;

            val := GetDSSArray_JSON(
                Norder, 
                pDoubleArray(PPDouble(PByte(obj) + PropertyOffset[Index])^),
                PropertyScale[Index]
            );
            Exit;
        end;
        TPropertyType.DoubleFArrayProperty:
        begin
            val := GetDSSArray_JSON(
                PropertyOffset2[Index], 
                pDoubleArray(PDouble(PByte(obj) + PropertyOffset[Index])),
                PropertyScale[Index]
            );
            Exit;
        end;
        TPropertyType.DoubleSymMatrixProperty:
        begin
            scale := PropertyScale[Index];
            Norder := PInteger(PByte(obj) + PropertyOffset3[Index])^;
            darray := PDoubleArray(PByte(obj) + PropertyOffset[Index]);
            if darray = NIL then
            begin
                val := TJSONNull.Create();
                Exit;
            end;
            valArray := TJSONArray.Create([]);
            val := valArray;
            if (darray <> NIL) and (Norder > 0) then
            begin
                for i := 1 to Norder do
                begin
                    valArrayItem := TJSONArray.Create();
                    for j := 1 to Norder do
                    begin
                        valArrayItem.Add(darray[(i - 1) * Norder + j] / scale);
                    end;
                    valArray.Add(valArrayItem);
                end;
            end;
            Exit;
        end;
        TPropertyType.ComplexPartSymMatrixProperty:
        begin
            if TPropertyFlag.ScaledByFunction in PropertyFlags[Index] then
                scale := TPropertyScaleFunction(Pointer(PropertyOffset2[Index]))(obj, True) // True = Getter scale
            else
                scale := PropertyScale[Index];

            mat := PCMatrix(Pointer(PByte(obj) + PropertyOffset[Index]))^;
            if mat = NIL then
            begin
                val := TJSONNull.Create();
                Exit;
            end;

            valArray := TJSONArray.Create([]);
            val := valArray;

            if TPropertyFlag.ImagPart in PropertyFlags[Index] then
            begin
                for i := 1 to mat.order do
                begin
                    valArrayItem := TJSONArray.Create();
                    for j := 1 to mat.order do
                    begin
                        valArrayItem.Add(mat[i, j].im / scale);
                    end;
                    valArray.Add(valArrayItem);
                end
            end
            else
            begin
                for i := 1 to mat.order do
                begin
                    valArrayItem := TJSONArray.Create();
                    for j := 1 to mat.order do
                    begin
                        valArrayItem.Add(mat[i, j].re / scale);
                    end;
                    valArray.Add(valArrayItem);
                end;
            end;
            Exit;
        end;
        TPropertyType.DoubleArrayOnStructArrayProperty:
        begin
            // Number of items
            intVal := PInteger(PByte(obj) + PropertyOffset2[Index])^;

            valArray := TJSONArray.Create([]);
            val := valArray;
            if intVal <= 0 then
                Exit;

            // Pointer to the first of the target fields
            doublePtr := PDouble(
                PPByte(PByte(obj) + PropertyStructArrayOffset)^ +
                PropertyOffset[Index]
            );

            scale := PropertyScale[Index];

            for i := 1 to intVal do
            begin
                valArray.Add(doublePtr^ / scale);
                doublePtr := PDouble(ptruint(doublePtr) + PropertyStructArrayStep);
            end;
            Exit;
        end;

        TPropertyType.BusesOnStructArrayProperty:
        begin
            // Number of items
            intVal := PInteger(PByte(obj) + PropertyOffset[Index])^;

            valArray := TJSONArray.Create([]);
            val := valArray;
            if intVal <= 0 then
                Exit;

            for i := 1 to intVal do
                valArray.Add(TDSSCktElement(obj).GetBus(i));

            Exit;
        end;
        TPropertyType.MappedStringEnumArrayProperty:
        begin
            if (TPropertyFlag.SizeIsFunction in PropertyFlags[Index]) then
                intVal := TIntegerPropertyFunction(Pointer(PropertyOffset3[Index]))(obj)
            else
                intVal := PropertyOffset3[Index];

            valArray := TJSONArray.Create([]);
            val := valArray;
            if intVal <= 0 then
                Exit;

            integerPtr := PPInteger(PByte(obj) + PropertyOffset[Index])^;

            if (joptions and Integer(DSSJSONOptions.EnumAsInt)) = 0 then
            begin
                for i := 1 to intVal do
                begin
                    valArray.Add(TDSSEnum(Pointer(PropertyOffset2[Index])).OrdinalToString(integerPtr^));
                    Inc(integerPtr);
                end;
            end
            else
            begin
                for i := 1 to intVal do
                begin
                    valArray.Add(integerPtr^);
                    Inc(integerPtr);
                end;
            end;
            Exit;
        end;
        TPropertyType.MappedStringEnumArrayOnStructArrayProperty:
        begin
            // Number of items
            intVal := PInteger(PByte(obj) + PropertyStructArrayCountOffset)^;

            valArray := TJSONArray.Create([]);
            val := valArray;
            if intVal <= 0 then
                Exit;

            // Pointer to the first of the target fields
            integerPtr := PInteger(
                PPByte(PByte(obj) + PropertyStructArrayOffset)^ +
                PropertyOffset[Index]
            );

            if joptions and Integer(DSSJSONOptions.EnumAsInt) = 0 then
            begin
                for i := 1 to intVal do
                begin
                    valArray.Add(TDSSEnum(Pointer(PropertyOffset2[Index])).OrdinalToString(integerPtr^));
                    integerPtr := PInteger(ptruint(integerPtr) + PropertyStructArrayStep);
                end;
            end
            else
            begin
                for i := 1 to intVal do
                begin
                    valArray.Add(integerPtr^);
                    integerPtr := PInteger(ptruint(integerPtr) + PropertyStructArrayStep);
                end;
            end;
            Exit;
        end;

        TPropertyType.IntegerArrayProperty:
        begin
            val := GetDSSArray_JSON(
                PInteger(PByte(obj) + PropertyOffset2[Index])^, 
                pIntegerArray(PPInteger(PByte(obj) + PropertyOffset[Index])^)
            );
            Exit;
        end;
        TPropertyType.StringListProperty:
        begin
            if TPropertyFlag.ReadByFunction in PropertyFlags[Index] then
                stringList := TStringListPropertyFunction(Pointer(PropertyReadFunction[Index]))(obj)
            else
                stringList := PStringList(PByte(obj) + PropertyOffset[Index])^;

            valArray := TJSONArray.Create([]);
            val := valArray;
            for i := 0 to stringList.Count - 1 do
                valArray.Add(stringList.Strings[i]);

            if TPropertyFlag.ReadByFunction in PropertyFlags[Index] then
                stringList.Free();

            Exit;
        end;
        TPropertyType.DSSObjectReferenceArrayProperty:
        begin
            if TPropertyFlag.ReadByFunction in PropertyFlags[Index] then
            begin
                ValueCount[0] := 0;
                ValueCount[1] := 0;
                if DSS_EXTENSIONS_ARRAY_DIMS then
                begin
                    ValueCount[2] := 0;
                    ValueCount[3] := 0;
                end;
                otherObjPtr0 := NIL;
                TObjRefsPropertyFunction(Pointer(PropertyReadFunction[Index]))(obj, PPointer(otherObjPtr0), @ValueCount[0]);
                otherObjPtr := otherObjPtr0;
                count := ValueCount[0];
            end
            else
            begin
                // Number of items
                count := PInteger(PByte(obj) + PropertyStructArrayCountOffset)^;
                // Start of array
                otherObjPtr := TDSSObjectPtrPtr((PtrUint(obj) + PtrUint(PropertyOffset[Index])))^;
            end;

            jsonArray := TJSONArray.Create([]);
            val := jsonArray;
            Result := True;
            if count < 1 then
                Exit;

            if (Pointer(PropertyOffset2[Index]) = NIL) or 
                ((joptions and Integer(DSSJSONOptions.FullNames)) <> 0) or 
                (TPropertyFlag.FullNameAsJSONArray in PropertyFlags[Index]) then
            begin
                for i := 1 to count do
                begin
                    jsonArray.Add(otherObjPtr^.FullName);
                    Inc(otherObjPtr);
                end;
            end
            else
            begin
                for i := 1 to count do
                begin
                    jsonArray.Add(otherObjPtr^.Name);
                    Inc(otherObjPtr);
                end;
            end;

            if TPropertyFlag.ReadByFunction in PropertyFlags[Index] then                
                DSS_Dispose_PPointer(PPointer(otherObjPtr0));

            Exit;
        end;
    end;
    Result := False;
end;



function TDSSClassHelper.SetObjPropertyJSONValue(obj: Pointer; Index: Integer; joptions: Integer; val: TJSONData): Boolean;
// Lots of code reused here from TDSSClassHelper.GetObjPropertyValue below.
// When ported to C++, we should address this duplication.
var
    // c: PComplex;
    // otherObj: TDSSObject;
    // otherObjPtr, otherObjPtr0: TDSSObjectPtr;
    // dval, scale: Double;
    // stringList: TStringList;
    // doublePtr: PDouble;
    // integerPtr: PInteger;
    // darray: PDoubleArray;
    // mat: TCMatrix;
    // ValueCount: Array[0..3] of TAPISize;
    // valArray: TJSONArray;
    // ptype: TPropertyType;
    // jsonArray: TJSONArray = NIL;

    i: Integer;

    boolVal: Boolean;
    arrayVal: TJSONArray;
    ints: ArrayOfInteger;
    doubles: ArrayOfDouble;
    strs: ArrayOfString;
    complexVal: Complex;
    objs: Array of TDSSObject;
    otherObj: TDSSObject = NIL;
    otherName: String;
begin
    if (PropertyArrayAlternative[Index] <> 0) then
    begin
        // This should handle most properties that have redundant element-in-array versions.
        // For the few remaining of types below, we check later in the function:
        // DoubleOnArrayProperty, 
        // DoubleOnStructArrayProperty,
        // IntegerOnStructArrayProperty,
        // DSSObjectReferenceProperty + OnArray flag
        Result := GetObjPropertyJSONValue(obj, PropertyArrayAlternative[Index], joptions, val, True);
        Exit;
    end;
{
    // if not ((Index > 0) and (Index <= NumProperties) and (PropertyOffset[Index] <> -1)) then
    // begin
    //     Result := False;
    //     Exit;
    // end;

    Result := True;
    ptype := PropertyType[Index];
    
    if not (TPropertyFlag.OnArray in PropertyFlags[index]) then
    begin
        case ptype of
            TPropertyType.DoubleProperty:
            begin
                SetObjDouble(obj, index, val.AsFloat, -1);
                Exit;
            end;
            TPropertyType.MappedIntEnumProperty,
            TPropertyType.IntegerProperty:
            begin        
                SetObjInteger(obj, index, val.AsInteger, -1);
                Exit;
            end;
        end;
    end;

    case ptype of
        TPropertyType.BooleanActionProperty,
        TPropertyType.EnabledProperty,
        TPropertyType.BooleanProperty:
        begin
            boolVal := val.AsBoolean;
            if boolVal then
                SetObjInteger(obj, index, 1, -1)
            else
                SetObjInteger(obj, index, 0, -1);
            Exit;
        end;
        TPropertyType.ComplexProperty,
        TPropertyType.ComplexPartsProperty:
        begin
            arrayVal := val as TJSONArray;
            if (val = NIL) or (val.Count <> 2) then
                raise Exception(_('Expected an array of two numbers for complex property.'));

            complexVal := cmplx(val.Items[0].AsFloat, val.Items[1].AsFloat);
            SetObjDoubles(obj, index, PDouble(@complexVal), 2);
            Exit;
        end;

        TPropertyType.IntegerProperty, // onArray...
        TPropertyType.IntegerOnStructArrayProperty,
        TPropertyType.IntegerArrayProperty:
        begin
            valArray := val as TJSONArray;
            Norder := PInteger(PByte(obj) + PropertyOffset2[Index])^;
            if (valArray.Count <> Norder) then
            begin
                //TODO: update size etc.
                raise Exception(Format(_('Expected an array of %d numbers.'), [Norder]));
            end;
            SetLength(ints, valArray.Count);
            for i := 0 to valArray.Count - 1 do
            begin
                ints[i] := valArray.Items[i].AsInteger;
            end;
            SetObjIntegers(obj, index, PInteger(@ints[0]), Length(ints));
            Exit;
        end;

        TPropertyType.BusProperty,
        TPropertyType.BusOnStructArrayProperty,
        TPropertyType.BusesOnStructArrayProperty,
        TPropertyType.StringEnumActionProperty,
        TPropertyType.StringProperty,
        TPropertyType.MakeLikeProperty,
        TPropertyType.MappedStringEnumOnStructArrayProperty,
        TPropertyType.MappedStringEnumArrayOnStructArrayProperty,
        TPropertyType.MappedStringEnumArrayProperty,
        TPropertyType.MappedStringEnumProperty,
        TPropertyType.StringListProperty,
        TPropertyType.DSSObjectReferenceArrayProperty,
        TPropertyType.DSSObjectReferenceProperty:
        begin
            if ((ptype = TPropertyType.StringListProperty) and (val is TJSONObject)) then
            begin
                //TODO: adjust and use InterpretTStringListArray
                Exit;
            end;

            if (val is TJSONArray) then
            begin
                valArray := val as TJSONArray;
                if valArray.Count = 0 then
                begin
                    SetObjStrings(obj, index, NIL, 0);
                    Exit;
                end;
                val := valArray[0];
                if (varType(val.Value) = varString) then
                begin
                    if (ptype in [TPropertyType.BusOnStructArrayProperty, TPropertyType.DSSObjectReferenceArrayProperty]) or 
                        (TPropertyFlag.OnArray in PropertyFlags[Index]) then
                    begin
                        // Array of strings
                        valArray := val as TJSONArray;
                        SetLength(strs, Max(valArray.Count));
                        for i := 0 to valArray.Count - 1 do
                        begin
                            strs[i] := valArray[i].Value;
                        end;
                        SetObjStrings(obj, index, PPAnsiChar(@strs[0]), Length(strs));
                        Exit;
                    end;
                    raise Exception(_('Expected a single value, got array.'));
                end
                else
                begin
                    if (TPropertyFlag.OnArray in PropertyFlags[Index]) then
                    begin
                        // Array of strings
                        valArray := val as TJSONArray;
                        SetLength(ints, Max(valArray.Count));
                        for i := 0 to valArray.Count - 1 do
                        begin
                            ints[i] := valArray[i].Value;
                        end;
                        SetObjIntegers(obj, index, PInteger(@ints[0]), Length(ints));
                        Exit;
                    end;
                    raise Exception(_('Expected a single value, got array.'));
                end;
            end;

            if  (TPropertyFlag.OnArray in PropertyFlags[Index]) or (ptype in [
                    TPropertyType.StringListProperty,
                    TPropertyType.BusOnStructArrayProperty,
                    TPropertyType.BusesOnStructArrayProperty,
                    TPropertyType.MappedStringEnumOnStructArrayProperty,
                    TPropertyType.MappedStringEnumArrayOnStructArrayProperty,
                    TPropertyType.MappedStringEnumArrayProperty,
                    TPropertyType.DSSObjectReferenceArrayProperty
                ]) then
                    raise Exception(_('Expected array, got a single value.'));

            if (varType(val.Value) = varString) then
            begin
                // Single string
                SetObjString(obj, index, val.Value);
                Exit;
            end
            else
            begin
                // Single integer
                SetObjInteger(obj, index, val.Value, -1);
                Exit;
            end;
        end;
        TPropertyType.DoubleProperty, // onArray
        TPropertyType.DoubleOnArrayProperty,
        TPropertyType.DoubleOnStructArrayProperty,
        TPropertyType.DoubleArrayProperty,
        TPropertyType.DoubleDArrayProperty,
        TPropertyType.DoubleVArrayProperty,
        TPropertyType.DoubleArrayOnStructArrayProperty,
        TPropertyType.DoubleFArrayProperty,
        TPropertyType.DoubleSymMatrixProperty,
        TPropertyType.ComplexPartSymMatrixProperty:
        begin
            if ptype = TPropertyType.DoubleFArrayProperty then
            begin
                Norder := PropertyOffset2[Index];
            end
            else
            begin
                if TPropertyFlag.SizeIsFunction in PropertyFlags[Index] then
                begin
                    Norder := TIntegerPropertyFunction(Pointer(PropertyOffset3[Index]))(obj)
                end
                else
                if PropertyType[Index] in [TPropertyType.DoubleSymMatrixProperty, TPropertyType.ComplexPartSymMatrixProperty] then
                begin
                    Norder := PInteger(PByte(obj) + PropertyOffset3[Index])^;
                end
                else
                begin
                    Norder := PInteger(PByte(obj) + PropertyOffset2[Index])^;
                end;
            end;

            valArray := val as TJSONArray;
            if (TPropertyFlag.AllowNone in PropertyFlags[Index]) and (val.IsNull or ((valArray <> NIL) and (valArray.Count = 0))) then
            begin
                SetObjDoubles(obj, index, NIL, 0);
                Exit;
            end;

            if (valArray.Count <> Norder) then
            begin
                //TODO: update size etc.
                raise Exception(Format(_('Expected an array of %d numbers (maximum length).'), [Norder]));
            end;
            SetLength(doubles, valArray.Count);
            for i := 0 to valArray.Count - 1 do
            begin
                doubles[i] := valArray.Items[i].AsFloat;
            end;
            SetObjDoubles(obj, index, PDouble(@doubles[0]), Length(doubles));
            Exit;
        end;
    end;
    Result := False;}
end;

function TDSSClassHelper.GetObjPropertyValue(obj: Pointer; Index: Integer; out PropStr: String): Boolean;
var
    c: PComplex;
    otherObj: TDSSObject;
    otherObjPtr, otherObjPtr0: TDSSObjectPtr;
    scale: Double;
    stringList: TStringList;
    i, j, intVal, Norder, count: Integer;
    doublePtr: PDouble;
    integerPtr: PInteger;
    darray: PDoubleArray;
    mat: TCMatrix;
    ValueCount: Array[0..3] of Integer;
begin
    if (Index > 0) and (Index <= NumProperties) and 
       (PropertyOffset[Index] <> -1) then
    begin
        case PropertyType[Index] of
            TPropertyType.DoubleProperty,
            TPropertyType.DoubleOnArrayProperty,
            TPropertyType.DoubleOnStructArrayProperty:
                PropStr := FloatToStrEx(GetObjDouble(obj, Index));

            TPropertyType.BusProperty,
            TPropertyType.BusOnStructArrayProperty,
            TPropertyType.StringSilentROFunctionProperty,
            TPropertyType.StringEnumActionProperty,
            TPropertyType.MappedStringEnumOnStructArrayProperty,
            TPropertyType.MappedStringEnumProperty,
            TPropertyType.StringProperty,
            TPropertyType.MakeLikeProperty:
                PropStr := GetObjString(obj, Index);

            TPropertyType.IntegerOnStructArrayProperty,
            TPropertyType.MappedIntEnumProperty,
            TPropertyType.IntegerProperty:
                PropStr := IntToStr(GetObjInteger(obj, Index));

            TPropertyType.BooleanActionProperty,
            TPropertyType.EnabledProperty,
            TPropertyType.BooleanProperty:
                PropStr := StrYOrN(Boolean(GetObjInteger(obj, Index) <> 0));


            TPropertyType.ComplexProperty:
            begin
                c := PComplex(PByte(obj) + PropertyOffset[Index]);
                PropStr := Format('[%g, %g]', [c.re, c.im]);
            end;

            TPropertyType.ComplexPartsProperty:
                PropStr := Format('[%g, %g]', [
                    PDouble(PByte(obj) + PropertyOffset[Index])^,
                    PDouble(PByte(obj) + PropertyOffset2[Index])^
                ]);
            
            TPropertyType.DoubleArrayProperty,
            TPropertyType.DoubleDArrayProperty,
            TPropertyType.DoubleVArrayProperty:
            begin
                if TPropertyFlag.SizeIsFunction in PropertyFlags[Index] then
                begin
                    Norder := TIntegerPropertyFunction(Pointer(PropertyOffset3[Index]))(obj);
                end
                else
                    Norder := PInteger(PByte(obj) + PropertyOffset2[Index])^;

                if (TPropertyFlag.AllowNone in PropertyFlags[Index]) and (Norder = 0) then
                begin
                    PropStr := '[NONE]';
                    Exit;
                end;

                if TPropertyFlag.ReadByFunction in PropertyFlags[Index] then
                begin
                    ValueCount[0] := 0;
                    ValueCount[1] := 0;
                    if DSS_EXTENSIONS_ARRAY_DIMS then
                    begin
                        ValueCount[2] := 0;
                        ValueCount[3] := 0;
                    end;
                    doublePtr := NIL;
                    TDoublesPropertyFunction(Pointer(PropertyReadFunction[Index]))(obj, doublePtr, @ValueCount[0]);
                    PropStr := GetDSSArray(
                        ValueCount[0],
                        pDoubleArray(doublePtr),
                        PropertyScale[Index]
                    );
                    DSS_Dispose_PDouble(doublePtr);
                    Exit;
                end;

                PropStr := GetDSSArray(
                    Norder, 
                    pDoubleArray(PPDouble(PByte(obj) + PropertyOffset[Index])^),
                    PropertyScale[Index]
                );
            end;
            TPropertyType.DoubleFArrayProperty:
                PropStr := GetDSSArray(
                    PropertyOffset2[Index], 
                    pDoubleArray(PDouble(PByte(obj) + PropertyOffset[Index])),
                    PropertyScale[Index]
                );
            TPropertyType.DoubleSymMatrixProperty:
            begin
                scale := PropertyScale[Index];
                Norder := PInteger(PByte(obj) + PropertyOffset3[Index])^;
                darray := PDoubleArray(PByte(obj) + PropertyOffset[Index]);
                PropStr := '(';
                if (darray <> NIL) and (Norder > 0) then
                    for i := 1 to Norder do
                    begin
                        for j := 1 to i do
                        begin
                            PropStr := PropStr + Format('%g', [darray[(i - 1) * Norder + j] / scale]) + ' ';
                        end;
                        if i < Norder then
                            PropStr := PropStr + '|';
                    end;
                PropStr := PropStr + ')';
            end;
            TPropertyType.ComplexPartSymMatrixProperty:
            begin
                if TPropertyFlag.ScaledByFunction in PropertyFlags[Index] then
                    scale := TPropertyScaleFunction(Pointer(PropertyOffset2[Index]))(obj, True) // True = Getter scale
                else
                    scale := PropertyScale[Index];

                mat := PCMatrix(Pointer(PByte(obj) + PropertyOffset[Index]))^;
                if mat = NIL then
                begin
                    PropStr := '';
                    Result := True;                    
                    Exit;
                end;
                PropStr := '[';
                if TPropertyFlag.ImagPart in PropertyFlags[Index] then
                    for i := 1 to mat.order do
                    begin
                        for j := 1 to i do
                            PropStr := PropStr + Format('%g ', [mat[i, j].im / scale]);
                        if i < mat.order then
                            PropStr := PropStr + '|';
                    end
                else
                    for i := 1 to mat.order do
                    begin
                        for j := 1 to i do
                            PropStr := PropStr + Format('%g ', [mat[i, j].re / scale]);
                        if i < mat.order then
                            PropStr := PropStr + '|';
                    end;
                PropStr := PropStr + ']';
            end;
            TPropertyType.DoubleArrayOnStructArrayProperty:
            begin
                // Number of items
                intVal := PInteger(PByte(obj) + PropertyOffset2[Index])^;

                if intVal <= 0 then
                begin
                    PropStr := '[]';
                    Result := True;
                    Exit;
                end;

                // Pointer to the first of the target fields
                doublePtr := PDouble(
                    PPByte(PByte(obj) + PropertyStructArrayOffset)^ +
                    PropertyOffset[Index]
                );

                scale := PropertyScale[Index];
                
                propStr := '[';
                for i := 1 to intVal do
                begin
                    PropStr := PropStr + Format('%.7g, ', [doublePtr^ / scale]);
                    doublePtr := PDouble(ptruint(doublePtr) + PropertyStructArrayStep);
                end;
                propStr := propStr + ']';
            end;

            TPropertyType.BusesOnStructArrayProperty:
            begin
                // Number of items
                intVal := PInteger(PByte(obj) + PropertyOffset[Index])^;

                if intVal <= 0 then
                begin
                    PropStr := '[]';
                    Result := True;                    
                    Exit;
                end;

                propStr := '[';
                for i := 1 to intVal do
                    PropStr := PropStr + Format('%s, ', [TDSSCktElement(obj).GetBus(i)]);

                propStr := propStr + ']';
            end;
            TPropertyType.MappedStringEnumArrayProperty:
            begin
                if (TPropertyFlag.SizeIsFunction in PropertyFlags[Index]) then
                    intVal := TIntegerPropertyFunction(Pointer(PropertyOffset3[Index]))(obj)
                else
                    intVal := PropertyOffset3[Index];

                if intVal <= 0 then
                begin
                    PropStr := '[]';
                    Result := True;
                    Exit;
                end;

                integerPtr := PPInteger(PByte(obj) + PropertyOffset[Index])^;

                propStr := '[';
                for i := 1 to intVal do
                begin
                    PropStr := PropStr + TDSSEnum(Pointer(PropertyOffset2[Index])).OrdinalToString(integerPtr^) + ', ';
                    Inc(integerPtr);
                end;
                propStr := propStr + ']';
                Result := True;
            end;
            TPropertyType.MappedStringEnumArrayOnStructArrayProperty:
            begin
                // Number of items
                intVal := PInteger(PByte(obj) + PropertyStructArrayCountOffset)^;

                if intVal <= 0 then
                begin
                    PropStr := '[]';
                    Result := True;
                    Exit;
                end;

                // Pointer to the first of the target fields
                integerPtr := PInteger(
                    PPByte(PByte(obj) + PropertyStructArrayOffset)^ +
                    PropertyOffset[Index]
                );

                propStr := '[';

                for i := 1 to intVal do
                begin
                    PropStr := PropStr + TDSSEnum(Pointer(PropertyOffset2[Index])).OrdinalToString(integerPtr^) + ', ';
                    integerPtr := PInteger(ptruint(integerPtr) + PropertyStructArrayStep);
                end;

                propStr := propStr + ']';
            end;

            TPropertyType.IntegerArrayProperty:
                PropStr := GetDSSArray(
                    PInteger(PByte(obj) + PropertyOffset2[Index])^, 
                    pIntegerArray(PPInteger(PByte(obj) + PropertyOffset[Index])^)
                );

            TPropertyType.DSSObjectReferenceProperty:
            begin
                otherObj := GetObjObject(Obj, Index);
                if otherObj <> NIL then
                begin
                    if PropertyOffset2[Index] = 0 then
                        PropStr := otherObj.FullName
                    else
                        PropStr := otherObj.Name
                end
                else
                    PropStr := '';
            end;
            TPropertyType.StringListProperty:
            begin
                if TPropertyFlag.ReadByFunction in PropertyFlags[Index] then
                    stringList := TStringListPropertyFunction(Pointer(PropertyReadFunction[Index]))(obj)
                else
                    stringList := PStringList(PByte(obj) + PropertyOffset[Index])^;

                PropStr := StringListToString(stringList);
                if TPropertyFlag.ReadByFunction in PropertyFlags[Index] then
                    stringList.Free();
            end;
            TPropertyType.DSSObjectReferenceArrayProperty:
            begin
                if TPropertyFlag.ReadByFunction in PropertyFlags[Index] then
                begin
                    ValueCount[0] := 0;
                    ValueCount[1] := 0;
                    if DSS_EXTENSIONS_ARRAY_DIMS then
                    begin
                        ValueCount[2] := 0;
                        ValueCount[3] := 0;
                    end;
                    otherObjPtr0 := NIL;
                    TObjRefsPropertyFunction(Pointer(PropertyReadFunction[Index]))(obj, PPointer(otherObjPtr0), @ValueCount[0]);
                    otherObjPtr := otherObjPtr0;
                    count := ValueCount[0];
                end
                else
                begin
                    // Number of items
                    count := PInteger(PByte(obj) + PropertyStructArrayCountOffset)^;
                    // Start of array
                    otherObjPtr := TDSSObjectPtrPtr((PtrUint(obj) + PtrUint(PropertyOffset[Index])))^;
                end;

                if count < 1 then
                begin
                    PropStr := '[]';
                    Exit;
                end;

                PropStr := '[';
                if Pointer(PropertyOffset2[Index]) = NIL then
                begin
                    for i := 1 to count do
                    begin
                        PropStr := PropStr + CheckForBlanks(otherObjPtr^.FullName);
                        if i <> count then
                            PropStr := PropStr + ', ';
                        Inc(otherObjPtr);
                    end;
                end
                else
                begin
                    for i := 1 to count do
                    begin
                        PropStr := PropStr + CheckForBlanks(otherObjPtr^.Name);
                        if i <> count then
                            PropStr := PropStr + ', ';
                        Inc(otherObjPtr);
                    end;
                end;
                PropStr := PropStr + ']';

                if TPropertyFlag.ReadByFunction in PropertyFlags[Index] then                
                    DSS_Dispose_PPointer(PPointer(otherObjPtr0))
            end;           
        end;
        Result := True;
    end;
    Result := False;
end;

procedure TDSSClassHelper.AddProperties_Double(props: Array of Integer; ptrs: Array of PDouble);
var
    i: Integer;
begin
    if Length(props) <> Length(ptrs) then
        raise Exception.Create('Number of properties must match number of pointers');

    for i := 0 to High(Props) do
    begin
        PropertyType[props[i]] := TPropertyType.DoubleProperty;
        PropertyOffset[props[i]] := ptrint(ptrs[i]);
    end;
end;

procedure TDSSClassHelper.AddProperties_Object(props: Array of Integer; ptrs: Array of TDSSObjectPtr; clss: Array of TDSSClass);
var
    i: Integer;
begin
    if (Length(props) <> Length(ptrs)) or (Length(props) <> Length(clss)) then
        raise Exception.Create('Number of properties must match number of pointers');

    for i := 0 to High(Props) do
    begin
        PropertyType[props[i]] := TPropertyType.DSSObjectReferenceProperty;
        PropertyOffset[props[i]] := ptrint(ptrs[i]);
        PropertyOffset2[props[i]] := ptrint(clss[i]);
    end;
end;

procedure TDSSClassHelper.SetObjObjects(ptr: Pointer;Index: Integer; Value: TDSSObjectPtr; ValueCount: Integer);
// Note: there is some duplication between this and ParseObjPropertyValue
var
    otherObjPtr: TDSSObjectPtr;
    i, maxCount: Integer;
    positionPtr: PInteger;
    flags: TPropertyFlags;
    Obj: TDSSObject;
begin
    Obj := TDSSObject(ptr);
    flags := PropertyFlags[Index];
    if TPropertyFlag.WriteByFunction in flags then
    begin
        TWriteObjRefsPropertyFunction(PropertyWriteFunction[Index])(obj, PPointer(Value), ValueCount);
        Exit;
    end;

    // Number of items
    maxCount := PInteger(PByte(obj) + PropertyStructArrayCountOffset)^;

    if maxCount < 1 then
    begin
        DoSimpleMsg(
            Format('%s.%s: No objects are expected! Check if the order of property assignments is correct.',
                [TDSSObject(obj).FullName, PropertyName[Index]]
            ), 402);
        Exit;
    end;

    // Current position
    positionPtr := NIL;
    if (PropertyStructArrayIndexOffset2 <> 0) or (PropertyStructArrayIndexOffset <> 0)  then
    begin
        if TPropertyFlag.AltIndex in flags then
            positionPtr := PInteger(PByte(obj) + PropertyStructArrayIndexOffset2)
        else
            positionPtr := PInteger(PByte(obj) + PropertyStructArrayIndexOffset);
    end;

    // Start of array
    otherObjPtr := TDSSObjectPtrPtr((PtrUint(obj) + PtrUint(PropertyOffset[Index])))^;

    //TODO: disallow incomplete arrays?
    maxCount := Min(ValueCount, maxCount);

    for i := 1 to maxCount do
    begin
        //TODO: add type validation
        otherObjPtr^ := Value^;
        Inc(otherObjPtr);
        Inc(Value);
    end;

    if positionPtr <> NIL then
        positionPtr^ := maxCount;
end;

procedure TDSSClassHelper.SetObjObject(ptr: Pointer; Index: Integer; Value: TDSSObject);
var
    otherObjPtr: TDSSObjectPtr;
    flags: TPropertyFlags;
    posPtr: PInteger;
    Obj: TDSSObject;
begin
    Obj := TDSSObject(ptr);
    flags := PropertyFlags[Index];

    //TODO: add type validation -- e.g. PD element for EnergyMeter
    if (TPropertyFlag.WriteByFunction in flags) then
        TWriteObjRefPropertyFunction(Pointer(PropertyWriteFunction[Index]))(obj, Value)
    else if not (TPropertyFlag.OnArray in flags) then
    begin
        otherObjPtr := TDSSObjectPtr((PtrUint(obj) + PtrUint(PropertyOffset[Index])));
        otherObjPtr^ := Value;
    end
    else // if TPropertyFlag.OnArray in flags then
    begin
        otherObjPtr := TDSSObjectPtr(PPByte(PtrUint(obj) + PropertyOffset[Index])^); // start of array
        posPtr := PInteger(PtrUint(obj) + PropertyStructArrayIndexOffset);
        inc(otherObjPtr, posPtr^ - 1);
        otherObjPtr^ := Value;
    end;
end;

procedure TDSSClassHelper.SetObjString(ptr: Pointer; Index: Integer; Value: String);
var
    stringPtr: PString;
    otherObj: TDSSObject;
    prevInt: Integer;
    flags: TPropertyFlags;
    Obj: TDSSObject;
begin
    Obj := TDSSObject(ptr);
    flags := PropertyFlags[Index];
    //TODO: if IsFilename, validate path here
    if TPropertyFlag.Transform_LowerCase in flags then
        Value := AnsiLowerCase(Value);

    case PropertyType[Index] of
        TPropertyType.DSSObjectReferenceProperty:
            ParseObjPropertyValue(Obj, Index, Value, prevInt);

        TPropertyType.MakeLikeProperty:
        begin
            otherObj := obj.ParentClass.Find(Value);
            if otherObj = NIL then
            begin
                DoSimpleMsg('Error in %s MakeLike: "%s" not found.', [obj.ParentClass.Name, Value], 383);
                Exit;
            end;
            obj.MakeLike(otherObj);
        end;
        TPropertyType.StringProperty:
        begin
            if TPropertyFlag.WriteByFunction in flags then
                TWriteStringPropertyFunction(Pointer(PropertyWriteFunction[Index]))(obj, Value)
            else
            begin
                //TODO: if IsFilename, validate path here
                stringPtr := PString(PByte(obj) + PropertyOffset[Index]);
                stringPtr^ := Value;
            end;
        end;
        TPropertyType.BusProperty:
        begin
            if TPropertyFlag.WriteByFunction in flags then
                TWriteStringPropertyFunction(Pointer(PropertyWriteFunction[Index]))(obj, Value)
            else
            begin
            TDSSCktElement(obj).SetBus(PropertyOffset[Index], Value);
            end;
        end;
        TPropertyType.BusOnStructArrayProperty:
            TDSSCktElement(obj).SetBus(PInteger(PByte(obj) + PropertyStructArrayIndexOffset)^, Value);
        TPropertyType.StringEnumActionProperty,
        TPropertyType.MappedStringEnumProperty:
        begin
            if (TPropertyFlag.ConditionalReadOnly in flags) and (PLongBool(PByte(obj) + PropertyOffset3[Index])^) then
                Exit;
            SetObjInteger(Obj, Index, TDSSEnum(Pointer(PropertyOffset2[Index])).StringToOrdinal(AnsiLowerCase(Value)), @prevInt);
        end;            
    end;
end;

procedure TDSSClassHelper.SetObjDouble(ptr: Pointer; Index: Integer; Value: Double);
var
    flags: TPropertyFlags;
    scale: Double;
    doublePtr: PDouble;
    Obj: TDSSObject;
begin
    Obj := TDSSObject(ptr);
    flags := PropertyFlags[Index];
    scale := PropertyScale[Index];
    if (flags = []) and (PropertyType[Index] = TPropertyType.DoubleProperty) then
    begin
        // Most properties don't have any flags set, just skip the checks
        doublePtr := PDouble(PByte(obj) + PropertyOffset[Index]);
        doublePtr^ := Value * scale;
        Exit;
    end;

    if TPropertyFlag.SilentReadOnly in flags then
        Exit; // Just in case a user calls this by error, no need to do anything

    if TPropertyFlag.ScaledByFunction in flags then
        scale := TPropertyScaleFunction(Pointer(PropertyOffset2[Index]))(obj, False); // False = Setter scale

    if flags <> [] then
    begin
        if (TPropertyFlag.GreaterThanOne in flags) and (Value <= 1) then
        begin
            if not (TPropertyFlag.IgnoreInvalid in flags) then
                DoSimpleMsg(
                    '%s.%s: Value (%g) must be greater than one.', 
                    [TDSSObject(obj).FullName, PropertyName[Index], Value],
                2020031);
            Exit;
        end;
        if (TPropertyFlag.NonZero in flags) and (Value = 0) then
        begin
            if not (TPropertyFlag.IgnoreInvalid in flags) then
                DoSimpleMsg(
                    '%s.%s: Value (%g) cannot be zero.', 
                    [TDSSObject(obj).FullName, PropertyName[Index], Value],
                2020031);

            Exit;
        end;
        if (TPropertyFlag.NonNegative in flags) and (Value < 0) then
        begin
            if not (TPropertyFlag.IgnoreInvalid in flags) then
                DoSimpleMsg(
                    '%s.%s: Value (%g) cannot be negative.', 
                    [TDSSObject(obj).FullName, PropertyName[Index], Value],
                2020032);
                
            Exit;
        end;
        if (TPropertyFlag.NonPositive in flags) and (Value > 0) then
        begin
            if not (TPropertyFlag.IgnoreInvalid in flags) then
                DoSimpleMsg(
                    '%s.%s: Value (%g) cannot be positive.', 
                    [TDSSObject(obj).FullName, PropertyName[Index], Value],
                2020033);
                
            Exit;
        end;
    end;

    Value := Value * scale;

    if (Value = 0) and (PropertyTrapZero[Index] <> 0) then
    begin
        // DoSimpleMsg('Zero Reactance specified for ......' + DSS.ActiveAutoTransObj.Name, 1011201);
        Value := PropertyTrapZero[Index];
    end;

    if (Value <> 0) and (TPropertyFlag.InverseValue in flags) then
        Value := 1.0 / Value;

    if (TPropertyFlag.WriteByFunction in flags) then
        TWriteDoublePropertyFunction(Pointer(PropertyWriteFunction[Index]))(obj, Value)
    else
    case PropertyType[Index] of
        TPropertyType.DoubleProperty:
        begin
            doublePtr := PDouble(PByte(obj) + PropertyOffset[Index]);
            doublePtr^ := Value;
        end;
        TPropertyType.DoubleOnArrayProperty:
        begin
            doublePtr := PPDouble(PByte(obj) + PropertyOffset[Index])^;
            doublePtr[PInteger(PByte(obj) + PropertyOffset2[Index])^ - 1] := Value;
        end;
        TPropertyType.DoubleOnStructArrayProperty:
        begin
            doublePtr := PDouble(
                PPByte(PByte(obj) + PropertyStructArrayOffset)^ + // Pointer to the pointer array
                PropertyOffset[Index] + ptruint(// base field
                    PropertyStructArrayStep * // step size
                    (PInteger(PByte(obj) + PropertyStructArrayIndexOffset)^ - 1) // index
                ) 
            );
            doublePtr^ := Value;
        end;
    end;
end;

procedure TDSSClassHelper.SetObjInteger(ptr: Pointer; Index: Integer; Value: Integer; prevInt: PInteger);
var
    flags: TPropertyFlags;
    integerPtr: PInteger = NIL;
    boolPtr: PLongBool;
    ptype: TPropertyType;
    Obj: TDSSObject;
begin
    Obj := TDSSObject(ptr);
    flags := PropertyFlags[Index];
    ptype := PropertyType[Index];

    if (TPropertyFlag.ConditionalReadOnly in flags) and (PLongBool(PByte(obj) + PropertyOffset3[Index])^) then
        Exit;

    if ptype in [TPropertyType.BooleanProperty, TPropertyType.EnabledProperty, TPropertyType.BooleanActionProperty] then
    begin
        Value := Integer(LongBool(value <> 0));
    end;

    if flags = [] then
    begin
        // Most properties don't have any flags set, just skip the checks
        if ptype = TPropertyType.BooleanProperty then
        begin
            boolPtr := PLongBool(PByte(obj) + PropertyOffset[Index]);
            if prevInt <> NIL then
                prevInt^ := Integer(boolPtr^);
            boolPtr^ := Value <> 0;
            Exit;
        end
        else 
        if ptype in [TPropertyType.IntegerProperty, TPropertyType.MappedIntEnumProperty, TPropertyType.MappedStringEnumProperty] then
        begin
            integerPtr := PInteger(PByte(obj) + PropertyOffset[Index]);
            if prevInt <> NIL then
                prevInt^ := integerPtr^;
            integerPtr^ := Value;
            Exit;
        end;
    end;
    if (TPropertyFlag.IntegerStructIndex in flags) and ((Value < 1) or (Value > PInteger(PByte(obj) + PropertyStructArrayCountOffset)^)) then
    begin
        if not (TPropertyFlag.IgnoreInvalid in flags) then
            DoSimpleMsg(
                '%s.%s: Invalid value (%d).', 
                [TDSSObject(obj).FullName, PropertyName[Index], Value],
            2020031);

        Exit;
    end;
    if (TPropertyFlag.GreaterThanOne in flags) and (Value <= 1) then
    begin
        if not (TPropertyFlag.IgnoreInvalid in flags) then
            DoSimpleMsg(
                '%s.%s: Value (%d) must be greater than one.', 
                [TDSSObject(obj).FullName, PropertyName[Index], Value],
            2020031);

        Exit;
    end;
    if (TPropertyFlag.NonZero in flags) and (Value = 0) then
    begin
        if not (TPropertyFlag.IgnoreInvalid in flags) then
            DoSimpleMsg(
                '%s.%s: Value (%d) cannot be zero.', 
                [TDSSObject(obj).FullName, PropertyName[Index], Value],
            2020031);

        Exit;
    end;
    if (TPropertyFlag.NonNegative in flags) and (Value < 0) then
    begin
        if not (TPropertyFlag.IgnoreInvalid in flags) then
            DoSimpleMsg(
                '%s.%s: Value (%d) cannot be negative.', 
                [TDSSObject(obj).FullName, PropertyName[Index], Value],
            2020032);
            
        Exit;
    end;
    if (TPropertyFlag.NonPositive in flags) and (Value > 0) then
    begin
        if not (TPropertyFlag.IgnoreInvalid in flags) then
            DoSimpleMsg(
                '%s.%s: Value (%d) cannot be positive.', 
                [TDSSObject(obj).FullName, PropertyName[Index], Value],
            2020033);
            
        Exit;
    end;
    if TPropertyFlag.ValueOffset in flags then
        Value := Value + Round(PropertyValueOffset[Index]);

    case ptype of
        TPropertyType.EnabledProperty:
        begin
            TDSSCktElement(obj).Enabled := Value <> 0;
            Exit
        end;
        TPropertyType.BooleanActionProperty:
        begin
            if Value <> 0 then
                TActionProcedure(PropertyOffset[Index])(obj);
            Exit;
        end;
        TPropertyType.IntegerOnStructArrayProperty:
        begin
            integerPtr := PInteger(
                PPByte(PByte(obj) + PropertyStructArrayOffset)^ + // Pointer to the pointer array
                PropertyOffset[Index] + ptruint(// base field
                    PropertyStructArrayStep * // step size
                    (PInteger(PByte(obj) + PropertyStructArrayIndexOffset)^ - 1) // index
                ) 
            );
        end;
        TPropertyType.MappedStringEnumOnStructArrayProperty:
        begin
            integerPtr := PInteger(
                PPByte(PByte(obj) + PropertyStructArrayOffset)^ + // Pointer to the pointer array
                PropertyOffset[Index] + ptruint(// base field
                    PropertyStructArrayStep * // step size
                    (PInteger(PByte(obj) + PropertyStructArrayIndexOffset)^ - 1) // index
                ) 
            );
        end;
        TPropertyType.MappedIntEnumProperty:
        begin
            integerPtr := PInteger(PByte(obj) + PropertyOffset[Index]);
        end;
        TPropertyType.MappedStringEnumProperty:
        begin
            if TPropertyFlag.OnArray in flags then
            begin
                integerPtr := PPInteger(PByte(obj) + PropertyOffset[Index])^;
                Inc(integerPtr, PInteger(PByte(obj) + PropertyStructArrayIndexOffset)^ - 1);
            end
            else
                integerPtr := PInteger(PByte(obj) + PropertyOffset[Index]);
        end;
        TPropertyType.StringEnumActionProperty:
        begin
            TEnumActionProcedure(PropertyOffset[Index])(obj, Value);
            Exit;
        end;
        TPropertyType.BooleanProperty:
        begin
            boolPtr := PLongBool(PByte(obj) + PropertyOffset[Index]);    
            if prevInt <> NIL then
                prevInt^ := Integer(boolPtr^);
            boolPtr^ := Value <> 0;
            Exit;
        end;
        TPropertyType.IntegerProperty:
            if not (TPropertyFlag.WriteByFunction in flags) then
            begin
                integerPtr := PInteger(PByte(obj) + PropertyOffset[Index]);
                if prevInt <> NIL then
                    prevInt^ := integerPtr^;
                integerPtr^ := Value;
                Exit;
            end;
    else
        //TODO: error?
        Exit;
    end;

    if (prevInt <> NIL) and (integerPtr <> NIL) then
        prevInt^ := integerPtr^;
    
    if (TPropertyFlag.WriteByFunction in flags) then
        TWriteIntegerPropertyFunction(Pointer(PropertyWriteFunction[Index]))(obj, Value)
    else
        integerPtr^ := Value;
end;

function TDSSObjectHelper.GetCircuit: TDSSCircuit;
begin
    Result := DSS.ActiveCircuit;
end;

function TDSSObjectHelper.AdjustInputFilePath(const Value: String): String;
begin
    Result := Utilities.AdjustInputFilePath(DSS, Value)
end;

function TDSSObjectHelper.ParsePropertyValue(Index: Integer; Value: String): Boolean;
var
    prevInt: Integer;
    singleEdit: Boolean;
begin
    singleEdit := not (Flg.EditingActive in Flags);
    if singleEdit then
        BeginEdit(True);
    Result := True; // TODO
    ParentClass.ParseObjPropertyValue(self, Index, Value, prevInt);
    if DSS.ErrorNumber = 0 then
    begin
        SetAsNextSeq(Index);
        PropertySideEffects(Index, prevInt);
    end;
    if singleEdit then
        EndEdit(1);
end;

function TDSSObjectHelper.SetDouble(Index: Integer; Value: Double): Boolean;
var
    singleEdit: Boolean;
begin
    singleEdit := not (Flg.EditingActive in Flags);
    if singleEdit then
        BeginEdit(True);
    Result := True; // TODO
    ParentClass.SetObjDouble(self, Index, Value);
    if DSS.ErrorNumber = 0 then
    begin
        SetAsNextSeq(Index);
        PropertySideEffects(Index);
    end;
    if singleEdit then
        EndEdit(1);
end;

function TDSSObjectHelper.SetString(Index: Integer; Value: String): Boolean;
var
    singleEdit: Boolean;
begin
    singleEdit := not (Flg.EditingActive in Flags);
    if singleEdit then
        BeginEdit(True);
    Result := True; // TODO
    ParentClass.SetObjString(self, Index, Value);
    if DSS.ErrorNumber = 0 then
    begin
        SetAsNextSeq(Index);
        PropertySideEffects(Index);
    end;
    if singleEdit then
        EndEdit(1);
end;

function TDSSObjectHelper.SetInteger(Index: Integer; Value: Integer): Boolean;
var
    prevInt: Integer;
    singleEdit: Boolean;
begin
    singleEdit := not (Flg.EditingActive in Flags);
    if singleEdit then
        BeginEdit(True);
    Result := True; // TODO
    ParentClass.SetObjInteger(self, Index, Value, @prevInt);
    SetAsNextSeq(Index);
    PropertySideEffects(Index, prevInt);
    if singleEdit then
        EndEdit(1);
end;

function TDSSObjectHelper.SetObject(Index: Integer; Value: TDSSObject): Boolean;
var
    singleEdit: Boolean;
begin
    singleEdit := not (Flg.EditingActive in Flags);
    if singleEdit then
        BeginEdit(True);
    Result := True; // TODO
    ParentClass.SetObjObject(self, Index, Value);
    if DSS.ErrorNumber = 0 then
    begin
        SetAsNextSeq(Index);
        PropertySideEffects(Index);
    end;
    if singleEdit then
        EndEdit(1);
end;

function TDSSObjectHelper.SetObjects(Index: Integer; Value: ArrayOfDSSObject): Boolean;
begin
    Result := SetObjects(Index, @Value[0], Length(Value));
end;

function TDSSObjectHelper.SetObjects(Index: Integer; Value: TDSSObjectPtr; ValueCount: Integer): Boolean;
var
    singleEdit: Boolean;
begin
    singleEdit := not (Flg.EditingActive in Flags);
    if singleEdit then
        BeginEdit(True);
    Result := True; // TODO
    ParentClass.SetObjObjects(self, Index, Value, ValueCount);
    if DSS.ErrorNumber = 0 then
    begin
        SetAsNextSeq(Index);
        PropertySideEffects(Index);
    end;
    if singleEdit then
        EndEdit(1);
end;

function TDSSObjectHelper.SetIntegers(Index: Integer; Value: ArrayOfInteger): Boolean;
begin
    Result := SetIntegers(Index, @Value[0], Length(Value));
end;

function TDSSObjectHelper.SetIntegers(Index: Integer; Value: PInteger; ValueCount: Integer): Boolean;
var
    singleEdit: Boolean;
begin
    singleEdit := not (Flg.EditingActive in Flags);
    if singleEdit then
        BeginEdit(True);
    Result := True;//TODO
    ParentClass.SetObjIntegers(self, Index, Value, ValueCount);
    if DSS.ErrorNumber = 0 then
    begin
        SetAsNextSeq(Index);
        PropertySideEffects(Index);
    end;
    if singleEdit then
        EndEdit(1);
end;

function TDSSObjectHelper.SetDoubles(Index: Integer; Value: ArrayOfDouble): Boolean;
begin
    Result := SetDoubles(Index, @Value[0], Length(Value));
end;

function TDSSObjectHelper.SetDoubles(Index: Integer; Value: PDouble; ValueCount: Integer): Boolean;
var
    singleEdit: Boolean;
begin
    singleEdit := not (Flg.EditingActive in Flags);
    if singleEdit then
        BeginEdit(True);
    Result := True;//TODO
    ParentClass.SetObjDoubles(self, Index, Value, ValueCount);
    if DSS.ErrorNumber = 0 then
    begin
        SetAsNextSeq(Index);
        PropertySideEffects(Index);
    end;
    if singleEdit then
        EndEdit(1);
end;

function TDSSObjectHelper.SetStrings(Index: Integer; Value: ArrayOfString): Boolean;
var
    ValuePChar: Array of PChar = NIL;
    i: Integer;
var
    singleEdit: Boolean;
begin
    singleEdit := not (Flg.EditingActive in Flags);
    if singleEdit then
        BeginEdit(True);
    Result := True;//TODO
    Setlength(ValuePChar, Length(Value));
    for i := 0 to High(Value) do
        ValuePChar[i] := PChar(Value[i]);

    ParentClass.SetObjStrings(self, Index, @ValuePChar[0], Length(Value));
    if DSS.ErrorNumber = 0 then
    begin
        SetAsNextSeq(Index);
        PropertySideEffects(Index);
    end;
    if singleEdit then
        EndEdit(1);
end;

function TDSSObjectHelper.SetStrings(Index: Integer; Value: PPAnsiChar; ValueCount: Integer): Boolean; overload;
var
    singleEdit: Boolean;
begin
    singleEdit := not (Flg.EditingActive in Flags);
    if singleEdit then
        BeginEdit(True);
    Result := True;//TODO
    ParentClass.SetObjStrings(self, Index, Value, ValueCount);
    if DSS.ErrorNumber = 0 then
    begin
        SetAsNextSeq(Index);
        PropertySideEffects(Index);
    end;
    if singleEdit then
        EndEdit(1);
end;

procedure TDSSObjectHelper.BeginEdit(Activate: Boolean);
begin
    ParentClass.BeginEdit(self, Activate);
end;

procedure TDSSObjectHelper.EndEdit(NumChanges: Integer);
begin
    ParentClass.EndEdit(self, NumChanges);
end;

function TDSSObjectHelper.GetDouble(Index: Integer): Double;
begin
    Result := ParentClass.GetObjDouble(self, Index);
end;

function TDSSObjectHelper.GetInteger(Index: Integer): Integer;
begin
    Result := ParentClass.GetObjInteger(self, Index);
end;

function TDSSObjectHelper.GetString(Index: Integer): String;
begin
    Result := ParentClass.GetObjString(self, Index);
end;

function TDSSObjectHelper.GetObject(Index: Integer): TDSSObject;
begin
    Result := ParentClass.GetObjObject(self, Index);
end;

procedure TDSSObjectHelper.GetDoubles(Index: Integer; var ResultPtr: PDouble; ResultCount: PAPISize);
begin
    ParentClass.GetObjDoubles(self, Index, ResultPtr, ResultCount);
end;

function TDSSObjectHelper.GetComplex(Index: Integer): Complex;
var
    resultPtr: PDouble = NIL;
    resultCount: Array[0..3] of TAPISize;
begin
    resultCount[0] := 0;
    resultCount[1] := 0;
    resultCount[2] := 0;
    resultCount[3] := 0;
    GetDoubles(Index, resultPtr, PAPISize(@resultCount));
    if resultCount[0] <> 2 then
    begin
        Result := NaN;
    end
    else
    begin
        Result := PComplex(resultPtr)^;
    end;
    DSS_Dispose_PDouble(resultPtr);
end;

procedure TDSSObjectHelper.GetIntegers(Index: Integer; var ResultPtr: PInteger; ResultCount: PAPISize);
begin
    ParentClass.GetObjIntegers(self, Index, ResultPtr, ResultCount);
end;

procedure TDSSObjectHelper.GetStrings(Index: Integer; var ResultPtr: PPAnsiChar; ResultCount: PAPISize);
begin
    ParentClass.GetObjStrings(self, Index, ResultPtr, ResultCount);
end;

procedure TDSSObjectHelper.GetObjects(Index: Integer; var ResultPtr: PPointer; ResultCount: PAPISize);
begin
    ParentClass.GetObjObjects(self, Index, ResultPtr, ResultCount);
end;

function TDSSObjectHelper.PrpSpecified(idx: Integer): Boolean; inline;
begin
    Result := PrpSequence[idx] <> 0;
end;

procedure TDSSClassHelper.SetObjIntegers(ptr: Pointer; Index: Integer; Value: PInteger; ValueCount: Integer);
var
    i, maxSize, step: Integer;
    integerPtr, positionPtr, sizePtr: PInteger;
    dataPtr: PPInteger;
    flags: TPropertyFlags;
    Obj: TDSSObject;
begin
    Obj := TDSSObject(ptr);
    flags := PropertyFlags[Index];
    case PropertyType[Index] of
        TPropertyType.IntegerArrayProperty:
        begin
            sizePtr := PInteger(PByte(obj) + PropertyOffset2[Index]);
            dataPtr := PPInteger(PByte(obj) + PropertyOffset[Index]);
            maxSize := sizePtr^;
            if maxSize <> ValueCount then
                Exit; //TODO: ERROR
                
            if dataPtr^ = NIL then
            begin
                // If not initialized, allocate here.
                // Note that this should not be used with dynamic arrays
                ReAllocmem(dataPtr^, Sizeof(Integer) * maxSize);
            end;
            integerPtr := dataPtr^;
            Move(Value^, integerPtr, SizeOf(Integer) * ValueCount);
        end;
        TPropertyType.MappedStringEnumArrayProperty:
        begin
            if (TPropertyFlag.SizeIsFunction in flags) then
                maxSize := TIntegerPropertyFunction(Pointer(PropertyOffset3[Index]))(obj)
            else
                maxSize := PropertyOffset3[Index];

            if maxSize <> ValueCount then
                Exit; //TODO: ERROR

            //TODO: validate -- if not TDSSEnum(Pointer(PropertyOffset2[Index])).OrdinalIsValid(Value^) then Exit;

            integerPtr := PPInteger(PByte(obj) + PropertyOffset[Index])^;
            // TODO: validate before copying
            Move(Value^, integerPtr, SizeOf(Integer) * ValueCount);
        end;
        TPropertyType.MappedStringEnumArrayOnStructArrayProperty:
        begin
            // Number of items
            maxSize := PInteger(PByte(obj) + PropertyStructArrayCountOffset)^;

            if maxSize <> ValueCount then
                Exit; //TODO: ERROR

            // Current position
            positionPtr := PInteger(PByte(obj) + PropertyStructArrayIndexOffset);

            // Pointer to the first of the target fields
            integerPtr := PInteger(PPByte(PByte(obj) + PropertyStructArrayOffset)^ + PropertyOffset[Index]);

            for i := 1 to maxSize do
            begin
                //TODO: validate -- if TDSSEnum(Pointer(PropertyOffset2[Index])).OrdinalIsValid(Value^) then 
                integerPtr^ := Value^;
                // Move to the next position
                integerPtr := PInteger(ptruint(integerPtr) + PropertyStructArrayStep);
                Inc(Value);
            end;

            positionPtr^ := maxSize; // match the effective behavior of the original code
        end;
        TPropertyType.MappedStringEnumProperty, // shortcut
        TPropertyType.IntegerOnStructArrayProperty, // shortcut
        TPropertyType.MappedStringEnumOnStructArrayProperty: // shortcut
        begin
            if (PropertyType[Index] = TPropertyType.MappedStringEnumProperty) and not (TPropertyFlag.OnArray in PropertyFlags[Index]) then
                Exit;

            // Number of items
            maxSize := PInteger(PByte(obj) + PropertyStructArrayCountOffset)^;
            if maxSize <> ValueCount then
                Exit; //TODO: ERROR

            if (PropertyType[Index] = TPropertyType.MappedStringEnumProperty) and (TPropertyFlag.OnArray in PropertyFlags[Index]) then
            begin
                step := SizeOf(Integer);
                integerPtr := PInteger(PPByte(PByte(obj) + PropertyOffset[Index])^);
            end
            else
            begin
                step := PropertyStructArrayStep;
                integerPtr := PInteger(
                    PPByte(PByte(obj) + PropertyStructArrayOffset)^ +
                    PropertyOffset[Index]
                );
            end;

            for i := 1 to maxSize do
            begin
                //TODO: validate -- if TDSSEnum(Pointer(PropertyOffset2[Index])).OrdinalIsValid(Value^) then 
                integerPtr^ := Value^;
                // Move to the next position
                integerPtr := PInteger(ptruint(integerPtr) + step);
                Inc(Value);
            end;
        end;
    end;
end;

procedure TDSSClassHelper.SetObjDoubles(ptr: Pointer; Index: Integer; Value: PDouble; ValueCount: Integer);
var
    i, j, maxSize, Norder, intVal, step: Integer;
    positionPtr, sizePtr: PInteger;
    scale: Double;
    doublePtr: PDouble;
    dataPtr: PPDouble = NIL;
    complexPtr: PComplex;
    ptype: TPropertyType;
    flags: TPropertyFlags;
    doubleVals: Array of Double = NIL;
    mat: TCMatrix;
    Obj: TDSSObject;
begin
    Obj := TDSSObject(ptr);
    ptype := PropertyType[Index];
    flags := PropertyFlags[Index];
    case ptype of
        TPropertyType.ComplexProperty:
            if ValueCount = 2 then
            begin
                complexPtr := PComplex(PByte(obj) + PropertyOffset[Index]);
                complexPtr^ := PComplex(Value)^;
            end
            else
            begin
                DoSimpleMsg(
                    '%s.%s: Invalid number of elements. Provide a complex number (pair of two float64 values).', 
                    [TDSSObject(obj).FullName, PropertyName[Index]],
                2020040);
                Exit;
            end;
        TPropertyType.ComplexPartsProperty:
            if ValueCount = 2 then
            begin
                doublePtr := PDouble(PByte(obj) + PropertyOffset[Index]);
                doublePtr^ := Value^;
                doublePtr := PDouble(PByte(obj) + PropertyOffset2[Index]);
                Inc(Value);
                doublePtr^ := Value^;
            end
            else
            begin
                DoSimpleMsg(
                    '%s.%s: Invalid number of elements. Provide a complex number (pair of two float64 values).', 
                    [TDSSObject(obj).FullName, PropertyName[Index]],
                2020039);
                Exit;
            end;
        TPropertyType.DoubleSymMatrixProperty:
        begin
            scale := PropertyScale[Index];
            Norder := PInteger(PByte(obj) + PropertyOffset3[Index])^; // e.g. Fnphases
            dataPtr := PPDouble(PByte(obj) + PropertyOffset[Index]);
            
            // Allow both the full matrix or the triangle
            if (ValueCount <> (Norder * Norder)) and (ValueCount <> (Norder * (Norder + 1)) div 2) then
            begin
                DoSimpleMsg(
                    '%s.%s: Invalid number of elements. Provide either the full matrix or the lower triangle.', 
                    [TDSSObject(obj).FullName, PropertyName[Index]],
                2020036);
                Exit;
            end;

            doublePtr := PPDouble(dataPtr)^;
            if doublePtr = NIL then
            begin
                ReAllocMem(dataPtr^, 0);
                doublePtr := Allocmem(Sizeof(Double) * Norder * Norder);
                dataPtr^ := doublePtr;
            end;

            if ValueCount = (Norder * Norder) then
                Move(Value^, doublePtr^, ValueCount * SizeOf(Double))
            else
            begin
                for i := 0 to Norder - 1 do
                    for j := 0 to i do
                    begin
                        doublePtr[i * Norder + j] := Value^;
                        if i <> j then
                            doublePtr[j * Norder + i] := Value^;

                        Inc(Value);
                    end;
            end;
            if scale <> 1 then
            begin
                for i := 0 to Norder - 1 do
                    for j := 0 to Norder - 1 do
                        doublePtr[i * Norder + j] *= scale;
            end;
        end;
        TPropertyType.ComplexPartSymMatrixProperty:
        begin
            if TPropertyFlag.ScaledByFunction in flags then
                scale := TPropertyScaleFunction(Pointer(PropertyOffset2[Index]))(obj, False) // False = Setter scale
            else
                scale := PropertyScale[Index];

            mat := PCMatrix(Pointer(PByte(obj) + PropertyOffset[Index]))^;
            doublePtr := PDouble(mat.GetValuesArrayPtr(Norder));
            if TPropertyFlag.ImagPart in flags then
                Inc(doublePtr);

            // Allow both the full matrix or the triangle
            if (ValueCount <> (Norder * Norder)) and (ValueCount <> (Norder * (Norder + 1)) div 2) then
            begin
                DoSimpleMsg(
                    '%s.%s: Invalid number of elements. Provide either the full matrix or the lower triangle.', 
                    [TDSSObject(obj).FullName, PropertyName[Index], Value],
                2020037);
                Exit;
            end;

            if ValueCount = (Norder * Norder) then
            begin
                // Full matrix, but still need to copy elements one by one. 
                for i := 0 to Norder - 1 do
                    for j := 0 to Norder - 1 do
                    begin
                        doublePtr[(i * Norder + j) * 2] := Value^;
                        Inc(Value);
                    end;
            end
            else
            begin
                for i := 0 to Norder - 1 do
                    for j := 0 to i do
                    begin
                        doublePtr[(i * Norder + j) * 2] := Value^;
                        if i <> j then
                            doublePtr[(j * Norder + i) * 2] := Value^;
                        Inc(Value);
                    end;
            end;
            if scale <> 1 then
            begin
                for i := 0 to Norder - 1 do
                    for j := 0 to Norder - 1 do
                        doublePtr[(i * Norder + j) * 2] *= scale;
            end;
        end;    
        TPropertyType.DoubleArrayProperty,
        TPropertyType.DoubleDArrayProperty,
        TPropertyType.DoubleFArrayProperty,
        TPropertyType.DoubleVArrayProperty:
        begin
            scale := PropertyScale[Index];

            sizePtr := NIL;
            if TPropertyFlag.SizeIsFunction in flags then
            begin
                intVal := TIntegerPropertyFunction(Pointer(PropertyOffset3[Index]))(obj);
                sizePtr := @intVal;
            end
            else
            if (ptype <> TPropertyType.DoubleFArrayProperty) then
                sizePtr := PInteger(PByte(obj) + PropertyOffset2[Index]); // Size pointer

            if (TPropertyFlag.AllowNone in flags) and (ValueCount = 0) then
            begin
                if sizePtr <> NIL then
                    sizePtr^ := 0;
                Exit;
            end;

            if TPropertyFlag.WriteByFunction in flags then
            begin
                doublePtr := Value;
            end
            else
            begin
                dataPtr := PPDouble(PByte(obj) + PropertyOffset[Index]);
                doublePtr := dataPtr^;
            end;

            if ((ptype = TPropertyType.DoubleArrayProperty) or (ptype = TPropertyType.DoubleVArrayProperty)) 
                and (doublePtr = NIL) then
            begin
                // If not initialized, allocate here.
                // Note that this should not be used with dynamic arrays
                ReAllocmem(dataPtr^, Sizeof(Double) * sizePtr^);
                doublePtr := dataPtr^;
            end;

            case ptype of
                TPropertyType.DoubleArrayProperty,
                TPropertyType.DoubleDArrayProperty,
                TPropertyType.DoubleVArrayProperty:
                begin
                    if TPropertyFlag.ArrayMaxSize in flags then
                        maxSize := PropertyOffset3[Index]
                    else
                        maxSize := sizePtr^;

                    if maxSize <> ValueCount then
                    begin
                        DoSimpleMsg(
                            '%s.%s: Invalid number of elements. Expected %d elements, got %d.', 
                            [TDSSObject(obj).FullName, PropertyName[Index], maxSize, ValueCount],
                        2020042);
                        Exit;
                    end;
                    if not (TPropertyFlag.WriteByFunction in flags) then
                        Move(Value^, doublePtr^, maxSize * SizeOf(Double));
                end;
                TPropertyType.DoubleFArrayProperty:
                begin
                    maxSize := PropertyOffset2[Index];
                    if maxSize <> ValueCount then
                    begin
                        DoSimpleMsg(
                            '%s.%s: Invalid number of elements. Expected %d elements, got %d.', 
                            [TDSSObject(obj).FullName, PropertyName[Index], maxSize, ValueCount],
                        2020042);
                        Exit;
                    end;
                    if not (TPropertyFlag.WriteByFunction in flags) then
                        Move(Value^, doublePtr^, maxSize * SizeOf(Double));
                end;
            end;

            if (scale <> 1) and not (TPropertyFlag.WriteByFunction in flags) then
            begin
                for i := 1 to sizePtr^ do
                begin
                    doublePtr^ := doublePtr^ * scale;
                    Inc(doublePtr);
                end;
            end;

            if TPropertyFlag.WriteByFunction in flags then
            begin
                SetLength(doubleVals, sizePtr^);
                Move(Value^, doubleVals[0], sizePtr^ * SizeOf(Double));
                TWriteDoublesPropertyFunction(Pointer(PropertyWriteFunction[Index]))(obj, @doubleVals[0], Length(doubleVals))
            end
        end;
        TPropertyType.DoubleOnStructArrayProperty, // shortcut
        TPropertyType.DoubleOnArrayProperty: // shortcut
        begin
            // Number of items
            maxSize := PInteger(PByte(obj) + PropertyStructArrayCountOffset)^;
            if maxSize <> ValueCount then
            begin
                DoSimpleMsg(
                    '%s.%s: Invalid number of elements. Expected %d elements, got %d.', 
                    [TDSSObject(obj).FullName, PropertyName[Index], maxSize, ValueCount],
                2020038);
                Exit;
            end;
            if PropertyType[Index] = TPropertyType.DoubleOnStructArrayProperty then
            begin
                step := PropertyStructArrayStep;
                doublePtr := PDouble(
                    PPByte(PByte(obj) + PropertyStructArrayOffset)^ + // Pointer to the pointer struct array
                    PropertyOffset[Index] // base field
                );
            end
            else
            begin
                step := SizeOf(Double);
                doublePtr := PDouble(
                    PPByte(PByte(obj) + PropertyOffset[Index])^ // Pointer to the pointer array
                );
            end;

            scale := PropertyScale[Index];
            if scale <> 1 then
            begin
                for i := 1 to maxSize do
                begin
                    doublePtr^ := Value^ * scale;
                    doublePtr := PDouble(PByte(doublePtr) + step);
                    Inc(Value);
                end;
            end
            else
            begin
                for i := 1 to maxSize do
                begin
                    doublePtr^ := Value^;
                    doublePtr := PDouble(PByte(doublePtr) + step);
                    Inc(Value);
                end;
            end;
        end;
        TPropertyType.DoubleArrayOnStructArrayProperty:
        begin
            // Number of items
            maxSize := PInteger(PByte(obj) + PropertyOffset2[Index])^;

            if maxSize <> ValueCount then
            begin
                DoSimpleMsg(
                    '%s.%s: Invalid number of elements. Expected %d elements, got %d.', 
                    [TDSSObject(obj).FullName, PropertyName[Index], maxSize, ValueCount],
                2020041);
                Exit;
            end;

            // Current position
            positionPtr := PInteger(PByte(obj) + PropertyStructArrayIndexOffset);

            // Pointer to the first of the target fields
            doublePtr := PDouble(
                PPByte(PByte(obj) + PropertyStructArrayOffset)^ + PropertyOffset[Index]
            );

            scale := PropertyScale[Index];
            
            // Loop for no more than the expected number of items;  Ignore omitted values
            for i := 1 to maxSize do
            begin
                doublePtr^ := Value^ * scale;
                // Move to the next position
                doublePtr := PDouble(ptruint(doublePtr) + PropertyStructArrayStep);
                Inc(Value);
            end;
            positionPtr^ := maxSize; // match the effective behavior of the original code
        end;
    end;
end;

procedure TDSSClassHelper.SetObjStrings(ptr: Pointer; Index: Integer; Value: PPAnsiChar; ValueCount: Integer);
var
    i, maxSize, step, intVal: Integer;
    positionPtr, integerPtr: PInteger;
    flags: TPropertyFlags;
    stringListPtr: PStringList;
    stringList: TStringList;
    stringPtr: PString;
    Obj: TDSSObject;
    cls: TDSSClass;
    ElemName: String;
    objs: Array of TDSSObject = NIL;
    otherObj: TDSSObject;
    otherObjPtr: TDSSObjectPtr;
begin
    Obj := TDSSObject(ptr);
    flags := PropertyFlags[Index];
    case PropertyType[Index] of 
        TPropertyType.DSSObjectReferenceArrayProperty:
        begin
            // Class of the objects
            cls := Pointer(PropertyOffset2[Index]);

            if TPropertyFlag.WriteByFunction in flags then
            begin
                SetLength(objs, ValueCount);
                if cls <> NIL then
                begin
                    for i := 1 to ValueCount do
                    begin
                        ElemName := Value^;
                        // if TPropertyFlag.CheckForVar in flags then
                        //     PropParser.CheckforVar(ElemName);

                        otherObj := cls.Find(ElemName, False);
                        if otherObj = NIL then
                        begin
                            DoSimpleMsg(
                                Format('%s.%s: %s object "%s" not found.',
                                    [TDSSObject(obj).FullName, PropertyName[Index], cls.Name, ElemName]
                                ), 403);
                            Exit;
                        end;
                        objs[i - 1] := otherObj;
                        Inc(Value);
                    end;
                end
                else
                begin
                    for i := 1 to ValueCount do
                    begin
                        ElemName := constructElemName(DSS, AnsiLowerCase(Value^));
                        intVal := GetCktElementIndex(DSS, ElemName);
                        otherObj := NIL;
                        if intVal > 0 then
                            otherObj := DSS.ActiveCircuit.CktElements.Get(intVal);

                        if otherObj = NIL then
                        begin
                            DoSimpleMsg(
                                Format('%s.%s: object "%s" not found.',
                                    [TDSSObject(obj).FullName, PropertyName[Index], ElemName]
                                ), 403);
                            Exit;
                        end;
                        objs[i - 1] := otherObj;
                        Inc(Value);
                    end;
                end;
                TWriteObjRefsPropertyFunction(Pointer(PropertyWriteFunction[Index]))(obj, PPointer(@objs[0]), Length(objs));
                // Result := True;
                Exit;
            end;

            // Number of items
            intVal := PInteger(PByte(obj) + PropertyStructArrayCountOffset)^;

            if intVal < 1 then
            begin
                DoSimpleMsg(
                    Format('%s.%s: No objects are expected! Check if the order of property assignments is correct.',
                        [TDSSObject(obj).FullName, PropertyName[Index]]
                    ), 402);
                Exit;
            end;
            if intVal <> ValueCount then
            begin
                DoSimpleMsg(
                    Format('%s.%s: Number of elements expected (%d) does not match the number of provided elements (%d).',
                        [TDSSObject(obj).FullName, PropertyName[Index], intVal, ValueCount]
                    ), 406);
                Exit;
            end;

            // Current position
            positionPtr := NIL;
            if (PropertyStructArrayIndexOffset2 <> 0) or (PropertyStructArrayIndexOffset <> 0)  then
            begin
                if TPropertyFlag.AltIndex in flags then
                    positionPtr := PInteger(PByte(obj) + PropertyStructArrayIndexOffset2)
                else
                    positionPtr := PInteger(PByte(obj) + PropertyStructArrayIndexOffset);
            end;

            // Start of array
            otherObjPtr := TDSSObjectPtrPtr((PtrUint(obj) + PtrUint(PropertyOffset[Index])))^;

            // TODO: if cls = NIL,..
            i := 0;
            for i := 1 to intVal do
            begin
                otherObj := cls.Find(Value^, False);
                if otherObj = NIL then
                begin
                    DoSimpleMsg(
                        Format('%s.%s: %s object "%s" not found.',
                            [TDSSObject(obj).FullName, PropertyName[Index], cls.Name, Value^]
                        ), 403);
                    Exit;
                end
                else
                    otherObjPtr^ := otherObj;

                Inc(Value);
            end;

            if positionPtr <> NIL then
                positionPtr^ := i;

            // Result := True;
        end;
        TPropertyType.StringListProperty:
        begin
            if (TPropertyFlag.WriteByFunction in flags) then
                stringList := TStringList.Create()
            else
            begin
                stringListPtr := PStringList(PByte(obj) + PropertyOffset[Index]);
                stringList := stringListPtr^;
            end;

            stringList.Clear();
            if TPropertyFlag.Transform_LowerCase in flags then
            begin
                for i := 1 to ValueCount do
                begin
                    stringList.Add(AnsiLowerCase(Value^));
                    Inc(Value);
                end;
            end
            else
            begin
                for i := 1 to ValueCount do
                begin
                    stringList.Add(Value^);
                    Inc(Value);
                end;
            end;

            if (TPropertyFlag.WriteByFunction in flags) then
                TWriteStringListPropertyFunction(Pointer(PropertyWriteFunction[Index]))(obj, stringList)                
        end;
        TPropertyType.BusesOnStructArrayProperty,
        TPropertyType.BusOnStructArrayProperty: // allow this one as a shortcut
        begin
            // Number of items
            maxSize := PInteger(PByte(obj) + PropertyStructArrayCountOffset)^;
            // Current position
            positionPtr := PInteger(PByte(obj) + PropertyStructArrayIndexOffset);

            if ValueCount <> maxSize then
                Exit; //TODO: error

            // Loop for no more than the expected number of items;  Ignore omitted values
            for i := 1 to maxSize do
            begin
                if Length(Value^) > 0 then
                    TDSSCktElement(obj).SetBus(i, Value^);
                Inc(Value);
            end;
            positionPtr^ := maxSize; // match the effective behavior of the original code
        end;
        TPropertyType.MappedStringEnumArrayProperty:
        begin
            if (TPropertyFlag.SizeIsFunction in flags) then
                maxSize := TIntegerPropertyFunction(Pointer(PropertyOffset3[Index]))(obj)
            else
                maxSize := PropertyOffset3[Index];

            integerPtr := PPInteger(PByte(obj) + PropertyOffset[Index])^;

            if ValueCount <> maxSize then
                Exit; //TODO: error

            for i := 1 to maxSize do
            begin
                if Length(Value^) > 0 then
                    integerPtr^ := TDSSEnum(Pointer(PropertyOffset2[Index])).StringToOrdinal(Value^);
                Inc(Value);
                Inc(integerPtr);
            end;
        end;
        TPropertyType.MappedStringEnumProperty, // shortcut
        TPropertyType.MappedStringEnumOnStructArrayProperty, // shortcut
        TPropertyType.MappedStringEnumArrayOnStructArrayProperty:
        begin
            if (PropertyType[Index] = TPropertyType.MappedStringEnumProperty) and not (TPropertyFlag.OnArray in PropertyFlags[Index]) then
                Exit;

            // Number of items
            maxSize := PInteger(PByte(obj) + PropertyStructArrayCountOffset)^;
            if maxSize <> ValueCount then
                Exit; //TODO: error

            // Current position
            positionPtr := PInteger(PByte(obj) + PropertyStructArrayIndexOffset);

            // Pointer to the first of the target fields
            if (PropertyType[Index] = TPropertyType.MappedStringEnumProperty) and (TPropertyFlag.OnArray in PropertyFlags[Index]) then
            begin
                step := SizeOf(Integer);
                integerPtr := PInteger(PPByte(PByte(obj) + PropertyOffset[Index])^);
            end
            else
            begin
                step := PropertyStructArrayStep;
                integerPtr := PInteger(
                    PPByte(PByte(obj) + PropertyStructArrayOffset)^ +
                    PropertyOffset[Index]
                );
            end;

            for i := 1 to maxSize do
            begin
                if Length(Value^) > 0 then
                    integerPtr^ := TDSSEnum(Pointer(PropertyOffset2[Index])).StringToOrdinal(Value^);

                // Move to the next position
                Inc(Value);
                integerPtr := PInteger(ptruint(integerPtr) + step);
            end;
            if PropertyType[Index] = TPropertyType.MappedStringEnumArrayOnStructArrayProperty then
                positionPtr^ := maxSize; // match the effective behavior of the original code
        end;
    end;
end;

function TDSSClassHelper.GetObjDouble(Obj: Pointer; Index: Integer): Double;
var
    scale: Double;
begin
    Result := NaN;
    if not ((Index > 0) and (Index <= NumProperties) and (PropertyOffset[Index] <> -1)) then
        Exit;

    case PropertyType[Index] of
        TPropertyType.DoubleProperty:
        begin
            if TPropertyFlag.ConditionalValue in PropertyFlags[index] then
                if not PLongBool(PtrUint(obj) + PropertyOffset3[Index])^ then
                begin
                    Result := NaN;
                    Exit;
                end;

            scale := PropertyScale[Index];

            if TPropertyFlag.ReadByFunction in PropertyFlags[index] then
                Result := TDoublePropertyFunction(Pointer(PropertyReadFunction[Index]))(obj)
            else
                Result := PDouble(PByte(obj) + PropertyOffset[Index])^;

            if TPropertyFlag.ScaledByFunction in PropertyFlags[index] then
                scale := TPropertyScaleFunction(Pointer(PropertyOffset2[Index]))(obj, True); // True = Getter scale

            if TPropertyFlag.InverseValue in PropertyFlags[index] then
                Result := 1 / (Result / scale)
            else
                Result := Result / scale;
        end;
        TPropertyType.DoubleOnArrayProperty:
            Result := (
                (PPDouble(PByte(obj) + PropertyOffset[Index])^) // base array
                    [PInteger(PByte(obj) + PropertyOffset2[Index])^ - 1] // element index
            );
        TPropertyType.DoubleOnStructArrayProperty:
            if PropertyScale[Index] <> 1 then
                Result := (PDouble(
                    PPByte(PByte(obj) + PropertyStructArrayOffset)^ + // Pointer to the pointer array
                    PropertyOffset[Index] + ptruint(// base field
                        PropertyStructArrayStep * // step size
                        (PInteger(PByte(obj) + PropertyStructArrayIndexOffset)^ - 1) // index
                    ) 
                )^ / PropertyScale[Index])
            else
                Result := PDouble(
                    PPByte(PByte(obj) + PropertyStructArrayOffset)^ + // Pointer to the pointer array
                    PropertyOffset[Index] + ptruint(// base field
                        PropertyStructArrayStep * // step size
                        (PInteger(PByte(obj) + PropertyStructArrayIndexOffset)^ - 1) // index
                    ) 
                )^;
    end;
end;

function TDSSClassHelper.GetObjString(Obj: Pointer; Index: Integer): String;
var
    integerPtr: PInteger;
    otherObj: TDSSObject;
begin
    Result := '';
    if not ((Index > 0) and (Index <= NumProperties) and (PropertyOffset[Index] <> -1)) then
        Exit;

    case PropertyType[Index] of
        TPropertyType.BusProperty:
            Result := TDSSCktElement(obj).GetBus(PropertyOffset[Index]);
        TPropertyType.BusOnStructArrayProperty:
            Result := TDSSCktElement(obj).GetBus(PInteger(PByte(obj) + PropertyStructArrayIndexOffset)^);
        TPropertyType.StringSilentROFunctionProperty:
            Result := TStringPropertyFunction(Pointer(PropertyOffset[Index]))(obj);
        TPropertyType.MappedStringEnumOnStructArrayProperty:
            Result := TDSSEnum(Pointer(PropertyOffset2[Index])).OrdinalToString(PInteger(
                PPByte(PByte(obj) + PropertyStructArrayOffset)^ + // Pointer to the pointer array
                PropertyOffset[Index] + ptruint(// base field
                    PropertyStructArrayStep * // step size
                    (PInteger(PByte(obj) + PropertyStructArrayIndexOffset)^ - 1) // index
                )
            )^);
        TPropertyType.StringProperty:
            if TPropertyFlag.ReadByFunction in PropertyFlags[index] then
                Result := TStringPropertyFunction(Pointer(PropertyReadFunction[Index]))(obj)
            else
                Result := PString(PByte(obj) + PropertyOffset[Index])^;
        TPropertyType.MakeLikeProperty:
            Result := '';//TODO? or should leave empty?
        TPropertyType.MappedStringEnumProperty:
        begin
            if TPropertyFlag.OnArray in PropertyFlags[Index] then
            begin
                integerPtr := PPInteger(PByte(obj) + PropertyOffset[Index])^;
                Inc(integerPtr, PInteger(PByte(obj) + PropertyStructArrayIndexOffset)^ - 1);
            end
            else
                integerPtr := PInteger(PByte(obj) + PropertyOffset[Index]);

            Result := TDSSEnum(Pointer(PropertyOffset2[Index])).OrdinalToString(integerPtr^);
        end;
        TPropertyType.StringEnumActionProperty:
            Result := '';

        TPropertyType.DSSObjectReferenceProperty:
        begin
            otherObj := GetObjObject(Obj, Index);
            if otherObj <> NIL then
                Result := otherObj.Name
            else
                Result := '';
        end;
    end;
end;

function TDSSClassHelper.GetObjInteger(Obj: Pointer; Index: Integer): Integer;
begin
    Result := -1;
    if not ((Index > 0) and (Index <= NumProperties) and (PropertyOffset[Index] <> -1)) then
        Exit;

    case PropertyType[Index] of
        TPropertyType.IntegerOnStructArrayProperty:
            Result := PInteger(
                PPByte(PByte(obj) + PropertyStructArrayOffset)^ + // Pointer to the pointer array
                PropertyOffset[Index] + ptruint(// base field
                    PropertyStructArrayStep * // step size
                    (PInteger(PByte(obj) + PropertyStructArrayIndexOffset)^ - 1) // index
                ) 
            )^;

        TPropertyType.MappedStringEnumOnStructArrayProperty:
            Result := PInteger(
                PPByte(PByte(obj) + PropertyStructArrayOffset)^ + // Pointer to the pointer array
                PropertyOffset[Index] + ptruint(// base field
                    PropertyStructArrayStep * // step size
                    (PInteger(PByte(obj) + PropertyStructArrayIndexOffset)^ - 1) // index
                )
            )^;


        TPropertyType.MappedIntEnumProperty,
        TPropertyType.IntegerProperty,
        TPropertyType.MappedStringEnumProperty:
        begin
            if (TPropertyFlag.ReadByFunction in PropertyFlags[Index]) then
                Result := TIntegerPropertyFunction(Pointer(PropertyReadFunction[Index]))(obj)
            else
                Result := PInteger(PByte(obj) + PropertyOffset[Index])^;

            Result := Result - Round(PropertyValueOffset[Index]);
        end;

        TPropertyType.EnabledProperty:
            Result := Integer(LongBool(TDSSCktElement(obj).Enabled));

        TPropertyType.BooleanProperty:
            Result := Integer(PLongBool(PByte(obj) + PropertyOffset[Index])^);


        TPropertyType.BooleanActionProperty:
            Result := 0;
    end;
end;

function TDSSClassHelper.GetObjObject(Obj: Pointer; Index: Integer): TDSSObject;
var
    otherObjPtr: TDSSObjectPtr;
    posPtr: PInteger;
begin
    Result := NIL;
    if not ((Index > 0) and (Index <= NumProperties) and (PropertyOffset[Index] <> -1)) then
        Exit;

    case PropertyType[Index] of
        TPropertyType.DSSObjectReferenceProperty:
            if not (TPropertyFlag.OnArray in PropertyFlags[Index]) then
                Result := TDSSObjectPtr((PtrUint(obj) + PtrUint(PropertyOffset[Index])))^
            else // if TPropertyFlag.OnArray in flags then
            begin
                otherObjPtr := TDSSObjectPtr(PPByte(PtrUint(obj) + PropertyOffset[Index])^); // start of array
                if otherObjPtr = NIL then
                    Exit;
                posPtr := PInteger(PtrUint(obj) + PropertyStructArrayIndexOffset);
                inc(otherObjPtr, posPtr^ - 1);
                Result := otherObjPtr^;
            end;
    end;
end;

procedure TDSSClassHelper.GetObjDoubles(Obj: Pointer; Index: Integer; var ResultPtr: PDouble; ResultCount: PAPISize); //TODO: check for missing array sizes, especially when ReadByFunction
var
    c: PComplex;
    i, j, count, dim1, dim2, step: Integer;
    doublePtr, outPtr: PDouble;
    mat: TCMatrix;
    scale: Double;
    Result: PDoubleArray0;
begin
    if not ((Index > 0) and (Index <= NumProperties) and (PropertyOffset[Index] <> -1)) then
    begin
        Result := DSS_RecreateArray_PDouble(ResultPtr, ResultCount, 0);
        Exit;
    end;

    case PropertyType[Index] of
        TPropertyType.ComplexProperty:
        begin
            Result := DSS_RecreateArray_PDouble(ResultPtr, ResultCount, 2);
            c := PComplex(PByte(obj) + PropertyOffset[Index]);
            Result[0] := c.re;
            Result[1] := c.im;
        end;
        TPropertyType.ComplexPartsProperty:
        begin
            Result := DSS_RecreateArray_PDouble(ResultPtr, ResultCount, 2);
            Result[0] := PDouble(PByte(obj) + PropertyOffset[Index])^;
            Result[1] := PDouble(PByte(obj) + PropertyOffset2[Index])^;
        end;
        TPropertyType.DoubleArrayProperty,
        TPropertyType.DoubleDArrayProperty,
        TPropertyType.DoubleVArrayProperty,
        TPropertyType.DoubleFArrayProperty,
        TPropertyType.DoubleSymMatrixProperty:
        begin
            doublePtr := NIL;
            if PropertyType[Index] = TPropertyType.DoubleFArrayProperty then
            begin
                doublePtr := PDouble(PByte(obj) + PropertyOffset[Index]);
                count := PropertyOffset2[Index]
            end
            else if TPropertyFlag.SizeIsFunction in PropertyFlags[Index] then
                count := TIntegerPropertyFunction(Pointer(PropertyOffset3[Index]))(obj) //TODO: check if we can use a simple flag for this
            else
            if PropertyType[Index] = TPropertyType.DoubleSymMatrixProperty then
                count := PInteger(PByte(obj) + PropertyOffset3[Index])^
            else
                count := PInteger(PByte(obj) + PropertyOffset2[Index])^;

            dim1 := 0;
            dim2 := 0;

            if TPropertyFlag.ReadByFunction in PropertyFlags[Index] then
            begin
                TDoublesPropertyFunction(Pointer(PropertyReadFunction[Index]))(obj, ResultPtr, ResultCount);
                Result := PDoubleArray0(ResultPtr);
                if PropertyScale[Index] <> 1 then
                    for i := 0 to ResultCount^ do
                        Result[i - 1] := Result[i - 1] / PropertyScale[Index];
                Exit;
            end;
            
            if doublePtr = NIL then
                doublePtr := PPDouble(PByte(obj) + PropertyOffset[Index])^;

            if PropertyType[Index] = TPropertyType.DoubleSymMatrixProperty then
            begin
                dim1 := count;
                dim2 := count;
                count := count * count;
            end;

            if (count <= 0) or (doublePtr = NIL) then
                Exit;

            Result := DSS_RecreateArray_PDouble(ResultPtr, ResultCount, count, dim1, dim2);
            if PropertyScale[Index] <> 1 then
            begin
                for i := 1 to count do
                begin
                    Result[i - 1] := doublePtr^ / PropertyScale[Index];
                    Inc(doublePtr);
                end;
            end
            else
                Move(doublePtr^, Result[0], count * SizeOf(Double));
        end;
        TPropertyType.ComplexPartSymMatrixProperty:
        begin
            if TPropertyFlag.ScaledByFunction in PropertyFlags[Index] then
                scale := TPropertyScaleFunction(Pointer(PropertyOffset2[Index]))(obj, True) // True = Getter scale
            else
                scale := PropertyScale[Index];

            mat := PCMatrix(Pointer(PByte(obj) + PropertyOffset[Index]))^;
            if mat = NIL then
                Exit;

            Result := DSS_RecreateArray_PDouble(ResultPtr, ResultCount, mat.Order * mat.Order, mat.Order, mat.Order);
            outPtr := @Result[0];

            if TPropertyFlag.ImagPart in PropertyFlags[Index] then
                for i := 1 to mat.Order do
                begin
                    for j := 1 to mat.Order do
                    begin
                        outPtr^ := mat[i, j].im / scale;
                        Inc(outPtr);
                    end;
                end
            else
                for i := 1 to mat.Order do
                begin
                    for j := 1 to mat.Order do
                    begin
                        outPtr^ := mat[i, j].re / scale;
                        Inc(outPtr);
                    end;
                end;
        end;
        TPropertyType.DoubleOnStructArrayProperty, // shortcut
        TPropertyType.DoubleOnArrayProperty: // shortcut
        begin
            // Number of items
            count := PInteger(PByte(obj) + PropertyStructArrayCountOffset)^;
            if count <= 0 then
                Exit;

            Result := DSS_RecreateArray_PDouble(ResultPtr, ResultCount, count);
            scale := PropertyScale[Index];
            if PropertyType[Index] = TPropertyType.DoubleOnStructArrayProperty then
            begin
                step := PropertyStructArrayStep;
                doublePtr := PDouble(
                    PPByte(PByte(obj) + PropertyStructArrayOffset)^ + // Pointer to the pointer struct array
                    PropertyOffset[Index] // base field
                );
            end
            else
            begin
                step := SizeOf(Double);
                doublePtr := PDouble(
                    PPByte(PByte(obj) + PropertyOffset[Index])^ // Pointer to the pointer array
                );
            end;

            if scale <> 1 then
            begin
                for i := 1 to count do
                begin
                    Result[i - 1] := doublePtr^ / scale;
                    doublePtr := PDouble(ptruint(doublePtr) + step);
                end
            end
            else
            begin
                for i := 1 to count do
                begin
                    Result[i - 1] := doublePtr^;
                    doublePtr := PDouble(ptruint(doublePtr) + step);
                end
            end;
        end;
        TPropertyType.DoubleArrayOnStructArrayProperty:
        begin
            // Number of items
            count := PInteger(PByte(obj) + PropertyOffset2[Index])^;
            if count <= 0 then
                Exit;

            // Pointer to the first of the target fields
            doublePtr := PDouble(
                PPByte(PByte(obj) + PropertyStructArrayOffset)^ +
                PropertyOffset[Index]
            );
            scale := PropertyScale[Index];
            Result := DSS_RecreateArray_PDouble(ResultPtr, ResultCount, count);
            for i := 1 to count do
            begin
                Result[i - 1] := doublePtr^ / scale;
                doublePtr := PDouble(ptruint(doublePtr) + PropertyStructArrayStep);
            end;
        end;
    end;
end;

procedure TDSSClassHelper.GetObjIntegers(Obj: Pointer; Index: Integer; var ResultPtr: PInteger; ResultCount: PAPISize);
var
    integerPtr: PInteger;
    i, count, step: Integer;
    Result: PIntegerArray0;
begin
    Result := NIL;
    if not ((Index > 0) and (Index <= NumProperties) and (PropertyOffset[Index] <> -1)) then
    begin
        Result := DSS_RecreateArray_PInteger(ResultPtr, ResultCount, 0);
        Exit;
    end;

    case PropertyType[Index] of
        TPropertyType.IntegerArrayProperty:
        begin
            count := PInteger(PByte(obj) + PropertyOffset2[Index])^;
            Result := DSS_RecreateArray_PInteger(ResultPtr, ResultCount, count);
            Move((PPInteger(PByte(obj) + PropertyOffset[Index])^)^, Result[0], count * SizeOf(Integer));
        end;
        TPropertyType.MappedStringEnumArrayProperty:
        begin
            if (TPropertyFlag.SizeIsFunction in PropertyFlags[Index]) then
                count := TIntegerPropertyFunction(Pointer(PropertyOffset3[Index]))(obj)
            else
                count := PropertyOffset3[Index];

            if count <= 0 then
                Exit;

            integerPtr := PPInteger(PByte(obj) + PropertyOffset[Index])^;
            Result := DSS_RecreateArray_PInteger(ResultPtr, ResultCount, count);
            Move(integerPtr^, Result[0], count * SizeOf(Integer));
        end;
        TPropertyType.MappedStringEnumArrayOnStructArrayProperty:
        begin
            // Number of items
            count := PInteger(PByte(obj) + PropertyStructArrayCountOffset)^;
            if count <= 0 then
                Exit;
            // Pointer to the first of the target fields
            integerPtr := PInteger(
                PPByte(PByte(obj) + PropertyStructArrayOffset)^ +
                PropertyOffset[Index]
            );
            Result := DSS_RecreateArray_PInteger(ResultPtr, ResultCount, count);
            for i := 1 to count do
            begin
                Result[i - 1] := integerPtr^;
                integerPtr := PInteger(ptruint(integerPtr) + PropertyStructArrayStep);
            end;
        end;
        TPropertyType.MappedStringEnumProperty, // shortcut
        TPropertyType.IntegerOnStructArrayProperty, // shortcut
        TPropertyType.MappedStringEnumOnStructArrayProperty: // shortcut
        begin
            if (PropertyType[Index] = TPropertyType.MappedStringEnumProperty) and not (TPropertyFlag.OnArray in PropertyFlags[Index]) then
                Exit;

            count := PInteger(PByte(obj) + PropertyStructArrayCountOffset)^;
            if count <= 0 then
                Exit;

            if (PropertyType[Index] = TPropertyType.MappedStringEnumProperty) and (TPropertyFlag.OnArray in PropertyFlags[Index]) then
            begin
                step := SizeOf(Integer);
                integerPtr := PInteger(PPByte(PByte(obj) + PropertyOffset[Index])^);
            end
            else
            begin
                step := PropertyStructArrayStep;
                integerPtr := PInteger(
                    PPByte(PByte(obj) + PropertyStructArrayOffset)^ +
                    PropertyOffset[Index]
                );
            end;

            Result := DSS_RecreateArray_PInteger(ResultPtr, ResultCount, count);
            for i := 1 to count do
            begin
                Result[i - 1] := integerPtr^;
                integerPtr := PInteger(ptruint(integerPtr) + step);
            end;
            // Inc(integerPtr, PInteger(PByte(obj) + PropertyStructArrayIndexOffset)^ - 1);
        end;
    end;
end;

procedure TDSSClassHelper.GetObjStrings(Obj: Pointer; Index: Integer; var ResultPtr: PPAnsiChar; ResultCount: PAPISize);
var
    i, count, step: Integer;
    stringList: TStringList;
    ptype: TPropertyType;
    integerPtr: PInteger;
    stringPtr: PString;
    otherObjPtr: TDSSObjectPtr;
    Result: PPAnsiCharArray0;
    ObjResultPtr: TDSSObjectPtr; 
    ObjResultCount: Array[0..3] of TAPISize;
begin
    if not ((Index > 0) and (Index <= NumProperties) and (PropertyOffset[Index] <> -1)) then
    begin
        Result := DSS_RecreateArray_PPAnsiChar(ResultPtr, ResultCount, 0);
        Exit;
    end;
    ptype := PropertyType[Index];
    case ptype of
        TPropertyType.BusesOnStructArrayProperty,
        TPropertyType.BusOnStructArrayProperty: // allow this one as a shortcut
        begin
            // Number of items
            count := PInteger(PByte(obj) + PropertyStructArrayCountOffset)^;
            if count <= 0 then
                Exit;
            Result := DSS_RecreateArray_PPAnsiChar(ResultPtr, ResultCount, count);
            for i := 1 to count do
                Result[i - 1] := DSS_CopyStringAsPChar(TDSSCktElement(obj).GetBus(i));
        end;
        TPropertyType.StringListProperty:
        begin
            if TPropertyFlag.ReadByFunction in PropertyFlags[Index] then
                stringList := TStringListPropertyFunction(Pointer(PropertyReadFunction[Index]))(obj)
            else
                stringList := PStringList(PByte(obj) + PropertyOffset[Index])^;

            if stringList = NIL then
                Exit;

            count := stringList.Count;
            Result := DSS_RecreateArray_PPAnsiChar(ResultPtr, ResultCount, count);
            for i := 1 to count do
                Result[i - 1] := DSS_CopyStringAsPChar(stringList.Strings[i - 1]);

            if TPropertyFlag.ReadByFunction in PropertyFlags[Index] then
                stringList.Free();
        end;
        TPropertyType.MappedStringEnumProperty, // shortcut
        TPropertyType.MappedStringEnumOnStructArrayProperty: // shortcut
        begin
            if (PropertyType[Index] = TPropertyType.MappedStringEnumProperty) and not (TPropertyFlag.OnArray in PropertyFlags[Index]) then
                Exit;

            count := PInteger(PByte(obj) + PropertyStructArrayCountOffset)^;
            if count <= 0 then
                Exit;

            if (PropertyType[Index] = TPropertyType.MappedStringEnumProperty) and (TPropertyFlag.OnArray in PropertyFlags[Index]) then
            begin
                step := SizeOf(Integer);
                integerPtr := PInteger(PPByte(PByte(obj) + PropertyOffset[Index])^);
            end
            else
            begin
                step := PropertyStructArrayStep;
                integerPtr := PInteger(
                    PPByte(PByte(obj) + PropertyStructArrayOffset)^ +
                    PropertyOffset[Index]
                );
            end;

            Result := DSS_RecreateArray_PPAnsiChar(ResultPtr, ResultCount, count);
            for i := 1 to count do
            begin
                Result[i - 1] := DSS_CopyStringAsPChar(TDSSEnum(Pointer(PropertyOffset2[Index])).OrdinalToString(integerPtr^));
                integerPtr := PInteger(ptruint(integerPtr) + PropertyStructArrayStep);
            end;
        end;

        TPropertyType.DSSObjectReferenceProperty:
        begin
            if not (TPropertyFlag.OnArray in PropertyFlags[Index]) then
                Exit;

            count := PInteger(PtrUint(obj) + PropertyStructArrayCountOffset)^;
            if count <= 0 then
                Exit;

            otherObjPtr := TDSSObjectPtr(PPByte(PtrUint(obj) + PropertyOffset[Index])^);
            Result := DSS_RecreateArray_PPAnsiChar(ResultPtr, ResultCount, count);
            if TPropertyFlag.FullNameAsArray in PropertyFlags[Index] then
                for i := 1 to count do
                begin
                    if otherObjPtr^ <> NIL then
                        Result[i - 1] := DSS_CopyStringAsPChar(otherObjPtr^.FullName)
                    else
                        Result[i - 1] := NIL;

                    inc(otherObjPtr);
                end
            else
                for i := 1 to count do
                begin
                    if otherObjPtr^ <> NIL then
                        Result[i - 1] := DSS_CopyStringAsPChar(otherObjPtr^.Name)
                    else
                        Result[i - 1] := NIL;

                    inc(otherObjPtr);
                end;
        end;
        TPropertyType.DSSObjectReferenceArrayProperty:
        begin
            ObjResultPtr := NIL;
            if TPropertyFlag.ReadByFunction in PropertyFlags[Index] then
            begin
                ObjResultCount[0] := 0;
                ObjResultCount[1] := 0;
                if DSS_EXTENSIONS_ARRAY_DIMS then
                begin
                    ObjResultCount[2] := 0;
                    ObjResultCount[3] := 0;
                end;
                TObjRefsPropertyFunction(Pointer(PropertyReadFunction[Index]))(obj, PPointer(ObjResultPtr), PAPISize(ObjResultCount));
                count := ObjResultCount[0];
                otherObjPtr := ObjResultPtr;
            end
            else
            begin
                // Number of items
                count := PInteger(PByte(obj) + PropertyStructArrayCountOffset)^;
                // Start of array
                otherObjPtr := TDSSObjectPtrPtr((PtrUint(obj) + PtrUint(PropertyOffset[Index])))^;
            end;
            Result := DSS_RecreateArray_PPAnsiChar(ResultPtr, ResultCount, count);

            
            for i := 1 to count do
            begin
                if otherObjPtr^ <> NIL then
                begin
                    if (PropertyOffset2[Index] = 0) or (TPropertyFlag.FullNameAsArray in PropertyFlags[Index]) then
                        Result[i - 1] := DSS_CopyStringAsPChar(otherObjPtr^.FullName)
                    else
                        Result[i - 1] := DSS_CopyStringAsPChar(otherObjPtr^.Name)
                end
                else
                    Result[i - 1] := NIL;
                Inc(otherObjPtr);
            end;

            if ObjResultPtr <> NIL then
                FreeMem(ObjResultPtr);
        end;
        TPropertyType.MappedStringEnumArrayProperty:
        begin
            if (TPropertyFlag.SizeIsFunction in PropertyFlags[Index]) then
                count := TIntegerPropertyFunction(Pointer(PropertyOffset3[Index]))(obj)
            else
                count := PropertyOffset3[Index];

            if count <= 0 then
                Exit;

            integerPtr := PPInteger(PByte(obj) + PropertyOffset[Index])^;
            if integerPtr = NIL then
                Result := DSS_RecreateArray_PPAnsiChar(ResultPtr, ResultCount, 0)
            else
            begin
                Result := DSS_RecreateArray_PPAnsiChar(ResultPtr, ResultCount, count);
                for i := 1 to count do
                begin
                    Result[i - 1] := DSS_CopyStringAsPChar(TDSSEnum(Pointer(PropertyOffset2[Index])).OrdinalToString(integerPtr^));
                    Inc(integerPtr);
                end;
            end;
        end;
        TPropertyType.MappedStringEnumArrayOnStructArrayProperty:
        begin
            // Number of items
            count := PInteger(PByte(obj) + PropertyStructArrayCountOffset)^;

            if count <= 0 then
                Exit;

            // Pointer to the first of the target fields
            integerPtr := PInteger(
                PPByte(PByte(obj) + PropertyStructArrayOffset)^ +
                PropertyOffset[Index]
            );

            Result := DSS_RecreateArray_PPAnsiChar(ResultPtr, ResultCount, count);
            for i := 1 to count do
            begin
                Result[i - 1] := DSS_CopyStringAsPChar(TDSSEnum(Pointer(PropertyOffset2[Index])).OrdinalToString(integerPtr^));
                integerPtr := PInteger(ptruint(integerPtr) + PropertyStructArrayStep);
            end;
        end;
    end;
end;

procedure TDSSClassHelper.GetObjObjects(Obj: Pointer; Index: Integer; var ResultPtr: PPointer; ResultCount: PAPISize);
var
    i, count: Integer;
    otherObjPtr: TDSSObjectPtr;
    Result: PPointerArray0;
begin
    Result := NIL;
    if not ((Index > 0) and (Index <= NumProperties) and (PropertyOffset[Index] <> -1)) then
        Exit;

    case PropertyType[Index] of
        TPropertyType.DSSObjectReferenceProperty:
        begin
            if not (TPropertyFlag.OnArray in PropertyFlags[Index]) then
                Exit;

            count := PInteger(PtrUint(obj) + PropertyStructArrayCountOffset)^;
            if count <= 0 then
            begin
                ResultCount^ := count;
                Exit;
            end;

            otherObjPtr := TDSSObjectPtr(PPByte(PtrUint(obj) + PropertyOffset[Index])^); // start of array
            Result := DSS_RecreateArray_PPointer(ResultPtr, ResultCount, count);
            for i := 1 to count do
            begin
                Result[i - 1] := otherObjPtr^;
                inc(otherObjPtr);
            end;
        end;
        TPropertyType.DSSObjectReferenceArrayProperty:
        begin
            if TPropertyFlag.ReadByFunction in PropertyFlags[Index] then
            begin
                TObjRefsPropertyFunction(Pointer(PropertyReadFunction[Index]))(obj, ResultPtr, ResultCount);
                Exit;
            end;

            // Number of items
            count := PInteger(PByte(obj) + PropertyStructArrayCountOffset)^;

            // Start of array
            otherObjPtr := TDSSObjectPtrPtr((PtrUint(obj) + PtrUint(PropertyOffset[Index])))^;

            Result := DSS_RecreateArray_PPointer(ResultPtr, ResultCount, count);

            for i := 1 to count do
            begin
                Result[i - 1] := otherObjPtr^;
                Inc(otherObjPtr);
            end;
        end;
    end;
end;

function TDSSClassHelper.FillObjFromJSON(obj: Pointer; json: TJSONObject; joptions: Integer): Boolean;
var
    propIndex: Integer;
    propName: String;
    jsonProp: TJSONEnum;
    propData: TJSONData = NIL;
    propFlags: TPropertyFlags;
    dssObj: TDSSObject;
    ptype: TPropertyType;
    numChanges: Integer = 0;
begin
    propIndex := -1;
    Result := false;
    dssObj := TDSSObject(obj);
{    for propIndex := 1 to orderedProperties.Count do
    begin
        //TODO: flag to lowercase transform key? legacy etc.?
        //TODO: flag to error on extra keys
        propName := PropertyNameJSON[propIndex];
        propFlags := PropertyFlags[propIndex]
        ptype := PropertyType[propIndex];
        if (not json.Find(propName, propData)) then
        begin
            if TPropertyFlag.Required in propFlags then
                raise Exception.Create(Format('JSON/%s/%s: required property not provided: "%s".', [Name, obj.Name, propName]));

            continue;
        end;
        
        if (ptype = TPropertyType.StringSilentROFunctionProperty) or (TPropertyFlag.ReadOnly in propFlags) then
            continue; // ignore...

        if not SetObjPropertyJSONValue(obj, propIndex, joptions, propData) then
            Exit;

        numChanges += 1;
    end;
    EndEdit(dssObj, numChanges);}
    Result := true;
end;

end.
