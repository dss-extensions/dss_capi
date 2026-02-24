unit CktElementClass;

// ----------------------------------------------------------
// Copyright (c) 2008-2015, Electric Power Research Institute, Inc.
// All rights reserved.
// ----------------------------------------------------------

interface

uses
    DSSClass,
    DSSObject,
    CAPI_Types;

type
{$SCOPEDENUMS ON}
    TCktElementPropLegacy = (
        INVALID = 0,
        basefreq = 1, 
        enabled = 2
    );

    TCktElementProp = (
        INVALID = 0,
        BaseFreq = 1, 
        Enabled = 2
    );
{$SCOPEDENUMS OFF}

    TCktElementClass = class(TDSSClass)
    PROTECTED
        procedure CountPropertiesAndAllocate(); override;
        procedure DefineProperties(); override;
    PUBLIC
        PropertyOffset_CktElementClass: Integer;

        constructor Create(dssContext: TDSSContext; DSSClsType: Integer; DSSClsName: String);
        destructor Destroy; OVERRIDE;
        function BeginEdit(ptr: Pointer; SetActive_: Boolean=True): Pointer; override;
        function EndEdit(ptr: Pointer; const NumChanges: integer): Boolean; override;
        function GetRegisterNames(obj: TDSSObject): ArrayOfString; virtual;
        function GetRegisterValues(obj: TDSSObject; var numRegisters: Integer): pDoubleArray; virtual;
    end;

implementation

uses
    CktElement,
    Utilities,
    DSSGlobals,
    DSSHelper,
    DSSObjectHelper;

type
    TObj = TDSSCktElement;
    TProp = TCktElementProp;
    TPropLegacy = TCktElementPropLegacy;
const
    NumPropsThisClass = Ord(High(TProp));
var 
    PropInfo: Pointer = NIL;
    PropInfoLegacy: Pointer = NIL;

procedure TCktElementClass.CountPropertiesAndAllocate();
begin
    NumProperties := NumProperties + NumPropsThisClass;
    inherited CountPropertiesAndAllocate;
end;

constructor TCktElementClass.Create(dssContext: TDSSContext; DSSClsType: Integer; DSSClsName: String);
begin
    if PropInfo = NIL then
    begin
        PropInfo := TypeInfo(TProp);
        PropInfoLegacy := TypeInfo(TPropLegacy);
    end;

    inherited Create(dssContext, DSSClsType, DSSClsName);
    RequiresCircuit := true;
    ClassParents.Add('CktElement');
end;

procedure CE_Set_Enabled(obj: TObj; value: WordBool);
begin
    obj.SetEnabled(value);
end;

procedure TCktElementClass.DefineProperties();
var
    obj: TObj = NIL; // NIL (0) on purpose
begin
    PopulatePropertyNames(ActiveProperty, NumPropsThisClass, PropInfo, PropInfoLegacy, False, 'CktElement');

    PropertyOffset_CktElementClass := ActiveProperty;

    // Special boolean property
    PropertyType[ActiveProperty + ord(TProp.enabled)] := TPropertyType.BooleanProperty;
    PropertyOffset[ActiveProperty + ord(TProp.enabled)] := PtrInt(@obj.FEnabled);
    PropertyFlags[ActiveProperty + ord(TProp.enabled)] := [TPropertyFlag.WriteByFunction];
    PropertyWriteFunction[ActiveProperty + ord(TProp.enabled)] := @CE_Set_Enabled;

    // double properties (default type)
    PropertyOffset[ActiveProperty + ord(TProp.basefreq)] := PtrInt(@obj.BaseFrequency);
    PropertyFlags[ActiveProperty + ord(TProp.basefreq)] := [TPropertyFlag.DynamicDefault, TPropertyFlag.NonNegative, TPropertyFlag.NonZero, TPropertyFlag.Units_Hz];

    ActiveProperty := ActiveProperty + NumPropsThisClass;
    inherited DefineProperties();
end;

function TCktElementClass.BeginEdit(ptr: Pointer; SetActive_: Boolean): Pointer;
var
    obj: TObj;
begin
    obj := TObj(inherited BeginEdit(ptr, False));
    if SetActive_ then
    begin
        //TODO: e.g. DSS.ActiveCapControlObj := obj; -- if ever required for all elements
        ActiveCircuit.SetActiveCktElement(obj);
    end;
    Result := obj;
end;

function TCktElementClass.EndEdit(ptr: Pointer; const NumChanges: integer): Boolean;
var
    obj: TObj;
begin
    obj := TObj(ptr);
    Exclude(obj.Flags, Flg.EditingActive);

    // This is the default action, many classes do more.
    TObj(ptr).RecalcElementData();
    Result := True;
end;

destructor TCktElementClass.Destroy;
begin
    inherited Destroy;
end;

function TCktElementClass.GetRegisterNames(obj: TDSSObject): ArrayOfString;
begin
    Result := NIL;
end;

function TCktElementClass.GetRegisterValues(obj: TDSSObject; var numRegisters: Integer): pDoubleArray;
begin
    numRegisters := 0;
    Result := NIL;
end;

end.
