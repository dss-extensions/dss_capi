unit CktElementClass;

{
    ----------------------------------------------------------
  Copyright (c) 2008-2015, Electric Power Research Institute, Inc.
  All rights reserved.
  ----------------------------------------------------------
}

interface

uses
    DSSClass;

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
        procedure CountPropertiesAndAllocate; override;
        procedure DefineProperties; override;
    PUBLIC
        PropertyOffset_CktElementClass: Integer;

        constructor Create(dssContext: TDSSContext; DSSClsType: Integer; DSSClsName: String);
        destructor Destroy; OVERRIDE;
        function BeginEdit(ptr: Pointer; SetActive_: Boolean=True): Pointer; override;
        function EndEdit(ptr: Pointer; const NumChanges: integer): Boolean; override;
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

procedure TCktElementClass.CountPropertiesAndAllocate;
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
    ClassParents.Add('CktElement');
end;

procedure TCktElementClass.DefineProperties;
var
    obj: TObj = NIL; // NIL (0) on purpose
begin
    PopulatePropertyNames(ActiveProperty, NumPropsThisClass, PropInfo, PropInfoLegacy, False, 'CktElement');

    PropertyOffset_CktElementClass := ActiveProperty;

    // Special boolean property
    PropertyType[ActiveProperty + ord(TProp.enabled)] := TPropertyType.EnabledProperty;
    PropertyOffset[ActiveProperty + ord(TProp.enabled)] := 1; // dummy value

    // double properties (default type)
    PropertyOffset[ActiveProperty + ord(TProp.basefreq)] := ptruint(@obj.BaseFrequency);
    PropertyFlags[ActiveProperty + ord(TProp.basefreq)] := [TPropertyFlag.DynamicDefault, TPropertyFlag.NonNegative, TPropertyFlag.NonZero, TPropertyFlag.Units_Hz];

    ActiveProperty := ActiveProperty + NumPropsThisClass;
    inherited DefineProperties;
end;

function TCktElementClass.BeginEdit(ptr: Pointer; SetActive_: Boolean): Pointer;
var
    Obj: TObj;
begin
    Obj := TObj(inherited BeginEdit(ptr, False));
    if SetActive_ then
    begin
        //TODO: e.g. DSS.ActiveCapControlObj := Obj; -- if ever required for all elements
        ActiveCircuit.ActiveCktElement := Obj;
    end;
    Result := Obj;
end;

function TCktElementClass.EndEdit(ptr: Pointer; const NumChanges: integer): Boolean;
var
    Obj: TObj;
begin
    Obj := TObj(ptr);
    Exclude(Obj.Flags, Flg.EditionActive);

    // This is the default action, many classes do more.
    TObj(ptr).RecalcElementData();
    Result := True;
end;

destructor TCktElementClass.Destroy;
begin
    inherited Destroy;
end;

end.
