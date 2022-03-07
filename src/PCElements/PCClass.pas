unit PCClass;

{
  ----------------------------------------------------------
  Copyright (c) 2008-2015, Electric Power Research Institute, Inc.
  All rights reserved.
  ----------------------------------------------------------
}

interface

uses
    DSSClass,
    CktElementClass;

type
{$SCOPEDENUMS ON}
    TPCElementProp = (
        INVALID = 0,
        spectrum = 1
    );
{$SCOPEDENUMS OFF}

    TPCClass = class(TCktElementClass)
    PROTECTED
        procedure CountPropertiesAndAllocate; override;
        procedure DefineProperties; override;

    PUBLIC
        constructor Create(dssContext: TDSSContext; DSSClsType: Integer; DSSClsName: String);
        destructor Destroy; OVERRIDE;
    end;


implementation

uses
    PCElement,
    DSSClassDefs,
    DSSGlobals,
    Utilities,
    DSSHelper,
    DSSObjectHelper,
    TypInfo;

type
    TObj = TPCElement;
    TProp = TPCElementProp;
const
    NumPropsThisClass = Ord(High(TProp));
var
    PropInfo: Pointer = NIL;    

constructor TPCClass.Create(dssContext: TDSSContext; DSSClsType: Integer; DSSClsName: String);
begin
    if PropInfo = NIL then
        PropInfo := TypeInfo(TProp);

    inherited Create(dssContext, DSSClsType, DSSClsName);

    if (DSSClassType and NON_PCPD_ELEM) <> NON_PCPD_ELEM then
        DSSClassType := DSSClassType or PC_ELEMENT;

    ClassParents.Add('PCClass');
end;

destructor TPCClass.Destroy;

begin
    inherited Destroy;
end;

procedure TPCClass.CountPropertiesAndAllocate;
begin
    NumProperties := NumProperties + NumPropsThisClass;
    inherited CountPropertiesAndAllocate;
end;

procedure TPCClass.DefineProperties;
var 
    obj: TObj = NIL; // NIL (0) on purpose
begin
    PopulatePropertyNames(ActiveProperty, NumPropsThisClass, PropInfo, False, 'PCClass');
    PropertyType[ActiveProperty + ord(TProp.Spectrum)] := TPropertyType.DSSObjectReferenceProperty;
    PropertyOffset[ActiveProperty + ord(TProp.Spectrum)] := ptruint(@obj.SpectrumObj);
    PropertyOffset2[ActiveProperty + ord(TProp.Spectrum)] := ptruint(DSS.SpectrumClass);
    ActiveProperty := ActiveProperty + NumPropsThisClass;
    inherited DefineProperties;
end;

end.