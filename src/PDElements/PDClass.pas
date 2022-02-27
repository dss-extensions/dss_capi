unit PDClass;

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
    TPDElementProp = (
        INVALID = 0,
        normamps=1,
        emergamps=2,
        faultrate=3,
        pctperm=4,
        repair=5
    );
{$SCOPEDENUMS OFF}

    TPDClass = class(TCktElementClass)
    PROTECTED
        procedure CountPropertiesAndAllocate; override;
        procedure DefineProperties; override;
    PUBLIC
        constructor Create(dssContext: TDSSContext; DSSClsType: Integer; DSSClsName: String);
        destructor Destroy; OVERRIDE;
    end;

implementation

uses
    DSSClassDefs,
    PDElement,
    DSSGlobals,
    Utilities,
    DSSHelper,
    DSSObjectHelper,
    TypInfo;

type
    TObj = TPDElement;
    TProp = TPDElementProp;
const
    NumPropsThisClass = Ord(High(TProp));
var
    PropInfo: Pointer;    

constructor TPDClass.Create(dssContext: TDSSContext; DSSClsType: Integer; DSSClsName: String);
begin
    if PropInfo = NIL then
        PropInfo := TypeInfo(TProp);

    inherited Create(dssContext, DSSClsType, DSSClsName);
    if (DSSClassType and NON_PCPD_ELEM) <> NON_PCPD_ELEM then
        DSSClassType := DSSClassType or PD_ELEMENT;

    ClassParents.Add('PDClass');
end;

destructor TPDClass.Destroy;
begin
    inherited Destroy;
end;

procedure TPDClass.CountPropertiesAndAllocate;
begin
    NumProperties := NumProperties + NumPropsThisClass;
    inherited CountPropertiesAndAllocate;
end;

procedure TPDClass.DefineProperties;
var 
    obj: TObj = NIL; // NIL (0) on purpose
begin
    PopulatePropertyNames(ActiveProperty, NumPropsThisClass, PropInfo, False);

    PropertyOffset[ActiveProperty + ord(TProp.normamps)] := ptruint(@obj.NormAmps);
    PropertyOffset[ActiveProperty + ord(TProp.emergamps)] := ptruint(@obj.EmergAmps);
    PropertyOffset[ActiveProperty + ord(TProp.faultrate)] := ptruint(@obj.FaultRate);
    PropertyOffset[ActiveProperty + ord(TProp.pctperm)] := ptruint(@obj.PctPerm);
    PropertyOffset[ActiveProperty + ord(TProp.repair)] := ptruint(@obj.HrsToRepair);

    ActiveProperty := ActiveProperty + NumPropsThisClass;
    inherited DefineProperties;
end;

initialization
    PropInfo := NIL;
end.
