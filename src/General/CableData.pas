unit CableData;

{
  ----------------------------------------------------------
  Copyright (c) 2008-2015, Electric Power Research Institute, Inc.
  All rights reserved.
  ----------------------------------------------------------
}
interface

uses
    Classes,
    Command,
    DSSClass,
    DSSObject,
    ConductorData;

type
{$SCOPEDENUMS ON}
    TCableDataProp = (
        INVALID = 0,
        EpsR = 1,
        InsLayer = 2,
        DiaIns = 3,
        DiaCable = 4
    );
{$SCOPEDENUMS OFF}

    TCableData = class(TConductorData)
    PROTECTED
        PropertyOffset_CableData: Integer;

        procedure CountPropertiesAndAllocate; override;
        procedure DefineProperties; override;
    PUBLIC
        constructor Create(dssContext: TDSSContext; DSSClsType: Integer; DSSClsName: String);
        destructor Destroy; OVERRIDE;
    end;

    TCableDataObj = class(TConductorDataObj)
    PUBLIC
        FEpsR: Double;
        // next 3 use parent RadiusUnits
        FInsLayer: Double;
        FDiaIns: Double;
        FDiaCable: Double;

        constructor Create(ParClass: TDSSClass; const CableDataName: String);
        destructor Destroy; OVERRIDE;
        procedure PropertySideEffects(Idx: Integer; previousIntVal: Integer = 0); override;
        procedure MakeLike(OtherObj: Pointer); override;

        property EpsR: Double READ FEpsR;
        property DiaIns: Double READ FDiaIns;
        property DiaCable: Double READ FDiaCable;
        property InsLayer: Double READ FInsLayer;
    end;

implementation

uses
    DSSGlobals,
    DSSClassDefs,
    Sysutils,
    UComplex, DSSUcomplex,
    Arraydef,
    LineUnits,
    Utilities,
    DSSHelper,
    DSSObjectHelper,
    TypInfo;

type
    TObj = TCableDataObj;
    TProp = TCableDataProp;
const
    NumPropsThisClass = Ord(High(TProp));
var
    PropInfo: Pointer = NIL;    

constructor TCableData.Create(dssContext: TDSSContext; DSSClsType: Integer; DSSClsName: String);
begin
    if PropInfo = NIL then
        PropInfo := TypeInfo(TProp);

    inherited Create(dssContext, DSSClsType or DSS_OBJECT, DSSClsName);
    ClassParents.Add('CableData');
end;

destructor TCableData.Destroy;
begin
    inherited Destroy;
end;

procedure TCableData.CountPropertiesAndAllocate;
begin
    NumProperties := NumProperties + NumPropsThisClass;
    inherited CountPropertiesAndAllocate;
end;

procedure TCableData.DefineProperties;
var 
    obj: TObj = NIL; // NIL (0) on purpose
begin
    PopulatePropertyNames(ActiveProperty, NumPropsThisClass, PropInfo);

    PropertyOffset_CableData := ActiveProperty;

    // double properties (default type)
    PropertyOffset[ActiveProperty + ord(TProp.EpsR)] := ptruint(@obj.FEpsR);
    PropertyOffset[ActiveProperty + ord(TProp.InsLayer)] := ptruint(@obj.FInsLayer);
    PropertyOffset[ActiveProperty + ord(TProp.DiaIns)] := ptruint(@obj.FDiaIns);
    PropertyOffset[ActiveProperty + ord(TProp.DiaCable)] := ptruint(@obj.FDiaCable);

    ActiveProperty := ActiveProperty + NumPropsThisClass;
    inherited DefineProperties;
end;

procedure TCableDataObj.PropertySideEffects(Idx: Integer; previousIntVal: Integer);
begin
    // Check for critical errors
    case (Idx - (ParentClass as TCableData).PropertyOffset_CableData)  of
        ord(TProp.EpsR):
            if (FEpsR < 1.0) then
                DoSimpleMsg('Error: Insulation permittivity must be greater than one for CableData %s', [Name], 999);
        ord(TProp.InsLayer):
            if (FInsLayer <= 0.0) then
                DoSimpleMsg('Error: Insulation layer thickness must be positive for CableData %s', [Name], 999);
        ord(TProp.DiaIns):
            if (FDiaIns <= 0.0) then
                DoSimpleMsg('Error: Diameter over insulation layer must be positive for CableData %s', [Name], 999);
        ord(TProp.DiaCable):
            if (FDiaCable <= 0.0) then
                DoSimpleMsg('Error: Diameter over cable must be positive for CableData %s', [Name], 999);
    end;
    inherited PropertySideEffects(Idx, previousIntVal);
end;

procedure TCableDataObj.MakeLike(OtherObj: Pointer);
var
    Other: TObj;
begin
    inherited MakeLike(OtherObj);
    Other := TCableDataObj(OtherObj);
    FEpsR := Other.FEpsR;
    FInsLayer := Other.FInsLayer;
    FDiaIns := Other.FDiaIns;
    FDiaCable := Other.FDiaCable;
end;

constructor TCableDataObj.Create(ParClass: TDSSClass; const CableDataName: String);
begin
    inherited Create(ParClass, CableDataName);
    Name := LowerCase(CableDataName);
    DSSObjType := ParClass.DSSClassType;

    FEpsR := 2.3;
    FInsLayer := -1.0;
    FDiaIns := -1.0;
    FDiaCable := -1.0;
end;

destructor TCableDataObj.Destroy;
begin
    inherited destroy;
end;

end.