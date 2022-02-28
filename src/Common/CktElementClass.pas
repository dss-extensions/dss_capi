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
    TCktElementProp = (
        INVALID = 0,
        basefreq = 1, 
        enabled = 2
    );
{$SCOPEDENUMS OFF}

    TCktElementClass = class(TDSSClass)
    PROTECTED
        procedure CountPropertiesAndAllocate; override;
        procedure DefineProperties; override;
    PUBLIC
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
const
    NumPropsThisClass = Ord(High(TProp));
var 
    PropInfo: Pointer = NIL;

procedure TCktElementClass.CountPropertiesAndAllocate;
begin
    NumProperties := NumProperties + NumPropsThisClass;
    inherited CountPropertiesAndAllocate;
end;

constructor TCktElementClass.Create(dssContext: TDSSContext; DSSClsType: Integer; DSSClsName: String);
begin
    if PropInfo = NIL then
        PropInfo := TypeInfo(TProp);

    inherited Create(dssContext, DSSClsType, DSSClsName);
    ClassParents.Add('CktElement');
end;

procedure TCktElementClass.DefineProperties;
var
    obj: TObj = NIL; // NIL (0) on purpose
begin
    PopulatePropertyNames(ActiveProperty, NumPropsThisClass, PropInfo);

    // Special boolean property
    PropertyType[ActiveProperty + ord(TProp.enabled)] := TPropertyType.EnabledProperty;
    PropertyOffset[ActiveProperty + ord(TProp.enabled)] := 1; // dummy value

    // double properties (default type)
    PropertyOffset[ActiveProperty + ord(TProp.basefreq)] := ptruint(@obj.BaseFrequency);

    ActiveProperty := ActiveProperty + NumPropsThisClass;
    inherited DefineProperties;
end;

function TCktElementClass.BeginEdit(ptr: Pointer; SetActive_: Boolean): Pointer;
var
    Obj: TObj;
begin
    // This is the default action, some classes do a bit more
    if ptr <> NIL then
        Obj := TObj(ptr)
    else
        Obj := ElementList.Active;

    if (Obj <> NIL) and (Flg.EditionActive in Obj.Flags) then
    begin
        DosimpleMsg('%s: Object already being edited!', [Obj.FullName], 37737);
        Exit;
    end;

    if SetActive_ then
    begin
        //TODO: e.g. DSS.ActiveCapControlObj := Obj; -- if ever required for all elements
        ActiveCircuit.ActiveCktElement := Obj;
    end;

    Include(Obj.Flags, Flg.EditionActive);
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