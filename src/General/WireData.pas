unit WireData;

// ----------------------------------------------------------
// Copyright (c) 2008-2020, Electric Power Research Institute, Inc.
// All rights reserved.
// ----------------------------------------------------------

interface

// Used for overhead line impedances.

uses
    Classes,
    Command,
    DSSClass,
    DSSObject,
    ConductorData;

type
    TWireData = class(TConductorData)
    PROTECTED
        procedure DefineProperties(); override;
    PUBLIC
        constructor Create(dssContext: TDSSContext);
        destructor Destroy; OVERRIDE;

        Function NewObject(const ObjName: String; Activate: Boolean = True): Pointer; OVERRIDE;
    end;

    TWireDataObj = class(TConductorDataObj)
    PUBLIC
        constructor Create(ParClass: TDSSClass; const WireDataName: String);
        destructor Destroy; OVERRIDE;
    end;

implementation

uses
    DSSGlobals,
    DSSClassDefs,
    Sysutils,
    UComplex, DSSUcomplex,
    Arraydef,
    LineUNits,
    Utilities,
    DSSHelper,
    DSSObjectHelper,
    TypInfo;

type
    TObj = TWireDataObj;

const
    NumPropsThisClass = 0; // because they were all moved to ConductorData

constructor TWireData.Create(dssContext: TDSSContext);
begin
    inherited Create(dssContext, DSS_OBJECT, 'WireData');
end;

destructor TWireData.Destroy;
begin
    inherited Destroy;
end;

procedure TWireData.DefineProperties();
begin
    NumProperties := NumPropsThisClass;
    CountPropertiesAndAllocate();

    ActiveProperty := NumPropsThisClass;
    inherited DefineProperties();
end;

function TWireData.NewObject(const ObjName: String; Activate: Boolean): Pointer;
var
    obj: TObj;
begin
    obj := TObj.Create(Self, ObjName);
    if Activate then 
        DSS.ActiveDSSObject := obj;
    obj.ClassIndex := AddObjectToList(obj, Activate);
    Result := obj;
end;

constructor TWireDataObj.Create(ParClass: TDSSClass; const WireDataName: String);
begin
    inherited Create(ParClass, WireDataName);
    DSSObjType := ParClass.DSSClassType;
end;

destructor TWireDataObj.Destroy;
begin
    inherited destroy;
end;

end.
