unit MeterClass;
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
    TMeterClass = class(TCktElementClass)
    PROTECTED
        procedure CountPropertiesAndAllocate; override;
        procedure DefineProperties; override;
    PUBLIC
        constructor Create(dssContext: TDSSContext; DSSClsType: Integer; DSSClsName: String);
        destructor Destroy; OVERRIDE;

        procedure ResetAll; VIRTUAL;
        procedure SampleAll; VIRTUAL;  // Force all monitors to take a sample
        procedure SaveAll; VIRTUAL;   // Force all monitors to save their buffers to disk
    end;

implementation

uses
    MeterElement,
    DSSClassDefs,
    DSSGlobals;

const
    NumPropsThisClass = 0;

constructor TMeterClass.Create(dssContext: TDSSContext; DSSClsType: Integer; DSSClsName: String);
begin
    inherited Create(dssContext, DSSClsType or METER_ELEMENT, DSSClsName);
    ClassParents.Add('MeterClass');
end;

destructor TMeterClass.Destroy;

begin
    inherited Destroy;
end;

procedure TMeterClass.CountPropertiesAndAllocate;
begin
    NumProperties := NumProperties + NumPropsThisClass;
    inherited CountPropertiesAndAllocate;
end;

procedure TMeterClass.DefineProperties;
begin
    // no properties
    ActiveProperty := ActiveProperty + NumPropsThisClass;
    inherited DefineProperties;
end;

procedure TMeterClass.ResetAll;
begin
    DoSimpleMsg('Programming Error: Base MeterClass.ResetAll Reached for Class: ' + Name, 760);
end;

procedure TMeterClass.SampleAll;
begin
    DoSimpleMsg('Programming Error: Base MeterClass.SampleAll Reached for Class: ' + Name, 761);
end;

procedure TMeterClass.SaveAll;
begin
    DoSimpleMsg('Programming Error: Base MeterClass.SaveAll Reached for Class: ' + Name, 762);
end;

end.
