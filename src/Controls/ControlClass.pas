unit ControlClass;

{
   ----------------------------------------------------------
  Copyright (c) 2008-2015, Electric Power Research Institute, Inc.
  All rights reserved.
  ----------------------------------------------------------
}
//   Base for control classes
interface

uses
    DSSClass,
    CktElementClass;

type
    TControlClass = class(TCktElementClass)
    PROTECTED
        procedure CountPropertiesAndAllocate; override;
        procedure DefineProperties; override;

    PUBLIC
        constructor Create(dssContext: TDSSContext; DSSClsType: Integer; DSSClsName: String);
        destructor Destroy; OVERRIDE;
    end;


implementation

uses
    ControlElem,
    DSSClassDefs,
    DSSGlobals;

const
    NumPropsThisClass = 0;

constructor TControlClass.Create(dssContext: TDSSContext; DSSClsType: Integer; DSSClsName: String);
begin
    inherited Create(dssContext, DSSClsType or CTRL_ELEMENT, DSSClsName);
    ClassParents.Add('ControlClass');
end;

destructor TControlClass.Destroy;
begin
    inherited Destroy;
end;

procedure TControlClass.CountPropertiesAndAllocate;
begin
    NumProperties := NumProperties + NumPropsThisClass;
    inherited CountPropertiesAndAllocate;
end;

procedure TControlClass.DefineProperties;
begin
    // no properties
    ActiveProperty := ActiveProperty + NumPropsThisClass;
    inherited DefineProperties;
end;

end.
