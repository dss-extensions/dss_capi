unit ControlClass;

{
   ----------------------------------------------------------
  Copyright (c) 2008-2015, Electric Power Research Institute, Inc.
  All rights reserved.
  ----------------------------------------------------------
}
{
   Base for control classes
}

{$M+}

interface

uses
    DSSClass,
    CktElementClass;

type
    TControlClass = class(TCktElementClass)
    PRIVATE

    PROTECTED
        procedure ClassEdit(const ActiveControlObj: Pointer; const ParamPointer: Integer);
        procedure ClassMakeLike(const OtherObj: Pointer);

        procedure CountProperties;  // Add no. of intrinsic properties
        procedure DefineProperties;  // Add Properties of this class to propName

    PUBLIC
        NumControlClassProps: Integer;
        constructor Create(dss: TDSS);
        destructor Destroy; OVERRIDE;
    PUBLISHED

    end;


implementation

uses
    ControlElem,
    ParserDel,
    DSSClassDefs,
    DSSGlobals;

constructor TControlClass.Create(dss: TDSS);
begin

    inherited Create(dss);
    NumControlClassProps := 0;
    DSSClassType := CTRL_ELEMENT;
end;

destructor TControlClass.Destroy;

begin
    inherited Destroy;
end;

procedure TControlClass.CountProperties;
begin
    NumProperties := NumProperties + NumControlClassProps;
    inherited CountProperties;
end;

procedure TControlClass.DefineProperties;

// Define the properties for the base power delivery element class

begin
   // no properties
     // PropertyName^[ActiveProperty + 1] := 'propname';
     // PropertyHelp^[ActiveProperty + 1] := 'prop help';

    ActiveProperty := ActiveProperty + NumControlClassProps;

    inherited DefineProperties;
end;

//- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
procedure TControlClass.ClassEdit(const ActiveControlObj: Pointer; const ParamPointer: Integer);
begin
  // continue parsing with contents of Parser
    if ParamPointer > 0 then
        with TControlElem(ActiveControlObj) do
        begin

      //CASE ParamPointer OF
       //1: BaseFrequency := DSS.Parser.Dblvalue;
       //ELSE
            inherited ClassEdit(ActiveControlObj, ParamPointer - NumControlClassProps)
      //END;
        end;

end;

procedure TControlClass.ClassMakeLike(const OtherObj: Pointer);

//Var
//   OtherControlObj : TControlElem;
begin

//   OtherControlObj := TControlElem(OtherObj);
    TControlElem.Create(OtherObj);

     //With TPCElement(DSS.ActiveDSSObject) Do
     //Begin
     //  value:= OtherControlObj.value;
     //End;

end;


end.
