unit MeterClass;

{
  ----------------------------------------------------------
  Copyright (c) 2008-2015, Electric Power Research Institute, Inc.
  All rights reserved.
  ----------------------------------------------------------
}
{
   Base for Meter classes
}


{$M+}

interface

uses
    CktElementClass;

type
    TMeterClass = class(TCktElementClass)
    PRIVATE

    PROTECTED
        function ClassEdit(const ActiveMeterObj: Pointer; const ParamPointer: Integer): Integer;
        procedure ClassMakeLike(const OtherObj: Pointer);

        procedure CountProperties;  // Add no. of intrinsic properties
        procedure DefineProperties;  // Add Properties of this class to propName

    PUBLIC
        NumMeterClassProps: Integer;
        constructor Create;
        destructor Destroy; OVERRIDE;

        procedure ResetAll; VIRTUAL;
        procedure SampleAll; VIRTUAL;  // Force all monitors to take a sample
        procedure SaveAll; VIRTUAL;   // Force all monitors to save their buffers to disk
    PUBLISHED

    end;


implementation

uses
    MeterElement,
    ParserDel,
    DSSClassDefs,
    DSSGlobals;

constructor TMeterClass.Create;
begin

    inherited Create;
    NumMeterClassProps := 0;
    DSSClassType := METER_ELEMENT;
end;

destructor TMeterClass.Destroy;

begin
    inherited Destroy;
end;

procedure TMeterClass.CountProperties;
begin
    NumProperties := NumProperties + NumMeterClassProps;
    inherited CountProperties;
end;

procedure TMeterClass.DefineProperties;

// Define the properties for the base power delivery element class

begin
   // no properties
     // PropertyName^[ActiveProperty + 1] := 'propname';
     // PropertyHelp^[ActiveProperty + 1] := 'prop help';

    ActiveProperty := ActiveProperty + NumMeterClassProps;

    inherited DefineProperties;
end;

//- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
function TMeterClass.ClassEdit(const ActiveMeterObj: Pointer; const ParamPointer: Integer): Integer;


begin
    Result := 0;
  // continue parsing with contents of Parser
    if ParamPointer > 0 then
        with TMeterElement(ActiveMeterObj) do
        begin

      //CASE ParamPointer OF
       //1: BaseFrequency := Parser.Dblvalue;
       //ELSE
            inherited ClassEdit(ActiveMeterObj, ParamPointer - NumMeterClassProps)
      //END;
        end;

end;

procedure TMeterClass.ClassMakeLike(const OtherObj: Pointer);

//Var
//   OtherMeterObj : TMeterElement;
begin

//     OtherMeterObj := TMeterElement(OtherObj);
    TMeterElement.Create(OtherObj);
     //With TPCElement(ActiveDSSObject) Do
     //Begin
     //  value:= OtherMeterObj.value;
     //End;

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
