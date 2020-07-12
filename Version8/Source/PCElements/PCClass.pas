unit PCClass;

{$M+}
{
  ----------------------------------------------------------
  Copyright (c) 2008-2015, Electric Power Research Institute, Inc.
  All rights reserved.
  ----------------------------------------------------------
}

interface

uses
    CktElementClass;

type
    TPCClass = class(TCktElementClass)
    PRIVATE

    PROTECTED
        function ClassEdit(const ActivePCObj: Pointer; const ParamPointer: Integer): Integer;
        procedure ClassMakeLike(const OtherObj: Pointer);

        procedure CountProperties;  // Add no. of intrinsic properties
        procedure DefineProperties;  // Add Properties of this class to propName

    PUBLIC
        NumPCClassProps: Integer;
        constructor Create;
        destructor Destroy; OVERRIDE;
    PUBLISHED

    end;


implementation

uses
    PCElement,
    ParserDel,
    DSSClassDefs,
    DSSGlobals,
    Utilities;

constructor TPCClass.Create;
begin

    inherited Create;
    NumPCClassProps := 1;
    DSSClassType := PC_ELEMENT;
end;

destructor TPCClass.Destroy;

begin
    inherited Destroy;
end;

procedure TPCClass.CountProperties;
begin
    NumProperties := NumProperties + NumPCClassProps;
    inherited CountProperties;
end;

procedure TPCClass.DefineProperties;

// Define the properties for the base power delivery element class

begin

    PropertyName^[ActiveProperty + 1] := 'spectrum';

    PropertyHelp^[ActiveProperty + 1] := 'Name of harmonic spectrum for this device.';

    ActiveProperty := ActiveProperty + NumPCClassProps;

    inherited DefineProperties;
end;

//- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
function TPCClass.ClassEdit(const ActivePCObj: Pointer; const ParamPointer: Integer): Integer;


begin
    Result := 0;
  // continue parsing with contents of Parser
    if ParamPointer > 0 then
        with TPCElement(ActivePCObj) do
        begin

            case ParamPointer of
                1:
                    Spectrum := Parser[ActiveActor].StrValue;
            else
                inherited ClassEdit(ActivePCObj, ParamPointer - NumPCClassProps)
            end;
        end;

end;

procedure TPCClass.ClassMakeLike(const OtherObj: Pointer);

var
    OtherPCObj: TPCElement;
begin

    OtherPCObj := TPCElement(OtherObj);

    with TPCElement(ActiveDSSObject[ActiveActor]) do
    begin
        Spectrum := OtherPCObj.Spectrum;
        SpectrumObj := OtherPCObj.SpectrumObj;
    end;

    inherited ClassMakeLike(OtherObj);

end;


end.
