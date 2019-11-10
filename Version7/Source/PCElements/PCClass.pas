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
    DSSClass,
    CktElementClass;

type
    TPCClass = class(TCktElementClass)
    PRIVATE

    PROTECTED
        procedure ClassEdit(const ActivePCObj: Pointer; const ParamPointer: Integer);
        procedure ClassMakeLike(const OtherObj: Pointer);

        procedure CountProperties;  // Add no. of intrinsic properties
        procedure DefineProperties;  // Add Properties of this class to propName

    PUBLIC
        NumPCClassProps: Integer;
        constructor Create(dss: TDSS);
        destructor Destroy; OVERRIDE;
    PUBLISHED

    end;


implementation

uses
    PCElement,
    ParserDel,
    DSSClassDefs,
    DSSGlobals,
    Utilities,
    DSSHelper;

constructor TPCClass.Create(dss: TDSS);
begin
    inherited Create(dss);
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
procedure TPCClass.ClassEdit(const ActivePCObj: Pointer; const ParamPointer: Integer);
begin
  // continue parsing with contents of Parser
    if ParamPointer > 0 then
        with TPCElement(ActivePCObj) do
        begin

            case ParamPointer of
                1:
                    Spectrum := Parser.StrValue;
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

    with TPCElement(DSS.ActiveDSSObject) do
    begin
        Spectrum := OtherPCObj.Spectrum;
        SpectrumObj := OtherPCObj.SpectrumObj;
    end;

    inherited ClassMakeLike(OtherObj);

end;


end.
