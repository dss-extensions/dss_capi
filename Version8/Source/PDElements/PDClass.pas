unit PDClass;

{
  ----------------------------------------------------------
  Copyright (c) 2008-2015, Electric Power Research Institute, Inc.
  All rights reserved.
  ----------------------------------------------------------
}

{$M+}

interface

uses
    CktElementClass;

type
    TPDClass = class(TCktElementClass)
    PRIVATE

    PROTECTED
        function ClassEdit(const ActivePDObj: Pointer; const ParamPointer: Integer): Integer;
        procedure ClassMakeLike(const OtherObj: Pointer);
        procedure CountProperties;  // Add no. of intrinsic properties
        procedure DefineProperties;  // Add Properties of this class to propName
    PUBLIC
        NumPDClassProps: Integer;
        constructor Create;
        destructor Destroy; OVERRIDE;
    PUBLISHED

    end;


implementation

uses
    PDElement,
    ParserDel,
    DSSClassDefs,
    DSSGlobals,
    Utilities;

constructor TPDClass.Create;
begin

    inherited Create;
    NumPDClassProps := 5;
    DSSClassType := PD_ELEMENT;
end;

destructor TPDClass.Destroy;

begin
    inherited Destroy;
end;

procedure TPDClass.CountProperties;
begin
    NumProperties := NumProperties + NumPDClassProps;
    inherited CountProperties;
end;

procedure TPDClass.DefineProperties;

// Define the properties for the base power delivery element class

begin
    PropertyName^[ActiveProperty + 1] := 'normamps';
    PropertyName^[ActiveProperty + 2] := 'emergamps';
    PropertyName^[ActiveProperty + 3] := 'faultrate';
    PropertyName^[ActiveProperty + 4] := 'pctperm';
    PropertyName^[ActiveProperty + 5] := 'repair';

    PropertyHelp^[ActiveProperty + 1] := 'Normal rated current.';
    PropertyHelp^[ActiveProperty + 2] := 'Maximum or emerg current.';
    PropertyHelp^[ActiveProperty + 3] := 'Failure rate per year.';
    PropertyHelp^[ActiveProperty + 4] := 'Percent of failures that become permanent.';
    PropertyHelp^[ActiveProperty + 5] := 'Hours to repair.';

    ActiveProperty := ActiveProperty + NumPDClassProps;

    inherited DefineProperties;
end;


//- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
function TPDClass.ClassEdit(const ActivePDObj: Pointer; const ParamPointer: Integer): Integer;


begin
    Result := 0;
  // continue parsing with contents of Parser

    if ParamPointer > 0 then
        with TPDElement(ActivePDObj) do
        begin

            case ParamPointer of
                1:
                    NormAmps := Parser[ActiveActor].Dblvalue;
                2:
                    EmergAmps := Parser[ActiveActor].Dblvalue;
                3:
                    FaultRate := Parser[ActiveActor].Dblvalue;
                4:
                    PctPerm := Parser[ActiveActor].Dblvalue;
                5:
                    HrsToRepair := Parser[ActiveActor].DblValue;
            else
                inherited ClassEdit(ActivePDObj, ParamPointer - NumPDClassProps)
            end;
        end;

end;

procedure TPDClass.ClassMakeLike(const OtherObj: Pointer);

var
    OtherPDObj: TPDElement;
begin

    OtherPDObj := TPDElement(OtherObj);

    with TPDElement(ActiveDSSObject[ActiveActor]) do
    begin
        NormAmps := OtherPDObj.NormAmps;
        EmergAmps := OtherPDObj.EmergAmps;
        FaultRate := OtherPDObj.FaultRate;
        PctPerm := OtherPDObj.PctPerm;
        HrsToRepair := OtherPDObj.HrsToRepair;
    end;

    inherited ClassMakeLike(OtherObj);

end;

end.
