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
    DSSClass,
    CktElementClass;

type
    TPDClass = class(TCktElementClass)
    PRIVATE

    PROTECTED
        procedure ClassEdit(const ActivePDObj: Pointer; const ParamPointer: Integer);
        procedure ClassMakeLike(const OtherObj: Pointer);
        procedure CountProperties;  // Add no. of intrinsic properties
        procedure DefineProperties;  // Add Properties of this class to propName
    PUBLIC
        NumPDClassProps: Integer;
        constructor Create(dss: TDSSContext);
        destructor Destroy; OVERRIDE;
    PUBLISHED

    end;


implementation

uses
    DSSClassDefs,
    PDElement,
    ParserDel,
    DSSGlobals,
    Utilities,
    DSSHelper;

constructor TPDClass.Create(dss: TDSSContext);
begin

    inherited Create(dss);
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
procedure TPDClass.ClassEdit(const ActivePDObj: Pointer; const ParamPointer: Integer);
begin
  // continue parsing with contents of Parser

    if ParamPointer > 0 then
        with TPDElement(ActivePDObj) do
        begin

            case ParamPointer of
                1:
                    NormAmps := DSS.Parser.Dblvalue;
                2:
                    EmergAmps := DSS.Parser.Dblvalue;
                3:
                    FaultRate := DSS.Parser.Dblvalue;
                4:
                    PctPerm := DSS.Parser.Dblvalue;
                5:
                    HrsToRepair := DSS.Parser.DblValue;
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

    with TPDElement(DSS.ActiveDSSObject) do
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