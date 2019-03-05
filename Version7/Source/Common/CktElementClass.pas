unit CktElementClass;

{
    ----------------------------------------------------------
  Copyright (c) 2008-2015, Electric Power Research Institute, Inc.
  All rights reserved.
  ----------------------------------------------------------
}
{ Created 5/17/01 RCD to balance inheritance tree for Circuit Elements}

{$M+}

interface

uses
    DSSClass;

type
    TCktElementClass = class(TDSSClass)
    PRIVATE

    PROTECTED
        function ClassEdit(const ActiveCktElemObj: Pointer; const ParamPointer: Integer): Integer;
        procedure ClassMakeLike(const OtherObj: Pointer);

        procedure CountProperties;  // Add no. of intrinsic properties
        procedure DefineProperties;  // Add Properties of this class to propName

    PUBLIC
        NumCktElemClassProps: Integer;
        constructor Create;
        destructor Destroy; OVERRIDE;
    PUBLISHED

    end;

implementation

uses
    CktElement,
    ParserDel,
    Utilities,
    DSSGlobals;

{ TCktElementClass }

function TCktElementClass.ClassEdit(const ActiveCktElemObj: Pointer;
    const ParamPointer: Integer): Integer;

begin
    Result := 0;
  // continue parsing with contents of Parser
    if ParamPointer > 0 then
        with TDSSCktElement(ActiveCktElemObj) do
        begin

            case ParamPointer of
                1:
                    BaseFrequency := Parser.Dblvalue;
                2:
                    Enabled := InterpretYesNo(Parser.StrValue);
            else
                inherited ClassEdit(ActiveCktElemObj, ParamPointer - NumCktElemClassProps)
            end;
        end;

end;

procedure TCktElementClass.ClassMakeLike(const OtherObj: Pointer);
var
    OtherCktObj: TDSSCktElement;
begin

    OtherCktObj := TDSSCktElement(OtherObj);

    with TDSSCktElement(ActiveDSSObject) do
    begin
        BaseFrequency := OtherCktObj.BaseFrequency;
        Enabled := TRUE;
    end;

end;

procedure TCktElementClass.CountProperties;

begin
    NumProperties := NumProperties + NumCktElemClassProps;
    inherited CountProperties;

end;

constructor TCktElementClass.Create;
begin

    inherited Create;
    NumCktElemClassProps := 2;

end;

procedure TCktElementClass.DefineProperties;

// Define the properties for the base power delivery element class

begin
    PropertyName^[ActiveProperty + 1] := 'basefreq';
    PropertyName^[ActiveProperty + 2] := 'enabled';

    PropertyHelp^[ActiveProperty + 1] := 'Base Frequency for ratings.';
    PropertyHelp^[ActiveProperty + 2] := '{Yes|No or True|False} Indicates whether this element is enabled.';

    ActiveProperty := ActiveProperty + NumCktElemClassProps;

    inherited DefineProperties;

end;

destructor TCktElementClass.Destroy;
begin
    inherited Destroy;

end;

end.
