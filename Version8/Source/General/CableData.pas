unit CableData;

{
  ----------------------------------------------------------
  Copyright (c) 2008-2015, Electric Power Research Institute, Inc.
  All rights reserved.
  ----------------------------------------------------------
}
interface

uses
    Command,
    DSSClass,
    DSSObject,
    ConductorData;

type
    TCableData = class(TConductorData)
    PRIVATE

    PROTECTED
        procedure CountProperties;
        procedure DefineProperties;
        function ClassEdit(const ActiveObj: Pointer; const ParamPointer: Integer): Integer;
        procedure ClassMakeLike(const OtherObj: Pointer);
    PUBLIC
        NumCableClassProps: Integer;
        constructor Create;
        destructor Destroy; OVERRIDE;
    end;

    TCableDataObj = class(TConductorDataObj)
    PRIVATE
        FEpsR: Double;
        // next 3 use parent RadiusUnits
        FInsLayer: Double;
        FDiaIns: Double;
        FDiaCable: Double;
    PUBLIC
        constructor Create(ParClass: TDSSClass; const CableDataName: String);
        destructor Destroy; OVERRIDE;

        property EpsR: Double READ FEpsR;
        property DiaIns: Double READ FDiaIns;
        property DiaCable: Double READ FDiaCable;
        property InsLayer: Double READ FInsLayer;

        procedure InitPropertyValues(ArrayOffset: Integer); OVERRIDE;
        procedure DumpProperties(var F: TextFile; Complete: Boolean); OVERRIDE;
    end;

implementation

uses
    ParserDel,
    DSSGlobals,
    DSSClassDefs,
    Sysutils,
    Ucomplex,
    Arraydef,
    LineUnits;

constructor TCableData.Create;  // Creates superstructure for all Line objects
begin
    inherited Create;
    NumCableClassProps := 4;
    DSSClassType := DSS_OBJECT;
end;

destructor TCableData.Destroy;
begin
    inherited Destroy;
end;

procedure TCableData.CountProperties;
begin
    NumProperties := NumProperties + NumCableClassProps;
    inherited CountProperties;
end;

procedure TCableData.DefineProperties;
begin
    PropertyName^[ActiveProperty + 1] := 'EpsR';
    PropertyName^[ActiveProperty + 2] := 'InsLayer';
    PropertyName^[ActiveProperty + 3] := 'DiaIns';
    PropertyName^[ActiveProperty + 4] := 'DiaCable';

    PropertyHelp^[ActiveProperty + 1] := 'Insulation layer relative permittivity; default is 2.3.';
    PropertyHelp^[ActiveProperty + 2] := 'Insulation layer thickness; same units as radius; no default. ' +
        'With DiaIns, establishes inner radius for capacitance calculation.';
    PropertyHelp^[ActiveProperty + 3] := 'Diameter over insulation layer; same units as radius; no default. ' +
        'Establishes outer radius for capacitance calculation.';
    PropertyHelp^[ActiveProperty + 4] := 'Diameter over cable; same units as radius; no default.';

    ActiveProperty := ActiveProperty + NumCableClassProps;
    inherited DefineProperties;
end;

//- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
function TCableData.ClassEdit(const ActiveObj: Pointer; const ParamPointer: Integer): Integer;
begin
    Result := 0;
  // continue parsing with contents of Parser
    if ParamPointer > 0 then
        with TCableDataObj(ActiveObj) do
        begin
            case ParamPointer of
                1:
                    FEpsR := Parser[ActiveActor].Dblvalue;
                2:
                    FInsLayer := Parser[ActiveActor].DblValue;
                3:
                    FDiaIns := Parser[ActiveActor].DblValue;
                4:
                    FDiaCable := Parser[ActiveActor].DblValue;
            else
                inherited ClassEdit(ActiveObj, ParamPointer - NumCableClassProps)
            end;
      {Check for critical errors}
            case ParamPointer of
                1:
                    if (FEpsR < 1.0) then
                        DoSimpleMsg('Error: Insulation permittivity must be greater than one for CableData ' + Name, 999);
                2:
                    if (FInsLayer <= 0.0) then
                        DoSimpleMsg('Error: Insulation layer thickness must be positive for CableData ' + Name, 999);
                3:
                    if (FDiaIns <= 0.0) then
                        DoSimpleMsg('Error: Diameter over insulation layer must be positive for CableData ' + Name, 999);
                4:
                    if (FDiaCable <= 0.0) then
                        DoSimpleMsg('Error: Diameter over cable must be positive for CableData ' + Name, 999);
            end;
        end;
end;

procedure TCableData.ClassMakeLike(const OtherObj: Pointer);
var
    OtherCableData: TCableDataObj;
begin
    OtherCableData := TCableDataObj(OtherObj);
    with TCableDataObj(ActiveDSSObject[ActiveActor]) do
    begin
        FEpsR := OtherCableData.FEpsR;
        FInsLayer := OtherCableData.FInsLayer;
        FDiaIns := OtherCableData.FDiaIns;
        FDiaCable := OtherCableData.FDiaCable;
    end;
    inherited ClassMakeLike(OtherObj);
end;

//- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
//      TConductorData Obj
//- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

constructor TCableDataObj.Create(ParClass: TDSSClass; const CableDataName: String);

begin
    inherited Create(ParClass, CableDataName);
    Name := LowerCase(CableDataName);
    DSSObjType := ParClass.DSSClassType;

    FEpsR := 2.3;
    FInsLayer := -1.0;
    FDiaIns := -1.0;
    FDiaCable := -1.0;
end;

//- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
destructor TCableDataObj.Destroy;
begin
    inherited destroy;
end;

procedure TCableDataObj.DumpProperties(var F: TextFile; Complete: Boolean);
var
    i: Integer;
begin
    inherited DumpProperties(F, Complete);
    with ParentClass do
    begin
        for i := 1 to NumProperties do
        begin
            Write(F, '~ ', PropertyName^[i], '=');
            case i of
                1:
                    Writeln(F, Format('%.3g', [FEpsR]));
                2:
                    Writeln(F, Format('%.6g', [FInsLayer]));
                3:
                    Writeln(F, Format('%.6g', [FDiaIns]));
                4:
                    Writeln(F, Format('%.6g', [FDiaCable]));
            end;
        end;
    end;
end;

procedure TCableDataObj.InitPropertyValues(ArrayOffset: Integer);
begin
    PropertyValue[ArrayOffset + 1] := '2.3';
    PropertyValue[ArrayOffset + 2] := '-1';
    PropertyValue[ArrayOffset + 3] := '-1';
    PropertyValue[ArrayOffset + 4] := '-1';
    inherited InitPropertyValues(ArrayOffset + 4);
end;

end.
