unit ConductorData;

{
  ----------------------------------------------------------
  Copyright (c) 2008-2020, Electric Power Research Institute, Inc.
  All rights reserved.
  ----------------------------------------------------------
}

interface

{The ConductorData object is a general DSS object used by all circuits
 as a reference for obtaining line impedances.

 The values are set by the normal New and Edit procedures for any DSS object.

 The values are retrieved by setting the Code Property in the ConductorData Class.
 This sets the active ConductorData object to be the one referenced by the Code Property;

 Then the values of that code can be retrieved via the public variables.
 }

uses
    Classes,
    Command,
    DSSClass,
    DSSObject,
    ArrayDef;

type
    ConductorChoice = (Overhead, ConcentricNeutral, TapeShield, Unknown);

    ConductorChoiceArray = array[1..100] of ConductorChoice;
    pConductorChoiceArray = ^ConductorChoiceArray;

    TConductorData = class(TDSSClass)
    PRIVATE

    PROTECTED
        procedure CountProperties;
        procedure DefineProperties;
        procedure ClassEdit(const ActiveObj: Pointer; const ParamPointer: Integer);
        procedure ClassMakeLike(const OtherObj: Pointer);
    PUBLIC
        NumConductorClassProps: Integer;
        constructor Create(dssContext: TDSSContext);
        destructor Destroy; OVERRIDE;
    end;

    TConductorDataObj = class(TDSSObject)
{$IFDEF DSS_CAPI}
    PUBLIC
{$ELSE}
    PRIVATE
{$ENDIF}
        FRDC: Double;
        FR60: Double;
        FGMR60: Double;
        Fcapradius60: Double;  // in case it is different than radius for cap calcs
        Fradius: Double;
        FGMRUnits: Integer;
        FResistanceUnits: Integer;
        FRadiusUnits: Integer;
    PUBLIC
        NormAmps: Double;
        EmergAmps: Double;
        NumAmpRatings: Integer;
        AmpRatings: array of Double;

        constructor Create(ParClass: TDSSClass; const ConductorDataName: String);
        destructor Destroy; OVERRIDE;

        property Rdc: Double READ FRDC;
        property Rac: Double READ FR60;
        property GMR: Double READ FGMR60;
        Property CapRadius: Double Read Fcapradius60;
        property Radius: Double READ FRadius;
        property ResUnits: Integer READ FresistanceUnits;
        property RadiusUnits: Integer READ FradiusUnits;
        property GMRUnits: Integer READ FGMRUnits;

        procedure InitPropertyValues(ArrayOffset: Integer); OVERRIDE;
        procedure DumpProperties(F: TFileStream; Complete: Boolean); OVERRIDE;
        function GetPropertyValue(Index: Integer): String; OVERRIDE;
    end;

    TConductorDataArray = array[1..100] of TConductorDataObj;
    pConductorDataArray = ^TConductorDataArray;

implementation

uses
    ParserDel,
    DSSGlobals,
    DSSClassDefs,
    Sysutils,
    Ucomplex,
    LineUNits,
    Utilities,
    DSSHelper,
    DSSObjectHelper,
    TypInfo;

const
    LineUnitsHelp = '{mi|kft|km|m|Ft|in|cm|mm} Default=none.';

//- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
constructor TConductorData.Create(dssContext: TDSSContext);  // Creates superstructure for all Line objects
begin
    NumConductorClassProps := 13;
    inherited Create(dssContext);
    DSSClassType := DSS_OBJECT;
end;

destructor TConductorData.Destroy;
begin
    inherited Destroy;
end;

procedure TConductorData.CountProperties;
begin
    NumProperties := NumProperties + NumConductorClassProps;
    inherited CountProperties;
end;

procedure TConductorData.DefineProperties;
begin
    PropertyName^[ActiveProperty + 1] := 'Rdc';
    PropertyName^[ActiveProperty + 2] := 'Rac';
    PropertyName^[ActiveProperty + 3] := 'Runits';
    PropertyName^[ActiveProperty + 4] := 'GMRac';
    PropertyName^[ActiveProperty + 5] := 'GMRunits';
    PropertyName^[ActiveProperty + 6] := 'radius';
    PropertyName^[ActiveProperty + 7] := 'radunits';
    PropertyName^[ActiveProperty + 8] := 'normamps';
    PropertyName^[ActiveProperty + 9] := 'emergamps';
    PropertyName^[ActiveProperty + 10] := 'diam';
    PropertyName^[ActiveProperty + 11] := 'Seasons';
    PropertyName^[ActiveProperty + 12] := 'Ratings';
    PropertyName^[ActiveProperty + 13] := 'Capradius';

    PropertyHelp^[ActiveProperty + 1] := 'dc Resistance, ohms per unit length (see Runits). Defaults to Rac/1.02 if not specified.';
    PropertyHelp^[ActiveProperty + 2] := 'Resistance at 60 Hz per unit length. Defaults to 1.02*Rdc if not specified.';
    PropertyHelp^[ActiveProperty + 3] := 'Length units for resistance: ohms per ' + LineUnitsHelp;
    PropertyHelp^[ActiveProperty + 4] := 'GMR at 60 Hz. Defaults to .7788*radius if not specified.';
    PropertyHelp^[ActiveProperty + 5] := 'Units for GMR: ' + LineUnitsHelp;
    PropertyHelp^[ActiveProperty + 6] := 'Outside radius of conductor. Defaults to GMR/0.7788 if not specified.';
    PropertyHelp^[ActiveProperty + 7] := 'Units for outside radius: ' + LineUnitsHelp;
    PropertyHelp^[ActiveProperty + 8] := 'Normal ampacity, amperes. Defaults to Emergency amps/1.5 if not specified.';
    PropertyHelp^[ActiveProperty + 9] := 'Emergency ampacity, amperes. Defaults to 1.5 * Normal Amps if not specified.';
    PropertyHelp^[ActiveProperty + 10] := 'Diameter; Alternative method for entering radius.';
    PropertyHelp^[ActiveProperty + 11] := 'Defines the number of ratings to be defined for the wire, to be used only when defining seasonal ratings using the "Ratings" property.';
    PropertyHelp^[ActiveProperty + 12] := 'An array of ratings to be used when the seasonal ratings flag is True. It can be used to insert' +
        CRLF + 'multiple ratings to change during a QSTS simulation to evaluate different ratings in lines.';
    PropertyHelp^[ActiveProperty + 13] := 'Equivalent conductor radius for capacitance calcs. Specify this for bundled conductors. Defaults to same value as radius.';

    ActiveProperty := ActiveProperty + NumConductorClassProps;
    inherited DefineProperties;
end;

//- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
procedure TConductorData.ClassEdit(const ActiveObj: Pointer; const ParamPointer: Integer);
var
    Param: String;
begin
  // continue parsing with contents of Parser
    if ParamPointer > 0 then
        with TConductorDataObj(ActiveObj) do
        begin
            case ParamPointer of
                1:
                    FRDC := Parser.Dblvalue;
                2:
                    FR60 := Parser.DblValue;
                3:
                    FresistanceUnits := GetUnitsCode(Parser.StrValue);
                4:
                    FGMR60 := Parser.DblValue;
                5:
                    FGMRUnits := GetUnitsCode(Parser.StrValue);
                6:
                    Fradius := Parser.DblValue;
                7:
                    FRadiusUnits := GetUnitsCode(Parser.StrValue);
                8:
                    NormAmps := Parser.DblValue;
                9:
                    EmergAmps := Parser.DblValue;
                10:
                    Fradius := Parser.DblValue / 2.0;
                11:
                begin
                    NumAmpRatings := Parser.IntValue;
                    setlength(AmpRatings, NumAmpRatings);
                end;
                12:
                begin
                    setlength(AmpRatings, NumAmpRatings);
                    Param := Parser.StrValue;
                    NumAmpRatings := InterpretDblArray(Param, NumAmpRatings, pointer(AmpRatings));
                end;
                13:
                    Fcapradius60 := Parser.DblValue;
            else
                inherited ClassEdit(ActiveObj, ParamPointer - NumConductorClassProps)
            end;
      {Set defaults}
            case ParamPointer of
                1:
                    if FR60 < 0.0 then
                        FR60 := 1.02 * FRDC;
                2:
                    if FRDC < 0.0 then
                        FRDC := FR60 / 1.02;
                4: // GMRac
                begin
                    if Fradius < 0.0 then
                        Fradius := FGMR60 / 0.7788;
                end;
                5:
                    if FradiusUnits = 0 then
                        FradiusUnits := FGMRunits;
                6, 10: // radius, diam
                begin
                    if FGMR60 < 0.0 then
                        FGMR60 := 0.7788 * FRadius;
                    if Fcapradius60 < 0.0 then
                        Fcapradius60 := Fradius;    // default to radius
                end;
                7:
                    if FGMRUnits = 0 then
                        FGMRunits := FradiusUnits;
                8:
                    if EmergAmps < 0.0 then
                        EmergAmps := 1.5 * NormAmps;
                9:
                    if NormAmps < 0.0 then
                        NormAmps := EmergAmps / 1.5;
            end;
      {Check for critical errors}
            case ParamPointer of
                4:
                    if (Fradius = 0.0) then
                        DoSimpleMsg('Error: Radius is specified as zero for ConductorData.' + Name, 999);
                6:
                    if (FGMR60 = 0.0) then
                        DoSimpleMsg('Error: GMR is specified as zero for ConductorData.' + Name, 999);
            end;
        end;
end;

procedure TConductorData.ClassMakeLike(const OtherObj: Pointer);
var
    OtherConductorData: TConductorDataObj;
begin
    OtherConductorData := TConductorDataObj(OtherObj);
    with TConductorDataObj(ActiveDSSObject) do
    begin
        FRDC := OtherConductorData.FRDC;
        FR60 := OtherConductorData.FR60;
        FResistanceUnits := OtherConductorData.FResistanceUnits;
        FGMR60 := OtherConductorData.FGMR60;
        Fcapradius60 := OtherConductorData.Fcapradius60;
        FGMRUnits := OtherConductorData.FGMRUnits;
        FRadius := OtherConductorData.FRadius;
        FRadiusUnits := OtherConductorData.FRadiusUnits;
        NormAmps := OtherConductorData.NormAmps;
        EmergAmps := OtherConductorData.EmergAmps;
    end;
  // Inherited ClassMakeLike(OtherObj);
end;

//- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
//      TConductorData Obj
//- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

constructor TConductorDataObj.Create(ParClass: TDSSClass; const ConductorDataName: String);

begin
    inherited Create(ParClass);
    Name := LowerCase(ConductorDataName);
    DSSObjType := ParClass.DSSClassType;

    FRDC := -1.0;
    FR60 := -1.0;
    FGMR60 := -1.0;
    Fradius := -1.0;
    Fcapradius60 := -1.0;   // init to not defined
    FGMRUnits := 0;
    FResistanceUnits := 0;
    FRadiusUnits := 0;
    Normamps := -1.0;
    EmergAmps := -1.0;
    NumAmpRatings := 1;
    setlength(AmpRatings, NumAmpRatings);
    AmpRatings[0] := NormAmps;
end;

//- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
destructor TConductorDataObj.Destroy;
begin
    inherited destroy;
end;

procedure TConductorDataObj.DumpProperties(F: TFileStream; Complete: Boolean);
var
    j,
    i: Integer;
    TempStr: String;
begin
    inherited DumpProperties(F, Complete);
    with ParentClass do
    begin
        for i := 1 to NumProperties do
        begin
            FSWrite(F, '~ ' + PropertyName^[i] + '=');
            case i of
                1:
                    FSWriteln(F, Format('%.6g', [FRDC]));
                2:
                    FSWriteln(F, Format('%.6g', [FR60]));
                3:
                    FSWriteln(F, Format('%s', [LineUnitsStr(FresistanceUnits)]));
                4:
                    FSWriteln(F, Format('%.6g', [FGMR60]));
                5:
                    FSWriteln(F, Format('%s', [LineUnitsStr(FGMRUnits)]));
                6:
                    FSWriteln(F, Format('%.6g', [Fradius]));
                7:
                    FSWriteln(F, Format('%s', [LineUnitsStr(FRadiusUnits)]));
                8:
                    FSWriteln(F, Format('%.6g', [NormAmps]));
                9:
                    FSWriteln(F, Format('%.6g', [EmergAmps]));
                10:
                    FSWriteln(F, Format('%.6g', [radius * 2.0]));
                11:
                    FSWriteln(F, Format('%d', [NumAmpRatings]));
                12:
                begin
                    TempStr := '[';
                    for  j := 1 to NumAmpRatings do
                        TempStr := TempStr + floattoStrf(AmpRatings[j - 1], ffgeneral, 8, 4) + ',';
                    TempStr := TempStr + ']';
                    FSWriteln(F, TempStr);
                end;
                13: 
                    FSWriteln(F, Format('%.6g',[Fcapradius60]));
            end;
        end;
    end;
end;

function TConductorDataObj.GetPropertyValue(Index: Integer): String;
Var
    j: Integer;
    Tempstr: String;
begin
    Result := '';
    case Index of  // Special cases
        1: 
            Result := Format('%.6g', [FRDC]);
        2: 
            Result := Format('%.6g', [FR60]);
        3: 
            Result := Format('%s', [LineUnitsStr(FresistanceUnits)]);
        4: 
            Result := Format('%.6g', [FGMR60]);
        5: 
            Result := Format('%s', [LineUnitsStr(FGMRUnits)]);
        6: 
            Result := Format('%.6g', [Fradius]);
        7: 
            Result := Format('%s', [LineUnitsStr(FRadiusUnits)]);
        8: 
            Result := Format('%.6g', [NormAmps]);
        9: 
            Result := Format('%.6g', [EmergAmps]);
        10: 
            Result := Format('%.6g', [radius*2.0]);
        11: 
            Result := Format('%d', [NumAmpRatings]);
        12: 
        begin
            TempStr := '[';

            for j := 1 to NumAmpRatings do
                TempStr := TempStr + FloatToStrf(AmpRatings[j-1], ffgeneral, 8, 4) + ',';

            TempStr := TempStr + ']';
            Result := TempStr;
        end;
        13: 
            Result := Format('%.6g',[Fcapradius60]);
    else
        Result := Inherited GetPropertyValue(index);
    end;
end;

procedure TConductorDataObj.InitPropertyValues(ArrayOffset: Integer);
begin
    PropertyValue[ArrayOffset + 1] := '-1';
    PropertyValue[ArrayOffset + 2] := '-1';
    PropertyValue[ArrayOffset + 3] := 'none';
    PropertyValue[ArrayOffset + 4] := '-1';
    PropertyValue[ArrayOffset + 5] := 'none';
    PropertyValue[ArrayOffset + 6] := '-1';
    PropertyValue[ArrayOffset + 7] := 'none';
    PropertyValue[ArrayOffset + 8] := '-1';
    PropertyValue[ArrayOffset + 9] := '-1';
    PropertyValue[ArrayOffset + 10] := '-1';
    PropertyValue[ArrayOffset + 11] := '1';
    PropertyValue[ArrayOffset + 12] := '[-1]';
    PropertyValue[ArrayOffset + 13] := '-1';
    inherited InitPropertyValues(ArrayOffset + 13);
end;

end.
