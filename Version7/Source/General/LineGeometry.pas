unit LineGeometry;

{
  ----------------------------------------------------------
  Copyright (c) 2008-2015, Electric Power Research Institute, Inc.
  All rights reserved.
  ----------------------------------------------------------
}

interface

{The LineGeometry object is a general DSS object used by all circuits
 as a reference for obtaining line impedances.

 The values are set by the normal New and Edit procedures for any DSS object.

 The values are retrieved by setting the Code Property in the LineGeometry Class.
 This sets the active LineGeometry object to be the one referenced by the Code Property;

 Then the values of that code can be retrieved via the public variables.

 }

uses
    Sysutils,
    Arraydef,
    Command,
    DSSClass,
    DSSObject,
    uCMatrix,
    LineConstants,
    ConductorData,
    CNData,
    TSData,
    LineSpacing;

type
    ELineGeometryProblem = class(Exception);

    TLineGeometry = class(TDSSClass)
    PRIVATE

        function Get_Code: String;  // Returns active line code string
        procedure Set_Code(const Value: String);  // sets the  active LineGeometry

    PROTECTED
        procedure DefineProperties;
        function MakeLike(const LineName: String): Integer; OVERRIDE;
    PUBLIC

        constructor Create(dss: TDSS);
        destructor Destroy; OVERRIDE;

        function Edit: Integer; OVERRIDE;     // uses global parser
        function Init(Handle: Integer): Integer; OVERRIDE;
        function NewObject(const ObjName: String): Integer; OVERRIDE;


       // Set this property to point ActiveLineGeometryObj to the right value
        property Code: String READ Get_Code WRITE Set_Code;

    end;


    TLineGeometryObj = class(TDSSObject)
{$IFNDEF DSS_CAPI}
    PRIVATE
{$ELSE}
    PUBLIC
{$ENDIF}
        FPhaseChoice: pConductorChoiceArray;
        FNConds: Integer;
        FNPhases: Integer;
        FCondName: pStringArray;
        FWireData: pConductorDataArray;
        FX: pDoubleArray;
        FY: pDoubleArray;
        FUnits: pIntegerArray;
        FLastUnit: Integer;
{$IFNDEF DSS_CAPI}
        DataChanged: Boolean;
        FReduce: Boolean;
{$ENDIF}
        FActiveCond: Integer;
        FSpacingType: String;

        FLineData: TLineConstants;
        procedure ChangeLineConstantsType(newPhaseChoice: ConductorChoice);

        procedure set_Nconds(const Value: Integer);
        procedure set_Nphases(const Value: Integer);
        procedure set_ActiveCond(const Value: Integer);
        function Get_YCmatrix(f, Lngth: Double; Units: Integer): Tcmatrix;
        function Get_Zmatrix(f, Lngth: Double; Units: Integer): Tcmatrix;
        function Get_RhoEarth: Double;
        procedure Set_RhoEarth(const Value: Double);
        function get_Nconds: Integer;
        procedure UpdateLineGeometryData(f: Double);   // call this before using the line data

        // CIM Accessors
        function Get_FX(i: Integer): Double;
        function Get_FY(i: Integer): Double;
        function Get_FUnits(i: Integer): Integer;
        function Get_ConductorName(i: Integer): String;
        function Get_ConductorData(i: Integer): TConductorDataObj;
{$IFDEF DSS_CAPI}
        procedure Set_FX(i: Integer; Value: Double);
        procedure Set_FY(i: Integer; Value: Double);
        procedure Set_FUnits(i: Integer; Value: Integer);
{$ENDIF}
        function Get_PhaseChoice(i: Integer): ConductorChoice;

    PUBLIC

{$IFDEF DSS_CAPI}
        DataChanged: Boolean;
        FReduce: Boolean;
{$ENDIF}
        NormAmps: Double;
        EmergAmps: Double;
        NumAmpRatings: Integer;
        AmpRatings: array of Double;

        constructor Create(ParClass: TDSSClass; const LineGeometryName: String);
        destructor Destroy; OVERRIDE;

        function GetPropertyValue(Index: Integer): String; OVERRIDE;
        procedure InitPropertyValues(ArrayOffset: Integer); OVERRIDE;
        procedure DumpProperties(var F: TextFile; Complete: Boolean); OVERRIDE;
        procedure SaveWrite(var F: TextFile); OVERRIDE;

        // called from a Line object that has its own Spacing and Wires input
        // automatically sets reduce=y if the spacing has more wires than phases
        procedure LoadSpacingAndWires(Spc: TLineSpacingObj; Wires: pConductorDataArray);

        property Nconds: Integer READ get_Nconds WRITE set_Nconds;
        property Nphases: Integer READ FNphases WRITE set_Nphases;
        property ActiveCond: Integer READ FActiveCond WRITE set_ActiveCond;
        property Zmatrix[f, Lngth: Double; Units: Integer]: Tcmatrix READ Get_Zmatrix;
        property YCmatrix[f, Lngth: Double; Units: Integer]: Tcmatrix READ Get_YCmatrix;
        property RhoEarth: Double READ Get_RhoEarth WRITE Set_RhoEarth;

        // CIM XML accessors
        property Xcoord[i: Integer]: Double READ Get_FX
{$IFDEF DSS_CAPI}
            WRITE Set_FX
{$ENDIF}
        ;
        property Ycoord[i: Integer]: Double READ Get_FY
{$IFDEF DSS_CAPI}
            WRITE Set_FY
{$ENDIF}
        ;
        property Units[i: Integer]: Integer READ Get_FUnits
{$IFDEF DSS_CAPI}
            WRITE Set_FUnits
{$ENDIF}
        ;
        property ConductorName[i: Integer]: String READ Get_ConductorName;
        property ConductorData[i: Integer]: TConductorDataObj READ Get_ConductorData;
        property NWires: Integer READ FNConds;
        property PhaseChoice[i: Integer]: ConductorChoice READ Get_PhaseChoice;
    end;

var
    ActiveLineGeometryObj: TLineGeometryObj;

implementation

uses
    ParserDel,
    DSSClassDefs,
    DSSGlobals,
    Ucomplex,
    Utilities,
    LineUnits,
    OHLineConstants,
    CNLineConstants,
    TSLineConstants,
    DSSHelper;

const
    NumPropsThisClass = 18;

//- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
constructor TLineGeometry.Create(dss: TDSS);  // Creates superstructure for all Line objects
begin
    inherited Create(dss);
    Class_Name := 'LineGeometry';
    DSSClassType := DSS_OBJECT;
    ActiveElement := 0;

    DefineProperties;

    CommandList := TCommandList.Create(Slice(PropertyName^, NumProperties));
    CommandList.Abbrev := TRUE;
end;

//- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
destructor TLineGeometry.Destroy;

begin
    // ElementList and  CommandList freed in inherited destroy
    inherited Destroy;
end;
//- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
procedure TLineGeometry.DefineProperties;
begin

    Numproperties := NumPropsThisClass;
    CountProperties;   // Get inherited property count
    AllocatePropertyArrays;


    PropertyName[1] := 'nconds';
    PropertyName[2] := 'nphases';
    PropertyName[3] := 'cond';
    PropertyName[4] := 'wire';
    PropertyName[5] := 'x';
    PropertyName[6] := 'h';
    PropertyName[7] := 'units';
    PropertyName[8] := 'normamps';
    PropertyName[9] := 'emergamps';
    PropertyName[10] := 'reduce';
    PropertyName[11] := 'spacing';
    PropertyName[12] := 'wires';
    PropertyName[13] := 'cncable';
    PropertyName[14] := 'tscable';
    PropertyName[15] := 'cncables';
    PropertyName[16] := 'tscables';
    PropertyName[17] := 'Seasons';
    PropertyName[18] := 'Ratings';

    PropertyHelp[1] := 'Number of conductors in this geometry. Default is 3. Triggers memory allocations. Define first!';
    PropertyHelp[2] := 'Number of phases. Default =3; All other conductors are considered neutrals and might be reduced out.';
    PropertyHelp[3] := 'Set this = number of the conductor you wish to define. Default is 1.';
    PropertyHelp[4] := 'Code from WireData. MUST BE PREVIOUSLY DEFINED. no default.' + CRLF +
        'Specifies use of Overhead Line parameter calculation,' + CRLF +
        'Unless Tape Shield cable previously assigned to phases, and this wire is a neutral.';
    PropertyHelp[5] := 'x coordinate.';
    PropertyHelp[6] := 'Height of conductor.';
    PropertyHelp[7] := 'Units for x and h: {mi|kft|km|m|Ft|in|cm } Initial default is "ft", but defaults to last unit defined';
    PropertyHelp[8] := 'Normal ampacity, amperes for the line. Defaults to first conductor if not specified.';
    PropertyHelp[9] := 'Emergency ampacity, amperes. Defaults to first conductor if not specified.';
    PropertyHelp[10] := '{Yes | No} Default = no. Reduce to Nphases (Kron Reduction). Reduce out neutrals.';
    PropertyHelp[11] := 'Reference to a LineSpacing for use in a line constants calculation.' + CRLF +
        'Alternative to x, h, and units. MUST BE PREVIOUSLY DEFINED.' + CRLF +
        'Must match "nconds" as previously defined for this geometry.' + CRLF +
        'Must be used in conjunction with the Wires property.';
    PropertyHelp[12] := 'Array of WireData names for use in a line constants calculation.' + CRLF +
        'Alternative to individual wire inputs. ALL MUST BE PREVIOUSLY DEFINED.' + CRLF +
        'Must match "nconds" as previously defined for this geometry,' + CRLF +
        'unless TSData or CNData were previously assigned to phases, and these wires are neutrals.' + CRLF +
        'Must be used in conjunction with the Spacing property.';
    PropertyHelp[13] := 'Code from CNData. MUST BE PREVIOUSLY DEFINED. no default.' + CRLF +
        'Specifies use of Concentric Neutral cable parameter calculation.';
    PropertyHelp[14] := 'Code from TSData. MUST BE PREVIOUSLY DEFINED. no default.' + CRLF +
        'Specifies use of Tape Shield cable parameter calculation.';
    PropertyHelp[15] := 'Array of CNData names for cable parameter calculation.' + CRLF +
        'All must be previously defined, and match "nphases" for this geometry.' + CRLF +
        'You can later define "nconds-nphases" wires for bare neutral conductors.';
    PropertyHelp[16] := 'Array of TSData names for cable parameter calculation.' + CRLF +
        'All must be previously defined, and match "nphases" for this geometry.' + CRLF +
        'You can later define "nconds-nphases" wires for bare neutral conductors.';
    PropertyHelp[17] := 'Defines the number of ratings to be defined for the wire, to be used only when defining seasonal ratings using the "Ratings" property.';
    PropertyHelp[18] := 'An array of ratings to be used when the seasonal ratings flag is True. It can be used to insert' +
        CRLF + 'multiple ratings to change during a QSTS simulation to evaluate different ratings in lines.';

    ActiveProperty := NumPropsThisClass;
    inherited DefineProperties;  // Add defs of inherited properties to bottom of list

end;
//- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
function TLineGeometry.NewObject(const ObjName: String): Integer;
begin
   // create a new object of this class and add to list
    with DSS.ActiveCircuit do
    begin
        ActiveDSSObject := TLineGeometryObj.Create(Self, ObjName);
        Result := AddObjectToList(ActiveDSSObject);
    end;
end;


//- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
function TLineGeometry.Edit: Integer;
var
    ParamPointer: Integer;
    ParamName: String;
    Param: String;
    i, istart, istop: Integer;

begin
    Result := 0;
  // continue parsing with contents of Parser
    ActiveLineGeometryObj := ElementList.Active;
    ActiveDSSObject := ActiveLineGeometryObj;

    with ActiveLineGeometryObj do
    begin

        ParamPointer := 0;
        ParamName := Parser.NextParam;
        Param := Parser.StrValue;
        while Length(Param) > 0 do
        begin
            if Length(ParamName) = 0 then
                Inc(ParamPointer)
            else
                ParamPointer := CommandList.GetCommand(ParamName);

            if (ParamPointer > 0) and (ParamPointer <= NumProperties) then
                PropertyValue[ParamPointer] := Param;

            case ParamPointer of
                0:
                    DoSimpleMsg('Unknown parameter "' + ParamName + '" for Object "' + Class_Name + '.' + Name + '"', 10101);
                1:
                    NConds := Parser.IntValue;  // Use property value to force reallocations
                2:
                    FNphases := Parser.IntValue;
                3:
                    ActiveCond := Parser.IntValue;
                4:
                begin
                    FCondName^[ActiveCond] := Param;
                    if FPhaseChoice^[ActiveCond] = Unknown then
                        ChangeLineConstantsType(Overhead);
                end;
                5:
                    FX^[ActiveCond] := Parser.DblValue;
                6:
                    FY^[ActiveCond] := Parser.DblValue;
                7:
                begin
                    FUnits^[ActiveCond] := GetUnitsCode(Param);
                    FLastUnit := FUnits^[ActiveCond];
                end;
                8:
                    NormAmps := Parser.DblValue;
                9:
                    EmergAmps := Parser.DblValue;
                10:
                    Freduce := InterpretYesNo(Param);
                11:
                begin
                    FSpacingType := Parser.StrValue;
                    if DSS.LineSpacingClass.SetActive(FSpacingType) then
                    begin
                        ActiveLineSpacingObj := DSS.LineSpacingClass.GetActiveObj;
                        if (FNConds = ActiveLineSpacingObj.NWires) then
                        begin
                            FLastUnit := ActiveLineSpacingObj.Units;
                            for i := 1 to FNConds do
                            begin
                                FX^[i] := ActiveLineSpacingObj.Xcoord[i];
                                FY^[i] := ActiveLineSpacingObj.Ycoord[i];
                                FUnits^[i] := FLastUnit;
                            end
                        end
                        else
                            DoSimpleMsg('LineSpacing object ' + FSpacingType + ' has the wrong number of wires.', 10103);
                    end
                    else
                        DoSimpleMsg('LineSpacing object ' + FSpacingType + ' has not been defined.', 10103);
                end;
                13:
                begin
                    FCondName^[ActiveCond] := Param;
                    ChangeLineConstantsType(ConcentricNeutral)
                end;
                14:
                begin
                    FCondName^[ActiveCond] := Param;
                    ChangeLineConstantsType(TapeShield)
                end;
                12, 15, 16:
                begin
                    istart := 1;
                    istop := FNConds;
                    if ParamPointer = 15 then
                    begin
                        ChangeLineConstantsType(ConcentricNeutral);
                        istop := FNPhases;
                    end
                    else
                    if ParamPointer = 16 then
                    begin
                        ChangeLineConstantsType(TapeShield);
                        istop := FNPhases;
                    end
                    else
                    if ParamPointer = 12 then
                    begin
                        if FPhaseChoice^[ActiveCond] = Unknown then
                            ChangeLineConstantsType(Overhead)
                        else if FPhaseChoice^[ActiveCond] <> Overhead then
                            // these are buried neutral wires 
                            // (only when the phase conductors not overhead)
                            istart := FNPhases + 1;
                    end;

                    AuxParser.CmdString := Parser.StrValue;
                    for i := istart to istop do
                    begin
                        AuxParser.NextParam; // ignore any parameter name  not expecting any
                        FCondName[i] := AuxParser.StrValue;
                        if ParamPointer = 15 then
                            DSS.CNDataClass.code := FCondName[i]
                        else
                        if ParamPointer = 16 then
                            DSS.TSDataClass.code := FCondName[i]
                        else
                            DSS.WireDataClass.Code := FCondName[i];
                        if Assigned(ActiveConductorDataObj) then
                        begin
                            FWireData^[i] := ActiveConductorDataObj;
                            if (i = 1) then
                            begin
                                if (ActiveConductorDataObj.NormAmps > 0.0) then
                                    Normamps := ActiveConductorDataObj.NormAmps;
                                if (ActiveConductorDataObj.Emergamps > 0.0) then
                                    Emergamps := ActiveConductorDataObj.EmergAmps;
                            end;
                        end
                        else
                        if ParamPointer = 15 then
                            DoSimpleMsg('CNData Object "' + FCondName[i] + '" not defined. Must be previously defined.', 10103)
                        else
                        if ParamPointer = 16 then
                            DoSimpleMsg('TSData Object "' + FCondName[i] + '" not defined. Must be previously defined.', 10103)
                        else
                            DoSimpleMsg('WireData Object "' + FCondName[i] + '" not defined. Must be previously defined.', 10103);
                    end;
                    FActiveCond := istop;
                end;
                17:
                begin
                    NumAmpRatings := Parser.IntValue;
                    setlength(AmpRatings, NumAmpRatings);
                end;
                18:
                begin
                    setlength(AmpRatings, NumAmpRatings);
                    Param := Parser.StrValue;
                    NumAmpRatings := InterpretDblArray(Param, NumAmpRatings, Pointer(AmpRatings));
                end
            else
           // Inherited parameters
                ClassEdit(ActiveLineGeometryObj, Parampointer - NumPropsThisClass)
            end;

         {Set defaults}
            case ParamPointer of

                2:
                    if (FNPhases > FNconds) then
                        FNPhases := FNConds;
                3:
                    if (ActiveCond < 1) or (ActiveCond > FNconds) then
                        DoSimpleMsg('Illegal cond= specification in Line Geometry:' + CRLF + Parser.cmdstring, 10102);
                4, 13, 14:
                begin
                    if ParamPointer = 4 then
                        DSS.WireDataClass.code := Param
                    else
                    if ParamPointer = 13 then
                        DSS.CNDataClass.code := Param
                    else
                        DSS.TSDataClass.Code := Param;
                    if Assigned(ActiveConductorDataObj) then
                    begin
                        FWireData^[ActiveCond] := ActiveConductorDataObj;
                  {Default the current ratings for this geometry to the rating of the first conductor}
                        if (ActiveCond = 1) then
                        begin
                            if (ActiveConductorDataObj.NormAmps > 0.0) then
                                Normamps := ActiveConductorDataObj.NormAmps;
                            if (ActiveConductorDataObj.Emergamps > 0.0) then
                                Emergamps := ActiveConductorDataObj.EmergAmps;
                        end;
                    end
                    else
                    if ParamPointer = 4 then
                        DoSimpleMsg('WireData Object "' + param + '" not defined. Must be previously defined.', 10103)
                    else
                    if ParamPointer = 13 then
                        DoSimpleMsg('CNData Object "' + param + '" not defined. Must be previously defined.', 10103)
                    else
                        DoSimpleMsg('TSData Object "' + param + '" not defined. Must be previously defined.', 10103);
                end;
            end;

            case ParamPointer of
                1, 4..7, 11..16:
                    DataChanged := TRUE;
            end;

            ParamName := Parser.NextParam;
            Param := Parser.StrValue;
        end;

    end;

end;

//- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
function TLineGeometry.MakeLike(const LineName: String): Integer;
var
    OtherLineGeometry: TLineGeometryObj;
    i: Integer;
begin
    Result := 0;
   {See if we can find this line code in the present collection}
    OtherLineGeometry := Find(LineName);
    if OtherLineGeometry <> NIL then
        with ActiveLineGeometryObj do
        begin
            NConds := OtherLineGeometry.NWires;   // allocates
            FNphases := OtherLineGeometry.FNphases;
            FSpacingType := OtherLineGeometry.FSpacingType;
            for i := 1 to FNConds do
                FPhaseChoice^[i] := OtherLineGeometry.FPhaseChoice^[i];
            for i := 1 to FNConds do
                FCondName^[i] := OtherLineGeometry.FCondName^[i];
            for i := 1 to FNConds do
                FWireData^[i] := OtherLineGeometry.FWireData^[i];
            for i := 1 to FNConds do
                FX^[i] := OtherLineGeometry.FX^[i];
            for i := 1 to FNConds do
                FY^[i] := OtherLineGeometry.FY^[i];
            for i := 1 to FNConds do
                FUnits^[i] := OtherLineGeometry.FUnits^[i];
            DataChanged := TRUE;
            NormAmps := OtherLineGeometry.NormAmps;
            EmergAmps := OtherLineGeometry.EmergAmps;

            UpdateLineGeometryData(DSS.activecircuit.solution.Frequency);

            for i := 1 to ParentClass.NumProperties do
                PropertyValue[i] := OtherLineGeometry.PropertyValue[i];
            Result := 1;
        end
    else
        DoSimpleMsg('Error in LineGeometry MakeLike: "' + LineName + '" Not Found.', 102);


end;

//- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
function TLineGeometry.Init(Handle: Integer): Integer;

begin
    DoSimpleMsg('Need to implement TLineGeometry.Init', -1);
    Result := 0;
end;

function TLineGeometry.Get_Code: String;  // Returns active line code string

begin

    Result := TLineGeometryObj(ElementList.Active).Name;

end;

procedure TLineGeometry.Set_Code(const Value: String);  // sets the  active LineGeometry
var
    LineGeometryObj: TLineGeometryObj;
begin

    ActiveLineGeometryObj := NIL;
    LineGeometryObj := ElementList.First;
    while LineGeometryObj <> NIL do
    begin

        if CompareText(LineGeometryObj.Name, Value) = 0 then
        begin
            ActiveLineGeometryObj := LineGeometryObj;
            Exit;
        end;

        LineGeometryObj := ElementList.Next;
    end;

    DoSimpleMsg('LineGeometry: "' + Value + '" not Found.', 103);

end;


//- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
//      TLineGeometry Obj
//- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -


constructor TLineGeometryObj.Create(ParClass: TDSSClass; const LineGeometryName: String);

begin
    inherited Create(ParClass);
    Name := LowerCase(LineGeometryName);
    DSSObjType := ParClass.DSSClassType;

    DataChanged := TRUE;

    FPhaseChoice := NIL;
    FCondName := NIL;
    FWireData := NIL;
    FX := NIL;
    FY := NIL;
    Funits := NIL;
    FLineData := NIL;
    FSpacingType := '';

(* was causing unnecessary allocations (was leaving dangling memory)
      Nconds      := 3;  // Allocates terminals
      FNphases    := 3;
*)
    FNconds := 0;
    FNPhases := 0;
//       ActiveCond  := 1;
    FActiveCond := 1;
    FLastUnit := UNITS_FT;
    Normamps := 0.0;
    EmergAmps := 0.0;

    FReduce := FALSE;
    NumAmpRatings := 1;
    setlength(AmpRatings, NumAmpRatings);
    AmpRatings[0] := NormAmps;

    InitPropertyValues(0);
end;

//- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
destructor TLineGeometryObj.Destroy;
begin

    FLineData.Free;
    FreeStringArray(FCondName, FnConds);
    Reallocmem(Fwiredata, 0);
    Reallocmem(FY, 0);
    Reallocmem(FX, 0);
    Reallocmem(Funits, 0);
    Reallocmem(FPhaseChoice, 0);

    inherited destroy;
end;


procedure TLineGeometryObj.DumpProperties(var F: TextFile; Complete: Boolean);

var
    i, j: Integer;

begin
    inherited DumpProperties(F, Complete);

    with ParentClass do
    begin
        for i := 1 to 2 do
        begin
            Writeln(F, '~ ', PropertyName^[i], '=', GetPropertyValue(i));
        end;
        for j := 1 to FNConds do
        begin
            ActiveCond := j;
            Writeln(F, '~ ', PropertyName^[3], '=', GetPropertyValue(3));
            Writeln(F, '~ ', PropertyName^[4], '=', GetPropertyValue(4));
            Writeln(F, '~ ', PropertyName^[5], '=', GetPropertyValue(5));
            Writeln(F, '~ ', PropertyName^[6], '=', GetPropertyValue(6));
            Writeln(F, '~ ', PropertyName^[7], '=', GetPropertyValue(7));
        end;
        for i := 8 to NumProperties do
        begin
            Writeln(F, '~ ', PropertyName^[i], '=', GetPropertyValue(i));
        end;

    end;

end;

function TLineGeometryObj.GetPropertyValue(Index: Integer): String;
var
    j,
    i: Integer;
{Return Property Value for Active index}

begin

    case Index of
        3:
            Result := Format('%d', [FActiveCond]);
        4, 13, 14:
            Result := FCondName^[FActiveCond];
        5:
            Result := Format('%-g', [FX^[FActiveCond]]);
        6:
            Result := Format('%-g', [FY^[FActiveCond]]);
        7:
            Result := LineUnitsStr(FUnits^[FActiveCond]);
        12, 15, 16:
        begin
            Result := '[';
            for i := 1 to FNConds do
                Result := Result + FCondName^[i] + ' ';
            Result := Result + ']';
        end;
        17:
            Result := inttostr(NumAmpRatings);
        18:
        begin
            Result := '[';
            for  j := 1 to NumAmpRatings do
                Result := Result + floattoStrf(AmpRatings[j - 1], ffgeneral, 8, 4) + ',';
            Result := Result + ']';
        end;
    else
     // Inherited parameters
        Result := inherited GetPropertyValue(Index);
    end;

end;

function TLineGeometryObj.Get_FX(i: Integer): Double;
begin
    if i <= FNConds then
        Result := FX^[i]
    else
        Result := 0.0;
end;

function TLineGeometryObj.Get_FY(i: Integer): Double;
begin
    if i <= FNConds then
        Result := FY^[i]
    else
        Result := 0.0;
end;

function TLineGeometryObj.Get_FUnits(i: Integer): Integer;
begin
    if i <= FNConds then
        Result := FUnits^[i]
    else
        Result := 0;
end;

{$IFDEF DSS_CAPI}
procedure TLineGeometryObj.Set_FX(i: Integer; Value: Double);
begin
    if i <= FNConds then
        FX^[i] := Value;
end;

procedure TLineGeometryObj.Set_FY(i: Integer; Value: Double);
begin
    if i <= FNConds then
        FY^[i] := Value;
end;

procedure TLineGeometryObj.Set_FUnits(i: Integer; Value: Integer);
begin
    if i <= FNConds then
        FUnits^[i] := Value;
end;

{$ENDIF}

function TLineGeometryObj.Get_ConductorName(i: Integer): String;
begin
    if i <= FNConds then
        Result := FCondName^[i]
    else
        Result := '';
end;

function TLineGeometryObj.Get_ConductorData(i: Integer): TConductorDataObj;
begin
    if i <= FNConds then
        Result := FWireData^[i]
    else
        Result := NIL;
end;

function TLineGeometryObj.get_Nconds: Integer;
begin
    if Freduce then
        Result := FNPhases
    else
        Result := FNConds;
end;

function TLineGeometryObj.Get_PhaseChoice(i: Integer): ConductorChoice;
begin
    Result := FPhaseChoice^[i];
end;

function TLineGeometryObj.Get_RhoEarth: Double;
begin
    Result := FLineData.rhoearth;
end;

function TLineGeometryObj.Get_YCmatrix(f, Lngth: Double;
    Units: Integer): Tcmatrix;
begin
    Result := NIL;
    if DataChanged then
        UpdateLineGeometryData(f);
    if not DSSPrime.SolutionAbort then
        Result := FLineData.YCMatrix[f, Lngth, Units];
end;

function TLineGeometryObj.Get_Zmatrix(f, Lngth: Double;
    Units: Integer): Tcmatrix;
begin
    Result := NIL;
    if DataChanged then
        UpdateLineGeometryData(f);
    if not DSSPrime.SolutionAbort then
        Result := FLineData.ZMatrix[F, Lngth, Units];
end;

procedure TLineGeometryObj.InitPropertyValues(ArrayOffset: Integer);
begin

    PropertyValue[1] := '3';
    PropertyValue[2] := '3';
    PropertyValue[3] := '1';
    PropertyValue[4] := '';
    PropertyValue[5] := '0';
    PropertyValue[6] := '32';
    PropertyValue[7] := 'ft';
    PropertyValue[8] := '0';
    PropertyValue[9] := '0';
    PropertyValue[17] := '1'; // 1 season
    PropertyValue[18] := '[400]'; // 1 rating

    inherited  InitPropertyValues(NumPropsThisClass);

end;


procedure TLineGeometryObj.SaveWrite(var F: TextFile);
{ Override standard SaveWrite}
{Linegeometry structure not conducive to standard means of saving}
var
    TempStr: String;
    strPhaseChoice: String;
    j,
    iprop: Integer;
    i: Integer;

begin
   {Write only properties that were explicitly set in the
   final order they were actually set}
    iProp := GetNextPropertySet(0); // Works on ActiveDSSObject
    if iProp > 0 then
        Writeln(F);

    while iProp > 0 do
    begin
        with ParentClass do

            case RevPropertyIdxMap[iProp] of
                3, 11, 12:
                begin   // if cond=, spacing, or wires were ever used write out arrays ...
                    for i := 1 to Fnconds do
                    begin
                        case PhaseChoice[i] of
                            Overhead:
                                strPhaseChoice := 'wire';
                            ConcentricNeutral:
                                strPhaseChoice := 'cncable';
                            TapeShield:
                                strPhaseChoice := 'tscable';
                        else
                            strPhaseChoice := 'wire';
                        end;
                        Writeln(F, Format('~ Cond=%d %s=%s X=%.7g h=%.7g units=%s',
                            [i, strPhaseChoice, FCondName^[i], FX^[i], FY^[i], LineUnitsStr(FUnits^[i])]));
                    end;
                end;
                4..7:
{do Nothing}; // Ignore these properties;
                8:
                    Writeln(F, Format('~ normamps=%.4g', [NormAmps]));
                9:
                    Writeln(F, Format('~ emergamps=%.4g', [EmergAmps]));
                10:
                    if FReduce then
                        Writeln(F, '~ Reduce=Yes');
                13..14: ;   {do Nothing} // Ignore these properties;
                18:
                begin
                    TempStr := '[';
                    for  j := 1 to NumAmpRatings do
                        TempStr := TempStr + floattoStrf(AmpRatings[j - 1], ffgeneral, 8, 4) + ',';
                    TempStr := TempStr + ']';
                    Writeln(F, 'ratings=' + TempStr);
                end;

            else
                Writeln(F, Format('~ %s=%s', [PropertyName^[RevPropertyIdxMap[iProp]], CheckForBlanks(PropertyValue[iProp])]));
            end;
        iProp := GetNextPropertySet(iProp);
    end;


end;

procedure TLineGeometryObj.set_ActiveCond(const Value: Integer);
begin
    if Value > 0 then
        if Value <= FNconds then
        begin
            FActiveCond := Value;
            if Funits^[FactiveCond] = -1 then
                Funits^[FactiveCond] := FLastUnit;  // makes this a sticky value so you don't have to repeat it
        end;
end;

procedure TLineGeometryObj.ChangeLineConstantsType(newPhaseChoice: ConductorChoice);
var
    newLineData: TLineConstants;
    needNew: Boolean;
begin
    newLineData := NIL;
    needNew := FALSE;
    if newPhaseChoice <> FPhaseChoice^[ActiveCond] then
        needNew := TRUE;
    if not Assigned(FLineData) then
        needNew := TRUE
    else
    if FNConds <> FLineData.Nconductors then
        needNew := TRUE;

    if needNew then
        case newPhaseChoice of
            Overhead:
                newLineData := TOHLineConstants.Create(FNConds);
            ConcentricNeutral:
                newLineData := TCNLineConstants.Create(FNConds);
            TapeShield:
                newLineData := TTSLineConstants.Create(FNConds);
        end;

    if Assigned(newLineData) then
    begin
        if Assigned(FLineData) then
        begin
            newLineData.Nphases := FLineData.Nphases;
            newLineData.rhoearth := FLineData.rhoearth;
        end
        else
            FreeAndNil(FLineData);
        FLineData := newLineData;
    end;
    FPhaseChoice^[ActiveCond] := newPhaseChoice;
end;

procedure TLineGeometryObj.set_Nconds(const Value: Integer);
var
    i: Integer;
begin
    if Value < 1 then
    begin
        DoSimpleMsg('Invalid number of conductors sent via DSS command. Please enter a value within range.', 185);
        Exit;
    end;

    if Assigned(FCondName) then
        FreestringArray(FCondName, FNConds);  // dispose of old allocation

    FNconds := Value;
    if Assigned(FLineData) then
        FreeAndNil(FLineData);


  {Allocations}
    Reallocmem(FWireData, Sizeof(FWireData^[1]) * FNconds);
    Reallocmem(FX, Sizeof(FX^[1]) * FNconds);
    Reallocmem(FY, Sizeof(FY^[1]) * FNconds);
    Reallocmem(FUnits, Sizeof(Funits^[1]) * FNconds);
    Reallocmem(FPhaseChoice, Sizeof(FPhaseChoice^[1]) * FNconds);

{Initialize Allocations}
    for i := 1 to FNconds do
        FPhaseChoice^[i] := Overhead;
    for i := 1 to FNconds do
        FWireData^[i] := NIL;
    for i := 1 to FNconds do
        FX^[i] := 0.0;
    for i := 1 to FNconds do
        FY^[i] := 0.0;
    for i := 1 to FNconds do
        FUnits^[i] := -1;  // default to ft
    FLastUnit := UNITS_FT;

    for i := 1 to FNconds do
    begin
        FActiveCond := i;
        ChangeLineConstantsType(Overhead);    // works on activecond
    end;
    // Reset the active conductor
    FActiveCond := 1;

    FCondName := AllocStringArray(FNconds);


end;

procedure TLineGeometryObj.set_Nphases(const Value: Integer);
begin
    if Value < 1 then
    begin
        DoSimpleMsg('Invalid number of phases sent via DSS command. Please enter a value within range.', 186);
        Exit;
    end;

    FNphases := Value;
    FLineData.Nphases := Value;
end;

procedure TLineGeometryObj.Set_RhoEarth(const Value: Double);
begin
    FLineData.RhoEarth := Value;
end;

procedure TLineGeometryObj.UpdateLineGeometryData(f: Double);
var
    i: Integer;
    LineGeomErrMsg: String;
    cnd: TCNDataObj;
    tsd: TTSDataObj;
begin

    for i := 1 to FNconds do
    begin
        FLineData.X[i, Funits^[i]] := FX^[i];
        FLineData.Y[i, Funits^[i]] := FY^[i];
        FLineData.radius[i, FWireData^[i].RadiusUnits] := FWireData^[i].Radius;
        FLineData.GMR[i, FWireData^[i].GMRUnits] := FWireData^[i].GMR;
        FLineData.Rdc[i, FWireData^[i].ResUnits] := FWireData^[i].Rdc;
        FLineData.Rac[i, FWireData^[i].ResUnits] := FWireData^[i].Rac;
        if (FWireData^[i] is TCNDataObj) then
        begin
            with (FLineData as TCNLineConstants) do
            begin
                cnd := (FWireData^[i] as TCNDataObj);
                EpsR[i] := cnd.EpsR;
                InsLayer[i, cnd.RadiusUnits] := cnd.InsLayer;
                DiaIns[i, cnd.RadiusUnits] := cnd.DiaIns;
                DiaCable[i, cnd.RadiusUnits] := cnd.DiaCable;
                kStrand[i] := cnd.NStrand;
                DiaStrand[i, cnd.RadiusUnits] := cnd.DiaStrand;
                GmrStrand[i, cnd.GMRUnits] := cnd.GmrStrand;
                RStrand[i, cnd.ResUnits] := cnd.RStrand;
            end;
        end
        else
        if (FWireData^[i] is TTSDataObj) then
        begin
            with (FLineData as TTSLineConstants) do
            begin
                tsd := (FWireData^[i] as TTSDataObj);
                EpsR[i] := tsd.EpsR;
                InsLayer[i, tsd.RadiusUnits] := tsd.InsLayer;
                DiaIns[i, tsd.RadiusUnits] := tsd.DiaIns;
                DiaCable[i, tsd.RadiusUnits] := tsd.DiaCable;
                DiaShield[i, tsd.RadiusUnits] := tsd.DiaShield;
                TapeLayer[i, tsd.RadiusUnits] := tsd.TapeLayer;
                TapeLap[i] := tsd.TapeLap;
            end;
        end;
    end;

    FLineData.Nphases := FNphases;
    DataChanged := FALSE;

  {Before we calc, check for bad conductor definitions}
    if FLineData.ConductorsInSameSpace(LineGeomErrMsg) then
    begin
        raise ELineGeometryProblem.Create('Error in LineGeometry.' + Name + ': ' + LineGeomErrMsg);
        DSSPrime.SolutionAbort := TRUE;
    end
    else
    begin
        FLineData.Calc(f);
        if FReduce then
            FLineData.Reduce; // reduce out neutrals
    end;
end;

procedure TLineGeometryObj.LoadSpacingAndWires(Spc: TLineSpacingObj; Wires: pConductorDataArray);
var
    i: Integer;
    newPhaseChoice: ConductorChoice;
begin
    NConds := Spc.NWires;   // allocates
    FNphases := Spc.Nphases;
    FSpacingType := Spc.Name;
    if FNConds > FNPhases then
        FReduce := TRUE;

    newPhaseChoice := Overhead;
    for i := 1 to FNConds do
    begin
        if Wires[i] is TCNDataObj then
            newPhaseChoice := ConcentricNeutral;
        if Wires[i] is TTSDataObj then
            newPhaseChoice := TapeShield;
    end;
    ChangeLineConstantsType(newPhaseChoice);

    for i := 1 to FNConds do
        FCondName^[i] := Wires^[i].Name;
    for i := 1 to FNConds do
        FWireData^[i] := Wires^[i];
    for i := 1 to FNConds do
        FX^[i] := Spc.Xcoord[i];
    for i := 1 to FNConds do
        FY^[i] := Spc.Ycoord[i];
    for i := 1 to FNConds do
        FUnits^[i] := Spc.Units;
    DataChanged := TRUE;
    NormAmps := Wires^[1].NormAmps;
    EmergAmps := Wires^[1].EmergAmps;

    UpdateLineGeometryData(DSSPrime.activecircuit.solution.Frequency);
end;

end.
