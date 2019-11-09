unit Line;

{
  ----------------------------------------------------------
  Copyright (c) 2008-2015, Electric Power Research Institute, Inc.
  All rights reserved.
  ----------------------------------------------------------
}

{  3-1-00   Reactivated line dump
   3-13-03  Fixed bug where terminal quantities were not getting reallocated in FetchCondCode
   2018        Added GIC stuff
}

interface

uses
    Command,
    DSSClass,
    Circuit,
    PDElement,
    UcMatrix,
    LineCode,
    ArrayDef,
    LineGeometry,
    LineSpacing,
    ConductorData,
    PDClass,
    Ucomplex;

type

    TLine = class(TPDClass)
    PRIVATE
        procedure DoRmatrix;
        procedure DoXmatrix;
        procedure DoCmatrix;

    PROTECTED
        procedure DefineProperties;  // Add Properties of this class to propName
        function MakeLike(const LineName: String): Integer; OVERRIDE;

    PUBLIC
        constructor Create(dss: TDSS);
        destructor Destroy; OVERRIDE;


        function Edit: Integer; OVERRIDE;     // uses global parser
        function Init(Handle: Integer): Integer; OVERRIDE;
        function NewObject(const ObjName: String): Integer; OVERRIDE;

    end;

    TLineObj = class(TPDElement)
{$IFDEF DSS_CAPI}
    PUBLIC
{$ELSE}
    PRIVATE
{$ENDIF}
        FZFrequency: Double; // keep track of last frequency computed for geometry
        FLineCodeUnits: Integer;
        FUnitsConvert: Double; // conversion factor
        FLineGeometryObj: TLineGeometryObj;
        FLineSpacingObj: TLineSpacingObj;
        FLineWireData: pConductorDataArray;
        FWireDataSize: Integer;
        FPhaseChoice: ConductorChoice;
        FrhoSpecified: Boolean;
        FLineCodeSpecified: Boolean;
        FEarthModel: Integer;
        FCapSpecified: Boolean; // To make sure user specifies C in some form

        procedure FMakeZFromGeometry(f: Double); // make new Z, Zinv, Yc, etc
        procedure KillGeometrySpecified;

        procedure FMakeZFromSpacing(f: Double); // make new Z, Zinv, Yc, etc
        procedure KillSpacingSpecified;

        procedure ClearYPrim;
        procedure ResetLengthUnits;
        procedure UpdatePDProperties;   // update inherited properties

        function NumConductorData: Integer;
        function FetchConductorData(i: Integer): TConductorDataObj;

        procedure ReallocZandYcMatrices;

        procedure DoLongLine(Frequency: Double);  // Long Line Correction for 1=phase
        procedure ConvertZinvToPosSeqR;  // for GIC analysis, primarily

    PROTECTED
        Zinv: TCMatrix;

    PUBLIC     // Moved to make values available to the COM interface

        Z: TCMatrix;   // Base Frequency Series Z matrix  per unit length
        Yc: TCMatrix;

        R1: Double;
        X1: Double;
        R0: Double;
        X0: Double;
        C1: Double;
        C0: Double;
        Len: Double;
        LengthUnits: Integer;

        Rg, Xg, KXg, rho: Double;
        GeneralPlotQuantity: Double;  // For general circuit plotting
        CondCode: String;
        GeometryCode: String;
        SpacingCode: String;
        GeometrySpecified: Boolean;
        SpacingSpecified: Boolean;
        SymComponentsChanged: Boolean;
        SymComponentsModel: Boolean;
        IsSwitch: Boolean;

        procedure GetLosses(var TotalLosses, LoadLosses, NoLoadLosses: Complex); OVERRIDE;
        procedure GetSeqLosses(var PosSeqLosses, NegSeqLosses, ZeroSeqLosses: complex); OVERRIDE;

        constructor Create(ParClass: TDSSClass; const LineName: String);
        destructor Destroy; OVERRIDE;

        procedure RecalcElementData; OVERRIDE;
        procedure CalcYPrim; OVERRIDE;

        procedure MakePosSequence; OVERRIDE;  // Make a positive Sequence Model
        function MergeWith(var OtherLine: TLineObj; Series: Boolean): Boolean;
        procedure UpdateControlElements(const NewName, OldName: String);

        function GetPropertyValue(Index: Integer): String; OVERRIDE;
        procedure InitPropertyValues(ArrayOffset: Integer); OVERRIDE;
        procedure DumpProperties(var F: TextFile; Complete: Boolean); OVERRIDE;

        // Public for the COM Interface
        procedure FetchLineCode(const Code: String);
        procedure FetchGeometryCode(const Code: String);
        procedure FetchLineSpacing(const Code: String);
        procedure FetchWireList(const Code: String);
        procedure FetchCNCableList(const Code: String);
        procedure FetchTSCableList(const Code: String);

        // Reliability calcs
        procedure CalcFltRate; OVERRIDE;  // Calc failure rates for section and buses

        // CIM XML access
        property LineCodeSpecified: Boolean READ FLineCodeSpecified;
        property PhaseChoice: ConductorChoice READ FPhaseChoice;

        property UnitsConvert: Double READ FUnitsConvert;  // conversion to present Line units

        property NumConductorsAvailable: Integer READ NumConductorData;
        property ConductorData[i: Integer]: TConductorDataObj READ FetchConductorData;
    end;

var
    ActiveLineObj: TLineObj;

implementation

uses
    ParserDel,
    DSSClassDefs,
    DSSGlobals,
    Sysutils,
    Utilities,
    Mathutil,
    ControlElem,
    LineUnits,
    DSSHelper;

const
    NumPropsThisClass = 29;
    //  MaxPhases = 20; // for fixed buffers

var
    CAP_EPSILON: Complex;

//- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
constructor TLine.Create(dss: TDSS);  // Creates superstructure for all Line objects
begin
    inherited Create(dss);
    Class_Name := 'Line';
    DSSClassType := DSSClassType + LINE_ELEMENT; // in both PDElement list and Linesection lists

    ActiveElement := 0;

    DefineProperties;

    CommandList := TCommandList.Create(Slice(PropertyName^, NumProperties));
    CommandList.Abbrev := TRUE;

end;

//- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
destructor TLine.Destroy;

begin

    // ElementList and  CommandList freed in inherited destroy
    inherited Destroy;
end;

//- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
procedure TLine.DefineProperties;
begin

    Numproperties := NumPropsThisClass;
    CountProperties;   // Get inherited property count
    AllocatePropertyArrays;


     // Define Property names
    PropertyName[1] := 'bus1';
    PropertyName[2] := 'bus2';
    PropertyName[3] := 'linecode';
    PropertyName[4] := 'length';
    PropertyName[5] := 'phases';
    PropertyName[6] := 'r1';
    PropertyName[7] := 'x1';
    PropertyName[8] := 'r0';
    PropertyName[9] := 'x0';
    PropertyName[10] := 'C1';
    PropertyName[11] := 'C0';
    PropertyName[12] := 'rmatrix';
    PropertyName[13] := 'xmatrix';
    PropertyName[14] := 'cmatrix';
    PropertyName[15] := 'Switch';
    PropertyName[16] := 'Rg';
    PropertyName[17] := 'Xg';
    PropertyName[18] := 'rho';
    PropertyName[19] := 'geometry';
    PropertyName[20] := 'units';
    PropertyName[21] := 'spacing';
    PropertyName[22] := 'wires';
    PropertyName[23] := 'EarthModel';
    PropertyName[24] := 'cncables';
    PropertyName[25] := 'tscables';
    PropertyName[26] := 'B1';
    PropertyName[27] := 'B0';
    PropertyName[28] := 'Seasons';
    PropertyName[29] := 'Ratings';

     // define Property help values

    PropertyHelp[1] := 'Name of bus to which first terminal is connected.' + CRLF +
        'Example:' + CRLF +
        'bus1=busname   (assumes all terminals connected in normal phase order)' + CRLF +
        'bus1=busname.3.1.2.0 (specify terminal to node connections explicitly)';
    PropertyHelp[2] := 'Name of bus to which 2nd terminal is connected.';
    PropertyHelp[3] := 'Name of linecode object describing line impedances.' + CRLF +
        'If you use a line code, you do not need to specify the impedances here. ' +
        'The line code must have been PREVIOUSLY defined. ' +
        'The values specified last will prevail over those specified earlier (left-to-right ' +
        'sequence of properties).  You can subsequently change the number of phases if symmetrical component quantities are specified.' +
        'If no line code or impedance data are specified, the line object ' +
        'defaults to 336 MCM ACSR on 4 ft spacing.';
    PropertyHelp[4] := 'Length of line. Default is 1.0. If units do not match the impedance data, specify "units" property. ';
    PropertyHelp[5] := 'Number of phases, this line.';
    PropertyHelp[6] := 'Positive-sequence Resistance, ohms per unit length. Setting any of R1, R0, X1, X0, C1, C0 forces ' +
        'the program to use the symmetrical component line definition. See also Rmatrix.';
    PropertyHelp[7] := 'Positive-sequence Reactance, ohms per unit length. Setting any of R1, R0, X1, X0, C1, C0 forces ' +
        'the program to use the symmetrical component line definition.  See also Xmatrix';
    PropertyHelp[8] := 'Zero-sequence Resistance, ohms per unit length. Setting any of R1, R0, X1, X0, C1, C0 forces ' +
        'the program to use the symmetrical component line definition.';
    PropertyHelp[9] := 'Zero-sequence Reactance, ohms per unit length. Setting any of R1, R0, X1, X0, C1, C0 forces ' +
        'the program to use the symmetrical component line definition.';
    PropertyHelp[10] := 'Positive-sequence capacitance, nf per unit length.  Setting any of R1, R0, X1, X0, C1, C0 forces ' +
        'the program to use the symmetrical component line definition. See also Cmatrix and B1.';
    PropertyHelp[11] := 'Zero-sequence capacitance, nf per unit length. Setting any of R1, R0, X1, X0, C1, C0 forces ' +
        'the program to use the symmetrical component line definition.See also B0.';
    PropertyHelp[12] := 'Resistance matrix, lower triangle, ohms per unit length. Order of the matrix is the number of phases. ' +
        'May be used to specify the impedance of any line configuration. Using any of Rmatrix, Xmatrix, Cmatrix ' +
        'forces program to use the matrix values for line impedance definition. For balanced line models, you may ' +
        'use the standard symmetrical component data definition instead.';
    PropertyHelp[13] := 'Reactance matrix, lower triangle, ohms per unit length. Order of the matrix is the number of phases. ' +
        'May be used to specify the impedance of any line configuration. Using any of Rmatrix, Xmatrix, Cmatrix ' +
        'forces program to use the matrix values for line impedance definition.  For balanced line models, you may ' +
        'use the standard symmetrical component data definition instead.';
    PropertyHelp[14] := 'Nodal Capacitance matrix, lower triangle, nf per unit length.Order of the matrix is the number of phases. ' +
        'May be used to specify the shunt capacitance of any line configuration. Using any of Rmatrix, Xmatrix, Cmatrix ' +
        'forces program to use the matrix values for line impedance definition.  For balanced line models, you may ' +
        'use the standard symmetrical component data definition instead.';
    PropertyHelp[15] := '{y/n | T/F}  Default= no/false.  Designates this line as a switch for graphics and algorithmic purposes. ' + CRLF +
        'SIDE EFFECT: Sets r1 = 1.0; x1 = 1.0; r0 = 1.0; x0 = 1.0; c1 = 1.1 ; c0 = 1.0;  length = 0.001; You must reset if you want something different.';
    PropertyHelp[16] := 'Carson earth return resistance per unit length used to compute impedance values at base frequency. ' +
        'Default is 0.01805 = 60 Hz value in ohms per kft (matches default line impedances). ' +
        'This value is required for harmonic solutions if you wish to adjust the earth return impedances for frequency. ' +
        'If not, set both Rg and Xg = 0.';
    PropertyHelp[17] := 'Carson earth return reactance per unit length used to compute impedance values at base frequency.  For making better frequency adjustments. ' +
        'Default is 0.155081 = 60 Hz value in ohms per kft (matches default line impedances). ' +
        'This value is required for harmonic solutions if you wish to adjust the earth return impedances for frequency. ' +
        'If not, set both Rg and Xg = 0.';
    PropertyHelp[18] := 'Default=100 meter ohms.  Earth resitivity used to compute earth correction factor. Overrides Line geometry definition if specified.';
    PropertyHelp[19] := 'Geometry code for LineGeometry Object. Supercedes any previous definition of line impedance. ' +
        'Line constants are computed for each frequency change or rho change. CAUTION: may alter number of phases. ' +
        'You cannot subsequently change the number of phases unless you change how the line impedance is defined.';
    PropertyHelp[20] := 'Length Units = {none | mi|kft|km|m|Ft|in|cm } Default is None - assumes length units match impedance units.';
    PropertyHelp[21] := 'Reference to a LineSpacing for use in a line constants calculation.' + CRLF +
        'Must be used in conjunction with the Wires property.' + CRLF +
        'Specify this before the wires property.';
    PropertyHelp[22] := 'Array of WireData names for use in an overhead line constants calculation.' + CRLF +
        'Must be used in conjunction with the Spacing property.' + CRLF +
        'Specify the Spacing first, and "ncond" wires.' + CRLF +
        'May also be used to specify bare neutrals with cables, using "ncond-nphase" wires.';
    PropertyHelp[23] := 'One of {Carson | FullCarson | Deri}. Default is the global value established with the Set EarthModel command. ' +
        'See the Options Help on EarthModel option. This is used to override the global value for this line. This ' +
        'option applies only when the "geometry" property is used.';
    PropertyHelp[24] := 'Array of CNData names for use in a cable constants calculation.' + CRLF +
        'Must be used in conjunction with the Spacing property.' + CRLF +
        'Specify the Spacing first, using "nphases" cncables.' + CRLF +
        'You may later specify "nconds-nphases" wires for separate neutrals';
    PropertyHelp[25] := 'Array of TSData names for use in a cable constants calculation.' + CRLF +
        'Must be used in conjunction with the Spacing property.' + CRLF +
        'Specify the Spacing first, using "nphases" tscables.' + CRLF +
        'You may later specify "nconds-nphases" wires for separate neutrals';
    PropertyHelp[26] := 'Alternate way to specify C1. MicroS per unit length';
    PropertyHelp[27] := 'Alternate way to specify C0. MicroS per unit length';
    PropertyHelp[28] := 'Defines the number of ratings to be defined for the wire, to be used only when defining seasonal ratings using the "Ratings" property.';
    PropertyHelp[29] := 'An array of ratings to be used when the seasonal ratings flag is True. It can be used to insert' +
        CRLF + 'multiple ratings to change during a QSTS simulation to evaluate different ratings in lines.';

    ActiveProperty := NumPropsThisClass;
    inherited DefineProperties;  // Add defs of inherited properties to bottom of list

    PropertyHelp[NumPropsThisClass + 3] := 'Failure rate PER UNIT LENGTH per year. Length must be same units as LENGTH property. Default is 0.1 fault per unit length per year.';
    PropertyHelp[NumPropsThisClass + 4] := PropertyHelp[NumPropsThisClass + 4] + ' Default is 20.';
    PropertyHelp[NumPropsThisClass + 5] := PropertyHelp[NumPropsThisClass + 5] + ' Default is 3 hr.';
end;

//- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
function TLine.NewObject(const ObjName: String): Integer;
begin
   // create a new object of this class and add to list
    with ActiveCircuit do
    begin
        ActiveCktElement := TLineObj.Create(Self, ObjName);
        Result := AddObjectToList(ActiveDSSObject);
    end;
end;

procedure TLineObj.UpdatePDProperties;
var
    TempStr: String;
    j: Integer;
begin
    PropertyValue[28] := Format('%-d', [NumAmpRatings]);
    TempStr := '[';
    for  j := 1 to NumAmpRatings do
        TempStr := TempStr + floattoStrf(AmpRatings[j - 1], ffgeneral, 8, 4) + ',';
    TempStr := TempStr + ']';
    PropertyValue[29] := TempStr;

    PropertyValue[NumPropsThisClass + 1] := Format('%-g', [Normamps]);
    PropertyValue[NumPropsThisClass + 2] := Format('%-g', [EmergAmps]);
  // commented out 8/26/2014
  // PropertyValue[NumPropsThisClass + 3] := Format('%-g', [FaultRate]);
  // PropertyValue[NumPropsThisClass + 4] := Format('%-g', [PctPerm]);
  // PropertyValue[NumPropsThisClass + 5] := Format('%-g', [HrsToRepair]);
end;

//- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
procedure TLineObj.FetchLineCode(const Code: String);
var
    LineCodeObj: TLineCodeObj;
    i: Integer;

begin
    if DSSPrime.LineCodeClass.SetActive(Code) then
    begin

        LineCodeObj := DSSPrime.LineCodeClass.GetActiveObj;

        CondCode := LowerCase(Code);

       // Frequency compensation takes place in calcYPrim.
        BaseFrequency := LineCodeObj.BaseFrequency;
       {Copy impedances from line code, but do not recalc because symmetrical
        component z's may not match what's in matrix}
        if LineCodeObj.SymComponentsModel then
        begin
            R1 := LineCodeObj.R1;
            X1 := LineCodeObj.X1;
            R0 := LineCodeObj.R0;
            X0 := LineCodeObj.X0;
            C1 := LineCodeObj.C1;
            C0 := LineCodeObj.C0;
            SymComponentsModel := TRUE;
        end
        else
            SymComponentsModel := FALSE;


       // Earth return impedances used to compensate for frequency
        Rg := LineCodeObj.Rg;
        Xg := LineCodeObj.Xg;
        rho := LineCodeObj.rho;
        Kxg := Xg / ln(658.5 * sqrt(rho / BaseFrequency));

        FLineCodeUnits := LineCodeObj.Units;
        FLineCodeSpecified := TRUE;

        FUnitsConvert := ConvertLineUnits(FLineCodeUnits, LengthUnits);

        NormAmps := LineCodeObj.NormAmps;
        EmergAmps := LineCodeObj.EmergAmps;

        NumAmpRatings := LineCodeObj.NumAmpRatings;
        setlength(AmpRatings, NumAmpRatings);
        for i := 0 to High(AmpRatings) do
            AmpRatings[i] := LineCodeObj.AmpRatings[i];

       // These three properties should not come from the Linecode
       //   But can vary from line section to line section
       // commented out 8/26/2014
       // FaultRate := LineCodeObj.FaultRate;
       // PctPerm   := LineCodeObj.PctPerm;
       // HrsToRepair := LineCodeObj.HrsToRepair;


        UpdatePDProperties;


        if Fnphases <> LineCodeObj.FNphases then
        begin
            NPhases := LineCodeObj.FNPhases;

            ReallocZandYcMatrices;
        end;

        if not SymComponentsModel then
        begin        // Copy matrices
            Z.CopyFrom(LineCodeObj.Z);
         {Zinv.CopyFrom(LineCodeObj.Zinv);}  // no need to copy Zinv
            Yc.CopyFrom(LineCodeObj.Yc);
        end
        else
            RecalcElementData;    // Compute matrices

        NConds := Fnphases;  // Force Reallocation of terminal info
       //Fnconds := Fnphases;
        Yorder := Fnconds * Fnterms;
       // YPrimInvalid := True;  (set in Edit; this is redundant)


    end
    else
        DoSimpleMsg('Line Code:' + Code + ' not found.', 180);

end;


//- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
procedure TLine.DoRmatrix;
var
    OrderFound, Norder, j: Integer;
    MatBuffer: pDoubleArray;
    Zvalues: pComplexArray;

begin
    with ActiveLineObj do
    begin
       {Added 3-17-15 in case Z and Yc do not get allocated to the proper value}
        if Z.Order <> Fnphases then
            ReallocZandYcMatrices;

        MatBuffer := Allocmem(Sizeof(Double) * Fnphases * Fnphases);
        OrderFound := Parser.ParseAsSymMatrix(Fnphases, MatBuffer);

        if OrderFound > 0 then    // Parse was successful
        begin    {R}
            ZValues := Z.GetValuesArrayPtr(Norder);
            if Norder = Fnphases then
                for j := 1 to Fnphases * Fnphases do
                    ZValues^[j].Re := MatBuffer^[j];
        end;

        Freemem(MatBuffer, Sizeof(Double) * Fnphases * Fnphases);
    end;
end;

//- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
procedure TLine.DoXmatrix;
var
    OrderFound, Norder, j: Integer;
    MatBuffer: pDoubleArray;
    Zvalues: pComplexArray;

begin
    with ActiveLineObj do
    begin
        if Z.Order <> Fnphases then
            ReallocZandYcMatrices;

        MatBuffer := Allocmem(Sizeof(Double) * Fnphases * Fnphases);
        OrderFound := Parser.ParseAsSymMatrix(Fnphases, MatBuffer);

        if OrderFound > 0 then    // Parse was successful
        begin    {X}
            ZValues := Z.GetValuesArrayPtr(Norder);
            if Norder = Fnphases then
                for j := 1 to Fnphases * Fnphases do
                    ZValues^[j].im := MatBuffer^[j];
        end;

        Freemem(MatBuffer, Sizeof(Double) * Fnphases * Fnphases);
    end;
end;

//- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
procedure TLine.DoCmatrix;
var
    OrderFound,
    Norder,
    j: Integer;
    MatBuffer: pDoubleArray;
    Yvalues: pComplexArray;
    Factor: Double;

begin
    with ActiveLineObj do
    begin
        if Z.Order <> Fnphases then
            ReallocZandYcMatrices;

        MatBuffer := Allocmem(Sizeof(Double) * Fnphases * Fnphases);
        OrderFound := Parser.ParseAsSymMatrix(Fnphases, MatBuffer);

        if OrderFound > 0 then    // Parse was successful
        begin    {X}
            Factor := TwoPi * BaseFrequency * 1.0e-9;
            YValues := YC.GetValuesArrayPtr(Norder);
            if Norder = Fnphases then
                for j := 1 to Fnphases * Fnphases do
                    YValues^[j].im := Factor * MatBuffer^[j];
        end;

        Freemem(MatBuffer, Sizeof(Double) * Fnphases * Fnphases);
    end;
end;

//- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
function TLine.Edit: Integer;

// A Line Defaults to 3-phases and some typical symmetrical component data
{
 Line impedances are specified in per unit length and are multiplied by the length
 when the primitive Y matrix is computed.

 You may specify the impedances of the line either by symmetrical components or
 by R, X, and nodal C matrices  (also per unit length).

 All C's is entered in nano farads.

 The ultimate values are in the matrices.  If you specify matrices, then the symmetrical
 component values are ignored.  However, if you change any of the symmetrical component values
 the matrices will be recomputed.  It is assumed you want to use symmetrical component values.
 Don't mix data entry by matrix and by symmetrical components.

 Note that if you change the number of phases, the matrices are reallocated and reinitialized
 with whatever is currently in the symmetrical component data.

}
var
    ParamPointer: Integer;
    ParamName: String;
    Param: String;
    NewLengthUnits: Integer;

begin
    Result := 0;
  // continue parsing with contents of Parser
    ActiveLineObj := ElementList.Active;
    ActiveCircuit.ActiveCktElement := ActiveLineObj;  // use property to set this value

    with ActiveLineObj do
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
                    DoSimpleMsg('Unknown parameter "' + ParamName + '" for Object "Line.' + Name + '"', 181);
                1:
                    Setbus(1, param);
                2:
                    Setbus(2, param);
                3:
                    FetchLineCode(Param);  // Define line by conductor code
                4:
                    Len := Parser.DblValue;
                5:
{Nphases: See below};
                6:
                    r1 := Parser.Dblvalue;
                7:
                    x1 := Parser.Dblvalue;
                8:
                    r0 := Parser.Dblvalue;
                9:
                    x0 := Parser.Dblvalue;
                10:
                begin
                    c1 := Parser.Dblvalue * 1.0e-9;
                    FCapSpecified := TRUE;
                end; // Convert from nano to farads
                11:
                begin
                    c0 := Parser.Dblvalue * 1.0e-9;
                    FCapSpecified := TRUE;
                end;
                12:
                    DoRmatrix;
                13:
                    DoXmatrix;
                14:
                begin
                    DoCMatrix;
                    FCapSpecified := TRUE;
                end;
                15:
                    IsSwitch := InterpretYesNo(Param);
                16:
                    Rg := Parser.DblValue;
                17:
                    Xg := Parser.DblValue;
                18:
                begin
                    rho := Parser.DblValue;
                    FrhoSpecified := TRUE;
                end;
                19:
                    FetchGeometryCode(Param);
                20:
                begin // Update units conversion factor that might have been changed previously
                    NewLengthUnits := GetUnitsCode(Param);
                    if FLineCodeSpecified then
                        FUnitsConvert := ConvertLineUnits(FLineCodeUnits, NewLengthUnits)
                    else
                        FUnitsConvert := FUnitsConvert * ConvertLineUnits(LengthUnits, NewLengthUnits);
                    LengthUnits := NewLengthUnits;
                end;
                21:
                    FetchLineSpacing(Param);
                22:
                    FetchWireList(Param);
                23:
                    FEarthModel := InterpretEarthModel(Param);
                24:
                    FetchCNCableList(Param);
                25:
                    FetchTSCableList(Param);
                26:
                begin
                    c1 := Parser.Dblvalue / (twopi * BaseFrequency) * 1.0e-6;
                    FCapSpecified := TRUE;
                end;
                27:
                begin
                    c0 := Parser.Dblvalue / (twopi * BaseFrequency) * 1.0e-6;
                    FCapSpecified := TRUE;
                end;
                28:
                begin
                    NumAmpRatings := Parser.IntValue;
                    setlength(AmpRatings, NumAmpRatings);
                end;
                29:
                begin
                    setlength(AmpRatings, NumAmpRatings);
                    Param := Parser.StrValue;
                    NumAmpRatings := InterpretDblArray(Param, NumAmpRatings, Pointer(AmpRatings));
                end
            else
            // Inherited Property Edits
                ClassEdit(ActiveLineObj, ParamPointer - NumPropsThisClass)
            end;

         // Side Effects ...
            case ParamPointer of
                3:
                begin
                    SpacingSpecified := FALSE;
                    if GeometrySpecified = TRUE then
                        KillGeometrySpecified;
                    GeometrySpecified := FALSE;
                end;
                4, 20:     // for Reliability calcs -- see PDElement.Pas
                    MilesThisLine := len * ConvertLineUnits(LengthUnits, UNITS_MILES);

                5: {Change the number of phases ... only valid if SymComponentsModel=TRUE}
                    if Fnphases <> Parser.IntValue then
                        if (not GeometrySpecified) and SymComponentsModel then
                        begin  // ignore change of nphases if geometry used
                            Nphases := Parser.IntValue;
                            NConds := Fnphases;  // Force Reallocation of terminal info
                            Yorder := Fnterms * Fnconds;
                 {YPrimInvalid := True;}  // now set below
                            RecalcElementData;  // Reallocate Z, etc.
                        end
                        else
                        begin
                            DoSimpleMsg('Illegal change of number of phases for Line.' + Name, 18101);
                        end;
                6..11, 26..27:
                begin
                    FLineCodeSpecified := FALSE;
                    KillGeometrySpecified;
                    KillSpacingSpecified;
                    ResetLengthUnits;
                    SymComponentsChanged := TRUE;
                    SymComponentsModel := TRUE;
                end;
                12..14:
                begin
                    FLineCodeSpecified := FALSE;
                    SymComponentsModel := FALSE;
                    ResetLengthUnits;
                    KillGeometrySpecified;
                    KillSpacingSpecified;
                end;
                15:
                    if IsSwitch then
                    begin
                        SymComponentsChanged := TRUE;
                        YprimInvalid := TRUE;
                        GeometrySpecified := FALSE;
                        SpacingSpecified := FALSE;
                        r1 := 1.0;
                        x1 := 1.0;
                        r0 := 1.0;
                        x0 := 1.0;
                        c1 := 1.1 * 1.0e-9;
                        c0 := 1.0 * 1.0e-9;
                        len := 0.001;
                        ResetLengthUnits;
                    end;

                17..18:
                    Kxg := Xg / ln(658.5 * sqrt(rho / BaseFrequency));
                19:
                begin
                    GeometrySpecified := TRUE;
                    SymComponentsModel := FALSE;
                    SymComponentsChanged := FALSE;
                end;
                21..22, 24..25:
                begin
                    if Assigned(FLineSpacingObj) and Assigned(FLineWireData) then
                    begin
                        SpacingSpecified := TRUE;
                        SymComponentsModel := FALSE;
                        SymComponentsChanged := FALSE;
                        KillGeometrySpecified;
                    end;
                    YprimInvalid := TRUE;
                end;
            else
            end;

         //YPrim invalidation on anything that changes impedance values
            case ParamPointer of
                3..14:
                    YprimInvalid := TRUE;
                18:
                    if GeometrySpecified and assigned(FLineGeometryObj) then
                        FlineGeometryObj.rhoearth := rho;
            else
            end;

            ParamName := Parser.NextParam;
            Param := Parser.StrValue;
        end;

     // If SymComponentsChanged THEN RecalcElementData;
    end;

end;

//- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
function TLine.MakeLike(const LineName: String): Integer;
var
    OtherLine: TLineObj;
    i: Integer;

begin
    Result := 0;
   {See if we can find this line name in the present collection}
    OtherLine := Find(LineName);
    if OtherLine <> NIL then
        with ActiveLineObj do
        begin

            if Fnphases <> OtherLine.Fnphases then
            begin
                Nphases := OtherLine.Fnphases;
                NConds := Fnphases; // force reallocation of terminals and conductors

                Yorder := Fnconds * Fnterms;
                YPrimInvalid := TRUE;

                if Z <> NIL then
                    Z.Free;
                if Zinv <> NIL then
                    Zinv.Free;
                if Yc <> NIL then
                    Yc.Free;

         // For a line, nphases = ncond, for now
                Z := TCmatrix.CreateMatrix(Fnphases);
                Zinv := TCMatrix.CreateMatrix(Fnphases);
                Yc := TCMatrix.CreateMatrix(Fnphases);
            end;

            Z.CopyFrom(OtherLine.Z);
       // Zinv.CopyFrom(OtherLine.Zinv);
            Yc.CopyFrom(OtherLine.Yc);

            R1 := OtherLine.R1;
            X1 := OtherLine.X1;
            R0 := OtherLine.R0;
            X0 := OtherLine.X0;
            C1 := OtherLine.C1;
            C0 := OtherLine.C0;
            Len := OtherLine.Len;

            SymComponentsModel := OtherLine.SymComponentsModel;
            FCapSpecified := OtherLine.FCapSpecified;

            ClassMakeLike(OtherLine);  // Take care of inherited class properties

            for i := 1 to ParentClass.NumProperties do
                FPropertyValue^[i] := OtherLine.FPropertyValue^[i];
            Result := 1;
        end
    else
        DoSimpleMsg('Error in Line MakeLike: "' + LineName + '" Not Found.', 182);

end;

//- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
function TLine.Init(Handle: Integer): Integer;

begin
    DoSimpleMsg('Need to implement TLine.Init', -1);
    Result := 0;
end;


//- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
//      TLine Obj
//- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

constructor TLineObj.Create(ParClass: TDSSClass; const LineName: String);

begin
    inherited Create(ParClass);

    Name := LowerCase(LineName);
    DSSObjType := ParClass.DSSClassType; // DSSObjType + LINESECTION; // in both PDElement list and Linesection lists

    Nphases := 3;  // Directly set conds and phases
    Fnconds := 3;
    Nterms := 2;  // Force allocation of terminals and conductors
    IsSwitch := FALSE;
    R1 := 0.0580;  //ohms per 1000 ft
    X1 := 0.1206;
    R0 := 0.1784;
    X0 := 0.4047;
    C1 := 3.4e-9;  // nf per 1000ft
    C0 := 1.6e-9;
    Len := 1.0;   // 1 kFt
    Z := NIL;
    Zinv := NIL;
    Yc := NIL;
    CondCode := '';

    Rg := 0.01805;    //ohms per 1000 ft
    Xg := 0.155081;
    rho := 100.0;
    Kxg := Xg / ln(658.5 * sqrt(rho / BaseFrequency));
    FrhoSpecified := FALSE;
    FCapSpecified := FALSE;

     {Basefrequency := 60.0;}  // set in base class
    Normamps := 400.0;
    EmergAmps := 600.0;
    PctPerm := 20.0;
    FaultRate := 0.1; // per mile per year
    HrsToRepair := 3.0;

    SymComponentsChanged := FALSE;
    SymComponentsModel := TRUE;

    GeometrySpecified := FALSE;
    GeometryCode := '';
    LengthUnits := UNITS_NONE; // Assume everything matches
    FUnitsConvert := 1.0;
    FLineCodeUnits := UNITS_NONE;
    FLineCodeSpecified := FALSE;
    FEarthModel := DSSPrime.DefaultEarthModel;

    SpacingSpecified := FALSE;
    FLineSpacingObj := NIL;
    FLineWireData := NIL;
    FWireDataSize := 0;
    FPhaseChoice := Unknown;
    SpacingCode := '';

    FZFrequency := -1.0; // indicate Z not computed.
    FLineGeometryObj := NIL;

    InitPropertyValues(0);


    Yorder := Fnterms * Fnconds;
    RecalcElementData;

    NumAmpRatings := 1;
    setlength(AmpRatings, NumAmpRatings);
    AmpRatings[0] := NormAmps;


end;

//- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
destructor TLineObj.Destroy;

begin
    if Assigned(Z) then
        Z.Free;
    if Assigned(Zinv) then
        Zinv.Free;
    if Assigned(Yc) then
        Yc.Free;
    Reallocmem(FLineWireData, 0);
    inherited destroy;
end;

//- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
procedure TLineObj.ReallocZandYcMatrices;
begin
    if Z <> NIL then
        Z.Free;
    if Zinv <> NIL then
        Zinv.Free;
    if Yc <> NIL then
        Yc.Free;

    // For a line, nphases = ncond, for now
    Z := TCmatrix.CreateMatrix(Fnphases);
    Zinv := TCMatrix.CreateMatrix(Fnphases);
    Yc := TCMatrix.CreateMatrix(Fnphases);
end;

procedure TLineObj.DoLongLine(Frequency: Double);
// do long line correction for len and frequwnen

var
    Zs, Zm, Ys, Ym: Complex;
    GammaL, ExpP, ExpM, Exp2P, Exp2M, SinhGL, Tanh2GL: Complex;

begin

 // nominal PI parameters per unit length but Len variable is used here
    Zs := cmplx(R1, X1);
    Ys := cmplx(0.0, TwoPi * Frequency * C1);
        // apply the long-line correction to obtain Zm and Ym
    GammaL := Csqrt(Cmul(Zs, Ys));
    GammaL := CmulReal(GammaL, Len);
    ExpP := CmulReal(cmplx(cos(GammaL.im), sin(GammaL.im)), exp(GammaL.re));
    Exp2P := CmulReal(cmplx(cos(0.5 * GammaL.im), sin(0.5 * GammaL.im)), exp(0.5 * GammaL.re));
    ExpM := Cinv(ExpP);
    Exp2M := Cinv(Exp2P);
    SinhGL := CmulReal(Csub(ExpP, ExpM), 0.5);
    Tanh2GL := Cdiv(Csub(Exp2P, Exp2M), Cadd(Exp2P, Exp2M));
    Zm := Cdiv(Cmul(CMulReal(Zs, Len), SinhGL), GammaL);
    Ym := Cdiv(Cmul(CMulReal(Ys, Len), Tanh2GL), CmulReal(GammaL, 0.5));
        // rely on this function being called only once, unless R1, X1, or C1 changes
    R1 := Zm.re / Len;
    X1 := Zm.im / Len;
    C1 := Ym.im / Len / TwoPi / Frequency;

end;

procedure TLineObj.RecalcElementData;

{
  This routine is only called when the symmetrical component data have changed
  It computes the values for Z and Yc in ohms per unit length

  Can also compute long line correction for 1-phase pos sequence line models
}
var
    Zs, Zm, Ys, Ym, Ztemp: Complex;
    i, j: Integer;
    Yc1, Yc0, OneThird: Double;

begin

    ReallocZandYcMatrices;

    OneThird := 1.0 / 3.0;  // Do this to get more precision in next few statements

    {Only time this is called is if symmetrical components are specified}

    Ztemp := CmulReal(cmplx(R1, X1), 2.0);
    {Handle special case for 1-phase line and/or pos seq model }
    if (FnPhases = 1) or ActiveCircuit.PositiveSequence then
    begin
      // long-line equivalent PI, but only for CktModel=Positive
        if ActiveCircuit.PositiveSequence and (C1 > 0) then
        begin
            DoLongLine(BaseFrequency);  // computes R1, X1, C1  per unit length
        end;
      // zero sequence the same as positive sequence
        R0 := R1;
        X0 := X1;
        C0 := C1;
    end;

    Zs := CmulReal(CAdd(Ztemp, Cmplx(R0, X0)), OneThird);
    Zm := CmulReal(Csub(cmplx(R0, X0), Cmplx(R1, X1)), OneThird);

    Yc1 := TwoPi * BaseFrequency * C1;
    Yc0 := TwoPi * BaseFrequency * C0;

    Ys := CMulReal(Cadd(CMulReal(Cmplx(0.0, Yc1), 2.0), Cmplx(0.0, Yc0)), OneThird);
    Ym := CmulReal(Csub(cmplx(0.0, Yc0), Cmplx(0.0, Yc1)), OneThird);

    for i := 1 to Fnphases do
    begin
        Z.SetElement(i, i, Zs);
        Yc.SetElement(i, i, Ys);
        for j := 1 to i - 1 do
        begin
            Z.SetElemsym(i, j, Zm);
            Yc.SetElemsym(i, j, Ym);
        end;
    end;

    SymComponentsChanged := FALSE;

    // values in ohms per unit length

end;

//- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
procedure TLineObj.CalcFltRate;
begin
  // inherited;

  // Assume Faultrate specified in same units as length
    BranchFltRate := Faultrate * pctperm * 0.01 * Len;
end;

procedure TLineObj.CalcYPrim;

var
    Value: Complex;
    ZinvValues: pComplexArray;
    ZValues: pComplexArray;
    YValues: pComplexArray;

    FreqMultiplier: Double;
    XgMod: Double;
    LengthMultiplier: Double;

    i, j, k, Norder: Integer;

begin
    FreqMultiplier := 1.0;
    LengthMultiplier := 1.0;

    if SymComponentsChanged then
    begin
      {Try to catch inadvertent user error when they forget to specify C1 and C0 }
      {Check to see if user has spec'd C1 and C0. If not, adjust default values for new length units}
        if not FCapSpecified then
        begin
            C1 := C1 / ConvertLineUnits(UNITS_KFT, LengthUnits); // were defined in kft
            C0 := C0 / ConvertLineUnits(UNITS_KFT, LengthUnits);
            FCapSpecified := TRUE;   // so we don't do it again
        end;

        RecalcElementData;
    end;

    ClearYPrim;


    // Build Series YPrim
    with YPrim_Series do
    begin

         {Build Zmatrix}
        if GeometrySpecified then
        begin

            FMakeZFromGeometry(ActiveCircuit.Solution.Frequency); // Includes length in proper units
            if DSSPrime.SolutionAbort then
                Exit;

        end
        else
        if SpacingSpecified then
        begin

            FMakeZFromSpacing(ActiveCircuit.Solution.Frequency); // Includes length in proper units
            if DSSPrime.SolutionAbort then
                Exit;

        end
        else
        begin  // Z is from line code or specified in line data
               // In this section Z is assumed in ohms per unit length
            LengthMultiplier := Len / FUnitsConvert;   // convert to per unit length
            FYprimFreq := ActiveCircuit.Solution.Frequency;
            FreqMultiplier := FYprimFreq / BaseFrequency;

               { Put in Series RL }
            ZValues := Z.GetValuesArrayPtr(Norder);
            ZinvValues := Zinv.GetValuesArrayPtr(Norder);

               // Correct the impedances for length and frequency
               // Rg increases with frequency
               // Xg modified by ln of sqrt(1/f)
            if Xg <> 0.0 then
                Xgmod := 0.5 * KXg * ln(FreqMultiplier)
            else
                Xgmod := 0.0;

            for i := 1 to Norder * Norder do
                ZinvValues^[i] := Cmplx((ZValues^[i].re + Rg * (FreqMultiplier - 1.0)) * LengthMultiplier, (ZValues^[i].im - Xgmod) * LengthMultiplier * FreqMultiplier);
            Zinv.Invert;  {Invert Z in place to get values to put in Yprim}


        end;

      {At this point have Z and Zinv in proper values including length}
      {If GIC simulation, convert Zinv back to sym components, R Only }

        if ActiveCircuit.Solution.Frequency < 0.51 then     // 0.5 Hz is cutoff
            ConvertZinvToPosSeqR;


        if Zinv.Inverterror > 0 then
        begin
                 {If error, put in tiny series conductance}
// TEMc - shut this up for the CDPSM connectivity profile test, or whenever else it gets annoying
            DoErrorMsg('TLineObj.CalcYPrim', 'Matrix Inversion Error for Line "' + Name + '"',
                'Invalid impedance specified. Replaced with tiny conductance.', 183);
            Zinv.Clear;
            for i := 1 to Fnphases do
                Zinv.SetElement(i, i, Cmplx(epsilon, 0.0));
        end
        else
           { Now, Put in Yprim_Series matrix }
            for i := 1 to Fnphases do
            begin
                for j := 1 to Fnphases do
                begin
                    Value := Zinv.GetElement(i, j);
                    SetElement(i, j, Value);
                    SetElement(i + Fnphases, j + Fnphases, Value);
                    Value := cnegate(Value);
                    SetElemSym(i, j + Fnphases, Value);
                end;
            end;


    end;   {With Yprim_series}

    YPrim.Copyfrom(Yprim_Series);      // Initialize YPrim for series impedances

     // 10/3/2006 moved this to after the copy to Yprim so it doesn't affect normal line model capacitance
        // 3-30-04  ----- Rev 2-4-09 to include both sides of line
        // Increase diagonal elements of both sides of line so that we will avoid isolated bus problem
        // add equivalent of 10 kvar capacitive at 345 kV
    with Yprim_Series do
        for i := 1 to Yorder do
            AddElement(i, i, CAP_EPSILON);

     // Now Build the Shunt admittances and add into YPrim
    if ActiveCircuit.Solution.Frequency > 0.51 then   // Skip Capacitance for GIC
        with YPrim_Shunt do
        begin

         {Put half the Shunt Capacitive Admittance at each end}
            YValues := Yc.GetValuesArrayPtr(Norder);

            if GeometrySpecified or SpacingSpecified then
            begin

            {Values are already compensated for length and frequency}
                k := 0;
                for j := 1 to Fnphases do
                    for i := 1 to Fnphases do
                    begin
                        Inc(k);    // Assume matrix in col order (1,1  2,1  3,1 ...)
                        Value := CDivReal(YValues^[k], 2.0);  // half at each end ...
                        AddElement(i, j, Value);
                        AddElement(i + Fnphases, j + Fnphases, Value);
                    end;

            end
            else
            begin
             {Regular line model - values computed per unit length at base frequency}
                k := 0;
                for j := 1 to Fnphases do
                    for i := 1 to Fnphases do
                    begin
                        Inc(k);    // Assume matrix in col order (1,1  2,1  3,1 ...)
                        Value := Cmplx(0.0, YValues^[k].im * LengthMultiplier * FreqMultiplier / 2.0);
                        AddElement(i, j, Value);
                        AddElement(i + Fnphases, j + Fnphases, Value);
                    end;

            end;

         {Now Account for Open Conductors}
         {For any conductor that is open, zero out row and column}

        end; {With YPRIM}

    YPrim.AddFrom(Yprim_Shunt);
    inherited CalcYPrim;
    YprimInvalid := FALSE;

end;

procedure TLineObj.DumpProperties(var F: TextFile; Complete: Boolean);

var
    i, j: Integer;

begin
    inherited DumpProperties(F, Complete);


    with ParentClass do
    begin
        Writeln(F, '~ ', PropertyName^[1], '=', firstbus);
        Writeln(F, '~ ', PropertyName^[2], '=', nextbus);

        Writeln(F, '~ ', PropertyName^[3], '=', CondCode);
        Writeln(F, '~ ', PropertyName^[4], '=', len: 0: 3);
        Writeln(F, '~ ', PropertyName^[5], '=', Fnphases: 0);
        Writeln(F, '~ ', PropertyName^[6], '=', R1: 0: 5);
        Writeln(F, '~ ', PropertyName^[7], '=', X1: 0: 5);
        Writeln(F, '~ ', PropertyName^[8], '=', R0: 0: 5);
        Writeln(F, '~ ', PropertyName^[9], '=', X0: 0: 5);
        Writeln(F, '~ ', PropertyName^[10], '=', C1 * 1.0e9: 0: 5);
        Writeln(F, '~ ', PropertyName^[11], '=', C0 * 1.0e9: 0: 5);
        Write(F, '~ ', PropertyName^[12], '=', '"');
        for i := 1 to Fnphases do
        begin
            for j := 1 to Fnphases do
            begin
                Write(F, Z.GetElement(i, j).re: 0: 5, ' ');
            end;
            Write(F, '|');
        end;
        Writeln(F, '"');
        Write(F, '~ ', PropertyName^[13], '=', '"');
        for i := 1 to Fnphases do
        begin
            for j := 1 to Fnphases do
            begin
                Write(F, Z.GetElement(i, j).im: 0: 5, ' ');
            end;
            Write(F, '|');
        end;
        Writeln(F, '"');
        Write(F, '~ ', PropertyName^[14], '=', '"');
        for i := 1 to Fnphases do
        begin
            for j := 1 to Fnphases do
            begin
                Write(F, (Yc.GetElement(i, j).im / TwoPi / BaseFrequency * 1.0E9): 0: 2, ' ');
            end;
            Write(F, '|');
        end;
        Writeln(F, '"');

        Write(F, '~ ', PropertyName^[15], '=');
        if IsSwitch then
            Writeln(F, 'true')
        else
            writeln(F, 'false');

         {Dump the rest by default}
        for i := 16 to NumProperties do
        begin
            Writeln(F, '~ ', PropertyName^[i], '=', PropertyValue[i]);
        end;
    end;

end;


{*********** Placeholder for Line module No Load Loss procedure *********}
procedure TLineObj.GetLosses(var TotalLosses, LoadLosses, NoLoadLosses: Complex);
begin

  {For Now, we'll just do the default behavior until we implement shunt losses}

    inherited;

end;

function TLineObj.GetPropertyValue(Index: Integer): String;
var
    k,
    i, j: Integer;
    Factor: Double;
    TempStr: String;
begin


    case Index of
        12..14:
            Result := '[';

    else
        Result := '';
    end;
       {Report Impedance values in ohms per unit length of present length units}
    case Index of
        1:
            Result := GetBus(1);
        2:
            Result := GetBus(2);
        4:
            Result := Format('%-.7g', [Len]);
        5:
            Result := Format('%d', [FNphases]);
        6:
            if SymComponentsModel then
                Result := Format('%-.7g', [R1 / FUnitsConvert])
            else
                Result := '----';
        7:
            if SymComponentsModel then
                Result := Format('%-.7g', [X1 / FUnitsConvert])
            else
                Result := '----';
        8:
            if SymComponentsModel then
                Result := Format('%-.7g', [R0 / FUnitsConvert])
            else
                Result := '----';
        9:
            if SymComponentsModel then
                Result := Format('%-.7g', [X0 / FUnitsConvert])
            else
                Result := '----';
        10:
            if SymComponentsModel then
                Result := Format('%-.7g', [C1 * 1.0e9 / FUnitsConvert])
            else
                Result := '----';
        11:
            if SymComponentsModel then
                Result := Format('%-.7g', [C0 * 1.0e9 / FUnitsConvert])
            else
                Result := '----';

        12:
            for i := 1 to FNconds do   // R matrix
            begin
                for j := 1 to i do
                begin  // report in per unit Length in length units
                    if GeometrySpecified or SpacingSpecified then
                        Result := Result + Format('%-.7g', [Z.GetElement(i, j).re / len]) + ' '
                    else
                        Result := Result + Format('%-.7g', [Z.GetElement(i, j).re / FUnitsConvert]) + ' ';
                end;
                if i < FNconds then
                    Result := Result + '|';
            end;

        13:
            for i := 1 to FNconds do      // X matrix
            begin
                for j := 1 to i do
                begin
                    if GeometrySpecified or SpacingSpecified then
                        Result := Result + Format('%-.7g', [Z.GetElement(i, j).im / Len]) + ' '
                    else
                        Result := Result + Format('%-.7g', [Z.GetElement(i, j).im / FUnitsConvert]) + ' ';
                end;
                if i < FNconds then
                    Result := Result + '|';
            end;

        14:
        begin  // CMatrix  nf
            Factor := TwoPi * BaseFrequency * 1.0e-9;
            for i := 1 to FNconds do
            begin
                for j := 1 to i do
                begin
                    if GeometrySpecified or SpacingSpecified then
                        Result := Result + Format('%-.7g', [YC.GetElement(i, j).im / Factor / Len]) + ' '
                    else
                        Result := Result + Format('%-.7g', [YC.GetElement(i, j).im / Factor / FUnitsConvert]) + ' ';
                end;
                if i < FNconds then
                    Result := Result + '|';
            end;
        end;
        15:
            if IsSwitch then
                Result := 'True'
            else
                Result := 'False';
        16:
            Result := Format('%-g', [Rg]);
        17:
            Result := Format('%-g', [Xg]);
        18:
            Result := Format('%-g', [Rho]);
        20:
            Result := LineUnitsStr(LengthUnits);
        23:
            Result := GetEarthModel(FEarthModel);
        26:
            if SymComponentsModel then
                Result := Format('%.7g', [twopi * Basefrequency * C1 * 1.0e6])
            else
                Result := '----';
        27:
            if SymComponentsModel then
                Result := Format('%.7g', [twopi * Basefrequency * C0 * 1.0e6])
            else
                Result := '----';
        28:
            Result := inttostr(NumAmpRatings);
        29:
        begin
            TempStr := '[';
            for  k := 1 to NumAmpRatings do
                TempStr := TempStr + floattoStrf(AmpRatings[k - 1], ffGeneral, 8, 4) + ',';
            TempStr := TempStr + ']';
            Result := TempStr;
        end;

           // Intercept FaultRate, PctPerm, and HourstoRepair
        (NumPropsThisClass + 3):
            Result := Format('%-g', [FaultRate]);

        (NumPropsThisClass + 4):
            Result := Format('%-g', [PctPerm]);

        (NumPropsThisClass + 5):
            Result := Format('%-g', [HrsToRepair]);

    else
        Result := inherited GetPropertyValue(index);
    end;

    case Index of
        12..14:
            Result := Result + ']';

    else
    end;


end;

procedure TLineObj.GetSeqLosses(var PosSeqLosses, NegSeqLosses, ZeroSeqLosses: complex);

{ Only consider 3-phase branches with Pos seq >> Neg seq
  Otherwise, we don't know whether it is a 3-phase line or just a line with 3 phases
}

var
    i, j, k: Integer;
    Vph,
    V012,
    I012: array[0..2] of Complex;

begin

    PosSeqLosses := CZERO;
    NegSeqLosses := CZERO;
    ZeroSeqLosses := CZERO;

    {Method: sum seq powers going into each terminal
    }

    if Fnphases = 3 then
    begin   {3-phase lines only}
        ComputeIterminal;
        for i := 1 to 2 do
        begin
            k := (i - 1) * Fnphases + 1;
            for j := 0 to 2 do
                Vph[j] := ActiveCircuit.Solution.NodeV^[NodeRef^[k + j]];
            Phase2SymComp(@Vph, @V012);
            Phase2SymComp(@Iterminal^[k], @I012);
            Caccum(PosSeqLosses, Cmul(V012[1], Conjg(I012[1])));
            Caccum(NegSeqLosses, Cmul(V012[2], Conjg(I012[2]))); // accumulate both line modes
            Caccum(ZeroSeqLosses, Cmul(V012[0], Conjg(I012[0])));
        end;
        cmulrealaccum(PosSeqLosses, 3.0);
        cmulrealaccum(NegSeqLosses, 3.0);
        cmulrealaccum(ZeroSeqLosses, 3.0);
    end;

end;

procedure TLineObj.InitPropertyValues(ArrayOffset: Integer);
begin

    PropertyValue[1] := Getbus(1);
    PropertyValue[2] := Getbus(2);
    PropertyValue[3] := '';
    PropertyValue[4] := '1.0';  // '5.28'; Changed 2/17/00
    PropertyValue[5] := '3';
    PropertyValue[6] := '.058';
    PropertyValue[7] := '.1206';
    PropertyValue[8] := '.1784';
    PropertyValue[9] := '.4047';
    PropertyValue[10] := '3.4';
    PropertyValue[11] := '1.6';
    PropertyValue[12] := '';
    PropertyValue[13] := '';
    PropertyValue[14] := '';
    PropertyValue[15] := 'false';
    PropertyValue[16] := '0.01805';
    PropertyValue[17] := '0.155081';
    PropertyValue[18] := '100';
    PropertyValue[19] := '';
    PropertyValue[20] := 'NONE';
    PropertyValue[21] := '';
    PropertyValue[22] := '';
    PropertyValue[23] := GetEarthModel(SIMPLECARSON);
    PropertyValue[24] := '';
    PropertyValue[25] := '';
    PropertyValue[26] := '1.2818'; // B1  microS
    PropertyValue[27] := '0.60319'; // B0  microS
    PropertyValue[28] := '1';      // 1 Season
    PropertyValue[29] := '[400]';  // 1 Season


    inherited InitPropertyValues(NumPropsThisClass);

      // Override Inherited properties  just in case
    PropertyValue[NumPropsThisClass + 1] := '400';  //Normamps
    PropertyValue[NumPropsThisClass + 2] := '600';  //emergamps
    PropertyValue[NumPropsThisClass + 3] := '0.1';  //Fault rate
    PropertyValue[NumPropsThisClass + 4] := '20';   // Pct Perm
    PropertyValue[NumPropsThisClass + 5] := '3';    // Hrs to repair

    ClearPropSeqArray;
end;

procedure TLineObj.MakePosSequence;
var
    S: String;
    C1_new, Cs, Cm: Double;
    Z1, ZS, Zm: Complex;
    i, j: Integer;
begin
// set to single phase and make sure R1, X1, C1 set.
// If already single phase, let alone
    if FnPhases > 1 then
    begin
    // Kill certain propertyvalue elements to get a cleaner looking save
        PrpSequence^[3] := 0;
        for i := 6 to 14 do
            PrpSequence^[i] := 0;

        if IsSwitch then
        begin
            S := ' R1=1 X1=1 C1=1.1 Phases=1 Len=0.001'
        end
        else
        begin
            if SymComponentsModel then
            begin  // keep the same Z1 and C1
                Z1.re := R1;
                Z1.im := X1;
                C1_new := C1 * 1.0e9; // convert to nF
            end
            else
            begin // matrix was input directly, or built from physical data
        // average the diagonal and off-dialgonal elements
                Zs := CZERO;
                for i := 1 to FnPhases do
                    Caccum(Zs, Z.GetElement(i, i));
                Zs := CdivReal(Zs, Fnphases);
                Zm := CZERO;
                for i := 1 to FnPhases - 1 do  // Corrected 6-21-04
                    for j := i + 1 to FnPhases do
                        Caccum(Zm, Z.GetElement(i, j));
                Zm := CdivReal(Zm, (Fnphases * (FnPhases - 1.0) / 2.0));
                Z1 := CSub(Zs, Zm);

        // Do same for Capacitances
                Cs := 0.0;
                for i := 1 to FnPhases do
                    Cs := Cs + Yc.GetElement(i, i).im;
                Cm := 0.0;
                for i := 2 to FnPhases do
                    for j := i + 1 to FnPhases do
                        Cm := Cm + Yc.GetElement(i, j).im;
                C1_new := (Cs - Cm) / TwoPi / BaseFrequency / (Fnphases * (FnPhases - 1.0) / 2.0) * 1.0e9; // nanofarads

        // compensate for length units
                Z1 := CDivReal(Z1, FunitsConvert);
                C1_New := C1_New / FunitsConvert;
            end;
            S := Format(' R1=%-.5g  %-.5g  C1=%-.5g Phases=1', [Z1.re, Z1.im, C1_new]);
        end;
    // Conductor Current Ratings
        S := S + Format(' Normamps=%-.5g  %-.5g', [NormAmps, EmergAmps]);
    // Repeat the Length Units to compensate for unexpected reset
        S := S + ' Units=' + LineUnitsStr(LengthUnits);
        Parser.CmdString := S;
        Edit;
    end;

    inherited MakePosSequence;
end;

function TLineObj.MergeWith(var OtherLine: TLineObj; Series: Boolean): Boolean;

// Cleaned up and corrected 3-17-15

// Merge this line with another line and disble the other line.
var
    Values1, Values2: pComplexArray;
    Order1, Order2,
    i, j,
    Common1, Common2: Integer;
    TotalLen, wnano: Double;
    S, NewName: String;
    TestBusNum: Integer;
    LenUnitsSaved: Integer;
    NewZ: Complex;
    LenSelf, LenOther: Double;

begin
    Result := FALSE;     // initialize the result
    if OtherLine <> NIL then
    begin
        if Fnphases <> OtherLine.Fnphases then
            Exit;  // Can't merge

        LenUnitsSaved := LengthUnits;

        YPrimInvalid := TRUE;

      // Redefine property values to make it appear that line was defined this way originally using matrices

        if Series then
            TotalLen := Len + Otherline.Len * ConvertLineUnits(OtherLine.LengthUnits, LengthUnits)
        else
            TotalLen := 1.0;

        if Series then
        begin
           { redefine the bus connections}

           // Find the bus in common between the two lines
            Common1 := 0;
            Common2 := 0;
            i := 1;
            while (Common1 = 0) and (i <= 2) do
            begin
                TestBusNum := ActiveCircuit.MapNodeToBus^[NodeRef^[1 + (i - 1) * Fnconds]].BusRef;
                for j := 1 to 2 do
                begin
                    if ActiveCircuit.MapNodeToBus^[OtherLine.NodeRef^[1 + (j - 1) * OtherLine.Nconds]].BusRef = TestBusNum then
                    begin
                        Common1 := i;
                        Common2 := j;
                        Break;
                    end;
                end;
                inc(i);
            end;

            if Common1 = 0 then
                Exit;  // There's been an error; didn't find anything in common

           {Redefine the bus connections, eliminating the common bus}
            case Common1 of
                1:
                    case Common2 of
                        1:
                            S := 'Bus1="' + OtherLine.GetBus(2) + '"';
                        2:
                            S := 'Bus1="' + OtherLine.GetBus(1) + '"';
                    end;
                2:
                    case Common2 of
                        1:
                            S := 'Bus2="' + OtherLine.GetBus(2) + '"';
                        2:
                            S := 'Bus2="' + OtherLine.GetBus(1) + '"';
                    end;
            end;

            Parser.cmdstring := S;
            Edit;

        end; {If Series}

      {Rename the line}
        if Series then
            NewName := OtherLine.Name + '~' + Name //(GetBus(1)) + '~'  + StripExtension(GetBus(2))
        else
            NewName := StripExtension(GetBus(1)) + '||' + StripExtension(GetBus(2));

       {Update ControlElement Connections to This Line}
        UpdateControlElements('line.' + NewName, 'line.' + Name);
        UpdateControlElements('line.' + NewName, 'line.' + OtherLine.Name);
        Name := NewName;

        if Series then
            IsSwitch := FALSE; // not allowed on series merge.

       {Now Do the impedances}

        LenSelf := Len / FunitsConvert;  // in units of the R X Data
        LenOther := OtherLine.Len / OtherLine.FunitsConvert;

       { If both lines are Symmmetrical Components, just merge the R1..C0 values}
       { This will catch many 3-phase lines since this is a common way to define lines}
        if SymComponentsModel and OtherLine.SymComponentsModel and (nphases = 3) then
        begin   {------------------------- Sym Component Model ----------------------------------}
            if Series then
            begin
                S := ' R1=' + Format('%-g', [(R1 * LenSelf + OtherLine.R1 * LenOther) / TotalLen]);     // Ohms per unit length of this line length units
                S := S + Format(' %-g', [(X1 * LenSelf + OtherLine.X1 * LenOther) / TotalLen]);
                S := S + Format(' %-g', [(R0 * LenSelf + OtherLine.R0 * LenOther) / TotalLen]);
                S := S + Format(' %-g', [(X0 * LenSelf + OtherLine.X0 * LenOther) / TotalLen]);
                S := S + Format(' %-g', [(C1 * LenSelf + OtherLine.C1 * LenOther) / TotalLen * 1.0e9]);
                S := S + Format(' %-g', [(C0 * LenSelf + OtherLine.C0 * LenOther) / TotalLen * 1.0e9]);
            end
            else   {parallel}
            begin
                if IsSwitch then
                    S := ''   {Leave as is if switch; just dummy z anyway}
                else
                if OtherLine.IsSwitch then
                    S := ' switch=yes'   {This will take care of setting Z's}
                else
                begin
{********* Will This work with Length multiplier?  did it ever work? *************************}
                    NewZ := ParallelZ(Cmplx(R1 * Len, X1 * Len), Cmplx(OtherLine.R1 * OtherLine.Len, OtherLine.X1 * OtherLine.Len));
                    S := ' R1=' + Format('%-g %-g ', [NewZ.Re, NewZ.im]);
                    NewZ := ParallelZ(Cmplx(R0 * Len, X0 * Len), Cmplx(OtherLine.R0 * OtherLine.Len, OtherLine.X0 * OtherLine.Len));
                    S := ' R0=' + Format('%-g %-g ', [NewZ.Re, NewZ.im]);
                    S := S + Format(' %-g', [(C1 * Len + OtherLine.C1 * OtherLine.Len) / TotalLen * 1.0e9]);
                    S := S + Format(' %-g', [(C0 * Len + OtherLine.C0 * OtherLine.Len) / TotalLen * 1.0e9]);
                end;
            end;

            Parser.cmdstring := S;   // This reset the length units
            Edit;

          // update length units
            Parser.cmdstring := Format(' Length=%-g  Units=%s', [TotalLen, LineUnitsStr(LenUnitsSaved)]);
            Edit;

          // Update symmetrical Components computation
          // (Only time this function is called is for sym comp update -- computes Z and Yc)
            RecalcelementData;

        end
        else  {------------- Matrix Model for anything other than Symmetrical Components -------------------------}
        if not Series then
            TotalLen := Len / 2.0 {We'll assume lines are equal for now}
        else
        begin  {Matrices were defined}

               // Merge Z matrices
            Values1 := Z.GetValuesArrayPtr(Order1);
            Values2 := OtherLine.Z.GetValuesArrayPtr(Order2);

            if Order1 <> Order2 then
                Exit;  // OOps.  Lines not same size for some reason

               // If Geometry specified, length is already included; so reset to 1.0
            if GeometrySpecified or SpacingSpecified then
                LenSelf := 1.0;
            if OtherLine.GeometrySpecified or OtherLine.SpacingSpecified then
                LenOther := 1.0;

               // Z <= (Z1 + Z2 )/TotalLen   to get equiv ohms per unit length
            for i := 1 to Order1 * Order1 do
                Values1^[i] := CDivReal(Cadd(CmulReal(Values1^[i], LenSelf), CmulReal(Values2^[i], LenOther)), TotalLen);

               // Merge Yc matrices
            Values1 := Yc.GetValuesArrayPtr(Order1);
            Values2 := OtherLine.Yc.GetValuesArrayPtr(Order2);

            if Order1 <> Order2 then
                Exit;  // OOps.  Lines not same size for some reason

            for i := 1 to Order1 * Order1 do
                Values1^[i] := CDivReal(Cadd(CmulReal(Values1^[i], LenSelf), CmulReal(Values2^[i], LenOther)), TotalLen);

               {R Matrix}
            S := 'Rmatrix=[';
            for i := 1 to Order1 do
            begin
                for j := 1 to i do
                    S := S + Format(' %-g', [Z.GetElement(i, j).Re]);
                S := S + ' | ';
            end;
            S := S + '] Xmatrix=[';
               {X Matrix}
            for i := 1 to Order1 do
            begin
                for j := 1 to i do
                    S := S + Format(' %-g', [Z.GetElement(i, j).im]);
                S := S + ' | ';
            end;
            S := S + ']';
            Parser.cmdstring := S;
            Edit;

               {C Matrix}
            wnano := TwoPi * BaseFrequency / 1.0e9;
            S := 'Cmatrix=[';
            for i := 1 to Order1 do
            begin
                for j := 1 to i do
                    S := S + Format(' %-g', [(Yc.GetElement(i, j).im / wnano)]);   // convert from mhos to nanofs
                S := S + ' | ';
            end;
            S := S + '] ';
            Parser.cmdstring := S;
            Edit;

               // update length units
            Parser.cmdstring := Format(' Length=%-g  Units=%s', [TotalLen, LineUnitsStr(LenUnitsSaved)]);
            Edit;
        end;  {Matrix definition}


        OtherLine.Enabled := FALSE;  // Disable the Other Line
        Result := TRUE;
    end
    else
        DoSimpleMsg('Error in Line Merge: Attempt to merge with invalid (nil) line object found.', 184);


end;

procedure TLineObj.UpdateControlElements(const NewName, OldName: String);

var
    pControlElem: TControlElem;
begin

    pControlElem := ActiveCircuit.DSSControls.First;
    while pControlElem <> NIL do
    begin
        if CompareText(OldName, pControlElem.ElementName) = 0 then
        begin
            Parser.cmdstring := ' Element=' + NewName;  // Change name of the property
            pControlElem.Edit;
        end;
        pControlElem := ActiveCircuit.DSSControls.Next;
    end;
end;

procedure TLineObj.FetchLineSpacing(const Code: String);
begin
    if DSSPrime.LineSpacingClass.SetActive(Code) then
    begin

        FLineSpacingObj := DSSPrime.LineSpacingClass.GetActiveObj;
        FLineCodeSpecified := FALSE;
        KillGeometrySpecified;
        SpacingCode := LowerCase(Code);

      // need to establish Yorder before FMakeZFromSpacing
        NPhases := FLineSpacingObj.NPhases;
        Nconds := FNPhases;  // Force Reallocation of terminal info
        Yorder := Fnconds * Fnterms;
        YPrimInvalid := TRUE;       // Force Rebuild of Y matrix

    end
    else
        DoSimpleMsg('Line Spacing object ' + Code + ' not found.(LINE.' + Name + ')', 181011);
end;

procedure TLineObj.FetchWireList(const Code: String);
var
    RatingsInc: Boolean;
    NewNumRat,
    j,
    i, istart: Integer;
    NewRatings: array of Double;
begin
    if not assigned(FLineSpacingObj) then
        DoSimpleMsg('You must assign the LineSpacing before the Wires Property (LINE.' + name + ').', 18102);

    if FPhaseChoice = Unknown then
    begin // it's an overhead line
        FLineCodeSpecified := FALSE;
        KillGeometrySpecified;
        FWireDataSize := FLineSpacingObj.NWires;
        FLineWireData := Allocmem(Sizeof(FLineWireData^[1]) * FWireDataSize);
        istart := 1;
        FPhaseChoice := Overhead;
    end
    else
    begin // adding bare neutrals to an underground line - TODO what about repeat invocation?
        istart := FLineSpacingObj.NPhases + 1;
    end;

    AuxParser.CmdString := Code;

    NewNumRat := 1;
    RatingsInc := FALSE;             // So far we don't know if there are seasonal ratings
    for i := istart to FLineSpacingObj.NWires do
    begin
        AuxParser.NextParam; // ignore any parameter name  not expecting any
        DSSPrime.WireDataClass.code := AuxParser.StrValue;
        if Assigned(ActiveConductorDataObj) then
        begin
            FLineWireData^[i] := ActiveConductorDataObj;
            if FLineWireData^[i].NumAmpRatings > NewNumRat then
            begin
                NewNumRat := FLineWireData^[i].NumAmpRatings;
                setlength(NewRatings, NewNumRat);
                for j := 0 to High(NewRatings) do
                    NewRatings[j] := FLineWireData^[i].AmpRatings[j];
                RatingsInc := TRUE;         // Yes, there are seasonal ratings
            end;
        end
        else
            DoSimpleMsg('Wire "' + AuxParser.StrValue + '" was not defined first (LINE.' + name + ').', 18103);
    end;

    if RatingsInc then
    begin
        NumAmpRatings := NewNumRat;
        setlength(AmpRatings, NumAmpRatings);
        for j := 0 to High(AmpRatings) do
            AmpRatings[j] := NewRatings[j];
    end;

end;

procedure TLineObj.FetchCNCableList(const Code: String);
var
    i: Integer;
begin
    FLineCodeSpecified := FALSE;
    KillGeometrySpecified;
    if not assigned(FLineSpacingObj) then
        DoSimpleMsg('Must assign the LineSpacing before CN cables.(LINE.' + Name + ')', 18104);

    FPhaseChoice := ConcentricNeutral;
    FLineWireData := Allocmem(Sizeof(FLineWireData^[1]) * FLineSpacingObj.NWires);
    AuxParser.CmdString := Code;
    for i := 1 to FLineSpacingObj.NPhases do
    begin // fill extra neutrals later
        AuxParser.NextParam; // ignore any parameter name  not expecting any
        DSSPrime.CNDataClass.code := AuxParser.StrValue;
        if Assigned(ActiveConductorDataObj) then
            FLineWireData^[i] := ActiveConductorDataObj
        else
            DoSimpleMsg('CN cable ' + AuxParser.StrValue + ' was not defined first.(LINE.' + Name + ')', 18105);
    end;
end;

procedure TLineObj.FetchTSCableList(const Code: String);
var
    i: Integer;
begin
    FLineCodeSpecified := FALSE;
    KillGeometrySpecified;
    if not assigned(FLineSpacingObj) then
        DoSimpleMsg('Must assign the LineSpacing before TS cables.(LINE.' + Name + ')', 18106);

    FPhaseChoice := TapeShield;
    FLineWireData := Allocmem(Sizeof(FLineWireData^[1]) * FLineSpacingObj.NWires);
    AuxParser.CmdString := Code;
    for i := 1 to FLineSpacingObj.NPhases do
    begin // fill extra neutrals later
        AuxParser.NextParam; // ignore any parameter name  not expecting any
        DSSPrime.TSDataClass.code := AuxParser.StrValue;
        if Assigned(ActiveConductorDataObj) then
            FLineWireData^[i] := ActiveConductorDataObj
        else
            DoSimpleMsg('TS cable ' + AuxParser.StrValue + ' was not defined first. (LINE.' + Name + ')', 18107);
    end;
end;

procedure TLineObj.FetchGeometryCode(const Code: String);
var
    i: Integer;
begin
    if DSSPrime.LineGeometryClass.SetActive(Code) then
    begin
        FLineCodeSpecified := FALSE;  // Cancel this flag
        SpacingSpecified := FALSE;

        FLineGeometryObj := DSSPrime.LineGeometryClass.GetActiveObj;
        FZFrequency := -1.0;  // Init to signify not computed

        GeometryCode := LowerCase(Code);

        if FrhoSpecified then
            FlineGeometryObj.rhoearth := rho;

        NormAmps := FLineGeometryObj.NormAmps;
        EmergAmps := FLineGeometryObj.EmergAmps;
        UpdatePDProperties;

        NPhases := FLineGeometryObj.Nconds;
        Nconds := FNPhases;  // Force Reallocation of terminal info
        Yorder := Fnconds * Fnterms;
        YPrimInvalid := TRUE;       // Force Rebuild of Y matrix

        NumAmpRatings := FLineGeometryObj.NumAmpRatings;
        setlength(AmpRatings, NumAmpRatings);
        for i := 0 to High(AmpRatings) do
            AmpRatings[i] := FLineGeometryObj.AmpRatings[i];

    end
    else
        DoSimpleMsg('Line Geometry Object:' + Code + ' not found. (LINE.' + Name + ')', 18108);

end;

procedure TLineObj.FMakeZFromGeometry(f: Double); // make new Z, Zinv, Yc, etc
begin
    if f = FZFrequency then
        exit;  // Already Done for this frequency, no need to do anything

    if Assigned(FLineGeometryObj) then
    begin
         {This will make a New Z; Throw away present allocations}

        if assigned(Z) then
        begin
            Z.Free;
            Z := NIL;
        end;
        if assigned(Zinv) then
        begin
            Zinv.Free;
            Zinv := NIL;
        end;
        if assigned(Yc) then
        begin
            Yc.Free;
            Yc := NIL;
        end;

        DSSPrime.ActiveEarthModel := FEarthModel;

        Z := FLineGeometryObj.Zmatrix[f, len, LengthUnits];
        Yc := FLineGeometryObj.YCmatrix[f, len, LengthUnits];
          {Init Zinv}
        if Assigned(Z) then
        begin
            Zinv := TCMatrix.CreateMatrix(Z.order);  // Either no. phases or no. conductors
            Zinv.CopyFrom(Z);
            Zinv.Invert;  {Invert Z in place to get values to put in Yprim}
        end;

          // Z and YC are actual total impedance for the line;

        FZFrequency := f;
    end;
end;

procedure TLineObj.FMakeZFromSpacing(f: Double); // make new Z, Zinv, Yc, etc
var
    pGeo: TLineGeometryObj;
begin
    if f = FZFrequency then
        exit;  // Already Done for this frequency, no need to do anything

    if assigned(Z) then
    begin
        Z.Free;
        Z := NIL;
    end;
    if assigned(Zinv) then
    begin
        Zinv.Free;
        Zinv := NIL;
    end;
    if assigned(Yc) then
    begin
        Yc.Free;
        Yc := NIL;
    end;

  // make a temporary LineGeometry to calculate line constants
    pGeo := TLineGeometryObj.Create(DSSPrime.LineGeometryClass, Name);
    pGeo.LoadSpacingAndWires(FLineSpacingObj, FLineWireData); // this sets OH, CN, or TS

    if FrhoSpecified then
        pGeo.rhoearth := rho;
    NormAmps := pGeo.NormAmps;
    EmergAmps := pGeo.EmergAmps;
    UpdatePDProperties;

    DSSPrime.ActiveEarthModel := FEarthModel;

    Z := pGeo.Zmatrix[f, len, LengthUnits];
    Yc := pGeo.YCmatrix[f, len, LengthUnits];
    if Assigned(Z) then
    begin
        Zinv := TCMatrix.CreateMatrix(Z.order);  // Either no. phases or no. conductors
        Zinv.CopyFrom(Z);
        Zinv.Invert;  {Invert Z in place to get values to put in Yprim}
    end;
    pGeo.Free;

    FZFrequency := f;
end;

procedure TLineObj.KillGeometrySpecified;
begin
{Indicate No Line Geometry specification if this is called}
    if GeometrySpecified then
    begin
        FLineGeometryObj := NIL;
        FZFrequency := -1.0;
        GeometrySpecified := FALSE;
    end;
end;

procedure TLineObj.KillSpacingSpecified;
begin
    if SpacingSpecified then
    begin
        FLineSpacingObj := NIL;
        Reallocmem(FLineWireData, 0);
        FPhaseChoice := Unknown;
        FZFrequency := -1.0;
        SpacingSpecified := FALSE;
    end;
end;

procedure TLineObj.ClearYPrim;
begin
 // Line Object needs both Series and Shunt YPrims built
    if (Yprim = NIL) OR (Yprim.order <> Yorder) OR (Yprim_Shunt = NIL) OR (Yprim_Series = NIL) {YPrimInvalid} then
    begin // Reallocate YPrim if something has invalidated old allocation
        if YPrim_Series <> NIL then
            YPrim_Series.Free;
        if YPrim_Shunt <> NIL then
            YPrim_Shunt.Free;
        if YPrim <> NIL then
            YPrim.Free;

        YPrim_Series := TcMatrix.CreateMatrix(Yorder);
        YPrim_Shunt := TcMatrix.CreateMatrix(Yorder);
        YPrim := TcMatrix.CreateMatrix(Yorder);
    end
    else
    begin
        YPrim_Series.Clear;   // zero out YPrim Series
        YPrim_Shunt.Clear;    // zero out YPrim Shunt
        YPrim.Clear;          // zero out YPrim
    end;
end;

procedure TLineObj.ConvertZinvToPosSeqR;

// For GIC Analysis, use only real part of Z

var
    Z1, ZS, Zm: Complex;
    i, j: Integer;

begin

// re-invert Zinv
    Zinv.Invert;
// Now Zinv is back to Z with length included

    // average the diagonal and off-dialgonal elements
    Zs := Zinv.AvgDiagonal;
    Zm := Zinv.AvgOffDiagonal;
    Z1 := CSub(Zs, Zm);
    Z1.im := 0.0;  // ignore X part

    Zinv.Clear;
    for i := 1 to Zinv.order do
        Zinv.SetElement(i, i, Z1);   // Set Diagonals

    Zinv.Invert;  // back to zinv for inserting in Yprim

end;

procedure TLineObj.ResetLengthUnits;
{If specify the impedances always assume the length units match}
begin
    FUnitsConvert := 1.0;
    LengthUnits := UNITS_NONE;
end;

function TLineObj.NumConductorData: Integer;
begin
    Result := 0;
    if Assigned(FLineWireData) then
        Result := FLineSpacingObj.NWires;
    if Assigned(FLineGeometryObj) then
        Result := FLineGeometryObj.NWires;
end;

function TLineObj.FetchConductorData(i: Integer): TConductorDataObj;
begin
    Result := NIL;
    if Assigned(FLineWireData) then
    begin
        if i <= FLineSpacingObj.Nwires then
            Result := FLineWireData[i];
    end
    else
    if Assigned(FLineGeometryObj) then
    begin
        if i <= FLineGeometryObj.Nwires then
            Result := FLineGeometryObj.ConductorData[i];
    end;
end;

initialization

    CAP_EPSILON := cmplx(0.0, 4.2e-8);  // 5 kvar of capacitive reactance at 345 kV to avoid open line problem

end.
