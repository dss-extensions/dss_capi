unit VSource;

{
  ----------------------------------------------------------
  Copyright (c) 2008-2015, Electric Power Research Institute, Inc.
  All rights reserved.
  ----------------------------------------------------------
}

{
 2-17-00  Change Log
          Added Angle to VMag calculation

 6-18-00  Added ability to do specify impedance in ohms or short circuit current
 5-17-01 Moved Spectrum to Base class
 2-10-09 Converted to 2-terminal voltage source

}

interface

uses
    DSSClass,
    PCClass,
    PCElement,
    ucmatrix,
    ucomplex,
    Spectrum,
    Loadshape;

type
// = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = =
    TVsource = class(TPCClass)
    PRIVATE
        procedure VsourceSetBus1(const S: String);
    PROTECTED
        procedure DefineProperties;
        function MakeLike(const OtherSource: String): Integer; OVERRIDE;
    PUBLIC
        constructor Create;
        destructor Destroy; OVERRIDE;

        function Edit: Integer; OVERRIDE;
        function Init(Handle: Integer): Integer; OVERRIDE;
        function NewObject(const ObjName: String): Integer; OVERRIDE;
    end;

// = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = =
    TVsourceObj = class(TPCElement)
    PRIVATE
        MVAsc3: Double;
        MVAsc1: Double;
        Isc3: Double;
        Isc1: Double;
        ZSpecType: Integer;
        R1, X1: Double;  // Pos Seq Z
        R2, X2: Double;  // Neg Seq Z
        R0, X0: Double;  // Zero Seq Z
        X1R1: Double;
        X0R0: Double;
        BaseMVA: Double;
        puZ1, puZ0, puZ2: Complex;
        ZBase: Double;

        Bus2Defined: Boolean;
        Z1Specified: Boolean;
        puZ1Specified: Boolean;
        puZ0Specified: Boolean;
        puZ2Specified: Boolean;
        Z2Specified: Boolean;
        Z0Specified: Boolean;

        ScanType: Integer;
        SequenceType: Integer;

        ShapeFactor: Complex;
        ShapeIsActual: Boolean;
        procedure GetVterminalForSource;

        procedure CalcDailyMult(Hr: Double);
        procedure CalcDutyMult(Hr: Double);
        procedure CalcYearlyMult(Hr: Double);

    PUBLIC

        Z: TCmatrix;  // Base Frequency Series Z matrix
        Zinv: TCMatrix;
        VMag: Double;

        kVBase: Double;
        PerUnit: Double;
        Angle: Double;
        SrcFrequency: Double;

        DailyShape: String;         // Daily (24 HR) load shape
        DailyShapeObj: TLoadShapeObj;  // Daily load Shape FOR this load
        DutyShape: String;         // Duty cycle load shape FOR changes typically less than one hour
        DutyShapeObj: TLoadShapeObj;  // Shape for this load
        YearlyShape: String;  // ='fixed' means no variation  exempt from variation
        YearlyShapeObj: TLoadShapeObj;  // Shape for this load


        constructor Create(ParClass: TDSSClass; const SourceName: String);
        destructor Destroy; OVERRIDE;

        procedure RecalcElementData; OVERRIDE;
        procedure CalcYPrim; OVERRIDE;

        function InjCurrents: Integer; OVERRIDE;
        procedure GetInjCurrents(Curr: pComplexArray); OVERRIDE;
        procedure GetCurrents(Curr: pComplexArray); OVERRIDE;

        procedure MakePosSequence; OVERRIDE;  // Make a positive Sequence Model

        procedure InitPropertyValues(ArrayOffset: Integer); OVERRIDE;
        procedure DumpProperties(var F: TextFile; Complete: Boolean); OVERRIDE;
        function GetPropertyValue(Index: Integer): String; OVERRIDE;

    end;

var
    ActiveVsourceObj: TVsourceObj;
    VSourceClass: TVsource;

// = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = =
implementation


uses
    ParserDel,
    Circuit,
    DSSClassDefs,
    DSSGlobals,
    Dynamics,
    Utilities,
    Sysutils,
    Command;

const
    NumPropsThisClass = 29;

var
    CDOUBLEONE: Complex;

//- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
constructor TVsource.Create;  // Creates superstructure for all Line objects
begin
    inherited Create;
    Class_Name := 'Vsource';
    DSSClassType := SOURCE + NON_PCPD_ELEM;  // Don't want this in PC Element List

    ActiveElement := 0;

    DefineProperties;

    CommandList := TCommandList.Create(Slice(PropertyName^, NumProperties));
    CommandList.Abbrev := TRUE;

    VsourceClass := Self;
end;

//- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
destructor TVsource.Destroy;

begin
    // ElementList and  CommandList freed in inherited destroy
    inherited Destroy;

end;

//- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
procedure TVSource.DefineProperties;
begin

    Numproperties := NumPropsThisClass;
    CountProperties;   // Get inherited property count
    AllocatePropertyArrays;

     // Define Property names
    PropertyName[1] := 'bus1';
    PropertyName[2] := 'basekv';
    PropertyName[3] := 'pu';
    PropertyName[4] := 'angle';
    PropertyName[5] := 'frequency';
    PropertyName[6] := 'phases';
    PropertyName[7] := 'MVAsc3';
    PropertyName[8] := 'MVAsc1';
    PropertyName[9] := 'x1r1';
    PropertyName[10] := 'x0r0';
    PropertyName[11] := 'Isc3';
    PropertyName[12] := 'Isc1';
    PropertyName[13] := 'R1';
    PropertyName[14] := 'X1';
    PropertyName[15] := 'R0';
    PropertyName[16] := 'X0';
    PropertyName[17] := 'ScanType';
    PropertyName[18] := 'Sequence';
    PropertyName[19] := 'bus2';
    PropertyName[20] := 'Z1';
    PropertyName[21] := 'Z0';
    PropertyName[22] := 'Z2';
    PropertyName[23] := 'puZ1';
    PropertyName[24] := 'puZ0';
    PropertyName[25] := 'puZ2';
    PropertyName[26] := 'baseMVA';
    PropertyName[27] := 'Yearly';
    PropertyName[28] := 'Daily';
    PropertyName[29] := 'Duty';

     // define Property help values
    PropertyHelp[1] := 'Name of bus to which the main terminal (1) is connected.' + CRLF + 'bus1=busname' + CRLF + 'bus1=busname.1.2.3' + CRLF + CRLF +
        'The VSOURCE object is a two-terminal voltage source (thevenin equivalent). ' +
        'Bus2 defaults to Bus1 with all phases connected to ground (node 0) unless previously specified. This is a Yg connection. ' +
        'If you want something different, define the Bus2 property ezplicitly.';
    PropertyHelp[2] := 'Base Source kV, usually phase-phase (L-L) unless you are making a positive-sequence model or 1-phase model' +
        'in which case, it will be phase-neutral (L-N) kV.';
    PropertyHelp[3] := 'Per unit of the base voltage that the source is actually operating at.' + CRLF +
        '"pu=1.05"';
    PropertyHelp[4] := 'Phase angle in degrees of first phase: e.g.,Angle=10.3';
    PropertyHelp[5] := 'Source frequency.  Defaults to system default base frequency.';
    PropertyHelp[6] := 'Number of phases.  Defaults to 3.';
    PropertyHelp[7] := 'MVA Short circuit, 3-phase fault. Default = 2000. ' +
        'Z1 is determined by squaring the base kv and dividing by this value. ' +
        'For single-phase source, this value is not used.';
    PropertyHelp[8] := 'MVA Short Circuit, 1-phase fault. Default = 2100. ' +
        'The "single-phase impedance", Zs, is determined by squaring the base kV and dividing by this value. ' +
        'Then Z0 is determined by Z0 = 3Zs - 2Z1.  For 1-phase sources, Zs is used directly. ' +
        'Use X0R0 to define X/R ratio for 1-phase source.';
    PropertyHelp[9] := 'Positive-sequence  X/R ratio. Default = 4.';
    PropertyHelp[10] := 'Zero-sequence X/R ratio.Default = 3.';
    PropertyHelp[11] := 'Alternate method of defining the source impedance. ' + CRLF +
        '3-phase short circuit current, amps.  Default is 10000.';
    PropertyHelp[12] := 'Alternate method of defining the source impedance. ' + CRLF +
        'single-phase short circuit current, amps.  Default is 10500.';
    PropertyHelp[13] := 'Alternate method of defining the source impedance. ' + CRLF +
        'Positive-sequence resistance, ohms.  Default is 1.65.';
    PropertyHelp[14] := 'Alternate method of defining the source impedance. ' + CRLF +
        'Positive-sequence reactance, ohms.  Default is 6.6.';
    PropertyHelp[15] := 'Alternate method of defining the source impedance. ' + CRLF +
        'Zero-sequence resistance, ohms.  Default is 1.9.';
    PropertyHelp[16] := 'Alternate method of defining the source impedance. ' + CRLF +
        'Zero-sequence reactance, ohms.  Default is 5.7.';
    PropertyHelp[17] := '{pos*| zero | none} Maintain specified sequence for harmonic solution. Default is positive sequence. ' +
        'Otherwise, angle between phases rotates with harmonic.';
    PropertyHelp[18] := '{pos*| neg | zero} Set the phase angles for the specified symmetrical component sequence for non-harmonic solution modes. ' +
        'Default is positive sequence. ';
    PropertyHelp[19] := 'Name of bus to which 2nd terminal is connected.' + CRLF + 'bus2=busname' + CRLF + 'bus2=busname.1.2.3' +
        CRLF + CRLF +
        'Default is Bus1.0.0.0 (grounded wye connection)';
    PropertyHelp[20] := 'Positive-sequence equivalent source impedance, ohms, as a 2-element array representing a complex number. Example: ' + CRLF + CRLF +
        'Z1=[1, 2]  ! represents 1 + j2 ' + CRLF + CRLF +
        'If defined, Z1, Z2, and Z0 are used to define the impedance matrix of the VSOURCE. ' +
        'Z1 MUST BE DEFINED TO USE THIS OPTION FOR DEFINING THE MATRIX.' + CRLF + CRLF +
        'Side Effect: Sets Z2 and Z0 to same values unless they were previously defined.';
    PropertyHelp[21] := 'Zero-sequence equivalent source impedance, ohms, as a 2-element array representing a complex number. Example: ' + CRLF + CRLF +
        'Z0=[3, 4]  ! represents 3 + j4 ' + CRLF + CRLF +
        'Used to define the impedance matrix of the VSOURCE if Z1 is also specified. ' + CRLF + CRLF +
        'Note: Z0 defaults to Z1 if it is not specifically defined. ';
    PropertyHelp[22] := 'Negative-sequence equivalent source impedance, ohms, as a 2-element array representing a complex number. Example: ' + CRLF + CRLF +
        'Z2=[1, 2]  ! represents 1 + j2 ' + CRLF + CRLF +
        'Used to define the impedance matrix of the VSOURCE if Z1 is also specified. ' + CRLF + CRLF +
        'Note: Z2 defaults to Z1 if it is not specifically defined. If Z2 is not equal to Z1, the impedance matrix is asymmetrical.';
    PropertyHelp[23] := '2-element array: e.g., [1  2]. An alternate way to specify Z1. See Z1 property. Per-unit positive-sequence impedance on base of Vsource BasekV and BaseMVA.';
    PropertyHelp[24] := '2-element array: e.g., [1  2]. An alternate way to specify Z0. See Z0 property. Per-unit zero-sequence impedance on base of Vsource BasekV and BaseMVA.';
    PropertyHelp[25] := '2-element array: e.g., [1  2]. An alternate way to specify Z2. See Z2 property. Per-unit negative-sequence impedance on base of Vsource BasekV and BaseMVA.';
    PropertyHelp[26] := 'Default value is 100. Base used to convert values specifiied with puZ1, puZ0, and puZ2 properties to ohms on kV base specified by BasekV property.';
    PropertyHelp[27] := 'LOADSHAPE object to use for the per-unit voltage for YEARLY-mode simulations. Set the Mult property of the LOADSHAPE ' +
        'to the pu curve. Qmult is not used. If UseActual=Yes then the Mult curve should be actual L-N kV.' + CRLF + CRLF +
        'Must be previously defined as a LOADSHAPE object. ' + CRLF + CRLF +
        'Is set to the Daily load shape when Daily is defined.  The daily load shape is repeated in this case. ' +
        'Set to NONE to reset to no loadahape for Yearly mode. ' +
        'The default is no variation.';
    PropertyHelp[28] := 'LOADSHAPE object to use for the per-unit voltage for DAILY-mode simulations. Set the Mult property of the LOADSHAPE ' +
        'to the pu curve. Qmult is not used. If UseActual=Yes then the Mult curve should be actual L-N kV.' + CRLF + CRLF +
        'Must be previously defined as a LOADSHAPE object. ' + CRLF + CRLF +
        'Sets Yearly curve if it is not already defined.   ' +
        'Set to NONE to reset to no loadahape for Yearly mode. ' +
        'The default is no variation.';
    PropertyHelp[29] := 'LOADSHAPE object to use for the per-unit voltage for DUTYCYCLE-mode simulations. Set the Mult property of the LOADSHAPE ' +
        'to the pu curve. Qmult is not used. If UseActual=Yes then the Mult curve should be actual L-N kV.' + CRLF + CRLF +
        'Must be previously defined as a LOADSHAPE object. ' + CRLF + CRLF +
        'Defaults to Daily load shape when Daily is defined.   ' +
        'Set to NONE to reset to no loadahape for Yearly mode. ' +
        'The default is no variation.';

    ActiveProperty := NumPropsThisClass;
    inherited DefineProperties;  // Add defs of inherited properties to bottom of list

     // Override help string
    PropertyHelp[NumPropsThisClass + 1] := 'Name of harmonic spectrum for this source.  Default is "defaultvsource", which is defined when the DSS starts.';

end;


//- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
function TVsource.NewObject(const ObjName: String): Integer;
begin
    // Make a new voltage source and add it to Vsource class list
    with ActiveCircuit do
    begin
        ActiveCktElement := TVsourceObj.Create(Self, ObjName);
        Result := AddObjectToList(ActiveDSSObject);
    end;
end;


//- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
procedure TVsource.VsourceSetBus1(const S: String);
var
    s2: String;
    i, dotpos: Integer;

   // Special handling for Bus 1
   // Set Bus2 = Bus1.0.0.0

begin
    with ActiveVSourceObj do
    begin
        SetBus(1, S);

        if not Bus2Defined then // Default Bus2 to zero node of Bus1. (Grounded-Y connection)
        begin
         // Strip node designations from S
            dotpos := Pos('.', S);
            if dotpos > 0 then
                S2 := Copy(S, 1, dotpos - 1)
            else
                S2 := Copy(S, 1, Length(S));  // copy up to Dot
            for i := 1 to Fnphases do
                S2 := S2 + '.0';   // append series of ".0"'s

            SetBus(2, S2);    // default setting for Bus2
        end;
    end;

end;

//- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
function TVsource.Edit: Integer;
var
    ParamPointer: Integer;
    ParamName,
    Param: String;
    ZTemp: Complex;

begin
  // continue parsing with contents of Parser
    ActiveVSourceObj := ElementList.Active;
    ActiveCircuit.ActiveCktElement := ActiveVSourceObj;

    Result := 0;

    with ActiveVSourceObj do
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
                    DoSimpleMsg('Unknown parameter "' + ParamName + '" for Object "VSource.' + Name + '"', 320);
                1:
                    VSourceSetBus1(param);   // special handling of Bus 1
                2:
                    kVBase := Parser.DblValue; // basekv
                3:
                    PerUnit := Parser.DblValue; // pu
                4:
                    Angle := Parser.DblValue; // Ang
                5:
                    SrcFrequency := Parser.DblValue; // freq
                6:
                begin
                    Nphases := Parser.Intvalue; // num phases
                    NConds := Fnphases;  // Force Reallocation of terminal info
                end;
                7:
                    MVAsc3 := Parser.DblValue; // MVAsc3
                8:
                    MVAsc1 := Parser.DblValue; // MVAsc1
                9:
                    X1R1 := Parser.DblValue; // X1/R1
                10:
                    X0R0 := Parser.DblValue; // X0/R0
                11:
                    Isc3 := Parser.DblValue;
                12:
                    Isc1 := Parser.DblValue;
                13:
                    R1 := Parser.DblValue;
                14:
                    X1 := Parser.DblValue;
                15:
                    R0 := Parser.DblValue;
                16:
                    X0 := Parser.DblValue;
                17:
                    case Uppercase(Param)[1] of
                        'P':
                            ScanType := 1;
                        'Z':
                            ScanType := 0;
                        'N':
                            ScanType := -1;
                    else
                        DoSimpleMsg('Unknown Scan Type for "' + Class_Name + '.' + Name + '": ' + Param, 321);
                    end;
                18:
                    case Uppercase(Param)[1] of
                        'P':
                            Sequencetype := 1;
                        'Z':
                            Sequencetype := 0;
                        'N':
                            Sequencetype := -1;
                    else
                        DoSimpleMsg('Unknown Sequence Type for "' + Class_Name + '.' + Name + '": ' + Param, 321);
                    end;
                19:
                    SetBus(2, param);
                20:
                    Ztemp := InterpretComplex(Param);
                21:
                    Ztemp := InterpretComplex(Param);
                22:
                    Ztemp := InterpretComplex(Param);
                23:
                    puZ1 := InterpretComplex(Param);
                24:
                    puZ0 := InterpretComplex(Param);
                25:
                    puZ2 := InterpretComplex(Param);
                26:
                    BaseMVA := Parser.DblValue;

                27:
                    YearlyShape := Param;
                28:
                    DailyShape := Param;
                29:
                    DutyShape := Param;
            else
                ClassEdit(ActiveVsourceObj, ParamPointer - NumPropsThisClass)
            end;

            case ParamPointer of
                20:
                begin
                    R1 := ZTemp.re;
                    X1 := Ztemp.im;
                    Z1Specified := TRUE;
                     // default values for Z2, Z0
                    if not Z2Specified then
                    begin
                        R2 := R1;
                        X2 := X1;
                    end;
                    if not Z0Specified then
                    begin
                        R0 := R1;
                        X0 := X1;
                    end;
                end;
                21:
                begin
                    R0 := ZTemp.re;
                    X0 := Ztemp.im;
                    Z0Specified := TRUE;
                end;
                22:
                begin
                    R2 := ZTemp.re;
                    X2 := Ztemp.im;
                    Z2Specified := TRUE;
                end;
                23:
                begin
                    puZ1Specified := TRUE;
                     // default values for Z2, Z0
                    if not puZ2Specified then
                    begin
                        puZ2 := puZ1;
                    end;
                    if not puZ0Specified then
                    begin
                        puZ0 := puZ1;
                    end;
                end;
                24:
                    puZ0Specified := TRUE;
                25:
                    puZ2Specified := TRUE;
    {Set shape objects;  returns nil if not valid}
    {Sets the kW and kvar properties to match the peak kW demand from the Loadshape}
                27:
                    YearlyShapeObj := LoadShapeClass.Find(YearlyShape);
                28:
                begin
                    DailyShapeObj := LoadShapeClass.Find(DailyShape);
                  {If Yearly load shape is not yet defined, make it the same as Daily}
                    if YearlyShapeObj = NIL then
                        YearlyShapeObj := DailyShapeObj;
                end;
                29:
                    DutyShapeObj := LoadShapeClass.Find(DutyShape);
            end;

            case ParamPointer of
                13:
                    R2 := R1;
                14:
                    X2 := X1;
            end;
         // Set the Z spec type switch depending on which was specified.
            case ParamPointer of
                7, 8:
                    ZSpecType := 1;  // MVAsc
                11, 12:
                    ZSpecType := 2;  // Isc

                13 .. 16:
                    ZSpecType := 3; // Specified in Ohms
                19:
                    Bus2Defined := TRUE;
                20..25:
                    Zspectype := 3;
            end;

            case ParamPointer of
                2:
                    ZBase := SQR(kvBase) / BaseMVA;
                23:
                begin
                    Z1Specified := TRUE;
                    puZ1Specified := TRUE;
                end;
                24:
                    puZ0Specified := TRUE;
                25:
                    puZ2Specified := TRUE;
                26:
                    ZBase := SQR(kvBase) / BaseMVA;
            end;

            ParamName := Parser.NextParam;
            Param := Parser.StrValue;
        end;

        RecalcElementData;
        YPrimInvalid := TRUE;
    end;

end;

//----------------------------------------------------------------------------
function TVsource.MakeLike(const OtherSource: String): Integer;
var
    OtherVSource: TVSourceObj;
    i: Integer;

begin
    Result := 0;
   {See if we can find this line name in the present collection}
    OtherVSource := Find(OtherSource);
    if OtherVSource <> NIL then
        with ActiveVsourceObj do
        begin

            if Fnphases <> OtherVSource.Fnphases then
            begin
                Nphases := OtherVSource.Fnphases;
                NConds := Fnphases;  // Forces reallocation of terminal stuff

                Yorder := Fnconds * Fnterms;
                YPrimInvalid := TRUE;

                if Z <> NIL then
                    Z.Free;
                if Zinv <> NIL then
                    Zinv.Free;

                Z := TCmatrix.CreateMatrix(Fnphases);
                Zinv := TCMatrix.CreateMatrix(Fnphases);
            end;

            Z.CopyFrom(OtherVSource.Z);
       // Zinv.CopyFrom(OtherLine.Zinv);
            VMag := OtherVsource.Vmag;
            kVBase := OtherVsource.kVBase;
            BaseMVA := OtherVsource.BaseMVA;
            PerUnit := OtherVsource.PerUnit;
            Angle := OtherVsource.Angle;
            MVAsc3 := OtherVsource.MVAsc3;
            MVAsc1 := OtherVsource.MVAsc1;

            Scantype := OtherVsource.Scantype;
            Sequencetype := OtherVsource.Sequencetype;
            SrcFrequency := OtherVsource.SrcFrequency;

            ZSpecType := OtherVsource.ZSpecType;
            R1 := OtherVsource.R1;
            X1 := OtherVsource.X1;
            R2 := OtherVsource.R2;
            X2 := OtherVsource.X2;
            R0 := OtherVsource.R0;
            X0 := OtherVsource.X0;
            X1R1 := OtherVsource.X1R1;
            X0R0 := OtherVsource.X0R0;
            BaseMVA := OtherVsource.BaseMVA;
            puZ1 := OtherVsource.puZ1;
            puZ0 := OtherVsource.puZ0;
            puZ2 := OtherVsource.puZ2;
            ZBase := OtherVsource.ZBase;
            Bus2Defined := OtherVsource.Bus2Defined;
            Z1Specified := OtherVsource.Z1Specified;
            Z2Specified := OtherVsource.Z2Specified;
            Z0Specified := OtherVsource.Z0Specified;
            puZ0Specified := OtherVsource.puZ0Specified;
            puZ1Specified := OtherVsource.puZ1Specified;
            puZ2Specified := OtherVsource.puZ2Specified;

        {Loadshape stuff}
            ShapeIsActual := OtherVsource.ShapeIsActual;
            DailyShape := OtherVsource.DailyShape;
            DailyShapeObj := OtherVsource.DailyShapeObj;
            DutyShape := OtherVsource.DutyShape;
            DutyShapeObj := OtherVsource.DutyShapeObj;
            YearlyShape := OtherVsource.YearlyShape;
            YearlyShapeObj := OtherVsource.YearlyShapeObj;

            ClassMakeLike(OtherVSource);

            for i := 1 to ParentClass.NumProperties do
                FPropertyValue[i] := OtherVsource.FPropertyValue[i];
            Result := 1;
        end
    else
        DoSimpleMsg('Error in Vsource MakeLike: "' + OtherSource + '" Not Found.', 322);

end;

//----------------------------------------------------------------------------
function TVsource.Init(Handle: Integer): Integer;

begin
    DoSimpleMsg('Need to implement TVsource.Init', -1);
    Result := 0;
end;

//=============================================================================
constructor TVsourceObj.Create(ParClass: TDSSClass; const SourceName: String);
begin
    inherited create(ParClass);
    Name := LowerCase(SourceName);
    DSSObjType := ParClass.DSSClassType; //SOURCE + NON_PCPD_ELEM;  // Don't want this in PC Element List

    Nphases := 3;
    Fnconds := 3;
    Nterms := 2;   // Now a 2-terminal device
    Z := NIL;
    Zinv := NIL;
     {Basefrequency := 60.0;} // set in base class
    MVAsc3 := 2000.0;
    MVAsc1 := 2100.0;
    ZSpecType := 1; // default to MVAsc

    R1 := 1.65;
    X1 := 6.6;
    R2 := R1;
    X2 := X1;
    R0 := 1.9;
    X0 := 5.7;
    Isc3 := 10000.0;
    Isc1 := 10540.0;
    X1R1 := 4.0;
    X0R0 := 3.0;
    PerUnit := 1.0;  // per unit voltage, not impedance
    kVBase := 115.0;
    BaseMVA := 100.0;
    ZBase := SQR(kvBase) / BaseMVA;

    SrcFrequency := BaseFrequency;
    Angle := 0.0;
    Scantype := 1;
    SequenceType := 1;

    Bus2Defined := FALSE;
    Z1Specified := FALSE;
    Z2Specified := FALSE;
    Z0Specified := FALSE;
    puZ0Specified := FALSE;
    puZ2Specified := FALSE;
    puZ1Specified := FALSE;

    Spectrum := 'defaultvsource';

    ShapeIsActual := FALSE;
    YearlyShape := '';
    YearlyShapeObj := NIL;  // IF YearlyShapeobj = nil THEN the Vsource alway stays nominal
    DailyShape := '';
    DailyShapeObj := NIL;  // IF DaillyShapeobj = nil THEN the Vsource alway stays nominal
    DutyShape := '';
    DutyShapeObj := NIL;  // IF DutyShapeobj = nil THEN the Vsource alway stays nominal

    InitPropertyValues(0);


    Yorder := Fnterms * Fnconds;
    RecalcElementData;

end;


//=============================================================================
destructor TVsourceObj.Destroy;
begin
    Z.Free;
    Zinv.Free;

    inherited Destroy;
end;

//=============================================================================
procedure TVsourceObj.RecalcElementData;
var
    Zs, Zm, Z1, Z2, Z0: Complex;
    Value, Value1, Value2: Complex;
    Calpha1, Calpha2: Complex;
    i, j: Integer;

    Factor: Double;

    Rs, Xs, Rm, Xm: Double;

begin
    if Z <> NIL then
        Z.Free;
    if Zinv <> NIL then
        Zinv.Free;

    // For a Source, nphases = ncond, for now
    Z := TCmatrix.CreateMatrix(Fnphases);
    Zinv := TCMatrix.CreateMatrix(Fnphases);

    if FNPhases = 1 then
        Factor := 1.0
    else
        Factor := SQRT3;

    Rs := 0.0;
    Rm := 0.0;
    Xs := 0.1;
    Xm := 0.0;

    {Calculate the short circuit impedance and make all other spec types agree}
    case ZSpecType of
        1:
        begin  // MVAsc
            X1 := Sqr(KvBase) / MVAsc3 / Sqrt(1.0 + 1.0 / Sqr(X1R1));
          //  Xs   := Sqr(KvBase) / MVAsc1/Sqrt(1.0 + 1.0/Sqr(X0R0)); // Approx
            R1 := X1 / X1R1;
            R2 := R1;  // default Z2 = Z1
            X2 := X1;
            Isc3 := MVAsc3 * 1000.0 / (SQRT3 * kVBase);
            Isc1 := MVAsc1 * 1000.0 / (Factor * kVBase);

        //  Compute R0, X0
            R0 := QuadSolver((1.0 + SQR(X0R0)), (4.0 * (R1 + X1 * X0R0)), (4.0 * (R1 * R1 + X1 * X1) - SQR(3.0 * kVBase * 1000.0 / Factor / Isc1)));
            X0 := R0 * X0R0;

            // for Z matrix
            Xs := (2.0 * X1 + X0) / 3.0;
            Rs := (2.0 * R1 + R0) / 3.0;

            Rm := (R0 - R1) / 3.0;
            Xm := (X0 - X1) / 3.0;
        end;

        2:
        begin  // Isc

            MVAsc3 := SQRT3 * kVBase * Isc3 / 1000.0;
            MVAsc1 := Factor * kVBase * Isc1 / 1000.0;
            X1 := Sqr(KvBase) / MVAsc3 / Sqrt(1.0 + 1.0 / Sqr(X1R1));
            R1 := X1 / X1R1;
            R2 := R1;  // default Z2 = Z1
            X2 := X1;
        //  Compute R0, X0
            R0 := QuadSolver((1.0 + SQR(X0R0)), (4.0 * (R1 + X1 * X0R0)), (4.0 * (R1 * R1 + X1 * X1) - SQR(3.0 * kVBase * 1000.0 / Factor / Isc1)));
            X0 := R0 * X0R0;

            // for Z matrix
            Xs := (2.0 * X1 + X0) / 3.0;
            Rs := (2.0 * R1 + R0) / 3.0;

            Rm := (R0 - R1) / 3.0;
            Xm := (X0 - X1) / 3.0;

        end;

        3:
        begin  // Z1, Z2, Z0    Specified

            // Compute Z1, Z2, Z0 in ohms if Z1 is specified in pu
            if puZ1Specified then
            begin
                R1 := puZ1.re * Zbase;
                X1 := puZ1.im * Zbase;
                R2 := puZ2.re * Zbase;
                X2 := puZ2.im * Zbase;
                R0 := puZ0.re * Zbase;
                X0 := puZ0.im * Zbase;
            end;

            // Compute equivalent Isc3, Isc1, MVAsc3, MVAsc1 values;
            Isc3 := kVBase * 1000.0 / SQRT3 / Cabs(cmplx(R1, X1));

            // compute nominal values for case where Z1=Z2
            // we won't necessarily use it to build Yprim matrix if Z2 <> Z1

            if Fnphases = 1 then
            begin  // Force Z0 and Z2 to be Z1 so Zs is same as Z1
                R0 := R1;
                X0 := X1;
                R2 := R1;
                X2 := X1;
            end;
            Rs := (2.0 * R1 + R0) / 3.0;
            Xs := (2.0 * X1 + X0) / 3.0;

            Isc1 := kVBase * 1000.0 / Factor / Cabs(cmplx(Rs, Xs));
            MVAsc3 := SQRT3 * kVBase * Isc3 / 1000.0;
            MVAsc1 := Factor * kVBase * Isc1 / 1000.0;
            Xm := Xs - X1;

            Rs := (2.0 * R1 + R0) / 3.0;
            Rm := (R0 - R1) / 3.0;

        end;

    end;

    {Update property Value array}
     { Don't change a specified value; only computed ones}


    if (R1 = R2) and (X1 = X2) then
    begin
    // Symmetric Matrix Case
        Zs := cmplx(Rs, Xs);
        Zm := cmplx(Rm, Xm);

        for i := 1 to Fnphases do
        begin
            Z.SetElement(i, i, Zs);
            for j := 1 to i - 1 do
            begin
                Z.SetElemsym(i, j, Zm);
            end;
        end;
    end
    else
    begin
    // Asymmetric Matrix case where Z2 <> Z1
        Z1 := Cmplx(R1, X1);
        Z2 := Cmplx(R2, X2);
        Z0 := Cmplx(R0, X0);

         // Diagonals  (all the same)
        Value := Cadd(Z2, Cadd(Z1, Z0));   // Z1 + Z2 + Z0
        Value := CdivReal(Value, 3.0);
        for i := 1 to Fnphases do
            Z.SetElement(i, i, Value);

         // Off-Diagonals
        if FnPhases = 3 then     // otherwise undefined
        begin

             // There are two possible off-diagonal elements  if Z1 <> Z2
             // Calpha is defined as 1 /_ -120 instead of 1 /_ 120

            Calpha1 := Conjg(Calpha);           // Change Calpha to agree with textbooks
            Calpha2 := Cmul(Calpha1, Calpha1);  // Alpha squared  = 1 /_ 240 = 1/_-120
             //(Z0 + aZ1 + a2 Z2)/3
            Value2 := Cadd(Cmul(Calpha2, Z2), Cadd(Cmul(Calpha1, Z1), Z0));
             //(Z0 + a2 Z1 + aZ2)/3
            Value1 := Cadd(Cmul(Calpha2, Z1), Cadd(Cmul(Calpha1, Z2), Z0));
             // Apply 1/3 ...
            Value1 := CdivReal(Value1, 3.0);
            Value2 := CdivReal(Value2, 3.0);
            with Z do
            begin
               //Lower Triangle
                SetElement(2, 1, Value1);
                SetElement(3, 1, Value2);
                SetElement(3, 2, Value1);
               //Upper Triangle
                SetElement(1, 2, Value2);
                SetElement(1, 3, Value1);
                SetElement(2, 3, Value2);
            end;

        end;

    end;

  // if not specified, compute a value for for puZ1 for display in formedit
    if not (puZ1Specified or puZ0Specified or puZ2Specified) and (Zbase > 0.0) then
    begin
        puZ1.re := R1 / Zbase;
        puZ1.im := X1 / Zbase;
        puZ2.re := R2 / Zbase;
        puZ2.im := X2 / Zbase;
        puZ0.re := R0 / Zbase;
        puZ0.im := X0 / Zbase;
    end;

    case Fnphases of
        1:
            Vmag := kVBase * PerUnit * 1000.0;
    else
        Vmag := kVBase * PerUnit * 1000.0 / 2.0 / Sin((180.0 / Fnphases) * PI / 180.0);
    end;

    SpectrumObj := SpectrumClass.Find(Spectrum);
    if SpectrumObj = NIL then
    begin
        DoSimpleMsg('Spectrum Object "' + Spectrum + '" for Device Vsource.' + Name + ' Not Found.', 324);
    end;

    {Now check for errors.  If any of these came out nil and the string was not nil, give warning}
    if CompareText(YearlyShape, 'none') = 0 then
        YearlyShape := '';
    if CompareText(DailyShape, 'none') = 0 then
        DailyShape := '';
    if CompareText(DutyShape, 'none') = 0 then
        DutyShape := '';
    if YearlyShapeObj = NIL then
        if Length(YearlyShape) > 0 then
            DoSimpleMsg('WARNING! Vsource Yearly load shape: "' + YearlyShape + '" Not Found.', 34583);
    if DailyShapeObj = NIL then
        if Length(DailyShape) > 0 then
            DoSimpleMsg('WARNING! Vsource Daily load shape: "' + DailyShape + '" Not Found.', 34584);
    if DutyShapeObj = NIL then
        if Length(DutyShape) > 0 then
            DoSimpleMsg('WARNING! Vsource Duty load shape: "' + DutyShape + '" Not Found.', 34585);

    Reallocmem(InjCurrent, SizeOf(InjCurrent^[1]) * Yorder);

end;

//=============================================================================
procedure TVsourceObj.CalcYPrim;

var
    Value: Complex;
    i, j: Integer;
    FreqMultiplier: Double;

begin

 // Build only YPrim Series
    if YPrimInvalid then
    begin
        if YPrim_Series <> NIL then
            YPrim_Series.Free;
        YPrim_Series := TcMatrix.CreateMatrix(Yorder);
        if YPrim <> NIL then
            YPrim.Free;
        YPrim := TcMatrix.CreateMatrix(Yorder);
    end
    else
    begin
        YPrim_Series.Clear;
        YPrim.Clear;
    end;

    FYprimFreq := ActiveCircuit.Solution.Frequency;
    FreqMultiplier := FYprimFreq / BaseFrequency;

     { Put in Series RL Adjusted for frequency }
    for i := 1 to Fnphases do
    begin
        for j := 1 to Fnphases do
        begin
            Value := Z.GetElement(i, j);
            Value.im := Value.im * FreqMultiplier;  {Modify from base freq}
            Zinv.SetElement(i, j, value);
        end;
    end;

    Zinv.Invert;  {Invert in place}

    if Zinv.InvertError > 0 then
    begin       {If error, put in Large series conductance}
        DoErrorMsg('TVsourceObj.CalcYPrim', 'Matrix Inversion Error for Vsource "' + Name + '"',
            'Invalid impedance specified. Replaced with small resistance.', 325);
        Zinv.Clear;
        for i := 1 to Fnphases do
            Zinv.SetElement(i, i, Cmplx(1.0 / EPSILON, 0.0));
    end;

   // YPrim_Series.CopyFrom(Zinv);

    for i := 1 to FNPhases do
    begin
        for j := 1 to FNPhases do
        begin
            Value := Zinv.GetElement(i, j);
            YPrim_series.SetElement(i, j, Value);
            YPrim_series.SetElement(i + FNPhases, j + FNPhases, Value);
          //YPrim_series.SetElemsym(i + FNPhases, j, CNegate(Value))
            YPrim_series.SetElement(i, j + Fnphases, Cnegate(Value));
            YPrim_series.SetElement(i + Fnphases, j, Cnegate(Value));
        end;
    end;

    YPrim.CopyFrom(YPrim_Series);

     {Now Account for Open Conductors}
     {For any conductor that is open, zero out row and column}
    inherited CalcYPrim;

    YPrimInvalid := FALSE;

end;

//=============================================================================
procedure TVsourceObj.GetVterminalForSource;

var
    i: Integer;
    Vharm: Complex;
    SrcHarmonic: Double;

begin

    try

  {
   This formulation will theoretically handle voltage sources of
   any number of phases assuming they are
   equally displaced in time.
  }


        with ActiveCircuit.Solution do
        begin

            ShapeIsActual := FALSE;

          {Modify magnitude based on a LOADSHAPE if assigned}
            case Mode of
               {Uses same logic as LOAD}
                DAILYMODE:
                begin
                    CalcDailyMult(DynaVars.dblHour);
                end;
                YEARLYMODE:
                begin
                    CalcYearlyMult(DynaVars.dblHour);
                end;
                DUTYCYCLE:
                begin
                    CalcDutyMult(DynaVars.dblHour);
                end;
            end;

            if (Mode = DAILYMODE) or     {If a loadshape mode simulation}
                (Mode = YEARLYMODE) or
                (Mode = DUTYCYCLE) then
            begin  {Loadshape cases}
                if ShapeIsActual then
                    Vmag := 1000.0 * ShapeFactor.re  // assumes actual L-N voltage or voltage across source
                else
                    case Fnphases of
                        1:
                            Vmag := kVBase * ShapeFactor.re * 1000.0;
                    else
                        Vmag := kVBase * ShapeFactor.re * 1000.0 / 2.0 / Sin((180.0 / Fnphases) * PI / 180.0);
                    end;
            end
            else  // Normal Case
                case Fnphases of
                    1:
                        Vmag := kVBase * PerUnit * 1000.0;
                else
                    Vmag := kVBase * PerUnit * 1000.0 / 2.0 / Sin((180.0 / Fnphases) * PI / 180.0);
                end;

            if IsHarmonicModel then
            begin

                SrcHarmonic := Frequency / SrcFrequency;
                Vharm := CMulReal(SpectrumObj.GetMult(SrcHarmonic), Vmag);  // Base voltage for this harmonic
                RotatePhasorDeg(Vharm, SrcHarmonic, Angle);  // Rotate for phase 1 shift
                for i := 1 to Fnphases do
                begin
                    Vterminal^[i] := Vharm;
                    VTerminal^[i + Fnphases] := CZERO;
                    if (i < Fnphases) then
                    begin
                        case ScanType of
                            1:
                                RotatePhasorDeg(Vharm, 1.0, -360.0 / Fnphases); // maintain pos seq
                            0: ;  // Do nothing for Zero Sequence; All the same
                        else
                            RotatePhasorDeg(Vharm, SrcHarmonic, -360.0 / Fnphases); // normal rotation
                        end;
                    end;
                end;

            end
            else
            begin  // non-harmonic modes

                if abs(Frequency - SrcFrequency) > EPSILON2 then
                    Vmag := 0.0;  // Solution Frequency and Source Frequency don't match!
         {NOTE: RE-uses VTerminal space}
                for i := 1 to Fnphases do
                begin
                    case Sequencetype of
                        -1:
                            Vterminal^[i] := pdegtocomplex(Vmag, (360.0 + Angle + (i - 1) * 360.0 / Fnphases));  // neg seq
                        0:
                            Vterminal^[i] := pdegtocomplex(Vmag, (360.0 + Angle));   // all the same for zero sequence
                    else
                        Vterminal^[i] := pdegtocomplex(Vmag, (360.0 + Angle - (i - 1) * 360.0 / Fnphases));
                    end;
                    VTerminal^[i + Fnphases] := CZERO;    // See comments in GetInjCurrents
                end;

            end;


        end;

    except
        DoSimpleMsg('Error computing Voltages for Vsource.' + Name + '. Check specification. Aborting.', 326);
        if In_Redirect then
            Redirect_Abort := TRUE;
    end;

end;

//===========================================================================

function TVsourceObj.InjCurrents: Integer;

begin

    GetInjCurrents(InjCurrent);

{This is source injection}

    Result := inherited InjCurrents; // Add into system array

end;

//===========================================================================
procedure TVsourceObj.GetCurrents(Curr: pComplexArray);

var
    i: Integer;

begin
    try
        with    ActiveCircuit.Solution do
        begin
     //FOR i := 1 TO (Nterms * NConds) DO Vtemp^[i] := V^[NodeRef^[i]];
     // This is safer    12/7/99
            for     i := 1 to Yorder do
                Vterminal^[i] := NodeV^[NodeRef^[i]];

            YPrim.MVMult(Curr, Vterminal);  // Current from Elements in System Y

            GetInjCurrents(ComplexBuffer);  // Get present value of inj currents
      // Add Together  with yprim currents
            for i := 1 to Yorder do
                Curr^[i] := Csub(Curr^[i], ComplexBuffer^[i]);

        end;  {With}
    except
        On E: Exception do
            DoErrorMsg(('GetCurrents for Element: ' + Name + '.'), E.Message,
                'Inadequate storage allotted for circuit element.', 327);
    end;

end;


//=============================================================================
procedure TVsourceObj.GetInjCurrents(Curr: pComplexArray);

begin

   { source injection currents given by this formula:
     _     _           _         _
     |Iinj1|           |Vsource  |
     |     | = [Yprim] |         |
     |Iinj2|           | 0       |
     _     _           _         _
   }

    GetVterminalForSource;  // gets voltage vector above
    YPrim.MVMult(Curr, Vterminal);

    ITerminalUpdated := FALSE;

end;

//=============================================================================
procedure TVsourceObj.DumpProperties(var F: TextFile; Complete: Boolean);

var
    i, j: Integer;
    c: Complex;

begin
    inherited DumpProperties(F, Complete);

    with ParentClass do
        for i := 1 to NumProperties do
        begin
            Writeln(F, '~ ', PropertyName^[i], '=', PropertyValue[i]);
        end;

    if Complete then
    begin
        Writeln(F);
        Writeln(F, 'BaseFrequency=', BaseFrequency: 0: 1);
        Writeln(F, 'VMag=', VMag: 0: 2);
        Writeln(F, 'Z Matrix=');
        for i := 1 to Fnphases do
        begin
            for j := 1 to i do
            begin
                c := Z.GetElement(i, j);
                Write(F, Format('%.8g +j %.8g ', [C.re, C.im]));
            end;
            Writeln(F);
        end;
    end;

end;


//=============================================================================
procedure TVsourceObj.InitPropertyValues(ArrayOffset: Integer);
begin

     {PropertyValue Allocated in DSSObject.Create}
    PropertyValue[1] := GetBus(1);
    PropertyValue[2] := '115';
    PropertyValue[3] := '1';
    PropertyValue[4] := '0';
    PropertyValue[5] := Format('%d', [Round(ActiveCircuit.Fundamental)]);
    PropertyValue[6] := '3';
    PropertyValue[7] := '2000';
    PropertyValue[8] := '2100';
    PropertyValue[9] := '4';
    PropertyValue[10] := '3';
    PropertyValue[11] := '10000';
    PropertyValue[12] := '10500';
    PropertyValue[13] := '1.65';
    PropertyValue[14] := '6.6';
    PropertyValue[15] := '1.9';
    PropertyValue[16] := '5.7';
    PropertyValue[17] := 'Pos';
    PropertyValue[18] := 'Pos';
    PropertyValue[19] := GetBus(2);
    PropertyValue[20] := '[ 0 0 ]';
    PropertyValue[21] := '[ 0 0 ]';
    PropertyValue[22] := '[ 0 0 ]';
    PropertyValue[23] := '[ 0 0 ]';
    PropertyValue[24] := '[ 0 0 ]';
    PropertyValue[25] := '[ 0 0 ]';
    PropertyValue[26] := '100';
    PropertyValue[27] := '';
    PropertyValue[28] := '';
    PropertyValue[29] := '';


    inherited  InitPropertyValues(NumPropsThisClass);

end;

//=============================================================================
function TVsourceObj.GetPropertyValue(Index: Integer): String;
begin
    case Index of
        1:
            Result := GetBus(1);
        7:
            Result := Format('%-.5g', [MVAsc3]);
        8:
            Result := Format('%-.5g', [MVAsc1]);
        11:
            Result := Format('%-.5g', [Isc3]);
        12:
            Result := Format('%-.5g', [Isc1]);
        13:
            Result := Format('%-.5g', [R1]);
        14:
            Result := Format('%-.5g', [X1]);
        15:
            Result := Format('%-.5g', [R0]);
        16:
            Result := Format('%-.5g', [X0]);
        19:
            Result := GetBus(2);
        20:
            Result := Format('[%-.8g, %-.8g]', [R1, X1]);
        21:
            Result := Format('[%-.8g, %-.8g]', [R0, X0]);
        22:
            Result := Format('[%-.8g, %-.8g]', [R2, X2]);
        23:
            Result := Format('[%-.8g, %-.8g]', [puZ1.re, puZ1.im]);
        24:
            Result := Format('[%-.8g, %-.8g]', [puZ0.re, puZ0.im]);
        25:
            Result := Format('[%-.8g, %-.8g]', [puZ2.re, puZ2.im]);
        26:
            Result := Format('%-.5g', [BaseMVA]);
    else
        Result := inherited GetPropertyValue(Index);
    end;
end;


//----------------------------------------------------------------------------
procedure TVSourceObj.CalcDailyMult(Hr: Double);

begin
    if DailyShapeObj <> NIL then
    begin
        ShapeFactor := DailyShapeObj.GetMult(Hr);
        ShapeIsActual := DailyShapeObj.UseActual;
    end
    else
        ShapeFactor := cmplx(PerUnit, 0.0); // CDOUBLEONE;  // Default to no daily variation
end;


//----------------------------------------------------------------------------
procedure TVSourceObj.CalcDutyMult(Hr: Double);

begin
    if DutyShapeObj <> NIL then
    begin
        ShapeFactor := DutyShapeObj.GetMult(Hr);
        ShapeIsActual := DutyShapeObj.UseActual;
    end
    else
        CalcDailyMult(Hr);  // Default to Daily Mult IF no duty curve specified
end;

//----------------------------------------------------------------------------
procedure TVSourceObj.CalcYearlyMult(Hr: Double);

begin
{Yearly curve is assumed to be hourly only}
    if YearlyShapeObj <> NIL then
    begin
        ShapeFactor := YearlyShapeObj.GetMult(Hr);
        ShapeIsActual := YearlyShapeObj.UseActual;
    end
    else
        ShapeFactor := cmplx(PerUnit, 0.0); // CDOUBLEONE;   // Defaults to no variation
end;

//=============================================================================
procedure TVsourceObj.MakePosSequence;

var
    S: String;
begin

    S := 'Phases=1 ';
    S := S + Format('BasekV=%-.5g ', [kVbase / SQRT3]);
    S := S + Format('R1=%-.5g ', [R1]);
    S := S + Format('X1=%-.5g ', [X1]);

    Parser.CmdString := S;
    Edit;

    inherited;

end;

initialization

    CDOUBLEONE := CMplx(1.0, 1.0);
end.
