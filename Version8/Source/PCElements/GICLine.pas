unit GICLine;

{
  ----------------------------------------------------------
  Copyright (c) 2011-2015, Electric Power Research Institute, Inc.
  All rights reserved.
  ----------------------------------------------------------
}

{
 6-23-2011 Created from VSource object

 Simplified 2-terminal VSource with series impedance for GIC studies.
 For representing induced voltages in lines

 Contains blocking capacitor inherent in model.  Set C > 0.0 to activate.

 Blocking capacitors may also be added as separate items or the branch may be
 disconnected.

 Example:
    New GICline.Myline  Bus1=MyBus1  Bus2=MyBus2  Volts=1234   R=0.5

    This takes the following defaults:
      Angle=0
      X=0
      C=0
      Frequency=0.1 Hz
      Sequence = ZERO sequence
      ScanType = ZERO sequence


}

interface

uses
    DSSClass,
    PCClass,
    PCElement,
    ucmatrix,
    ucomplex,
    Spectrum;

type
// = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = =
    TGICLine = class(TPCClass)
    PRIVATE
        procedure GICLineSetBus1(const S: String);
    PROTECTED
        procedure DefineProperties;
        function MakeLike(const OtherLine: String): Integer; OVERRIDE;
    PUBLIC
        constructor Create;
        destructor Destroy; OVERRIDE;

        function Edit(ActorID: Integer): Integer; OVERRIDE;
        function Init(Handle: Integer; ActorID: Integer): Integer; OVERRIDE;
        function NewObject(const ObjName: String): Integer; OVERRIDE;
    end;

// = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = =
    TGICLineObj = class(TPCElement)
    PRIVATE
        Angle: Double;
        Volts: Double;
        Vmag: Double;  // Present voltage magnitude
        SrcFrequency: Double;
        R,
        X,
        C,
        ENorth,
        EEast,
        Lat1,
        Lon1,
        Lat2,
        Lon2: Double;

        VN, VE: Double;  // components of vmag

        ScanType: Integer;
        SequenceType: Integer;
        VoltsSpecified: Boolean;

        procedure GetVterminalForSource;
        function Compute_VLine: Double;
    PUBLIC
        Z: TCmatrix;  // Base Frequency Series Z matrix
        Zinv: TCMatrix;


        constructor Create(ParClass: TDSSClass; const SourceName: String);
        destructor Destroy; OVERRIDE;

        procedure RecalcElementData(ActorID: Integer); OVERRIDE;
        procedure CalcYPrim(ActorID: Integer); OVERRIDE;

        function InjCurrents(ActorID: Integer): Integer; OVERRIDE;
        procedure GetInjCurrents(Curr: pComplexArray; ActorID: Integer); OVERRIDE;
        procedure GetCurrents(Curr: pComplexArray; ActorID: Integer); OVERRIDE;

        procedure MakePosSequence(ActorID: Integer); OVERRIDE;  // Make a positive Sequence Model

        procedure InitPropertyValues(ArrayOffset: Integer); OVERRIDE;
        procedure DumpProperties(var F: TextFile; Complete: Boolean); OVERRIDE;
        function GetPropertyValue(Index: Integer): String; OVERRIDE;

    end;

var
    ActiveGICLineObj: TGICLineObj;

// = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = =
implementation

uses
    ParserDel,
    Circuit,
    MyDSSClassDefs,
    DSSClassDefs,
    DSSGlobals,
    Utilities,
    Sysutils,
    Command;

const
    NumPropsThisClass = 15;

//- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
constructor TGICLine.Create;  // Creates superstructure for all Line objects
begin
    inherited Create;
    Class_Name := 'GICLine';
    DSSClassType := GIC_Line + PC_ELEMENT;

    ActiveElement := 0;

    DefineProperties;

    CommandList := TCommandList.Create(Slice(PropertyName^, NumProperties));
    CommandList.Abbrev := TRUE;
end;

//- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
destructor TGICLine.Destroy;

begin
    // ElementList and  CommandList freed in inherited destroy
    inherited Destroy;

end;

//- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
procedure TGICLine.DefineProperties;
begin

    Numproperties := NumPropsThisClass;
    CountProperties;   // Get inherited property count
    AllocatePropertyArrays;

     // Define Property names
    PropertyName[1] := 'bus1';
    PropertyName[2] := 'bus2';
    PropertyName[3] := 'Volts';
    PropertyName[4] := 'Angle';
    PropertyName[5] := 'frequency';
    PropertyName[6] := 'phases';
    PropertyName[7] := 'R';
    PropertyName[8] := 'X';
    PropertyName[9] := 'C';
  //   PropertyName[10] := 'ScanType';
  //   PropertyName[11] := 'Sequence';
    PropertyName[10] := 'EN';
    PropertyName[11] := 'EE';
    PropertyName[12] := 'Lat1';
    PropertyName[13] := 'Lon1';
    PropertyName[14] := 'Lat2';
    PropertyName[15] := 'Lon2';

     // define Property help values
    PropertyHelp[1] := 'Name of bus to which the main terminal (1) is connected.' + CRLF +
        'bus1=busname' + CRLF +
        'bus1=busname.1.2.3';
    PropertyHelp[2] := 'Name of bus to which 2nd terminal is connected.' + CRLF +
        'bus2=busname' + CRLF +
        'bus2=busname.1.2.3' + CRLF + CRLF +
        'No Default; must be specified.';

    PropertyHelp[3] := 'Voltage magnitude, in volts, of the GIC voltage induced across this line. ' +
        'When spedified, voltage source is assumed defined by Voltage and Angle properties. ' + CRLF + CRLF +
        'Specify this value' + CRLF + CRLF + 'OR' + CRLF + CRLF +
        'EN, EE, lat1, lon1, lat2, lon2. ' + CRLF + CRLF +
        'Not both!!  Last one entered will take precedence. ' +
        'Assumed identical in each phase of the Line object.';
    PropertyHelp[4] := 'Phase angle in degrees of first phase. Default=0.0.  See Voltage property';
    PropertyHelp[5] := 'Source frequency.  Defaults to 0.1 Hz.';
    PropertyHelp[6] := 'Number of phases.  Defaults to 3.';
    PropertyHelp[7] := 'Resistance of line, ohms of impedance in series with GIC voltage source. ';
    PropertyHelp[8] := 'Reactance at base frequency, ohms. Default = 0.0. This value is generally not important for GIC studies but may be used if desired.';
    PropertyHelp[9] := 'Value of line blocking capacitance in microfarads. Default = 0.0, implying that there is no line blocking capacitor.';
 //    PropertyHelp[10] := '{pos | zero* | none} Maintain specified sequence for harmonic solution. Default is ZERO sequence. '+
 //                        'Otherwise, angle between phases rotates with harmonic.';
 //    PropertyHelp[11] := '{pos | neg | zero*} Set the phase angles for the specified symmetrical component sequence for non-harmonic solution modes. '+
 //                        'Default is ZERO sequence. ';
    PropertyHelp[10] := 'Northward Electric field (V/km). If specified, Voltage and Angle are computed from EN, EE, lat and lon values.';
    PropertyHelp[11] := 'Eastward Electric field (V/km).  If specified, Voltage and Angle are computed from EN, EE, lat and lon values.';
    PropertyHelp[12] := 'Latitude of Bus1 (degrees)';
    PropertyHelp[13] := 'Longitude of Bus1 (degrees)';
    PropertyHelp[14] := 'Latitude of Bus2 (degrees)';
    PropertyHelp[15] := 'Longitude of Bus2 (degrees)';

    ActiveProperty := NumPropsThisClass;
    inherited DefineProperties;  // Add defs of inherited properties to bottom of list

     // Override help string
    PropertyHelp[NumPropsThisClass + 1] := 'Inherited Property for all PCElements. Name of harmonic spectrum for this source.  Default is "defaultvsource", which is defined when the DSS starts.';
    PropertyHelp[NumPropsThisClass + 2] := 'Inherited Property for all PCElements. Base frequency for specification of reactance value.';
end;


//- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
function TGICLine.NewObject(const ObjName: String): Integer;
begin
    // Make a new voltage source and add it to GICLine class list
    with ActiveCircuit[ActiveActor] do
    begin
        ActiveCktElement := TGICLineObj.Create(Self, ObjName);
        Result := AddObjectToList(ActiveDSSObject[ActiveActor]);
    end;
end;


//- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
procedure TGICLine.GICLineSetBus1(const S: String);
var
    s2: String;
    dotpos: Integer;

   // Special handling for Bus 1
   // Set Bus2 = Bus1.0.0.0

begin
    with ActiveGICLineObj do
    begin
        SetBus(1, S);

     // Strip node designations from S
        dotpos := Pos('.', S);
        if dotpos > 0 then
            S2 := Copy(S, 1, dotpos - 1)
        else
            S2 := Copy(S, 1, Length(S));  // copy up to Dot

        SetBus(2, S2);    // default setting for Bus2  is same as Bus1
    end;

end;

//- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
function TGICLine.Edit(ActorID: Integer): Integer;
var
    ParamPointer: Integer;
    ParamName,
    Param: String;

begin
  // continue parsing with contents of Parser
    ActiveGICLineObj := ElementList.Active;
    ActiveCircuit[ActorID].ActiveCktElement := ActiveGICLineObj;

    Result := 0;

    with ActiveGICLineObj do
    begin

        ParamPointer := 0;
        ParamName := Parser[ActorID].NextParam;
        Param := Parser[ActorID].StrValue;
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
                    GICLineSetBus1(param);   // special handling of Bus 1
                2:
                    SetBus(2, param);

                3:
                    Volts := Parser[ActorID].DblValue; // basekv
                4:
                    Angle := Parser[ActorID].DblValue; // Ang
                5:
                    SrcFrequency := Parser[ActorID].DblValue; // freq
                6:
                begin
                    Nphases := Parser[ActorID].Intvalue; // num phases
                    NConds := Fnphases;  // Force Reallocation of terminal info
                end;
                7:
                    R := Parser[ActorID].DblValue;
                8:
                    X := Parser[ActorID].DblValue;
                9:
                    C := Parser[ActorID].DblValue;

    (*     10:  Case Uppercase(Param)[1] of
                  'P': ScanType :=  1;
                  'Z': ScanType :=  0;
                  'N': ScanType := -1;
                ELSE
                   DoSimpleMsg('Unknown Scan Type for "' + Class_Name +'.'+ Name + '": '+Param, 321);
                END;
           11:  Case Uppercase(Param)[1] of
                  'P': Sequencetype :=  1;
                  'Z': Sequencetype :=  0;
                  'N': Sequencetype := -1;
                ELSE
                   DoSimpleMsg('Unknown Sequence Type for "' + Class_Name +'.'+ Name + '": '+Param, 321);
                END;
    *)
                10:
                    ENorth := Parser[ActorID].DblValue;
                11:
                    EEast := Parser[ActorID].DblValue;
                12:
                    Lat1 := Parser[ActorID].DblValue;
                13:
                    Lon1 := Parser[ActorID].DblValue;
                14:
                    Lat2 := Parser[ActorID].DblValue;
                15:
                    Lon2 := Parser[ActorID].DblValue;

            else
                ClassEdit(ActiveGICLineObj, ParamPointer - NumPropsThisClass)
            end;

            case ParamPointer of
                3, 4:
                    VoltsSpecified := TRUE;
                10..15:
                    VoltsSpecified := FALSE;
            end;

            ParamName := Parser[ActorID].NextParam;
            Param := Parser[ActorID].StrValue;
        end;

        RecalcElementData(ActorID);
        YprimInvalid[ActorID] := TRUE;
    end;

end;

//----------------------------------------------------------------------------
function TGICLine.MakeLike(const OtherLine: String): Integer;
var
    OtherGICLine: TGICLineObj;
    i: Integer;

begin
    Result := 0;
   {See if we can find this line name in the present collection}
    OtherGICLine := Find(OtherLine);
    if OtherGICLine <> NIL then
        with ActiveGICLineObj do
        begin

            if Fnphases <> OtherGICLine.Fnphases then
            begin
                Nphases := OtherGICLine.Fnphases;
                NConds := Fnphases;  // Forces reallocation of terminal stuff

                Yorder := Fnconds * Fnterms;
                YprimInvalid[ActiveActor] := TRUE;

                if Z <> NIL then
                    Z.Free;
                if Zinv <> NIL then
                    Zinv.Free;

                Z := TCmatrix.CreateMatrix(Fnphases);
                Zinv := TCMatrix.CreateMatrix(Fnphases);
            end;

            Z.CopyFrom(OtherGICLine.Z);
       // Zinv.CopyFrom(OtherLine.Zinv);
            R := OtherGICLine.R;
            X := OtherGICLine.X;
            C := OtherGICLine.C;
            Volts := OtherGICLine.Volts;
            Angle := OtherGICLine.Angle;

            SrcFrequency := OtherGICLine.SrcFrequency;
            Scantype := OtherGICLine.Scantype;
            Sequencetype := OtherGICLine.Sequencetype;

            ClassMakeLike(OtherGICLine);

            for i := 1 to ParentClass.NumProperties do
                FPropertyValue[i] := OtherGICLine.FPropertyValue[i];
            Result := 1;
        end
    else
        DoSimpleMsg('Error in GICLine MakeLike: "' + OtherLine + '" Not Found.', 322);

end;

//----------------------------------------------------------------------------
function TGICLine.Init(Handle: Integer; ActorID: Integer): Integer;

begin
    DoSimpleMsg('Need to implement TGICLine.Init', -1);
    Result := 0;
end;

//=============================================================================
function TGICLineObj.Compute_VLine: Double;
var

    Phi: Double;
    DeltaLat, DeltaLon: Double;
begin

    Phi := (Lat2 + Lat1) / 2.0 * (pi / 180.0);   // deg to radians
    DeltaLat := Lat2 - Lat1;
    DeltaLon := Lon2 - Lon1;
    VE := (111.133 - 0.56 * cos(2.0 * phi)) * DeltaLat * ENorth;
    VN := (111.5065 - 0.1872 * cos(2.0 * phi)) * Cos(phi) * DeltaLon * EEast;
    Result := VN + VE;
end;

constructor TGICLineObj.Create(ParClass: TDSSClass; const SourceName: String);
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

    R := 1.0;
    X := 0.0;
    C := 0.0;

    ENorth := 1.0;
    EEast := 1.0;
    Lat1 := 33.613499;
    Lon1 := -87.373673;
    Lat2 := 33.547885;
    Lon2 := -86.074605;

    VoltsSpecified := FALSE;

    SrcFrequency := 0.1;  // Typical GIC study frequency
    Angle := 0.0;
    Scantype := 0;
    SequenceType := 0; // default to zero sequence (same voltage induced in all phases)

    Spectrum := '';  // no default

    InitPropertyValues(0);

    Yorder := Fnterms * Fnconds;
    RecalcElementData(ActiveActor);

end;


//=============================================================================
destructor TGICLineObj.Destroy;
begin
    Z.Free;
    Zinv.Free;

    inherited Destroy;
end;

//=============================================================================
procedure TGICLineObj.RecalcElementData(ActorID: Integer);
var
    Zs, Zm: Complex;
    i, j: Integer;

begin
    if Z <> NIL then
        Z.Free;
    if Zinv <> NIL then
        Zinv.Free;

    // For a Source, nphases = ncond, for now
    Z := TCmatrix.CreateMatrix(Fnphases);
    Zinv := TCMatrix.CreateMatrix(Fnphases);

    {Update property Value array}
     { Don't change a specified value; only computed ones}

    Zs := cmplx(R, X);
    Zm := CZERO;

    for i := 1 to Fnphases do
    begin
        Z.SetElement(i, i, Zs);
        for j := 1 to i - 1 do
        begin
            Z.SetElemsym(i, j, Zm);
        end;
    end;

    if not VoltsSpecified then
        Volts := Compute_VLine;

    Vmag := Volts;

    SpectrumObj := SpectrumClass[ActorID].Find(Spectrum);
    if (SpectrumObj = NIL) and (Length(Spectrum) > 0) then
    begin
        DoSimpleMsg('Spectrum Object "' + Spectrum + '" for Device GICLine.' + Name + ' Not Found.', 324);
    end;

    Reallocmem(InjCurrent, SizeOf(InjCurrent^[1]) * Yorder);

end;

//=============================================================================
procedure TGICLineObj.CalcYPrim(ActorID: Integer);

var
    Value: Complex;
    i, j: Integer;
    FreqMultiplier: Double;
    Xc: Double;

begin

 // Build only YPrim Series
    if YprimInvalid[ActorID] then
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

    FYprimFreq := ActiveCircuit[ActorID].Solution.Frequency;
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

    if C > 0.0 then // Add 1/wC into diagonals of Zinv
    begin
        Xc := -1.0 / (twopi * FYprimFreq * C * 1.0e-6);
        for i := 1 to Fnphases do
            Zinv.AddElement(i, i, Cmplx(0.0, Xc));
    end;

    Zinv.Invert;  {Invert in place}

    if Zinv.InvertError > 0 then
    begin       {If error, put in Large series conductance}
        DoErrorMsg('TGICLineObj.CalcYPrim', 'Matrix Inversion Error for GICLine "' + Name + '"',
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
            YPrim_series.SetElemsym(i + FNPhases, j, CNegate(Value))
        end;
    end;

    YPrim.CopyFrom(YPrim_Series);

     {Now Account for Open Conductors}
     {For any conductor that is open, zero out row and column}
    inherited CalcYPrim(ActorID);

    YprimInvalid[ActorID] := FALSE;

end;

//=============================================================================
procedure TGICLineObj.GetVterminalForSource;

var
    i: Integer;
    Vharm: Complex;
    SrcHarmonic: Double;

begin

    try

  {This formulation will theoretically handle voltage sources of any number of phases assuming they are
   equally displaced in time.}
        Vmag := Volts;

        with ActiveCircuit[ActiveActor].Solution do

            if IsHarmonicModel and (SpectrumObj <> NIL) then
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
            begin  // non-harmonic modes or no spectrum
                if abs(Frequency - SrcFrequency) > EPSILON2 then
                    Vmag := 0.0;  // Solution Frequency and Source Frequency don't match!
       {NOTE: RE-uses VTerminal space}
                for i := 1 to Fnphases do
                begin
                    case Sequencetype of   // Always 0 for GIC
                        -1:
                            Vterminal^[i] := pdegtocomplex(Vmag, (360.0 + Angle + (i - 1) * 360.0 / Fnphases));  // neg seq
                        0:
                            Vterminal^[i] := pdegtocomplex(Vmag, (360.0 + Angle));   // all the same for zero sequence
                    else
                        Vterminal^[i] := pdegtocomplex(Vmag, (360.0 + Angle - (i - 1) * 360.0 / Fnphases));
                    end;
                // bottom part of the vector is zero
                    VTerminal^[i + Fnphases] := CZERO;    // See comments in GetInjCurrents
                end;
            end;

    except
        DoSimpleMsg('Error computing Voltages for GICLine.' + Name + '. Check specification. Aborting.', 326);
        if In_Redirect then
            Redirect_Abort := TRUE;
    end;

end;

//===========================================================================

function TGICLineObj.InjCurrents(ActorID: Integer): Integer;

begin

    GetInjCurrents(InjCurrent, ActorID);

{This is source injection}

    Result := inherited InjCurrents(ActorID); // Add into system array

end;

//===========================================================================
procedure TGICLineObj.GetCurrents(Curr: pComplexArray; ActorID: Integer);

var
    i: Integer;

begin
    try
        with    ActiveCircuit[ActorID].Solution do
        begin

            for  i := 1 to Yorder do
                Vterminal^[i] := NodeV^[NodeRef^[i]];

            YPrim.MVMult(Curr, Vterminal);  // Current from Elements in System Y

            GetInjCurrents(ComplexBuffer, ActorID);  // Get present value of inj currents
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
procedure TGICLineObj.GetInjCurrents(Curr: pComplexArray; ActorID: Integer);

begin

   { source injection currents given by this formula:
     _     _           _         _
     |Iinj1|           |GICLine  |
     |     | = [Yprim] |         |
     |Iinj2|           | 0       |
     _     _           _         _
   }

    GetVterminalForSource;  // gets voltage vector above
    YPrim.MVMult(Curr, Vterminal);

    set_ITerminalUpdated(FALSE, ActorID);

end;

//=============================================================================
procedure TGICLineObj.DumpProperties(var F: TextFile; Complete: Boolean);

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
        Writeln(F, 'Volts=', Volts: 0: 2);
        Writeln(F, 'VMag=', VMag: 0: 2);
        Writeln(F, 'VE=', VE: 0: 4);
        Writeln(F, 'VN=', VN: 0: 4);
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
procedure TGICLineObj.InitPropertyValues(ArrayOffset: Integer);
begin

     {PropertyValue Allocated in DSSObject.Create}
    PropertyValue[1] := GetBus(1);
    PropertyValue[2] := GetBus(2);
    PropertyValue[3] := '0.0';
    PropertyValue[4] := '0';
    PropertyValue[5] := '0.1';
    PropertyValue[6] := '3';
    PropertyValue[7] := '1.0';
    PropertyValue[8] := '0';
    PropertyValue[9] := '0';

   //  PropertyValue[10] := 'zero';
  //   PropertyValue[11] := 'zero';
    PropertyValue[10] := '1.0';
    PropertyValue[11] := '1.0';
    PropertyValue[12] := '33.613499';
    PropertyValue[13] := '-87.373673';
    PropertyValue[14] := '33.547885';
    PropertyValue[15] := '-86.074605';

    inherited  InitPropertyValues(NumPropsThisClass);

end;

//=============================================================================
function TGICLineObj.GetPropertyValue(Index: Integer): String;
begin
    case Index of
        1:
            Result := GetBus(1);
        2:
            Result := GetBus(2);
        3:
            Result := Format('%.8g', [Volts]);
        4:
            Result := Format('%.8g', [Angle]);
        5:
            Result := Format('%.8g', [SrcFrequency]);
    else
        Result := inherited GetPropertyValue(Index);
    end;
end;

//=============================================================================
procedure TGICLineObj.MakePosSequence(ActorID: Integer);

var
    S: String;
begin

    S := 'Phases=1 ';
    S := S + Format('Voltage=%-.8g  Angle=%=.5g', [Volts, Angle]);
    S := S + Format('R=%-.8g ', [R]);
    S := S + Format('X=%-.8g ', [X]);

    Parser[ActorID].CmdString := S;
    Edit(ActorID);

    inherited;

end;

end.
