unit Isource;

{
  ----------------------------------------------------------
  Copyright (c) 2008-2015, Electric Power Research Institute, Inc.
  All rights reserved.
  ----------------------------------------------------------
}

{  Ideal current source

   Stick'em on wherever you want as many as you want

   ISource maintains a positive sequence for harmonic scans.  If you want zero sequence,
   use three single-phase ISource.


 10-25-00  Created from Vsource
 5-17-02  Moved spectrum to base class
 2-19-03 Added Phaseshift variable for n-phase elements

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
    TIsource = class(TPCClass)
    PRIVATE
        procedure IsourceSetBus1(const S: String);
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
    TIsourceObj = class(TPCElement)
    PRIVATE

        FphaseShift: Double;
        ShapeIsActual: Boolean;
        ShapeFactor: Complex;
        Bus2Defined: Boolean;

        function GetBaseCurr: Complex;
        procedure CalcDailyMult(Hr: Double);
        procedure CalcDutyMult(Hr: Double);
        procedure CalcYearlyMult(Hr: Double);

    PUBLIC

        Amps: Double;
        Angle: Double;
        SrcFrequency: Double;
        ScanType,
        SequenceType: Integer;
        PerUnit: Double;
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

        procedure MakePosSequence; OVERRIDE;  // Make a positive Sequence Model

        function InjCurrents: Integer; OVERRIDE;
        procedure GetInjCurrents(Curr: pComplexArray); OVERRIDE;
        procedure GetCurrents(Curr: pComplexArray); OVERRIDE;
        procedure InitPropertyValues(ArrayOffset: Integer); OVERRIDE;
        procedure DumpProperties(var F: TextFile; Complete: Boolean); OVERRIDE;

    end;

var
    ActiveIsourceObj: TIsourceObj;
    IsourceClass: TISource;

// = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = =
implementation


uses
    ParserDel,
    Circuit,
    DSSClassDefs,
    DSSGlobals,
    Utilities,
    Sysutils,
    Command,
    dynamics;

var
    NumPropsThisClass: Integer;

//- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
constructor TIsource.Create;  // Creates superstructure for all Line objects
begin
    inherited Create;
    Class_Name := 'Isource';
    DSSClassType := SOURCE + NON_PCPD_ELEM;  // Don't want this in PC Element List

    ActiveElement := 0;

    DefineProperties;

    CommandList := TCommandList.Create(Slice(PropertyName^, NumProperties));
    CommandList.Abbrev := TRUE;

    IsourceClass := Self;
end;

//- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
destructor TIsource.Destroy;

begin
    // ElementList and  CommandList freed in inherited destroy
    inherited Destroy;

end;

//- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
procedure TIsource.DefineProperties;
begin
    NumPropsThisClass := 11;

    Numproperties := NumPropsThisClass;
    CountProperties;   // Get inherited property count
    AllocatePropertyArrays;


     // Define Property names
    PropertyName[1] := 'bus1';
    PropertyName[2] := 'amps';
    PropertyName[3] := 'angle';
    PropertyName[4] := 'frequency';
    PropertyName[5] := 'phases';
    PropertyName[6] := 'scantype';
    PropertyName[7] := 'sequence';
    PropertyName[8] := 'Yearly';
    PropertyName[9] := 'Daily';
    PropertyName[10] := 'Duty';
    PropertyName[11] := 'Bus2';

     // define Property help values
    PropertyHelp[1] := 'Name of bus to which source is connected.' + CRLF + 'bus1=busname' + CRLF + 'bus1=busname.1.2.3';
    PropertyHelp[2] := 'Magnitude of current source, each phase, in Amps.';
    PropertyHelp[3] := 'Phase angle in degrees of first phase: e.g.,Angle=10.3.' + CRLF +
        'Phase shift between phases is assumed 120 degrees when ' +
        'number of phases <= 3';
    PropertyHelp[4] := 'Source frequency.  Defaults to  circuit fundamental frequency.';
    PropertyHelp[5] := 'Number of phases.  Defaults to 3. For 3 or less, phase shift is 120 degrees.';
    PropertyHelp[6] := '{pos*| zero | none} Maintain specified sequence for harmonic solution. Default is positive sequence. ' +
        'Otherwise, angle between phases rotates with harmonic.';
    PropertyHelp[7] := '{pos*| neg | zero} Set the phase angles for the specified symmetrical component sequence for non-harmonic solution modes. ' +
        'Default is positive sequence. ';
    PropertyHelp[8] := 'LOADSHAPE object to use for the per-unit current for YEARLY-mode simulations. Set the Mult property of the LOADSHAPE ' +
        'to the pu curve. Qmult is not used. If UseActual=Yes then the Mult curve should be actual Amp.' + CRLF + CRLF +
        'Must be previously defined as a LOADSHAPE object. ' + CRLF + CRLF +
        'Is set to the Daily load shape when Daily is defined.  The daily load shape is repeated in this case. ' +
        'Set to NONE to reset to no loadahape for Yearly mode. ' +
        'The default is no variation.';
    PropertyHelp[9] := 'LOADSHAPE object to use for the per-unit current for DAILY-mode simulations. Set the Mult property of the LOADSHAPE ' +
        'to the pu curve. Qmult is not used. If UseActual=Yes then the Mult curve should be actual A.' + CRLF + CRLF +
        'Must be previously defined as a LOADSHAPE object. ' + CRLF + CRLF +
        'Sets Yearly curve if it is not already defined.   ' +
        'Set to NONE to reset to no loadahape for Yearly mode. ' +
        'The default is no variation.';
    PropertyHelp[10] := 'LOADSHAPE object to use for the per-unit current for DUTYCYCLE-mode simulations. Set the Mult property of the LOADSHAPE ' +
        'to the pu curve. Qmult is not used. If UseActual=Yes then the Mult curve should be actual A.' + CRLF + CRLF +
        'Must be previously defined as a LOADSHAPE object. ' + CRLF + CRLF +
        'Defaults to Daily load shape when Daily is defined.   ' +
        'Set to NONE to reset to no loadahape for Yearly mode. ' +
        'The default is no variation.';
    PropertyHelp[11] := 'Name of bus to which 2nd terminal is connected.' + CRLF + 'bus2=busname' + CRLF + 'bus2=busname.1.2.3' +
        CRLF + CRLF +
        'Default is Bus1.0.0.0 (grounded-wye connection)';

    ActiveProperty := NumPropsThisClass;
    inherited DefineProperties;  // Add defs of inherited properties to bottom of list

     // Override help string
    PropertyHelp[NumPropsThisClass + 1] := 'Harmonic spectrum assumed for this source.  Default is "default".';

end;


//- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
function TIsource.NewObject(const ObjName: String): Integer;
begin
    // Make a new voltage source and add it to Isource class list
    with ActiveCircuit do
    begin
        ActiveCktElement := TIsourceObj.Create(Self, ObjName);
        Result := AddObjectToList(ActiveDSSObject);
    end;
end;


//- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
function TIsource.Edit: Integer;
var
    ParamPointer: Integer;
    ParamName,
    Param: String;

begin
  // continue parsing with contents of Parser
    ActiveIsourceObj := ElementList.Active;
    ActiveCircuit.ActiveCktElement := ActiveIsourceObj;

    Result := 0;

    with ActiveIsourceObj do
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
                    DoSimpleMsg('Unknown parameter "' + ParamName + '" for Object "' + Class_Name + '.' + Name + '"', 330);
                1:
                    IsourceSetBus1(param);
                2:
                    Amps := Parser.DblValue;
                3:
                    Angle := Parser.DblValue; // Ang
                4:
                    SrcFrequency := Parser.DblValue; // freq
                5:
                begin
                    Nphases := Parser.IntValue; // num phases
                    case FNphases of
                        1:
                            FphaseShift := 0.0;
                        2, 3:
                            FphaseShift := 120.0;
                    else     // higher order systems
                        FphaseShift := 360.0 / FNphases;
                    end;
                    NConds := Fnphases;  // Force Reallocation of terminal info
                end;
                6:
                    case Uppercase(Param)[1] of
                        'P':
                            ScanType := 1;
                        'Z':
                            ScanType := 0;
                        'N':
                            ScanType := -1;
                    else
                        DoSimpleMsg('Unknown Scan Type for "' + Class_Name + '.' + Name + '": ' + Param, 331);
                    end;
                7:
                    case Uppercase(Param)[1] of
                        'P':
                            SequenceType := 1;
                        'Z':
                            SequenceType := 0;
                        'N':
                            SequenceType := -1;
                    else
                        DoSimpleMsg('Unknown Sequence Type for "' + Class_Name + '.' + Name + '": ' + Param, 331);
                    end;
                8:
                    YearlyShape := Param;
                9:
                    DailyShape := Param;
                10:
                    DutyShape := Param;
                11:
                    SetBus(2, Param);
            else
                ClassEdit(ActiveIsourceObj, ParamPointer - NumPropsThisClass);
            end;

            case ParamPointer of
            {Set shape objects;  returns nil if not valid}
            {Sets the kW and kvar properties to match the peak kW demand from the Loadshape}
                8:
                    YearlyShapeObj := LoadShapeClass.Find(YearlyShape);
                9:
                begin
                    DailyShapeObj := LoadShapeClass.Find(DailyShape);
                  {If Yearly load shape is not yet defined, make it the same as Daily}
                    if YearlyShapeObj = NIL then
                        YearlyShapeObj := DailyShapeObj;
                end;
                10:
                    DutyShapeObj := LoadShapeClass.Find(DutyShape);
            end;
            ParamName := Parser.NextParam;
            Param := Parser.StrValue;
        end;

        RecalcElementData;
        YPrimInvalid := TRUE;
    end;

end;

//----------------------------------------------------------------------------
function TIsource.MakeLike(const OtherSource: String): Integer;
var
    OtherIsource: TIsourceObj;
    i: Integer;

begin
    Result := 0;
   {See if we can find this line name in the present collection}
    OtherIsource := Find(OtherSource);
    if OtherIsource <> NIL then
        with ActiveIsourceObj do
        begin

            if Fnphases <> OtherIsource.Fnphases then
            begin
                Nphases := OtherIsource.Fnphases;
                NConds := Fnphases;  // Forces reallocation of terminal stuff

                Yorder := Fnconds * Fnterms;
                YPrimInvalid := TRUE;
            end;

            Amps := OtherIsource.Amps;
            Angle := OtherIsource.Angle;
            SrcFrequency := OtherIsource.SrcFrequency;
            Scantype := OtherIsource.Scantype;
            Sequencetype := OtherIsource.Sequencetype;

            ShapeIsActual := OtherIsource.ShapeIsActual;
            DailyShape := OtherIsource.DailyShape;
            DailyShapeObj := OtherIsource.DailyShapeObj;
            DutyShape := OtherIsource.DutyShape;
            DutyShapeObj := OtherIsource.DutyShapeObj;
            YearlyShape := OtherIsource.YearlyShape;
            YearlyShapeObj := OtherIsource.YearlyShapeObj;

            Bus2Defined := OtherIsource.Bus2Defined;

            ClassMakeLike(OtherIsource); // set spectrum,  base frequency

            for i := 1 to ParentClass.NumProperties do
                PropertyValue[i] := OtherIsource.PropertyValue[i];
            Result := 1;
        end
    else
        DoSimpleMsg('Error in Isource MakeLike: "' + OtherSource + '" Not Found.', 332);

end;

//----------------------------------------------------------------------------
function TIsource.Init(Handle: Integer): Integer;

begin
    DoSimpleMsg('Need to implement TIsource.Init', -1);
    Result := 0;
end;

procedure TIsource.IsourceSetBus1(const S: String);
var
    s2: String;
    i, dotpos: Integer;

   // Special handling for Bus 1
   // Set Bus2 = Bus1.0.0.0

begin
    with ActiveISourceObj do
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

//----------------------------------------------------------------------------
constructor TIsourceObj.Create(ParClass: TDSSClass; const SourceName: String);
begin
    inherited create(ParClass);
    Name := LowerCase(SourceName);
    DSSObjType := ParClass.DSSClassType; // SOURCE + NON_PCPD_ELEM;  // Don't want this in PC Element List

    Nphases := 3;
    Fnconds := 3;
    Nterms := 2;   // 4/27/2018 made a 2-terminal I source

    Amps := 0.0;
    Angle := 0.0;
    PerUnit := 1.0;  // for future use if pu property added,
    SrcFrequency := BaseFrequency;
    FphaseShift := 120.0;
    ScanType := 1;  // Pos Sequence
    Sequencetype := 1;
    Bus2Defined := FALSE;
    InitPropertyValues(0);
    ShapeIsActual := FALSE;
    YearlyShape := '';
    YearlyShapeObj := NIL;  // IF YearlyShapeobj = nil THEN the Vsource alway stays nominal
    DailyShape := '';
    DailyShapeObj := NIL;  // IF DaillyShapeobj = nil THEN the Vsource alway stays nominal
    DutyShape := '';
    DutyShapeObj := NIL;  // IF DutyShapeobj = nil THEN the Vsource alway stays nominal

    Yorder := Fnterms * Fnconds;
    RecalcElementData;

end;


//----------------------------------------------------------------------------
destructor TIsourceObj.Destroy;
begin
    inherited Destroy;
end;

//----------------------------------------------------------------------------
procedure TIsourceObj.RecalcElementData;
begin

    SpectrumObj := SpectrumClass.Find(Spectrum);

    if SpectrumObj = NIL then
    begin
        DoSimpleMsg('Spectrum Object "' + Spectrum + '" for Device Isource.' + Name + ' Not Found.', 333);
    end;

    Reallocmem(InjCurrent, SizeOf(InjCurrent^[1]) * Yorder);

end;

//----------------------------------------------------------------------------
procedure TIsourceObj.CalcYPrim;


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


     {Yprim = 0  for Ideal Current Source;  just leave it zeroed}

     {Now Account for Open Conductors}
     {For any conductor that is open, zero out row and column}
    inherited CalcYPrim;

    YPrimInvalid := FALSE;

end;

function TIsourceObj.GetBaseCurr: Complex;

var
    SrcHarmonic: Double;
    NAmps: Double;

begin

    try

        with ActiveCircuit.Solution do
  {Get first Phase Current}
            if IsHarmonicModel then
            begin
                SrcHarmonic := Frequency / SrcFrequency;
                Result := CMulReal(SpectrumObj.GetMult(SrcHarmonic), Amps);  // Base current for this harmonic
                RotatePhasorDeg(Result, SrcHarmonic, Angle);
            end
            else
            begin
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
                NAmps := Amps;
                if (Mode = DAILYMODE) or     {If a loadshape mode simulation}
                    (Mode = YEARLYMODE) or
                    (Mode = DUTYCYCLE) then
                    NAmps := Amps * ShapeFactor.re;
                if abs(Frequency - SrcFrequency) < EPSILON2 then
                    Result := pdegtocomplex(NAmps, Angle)
                else
                    Result := CZERO;
            end;

    except
        DoSimpleMsg('Error computing current for Isource.' + Name + '. Check specification. Aborting.', 334);
        if In_Redirect then
            Redirect_Abort := TRUE;
    end;

end;

function TIsourceObj.InjCurrents: Integer;

{Sum Currents directly into solution array}

begin
    GetInjCurrents(InjCurrent);

    Result := inherited Injcurrents;  // Adds into system array

end;

procedure TIsourceObj.GetCurrents(Curr: pComplexArray);

{Total currents into a device}

var
    i: Integer;

begin

    try
        GetInjCurrents(ComplexBuffer);  // Get present value of inj currents
      // Add Together  with yprim currents
        for i := 1 to Yorder do
            Curr^[i] := Cnegate(ComplexBuffer^[i]);

    except
        On E: Exception do
            DoErrorMsg(('GetCurrents for Isource Element: ' + Name + '.'), E.Message,
                'Inadequate storage allotted for circuit element?', 335);
    end;

end;

procedure TIsourceObj.GetInjCurrents(Curr: pComplexArray);

{Fill Up an array of injection currents}

var
    i: Integer;
    BaseCurr: complex;
begin

    with ActiveCircuit.solution do
    begin
        BaseCurr := GetBaseCurr;   // this func applies spectrum if needed

        for i := 1 to Fnphases do
        begin
            Curr^[i] := BaseCurr;
            Curr^[i + FnPhases] := Cnegate(BaseCurr);  // 2nd Terminal
            if (i < Fnphases) then
            begin

                if IsHarmonicModel then

                    case ScanType of
                        1:
                            RotatePhasorDeg(BaseCurr, 1.0, -FphaseShift); // maintain positive sequence for isource
                        0: ;  // Do not rotate for zero sequence
                    else
                        RotatePhasorDeg(BaseCurr, Harmonic, -FphaseShift) // rotate by frequency
                     {Harmonic 1 will be pos; 2 is neg; 3 is zero, and so on.}
                    end

                else

                    case SequenceType of
                        -1:
                            RotatePhasorDeg(BaseCurr, 1.0, FphaseShift); // Neg seq
                        0: ;  // Do not rotate for zero sequence
                    else
                        RotatePhasorDeg(BaseCurr, 1.0, -FphaseShift); // Maintain pos seq
                    end;

            end;
        end;
    end;
end;

procedure TIsourceObj.DumpProperties(var F: TextFile; Complete: Boolean);

var
    i: Integer;

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
        Writeln(F);
    end;

end;

procedure TIsourceObj.InitPropertyValues(ArrayOffset: Integer);
begin

    PropertyValue[1] := GetBus(1);
    PropertyValue[2] := '0';
    PropertyValue[3] := '0';
    PropertyValue[4] := Format('%-.6g', [SrcFrequency]);
    PropertyValue[5] := '3';
    PropertyValue[6] := 'pos';
    PropertyValue[7] := 'pos';
    PropertyValue[8] := '';
    PropertyValue[9] := '';
    PropertyValue[10] := '';
    PropertyValue[11] := '';

    inherited  InitPropertyValues(NumPropsThisClass);

end;

procedure TIsourceObj.MakePosSequence;
begin

    if Fnphases > 1 then
    begin
        Parser.CmdString := 'phases=1';
        Edit;
    end;
    inherited;

end;

procedure TISourceObj.CalcDailyMult(Hr: Double);

begin
    if DailyShapeObj <> NIL then
    begin
        ShapeFactor := DailyShapeObj.GetMult(Hr);
        ShapeIsActual := DailyShapeObj.UseActual;
    end
    else
        ShapeFactor := cmplx(PerUnit, 0.0); // CDOUBLEONE;  // Default to no daily variation
end;

procedure TISourceObj.CalcDutyMult(Hr: Double);

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
procedure TISourceObj.CalcYearlyMult(Hr: Double);

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

end.
