unit GICsource;

{
  ----------------------------------------------------------
  Copyright (c) 2008-2018, Electric Power Research Institute, Inc.
  All rights reserved.
  ----------------------------------------------------------
}

{
   Develpoed from Isource and GICLine May 2018
}


interface

uses
    DSSClass,
    PCClass,
    PCElement,
    ucmatrix,
    ucomplex,
    Line;

type
// = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = =
    TGICsource = class(TPCClass)
    PRIVATE
    PROTECTED
        procedure DefineProperties;
        function MakeLike(const OtherSource: String): Integer; OVERRIDE;
    PUBLIC
        constructor Create;
        destructor Destroy; OVERRIDE;

        function Edit(ActorID: Integer): Integer; OVERRIDE;
        function Init(Handle: Integer; ActorID: Integer): Integer; OVERRIDE;
        function NewObject(const ObjName: String): Integer; OVERRIDE;
    end;

// = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = =
    TGICSourceObj = class(TPCElement)
    PRIVATE

        FphaseShift: Double;
        Bus2Defined: Boolean;

        Vmag: Double;
        Angle: Double;
        SrcFrequency: Double;
        LineName: String;
        pLineElem: TLineObj;  // Pointer to associated Line

        VN, VE: Double;  // components of vmag

        LineClass: Tline;

        procedure GetVterminalForSource(ActorID: Integer);
        function Compute_VLine: Double;
    PUBLIC

        ENorth,
        EEast,
        Lat1,
        Lon1,
        Lat2,
        Lon2: Double;
        Volts: Double;
        VoltsSpecified: Boolean;

        constructor Create(ParClass: TDSSClass; const SourceName: String);
        destructor Destroy; OVERRIDE;

        procedure RecalcElementData(ActorID: Integer); OVERRIDE;
        procedure CalcYPrim(ActorID: Integer); OVERRIDE;

        procedure MakePosSequence(ActorID: Integer); OVERRIDE;  // Make a positive Sequence Model

        function InjCurrents(ActorID: Integer): Integer; OVERRIDE;
        procedure GetInjCurrents(Curr: pComplexArray; ActorID: Integer); OVERRIDE;
        procedure GetCurrents(Curr: pComplexArray; ActorID: Integer); OVERRIDE;
        procedure InitPropertyValues(ArrayOffset: Integer); OVERRIDE;
        procedure DumpProperties(var F: TextFile; Complete: Boolean); OVERRIDE;
        function GetPropertyValue(Index: Integer): String; OVERRIDE;

    end;

var
    ActiveGICsourceObj: TGICSourceObj;
    GICsourceClass: TGICsource;

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
constructor TGICsource.Create;  // Creates superstructure for all Line objects
begin
    inherited Create;
    Class_Name := 'GICsource';
    DSSClassType := SOURCE + NON_PCPD_ELEM;  // Don't want this in PC Element List

    ActiveElement := 0;

    DefineProperties;

    CommandList := TCommandList.Create(Slice(PropertyName^, NumProperties));
    CommandList.Abbrev := TRUE;

    GICsourceClass := Self;
end;

//- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
destructor TGICsource.Destroy;

begin
    // ElementList and  CommandList freed in inherited destroy
    inherited Destroy;

end;

//- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
procedure TGICsource.DefineProperties;
begin
    NumPropsThisClass := 10;

    Numproperties := NumPropsThisClass;
    CountProperties;   // Get inherited property count
    AllocatePropertyArrays;


     // Define Property names
    PropertyName[1] := 'Volts';
    PropertyName[2] := 'angle';
    PropertyName[3] := 'frequency';
    PropertyName[4] := 'phases';
    PropertyName[5] := 'EN';
    PropertyName[6] := 'EE';
    PropertyName[7] := 'Lat1';
    PropertyName[8] := 'Lon1';
    PropertyName[9] := 'Lat2';
    PropertyName[10] := 'Lon2';

     // define Property help values
    PropertyHelp[1] := 'Voltage magnitude, in volts, of the GIC voltage induced across the associated line. ' +
        'When specified, induced voltage is assumed defined by Voltage and Angle properties. ' + CRLF + CRLF +
        'Specify this value' + CRLF + CRLF + 'OR' + CRLF + CRLF +
        'EN, EE, lat1, lon1, lat2, lon2. ' + CRLF + CRLF +
        'Not both!!  Last one entered will take precedence. ' +
        'Assumed identical in each phase of the Line object.';
    PropertyHelp[2] := 'Phase angle in degrees of first phase. Default=0.0.  See Voltage property';
    PropertyHelp[3] := 'Source frequency.  Defaults to  0.1 Hz. So GICSource=0 at power frequency.';
    PropertyHelp[4] := 'Number of phases.  Defaults to 3. All three phases are assumed in phase (zero sequence)';
    PropertyHelp[5] := 'Northward Electric field (V/km). If specified, Voltage and Angle are computed from EN, EE, lat and lon values.';
    PropertyHelp[6] := 'Eastward Electric field (V/km).  If specified, Voltage and Angle are computed from EN, EE, lat and lon values.';
    PropertyHelp[7] := 'Latitude of Bus1 of the line(degrees)';
    PropertyHelp[8] := 'Longitude of Bus1 of the line (degrees)';
    PropertyHelp[9] := 'Latitude of Bus2 of the line (degrees)';
    PropertyHelp[10] := 'Longitude of Bus2 of the line (degrees)';

    ActiveProperty := NumPropsThisClass;
    inherited DefineProperties;  // Add defs of inherited properties to bottom of list

     // Override help string
    PropertyHelp[NumPropsThisClass + 1] := 'Not used.';
    PropertyHelp[NumPropsThisClass + 2] := 'Not used.';

end;


//- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
function TGICsource.NewObject(const ObjName: String): Integer;
begin
    // Make a new voltage source and add it to GICsource class list
    with ActiveCircuit[ActiveActor] do
    begin
        ActiveCktElement := TGICSourceObj.Create(Self, ObjName);
        Result := AddObjectToList(ActiveDSSObject[ActiveActor]);
    end;
end;


//- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
function TGICsource.Edit(ActorID: Integer): Integer;
var
    ParamPointer: Integer;
    ParamName,
    Param: String;

begin
  // continue parsing with contents of Parser
    ActiveGICsourceObj := ElementList.Active;
    ActiveCircuit[ActorID].ActiveCktElement := ActiveGICsourceObj;

    Result := 0;

    with ActiveGICsourceObj do
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
                    DoSimpleMsg('Unknown parameter "' + ParamName + '" for Object "' + Class_Name + '.' + Name + '"', 330);
                1:
                    Volts := Parser[ActorID].DblValue;
                2:
                    Angle := Parser[ActorID].DblValue; // Ang
                3:
                    SrcFrequency := Parser[ActorID].DblValue; // freq   Usually 0.1 Hz
                4:
                begin
                    Nphases := Parser[ActorID].IntValue; // num phases
                    FphaseShift := 0.0;     // Zero Sequence
                    NConds := Fnphases;  // Force Reallocation of terminal info
                end;
                5:
                    ENorth := Parser[ActorID].DblValue;
                6:
                    EEast := Parser[ActorID].DblValue;
                7:
                    Lat1 := Parser[ActorID].DblValue;
                8:
                    Lon1 := Parser[ActorID].DblValue;
                9:
                    Lat2 := Parser[ActorID].DblValue;
                10:
                    Lon2 := Parser[ActorID].DblValue;

            else
                ClassEdit(ActiveGICsourceObj, ParamPointer - NumPropsThisClass);
            end;

            case ParamPointer of
                1, 2:
                    VoltsSpecified := TRUE;
                5..10:
                    VoltsSpecified := FALSE;
            end;

            ParamName := Parser[ActorID].NextParam;
            Param := Parser[ActorID].StrValue;
        end;

        RecalcElementData(ActorID);
        YPrimInvalid[ActorID] := TRUE;
    end;

end;

//----------------------------------------------------------------------------
function TGICsource.MakeLike(const OtherSource: String): Integer;
var
    OtherGICsource: TGICSourceObj;
    i: Integer;

begin
    Result := 0;
   {See if we can find this line name in the present collection}
    OtherGICsource := Find(OtherSource);
    if OtherGICsource <> NIL then
        with ActiveGICsourceObj do
        begin

            if Fnphases <> OtherGICsource.Fnphases then
            begin
                Nphases := OtherGICsource.Fnphases;
                NConds := Fnphases;  // Forces reallocation of terminal stuff

                Yorder := Fnconds * Fnterms;
                YPrimInvalid[ActiveActor] := TRUE;
            end;

            Volts := OtherGICsource.Volts;
            Angle := OtherGICsource.Angle;
            SrcFrequency := OtherGICsource.SrcFrequency;
            LineName := OtherGICsource.LineName;

            ENorth := OtherGICsource.ENorth;
            EEast := OtherGICsource.EEast;
            Lat1 := OtherGICsource.Lat1;
            Lon1 := OtherGICsource.Lon1;
            Lat2 := OtherGICsource.Lat2;
            Lon2 := OtherGICsource.Lon2;

            Bus2Defined := OtherGICsource.Bus2Defined;

            ClassMakeLike(OtherGICsource); // set spectrum,  base frequency

            Spectrum := '';  // Spectrum not allowed
            SpectrumObj := NIL;

            for i := 1 to ParentClass.NumProperties do
                PropertyValue[i] := OtherGICsource.PropertyValue[i];
            Result := 1;
        end
    else
        DoSimpleMsg('Error in GICsource MakeLike: "' + OtherSource + '" Not Found.', 332);

end;

//----------------------------------------------------------------------------
function TGICsource.Init(Handle: Integer; ActorID: Integer): Integer;

begin
    DoSimpleMsg('Need to implement TGICsource.Init', -1);
    Result := 0;
end;


//----------------------------------------------------------------------------
constructor TGICSourceObj.Create(ParClass: TDSSClass; const SourceName: String);
begin
    inherited create(ParClass);
    Name := LowerCase(SourceName);
    DSSObjType := ParClass.DSSClassType; // SOURCE + NON_PCPD_ELEM;  // Don't want this in PC Element List
    LineName := Name;  // GICsource name must be same as associated Line

    LineClass := DSSClassList[ActiveActor].Get(ClassNames[ActiveActor].Find('Line'));
    Nphases := 3;
    Fnconds := 3;
    Nterms := 2;   // 4/27/2018 made a 2-terminal I source

    Volts := 0.0;
    Angle := 0.0;

    ENorth := 1.0;
    EEast := 1.0;
    Lat1 := 33.613499;
    Lon1 := -87.373673;
    Lat2 := 33.547885;
    Lon2 := -86.074605;

    VoltsSpecified := FALSE;
    SrcFrequency := 0.1;   // this is the GIC source
    FphaseShift := 0.0;    // always zero sequence
    Bus2Defined := FALSE;
    InitPropertyValues(0);

    Yorder := Fnterms * Fnconds;
    // Don't do This here RecalcElementData;

    Spectrum := '';  // Spectrum not allowed
    SpectrumObj := NIL;

end;


//----------------------------------------------------------------------------
destructor TGICSourceObj.Destroy;
begin
    LineName := '';
    inherited Destroy;
end;

//=============================================================================
function TGICSourceObj.Compute_VLine: Double;
var
    Phi: Double;
    DeltaLat, DeltaLon: Double;

begin
    Phi := (Lat2 + Lat1) / 2.0 * (pi / 180.0);   // deg to radians
    DeltaLat := Lat1 - Lat2; // switched 11-20 to get pos GIC for pos ENorth
    DeltaLon := Lon1 - Lon2;
    VE := (111.133 - 0.56 * cos(2.0 * phi)) * DeltaLat * ENorth;
    VN := (111.5065 - 0.1872 * cos(2.0 * phi)) * Cos(phi) * DeltaLon * EEast;
    Result := VN + VE;
end;

//----------------------------------------------------------------------------
procedure TGICSourceObj.RecalcElementData(ActorID: Integer);

var
    GICBus: String;
    LineBus2: String;

begin

    pLineElem := LineClass.Find(LineName);

    if pLineElem = NIL then
    begin
        DoSimpleMsg('Line Object "' + LineName + '" associated with GICsource.' + Name + ' Not Found. Make sure you define it first.', 333);
    end
    else
    begin

        LineBus2 := pLineElem.GetBus(2);

         // If LineBus2 already begins with GIC, Don't insert the GIC Bis

        if CompareTextShortest('GIC_', LineBus2) <> 0 then
        begin
             // Define buses -- inserting a new bus GIC_{LineName}
            GICBus := 'GIC_' + LineName;
            SetBus(1, GICBus);
            SetBus(2, LineBus2);
             // Redefine the bus2 spec for LineElem
            Parser[ActorID].CmdString := 'Bus2=' + GICBus;
            pLineElem.Edit(ActorID);  // invoke the Line's editor to process Parser
        end;

        Bus2Defined := TRUE;
        if not VoltsSpecified then
            Volts := Compute_VLine;

    end;

    Reallocmem(InjCurrent, SizeOf(InjCurrent^[1]) * Yorder);

end;

//----------------------------------------------------------------------------
procedure TGICSourceObj.CalcYPrim(ActorID: Integer);

var
    Rs, Rm, Rzero: Double;
    i: Integer;
    Value: Complex;
    NegValue: Complex;

begin

 // Build only YPrim Series
    if YPrimInvalid[ActorID] then
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


     {
       Assume 0.0001 ohms resistance for GIC Source
     }
    Value := Cmplx(10000.0, 0.0);
    NegValue := Cnegate(Value);
    with YPrim_Series do
        for i := 1 to Fnphases do
        begin
            SetElement(i, i, Value);
            SetElement(i + Fnphases, i + Fnphases, Value);
            SetElemSym(i, i + Fnphases, NegValue);
        end;

    YPrim.Copyfrom(Yprim_Series);      // Initialize YPrim for series impedances
(* ****************************************************************************
    {
       Compute R0 of associated line
       Average diagonals of Z matrix and off-diagonals
       Z0=Zs + 2 Zm
     }

      IF abs(ActiveCircuit.Solution.Frequency - SrcFrequency) < EPSILON2 THEN Begin
          Rs := 0.0;  { zero the accumulators}
          Rm := 0.0;
          With pLineElem Do
          Begin
            for i := 1 to NPhases do Begin
              Rs := Rs + Z.GetElement(i,i).re;
              for j := i+1 to NPhases  do
                  Rm := Rm + Z.GetElement(i,j).re;
            End;
            Rs := Rs / NPhases;
            Rm := Rm / (NPhases * (NPhases-1)/2);
            Rzero :=  (Rs + 2.0*Rm) * Len / UnitsConvert;  // Total for entire line
            Gzero := 1.0/Rzero; // zero-sequence conductance of line

          End;
      End;
   ***********************************************************************************
 *)

     {Now Account for Open Conductors}
     {For any conductor that is open, zero out row and column}
    inherited CalcYPrim(ActorID);

    YPrimInvalid[ActorID] := FALSE;

end;

procedure TGICSourceObj.GetVterminalForSource(ActorID: Integer);

var
    Vmag: Double;
    i: Integer;

begin

    try
     // If the solution frequency not 0.1 Hz, source is shorted.
        with ActiveCircuit[ActorID].Solution do
        begin
            if abs(Frequency - SrcFrequency) < EPSILON2 then
                Vmag := Volts
            else
                Vmag := 0.0;
            for i := 1 to Fnphases do
            begin
                Vterminal^[i] := pdegtocomplex(Vmag, (Angle));   // all the same for zero sequence
                 // bottom part of the vector is zero
                VTerminal^[i + Fnphases] := CZERO;    // See comments in GetInjCurrents
            end;
        end;

    except
        DoSimpleMsg('Error computing current for GICsource.' + Name + '. Check specification. Aborting.', 334);
        if In_Redirect then
            Redirect_Abort := TRUE;
    end;

end;

function TGICSourceObj.InjCurrents(ActorID: Integer): Integer;

{Sum Currents directly into solution array}

begin

    GetInjCurrents(InjCurrent, ActorID);

    Result := inherited Injcurrents(ActorID);  // Adds into system array

end;

procedure TGICSourceObj.GetCurrents(Curr: pComplexArray; ActorID: Integer);

{Total currents into a device}

var
    i: Integer;

begin

    try
        with    ActiveCircuit[ActorID].Solution do
        begin

            for     i := 1 to Yorder do
                Vterminal^[i] := NodeV^[NodeRef^[i]];

            YPrim.MVMult(Curr, Vterminal);  // Current from Elements in System Y

            GetInjCurrents(ComplexBuffer, ActorID);  // Get present value of inj currents
      // Add Together  with yprim currents
            for i := 1 to Yorder do
                Curr^[i] := Csub(Curr^[i], ComplexBuffer^[i]);

        end;  {With}

    except
        On E: Exception do
            DoErrorMsg(('GetCurrents for GICsource Element: ' + Name + '.'), E.Message,
                'Inadequate storage allotted for circuit element?', 335);
    end;

end;

procedure TGICSourceObj.GetInjCurrents(Curr: pComplexArray; ActorID: Integer);

  { source injection currents given by this formula:
     _     _           _         _
     |Iinj1|           |GICLineVolts  |
     |     | = [Yprim] |         |
     |Iinj2|           | 0       |
     _     _           _         _
   }

begin
    GetVterminalForSource(ActorID);    // only at 0.1 Hz
    YPrim.MVMult(Curr, Vterminal);

    set_ITerminalUpdated(FALSE, ActorID);
end;

function TGICSourceObj.GetPropertyValue(Index: Integer): String;
begin
    begin
        case Index of
            1:
                Result := Format('%.8g', [Volts]);
            2:
                Result := Format('%.8g', [Angle]);
            3:
                Result := Format('%.8g', [SrcFrequency]);
        else
            Result := inherited GetPropertyValue(Index);
        end;
    end;
end;

procedure TGICSourceObj.DumpProperties(var F: TextFile; Complete: Boolean);

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

procedure TGICSourceObj.InitPropertyValues(ArrayOffset: Integer);
begin

    PropertyValue[1] := '0';
    PropertyValue[2] := '0';
    PropertyValue[3] := Format('%-.6g', [SrcFrequency]);
    PropertyValue[4] := '3';
    PropertyValue[5] := '1.0';
    PropertyValue[6] := '1.0';
    PropertyValue[7] := '33.613499';
    PropertyValue[8] := '-87.373673';
    PropertyValue[9] := '33.547885';
    PropertyValue[10] := '-86.074605';

    inherited  InitPropertyValues(NumPropsThisClass);

end;

procedure TGICSourceObj.MakePosSequence(ActorID: Integer);
begin

    if Fnphases > 1 then
    begin
        Parser[ActorID].CmdString := 'phases=1';
        Edit(ActorID);
    end;
    inherited;

end;


initialization

end.
