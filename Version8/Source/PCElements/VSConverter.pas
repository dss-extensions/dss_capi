unit VSConverter;

{
  ----------------------------------------------------------
  Copyright (c) 2013-2015, University of Pittsburgh
  All rights reserved.
  ----------------------------------------------------------
}
interface

uses
    Command,
    DSSClass,
    PCClass,
    Circuit,
    PCElement,
    UcMatrix,
    Ucomplex,
    ArrayDef,
    XYCurve;

type
    TVSConverter = class(TPCClass)
    PRIVATE
        procedure VscSetBus1(const S: String);
    PROTECTED
        procedure DefineProperties;
        function MakeLike(const VSCName: String): Integer; OVERRIDE;
    PUBLIC
        constructor Create;
        destructor Destroy; OVERRIDE;
        function Edit(ActorID: Integer): Integer; OVERRIDE;
        function Init(Handle: Integer; ActorID: Integer): Integer; OVERRIDE;
        function NewObject(const ObjName: String): Integer; OVERRIDE;
    end;

    TVSConverterObj = class(TPCElement)
    PRIVATE
        FkVac: Double;
        FkVdc: Double;
        FkW: Double;
        Fm: Double;
        Fd: Double;
        FRac: Double;
        FXac: Double;
        FrefVac: Double;
        FrefVdc: Double;
        FrefPac: Double;
        FrefQac: Double;
        FMinM: Double;
        FMaxM: Double;
        FMaxIac: Double;
        FMaxIdc: Double;
        Fmode: Integer;
        FNdc: Integer;
        LastCurrents: pComplexArray; // state memory for GetInjCurrents
    PUBLIC
        constructor Create(ParClass: TDSSClass; const FaultName: String);
        destructor Destroy; OVERRIDE;

        procedure RecalcElementData(ActorID: Integer); OVERRIDE;
        procedure CalcYPrim(ActorID: Integer); OVERRIDE;

      // these three functions make it a PCElement
        function InjCurrents(ActorID: Integer): Integer; OVERRIDE;
        procedure GetInjCurrents(Curr: pComplexArray; ActorID: Integer); OVERRIDE;
        procedure GetCurrents(Curr: pComplexArray; ActorID: Integer); OVERRIDE;

        procedure MakePosSequence(ActorID: Integer); OVERRIDE;

        function GetPropertyValue(Index: Integer): String; OVERRIDE;
        procedure InitPropertyValues(ArrayOffset: Integer); OVERRIDE;
        procedure DumpProperties(var F: TextFile; Complete: Boolean); OVERRIDE;
    end;

var
    ActiveVSConverterObj: TVSConverterObj;

implementation

uses
    ParserDel,
    MyDSSClassDefs,
    DSSClassDefs,
    DSSGlobals,
    Dynamics,
    Sysutils,
    MathUtil,
    Utilities,
    StrUtils;

const
    NumPropsthisclass = 19;
    VSC_FIXED = 0;
    VSC_PACVAC = 1;
    VSC_PACQAC = 2;
    VSC_VDCVAC = 3;
    VSC_VDCQAC = 4;

// =====================================================
// Class Methods
// =====================================================

constructor TVSConverter.Create;
begin
    inherited Create;
    Class_Name := 'VSConverter';
    DSSClassType := VS_CONVERTER + PC_ELEMENT;
    ActiveElement := 0;
    DefineProperties;
    CommandList := TCommandList.Create(Slice(PropertyName^, NumProperties));
    CommandList.Abbrev := TRUE;
end;

destructor TVSConverter.Destroy;
begin
    inherited Destroy;
end;

procedure TVSConverter.DefineProperties;
begin
    Numproperties := NumPropsThisClass;
    CountProperties;
    AllocatePropertyArrays;

    PropertyName^[1] := 'phases';
    PropertyName^[2] := 'Bus1';
    PropertyName^[3] := 'kVac';
    PropertyName^[4] := 'kVdc';
    PropertyName^[5] := 'kW';
    PropertyName^[6] := 'Ndc';
    PropertyName^[7] := 'Rac';
    PropertyName^[8] := 'Xac';
    PropertyName^[9] := 'm0';
    PropertyName^[10] := 'd0';
    PropertyName^[11] := 'Mmin';
    PropertyName^[12] := 'Mmax';
    PropertyName^[13] := 'Iacmax';
    PropertyName^[14] := 'Idcmax';
    PropertyName^[15] := 'Vacref';
    PropertyName^[16] := 'Pacref';
    PropertyName^[17] := 'Qacref';
    PropertyName^[18] := 'Vdcref';
    PropertyName^[19] := 'VscMode';

    PropertyHelp[1] := 'Number of AC plus DC conductors. Default is 4. AC phases numbered before DC conductors.';
    PropertyHelp[2] := 'Name of converter bus, containing both AC and DC conductors. Bus2 is always ground.';
    PropertyHelp[3] := 'Nominal AC line-neutral voltage in kV. Must be specified > 0.';
    PropertyHelp[4] := 'Nominal DC voltage in kV. Must be specified > 0.';
    PropertyHelp[5] := 'Nominal converter power in kW. Must be specified > 0.';
    PropertyHelp[6] := 'Number of DC conductors. Default is 1. DC conductors numbered after AC phases.';
    PropertyHelp[7] := 'AC resistance (ohms) for the converter transformer, plus any series reactors. Default is 0.' + CRLF +
        'Must be 0 for Vac control mode.';
    PropertyHelp[8] := 'AC reactance (ohms) for the converter transformer, plus any series reactors. Default is 0.' + CRLF +
        'Must be 0 for Vac control mode. Must be >0 for PacVac, PacQac or VacVdc control mode.';
    PropertyHelp[9] := 'Fixed or initial value of the modulation index. Default is 0.5.';
    PropertyHelp[10] := 'Fixed or initial value of the power angle in degrees. Default is 0.';
    PropertyHelp[11] := 'Minimum value of modulation index. Default is 0.1.';
    PropertyHelp[12] := 'Maximum value of modulation index. Default is 0.9.';
    PropertyHelp[13] := 'Maximum value of AC line current, per-unit of nominal. Default is 2.';
    PropertyHelp[14] := 'Maximum value of DC current, per-unit of nominal. Default is 2.';
    PropertyHelp[15] := 'Reference AC line-to-neutral voltage, RMS Volts. Default is 0.' + CRLF +
        'Applies to PacVac and VdcVac control modes, influencing m.';
    PropertyHelp[16] := 'Reference total AC real power, Watts. Default is 0.' + CRLF +
        'Applies to PacVac and PacQac control modes, influencing d.';
    PropertyHelp[17] := 'Reference total AC reactive power, Vars. Default is 0.' + CRLF +
        'Applies to PacQac and VdcQac control modes, influencing m.';
    PropertyHelp[18] := 'Reference DC voltage, Volts. Default is 0.' + CRLF +
        'Applies to VdcVac control mode, influencing d.';
    PropertyHelp[19] := 'Control Mode (Fixed|PacVac|PacQac|VdcVac|VdcQac). Default is Fixed.';

    ActiveProperty := NumPropsThisClass;
    inherited DefineProperties;
end;

function TVSConverter.NewObject(const ObjName: String): Integer;
begin
    with ActiveCircuit[ActiveActor] do
    begin
        ActiveCktElement := TVSConverterObj.Create(Self, ObjName);
        Result := AddObjectToList(ActiveDSSObject[ActiveActor]);
    end;
end;

procedure TVSConverter.VscSetBus1(const S: String);
var
    s2: String;
    i, dotpos: Integer;
begin
    with ActiveVSconverterObj do
    begin
        SetBus(1, S);
        dotpos := Pos('.', S);
        if dotpos > 0 then
            S2 := Copy(S, 1, dotpos - 1)
        else
            S2 := Copy(S, 1, Length(S));
        for i := 1 to Fnphases do
            S2 := S2 + '.0';
        SetBus(2, S2); // default setting for Bus2=Bus1.0.0.0.0
    end;
end;

function TVSConverter.Edit(ActorID: Integer): Integer;
var
    ParamPointer: Integer;
    ParamName: String;
    Param: String;
    Tok: String;
begin
    Result := 0;
    ActiveVSConverterObj := ElementList.Active;
    ActiveCircuit[ActorID].ActiveCktElement := ActiveVSConverterObj;  // use property to set this value

    with ActiveVSConverterObj do
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
                    DoSimpleMsg('Unknown parameter "' + ParamName + '" for Object "' + Class_Name + '.' + Name + '"', 350);
                1:
                    if Fnphases <> Parser[ActorID].IntValue then
                    begin
                        Nphases := Parser[ActorID].IntValue;
                        NConds := Fnphases;
                        ActiveCircuit[ActorID].BusNameRedefined := TRUE;
                    end;
                2:
                    VscSetBus1(param);
                3:
                    FkVac := Parser[ActorID].DblValue;
                4:
                    FkVdc := Parser[ActorID].DblValue;
                5:
                    FkW := Parser[ActorID].DblValue;
                6:
                    FNdc := Parser[ActorID].IntValue;
                7:
                    FRac := Parser[ActorID].DblValue;
                8:
                    FXac := Parser[ActorID].DblValue;
                9:
                    Fm := Parser[ActorID].DblValue;
                10:
                    Fd := Parser[ActorID].DblValue;
                11:
                    FMinM := Parser[ActorID].DblValue;
                12:
                    FMaxM := Parser[ActorID].DblValue;
                13:
                    FMaxIac := Parser[ActorID].DblValue;
                14:
                    FMaxIdc := Parser[ActorID].DblValue;
                15:
                    FRefVac := Parser[ActorID].DblValue;
                16:
                    FRefPac := Parser[ActorID].DblValue;
                17:
                    FRefQac := Parser[ActorID].DblValue;
                18:
                    FRefVdc := Parser[ActorID].DblValue;
                19:
                begin
                    Tok := Uppercase(LeftStr(param, 4));
                    if CompareStr(LeftStr(Tok, 1), 'F') = 0 then
                        Fmode := VSC_FIXED
                    else
                    if CompareStr(Tok, 'PACV') = 0 then
                        Fmode := VSC_PACVAC
                    else
                    if CompareStr(Tok, 'PACQ') = 0 then
                        Fmode := VSC_PACQAC
                    else
                    if CompareStr(Tok, 'VDCV') = 0 then
                        Fmode := VSC_VDCVAC
                    else
                    if CompareStr(Tok, 'VDCQ') = 0 then
                        Fmode := VSC_VDCQAC
                    else
                        Fmode := VSC_FIXED
                end;
            else
                ClassEdit(ActiveVSConverterObj, ParamPointer - NumPropsThisClass)
            end;

            case ParamPointer of
                1..16:
                    YprimInvalid[ActorID] := TRUE;
            else
            end;

            ParamName := Parser[ActorID].NextParam;
            Param := Parser[ActorID].StrValue;
        end;
        RecalcElementData(ActorID);
    end;
end;

function TVSConverter.MakeLike(const VSCName: String): Integer;
var
    OtherVSC: TVSConverterObj;
    i: Integer;
begin
    Result := 0;
    OtherVSC := Find(VSCName);
    if OtherVSC <> NIL then
        with ActiveVSConverterObj do
        begin
            if Fnphases <> OtherVSC.Fnphases then
            begin
                Fnphases := OtherVSC.Fnphases;
                FnTerms := OtherVSC.FnTerms;
                NConds := Fnphases;
                FNdc := OtherVSC.FNdc;
                Yorder := FnConds * FnTerms;
                YprimInvalid[ActiveActor] := TRUE;
                FkVac := OtherVSC.FkVac;
                FkVdc := OtherVSC.FkVdc;
                FkW := OtherVSC.FkW;
                FRac := OtherVSC.FRac;
                FXac := OtherVSC.FXac;
                Fm := OtherVSC.Fm;
                Fd := OtherVSC.Fd;
                FMinM := OtherVSC.FMinM;
                FMaxM := OtherVSC.FMaxM;
                FMaxIac := OtherVSC.FMaxIac;
                FMaxIdc := OtherVSC.FMaxIdc;
                FRefVac := OtherVSC.FRefVac;
                FRefPac := OtherVSC.FRefPac;
                FRefQac := OtherVSC.FRefQac;
                FRefVdc := OtherVSC.FRefVdc;
                Fmode := OtherVSC.Fmode;
            end;
            BaseFrequency := OtherVSC.BaseFrequency;
            ClassMakeLike(OtherVSC);
            for i := 1 to ParentClass.NumProperties do
                PropertyValue[i] := OtherVSC.PropertyValue[i];
            Result := 1;
        end // with
    else
        DoSimpleMsg('Error in VSConverter MakeLike: "' + VSCName + '" Not Found.', 351);
end;

function TVSConverter.Init(Handle: Integer; ActorID: Integer): Integer;
begin
    DoSimpleMsg('Need to implement TVSConverter.Init', -1);
    Result := 0;
end;

//- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
//      Object Methods
//- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

constructor TVSConverterObj.Create(ParClass: TDSSClass; const FaultName: String);
begin
    inherited Create(ParClass);
    DSSObjType := ParClass.DSSClassType;
    Name := LowerCase(FaultName);

    LastCurrents := NIL;

  // typically the first 3 "phases" are AC, and the last one is DC
    NPhases := 4;
    Fnconds := 4;
    Nterms := 2; // two-terminal device, like the voltage source
    FNdc := 1;

    FkVac := 1.0;
    FkVdc := 1.0;
    FkW := 1.0;

    Fmode := VSC_FIXED;
    FRac := EPSILON;
    FXac := 0.0;
    Fm := 0.5;
    Fd := 0.0;
    FrefVac := 0.0;
    FrefPac := 0.0;
    FrefQac := 0.0;
    FrefVdc := 0.0;
    FminM := 0.1;
    FmaxM := 0.9;
    FmaxIac := 2.0;
    FmaxIdc := 2.0;

    InitPropertyValues(0);
    Yorder := Fnterms * Fnconds;
    RecalcElementData(ActiveActor);
end;

destructor TVSConverterObj.Destroy;
begin
    Reallocmem(LastCurrents, 0);
    inherited destroy;
end;

procedure TVSConverterObj.RecalcElementData(ActorID: Integer);
var
    i: Integer;
begin
    if (FRac = 0.0) and (FXac = 0.0) then
        FRac := EPSILON;
    Reallocmem(InjCurrent, SizeOf(InjCurrent^[1]) * Yorder);
    Reallocmem(LastCurrents, SizeOf(LastCurrents^[1]) * Yorder);
    for i := 1 to Yorder do
        LastCurrents^[i] := CZERO;
end;

procedure TVSConverterObj.CalcYPrim(ActorID: Integer);
var
    Value, Value2: Complex;
    FreqMultiplier: Double;
    i: Integer;
begin
// build YPrim_Series non-zero for just the AC phases, and it will be diagonal
    if YprimInvalid[ActorID] then
    begin
        if YPrim_Series <> NIL then
            YPrim_Series.Free;
        YPrim_Series := TCmatrix.CreateMatrix(Yorder);
        if YPrim <> NIL then
            YPrim.Free;
        YPrim := TcMatrix.CreateMatrix(Yorder);
    end
    else
    begin
        YPrim_Series.Clear;
        Yprim.Clear;
    end;

  // calculate the AC voltage source admittance
    FYprimFreq := ActiveCircuit[ActorID].Solution.Frequency;
    FreqMultiplier := FYprimFreq / BaseFrequency;
    Value.re := FRac;
    Value.im := FXac * FreqMultiplier;
    Value := cinv(Value);
    Value2 := cnegate(Value);

    with YPrim_Series do
    begin
        for i := 1 to (Fnphases - FNdc) do
        begin
            SetElement(i, i, Value);
            SetElement(i + Fnphases, i + Fnphases, Value);
            SetElemSym(i, i + Fnphases, Value2);
        end;
    end;
    YPrim.CopyFrom(YPrim_Series);
    inherited CalcYPrim(ActorID); // may open some conductors
    YprimInvalid[ActorID] := FALSE;
end;

function TVSConverterObj.InjCurrents(ActorID: Integer): Integer;
begin
    GetInjCurrents(InjCurrent, ActorID);
    Result := inherited InjCurrents(ActorID); // Add into system array
end;

procedure TVSConverterObj.GetCurrents(Curr: pComplexArray; ActorID: Integer);
var
    i: Integer;
begin
    try
        with ActiveCircuit[ActorID].Solution do
        begin
            ComputeVTerminal(ActorID);
      // add the injection currents from both AC and DC nodes, to the
      // currents from Yprim elements, which should be zero at the DC nodes
            YPrim.MVMult(Curr, Vterminal);
            GetInjCurrents(ComplexBuffer, ActorID);
            for i := 1 to Yorder do
            begin
                Curr^[i] := Csub(Curr^[i], ComplexBuffer^[i]);
                LastCurrents^[i] := Curr^[i];
            end;
        end;
    except
        on E: Exception do
            DoErrorMsg(('GetCurrents for Element: ' + Name + '.'), E.Message,
                'Inadequate storage allotted for circuit element.', 327);
    end;
end;

procedure TVSConverterObj.GetInjCurrents(Curr: pComplexArray; ActorID: Integer);
var
    Vmag: Complex;
    Vdc, Sphase, Stotal: Complex;
    Pac, Deg, Idc, Idclim, Iaclim, Itmag: Double;
    i, Nac: Integer;
begin

   { AC Voltage source injection currents given by this formula:
     _     _           _         _
     |Iinj1|           |Vsource  |
     |     | = [Yprim] |         |
     |Iinj2|           | 0       |
     _     _           _         _
   }

    Nac := FNphases - FNdc;
    Idclim := FMaxIdc * Fkw / FkVdc;
    Iaclim := FMaxIac * Fkw / FkVac / Nac;

  // obtain the terminal control quantities
    ComputeVterminal(ActorID);
    set_ITerminalUpdated(FALSE, ActorID);
    GetTerminalCurrents(ITerminal, ActorID);
//  for i := 1 to Nac do begin
//    Itmag := cabs(Iterminal^[i]);
//    if Itmag > Iaclim then begin
//      Itmag := Iaclim / Itmag;
//      Iterminal^[i].re := Iterminal^[i].re * Itmag;
//      Iterminal^[i].im := Iterminal^[i].im * Itmag;
//    end;
//  end;

  // do the AC voltage source injection - dependent voltage sources kept in ComplexBuffer
    Vdc := Vterminal^[FNphases];
    if (Vdc.re = 0.0) and (Vdc.im = 0.0) then
        Vdc.re := 1000.0 * FkVdc;
    Vmag := CMulReal(Vdc, 0.353553 * Fm);
    RotatePhasorDeg(Vmag, 1.0, Fd);
    ComplexBuffer^[1] := Vmag;
    Deg := -360.0 / Nac;
    for i := 2 to Nac do
    begin
        RotatePhasorDeg(Vmag, 1.0, Deg);
        ComplexBuffer^[i] := Vmag;
    end;
    ComplexBuffer^[FNPhases] := CZERO;
    YPrim.MVMult(Curr, ComplexBuffer);

  // calculate the converter AC power, exclusive of the losses, using LastCurrents
    Stotal.re := 0.0;
    Stotal.im := 0.0;
    for i := 1 to Nac do
    begin
//    Sphase := Cmul (ComplexBuffer^[i], Conjg(LastCurrents^[i]));
        Sphase := Cmul(ComplexBuffer^[i], Conjg(Iterminal^[i]));
        Stotal := Cadd(Stotal, Sphase);
    end;
    Pac := Stotal.re;
//  Qac := Stotal.im;
    if (Pac = 0.0) then
        Pac := 1000.0 * FkW;

  // DC current source injection
    Idc := Pac / Cabs(Vdc);
    if Idc > Idclim then
        Idc := Idclim;
    if Idc < -Idclim then
        Idc := -Idclim;

    Curr^[FNphases] := cmplx(Idc, 0.0);
    Curr^[2 * FNphases] := cmplx(-Idc, 0.0);
    set_ITerminalUpdated(FALSE, ActorID);
end;

procedure TVSConverterObj.DumpProperties(var F: TextFile; Complete: Boolean);
var
    i: Integer;
begin
    inherited DumpProperties(F, complete);
    with ParentClass do
    begin
        Writeln(F, '~ ', PropertyName^[1], '=', Fnphases: 0);
        Writeln(F, '~ ', PropertyName^[2], '=', firstbus);
        Writeln(F, '~ ', PropertyName^[3], '=', FkVac: 8: 1);
        Writeln(F, '~ ', PropertyName^[4], '=', FkVdc: 8: 1);
        Writeln(F, '~ ', PropertyName^[5], '=', FkW: 8: 1);
        Writeln(F, '~ ', PropertyName^[6], '=', FNdc: 0);
        Writeln(F, '~ ', PropertyName^[7], '=', FRac: 0: 4);
        Writeln(F, '~ ', PropertyName^[8], '=', FXac: 0: 4);
        Writeln(F, '~ ', PropertyName^[9], '=', Fm: 0: 4);
        Writeln(F, '~ ', PropertyName^[10], '=', Fd: 0: 4);
        Writeln(F, '~ ', PropertyName^[11], '=', FMinM: 0: 4);
        Writeln(F, '~ ', PropertyName^[12], '=', FMaxM: 0: 4);
        Writeln(F, '~ ', PropertyName^[13], '=', FMaxIac: 0: 4);
        Writeln(F, '~ ', PropertyName^[14], '=', FMaxIdc: 0: 4);
        Writeln(F, '~ ', PropertyName^[15], '=', FRefVac: 0: 4);
        Writeln(F, '~ ', PropertyName^[16], '=', FRefPac: 0: 4);
        Writeln(F, '~ ', PropertyName^[17], '=', FRefQac: 0: 4);
        Writeln(F, '~ ', PropertyName^[18], '=', FRefVdc: 0: 4);
        case Fmode of
            VSC_FIXED:
                Writeln(F, '~ ', PropertyName^[19], '= Fixed');
            VSC_PACVAC:
                Writeln(F, '~ ', PropertyName^[19], '= PacVac');
            VSC_PACQAC:
                Writeln(F, '~ ', PropertyName^[19], '= PacQac');
            VSC_VDCVAC:
                Writeln(F, '~ ', PropertyName^[19], '= VdcVac');
            VSC_VDCQAC:
                Writeln(F, '~ ', PropertyName^[19], '= VdcQac');
        end;
        for i := NumPropsthisClass + 1 to NumProperties do
        begin
            Writeln(F, '~ ', PropertyName^[i], '=', PropertyValue[i]);
        end;
    end;
end;

procedure TVSConverterObj.InitPropertyValues(ArrayOffset: Integer);
begin
    PropertyValue[1] := '4';
    PropertyValue[2] := getbus(1);
    PropertyValue[3] := '1';
    PropertyValue[4] := '1';
    PropertyValue[5] := '1';
    PropertyValue[6] := '1';
    PropertyValue[7] := '0';
    PropertyValue[8] := '0';
    PropertyValue[9] := '0.5';
    PropertyValue[10] := '0';
    PropertyValue[11] := '0.1';
    PropertyValue[12] := '0.9';
    PropertyValue[13] := '0';
    PropertyValue[14] := '0';
    PropertyValue[15] := '0';
    PropertyValue[16] := '0';
    PropertyValue[17] := '0';
    PropertyValue[18] := '0';
    PropertyValue[19] := 'FIXED';

    inherited  InitPropertyValues(NumPropsThisClass);
end;

function TVSConverterObj.GetPropertyValue(Index: Integer): String;
begin
    case Index of
        1:
            Result := Format('%d', [Nphases]);
        2:
            Result := GetBus(1);
        3:
            Result := Format('%.8g', [FkVac]);
        4:
            Result := Format('%.8g', [FkVdc]);
        5:
            Result := Format('%.8g', [FkW]);
        6:
            Result := Format('%d', [FNdc]);
        7:
            Result := Format('%.8g', [FRac]);
        8:
            Result := Format('%.8g', [FXac]);
        9:
            Result := Format('%.8g', [Fm]);
        10:
            Result := Format('%.8g', [Fd]);
        11:
            Result := Format('%.8g', [FMinM]);
        12:
            Result := Format('%.8g', [FMaxM]);
        13:
            Result := Format('%.8g', [FMaxIac]);
        14:
            Result := Format('%.8g', [FMaxIdc]);
        15:
            Result := Format('%.8g', [FRefVac]);
        16:
            Result := Format('%.8g', [FRefPac]);
        17:
            Result := Format('%.8g', [FRefQac]);
        18:
            Result := Format('%.8g', [FRefVdc]);
        19:
            case Fmode of
                VSC_FIXED:
                    Result := 'Fixed';
                VSC_PACVAC:
                    Result := 'PacVac';
                VSC_PACQAC:
                    Result := 'PacQac';
                VSC_VDCVAC:
                    Result := 'VdcVac';
                VSC_VDCQAC:
                    Result := 'VdcQac';
            end
    else
        Result := inherited GetPropertyValue(Index);
    end;
end;

procedure TVSConverterObj.MakePosSequence(ActorID: Integer);
begin
    if FnPhases <> 2 then
    begin
        Parser[ActorID].CmdString := 'Phases=2';
        Edit(ActorID);
        Parser[ActorID].CmdString := 'Ndc=1';
        Edit(ActorID);
    end;
    inherited;
end;

end.
