unit Equivalent;

 {
  ----------------------------------------------------------
  Copyright (c) 2008-2015, Electric Power Research Institute, Inc.
  All rights reserved.
  ----------------------------------------------------------
}

{Multi terminal, multi-phase Short Circuit (Thevinen) Equivalent

  Enter positive and zero short circuit impedance matrices
  And Voltage behind the equivalent
}

interface

uses
    DSSClass,
    PCClass,
    PCElement,
    ucmatrix,
    ucomplex,
    Spectrum,
    Arraydef;

type
// = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = =
    TEquivalent = class(TPCClass)
    PRIVATE
    PROTECTED
        procedure DefineProperties;
        function MakeLike(const OtherSource: String): Integer; OVERRIDE;
        procedure InterpretAllBuses(const S: String);
    PUBLIC
        constructor Create;
        destructor Destroy; OVERRIDE;

        function Edit: Integer; OVERRIDE;
        function Init(Handle: Integer): Integer; OVERRIDE;
        function NewObject(const ObjName: String): Integer; OVERRIDE;
    end;

// = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = =
    TEquivalentObj = class(TPCElement)
    PRIVATE

        kVBase: Double;

        VMag: Double;
        PerUnit: Double;
        Angle: Double;
        EquivFrequency: Double;

        R1, X1, R0, X0: pdoubleArray;

        NeedToDoRecalc: Boolean;

        procedure GetVterminalForSource;
        procedure ReallocRX;
        procedure ParseDblMatrix(Mat: pDoubleArray);
        function DoTerminalsDef(const N: Integer): Integer;

    PUBLIC
        Z: TCmatrix;  // Base Frequency Series Z matrix
        Zinv: TCMatrix;

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
    ActiveEquivalentObj: TEquivalentObj;

// = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = =
implementation


uses
    ParserDel,
    Circuit,
    DSSClassDefs,
    DSSGlobals,
    Utilities,
    Sysutils,
    Command;

const
    NumPropsThisClass = 16;

//- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
constructor TEquivalent.Create;  // Creates superstructure for all Line objects
begin

    inherited Create;
    Class_Name := 'Equivalent';
    DSSClassType := SOURCE + NON_PCPD_ELEM;  // Don't want this in PC Element List

    ActiveElement := 0;

    DefineProperties;

    CommandList := TCommandList.Create(Slice(PropertyName^, NumProperties));
    CommandList.Abbrev := TRUE;

end;

//- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
destructor TEquivalent.Destroy;

begin
    // ElementList and  CommandList freed in inherited destroy
    inherited Destroy;

end;

//- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
procedure TEquivalent.DefineProperties;
begin

    Numproperties := NumPropsThisClass;
    CountProperties;   // Get inherited property count
    AllocatePropertyArrays;

     // Define Property names
    PropertyName[1] := 'terminals';
    PropertyName[2] := 'buses';
    PropertyName[3] := 'basekv';
    PropertyName[4] := 'pu';
    PropertyName[5] := 'angle';
    PropertyName[6] := 'frequency';
    PropertyName[7] := 'phases';
    PropertyName[8] := 'R1';
    PropertyName[9] := 'X1';
    PropertyName[10] := 'R0';
    PropertyName[11] := 'X0';

     // define Property help values
    PropertyHelp[1] := 'Number of terminals.  Default =1. Set this BEFORE defining matrices.';
    PropertyHelp[2] := 'Array of Bus Names to which equivalent source is connected.' + CRLF + 'buses=(b1 b2 b3)';
    PropertyHelp[3] := 'Base Source kV, usually L-L unless you are making a positive-sequence model' +
        'in which case, it will be L-N.';
    PropertyHelp[4] := 'Per unit of the base voltage that the source is actually operating at.' + CRLF +
        '"pu=1.05"';
    PropertyHelp[5] := 'Phase angle in degrees of first phase: e.g.,Angle=10.3';
    PropertyHelp[6] := 'Source frequency.  Defaults to  60 Hz.';
    PropertyHelp[7] := 'Number of phases.  Defaults to 3.';
    PropertyHelp[8] := 'Positive-sequence resistance matrix, lower triangle.';
    PropertyHelp[9] := 'Positive-sequence reactance matrix, lower triangle.';
    PropertyHelp[10] := 'Zero-sequence resistance matrix, lower triangle.';
    PropertyHelp[11] := 'Zero-sequence reactance matrix, lower triangle.';

    ActiveProperty := NumPropsThisClass;
    inherited DefineProperties;  // Add defs of inherited properties to bottom of list

     // Override help string
    PropertyHelp[NumPropsThisClass + 1] := 'Name of harmonic spectrum for this source.  Default is "defaultvsource", which is defined when the DSS starts.';

end;


//- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
function TEquivalent.NewObject(const ObjName: String): Integer;
begin
    // Make a new voltage source and add it to Equivalent class list
    with ActiveCircuit do
    begin
        ActiveCktElement := TEquivalentObj.Create(Self, ObjName);
        Result := AddObjectToList(ActiveDSSObject);
    end;
end;


//- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
function TEquivalent.Edit: Integer;
var
    ParamPointer: Integer;
    ParamName,
    Param: String;

begin
  // continue parsing with contents of Parser
    ActiveEquivalentObj := ElementList.Active;
    ActiveCircuit.ActiveCktElement := ActiveEquivalentObj;

    Result := 0;

    with ActiveEquivalentObj do
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
                    DoSimpleMsg('Unknown parameter "' + ParamName + '" for Object "Equivalent.' + Name + '"', 800);
                1:
                    Nterms := DoTerminalsDef(Parser.IntValue);  // This will allocate a bunch of stuff
                2:
                    InterpretAllBuses(param);
                3:
                    kVBase := Parser.DblValue; // basekv
                4:
                    PerUnit := Parser.DblValue; // pu
                5:
                    Angle := Parser.DblValue; // Ang
                6:
                    EquivFrequency := Parser.DblValue; // freq
                7:
                begin
                    Nphases := Parser.Intvalue; // num phases
                    NConds := Fnphases;  // Force Reallocation of terminal info
                end;
                8:
                    ParseDblMatrix(R1);
                9:
                    ParseDblMatrix(X1);
                10:
                    ParseDblMatrix(R0);
                11:
                    ParseDblMatrix(X0);
            else
                ClassEdit(ActiveEquivalentObj, ParamPointer - NumPropsThisClass)
            end;

            case ParamPointer of
                1, 8..11:
                    NeedToDoRecalc := TRUE;
            else

            end;

            ParamName := Parser.NextParam;
            Param := Parser.StrValue;
        end;

    // RecalcElementData;
        YPrimInvalid := TRUE;
    end;

end;

//----------------------------------------------------------------------------
function TEquivalent.MakeLike(const OtherSource: String): Integer;
var
    OtherEquivalent: TEquivalentObj;
    i: Integer;

begin
    Result := 0;
   {See if we can find this line name in the present collection}
    OtherEquivalent := Find(OtherSource);
    if OtherEquivalent <> NIL then
        with ActiveEquivalentObj do
        begin

            if (Fnphases <> OtherEquivalent.Fnphases) or
                (FNterms <> OtherEquivalent.FNterms) then
            begin

                Nterms := DoTerminalsDef(OtherEquivalent.FNTerms);
                Nphases := OtherEquivalent.Fnphases;
                NConds := Fnphases;  // Forces reallocation of terminal stuff

                Yorder := Fnconds * Fnterms;
                YPrimInvalid := TRUE;

                for i := 1 to FnTerms do
                    R1^[i] := OtherEquivalent.R1^[i];
                for i := 1 to FnTerms do
                    R0^[i] := OtherEquivalent.R0^[i];

                for i := 1 to FnTerms do
                    X1^[i] := OtherEquivalent.X1^[i];
                for i := 1 to FnTerms do
                    X0^[i] := OtherEquivalent.X0^[i];

                if Z <> NIL then
                    Z.Free;
                if Zinv <> NIL then
                    Zinv.Free;

                Z := TCmatrix.CreateMatrix(Fnphases);
                Zinv := TCMatrix.CreateMatrix(Fnphases);
            end;

            Z.CopyFrom(OtherEquivalent.Z);
       // Zinv.CopyFrom(OtherLine.Zinv);
            VMag := OtherEquivalent.Vmag;
            kVBase := OtherEquivalent.kVBase;
            PerUnit := OtherEquivalent.PerUnit;
            Angle := OtherEquivalent.Angle;
            EquivFrequency := OtherEquivalent.EquivFrequency;

            ClassMakeLike(OtherEquivalent);

            for i := 1 to ParentClass.NumProperties do
                FPropertyValue[i] := OtherEquivalent.FPropertyValue[i];
            Result := 1;
        end
    else
        DoSimpleMsg('Error in Equivalent MakeLike: "' + OtherSource + '" Not Found.', 801);

end;

//----------------------------------------------------------------------------
function TEquivalent.Init(Handle: Integer): Integer;

begin
    DoSimpleMsg('Need to implement TEquivalent.Init', -1);
    Result := 0;
end;

//----------------------------------------------------------------------------
constructor TEquivalentObj.Create(ParClass: TDSSClass; const SourceName: String);
begin
    inherited create(ParClass);
    Name := LowerCase(SourceName);
    DSSObjType := ParClass.DSSClassType; //SOURCE + NON_PCPD_ELEM;  // Don't want this in PC Element List

    Nphases := 3;
    Fnconds := 3;
    Nterms := 1;
    Z := NIL;
    Zinv := NIL;
     {Basefrequency := 60.0;} // set in base class

    R1 := NIL;
    X1 := NIL;
    R0 := NIL;
    X0 := NIL;

    ReallocRX;

    R1^[1] := 1.65;
    X1^[1] := 6.6;
    R0^[1] := 1.9;
    X0^[1] := 5.7;

    kVBase := 115.0;
    PerUnit := 1.0;
    EquivFrequency := BaseFrequency;
    Angle := 0.0;

    Spectrum := 'defaultvsource';

    InitPropertyValues(0);

    Yorder := Fnterms * Fnconds;
    RecalcElementData;

end;


//----------------------------------------------------------------------------
destructor TEquivalentObj.Destroy;
begin
    Z.Free;
    Zinv.Free;

    Reallocmem(R1, 0);
    Reallocmem(R0, 0);
    Reallocmem(X1, 0);
    Reallocmem(X0, 0);

    inherited Destroy;
end;

//----------------------------------------------------------------------------
procedure TEquivalentObj.RecalcElementData;
var
    Zs, Zm: Complex;
    i, j: Integer;
    ii, jj: Integer;
    ioffset, joffset, indx: Integer;

    function idx(a, b: Integer): Integer;
    begin
        Result := (b - 1) * FNterms + a;
    end;

begin
    if Z <> NIL then
        Z.Free;
    if Zinv <> NIL then
        Zinv.Free;

    // For a Source, nphases = ncond, for now
    Z := TCmatrix.CreateMatrix(Fnphases * FNterms);
    Zinv := TCMatrix.CreateMatrix(Fnphases * FNterms);

     // Build Z matrix for all phases
    for i := 1 to FNterms do
        for j := 1 to FNterms do
        begin
            indx := idx(i, j);
            Zs := CdivReal(cmplx(2.0 * R1^[indx] + R0^[indx], 2.0 * X1^[indx] + X0^[indx]), 3.0);
            Zm := CdivReal(cmplx(R0^[indx] - R1^[indx], X0^[indx] - X1^[indx]), 3.0);

            iOffset := (i - 1) * Fnphases;
            jOffset := (j - 1) * Fnphases;

            for ii := 1 to Fnphases do
                for jj := 1 to ii do
                begin
                    if ii = jj then
                        Z.SetElement(ii + ioffset, jj + joffset, Zs)
                    else
                    begin
                        Z.SetElement(ii + ioffset, jj + joffset, Zm);
                        Z.SetElement(jj + ioffset, ii + joffset, Zm);  // set other offdiagonal in this submatrix
                    end
                end;

        end;

   // Voltage source properties
    case Fnphases of
        1:
            Vmag := kVBase * PerUnit * 1000.0;
    else
        Vmag := kVBase * PerUnit * 1000.0 / 2.0 / Sin((180.0 / Fnphases) * PI / 180.0);
    end;

    SpectrumObj := SpectrumClass.Find(Spectrum);
    if SpectrumObj = NIL then
    begin
        DoSimpleMsg('Spectrum Object "' + Spectrum + '" for Device Equivalent.' + Name + ' Not Found.', 802);
    end;

    Reallocmem(InjCurrent, SizeOf(InjCurrent^[1]) * Yorder);

    NeedToDoRecalc := FALSE;

end;

//----------------------------------------------------------------------------
procedure TEquivalentObj.CalcYPrim;

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

    if NeedToDoRecalc then
        RecalcElementData;

    FYprimFreq := ActiveCircuit.Solution.Frequency;
    FreqMultiplier := FYprimFreq / BaseFrequency;

     { Put in Series RL matrix Adjusted for frequency }
    for i := 1 to Yorder do
    begin
        for j := 1 to Yorder do
        begin
            Value := Z.GetElement(i, j);
            Value.im := Value.im * FreqMultiplier;  {Modify from base freq}
            Zinv.SetElement(i, j, value);
        end;
    end;

    Zinv.Invert;  {Invert in place}

    if Zinv.InvertError > 0 then
    begin       {If error, put in Large series conductance}
        DoErrorMsg('TEquivalentObj.CalcYPrim', 'Matrix Inversion Error for Equivalent "' + Name + '"',
            'Invalid impedance specified. Replaced with small resistance.', 803);
        Zinv.Clear;
        for i := 1 to Fnphases do
            Zinv.SetElement(i, i, Cmplx(1.0 / EPSILON, 0.0));
    end;


    YPrim_Series.CopyFrom(Zinv);

    YPrim.CopyFrom(YPrim_Series);

     {Now Account for Open Conductors}
     {For any conductor that is open, zero out row and column}
    inherited CalcYPrim;

    YPrimInvalid := FALSE;

end;

//====================================
procedure TEquivalentObj.GetVterminalForSource;

var
    i: Integer;
    Vharm: Complex;
    EquivHarm: Double;

begin

    try

  {This formulation will theoretically handle voltage sources of any number of phases assuming they are
   equally displaced in time.}

        case Fnphases of
            1:
                Vmag := kVBase * PerUnit * 1000.0;
        else
            Vmag := kVBase * PerUnit * 1000.0 / 2.0 / Sin((180.0 / Fnphases) * PI / 180.0);
        end;

        with ActiveCircuit.Solution do
            if IsHarmonicModel then
            begin
                EquivHarm := Frequency / EquivFrequency;
                Vharm := CMulReal(SpectrumObj.GetMult(EquivHarm), Vmag);  // Base voltage for this harmonic
                RotatePhasorDeg(Vharm, EquivHarm, Angle);  // Rotate for phase 1 shift
                for i := 1 to Fnphases do
                begin
                    Vterminal^[i] := Vharm;
                    if (i < Fnphases) then
                        RotatePhasorDeg(Vharm, EquivHarm, -360.0 / Fnphases);
                end;

            end
            else
            begin

                for i := 1 to Fnphases do
                begin
                    Vterminal^[i] := pdegtocomplex(Vmag, (360.0 + Angle - (i - 1) * 360.0 / Fnphases));
                end;

            end;

    except
        DoSimpleMsg('Error computing Voltages for Equivalent.' + Name + '. Check specification. Aborting.', 804);
        if In_Redirect then
            Redirect_Abort := TRUE;
    end;

end;

//====================================

function TEquivalentObj.InjCurrents: Integer;

begin

    GetInjCurrents(InjCurrent);

{This is source injection}

    Result := inherited InjCurrents; // Add into system array

end;

//====================================
procedure TEquivalentObj.GetCurrents(Curr: pComplexArray);

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

            YPrim.MVMult(Curr, Vterminal);

            GetInjCurrents(ComplexBuffer);  // Get present value of inj currents
      // Add Together  with yprim currents
            for i := 1 to Yorder do
                Curr^[i] := Csub(Curr^[i], ComplexBuffer^[i]);

        end;  {With}
    except
        On E: Exception do
            DoErrorMsg(('GetCurrents for Element: ' + Name + '.'), E.Message,
                'Inadequate storage allotted for circuit element.', 805);
    end;

end;


//====================================
procedure TEquivalentObj.GetInjCurrents(Curr: pComplexArray);

begin

    GetVterminalForSource;
    YPrim.MVMult(Curr, Vterminal); {I = Y V}

    ITerminalUpdated := FALSE;

end;

procedure TEquivalentObj.DumpProperties(var F: TextFile; Complete: Boolean);

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
                Write(F, C.re: 0: 3, ' + j', C.im: 0: 3);
            end;
            Writeln(F);
        end;
    end;

end;


procedure TEquivalentObj.InitPropertyValues(ArrayOffset: Integer);
begin
{

    PropertyName[1] := 'terminals';
     PropertyName[2] := 'buses';
     PropertyName[3] := 'basekv';
     PropertyName[4] := 'pu';
     PropertyName[5] := 'angle';
     PropertyName[6] := 'frequency';
     PropertyName[7] := 'phases';
     PropertyName[8] := 'R1';
     PropertyName[9] := 'X1';
     PropertyName[10] := 'R0';
     PropertyName[11] := 'X0';
}
     {PropertyValue Allocated in DSSObject.Create}
    PropertyValue[1] := '1';
    PropertyValue[2] := GetBus(1);
    PropertyValue[3] := '115';
    PropertyValue[4] := '1';
    PropertyValue[5] := '0';
    PropertyValue[6] := '60';
    PropertyValue[7] := '3';
    PropertyValue[8] := '1.65';
    PropertyValue[9] := '6.6';
    PropertyValue[10] := '1.9';
    PropertyValue[11] := '5.7';

    inherited  InitPropertyValues(NumPropsThisClass);

end;

function TEquivalentObj.GetPropertyValue(Index: Integer): String;
begin
    case Index of
        1:
        begin
        end;

    else
        Result := inherited GetPropertyValue(Index);
    end;
end;

procedure TEquivalentObj.MakePosSequence;

var
    S: String;
begin


/// ????


    S := 'Phases=1 ';
    S := S + Format('BasekV=%-.5g ', [kVbase / SQRT3]);
    S := S + Format('R1=%-.5g ', [R1]);
    S := S + Format('X1=%-.5g ', [X1]);

    Parser.CmdString := S;
    Edit;

    inherited;

end;

function TEquivalentObj.DoTerminalsDef(const n: Integer): Integer;
begin
    Result := FNTerms;
    if N <> FNterms then
        if N > 0 then
            ReallocRX;
end;

procedure TEquivalentObj.ParseDblMatrix(Mat: pDoubleArray);

// Parse input string as an array

begin

    Parser.ParseAsSymMatrix(FnTerms, Mat);

end;

procedure TEquivalentObj.ReallocRX;
begin
    Reallocmem(R1, Sizeof(R1^[1]) * SQR(FnTerms));
    Reallocmem(X1, Sizeof(X1^[1]) * SQR(FnTerms));
    Reallocmem(R0, Sizeof(R0^[1]) * SQR(FnTerms));
    Reallocmem(X0, Sizeof(X0^[1]) * SQR(FnTerms));
end;

procedure TEquivalent.InterpretAllBuses(const S: String);
//  routine expecting all winding connections expressed in one array of strings
var
    S1, BusNam: String;
    i: Integer;
begin

    AuxParser.CmdString := S;  // Load up Parser

    {Loop for no more than the expected number of windings;  Ignore omitted values}
    with ActiveEquivalentObj do
        for i := 1 to FNterms do
        begin
            S1 := AuxParser.NextParam; // ignore any parameter name  not expecting any
            BusNam := AuxParser.StrValue;
            if Length(BusNam) > 0 then
                SetBus(i, BusNam);
        end;

end;

end.
