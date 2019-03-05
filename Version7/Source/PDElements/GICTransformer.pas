unit GICTransformer;

{
  ----------------------------------------------------------
  Copyright (c) 2011-2015, Electric Power Research Institute, Inc.
  All rights reserved.
  ----------------------------------------------------------
}

{
   6-21-2011 Created from Fault Object
}

{

   Special resistance-only model of transformers for geomagnetically-induced current (GIC) studies
}

interface

uses
    Command,
    DSSClass,
    PDClass,
    Circuit,
    PDElement,
    UcMatrix,
    ArrayDef,
    XYCurve;

type

    TGICTransformer = class(TPDClass)
    PRIVATE

        procedure GICTransSetBusH(const s: String);
        procedure GICTransSetBusX(const s: String);
    PROTECTED
        procedure DefineProperties;
        function MakeLike(const GICTransName: String): Integer; OVERRIDE;
    PUBLIC
        constructor Create;
        destructor Destroy; OVERRIDE;

        function Edit: Integer; OVERRIDE;     // uses global parser
        function Init(Handle: Integer): Integer; OVERRIDE;
        function NewObject(const ObjName: String): Integer; OVERRIDE;

    end;

    TGICTransformerObj = class(TPDElement)
    PRIVATE
        G1, G2: Double;         // single G per phase (line rating)

        SpecType: Integer;
        FMVARating: Double;
        FVarCurve: String;
        FVarCurveObj: TXYCurveObj;
        FpctR1,
        FpctR2: Double;
        FZbase1,
        FZbase2: Double;
        FkVSpecified: Boolean;
        FpctRSpecified: Boolean;
        KSpecified: Boolean;
        FKFactor: Double;
        FkV1,
        FkV2: Double;

    PUBLIC
        constructor Create(ParClass: TDSSClass; const FaultName: String);
        destructor Destroy; OVERRIDE;

        procedure RecalcElementData; OVERRIDE;
        procedure CalcYPrim; OVERRIDE;

        procedure MakePosSequence; OVERRIDE;  // Make a positive Sequence Model

        function GetPropertyValue(Index: Integer): String; OVERRIDE;
        procedure InitPropertyValues(ArrayOffset: Integer); OVERRIDE;
        procedure DumpProperties(var F: TextFile; Complete: Boolean); OVERRIDE;

        procedure WriteVarOutputRecord(var F: TextFile); // Add a record to the ouput file based on present GIC
    end;

var
    ActiveGICTransformerObj: TGICTransformerObj;

implementation

uses
    ParserDel,
    MyDSSClassDefs,
    DSSClassDefs,
    DSSGlobals,
    dynamics,
    Sysutils,
    Ucomplex,
    MathUtil,
    Utilities;

const
    NumPropsthisclass = 15;

    SPEC_GSU = 1;
    SPEC_AUTO = 2;
    SPEC_YY = 3;

//- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
constructor TGICTransformer.Create;  // Creates superstructure for all Fault objects
begin
    inherited Create;
    Class_Name := 'GICTransformer';
    DSSClassType := GIC_TRANSFORMER + PD_ELEMENT;

    ActiveElement := 0;

    DefineProperties;

    CommandList := TCommandList.Create(Slice(PropertyName^, NumProperties));
    CommandList.Abbrev := TRUE;
end;

//- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
destructor TGICTransformer.Destroy;

begin
    // ElementList and  CommandList freed in inherited destroy
    inherited Destroy;
end;

//- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
procedure TGICTransformer.DefineProperties;
begin

    Numproperties := NumPropsThisClass;
    CountProperties;   // Get inherited property count
    AllocatePropertyArrays;

     // Define Property names

    PropertyName^[1] := 'BusH';
    PropertyName^[2] := 'BusNH';
    PropertyName^[3] := 'BusX';
    PropertyName^[4] := 'BusNX';
    PropertyName^[5] := 'phases';
    PropertyName^[6] := 'Type';
    PropertyName^[7] := 'R1';
    PropertyName^[8] := 'R2';
    PropertyName^[9] := 'KVLL1';
    PropertyName^[10] := 'KVLL2';
    PropertyName^[11] := 'MVA';
    PropertyName^[12] := 'VarCurve';
    PropertyName^[13] := '%R1';
    PropertyName^[14] := '%R2';
    PropertyName^[15] := 'K';

     // define Property help values
    PropertyHelp[1] := 'Name of High-side(H) bus. Examples:' + CRLF +
        'BusH=busname' + CRLF +
        'BusH=busname.1.2.3';
    PropertyHelp[2] := 'Name of Neutral bus for H, or first, winding. Defaults to all phases connected ' +
        'to H-side bus, node 0, if not specified and transformer type is either GSU or YY. ' +
        '(Shunt Wye Connection to ground reference)' +
        'For Auto, this is automatically set to the X bus.';
    PropertyHelp[3] := 'Name of Low-side(X) bus, if type=Auto or YY. ';
    PropertyHelp[4] := 'Name of Neutral bus for X, or Second, winding. Defaults to all phases connected ' +
        'to X-side bus, node 0, if not specified. (Shunt Wye Connection to ground reference)';
    PropertyHelp[5] := 'Number of Phases. Default is 3.';
    PropertyHelp[6] := 'Type of transformer: {GSU* | Auto | YY}. Default is GSU.';
    PropertyHelp[7] := 'Resistance, each phase, ohms for H winding, (Series winding, if Auto). Default is 0.0001. If ';
    PropertyHelp[8] := 'Resistance, each phase, ohms for X winding, (Common winding, if Auto). Default is 0.0001. ';
    PropertyHelp[9] := 'Optional. kV LL rating for H winding (winding 1). Default is 500. Required if you are going to export vars for power flow analysis ' +
        'or enter winding resistances in percent.';
    PropertyHelp[10] := 'Optional. kV LL rating for X winding (winding 2). Default is 138. Required if you are going to export vars for power flow analysis ' +
        'or enter winding resistances in percent..';
    PropertyHelp[11] := 'Optional. MVA Rating assumed Transformer. Default is 100. Used for computing vars due to GIC and ' +
        'winding resistances if kV and MVA ratings are specified.';
    PropertyHelp[12] := 'Optional. XYCurve object name. Curve is expected as TOTAL pu vars vs pu GIC amps/phase. Vars are in pu of the MVA property. No Default value. ' +
        'Required only if you are going to export vars for power flow analysis. ' +
        'See K property.';

    PropertyHelp[13] := 'Optional. Percent Resistance, each phase, for H winding (1), (Series winding, if Auto). Default is 0.2. ' + CRLF + CRLF +
        'Alternative way to enter R1 value. It is the actual resistances in ohmns that matter. MVA and kV should be specified.';
    PropertyHelp[14] := 'Optional. Percent Resistance, each phase, for X winding (2), (Common winding, if Auto). Default is 0.2. ' + CRLF + CRLF +
        'Alternative way to enter R2 value. It is the actual resistances in ohms that matter. MVA and kV should be specified.';
    PropertyHelp[15] := 'Mvar K factor. Default way to convert GIC Amps in H winding (winding 1) to Mvar. Default is 2.2. ' +
        'Commonly-used simple multiplier for estimating Mvar losses for power flow analysis. ' + CRLF + CRLF +
        'Mvar = K * kvLL * GIC per phase / 1000 ' + CRLF + CRLF +
        'Mutually exclusive with using the VarCurve property and pu curves.' +
        'If you specify this (default), VarCurve is ignored.';


    ActiveProperty := NumPropsThisClass;
    inherited DefineProperties;  // Add defs of inherited properties to bottom of list

end;

//- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
function TGICTransformer.NewObject(const ObjName: String): Integer;
begin
   // create a new object of this class and add to list
    with ActiveCircuit do
    begin
        ActiveCktElement := TGICTransformerObj.Create(Self, ObjName);
        Result := AddObjectToList(ActiveDSSObject);
    end;

end;

//- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
procedure TGICTransformer.GICTransSetBusH(const s: String);

var
    s2: String;
    dotpos: Integer;

   // Set Bus2 = BusH1.0.0.0

begin
    with ActiveGICTransformerObj do
    begin

        SetBus(1, S);

     // Default Bus2 to zero node of Bus1. (Wye Grounded connection)

     // Strip node designations from S
        dotpos := Pos('.', S);
        if dotpos > 0 then
            S2 := Copy(S, 1, dotpos - 1)  // copy up to Dot
        else
            S2 := Copy(S, 1, Length(S));

        S2 := S2 + '.0.0.0';     // Set Default for up to 3 phases

        SetBus(2, S2);
        IsShunt := TRUE;
    end;
end;

//- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
procedure TGICTransformer.GICTransSetBusX(const s: String);

var
    s2: String;
    dotpos: Integer;

   // Special handling for Bus X
   // Make sure we have enough terminals defined
   // Set Bus2 = Bus1.0.0.0

begin
    with ActiveGICTransformerObj do
    begin

        if Nterms <> 4 then   // have to have 4 terminals to set this property
        begin
            Nterms := 4;
            NConds := Fnphases; // force reallocation of terminals and conductors
        end;

        SetBus(3, S);

     // Default Bus4 to zero node of Bus3. (Wye Grounded connection)

     // Strip node designations from S
        dotpos := Pos('.', S);
        if dotpos > 0 then
            S2 := Copy(S, 1, dotpos - 1)  // copy up to Dot
        else
            S2 := Copy(S, 1, Length(S));

        S2 := S2 + '.0.0.0';     // Set Default for up to 3 phases

        SetBus(4, S2);
        IsShunt := TRUE;
    end;
end;


//- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
function TGICTransformer.Edit: Integer;

var
    ParamPointer: Integer;
    ParamName: String;
    Param: String;

begin
    Result := 0;
  // continue parsing with contents of Parser
    ActiveGICTransformerObj := ElementList.Active;
    ActiveCircuit.ActiveCktElement := ActiveGICTransformerObj;  // use property to set this value

    with ActiveGICTransformerObj do
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
                    DoSimpleMsg('Unknown parameter "' + ParamName + '" for Object "' + Class_Name + '.' + Name + '"', 350);
                1:
                    GICTransSetBusH(param);
                2:
                    Setbus(2, param);
                3:
                    GICTransSetBusX(param);
                4:
                    Setbus(4, param);
                5: ; // see below
                6:
                    case Uppercase(param)[1] of
                        'G':
                            SpecType := SPEC_GSU;
                        'A':
                            SpecType := SPEC_AUTO;
                        'Y':
                            SpecType := SPEC_YY;
                    end;
                7:
                begin
                    G1 := Parser.Dblvalue;
                    if G1 <> 0.0 then
                        G1 := 1.0 / G1
                    else
                        G1 := 10000.0;  // Default to a low resistance
                end;
                8:
                begin
                    G2 := Parser.Dblvalue;
                    if G2 <> 0.0 then
                        G2 := 1.0 / G2
                    else
                        G2 := 10000.0;  // Default to a low resistance
                end;
                9:
                    FkV1 := Parser.DblValue;
                10:
                    FkV2 := Parser.DblValue;
                11:
                    FMVArating := Parser.DblValue;
                12:
                    FVarCurve := Param;
                13:
                    FpctR1 := Parser.DblValue;
                14:
                    FpctR2 := Parser.DblValue;
                15:
                    FKFactor := Parser.DblValue;
            else
           // Inherited
                ClassEdit(ActiveGICTransformerObj, ParamPointer - NumPropsThisClass)
            end;

         // Some specials ...
            case ParamPointer of
                1:
                    PropertyValue[2] := GetBus(2);  // Bus2 gets modified if bus1 is set
                3:
                begin
                    PropertyValue[4] := GetBus(4);  // Bus4 gets modified if bus3(X) is set
                    if SpecType = SPEC_AUTO then
                    begin   // automatically make up series-to-common connection
                        SetBus(2, GetBus(3));
                        PropertyValue[2] := GetBus(2);
                    end;
                end;
                5:
                    if Fnphases <> Parser.IntValue then
                    begin
                        Nphases := Parser.IntValue;
                        NConds := Fnphases;  // Force Reallocation of terminal info if different size
                        ActiveCircuit.BusNameRedefined := TRUE;  // Set Global Flag to signal circuit to rebuild busdefs
                    end;
                6:
                    case Spectype of
                        SPEC_AUTO:
                        begin
                            if Nterms = 2 then
                            begin
                                Nterms := 4;
                                NConds := Fnphases;
                            end;
                            SetBus(2, GetBus(3));
                        end;
                    end;
                7..8:
                    FpctRSpecified := FALSE;
                9..10:
                    FkVSpecified := TRUE;
                12:
                begin
                    FVarCurveObj := XYCurveClass.Find(FVarCurve);
                    Kspecified := FALSE;
                end;
                13..14:
                    FpctRSpecified := TRUE;
                15:
                    KSpecified := TRUE;
            else
            end;

         //YPrim invalidation on anything that changes impedance values or no. of terminals
            case ParamPointer of
                3..8:
                    YprimInvalid := TRUE;
            else
            end;

            ParamName := Parser.NextParam;
            Param := Parser.StrValue;
        end;

        RecalcElementData;
    end;

end;

//- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
function TGICTransformer.MakeLike(const GICTransName: String): Integer;
var
    OtherGICTrans: TGICTransformerObj;
    i: Integer;
begin
    Result := 0;
   {See if we can find this Fault name in the present collection}
    OtherGICTrans := Find(GICTransName);
    if OtherGICTrans <> NIL then
        with ActiveGICTransformerObj do
        begin

            if Fnphases <> OtherGICTrans.Fnphases then
            begin
                Fnphases := OtherGICTrans.Fnphases;
                FnTerms := OtherGICTrans.FnTerms;
                NConds := Fnphases; // force reallocation of terminals and conductors

                Yorder := Fnconds * Fnterms;
                YPrimInvalid := TRUE;

            end;

            BaseFrequency := OtherGICTrans.BaseFrequency;
            G1 := OtherGICTrans.G1;
            G2 := OtherGICTrans.G2;
            SpecType := OtherGICTrans.SpecType;
            FMVARating := OtherGICTrans.FMVARating;
            FVarcurve := OtherGICTrans.FVarcurve;
            FVarcurveObj := OtherGICTrans.FVarcurveObj;
            FkV1 := OtherGICTrans.FkV1;
            FkV2 := OtherGICTrans.FkV2;
            FpctR1 := OtherGICTrans.FpctR1;
            FpctR2 := OtherGICTrans.FpctR2;
            FpctRSpecified := OtherGICTrans.FpctRSpecified;
            FkVSpecified := OtherGICTrans.FkVSpecified;
            FZBase1 := OtherGICTrans.FZBase1;
            FZBase2 := OtherGICTrans.FZBase2;
            FKfactor := OtherGICTrans.FKfactor;
            KSpecified := OtherGICTrans.KSpecified;

            ClassMakeLike(OtherGICTrans);

            for i := 1 to ParentClass.NumProperties do
                PropertyValue[i] := OtherGICTrans.PropertyValue[i];
            Result := 1;
        end
    else
        DoSimpleMsg('Error in GICTransformer MakeLike: "' + GICTransName + '" Not Found.', 351);


end;

//- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
function TGICTransformer.Init(Handle: Integer): Integer;

begin
    DoSimpleMsg('Need to implement TGICTransformer.Init', -1);
    Result := 0;
end;


//- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
//      TGICTransformer Obj
//- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

constructor TGICTransformerObj.Create(ParClass: TDSSClass; const FaultName: String);

begin
    inherited Create(ParClass);
    DSSObjType := ParClass.DSSClassType;
    Name := LowerCase(FaultName);

    NPhases := 3;  // Directly set conds and phases
    Fnconds := 3;
    Nterms := 2;  // Force allocation of terminals and conductors

    Setbus(2, (GetBus(1) + '.0'));  // Default to grounded
    IsShunt := TRUE;

    G1 := 10000.0;
    G2 := 10000.0;
    SpecType := SPEC_GSU;

    FMVARating := 100.0;
    FVarCurve := '';
    FVarCurveObj := NIL;

    FkVSpecified := FALSE;
    FkV1 := 500.0;
    FkV2 := 138.0;
    FpctR1 := 0.2;
    FpctR2 := 0.2;

    FKfactor := 2.2;
    KSpecified := TRUE;

    NormAmps := 0.0;
    EmergAmps := 0.0;
    FaultRate := 0.0;
    PctPerm := 100.0;
    HrsToRepair := 0.0;

    InitPropertyValues(0);

    Yorder := Fnterms * Fnconds;

    FpctRSpecified := TRUE;  // Force computation of G1, G2
    RecalcElementData;
    FpctRSpecified := FALSE;  // Turn flag off
end;

//- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
destructor TGICTransformerObj.Destroy;
begin
    inherited destroy;
end;

//- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
procedure TGICTransformerObj.RecalcElementData;

begin

    FZbase1 := SQR(FkV1) / FMVArating;
    FZbase2 := SQR(FkV2) / FMVArating;

    if FpctRSpecified then
    begin
        G1 := 100.0 / (FZBase1 * FPctR1);
        G2 := 100.0 / (FZBase2 * FPctR1);
    end
    else
    begin
        FPctR1 := 100.0 / (FZBase1 * G1);
        FPctR2 := 100.0 / (FZBase2 * G2);
    end;

end;

//- - - - - - - - VAR EXPORT - - - - - - - - - - - - - - - - - - - -
procedure TGICTransformerObj.WriteVarOutputRecord(var F: TextFile);
var
    Curr: Complex;
    MVarMag: Double;
    GICperPhase: Double;
    puCurrMag: Double;
    i: Integer;

begin
    ComputeIterminal;
    Curr := CZERO;
    for i := 1 to Fnphases do
        Caccum(Curr, Iterminal^[i]);
    GICperPhase := Cabs(Curr) / Fnphases;
    if Kspecified then
    begin
        MVarMag := FKfactor * FkV1 * GICperPhase / 1000.0;
    end
    else
    begin
        if Assigned(FVarCurveObj) then
        begin
                // MVA = sqrt(3) * kVLL * I/1000
                // pu A per phase (Avg)
            puCurrMag := GICperPhase / (FMVArating * 1000.0 / FkV1 / Sqrt3);
            MVarMag := FVarCurveObj.GetYValue(puCurrMag) * FMVARating / Sqrt2;
        end
        else
            MvarMag := 0.0;
    end;

    Writeln(F, Format('%s, %.8g, %.8g', [GetBus(1), MVarMag, (GICperPhase)]));
end;

//- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
procedure TGICTransformerObj.CalcYPrim;

var
    Value, Value2: Complex;
    i: Integer;

    YPrimTemp: TCMatrix;

begin

    if YPrimInvalid then
    begin    // Reallocate YPrim if something has invalidated old allocation
        if YPrim_Series <> NIL then
            YPrim_Series.Free;
        YPrim_Series := TCmatrix.CreateMatrix(Yorder);
        if YPrim_Shunt <> NIL then
            YPrim_Shunt.Free;
        YPrim_Shunt := TCmatrix.CreateMatrix(Yorder);
        if YPrim <> NIL then
            YPrim.Free;
        YPrim := TcMatrix.CreateMatrix(Yorder);
    end
    else
    begin
        YPrim_Series.Clear; // zero out YPrim
        YPrim_Shunt.Clear; // zero out YPrim
        Yprim.Clear;
    end;


    if IsShunt then
        YPrimTemp := YPrim_Shunt
    else
        YPrimTemp := Yprim_Series;

  // make sure randommult is 1.0 if not solution mode MonteFault


    with YPrimTemp do
    begin

    { Now, Put in Yprim matrix }

    {If the fault is not ON, the set zero conductance}

        case SpecType of

            SPEC_GSU:
            begin
                Value := Cmplx(G1, 0.0);
                Value2 := cnegate(Value);
                for i := 1 to Fnphases do
                begin
                    SetElement(i, i, Value);     // Elements are only on the diagonals
                    SetElement(i + Fnphases, i + Fnphases, Value);
                    SetElemSym(i, i + Fnphases, Value2);
                end;  {For}
            end;

            SPEC_AUTO:
            begin
                // Terminals 1 and 2
                Value := Cmplx(G1, 0.0);
                Value2 := cnegate(Value);
                for i := 1 to Fnphases do
                begin
                    SetElement(i, i, Value);     // Elements are only on the diagonals
                    SetElement(i + Fnphases, i + Fnphases, Value);
                    SetElemSym(i, i + Fnphases, Value2);
                end;  {For}
                // Terminals 3 and 4
                Value := Cmplx(G2, 0.0);
                Value2 := cnegate(Value);
                for i := (2 * Fnphases + 1) to 3 * Fnphases do
                begin
                    SetElement(i, i, Value);     // Elements are only on the diagonals
                    SetElement(i + Fnphases, i + Fnphases, Value);
                    SetElemSym(i, i + Fnphases, Value2);
                end;  {For}
            end;

            SPEC_YY:
            begin
                // Terminals 1 and 2
                Value := Cmplx(G1, 0.0);
                Value2 := cnegate(Value);
                for i := 1 to Fnphases do
                begin
                    SetElement(i, i, Value);     // Elements are only on the diagonals
                    SetElement(i + Fnphases, i + Fnphases, Value);
                    SetElemSym(i, i + Fnphases, Value2);
                end;  {For}
                // Terminals 3 and 4
                Value := Cmplx(G2, 0.0);
                Value2 := cnegate(Value);
                for i := (2 * Fnphases + 1) to 3 * Fnphases do
                begin
                    SetElement(i, i, Value);     // Elements are only on the diagonals
                    SetElement(i + Fnphases, i + Fnphases, Value);
                    SetElemSym(i, i + Fnphases, Value2);
                end;  {For}
            end;

        end;

    end; {With YPRIM}

    YPrim.CopyFrom(YPrimTemp);

    inherited CalcYPrim;
    YprimInvalid := FALSE;
end;

procedure TGICTransformerObj.DumpProperties(var F: TextFile; Complete: Boolean);

var
    i: Integer;

begin
    inherited DumpProperties(F, complete);


    with ParentClass do
    begin
        Writeln(F, '~ ', PropertyName^[1], '=', firstbus);
        Writeln(F, '~ ', PropertyName^[2], '=', nextbus);
        Writeln(F, '~ ', PropertyName^[3], '=', nextbus);
        Writeln(F, '~ ', PropertyName^[4], '=', nextbus);

        Writeln(F, '~ ', PropertyName^[5], '=', Fnphases: 0);
        case Spectype of
            SPEC_GSU:
                Writeln(F, '~ ', PropertyName^[6], '= GSU');
            SPEC_AUTO:
                Writeln(F, '~ ', PropertyName^[6], '= AUTO');
            SPEC_YY:
                Writeln(F, '~ ', PropertyName^[6], '= YY');
        end;
        Writeln(F, '~ ', PropertyName^[7], '=', Format('%.8g', [1.0 / G1]));
        Writeln(F, '~ ', PropertyName^[8], '=', Format('%.8g', [1.0 / G2]));
        Writeln(F, '~ ', PropertyName^[9], '=', FkV1: 0: 2);
        Writeln(F, '~ ', PropertyName^[10], '=', FkV2: 0: 2);
        Writeln(F, '~ ', PropertyName^[11], '=', FMVARating: 0: 2);
        Writeln(F, '~ ', PropertyName^[12], '=', FVarcurve);
        Writeln(F, '~ ', PropertyName^[13], '=', Format('%.8g', [FpctR1]));
        Writeln(F, '~ ', PropertyName^[14], '=', Format('%.8g', [FpctR2]));

        for i := NumPropsthisClass + 1 to NumProperties do
        begin
            Writeln(F, '~ ', PropertyName^[i], '=', PropertyValue[i]);
        end;

    end;

end;


procedure TGICTransformerObj.InitPropertyValues(ArrayOffset: Integer);
begin

    PropertyValue[1] := getbus(1);
    PropertyValue[2] := getbus(2);
    PropertyValue[3] := getbus(3);
    PropertyValue[4] := getbus(4);
    PropertyValue[5] := '3';
    PropertyValue[6] := 'GSU';
    PropertyValue[7] := '0.0001';
    PropertyValue[8] := '0.0001';
    PropertyValue[9] := '500';
    PropertyValue[10] := '138';
    PropertyValue[11] := '100';
    PropertyValue[12] := '';
    PropertyValue[13] := '0.2';
    PropertyValue[14] := '0.2';
    PropertyValue[15] := '2.2';


    inherited  InitPropertyValues(NumPropsThisClass);

     // Override Inherited Properties
    PropertyValue[NumPropsThisClass + 1] := '0';  //Normamps
    PropertyValue[NumPropsThisClass + 2] := '0';  //emergamps
    PropertyValue[NumPropsThisClass + 3] := '0';  //Fault rate
    PropertyValue[NumPropsThisClass + 4] := '0';   // Pct Perm
    PropertyValue[NumPropsThisClass + 5] := '0';    // Hrs to repair

end;

function TGICTransformerObj.GetPropertyValue(Index: Integer): String;

begin
    case INdex of
        1:
            Result := GetBus(1);
        2:
            Result := GetBus(2);
        3:
            Result := GetBus(3);
        4:
            Result := GetBus(4);
        5:
            Result := Format('%d', [Nphases]);
        7:
            Result := Format('%.8g', [1.0 / G1]);
        8:
            Result := Format('%.8g', [1.0 / G2]);
        9:
            Result := Format('%.8g', [FkV1]);
        10:
            Result := Format('%.8g', [FkV2]);
        11:
            Result := Format('%.8g', [FMVARating]);
        12:
            Result := Format('%s', [Fvarcurve]);
        13:
            Result := Format('%.8g', [FpctR1]);
        14:
            Result := Format('%.8g', [FpctR2]);
        15:
            Result := Format('%.8g', [FKFactor]);
    else
        Result := inherited GetPropertyValue(Index);
    end;
end;

procedure TGICTransformerObj.MakePosSequence;


begin
    if FnPhases <> 1 then
    begin
        Parser.CmdString := 'Phases=1';
        Edit;
    end;
    inherited;

end;

end.
