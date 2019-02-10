unit LineCode;

{
  ----------------------------------------------------------
  Copyright (c) 2008-2015, Electric Power Research Institute, Inc.
  All rights reserved.
  ----------------------------------------------------------
}

interface

{The Linecode object is a general DSS object used by all circuits
 as a reference for obtaining line impedances.

 The values are set by the normal New and Edit procedures for any DSS object.

 The values are retrieved by setting the Code Property in the LineCode Class.
 This sets the active Linecode object to be the one referenced by the Code Property;

 Then the values of that code can be retrieved via the public variables.

 }

uses
    Command,
    DSSClass,
    DSSObject,
    UcMatrix;

type

    TLineCode = class(TDSSClass)
    PRIVATE
        SymComponentsChanged: Boolean;
        MatrixChanged: Boolean;

        function Get_Code: String;  // Returns active line code string
        procedure Set_Code(const Value: String);  // sets the  active linecode

        procedure SetZ1Z0(i: Integer; Value: Double);
        procedure SetUnits(const s: String);  // decode units specification

        procedure DoMatrix(i: Integer; ActorID: Integer);  // set impedances as matrices


    PROTECTED
        procedure DefineProperties;
        function MakeLike(const LineName: String): Integer; OVERRIDE;
    PUBLIC

        constructor Create;
        destructor Destroy; OVERRIDE;

        function Edit(ActorID: Integer): Integer; OVERRIDE;     // uses global parser
        function Init(Handle: Integer; ActorID: Integer): Integer; OVERRIDE;
        function NewObject(const ObjName: String): Integer; OVERRIDE;

       // Set this property to point ActiveLineCodeObj to the right value
        property Code: String READ Get_Code WRITE Set_Code;

    end;

    TLineCodeObj = class(TDSSObject)
    PRIVATE

        FNeutralConductor: Integer;

        procedure Set_NPhases(Value: Integer);
        procedure DoKronReduction;
        function get_Rmatrix: String;
        function get_Xmatrix: String;
        function get_CMatrix: String;

    PUBLIC
        FNPhases: Integer;

        SymComponentsModel,
        ReduceByKron: Boolean;

        Z,         // Base Frequency Series Z matrix
        Zinv,
        YC: TCMatrix;  // Shunt capacitance matrix at Base frequency.

        BaseFrequency: Double;

        R1,
        X1,
        R0,
        X0,
        C1,
        C0,
        NormAmps,
        EmergAmps,
        FaultRate,
        PctPerm,
        HrsToRepair,
        Rg,
        Xg,
        rho: Double;

        Units: Integer;  {See LineUnits}

        constructor Create(ParClass: TDSSClass; const LineCodeName: String);
        destructor Destroy; OVERRIDE;
        property NumPhases: Integer READ FNPhases WRITE Set_Nphases;
        procedure CalcMatricesFromZ1Z0;

        function GetPropertyValue(Index: Integer): String; OVERRIDE;
        procedure InitPropertyValues(ArrayOffset: Integer); OVERRIDE;
        procedure DumpProperties(var F: TextFile; Complete: Boolean); OVERRIDE;

    end;

var
    LineCodeClass: TLineCode;
    ActiveLineCodeObj: TLineCodeObj;

implementation

uses
    ParserDel,
    DSSClassDefs,
    DSSGlobals,
    Sysutils,
    Ucomplex,
    Arraydef,
    Utilities,
    LineUnits;

const
    NumPropsThisClass = 24;

//- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
constructor TLineCode.Create;  // Creates superstructure for all Line objects
begin
    inherited Create;
    Class_Name := 'LineCode';
    DSSClassType := DSS_OBJECT;
    ActiveElement := 0;

    DefineProperties;

    CommandList := TCommandList.Create(Slice(PropertyName^, NumProperties));
    CommandList.Abbrev := TRUE;

    LineCodeClass := Self;
end;

//- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
destructor TLineCode.Destroy;

begin
    // ElementList and  CommandList freed in inherited destroy
    inherited Destroy;
end;
//- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
procedure TLineCode.DefineProperties;
begin

    Numproperties := NumPropsThisClass;
    CountProperties;   // Get inherited property count
    AllocatePropertyArrays;


    PropertyName[1] := 'nphases';
    PropertyName[2] := 'r1';
    PropertyName[3] := 'x1';
    PropertyName[4] := 'r0';
    PropertyName[5] := 'x0';
    PropertyName[6] := 'C1';
    PropertyName[7] := 'C0';
    PropertyName[8] := 'units';
    PropertyName[9] := 'rmatrix';
    PropertyName[10] := 'xmatrix';
    PropertyName[11] := 'cmatrix';
    PropertyName[12] := 'baseFreq';
    PropertyName[13] := 'normamps';
    PropertyName[14] := 'emergamps';
    PropertyName[15] := 'faultrate';
    PropertyName[16] := 'pctperm';
    PropertyName[17] := 'repair';
    PropertyName[18] := 'Kron';
    PropertyName[19] := 'Rg';
    PropertyName[20] := 'Xg';
    PropertyName[21] := 'rho';
    PropertyName[22] := 'neutral';
    PropertyName[23] := 'B1';
    PropertyName[24] := 'B0';


    PropertyHelp[1] := 'Number of phases in the line this line code data represents.  Setting this property reinitializes the line code.  Impedance matrix is reset for default symmetrical component.';
    PropertyHelp[2] := 'Positive-sequence Resistance, ohms per unit length. Setting any of R1, R0, X1, X0, C1, C0 forces ' +
        'the program to use the symmetrical component line definition. See also Rmatrix.';
    PropertyHelp[3] := 'Positive-sequence Reactance, ohms per unit length. Setting any of R1, R0, X1, X0, C1, C0 forces ' +
        'the program to use the symmetrical component line definition. See also Xmatrix';
    PropertyHelp[4] := 'Zero-sequence Resistance, ohms per unit length. Setting any of R1, R0, X1, X0, C1, C0 forces ' +
        'the program to use the symmetrical component line definition.';
    PropertyHelp[5] := 'Zero-sequence Reactance, ohms per unit length. Setting any of R1, R0, X1, X0, C1, C0 forces ' +
        'the program to use the symmetrical component line definition.';
    PropertyHelp[6] := 'Positive-sequence capacitance, nf per unit length. Setting any of R1, R0, X1, X0, C1, C0 forces ' +
        'the program to use the symmetrical component line definition. See also Cmatrix and B1.';
    PropertyHelp[7] := 'Zero-sequence capacitance, nf per unit length. Setting any of R1, R0, X1, X0, C1, C0 forces ' +
        'the program to use the symmetrical component line definition. See also B0.';
    PropertyHelp[8] := 'One of (ohms per ...) {none|mi|km|kft|m|me|ft|in|cm}.  Default is none; assumes units agree with length units' +
        'given in Line object';
    PropertyHelp[9] := 'Resistance matrix, lower triangle, ohms per unit length. Order of the matrix is the number of phases. ' +
        'May be used to specify the impedance of any line configuration.  For balanced line models, you may ' +
        'use the standard symmetrical component data definition instead.';
    PropertyHelp[10] := 'Reactance matrix, lower triangle, ohms per unit length. Order of the matrix is the number of phases. ' +
        'May be used to specify the impedance of any line configuration.  For balanced line models, you may ' +
        'use the standard symmetrical component data definition instead.';
    PropertyHelp[11] := 'Nodal Capacitance matrix, lower triangle, nf per unit length.Order of the matrix is the number of phases. ' +
        'May be used to specify the shunt capacitance of any line configuration.  For balanced line models, you may ' +
        'use the standard symmetrical component data definition instead.';
    PropertyHelp[12] := 'Frequency at which impedances are specified.';
    PropertyHelp[13] := 'Normal ampere limit on line.  This is the so-called Planning Limit. It may also be ' +
        'the value above which load will have to be dropped in a contingency.  Usually about ' +
        '75% - 80% of the emergency (one-hour) rating.';
    PropertyHelp[14] := 'Emergency ampere limit on line (usually one-hour rating).';
    PropertyHelp[15] := 'Number of faults per unit length per year.';
    PropertyHelp[16] := 'Percentage of the faults that become permanent.';
    PropertyHelp[17] := 'Hours to repair.';
    PropertyHelp[18] := 'Kron = Y/N. Default=N.  Perform Kron reduction on the impedance matrix after it is formed, reducing order by 1. ' +
        'Eliminates the conductor designated by the "Neutral=" property. ' +
        'Do this after the R, X, and C matrices are defined. Ignored for symmetrical components. ' +
        'May be issued more than once to eliminate more than one conductor by resetting the Neutral property after the previous ' +
        'invoking of this property. Generally, you do not want to do a Kron reduction on the matrix if you intend to solve at a ' +
        'frequency other than the base frequency and exploit the Rg and Xg values.';
    PropertyHelp[19] := 'Carson earth return resistance per unit length used to compute impedance values at base frequency.  For making better frequency adjustments. ' +
        'Default is 0.01805 = 60 Hz value in ohms per kft (matches default line impedances). ' +
        'This value is required for harmonic solutions if you wish to adjust the earth return impedances for frequency. ' +
        'If not, set both Rg and Xg = 0.';
    PropertyHelp[20] := 'Carson earth return reactance per unit length used to compute impedance values at base frequency.  For making better frequency adjustments. ' +
        'Default value is 0.155081 = 60 Hz value in ohms per kft (matches default line impedances). ' +
        'This value is required for harmonic solutions if you wish to adjust the earth return impedances for frequency. ' +
        'If not, set both Rg and Xg = 0.';
    PropertyHelp[21] := 'Default=100 meter ohms.  Earth resitivity used to compute earth correction factor.';
    PropertyHelp[22] := 'Designates which conductor is the "neutral" conductor that will be eliminated by Kron reduction. ' +
        'Default is the last conductor (nphases value). After Kron reduction is set to 0. Subsequent issuing of Kron=Yes ' +
        'will not do anything until this property is set to a legal value. Applies only to LineCodes defined by R, X, and C matrix.';

    PropertyHelp[23] := 'Alternate way to specify C1. MicroS per unit length';
    PropertyHelp[24] := 'Alternate way to specify C0. MicroS per unit length';

    ActiveProperty := NumPropsThisClass;
    inherited DefineProperties;  // Add defs of inherited properties to bottom of list

end;
//- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
function TLineCode.NewObject(const ObjName: String): Integer;
begin
   // create a new object of this class and add to list
    with ActiveCircuit[ActiveActor] do
    begin
        ActiveDSSObject[ActiveActor] := TLineCodeObj.Create(Self, ObjName);
        Result := AddObjectToList(ActiveDSSObject[ActiveActor]);
    end;
end;

function TLineCodeObj.get_Rmatrix: String;
var
    j: Integer;
    i: Integer;
begin
    Result := '[';
    for i := 1 to FNPhases do
    begin
        for j := 1 to FNphases do
        begin
            Result := Result + Format('%12.8f ', [Z.GetElement(i, j).re]);
        end;
        if i < FNphases then
            Result := Result + '|';
    end;
    Result := Result + ']';
end;

function TLineCodeObj.get_Xmatrix: String;
var
    j: Integer;
    i: Integer;
begin
    Result := '[';
    for i := 1 to FNPhases do
    begin
        for j := 1 to FNphases do
        begin
            Result := Result + Format('%12.8f ', [Z.GetElement(i, j).im]);
        end;
        if i < FNphases then
            Result := Result + '|';
    end;
    Result := Result + ']';
end;

function TLineCodeObj.get_CMatrix: String;
var
    i, j: Integer;
begin
    Result := '[';
    for i := 1 to FNPhases do
    begin
        for j := 1 to FNphases do
        begin
            Result := Result + Format('%12.8f ', [Yc.GetElement(i, j).im / TwoPi / BaseFrequency * 1.0E9]);
        end;
        if i < FNphases then
            Result := Result + '|';
    end;
    Result := Result + ']';
end;

//- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
procedure TLineCode.SetUnits(const s: String);
// decodes the units string and sets the Units variable

begin
    ActiveLineCodeObj.Units := GetUnitsCode(S);
end;

//- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
procedure TLineCode.SetZ1Z0(i: Integer; Value: Double);
// set symmetrical component impedances and a flag to indicate they were changed
begin

    SymComponentsChanged := TRUE;


    with ActiveLineCodeObj do
    begin
        SymComponentsModel := TRUE;
        case i of
            1:
                R1 := Value;
            2:
                X1 := Value;
            3:
                R0 := Value;
            4:
                X0 := Value;
            5:
                C1 := Value;
            6:
                C0 := Value;
        else
        end;
    end;

end;

//- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
procedure TLineCode.DoMatrix(i: Integer; ActorID: Integer);

var
    OrderFound, Norder, j: Integer;
    MatBuffer: pDoubleArray;
    Zvalues: pComplexArray;
    Factor: Double;

begin
    with ActiveLineCodeObj do
    begin
        MatrixChanged := TRUE;
        MatBuffer := Allocmem(Sizeof(Double) * FNphases * FNphases);
        OrderFound := Parser[ActorID].ParseAsSymMatrix(FNphases, MatBuffer);

        if OrderFound > 0 then    // Parse was successful
            case i of
                1:
                begin    {R}
                    ZValues := Z.GetValuesArrayPtr(Norder);
                    if Norder = FNphases then
                        for j := 1 to FNphases * FNphases do
                            ZValues^[j].Re := MatBuffer^[j];
                end;
                2:
                begin   {X}
                    ZValues := Z.GetValuesArrayPtr(Norder);
                    if Norder = FNphases then
                        for j := 1 to FNphases * FNphases do
                            ZValues^[j].im := MatBuffer^[j];
                end;
                3:
                begin    {YC Matrix}
                    Factor := TwoPi * BaseFrequency * 1.0e-9;
                    ZValues := YC.GetValuesArrayPtr(Norder);
                    if Norder = FNphases then
                        for j := 1 to FNphases * FNphases do
                            ZValues^[j].im := Factor * MatBuffer^[j];
                end;
            else
            end;

        Freemem(MatBuffer, Sizeof(Double) * FNphases * FNphases);
    end;
end;


//- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
function TLineCode.Edit(ActorID: Integer): Integer;
var
    ParamPointer: Integer;
    ParamName: String;
    Param: String;

begin
    Result := 0;
  // continue parsing with contents of Parser
    ActiveLineCodeObj := ElementList.Active;
    ActiveDSSObject[ActorID] := ActiveLineCodeObj;
    SymComponentsChanged := FALSE;
    MatrixChanged := FALSE;
    ActiveLineCodeObj.ReduceByKron := FALSE;  // Allow all matrices to be computed it raw form

    with ActiveLineCodeObj do
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
                    DoSimpleMsg('Unknown parameter "' + ParamName + '" for Object "' + Class_Name + '.' + Name + '"', 101);
                1:
                    Numphases := Parser[ActorID].IntValue;  // Use property value to force reallocations
                2:
                    SetZ1Z0(1, Parser[ActorID].Dblvalue);  {R1}
                3:
                    SetZ1Z0(2, Parser[ActorID].Dblvalue);  {X0}
                4:
                    SetZ1Z0(3, Parser[ActorID].Dblvalue);  {R1}
                5:
                    SetZ1Z0(4, Parser[ActorID].Dblvalue);  {X0}
                6:
                    SetZ1Z0(5, Parser[ActorID].Dblvalue * 1.0e-9); {C1}   // Convert from nano to farads
                7:
                    SetZ1Z0(6, Parser[ActorID].Dblvalue * 1.0e-9); {C0}
                8:
                    SetUnits(Param);
                9:
{Rmatrix} DoMatrix(1, ActorID);
                10:
{Xmatrix} DoMatrix(2, ActorID);
                11:
{Cmatrix} DoMatrix(3, ActorID);
                12:
                    BaseFrequency := Parser[ActorID].DblValue;
                13:
                    NormAmps := Parser[ActorID].Dblvalue;
                14:
                    EmergAmps := Parser[ActorID].Dblvalue;
                15:
                    FaultRate := Parser[ActorID].Dblvalue;
                16:
                    PctPerm := Parser[ActorID].Dblvalue;
                17:
                    HrsToRepair := Parser[ActorID].Dblvalue;
                18:
                    ReduceByKron := InterpretYesNo(Param);
                19:
                    Rg := Parser[ActorID].DblValue;
                20:
                    Xg := Parser[ActorID].DblValue;
                21:
                    rho := Parser[ActorID].DblValue;
                22:
                    FNeutralConductor := Parser[ActorID].IntValue;
                23:
                    SetZ1Z0(5, Parser[ActorID].Dblvalue / (twopi * BaseFrequency) * 1.0e-6); {B1 -> C1}
                24:
                    SetZ1Z0(6, Parser[ActorID].Dblvalue / (twopi * BaseFrequency) * 1.0e-6); {B0 -> C0}
            else
                ClassEdit(ActiveLineCodeObj, Parampointer - NumPropsThisClass)
            end;

            case ParamPointer of
                9..11:
                    SymComponentsModel := FALSE;
                18:
                    if ReduceByKron and not SymComponentsModel then
                        DoKronReduction;
            end;


            ParamName := Parser[ActorID].NextParam;
            Param := Parser[ActorID].StrValue;
        end;

        if SymComponentsModel then
            CalcMatricesFromZ1Z0;
        if MatrixChanged then
        begin
            Zinv.Copyfrom(Z);
            Zinv.Invert;
        end;
    end;

end;

//- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
function TLineCode.MakeLike(const LineName: String): Integer;
var
    OtherLineCode: TLineCodeObj;
    i: Integer;
begin
    Result := 0;
   {See if we can find this line code in the present collection}
    OtherLineCode := Find(LineName);
    if OtherLineCode <> NIL then
        with ActiveLineCodeObj do
        begin

            if FNPhases <> OtherLineCode.FNphases then
            begin
                FNphases := OtherLineCode.FNphases;

                if Z <> NIL then
                    Z.Free;
                if Zinv <> NIL then
                    Zinv.Free;
                if Yc <> NIL then
                    Yc.Free;

                Z := TCmatrix.CreateMatrix(FNphases);
                Zinv := TCMatrix.CreateMatrix(FNphases);
                Yc := TCMatrix.CreateMatrix(FNphases);
            end;

            Z.CopyFrom(OtherLineCode.Z);
            Zinv.CopyFrom(OtherLineCode.Zinv);
            Yc.CopyFrom(OtherLineCode.Yc);
            BaseFrequency := OtherLineCode.BaseFrequency;
            R1 := OtherLineCode.R1;
            X1 := OtherLineCode.X1;
            R0 := OtherLineCode.R0;
            X0 := OtherLineCode.X0;
            C1 := OtherLineCode.C1;
            C0 := OtherLineCode.C0;
            Rg := OtherLineCode.Rg;
            Xg := OtherLineCode.Xg;
            rho := OtherLineCode.rho;
            FNeutralConductor := OtherLineCode.FNeutralConductor;
            NormAmps := OtherLineCode.NormAmps;
            EmergAmps := OtherLineCode.EmergAmps;
            FaultRate := OtherLineCode.FaultRate;
            PctPerm := OtherLineCode.PctPerm;
            HrsToRepair := OtherLineCode.HrsToRepair;

            for i := 1 to ParentClass.NumProperties do
                PropertyValue[i] := OtherLineCode.PropertyValue[i];
            Result := 1;
        end
    else
        DoSimpleMsg('Error in Line MakeLike: "' + LineName + '" Not Found.', 102);


end;

//- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
function TLineCode.Init(Handle: Integer; ActorID: Integer): Integer;

begin
    DoSimpleMsg('Need to implement TLineCode.Init', -1);
    REsult := 0;
end;

function TLineCode.Get_Code: String;  // Returns active line code string

begin

    Result := TlineCodeObj(ElementList.Active).Name;

end;

procedure TLineCode.Set_Code(const Value: String);  // sets the  active linecode
var
    LineCodeObj: TLineCodeObj;
begin

    ActiveLineCodeObj := NIL;
    LineCodeObj := ElementList.First;
    while LineCodeObj <> NIL do
    begin

        if CompareText(LineCodeObj.Name, Value) = 0 then
        begin
            ActiveLineCodeObj := LineCodeObj;
            Exit;
        end;

        LineCodeObj := ElementList.Next;
    end;

    DoSimpleMsg('Linecode: "' + Value + '" not Found.', 103);

end;


//- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
//      TLineCode Obj
//- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

constructor TLineCodeObj.Create(ParClass: TDSSClass; const LineCodeName: String);

begin
    inherited Create(ParClass);
    Name := LowerCase(LineCodeName);
    DSSObjType := ParClass.DSSClassType;

    FNPhases := 3;  // Directly set conds and phases
    FNeutralConductor := FNphases;  // initialize to last conductor
    R1 := 0.0580;  //ohms per 1000 ft
    X1 := 0.1206;
    R0 := 0.1784;
    X0 := 0.4047;
    C1 := 3.4e-9;  // nf per 1000ft
    C0 := 1.6e-9;
    Z := NIL;
    Zinv := NIL;
    Yc := NIL;
    Basefrequency := ActiveCircuit[ActiveActor].Fundamental;
    Units := UNITS_NONE;  // default to none  (no conversion)
    Normamps := 400.0;
    EmergAmps := 600.0;
    PctPerm := 20.0;
    FaultRate := 0.1;

    Rg := 0.01805;  // ohms per 1000'
    Xg := 0.155081;
    rho := 100.0;

    SymComponentsModel := TRUE;
    ReduceByKron := FALSE;
    CalcMatricesFromZ1Z0;  // put some reasonable values in

    InitPropertyValues(0);
end;

//- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
destructor TLineCodeObj.Destroy;
begin
    Z.Free;
    Zinv.Free;
    Yc.Free;
    inherited destroy;
end;

//- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
procedure TLineCodeObj.Set_NPhases(Value: Integer);
// Set the number of phases and reallocate phase-sensitive arrays
// Need to preserve values in Z matrices

begin
    if Value > 0 then
    begin
        if FNphases <> Value then
        begin    // If size is no different, we don't need to do anything
            FNPhases := Value;
            FNeutralConductor := FNphases;  // Init to last conductor
        // Put some reasonable values in these matrices
            CalcMatricesFromZ1Z0;  // reallocs matrices
        end;
    end;
end;

//- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
procedure TLineCodeObj.CalcMatricesFromZ1Z0;
var
    Zs, Zm, Ys, Ym, Ztemp: Complex;
    i, j: Integer;
    Yc1, Yc0, OneThird: Double;

begin
    if Z <> NIL then
        Z.Free;
    if Zinv <> NIL then
        Zinv.Free;
    if Yc <> NIL then
        Yc.Free;

    // For a line, nphases = ncond, for now
    Z := TCmatrix.CreateMatrix(FNphases);
    Zinv := TCMatrix.CreateMatrix(FNphases);
    Yc := TCMatrix.CreateMatrix(FNphases);

    OneThird := 1.0 / 3.0;  // Do this to get more precision in next few statements

    Ztemp := CmulReal(cmplx(R1, X1), 2.0);
    Zs := CmulReal(CAdd(Ztemp, Cmplx(R0, X0)), OneThird);
    Zm := CmulReal(Csub(cmplx(R0, X0), Cmplx(R1, X1)), OneThird);

    Yc1 := TwoPi * BaseFrequency * C1;
    Yc0 := TwoPi * BaseFrequency * C0;

    Ys := CMulReal(Cadd(CMulReal(Cmplx(0.0, Yc1), 2.0), Cmplx(0.0, Yc0)), OneThird);
    Ym := CmulReal(Csub(cmplx(0.0, Yc0), Cmplx(0.0, Yc1)), OneThird);

    for i := 1 to FNphases do
    begin
        Z.SetElement(i, i, Zs);
        Yc.SetElement(i, i, Ys);
        for j := 1 to i - 1 do
        begin
            Z.SetElemsym(i, j, Zm);
            Yc.SetElemsym(i, j, Ym);
        end;
    end;
    Zinv.Copyfrom(Z);
    Zinv.Invert;
end;

procedure TLineCodeObj.DumpProperties(var F: TextFile; Complete: Boolean);

var
    i, j: Integer;

begin
    inherited DumpProperties(F, Complete);

    with ParentClass do
    begin

        Writeln(F, '~ ', PropertyName^[1], '=', FNphases: 0);
        Writeln(F, '~ ', PropertyName^[2], '=', R1: 0: 5);
        Writeln(F, '~ ', PropertyName^[3], '=', X1: 0: 5);
        Writeln(F, '~ ', PropertyName^[4], '=', R0: 0: 5);
        Writeln(F, '~ ', PropertyName^[5], '=', X0: 0: 5);
        Writeln(F, '~ ', PropertyName^[6], '=', C1 * 1.0e9: 0: 5);
        Writeln(F, '~ ', PropertyName^[7], '=', C0 * 1.0e9: 0: 5);
        Writeln(F, '~ ', PropertyName^[8], '=', PropertyValue[8]);
        Write(F, '~ ', PropertyName^[9], '=', '"');
        for i := 1 to FNPhases do
        begin
            for j := 1 to FNphases do
            begin
                Write(F, Z.GetElement(i, j).re: 0: 8, ' ');
            end;
            Write(F, '|');
        end;
        Writeln(F, '"');
        Write(F, '~ ', PropertyName^[10], '=', '"');
        for i := 1 to FNPhases do
        begin
            for j := 1 to FNphases do
            begin
                Write(F, Z.GetElement(i, j).im: 0: 8, ' ');
            end;
            Write(F, '|');
        end;
        Writeln(F, '"');
        Write(F, '~ ', PropertyName^[11], '=', '"');
        for i := 1 to FNPhases do
        begin
            for j := 1 to FNphases do
            begin
                Write(F, (Yc.GetElement(i, j).im / TwoPi / BaseFrequency * 1.0E9): 0: 8, ' ');
            end;
            Write(F, '|');
        end;
        Writeln(F, '"');


        for i := 12 to 21 do
        begin
            Writeln(F, '~ ', PropertyName^[i], '=', PropertyValue[i]);
        end;

        Writeln(F, Format('~ %s=%d', [PropertyName^[22], FNeutralConductor]));

    end;

end;

function TLineCodeObj.GetPropertyValue(Index: Integer): String;
begin
    case Index of
        1:
            Result := Format('%d', [FnPhases]);
        2:
            if SymComponentsModel then
                Result := Format('%.5g', [R1])
            else
                Result := '----';
        3:
            if SymComponentsModel then
                Result := Format('%.5g', [X1])
            else
                Result := '----';
        4:
            if SymComponentsModel then
                Result := Format('%.5g', [R0])
            else
                Result := '----';
        5:
            if SymComponentsModel then
                Result := Format('%.5g', [X0])
            else
                Result := '----';
        6:
            if SymComponentsModel then
                Result := Format('%.5g', [C1 * 1.0e9])
            else
                Result := '----';
        7:
            if SymComponentsModel then
                Result := Format('%.5g', [C0 * 1.0e9])
            else
                Result := '----';
        8:
            Result := LineUnitsStr(Units);
        9:
            Result := Get_Rmatrix;
        10:
            Result := Get_Xmatrix;
        11:
            Result := get_Cmatrix;
        12:
            Result := Format('%.g', [Basefrequency]); //  was defaultbasefrequency ??? 'baseFreq';
        18:
            if ReduceByKron then
                Result := 'Y'
            else
                Result := 'N';
        19:
            Result := Format('%.5g', [Rg]);
        20:
            Result := Format('%.5g', [Xg]);
        21:
            Result := Format('%.5g', [Rho]);
        22:
            Result := IntToStr(FNeutralConductor);
        23:
            if SymComponentsModel then
                Result := Format('%.5g', [twopi * Basefrequency * C1 * 1.0e6])
            else
                Result := '----';
        24:
            if SymComponentsModel then
                Result := Format('%.5g', [twopi * Basefrequency * C0 * 1.0e6])
            else
                Result := '----';
    else
        Result := inherited GetPropertyValue(index);
    end;
end;

procedure TLineCodeObj.InitPropertyValues(ArrayOffset: Integer);
begin

    PropertyValue[1] := '3'; // 'nphases';
    PropertyValue[2] := '.058'; // 'r1';
    PropertyValue[3] := '.1206'; // 'x1';
    PropertyValue[4] := '0.1784'; // 'r0';
    PropertyValue[5] := '0.4047'; // 'x0';
    PropertyValue[6] := '3.4'; // 'c1';
    PropertyValue[7] := '1.6'; // 'c0';
    PropertyValue[8] := 'none'; // 'units';
    PropertyValue[9] := ''; // 'rmatrix';
    PropertyValue[10] := ''; // 'xmatrix';
    PropertyValue[11] := ''; // 'cmatrix';
    PropertyValue[12] := Format('%6.1f', [DefaultBaseFreq]); // 'baseFreq';
    PropertyValue[13] := '400'; // 'normamps';
    PropertyValue[14] := '600'; // 'emergamps';
    PropertyValue[15] := '0.1'; // 'faultrate';
    PropertyValue[16] := '20'; // 'pctperm';
    PropertyValue[17] := '3'; // 'Hrs to repair';
    PropertyValue[18] := 'N'; // 'Kron';
    PropertyValue[19] := '.01805'; // 'Rg';
    PropertyValue[20] := '.155081'; // 'Xg';
    PropertyValue[21] := '100'; // 'rho';
    PropertyValue[22] := '3'; // 'Neutral';
    PropertyValue[23] := '1.2818'; // B1  microS
    PropertyValue[24] := '0.60319'; // B0  microS

    inherited  InitPropertyValues(NumPropsThisClass);

end;

procedure TLineCodeObj.DoKronReduction;
var
    NewZ, NewYC: TcMatrix;

begin
    if FneutralConductor = 0 then
        Exit;   // Do Nothing

    NewZ := NIL;
    NewYC := NIL;

    if Fnphases > 1 then
    begin
        try
            NewZ := Z.Kron(FNeutralConductor);       // Perform Kron Reductions into temp space
        { Have to invert the Y matrix to eliminate properly}
            YC.Invert;  // Vn = 0 not In
            NewYC := YC.Kron(FNeutralConductor);
        except
            On E: Exception do
                DoSimpleMsg(Format('Kron Reduction failed: LineCode.%s. Attempting to eliminate Neutral Conductor %d.', [Name, FNeutralConductor]), 103);
        end;

        // Reallocate into smaller space   if Kron was successful

        if (NewZ <> NIL) and (NewYC <> NIL) then
        begin

            NewYC.Invert;  // Back to Y

            Numphases := NewZ.order;

            // Get rid of Z and YC and replace
            Z.Free;
            YC.Free;

            Z := NewZ;
            YC := NewYC;

            FNeutralConductor := 0;
            ReduceByKron := FALSE;

            {Change Property values to reflect Kron reduction for save circuit function}
            PropertyValue[1] := Format('%d', [FnPhases]);
            PropertyValue[9] := get_Rmatrix;
            PropertyValue[10] := get_Xmatrix;
            PropertyValue[11] := get_Cmatrix;

        end
        else
        begin
            DoSimpleMsg(Format('Kron Reduction failed: LineCode.%s. Attempting to eliminate Neutral Conductor %d.', [Name, FNeutralConductor]), 103);
        end;

    end
    else
    begin
        DoSimpleMsg('Cannot perform Kron Reduction on a 1-phase LineCode: LineCode.' + Name, 103);
    end;
    ;

end;


end.
