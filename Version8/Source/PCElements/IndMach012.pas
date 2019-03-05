unit IndMach012;

// Symmetrical component Induction Machine model

//    ************  DRAFT Version 2 ******************************

{
  ----------------------------------------------------------
  Copyright (c) 2008-2017, Electric Power Research Institute, Inc.
  All rights reserved.
  ----------------------------------------------------------
}

{   Change Log

   November 10, 2016

   Created by
     Andres Ovalle
     Celso Rocha

}

{
   Description

   This is a Power Converstion (PC) element.

   PC elements are Load, Generator, Vsource, Isource, etc. PC elements are
   used to model devices that convert the power delivered by the Power Delivery (PD)
   elements into some other form.  PC elements are generally considered to be
   in shunt with the power system and are the terminations of the power flow
   while PD elements are considered to be in series with the power flow path.

   Both PC and PD elements are represpented by their primitive Y matrices. PC elements
   are also used to model the nonlinear devices in the system (see the Load model). They
   differ from PD elements in that they have a current injection source in parallel with
   the primitive Y matrix.

}


interface

{Add other modules accessed by this class}

uses
    DSSClass,   // Base class for most DSS objects
    PCClass,    // Base class for collection manager for PC elements
    PCElement,  // Base class for PC  Elements
    ucmatrix,     // Unit for managing complex matrice (for Yprim, etc)
    ucomplex,     // Complex math functions, type definitions
    ArrayDef,     // definitions of basic DSS arrays

    // common modules used in PC elements
    LoadShape,    // class for supporting/representing loadshapes
    GrowthShape,  // Class for holding growth shapes
    Spectrum,     // Definitions for harmonic spectra
    Dynamics,
    GeneratorVars;     // for elements that interact with dynamics variables


type

// = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = =

{ Collection manager for this class of element }
    TIndMach012 = class(TPCClass)   { Notes Andres: -- definition of the class -- }
    PRIVATE

      {These private functions are generally helper functions for Edit procedure}

      { A typical function }
        procedure SetNcondsForConnection;

    PROTECTED
        procedure DefineProperties;    // Define the property names and help strings
        function MakeLike(const OtherIndMach012Name: String): Integer; OVERRIDE;  // copy properties of another similar object

    PUBLIC

        constructor Create;
        destructor Destroy; OVERRIDE;

        function Edit(ActorID: Integer): Integer; OVERRIDE;      // Definition of the main property editing function
        function Init(Handle: Integer; ActorID: Integer): Integer; OVERRIDE;  // Initialize by handle (index), if necessary


        function NewObject(const ObjName: String): Integer; OVERRIDE; // This function is called by the DSS New command

     {any public functions that might be called from other elements}

    end;

// = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = =

{ Class definition for this class of element}
    TSymCompArray = array[0..2] of Complex;
    //pTDynamicsRec =  ^TDynamicsRec;
    //pTGeneratorVars = ^TGeneratorVars;

    TIndMach012Obj = class(TPCElement)
    PRIVATE

      {Private variables of this class}
        Connection: Integer;  {0 = line-neutral; 1=Delta}
        Yeq: Complex;   // Y at nominal voltage

        puRs, puXs, puRr, puXr, puXm,
        S1,        // Pos seq slip
        S2,
        MaxSlip,  // limit for slip to prevent solution blowing up
        dSdP,  // for power flow

        {Dynamics variables}
        Xopen,
        Xp,
        T0p // Rotor time constant
        : Double;

        InDynamics: Boolean;

        Zs, Zm, Zr,
        Is1, Ir1, V1,    // Keep the last computed voltages and currents
        Is2, Ir2, V2: Complex;

        {Complex variables for dynamics}
        E1, E1n, dE1dt, dE1dtn,
        E2, E2n, dE2dt, dE2dtn,
        Zsp: Complex;

        FirstIteration, FixedSlip: Boolean;

        RandomMult: Double;
        IndMach012SolutionCount: Integer;
        IndMach012SwitchOpen: Boolean;

        // Debugging
        TraceFile: TextFile;
        DebugTrace: Boolean;

        MachineData: TGeneratorVars;    // Use generator variable structure

        // Andres: NEW variables from generator
        MachineON: Boolean;
        ShapeFactor: Complex;

        ShapeIsActual: Boolean;
        // Andres: end NEW variables from generator

        VBase: Double;
        kWBase: Double;

        procedure InterpretOption(s: String);

        procedure set_Localslip(const Value: Double);

        procedure Get_PFlowModelCurrent(const V: Complex; const S: Double; var Istator, Irotor: Complex);
        procedure Get_DynamicModelCurrent;
        procedure Set_Slip(const Value: Double);

        function GetRotorLosses: Double;
        function GetStatorLosses: Double;
        function Compute_dSdP: Double;
        procedure Randomize(Opt: Integer);
        procedure InitModel(V012, I012: TSymCompArray);

        procedure CalcYPrimMatrix(Ymatrix: TcMatrix; ActorID: Integer);
        procedure CalcIndMach012ModelContribution(ActorID: Integer);
        procedure CalcInjCurrentArray(ActorID: Integer);

        procedure DoIndMach012Model(ActorID: Integer);

        procedure CalcModel(V, I: pComplexArray; ActorID: Integer);

        // Andres: NEW procedures from generator
        procedure CalcDailyMult(Hr: Double);
        procedure CalcYearlyMult(Hr: Double);
        procedure CalcDutyMult(Hr: Double);
        // Andres: NEW procedures from generator

        procedure InitTraceFile;
        procedure WriteTraceRecord(ActorID: Integer);
        function Get_PresentkV: Double;
        procedure Set_PresentkV(const Value: Double);

        procedure SetPowerkW(const PkW: Double);

    PROTECTED

        {A couple of virtual procedures you can override}
        procedure Set_ConductorClosed(Index: Integer; ActorID: Integer; Value: Boolean); OVERRIDE;
        procedure GetTerminalCurrents(Curr: pComplexArray; ActorID: Integer); OVERRIDE;

        procedure DoDynamicMode(ActorID: Integer);
        procedure DoHarmonicMode(ActorID: Integer);

    PUBLIC

        {Variables and functions accessed by DSS and other objects}

        // Andres: new variables from generator
        DailyDispShape: String;  // Daily (24 HR) Generator shape
        DailyDispShapeObj: TLoadShapeObj;  // Daily Generator Shape for this load
        DutyShapeObj: TLoadShapeObj;  // Shape for this generator
        DutyShape: String;  //
        YearlyShape: String;  // ='fixed' means no variation  on all the time
        YearlyShapeObj: TLoadShapeObj;  // Shape for this Generator
        // Andres: New variables from generator

        constructor Create(ParClass: TDSSClass; const IndMach012ObjName: String);
        destructor Destroy; OVERRIDE;

        procedure RecalcElementData(ActorID: Integer); OVERRIDE;   // Generally called after Edit is complete to recompute variables
        procedure CalcYPrim(ActorID: Integer); OVERRIDE;   // Calculate Primitive Y matrix
        procedure Integrate(ActorID: Integer);
        procedure CalcDynamic(var V012, I012: TSymCompArray);
        procedure CalcPFlow(var V012, I012: TSymCompArray);
        procedure SetNominalPower(ActorID: Integer);

        // Injection current management functions (unique to PC Elements)
          // This is how the DSS represents elements with nonlinear characteristics
        // Inj currents are the difference between the desired total terminal currents and the
        // currents that result from the linear admittance matrix of the element
        function InjCurrents(ActorID: Integer): Integer; OVERRIDE;
        procedure GetInjCurrents(Curr: pComplexArray; ActorID: Integer); OVERRIDE;

          // State variable management functions, if any
        // You can omit these if your PC element model is not using these
        // Default behavior is to basically do nothing
        function NumVariables: Integer; OVERRIDE;
        procedure GetAllVariables(States: pDoubleArray); OVERRIDE;
        function Get_Variable(i: Integer): Double; OVERRIDE;
        procedure Set_Variable(i: Integer; Value: Double); OVERRIDE;
        function VariableName(i: Integer): String; OVERRIDE;

        // Support for Dynamics Mode
        procedure InitStateVars(ActorID: Integer); OVERRIDE;
        procedure IntegrateStates(ActorID: Integer); OVERRIDE;

        // Support for Harmonics Mode
        procedure InitHarmonics(ActorID: Integer); OVERRIDE;

        procedure MakePosSequence(ActorID: Integer); OVERRIDE;  // Make a positive Sequence Model, if possible

       // Functions required for managing values of properties
        procedure InitPropertyValues(ArrayOffset: Integer); OVERRIDE;
        procedure DumpProperties(var F: TextFile; Complete: Boolean); OVERRIDE;
        function GetPropertyValue(Index: Integer): String; OVERRIDE;

        property LocalSlip: Double READ S1 WRITE set_Localslip;
        property Slip: Double WRITE Set_Slip;
        property PresentkV: Double READ Get_PresentkV WRITE Set_PresentkV;

       //Property Variable[i:Integer]:Double Read Get_Variable Write Set_Variable;

       {Put any class properties here}
       {Use properties when some method must be executed when a value is set or retrieved}

       {   Example (from Load)
         Property ConnectedkVA        :Double Read FConnectedkVA        Write Set_ConnectedkVA;
       }

    end;

var
    IndMach012Class: TIndMach012;
    ActiveIndMach012Obj: TIndMach012Obj;

// = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = =
implementation

{Typical Uses Clause -- not all may not be needed}
uses
    ParserDel,     // DSS parser
    DSSClassDefs,  // Where class is instantiated
    DSSGlobals,    // Global DSS variables
    Circuit,       // If access to circuit variables is needed
    Command,       // DSS command and property support module
    Sysutils,      // Delphi misc utility functions
    Math,          // Delphi Math functions
    MathUtil,      // DSS Math utilities
    Utilities;     // DSS misc utility functions

const
    NumPropsThisClass = 21; // Set this constant to the actual number of properties you define
    NumIndMach012Variables = 22;


var  // Define any useful module vars here, for example:
    cBuffer: array[1..24] of Complex;  // Temp buffer for complex math calcs; allows up to 24-phase models.
    CDOUBLEONE: Complex;   // 1 + j1  (see Initialization section below)


function CmplxArrayToString(cpxarray: pComplexArray; count: Integer): String;
// Put array values in brackets separated by commas.
// Special version that appends magnitude and angle.

var
    i: Integer;

    procedure AppendMagAngle;
    begin
        Result := Result + Format(' (%.6g, %.5g)', [Cabs(cpxarray^[i]), Cdang(cpxarray^[i])]);
    end;

begin

    Result := '[NULL]';
    if count > 0 then
    begin
        Result := Format('[%.6g +j %.6g', [cpxarray^[1].re, cpxarray^[1].im]);
        i := 1;
        AppendMagAngle;
        for i := 2 to count do
        begin
            Result := Result + Format(', %.6g +j %.6g', [cpxarray^[i].re, cpxarray^[i].im]);
            AppendMagAngle;
        end;
        Result := Result + ']';
    end;

end;

//- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
constructor TIndMach012.Create;  // Creates main collection handler for all IndMach012 objects
begin
    inherited Create;  // make the base class  and init DSSClassType

     // Specify class name and bit mask ID for this class type
     // IndMach012_ELEMENT must be defined in DSSClassDefs as nn*8
     // First 3 bits are used for base class type (DSSClassType)
    Class_Name := 'IndMach012';
    DSSClassType := DSSClassType + INDMACH012_ELEMENT;

    ActiveElement := 0;   // no active elements yet; init to 0

     {Initialize any other special variables here}

    DefineProperties;   // This is where the properties for this class are defined

     // Use the Command processor to manage property names
     // PropertyName is an array of String defined in DefineProperties
    CommandList := TCommandList.Create(Slice(PropertyName^, NumProperties));
    CommandList.Abbrev := TRUE;

    IndMach012Class := Self;
end;

//- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
destructor TIndMach012.Destroy;

begin

    // ElementList and  CommandList freed in inherited destroy
    inherited Destroy;

end;

//- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
procedure TIndMach012.DefineProperties;

// This is where the properties are defined, assigned names, indexes, and help strings
// The Help strings will automatically show up when the Help is invoked

begin

    Numproperties := NumPropsThisClass;
    CountProperties;   // Get inherited property count
    AllocatePropertyArrays;   {see DSSClass}

     // Refer to other classes for alternative methods of assigning properties
     // This example uses the AddProperty function to assign Name, Index, and Help string
     // in one statement.

     // First argument is string name of the property
     // Second argument is the index for the CASE statement
     // Third argument is help string

     // DSS properties are accessed in sequence if the property name is not explicitly specified.
     // The advantage of using the AddProperty function is that you may change the sequence simply
     // by shuffling the order of the definitions and you do not have to change the index in the CASE
     // statement in the EDIT function


    PropertyName[1] := 'phases';
    PropertyName[2] := 'bus1';
    PropertyName[3] := 'kv';
    PropertyName[4] := 'kW';
    PropertyName[5] := 'pf';
    PropertyName[6] := 'conn';
    PropertyName[7] := 'kVA';
    PropertyName[8] := 'H';
    PropertyName[9] := 'D';
    PropertyName[10] := 'puRs';
    PropertyName[11] := 'puXs';
    PropertyName[12] := 'puRr';
    PropertyName[13] := 'puXr';
    PropertyName[14] := 'puXm';
    PropertyName[15] := 'Slip';
    PropertyName[16] := 'MaxSlip';
    PropertyName[17] := 'SlipOption';
    PropertyName[18] := 'Yearly';
    PropertyName[19] := 'Daily';
    PropertyName[20] := 'Duty';
    PropertyName[21] := 'Debugtrace';

    PropertyHelp[1] := 'Number of Phases, this Induction Machine.  ';
    PropertyHelp[2] := 'Bus to which the Induction Machine is connected.  May include specific node specification.';
    PropertyHelp[3] := 'Nominal rated (1.0 per unit) voltage, kV. For 2- and 3-phase machines, specify phase-phase kV. ' +
        'Otherwise, specify actual kV across each branch of the machine. ' +
        'If wye (star), specify phase-neutral kV. ' +
        'If delta or phase-phase connected, specify phase-phase kV.';  // line-neutral voltage//  base voltage
    PropertyHelp[4] := 'Shaft Power, kW, for the Induction Machine.  A positive value denotes power for a load. ' + CRLF +
        'Negative value denotes an induction generator. ';
    PropertyHelp[5] := '[Read Only] Present power factor for the machine. ';
    PropertyHelp[6] := 'Connection of stator: Delta or Wye. Default is Delta.';
    PropertyHelp[7] := 'Rated kVA for the machine.';
    PropertyHelp[8] := 'Per unit mass constant of the machine.  MW-sec/MVA.  Default is 1.0.';
    PropertyHelp[9] := 'Damping constant.  Usual range is 0 to 4. Default is 1.0.  Adjust to get damping in Dynamics mode,';
    PropertyHelp[10] := 'Per unit stator resistance. Default is 0.0053.';
    PropertyHelp[11] := 'Per unit stator leakage reactance. Default is 0.106.';
    PropertyHelp[12] := 'Per unit rotor  resistance. Default is 0.007.';
    PropertyHelp[13] := 'Per unit rotor leakage reactance. Default is 0.12.';
    PropertyHelp[14] := 'Per unit magnetizing reactance.Default is 4.0.';
    PropertyHelp[15] := 'Initial slip value. Default is 0.007';
    PropertyHelp[16] := 'Max slip value to allow. Default is 0.1. Set this before setting slip.';
    PropertyHelp[17] := 'Option for slip model. One of {fixedslip | variableslip*  }';
    PropertyHelp[18] := 'LOADSHAPE object to use for yearly simulations.  Must be previously defined ' +
        'as a Loadshape object. Is set to the Daily load shape ' +
        ' when Daily is defined.  The daily load shape is repeated in this case. ' +
        'Set Status=Fixed to ignore Loadshape designation. ' +
        'Set to NONE to reset to no loadahape. ' +
        'The default is no variation.';
    PropertyHelp[19] := 'LOADSHAPE object to use for daily simulations.  Must be previously defined ' +
        'as a Loadshape object of 24 hrs, typically. ' +
        'Set Status=Fixed to ignore Loadshape designation. ' +
        'Set to NONE to reset to no loadahape. ' +
        'Default is no variation (constant) if not defined. ' +
        'Side effect: Sets Yearly load shape if not already defined.';
    PropertyHelp[20] := 'LOADSHAPE object to use for duty cycle simulations.  Must be previously defined ' +
        'as a Loadshape object.  Typically would have time intervals less than 1 hr. ' +
        'Designate the number of points to solve using the Set Number=xxxx command. ' +
        'If there are fewer points in the actual shape, the shape is assumed to repeat.' +
        'Set to NONE to reset to no loadahape. ' +
        'Set Status=Fixed to ignore Loadshape designation. ' +
        ' Defaults to Daily curve If not specified.';
    PropertyHelp[21] := '[Yes | No*] Write DebugTrace file.';


     { add properties here }

     // Finally, we have to pick up any properties that were inherited
    ActiveProperty := NumPropsThisClass;
    inherited DefineProperties;  // Add defs of inherited properties to bottom of list

     // You can optionally override default help string of an inherited property, for example
    PropertyHelp[NumPropsThisClass + 1] := 'Name of harmonic voltage or current spectrum for this IndMach012. ' +
        'Voltage behind Xd" for machine - default. Current injection for inverter. ' +
        'Default value is "default", which is defined when the DSS starts.';

end;

//- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
function TIndMach012.NewObject(const ObjName: String): Integer;

// This function is called  by the DSS whenever a New IndMach012... command is encountered

begin
    // Make a new IndMach012 and add it to IndMach012 class list
    with ActiveCircuit[ActiveActor] do
    begin
        ActiveCktElement := TIndMach012Obj.Create(Self, ObjName);
        Result := AddObjectToList(ActiveDSSObject[ActiveActor]);
    end;

end;

//- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
procedure TIndMach012.SetNcondsForConnection;

// This is a typical helper function found in many DSS circuit element class
// for defining the number of conductors per terminal (Nconds) based on Y or delta connection

begin

    with ActiveIndMach012Obj do
    begin
        case Connection of
            0:
                NConds := Fnphases;  // Neutral is not connected for induction machine
            1:
                case Fnphases of        // Delta connection
                    1, 2:
                        NConds := Fnphases + 1; // L-L and Open-delta
                else
                    NConds := Fnphases;    // no neutral for this connection
                end;
        end;
    end;

end;


//- - - - - - - - - - - - - MAIN EDIT FUNCTION  - - - - - - - - - - - - - - -
//----------------------------------------------------------------------------
function TIndMach012.Edit(ActorID: Integer): Integer;
//----------------------------------------------------------------------------

// This function is the heart of the property managment for this class

var     // Define some local vars for handling parser results

    ParamPointer: Integer;
    ParamName: String;
    Param: String;

// The Edit function starts where the Parser is presently pointing and
// manages the parsing of the rest of the command line in the parser.

// The DSS executive processes the command verb on the front of the line and
// then passes control to the appropriate Edit function

begin

  // set the present element active
  // and continue parsing with contents of Parser
    ActiveIndMach012Obj := ElementList.Active;
    ActiveCircuit[ActorID].ActiveCktElement := ActiveIndMach012Obj;

    Result := 0;

    with ActiveIndMach012Obj do
    begin
     // peel off the next token on the edit line
        ParamPointer := 0;
        ParamName := Parser[ActorID].NextParam;
        Param := Parser[ActorID].StrValue;

        while Length(Param) > 0 do
        begin
         // Find the index for the CASE statement
         // If property is not named, just increment the index to the next property
            if (Length(ParamName) = 0) then
                Inc(ParamPointer)
            else
                ParamPointer := CommandList.GetCommand(ParamName);

         // Update the PropertyValy for this property
         // Actual index is mapped via PropertyIdxMap array for this class
            if (ParamPointer > 0) and (ParamPointer <= NumProperties) then
                PropertyValue[PropertyIdxMap[ParamPointer]] := Param
            else
                DoSimpleMsg('Unknown parameter "' + ParamName + '" for IndMach012 "' + Name + '"', 560);

         // --------------- MAIN CASE STATEMENT ----------------------
            if ParamPointer > 0 then
         // since we used AddProperty function to define properties, have to
         // use PropertyIdxMap to map to the correct Case index
                case PropertyIdxMap[ParamPointer] of
                    0:
                        DoSimpleMsg('Unknown parameter "' + ParamName + '" for Object "' + Class_Name + '.' + Name + '"', 561);
                    1:
                        NPhases := Parser[ActorID].Intvalue; // num phases
                    2:
                        SetBus(1, param);
                    3:
                        PresentkV := Parser[ActorID].DblValue;
                    4:
                        kWBase := Parser[ActorID].DblValue;
                    5: ; // Do nothing; read only power factor    := Parser.DblValue;
                    6:
                        InterpretConnection(Parser[ActorID].StrValue);
                    7:
                        MachineData.kVArating := Parser[ActorID].DblValue;
                    8:
                        MachineData.Hmass := Parser[ActorID].DblValue;
                    9:
                        MachineData.D := Parser[ActorID].DblValue;
                    10:
                        puRs := Parser[ActorID].DblValue;
                    11:
                        puXs := Parser[ActorID].DblValue;
                    12:
                        puRr := Parser[ActorID].DblValue;
                    13:
                        puXr := Parser[ActorID].DblValue;
                    14:
                        puXm := Parser[ActorID].DblValue;
                    15:
                        Slip := Parser[ActorID].DblValue;
                    16:
                        MaxSlip := Parser[ActorID].DblValue;
                    17:
                        InterpretOption(Parser[ActorID].StrValue);
                    18:
                        YearlyShape := Param;
                    19:
                        DailyDispShape := Param;
                    20:
                        DutyShape := Param;
                    21:
                        DebugTrace := InterpretYesNo(Param);

                else
           // Handle Inherited properties
                    ClassEdit(ActiveIndMach012Obj, ParamPointer - NumPropsThisClass)
                end;

         // ---------------- SIDE EFFECTS CASE STATEMENT ---------------------
         // This case statment handles any side effects from setting a property
         // (for example, from Generator)
            if ParamPointer > 0 then
                case PropertyIdxMap[ParamPointer] of
                    1:
                        SetNcondsForConnection;  // Force Reallocation of terminal info
                    18:
                    begin
                        YearlyShapeObj := LoadShapeClass[ActorID].Find(YearlyShape);
                        if Assigned(YearlyShapeObj) then
                            with YearlyShapeObj do
                                if UseActual then
                                    SetPowerkW(MaxP);
                    end;
                    19:
                    begin
                        DailyDispShapeObj := LoadShapeClass[ActorID].Find(DailyDispShape);
                        if Assigned(DailyDispShapeObj) then
                            with DailyDispShapeObj do
                                if UseActual then
                                    SetPowerkW(MaxP);
                    end;
                    20:
                    begin
                        DutyShapeObj := LoadShapeClass[ActorID].Find(DutyShape);
                        if Assigned(DutyShapeObj) then
                            with DutyShapeObj do
                                if UseActual then
                                    SetPowerkW(MaxP);
                    end;
                else
                end;

         // Get next token off Parser and continue editing properties
            ParamName := Parser[ActorID].NextParam;
            Param := Parser[ActorID].StrValue;
        end;

     // After editing is complete, the typical next step is to call the RecalcElementData function
        RecalcElementData(ActorID);
        YprimInvalid[ActorID] := TRUE; // Setting this flag notifies the DSS that something has changed
                           // and the Yprim will have to be rebuilt

    end;

end;

//----------------------------------------------------------------------------
//----------------------------------------------------------------------------
function TIndMach012.MakeLike(const OtherIndMach012Name: String): Integer;

// This function should be defined to handle the Like property inherited from
// the base class.

// The function copies the essential properties of another object of this class

var
    OtherIndMach012: TIndMach012Obj;
    i: Integer;

begin
    Result := 0;
   {See if we can find this IndMach012 name in the present collection}
    OtherIndMach012 := Find(OtherIndMach012Name);
    if (OtherIndMach012 <> NIL)   // skip if not found
    then
        with ActiveIndMach012Obj do
        begin
       // You should first set the basic circuit element properties, for example
            if (Fnphases <> OtherIndMach012.Fnphases) then
            begin
                Nphases := OtherIndMach012.Fnphases;
                NConds := Fnphases;  // Forces reallocation of terminal stuff

                Yorder := Fnconds * Fnterms;
                YprimInvalid[ActiveActor] := TRUE;
            end;

            PresentkV := OtherIndMach012.PresentkV;
            kWBase := OtherIndMach012.kWBase;

            puRs := OtherIndMach012.puRs;
            puRr := OtherIndMach012.puRr;
            puXr := OtherIndMach012.puXr;
            puXm := OtherIndMach012.puXm;
            puXs := OtherIndMach012.puXs;
            MaxSlip := OtherIndMach012.MaxSlip;

            MachineData.kVArating := OtherIndMach012.MachineData.kVArating;
            MachineData.Hmass := OtherIndMach012.MachineData.Hmass;
            MachineData.D := OtherIndMach012.MachineData.D;

       // Do inherited properties
            ClassMakeLike(OtherIndMach012);

       // Finally initialize all the property value strings to be the same as
       // the copied element
            for i := 1 to ParentClass.NumProperties do
          // Skip read only properties
                if i <> 5 then
                    FPropertyValue^[i] := OtherIndMach012.FPropertyValue^[i];

            Result := 1;
        end
    else
        DoSimpleMsg('Error in Load MakeLike: "' + OtherIndMach012Name + '" Not Found.', 562);

end;

//----------------------------------------------------------------------------
function TIndMach012.Init(Handle: Integer; ActorID: Integer): Integer;
//----------------------------------------------------------------------------

// Optional function if you want to do anything to initialize objects of this class

var
    p: TIndMach012Obj;

begin

    if (Handle = 0) then
    begin  // init all
        p := elementList.First;
        while (p <> NIL) do
        begin
            p.Randomize(0);
            p := elementlist.Next;
        end;
    end
    else
    begin
        Active := Handle;
        p := GetActiveObj;
        p.Randomize(0);
    end;

    DoSimpleMsg('Need to implement TIndMach012.Init', -1);
    Result := 0;

end;


//------------------------- MAIN OBJECT CONSTRUCTOR ---------------------
constructor TIndMach012Obj.Create(ParClass: TDSSClass; const IndMach012ObjName: String);
//----------------------------------------------------------------------------
begin
    inherited create(ParClass);
    Name := LowerCase(IndMach012ObjName);
    DSSObjType := ParClass.DSSClassType; // Same as Parent Class

     // Set some basic circuit element properties
    Nphases := 3;  // typical DSS default for a circuit element
    Fnconds := 3;  // defaults to delta
    Yorder := 0;  // To trigger an initial allocation
    Nterms := 1;  // forces allocations of terminal quantities
    kWBase := 1000.0;

    YearlyShape := '';
    YearlyShapeObj := NIL;  // if YearlyShapeobj = nil then the load alway stays nominal * global multipliers
    DailyDispShape := '';
    DailyDispShapeObj := NIL;  // if DaillyShapeobj = nil then the load alway stays nominal * global multipliers
    DutyShape := '';
    DutyShapeObj := NIL;  // if DutyShapeobj = nil then the load alway stays nominal * global multipliers

    Debugtrace := FALSE;

    Yorder := Fnterms * Fnconds;
    ShapeIsActual := FALSE;
    IndMach012SwitchOpen := FALSE;

    Connection := 1;  // Delta Default

    MachineData.kVGeneratorBase := 12.47;

    MachineData.kVArating := kWBase * 1.2;
    with MachineData do
    begin
        Hmass := 1.0;       //  W-sec/VA rating
        Theta := 0.0;
        w0 := TwoPi * Basefrequency;
        Speed := 0.0;  // relative speed
        dSpeed := 0.0;
        D := 1.0;
        XRdp := 20.0;   // not used for indmach

           // newly added
        Conn := connection;
        NumPhases := Fnphases;
        NumConductors := Fnconds;
    end;

    {---- end note Andres: from dll model ----}

    {Typical machine impedance data}
    puRs := 0.0053;
    puXs := 0.106;
    puRr := 0.007;
    puXr := 0.12;
    puXm := 4.0;

      // Set slip local and make generator model agree
    MaxSlip := 0.1;  // 10% slip limit     - set this before setting slip
    Slip := 0.007;   // About 1 pu power
    FixedSlip := FALSE;  // Allow Slip to float to match specified power

    InDynamics := FALSE;

     // call the procedure to set the initial property string values
    InitPropertyValues(0);

     // Update anything that has to be calculated from property values
    RecalcElementData(ActiveActor);

end;


//----------------------------------------------------------------------------
destructor TIndMach012Obj.Destroy;
//----------------------------------------------------------------------------

// Free everything here that needs to be freed
// If you allocated anything, dispose of it here

begin

    inherited Destroy;   // This will take care of most common circuit element arrays, etc.

end;


//----------------------------------------------------------------------------
procedure TIndMach012Obj.RecalcElementData(ActorID: Integer);
//----------------------------------------------------------------------------

var
    Rs, Xs,
    Rr, Xr,
    Xm, ZBase: Double;

begin

    with MachineData do
    begin
        ZBase := Sqr(kVGeneratorBase) / kVArating * 1000.0;
        Conn := connection;
        NumPhases := Fnphases;
        NumConductors := Fnconds;
    end;


    Rs := puRs * ZBase;
    Xs := puXs * ZBase;
    Rr := puRr * ZBase;
    Xr := puXr * ZBase;
    Xm := puXm * ZBase;
    Zs := Cmplx(Rs, Xs);
    Zm := Cmplx(0.0, Xm);
    Zr := Cmplx(Rr, Xr);

    Xopen := Xs + Xm;
    Xp := Xs + (Xr * Xm) / (Xr + Xm);
    Zsp := Cmplx(Rs, Xp);
    //Yeq := Cinv(Zsp);   // for Yprim  for dynamics
    //Yeq := Cmplx(1.0/ZBase, -0.5/Zbase);   // vars are half the watts
    Yeq := Cmplx(0.0, -1.0 / ZBase);   // vars only for power flow
    T0p := (Xr + Xm) / (MachineData.w0 * Rr);

    dSdP := Compute_dSdP;

    Is1 := CZERO;
    V1 := CZERO;
    Is2 := CZERO;
    V2 := CZERO;

    FirstIteration := TRUE;

    Reallocmem(InjCurrent, SizeOf(InjCurrent^[1]) * Yorder);

    SetNominalPower(ActorID);

    if CompareText(YearlyShape, 'none') = 0 then
        YearlyShape := '';
    if CompareText(DailyDispShape, 'none') = 0 then
        DailyDispShape := '';
    if CompareText(DutyShape, 'none') = 0 then
        DutyShape := '';

    if YearlyShapeObj = NIL then
        if Length(YearlyShape) > 0 then
            DoSimpleMsg('WARNING! Yearly load shape: "' + YearlyShape + '" Not Found.', 563);
    if DailyDispShapeObj = NIL then
        if Length(DailyDispShape) > 0 then
            DoSimpleMsg('WARNING! Daily load shape: "' + DailyDispShape + '" Not Found.', 564);
    if DutyShapeObj = NIL then
        if Length(DutyShape) > 0 then
            DoSimpleMsg('WARNING! Duty load shape: "' + DutyShape + '" Not Found.', 565);

    SpectrumObj := SpectrumClass[ActorID].Find(Spectrum);
    if SpectrumObj = NIL then
        DoSimpleMsg('ERROR! Spectrum "' + Spectrum + '" Not Found.', 566);

    if DebugTrace then
        InitTraceFile;
end;


function TIndMach012obj.Get_PresentkV: Double;
begin
    Result := MachineData.kVGeneratorBase;
end;

procedure TIndMach012obj.Set_PresentkV(const Value: Double);
begin
    with MachineData do
    begin
        kVGeneratorBase := Value;
        case FNphases of
            2, 3:
                VBase := kVGeneratorBase * InvSQRT3x1000;
        else
            VBase := kVGeneratorBase * 1000.0;
        end;
    end;
end;

procedure TIndMach012obj.InterpretOption(s: String);
begin
    case Uppercase(s)[1] of
        'F':
            Fixedslip := TRUE;
        'V':
            Fixedslip := FALSE;
    else

    end;
end;

//----------------------------------------------------------------------------
procedure TIndMach012Obj.SetPowerkW(const PkW: Double);
begin
    kWBase := PkW;
end;

//----------------------------------------------------------------------------
//--------------------- MAIN CALC ROUTINES -----------------------------------

//----------------------------------------------------------------------------
procedure TIndMach012Obj.Integrate(ActorID: Integer);
//----------------------------------------------------------------------------

var
    h2: Double;

begin
    with  ActiveCircuit[ActorID].Solution.Dynavars do
    begin
        if IterationFlag = 0 then
        begin  // on predictor step
            E1n := E1;            // update old values
            dE1dtn := dE1dt;
            E2n := E2;
            dE2dtn := dE2dt;
        end;

     // Derivative of E
      // dEdt = -jw0SE' - (E' - j(X-X')I')/T0'
        dE1dt := Csub(cmul(cmplx(0.0, -MachineData.w0 * S1), E1), Cdivreal(Csub(E1, cmul(cmplx(0.0, (Xopen - Xp)), Is1)), T0p));
        dE2dt := Csub(cmul(cmplx(0.0, -MachineData.w0 * S2), E2), Cdivreal(Csub(E2, cmul(cmplx(0.0, (Xopen - Xp)), Is2)), T0p));

      // Trapezoidal Integration
        h2 := h * 0.5;
        E1 := Cadd(E1n, CmulReal(Cadd(dE1dt, dE1dtn), h2));
        E2 := Cadd(E2n, CmulReal(Cadd(dE2dt, dE2dtn), h2));
    end;

end;

//----------------------------------------------------------------------------
procedure TIndMach012Obj.CalcDynamic(var V012, I012: TSymCompArray);
//----------------------------------------------------------------------------
begin
      {In dynamics mode, slip is allowed to vary}
    InDynamics := TRUE;
    V1 := V012[1];   // Save for variable calcs
    V2 := V012[2];
      {Gets slip from shaft speed}
    with MachineData do
        LocalSlip := (-Speed) / w0;
    Get_DynamicModelCurrent;

     //  Get_ModelCurrent(V2, S2, Is2, Ir2);
    I012[1] := Is1;    // Save for variable calcs
    I012[2] := Is2;
    I012[0] := cmplx(0.0, 0.0);

end;


//----------------------------------------------------------------------------
procedure TIndMach012Obj.CalcPFlow(var V012, I012: TSymCompArray);
//----------------------------------------------------------------------------

var
    P_Error: Double;

begin
    V1 := V012[1];   // Save for variable calcs
    V2 := V012[2];

    InDynamics := FALSE;

    if FirstIteration then
    begin
        Get_PFlowModelCurrent(V1, S1, Is1, Ir1);  // initialize Is1
        FirstIteration := FALSE;
    end;

      {If Fixed slip option set, then use the value set by the user}
    if not FixedSlip then
    begin
        P_Error := MachineData.PnominalperPhase - Cmul(V1, Conjg(Is1)).re;
        LocalSlip := S1 + dSdP * P_Error;   // make new guess at slip
    end;

    Get_PFlowModelCurrent(V1, S1, Is1, Ir1);
    Get_PFlowModelCurrent(V2, S2, Is2, Ir2);

    I012[1] := Is1;    // Save for variable calcs
    I012[2] := Is2;
    I012[0] := cmplx(0.0, 0.0);

end;


//----------------------------------------------------------------------------
procedure TIndMach012Obj.Randomize(Opt: Integer);
//----------------------------------------------------------------------------

// typical proc for handling randomization in DSS fashion

begin
    case Opt of
        0:
            RandomMult := 1.0;
    //   GAUSSIAN:  RandomMult := Gauss(YearlyShapeObj.Mean, YearlyShapeObj.StdDev);
        UNIfORM:
            RandomMult := Random;  // number between 0 and 1.0
     //  LOGNORMAL: RandomMult := QuasiLognormal(YearlyShapeObj.Mean);
    end;
end;


{-------------------------------------------------------------------------------------------------------------}
procedure TIndMach012Obj.InitModel(V012, I012: TSymCompArray);
{-------------------------------------------------------------------------------------------------------------}

// Init for Dynamics mode

begin

   // Compute Voltage behind transient reactance and set derivatives to zero
  // *** already done *** E1 := csub(V012[1], cmul(I012[1], Zsp));
    dE1dt := czero;
    E1n := E1;
    dE1dtn := dE1dt;
    E2 := csub(V012[2], cmul(I012[2], Zsp));
    dE2dt := czero;
    E2n := E2;
    dE2dtn := dE2dt;

end;


//----------------------------------------------------------------------------
procedure TIndMach012Obj.InitStateVars(ActorID: Integer);
//----------------------------------------------------------------------------

var
    i: Integer;
    V012,
    I012: TSymCompArray;
    Vabc: array[1..3] of Complex;

begin

    YprimInvalid[ActorID] := TRUE;  // Force rebuild of YPrims

    with MachineData do
    begin

     {Compute nominal Positive sequence voltage behind transient reactance}

        if MachineON then
            with ActiveCircuit[ActorID].Solution do
            begin

                Yeq := Cinv(Zsp);

                ComputeIterminal(ActorID);

                case Fnphases of

                    1:
                    begin
                        E1 := Csub(CSub(NodeV^[NodeRef^[1]], NodeV^[NodeRef^[2]]), Cmul(ITerminal^[1], Zsp));
                    end;

                    3:
                    begin
                 // Calculate E1 based on Pos Seq only
                        Phase2SymComp(ITerminal, @I012);   // terminal currents

                     // Voltage behind Zsp  (transient reactance), volts

                        for i := 1 to FNphases do
                            Vabc[i] := NodeV^[NodeRef^[i]];   // Wye Voltage
                        Phase2SymComp(@Vabc, @V012);
                        E1 := Csub(V012[1], Cmul(I012[1], Zsp));    // Pos sequence
                    end;
                else
                    DoSimpleMsg(Format('Dynamics mode is implemented only for 1- or 3-phase Motors. IndMach012.' + name + ' has %d phases.', [Fnphases]), 5672);
                    SolutionAbort := TRUE;
                end;

                InitModel(V012, I012); // E2, etc

         // Shaft variables
                Theta := Cang(E1);
                dTheta := 0.0;
                w0 := Twopi * ActiveCircuit[ActorID].Solution.Frequency;
         // recalc Mmass and D in case the frequency has changed
                with MachineData do
                begin
                    Mmass := 2.0 * Hmass * kVArating * 1000.0 / (w0);   // M = W-sec
                    D := Dpu * kVArating * 1000.0 / (w0);
                end;
                Pshaft := Power[1, ActorID].re; // Initialize Pshaft to present power consumption of motor

                Speed := -LocalSlip * w0;    // relative to synch speed
                dSpeed := 0.0;

                if DebugTrace then     // Put in a separator record
                begin
                    Append(TraceFile);
                    Writeln(TraceFile);
                    Writeln(TraceFile, '*************** Entering Dynamics Mode ***********************');
                    Writeln(TraceFile);
                    Close(Tracefile);
                end;

            end
        else
        begin
            Theta := 0.0;
            dTheta := 0.0;
            w0 := 0;
            Speed := 0.0;
            dSpeed := 0.0;
        end;
    end;  {With}
end;

//----------------------------------------------------------------------------
procedure TIndMach012Obj.CalcYPrimMatrix(Ymatrix: TcMatrix; ActorID: Integer);

{A typical helper function for PC elements to assist in the computation
 of Yprim
}

var
    Y, Yij, Yadder: Complex;
    i, j: Integer;
    FreqMultiplier: Double;

begin

    FYprimFreq := ActiveCircuit[ActorID].Solution.Frequency;
    FreqMultiplier := FYprimFreq / BaseFrequency;  // ratio to adjust reactances for present solution frequency

    with  ActiveCircuit[ActorID].solution do
        if IsDynamicModel or IsHarmonicModel then
   // for Dynamics and Harmonics modes use constant equivalent Y
        begin
            if MachineON then
                Y := Yeq   // L-N value computed in initialization routines
            else
                Y := Cmplx(EPSILON, 0.0);

            if Connection = 1 then
                Y := CDivReal(Y, 3.0); // Convert to delta impedance
            Y.im := Y.im / FreqMultiplier;  // adjust for frequency
            Yij := Cnegate(Y);
            for i := 1 to Fnphases do
            begin
                case Connection of
                    0:
                    begin
                        Ymatrix.SetElement(i, i, Y);  // sets the element
                 {
                   Ymatrix.AddElement(Fnconds, Fnconds, Y);  // sums the element
                   Ymatrix.SetElemsym(i, Fnconds, Yij);
                 }
                    end;
                    1:
                    begin   {Delta connection}
                        Yadder := CmulReal(Y, 1.000001);  // to prevent floating delta
                        Ymatrix.SetElement(i, i, Cadd(Y, Yadder));   // add a little bit to diagonal
                        Ymatrix.AddElement(i, i, Y);  // put it in again
                        for j := 1 to i - 1 do
                            Ymatrix.SetElemsym(i, j, Yij);
                    end;
                end;
            end;
        end

        else
        begin

    //  Typical code for a regular power flow  model
    //  Borrowed from Generator object

       {Yeq is typically expected as the equivalent line-neutral admittance}

            Y := Yeq;  //     Yeq is L-N quantity

       // ****** Need to modify the base admittance for real harmonics calcs
            Y.im := Y.im / FreqMultiplier;

            case Connection of

                0:
                    with YMatrix do
                    begin // WYE
                        for i := 1 to Fnphases do
                        begin
                            SetElement(i, i, Y);
                     {
                     AddElement(Fnconds, Fnconds, Y);
                     SetElemsym(i, Fnconds, Yij);
                     }
                        end;
                    end;

                1:
                    with YMatrix do
                    begin  // Delta  or L-L
                        Y := CDivReal(Y, 3.0); // Convert to delta impedance
                        Yij := Cnegate(Y);
                        for i := 1 to Fnphases do
                        begin
                            j := i + 1;
                            if j > Fnconds then
                                j := 1;  // wrap around for closed connections
                            AddElement(i, i, Y);
                            AddElement(j, j, Y);
                            AddElemSym(i, j, Yij);
                        end;
                    end;
            end;
        end;  {ELSE IF Solution.mode}

end;

{--- Notes Andres: Added according to IndMach012.dll model }
function TIndMach012Obj.Compute_dSdP: Double;
begin
// dSdP based on rated slip and rated voltage
    V1 := Cmplx(MachineData.kvGeneratorBase * 1000.0 / 1.732, 0.0);
    if S1 <> 0.0 then
        Get_PFlowModelCurrent(V1, S1, Is1, Ir1);
    Result := S1 / Cmul(V1, Conjg(Is1)).Re;
end;

//----------------------------------------------------------------------------
procedure TIndMach012Obj.CalcYPrim(ActorID: Integer);

// Required routine to calculate the primitive Y matrix for this element

// This example uses a helper function (CalcYPrimMatrix) to keep the code
// here clean

var
    i: Integer;

begin

{
  There are three Yprim matrices that could be computed:

     YPrim_Series:  Used for zero-load solutions to initialize the first guess
     YPrim_Shunt:   Equivalent Y in shunt with power system
                    For PC elements, this is typically the main YPrim
     YPrim:         Generally the sum of the other two; the total YPrim
}

     // Typical PC Elements build only shunt Yprim
     // Also, build a dummy Yprim Series so that CalcVoltagebases does not fail

     // First clear present value; redefine if necessary
     // Note: Complex matrix (TcMatrix -- see uCmatrix.pas) is used for this
    if YprimInvalid[ActorID] then
    begin
        if YPrim_Shunt <> NIL then
            YPrim_Shunt.Free;
        YPrim_Shunt := TcMatrix.CreateMatrix(Yorder);
        if YPrim_Series <> NIL then
            Yprim_Series.Free;
        YPrim_Series := TcMatrix.CreateMatrix(Yorder);
        if YPrim <> NIL then
            YPrim.Free;
        YPrim := TcMatrix.CreateMatrix(Yorder);
    end

    else
    begin
        YPrim_Shunt.Clear;
        YPrim_Series.Clear;
        YPrim.Clear;
    end;


     // call helper routine to compute YPrim_Shunt
    CalcYPrimMatrix(YPrim_Shunt, ActorID);

     // Set YPrim_Series based on a small fraction of the diagonals of YPrim_shunt
     // so that CalcVoltages doesn't fail
     // This is just one of a number of possible strategies but seems to work most of the time
    for i := 1 to Yorder do
        Yprim_Series.SetElement(i, i, CmulReal(Yprim_Shunt.Getelement(i, i), 1.0e-10));

     // copy YPrim_shunt into YPrim; That's all that is needed for most PC Elements
    YPrim.CopyFrom(YPrim_Shunt);

     // Account for Open Conductors -- done in base class
    inherited CalcYPrim(ActorID);

end;

// - - - - - - - - - - - - - - - - - - - - - - - -- - - - - - - - - - - - - - - - - -

//----------------------------------------------------------------------------
procedure TIndMach012Obj.DoIndMach012Model(ActorID: Integer);
//----------------------------------------------------------------------------
{Compute total terminal Current }
var
    i: Integer;

begin

    CalcYPrimContribution(InjCurrent, ActorID);  // Init InjCurrent Array

    CalcModel(Vterminal, Iterminal, ActorID);

    set_ITerminalUpdated(TRUE, ActorID);

    for i := 1 to Nphases do
        Caccum(InjCurrent^[i], Cnegate(Iterminal^[i]));
    if (DebugTrace) then
        WriteTraceRecord(ActorID);

end;

procedure TIndMach012Obj.CalcModel(V, I: pComplexArray; ActorID: Integer); // given voltages returns currents

var
    V012, I012: TSymCompArray;

begin

    // Convert abc voltages to 012
    Phase2SymComp(V, @V012);

    // compute I012

    case ActiveCircuit[ActorID].Solution.DynaVars.SolutionMode of
        DYNAMICMODE:
        begin
            CalcDynamic(V012, I012);
        end;
    else  {All other modes are power flow modes}
    begin
        CalcPflow(V012, I012);
    end;
    end;

    SymComp2Phase(I, @I012);       // convert back to I abc

end;

//----------------------------------------------------------------------------
procedure TIndMach012Obj.DoDynamicMode(ActorID: Integer);
//----------------------------------------------------------------------------

{ This is an example taken from Generator illustrating how a PC element might
  handle Dynamics mode with a Thevenin equivalent

  Also illustrates the computation of symmetrical component values
}

{Compute Total Current and add into InjTemp}

var
    i: Integer;

begin

   // Start off by getting the current in the admittance branch of the model
    CalcYPrimContribution(InjCurrent, ActorID);  // Init InjCurrent Array

   {Inj = -Itotal (in) - Yprim*Vtemp}

    CalcModel(Vterminal, Iterminal, ActorID);

    set_ITerminalUpdated(TRUE, ActorID);
    for i := 1 to Nphases do
        Caccum(InjCurrent^[i], Cnegate(ITerminal^[i]));

end;


// - - - - - - - - - - - - - - - - - - - - - - - -- - - - - - - - - - - - - - - - - -
procedure TIndMach012Obj.DoHarmonicMode(ActorID: Integer);

{
  Example taken from Generator illustrating how a PC element might handle
  current calcs for Harmonics mode

  Note: Generator objects assume a Thevenin model (voltage behind and impedance)
        while Load objects assume the Spectrum applies to a Norton model injection current
}

{Compute Injection Current Only when in harmonics mode}

{Assumes spectrum is a voltage source behind subtransient reactance and YPrim has been built}
{Vd is the fundamental frequency voltage behind Xd" for phase 1}

var
    i: Integer;
    E: Complex;
    GenHarmonic: Double;

begin

   // Set the VTerminal array
    ComputeVterminal(ActorID);

    with ActiveCircuit[ActorID].Solution do
    begin
        GenHarmonic := Frequency / BaseFrequency; // harmonic based on the fundamental for this object
        // get the spectrum multiplier and multiply by the V thev (or Norton current for load objects)
      // ???  E := CmulReal(SpectrumObj.GetMult(GenHarmonic), VThevHarm); // Get base harmonic magnitude
      // ???  RotatePhasorRad(E, GenHarmonic, ThetaHarm);  // Time shift by fundamental frequency phase shift

        // Put the values in a temp complex buffer
        for i := 1 to Fnphases do
        begin
            cBuffer[i] := E;
            if i < Fnphases then
                RotatePhasorDeg(E, GenHarmonic, -120.0);  // Assume 3-phase IndMach012
        end;
    end;

   {Handle Wye Connection}
    if Connection = 0 then
        cbuffer[Fnconds] := Vterminal^[Fnconds];  // assume no neutral injection voltage

   // In this case the injection currents are simply Yprim(frequency) times the voltage buffer
   // Refer to Load.Pas for load-type objects
   {Inj currents = Yprim (E) }
    YPrim.MVMult(InjCurrent, @cBuffer);

end;


// - - - - - - - - - - - - - - - - - - - - - - - -- - - - - - - - - - - - - - - - - -
procedure TIndMach012Obj.CalcIndMach012ModelContribution(ActorID: Integer);

// Main dispatcher for computing PC Element currnts

// Calculates IndMach012 current and adds it properly into the injcurrent array
// routines may also compute ITerminal  (ITerminalUpdated flag)

begin
    set_ITerminalUpdated(FALSE, ActorID);
    with  ActiveCircuit[ActorID], ActiveCircuit[ActorID].Solution do
    begin
        if IsDynamicModel then
            DoDynamicMode(ActorID)
        else
        if IsHarmonicModel and (Frequency <> Fundamental) then
            DoHarmonicMode(ActorID)
        else
            DoIndMach012Model(ActorID);

    end; {WITH}

   {When this is done, ITerminal is up to date}

end;

// - - - - - - - - - - - - - - - - - - - - - - - -- - - - - - - - - - - - - - - - - -
procedure TIndMach012Obj.CalcInjCurrentArray(ActorID: Integer);
//----------------------------------------------------------------------------

// Main procedure for controlling computation of InjCurrent array

// InjCurrent is difference between currents in YPrim and total terminal current


begin

// You usually will want some logic like this

       // If the element is open, just zero the array and return
    if IndMach012SwitchOpen then
        ZeroInjCurrent

       // otherwise, go to a routine that manages the calculation
    else
        CalcIndMach012ModelContribution(ActorID);

end;

// - - - - - - - - - - - - - - - - - - - - - - - -- - - - - - - - - - - - - - - - - -
procedure TIndMach012Obj.GetTerminalCurrents(Curr: pComplexArray; ActorID: Integer);
//----------------------------------------------------------------------------

// This function controls the calculation of the total terminal currents

// Note that it only does something if the solution count has changed.
// Otherwise, Iterminal array already contains the currents


begin

    with ActiveCircuit[ActorID].Solution do
    begin
        if IterminalSolutionCount[ActorID] <> ActiveCircuit[ActorID].Solution.SolutionCount then
        begin     // recalc the contribution
          // You will likely want some logic like this
            if not IndMach012SwitchOpen then
                CalcIndMach012ModelContribution(ActorID);  // Adds totals in Iterminal as a side effect
        end;
        inherited GetTerminalCurrents(Curr, ActorID); // add in inherited contribution
    end;

end;

// - - - - - - - - - - - - - - - - - - - - - - - -- - - - - - - - - - - - - - - - - -
function TIndMach012Obj.InjCurrents(ActorID: Integer): Integer;
//----------------------------------------------------------------------------

// Required function for managing computing of InjCurrents

begin

    with ActiveCircuit[ActorID].Solution do
    begin

      // Generators and Loads use logic like this:
        if LoadsNeedUpdating then
            SetNominalPower(ActorID); // Set the nominal kW, etc for the type of solution being done

       // call the main function for doing calculation
        CalcInjCurrentArray(ActorID);          // Difference between currents in YPrim and total terminal current

      // If (DebugTrace) Then WriteTraceRecord;

       // Add into System Injection Current Array
        Result := inherited InjCurrents(ActorID);

    end;

end;

//----------------------------------------------------------------------------
procedure TIndMach012Obj.SetNominalPower(ActorID: Integer);
//----------------------------------------------------------------------------
// Set shaft power
var
    Factor: Double;
    MachineOn_Saved: Boolean;

begin
    MachineOn_Saved := MachineON;
    ShapeFactor := CDOUBLEONE;
    // Check to make sure the generation is ON
    with ActiveCircuit[ActorID], ActiveCircuit[ActorID].Solution do
    begin
        if not (IsDynamicModel or IsHarmonicModel) then     // Leave machine in whatever state it was prior to entering Dynamic mode
        begin
            MachineON := TRUE;   // Init to on then check if it should be off
        end;


        if not MachineON then
        begin
         // If Machine is OFF enter as tiny resistive load (.0001 pu) so we don't get divide by zero in matrix
            MachineData.Pnominalperphase := -0.1 * kWBase / Fnphases;
          // Pnominalperphase   := 0.0;
            MachineData.Qnominalperphase := 0.0;   // This really doesn't matter
        end
        else
        begin    // Generator is on, compute it's nominal watts and vars
            with Solution do

                case Mode of
                    SNAPSHOT:
                        Factor := 1.0;
                    DAILYMODE:
                    begin
                        Factor := 1.0;
                        CalcDailyMult(DynaVars.dblHour) // Daily dispatch curve
                    end;
                    YEARLYMODE:
                    begin
                        Factor := 1.0;
                        CalcYearlyMult(DynaVars.dblHour);
                    end;
                    DUTYCYCLE:
                    begin
                        Factor := 1.0;
                        CalcDutyMult(DynaVars.dblHour);
                    end;
                    GENERALTIME,   // General sequential time simulation
                    DYNAMICMODE:
                    begin
                        Factor := 1.0;
                                   // This mode allows use of one class of load shape
                        case ActiveCircuit[ActorID].ActiveLoadShapeClass of
                            USEDAILY:
                                CalcDailyMult(DynaVars.dblHour);
                            USEYEARLY:
                                CalcYearlyMult(DynaVars.dblHour);
                            USEDUTY:
                                CalcDutyMult(DynaVars.dblHour);
                        else
                            ShapeFactor := CDOUBLEONE     // default to 1 + j1 if not known
                        end;
                    end;
                    MONTECARLO1,
                    MONTEFAULT,
                    FAULTSTUDY:
                        Factor := 1.0;
                    MONTECARLO2,
                    MONTECARLO3,
                    LOADDURATION1,
                    LOADDURATION2:
                    begin
                        Factor := 1.0;
                        CalcDailyMult(DynaVars.dblHour);
                    end;
                    PEAKDAY:
                    begin
                        Factor := 1.0;
                        CalcDailyMult(DynaVars.dblHour);
                    end;
                    AUTOADDFLAG:
                        Factor := 1.0;
                else
                    Factor := 1.0
                end;

            if not (IsDynamicModel or IsHarmonicModel) then         //******
            begin
                if ShapeIsActual then
                    MachineData.Pnominalperphase := 1000.0 * ShapeFactor.re / Fnphases
                else
                    MachineData.Pnominalperphase := 1000.0 * kWBase * Factor * ShapeFactor.re / Fnphases;

                // cannot dispatch vars in induction machine
                // you get what you get

            end;
        end; {ELSE GenON}

    end;  {With ActiveCircuit}

   // If machine state changes, force re-calc of Y matrix
    if MachineON <> MachineOn_Saved then
        YprimInvalid[ActorID] := TRUE;

end;

// - - - - - - - - - - - - - - - - - - - - - - - -- - - - - - - - - - - - - - - - - -
procedure TIndMach012Obj.CalcDailyMult(Hr: Double);
//----------------------------------------------------------------------------

begin
    if (DailyDispShapeObj <> NIL) then
    begin
        ShapeFactor := DailyDispShapeObj.GetMult(Hr);
        ShapeIsActual := DailyDispShapeObj.UseActual;
    end
    else
        ShapeFactor := CDOUBLEONE;  // Default to no daily variation
end;

//----------------------------------------------------------------------------
procedure TIndMach012Obj.CalcDutyMult(Hr: Double);
//----------------------------------------------------------------------------

begin
    if DutyShapeObj <> NIL then
    begin
        ShapeFactor := DutyShapeObj.GetMult(Hr);
        ShapeIsActual := DutyShapeObj.UseActual;
    end
    else
        CalcDailyMult(Hr);  // Default to Daily Mult if no duty curve specified
end;

//----------------------------------------------------------------------------
procedure TIndMach012Obj.CalcYearlyMult(Hr: Double);
//----------------------------------------------------------------------------

begin
{Yearly curve is assumed to be hourly only}
    if YearlyShapeObj <> NIL then
    begin
        ShapeFactor := YearlyShapeObj.GetMult(Hr);
        ShapeIsActual := YearlyShapeObj.UseActual;
    end
    else
        ShapeFactor := CDOUBLEONE;  // Defaults to no variation

end;

// - - - - - - - - - - - - - - - - - - - - - - - -- - - - - - - - - - - - - - - - - -
procedure TIndMach012Obj.GetInjCurrents(Curr: pComplexArray; ActorID: Integer);
//----------------------------------------------------------------------------

// Gets the currents for the last solution performed

// Do not call anything that may change the basic element values from the last solution

var
    i: Integer;

begin

    CalcInjCurrentArray(ActorID);  // Difference between currents in YPrim and total current

    try    // an exception here generally means an array boundary overrun
   // Copy into buffer array
        for i := 1 to Yorder do
            Curr^[i] := InjCurrent^[i];

    except
        ON E: Exception do
            DoErrorMsg('IndMach012 Object: "' + Name + '" in GetInjCurrents function.',
                E.Message,
                'Current buffer not big enough.', 568);
    end;

end;
//= = =  = = = = = = = = = = = = = = = = = = = = = = = = = = = =

// - - - - - - - - - - - - - - - - - - - - - - - -- - - - - - - - - - - - - - - - - -
procedure TIndMach012Obj.DumpProperties(var F: TextFile; Complete: Boolean);
//----------------------------------------------------------------------------
{
 This procedure is require to respond to various commands such as Dump that
 write all the device's property values to a file.
}

var
    i, idx: Integer;

begin
    inherited DumpProperties(F, Complete);

    {Write out any specials here, usually preceded by a "!"}

    with ParentClass do
        for i := 1 to NumProperties do
        begin
            idx := PropertyIdxMap[i]; // Map to get proper index into property value array
            case idx of
          {Trap any specials here, such as values that are array properties, for example}
                34, 36:
                    Writeln(F, '~ ', PropertyName^[i], '=(', PropertyValue[idx], ')')
            else
                Writeln(F, '~ ', PropertyName^[i], '=', PropertyValue[idx]);
            end;
        end;

    Writeln(F);

end;


//----------------------------------------------------------------------------
procedure TIndMach012Obj.InitHarmonics(ActorID: Integer);
//----------------------------------------------------------------------------

{Procedure to initialize for Harmonics solution}

{This example is extracted from Generator and constructs a Thevinen equivalent.
 Refer to Load for how to do a Norton equivalent
 }

var
    E, Va: complex;
begin

    YprimInvalid[ActorID] := TRUE;  // Force rebuild of YPrims

(****
     GenFundamental := ActiveCircuit.Solution.Frequency ;  // Whatever the frequency is when we enter here.

     With GenVars Do Begin

         // Xd" is used for harmonics analysis for generators
         Yeq := Cinv(Cmplx(0.0, Xdpp));      // used for current calcs  Always L-N

         {Compute reference Thevinen voltage from phase 1 current}

         IF GenON Then
           Begin

             ComputeIterminal;  // Get present value of current

             With ActiveCircuit.solution Do
             Case Connection of
               0: Begin {wye - neutral is explicit}
                    Va := Csub(NodeV^[NodeRef^[1]], NodeV^[NodeRef^[Fnconds]]);
                  End;
               1: Begin  {delta -- assume neutral is at zero}
                    Va := NodeV^[NodeRef^[1]];
                  End;
             End;

             E         := Csub(Va, Cmul(Iterminal^[1], cmplx(0.0, Xdpp)));
             Vthevharm := Cabs(E);   // establish base mag and angle
             ThetaHarm := Cang(E);
           End
         ELSE  Begin
           // If Generator is off, just set to zero
             Vthevharm := 0.0;
             ThetaHarm := 0.0;
         End;
     End;
 ***)
end;

// ******************* PROPERTY VALUES   *******************

//----------------------------------------------------------------------------
procedure TIndMach012Obj.InitPropertyValues(ArrayOffset: Integer);
//----------------------------------------------------------------------------

// required procedure to initialize the string value of the properties

begin
   // Some examples
    PropertyValue[1] := '3';        //'phases';
    PropertyValue[2] := Getbus(1);  //'bus1';
    PropertyValue[3] := '12.47';
    PropertyValue[4] := '100';
    PropertyValue[5] := '.80';
    PropertyValue[6] := 'Delta';
    PropertyValue[7] := Format('%-g', [MachineData.kVARating]);
    PropertyValue[8] := Format('%-g', [MachineData.Hmass]);
    PropertyValue[9] := Format('%-g', [MachineData.D]);
    PropertyValue[10] := '0.0053';
    PropertyValue[11] := '0.106';
    PropertyValue[12] := '0.007';
    PropertyValue[13] := '0.12';
    PropertyValue[14] := '4.0';

    PropertyValue[15] := '0.007';
    PropertyValue[16] := '0.1';
    PropertyValue[17] := 'variable';

    PropertyValue[18] := '';
    PropertyValue[19] := '';
    PropertyValue[20] := '';     {...}
    PropertyValue[21] := 'NO';

{Call inherited function to init inherited property values}
    inherited  InitPropertyValues(NumPropsThisClass);

end;


//----------------------------------------------------------------------------
function TIndMach012Obj.GetPropertyValue(Index: Integer): String;
//----------------------------------------------------------------------------

// Return i-th property value as a string

begin

    Result := '';   // Init the string
    case Index of
         // Put special cases here
         // often a good idea to convert numeric values to strings, for example
        4:
            Result := Format('%.6g', [kWBase]);
        5:
            Result := Format('%.6g', [PowerFactor(Power[1, ActiveActor])]);
        7:
            Result := Format('%.6g', [MachineData.kVArating]);
        8:
            Result := Format('%.6g', [MachineData.Hmass]);
        9:
            Result := Format('%.6g', [MachineData.D]);
        15:
            Result := Format('%.6g', [localslip]);
        18:
            Result := YearlyShape;
        19:
            Result := DailyDispShape;
        20:
            Result := DutyShape;
         {...}
    else

         // The default is to just return the current string value of the property
        Result := inherited GetPropertyValue(index);

    end;
end;

// ******************* END PROPERTY VALUES   *******************


//----------------------------------------------------------------------------
procedure TIndMach012Obj.IntegrateStates(ActorID: Integer);
//----------------------------------------------------------------------------

{
  This is a virtual function. You do not need to write this routine
  if you are not integrating state variables in dynamics mode.
}

// Integrate state variables for Dynamics analysis
// Example from Generator

// Illustrates use of debug tracing

// Present technique is a predictor-corrector trapezoidal rule

var
    TracePower: Complex;


begin
   // Compute Derivatives and then integrate

    ComputeIterminal(ActorID);

    with ActiveCircuit[ActorID].Solution, MachineData do
    begin

        with DynaVars do
            if (IterationFlag = 0) then
            begin {First iteration of new time step}
                ThetaHistory := Theta + 0.5 * h * dTheta;
                SpeedHistory := Speed + 0.5 * h * dSpeed;
            end;

      // Compute shaft dynamics
        TracePower := TerminalPowerIn(Vterminal, Iterminal, FnPhases); // in watts
        dSpeed := (TracePower.re - Pshaft - abs(D * Speed)) / Mmass;
        dTheta := Speed;

     // Trapezoidal method
        with DynaVars do
        begin
            Speed := SpeedHistory + 0.5 * h * dSpeed;
            Theta := ThetaHistory + 0.5 * h * dTheta;
        end;

        if DebugTrace then
            WriteTraceRecord(ActorID);

        Integrate(ActorID);

    end;
end;

//----------------------------------------------------------------------------
procedure TIndMach012Obj.Get_DynamicModelCurrent;
//----------------------------------------------------------------------------
begin

    Is1 := Cdiv(Csub(V1, E1), Zsp); // I = (V-E')/Z'
    Is2 := Cdiv(Csub(V2, E2), Zsp); // I = (V-E')/Z'

    // rotor current  Ir1= Is1-Vm/jXm
    Ir1 := Csub(Is1, Cdiv(Csub(V1, cmul(Is1, Zsp)), Zm));
    Ir2 := Csub(Is2, Cdiv(Csub(V2, cmul(Is2, Zsp)), Zm));

end;

//----------------------------------------------------------------------------
procedure TIndMach012Obj.Get_PFlowModelCurrent(const V: Complex; const S: Double; var Istator, Irotor: Complex);
//----------------------------------------------------------------------------
var
    RL: Double;
    ZRotor, Numerator, Zmotor: Complex;

begin

    if s <> 0.0 then
        RL := Zr.re * (1.0 - s) / s
    else
        RL := Zr.re * 1.0e6;

    ZRotor := Cadd(Cmplx(RL, 0.0), Zr);
    Numerator := Cmul(Zm, Zrotor);
    Zmotor := Cadd(Zs, Cdiv(Numerator, Cadd(ZRotor, Zm)));
    Istator := Cdiv(V, Zmotor);
    {Ir = Is -(V-ZsIs)/Zm}
    Irotor := Csub(Istator, Cdiv(Csub(V, Cmul(Zs, Istator)), Zm));

end;

//----------------------------------------------------------------------------
// ********************** VARIABLES ***************************************
//----------------------------------------------------------------------------

//----------------------------------------------------------------------------
function TIndMach012Obj.NumVariables: Integer;
//----------------------------------------------------------------------------
{
  Return the number of state variables

  This is a virtual function. You do not need to write this routine
  if you are not defining state variables.
  Note: it is not necessary to define any state variables
}

begin
    Result := NumIndMach012Variables;
end;


//----------------------------------------------------------------------------
function TIndMach012Obj.VariableName(i: Integer): String;
//----------------------------------------------------------------------------

{
  Returns the i-th state variable in a string

  This is a virtual function. You do not need to write this routine
  if you are not defining state variables.
}

begin
    if i < 1 then
        Exit;  // This means Someone goofed
    case i of

        1:
            Result := 'Frequency';
        2:
            Result := 'Theta (deg)';
        3:
            Result := 'E1';
        4:
            Result := 'Pshaft';
        5:
            Result := 'dSpeed (deg/sec)';
        6:
            Result := 'dTheta (deg)';
        7:
            Result := 'Slip';
        8:
            Result := 'puRs';
        9:
            Result := 'puXs';
        10:
            Result := 'puRr';
        11:
            Result := 'puXr';
        12:
            Result := 'puXm';
        13:
            Result := 'Maxslip';
        14:
            Result := 'Is1';
        15:
            Result := 'Is2';
        16:
            Result := 'Ir1';
        17:
            Result := 'Ir2';
        18:
            Result := 'Stator Losses';
        19:
            Result := 'Rotor Losses';
        20:
            Result := 'Shaft Power (hp)';
        21:
            Result := 'Power Factor';
        22:
            Result := 'Efficiency (%)';
    else
    end;

end;

//----------------------------------------------------------------------------
function TIndMach012Obj.Get_Variable(i: Integer): Double;
//----------------------------------------------------------------------------
begin

    Result := -9999.99;   // Error Value

    with MachineData do
        case i of
            1:
                Result := (w0 + Speed) / TwoPi;  // Frequency, Hz
            2:
                Result := (Theta) * RadiansToDegrees;  // Report in Deg
            3:
                Result := Cabs(E1) / vbase;      // Report in pu
            4:
                Result := Pshaft;
            5:
                Result := dSpeed * RadiansToDegrees; // Report in Deg      57.29577951
            6:
                Result := dTheta;
            7:
                Result := LocalSlip;
            8:
                Result := puRs;
            9:
                Result := puXs;
            10:
                Result := puRr;
            11:
                Result := puXr;
            12:
                Result := puXm;
            13:
                Result := MaxSlip;
            14:
                Result := Cabs(Is1);
            15:
                Result := Cabs(Is2);
            16:
                Result := Cabs(Ir1);
            17:
                Result := Cabs(Ir2);
            18:
                Result := GetStatorLosses;
            19:
                Result := GetRotorLosses;
            20:
            begin  // Shaft Power  (hp)
                Result := 3.0 / 746.0 * (Sqr(Cabs(Ir1)) * (1.0 - S1) / S1 + Sqr(Cabs(Ir2)) * (1.0 - S2) / S2) * Zr.re;
            end;
            21:
                Result := PowerFactor(Power[1, ActiveActor]);
            22:
                Result := (1.0 - (GetStatorLosses + GetRotorLosses) / power[1, ActiveActor].re) * 100.0;    // Efficiency
        else

        end;

end;

//----------------------------------------------------------------------------
procedure TIndMach012Obj.Set_Variable(i: Integer; Value: Double);
//----------------------------------------------------------------------------
begin
    case i of

        7:
            Slip := Value;
        8:
            puRs := Value;
        9:
            puXs := Value;
        10:
            puRr := Value;
        11:
            puXr := Value;
        12:
            puXm := Value;

    else
        {Do Nothing for other variables: they are read only}
    end;
end;

//----------------------------------------------------------------------------
procedure TIndMach012Obj.GetAllVariables(States: pDoubleArray);
//----------------------------------------------------------------------------
{
  Return all state variables in double array (allocated by calling function)

  This is a virtual function. You do not need to write this routine
  if you are not defining state variables.
}
var
    i, N: Integer;
begin
    N := 0;
    for i := 1 to NumIndMach012Variables do
        States^[i] := Variable[i];
end;

// ********************** END VARIABLES ***************************************


//----------------------------------------------------------------------------
function TIndMach012Obj.GetRotorLosses: Double;
//----------------------------------------------------------------------------
begin
    Result := 3.0 * (Sqr(Ir1.re) + Sqr(Ir1.im) + Sqr(Ir2.re) + Sqr(Ir2.im)) * Zr.re;
end;

//----------------------------------------------------------------------------
function TIndMach012Obj.GetStatorLosses: Double;
//----------------------------------------------------------------------------
begin
    Result := 3.0 * (Sqr(Is1.re) + Sqr(Is1.im) + Sqr(Is2.re) + Sqr(Is2.im)) * Zs.re;
end;


//----------------------------------------------------------------------------
procedure TIndMach012Obj.MakePosSequence(ActorID: Integer);
//----------------------------------------------------------------------------

{
  This is a virtual function. You do not need to write this routine
  if the base class function will suffice.
}

// Routine to convert existing three-phase models to a single-phase positive-
// sequence model

var
    S: String;
    V: Double;

begin

{
     The usual technique is to create a new property editing string
     based on the present values of properties. Once the string is
     created, it is pushed into the Parser and the Edit routine for this
     class is invoked.

     Thus, the positive sequence model is created in memory. Do a
     "Save Circuit" command to save the model that is created. Some
     editing of the resulting scripts will likely be required. Not all
     elements have an obvious positive sequence equivalent.
}


 // example from Generator class
 // Modify as necessary

    S := 'Phases=1 conn=wye';    // Positive sequence model is 1-phase wye

  (****

  // Make sure voltage is line-neutral
  If (Fnphases>1) or (connection<>0) Then   V :=  GenVars.kVGeneratorBase/SQRT3
  Else V :=  GenVars.kVGeneratorBase;

  S := S + Format(' kV=%-.5g',[V]);

  // Divide the load by no. phases
  If Fnphases>1 Then
  Begin
      S := S + Format(' kW=%-.5g  PF=%-.5g',[kWbase/Fnphases, PFNominal]);
      If (PrpSequence^[19]<>0) or (PrpSequence^[20]<>0) Then S := S + Format(' maxkvar=%-.5g  minkvar=%-.5g',[kvarmax/Fnphases, kvarmin/Fnphases]);
      If PrpSequence^[26]>0 Then S := S + Format(' kva=%-.5g  ',[genvars.kvarating/Fnphases]);
      If PrpSequence^[27]>0 Then S := S + Format(' MVA=%-.5g  ',[genvars.kvarating/1000.0/Fnphases]);
  End;

  Parser.CmdString := S;   // Push the string into the Parser object
  Edit;    // Invoke the Edit method for this class

  inherited;  // sets the terminal bus references, must do after editing number of phases

  ***)

end;

//----------------------------------------------------------------------------
procedure TIndMach012Obj.Set_ConductorClosed(Index: Integer; ActorID: Integer; Value: Boolean);
//----------------------------------------------------------------------------

// Routine for handling Open/Close procedures

begin
    inherited;

    if Value then
        IndMach012SwitchOpen := FALSE
    else
        IndMach012SwitchOpen := TRUE;

end;


//----------------------------------------------------------------------------
procedure TIndMach012Obj.set_Localslip(const Value: Double);
//----------------------------------------------------------------------------

    function Sign(const x: Double): Double;
    begin
        if x < 0.0 then
            Result := -1.0
        else
            Result := 1.0;
    end;

begin
    S1 := Value;
    if not InDynamics then
        if Abs(S1) > MaxSlip then
            S1 := Sign(S1) * MaxSlip;   // Put limits on the slip  unless dynamics
    S2 := 2.0 - S1;
end;

//----------------------------------------------------------------------------
procedure TIndMach012Obj.Set_Slip(const Value: Double);
//----------------------------------------------------------------------------
begin
    LocalSlip := Value;
    MachineData.Speed := MachineData.w0 * (-S1); // make motor speed agree
end;

//----------------------------------------------------------------------------
procedure TIndMach012Obj.InitTraceFile;
//----------------------------------------------------------------------------
begin

    AssignFile(TraceFile, Format('%s_IndMach012_Trace.CSV', [Name]));
    Rewrite(TraceFile);

    Write(TraceFile, 'Time, Iteration, S1, |IS1|, |IS2|, |E1|, |dE1dt|, |E2|, |dE2dt|, |V1|, |V2|, Pshaft, Pin, Speed, dSpeed');
    Writeln(TraceFile);

    CloseFile(TraceFile);
end;

//----------------------------------------------------------------------------
procedure TIndMach012Obj.WriteTraceRecord(ActorID: Integer);
//----------------------------------------------------------------------------
begin
    Append(TraceFile);
    with ActiveCircuit[ActorID].Solution do
        Write(TraceFile, Format('%-.6g, %d, %-.6g, ', [Dynavars.dblHour * 3600.0, Iteration, S1]));

    Write(TraceFile, Format('%-.6g, %-.6g, ', [Cabs(Is1), Cabs(Is2)]));
    Write(TraceFile, Format('%-.6g, %-.6g, %-.6g, %-.6g, ', [Cabs(E1), Cabs(dE1dt), Cabs(E2), Cabs(dE2dt)]));
    Write(TraceFile, Format('%-.6g, %-.6g, ', [Cabs(V1), Cabs(V2)]));
    Write(TraceFile, Format('%-.6g, %-.6g, ', [MachineData.Pshaft, power[1, ActorID].re]));
    Write(TraceFile, Format('%-.6g, %-.6g, ', [MachineData.speed, MachineData.dSpeed]));

    Writeln(TraceFile);

    CloseFile(TraceFile);
end;

initialization

// Initialize any variables here


  // For Example:  1 + j 1

    CDOUBLEONE := CMPLX(1.0, 1.0);


end.
