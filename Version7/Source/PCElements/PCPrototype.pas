unit PCPrototype;

// Prototype for PC Element class

// Do a global Replace on "PCPrototype" to the name of your class to start

{
  ----------------------------------------------------------
  Copyright (c) 2008, Electric Power Research Institute, Inc.
  All rights reserved.
  ----------------------------------------------------------
}

{   Change Log

}

{
   Description

   This is a prototype of a Power Converstion (PC) element.

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
    Dynamics;     // for elements that interact with dynamics variables


type

// = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = =

{ Collection manager for this class of element }
    TPCPrototype = class(TPCClass)
    PRIVATE

      {These private functions are generally helper functions for Edit procedure}

      { A typical function }
        procedure SetNcondsForConnection;

    PROTECTED
        procedure DefineProperties;    // Define the property names and help strings
        function MakeLike(const OtherPCPrototypeName: String): Integer; OVERRIDE;  // copy properties of another similar object

    PUBLIC

        constructor Create;
        destructor Destroy; OVERRIDE;

        function Edit: Integer; OVERRIDE;      // Definition of the main property editing function 
        function Init(Handle: Integer): Integer; OVERRIDE;  // Initialize by handle (index), if necessary

        function NewObject(const ObjName: String): Integer; OVERRIDE; // This function is called by the DSS New command

     {any public functions that might be called from other elements}

    end;

// = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = =

{ Class definition for this class of element}
    TPCPrototypeObj = class(TPCElement)
    PRIVATE

      {Private variables of this class}
       // a typical private variable:
        Yeq: Complex;   // Y at nominal voltage

       // a typical procedure if user models are supported
        procedure DoUserModel;

       // many PC elements have a proc like this to map computed currents into appropriate array
        procedure StickCurrInTerminalArray(TermArray: pComplexArray; const Curr: Complex; i: Integer);

    PROTECTED

        {A couple of virtual procedures you can override}
        procedure Set_ConductorClosed(Index: Integer; Value: Boolean); OVERRIDE;
        procedure GetTerminalCurrents(Curr: pComplexArray); OVERRIDE;

    PUBLIC

        {Variables and functions accessed by DSS and other objects}

        constructor Create(ParClass: TDSSClass; const PCPrototypeObjName: String);
        destructor Destroy; OVERRIDE;

        procedure RecalcElementData; OVERRIDE;   // Generally called after Edit is complete to recompute variables
        procedure CalcYPrim; OVERRIDE;   // Calculate Primitive Y matrix 

        // Injection current management functions (unique to PC Elements)
          // This is how the DSS represents elements with nonlinear characteristics
        // Inj currents are the difference between the desired total terminal currents and the
        // currents that result from the linear admittance matrix of the element
        function InjCurrents: Integer; OVERRIDE;
        procedure GetInjCurrents(Curr: pComplexArray); OVERRIDE;

          // State variable management functions, if any
        // You can omit these if your PC element model is not using these
        // Default behavior is to basically do nothing
        function NumVariables: Integer; OVERRIDE;
        procedure GetAllVariables(States: pDoubleArray); OVERRIDE;
        function Get_Variable(i: Integer): Double; OVERRIDE;
        procedure Set_Variable(i: Integer; Value: Double); OVERRIDE;
        function VariableName(i: Integer): String; OVERRIDE;

        // Support for Dynamics Mode
        procedure InitStateVars; OVERRIDE;
        procedure IntegrateStates; OVERRIDE;

        // Support for Harmonics Mode
        procedure InitHarmonics; OVERRIDE;

        procedure MakePosSequence; OVERRIDE;  // Make a positive Sequence Model, if possible

       // Functions required for managing values of properties
        procedure InitPropertyValues(ArrayOffset: Integer); OVERRIDE;
        procedure DumpProperties(var F: TextFile; Complete: Boolean); OVERRIDE;
        function GetPropertyValue(Index: Integer): String; OVERRIDE;

       {Put any class properties here}
       {Use properties when some method must be executed when a value is set or retrieved}

       {   Example (from Load)
         Property ConnectedkVA        :Double Read FConnectedkVA        Write Set_ConnectedkVA;
       }

    end;

var
    ActivePCPrototypeObj: TPCPrototypeObj;

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
    NumPropsThisClass = 36; // Set this constant to the actual number of properties you define

var  // Define any useful module vars here, for example:
    cBuffer: array[1..24] of Complex;  // Temp buffer for complex math calcs; allows up to 24-phase models.
    CDOUBLEONE: Complex;   // 1 + j1  (see Initialization section below)

//- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
constructor TPCPrototype.Create;  // Creates main collection handler for all PCPrototype objects
begin
    inherited Create;  // make the base class  and init DSSClassType

     // Specify class name and bit mask ID for this class type
     // PCPROTOTYPE_ELEMENT must be defined in DSSClassDefs as nn*8
     // First 3 bits are used for base class type (DSSClassType)
    Class_Name := 'PCPrototype';
    DSSClassType := DSSClassType + PCPROTOTYPE_ELEMENT;

    ActiveElement := 0;   // no active elements yet; init to 0

     {Initialize any other special variables here}

    DefineProperties;   // This is where the properties for this class are defined

     // Use the Command processor to manage property names
     // PropertyName is an array of String defined in DefineProperties
    CommandList := TCommandList.Create(Slice(PropertyName^, NumProperties));
    CommandList.Abbrev := TRUE;
end;

//- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
destructor TPCPrototype.Destroy;

begin

    // ElementList and  CommandList freed in inherited destroy
    inherited Destroy;

end;

//- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
procedure TPCPrototype.DefineProperties;

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

     // Define Property names, for example (from Generator class)
    AddProperty('phases', 1, 'Number of Phases, this PCPrototype.  Power is evenly divided among phases.');
    AddProperty('bus1', 2, 'Bus to which the PCPrototype is connected.  May include specific node specification.');
    AddProperty('kv', 3, 'Nominal rated (1.0 per unit) voltage, kV, for PCPrototype. For 2- and 3-phase PCPrototypes, specify phase-phase kV. ' +
        'Otherwise, specify actual kV across each branch of the PCPrototype. ' +
        'If wye (star), specify phase-neutral kV. ' +
        'If delta or phase-phase connected, specify phase-phase kV.');  // line-neutral voltage//  base voltage
    AddProperty('kW', 4, 'Total base kW for the PCPrototype.  A positive value denotes power coming OUT of the element, ' + CRLF +
        'which is the opposite of a load. This value is modified depending on the dispatch mode. ' +
        'Unaffected by the global load multiplier and growth curves. ' +
        'If you want there to be more generation, you must add more PCPrototypes or change this value.');


                    {...}
                    {etc.}


     // Finally, we have to pick up any properties that were inherited
    ActiveProperty := NumPropsThisClass;
    inherited DefineProperties;  // Add defs of inherited properties to bottom of list

     // You can optionally override default help string of an inherited property, for example
    PropertyHelp[NumPropsThisClass + 1] := 'Name of harmonic voltage or current spectrum for this PCPrototype. ' +
        'Voltage behind Xd" for machine - default. Current injection for inverter. ' +
        'Default value is "default", which is defined when the DSS starts.';

end;

//- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
function TPCPrototype.NewObject(const ObjName: String): Integer;

// This function is called  by the DSS whenever a New PCPrototype... command is encountered

begin
    // Make a new PCPrototype and add it to PCPrototype class list
    with ActiveCircuit do
    begin
        ActiveCktElement := TPCPrototypeObj.Create(Self, ObjName);
        Result := AddObjectToList(ActiveDSSObject);
    end;
end;

//- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
procedure TPCPrototype.SetNcondsForConnection;

// This is a typical helper function found in many DSS circuit element class
// for defining the number of conductors per terminal (Nconds) based on Y or delta connection

begin
    with ActivePCPrototypeObj do
    begin
        case Connection of
            0:
                NConds := Fnphases + 1;  // Wye connection (has a neutral terminal)
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


//- - - - - - - - - - - - -MAIN EDIT FUNCTION  - - - - - - - - - - - - - - -
function TPCPrototype.Edit: Integer;

// This function is the heart of the property managment for this class

var     // Define some local vars for handling parser results
    i,
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
    ActivePCPrototypeObj := ElementList.Active;
    ActiveCircuit.ActiveCktElement := ActivePCPrototypeObj;

    Result := 0;

    with ActivePCPrototypeObj do
    begin
     // peel off the next token on the edit line
        ParamPointer := 0;
        ParamName := Parser.NextParam;
        Param := Parser.StrValue;

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
                DoSimpleMsg('Unknown parameter "' + ParamName + '" for PCPrototype "' + Name + '"', 560);

         // --------------- MAIN CASE STATEMENT ----------------------
            if ParamPointer > 0 then
         // since we used AddProperty function to define properties, have to
         // use PropertyIdxMap to map to the correct Case index
                case PropertyIdxMap[ParamPointer] of
                    0:
                        DoSimpleMsg('Unknown parameter "' + ParamName + '" for Object "' + Class_Name + '.' + Name + '"', 561);
                    1:
                        NPhases := Parser.Intvalue; // num phases
                    2:
                        SetBus(1, param);
                    3:
                        PresentkV := Parser.DblValue;

            {...}
            {etc.}

            {One case for each property}

                else
           // Handle Inherited properties
                    ClassEdit(ActivePCPrototypeObj, ParamPointer - NumPropsThisClass)
                end;

         // ---------------- SIDE EFFECTS CASE STATEMENT ---------------------
         // This case statment handles any side effects from setting a property
         // (for example, from Generator)
            if ParamPointer > 0 then
                case PropertyIdxMap[ParamPointer] of
                    1:
                        SetNcondsForConnection;  // Force Reallocation of terminal info

            // keep kvar nominal up to date with kW and PF
                    4, 5:
                        SyncUpPowerQuantities;

           {etc.}

                end;

         // Get next token off Parser and continue editing properties
            ParamName := Parser.NextParam;
            Param := Parser.StrValue;
        end;

     // After editing is complete, the typical next step is to call the RecalcElementData function
        RecalcElementData;
        YPrimInvalid := TRUE; // Setting this flag notifies the DSS that something has changed
                           // and the Yprim will have to be rebuilt
    end;

end;

//----------------------------------------------------------------------------
function TPCPrototype.MakeLike(const OtherPCPrototypeName: String): Integer;

// This function should be defined to handle the Like property inherited from
// the base class.

// The function copies the essential properties of another object of this class

var
    OtherPCPrototype: TPCPrototypeObj;
    i: Integer;

begin
    Result := 0;
   {See if we can find this PCPrototype name in the present collection}
    OtherPCPrototype := Find(OtherPCPrototypeName);
    if (OtherPCPrototype <> NIL)   // skip if not found
    then
        with ActivePCPrototypeObj do
        begin
       // You should first set the basic circuit element properties, for example
            if (Fnphases <> OtherPCPrototype.Fnphases) then
            begin
                Nphases := OtherPCPrototype.Fnphases;
                NConds := Fnphases;  // Forces reallocation of terminal stuff

                Yorder := Fnconds * Fnterms;
                YPrimInvalid := TRUE;

            end;

       // Then set other property values, for example from Generator
            GenVars.kVGeneratorBase := OtherPCPrototype.GenVars.kVGeneratorBase;
            Vbase := OtherPCPrototype.Vbase;
            kWBase := OtherPCPrototype.kWBase;
            kvarBase := OtherPCPrototype.kvarBase;
            UserModel.Name := OtherPCPrototype.UserModel.Name;  // Connect to user written models

       {...}
       {etc.}


       // Do inherited properties
            ClassMakeLike(OtherPCPrototype);

       // Finally initialize all the property value strings to be the same as
       // the copied element
            for i := 1 to ParentClass.NumProperties do
                FPropertyValue^[i] := OtherPCPrototype.FPropertyValue^[i];

            Result := 1;
        end
    else
        DoSimpleMsg('Error in Load MakeLike: "' + OtherPCPrototypeName + '" Not Found.', 562);

end;

//----------------------------------------------------------------------------
function TPCPrototype.Init(Handle: Integer): Integer;

// Optional function if you want to do anything to initialize objects of this class

var
    p: TPCPrototypeObj;

begin

  { For example: set up for randomization}
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

    Result := 0;

end;


//------------------------- MAIN OBJECT CONSTRUCTOR ---------------------
constructor TPCPrototypeObj.Create(ParClass: TDSSClass; const PCPrototypeObjName: String);
begin
    inherited create(ParClass);
    Name := LowerCase(PCPrototypeObjName);
    DSSObjType := ParClass.DSSClassType; // Same as Parent Class

     // Set some basic circuit element properties
    Nphases := 3;  // typical DSS default for a circuit element
    Fnconds := 4;  // defaults to wye
    Yorder := 0;  // To trigger an initial allocation
    Nterms := 1;  // forces allocations of terminal quantities

   {Initialize variables for this object, for example}
    kvarMax := kvarBase * 2.0;
    kvarMin := -kvarmax;
    PFNominal := 0.88;
    YearlyShape := '';
    YearlyShapeObj := NIL;  // if YearlyShapeobj = nil then the load alway stays nominal * global multipliers
    Vpu := 1.0;
    VBase := 7200.0;
    Vminpu := 0.90;
    Vmaxpu := 1.10;
    VBase95 := Vminpu * Vbase;
    VBase105 := Vmaxpu * Vbase;
    Yorder := Fnterms * Fnconds;


     {etc.}


     // If you support a user-written DLL, Initialize here
     // For example, from Generator
    UserModel := TGenUserModel.Create(@Genvars);


     // call the procedure to set the initial property string values
    InitPropertyValues(0);

     // Update anything that has to be calculated from property values
    RecalcElementData;

end;


//----------------------------------------------------------------------------
destructor TPCPrototypeObj.Destroy;

// Free everything here that needs to be freed
// If you allocated anything, dispose of it here

begin

    UserModel.Free;  // If you have a user-written DLL

    inherited Destroy;   // This will take care of most common circuit element arrays, etc.

end;

//----------------------------------------------------------------------------
procedure TPCPrototypeObj.Randomize(Opt: Integer);

// typical proc for handling randomization in DSS fashion

begin
    case Opt of
        0:
            RandomMult := 1.0;
        GAUSSIAN:
            RandomMult := Gauss(YearlyShapeObj.Mean, YearlyShapeObj.StdDev);
        UNIfORM:
            RandomMult := Random;  // number between 0 and 1.0
        LOGNORMAL:
            RandomMult := QuasiLognormal(YearlyShapeObj.Mean);
    end;
end;


//----------------------------------------------------------------------------
procedure TPCPrototypeObj.RecalcElementData;

// Anything that needs to re-calculated  after an Edit

begin


    {For example:}
    VBase95 := VMinPu * VBase;
    VBase105 := VMaxPu * VBase;

    {...}
    {etc.}

    // For example, find specified Spectrum object  and report error if not found
    SpectrumObj := SpectrumClass.Find(Spectrum);
    if SpectrumObj = NIL then
        DoSimpleMsg('ERROR! Spectrum "' + Spectrum + '" Not Found.', 566);

    // For PC elements, a good idea to reallocate InjCurrent in case Yorder has changed
    Reallocmem(InjCurrent, SizeOf(InjCurrent^[1]) * Yorder);

    {Update any user-written models, for example}
    if Usermodel.Exists then
        UserModel.FUpdateModel;

end;

//----------------------------------------------------------------------------
procedure TPCPrototypeObj.CalcYPrimMatrix(Ymatrix: TcMatrix);

{A typical helper function for PC elements to assist in the computation
 of Yprim
}

var
    Y, Yij: Complex;
    i, j: Integer;
    FreqMultiplier: Double;

begin

    FYprimFreq := ActiveCircuit.Solution.Frequency;
    FreqMultiplier := FYprimFreq / BaseFrequency;  // ratio to adjust reactances for present solution frequency

    with  ActiveCircuit.solution do
        if IsDynamicModel or IsHarmonicModel then
   // for Dynamics and Harmonics modes use constant equivalent Y
        begin
       // Example from Generator
       // If the generator is on, use equivalent Y else some very small value
            if GenON then
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
                        Ymatrix.AddElement(Fnconds, Fnconds, Y);  // sums the element
                        Ymatrix.SetElemsym(i, Fnconds, Yij);
                    end;
                    1:
                    begin   {Delta connection}
                        Ymatrix.SetElement(i, i, Y);
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
    //  Example from Generator object

       {Yeq is typically expected as the equivalent line-neutral admittance}

            Y := cnegate(Yeq);  // negate for generation    Yeq is L-N quantity

       // ****** Need to modify the base admittance for real harmonics calcs
            Y.im := Y.im / FreqMultiplier;

            case Connection of

                0:
                    with YMatrix do
                    begin // WYE
                        Yij := Cnegate(Y);
                        for i := 1 to Fnphases do
                        begin
                            SetElement(i, i, Y);
                            AddElement(Fnconds, Fnconds, Y);
                            SetElemsym(i, Fnconds, Yij);
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


//----------------------------------------------------------------------------
procedure TPCPrototypeObj.CalcYPrim;

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
    if YPrimInvalid then
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

     {do whatever you have to do to determine Yeq here}

     // call helper routine to compute YPrim_Shunt
    CalcYPrimMatrix(YPrim_Shunt);

     // Set YPrim_Series based on a small fraction of the diagonals of YPrim_shunt
     // so that CalcVoltages doesn't fail
     // This is just one of a number of possible strategies but seems to work most of the time
    for i := 1 to Yorder do
        Yprim_Series.SetElement(i, i, CmulReal(Yprim_Shunt.Getelement(i, i), 1.0e-10));

     // copy YPrim_shunt into YPrim; That's all that is needed for most PC Elements
    YPrim.CopyFrom(YPrim_Shunt);

     // Account for Open Conductors -- done in base class
    inherited CalcYPrim;

end;

// - - - - - - - - - - - - - - - - - - - - - - - -- - - - - - - - - - - - - - - - - -
procedure TPCPrototypeObj.StickCurrInTerminalArray(TermArray: pComplexArray; const Curr: Complex; i: Integer);

// Most PC Elements should have a routine like this to make the current injections into the proper place

 {Add the current into the proper array position according to connection}

 {
  This example is from GENERATOR
  Reverse of similar routine in LOAD  (Cnegates are switched)
 }

var
    j: Integer;

begin
    case Connection of

        0:
        begin  //Wye
            Caccum(TermArray^[i], Curr);
            Caccum(TermArray^[Fnconds], Cnegate(Curr)); // Neutral
        end;

        1:
        begin //DELTA
            Caccum(TermArray^[i], Curr);
            j := i + 1;
            if j > Fnconds then
                j := 1;
            Caccum(TermArray^[j], Cnegate(Curr));
        end;
    end;
end;


// - - - - - - - - - - - - - - - - - - - - - - - -- - - - - - - - - - - - - - - - - -
procedure TPCPrototypeObj.DoConstantPQGen;

{
 This routine is an example from Generator to illustrate how to compute currents
 for PC elements. This is a common constant P + jQ model.
}

{Compute the total terminal current for Constant PQ}

var
    i: Integer;
    Curr, V: Complex;
    Vmag: Double;
begin

    // First compute the contribution due to Yprim matrix
    CalcYPrimContribution(InjCurrent);

    // Zero out the Iterminal array to hold the results of this calculation
    ZeroITerminal;
    // get actual voltage across each phase of the load  and put into VTerminal array
    CalcVTerminalPhase;

    for i := 1 to Fnphases do
    begin
        V := Vterminal^[i];
        VMag := Cabs(V);

        case Connection of
            0:
            begin  {Wye}
                if VMag <= VBase95 then
                    Curr := Cmul(Yeq95, V)  // Below 95% use an impedance model
                else
                if VMag > VBase105 then
                    Curr := Cmul(Yeq105, V)  // above 105% use an impedance model
                else
                    with Genvars do
                        Curr := Conjg(Cdiv(Cmplx(Pnominalperphase, Qnominalperphase), V));  // Between 95% -105%, constant PQ
            end;
            1:
            begin  {Delta}
                VMag := VMag / SQRT3;  // L-N magnitude
                if VMag <= VBase95 then
                    Curr := Cmul(CdivReal(Yeq95, 3.0), V)  // Below 95% use an impedance model
                else
                if VMag > VBase105 then
                    Curr := Cmul(CdivReal(Yeq105, 3.0), V)  // above 105% use an impedance model
                else
                    with Genvars do
                        Curr := Conjg(Cdiv(Cmplx(Pnominalperphase, Qnominalperphase), V));  // Between 95% -105%, constant PQ
            end;
        end;

       // Add into ITerminal (initialized above)
        StickCurrInTerminalArray(ITerminal, Cnegate(Curr), i);  // Put into Terminal array taking into account connection
        IterminalUpdated := TRUE;
       // Now, add into InjCurrent array. This is used in the Normal solution algorithm
        StickCurrInTerminalArray(InjCurrent, Curr, i);  // Put into Terminal array taking into account connection
    end;
end;


// - - - - - - - - - - - - - - - - - - - - - - - -- - - - - - - - - - - - - - - - - -
procedure TPCPrototypeObj.DoDynamicMode;

{ This is an example taken from Generator illustrating how a PC element might
  handle Dynamics mode with a Thevenin equivalent

  Also illustrates the computation of symmetrical component values
}

{Compute Total Current and add into InjTemp}

var
    i: Integer;
    V012,
    I012: array[0..2] of Complex;

begin

   // Start off by getting the current in the admittance branch of the model
    CalcYPrimContribution(InjCurrent);  // Init InjCurrent Array

   {Inj = -Itotal (in) - Yprim*Vtemp}

    with Genvars do
    begin

        // Convert Terminal voltages to symmetrical component
        Phase2SymComp(Vterminal, @V012);

        // Positive Sequence Contribution to Iterminal

        // Compute present value of VThev
        CalcVthev_Dyn;  // Update for latest phase angle

        // Positive Sequence Contribution to Iterminal
        // Use Xd' for pos seq;  Xd" for neg seq
        I012[1] := CDiv(Csub(V012[1], Vthev), Cmplx(0.0, Xdp));
        I012[2] := Cdiv(V012[2], Cmplx(0.0, Xdpp));
        if Connection = 1 then
            I012[0] := CZERO
        else
            I012[0] := Cdiv(V012[0], Cmplx(0.0, Xdpp));

        // Convert back to phase components
        SymComp2Phase(ITerminal, @I012);

        // Neutral current, if any
        if Connection = 0 then
            ITerminal^[FnConds] := Cnegate(CmulReal(I012[0], 3.0));

    end;
    IterminalUpdated := TRUE;

   {Add it into inj current array}
    for i := 1 to FnConds do
        Caccum(InjCurrent^[i], Cnegate(Iterminal^[i]));

end;


// - - - - - - - - - - - - - - - - - - - - - - - -- - - - - - - - - - - - - - - - - -
procedure TPCPrototypeObj.DoHarmonicMode;

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
    ComputeVterminal;

    with ActiveCircuit.Solution do
    begin
        GenHarmonic := Frequency / GenFundamental; // harmonic based on the fundamental for this object
        // get the spectrum multiplier and multiply by the V thev (or Norton current for load objects)
        E := CmulReal(SpectrumObj.GetMult(GenHarmonic), VThevHarm); // Get base harmonic magnitude
        RotatePhasorRad(E, GenHarmonic, ThetaHarm);  // Time shift by fundamental frequency phase shift

        // Put the values in a temp complex buffer
        for i := 1 to Fnphases do
        begin
            cBuffer[i] := E;
            if i < Fnphases then
                RotatePhasorDeg(E, GenHarmonic, -120.0);  // Assume 3-phase PCPrototype
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
procedure TPCPrototypeObj.CalcVTerminalPhase;

{
  Many PC Element models will contain a Proc like this to compute terminal voltages
  differently for Y or Delta connections
}

var
    i, j: Integer;

begin

{ Establish phase voltages and stick in Vterminal}
    case Connection of

        0:
        begin
            with ActiveCircuit.Solution do
                for i := 1 to Fnphases do
                    Vterminal^[i] := VDiff(NodeRef^[i], NodeRef^[Fnconds]);
        end;

        1:
        begin
            with ActiveCircuit.Solution do
                for i := 1 to Fnphases do
                begin
                    j := i + 1;
                    if j > Fnconds then
                        j := 1;
                    Vterminal^[i] := VDiff(NodeRef^[i], NodeRef^[j]);
                end;
        end;

    end;


   // It is often advantageous to keep track of which solution VTerminal applies to
   // You can use this to avoid unnecessary recalcs of Vterminal if the solution hasn't changed
    PCPrototypeSolutionCount := ActiveCircuit.Solution.SolutionCount;

end;

// - - - - - - - - - - - - - - - - - - - - - - - -- - - - - - - - - - - - - - - - - -
procedure TPCPrototypeObj.CalcVTerminal;

{ this is just the standard routine to put terminal voltages in an array
  But it also keeps track of the solution count for computational efficiency
}


begin

    ComputeVTerminal;

    PCPrototypeSolutionCount := ActiveCircuit.Solution.SolutionCount;

end;

// - - - - - - - - - - - - - - - - - - - - - - - -- - - - - - - - - - - - - - - - - -
procedure TPCPrototypeObj.CalcPCPrototypeModelContribution;

// Main dispatcher for computing PC Element currnts

// Calculates PCPrototype current and adds it properly into the injcurrent array
// routines may also compute ITerminal  (ITerminalUpdated flag)

begin
    IterminalUpdated := FALSE;
    with  ActiveCircuit, ActiveCircuit.Solution do
    begin
        if IsDynamicModel then
            DoDynamicMode
        else
        if IsHarmonicModel and (Frequency <> Fundamental) then
            DoHarmonicMode
        else
        begin
           //  compute currents and put into InjTemp array;
            case PCPrototypeModel of
                1:
                    DoConstantPQGen;  // for example, from Generator
                2:
{etc.};  // Different models
            else
              // Put a default model proc call here

            end;
        end; {ELSE}
    end; {WITH}

   {When this is done, ITerminal is up to date}

end;

// - - - - - - - - - - - - - - - - - - - - - - - -- - - - - - - - - - - - - - - - - -
procedure TPCPrototypeObj.CalcInjCurrentArray;

// Main procedure for controlling computation of InjCurrent array

// InjCurrent is difference between currents in YPrim and total terminal current


begin

// You usually will want some logic like this

       // If the element is open, just zero the array and return
    if PCPrototypeSwitchOpen then
        ZeroInjCurrent
       // otherwise, go to a routine that manages the calculation
    else
        CalcPCPrototypeModelContribution;

end;

// - - - - - - - - - - - - - - - - - - - - - - - -- - - - - - - - - - - - - - - - - -
procedure TPCPrototypeObj.GetTerminalCurrents(Curr: pComplexArray);

// This function controls the calculation of the total terminal currents

// Note that it only does something if the solution count has changed.
// Otherwise, Iterminal array already contains the currents


begin
    with ActiveCircuit.Solution do
    begin
        if IterminalSolutionCount <> ActiveCircuit.Solution.SolutionCount then
        begin     // recalc the contribution
          // You will likely want some logic like this
            if not PCPrototypeSwitchOpen then
                CalcPCPrototypeModelContribution;  // Adds totals in Iterminal as a side effect
        end;
        inherited GetTerminalCurrents(Curr); // add in inherited contribution
    end;
end;

// - - - - - - - - - - - - - - - - - - - - - - - -- - - - - - - - - - - - - - - - - -
function TPCPrototypeObj.InjCurrents: Integer;

// Required function for managing computing of InjCurrents


begin

    with ActiveCircuit.Solution do
    begin

      // Generators and Loads use logic like this:
      //   If LoadsNeedUpdating Then SetNominalGeneration; // Set the nominal kW, etc for the type of solution being done

       // call the main function for doing calculation
        CalcInjCurrentArray;          // Difference between currents in YPrim and total terminal current


       // Add into System Injection Current Array
        Result := inherited InjCurrents;

    end;

end;

// - - - - - - - - - - - - - - - - - - - - - - - -- - - - - - - - - - - - - - - - - -
procedure TPCPrototypeObj.GetInjCurrents(Curr: pComplexArray);

// Gets the currents for the last solution performed

// Do not call anything that may change the basic element values from the last solution

var
    i: Integer;

begin

    CalcInjCurrentArray;  // Difference between currents in YPrim and total current

    try    // an exception here generally means an array boundary overrun
   // Copy into buffer array
        for i := 1 to Yorder do
            Curr^[i] := InjCurrent^[i];

    except
        ON E: Exception do
            DoErrorMsg('PCPrototype Object: "' + Name + '" in GetInjCurrents function.',
                E.Message,
                'Current buffer not big enough.', 568);
    end;

end;
//= = =  = = = = = = = = = = = = = = = = = = = = = = = = = = = =


// - - - - - - - - - - - - - - - - - - - - - - - -- - - - - - - - - - - - - - - - - -
procedure TPCPrototypeObj.DumpProperties(var F: TextFile; Complete: Boolean);

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


procedure TPCPrototypeObj.InitHarmonics;

{Procedure to initialize for Harmonics solution}

{This example is extracted from Generator and constructs a Thevinen equivalent.
 Refer to Load for how to do a Norton equivalent
 }

var
    E, Va: complex;
begin

    YPrimInvalid := TRUE;  // Force rebuild of YPrims
    GenFundamental := ActiveCircuit.Solution.Frequency;  // Whatever the frequency is when we enter here.

    with GenVars do
    begin

         // Xd" is used for harmonics analysis for generators
        Yeq := Cinv(Cmplx(0.0, Xdpp));      // used for current calcs  Always L-N

         {Compute reference Thevinen voltage from phase 1 current}

        if GenON then
        begin

            ComputeIterminal;  // Get present value of current

            with ActiveCircuit.solution do
                case Connection of
                    0:
                    begin {wye - neutral is explicit}
                        Va := Csub(NodeV^[NodeRef^[1]], NodeV^[NodeRef^[Fnconds]]);
                    end;
                    1:
                    begin  {delta -- assume neutral is at zero}
                        Va := NodeV^[NodeRef^[1]];
                    end;
                end;

            E := Csub(Va, Cmul(Iterminal^[1], cmplx(0.0, Xdpp)));
            Vthevharm := Cabs(E);   // establish base mag and angle
            ThetaHarm := Cang(E);
        end
        else
        begin
           // If Generator is off, just set to zero
            Vthevharm := 0.0;
            ThetaHarm := 0.0;
        end;
    end;

end;

procedure TPCPrototypeObj.InitPropertyValues(ArrayOffset: Integer);

// required procedure to initialize the string value of the properties

begin
   // Some examples
    PropertyValue[1] := '3';        //'phases';
    PropertyValue[2] := Getbus(1);  //'bus1';
    PropertyValue[3] := '12.47';
     {...}
    PropertyValue[26] := Format('%-g', [GenVars.kVARating]);
     {...}
    PropertyValue[33] := '';   // null string


{Call inherited function to init inherited property values}
    inherited  InitPropertyValues(NumPropsThisClass);

end;

procedure TPCPrototypeObj.InitStateVars;

// Initialize state variables, principally for Dynamics analysis

// Example here is standard Generator model; Refer to other modules for other approaches.
// This model uses symmetrical components

var
    VNeut,
    Edp: Complex;
    i: Integer;
    V012,
    I012: array[0..2] of Complex;
    Vabc: array[1..3] of Complex;

begin
    YPrimInvalid := TRUE;  // Force rebuild of YPrims

    with GenVars do
    begin

        Yeq := Cinv(Cmplx(0.0, Xdp));

     {Compute nominal Positive sequence voltage behind transient reactance}

        if GenON then
            with ActiveCircuit.Solution do
            begin

                ComputeIterminal;
                Phase2SymComp(ITerminal, @I012);
         // Voltage behind Xdp  (transient reactance), volts
                case Connection of
                    0:
                        Vneut := NodeV^[NodeRef^[Fnconds]]
                else
                    Vneut := CZERO;
                end;

                for i := 1 to FNphases do
                    Vabc[i] := NodeV^[NodeRef^[i]];   // Wye Voltage
                Phase2SymComp(@Vabc, @V012);
                Edp := Csub(V012[1], Cmul(I012[1], cmplx(0.0, Xdp)));    // Pos sequence
                VThevMag := Cabs(Edp);

                Theta := Cang(Edp);
                dTheta := 0.0;
                w0 := Twopi * ActiveCircuit.Solution.Frequency;
         // recalc Mmass and D in case the frequency has changed
                with GenVars do
                begin
                    GenVars.Mmass := 2.0 * GenVars.Hmass * GenVars.kVArating * 1000.0 / (w0);   // M = W-sec
                    D := Dpu * kVArating * 1000.0 / (w0);
                end;
                Pshaft := -Power[1].re; // Initialize Pshaft to present power Output

                Speed := 0.0;    // relative to synch speed
                dSpeed := 0.0;

         // Init User-written models
         //Ncond:Integer; V, I:pComplexArray; const X,Pshaft,Theta,Speed,dt,time:Double
                with ActiveCircuit.Solution do
                    if GenModel = 6 then
                    begin
                        if UserModel.Exists then
                            UserModel.FInit(Vterminal, Iterminal);
                    end;

            end
        else
        begin
            Vthev := cZERO;
            Theta := 0.0;
            dTheta := 0.0;
            w0 := 0;
            Speed := 0.0;
            dSpeed := 0.0;
        end;
    end;  {With}
end;

procedure TPCPrototypeObj.IntegrateStates;

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

    ComputeIterminal;

// Check for user-written exciter model.
    //Function(V, I:pComplexArray; const Pshaft,Theta,Speed,dt,time:Double)
    with ActiveCircuit.Solution, GenVars do
    begin


    // handling of iteration flag
        with DynaVars do
            if (IterationFlag = 0) then
            begin {First iteration of new time step}
                ThetaHistory := Theta + 0.5 * h * dTheta;
                SpeedHistory := Speed + 0.5 * h * dSpeed;
            end;

      // Compute shaft dynamics
        TracePower := TerminalPowerIn(Vterminal, Iterminal, FnPhases);
        dSpeed := (Pshaft + TracePower.re - D * Speed) / Mmass;
        dTheta := Speed;

     // Trapezoidal method
        with DynaVars do
        begin
            Speed := SpeedHistory + 0.5 * h * dSpeed;
            Theta := ThetaHistory + 0.5 * h * dTheta;
        end;

      // Write Dynamics Trace Record
        if DebugTrace then
        begin
            Append(TraceFile);
            Write(TraceFile, Format('t=%-.5g ', [Dynavars.t]));
            Write(TraceFile, Format(' Flag=%d ', [Dynavars.Iterationflag]));
            Write(TraceFile, Format(' Speed=%-.5g ', [Speed]));
            Write(TraceFile, Format(' dSpeed=%-.5g ', [dSpeed]));
            Write(TraceFile, Format(' Pshaft=%-.5g ', [PShaft]));
            Write(TraceFile, Format(' P=%-.5g Q= %-.5g', [TracePower.Re, TracePower.im]));
            Write(TraceFile, Format(' M=%-.5g ', [Mmass]));
            Writeln(TraceFile);
            CloseFile(TraceFile);
        end;

      // Handline of user models, if any
        if GenModel = 6 then
        begin
            if UserModel.Exists then
                UserModel.Integrate;
        end;

    end;
end;

function TPCPrototypeObj.Get_Variable(i: Integer): Double;

{
  This is a virtual function. You do not need to write this routine
  if you are not defining state variables.
}

{
 Return i-th state variable one at a time
 Mainly for reports
}

var
    N, k: Integer;

begin
    N := 0;
    Result := -9999.99;  // error return value
    if i < 1 then
        Exit;  // Someone goofed

    with GenVars do
        case i of
    // for example, the intrinsic state variables of a Generator
    // change to whatever is appropriate to report in desired units
            1:
                Result := (w0 + Speed) / TwoPi;  // Frequency, Hz
            2:
                Result := (Theta) * RadiansToDegrees;  // Report in Deg
            3:
                Result := Cabs(Vthev) / vbase;      // Report in pu
            4:
                Result := Pshaft;
            5:
                Result := dSpeed * RadiansToDegrees; // Report in Deg      57.29577951
            6:
                Result := dTheta;
        else
        begin
            if UserModel.Exists then
            begin
                N := UserModel.FNumVars;
                k := (i - NumPCPrototypeVariables);
                if k <= N then
                begin
                    Result := UserModel.FGetVariable(k);
                    Exit;
                end;
            end;

        end;
        end;

end;

procedure TPCPrototypeObj.Set_Variable(i: Integer; Value: Double);

{
  This is a virtual function. You do not need to write this routine
  if you are not defining state variables.
}

// Sets i-th state variable to specified Value

var
    N, k: Integer;

begin
    N := 0;
    if i < 1 then
        Exit;  // Someone goofed
    with GenVars do
        case i of
      // for example, the intrinsic state vars of a generator
      // change to appropriate values
            1:
                Speed := (Value - w0) * TwoPi;
            2:
                Theta := Value / RadiansToDegrees; // deg to rad
            3: ;// meaningless to set Vd := Value * vbase; // pu to volts
            4:
                Pshaft := Value;
            5:
                dSpeed := Value / RadiansToDegrees;
            6:
                dTheta := Value;
        else
        begin
            if UserModel.Exists then
            begin
                N := UserModel.FNumVars;
                k := (i - NumPCPrototypeVariables);
                if k <= N then
                begin
                    UserModel.FSetVariable(k, Value);
                    Exit;
                end;
            end;
        end;
        end;
end;

procedure TPCPrototypeObj.GetAllVariables(States: pDoubleArray);

{
  This is a virtual function. You do not need to write this routine
  if you are not defining state variables.
}

// Return all state variables in double array (allocated by calling function)

var
    i, N: Integer;
begin
    N := 0;
    for i := 1 to NumPCPrototypeVariables do
        States^[i] := Variable[i];

    if UserModel.Exists then
    begin
        N := UserModel.FNumVars;
        UserModel.FGetAllVars(@States^[NumPCPrototypeVariables + 1]);
    end;

end;

function TPCPrototypeObj.NumVariables: Integer;

{
  This is a virtual function. You do not need to write this routine
  if you are not defining state variables.
}

// Return the number of state variables

// Note: it is not necessary to define any state variables

begin
    Result := NumPCPrototypeVariables;
    if UserModel.Exists then
        Result := Result + UserModel.FNumVars;
end;

function TPCPrototypeObj.VariableName(i: Integer): String;

{
  This is a virtual function. You do not need to write this routine
  if you are not defining state variables.
}

// Returns the i-th state variable in a string

const
    BuffSize = 255;
var
    n,
    i2: Integer;
    Buff: array[0..BuffSize] of Char;
    pName: Pchar;

begin
    n := 0;
    if i < 1 then
        Exit;  // Someone goofed
    case i of
    // For example, these are the 6 intrinsic state variables of a generator
    // Change to appropriate names
        1:
            Result := 'Frequency';
        2:
            Result := 'Theta (Deg)';
        3:
            Result := 'Vd';
        4:
            Result := 'PShaft';
        5:
            Result := 'dSpeed (Deg/sec)';
        6:
            Result := 'dTheta (Deg)';
    else
    begin
      // get user model variable names using DLL interface functions
        if UserModel.Exists then
        begin
            pName := @Buff;
            n := UserModel.FNumVars;
            i2 := i - NumPCPrototypeVariables;
            if i2 <= n then
            begin
                UserModel.FGetVarName(i2, pName, BuffSize);
                Result := pName;
                Exit;
            end;
        end;

    end;
    end;

end;

function TPCPrototypeObj.GetPropertyValue(Index: Integer): String;

// Return i-th property value as a string

begin

    Result := '';   // Init the string
    case Index of
         // Put special cases here
         // often a good idea to convert numeric values to strings, for example
        4:
            Result := Format('%.6g', [kWBase]);
        5:
            Result := Format('%.6g', [PFNominal]);

         {...}
         // Some values must be enclosed in parens or brackets
        34:
            Result := '(' + inherited GetPropertyValue(index) + ')';
    else

         // The default is to just return the current string value of the property
        Result := inherited GetPropertyValue(index);

    end;
end;

procedure TPCPrototypeObj.MakePosSequence;

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

  // Make sure voltage is line-neutral
    if (Fnphases > 1) or (connection <> 0) then
        V := GenVars.kVGeneratorBase / SQRT3
    else
        V := GenVars.kVGeneratorBase;

    S := S + Format(' kV=%-.5g', [V]);

  // Divide the load by no. phases
    if Fnphases > 1 then
    begin
        S := S + Format(' kW=%-.5g  PF=%-.5g', [kWbase / Fnphases, PFNominal]);
        if (PrpSequence^[19] <> 0) or (PrpSequence^[20] <> 0) then
            S := S + Format(' maxkvar=%-.5g  minkvar=%-.5g', [kvarmax / Fnphases, kvarmin / Fnphases]);
        if PrpSequence^[26] > 0 then
            S := S + Format(' kva=%-.5g  ', [genvars.kvarating / Fnphases]);
        if PrpSequence^[27] > 0 then
            S := S + Format(' MVA=%-.5g  ', [genvars.kvarating / 1000.0 / Fnphases]);
    end;

    Parser.CmdString := S;   // Push the string into the Parser object
    Edit;    // Invoke the Edit method for this class

    inherited;  // sets the terminal bus references, must do after editing number of phases

end;

procedure TPCPrototypeObj.Set_ConductorClosed(Index: Integer;
    Value: Boolean);

// Routine for handling Open/Close procedures

begin
    inherited;

 // In this example from Generator, just turn the object on or off;

    if Value then
        PCPrototypeSwitchOpen := FALSE
    else
        PCPrototypeSwitchOpen := TRUE;

end;


initialization

// Initialize any variables here


  // For Example:  1 + j 1
    CDOUBLEONE := CMPLX(1.0, 1.0);


end.
