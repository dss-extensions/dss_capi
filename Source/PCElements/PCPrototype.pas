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

USES  
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


TYPE

// = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = =

{ Collection manager for this class of element }
   TPCPrototype = CLASS(TPCClass)
     private

      {These private functions are generally helper functions for Edit procedure}

      { A typical function }
       Procedure SetNcondsForConnection;

     Protected
       Procedure DefineProperties;    // Define the property names and help strings
       Function  MakeLike(Const OtherPCPrototypeName:STring):Integer;Override;  // copy properties of another similar object

     public

       constructor Create;
       destructor Destroy; override;

       Function Edit:Integer; override;      // Definition of the main property editing function 
       Function Init(Handle:Integer):Integer; override;  // Initialize by handle (index), if necessary

       Function NewObject(const ObjName:String):Integer; override; // This function is called by the DSS New command

     {any public functions that might be called from other elements}

   End;

// = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = =

{ Class definition for this class of element}
   TPCPrototypeObj = class(TPCElement)
      Private

      {Private variables of this class}
       // a typical private variable:
        Yeq             :Complex;   // Y at nominal voltage

       // a typical procedure if user models are supported
        Procedure DoUserModel;

       // many PC elements have a proc like this to map computed currents into appropriate array
        Procedure StickCurrInTerminalArray(TermArray:pComplexArray; Const Curr:Complex; i:Integer);
	
      Protected

        {A couple of virtual procedures you can override}
        PROCEDURE Set_ConductorClosed(Index:Integer; Value:Boolean); Override;
        Procedure GetTerminalCurrents(Curr:pComplexArray); Override ;

      public

        {Variables and functions accessed by DSS and other objects}  
	
        constructor Create(ParClass :TDSSClass; const PCPrototypeObjName :String);
        destructor  Destroy; override;

        Procedure RecalcElementData; Override;   // Generally called after Edit is complete to recompute variables
        Procedure CalcYPrim; Override;   // Calculate Primitive Y matrix 

        // Injection current management functions (unique to PC Elements)
	      // This is how the DSS represents elements with nonlinear characteristics
        // Inj currents are the difference between the desired total terminal currents and the
        // currents that result from the linear admittance matrix of the element
        Function  InjCurrents:Integer; Override;
        Procedure GetInjCurrents(Curr:pComplexArray); Override;

      	// State variable management functions, if any
        // You can omit these if your PC element model is not using these
        // Default behavior is to basically do nothing
        Function  NumVariables:Integer;Override;
        Procedure GetAllVariables(States:pDoubleArray);Override;
        Function  Get_Variable(i: Integer): Double; Override;
        procedure Set_Variable(i: Integer; Value: Double);  Override;
        Function  VariableName(i:Integer):String ;Override;

        // Support for Dynamics Mode
        Procedure InitStateVars;  Override;
        Procedure IntegrateStates;Override;

        // Support for Harmonics Mode
        Procedure InitHarmonics; Override;

       PROCEDURE MakePosSequence;Override;  // Make a positive Sequence Model, if possible

       // Functions required for managing values of properties
       PROCEDURE InitPropertyValues(ArrayOffset:Integer);Override;
       Procedure DumpProperties(Var F:TextFile; Complete:Boolean);Override;
       FUNCTION  GetPropertyValue(Index:Integer):String;Override;

       {Put any class properties here}
       {Use properties when some method must be executed when a value is set or retrieved}

       {   Example (from Load)
         Property ConnectedkVA        :Double Read FConnectedkVA        Write Set_ConnectedkVA;
       }

   End;

VAR
    ActivePCPrototypeObj:TPCPrototypeObj;

// = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = =
implementation

{Typical Uses Clause -- not all may not be needed}
USES  ParserDel,     // DSS parser
      DSSClassDefs,  // Where class is instantiated
      DSSGlobals,    // Global DSS variables
      Circuit,       // If access to circuit variables is needed
      Command,       // DSS command and property support module
      Sysutils,      // Delphi misc utility functions
      Math,          // Delphi Math functions
      MathUtil,      // DSS Math utilities
      Utilities;     // DSS misc utility functions

CONST
     NumPropsThisClass = 36; // Set this constant to the actual number of properties you define

VAR  // Define any useful module vars here, for example:
     cBuffer:Array[1..24] of Complex;  // Temp buffer for complex math calcs; allows up to 24-phase models.
     CDOUBLEONE: Complex;   // 1 + j1  (see Initialization section below)

//- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
constructor TPCPrototype.Create;  // Creates main collection handler for all PCPrototype objects
Begin
     Inherited Create;  // make the base class  and init DSSClassType

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
End;

//- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Destructor TPCPrototype.Destroy;

Begin

    // ElementList and  CommandList freed in inherited destroy
    Inherited Destroy;

End;

//- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Procedure TPCPrototype.DefineProperties;

// This is where the properties are defined, assigned names, indexes, and help strings
// The Help strings will automatically show up when the Help is invoked

Begin

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
     AddProperty('kv',  3,  'Nominal rated (1.0 per unit) voltage, kV, for PCPrototype. For 2- and 3-phase PCPrototypes, specify phase-phase kV. '+
                    'Otherwise, specify actual kV across each branch of the PCPrototype. '+
                    'If wye (star), specify phase-neutral kV. '+
                    'If delta or phase-phase connected, specify phase-phase kV.');  // line-neutral voltage//  base voltage
     AddProperty('kW', 4, 'Total base kW for the PCPrototype.  A positive value denotes power coming OUT of the element, '+CRLF+
                    'which is the opposite of a load. This value is modified depending on the dispatch mode. ' +
                    'Unaffected by the global load multiplier and growth curves. ' +
                    'If you want there to be more generation, you must add more PCPrototypes or change this value.');




                    {...}
                    {etc.}




     // Finally, we have to pick up any properties that were inherited
     ActiveProperty := NumPropsThisClass;
     inherited DefineProperties;  // Add defs of inherited properties to bottom of list

     // You can optionally override default help string of an inherited property, for example
     PropertyHelp[NumPropsThisClass +1] := 'Name of harmonic voltage or current spectrum for this PCPrototype. ' +
                         'Voltage behind Xd" for machine - default. Current injection for inverter. ' +
                         'Default value is "default", which is defined when the DSS starts.';

End;

//- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Function TPCPrototype.NewObject(const ObjName:String):Integer;

// This function is called  by the DSS whenever a New PCPrototype... command is encountered

Begin
    // Make a new PCPrototype and add it to PCPrototype class list
    With ActiveCircuit Do
    Begin
      ActiveCktElement := TPCPrototypeObj.Create(Self, ObjName);
      Result := AddObjectToList(ActiveDSSObject);
    End;
End;

//- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Procedure TPCPrototype.SetNcondsForConnection;

// This is a typical helper function found in many DSS circuit element class
// for defining the number of conductors per terminal (Nconds) based on Y or delta connection

Begin
  With ActivePCPrototypeObj Do
  Begin
   CASE Connection OF
     0: NConds := Fnphases +1;  // Wye connection (has a neutral terminal)
     1: CASE Fnphases OF        // Delta connection
            1,2: NConds := Fnphases +1; // L-L and Open-delta
        ELSE
            NConds := Fnphases;    // no neutral for this connection
        End;
   End;
  End;
End;



//- - - - - - - - - - - - -MAIN EDIT FUNCTION  - - - - - - - - - - - - - - -
Function TPCPrototype.Edit:Integer;

// This function is the heart of the property managment for this class

VAR     // Define some local vars for handling parser results
   i,
   ParamPointer:Integer;
   ParamName:String;
   Param:String;

// The Edit function starts where the Parser is presently pointing and
// manages the parsing of the rest of the command line in the parser.

// The DSS executive processes the command verb on the front of the line and
// then passes control to the appropriate Edit function

Begin
  // set the present element active
  // and continue parsing with contents of Parser
  ActivePCPrototypeObj := ElementList.Active;
  ActiveCircuit.ActiveCktElement := ActivePCPrototypeObj;

  Result := 0;

  With ActivePCPrototypeObj Do
  Begin
     // peel off the next token on the edit line
     ParamPointer := 0;
     ParamName := Parser.NextParam;
     Param := Parser.StrValue;

     While Length(Param)>0 Do
     Begin
         // Find the index for the CASE statement
         // If property is not named, just increment the index to the next property
         If  (Length(ParamName) = 0) Then Inc(ParamPointer)
         ELSE ParamPointer := CommandList.GetCommand(ParamName);

         // Update the PropertyValy for this property
         // Actual index is mapped via PropertyIdxMap array for this class
         If  (ParamPointer>0) and (ParamPointer<=NumProperties)
         Then PropertyValue[PropertyIdxMap[ParamPointer]] := Param
         ELSE DoSimpleMsg('Unknown parameter "'+ParamName+'" for PCPrototype "'+Name+'"', 560);

         // --------------- MAIN CASE STATEMENT ----------------------
         If ParamPointer > 0 Then
         // since we used AddProperty function to define properties, have to
         // use PropertyIdxMap to map to the correct Case index
         CASE PropertyIdxMap[ParamPointer] OF
            0: DoSimpleMsg('Unknown parameter "' + ParamName + '" for Object "' + Class_Name +'.'+ Name + '"', 561);
            1: NPhases    := Parser.Intvalue; // num phases
            2: SetBus(1, param);
            3: PresentkV    := Parser.DblValue;

            {...}
            {etc.}

            {One case for each property}

         ELSE
           // Handle Inherited properties
             ClassEdit(ActivePCPrototypeObj, ParamPointer - NumPropsThisClass)
         End;

         // ---------------- SIDE EFFECTS CASE STATEMENT ---------------------
         // This case statment handles any side effects from setting a property
         // (for example, from Generator)
         If ParamPointer > 0 Then
         CASE PropertyIdxMap[ParamPointer] OF
            1: SetNcondsForConnection;  // Force Reallocation of terminal info

            // keep kvar nominal up to date with kW and PF
            4,5: SyncUpPowerQuantities;

           {etc.}

         End;

         // Get next token off Parser and continue editing properties
         ParamName := Parser.NextParam;
         Param     := Parser.StrValue;
     End;

     // After editing is complete, the typical next step is to call the RecalcElementData function
     RecalcElementData;
     YprimInvalid[ActorID] := True; // Setting this flag notifies the DSS that something has changed
                           // and the Yprim will have to be rebuilt
  End;

End;

//----------------------------------------------------------------------------
Function TPCPrototype.MakeLike(Const OtherPCPrototypeName:String):Integer;

// This function should be defined to handle the Like property inherited from
// the base class.

// The function copies the essential properties of another object of this class

VAR
   OtherPCPrototype:TPCPrototypeObj;
   i:Integer;

Begin
   Result := 0;
   {See if we can find this PCPrototype name in the present collection}
   OtherPCPrototype := Find(OtherPCPrototypeName);
   If   (OtherPCPrototype <> Nil)   // skip if not found
   Then With ActivePCPrototypeObj Do
   Begin
       // You should first set the basic circuit element properties, for example
       If (Fnphases <> OtherPCPrototype.Fnphases)
       Then Begin
         Nphases := OtherPCPrototype.Fnphases;
         NConds := Fnphases;  // Forces reallocation of terminal stuff

         Yorder := Fnconds*Fnterms;
         YprimInvalid[ActorID] := True;

       End;

       // Then set other property values, for example from Generator
       GenVars.kVGeneratorBase := OtherPCPrototype.GenVars.kVGeneratorBase;
       Vbase          := OtherPCPrototype.Vbase;
       kWBase         := OtherPCPrototype.kWBase;
       kvarBase       := OtherPCPrototype.kvarBase;
       UserModel.Name    := OtherPCPrototype.UserModel.Name;  // Connect to user written models

       {...}
       {etc.}




       // Do inherited properties
       ClassMakeLike(OtherPCPrototype);

       // Finally initialize all the property value strings to be the same as
       // the copied element
       For i := 1 to ParentClass.NumProperties Do
           FPropertyValue^[i] := OtherPCPrototype.FPropertyValue^[i];

       Result := 1;
   End
   ELSE  DoSimpleMsg('Error in Load MakeLike: "' + OtherPCPrototypeName + '" Not Found.', 562);

End;

//----------------------------------------------------------------------------
Function TPCPrototype.Init(Handle:Integer):Integer;

// Optional function if you want to do anything to initialize objects of this class

VAR
   p:TPCPrototypeObj;

Begin

  { For example: set up for randomization}
   If (Handle = 0)
   Then Begin  // init all
     p := elementList.First;
     WHILE (p <> nil) Do
     Begin
        p.Randomize(0);
        p := elementlist.Next;
     End;
   End
   ELSE Begin
     Active := Handle;
     p := GetActiveObj;
     p.Randomize(0);
   End;

   Result := 0;

End;


//------------------------- MAIN OBJECT CONSTRUCTOR ---------------------
Constructor TPCPrototypeObj.Create(ParClass:TDSSClass; const PCPrototypeObjName:String);
Begin
     Inherited create(ParClass);
     Name := LowerCase(PCPrototypeObjName);
     DSSObjType := ParClass.DSSClassType ; // Same as Parent Class

     // Set some basic circuit element properties
     Nphases      := 3;  // typical DSS default for a circuit element
     Fnconds      := 4;  // defaults to wye
     Yorder       := 0;  // To trigger an initial allocation
     Nterms       := 1;  // forces allocations of terminal quantities

   {Initialize variables for this object, for example}
     kvarMax          := kvarBase * 2.0;
     kvarMin          :=-kvarmax;
     PFNominal        := 0.88;
     YearlyShape      := '';
     YearlyShapeObj   := nil;  // if YearlyShapeobj = nil then the load alway stays nominal * global multipliers
     Vpu              := 1.0;
     VBase            := 7200.0;
     Vminpu           := 0.90;
     Vmaxpu           := 1.10;
     VBase95          := Vminpu * Vbase;
     VBase105         := Vmaxpu * Vbase;
     Yorder           := Fnterms * Fnconds;


     {etc.}




     // If you support a user-written DLL, Initialize here
     // For example, from Generator
     UserModel  := TGenUserModel.Create(@Genvars) ;



     // call the procedure to set the initial property string values
     InitPropertyValues(0);

     // Update anything that has to be calculated from property values
     RecalcElementData;

End;


//----------------------------------------------------------------------------
Destructor TPCPrototypeObj.Destroy;

// Free everything here that needs to be freed
// If you allocated anything, dispose of it here

Begin

    UserModel.Free;  // If you have a user-written DLL

    Inherited Destroy;   // This will take care of most common circuit element arrays, etc.

End;

//----------------------------------------------------------------------------
Procedure TPCPrototypeObj.Randomize(Opt:Integer);

// typical proc for handling randomization in DSS fashion

Begin
   CASE Opt OF
       0: RandomMult := 1.0;
       GAUSSIAN:  RandomMult := Gauss(YearlyShapeObj.Mean, YearlyShapeObj.StdDev);
       UNIfORM:   RandomMult := Random;  // number between 0 and 1.0
       LOGNORMAL: RandomMult := QuasiLognormal(YearlyShapeObj.Mean);
   End;
End;


//----------------------------------------------------------------------------
Procedure TPCPrototypeObj.RecalcElementData;

// Anything that needs to re-calculated  after an Edit

Begin


    {For example:}
    VBase95  := VMinPu * VBase;
    VBase105 := VMaxPu * VBase;

    {...}
    {etc.}

    // For example, find specified Spectrum object  and report error if not found
    SpectrumObj := SpectrumClass.Find(Spectrum);
    If SpectrumObj=Nil Then DoSimpleMsg('ERROR! Spectrum "'+Spectrum+'" Not Found.', 566);

    // For PC elements, a good idea to reallocate InjCurrent in case Yorder has changed
    Reallocmem(InjCurrent, SizeOf(InjCurrent^[1])*Yorder);

    {Update any user-written models, for example}
    If Usermodel.Exists  Then UserModel.FUpdateModel;

End;

//----------------------------------------------------------------------------
Procedure TPCPrototypeObj.CalcYPrimMatrix(Ymatrix:TcMatrix);

{A typical helper function for PC elements to assist in the computation
 of Yprim
}

Var
   Y , Yij  :Complex;
   i,j :Integer;
   FreqMultiplier :Double;

Begin

   FYprimFreq := ActiveCircuit.Solution.Frequency  ;
   FreqMultiplier := FYprimFreq / BaseFrequency;  // ratio to adjust reactances for present solution frequency

   With  ActiveCircuit.solution  Do
   IF IsDynamicModel or IsHarmonicModel Then
   // for Dynamics and Harmonics modes use constant equivalent Y
     Begin
       // Example from Generator
       // If the generator is on, use equivalent Y else some very small value
       IF GenON Then   Y  := Yeq   // L-N value computed in initialization routines
       ELSE Y := Cmplx(EPSILON, 0.0);

       IF Connection=1 Then Y := CDivReal(Y, 3.0); // Convert to delta impedance
       Y.im := Y.im / FreqMultiplier;  // adjust for frequency
       Yij := Cnegate(Y);
       FOR i := 1 to Fnphases Do
         Begin
           Case Connection of
           0: Begin
                 Ymatrix.SetElement(i, i, Y);  // sets the element
                 Ymatrix.AddElement(Fnconds, Fnconds, Y);  // sums the element
                 Ymatrix.SetElemsym(i, Fnconds, Yij);
              End;
           1: Begin   {Delta connection}
                 Ymatrix.SetElement(i, i, Y);
                 Ymatrix.AddElement(i, i, Y);  // put it in again
                 For j := 1 to i-1 Do Ymatrix.SetElemsym(i, j, Yij);
              End;
           End;
         End;
     End

   ELSE Begin

    //  Typical code for a regular power flow  model
    //  Example from Generator object

       {Yeq is typically expected as the equivalent line-neutral admittance}

       Y := cnegate(Yeq);  // negate for generation    Yeq is L-N quantity

       // ****** Need to modify the base admittance for real harmonics calcs
       Y.im           := Y.im / FreqMultiplier;

         CASE Connection OF

           0: With YMatrix Do Begin // WYE
                     Yij := Cnegate(Y);
                     FOR i := 1 to Fnphases Do Begin
                     SetElement(i, i, Y);
                     AddElement(Fnconds, Fnconds, Y);
                     SetElemsym(i, Fnconds, Yij);
                 End;
              End;
           1: With YMatrix Do Begin  // Delta  or L-L
                  Y    := CDivReal(Y, 3.0); // Convert to delta impedance
                  Yij  := Cnegate(Y);
                  FOR i := 1 to Fnphases Do Begin
                     j := i+1;
                     If j>Fnconds Then j := 1;  // wrap around for closed connections
                     AddElement(i,i, Y);
                     AddElement(j,j, Y);
                     AddElemSym(i,j, Yij);
                  End;
              End;
         End;
     End;  {ELSE IF Solution.mode}

End;


//----------------------------------------------------------------------------
Procedure TPCPrototypeObj.CalcYPrim;

// Required routine to calculate the primitive Y matrix for this element

// This example uses a helper function (CalcYPrimMatrix) to keep the code
// here clean

Var
        i:integer;
        
Begin

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
     If YprimInvalid[ActorID]
     Then  Begin
         If YPrim_Shunt<>nil Then YPrim_Shunt.Free;
         YPrim_Shunt := TcMatrix.CreateMatrix(Yorder);
         IF YPrim_Series <> nil THEN Yprim_Series.Free;
         YPrim_Series := TcMatrix.CreateMatrix(Yorder);
         If YPrim <> nil Then  YPrim.Free;
         YPrim := TcMatrix.CreateMatrix(Yorder);
     End
     ELSE Begin
          YPrim_Shunt.Clear;
          YPrim_Series.Clear;
          YPrim.Clear;
     End;

     {do whatever you have to do to determine Yeq here}

     // call helper routine to compute YPrim_Shunt
     CalcYPrimMatrix(YPrim_Shunt);

     // Set YPrim_Series based on a small fraction of the diagonals of YPrim_shunt
     // so that CalcVoltages doesn't fail
     // This is just one of a number of possible strategies but seems to work most of the time
     For i := 1 to Yorder Do Yprim_Series.SetElement(i, i, CmulReal(Yprim_Shunt.Getelement(i, i), 1.0e-10));

     // copy YPrim_shunt into YPrim; That's all that is needed for most PC Elements
     YPrim.CopyFrom(YPrim_Shunt);

     // Account for Open Conductors -- done in base class
     Inherited CalcYPrim;

End;

// - - - - - - - - - - - - - - - - - - - - - - - -- - - - - - - - - - - - - - - - - -
Procedure TPCPrototypeObj.StickCurrInTerminalArray(TermArray:pComplexArray; Const Curr:Complex; i:Integer);

// Most PC Elements should have a routine like this to make the current injections into the proper place

 {Add the current into the proper array position according to connection}

 {
  This example is from GENERATOR
  Reverse of similar routine in LOAD  (Cnegates are switched)
 }

VAR j :Integer;

Begin
    CASE Connection OF

         0: Begin  //Wye
                 Caccum(TermArray^[i], Curr );
                 Caccum(TermArray^[Fnconds], Cnegate(Curr) ); // Neutral
            End;

         1: Begin //DELTA
                 Caccum(TermArray^[i], Curr );
                 j := i + 1;
                 If j > Fnconds Then j := 1;
                 Caccum(TermArray^[j], Cnegate(Curr) );
            End;
    End;
End;


// - - - - - - - - - - - - - - - - - - - - - - - -- - - - - - - - - - - - - - - - - -
Procedure TPCPrototypeObj.DoConstantPQGen;

{
 This routine is an example from Generator to illustrate how to compute currents
 for PC elements. This is a common constant P + jQ model.
}

{Compute the total terminal current for Constant PQ}

VAR
   i:Integer;
   Curr, V:Complex;
   Vmag: Double;
Begin

    // First compute the contribution due to Yprim matrix
    CalcYPrimContribution(InjCurrent);

    // Zero out the Iterminal array to hold the results of this calculation
    ZeroITerminal;
    // get actual voltage across each phase of the load  and put into VTerminal array
    CalcVTerminalPhase;

    FOR i := 1 to Fnphases Do Begin
        V    := Vterminal^[i];
        VMag := Cabs(V);

        CASE Connection of
         0: Begin  {Wye}
              IF   VMag <= VBase95
              THEN Curr := Cmul(Yeq95, V)  // Below 95% use an impedance model
              ELSE If VMag > VBase105
              THEN Curr := Cmul(Yeq105, V)  // above 105% use an impedance model
              ELSE With Genvars Do Curr := Conjg(Cdiv(Cmplx(Pnominalperphase, Qnominalperphase), V));  // Between 95% -105%, constant PQ
            End;
          1: Begin  {Delta}
              VMag := VMag/SQRT3;  // L-N magnitude
              IF   VMag <= VBase95
              THEN Curr := Cmul(CdivReal(Yeq95, 3.0), V)  // Below 95% use an impedance model
              ELSE If VMag > VBase105
              THEN Curr := Cmul(CdivReal(Yeq105, 3.0), V)  // above 105% use an impedance model
              ELSE With Genvars Do Curr := Conjg(Cdiv(Cmplx(Pnominalperphase, Qnominalperphase), V));  // Between 95% -105%, constant PQ
            End;
         END;

       // Add into ITerminal (initialized above)
        StickCurrInTerminalArray(ITerminal, Cnegate(Curr), i);  // Put into Terminal array taking into account connection
        IterminalUpdated := TRUE;
       // Now, add into InjCurrent array. This is used in the Normal solution algorithm
        StickCurrInTerminalArray(InjCurrent, Curr, i);  // Put into Terminal array taking into account connection
    End;
End;


// - - - - - - - - - - - - - - - - - - - - - - - -- - - - - - - - - - - - - - - - - -
PROCEDURE TPCPrototypeObj.DoDynamicMode;

{ This is an example taken from Generator illustrating how a PC element might
  handle Dynamics mode with a Thevenin equivalent

  Also illustrates the computation of symmetrical component values
}

{Compute Total Current and add into InjTemp}

Var
   i     : Integer;
   V012,
   I012  : Array[0..2] of Complex;

Begin

   // Start off by getting the current in the admittance branch of the model
   CalcYPrimContribution(InjCurrent);  // Init InjCurrent Array

   {Inj = -Itotal (in) - Yprim*Vtemp}

      With Genvars Do Begin

        // Convert Terminal voltages to symmetrical component
        Phase2SymComp(Vterminal, @V012);

        // Positive Sequence Contribution to Iterminal

        // Compute present value of VThev
        CalcVthev_Dyn;  // Update for latest phase angle

        // Positive Sequence Contribution to Iterminal
        // Use Xd' for pos seq;  Xd" for neg seq
        I012[1] := CDiv(Csub(V012[1], Vthev), Cmplx(0.0, Xdp));
        I012[2] := Cdiv(V012[2], Cmplx(0.0, Xdpp));
        If Connection=1 Then I012[0] := CZERO
                        Else I012[0] := Cdiv(V012[0], Cmplx(0.0, Xdpp));

        // Convert back to phase components
        SymComp2Phase(ITerminal, @I012);

        // Neutral current, if any
        If Connection=0 Then ITerminal^[FnConds] := Cnegate(CmulReal(I012[0], 3.0));

      End;
   IterminalUpdated := TRUE;

   {Add it into inj current array}
   FOR i := 1 to FnConds Do Caccum(InjCurrent^[i], Cnegate(Iterminal^[i]));

End;


// - - - - - - - - - - - - - - - - - - - - - - - -- - - - - - - - - - - - - - - - - -
PROCEDURE TPCPrototypeObj.DoHarmonicMode;

{
  Example taken from Generator illustrating how a PC element might handle
  current calcs for Harmonics mode

  Note: Generator objects assume a Thevenin model (voltage behind and impedance)
        while Load objects assume the Spectrum applies to a Norton model injection current
}

{Compute Injection Current Only when in harmonics mode}

{Assumes spectrum is a voltage source behind subtransient reactance and YPrim has been built}
{Vd is the fundamental frequency voltage behind Xd" for phase 1}

Var
   i     :Integer;
   E     :Complex;
   GenHarmonic :double;

Begin

   // Set the VTerminal array
   ComputeVterminal;

   WITH ActiveCircuit.Solution Do
     Begin
        GenHarmonic := Frequency/GenFundamental; // harmonic based on the fundamental for this object
        // get the spectrum multiplier and multiply by the V thev (or Norton current for load objects)
        E := CmulReal(SpectrumObj.GetMult(GenHarmonic), VThevHarm); // Get base harmonic magnitude
        RotatePhasorRad(E, GenHarmonic, ThetaHarm);  // Time shift by fundamental frequency phase shift

        // Put the values in a temp complex buffer
        FOR i := 1 to Fnphases DO Begin
           cBuffer[i] := E;
           If i < Fnphases Then RotatePhasorDeg(E, GenHarmonic, -120.0);  // Assume 3-phase PCPrototype
        End;
     END;

   {Handle Wye Connection}
   IF Connection=0 THEN cbuffer[Fnconds] := Vterminal^[Fnconds];  // assume no neutral injection voltage

   // In this case the injection currents are simply Yprim(frequency) times the voltage buffer
   // Refer to Load.Pas for load-type objects
   {Inj currents = Yprim (E) }
   YPrim.MVMult(InjCurrent,@cBuffer);

End;


// - - - - - - - - - - - - - - - - - - - - - - - -- - - - - - - - - - - - - - - - - -
Procedure TPCPrototypeObj.CalcVTerminalPhase;

{
  Many PC Element models will contain a Proc like this to compute terminal voltages
  differently for Y or Delta connections
}

VAR i,j:Integer;

Begin

{ Establish phase voltages and stick in Vterminal}
   Case Connection OF

     0:Begin
         With ActiveCircuit.Solution Do
           FOR i := 1 to Fnphases Do Vterminal^[i] := VDiff(NodeRef^[i], NodeRef^[Fnconds]);
       End;

     1:Begin
         With ActiveCircuit.Solution Do
          FOR i := 1 to Fnphases Do  Begin
             j := i + 1;
             If j > Fnconds Then j := 1;
             Vterminal^[i] := VDiff( NodeRef^[i] , NodeRef^[j]);
          End;
       End;

   End;


   // It is often advantageous to keep track of which solution VTerminal applies to
   // You can use this to avoid unnecessary recalcs of Vterminal if the solution hasn't changed
   PCPrototypeSolutionCount := ActiveCircuit.Solution.SolutionCount;

End;

// - - - - - - - - - - - - - - - - - - - - - - - -- - - - - - - - - - - - - - - - - -
Procedure TPCPrototypeObj.CalcVTerminal;

{ this is just the standard routine to put terminal voltages in an array
  But it also keeps track of the solution count for computational efficiency
}


Begin

   ComputeVTerminal;

   PCPrototypeSolutionCount := ActiveCircuit.Solution.SolutionCount;

End;

// - - - - - - - - - - - - - - - - - - - - - - - -- - - - - - - - - - - - - - - - - -
Procedure TPCPrototypeObj.CalcPCPrototypeModelContribution;

// Main dispatcher for computing PC Element currnts

// Calculates PCPrototype current and adds it properly into the injcurrent array
// routines may also compute ITerminal  (ITerminalUpdated flag)

Begin
  IterminalUpdated := FALSE;
  WITH  ActiveCircuit, ActiveCircuit.Solution DO Begin
      IF      IsDynamicModel THEN  DoDynamicMode
      ELSE IF IsHarmonicModel and (Frequency <> Fundamental) THEN  DoHarmonicMode
      ELSE  Begin
           //  compute currents and put into InjTemp array;
           CASE PCPrototypeModel OF
              1: DoConstantPQGen;  // for example, from Generator
              2: {etc.};  // Different models
           ELSE
              // Put a default model proc call here

           End;
        End; {ELSE}
   END; {WITH}

   {When this is done, ITerminal is up to date}

End;

// - - - - - - - - - - - - - - - - - - - - - - - -- - - - - - - - - - - - - - - - - -
Procedure TPCPrototypeObj.CalcInjCurrentArray;

// Main procedure for controlling computation of InjCurrent array

// InjCurrent is difference between currents in YPrim and total terminal current


Begin

// You usually will want some logic like this

       // If the element is open, just zero the array and return
       If PCPrototypeSwitchOpen Then ZeroInjCurrent
       // otherwise, go to a routine that manages the calculation
       Else CalcPCPrototypeModelContribution;

End;

// - - - - - - - - - - - - - - - - - - - - - - - -- - - - - - - - - - - - - - - - - -
Procedure TPCPrototypeObj.GetTerminalCurrents(Curr:pComplexArray);

// This function controls the calculation of the total terminal currents

// Note that it only does something if the solution count has changed.
// Otherwise, Iterminal array already contains the currents


Begin
   WITH ActiveCircuit.Solution  DO
     Begin
        If IterminalSolutionCount <> ActiveCircuit.Solution.SolutionCount Then
        Begin     // recalc the contribution
          // You will likely want some logic like this
          IF Not PCPrototypeSwitchOpen Then CalcPCPrototypeModelContribution;  // Adds totals in Iterminal as a side effect
        End;
        Inherited GetTerminalCurrents(Curr); // add in inherited contribution
     End;
End;

// - - - - - - - - - - - - - - - - - - - - - - - -- - - - - - - - - - - - - - - - - -
Function TPCPrototypeObj.InjCurrents:Integer;

// Required function for managing computing of InjCurrents


Begin

   With ActiveCircuit.Solution Do
    Begin

      // Generators and Loads use logic like this:
      //   If LoadsNeedUpdating Then SetNominalGeneration; // Set the nominal kW, etc for the type of solution being done

       // call the main function for doing calculation
       CalcInjCurrentArray;          // Difference between currents in YPrim and total terminal current


       // Add into System Injection Current Array
       Result := Inherited InjCurrents;

    End;

End;

// - - - - - - - - - - - - - - - - - - - - - - - -- - - - - - - - - - - - - - - - - -
Procedure TPCPrototypeObj.GetInjCurrents(Curr:pComplexArray);

// Gets the currents for the last solution performed

// Do not call anything that may change the basic element values from the last solution

VAR
   i:Integer;

Begin

   CalcInjCurrentArray;  // Difference between currents in YPrim and total current

   TRY    // an exception here generally means an array boundary overrun
   // Copy into buffer array
     FOR i := 1 TO Yorder Do Curr^[i] := InjCurrent^[i];

   EXCEPT
     ON E: Exception Do
        DoErrorMsg('PCPrototype Object: "' + Name + '" in GetInjCurrents function.',
                    E.Message,
                   'Current buffer not big enough.', 568);
   End;

End;
//= = =  = = = = = = = = = = = = = = = = = = = = = = = = = = = =




// - - - - - - - - - - - - - - - - - - - - - - - -- - - - - - - - - - - - - - - - - -
Procedure TPCPrototypeObj.DumpProperties(Var F:TextFile; Complete:Boolean);

{
 This procedure is require to respond to various commands such as Dump that
 write all the device's property values to a file.
}

Var
   i, idx :Integer;

Begin
    Inherited DumpProperties(F, Complete);

    {Write out any specials here, usually preceded by a "!"}

    With ParentClass Do
     For i := 1 to NumProperties Do
     Begin
        idx := PropertyIdxMap[i] ; // Map to get proper index into property value array
        Case idx of

          {Trap any specials here, such as values that are array properties, for example}
           34, 36: Writeln(F,'~ ',PropertyName^[i],'=(',PropertyValue[idx],')')
        Else
          Writeln(F,'~ ',PropertyName^[i],'=',PropertyValue[idx]);
        End;
     End;

    Writeln(F);

End;


Procedure TPCPrototypeObj.InitHarmonics;

{Procedure to initialize for Harmonics solution}

{This example is extracted from Generator and constructs a Thevinen equivalent.
 Refer to Load for how to do a Norton equivalent
 }

Var
  E, Va:complex;
begin

     YprimInvalid[ActorID]   := TRUE;  // Force rebuild of YPrims
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

end;

procedure TPCPrototypeObj.InitPropertyValues(ArrayOffset: Integer);

// required procedure to initialize the string value of the properties

begin
   // Some examples
     PropertyValue[1]      := '3';        //'phases';
     PropertyValue[2]      := Getbus(1);  //'bus1';
     PropertyValue[3]      := '12.47';
     {...}
     PropertyValue[26]     := Format('%-g', [GenVars.kVARating]);
     {...}
     PropertyValue[33]     := '';   // null string


{Call inherited function to init inherited property values}
  inherited  InitPropertyValues(NumPropsThisClass);

end;

PROCEDURE TPCPrototypeObj.InitStateVars;

// Initialize state variables, principally for Dynamics analysis

// Example here is standard Generator model; Refer to other modules for other approaches.
// This model uses symmetrical components

Var
    VNeut,
    Edp   :Complex;
    i     :Integer;
    V012,
    I012  :Array[0..2] of Complex;
    Vabc  :Array[1..3] of Complex;

begin
  YprimInvalid[ActorID] := TRUE;  // Force rebuild of YPrims

  With GenVars Do Begin

     Yeq := Cinv(Cmplx(0.0, Xdp));

     {Compute nominal Positive sequence voltage behind transient reactance}

     IF GenON Then With ActiveCircuit.Solution Do
       Begin

         ComputeIterminal;
         Phase2SymComp(ITerminal, @I012);
         // Voltage behind Xdp  (transient reactance), volts
         Case Connection of
            0: Vneut :=  NodeV^[NodeRef^[Fnconds]]
         Else
            Vneut :=  CZERO;
         End;

         For i := 1 to FNphases Do Vabc[i] := NodeV^[NodeRef^[i]];   // Wye Voltage
         Phase2SymComp(@Vabc, @V012);
         Edp      := Csub( V012[1] , Cmul(I012[1], cmplx(0.0, Xdp)));    // Pos sequence
         VThevMag := Cabs(Edp);

         Theta  := Cang(Edp) ;
         dTheta := 0.0;
         w0     := Twopi * ActiveCircuit.Solution.Frequency;
         // recalc Mmass and D in case the frequency has changed
         With GenVars Do Begin
           GenVars.Mmass := 2.0 * GenVars.Hmass * GenVars.kVArating * 1000.0/ (w0);   // M = W-sec
           D := Dpu * kVArating *1000.0/(w0);
         End;
         Pshaft := -Power[1].re; // Initialize Pshaft to present power Output

         Speed  := 0.0;    // relative to synch speed
         dSpeed := 0.0;

         // Init User-written models
         //Ncond:Integer; V, I:pComplexArray; const X,Pshaft,Theta,Speed,dt,time:Double
         With ActiveCircuit.Solution Do If GenModel=6 then Begin
           If UserModel.Exists Then UserModel.FInit( Vterminal, Iterminal);
         End;

       End
     ELSE  Begin
         Vthev  := cZERO;
         Theta  := 0.0;
         dTheta := 0.0;
         w0     := 0;
         Speed  := 0.0;
         dSpeed := 0.0;
     End;
  End;  {With}
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

Var
    TracePower:Complex;


begin
   // Compute Derivatives and then integrate

   ComputeIterminal;

// Check for user-written exciter model.
    //Function(V, I:pComplexArray; const Pshaft,Theta,Speed,dt,time:Double)
    With ActiveCircuit.Solution, GenVars Do  Begin


    // handling of iteration flag
      With DynaVars Do
      If (IterationFlag = 0) Then Begin {First iteration of new time step}
          ThetaHistory := Theta + 0.5*h*dTheta;
          SpeedHistory := Speed + 0.5*h*dSpeed;
      End;

      // Compute shaft dynamics
      TracePower := TerminalPowerIn(Vterminal,Iterminal,FnPhases) ;
      dSpeed     := (Pshaft + TracePower.re - D*Speed) / Mmass;
      dTheta     := Speed ;

     // Trapezoidal method
      With DynaVars Do Begin
       Speed := SpeedHistory + 0.5*h*dSpeed;
       Theta := ThetaHistory + 0.5*h*dTheta;
      End;

      // Write Dynamics Trace Record
        IF DebugTrace Then
          Begin
             Append(TraceFile);
             Write(TraceFile,Format('t=%-.5g ',[Dynavars.t]));
             Write(TraceFile,Format(' Flag=%d ',[Dynavars.Iterationflag]));
             Write(TraceFile,Format(' Speed=%-.5g ',[Speed]));
             Write(TraceFile,Format(' dSpeed=%-.5g ',[dSpeed]));
             Write(TraceFile,Format(' Pshaft=%-.5g ',[PShaft]));
             Write(TraceFile,Format(' P=%-.5g Q= %-.5g',[TracePower.Re, TracePower.im]));
             Write(TraceFile,Format(' M=%-.5g ',[Mmass]));
             Writeln(TraceFile);
             CloseFile(TraceFile);
         End;

      // Handline of user models, if any
       If GenModel=6 then Begin
         If UserModel.Exists    Then UserModel.Integrate;
       End;

   End;
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

Var
      N, k:Integer;

begin
     N := 0;
    Result := -9999.99;  // error return value
    If i < 1 Then Exit;  // Someone goofed

    With GenVars Do
    Case i of
    // for example, the intrinsic state variables of a Generator
    // change to whatever is appropriate to report in desired units
       1: Result := (w0+Speed)/TwoPi;  // Frequency, Hz
       2: Result := (Theta ) * RadiansToDegrees;  // Report in Deg
       3: Result := Cabs(Vthev)/vbase;      // Report in pu
       4: Result := Pshaft;
       5: Result := dSpeed * RadiansToDegrees; // Report in Deg      57.29577951
       6: Result := dTheta ;
     Else
        Begin
           If UserModel.Exists Then Begin
              N := UserModel.FNumVars;
              k := (i-NumPCPrototypeVariables);
              If k <= N Then Begin
                  Result := UserModel.FGetVariable(k);
                  Exit;
              End;
           End;

        End;
     End;

end;

procedure TPCPrototypeObj.Set_Variable(i: Integer;  Value: Double);

{
  This is a virtual function. You do not need to write this routine
  if you are not defining state variables.
}

// Sets i-th state variable to specified Value

var N, k:Integer;

begin
  N := 0;
  If i<1 Then Exit;  // Someone goofed
  With GenVars Do
    Case i of
      // for example, the intrinsic state vars of a generator
      // change to appropriate values
       1: Speed := (Value-w0)*TwoPi;
       2: Theta := Value/RadiansToDegrees; // deg to rad
       3: ;// meaningless to set Vd := Value * vbase; // pu to volts
       4: Pshaft := Value;
       5: dSpeed := Value / RadiansToDegrees;
       6: dTheta := Value ;
     Else
       Begin
         If UserModel.Exists Then Begin
            N := UserModel.FNumVars;
            k := (i-NumPCPrototypeVariables) ;
            If  k<= N Then Begin
                UserModel.FSetVariable( k, Value );
                Exit;
              End;
          End;
       End;
     End;
end;

procedure TPCPrototypeObj.GetAllVariables(States: pDoubleArray);

{
  This is a virtual function. You do not need to write this routine
  if you are not defining state variables.
}

// Return all state variables in double array (allocated by calling function)

Var  i, N:Integer;
begin
     N := 0;
     For i := 1 to NumPCPrototypeVariables Do States^[i] := Variable[i];

     If UserModel.Exists Then Begin
        N := UserModel.FNumVars;
        UserModel.FGetAllVars(@States^[NumPCPrototypeVariables+1]);
     End;

end;

function TPCPrototypeObj.NumVariables: Integer;

{
  This is a virtual function. You do not need to write this routine
  if you are not defining state variables.
}

// Return the number of state variables

// Note: it is not necessary to define any state variables

begin
     Result  := NumPCPrototypeVariables;
     If UserModel.Exists    then Result := Result + UserModel.FNumVars;
end;

Function TPCPrototypeObj.VariableName(i: Integer):String;

{
  This is a virtual function. You do not need to write this routine
  if you are not defining state variables.
}

// Returns the i-th state variable in a string

Const
    BuffSize = 255;
Var
    n,
    i2    :integer;
    Buff  :Array[0..BuffSize] of Char;
    pName :pchar;
    
begin
    n:=0;
    If i<1 Then Exit;  // Someone goofed
    Case i of
    // For example, these are the 6 intrinsic state variables of a generator
    // Change to appropriate names
        1:Result := 'Frequency';
        2:Result := 'Theta (Deg)';
        3:Result := 'Vd';
        4:Result := 'PShaft';
        5:Result := 'dSpeed (Deg/sec)';
        6:Result := 'dTheta (Deg)';
    Else Begin
      // get user model variable names using DLL interface functions
          If UserModel.Exists Then
            Begin
              pName := @Buff;
              n := UserModel.FNumVars;
              i2 := i-NumPCPrototypeVariables;
              If i2 <= n Then
                Begin
                 UserModel.FGetVarName(i2, pName, BuffSize);
                 Result := pName;
                 Exit;
                End;
            End;

        End;
    End;

end;

function TPCPrototypeObj.GetPropertyValue(Index: Integer): String;

// Return i-th property value as a string

begin

      Result := '';   // Init the string
      CASE Index of
         // Put special cases here
         // often a good idea to convert numeric values to strings, for example
         4:  Result := Format('%.6g', [kWBase]);
         5:  Result := Format('%.6g', [PFNominal]);

         {...}
         // Some values must be enclosed in parens or brackets
         34: Result := '(' + inherited GetPropertyValue(index) + ')';
      ELSE

         // The default is to just return the current string value of the property
         Result := Inherited GetPropertyValue(index);

      END;
end;

procedure TPCPrototypeObj.MakePosSequence;

{
  This is a virtual function. You do not need to write this routine
  if the base class function will suffice.
}

// Routine to convert existing three-phase models to a single-phase positive-
// sequence model

Var
    S :String;
    V :Double;

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

end;

procedure TPCPrototypeObj.Set_ConductorClosed(Index: Integer;
  Value: Boolean);

// Routine for handling Open/Close procedures

begin
   inherited;

 // In this example from Generator, just turn the object on or off;

   If Value Then PCPrototypeSwitchOpen := FALSE Else PCPrototypeSwitchOpen := TRUE;

end;



initialization

// Initialize any variables here


  // For Example:  1 + j 1
   CDOUBLEONE := CMPLX(1.0, 1.0);


end.
