unit WindGen;
{
  ----------------------------------------------------------
  Copyright (c) 2024, Electric Power Research Institute, Inc.
  All rights reserved.
  ----------------------------------------------------------
}

{   Change Log

   2/26/21 Created from   Generator.pas
   3/25/21 Removed Generator-related properties  (e.g., Fuel variables)
   5/25/22 Dynamic expression compatibility added.

}
{
  In power flow modes, the WindGen element is essentially a negative load that can be dispatched.
}

//  The WindGen is assumed balanced over the no. of phases defined

// If you do not specify load shapes defaults are:
//    Yearly:  Defaults to No variation (i.e. multiplier = 1.0 always)
//    Daily:   Defaults to No variation
//    Dutycycle: Defaults to Daily shape

interface

USES WindGenVars, WindGenUserModel, DSSClass,  PCClass, PCElement, ucmatrix, ucomplex, LoadShape, GrowthShape,
    Spectrum, ArrayDef, Dynamics, WTG3_Model, XYCurve;

Const  NumWGenRegisters = 6;    // Number of energy meter registers
       NumWGenVariables = 22;

TYPE

// = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = =
   TWindGen = CLASS(TPCClass)
     private

       Procedure InterpretConnection(const S:String);
       Procedure SetNcondsForConnection;
     Protected
       Procedure DefineProperties;
       Function MakeLike(Const OtherWindGenName:STring):Integer;Override;
     public
       RegisterNames:Array[1..NumWGenregisters] of String;

       constructor Create;
       destructor Destroy; override;

       Function Edit(ActorID : Integer):Integer; override;
       Function Init(Handle:Integer; ActorID : Integer):Integer; override;
       Function NewObject(const ObjName:String):Integer; override;

       Procedure ResetRegistersAll(ActorID : Integer);
       Procedure SampleAll(ActorID : Integer);

   End;

// = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = =
   TWindGenObj = class(TPCElement)
      Private
// Moved to WindGenVars        Zthev           :Complex;
        Yeq             :Complex;   // at nominal
        Yeq95           :Complex;   // at 95%
        Yeq105          :Complex;   // at 105%

        Edp               :Complex;
        PhaseCurrentLimit    :Complex;
        Model7MaxPhaseCurr   :Double;
        Model7LastAngle      :Double;
        DebugTrace      :Boolean;
        DeltaQMax       :Double;  // Max allowable var change on Model=3 per iteration

        DQDV            :Double;
        DQDVSaved       :Double;
        FForcedON       :Boolean;
        FirstSampleAfterReset  :Boolean;
        IsFixed         :Boolean;   // if Fixed, always at base value
        WindGenSolutionCount    :Integer;
        GenFundamental  :Double;  {Thevinen equivalent voltage mag and angle reference for Harmonic model}
        GenON           :Boolean;           {Indicates whether WindGen is currently on}
        GenSwitchOpen   :Boolean;
        kVANotSet       :Boolean;
        LastGrowthFactor :Double;
        LastYear         :Integer;   // added for speedup so we don't have to search for growth factor a lot
        OpenWindGenSolutionCount :Integer;
        PVFactor        :Double;  // deceleration Factor for computing vars for PV WindGens
        RandomMult      :Double;
        Reg_Hours       :Integer;
        Reg_kvarh       :Integer;
        Reg_kWh         :Integer;
        Reg_MaxkVA      :Integer;
        Reg_MaxkW       :Integer;
        Reg_Price       :Integer;
        ShapeFactor     :Complex;
// moved to WindGenVars        Thetaharm       :Double;  {Thevinen equivalent voltage angle reference for Harmonic model}
        Tracefile       : TextFile;
        UserModel, ShaftModel : TWindGenUserModel;   {User-Written Models}
        V_Avg           :Double;
        V_Remembered    :Double;
        var_Remembered  :Double;
        varBase         :Double; // Base vars per phase
        varMax          :Double;
        varMin          :Double;
        VBase           :Double;  // Base volts suitable for computing currents
        VBase105        :Double;
        VBase95         :Double;
        Vthev           :Complex;  {Thevinen equivalent voltage (complex) for dynamic model}
// moved to WindGenVars        Vthevharm       :Double;  {Thevinen equivalent voltage mag reference for Harmonic model}
// moved to WindGenVars        VthevMag        :Double;    {Thevinen equivalent voltage for dynamic model}
        YPrimOpenCond   :TCmatrix;  // To handle cases where one conductor of load is open ; We revert to admittance for inj currents
        YQFixed         :Double;  // Fixed value of y for type 7 load
        ShapeIsActual   :Boolean;
        ForceBalanced   :Boolean;

        PROCEDURE CalcDailyMult(Hr:double);
        PROCEDURE CalcDutyMult(Hr:double);  // now incorporates DutyStart offset
        Procedure CalcGenModelContribution(ActorID : Integer);
        Procedure CalcInjCurrentArray(ActorID : Integer);
        Procedure CalcVterminal(ActorID : Integer);
        Procedure CalcVTerminalPhase(ActorID : Integer);
        Procedure CalcVthev_Dyn;      // 3-phase Voltage behind transient reactance
        Procedure CalcVthev_Dyn_Mod7(const V:Complex);
        PROCEDURE CalcYearlyMult(Hr:double);
        Procedure CalcYPrimMatrix(Ymatrix:TcMatrix;ActorID : Integer);

        Procedure DoConstantPQGen(ActorID : Integer);
        Procedure DoConstantZGen(ActorID : Integer);
        Procedure DoCurrentLimitedPQ(ActorID : Integer);
        PROCEDURE DoDynamicMode(ActorID : Integer);
        PROCEDURE DoFixedQGen(ActorID : Integer);
        PROCEDURE DoFixedQZGen(ActorID : Integer);
        PROCEDURE DoHarmonicMode(ActorID : Integer);
        Procedure DoPVTypeGen(ActorID : Integer);
        Procedure DoUserModel(ActorID : Integer);

        Procedure Integrate(Reg:Integer; const Deriv:Double; Const Interval:Double; ActorID: integer);
        Procedure SetDragHandRegister(Reg:Integer; const Value:Double);
        Procedure StickCurrInTerminalArray(TermArray:pComplexArray; Const Curr:Complex; i:Integer);

        Procedure WriteTraceRecord(const s:string;ActorID : Integer);

        procedure SyncUpPowerQuantities;


        Function Get_PresentkW:Double;
        Function Get_Presentkvar:Double;
        function Get_PresentkV: Double;
        procedure Set_PresentkV(const Value: Double);
        procedure Set_Presentkvar(const Value: Double);
        procedure Set_PresentkW(const Value: Double);
        procedure Set_PowerFactor(const Value: Double);

        PROCEDURE SetkWkvar(const PkW, Qkvar:Double);

      Protected
        PROCEDURE Set_ConductorClosed(Index:Integer; ActorID:integer; Value:Boolean); Override;
        Procedure GetTerminalCurrents(Curr:pComplexArray; ActorID : Integer); Override ;

      public

        WindModelDyn    : TGE_WTG3_Model;
        Connection      :Integer;  {0 = line-neutral; 1=Delta}
        DailyDispShape  :String;  // Daily (24 HR) WindGen shape
        DailyDispShapeObj :TLoadShapeObj;  // Daily WindGen Shape for this load
        DutyShape       :String;  // Duty cycle load shape for changes typically less than one hour
        DutyShapeObj    :TLoadShapeObj;  // Shape for this WindGen
        DutyStart       :Double; // starting time offset into the DutyShape [hrs] for this WindGen
        GenClass        :Integer;
        GenModel        :Integer;   // Variation with voltage
        WindGenVars     :TWindGenVars; {State Variables}
        kvarBase        :Double;
        kvarMax         :Double;
        kvarMin         :Double;
        kWBase          :Double;
        PFNominal       :Double;
        Vpu             :Double;   // per unit Target voltage for WindGen with voltage control
        Vmaxpu          :Double;
        Vminpu          :Double;
        VV_Curve        :String;
        VV_CurveObj     :TXYcurveObj;
        Loss_CurveObj   :TXYcurveObj;

        GenActive: Boolean;
// Fuel variables from Generator model removed

// moved to WindGenVars        VTarget         :Double;  // Target voltage for WindGen with voltage control
        YearlyShape     :String;  // ='fixed' means no variation  on all the time
        YearlyShapeObj  :TLoadShapeObj;  // Shape for this WindGen

        Registers,  Derivatives         :Array[1..NumWGenregisters] of Double;

        constructor Create(ParClass :TDSSClass; const SourceName :String);
        destructor  Destroy; override;

        Procedure RecalcElementData(ActorID : Integer); Override;
        Procedure CalcYPrim(ActorID : Integer); Override;

        Function  InjCurrents(ActorID : Integer):Integer; Override;
        Procedure GetInjCurrents(Curr:pComplexArray; ActorID : Integer); Override;
        Function  NumVariables:Integer;Override;
        Procedure GetAllVariables(States:pDoubleArray);Override;
        Function  Get_Variable(i: Integer): Double; Override;
        procedure Set_Variable(i: Integer; Value: Double);  Override;
        Function  VariableName(i:Integer):String ;Override;

        Procedure SetNominalGeneration(ActorID : Integer);
        Procedure Randomize(Opt:Integer);   // 0 = reset to 1.0; 1 = Gaussian around mean and std Dev  ;  // 2 = uniform

        Procedure ResetRegisters;
        Procedure TakeSample(ActorID: integer);

        // Procedures for setting the DQDV used by the Solution Object
        Procedure InitDQDVCalc;
        Procedure BumpUpQ;
        Procedure RememberQV(ActorID : Integer);
        Procedure CalcDQDV(ActorID : Integer);
        Procedure ResetStartPoint;

        // Support for Dynamics Mode
        Procedure InitStateVars(ActorID : Integer); Override;
        Procedure IntegrateStates(ActorID : Integer);Override;

        // Support for Harmonics Mode
        Procedure InitHarmonics(ActorID : Integer); Override;

       PROCEDURE MakePosSequence(ActorID : Integer);Override;  // Make a positive Sequence Model

       PROCEDURE InitPropertyValues(ArrayOffset:Integer);Override;
       Procedure DumpProperties(Var F:TextFile; Complete:Boolean);Override;
       FUNCTION  GetPropertyValue(Index:Integer):String;Override;
       FUNCTION CheckIfDynVar(myVar  : String; ActorID : Integer):Integer;    // for Dynamic expressions
       PROCEDURE SetDynOutput(myVar  : String);                               // for Dynamic expressions
       FUNCTION GetDynOutputStr(): string;                                    // for Dynamic expressions

       Property PresentkW    :Double  Read Get_PresentkW   Write Set_PresentkW;
       Property Presentkvar  :Double  Read Get_Presentkvar Write Set_Presentkvar;
       Property ForcedON     :Boolean Read FForcedON       Write FForcedON;
       Property PresentkV    :Double  Read Get_PresentkV   Write Set_PresentkV;
       Property PowerFactor  :Double  Read PFNominal       Write Set_PowerFactor;

   End;

VAR
    ActiveWindGenObj:TWindGenObj;

// = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = =
implementation


USES  ParserDel, Circuit,  Sysutils, Command, Math, MathUtil, DSSClassDefs, DSSGlobals, Utilities, Classes;

Const NumPropsThisClass = 44;  // removed junk variables
  // Dispatch modes
      DEFAULT = 0;
      LOADMODE = 1;

Var cBuffer:Array[1..24] of Complex;  // Temp buffer for calcs  24-phase WindGen?

//- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
constructor TWindGen.Create;  // Creates superstructure for all objects
Begin
     Inherited Create;
     Class_Name := 'WindGen';
     DSSClassType := DSSClassType + WINDGEN_ELEMENT;  // In both PCelement and Genelement list

     ActiveElement := 0;

     // Set Register names
     RegisterNames[1]  := 'kWh';
     RegisterNames[2]  := 'kvarh';
     RegisterNames[3]  := 'Max kW';
     RegisterNames[4]  := 'Max kVA';
     RegisterNames[5]  := 'Hours';
     RegisterNames[6]  := '$';

     DefineProperties;

     CommandList := TCommandList.Create(PropertyName, NumProperties);
     CommandList.Abbrev := TRUE;

     WindGenClass[ActiveActor] := Self;
End;

//- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Destructor TWindGen.Destroy;

Begin
    // ElementList and  CommandList freed in inherited destroy
    Inherited Destroy;

End;

//- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Procedure TWindGen.DefineProperties;
Begin

     Numproperties := NumPropsThisClass;
     CountProperties;   // Get inherited property count
     AllocatePropertyArrays;   {see DSSClass}

     // Define Property names
     PropertyName^[1] := 'phases';
     PropertyHelp^[1] := 'Number of Phases, this WindGen.  Power is evenly divided among phases.';

     PropertyName^[2] := 'bus1';
     PropertyHelp^[2] := 'Bus to which the WindGen is connected.  May include specific node specification.';

     PropertyName^[3] := 'kv';
     PropertyHelp^[3] := 'Nominal rated (1.0 per unit) voltage, kV, for WindGen. For 2- and 3-phase WindGens, specify phase-phase kV. '+
                    'Otherwise, for phases=1 or phases>3, specify actual kV across each branch of the WindGen. '+
                    'If wye (star), specify phase-neutral kV. '+
                    'If delta or phase-phase connected, specify phase-phase kV.';

     PropertyName^[4] := 'kW';
     PropertyHelp^[4] := 'Total base kW for the WindGen.  A positive value denotes power coming OUT of the element, '+CRLF+
                    'which is the opposite of a load. This value is modified depending on the dispatch mode. ' +
                    'Unaffected by the global load multiplier and growth curves. ' +
                    'If you want there to be more generation, you must add more WindGens or change this value.';

     PropertyName^[5] := 'PF';
     PropertyHelp^[5] := 'WindGen power factor. Default is 0.80. Enter negative for leading powerfactor '+
                    '(when kW and kvar have opposite signs.)'+CRLF+
                    'A positive power factor for a WindGen signifies that the WindGen produces vars ' + CRLF +
                    'as is typical for a synchronous WindGen.  Induction machines would be ' +CRLF+
                    'generally specified with a negative power factor.';

     PropertyName^[6] := 'model';
     PropertyHelp^[6] := 'Integer code for the model to use for generation variation with voltage. '+
                    'Valid values are:' +CRLF+CRLF+
                    '1:WindGen injects a constant kW at specified power factor.'+CRLF+
                    '2:WindGen is modeled as a constant admittance.'  +CRLF+
                    '3:Const kW, constant kV.  Voltage-regulated model.'+CRLF+
                    '4:Const kW, Fixed Q (Q never varies)'+CRLF+
                    '5:Const kW, Fixed Q(as a constant reactance)'+CRLF+
                    '6:Compute load injection from User-written Model.(see usage of Xd, Xdp)';


     PropertyName^[7] := 'yearly';
     PropertyHelp^[7] := 'Wind speed shape to use for yearly-mode simulations.  Must be previously defined '+
                    'as a Loadshape object. If this is not specified, a constant value is assumed (no variation). '+
                    'Set to NONE to reset to no loadahape. ' +
                    'Nominally for 8760 simulations.  If there are fewer points in the designated shape than '+
                    'the number of points in the solution, the curve is repeated.';

     PropertyName^[8] := 'daily';
     PropertyHelp^[8] := 'Wind speed shape to use for daily-mode simulations.  Must be previously defined '+
                    'as a Loadshape object of 24 hrs, typically.' +
                    'Set to NONE to reset to no loadahape. ' ; // daily dispatch (hourly)

     PropertyName^[9] := 'duty';
     PropertyHelp^[9] := 'Load shape to use for duty cycle dispatch simulations such as for wind or solar generation. ' +
                    'Must be previously defined as a Loadshape object. '+
                    'Typically would have time intervals less than 1 hr -- perhaps, in seconds. '+
                    'Set to NONE to reset to no loadahape. ' +
                    'Designate the number of points to solve using the Set Number=xxxx command. '+
                    'If there are fewer points in the actual shape, the shape is assumed to repeat.';  // as for wind generation

      PropertyName^[10] := 'conn';
      PropertyHelp^[10] := '={wye|LN|delta|LL}.  Default is wye.';

      PropertyName^[11] := 'kvar';
      PropertyHelp^[11] := 'Specify the base kvar.  Alternative to specifying the power factor.  Side effect: '+
                          ' the power factor value is altered to agree based on present value of kW.';

      PropertyName^[12] := 'class';
      PropertyHelp^[12] := 'An arbitrary integer number representing the class of WindGen so that WindGen values may '+
                          'be segregated by class.'; // integer

      PropertyName^[13] := 'debugtrace';
      PropertyHelp^[13] := '{Yes | No }  Default is no.  Turn this on to capture the progress of the WindGen model ' +
                          'for each iteration.  Creates a separate file for each WindGen named "GEN_name.CSV".' ;

      PropertyName^[14] := 'Vminpu';
      PropertyHelp^[14] := 'Default = 0.90.  Minimum per unit voltage for which the Model is assumed to apply. ' +
                          'Below this value, the Windgen model reverts to a constant impedance model. For model 7, the current is ' +
                          'limited to the value computed for constant power at Vminpu.';

      PropertyName^[15] := 'Vmaxpu';
      PropertyHelp^[15] := 'Default = 1.10.  Maximum per unit voltage for which the Model is assumed to apply. ' +
                          'Above this value, the Windgen model reverts to a constant impedance model.';

      PropertyName^[16] := 'kVA';
      PropertyHelp^[16] := 'kVA rating of electrical machine. Defaults to 1.2* kW if not specified. Applied to machine or inverter definition for Dynamics mode solutions. ';

      PropertyName^[17] := 'MVA';
      PropertyHelp^[17] := 'MVA rating of electrical machine.  Alternative to using kVA=.';

      PropertyName^[18] := 'UserModel';
      PropertyHelp^[18] := 'Name of DLL containing user-written model, which computes the terminal currents for Dynamics studies, ' +
                                  'overriding the default model.  Set to "none" to negate previous setting.';
      PropertyName^[19] := 'UserData';
      PropertyHelp^[19] := 'String (in quotes or parentheses) that gets passed to user-written model for defining the data required for that model.';

      PropertyName^[20] := 'DutyStart';
      PropertyHelp^[20] := 'Starting time offset [hours] into the duty cycle shape for this WindGen, defaults to 0';

      PropertyName^[21] := 'DynamicEq';
      PropertyHelp^[21] := 'The name of the dynamic equation (DinamicExp) that will be used for defining the dynamic behavior of the generator. ' +
                                 'if not defined, the generator dynamics will follow the built-in dynamic equation.';

      PropertyName^[22] := 'DynOut';
      PropertyHelp^[22] := 'The name of the variables within the Dynamic equation that will be used to govern the generator dynamics.' +
                                 'This generator model requires 2 outputs from the dynamic equation: ' + CRLF + CRLF +
                                 '1. Shaft speed (velocity) relative to synchronous speed.' + CRLF +
                                 '2. Shaft, or power, angle (relative to synchronous reference frame).' + CRLF + CRLF +
                                 'The output variables need to be defined in the same order.';

      PropertyName^[23] := 'Rthev';
      PropertyHelp^[23] := 'per unit Thevenin equivalent R.';;

      PropertyName^[24] := 'Xthev';
      PropertyHelp^[24] := 'per unit Thevenin equivalent X.';

      PropertyName^[25] := 'Vss';
      PropertyHelp^[25] := 'Steady state voltage magnitude.';

      PropertyName^[26] := 'Pss';
      PropertyHelp^[26] := 'Steady state output real power.';

      PropertyName^[27] := 'Qss';
      PropertyHelp^[27] := 'Steady state output reactive power.';

      PropertyName^[28] := 'vwind';
      PropertyHelp^[28] := 'Wind speed in m/s';

      PropertyName^[29] := 'QMode';
      PropertyHelp^[29] := 'Q control mode (0:Q, 1:PF, 2:VV).';

      PropertyName^[30] := 'SimMechFlg';
      PropertyHelp^[30] := '1 to simulate mechanical system. Otherwise (0) only uses the electrical system. For dynamics simulation purposes.';

      PropertyName^[31] := 'APCFlg';
      PropertyHelp^[31] := '1 to enable active power control.';

      PropertyName^[32] := 'QFlg';
      PropertyHelp^[32] := '1 to enable reactive power and voltage control.';

      PropertyName^[33] := 'delt0';
      PropertyHelp^[33] := 'User defined internal simulation step.';

      PropertyName^[34] := 'N_WTG';
      PropertyHelp^[34] := 'Number of WTG in aggregation.';

      PropertyName^[35] := 'VV_Curve';
      PropertyHelp^[35] := 'Name of the XY curve defining the control curve for implementing Vol-var control with this inverter.';

      PropertyName^[36] := 'Ag';
      PropertyHelp^[36] := 'Gearbox ratio (Default 1/90).';

      PropertyName^[37] := 'Cp';
      PropertyHelp^[37] := 'Turbine performance coefficient (deafult 0.41).';

      PropertyName^[38] := 'Lamda';
      PropertyHelp^[38] := 'Tip speed ratio (Default 7.95).';

      PropertyName^[39] := 'P';
      PropertyHelp^[39] := 'Number of pole pairs of the induction generator (Default 2).';

      PropertyName^[40] := 'pd';
      PropertyHelp^[40] := 'Air density in kg/m3 (Default 1.225).';

      PropertyName^[41] := 'PLoss';
      PropertyHelp^[41] := 'Name of the XYCurve object describing the active power losses in pct versus the wind speed.';

      PropertyName^[42] := 'Rad';
      PropertyHelp^[42] := 'Rotor radius in meters (Default 40).';

      PropertyName^[43] := 'VCutIn';
      PropertyHelp^[43] := 'Cut-in speed for the wind generator (m/s - default 5).';

      PropertyName^[44] := 'VCutOut';
      PropertyHelp^[44] := 'Cut-out speed for the wind generator (m/s - default 23).';

      {Removed Fuel-related variables 40-44 from Generator model}
      {Added 40-41 to make Windgen comaptible with DynamicExp}

     ActiveProperty := NumPropsThisClass;
     inherited DefineProperties;  // Add defs of inherited properties to bottom of list

     // Override default help string
     PropertyHelp^[NumPropsThisClass +1] := 'Name of harmonic voltage or current spectrum for this WindGen. ' +
                         'Voltage behind Xd" for machine - default. Current injection for inverter. ' +
                         'Default value is "default", which is defined when the DSS starts.';

End;

//- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Function TWindGen.NewObject(const ObjName:String):Integer;
Begin
    // Make a new WindGen and add it to WindGen class list
    With ActiveCircuit[ActiveActor] Do
    Begin
      ActiveCktElement := TWindGenObj.Create(Self, ObjName);
      Result := AddObjectToList(ActiveDSSObject[ActiveActor]);
    End;
End;

//- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Procedure TWindGen.SetNcondsForConnection;

Begin
  With ActiveWindGenObj Do
  Begin
   CASE Connection OF
     0: NConds := Fnphases +1;
     1: CASE Fnphases OF
            1,2: NConds := Fnphases +1; // L-L and Open-delta
        ELSE
            NConds := Fnphases;
        End;
   End;
  End;
End;

//- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Procedure TWindGen.InterpretConnection(const S:String);

// Accepts
//    delta or LL           (Case insensitive)
//    Y, wye, or LN
VAR
    TestS:String;

Begin                       
        With ActiveWindGenObj Do Begin
            TestS := lowercase(S);
            CASE TestS[1] OF
              'y','w': Connection := 0;  {Wye}
              'd': Connection := 1;  {Delta or line-Line}
              'l': CASE Tests[2] OF
                   'n': Connection := 0;
                   'l': Connection := 1;
                   End;

            End;

            SetNCondsForConnection;

            {VBase is always L-N voltage unless 1-phase device or more than 3 phases}

            With WindGenVars Do {CASE Connection OF
              1: VBase := kVWindGenBase * 1000.0 ;
              Else}
                  Case Fnphases Of
                   2,3: VBase := kVWindGenBase * InvSQRT3x1000;    // L-N Volts
                   Else
                       VBase := kVWindGenBase * 1000.0 ;   // Just use what is supplied
                   End;
            {End;}
            VBase95  := Vminpu * VBase;
            VBase105 := Vmaxpu * VBase;

            Yorder := Fnconds * Fnterms;
            YprimInvalid[ActiveActor] := True;
        End;

End;

//- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
FUNCTION InterpretDispMode(const S:String):Integer;
BEGIN

        CASE lowercase(S)[1] of
           'l': Result := LOADMODE;
        ELSE
                Result := DEFAULT;
        END;

End;


//- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Function TWindGen.Edit(ActorID : Integer):Integer;
VAR
   VarIdx,
   i,
   ParamPointer:Integer;
   ParamName:String;
   Param:String;



Begin
  // continue parsing with contents of Parser
  ActiveWindGenObj := ElementList.Active;
  ActiveCircuit[ActorID].ActiveCktElement := ActiveWindGenObj;

  Result := 0;

  With ActiveWindGenObj Do
  Begin

     ParamPointer := 0;
     ParamName := Parser[ActorID].NextParam;
     Param := Parser[ActorID].StrValue;
     While Length(Param)>0 Do
     Begin
       If  (Length(ParamName) = 0)
       Then Inc(ParamPointer)
       ELSE ParamPointer := CommandList.GetCommand(ParamName);

       If  (ParamPointer>0) and (ParamPointer<=NumProperties)
       Then PropertyValue[PropertyIdxMap^[ParamPointer]] := Param
       ELSE
       Begin
         // first, checks if there is a dynamic eq assigned, then
         // checks if the new property edit the state variables within
         VarIdx   :=  CheckIfDynVar(ParamName, ActorID);
         if VarIdx < 0 then
           DoSimpleMsg('Unknown parameter "'+ParamName+'" for WindGen "'+Name+'"', 560);
       End;

       If ParamPointer > 0 Then
       CASE PropertyIdxMap^[ParamPointer] OF
          0: DoSimpleMsg('Unknown parameter "' + ParamName + '" for Object "' + Class_Name +'.'+ Name + '"', 561);
          1: NPhases              :=  Parser[ActorID].Intvalue; // num phases
          2: SetBus(1, param);
          3: PresentkV            :=  Parser[ActorID].DblValue;
          4: kWBase               :=  Parser[ActorID].DblValue;
          5: PFNominal            :=  Parser[ActorID].DblValue;
          6: GenModel             :=  Parser[ActorID].IntValue;
          7: YearlyShape          :=  Param;
          8: DailyDispShape       :=  Param;
          9: DutyShape            :=  Param;
         10: InterpretConnection(Param);
         11: Presentkvar          := Parser[ActorID].DblValue;
         12: GenClass             :=  Parser[ActorID].IntValue;
         14: VMinPu               :=  Parser[ActorID].DblValue;
         15: VMaxPu               :=  Parser[ActorID].DblValue;
         16: Begin
              WindModelDyn.EditProp(13,Param);
              WindGenVars.kVArating    :=  Parser[ActorID].DblValue;
             End;
         17: Begin
              WindGenVars.kVArating    :=  Parser[ActorID].DblValue * 1000.0;  // 'MVA';
              WindModelDyn.EditProp(13,FloatToStr(WindGenVars.kVArating));
             End;
         18: UserModel.Name       :=  Parser[ActorID].StrValue;  // Connect to user written models
         19: UserModel.Edit       :=  Parser[ActorID].StrValue;  // Send edit string to user model
         20: DutyStart            :=  Parser[ActorID].DblValue;
         21: DynamicEq            :=  Param;
         22: SetDynOutput(Param);
         23: WindModelDyn.EditProp(1,Param);
         24: WindModelDyn.EditProp(2,Param);
         25: WindModelDyn.EditProp(3,Param);
         26: WindModelDyn.EditProp(4,Param);
         27: WindModelDyn.EditProp(5,Param);
         28: WindModelDyn.EditProp(6,Param);
         29: WindModelDyn.EditProp(7,Param);
         30: WindModelDyn.EditProp(8,Param);
         31: WindModelDyn.EditProp(9,Param);
         32: WindModelDyn.EditProp(10,Param);
         33: WindModelDyn.EditProp(12,Param);
         34: WindModelDyn.EditProp(22,Param);
         35: VV_Curve := Param;
         36: WindgenVars.ag     :=  Parser[ActorID].DblValue;
         37: WindgenVars.Cp     :=  Parser[ActorID].DblValue;
         38: WindgenVars.Lamda  :=  Parser[ActorID].DblValue;
         39: WindgenVars.Poles  :=  Parser[ActorID].DblValue;
         40: WindgenVars.pd     :=  Parser[ActorID].DblValue;
         41: WindgenVars.PLoss  :=  Parser[ActorID].StrValue;
         42: WindgenVars.Rad    :=  Parser[ActorID].DblValue;
         43: WindgenVars.VCutin :=  Parser[ActorID].DblValue;
         44: WindgenVars.VCutout:=  Parser[ActorID].DblValue;

       ELSE
         // Inherited parameters
           ClassEdit(ActiveWindGenObj, ParamPointer - NumPropsThisClass)
       End;

       If ParamPointer > 0 Then
       CASE PropertyIdxMap^[ParamPointer] OF
          1: SetNcondsForConnection;  // Force Reallocation of terminal info

          // keep kvar nominal up to date with kW and PF
          4,5: SyncUpPowerQuantities;

          // if a model 3 WindGen added, force calc of dQdV
          6: If GenModel=3 Then ActiveCircuit[ActorID].Solution.SolutionInitialized := FALSE;

  {Set shape objects;  returns nil if not valid}
   {Sets the kW and kvar properties to match the peak kW demand from the Loadshape}
          7: Begin
                YearlyShapeObj := LoadShapeClass[ActorID].Find(YearlyShape);
                If Assigned(YearlyShapeObj) then With YearlyShapeObj Do
                      If UseActual then SetkWkvar(MaxP, MaxQ);
             End;
          8: Begin
              DailyDispShapeObj := LoadShapeClass[ActorID].Find(DailyDispShape);
                If Assigned(DailyDispShapeObj) then With DailyDispShapeObj Do
                      If UseActual then SetkWkvar(MaxP, MaxQ);
             End;
          9: Begin
                  DutyShapeObj := LoadShapeClass[ActorID].Find(DutyShape);
                  If Assigned(DutyShapeObj) then With DutyShapeObj Do
                      If UseActual then SetkWkvar(MaxP, MaxQ);
             End;

          13: IF DebugTrace
              THEN Begin
                 WindModelDyn.EditProp(11,'1');
                 AssignFile(TraceFile, GetOutputDirectory + 'WINDGEN_'+Name+'.CSV');
                 ReWrite(TraceFile);
                 Write(TraceFile, 't, Iteration, LoadMultiplier, Mode, LoadModel, GenModel, dQdV, Avg_Vpu, Vdiff, MQnominalperphase, MPnominalperphase, CurrentType');
                 For i := 1 to nphases Do Write(Tracefile,  ', |Iinj'+IntToStr(i)+'|');
                 For i := 1 to nphases Do Write(Tracefile,  ', |Iterm'+IntToStr(i)+'|');
                 For i := 1 to nphases Do Write(Tracefile,  ', |Vterm'+IntToStr(i)+'|');
                 Write(TraceFile, ',Vthev, Theta');
                 Writeln(TraceFile);
                 CloseFile(Tracefile);
              End;
          16, 17: kVANotSet := FALSE;
          21: Begin
                DynamicEqObj :=  TDynamicExpClass[ActorID].Find(DynamicEq);
                If Assigned(DynamicEqObj) then With DynamicEqObj Do
                  setlength(DynamicEqVals, NumVars);
              End;
          35: Begin     // get the Volt-var control curve
                VV_CurveObj := XYCurveClass[ActorID].Find(VV_Curve);
                If Assigned(VV_CurveObj) then
                Begin
                  With VV_CurveObj Do
                  Begin
                    WindModelDyn.EditProp(14,FloatToStr(XValue_pt[1]));
                    WindModelDyn.EditProp(15,FloatToStr(XValue_pt[2]));
                    WindModelDyn.EditProp(16,FloatToStr(XValue_pt[3]));
                    WindModelDyn.EditProp(17,FloatToStr(XValue_pt[4]));
                    WindModelDyn.EditProp(18,FloatToStr(YValue_pt[1]));
                    WindModelDyn.EditProp(19,FloatToStr(YValue_pt[2]));
                    WindModelDyn.EditProp(20,FloatToStr(YValue_pt[3]));
                    WindModelDyn.EditProp(21,FloatToStr(YValue_pt[4]));
                  End;
                End
                Else
                Begin
                  DoSimpleMsg('Volt-var control curve "'+VV_Curve+'" not found, make sure that it was not defined before this element', 565);
                End;
              End;
          41: Begin     // get the Volt-var control curve
                Loss_CurveObj := XYCurveClass[ActorID].Find(WindgenVars.PLoss);
                If not Assigned(Loss_CurveObj) then
                Begin
                  DoSimpleMsg('Losses curve "'+ WindgenVars.PLoss +'" not found, make sure that it was not defined before this element', 566);
                  WindgenVars.PLoss := '';
                End;
              End;
       End;

       ParamName := Parser[ActorID].NextParam;
       Param     := Parser[ActorID].StrValue;
     End;

     RecalcElementData(ActorID);
     YprimInvalid[ActorID] := True;
  End;

End;

//----------------------------------------------------------------------------
Function TWindGen.MakeLike(Const OtherWindGenName:String):Integer;
VAR
   OtherWindGen:TWindGenObj;
   i:Integer;
Begin
   Result := 0;
   {See if we can find this line name in the present collection}
   OtherWindGen := Find(OtherWindGenName);
   If   (OtherWindGen <> Nil)
   Then With ActiveWindGenObj Do
   Begin

       If (Fnphases <> OtherWindGen.Fnphases) Then Begin
         Nphases := OtherWindGen.Fnphases;
         NConds := Fnphases;  // Forces reallocation of terminal stuff

         Yorder := Fnconds*Fnterms;
         YprimInvalid[ActiveActor] := True;
       End;

       WindGenVars.kVWindGenBase := OtherWindGen.WindGenVars.kVWindGenBase;
       Vbase                  := OtherWindGen.Vbase;
       Vminpu                 := OtherWindGen.Vminpu;
       Vmaxpu                 := OtherWindGen.Vmaxpu;
       Vbase95                := OtherWindGen.Vbase95;
       Vbase105               := OtherWindGen.Vbase105;
       kWBase                 := OtherWindGen.kWBase;
       kvarBase               := OtherWindGen.kvarBase;
       WindGenVars.Pnominalperphase       := OtherWindGen.WindGenVars.Pnominalperphase;
       PFNominal              := OtherWindGen.PFNominal;
       WindGenVars.Qnominalperphase     := OtherWindGen.WindGenVars.Qnominalperphase;
       varMin                 := OtherWindGen.varMin;
       varMax                 := OtherWindGen.varMax;
       Connection             := OtherWindGen.Connection;
     //  Rneut          := OtherWindGen.Rneut;
      // Xneut          := OtherWindGen.Xneut;
       YearlyShape            := OtherWindGen.YearlyShape;
       YearlyShapeObj         := OtherWindGen.YearlyShapeObj;
       DailyDispShape         := OtherWindGen.DailyDispShape;
       DailyDispShapeObj      := OtherWindGen.DailyDispShapeObj;
       DutyShape              := OtherWindGen.DutyShape;
       DutyShapeObj           := OtherWindGen.DutyShapeObj;
       DutyStart              := OtherWindGen.DutyStart;
       GenClass               := OtherWindGen.GenClass;
       GenModel               := OtherWindGen.GenModel;
       IsFixed                := OtherWindGen.IsFixed;
       WindGenVars.VTarget        := OtherWindGen.WindGenvars.VTarget;
       Vpu                    := OtherWindGen.Vpu;
       kvarMax                := OtherWindGen.kvarMax;
       kvarMin                := OtherWindGen.kvarMin;
       FForcedON              := OtherWindGen.FForcedON;
       kVANotSet              := OtherWindGen.kVANotSet;

       WindGenVars.kVArating      := OtherWindGen.WindGenVars.kVArating;
       WindGenVars.puXd           := OtherWindGen.WindGenVars.puXd;
       WindGenVars.puXdp          := OtherWindGen.WindGenVars.puXdp;
       WindGenVars.puXdpp         := OtherWindGen.WindGenVars.puXdpp;
       WindGenVars.Hmass          := OtherWindGen.WindGenVars.Hmass;
       WindGenVars.Theta          := OtherWindGen.WindGenVars.Theta;
       WindGenVars.Speed          := OtherWindGen.WindGenVars.Speed;
       WindGenVars.w0             := OtherWindGen.WindGenVars.w0;
       WindGenVars.dSpeed         := OtherWindGen.WindGenVars.dSpeed;
       WindGenVars.D              := OtherWindGen.WindGenVars.D;
       WindGenVars.Dpu            := OtherWindGen.WindGenVars.Dpu;
       WindGenVars.XRdp           := OtherWindGen.WindGenVars.Xrdp;

       UserModel.Name    := OtherWindGen.UserModel.Name;  // Connect to user written models
       ShaftModel.Name   := OtherWindGen.ShaftModel.Name;

       ClassMakeLike(OtherWindGen);

       For i := 1 to ParentClass.NumProperties Do
           FPropertyValue[i] := OtherWindGen.FPropertyValue[i];

       Result := 1;
   End
   ELSE  DoSimpleMsg('Error in Load MakeLike: "' + OtherWindGenName + '" Not Found.', 562);

End;

//----------------------------------------------------------------------------
Function TWindGen.Init(Handle:Integer; ActorID : Integer):Integer;
VAR
   p:TWindGenObj;

Begin

   If (Handle = 0)   Then Begin  // init all
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

   DoSimpleMsg('Need to implement TWindGen.Init', -1);
   Result := 0;

End;

{--------------------------------------------------------------------------}
Procedure TWindGen.ResetRegistersAll(ActorID : Integer);  // Force all EnergyMeters in the circuit to reset

VAR
   pGen:TWindGenObj;

Begin
      pGen := ActiveCircuit[ActorID].WindGens.First;
      WHILE (pGen <> Nil) Do
      Begin
          pGen.ResetRegisters;
          pGen := ActiveCircuit[ActorID].WindGens.Next;
      End;

End;

{--------------------------------------------------------------------------}
Procedure TWindGen.SampleAll(ActorID : Integer);  // Force all EnergyMeters in the circuit to take a sample

VAR
   pGen:TWindGenObj;

Begin
      pGen := ActiveCircuit[ActorID].WindGens.First;
      WHILE pGen<>Nil Do
      Begin
          If pGen.enabled Then pGen.TakeSample(ActorID);
          pGen := ActiveCircuit[ActorID].WindGens.Next;
      End;
End;

//----------------------------------------------------------------------------
Constructor TWindGenObj.Create(ParClass:TDSSClass; const SourceName:String);
Begin
     Inherited create(ParClass);
     Name := LowerCase(SourceName);
     DSSObjType := ParClass.DSSClassType ; // + WINDGEN_ELEMENT;  // In both PCelement and Genelement list

     Nphases      := 3;
     Fnconds       := 4;  // defaults to wye
     Yorder       := 0;  // To trigger an initial allocation
     Nterms := 1;  // forces allocations
     kWBase       := 1000.0;
     kvarBase     := 60.0;


     kvarMax      := kvarBase * 2.0;
     kvarMin      :=-kvarmax;
     PFNominal    := 0.88;
     YearlyShape    := '';
     YearlyShapeObj := nil;  // if YearlyShapeobj = nil then the load alway stays nominal * global multipliers
     DailyDispShape := '';
     DailyDispShapeObj := nil;  // if DaillyShapeobj = nil then the load alway stays nominal * global multipliers
     DutyShape         := '';
     DutyShapeObj      := nil;  // if DutyShapeobj = nil then the load alway stays nominal * global multipliers
     DutyStart         := 0.0;
     Connection        := 0;    // Wye (star)
     GenModel          := 1;  {Typical fixed kW negative load}
     GenClass          := 1;
     LastYear          := 0;
     LastGrowthFactor  := 1.0;

     DQDVSaved  := 0.0;  // Initialize this here.  Allows WindGens to be turned off and on


     WindGenSolutionCount     := -1;  // For keep track of the present solution in Injcurrent calcs
     OpenWindGenSolutionCount := -1;
     YPrimOpenCond              := nil;

     WindGenVars.kVWindGenBase  := 12.47;
     Vpu              := 1.0;
     WindGenVars.VTarget  := 1000.0 * Vpu * WindGenVars.kVWindGenBase / SQRT3;  {Line-to-Neutral target}
     VBase            := 7200.0;
     Vminpu           := 0.90;
     Vmaxpu           := 1.10;
     VBase95          := Vminpu * Vbase;
     VBase105         := Vmaxpu * Vbase;
     Yorder           := Fnterms * Fnconds;
     RandomMult       := 1.0 ;
     IsFixed          := FALSE;

     {Machine rating stuff}
     WindGenVars.kVArating  := kWBase *1.2;
     kVANotSet   := TRUE;  // Flag for default value for kVA

     NumStateVars := NumWGenVariables;

     With WindGenVars Do
     Begin

        // These are inherited from the generator object, it is uncertain if needed
        puXd        := 1.0;
        puXdp       := 0.28;
        puXdpp      := 0.20;
        Xd          :=  puXd   * SQR(kVWindGenBase) * 1000.0 / kVARating;
        Xdp         :=  puXdp  * SQR(kVWindGenBase) * 1000.0 / kVARating;
        Xdpp        :=  puXdpp * SQR(kVWindGenBase) * 1000.0 / kVARating;
        Hmass       := 1.0;       //  W-sec/VA rating
        Theta       := 0.0;
        w0          := TwoPi * Basefrequency;
        Speed       := 0.0;
        dSpeed      := 0.0;
        D           := 1.0;
        XRdp        := 20.0;
        // Added for the wind generator specifically
        PLoss       := '';
        ag          := 1/90;
        Cp          := 0.41;
        Lamda       := 7.95;
        Poles       := 2;
        pd          := 1.225;
        Rad         := 40;
        VCutin      := 5;
        VCutout     := 23;
        Pm          := 0;
        Ps          := 0;
        Pr          := 0;
        Pg          := 0;
        s           := 0;
     End;

     {Advertise Genvars struct as public}

     PublicDataStruct := pointer(@WindGenVars);
     PublicDataSize   := SizeOf(TWindGenVars);

     UserModel  := TWindGenUserModel.Create(@WindGenVars) ;
     ShaftModel := TWindGenUserModel.Create(@WindGenVars);

  // Register values inherited from Generator model
     Reg_kWh        := 1;
     Reg_kvarh      := 2;
     Reg_MaxkW      := 3;
     Reg_MaxkVA     := 4;
     Reg_Hours      := 5;
     Reg_Price      := 6;

     PVFactor       := 0.1;
     DebugTrace     := FALSE;
     FForcedON      := FALSE;
     GenSwitchOpen  := FALSE;
     ShapeIsActual  := FALSE;
     ForceBalanced  := FALSE;

     Spectrum := 'defaultgen';  // override base class

     GenActive      :=  True;   // variable to use if needed

     // Creates the Dynamic model for the Wind Turbine
     WindModelDyn  :=  TGE_WTG3_Model.Create(WindGenVars, ActiveCircuit[Activeactor].Solution.DynaVars);
     WindModelDyn.EditProp(6,'12');
     WindModelDyn.QMode := 0;
     InitPropertyValues(0);

     RecalcElementData(ActiveActor);

End;


//----------------------------------------------------------------------------
Destructor TWindGenObj.Destroy;
Begin
    YPrimOpenCond.Free;
    UserModel.Free;
    ShaftModel.Free;
    Inherited Destroy;
End;

//----------------------------------------------------------------------------
Procedure TWindGenObj.Randomize(Opt:Integer);
Begin
   CASE Opt OF
       0: RandomMult := 1.0;
       GAUSSIAN:  RandomMult := Gauss(YearlyShapeObj.Mean, YearlyShapeObj.StdDev);
       UNIfORM:   RandomMult := Random;  // number between 0 and 1.0
       LOGNORMAL: RandomMult := QuasiLognormal(YearlyShapeObj.Mean);
   End;
End;

//----------------------------------------------------------------------------
{Evaluates if the value provided corresponds to a constant value or to an operand
 for calculating the value using the simulation results}
FUNCTION TWindGenObj.CheckIfDynVar(myVar  : String; ActorID : Integer):Integer;
var
  myOp    : Integer;        // Operator found
  myValue : String;         // Value entered by the user
Begin

  Result := -1;
  If Assigned(DynamicEqObj) then
  Begin

    Result  :=   DynamicEqObj.Get_Var_Idx(myVar);
    if (Result >= 0) and (Result < 50000) then
    Begin
      myValue :=  Parser[ActorID].StrValue;
      if (DynamicEqObj.Check_If_CalcValue(myValue, myOp)) then
      Begin
        // Adss the pair (var index + operand index)
        setlength(DynamicEqPair,length(DynamicEqPair) + 2);
        DynamicEqPair[High(DynamicEqPair) - 1]  :=  Result;
        DynamicEqPair[High(DynamicEqPair)]      :=  myOp;
      End
      else // Otherwise, move the value to the values array
         DynamicEqVals[Result][0]  :=  Parser[ActorID].DblValue;
    End
    else
      Result := -1;     // in case is a constant

  End;

End;

//----------------------------------------------------------------------------
{Obtains the indexes of the given variables to use them as reference for setting
the dynamic output for the generator}
PROCEDURE TWindGenObj.SetDynOutput(myVar  : String);
var
  VarIdx,
  idx         : Integer;
  myStrArray  : TStringList;
Begin
  if DynamicEqObj <> nil then        // Making sure we have a dynamic eq linked
  Begin
    // First, set the length for the index array, 2 variables in this case
    setlength(DynOut,2);
    myStrArray  :=  TStringList.Create;
    InterpretTStringListArray(myVar, myStrArray);
    // ensuring they are lower case
    for idx := 0 to 1 do
    Begin

      myStrArray[idx]  :=  LowerCase(myStrArray[idx]);
      VarIdx           :=  DynamicEqObj.Get_Out_Idx(myStrArray[idx]);
      if ( VarIdx < 0 ) then
        // Being here means that the given name doesn't exist or is a constant
        DoSimpleMsg('DynamicExp variable "' + myStrArray[idx] + '" not found or not defined as an output.', 50008)
      else
        DynOut[idx] :=  VarIdx;

    End;

    myStrArray.Free;
  End
  else
      DoSimpleMsg('A DynamicExp object needs to be assigned to this element before this declaration: DynOut = [' + myVar + ']', 50007);
End;

//----------------------------------------------------------------------------
{Returns the names of the variables to be used as outputs for the dynamic expression}
FUNCTION TWindGenObj.GetDynOutputStr(): string;
var
  idx   : Integer;
Begin
  Result  :=  '[';                   // Open array str
  if DynamicEqObj <> nil then        // Making sure we have a dynamic eq linked
  Begin
    for idx := 0 to High(DynOut) do
      Result  :=  Result + DynamicEqObj.Get_VarName(DynOut[idx]) + ',';
  End;

  Result  :=  Result + ']';         // Close array str
End;

//----------------------------------------------------------------------------
Procedure TWindGenObj.CalcDailyMult(Hr:Double);

Begin
     If (DailyDispShapeObj <> Nil) Then
       Begin
         ShapeFactor := DailyDispShapeObj.GetMult(Hr);
         ShapeIsActual := DailyDispShapeObj.UseActual;
       End
     ELSE ShapeFactor := cmplx(WindModelDyn.vwind,0);  // Default to no daily variation
End;


//----------------------------------------------------------------------------
Procedure TWindGenObj.CalcDutyMult(Hr:Double);

Begin
     If DutyShapeObj <> Nil Then
       Begin
         ShapeFactor := DutyShapeObj.GetMult(Hr + DutyStart);
         ShapeIsActual := DutyShapeObj.UseActual;
       End
     ELSE CalcDailyMult(Hr);  // Default to Daily Mult if no duty curve specified
End;

//----------------------------------------------------------------------------
Procedure TWindGenObj.CalcYearlyMult(Hr:Double);

Begin
{Yearly curve is assumed to be hourly only}
 If YearlyShapeObj<>Nil Then Begin
      ShapeFactor := YearlyShapeObj.GetMult(Hr);
      ShapeIsActual := YearlyShapeObj.UseActual;
 End
 ELSE
      ShapeFactor := cmplx(WindModelDyn.vwind,0);  // Defaults to no variation

End;



//----------------------------------------------------------------------------
Procedure TWindGenObj.SetNominalGeneration(ActorID : Integer);
VAR
   myV          : complex;
   VMag,
   VMagTmp,
   LeadLag,
   kVATmp,
   kvarCalc,
   myLosses,
   Factor       : Double;
   GenOn_Saved  : Boolean;
   i            : integer;

Begin
  VMag    := 0.0;
  VMagTmp := 0.0;
  myV     := CZero;
  GenOn_Saved := GenON;
  ShapeFactor := cmplx(WindModelDyn.vwind,0);
  // Check to make sure the generation is ON
  With ActiveCircuit[ActorID], ActiveCircuit[ActorID].Solution Do
  Begin

    kvarCalc  :=  0.0;
    GenON := TRUE;   // The first assumption is that the generator is ON

    // first, get wind speed (Factor)
    If IsFixed Then
    Begin
       Factor := 1.0;   // for fixed WindGens, set constant
    End
    ELSE
    Begin
      CASE Mode OF
          SNAPSHOT:     Factor := GenMultiplier;
          DAILYMODE:    Begin
                             Factor := GenMultiplier  ;
                             CalcDailyMult(DynaVars.dblHour) // Daily dispatch curve
                        End;
          YEARLYMODE:   Begin Factor := GenMultiplier; CalcYearlyMult(DynaVars.dblHour);  End;
          DUTYCYCLE:    Begin Factor := GenMultiplier; CalcDutyMult(DynaVars.dblHour) ; End;
          GENERALTIME,   // General sequential time simulation
          DYNAMICMODE:  Begin
                           Factor := GenMultiplier;
                           // This mode allows use of one class of load shape
                           case ActiveLoadShapeClass of
                                USEDAILY:  CalcDailyMult(DynaVars.dblHour);
                                USEYEARLY: CalcYearlyMult(DynaVars.dblHour);
                                USEDUTY:   CalcDutyMult(DynaVars.dblHour);
                           else
                                ShapeFactor := cmplx(WindModelDyn.vwind,0);     // default to the wind speed set by default
                           end;
                        End;
          MONTECARLO1,
          MONTEFAULT,
          FAULTSTUDY:  Factor := GenMultiplier * 1.0;
          MONTECARLO2,
          MONTECARLO3,
          LOADDURATION1,
          LOADDURATION2:Begin Factor := GenMultiplier; CalcDailyMult(DynaVars.dblHour); End;
          PEAKDAY:      Begin Factor := GenMultiplier; CalcDailyMult(DynaVars.dblHour); End;
          AUTOADDFLAG:  Factor := 1.0;
      ELSE
          Factor := GenMultiplier;
      End;
    End;
    WindModelDyn.vwind := ShapeFactor.re;
    if (ShapeFactor.re > WindgenVars.VCutout) or (ShapeFactor.re < WindgenVars.VCutin) then
    Begin
       WindGenvars.Pnominalperphase   := 0.001* kWBase;
       WindGenvars.Qnominalperphase   := 0.0;
       WindGenvars.Pm := 0.0;
       WindGenvars.Pg := 0.0;
       WindGenvars.Ps := 0.0;
       WindGenvars.Pr := 0.0;
       WindGenvars.s := 0.0;
    End
    Else
    Begin
      IF NOT (IsDynamicModel or IsHarmonicModel) THEN         //******
      Begin
        // start by getting the losses from the provided curve (if any)
        if Assigned(Loss_CurveObj) then
          myLosses :=  Loss_CurveObj.GetYValue(WindModelDyn.vwind)
        else
          myLosses := 0.0;  // no losses given that the curve was not provided

        LeadLag := 1;
        With WindgenVars Do
        Begin
          Pm    := 0.5 * pd * PI * math.Power(Rad,2) * math.Power(Shapefactor.re,3) * Cp;
          myLosses  := Pm * myLosses / 100;
          Pg    := (Pm - myLosses) / 1e3;     // in kW
          if Pg > kWBase then
            Pg  := kWBase;                    // Generation limits
          s     := 1 - ( (Poles * Shapefactor.re * Lamda) / (w0 * ag* Rad) );
          Ps    := Pg / (1 - s);
          Pr    := Ps * s;

          Pnominalperphase   := ( 1e3 * Factor * Pg ) / Fnphases;
          // Now check for Q depending on QMode
          case WindModelDyn.QMode of
            1: // PF
                Begin
                  kvarCalc  := math.Power(Pg / Abs(PFNominal), 2) - math.Power(Pg, 2);
                  kvarCalc := sqrt(kvarCalc);
                  kVATmp := sqrt(math.Power(Pg,2) + math.Power(kvarCalc,2));

                  if kVATmp > KVARating then        // Check saturation
                    kvarCalc := kvarBase;

                  if PFNominal < 0 then
                    LeadLag := -1.0;
                End;
            2: // Volt-var ctrl
                Begin
                  if Assigned(NodeRef) then
                  Begin
                    // get the highest voltage done locally given with whatever is on memory
                    for i := 1 to NumPhases do
                    Begin
                      myV     := NodeV[NodeRef[i]];
                      VMagTmp := ctopolar(myV).mag;
                      if VMagTmp > VMag then
                        VMag := VmagTmp;
                    End;
                    Vmag := Vmag / VBase;   // in pu

                    // start by getting the losses from the provided curve (if any)
                    if Assigned(VV_CurveObj) then
                      VmagTmp :=  VV_CurveObj.GetYValue(Vmag)
                    else
                      VmagTmp := 0.0;  // no losses given that the curve was not provided
                  End
                  Else
                    VmagTmp := 0.0;

                  // Calculates Q based on the
                  kvarCalc := kvarBase * VmagTmp;
                  if Abs(kvarCalc) > kvarBase then
                  Begin
                    kvarCalc := kvarBase;
                    if VmagTmp < 0 then  LeadLag := -1.0;
                  End;
                End
            Else
              kvarCalc := 0;
          end;


          Qnominalperphase := 1e3 * kvarCalc * LeadLag * Factor / Fnphases;
        End;

      End;

    End;

    IF NOT (IsDynamicModel or IsHarmonicModel) THEN         //******
    Begin
      // build the Y primitive eq
      CASE GenModel  of
           6: Yeq := Cinv(cmplx(0.0, -WindGenvars.Xd))  ;  // Gets negated in CalcYPrim
      ELSE
          With WindGenvars Do Yeq  := CDivReal(Cmplx(Pnominalperphase, -Qnominalperphase), Sqr(Vbase));   // Vbase must be L-N for 3-phase
          If   (Vminpu <> 0.0) Then Yeq95 := CDivReal(Yeq, sqr(Vminpu))  // at 95% voltage
                               Else Yeq95 := Yeq; // Always a constant Z model

          If   (Vmaxpu <> 0.0) Then  Yeq105 := CDivReal(Yeq, Sqr(Vmaxpu))   // at 105% voltage
                               Else  Yeq105 := Yeq;
      END;
    End;

     // If WindGen state changes, force re-calc of Y matrix
     If GenON <> GenON_Saved Then YprimInvalid[ActorID] := True;
  End;

End;

//----------------------------------------------------------------------------
Procedure TWindGenObj.RecalcElementData(ActorID : Integer);

Begin

    VBase95  := VMinPu * VBase;
    VBase105 := VMaxPu * VBase;

    varBase  := 1000.0 * kvarBase / Fnphases;
    varMin   := 1000.0 * kvarMin  / Fnphases;
    varMax   := 1000.0 * kvarMax  / Fnphases;

    {Populate data structures used for interchange with user-written models.}
    With WindGenvars Do
      Begin
        Xd    :=  puXd   * 1000.0 * SQR(kVWindGenBase)/kVARating;
        Xdp   :=  puXdp  * 1000.0 * SQR(kVWindGenBase)/kVArating;
        Xdpp  :=  puXdpp * 1000.0 * SQR(kVWindGenBase)/kVArating;
        Conn := connection;
        NumPhases := Fnphases;
        NumConductors := Fnconds;

        if not (kVANotSet) then
        Begin
          kWBase    :=  (kVArating * Abs(PFNominal));
          kvarbase  :=  sqrt(sqr(kVArating) - sqr(kWBase));
        End
        Else
        Begin
          kVArating :=  kWBase / Abs(PFNominal);
          WindModelDyn.EditProp(13,FloatToStr(kVArating));
        End;
      End;

    SetNominalGeneration(ActorID);

    {Now check for errors.  If any of these came out nil and the string was not nil, give warning}
    If CompareText(YearlyShape,    'none')=0 Then YearlyShape    := '';
    If CompareText(DailyDispShape, 'none')=0 Then DailyDispShape := '';
    If CompareText(DutyShape,      'none')=0 Then DutyShape      := '';

    If YearlyShapeObj=Nil Then
      If Length(YearlyShape)>0 Then DoSimpleMsg('WARNING! Yearly load shape: "'+ YearlyShape +'" Not Found.', 563);
    If DailyDispShapeObj=Nil Then
      If Length(DailyDispShape)>0 Then DoSimpleMsg('WARNING! Daily load shape: "'+ DailyDispShape +'" Not Found.', 564);
    If DutyShapeObj=Nil Then
      If Length(DutyShape)>0 Then DoSimpleMsg('WARNING! Duty load shape: "'+ DutyShape +'" Not Found.', 565);

    SpectrumObj := SpectrumClass[ActorID].Find(Spectrum);
    If SpectrumObj=Nil Then DoSimpleMsg('ERROR! Spectrum "'+Spectrum+'" Not Found.', 566);


    YQFixed := -varBase / Sqr(VBase);   //10-17-02  Fixed negative sign
    WindGenvars.Vtarget := Vpu * 1000.0 * WindGenvars.kVWindGenBase;

    If Fnphases>1 then WindGenvars.VTarget := WindGenvars.VTarget / SQRT3;

    // Initialize to Zero - defaults to PQ WindGen
    // Solution object will reset after circuit modifications
    DQDV      := DQDVSaved;         // for Model = 3
    DeltaQMax := (varMax - varMin) * 0.10;  // Limit to 10% of range

    Reallocmem(InjCurrent, SizeOf(InjCurrent^[1])*Yorder);

    {Update any user-written models}
    If Usermodel.Exists  Then UserModel.FUpdateModel;
    If Shaftmodel.Exists Then Shaftmodel.FUpdateModel;

    if (WindModelDyn <> nil) then
        WindModelDyn.ReCalcElementData;

End;

//----------------------------------------------------------------------------
Procedure TWindGenObj.CalcYPrimMatrix(Ymatrix:TcMatrix;ActorID : Integer);

Var
   Y , Yij        : Complex;
   i,j            : Integer;
   FreqMultiplier : Double;
   WTGZLV         : Double;

Begin

   FYprimFreq := ActiveCircuit[ActorID].Solution.Frequency  ;
   FreqMultiplier := FYprimFreq / BaseFrequency;

   With  ActiveCircuit[ActorID].solution  Do
   IF IsDynamicModel or IsHarmonicModel Then
   Begin
     IF GenON Then
     Begin
      With  WindModelDyn Do
      Begin
        WTGZLV :=  sqr(PresentkV) * 1e3 / WindGenVars.kVArating;
        Y  := Cmplx(EPSILON,  -N_WTG / (Xthev*WTGZLV)) //Yeq  // L-N value computed in initial condition routines
      End;
     End
     ELSE Y := Cmplx(EPSILON, 0.0);

     IF Connection=1 Then Y := CDivReal(Y, 3.0); // Convert to delta impedance
     Y.im := Y.im / FreqMultiplier;
     Yij := Cnegate(Y);
     FOR i := 1 to Fnphases Do
     Begin
       Case Connection of
       0: Begin
               Ymatrix.SetElement(i, i, Y);
               Ymatrix.AddElement(Fnconds, Fnconds, Y);
               Ymatrix.SetElemsym(i, Fnconds, Yij);
          End;
       1: Begin   {Delta connection}
               Ymatrix.SetElement(i, i, Y);
               Ymatrix.AddElement(i, i, Y);  // put it in again
               For j := 1 to i-1 Do Ymatrix.SetElemsym(i, j, Yij);
          End;
       End;
     End;

    (**** Removed Neutral / Neutral may float

     IF Connection = 0 Then   With Ymatrix Do  // Take care of neutral issues
       Begin
         AddElement(Fnconds, Fnconds, YNeut);  // Add in user specified Neutral Z, if any
         // Bump up neutral-ground in case neutral ends up floating
         SetElement(Fnconds, Fnconds, CmulReal(GetElement(Fnconds, Fnconds), 1.000001));
       End;

    *)
   End

   ELSE
   Begin  //  Regular power flow WindGen model

     {Yeq is always expected as the equivalent line-neutral admittance}

     Y := cnegate(Yeq);  // negate for generation    Yeq is L-N quantity
     // ****** Need to modify the base admittance for real harmonics calcs
     Y.im           := Y.im / FreqMultiplier;

       CASE Connection OF

         0: With YMatrix Do
            Begin // WYE
                   Yij := Cnegate(Y);
                   FOR i := 1 to Fnphases Do Begin
                     SetElement(i, i, Y);
                     AddElement(Fnconds, Fnconds, Y);
                     SetElemsym(i, Fnconds, Yij);
                   End;
            End;
         1: With YMatrix Do
            Begin  // Delta  or L-L
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
Procedure TWindGenObj.CalcYPrim(ActorID : Integer);

Var
        i:integer;
        
Begin

     // Build only shunt Yprim
     // Build a dummy Yprim Series so that CalcV does not fail
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

     If ActiveCircuit[ActorID].Solution.LoadModel=POWERFLOW
     Then Begin
     
        // 12-7-99 we'll start with Yeq in system matrix
         SetNominalGeneration(ActorID);
         CalcYPrimMatrix(YPrim_Shunt,ActorID);

     End
     ELSE Begin

         // ADMITTANCE model wanted

         SetNominalGeneration(ActorID);
         CalcYPrimMatrix(YPrim_Shunt,ActorID);

     End;

     // Set YPrim_Series based on diagonals of YPrim_shunt  so that CalcVoltages doesn't fail
     For i := 1 to Yorder Do Yprim_Series.SetElement(i, i, CmulReal(Yprim_Shunt.Getelement(i, i), 1.0e-10));
     
     YPrim.CopyFrom(YPrim_Shunt);

     // Account for Open Conductors
     Inherited CalcYPrim(ActorID);

End;

// - - - - - - - - - - - - - - - - - - - - - - - -- - - - - - - - - - - - - - - - - -
Procedure TWindGenObj.StickCurrInTerminalArray(TermArray:pComplexArray; Const Curr:Complex; i:Integer);
 {Add the current into the proper location according to connection}

 {Reverse of similar routine in load  (Cnegates are switched)}

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
Procedure TWindGenObj.WriteTraceRecord(const s:string;ActorID : Integer);

Var i:Integer;

Begin

      Try
      If (Not InshowResults) Then

      Begin
           Append(TraceFile);
           Write(TraceFile,Format('%-.g, %d, %-.g, ',
                    [ActiveCircuit[ActorID].Solution.DynaVars.t + ActiveCircuit[ActorID].Solution.Dynavars.IntHour * 3600.0,
                    ActiveCircuit[ActorID].Solution.Iteration,
                    ActiveCircuit[ActorID].LoadMultiplier]),
                    GetSolutionModeID,', ',
                    GetLoadModel,', ',
                    GenModel:0,', ',
                    DQDV:8:0,', ',
                   (V_Avg*0.001732/WindGenvars.kVWindGenbase):8:3,', ',
                   (WindGenvars.Vtarget- V_Avg):9:1,', ',
                   (WindGenvars.Qnominalperphase*3.0/1.0e6):8:2,', ',
                   (WindGenvars.Pnominalperphase*3.0/1.0e6):8:2,', ',
                   s,', ');
           For i := 1 to nphases Do Write(TraceFile,(Cabs(InjCurrent^[i])):8:1 ,', ');
           For i := 1 to nphases Do Write(TraceFile,(Cabs(ITerminal^[i])):8:1 ,', ');
           For i := 1 to nphases Do Write(TraceFile,(Cabs(Vterminal^[i])):8:1 ,', ');
           Write(TraceFile,WindGenvars.VThevMag:8:1 ,', ', WindGenvars.Theta*180.0/PI);
           Writeln(TRacefile);
           CloseFile(TraceFile);
      End;
      Except
            On E:Exception Do Begin End;

      End;
End;


// - - - - - - - - - - - - - - - - - - - - - - - -- - - - - - - - - - - - - - - - - -
Procedure TWindGenObj.DoConstantPQGen(ActorID : Integer);

{Compute total terminal current for Constant PQ}

VAR
   i    : Integer;
   Curr,
   V    : Complex;
   Vmag : Double;

Begin
     //Treat this just like the Load model

    CalcYPrimContribution(InjCurrent, ActorID);  // Init InjCurrent Array
    FOR i := 1 to FnConds Do InjCurrent^[i] := CZero;
    ZeroITerminal;

    CalcVTerminalPhase(ActorID); // get actual voltage across each phase of the load

    FOR i := 1 to Fnphases Do Begin
        V    := Vterminal^[i];
        VMag := Cabs(V);

        CASE Connection of
          0: Begin  //Wye

              IF   VMag <= VBase95
              THEN Curr := Cmul(Yeq95, V)  // Below 95% use an impedance model
              ELSE If VMag > VBase105
              THEN Curr := Cmul(Yeq105, V)  // above 105% use an impedance model
              ELSE
                With WindGenvars Do Curr := Conjg(Cdiv(Cmplx(Pnominalperphase, Qnominalperphase), V));  // Between 95% -105%, constant PQ
             End;
          1: Begin  //Delta
                case Fnphases of
                     2, 3: VMag := VMag/SQRT3;  // L-N magnitude
                else
                    //leave Vmag as is
                end;

                IF   VMag <= VBase95
                THEN Curr := Cmul(CdivReal(Yeq95, 3.0), V)  // Below 95% use an impedance model
                ELSE If VMag > VBase105
                THEN Curr := Cmul(CdivReal(Yeq105, 3.0), V)  // above 105% use an impedance model
                ELSE With WindGenvars Do Curr := Conjg(Cdiv(Cmplx(Pnominalperphase, Qnominalperphase), V));  // Between 95% -105%, constant PQ
             End;
         END;

        StickCurrInTerminalArray(ITerminal, Cnegate(Curr), i);  // Put into Terminal array taking into account connection
        set_ITerminalUpdated(TRUE, ActorID);
        StickCurrInTerminalArray(InjCurrent, Curr, i);  // Put into Terminal array taking into account connection
    End;


End;

// - - - - - - - - - - - - - - - - - - - - - - - -- - - - - - - - - - - - - - - - - -
Procedure TWindGenObj.DoConstantZGen(ActorID : Integer);
VAR
   i    :Integer;
   Curr,
   Yeq2 :Complex;

Begin

// Assume Yeq is kept up to date
    CalcYPrimContribution(InjCurrent,ActorID);  // Init InjCurrent Array
    CalcVTerminalPhase(ActorID); // get actual voltage across each phase of the load
    ZeroITerminal;
    If Connection=0 Then Yeq2 := Yeq Else Yeq2 := CdivReal(Yeq, 3.0);

     FOR i := 1 to Fnphases Do Begin
          Curr := Cmul(Yeq2, Vterminal^[i]);   // Yeq is always line to neutral

          StickCurrInTerminalArray(ITerminal, Cnegate(Curr), i);  // Put into Terminal array taking into account connection
          set_ITerminalUpdated(TRUE, ActorID);
          StickCurrInTerminalArray(InjCurrent, Curr, i);  // Put into Terminal array taking into account connection
     End;

End;

// - - - - - - - - - - - - - - - - - - - - - - - -- - - - - - - - - - - - - - - - - -
Procedure TWindGenObj.DoPVTypeGen(ActorID : Integer);
{Compute total terminal current for Constant P,|V|}

// Constant P, constant |V|

Var

   i  : Integer;
   DQ : Double;
   Curr:Complex;

Begin

    CalcYPrimContribution(InjCurrent, ActorID);  // Init InjCurrent Array
    CalcVTerminalPhase(ActorID); // get actual voltage across each phase of the WindGen
    ZeroITerminal;

    // Guess at a new var output value
   V_Avg := 0.0;
   For i := 1 to Fnphases
   Do  V_Avg := V_Avg + Cabs(Vterminal^[i]);

   If Connection =1 then V_Avg := V_Avg/(SQRT3*Fnphases) Else  V_Avg := V_Avg / Fnphases;

   // 12-9-99 added empirical 0.7 factor to improve iteration
   // 12-17-99 changed to 0.1 because first guess was consistently too high
   DQ :=  PVFactor * DQDV * (WindGenvars.Vtarget - V_Avg);   // Vtarget is L-N
   If (Abs(DQ) > DeltaQMax)
   Then IF (DQ < 0.0) Then DQ := -DeltaQMax Else DQ := DeltaQMax;
   With WindGenvars Do Qnominalperphase := Qnominalperphase + DQ;

   { Test Limits}
   With WindGenvars Do  Begin
       If      (Qnominalperphase > varMax) Then Qnominalperphase := varMax
       Else if (Qnominalperphase < varMin) Then Qnominalperphase := varMin;

       // Compute injection currents using W and var values
       // Do not use comstant Z models outside normal range
       // Presumably the var source will take care of the voltage problems
        FOR i := 1 to Fnphases Do Begin
            Curr :=  Conjg( Cdiv( Cmplx(Pnominalperphase, Qnominalperphase), Vterminal^[i])) ;

            StickCurrInTerminalArray(ITerminal, Cnegate(Curr), i);  // Put into Terminal array taking into account connection
            set_ITerminalUpdated(TRUE, ActorID);
            StickCurrInTerminalArray(InjCurrent,Curr, i);  // Put into Terminal array taking into account connection
        End;
   end; {With}
End;


// - - - - - - - - - - - - - - - - - - - - - - - -- - - - - - - - - - - - - - - - - -
PROCEDURE TWindGenObj.DoFixedQGen(ActorID : Integer);

{Compute total terminal current for Fixed Q}
// Constant P, Fixed Q  Q is always kvarBase
Var
   i        :Integer;
   Curr,
   V        :Complex;
   Vmag     :Double;

Begin
    CalcYPrimContribution(InjCurrent, ActorID);  // Init InjCurrent Array
    CalcVTerminalPhase(ActorID); // get actual voltage across each phase of the load
    ZeroITerminal;

    FOR i := 1 to Fnphases DO Begin
        V    := Vterminal^[i];
        VMag := Cabs(V);

        CASE Connection of
            0:Begin
                IF   VMag <= VBase95
                THEN Curr := Cmul(Cmplx(Yeq95.re, YQfixed), V)  // Below 95% use an impedance model
                ELSE IF VMag > VBase105
                THEN Curr := Cmul(Cmplx(Yeq105.re, YQfixed), V)  // above 105% use an impedance model
                ELSE Curr := Conjg(Cdiv(Cmplx(WindGenvars.Pnominalperphase, varBase), V));
              End;
            1:Begin
                case Fnphases of
                     2, 3: VMag := VMag/SQRT3;  // L-N magnitude
                else
                    {leave Vmag as is}
                end;
                IF   VMag <= VBase95
                THEN Curr := Cmul(Cmplx(Yeq95.re/3.0, YQfixed/3.0), V)  // Below 95% use an impedance model
                ELSE IF VMag > VBase105
                THEN Curr := Cmul(Cmplx(Yeq105.re/3.0, YQfixed/3.0), V)  // above 105% use an impedance model
                ELSE Curr := Conjg(Cdiv(Cmplx(WindGenvars.Pnominalperphase, varBase), V));
               End;
        END;

        StickCurrInTerminalArray(ITerminal, Cnegate(Curr), i);  // Put into Terminal array taking into account connection
        set_ITerminalUpdated(TRUE, ActorID);
        StickCurrInTerminalArray(InjCurrent,Curr, i);  // Put into Terminal array taking into account connection
    End;
End;

// - - - - - - - - - - - - - - - - - - - - - - - -- - - - - - - - - - - - - - - - - -
PROCEDURE TWindGenObj.DoFixedQZGen(ActorID : Integer);

{Compute total terminal current for }
// Constant P, Fixed Q  Q is always a fixed Z derived from kvarBase
Var
   i     :Integer;
   Curr,
   V     :Complex;
   Vmag  :Double;

Begin

    CalcYPrimContribution(InjCurrent, ActorID);  // Init InjCurrent Array
    CalcVTerminalPhase(ActorID); // get actual voltage across each phase of the load
    ZeroITerminal;

    FOR i := 1 to Fnphases DO
     Begin
        V    := Vterminal^[i];
        Vmag := Cabs(V);

        CASE Connection of
            0:Begin
                  IF   Vmag <= VBase95
                  THEN Curr := Cmul(Cmplx(Yeq95.re, YQfixed), V)  // Below 95% use an impedance model
                  ELSE IF VMag > VBase105
                  THEN Curr := Cmul(Cmplx(Yeq105.re, YQfixed), V)
                  ELSE Begin
                        Curr := Conjg(Cdiv(Cmplx(WindGenvars.Pnominalperphase, 0.0), V)); // P component of current
                        Caccum(Curr, Cmul(Cmplx(0.0, YQFixed ), V));  // add in Q component of current
                  End;
               End;
            1:Begin
                  case Fnphases of
                       2, 3: VMag := VMag/SQRT3;  // L-N magnitude
                  else
                      {leave Vmag as is}
                  end;
                  IF   Vmag <= VBase95
                  THEN Curr := Cmul(Cmplx(Yeq95.re/3.0, YQfixed/3.0), V)  // Below 95% use an impedance model
                  ELSE IF VMag > VBase105
                  THEN Curr := Cmul(Cmplx(Yeq105.re/3.0, YQfixed/3.0), V)
                  ELSE Begin
                        Curr := Conjg(Cdiv(Cmplx(WindGenvars.Pnominalperphase, 0.0), V)); // P component of current
                        Caccum(Curr, Cmul(Cmplx(0.0, YQFixed /3.0), V));  // add in Q component of current
                  End;
               End;
        END;

        StickCurrInTerminalArray(ITerminal, Cnegate(Curr), i);  // Put into Terminal array taking into account connection
        set_ITerminalUpdated(TRUE, ActorID);
        StickCurrInTerminalArray(InjCurrent,Curr, i);  // Put into Terminal array taking into account connection
     End; {FOR}
End;
// - - - - - - - - - - - - - - - - - - - - - - - -- - - - - - - - - - - - - - - - - -
PROCEDURE TWindGenObj.DoUserModel(ActorID : Integer);
{Compute total terminal Current from User-written model}
Var
   i:Integer;

Begin

   CalcYPrimContribution(InjCurrent, ActorID);  // Init InjCurrent Array

   If UserModel.Exists Then    // Check automatically selects the usermodel if true
     Begin
         //AppendToEventLog('Wnominal=', Format('%-.5g',[Pnominalperphase]));
         UserModel.FCalc (Vterminal, Iterminal);
         set_ITerminalUpdated(TRUE, ActorID);
         With ActiveCircuit[ActorID].Solution Do  Begin          // Negate currents from user model for power flow WindGen model
               FOR i := 1 to FnConds Do Caccum(InjCurrent^[i], Cnegate(Iterminal^[i]));
         End;
     End
   Else
     Begin
        DoSimpleMsg('WindGen.' + name + ' model designated to use user-written model, but user-written model is not defined.', 567);
     End;

End;

// - - - - - - - - - - - - - - - - - - - - - - - -- - - - - - - - - - - - - - - - - -
procedure TWindGenObj.DoCurrentLimitedPQ(ActorID : Integer);
{Compute total terminal current for Constant PQ, but limit to max current below
 Vminpu}


VAR
   i : Integer;
   PhaseCurr, DeltaCurr, VLN, VLL : Complex;
   VMagLN, VMagLL : Double;
   V012 : Array[0..2] of Complex;  // Sequence voltages

Begin
     //Treat this just like the Load model

    CalcYPrimContribution(InjCurrent, ActorID);  // Init InjCurrent Array
    CalcVTerminalPhase(ActorID); // get actual voltage across each phase of the load

    If ForceBalanced and (Fnphases=3) Then Begin    // convert to pos-seq only
        Phase2SymComp(Vterminal, @V012);
        V012[0] := CZERO; // Force zero-sequence voltage to zero
        V012[2] := CZERO; // Force negative-sequence voltage to zero
        SymComp2Phase(Vterminal, @V012);  // Reconstitute Vterminal as balanced
    End;

    ZeroITerminal;

    FOR i := 1 to Fnphases Do
    Begin

      CASE Connection of
        0: Begin
              VLN    := Vterminal^[i];   // VTerminal is LN for this connection
              VMagLN := Cabs(VLN);
              With WindGenvars Do
                 PhaseCurr := Conjg(Cdiv(Cmplx(Pnominalperphase, Qnominalperphase), VLN));
              If Cabs(PhaseCurr) > Model7MaxPhaseCurr Then
                 PhaseCurr := Conjg( Cdiv( PhaseCurrentLimit, CDivReal(VLN, VMagLN)) );

              StickCurrInTerminalArray(ITerminal, Cnegate(PhaseCurr), i);  // Put into Terminal array taking into account connection
              set_ITerminalUpdated(TRUE, ActorID);
              StickCurrInTerminalArray(InjCurrent,PhaseCurr, i);  // Put into Terminal array taking into account connection
           End;
        1: Begin
              VLL    := Vterminal^[i];     // VTerminal is LL for this connection
              VMagLL := Cabs(VLL);
              case Fnphases of
                 2, 3:   // 2 or 3 phase WindGen model 7
                     Begin
                       With WindGenvars Do
                       DeltaCurr := Conjg(Cdiv(Cmplx(Pnominalperphase, Qnominalperphase), VLL));
                       If Cabs(DeltaCurr)*SQRT3 > Model7MaxPhaseCurr Then
                       DeltaCurr := Conjg( Cdiv( PhaseCurrentLimit, CDivReal(VLL, VMagLL/SQRT3)) );
                     End
              else  // 1-phase WindGen model 7
                   With WindGenvars Do
                     DeltaCurr := Conjg(Cdiv(Cmplx(Pnominalperphase, Qnominalperphase), VLL));
                   If Cabs(DeltaCurr) > Model7MaxPhaseCurr Then
                     DeltaCurr := Conjg( Cdiv( PhaseCurrentLimit, CDivReal(VLL, VMagLL)) );
              end;

              StickCurrInTerminalArray(ITerminal, Cnegate(DeltaCurr), i);  // Put into Terminal array taking into account connection
              set_ITerminalUpdated(TRUE, ActorID);
              StickCurrInTerminalArray(InjCurrent,DeltaCurr, i);  // Put into Terminal array taking into account connection
           End;
      END;

    End;

end;


// - - - - - - - - - - - - - - - - - - - - - - - -- - - - - - - - - - - - - - - - - -
PROCEDURE TWindGenObj.DoDynamicMode(ActorID : Integer);

{Compute Total Current and add into InjTemp}

Var
   i     : Integer;
   V012,
   I012  : Array[0..2] of Complex;

Begin

   //CalcYPrimContribution(InjCurrent, ActorID);  // Init InjCurrent Array  and computes VTerminal L-N
   ComputeVTerminal(ActorID);
   FOR i := 1 to FnConds Do InjCurrent^[i] := CZero;
   {Inj = -Itotal (in) - Yprim*Vtemp}

   CASE GenModel of

       6:If UserModel.Exists Then       // auto selects model
              Begin   {We have total currents in Iterminal}
                UserModel.FCalc(Vterminal, Iterminal);  // returns terminal currents in Iterminal
              End
         ELSE Begin
                  DoSimpleMsg(Format('Dynamics model missing for WindGen.%s ',[Name]), 5671);
                  SolutionAbort := TRUE;
              End;
   ELSE
      WindModelDyn.CalcDynamic(Vterminal, Iterminal);
   END;

   set_ITerminalUpdated(TRUE, ActorID);

    {Add it into inj current array}
   FOR i := 1 to FnConds Do Caccum(InjCurrent^[i], Cnegate(Iterminal^[i]));

   {Take Care of any shaft model calcs}
    If (GenModel=6) and ShaftModel.Exists Then      // auto selects model
    Begin           // Compute Mech Power to shaft
         ShaftModel.FCalc(Vterminal, Iterminal);     // Returns pshaft at least
    End;
End;


// - - - - - - - - - - - - - - - - - - - - - - - -- - - - - - - - - - - - - - - - - -
PROCEDURE TWindGenObj.DoHarmonicMode(ActorID : Integer);

{Compute Injection Current Only when in harmonics mode}

{Assumes spectrum is a voltage source behind subtransient reactance and YPrim has been built}
{Vd is the fundamental frequency voltage behind Xd" for phase 1}

Var
   i     :Integer;
   E     :Complex;
   GenHarmonic :double;

Begin

   ComputeVterminal(ActorID);

   WITH ActiveCircuit[ActorID].Solution Do
     Begin
        GenHarmonic := Frequency/GenFundamental;
        E := CmulReal(SpectrumObj.GetMult(GenHarmonic), WindGenvars.VThevHarm); // Get base harmonic magnitude
        RotatePhasorRad(E, GenHarmonic, WindGenvars.ThetaHarm);  // Time shift by fundamental frequency phase shift
        FOR i := 1 to Fnphases DO Begin
           cBuffer[i] := E;
           If i < Fnphases Then RotatePhasorDeg(E, GenHarmonic, -120.0);  // Assume 3-phase WindGen
        End;
     END;

   {Handle Wye Connection}
   IF Connection=0 THEN cbuffer[Fnconds] := Vterminal^[Fnconds];  // assume no neutral injection voltage

   {Inj currents = Yprim (E) }
   YPrim.MVMult(InjCurrent,@cBuffer);

End;


// - - - - - - - - - - - - - - - - - - - - - - - -- - - - - - - - - - - - - - - - - -
Procedure TWindGenObj.CalcVTerminalPhase(ActorID : Integer);

VAR i,j:Integer;

Begin

{ Establish phase voltages and stick in Vterminal}
   Case Connection OF

     0:Begin
         With ActiveCircuit[ActorID].Solution Do
           FOR i := 1 to Fnphases Do Vterminal^[i] := VDiff(NodeRef^[i], NodeRef^[Fnconds], ActorID);
       End;

     1:Begin
         With ActiveCircuit[ActorID].Solution Do
          FOR i := 1 to Fnphases Do  Begin
             j := i + 1;
             If j > Fnconds Then j := 1;
             Vterminal^[i] := VDiff( NodeRef^[i] , NodeRef^[j], ActorID);
          End;
       End;

   End;

   WindGenSolutionCount := ActiveCircuit[ActorID].Solution.SolutionCount;

End;

// - - - - - - - - - - - - - - - - - - - - - - - -- - - - - - - - - - - - - - - - - -
Procedure TWindGenObj.CalcVTerminal(ActorID : Integer);

{Put terminal voltages in an array}


Begin

   ComputeVTerminal(ActorID);

   WindGenSolutionCount := ActiveCircuit[ActorID].Solution.SolutionCount;

End;

// - - - - - - - - - - - - - - - - - - - - - - - -- - - - - - - - - - - - - - - - - -
Procedure TWindGenObj.CalcGenModelContribution(ActorID : Integer);
// Calculates WindGen current and adds it properly into the injcurrent array
// routines may also compute ITerminal  (ITerminalUpdated flag)

Begin
  set_ITerminalUpdated(FALSE, ActorID);
  WITH  ActiveCircuit[ActorID], ActiveCircuit[ActorID].Solution DO Begin
      IF      IsDynamicModel THEN  DoDynamicMode(ActorID)
      ELSE IF IsHarmonicModel and (Frequency <> Fundamental) THEN  DoHarmonicMode(ActorID)
      ELSE  Begin
           //  compute currents and put into InjTemp array;
           CASE GenModel OF
              1: DoConstantPQGen(ActorID);
              2: DoConstantZGen(ActorID);
              3: DoPVTypeGen(ActorID);  // Constant P, |V|
              4: DoFixedQGen(ActorID);
              5: DoFixedQZGen(ActorID);
              6: DoUserModel(ActorID);
              7: DoCurrentLimitedPQ(ActorID);
           ELSE
              DoConstantPQGen(ActorID);  // for now, until we implement the other models.
           End;
        End; {ELSE}
   END; {WITH}

   {When this is done, ITerminal is up to date}

End;

// - - - - - - - - - - - - - - - - - - - - - - - -- - - - - - - - - - - - - - - - - -
Procedure TWindGenObj.CalcInjCurrentArray(ActorID : Integer);
// Difference between currents in YPrim and total current
Begin
// Now Get Injection Currents
  If GenSwitchOpen Then
    ZeroInjCurrent
  Else
    CalcGenModelContribution(ActorID);
End;

// - - - - - - - - - - - - - - - - - - - - - - - -- - - - - - - - - - - - - - - - - -
Procedure TWindGenObj.GetTerminalCurrents(Curr:pComplexArray; ActorID : Integer);

// Compute total Currents
Var
  i   : integer;

Begin
   WITH ActiveCircuit[ActorID].Solution  DO
     Begin
        If IterminalSolutionCount[ActorID] <> ActiveCircuit[ActorID].Solution.SolutionCount Then Begin     // recalc the contribution
          IF Not GenSwitchOpen Then CalcGenModelContribution(ActorID);  // Adds totals in Iterminal as a side effect
        End
        Else
        Begin
          inherited GetTerminalCurrents(Curr,ActorID);
        End;

     End;

   If (DebugTrace) Then WriteTraceRecord('TotalCurrent',ActorID);

End;

// - - - - - - - - - - - - - - - - - - - - - - - -- - - - - - - - - - - - - - - - - -
Function TWindGenObj.InjCurrents(ActorID : Integer):Integer;


Begin

   With ActiveCircuit[ActorID].Solution Do
    Begin
       If LoadsNeedUpdating Then SetNominalGeneration(ActorID); // Set the nominal kW, etc for the type of solution being done

       CalcInjCurrentArray(ActorID);          // Difference between currents in YPrim and total terminal current

       If (DebugTrace) Then WriteTraceRecord('Injection',ActorID);

       // Add into System Injection Current Array

       Result := Inherited InjCurrents(ActorID);

    End;

End;

// - - - - - - - - - - - - - - - - - - - - - - - -- - - - - - - - - - - - - - - - - -
Procedure TWindGenObj.GetInjCurrents(Curr:pComplexArray; ActorID : Integer);

// Gives the currents for the last solution performed

// Do not call SetNominalLoad, as that may change the load values

VAR
   i:Integer;

Begin

   CalcInjCurrentArray(ActorID);  // Difference between currents in YPrim and total current

   TRY
   // Copy into buffer array
     FOR i := 1 TO Yorder Do Curr^[i] := InjCurrent^[i];

   EXCEPT
     ON E: Exception Do
        DoErrorMsg('WindGen Object: "' + Name + '" in GetInjCurrents function.',
                    E.Message,
                   'Current buffer not big enough.', 568);
   End;

End;
//= = =  = = = = = = = = = = = = = = = = = = = = = = = = = = = =


//- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Procedure TWindGenObj.ResetRegisters;

VAR
   i : Integer;

Begin
       For i := 1 to NumWGenregisters Do Registers[i]   := 0.0;
       For i := 1 to NumWGenregisters Do Derivatives[i] := 0.0;
       FirstSampleAfterReset := True;  // initialize for trapezoidal integration
End;

//- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Procedure TWindGenObj.Integrate(Reg:Integer; const Deriv:Double; Const Interval:Double; ActorID: integer);

Begin
     IF ActiveCircuit[ActorID].TrapezoidalIntegration
     THEN Begin
        {Trapezoidal Rule Integration}
        If Not FirstSampleAfterReset Then Registers[Reg] := Registers[Reg] + 0.5 * Interval * (Deriv + Derivatives[Reg]);
     End
     ELSE   {Plain Euler integration}
         Registers[Reg] := Registers[Reg] + Interval * Deriv;

     Derivatives[Reg] := Deriv;
End;

//- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Procedure TWindGenObj.TakeSample(ActorID: integer);
// Update Energy from metered zone

VAR
   S         :Complex;
   Smag      :double;
   HourValue :Double;

Begin

// Compute energy in WindGen branch
   IF  Enabled  THEN Begin

      IF GenON Then Begin
        S := cmplx(Get_PresentkW, Get_Presentkvar);
        Smag := Cabs(S);
        HourValue := 1.0;
      End
      Else Begin
         S := CZERO;
         Smag := 0.0;
         HourValue :=0.0;
      End;

      IF GenON or ActiveCircuit[ActorID].TrapezoidalIntegration THEN
      {Make sure we always integrate for Trapezoidal case
       Don't need to for Gen Off and normal integration}
      WITH ActiveCircuit[ActorID].Solution Do Begin
           IF ActiveCircuit[ActorID].PositiveSequence THEN Begin
              S    := CmulReal(S, 3.0);
              Smag := 3.0*Smag;
           End;
           Integrate            (Reg_kWh,   S.re, IntervalHrs, ActorID);   // Accumulate the power
           Integrate            (Reg_kvarh, S.im, IntervalHrs, ActorID);
           SetDragHandRegister  (Reg_MaxkW, abs(S.re));
           SetDragHandRegister  (Reg_MaxkVA, Smag);
           Integrate            (Reg_Hours, HourValue, IntervalHrs, ActorID);  // Accumulate Hours in operation
           Integrate            (Reg_Price, S.re*ActiveCircuit[ActorID].PriceSignal * 0.001 , IntervalHrs, ActorID);  // Accumulate Hours in operation
           FirstSampleAfterReset := False;

      End;
   End;
End;

// - - - - - - - - - - - - - - - - - - - - - - - -- - - - - - - - - - - - - - - - - -
Function TWindGenObj.Get_PresentkW:Double;
Begin
     Result := WindGenvars.Pnominalperphase * 0.001 * Fnphases;
End;

// - - - - - - - - - - - - - - - - - - - - - - - -- - - - - - - - - - - - - - - - - -
function TWindGenObj.Get_PresentkV: Double;
begin
     Result := WindGenvars.kVWindGenBase;
end;

Function TWindGenObj.Get_Presentkvar:Double;
Begin
     Result := WindGenvars.Qnominalperphase * 0.001 * Fnphases;
End;

// - - - - - - - - - - - - - - - - - - - - - - - -- - - - - - - - - - - - - - - - - -
Procedure TWindGenObj.InitDQDVCalc;

Begin
    DQDV := 0.0;
    WindGenvars.Qnominalperphase := 0.5 * (varmax + varmin);   // avg of the limits
End;

// - - - - - - - - - - - - - - - - - - - - - - - -- - - - - - - - - - - - - - - - - -
Procedure TWindGenObj.BumpUpQ;
{Bump up vars by 10% of range for next calc}
Begin
    with WindGenvars Do Qnominalperphase := Qnominalperphase + 0.1 * (varmax - varmin);
End;

// - - - - - - - - - - - - - - - - - - - - - - - -- - - - - - - - - - - - - - - - - -
Procedure TWindGenObj.RememberQV(ActorID : Integer);

Var
   i:integer;

Begin
     var_Remembered := WindGenvars.Qnominalperphase;
     CalcVTerminal(ActorID);
     V_Avg := 0.0;
     For i := 1 to Fnphases Do V_Avg := V_Avg + Cabs(Vterminal^[i]);
     V_Avg := V_Avg / Fnphases;
     V_Remembered := V_Avg;
End;

// - - - - - - - - - - - - - - - - - - - - - - - -- - - - - - - - - - - - - - - - - -
Procedure TWindGenObj.CalcDQDV(ActorID : Integer);
Var
   Vdiff :Double;
   i     :Integer;
Begin

     CalcVTerminal(ActorID);
     V_Avg := 0.0;
     For i := 1 to Fnphases Do V_Avg := V_Avg + Cabs(Vterminal^[i]);
     V_Avg := V_Avg / Fnphases;

     Vdiff := V_Avg - V_Remembered;
     If (Vdiff <> 0.0) Then DQDV := (WindGenvars.Qnominalperphase - var_Remembered) / Vdiff
                       Else DQDV := 0.0;  // Something strange has occured
                       // this will force a de facto P,Q model
     DQDVSaved := DQDV;  //Save for next time  Allows WindGen to be enabled/disabled during simulation
End;

// - - - - - - - - - - - - - - - - - - - - - - - -- - - - - - - - - - - - - - - - - -
Procedure TWindGenObj.ResetStartPoint;

Begin
     WindGenvars.Qnominalperphase := 1000.0* kvarBase / Fnphases;
End;


// - - - - - - - - - - - - - - - - - - - - - - - -- - - - - - - - - - - - - - - - - -
Procedure TWindGenObj.DumpProperties(Var F:TextFile; Complete:Boolean);

Var
   i, idx :Integer;

Begin
    Inherited DumpProperties(F, Complete);

    Writeln(F,'!DQDV=', DQDV:10:2);


    With ParentClass Do
     For i := 1 to NumProperties Do
     Begin
        idx := PropertyIdxMap^[i] ;
        Case idx of
           34, 36: Writeln(F,'~ ',PropertyName^[i],'=(',PropertyValue[idx],')');
           44 : Writeln(F,'~ ',PropertyName^[i],'=False')  // This one has no variable associated, not needed

        Else
          Writeln(F,'~ ',PropertyName^[i],'=',PropertyValue[idx]);
        End;
     End;

    Writeln(F);

End;

      
Procedure TWindGenObj.InitHarmonics(ActorID : Integer);
Var
  E, Va:complex;
begin

     YprimInvalid[ActorID]   := TRUE;  // Force rebuild of YPrims
     GenFundamental := ActiveCircuit[ActorID].Solution.Frequency ;  // Whatever the frequency is when we enter here.

     With WindGenvars Do Begin

         Yeq := Cinv(Cmplx(0.0, Xdpp));      // used for current calcs  Always L-N

         {Compute reference Thevinen voltage from phase 1 current}

         IF GenON Then
           Begin

             ComputeIterminal(ActorID);  // Get present value of current

             With ActiveCircuit[ActorID].solution Do
             Case Connection of
               0: Begin {wye - neutral is explicit}
                    if not ADiakoptics or (ActorID = 1) then Va := Csub(NodeV^[NodeRef^[1]], NodeV^[NodeRef^[Fnconds]])
                    else Va := Csub(VoltInActor1(NodeRef^[1]), VoltInActor1(NodeRef^[Fnconds]));
                  End;
               1: Begin  {delta -- assume neutral is at zero}
                    if not ADiakoptics or (ActorID = 1) then Va := NodeV^[NodeRef^[1]]
                    else Va := VoltInActor1(NodeRef^[1]);
                  End;
             End;

                     E := Csub(Va, Cmul(Iterminal^[1], cmplx(0.0, Xdpp)));
             Vthevharm := Cabs(E);   // establish base mag and angle
             ThetaHarm := Cang(E);
           End
         ELSE  Begin
             Vthevharm := 0.0;
             ThetaHarm := 0.0;
         End;
     End;

end;

procedure TWindGenObj.InitPropertyValues(ArrayOffset: Integer);

begin

     PropertyValue[1]      := '3';        //'phases';
     PropertyValue[2]      := Getbus(1);  //'bus1';
     PropertyValue[3]      := '12.47';    // kV
     PropertyValue[4]      := '100';      // kW
     PropertyValue[5]      := '.80';      // PF
     PropertyValue[6]      := '1';        // model
     PropertyValue[7]      := '';         // yearly
     PropertyValue[8]      := '';         // daily
     PropertyValue[9]      := '';         // duty
     PropertyValue[10]     := 'wye';      // conn
     PropertyValue[11]     := '60';       // kvar
     PropertyValue[12]     := '100';      // class
     PropertyValue[13]     := 'no';       // debugtrace;
     PropertyValue[14]     := '0.90';     // VMinPu
     PropertyValue[15]     := '1.1';      // VMaxPu
     PropertyValue[16]     := Format('%-g', [WindGenvars.kVARating]);           // kVA
     PropertyValue[17]     := Format('%-g', [WindGenvars.kVARating*0.001]);     // MVA
     PropertyValue[18]     := '';         // UserModel
     PropertyValue[19]     := '';         // UserData
     PropertyValue[20]     := '0.0';      // DutyStart
     PropertyValue[21]     := '';         // DynamicEq
     PropertyValue[22]     := '';         // DynOut
     PropertyValue[23]     := Format('%-g', [WindModelDyn.Rthev]);              // RThev
     PropertyValue[24]     := Format('%-g', [WindModelDyn.Xthev]);              // XThev
     PropertyValue[25]     := Format('%-g', [WindModelDyn.Vss]);                // Vss
     PropertyValue[26]     := Format('%-g', [WindModelDyn.Pss]);                // Pss
     PropertyValue[27]     := Format('%-g', [WindModelDyn.Qss]);                // Wss
     PropertyValue[28]     := Format('%-g', [WindModelDyn.vwind]);              // VWind
     PropertyValue[29]     := Format('%d', [WindModelDyn.QMode]);              // QMode
     PropertyValue[30]     := Format('%d', [WindModelDyn.SimMechFlg]);         // SimMechFlg
     PropertyValue[31]     := Format('%d', [WindModelDyn.APCFLG]);             // APCFlg
     PropertyValue[32]     := Format('%d', [WindModelDyn.QFlg]);               // QFlg
     PropertyValue[33]     := Format('%-g', [WindModelDyn.delt0]);              // delt0
     PropertyValue[34]     := Format('%d', [WindModelDyn.N_WTG]);              // N_WTG
     PropertyValue[35]     := '';                                               // VVCurve
     PropertyValue[36]     := Format('%-g', [WindgenVars.ag]);                  // Ag
     PropertyValue[37]     := Format('%-g', [WindgenVars.Cp]);                  // Cp
     PropertyValue[38]     := Format('%-g', [WindgenVars.Lamda]);               // Lamda
     PropertyValue[39]     := Format('%-g', [WindgenVars.Poles]);               // P
     PropertyValue[40]     := Format('%-g', [WindgenVars.pd]);                  // pd
     PropertyValue[41]     := WindgenVars.PLoss;                                // PLoss
     PropertyValue[42]     := Format('%-g', [WindgenVars.Rad]);                 // Rad
     PropertyValue[43]     := Format('%-g', [WindgenVars.VCutin]);              // VCutIn
     PropertyValue[44]     := Format('%-g', [WindgenVars.VCutout]);             // VCutOut

  inherited  InitPropertyValues(NumPropsThisClass);

end;

PROCEDURE TWindGenObj.InitStateVars(ActorID : Integer);
Var
    {VNeut,}
    NumData,
    i     :Integer;
    V012,
    I012  :Array[0..2] of Complex;
    Vabc  :Array[1..3] of Complex;

begin
  YprimInvalid[ActorID] := TRUE;  // Force rebuild of YPrims
  With WindGenvars Do
  Begin

     CASE Genmodel of
         7: Zthev := Cmplx(Xdp, 0.0); // use Xd' as an equivalent R for the inverter
     ELSE
            Zthev := Cmplx(Xdp/XRdp, Xdp);
     END;

     Yeq := Cinv(Zthev);

     {Compute nominal Positive sequence voltage behind transient reactance}

     IF GenON Then With ActiveCircuit[ActorID].Solution Do
       Begin

         ComputeIterminal(ActorID);

         case Fnphases of

              1: Begin
                  if not ADiakoptics or (ActorID = 1) then
                      Edp      := Csub( CSub(NodeV^[NodeRef^[1]], NodeV^[NodeRef^[2]]) , Cmul(ITerminal^[1], Zthev))
                  else
                      Edp      := Csub( CSub(VoltInActor1(NodeRef^[1]), VoltInActor1(NodeRef^[2])) , Cmul(ITerminal^[1], Zthev));
                      VThevMag := Cabs(Edp);
                 End;

              3: Begin
                 // Calculate Edp based on Pos Seq only
                     Phase2SymComp(ITerminal, @I012);
                     // Voltage behind Xdp  (transient reactance), volts

                     For i := 1 to FNphases Do
                      if not ADiakoptics or (ActorID = 1) then Vabc[i] := NodeV^[NodeRef^[i]]   // Wye Voltage
                      else Vabc[i] := VoltInActor1(NodeRef^[i]);   // Wye Voltage

                     Phase2SymComp(@Vabc, @V012);
                     Edp      := Csub( V012[1] , Cmul(I012[1], Zthev));    // Pos sequence
                     VThevMag := Cabs(Edp);
                 End;
         Else
              DoSimpleMsg(Format('Dynamics mode is implemented only for 1- or 3-phase WindGens. WindGen.'+name+' has %d phases.', [Fnphases]), 5672);
              SolutionAbort := TRUE;
         end;

         if DynamicEqObj = nil then
         Begin
           // Shaft variables
           // Theta is angle on Vthev[1] relative to system reference
           //Theta  := Cang(Vthev^[1]);   // Assume source at 0
           Theta  := Cang(Edp) ;
           If GenModel=7 Then Model7LastAngle := Theta;

           dTheta := 0.0;
           w0     := Twopi * ActiveCircuit[ActorID].Solution.Frequency;
           // recalc Mmass and D in case the frequency has changed
           With WindGenvars Do Begin
             WindGenvars.Mmass := 2.0 * WindGenvars.Hmass * WindGenvars.kVArating * 1000.0/ (w0);   // M = W-sec
             D := Dpu * kVArating *1000.0/(w0);
           End;
           Pshaft := -Power[1,ActorID].re; // Initialize Pshaft to present power Output

           Speed  := 0.0;    // relative to synch speed
           dSpeed := 0.0;

           // Init User-written models
           //Ncond:Integer; V, I:pComplexArray; const X,Pshaft,Theta,Speed,dt,time:Double
           With ActiveCircuit[ActorID].Solution Do
           Begin
             If GenModel=6 then
             Begin
               If UserModel.Exists  Then UserModel.FInit(  Vterminal, Iterminal);
               If ShaftModel.Exists Then ShaftModel.Finit( Vterminal, Iterminal);
             End
             else
             Begin
               WindModelDyn.Init(Vterminal, Iterminal);
             End;
           End;
         End
         else
         Begin
           // Initializes the memory values for the dynamic equation
          for i := 0 to High(DynamicEqVals) do  DynamicEqVals[i][1] :=  0.0;
           // Check for initial conditions using calculated values (P0, Q0)
          NumData   :=  ( length(DynamicEqPair) div 2 )  - 1 ;
          for i := 0 to NumData do
            if DynamicEqObj.IsInitVal(DynamicEqPair[( i * 2 ) + 1]) then
            Begin
              case DynamicEqPair[( i * 2 ) + 1] of
                9 :  Begin
                     DynamicEqVals[DynamicEqPair[ i * 2 ]][0] := Cang(Edp);
                     If GenModel=7 Then Model7LastAngle := DynamicEqVals[DynamicEqPair[ i * 2 ]][0];
                     end;
                else
                    DynamicEqVals[DynamicEqPair[ i * 2 ]][0] := PCEValue[1, DynamicEqPair[( i * 2 ) + 1], ActorID];
              end;
            End;

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

procedure TWindGenObj.IntegrateStates(ActorID : Integer);
Var
  i,
  Numdata   : Integer;
  TracePower:Complex;
begin
   // Compute Derivatives and then integrate

   ComputeIterminal(ActorID);

// Check for user-written exciter model.
    //Function(V, I:pComplexArray; const Pshaft,Theta,Speed,dt,time:Double)

  With ActiveCircuit[ActorID].Solution, WindGenvars Do
  Begin

    if DynamicEqObj = nil then
    Begin
      // Dynamics using the internal equation
      With DynaVars Do
      If (IterationFlag = 0) Then Begin {First iteration of new time step}
          ThetaHistory := Theta + 0.5*h*dTheta;
          SpeedHistory := Speed + 0.5*h*dSpeed;
      End;

      // Compute shaft dynamics
      TracePower := TerminalPowerIn(Vterminal,Iterminal,FnPhases) ;
      dSpeed := (Pshaft + TracePower.re - D*Speed) / Mmass;
//      dSpeed := (Torque + TerminalPowerIn(Vtemp,Itemp,FnPhases).re/Speed) / (Mmass);
      dTheta  := Speed ;

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

      If GenModel=6 then Begin
       If UserModel.Exists    Then UserModel.Integrate;
       If ShaftModel.Exists   Then ShaftModel.Integrate;
      End
      else
      Begin
        WindModelDyn.Integrate;
      End;

    End
    else
    Begin
      // Dynamics using an external equation
      With DynaVars Do
        If (IterationFlag = 0) Then Begin {First iteration of new time step}
            SpeedHistory := DynamicEqVals[DynOut[0]][0] + 0.5*h*DynamicEqVals[DynOut[0]][1]; // first speed
            ThetaHistory := DynamicEqVals[DynOut[1]][0] + 0.5*h*DynamicEqVals[DynOut[1]][1]; // then angle
        End;

      // Check for initial conditions using calculated values (P, Q, VMag, VAng, IMag, IAng)
      NumData   :=  ( length(DynamicEqPair) div 2 )  - 1 ;
      for i := 0 to NumData do
        if not DynamicEqObj.IsInitVal(DynamicEqPair[( i * 2 ) + 1]) then     // it's not intialization
        Begin
          case DynamicEqPair[( i * 2 ) + 1] of
            0 :  DynamicEqVals[DynamicEqPair[ i * 2 ]][0] := -TerminalPowerIn(Vterminal,Iterminal,FnPhases).re;
            1 :  DynamicEqVals[DynamicEqPair[ i * 2 ]][0] := -TerminalPowerIn(Vterminal,Iterminal,FnPhases).im;
          else
            DynamicEqVals[DynamicEqPair[ i * 2 ]][0] := PCEValue[1, DynamicEqPair[( i * 2 ) + 1], ActorID];
          end;
        End;
      // solves the differential equation using the given values
      DynamicEqObj.SolveEq(DynamicEqVals);
      // Trapezoidal method   - Places the calues in the same vars to keep the code consistent
      With DynaVars Do Begin
       Speed := SpeedHistory + 0.5*h*DynamicEqVals[DynOut[0]][1];
       Theta := ThetaHistory + 0.5*h*DynamicEqVals[DynOut[1]][1];
      End;

      // saves the new integration values in memoryspace
      DynamicEqVals[DynOut[0]][0] :=  Speed;
      DynamicEqVals[DynOut[1]][0] :=  Theta;
    End;

  End;

end;

function TWindGenObj.Get_Variable(i: Integer): Double;
{Return variables one at a time}

Var
      N, k:Integer;

begin
  N := 0;
  Result := -9999.99;  // error return value
  If i < 1 Then Exit;  // Someone goofed

  if i < 19 then
    Result := WindModelDyn.Variable[i]
  else
  Begin
    case i of
      19 : Result := WindGenVars.Pg;
      20 : Result := WindGenVars.Ps;
      21 : Result := WindGenVars.Pr;
      22 : Result := WindGenVars.s;
      else
       Result := -9999.99;
    end;
  End;

  If UserModel.Exists Then
  Begin
    N := UserModel.FNumVars;
    k := (i-NumWGenVariables);
    If k <= N Then Begin
        Result := UserModel.FGetVariable(k);
        Exit;
    End;
  End;

  {If we get here, must be in the Shaft Model if anywhere}
  If ShaftModel.Exists Then
  Begin
    k := i-(NumWGenVariables+N);
    If k > 0 Then Result := ShaftModel.FGetVariable( k );
  End;

end;

procedure TWindGenObj.Set_Variable(i: Integer;  Value: Double);
var N, k:Integer;

begin
  N := 0;
  If i<1 Then Exit;  // Someone goofed
  With WindGenvars Do
  Begin
      if i < 19 then
        WindModelDyn.Variable[i] := Value
      else
      Begin
        case i of
            19 : WindGenVars.Pg := Value;
            20 : WindGenVars.Ps := Value;
            21 : WindGenVars.Pr := Value;
            22 : WindGenVars.s := Value;
            else
            Begin
              //Do nothing
            End;
        end;
      End;
         If UserModel.Exists Then Begin
            N := UserModel.FNumVars;
            k := (i-NumWGenVariables) ;
            If  k<= N Then Begin
                UserModel.FSetVariable( k, Value );
                Exit;
              End;
          End;
         // If we get here, must be in the shaft model
         If ShaftModel.Exists Then Begin
            k := (i-(NumWGenVariables+N)) ;
            If  k > 0 Then ShaftModel.FSetVariable( k, Value );
          End;
     End;
end;

procedure TWindGenObj.GetAllVariables(States: pDoubleArray);

Var  i, N:Integer;
begin
  N := 0;
  if DynamiceqObj = nil then
    For i := 1 to NumWGenVariables Do States^[i] := Variable[i]
  else
    For i := 1 to DynamiceqObj.NumVars * length(DynamicEqVals[0]) Do
      States^[i] := DynamiceqObj.Get_DynamicEqVal(i - 1, DynamicEqVals);

  If UserModel.Exists Then Begin
    N := UserModel.FNumVars;
    UserModel.FGetAllVars(@States^[NumWGenVariables+1]);
  End;

  If ShaftModel.Exists Then Begin
    ShaftModel.FGetAllVars(@States^[NumWGenVariables+1+N]);
  End;
end;

function TWindGenObj.NumVariables: Integer;
begin
     Result  := NumWGenVariables;
     If UserModel.Exists    then Result := Result + UserModel.FNumVars;
     If ShaftModel.Exists   then Result := Result + ShaftModel.FNumVars;
end;

Function TWindGenObj.VariableName(i: Integer):String;
Const
    BuffSize = 255;
Var
    n,
    i2    :integer;
    Buff  :Array[0..BuffSize] of {$IFDEF MSWINDOWS}AnsiChar{$ELSE}char{$ENDIF};
    pName :pAnsichar;
    
begin
    n:=0;
    If i<1 Then Exit;  // Someone goofed
    Case i of
        1:  Result := 'userTrip';
        2:  Result := 'wtgTrip';
        3:  Result := 'Pcurtail';
        4:  Result := 'Pcmd';
        5:  Result := 'Pgen';
        6:  Result := 'Qcmd';
        7:  Result := 'Qgen';
        8:  Result := 'Vref';
        9:  Result := 'Vmag';
        10: Result := 'vwind';
        11: Result := 'WtRef';
        12: Result := 'WtAct';
        13: Result := 'dOmg';
        14: Result := 'dFrqPuTest';
        15: Result := 'QMode';
        16: Result := 'Qref';
        17: Result := 'PFref';
        18: Result := 'thetaPitch';
        19: Result := 'Pg';
        20: Result := 'Ps';
        21: Result := 'Pr';
        22: Result := 's';
    Else
      Begin
        If UserModel.Exists Then  // Checks for existence and Selects
          Begin
            pName := @Buff;
            n := UserModel.FNumVars;
            i2 := i-NumWGenVariables;
            If i2 <= n Then
              Begin
               // DLL functions require AnsiString (AnsiString) type
               UserModel.FGetVarName(i2, pName, BuffSize);
               Result := String(pName);
               Exit;
              End;
          End;

        If ShaftModel.Exists Then
          Begin
            pName := @Buff;
            i2 := i-NumWGenVariables-n;
            If i2>0 Then UserModel.FGetVarName(i2, pName, BuffSize);
            Result := String(pName);
          End;
      End;
    End;

end;

function TWindGenObj.GetPropertyValue(Index: Integer): String;

begin
      Result := '';
      CASE Index of
         3:  Result := Format('%.6g', [WindGenvars.kVWindGenBase]);
         4:  Result := Format('%.6g', [kWBase]);
         5:  Result := Format('%.6g', [PFNominal]);
         7:  Result := Yearlyshape;
         8:  Result := Dailydispshape;
         9:  Result := DutyShape;
         13: Result := Format('%.6g', [kvarBase]);
         19: Result := Format('%.6g', [kvarMax]);
         20: Result := Format('%.6g', [kvarMin]);
         26: Result := Format('%.6g', [WindGenvars.kVArating]);
         27: Result := Format('%.6g', [WindGenvars.kVArating*0.001]);
         34,36: Begin
                    Result := '(' + inherited GetPropertyValue(index) + ')';
                End;
         37: Result := Format('%.6g', [DutyStart]);
         38: If ForceBalanced Then Result := 'Yes' else Result := 'No';
         40: Result :=  DynamicEq;
         41: Result :=  GetDynOutputStr();
      ELSE
         Result := Inherited GetPropertyValue(index);
      END;
end;

procedure TWindGenObj.MakePosSequence(ActorID : Integer);

Var
    S :String;
    V :Double;

begin

  S := 'Phases=1 conn=wye';

  // Make sure voltage is line-neutral
  If (Fnphases>1) or (connection<>0) Then   V :=  WindGenvars.kVWindGenBase/SQRT3
  Else V :=  WindGenvars.kVWindGenBase;

  S := S + Format(' kV=%-.5g',[V]);

  // Divide the load by no. phases
  If Fnphases>1 Then
  Begin
      S := S + Format(' kW=%-.5g  PF=%-.5g',[kWbase/Fnphases, PFNominal]);
      If (PrpSequence^[19]<>0) or (PrpSequence^[20]<>0) Then S := S + Format(' maxkvar=%-.5g  minkvar=%-.5g',[kvarmax/Fnphases, kvarmin/Fnphases]);
      If PrpSequence^[26]>0 Then S := S + Format(' kva=%-.5g  ',[WindGenvars.kvarating/Fnphases]);
      If PrpSequence^[27]>0 Then S := S + Format(' MVA=%-.5g  ',[WindGenvars.kvarating/1000.0/Fnphases]);
  End;

  Parser[ActorID].CmdString := S;
  Edit(ActorID);

  inherited;
end;

procedure TWindGenObj.Set_ConductorClosed(Index: Integer; ActorID: Integer;
  Value: Boolean);
begin
   inherited;

 // Just turn WindGen on or off;

   If Value Then GenSwitchOpen := FALSE Else GenSwitchOpen := TRUE;

end;



procedure TWindGenObj.Set_PowerFactor(const Value: Double);
begin
     PFNominal := Value;
     SyncUpPowerQuantities;
end;

procedure TWindGenObj.Set_PresentkV(const Value: Double);
begin
   With WindGenvars Do Begin
      kVWindGenBase := Value ;
      Case FNphases Of
           2,3: VBase := kVWindGenBase * InvSQRT3x1000;
      Else
             VBase := kVWindGenBase * 1000.0 ;
      End;
   End;
end;

procedure TWindGenObj.Set_Presentkvar(const Value: Double);
Var
   kVA_Gen :Double;

begin
   kvarBase := Value;
   WindGenvars.Qnominalperphase := 1000.0 * kvarBase  / Fnphases; // init to something reasonable
   kVA_Gen := Sqrt(Sqr(kWBase) + Sqr(kvarBase)) ;
   IF kVA_Gen <> 0.0 THEN PFNominal := kWBase / kVA_Gen ELSE PFNominal := 1.0;
   If (kWBase*kvarBase) < 0.0 Then PFNominal := -PFNominal;

   kvarMax  := 2.0 * kvarBase;
   kvarMin  := -kvarMax;
end;

procedure TWindGenObj.Set_PresentkW(const Value: Double);
begin

   kWBase := Value;
   SyncUpPowerQuantities;

End;

procedure TWindGenObj.SyncUpPowerQuantities;
Begin

   // keep kvar nominal up to date with kW and PF
   If (PFNominal <> 0.0)  Then Begin
      kvarBase := kWBase* sqrt(1.0/Sqr(PFNominal) - 1.0);
      WindGenvars.Qnominalperphase := 1000.0* kvarBase / Fnphases;
      kvarMax  := 2.0 * kvarBase;
      kvarMin  := -kvarMax;
      If PFNominal<0.0 Then kvarBase := -kvarBase;

      If kVANotSet Then WindGenvars.kVARating := kWBase * 1.2;

   End;

end;

procedure TWindGenObj.SetDragHandRegister(Reg: Integer;
  const Value: Double);
begin
    If Value>Registers[reg] Then Registers[Reg] := Value;
end;

procedure TWindGenObj.SetkWkvar(const PkW, Qkvar: Double);
begin

     kWBase      := PkW;
     Presentkvar := Qkvar;

end;

procedure TWindGenObj.CalcVthev_Dyn;
begin
   If GenSwitchOpen Then WindGenvars.VThevMag := 0.0;
   Vthev := pclx(WindGenvars.VthevMag, WindGenvars.Theta);
end;

procedure TWindGenObj.CalcVthev_Dyn_Mod7(const V: Complex);
{Adjust VThev to be in phase with V, if possible}
{
 If the voltage magnitude drops below 15% or so, the accuracy of determining the
 phase angle gets flaky. This algorithm approximates the action of a PLL that will
 hold the last phase angle until the voltage recovers.
}
Var
    Model7angle : Double;
begin
   If GenSwitchOpen Then WindGenvars.VThevMag := 0.0;
   {
      For Phases=1, Vbase is voltage across the terminals.
      Else it is LN voltage.
   }
   If Cabs(V) > 0.2 * Vbase Then  Model7angle := Cang(V)
   Else Model7Angle := Model7LastAngle;

   Vthev := pclx(WindGenvars.VthevMag, Model7angle);
   Model7Lastangle := Model7angle;

end;

end.

