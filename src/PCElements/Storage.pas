unit Storage;

{
  ----------------------------------------------------------
  Copyright (c) 2009-2016, Electric Power Research Institute, Inc.
  All rights reserved.
  ----------------------------------------------------------
}

{   Change Log

    10/04/2009 Created from Generator Model


  To Do:
    Make connection to User model
    Yprim for various modes
    Define state vars and dynamics mode behavior
    Complete Harmonics mode algorithm (generator mode is implemented)
}
{
  The storage element is essentially a generator that can be dispatched
  to either produce power or consume power commensurate with rating and
  amount of stored energy.

  The storage element can also produce or absorb vars within the kVA rating of the inverter.
  That is, a StorageController object requests kvar and the storage element provides them if
  it has any capacity left. The storage element can produce/absorb kvar while idling.
}

//  The Storage element is assumed balanced over the no. of phases defined


interface

uses
    Classes,
    StorageVars,
    StoreUserModel,
    DSSClass,
    PCClass,
    PCElement,
    ucmatrix,
    ucomplex,
    LoadShape,
    Spectrum,
    ArrayDef,
    Dynamics;

const
    NumStorageRegisters = 6;    // Number of energy meter registers
    NumStorageVariables = 7;    // No state variables

//= = = = = = = = = = = = = = DEFINE STATES = = = = = = = = = = = = = = = = = = = = = = = = =

    STORE_CHARGING = -1;
    STORE_IDLING = 0;
    STORE_DISCHARGING = 1;
//= = = = = = = = = = = = = = DEFINE DISPATCH MODES = = = = = = = = = = = = = = = = = = = = = = = = =

    STORE_DEFAULT = 0;
    STORE_LOADMODE = 1;
    STORE_PRICEMODE = 2;
    STORE_EXTERNALMODE = 3;
    STORE_FOLLOW = 4;

type


// = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = =
    TStorage = class(TPCClass)
    PRIVATE

        procedure InterpretConnection(const S: String);
        procedure SetNcondsForConnection;
    PROTECTED
        procedure DefineProperties;
        function MakeLike(const OtherStorageObjName: String): Integer; OVERRIDE;
    PUBLIC
        RegisterNames: array[1..NumStorageRegisters] of String;

        constructor Create;
        destructor Destroy; OVERRIDE;

        function Edit: Integer; OVERRIDE;
        function NewObject(const ObjName: String): Integer; OVERRIDE;

        procedure ResetRegistersAll;
        procedure SampleAll;
        procedure UpdateAll;

    end;

// = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = =
    TStorageObj = class(TPCElement)
    PRIVATE
        Yeq: Complex;   // at nominal
        Yeq95: Complex;   // at 95%
        Yeq105: Complex;   // at 105%
        YeqIdling: Complex;   // in shunt representing idle impedance
        YeqDischarge: Complex;   // equiv at rated power of storage element only
        PhaseCurrentLimit: Complex;
        MaxDynPhaseCurrent: Double;

        DebugTrace: Boolean;
        FState: Integer;
        FStateChanged: Boolean;
        FirstSampleAfterReset: Boolean;
        StorageSolutionCount: Integer;
        StorageFundamental: Double;  {Thevinen equivalent voltage mag and angle reference for Harmonic model}
        StorageObjSwitchOpen: Boolean;

        ForceBalanced: Boolean;
        CurrentLimited: Boolean;

        kVANotSet: Boolean;
        kvar_out: Double;
        kW_out: Double;
        pctIdlekW: Double;
        pctIdlekvar: Double;
        pctChargeEff: Double;
        pctDischargeEff: Double;
        DischargeTrigger: Double;
        ChargeTrigger: Double;
        ChargeTime: Double;
        kWhBeforeUpdate: Double;

        pctR: Double;
        pctX: Double;

        OpenStorageSolutionCount: Integer;
        Pnominalperphase: Double;
        Qnominalperphase: Double;
        RandomMult: Double;

        Reg_Hours: Integer;
        Reg_kvarh: Integer;
        Reg_kWh: Integer;
        Reg_MaxkVA: Integer;
        Reg_MaxkW: Integer;
        Reg_Price: Integer;
        ShapeFactor: Complex;
        TraceFile: TFileStream;
        IsUserModel: Boolean;
        UserModel: TStoreUserModel;   {User-Written Models}
        DynaModel: TStoreDynaModel;
        kvarBase: Double;  // Base vars per phase
        VBase: Double;  // Base volts suitable for computing currents
        VBase105: Double;
        VBase95: Double;
        Vmaxpu: Double;
        Vminpu: Double;
        YPrimOpenCond: TCmatrix;


        procedure CalcDailyMult(Hr: Double);
        procedure CalcDutyMult(Hr: Double);
        procedure CalcStorageModelContribution;
        procedure CalcInjCurrentArray;
        (*PROCEDURE CalcVterminal;*)
        procedure CalcVTerminalPhase;
        procedure CalcYearlyMult(Hr: Double);
        procedure CalcYPrimMatrix(Ymatrix: TcMatrix);

        procedure DoConstantPQStorageObj;
        procedure DoConstantZStorageObj;
        procedure DoDynamicMode;
        procedure DoHarmonicMode;
        procedure DoUserModel;
        procedure DoDynaModel;

        procedure Integrate(Reg: Integer; const Deriv: Double; const Interval: Double);
        procedure SetDragHandRegister(Reg: Integer; const Value: Double);
        procedure StickCurrInTerminalArray(TermArray: pComplexArray; const Curr: Complex; i: Integer);

        procedure WriteTraceRecord(const s: String);

        procedure SyncUpPowerQuantities;
        procedure SetKWandKvarOut;
        procedure CheckStateTriggerLevel(Level: Double);
        procedure UpdateStorage;    // Update Storage elements based on present kW and IntervalHrs variable
        function NormalizeToTOD(h: Integer; sec: Double): Double;

        function InterpretState(const S: String): Integer;
//        FUNCTION  StateToStr:String;
        function DecodeState: String;

        function Get_PresentkW: Double;
        function Get_Presentkvar: Double;
        function Get_PresentkV: Double;
        procedure Set_PresentkV(const Value: Double);
        procedure Set_Presentkvar(const Value: Double);
        procedure Set_PresentkW(const Value: Double);
        procedure Set_PowerFactor(const Value: Double);
        procedure Set_StorageState(const Value: Integer);
        procedure Set_pctkvarOut(const Value: Double);
        procedure Set_pctkWOut(const Value: Double);
        function Get_kWTotalLosses: Double;
        function Get_kWIdlingLosses: Double;

    PROTECTED
        procedure GetTerminalCurrents(Curr: pComplexArray); OVERRIDE;

    PUBLIC

        Connection: Integer;  {0 = line-neutral; 1=Delta}
        DailyShape: String;  // Daily (24 HR) Storage element shape
        DailyShapeObj: TLoadShapeObj;  // Daily Storage element Shape for this load
        DutyShape: String;  // Duty cycle load shape for changes typically less than one hour
        DutyShapeObj: TLoadShapeObj;  // Shape for this Storage element
        StorageClass: Integer;
        VoltageModel: Integer;   // Variation with voltage
        PFNominal: Double;
        YearlyShape: String;  // ='fixed' means no variation  on all the time
        YearlyShapeObj: TLoadShapeObj;  // Shape for this Storage element

        StorageVars: TStorageVars;

        FpctkWout: Double;   // percent of kW rated output currently dispatched
        Fpctkvarout: Double;
        pctkWin: Double;
        pctReserve: Double;
        DispatchMode: Integer;

        Registers, Derivatives: array[1..NumStorageRegisters] of Double;

        constructor Create(ParClass: TDSSClass; const SourceName: String);
        destructor Destroy; OVERRIDE;

        procedure Set_ConductorClosed(Index: Integer; Value: Boolean); OVERRIDE;
        procedure RecalcElementData; OVERRIDE;
        procedure CalcYPrim; OVERRIDE;

        function InjCurrents: Integer; OVERRIDE;
        procedure GetInjCurrents(Curr: pComplexArray); OVERRIDE;
        function NumVariables: Integer; OVERRIDE;
        procedure GetAllVariables(States: pDoubleArray); OVERRIDE;
        function Get_Variable(i: Integer): Double; OVERRIDE;
        procedure Set_Variable(i: Integer; Value: Double); OVERRIDE;
        function VariableName(i: Integer): String; OVERRIDE;

        procedure SetNominalStorageOuput;
        procedure Randomize(Opt: Integer);   // 0 = reset to 1.0; 1 = Gaussian around mean and std Dev  ;  // 2 = uniform

        procedure ResetRegisters;
        procedure TakeSample;

        // Support for Dynamics Mode
        procedure InitStateVars; OVERRIDE;
        procedure IntegrateStates; OVERRIDE;

        // Support for Harmonics Mode
        procedure InitHarmonics; OVERRIDE;

        procedure MakePosSequence; OVERRIDE;  // Make a positive Sequence Model

        procedure InitPropertyValues(ArrayOffset: Integer); OVERRIDE;
        procedure DumpProperties(F: TFileStream; Complete: Boolean); OVERRIDE;
        function GetPropertyValue(Index: Integer): String; OVERRIDE;

        property PresentkW: Double READ Get_PresentkW WRITE Set_PresentkW;
        property Presentkvar: Double READ Get_Presentkvar WRITE Set_Presentkvar;
        property PresentkV: Double READ Get_PresentkV WRITE Set_PresentkV;
        property PowerFactor: Double READ PFNominal WRITE Set_PowerFactor;

        property StorageState: Integer READ FState WRITE Set_StorageState;
        property PctkWOut: Double READ FpctkWOut WRITE Set_pctkWOut;
        property PctkVarOut: Double READ FpctkvarOut WRITE Set_pctkvarOut;

        property kWTotalLosses: Double READ Get_kWTotalLosses;
        property kWIdlingLosses: Double READ Get_kWIdlingLosses;

        property MinModelVoltagePU: Double READ VminPu;
    end;

var
    ActiveStorageObj: TStorageObj;

// = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = =
implementation


uses
    ParserDel,
    Circuit,
    Sysutils,
    Command,
    Math,
    MathUtil,
    DSSClassDefs,
    DSSGlobals,
    Utilities;

const

{  = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = =
   To add a property,
    1) add a property constant to this list
    2) add a handler to the CASE statement in the Edit FUNCTION
    3) add a statement(s) to InitPropertyValues FUNCTION to initialize the string value
    4) add any special handlers to DumpProperties and GetPropertyValue, If needed
 = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = =}

    propKV = 3;
    propKW = 4;
    propPF = 5;
    propMODEL = 6;
    propYEARLY = 7;
    propDAILY = 8;
    propDUTY = 9;
    propDISPMODE = 10;
    propIDLEKVAR = 11;
    propCONNECTION = 12;
    propKVAR = 13;
    propPCTR = 14;
    propPCTX = 15;
    propIDLEKW = 16;
    propCLASS = 17;
    propDISPOUTTRIG = 18;
    propDISPINTRIG = 19;
    propCHARGEEFF = 20;
    propDISCHARGEEFF = 21;
    propPCTKWOUT = 22;
    propVMINPU = 23;
    propVMAXPU = 24;
    propSTATE = 25;
    propKVA = 26;
    propKWRATED = 27;
    propKWHRATED = 28;
    propKWHSTORED = 29;
    propPCTRESERVE = 30;
    propUSERMODEL = 31;
    propUSERDATA = 32;
    propDEBUGTRACE = 33;
    propPCTKWIN = 34;
    propPCTSTORED = 35;
    propCHARGETIME = 36;
    propDynaDLL = 37;
    propDynaData = 38;
    propBalanced = 39;
    propLimited = 40;

    NumPropsThisClass = 40; // Make this agree with the last property constant


var
    cBuffer: array[1..24] of Complex;  // Temp buffer for calcs  24-phase Storage element?
    CDOUBLEONE: Complex;

//- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
constructor TStorage.Create;  // Creates superstructure for all Storage elements
begin
    inherited Create;
    Class_Name := 'Storage';
    DSSClassType := DSSClassType + STORAGE_ELEMENT;  // In both PCelement and Storage element list

    ActiveElement := 0;

     // Set Register names
    RegisterNames[1] := 'kWh';
    RegisterNames[2] := 'kvarh';
    RegisterNames[3] := 'Max kW';
    RegisterNames[4] := 'Max kVA';
    RegisterNames[5] := 'Hours';
    RegisterNames[6] := 'Price($)';

    DefineProperties;

    CommandList := TCommandList.Create(Slice(PropertyName^, NumProperties));
    CommandList.Abbrev := TRUE;
end;

//- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
destructor TStorage.Destroy;

begin
    // ElementList and  CommandList freed in inherited destroy
    inherited Destroy;

end;

//- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
procedure TStorage.DefineProperties;
begin

    Numproperties := NumPropsThisClass;
    CountProperties;   // Get inherited property count
    AllocatePropertyArrays;   {see DSSClass}

     // Define Property names
     {
      Using the AddProperty FUNCTION, you can list the properties here in the order you want
      them to appear when properties are accessed sequentially without tags.   Syntax:

      AddProperty( <name of property>, <index in the EDIT Case statement>, <help text>);

     }
    AddProperty('phases', 1,
        'Number of Phases, this Storage element.  Power is evenly divided among phases.');
    AddProperty('bus1', 2,
        'Bus to which the Storage element is connected.  May include specific node specification.');
    AddProperty('kv', propKV,
        'Nominal rated (1.0 per unit) voltage, kV, for Storage element. For 2- and 3-phase Storage elements, specify phase-phase kV. ' +
        'Otherwise, specify actual kV across each branch of the Storage element. ' + CRLF + CRLF +
        'If wye (star), specify phase-neutral kV. ' + CRLF + CRLF +
        'If delta or phase-phase connected, specify phase-phase kV.');  // line-neutral voltage//  base voltage
    AddProperty('kW', propKW,
        'Get/set the present kW value.  A positive value denotes power coming OUT of the element, ' +
        'which is the opposite of a Load element. A negative value indicates the Storage element is in Charging state. ' +
        'This value is modified internally depending on the dispatch mode. ');
    AddProperty('pf', propPF,
        'Nominally, the power factor for discharging (acting as a generator). Default is 1.0. ' + CRLF + CRLF +
        'Setting this property will also set the kvar property.' +
        'Enter negative for leading powerfactor ' +
        '(when kW and kvar have opposite signs.)' + CRLF + CRLF +
        'A positive power factor for a generator signifies that the Storage element produces vars ' +
        'as is typical for a generator.  ');
    AddProperty('conn', propCONNECTION,
        '={wye|LN|delta|LL}.  Default is wye.');
    AddProperty('kvar', propKVAR,
        'Get/set the present kvar value.  Alternative to specifying the power factor.  Side effect: ' +
        ' the power factor value is altered to agree based on present value of kW.');
    AddProperty('kVA', propKVA,
        'kVA rating of power output. Defaults to rated kW. Used as the base for Dynamics mode and Harmonics mode values.');
    AddProperty('kWrated', propKWRATED,
        'kW rating of power output. Base for Loadshapes when DispMode=Follow. Side effect: Sets KVA property.');

    AddProperty('kWhrated', propKWHRATED,
        'Rated storage capacity in kWh. Default is 50.');
    AddProperty('kWhstored', propKWHSTORED,
        'Present amount of energy stored, kWh. Default is same as kWh rated.');
    AddProperty('%stored', propPCTSTORED,
        'Present amount of energy stored, % of rated kWh. Default is 100%.');
    AddProperty('%reserve', propPCTRESERVE,
        'Percent of rated kWh storage capacity to be held in reserve for normal operation. Default = 20. ' + CRLF +
        'This is treated as the minimum energy discharge level unless there is an emergency. For emergency operation ' +
        'set this property lower. Cannot be less than zero.');
    AddProperty('State', propSTATE,
        '{IDLING | CHARGING | DISCHARGING}  Get/Set present operational state. In DISCHARGING mode, the Storage element ' +
        'acts as a generator and the kW property is positive. The element continues discharging at the scheduled output power level ' +
        'until the storage reaches the reserve value. Then the state reverts to IDLING. ' +
        'In the CHARGING state, the Storage element behaves like a Load and the kW property is negative. ' +
        'The element continues to charge until the max storage kWh is reached and Then switches to IDLING state. ' +
        'In IDLING state, the kW property shows zero. However, the resistive and reactive loss elements remain in the circuit ' +
        'and the power flow report will show power being consumed.');
    AddProperty('%Discharge', propPCTKWOUT,
        'Discharge rate (output power) in Percent of rated kW. Default = 100.');
    AddProperty('%Charge', propPCTKWIN,
        'Charging rate (input power) in Percent of rated kW. Default = 100.');
    AddProperty('%EffCharge', propCHARGEEFF,
        'Percent efficiency for CHARGING the storage element. Default = 90.');
    AddProperty('%EffDischarge', propDISCHARGEEFF,
        'Percent efficiency for DISCHARGING the storage element. Default = 90.' +
        'Idling losses are handled by %IdlingkW property and are in addition to the charging and discharging efficiency losses ' +
        'in the power conversion process inside the unit.');
    AddProperty('%IdlingkW', propIDLEKW,
        'Percent of rated kW consumed while idling. Default = 1.');
    AddProperty('%Idlingkvar', propIDLEKVAR,
        'Percent of rated kW consumed as reactive power (kvar) while idling. Default = 0.');
    AddProperty('%R', propPCTR,
        'Equivalent percent internal resistance, ohms. Default is 0. Placed in series with internal voltage source' +
        ' for harmonics and dynamics modes. Use a combination of %IdlekW and %EffCharge and %EffDischarge to account for ' +
        'losses in power flow modes.');
    AddProperty('%X', propPCTX,
        'Equivalent percent internal reactance, ohms. Default is 50%. Placed in series with internal voltage source' +
        ' for harmonics and dynamics modes. (Limits fault current to 2 pu.) ' +
        'Use %Idlekvar and kvar properties to account for any reactive power during power flow solutions.');
    AddProperty('model', propMODEL,
        'Integer code (default=1) for the model to use for powet output variation with voltage. ' +
        'Valid values are:' + CRLF + CRLF +
        '1:Storage element injects a CONSTANT kW at specified power factor.' + CRLF +
        '2:Storage element is modeled as a CONSTANT ADMITTANCE.' + CRLF +
        '3:Compute load injection from User-written Model.');

    AddProperty('Vminpu', propVMINPU,
        'Default = 0.90.  Minimum per unit voltage for which the Model is assumed to apply. ' +
        'Below this value, the load model reverts to a constant impedance model.');
    AddProperty('Vmaxpu', propVMAXPU,
        'Default = 1.10.  Maximum per unit voltage for which the Model is assumed to apply. ' +
        'Above this value, the load model reverts to a constant impedance model.');
    AddProperty('Balanced', propBalanced, '{Yes | No*} Default is No.  Force balanced current only for 3-phase Storage. Forces zero- and negative-sequence to zero.  ');
    AddProperty('LimitCurrent', propLimited, 'Limits current magnitude to Vminpu value for both 1-phase and 3-phase Storage similar to Generator Model 7. For 3-phase, ' +
        'limits the positive-sequence current but not the negative-sequence.');
    AddProperty('yearly', propYEARLY,
        'Dispatch shape to use for yearly simulations.  Must be previously defined ' +
        'as a Loadshape object. If this is not specified, the Daily dispatch shape, if any, is repeated ' +
        'during Yearly solution modes. In the default dispatch mode, ' +
        'the Storage element uses this loadshape to trigger State changes.');
    AddProperty('daily', propDAILY,
        'Dispatch shape to use for daily simulations.  Must be previously defined ' +
        'as a Loadshape object of 24 hrs, typically.  In the default dispatch mode, ' +
        'the Storage element uses this loadshape to trigger State changes.'); // daily dispatch (hourly)
    AddProperty('duty', propDUTY,
        'Load shape to use for duty cycle dispatch simulations such as for solar ramp rate studies. ' +
        'Must be previously defined as a Loadshape object. ' + CRLF + CRLF +
        'Typically would have time intervals of 1-5 seconds. ' + CRLF + CRLF +
        'Designate the number of points to solve using the Set Number=xxxx command. ' +
        'If there are fewer points in the actual shape, the shape is assumed to repeat.');  // as for wind generation
    AddProperty('DispMode', propDISPMODE,
        '{DEFAULT | FOLLOW | EXTERNAL | LOADLEVEL | PRICE } Default = "DEFAULT". Dispatch mode. ' + CRLF + CRLF +
        'In DEFAULT mode, Storage element state is triggered to discharge or charge at the specified rate by the ' +
        'loadshape curve corresponding to the solution mode. ' + CRLF + CRLF +
        'In FOLLOW mode the kW and kvar output of the STORAGE element follows the active loadshape multipliers ' +
        'until storage is either exhausted or full. ' +
        'The element discharges for positive values and charges for negative values.  The loadshapes are based on the kW and kvar ' +
        'values in the most recent definition of kW and PF or kW and kvar properties. ' + CRLF + CRLF +
        'In EXTERNAL mode, Storage element state is controlled by an external Storage controller. ' +
        'This mode is automatically set if this Storage element is included in the element list of a StorageController element. ' + CRLF + CRLF +
        'For the other two dispatch modes, the Storage element state is controlled by either the global default Loadlevel value or the price level. ');
    AddProperty('DischargeTrigger', propDISPOUTTRIG,
        'Dispatch trigger value for discharging the storage. ' + CRLF +
        'If = 0.0 the Storage element state is changed by the State command or by a StorageController object. ' + CRLF +
        'If <> 0  the Storage element state is set to DISCHARGING when this trigger level is EXCEEDED by either the specified ' +
        'Loadshape curve value or the price signal or global Loadlevel value, depending on dispatch mode. See State property.');
    AddProperty('ChargeTrigger', propDISPINTRIG,
        'Dispatch trigger value for charging the storage. ' + CRLF + CRLF +
        'If = 0.0 the Storage element state is changed by the State command or StorageController object.  ' + CRLF + CRLF +
        'If <> 0  the Storage element state is set to CHARGING when this trigger level is GREATER than either the specified ' +
        'Loadshape curve value or the price signal or global Loadlevel value, depending on dispatch mode. See State property.');
    AddProperty('TimeChargeTrig', propCHARGETIME,
        'Time of day in fractional hours (0230 = 2.5) at which storage element will automatically go into charge state. ' +
        'Default is 2.0.  Enter a negative time value to disable this feature.');

    AddProperty('class', propCLASS,
        'An arbitrary integer number representing the class of Storage element so that Storage values may ' +
        'be segregated by class.'); // integer

    AddProperty('DynaDLL', propDynaDLL,
        'Name of DLL containing user-written dynamics model, which computes the terminal currents for Dynamics-mode simulations, ' +
        'overriding the default model.  Set to "none" to negate previous setting. ' +
        'This DLL has a simpler interface than the UserModel DLL and is only used for Dynamics mode.');
    AddProperty('DynaData', propDYNADATA,
        'String (in quotes or parentheses if necessary) that gets passed to the user-written dynamics model Edit function for defining the data required for that model.');
    AddProperty('UserModel', propUSERMODEL,
        'Name of DLL containing user-written model, which computes the terminal currents for both power flow and dynamics, ' +
        'overriding the default model.  Set to "none" to negate previous setting.');
    AddProperty('UserData', propUSERDATA,
        'String (in quotes or parentheses) that gets passed to user-written model for defining the data required for that model.');
    AddProperty('debugtrace', propDEBUGTRACE,
        '{Yes | No }  Default is no.  Turn this on to capture the progress of the Storage model ' +
        'for each iteration.  Creates a separate file for each Storage element named "STORAGE_name.CSV".');


    ActiveProperty := NumPropsThisClass;
    inherited DefineProperties;  // Add defs of inherited properties to bottom of list

     // Override default help string
    PropertyHelp[NumPropsThisClass + 1] := 'Name of harmonic voltage or current spectrum for this Storage element. ' +
        'Current injection is assumed for inverter. ' +
        'Default value is "default", which is defined when the DSS starts.';

end;

//- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
function TStorage.NewObject(const ObjName: String): Integer;
begin
    // Make a new Storage element and add it to Storage class list
    with ActiveCircuit do
    begin
        ActiveCktElement := TStorageObj.Create(Self, ObjName);
        Result := AddObjectToList(ActiveDSSObject);
    end;
end;

//- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
procedure TStorage.SetNcondsForConnection;

begin
    with ActiveStorageObj do
    begin
        case Connection of
            0:
                NConds := Fnphases + 1;
            1:
                case Fnphases of
                    1, 2:
                        NConds := Fnphases + 1; // L-L and Open-delta
                else
                    NConds := Fnphases;
                end;
        end;
    end;
end;

//- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
procedure TStorage.UpdateAll;
var
    i: Integer;
begin
    for i := 1 to ElementList.ListSize do
        with TStorageObj(ElementList.Get(i)) do
            if Enabled then
                UpdateStorage;
end;

//- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
procedure TStorage.InterpretConnection(const S: String);

// Accepts
//    delta or LL           (Case insensitive)
//    Y, wye, or LN
var
    TestS: String;

begin
    with ActiveStorageObj do
    begin
        TestS := lowercase(S);
        case TestS[1] of
            'y', 'w':
                Connection := 0;  {Wye}
            'd':
                Connection := 1;  {Delta or line-Line}
            'l':
                case Tests[2] of
                    'n':
                        Connection := 0;
                    'l':
                        Connection := 1;
                end;
        end;

        SetNCondsForConnection;

          {VBase is always L-N voltage unless 1-phase device or more than 3 phases}

        case Fnphases of
            2, 3:
                VBase := StorageVars.kVStorageBase * InvSQRT3x1000;    // L-N Volts
        else
            VBase := StorageVars.kVStorageBase * 1000.0;   // Just use what is supplied
        end;

        VBase95 := Vminpu * VBase;
        VBase105 := Vmaxpu * VBase;

        Yorder := Fnconds * Fnterms;
        YPrimInvalid := TRUE;
    end;
end;

//- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
function InterpretDispMode(const S: String): Integer;
begin
    case lowercase(S)[1] of
        'e':
            Result := STORE_EXTERNALMODE;
        'f':
            Result := STORE_FOLLOW;
        'l':
            Result := STORE_LOADMODE;
        'p':
            Result := STORE_PRICEMODE;
    else
        Result := STORE_DEFAULT;
    end;
end;

//- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
function ReturnDispMode(const imode: Integer): String;
begin
    case imode of
        STORE_EXTERNALMODE:
            Result := 'External';
        STORE_FOLLOW:
            Result := 'Follow';
        STORE_LOADMODE:
            Result := 'Loadshape';
        STORE_PRICEMODE:
            Result := 'Price';
    else
        Result := 'default';
    end;
end;


//- - - - - - - - - - - - - - -MAIN EDIT FUNCTION - - - - - - - - - - - - - - -

function TStorage.Edit: Integer;

var
    i, iCase,
    ParamPointer: Integer;
    ParamName: String;
    Param: String;

begin

  // continue parsing with contents of Parser
    ActiveStorageObj := ElementList.Active;
    ActiveCircuit.ActiveCktElement := ActiveStorageObj;

    Result := 0;

    with ActiveStorageObj do
    begin

        ParamPointer := 0;
        ParamName := Parser.NextParam;  // Parse next property off the command line
        Param := Parser.StrValue;   // Put the string value of the property value in local memory for faster access
        while Length(Param) > 0 do
        begin

            if (Length(ParamName) = 0) then
                Inc(ParamPointer)       // If it is not a named property, assume the next property
            else
                ParamPointer := CommandList.GetCommand(ParamName);  // Look up the name in the list for this class

            if (ParamPointer > 0) and (ParamPointer <= NumProperties) then
                PropertyValue[PropertyIdxMap[ParamPointer]] := Param   // Update the string value of the property
            else
                DoSimpleMsg('Unknown parameter "' + ParamName + '" for Storage "' + Name + '"', 560);

            if ParamPointer > 0 then
            begin
                iCase := PropertyIdxMap[ParamPointer];
                case iCASE of
                    0:
                        DoSimpleMsg('Unknown parameter "' + ParamName + '" for Object "' + Class_Name + '.' + Name + '"', 561);
                    1:
                        NPhases := Parser.Intvalue; // num phases
                    2:
                        SetBus(1, param);
                    propKV:
                        PresentkV := Parser.DblValue;
                    propKW:
                        PresentkW := Parser.DblValue;
                    propPF:
                        PFNominal := Parser.DblValue;
                    propMODEL:
                        VoltageModel := Parser.IntValue;
                    propYEARLY:
                        YearlyShape := Param;
                    propDAILY:
                        DailyShape := Param;
                    propDUTY:
                        DutyShape := Param;
                    propDISPMODE:
                        DispatchMode := InterpretDispMode(Param);
                    propIDLEKVAR:
                        pctIdlekvar := Parser.DblValue;
                    propCONNECTION:
                        InterpretConnection(Param);
                    propKVAR:
                        Presentkvar := Parser.DblValue;
                    propPCTR:
                        pctR := Parser.DblValue;
                    propPCTX:
                        pctX := Parser.DblValue;
                    propIDLEKW:
                        pctIdlekW := Parser.DblValue;
                    propCLASS:
                        StorageClass := Parser.IntValue;
                    propDISPOUTTRIG:
                        DischargeTrigger := Parser.DblValue;
                    propDISPINTRIG:
                        ChargeTrigger := Parser.DblValue;
                    propCHARGEEFF:
                        pctChargeEff := Parser.DblValue;
                    propDISCHARGEEFF:
                        pctDischargeEff := Parser.DblValue;
                    propPCTKWOUT:
                        pctkWout := Parser.DblValue;
                    propVMINPU:
                        VMinPu := Parser.DblValue;
                    propVMAXPU:
                        VMaxPu := Parser.DblValue;
                    propSTATE:
                        FState := InterpretState(Param); //****
                    propKVA:
                        StorageVars.kVArating := Parser.DblValue;
                    propKWRATED:
                        StorageVars.kWrating := Parser.DblValue;
                    propKWHRATED:
                        StorageVars.kWhrating := Parser.DblValue;
                    propKWHSTORED:
                        StorageVars.kWhstored := Parser.DblValue;
                    propPCTRESERVE:
                        pctReserve := Parser.DblValue;
                    propUSERMODEL:
                        UserModel.Name := Parser.StrValue;  // Connect to user written models
                    propUSERDATA:
                        if UserModel.Exists then
                            UserModel.Edit := Parser.StrValue;  // Send edit string to user model
                    propDEBUGTRACE:
                        DebugTrace := InterpretYesNo(Param);
                    propPCTKWIN:
                        pctkWIn := Parser.DblValue;
                    propPCTSTORED:
                        StorageVars.kWhStored := Parser.DblValue * 0.01 * StorageVars.kWhRating;
                    propCHARGETIME:
                        ChargeTime := Parser.DblValue;
                    propDynaDLL:
                        DynaModel.Name := Parser.StrValue;
                    propDynaData:
                        if DynaModel.Exists then
                            DynaModel.Edit := Parser.StrValue;
                    propBalanced:
                        ForceBalanced := InterpretYesNo(Param);
                    propLimited:
                        CurrentLimited := InterpretYesNo(Param);


                else
               // Inherited parameters
                    ClassEdit(ActiveStorageObj, ParamPointer - NumPropsThisClass)
                end;

                case iCase of
                    1:
                        SetNcondsForConnection;  // Force Reallocation of terminal info
                    propKW, propPF:
                    begin
                        SyncUpPowerQuantities;   // keep kvar nominal up to date with kW and PF

                    end;

        {Set loadshape objects;  returns nil If not valid}
                    propYEARLY:
                        YearlyShapeObj := LoadShapeClass.Find(YearlyShape);
                    propDAILY:
                        DailyShapeObj := LoadShapeClass.Find(DailyShape);
                    propDUTY:
                        DutyShapeObj := LoadShapeClass.Find(DutyShape);
                    propKWRATED:
                        StorageVars.kVArating := StorageVars.kWrating;
                    propKWHRATED:
                    begin
                        StorageVars.kWhStored := StorageVars.kWhRating; // Assume fully charged
                        kWhBeforeUpdate := StorageVars.kWhStored;
                        StorageVars.kWhReserve := StorageVars.kWhRating * pctReserve * 0.01;
                    end;

                    propPCTRESERVE:
                        StorageVars.kWhReserve := StorageVars.kWhRating * pctReserve * 0.01;

                    propDEBUGTRACE:
                        if DebugTrace then
                        begin   // Init trace file
                            FreeAndNil(TraceFile);
                            TraceFile := TFileStream.Create(GetOutputDirectory + 'STOR_' + Name + '.CSV', fmCreate);
                            FSWrite(TraceFile, 't, Iteration, LoadMultiplier, Mode, LoadModel, StorageModel,  Qnominalperphase, Pnominalperphase, CurrentType');
                            for i := 1 to nphases do
                                FSWrite(Tracefile, ', |Iinj' + IntToStr(i) + '|');
                            for i := 1 to nphases do
                                FSWrite(Tracefile, ', |Iterm' + IntToStr(i) + '|');
                            for i := 1 to nphases do
                                FSWrite(Tracefile, ', |Vterm' + IntToStr(i) + '|');
                            for i := 1 to NumVariables do
                                FSWrite(Tracefile, ', ' + VariableName(i));

                            FSWrite(TraceFile, ',Vthev, Theta');
                            FSWriteln(TraceFile);
                            FSFlush(Tracefile);
                        end
                        else
                        begin
                            FreeAndNil(TraceFile);
                        end;

                    propKVA:
                        kVANotSet := FALSE;
                    propUSERMODEL:
                        IsUserModel := UserModel.Exists;
                    propDynaDLL:
                        IsUserModel := DynaModel.Exists;
                end;
            end;

            ParamName := Parser.NextParam;
            Param := Parser.StrValue;
        end;

        RecalcElementData;
        YPrimInvalid := TRUE;
    end;

end;

//----------------------------------------------------------------------------
function TStorage.MakeLike(const OtherStorageObjName: String): Integer;

// Copy over essential properties from other object

var
    OtherStorageObj: TStorageObj;
    i: Integer;
begin
    Result := 0;
     {See If we can find this line name in the present collection}
    OtherStorageObj := Find(OtherStorageObjName);
    if (OtherStorageObj <> NIL) then
        with ActiveStorageObj do
        begin
            if (Fnphases <> OtherStorageObj.Fnphases) then
            begin
                Nphases := OtherStorageObj.Fnphases;
                NConds := Fnphases;  // Forces reallocation of terminal stuff
                Yorder := Fnconds * Fnterms;
                YPrimInvalid := TRUE;
            end;

            StorageVars.kVStorageBase := OtherStorageObj.StorageVars.kVStorageBase;
            Vbase := OtherStorageObj.Vbase;
            Vminpu := OtherStorageObj.Vminpu;
            Vmaxpu := OtherStorageObj.Vmaxpu;
            Vbase95 := OtherStorageObj.Vbase95;
            Vbase105 := OtherStorageObj.Vbase105;
            kW_out := OtherStorageObj.kW_out;
            kvar_out := OtherStorageObj.kvar_out;
            Pnominalperphase := OtherStorageObj.Pnominalperphase;
            PFNominal := OtherStorageObj.PFNominal;
            Qnominalperphase := OtherStorageObj.Qnominalperphase;
            Connection := OtherStorageObj.Connection;
            YearlyShape := OtherStorageObj.YearlyShape;
            YearlyShapeObj := OtherStorageObj.YearlyShapeObj;
            DailyShape := OtherStorageObj.DailyShape;
            DailyShapeObj := OtherStorageObj.DailyShapeObj;
            DutyShape := OtherStorageObj.DutyShape;
            DutyShapeObj := OtherStorageObj.DutyShapeObj;
            DispatchMode := OtherStorageObj.DispatchMode;
            StorageClass := OtherStorageObj.StorageClass;
            VoltageModel := OtherStorageObj.VoltageModel;

            Fstate := OtherStorageObj.Fstate;
            FstateChanged := OtherStorageObj.FstateChanged;
            kVANotSet := OtherStorageObj.kVANotSet;

            StorageVars.kVArating := OtherStorageObj.StorageVars.kVArating;
            StorageVars.kWRating := OtherStorageObj.StorageVars.kWRating;
            StorageVars.kWhRating := OtherStorageObj.StorageVars.kWhRating;
            StorageVars.kWhStored := OtherStorageObj.StorageVars.kWhStored;
            StorageVars.kWhReserve := OtherStorageObj.StorageVars.kWhReserve;
            kWhBeforeUpdate := OtherStorageObj.kWhBeforeUpdate;
            pctReserve := OtherStorageObj.pctReserve;
            DischargeTrigger := OtherStorageObj.DischargeTrigger;
            ChargeTrigger := OtherStorageObj.ChargeTrigger;
            pctChargeEff := OtherStorageObj.pctChargeEff;
            pctDischargeEff := OtherStorageObj.pctDischargeEff;
            pctkWout := OtherStorageObj.pctkWout;
            pctkWin := OtherStorageObj.pctkWin;
            pctIdlekW := OtherStorageObj.pctIdlekW;
            pctIdlekvar := OtherStorageObj.pctIdlekvar;
            ChargeTime := OtherStorageObj.ChargeTime;

            pctR := OtherStorageObj.pctR;
            pctX := OtherStorageObj.pctX;

            RandomMult := OtherStorageObj.RandomMult;

            UserModel.Name := OtherStorageObj.UserModel.Name;  // Connect to user written models
            DynaModel.Name := OtherStorageObj.DynaModel.Name;
            IsUserModel := OtherStorageObj.IsUserModel;
            ForceBalanced := OtherStorageObj.ForceBalanced;
            CurrentLimited := OtherStorageObj.CurrentLimited;

            ClassMakeLike(OtherStorageObj);

            for i := 1 to ParentClass.NumProperties do
                FPropertyValue^[i] := OtherStorageObj.FPropertyValue^[i];

            Result := 1;
        end
    else
        DoSimpleMsg('Error in Storage MakeLike: "' + OtherStorageObjName + '" Not Found.', 562);

end;

{--------------------------------------------------------------------------}
procedure TStorage.ResetRegistersAll;  // Force all EnergyMeters in the circuit to reset

var
    idx: Integer;

begin
    idx := First;
    while idx > 0 do
    begin
        TStorageObj(GetActiveObj).ResetRegisters;
        idx := Next;
    end;
end;

{--------------------------------------------------------------------------}
procedure TStorage.SampleAll;  // Force all Storage elements in the circuit to take a sample

var
    i: Integer;
begin
    for i := 1 to ElementList.ListSize do
        with TStorageObj(ElementList.Get(i)) do
            if Enabled then
                TakeSample;
end;

//----------------------------------------------------------------------------
constructor TStorageObj.Create(ParClass: TDSSClass; const SourceName: String);
begin

    inherited create(ParClass);
    Name := LowerCase(SourceName);
    DSSObjType := ParClass.DSSClassType; // + STORAGE_ELEMENT;  // In both PCelement and Storageelement list
    TraceFile := nil;

    Nphases := 3;
    Fnconds := 4;  // defaults to wye
    Yorder := 0;  // To trigger an initial allocation
    Nterms := 1;  // forces allocations

    YearlyShape := '';
    YearlyShapeObj := NIL;  // If YearlyShapeobj = nil Then the load alway stays nominal * global multipliers
    DailyShape := '';
    DailyShapeObj := NIL;  // If DaillyShapeobj = nil Then the load alway stays nominal * global multipliers
    DutyShape := '';
    DutyShapeObj := NIL;  // If DutyShapeobj = nil Then the load alway stays nominal * global multipliers
    Connection := 0;    // Wye (star)
    VoltageModel := 1;  {Typical fixed kW negative load}
    StorageClass := 1;

    StorageSolutionCount := -1;  // For keep track of the present solution in Injcurrent calcs
    OpenStorageSolutionCount := -1;
    YPrimOpenCond := NIL;

    StorageVars.kVStorageBase := 12.47;
    VBase := 7200.0;
    Vminpu := 0.90;
    Vmaxpu := 1.10;
    VBase95 := Vminpu * Vbase;
    VBase105 := Vmaxpu * Vbase;
    Yorder := Fnterms * Fnconds;
    RandomMult := 1.0;

      {Output rating stuff}
    kW_out := 25.0;
    kvar_out := 0.0;
    kvarBase := kvar_out;     // initialize
    PFNominal := 1.0;
    with StorageVars do
    begin
        kWRating := 25.0;
        kVArating := kWRating * 1.0;
        kWhRating := 50;
        kWhStored := kWhRating;
        kWhBeforeUpdate := kWhRating;
        kWhReserve := kWhRating * pctReserve / 100.0;
    end;

    FState := STORE_IDLING;  // Idling and fully charged
    FStateChanged := TRUE;  // Force building of YPrim
    pctReserve := 20.0;  // per cent of kWhRating
    pctR := 0.0;
    ;
    pctX := 50.0;
    pctIdlekW := 1.0;
    pctIdlekvar := 0.0;

    DischargeTrigger := 0.0;
    ChargeTrigger := 0.0;
    pctChargeEff := 90.0;
    pctDischargeEff := 90.0;
    FpctkWout := 100.0;
    Fpctkvarout := 100.0;
    pctkWin := 100.0;

    ChargeTime := 2.0;   // 2 AM

    kVANotSet := TRUE;  // Flag to set the default value for kVA

     {Make the StorageVars struct as public}
    PublicDataStruct := @StorageVars;
    PublicDataSize := SizeOf(TStorageVars);

    IsUserModel := FALSE;
    UserModel := TStoreUserModel.Create;
    DynaModel := TStoreDynaModel.Create;

    Reg_kWh := 1;
    Reg_kvarh := 2;
    Reg_MaxkW := 3;
    Reg_MaxkVA := 4;
    Reg_Hours := 5;
    Reg_Price := 6;

    DebugTrace := FALSE;
    StorageObjSwitchOpen := FALSE;
    Spectrum := '';  // override base class
    SpectrumObj := NIL;

    ForceBalanced := FALSE;
    CurrentLimited := FALSE;

    InitPropertyValues(0);
    RecalcElementData;

end;


//----------------------------------------------------------------------------
function TStorageObj.DecodeState: String;
begin
    case Fstate of
        STORE_CHARGING:
            Result := 'CHARGING';
        STORE_DISCHARGING:
            Result := 'DISCHARGING';
    else
        Result := 'IDLING';
    end;
end;

//----------------------------------------------------------------------------
procedure TStorageObj.InitPropertyValues(ArrayOffset: Integer);

// Define default values for the properties

begin

    PropertyValue[1] := '3';         //'phases';
    PropertyValue[2] := Getbus(1);   //'bus1';

    PropertyValue[propKV] := Format('%-g', [StorageVars.kVStorageBase]);
    PropertyValue[propKW] := Format('%-g', [kW_out]);
    PropertyValue[propPF] := Format('%-g', [PFNominal]);
    PropertyValue[propMODEL] := '1';
    PropertyValue[propYEARLY] := '';
    PropertyValue[propDAILY] := '';
    PropertyValue[propDUTY] := '';
    PropertyValue[propDISPMODE] := 'Default';
    PropertyValue[propIDLEKVAR] := '0';
    PropertyValue[propCONNECTION] := 'wye';
    PropertyValue[propKVAR] := Format('%-g', [Presentkvar]);

    PropertyValue[propPCTR] := Format('%-g', [pctR]);
    PropertyValue[propPCTX] := Format('%-g', [pctX]);

    PropertyValue[propIDLEKW] := '1';       // PERCENT
    PropertyValue[propCLASS] := '1'; //'class'
    PropertyValue[propDISPOUTTRIG] := '0';   // 0 MEANS NO TRIGGER LEVEL
    PropertyValue[propDISPINTRIG] := '0';
    PropertyValue[propCHARGEEFF] := '90';
    PropertyValue[propDISCHARGEEFF] := '90';
    PropertyValue[propPCTKWOUT] := '100';
    PropertyValue[propPCTKWIN] := '100';

    PropertyValue[propVMINPU] := '0.90';
    PropertyValue[propVMAXPU] := '1.10';
    PropertyValue[propSTATE] := 'IDLING';

    with StorageVars do
    begin
        PropertyValue[propKVA] := Format('%-g', [StorageVars.kVARating]);
        PropertyValue[propKWRATED] := Format('%-g', [kWRating]);
        PropertyValue[propKWHRATED] := Format('%-g', [kWhRating]);
        PropertyValue[propKWHSTORED] := Format('%-g', [kWhStored]);
        PropertyValue[propPCTSTORED] := Format('%-g', [kWhStored / kWhRating * 100.0]);
    end;

    PropertyValue[propPCTRESERVE] := Format('%-g', [pctReserve]);
    PropertyValue[propCHARGETIME] := Format('%-g', [ChargeTime]);

    PropertyValue[propUSERMODEL] := '';  // Usermodel
    PropertyValue[propUSERDATA] := '';  // Userdata
    PropertyValue[propDYNADLL] := '';  //
    PropertyValue[propDYNADATA] := '';  //
    PropertyValue[propDEBUGTRACE] := 'NO';
    PropertyValue[propBalanced] := 'NO';
    PropertyValue[propLimited] := 'NO';

    inherited  InitPropertyValues(NumPropsThisClass);

end;


//----------------------------------------------------------------------------
function TStorageObj.GetPropertyValue(Index: Integer): String;


begin

    Result := '';
    with StorageVars do
        case Index of
            propKV:
                Result := Format('%.6g', [StorageVars.kVStorageBase]);
            propKW:
                Result := Format('%.6g', [kW_out]);
            propPF:
                Result := Format('%.6g', [PFNominal]);
            propMODEL:
                Result := Format('%d', [VoltageModel]);
            propYEARLY:
                Result := YearlyShape;
            propDAILY:
                Result := DailyShape;
            propDUTY:
                Result := DutyShape;
            propDISPMODE:
                Result := ReturnDispMode(DispatchMode);
            propIDLEKVAR:
                Result := Format('%.6g', [pctIdlekvar]);
          {propCONNECTION :;}
            propKVAR:
                Result := Format('%.6g', [kvar_out]);
            propPCTR:
                Result := Format('%.6g', [pctR]);
            propPCTX:
                Result := Format('%.6g', [pctX]);
            propIDLEKW:
                Result := Format('%.6g', [pctIdlekW]);
          {propCLASS      = 17;}
            propDISPOUTTRIG:
                Result := Format('%.6g', [DischargeTrigger]);
            propDISPINTRIG:
                Result := Format('%.6g', [ChargeTrigger]);
            propCHARGEEFF:
                Result := Format('%.6g', [pctChargeEff]);
            propDISCHARGEEFF:
                Result := Format('%.6g', [pctDischargeEff]);
            propPCTKWOUT:
                Result := Format('%.6g', [pctkWout]);
            propVMINPU:
                Result := Format('%.6g', [VMinPu]);
            propVMAXPU:
                Result := Format('%.6g', [VMaxPu]);
            propSTATE:
                Result := DecodeState;
          {StorageVars}
            propKVA:
                Result := Format('%.6g', [kVArating]);
            propKWRATED:
                Result := Format('%.6g', [kWrating]);
            propKWHRATED:
                Result := Format('%.6g', [kWhrating]);
            propKWHSTORED:
                Result := Format('%.6g', [kWHStored]);
            propPCTRESERVE:
                Result := Format('%.6g', [pctReserve]);
            propUSERMODEL:
                Result := UserModel.Name;
            propUSERDATA:
                Result := '(' + inherited GetPropertyValue(index) + ')';
            propDynaDLL:
                Result := DynaModel.Name;
            propdynaDATA:
                Result := '(' + inherited GetPropertyValue(index) + ')';
          {propDEBUGTRACE = 33;}
            propPCTKWIN:
                Result := Format('%.6g', [pctkWin]);
            propPCTSTORED:
                Result := Format('%.6g', [kWhStored / kWhRating * 100.0]);
            propCHARGETIME:
                Result := Format('%.6g', [Chargetime]);
            propBalanced:
                if ForceBalanced then
                    Result := 'Yes'
                else
                    Result := 'No';
            propLimited:
                if CurrentLimited then
                    Result := 'Yes'
                else
                    Result := 'No';

        else  // take the generic handler
            Result := inherited GetPropertyValue(index);
        end;
end;

//----------------------------------------------------------------------------
destructor TStorageObj.Destroy;
begin
    YPrimOpenCond.Free;
    UserModel.Free;
    DynaModel.Free;
    FreeAndNil(TraceFile);
    inherited Destroy;
end;

//----------------------------------------------------------------------------
procedure TStorageObj.Randomize(Opt: Integer);
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
procedure TStorageObj.CalcDailyMult(Hr: Double);

begin
    if (DailyShapeObj <> NIL) then
    begin
        ShapeFactor := DailyShapeObj.GetMultAtHour(Hr);
    end
    else
        ShapeFactor := CDOUBLEONE;  // Default to no  variation

    CheckStateTriggerLevel(ShapeFactor.re);   // last recourse
end;


//----------------------------------------------------------------------------
procedure TStorageObj.CalcDutyMult(Hr: Double);

begin
    if DutyShapeObj <> NIL then
    begin
        ShapeFactor := DutyShapeObj.GetMultAtHour(Hr);
        CheckStateTriggerLevel(ShapeFactor.re);
    end
    else
        CalcDailyMult(Hr);  // Default to Daily Mult If no duty curve specified
end;

//----------------------------------------------------------------------------
procedure TStorageObj.CalcYearlyMult(Hr: Double);

begin
    if YearlyShapeObj <> NIL then
    begin
        ShapeFactor := YearlyShapeObj.GetMultAtHour(Hr);
        CheckStateTriggerLevel(ShapeFactor.re);
    end
    else
        CalcDailyMult(Hr);  // Defaults to Daily curve
end;

//----------------------------------------------------------------------------
procedure TStorageObj.SetKWandKvarOut;
var
    OldState: Integer;
begin
    OldState := Fstate;
    with StorageVars do
        case FState of

            STORE_CHARGING:
            begin
                if kWhStored < kWhRating then
                    case DispatchMode of
                        STORE_FOLLOW:
                        begin
                            kW_out := kWRating * ShapeFactor.re;
                            kvar_out := kvarBase * ShapeFactor.im;    // ???
                        end
                    else
                        kW_out := -kWRating * pctkWin / 100.0;
                        if PFNominal = 1.0 then
                            kvar_out := 0.0
                        else
                            SyncUpPowerQuantities;  // computes kvar_out from PF
                    end
                else
                    Fstate := STORE_IDLING;   // all charged up
            end;


            STORE_DISCHARGING:
            begin
                if kWhStored > kWhReserve then
                    case DispatchMode of
                        STORE_FOLLOW:
                        begin
                            kW_out := kWRating * ShapeFactor.re;
                            kvar_out := kvarBase * ShapeFactor.im;
                        end
                    else
                        kW_out := kWRating * pctkWout / 100.0;
                        if PFNominal = 1.0 then
                            kvar_out := 0.0
                        else
                            SyncUpPowerQuantities; // computes kvar_out from PF
                    end
                else
                    Fstate := STORE_IDLING;  // not enough storage to discharge
            end;

        end;

    {If idling output is only losses}

    if Fstate = STORE_IDLING then
    begin
        kW_out := 0.0;   // -kWIdlingLosses;     Just use YeqIdling
        kvar_out := 0.0;
    end;

    if OldState <> Fstate then
        FstateChanged := TRUE;

end;


//----------------------------------------------------------------------------
procedure TStorageObj.SetNominalStorageOuput;

begin

    ShapeFactor := CDOUBLEONE;  // init here; changed by curve routine
    // Check to make sure the Storage element is ON
    with ActiveCircuit, ActiveCircuit.Solution do
    begin
        if not (IsDynamicModel or IsHarmonicModel) then     // Leave Storage element in whatever state it was prior to entering Dynamic mode
        begin
          // Check dispatch to see what state the storage element should be in
            case DispatchMode of

                STORE_EXTERNALMODE: ;  // Do nothing
                STORE_LOADMODE:
                    CheckStateTriggerLevel(GeneratorDispatchReference);
                STORE_PRICEMODE:
                    CheckStateTriggerLevel(PriceSignal);

            else // dispatch off element's loadshapes, If any

                with Solution do
                    case Mode of
                        SNAPSHOT: ; {Just solve for the present kW, kvar}  // Don't check for state change
                        DAILYMODE:
                            CalcDailyMult(DynaVars.dblHour); // Daily dispatch curve
                        YEARLYMODE:
                            CalcYearlyMult(DynaVars.dblHour);
             (*
                MONTECARLO1,
                MONTEFAULT,
                FAULTSTUDY,
                DYNAMICMODE:   ; // {do nothing for these modes}
             *)
                        GENERALTIME:
                        begin
                         // This mode allows use of one class of load shape
                            case ActiveCircuit.ActiveLoadShapeClass of
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
                // Assume Daily curve, If any, for the following
                        MONTECARLO2,
                        MONTECARLO3,
                        LOADDURATION1,
                        LOADDURATION2:
                            CalcDailyMult(DynaVars.dblHour);
                        PEAKDAY:
                            CalcDailyMult(DynaVars.dblHour);

                        DUTYCYCLE:
                            CalcDutyMult(DynaVars.dblHour);
                {AUTOADDFLAG:  ; }
                    end;

            end;


            SetKWandKvarOut;   // Based on State and amount of energy left in storage

          {
           Pnominalperphase is net at the terminal.  When discharging, the storage supplies the idling losses.
           When charging, the idling losses are subtracting from the amount entering the storage element.
          }

            Pnominalperphase := 1000.0 * kW_out / Fnphases;

            if Fstate = STORE_IDLING then
            begin
                if DispatchMode = STORE_EXTERNALMODE then   // Check for requested kvar
                    Qnominalperphase := StorageVars.kvarRequested / Fnphases * 1000.0
                else
                    Qnominalperphase := 0.0;
                Yeq := CDivReal(Cmplx(Pnominalperphase, -Qnominalperphase), Sqr(Vbase));   // Vbase must be L-N for 3-phase
                Yeq95 := Yeq;
                Yeq105 := Yeq;
            end
            else
            begin

                Qnominalperphase := 1000.0 * kvar_out / Fnphases;

                case VoltageModel of
        //****  Fix this when user model gets connected in
                    3: // Yeq := Cinv(cmplx(0.0, -StoreVARs.Xd))  ;  // Gets negated in CalcYPrim
                else
                     {
                      Yeq no longer used for anything other than this calculation of Yeq95, Yeq105 and
                      constant Z power flow model
                     }
                    Yeq := CDivReal(Cmplx(Pnominalperphase, -Qnominalperphase), Sqr(Vbase));   // Vbase must be L-N for 3-phase
                    if (Vminpu <> 0.0) then
                        Yeq95 := CDivReal(Yeq, sqr(Vminpu))  // at 95% voltage
                    else
                        Yeq95 := Yeq; // Always a constant Z model

                    if (Vmaxpu <> 0.0) then
                        Yeq105 := CDivReal(Yeq, Sqr(Vmaxpu))   // at 105% voltage
                    else
                        Yeq105 := Yeq;
                end;
                  { Like Model 7 generator, max current is based on amount of current to get out requested power at min voltage
                }
                with StorageVars do
                begin
                    PhaseCurrentLimit := Cdivreal(Cmplx(Pnominalperphase, Qnominalperphase), VBase95);
                    MaxDynPhaseCurrent := Cabs(PhaseCurrentLimit);
                end;

            end;
              { When we leave here, all the Yeq's are in L-N values}

        end;  {If  NOT (IsDynamicModel or IsHarmonicModel)}
    end;  {With ActiveCircuit}

   // If Storage element state changes, force re-calc of Y matrix
    if FStateChanged then
    begin
        YPrimInvalid := TRUE;
        FStateChanged := FALSE;  // reset the flag
    end;

end;

//----------------------------------------------------------------------------
procedure TStorageObj.RecalcElementData;

begin

    VBase95 := VMinPu * VBase;
    VBase105 := VMaxPu * VBase;

   // removed 5/8/17 kvarBase := kvar_out ;  // remember this for Follow Mode

    // values in ohms for thevenin equivalents
    StorageVars.RThev := pctR * 0.01 * SQR(PresentkV) / StorageVars.kVARating * 1000.0;
    StorageVars.XThev := pctX * 0.01 * SQR(PresentkV) / StorageVars.kVARating * 1000.0;

    // efficiencies
    StorageVars.ChargeEff := pctChargeEff * 0.01;
    StorageVars.DisChargeEff := pctDisChargeEff * 0.01;

    YeqIdling := CmulReal(Cmplx(pctIdlekW, pctIdlekvar), (StorageVars.kWrating * 10.0 / SQR(vbase) / FNPhases));  // 10.0 = 1000/100 = kW->W/pct
    YeqDischarge := Cmplx((StorageVars.kWrating * 1000.0 / SQR(vbase) / FNPhases), 0.0);

    SetNominalStorageOuput;

    {Now check for errors.  If any of these came out nil and the string was not nil, give warning}
    if YearlyShapeObj = NIL then
        if Length(YearlyShape) > 0 then
            DoSimpleMsg('WARNING! Yearly load shape: "' + YearlyShape + '" Not Found.', 563);
    if DailyShapeObj = NIL then
        if Length(DailyShape) > 0 then
            DoSimpleMsg('WARNING! Daily load shape: "' + DailyShape + '" Not Found.', 564);
    if DutyShapeObj = NIL then
        if Length(DutyShape) > 0 then
            DoSimpleMsg('WARNING! Duty load shape: "' + DutyShape + '" Not Found.', 565);

    if Length(Spectrum) > 0 then
    begin
        SpectrumObj := SpectrumClass.Find(Spectrum);
        if SpectrumObj = NIL then
            DoSimpleMsg('ERROR! Spectrum "' + Spectrum + '" Not Found.', 566);
    end
    else
        SpectrumObj := NIL;

    // Initialize to Zero - defaults to PQ Storage element
    // Solution object will reset after circuit modifications

    Reallocmem(InjCurrent, SizeOf(InjCurrent^[1]) * Yorder);

    {Update any user-written models}
    if Usermodel.Exists then
        UserModel.FUpdateModel;  // Checks for existence and Selects
    if Dynamodel.Exists then
        Dynamodel.FUpdateModel;  // Checks for existence and Selects

end;

//----------------------------------------------------------------------------
procedure TStorageObj.CalcYPrimMatrix(Ymatrix: TcMatrix);

var
    Y, Yij: Complex;
    i, j: Integer;
    FreqMultiplier: Double;

begin

    FYprimFreq := ActiveCircuit.Solution.Frequency;
    FreqMultiplier := FYprimFreq / BaseFrequency;

    with  ActiveCircuit.solution do
        if {IsDynamicModel or} IsHarmonicModel then
        begin
       {Yeq is computed from %R and %X -- inverse of Rthev + j Xthev}
            case Fstate of
                STORE_CHARGING:
                    Y := Cadd(YeqDischarge, YeqIdling);
                STORE_IDLING:
                    Y := YeqIdling;
                STORE_DISCHARGING:
                    Y := Cadd(cnegate(YeqDischarge), YeqIdling);
               // old way Y  := Yeq   // L-N value computed in initialization routines
            end;

            if Connection = 1 then
                Y := CDivReal(Y, 3.0); // Convert to delta impedance
            Y.im := Y.im / FreqMultiplier;
            Yij := Cnegate(Y);
            for i := 1 to Fnphases do
            begin
                case Connection of
                    0:
                    begin
                        Ymatrix.SetElement(i, i, Y);
                        Ymatrix.AddElement(Fnconds, Fnconds, Y);
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
        begin  //  Regular power flow Storage element model

       {Yeq is always expected as the equivalent line-neutral admittance}


            case Fstate of
                STORE_CHARGING:
                    Y := Cadd(YeqDischarge, YeqIdling);
                STORE_IDLING:
                    Y := YeqIdling;
                STORE_DISCHARGING:
                    Y := Cadd(cnegate(YeqDischarge), YeqIdling);
            end;

       //---DEBUG--- WriteDLLDebugFile(Format('t=%.8g, Change To State=%s, Y=%.8g +j %.8g',[ActiveCircuit.Solution.dblHour, StateToStr, Y.re, Y.im]));

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
function TStorageObj.NormalizeToTOD(h: Integer; sec: Double): Double;
// Normalize time to a floating point number representing time of day If Hour > 24
// time should be 0 to 24.
var
    HourOfDay: Integer;

begin

    if h > 23 then
        HourOfDay := (h - (h div 24) * 24)
    else
        HourOfDay := h;

    Result := HourOfDay + sec / 3600.0;

    if Result > 24.0 then
        Result := Result - 24.0;   // Wrap around

end;

//----------------------------------------------------------------------------
procedure TStorageObj.CheckStateTriggerLevel(Level: Double);
{This is where we set the state of the Storage element}

var
    OldState: Integer;

begin
    FStateChanged := FALSE;

    OldState := Fstate;

    with StorageVars do
        if DispatchMode = STORE_FOLLOW then
        begin

         // set charge and discharge modes based on sign of loadshape
            if (Level > 0.0) and (kWhStored > kWhReserve) then
                StorageState := STORE_DISCHARGING
            else
            if (Level < 0.0) and (kWhStored < kWhRating) then
                StorageState := STORE_CHARGING
            else
                StorageState := STORE_IDLING;

        end
        else
        begin   // All other dispatch modes  Just compare to trigger value

            if (ChargeTrigger = 0.0) and (DischargeTrigger = 0.0) then
                Exit;

      // First see If we want to turn off Charging or Discharging State
            case Fstate of
                STORE_CHARGING:
                    if (ChargeTrigger <> 0.0) then
                        if (ChargeTrigger < Level) or (kWhStored >= kWHRating) then
                            Fstate := STORE_IDLING;
                STORE_DISCHARGING:
                    if (DischargeTrigger <> 0.0) then
                        if (DischargeTrigger > Level) or (kWhStored <= kWHReserve) then
                            Fstate := STORE_IDLING;
            end;

      // Now check to see If we want to turn on the opposite state
            case Fstate of
                STORE_IDLING:
                begin
                    if (DischargeTrigger <> 0.0) and (DischargeTrigger < Level) and (kWhStored > kWHReserve) then
                        FState := STORE_DISCHARGING
                    else
                    if (ChargeTrigger <> 0.0) and (ChargeTrigger > Level) and (kWhStored < kWHRating) then
                        Fstate := STORE_CHARGING;

                               // Check to see If it is time to turn the charge cycle on If it is not already on.
                    if not (Fstate = STORE_CHARGING) then
                        if ChargeTime > 0.0 then
                            with ActiveCircuit.Solution do
                            begin
                                if abs(NormalizeToTOD(DynaVars.intHour, DynaVARs.t) - ChargeTime) < DynaVARs.h / 3600.0 then
                                    Fstate := STORE_CHARGING;
                            end;
                end;
            end;
        end;

    if OldState <> Fstate then
    begin
        FstateChanged := TRUE;
        YPrimInvalid := TRUE;
    end;
end;

//----------------------------------------------------------------------------
procedure TStorageObj.CalcYPrim;

var
    i: Integer;

begin

     // Build only shunt Yprim
     // Build a dummy Yprim Series so that CalcV Does not fail
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

    SetNominalStorageOuput;
    CalcYPrimMatrix(YPrim_Shunt);

     // Set YPrim_Series based on diagonals of YPrim_shunt  so that CalcVoltages Doesn't fail
    for i := 1 to Yorder do
        Yprim_Series.SetElement(i, i, CmulReal(Yprim_Shunt.Getelement(i, i), 1.0e-10));

    YPrim.CopyFrom(YPrim_Shunt);

     // Account for Open Conductors
    inherited CalcYPrim;

end;

// - - - - - - - - - - - - - - - - - - - - - - - -- - - - - - - - - - - - - - - - - -
procedure TStorageObj.StickCurrInTerminalArray(TermArray: pComplexArray; const Curr: Complex; i: Integer);
 {Add the current into the proper location according to connection}

 {Reverse of similar routine in load  (Cnegates are switched)}

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
procedure TStorageObj.WriteTraceRecord(const s: String);

var
    i: Integer;
    sout: String;
begin

    try
        if (not InshowResults) then
        begin
            WriteStr(sout, Format('%-.g, %d, %-.g, ',
                [ActiveCircuit.Solution.DynaVars.dblHour,
                ActiveCircuit.Solution.Iteration,
                ActiveCircuit.LoadMultiplier]),
                GetSolutionModeID, ', ',
                GetLoadModel, ', ',
                VoltageModel: 0, ', ',
                (Qnominalperphase * 3.0 / 1.0e6): 8: 2, ', ',
                (Pnominalperphase * 3.0 / 1.0e6): 8: 2, ', ',
                s, ', ');
            FSWrite(TraceFile, sout);
            
            for i := 1 to nphases do
            begin
                WriteStr(sout, (Cabs(InjCurrent^[i])): 8: 1, ', ');
                FSWrite(TraceFile, sout);
            end;
            for i := 1 to nphases do
            begin
                WriteStr(sout, (Cabs(ITerminal^[i])): 8: 1, ', ');
                FSWrite(TraceFile, sout);
            end;
            for i := 1 to nphases do
            begin
                WriteStr(sout, (Cabs(Vterminal^[i])): 8: 1, ', ');
                FSWrite(TraceFile, sout);
            end;
            for i := 1 to NumVariables do
                FSWrite(TraceFile, Format('%-.g, ', [Variable[i]]));

   //****        FSWrite(TraceFile,VThevMag:8:1 ,', ', StoreVARs.Theta*180.0/PI);
            FSWriteln(Tracefile);
            FSFlush(TraceFile);
        end;
    except
        On E: Exception do
        begin
        end;

    end;
end;
// - - - - - - - - - - - - - - - - - - - - - - - -- - - - - - - - - - - - - - - - - -
procedure TStorageObj.DoConstantPQStorageObj;

{Compute total terminal current for Constant PQ}

var
    i: Integer;
    Curr,
    VLN, VLL: Complex;
   //---DEBUG--- S:Complex;
    VmagLN,
    VmagLL: Double;
    V012: array[0..2] of Complex;  // Sequence voltages

begin
     //Treat this just like the Load model

    CalcYPrimContribution(InjCurrent);  // Init InjCurrent Array
    ZeroITerminal;

    //---DEBUG--- WriteDLLDebugFile(Format('t=%.8g, State=%s, Iyprim= %s', [ActiveCircuit.Solution.dblHour, StateToStr, CmplxArrayToString(InjCurrent, Yprim.Order) ]));

    case FState of
        STORE_IDLING:  // YPrim current is only current
        begin
            for i := 1 to FNPhases do
            begin
                Curr := InjCurrent^[i];
                StickCurrInTerminalArray(ITerminal, Curr, i);  // Put YPrim contribution into Terminal array taking into account connection
                IterminalUpdated := TRUE;
                StickCurrInTerminalArray(InjCurrent, Cnegate(Curr), i);    // Compensation current is zero since terminal current is same as Yprim contribution
                    //---DEBUG--- S := Cmul(Vterminal^[i] , Conjg(Iterminal^[i]));  // for debugging below
                    //---DEBUG--- WriteDLLDebugFile(Format('        Phase=%d, Pnom=%.8g +j %.8g',[i, S.re, S.im ]));
            end;
             //---DEBUG--- WriteDLLDebugFile(Format('        Icomp=%s ', [CmplxArrayToString(InjCurrent, Yprim.Order) ]));
        end;
    else   // For Charging and Discharging

        CalcVTerminalPhase; // get actual voltage across each phase of the load

        if ForceBalanced and (Fnphases = 3) then
        begin  // convert to pos-seq only
            Phase2SymComp(Vterminal, @V012);
            V012[0] := CZERO; // Force zero-sequence voltage to zero
            V012[2] := CZERO; // Force negative-sequence voltage to zero
            SymComp2Phase(Vterminal, @V012);  // Reconstitute Vterminal as balanced
        end;

        for i := 1 to Fnphases do
        begin

            case Connection of

                0:
                begin  {Wye}
                    VLN := Vterminal^[i];
                    VMagLN := Cabs(VLN);
                    if VMagLN <= VBase95 then
                        Curr := Cmul(Yeq95, VLN)  // Below 95% use an impedance model
                    else
                    if VMagLN > VBase105 then
                        Curr := Cmul(Yeq105, VLN)  // above 105% use an impedance model
                    else
                        Curr := Conjg(Cdiv(Cmplx(Pnominalperphase, Qnominalperphase), VLN));  // Between 95% -105%, constant PQ
                    if CurrentLimited then
                        if Cabs(Curr) > MaxDynPhaseCurrent then
                            Curr := Conjg(Cdiv(PhaseCurrentLimit, CDivReal(VLN, VMagLN)));
                end;

                1:
                begin  {Delta}
                    VLL := Vterminal^[i];
                    VMagLL := Cabs(VLL);
                    if Fnphases > 1 then
                        VMagLN := VMagLL / SQRT3
                    else
                        VMagLN := VmagLL;  // L-N magnitude
                    if VMagLN <= VBase95 then
                        Curr := Cmul(CdivReal(Yeq95, 3.0), VLL)  // Below 95% use an impedance model
                    else
                    if VMagLN > VBase105 then
                        Curr := Cmul(CdivReal(Yeq105, 3.0), VLL)  // above 105% use an impedance model
                    else
                        Curr := Conjg(Cdiv(Cmplx(Pnominalperphase, Qnominalperphase), VLL));  // Between 95% -105%, constant PQ
                    if CurrentLimited then
                        if Cabs(Curr) * SQRT3 > MaxDynPhaseCurrent then
                            Curr := Conjg(Cdiv(PhaseCurrentLimit, CDivReal(VLL, VMagLN))); // Note VmagLN has sqrt3 factor in it
                end;

            end;

         //---DEBUG--- WriteDLLDebugFile(Format('        Phase=%d, Pnom=%.8g +j %.8g', [i, Pnominalperphase, Qnominalperphase ]));

            StickCurrInTerminalArray(ITerminal, Cnegate(Curr), i);  // Put into Terminal array taking into account connection
            IterminalUpdated := TRUE;
            StickCurrInTerminalArray(InjCurrent, Curr, i);  // Put into Terminal array taking into account connection
        end;
        //---DEBUG--- WriteDLLDebugFile(Format('        Icomp=%s ', [CmplxArrayToString(InjCurrent, Yprim.Order) ]));
    end;

end;

// - - - - - - - - - - - - - - - - - - - - - - - -- - - - - - - - - - - - - - - - - -
procedure TStorageObj.DoConstantZStorageObj;

{constant Z model}
var
    i: Integer;
    Curr,
    Yeq2: Complex;
    V012: array[0..2] of Complex;  // Sequence voltages

begin

// Assume Yeq is kept up to date

    CalcYPrimContribution(InjCurrent);  // Init InjCurrent Array
    CalcVTerminalPhase; // get actual voltage across each phase of the load
    ZeroITerminal;
    if Connection = 0 then
        Yeq2 := Yeq
    else
        Yeq2 := CdivReal(Yeq, 3.0);

    if ForceBalanced and (Fnphases = 3) then
    begin  // convert to pos-seq only
        Phase2SymComp(Vterminal, @V012);
        V012[0] := CZERO; // Force zero-sequence voltage to zero
        V012[2] := CZERO; // Force negative-sequence voltage to zero
        SymComp2Phase(Vterminal, @V012);  // Reconstitute Vterminal as balanced
    end;

    for i := 1 to Fnphases do
    begin

        Curr := Cmul(Yeq2, Vterminal^[i]);   // Yeq is always line to neutral
        StickCurrInTerminalArray(ITerminal, Cnegate(Curr), i);  // Put into Terminal array taking into account connection
        IterminalUpdated := TRUE;
        StickCurrInTerminalArray(InjCurrent, Curr, i);  // Put into Terminal array taking into account connection

    end;

end;


// - - - - - - - - - - - - - - - - - - - - - - - -- - - - - - - - - - - - - - - - - -
procedure TStorageObj.DoUserModel;
{Compute total terminal Current from User-written model}
var
    i: Integer;

begin

    CalcYPrimContribution(InjCurrent);  // Init InjCurrent Array

    if UserModel.Exists then    // Check automatically selects the usermodel If true
    begin
        UserModel.FCalc(Vterminal, Iterminal);
        IterminalUpdated := TRUE;
        with ActiveCircuit.Solution do
        begin          // Negate currents from user model for power flow Storage element model
            for i := 1 to FnConds do
                Caccum(InjCurrent^[i], Cnegate(Iterminal^[i]));
        end;
    end
    else
    begin
        DoSimpleMsg('Storage.' + name + ' model designated to use user-written model, but user-written model is not defined.', 567);
    end;

end;


// - - - - - - - - - - - - - - - - - - - - - - - -- - - - - - - - - - - - - - - - - -
procedure TStorageObj.DoDynamicMode;

{Compute Total Current and add into InjTemp}
{
   For now, just assume the storage element Thevenin voltage is constant
   for the duration of the dynamic simulation.
}
{****}
var
    i: Integer;
    V012,
    I012: array[0..2] of Complex;


    procedure CalcVthev_Dyn;
    begin
        with StorageVars do
            Vthev := pclx(VthevMag, Theta);   // keeps theta constant
    end;

begin

{****}  // Test using DESS model
   // Compute Vterminal

    if DynaModel.Exists then
        DoDynaModel   // do user-written model

    else
    begin

        CalcYPrimContribution(InjCurrent);  // Init InjCurrent Array
        ZeroITerminal;

       // Simple Thevenin equivalent
       // compute terminal current (Iterminal) and take out the Yprim contribution

        with StorageVars do
            case Fnphases of
                1:
                begin
                    CalcVthev_Dyn;  // Update for latest phase angle
                    ITerminal^[1] := CDiv(CSub(Csub(VTerminal^[1], Vthev), VTerminal^[2]), Zthev);
                    if CurrentLimited then
                        if Cabs(Iterminal^[1]) > MaxDynPhaseCurrent then   // Limit the current but keep phase angle
                            ITerminal^[1] := ptocomplex(topolar(MaxDynPhaseCurrent, cang(Iterminal^[1])));
                    ITerminal^[2] := Cnegate(ITerminal^[1]);
                end;
                3:
                begin
                    Phase2SymComp(Vterminal, @V012);

                  // Positive Sequence Contribution to Iterminal
                    CalcVthev_Dyn;  // Update for latest phase angle

                  // Positive Sequence Contribution to Iterminal
                    I012[1] := CDiv(Csub(V012[1], Vthev), Zthev);

                    if CurrentLimited and (Cabs(I012[1]) > MaxDynPhaseCurrent) then   // Limit the pos seq current but keep phase angle
                        I012[1] := ptocomplex(topolar(MaxDynPhaseCurrent, cang(I012[1])));

                    if ForceBalanced then
                    begin
                        I012[2] := CZERO;
                    end
                    else
                        I012[2] := Cdiv(V012[2], Zthev);  // for inverter

                    I012[0] := CZERO;

                    SymComp2Phase(ITerminal, @I012);  // Convert back to phase components

                end;
            else
                DoSimpleMsg(Format('Dynamics mode is implemented only for 1- or 3-phase Storage Element. Storage.%s has %d phases.', [name, Fnphases]), 5671);
                SolutionAbort := TRUE;
            end;

    {Add it into inj current array}
        for i := 1 to FnConds do
            Caccum(InjCurrent^[i], Cnegate(Iterminal^[i]));

    end;

end;


procedure TStorageObj.DoDynaModel;
var
    DESSCurr: array[1..6] of Complex;  // Temporary biffer
    i: Integer;

begin
// do user written dynamics model

    with ActiveCircuit.Solution do
    begin  // Just pass node voltages to ground and let dynamic model take care of it
        for i := 1 to FNconds do
            VTerminal^[i] := NodeV^[NodeRef^[i]];
        StorageVars.w_grid := TwoPi * Frequency;
    end;

    DynaModel.FCalc(Vterminal, @DESSCurr);

    CalcYPrimContribution(InjCurrent);  // Init InjCurrent Array
    ZeroITerminal;

    for i := 1 to Fnphases do
    begin
        StickCurrInTerminalArray(ITerminal, Cnegate(DESSCurr[i]), i);  // Put into Terminal array taking into account connection
        IterminalUpdated := TRUE;
        StickCurrInTerminalArray(InjCurrent, DESSCurr[i], i);  // Put into Terminal array taking into account connection
    end;

end;

// - - - - - - - - - - - - - - - - - - - - - - - -- - - - - - - - - - - - - - - - - -
procedure TStorageObj.DoHarmonicMode;

{Compute Injection Current Only when in harmonics mode}

{Assumes spectrum is a voltage source behind subtransient reactance and YPrim has been built}
{Vd is the fundamental frequency voltage behind Xd" for phase 1}

var
    i: Integer;
    E: Complex;
    StorageHarmonic: Double;

begin

    ComputeVterminal;

    with ActiveCircuit.Solution do
    begin
        StorageHarmonic := Frequency / StorageFundamental;
        if SpectrumObj <> NIL then
            E := CmulReal(SpectrumObj.GetMult(StorageHarmonic), StorageVars.VThevHarm) // Get base harmonic magnitude
        else
            E := CZERO;

        RotatePhasorRad(E, StorageHarmonic, StorageVars.ThetaHarm);  // Time shift by fundamental frequency phase shift
        for i := 1 to Fnphases do
        begin
            cBuffer[i] := E;
            if i < Fnphases then
                RotatePhasorDeg(E, StorageHarmonic, -120.0);  // Assume 3-phase Storage element
        end;
    end;

   {Handle Wye Connection}
    if Connection = 0 then
        cbuffer[Fnconds] := Vterminal^[Fnconds];  // assume no neutral injection voltage

   {Inj currents = Yprim (E) }
    YPrim.MVMult(InjCurrent, @cBuffer);

end;


// - - - - - - - - - - - - - - - - - - - - - - - -- - - - - - - - - - - - - - - - - -
procedure TStorageObj.CalcVTerminalPhase;

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

    StorageSolutionCount := ActiveCircuit.Solution.SolutionCount;

end;


// - - - - - - - - - - - - - - - - - - - - - - - -- - - - - - - - - - - - - - - - - -
(*
PROCEDURE TStorageObj.CalcVTerminal;
{Put terminal voltages in an array}
Begin
   ComputeVTerminal;
   StorageSolutionCount := ActiveCircuit.Solution.SolutionCount;
End;
*)


// - - - - - - - - - - - - - - - - - - - - - - - -- - - - - - - - - - - - - - - - - -
procedure TStorageObj.CalcStorageModelContribution;

// Calculates Storage element current and adds it properly into the injcurrent array
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
            case VoltageModel of
                1:
                    DoConstantPQStorageObj;
                2:
                    DoConstantZStorageObj;
                3:
                    DoUserModel;
            else
                DoConstantPQStorageObj;  // for now, until we implement the other models.
            end;
        end; {ELSE}
    end; {WITH}

   {When this is Done, ITerminal is up to date}

end;

// - - - - - - - - - - - - - - - - - - - - - - - -- - - - - - - - - - - - - - - - - -
procedure TStorageObj.CalcInjCurrentArray;
// Difference between currents in YPrim and total current
begin
      // Now Get Injection Currents
    if StorageObjSwitchOpen then
        ZeroInjCurrent
    else
        CalcStorageModelContribution;
end;

// - - - - - - - - - - - - - - - - - - - - - - - -- - - - - - - - - - - - - - - - - -
procedure TStorageObj.GetTerminalCurrents(Curr: pComplexArray);

// Compute total Currents

begin
    with ActiveCircuit.Solution do
    begin
        if IterminalSolutionCount <> ActiveCircuit.Solution.SolutionCount then
        begin     // recalc the contribution
            if not StorageObjSwitchOpen then
                CalcStorageModelContribution;  // Adds totals in Iterminal as a side effect
        end;
        inherited GetTerminalCurrents(Curr);
    end;

    if (DebugTrace) then
        WriteTraceRecord('TotalCurrent');

end;

// - - - - - - - - - - - - - - - - - - - - - - - -- - - - - - - - - - - - - - - - - -
function TStorageObj.InjCurrents: Integer;

begin
    with ActiveCircuit.Solution do
    begin
        if LoadsNeedUpdating then
            SetNominalStorageOuput; // Set the nominal kW, etc for the type of solution being Done

        CalcInjCurrentArray;          // Difference between currents in YPrim and total terminal current

        if (DebugTrace) then
            WriteTraceRecord('Injection');

         // Add into System Injection Current Array

        Result := inherited InjCurrents;
    end;
end;

// - - - - - - - - - - - - - - - - - - - - - - - -- - - - - - - - - - - - - - - - - -
procedure TStorageObj.GetInjCurrents(Curr: pComplexArray);

// Gives the currents for the last solution performed

// Do not call SetNominalLoad, as that may change the load values

var
    i: Integer;

begin

    CalcInjCurrentArray;  // Difference between currents in YPrim and total current

    try
   // Copy into buffer array
        for i := 1 to Yorder do
            Curr^[i] := InjCurrent^[i];

    except
        ON E: Exception do
            DoErrorMsg('Storage Object: "' + Name + '" in GetInjCurrents FUNCTION.',
                E.Message,
                'Current buffer not big enough.', 568);
    end;

end;


//- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
procedure TStorageObj.ResetRegisters;

var
    i: Integer;

begin
    for i := 1 to NumStorageRegisters do
        Registers[i] := 0.0;
    for i := 1 to NumStorageRegisters do
        Derivatives[i] := 0.0;
    FirstSampleAfterReset := TRUE;  // initialize for trapezoidal integration
end;

//- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
procedure TStorageObj.Integrate(Reg: Integer; const Deriv: Double; const Interval: Double);

begin
    if ActiveCircuit.TrapezoidalIntegration then
    begin
        {Trapezoidal Rule Integration}
        if not FirstSampleAfterReset then
            Registers[Reg] := Registers[Reg] + 0.5 * Interval * (Deriv + Derivatives[Reg]);
    end
    else   {Plain Euler integration}
        Registers[Reg] := Registers[Reg] + Interval * Deriv;

    Derivatives[Reg] := Deriv;
end;

//- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
procedure TStorageObj.TakeSample;
// Update Energy from metered zone

var
    S: Complex;
    Smag: Double;
    HourValue: Double;

begin

// Compute energy in Storage element branch
    if Enabled then
    begin

     // Only tabulate discharge hours
        if FSTate = STORE_DISCHARGING then
        begin
            S := cmplx(Get_PresentkW, Get_Presentkvar);
            Smag := Cabs(S);
            HourValue := 1.0;
        end
        else
        begin
            S := CZERO;
            Smag := 0.0;
            HourValue := 0.0;
        end;

        if (FState = STORE_DISCHARGING) or ActiveCircuit.TrapezoidalIntegration then
        {Make sure we always integrate for Trapezoidal case
         Don't need to for Gen Off and normal integration}
            with ActiveCircuit.Solution do
            begin
                if ActiveCircuit.PositiveSequence then
                begin
                    S := CmulReal(S, 3.0);
                    Smag := 3.0 * Smag;
                end;
                Integrate(Reg_kWh, S.re, IntervalHrs);   // Accumulate the power
                Integrate(Reg_kvarh, S.im, IntervalHrs);
                SetDragHandRegister(Reg_MaxkW, abs(S.re));
                SetDragHandRegister(Reg_MaxkVA, Smag);
                Integrate(Reg_Hours, HourValue, IntervalHrs);  // Accumulate Hours in operation
                Integrate(Reg_Price, S.re * ActiveCircuit.PriceSignal * 0.001, IntervalHrs);  // Accumulate Hours in operation
                FirstSampleAfterReset := FALSE;
            end;
    end;
end;

//----------------------------------------------------------------------------
procedure TStorageObj.UpdateStorage;
{Update Storage levels}
begin

    with StorageVars do
    begin

        kWhBeforeUpdate := kWhStored;   // keep this for reporting change in storage as a variable

    {Assume User model will take care of updating storage in dynamics mode}
        if ActiveCircuit.solution.IsDynamicModel and IsUserModel then
            Exit;


        with ActiveCircuit.Solution do
            case FState of

                STORE_DISCHARGING:
                begin
                               {Deplete storage by amount of Idling Power to achieve Present kW output}
                    kWhStored := kWhStored - (PresentkW + kWIdlingLosses) * IntervalHrs / DischargeEff;
                    if kWhStored < kWhReserve then
                    begin
                        kWhStored := kWhReserve;
                        Fstate := STORE_IDLING;  // It's empty Turn it off
                        FstateChanged := TRUE;
                    end;
                end;

                STORE_CHARGING:
                begin
                              {kWIdlingLosses is always positive while PresentkW is negative for Charging}
                    kWhStored := kWhStored - (PresentkW + kWIdlingLosses) * IntervalHrs * ChargeEff;
                    if kWhStored > kWhRating then
                    begin
                        kWhStored := kWhRating;
                        Fstate := STORE_IDLING;  // It's full Turn it off
                        FstateChanged := TRUE;
                    end;
                end;
            end;

    end;

    // the update is done at the end of a time step so have to force
    // a recalc of the Yprim for the next time step.  Else it will stay the same.
    if FstateChanged then
        YPrimInvalid := TRUE;

end;

// - - - - - - - - - - - - - - - - - - - - - - - -- - - - - - - - - - - - - - - - - -
function TStorageObj.Get_PresentkW: Double;
begin
    Result := kW_Out;  //Pnominalperphase * 0.001 * Fnphases;
end;

// - - - - - - - - - - - - - - - - - - - - - - - -- - - - - - - - - - - - - - - - - -
function TStorageObj.Get_kWTotalLosses: Double;
begin
    Result := 0.0;
    case StorageState of
        STORE_CHARGING:
            Result := abs(Power[1].re * (100.0 - pctChargeEff) / 100000.0) + pctChargeEff * kWIdlingLosses / 100.0; // kW
        STORE_IDLING:
            Result := kWIdlingLosses;
        STORE_DISCHARGING:
            Result := abs(Power[1].re * (100.0 / pctDisChargeEff - 1.0) / 1000.0) + (100.0 / pctDisChargeEff) * kWIdlingLosses;  // kW
    end;
end;

function TStorageObj.Get_kWIdlingLosses: Double;
var
    i: Integer;
begin
    ComputeVterminal;

    Result := 0.0;
      // Compute sum of SQR(V) at this device -- sum of VV*
    for i := 1 to FNphases do
        Result := Result + Cmul(Vterminal^[i], Conjg(VTerminal^[i])).re;

    Result := Result * YeqIdling.re * 0.001;  // to kW

end;

function TStorageObj.Get_PresentkV: Double;
begin
    Result := StorageVars.kVStorageBase;
end;

function TStorageObj.Get_Presentkvar: Double;
begin
    Result := kvar_out;   // Qnominalperphase * 0.001 * Fnphases;
end;


// - - - - - - - - - - - - - - - - - - - - - - - -- - - - - - - - - - - - - - - - - -
procedure TStorageObj.DumpProperties(F: TFileStream; Complete: Boolean);

var
    i, idx: Integer;

begin
    inherited DumpProperties(F, Complete);

    with ParentClass do
        for i := 1 to NumProperties do
        begin
            idx := PropertyIdxMap[i];
            case idx of
                propUSERDATA:
                    FSWriteln(F, '~ ' + PropertyName^[i] + '=(' + PropertyValue[idx] + ')');
                propDynaData:
                    FSWriteln(F, '~ ' + PropertyName^[i] + '=(' + PropertyValue[idx] + ')');
            else
                FSWriteln(F, '~ ' + PropertyName^[i] + '=' + PropertyValue[idx]);
            end;
        end;

    FSWriteln(F);
end;


//----------------------------------------------------------------------------
procedure TStorageObj.InitHarmonics;

// This routine makes a thevenin equivalent behis the reactance spec'd in %R and %X

var
    E, Va: complex;

begin
    YPrimInvalid := TRUE;  // Force rebuild of YPrims
    StorageFundamental := ActiveCircuit.Solution.Frequency;  // Whatever the frequency is when we enter here.

    Yeq := Cinv(Cmplx(StorageVars.RThev, StorageVars.XThev));      // used for current calcs  Always L-N

     {Compute reference Thevinen voltage from phase 1 current}

    if FState = STORE_DISCHARGING then
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

        E := Csub(Va, Cmul(Iterminal^[1], cmplx(StorageVars.Rthev, StorageVars.Xthev)));
        StorageVars.Vthevharm := Cabs(E);   // establish base mag and angle
        StorageVars.ThetaHarm := Cang(E);
    end
    else
    begin
        StorageVars.Vthevharm := 0.0;
        StorageVars.ThetaHarm := 0.0;
    end;
end;


//----------------------------------------------------------------------------
procedure TStorageObj.InitStateVars;

// for going into dynamics mode
var
    VNeut: Complex;
    VThevPolar: Polar;
    i: Integer;
    V012,
    I012: array[0..2] of Complex;
    Vabc: array[1..3] of Complex;


begin

    YPrimInvalid := TRUE;  // Force rebuild of YPrims

    with StorageVars do
    begin
        ZThev := Cmplx(RThev, XThev);
        Yeq := Cinv(ZThev);  // used to init state vars
    end;


    if DynaModel.Exists then   // Checks existence and selects
    begin
        ComputeIterminal;
        ComputeVterminal;
        with StorageVars do
        begin
            NumPhases := Fnphases;
            NumConductors := Fnconds;
            w_grid := twopi * ActiveCircuit.Solution.Frequency;
        end;
        DynaModel.FInit(Vterminal, Iterminal);
    end

    else
    begin

     {Compute nominal Positive sequence voltage behind equivalent filter impedance}

        if FState = STORE_DISCHARGING then
            with ActiveCircuit.Solution do
            begin
                ComputeIterminal;

                if FnPhases = 3 then
                begin
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
                    with StorageVars do
                    begin
                        Vthev := Csub(V012[1], Cmul(I012[1], ZThev));    // Pos sequence
                        VThevPolar := cToPolar(VThev);
                        VThevMag := VThevPolar.mag;
                        Theta := VThevPolar.ang;  // Initial phase angle
                    end;
                end
                else
                begin   // Single-phase Element
                    for i := 1 to Fnconds do
                        Vabc[i] := NodeV^[NodeRef^[i]];
                    with StorageVars do
                    begin
                        Vthev := Csub(VDiff(NodeRef^[1], NodeRef^[2]), Cmul(ITerminal^[1], ZThev));    // Pos sequence
                        VThevPolar := cToPolar(VThev);
                        VThevMag := VThevPolar.mag;
                        Theta := VThevPolar.ang;  // Initial phase angle
                    end;

                end;
            end;
    end;

end;

//----------------------------------------------------------------------------
procedure TStorageObj.IntegrateStates;

// dynamics mode integration routine

var
    TracePower: Complex;

begin
   // Compute Derivatives and Then integrate

    ComputeIterminal;

    if Dynamodel.Exists then   // Checks for existence and Selects

        DynaModel.Integrate

    else

        with ActiveCircuit.Solution, StorageVars do
        begin

            with StorageVars do
                if (Dynavars.IterationFlag = 0) then
                begin {First iteration of new time step}
//****          ThetaHistory := Theta + 0.5*h*dTheta;
//****          SpeedHistory := Speed + 0.5*h*dSpeed;
                end;

      // Compute shaft dynamics
            TracePower := TerminalPowerIn(Vterminal, Iterminal, FnPhases);

//****      dSpeed := (Pshaft + TracePower.re - D*Speed) / Mmass;
//      dSpeed := (Torque + TerminalPowerIn(Vtemp,Itemp,FnPhases).re/Speed) / (Mmass);
//****      dTheta  := Speed ;

     // Trapezoidal method
            with StorageVars do
            begin
//****       Speed := SpeedHistory + 0.5*h*dSpeed;
//****       Theta := ThetaHistory + 0.5*h*dTheta;
            end;

   // Write Dynamics Trace Record
            if DebugTrace then
            begin
                FSWrite(TraceFile, Format('t=%-.5g ', [Dynavars.t]));
                FSWrite(TraceFile, Format(' Flag=%d ', [Dynavars.Iterationflag]));
                FSWriteln(TraceFile);
                FSFlush(TraceFile);
            end;

        end;

end;

//----------------------------------------------------------------------------
function TStorageObj.InterpretState(const S: String): Integer;
begin
    case LowerCase(S)[1] of
        'c':
            Result := STORE_CHARGING;
        'd':
            Result := STORE_DISCHARGING;
    else
        Result := STORE_IDLING;
    end;
end;

{ apparently for debugging only
//----------------------------------------------------------------------------
Function TStorageObj.StateToStr:String;
Begin
      CASE FState of
          STORE_CHARGING: Result := 'Charging';
          STORE_IDLING: Result := 'Idling';
          STORE_DISCHARGING: Result := 'Discharging';
      END;
End;
}

//----------------------------------------------------------------------------
function TStorageObj.Get_Variable(i: Integer): Double;
{Return variables one at a time}

var
    N, k: Integer;

begin
    Result := -9999.99;  // error return value; no state fars
    if i < 1 then
        Exit;
// for now, report kWhstored and mode
    with StorageVars do
        case i of
            1:
                Result := kWhStored;
            2:
                Result := FState;
            3:
                if not (FState = STORE_DISCHARGING) then
                    Result := 0.0
                else
                    Result := Power[1].re * 0.001; // kW_Out; // pctkWout;
            4:
                if not (FState = STORE_CHARGING) then
                    Result := 0.0
                else
                    Result := Power[1].re * 0.001; // kW_out; // pctkWin;
            5:
                Result := kWTotalLosses; {Present kW charge or discharge loss incl idle losses}
            6:
                Result := kWIdlingLosses; {Present Idling Loss}
            7:
                Result := kWhStored - kWhBeforeUpdate;
        else
        begin
            if UserModel.Exists then   // Checks for existence and Selects
            begin
                N := UserModel.FNumVars;
                k := (i - NumStorageVariables);
                if k <= N then
                begin
                    Result := UserModel.FGetVariable(k);
                    Exit;
                end;
            end;
            if DynaModel.Exists then  // Checks for existence and Selects
            begin
                N := DynaModel.FNumVars;
                k := (i - NumStorageVariables);
                if k <= N then
                begin
                    Result := DynaModel.FGetVariable(k);
                    Exit;
                end;
            end;
        end;
        end;
end;

//----------------------------------------------------------------------------
procedure TStorageObj.Set_Variable(i: Integer; Value: Double);
var
    N, k: Integer;

begin
    if i < 1 then
        Exit;  // No variables to set

    with StorageVars do
        case i of
            1:
                kWhStored := Value;
            2:
                Fstate := Trunc(Value);
            3:
                pctkWout := Value;
            4:
                pctkWin := Value;
            5..7: ; {Do Nothing; read only}
        else
        begin
            if UserModel.Exists then    // Checks for existence and Selects
            begin
                N := UserModel.FNumVars;
                k := (i - NumStorageVariables);
                if k <= N then
                begin
                    UserModel.FSetVariable(k, Value);
                    Exit;
                end;
            end;
            if DynaModel.Exists then     // Checks for existence and Selects
            begin
                N := DynaModel.FNumVars;
                k := (i - NumStorageVariables);
                if k <= N then
                begin
                    DynaModel.FSetVariable(k, Value);
                    Exit;
                end;
            end;
        end;
        end;

end;

//----------------------------------------------------------------------------
procedure TStorageObj.GetAllVariables(States: pDoubleArray);

var
    i{, N}: Integer;
begin
    for i := 1 to NumStorageVariables do
        States^[i] := Variable[i];

    if UserModel.Exists then
    begin    // Checks for existence and Selects
        {N := UserModel.FNumVars;}
        UserModel.FGetAllVars(@States^[NumStorageVariables + 1]);
    end;
    if DynaModel.Exists then
    begin    // Checks for existence and Selects
        {N := UserModel.FNumVars;}
        DynaModel.FGetAllVars(@States^[NumStorageVariables + 1]);
    end;

end;

//----------------------------------------------------------------------------
function TStorageObj.NumVariables: Integer;
begin
    Result := NumStorageVariables;

     // Exists does a check and then does a Select
    if UserModel.Exists then
        Result := Result + UserModel.FNumVars;
    if DynaModel.Exists then
        Result := Result + DynaModel.FNumVars;
end;

//----------------------------------------------------------------------------
function TStorageObj.VariableName(i: Integer): String;

const
    BuffSize = 255;
var
    n,
    i2: Integer;
    Buff: array[0..BuffSize] of AnsiChar;
    pName: pAnsichar;

begin
    if i < 1 then
        Exit;  // Someone goofed

    case i of
        1:
            Result := 'kWh';
        2:
            Result := 'State';
        3:
            Result := 'kWOut';
        4:
            Result := 'kWIn';
        5:
            Result := 'Losses';
        6:
            Result := 'Idling';
        7:
            Result := 'kWh Chng';
    else
    begin
        if UserModel.Exists then    // Checks for existence and Selects
        begin
            pName := @Buff;
            n := UserModel.FNumVars;
            i2 := i - NumStorageVariables;
            if i2 <= n then
            begin
                UserModel.FGetVarName(i2, pName, BuffSize);
                Result := String(pName);
                Exit;
            end;
        end;
        if DynaModel.Exists then   // Checks for existence and Selects
        begin
            pName := @Buff;
            n := DynaModel.FNumVars;
            i2 := i - NumStorageVariables; // Relative index
            if i2 <= n then
            begin
                DynaModel.FGetVarName(i2, pName, BuffSize);
                Result := String(pName);
                Exit;
            end;
        end;
    end;
    end;

end;

//----------------------------------------------------------------------------
procedure TStorageObj.MakePosSequence;

var
    S: String;
    V: Double;

begin

    S := 'Phases=1 conn=wye';

  // Make sure voltage is line-neutral
    if (Fnphases > 1) or (connection <> 0) then
        V := StorageVars.kVStorageBase / SQRT3
    else
        V := StorageVars.kVStorageBase;

    S := S + Format(' kV=%-.5g', [V]);

    if Fnphases > 1 then
    begin
        S := S + Format(' kWrating=%-.5g  PF=%-.5g', [StorageVars.kWrating / Fnphases, PFNominal]);
    end;

    Parser.CmdString := S;
    Edit;

    inherited;   // write out other properties
end;

procedure TStorageObj.Set_ConductorClosed(Index: Integer;
    Value: Boolean);
begin
    inherited;

 // Just turn storage element on or off;

    if Value then
        StorageObjSwitchOpen := FALSE
    else
        StorageObjSwitchOpen := TRUE;

end;

procedure TStorageObj.Set_pctkvarOut(const Value: Double);
begin
    FpctkvarOut := Value;
   // Force recompute of target PF and requested kVAr
    Presentkvar := StorageVars.kWRating * sqrt(1.0 / SQR(PFNominal) - 1.0) * FpctkvarOut / 100.0;
end;

procedure TStorageObj.Set_pctkWOut(const Value: Double);
begin
    FpctkWOut := Value;
    kW_Out := FpctkWOut * StorageVars.kWRating / 100.0;
end;

//----------------------------------------------------------------------------
procedure TStorageObj.Set_PowerFactor(const Value: Double);
begin
    PFNominal := Value;
    SyncUpPowerQuantities;
end;

//----------------------------------------------------------------------------
procedure TStorageObj.Set_PresentkV(const Value: Double);
begin
    StorageVars.kVStorageBase := Value;
    case FNphases of
        2, 3:
            VBase := StorageVars.kVStorageBase * InvSQRT3x1000;
    else
        VBase := StorageVars.kVStorageBase * 1000.0;
    end;
end;

//----------------------------------------------------------------------------
procedure TStorageObj.Set_Presentkvar(const Value: Double);
// set the kvar to requested value within rating of inverter
var
    kVA_Gen: Double;
begin
    kvar_out := Value;
    StorageVars.kvarRequested := Value;
     {Requested kVA output}
    kVA_Gen := Sqrt(Sqr(kW_out) + Sqr(kvar_out));
    with StorageVars do
        if kVA_Gen > kVArating then
            kVA_Gen := kVARating;  // Limit kVA to rated value
    if kVA_Gen <> 0.0 then
        PFNominal := abs(kW_out / kVA_Gen)
    else
        PFNominal := 1.0;
    if (kW_out * kvar_out) < 0.0 then
        PFNominal := -PFNominal;
end;

//----------------------------------------------------------------------------
procedure TStorageObj.Set_PresentkW(const Value: Double);
begin
    FpctkWOut := Value / StorageVars.kWRating * 100.0;
    kW_Out := Value;
     //SyncUpPowerQuantities;
end;

procedure TStorageObj.Set_StorageState(const Value: Integer);
var
    SavedState: Integer;
begin
    SavedState := Fstate;

     // Decline if storage is at its limits ; set to idling instead

    with StorageVars do
        case Value of

            STORE_CHARGING:
            begin
                if kWhStored < kWhRating then
                    Fstate := Value
                else
                    Fstate := STORE_IDLING;   // all charged up
            end;

            STORE_DISCHARGING:
            begin
                if kWhStored > kWhReserve then
                    Fstate := Value
                else
                    Fstate := STORE_IDLING;  // not enough storage to discharge
            end;
        else
            Fstate := STORE_IDLING;
        end;

    if SavedState <> Fstate then
        FStateChanged := TRUE;

     //---DEBUG--- WriteDLLDebugFile(Format('t=%.8g, ---State Set To %s', [ActiveCircuit.Solution.dblHour, StateToStr ]));
end;

//----------------------------------------------------------------------------
procedure TStorageObj.SyncUpPowerQuantities;
begin

    if kVANotSet then
        StorageVars.kVARating := StorageVars.kWrating;
    kvar_out := 0.0;
     // keep kvar nominal up to date with kW and PF
    if (PFNominal <> 0.0) then
    begin
        kvar_out := kW_out * sqrt(1.0 / Sqr(PFNominal) - 1.0);
        if PFNominal < 0.0 then
            kvar_out := -kvar_out;
    end;

     // 5-8-2017  moved this from recalcElementdata
    kvarbase := kvar_out;   // remember for follow mode; synch up here
end;

//----------------------------------------------------------------------------
procedure TStorageObj.SetDragHandRegister(Reg: Integer; const Value: Double);
begin
    if Value > Registers[reg] then
        Registers[Reg] := Value;
end;

//----------------------------------------------------------------------------

initialization

    CDOUBLEONE := CMPLX(1.0, 1.0);

end.
