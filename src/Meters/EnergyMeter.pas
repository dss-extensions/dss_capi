unit EnergyMeter;

{
  ----------------------------------------------------------
  Copyright (c) 2008-2015, Electric Power Research Institute, Inc.
  All rights reserved.
  ----------------------------------------------------------
}
{
     This class of device accumulates the energy of the voltage and current in the
     terminal of the device to which it is connected.

     It is an intelligent energy meter capable of measuring losses of all
     devices within its "zone".

     The Zone is determined automatically after a circuit change.  The Zone starts on the
     opposite side of the branch on which the meter is located and continues in the same
     direction through the network until
       a) an open point is encountered
       b) an open terminal or switch is encountered
       c) another energy meter is encountered
       d) a branch that is already included in a zone is encountered

     It keeps track of kwh, kvarh, UE,  EEN, Losses, etc., having registers FOR each
     of these quantities.

     In EEN/UE calculations, line overload takes precedence.

     If the Max Zone kW limits are specified, then these replace the line overload UE/EEN numbers.
     These limits were added so that the user can override line limits in cases
     such as networks where it is difficult to judge the UE from the individual
     line limits.

     Only the maximum |kVA| overload is accumulated, not all.  Loads downline from
     an overload are marked WITH a factor representing the degree of overload.  This
     is used to compute EEN/UE FOR loads.

     FOR low voltages, the full kW FOR loads below the emergency min voltage are counted.
     The EEN is proportioned based on how low the voltage is.

     Emergency min voltage must be less than normal min voltage.

}

{                 CHANGE LOG

8-3-99  Added Option property
        Revised EEN/UE computation to do either total or excess
8-4-99 Save always rewrites file now and returns file name.

11-11-99 Fixed bug in Take sample to use the maxvalue of the overload_EEN

1-4-99  Modified tree checking to avoid picking up the same load more than once
        Fixed bugs in sampling load EEN/UE
        Modified Overload UE; added kwnormal, kwemerg properties for whole zone

1-28-00 Changed to derived from Meter Element
2-2-00  Trapezoidal Integration option
4-14-00 Added load allocation algorithm
4-17-00 Removed shunt capacitors from meter zones
5-3-00  Corrected Zone kW, kvar accumulation to be actual power not target power
5-29-00 Fixed problem with Nphases not being set right for 1-phase devices.
6-15-01 Added Zonelist and LocalOnly options
7/6/01  Added Voltage Only option for Load UE calcs.
7/19/01 Added Totalizer Function for meterclass
7/24/01 Added Generator registers and code for adding generators to zone lists.
        Changed to use zone loads and gens even if local only. If you only want the local
        measurements, specify a null zone manually.
8/2/01  Fixed hole in Local only options.
4/29/03 Added ReduceZone Function
2/7/07  Fixed overload formulas
9/18/08 Added load loss and no load loss registers  and aux registers
11/8/08 Revamped TakeSample to fix bugs with Demand Interval reporting
8/8/13  Added initial reliability calcs
3/27/2018 Corrected SAIDI calcs
}

{$WARN UNIT_PLATFORM OFF}

interface

uses
    DSSClass,
    MeterClass,
    MeterElement,
    CktElement,
    PDElement,
    arrayDef,
    DSSPointerList,
    CktTree,
    ucomplex,
    Load,
    Generator,
    XYCurve,
    Command,
    Classes;

const
    NumEMVbase = 7;
    NumEMRegisters = 32 + 5 * NumEMVbase;   // Total Number of energy meter registers
    {Fixed Registers}
    Reg_kWh = 1;
    Reg_kvarh = 2;
    Reg_MaxkW = 3;
    Reg_MaxkVA = 4;
    Reg_ZonekWh = 5;
    Reg_Zonekvarh = 6;
    Reg_ZoneMaxkW = 7;
    Reg_ZoneMaxkVA = 8;
    Reg_OverloadkWhNorm = 9;    // Max overload
    Reg_OverloadkWhEmerg = 10;
    Reg_LoadEEN = 11;
    Reg_LoadUE = 12;  // Energy served below normal voltage
    Reg_ZoneLosseskWh = 13;
    Reg_ZoneLosseskvarh = 14;
    Reg_LossesMaxkW = 15;
    Reg_LossesMaxkvar = 16;
    Reg_LoadLosseskWh = 17;
    Reg_LoadLosseskvarh = 18;
    Reg_NoLoadLosseskWh = 19;
    Reg_NoLoadLosseskvarh = 20;
    Reg_MaxLoadLosses = 21;
    Reg_MaxNoLoadLosses = 22;
    Reg_LineLosseskWh = 23;
    Reg_TransformerLosseskWh = 24;
    Reg_LineModeLineLoss = 25;    // for 3-phase feeder lines
    Reg_ZeroModeLineLoss = 26;
    Reg_3_phaseLineLoss = 27;
    Reg_1_phaseLineLoss = 28;
    Reg_GenkWh = 29;
    Reg_Genkvarh = 30;
    Reg_GenMaxkW = 31;
    Reg_GenMaxkVA = 32;
    Reg_VBaseStart = 32;  // anchor for the voltage base loss registers

type
    TRegisterArray = array[1..NumEMregisters] of Double;

    //  --------- Feeder Section Definition -----------
    TFeederSection = record
        OCPDeviceType: Integer;  // 1=Fuse; 2=Recloser; 3=Relay
        NCustomers: Integer;
        NBranches: Integer;
        TotalCustomers: Integer;
        SeqIndex: Integer;  // index of pdelement with OCP device at head of section
        AverageRepairTime: Double;
        SectFaultRate: Double;
        SumFltRatesXRepairHrs: Double;
        SumBranchFltRates: Double;
    end;

    pFeederSections = ^FeederSectionArray;
    FeederSectionArray = array[1..100] of TFeederSection;   // Dummy dimension
    //  --------- Feeder Section Definition -----------

    TSystemMeter = class(Tobject)
    PRIVATE
        kWh, dkWh,
        kvarh, dkvarh,
        peakkW,
        peakkVA,
        Losseskwh, dLosseskWh,
        Losseskvarh, dlosseskvarh,
        PeakLosseskW: Double;
        FirstSampleAfterReset,
        This_Meter_DIFileIsOpen: Boolean;
        cPower, cLosses: Complex;

        procedure Clear;
        procedure Integrate(var Reg: Double; Value: Double; var Deriv: Double);
        procedure WriteRegisters(F: TFileStream);
        procedure WriteRegisterNames(F: TFileStream);

    PROTECTED

        procedure OpenDemandIntervalFile;
        procedure WriteDemandIntervalData;
        procedure CloseDemandIntervalFile;
        procedure AppendDemandIntervalFile;

    PUBLIC

        procedure TakeSample;
        procedure Reset;
        procedure Save;

        constructor Create;
        destructor Destroy; OVERRIDE;

    end;

    TEnergyMeter = class(TMeterClass)    // derive strait from base class
    PRIVATE
        GeneratorClass: TGenerator;
        FSaveDemandInterval: Boolean;
        FDI_Verbose: Boolean;

        procedure ProcessOptions(const Opts: String);
        procedure Set_SaveDemandInterval(const Value: Boolean);
        function Get_SaveDemandInterval: Boolean;
        procedure CreateMeterTotals;
        procedure CreateFDI_Totals;
        procedure ClearDI_Totals;
        procedure WriteTotalsFile;
        procedure OpenOverloadReportFile;
        procedure OpenVoltageReportFile;
        procedure WriteOverloadReport;
        procedure WriteVoltageReport;
        procedure InterpretRegisterMaskArray(var Mask: TRegisterArray);
        procedure Set_DI_Verbose(const Value: Boolean);
        function Get_DI_Verbose: Boolean;

    PROTECTED
        procedure DefineProperties;
        function MakeLike(const EnergyMeterName: String): Integer; OVERRIDE;
        procedure SetHasMeterFlag;

    PUBLIC

        DI_RegisterTotals: TRegisterArray;
        DI_Dir: String;
        FDI_Totals: TFileStream;
        FMeterTotals: TFileStream;

        SystemMeter: TSystemMeter;
        Do_OverloadReport: Boolean;
        Do_VoltageExceptionReport: Boolean;
        OverLoadFileIsOpen: Boolean;
        VoltageFileIsOpen: Boolean;

        constructor Create;
        destructor Destroy; OVERRIDE;

        function Edit: Integer; OVERRIDE;     // uses global parser
        function NewObject(const ObjName: String): Integer; OVERRIDE;

        procedure ResetMeterZonesAll;
        procedure ResetAll; OVERRIDE;  // Reset all meters in active circuit to zero
        procedure SampleAll; OVERRIDE;   // Force all meters in active circuit to sample
        procedure SaveAll; OVERRIDE;

        procedure AppendAllDIFiles;
        procedure OpenAllDIFiles;
        procedure CloseAllDIFiles;

        property SaveDemandInterval: Boolean READ FSaveDemandInterval WRITE Set_SaveDemandInterval;
        property DI_Verbose: Boolean READ FDI_Verbose WRITE Set_DI_Verbose;

    end;

    TEnergyMeterObj = class(TMeterElement)
    PRIVATE
        FirstSampleAfterReset: Boolean;
        ExcessFlag: Boolean;
        ZoneIsRadial: Boolean;
        VoltageUEOnly: Boolean;
        LocalOnly: Boolean;

        FLosses: Boolean;
        FLineLosses: Boolean;
        FXfmrLosses: Boolean;
        FSeqLosses: Boolean;
        F3PhaseLosses: Boolean;
        FVBaseLosses: Boolean;
        FPhaseVoltageReport: Boolean;

        DefinedZoneList: pStringArray;
        DefinedZoneListSize: Integer;

       {Limits on the entire load in the zone for networks where UE cannot be determined
        by the individual branches}
        MaxZonekVA_Norm: Double;
        MaxZonekVA_Emerg: Double;

       {Voltage bases in the Meter Zone}
        VBaseTotalLosses: pDoubleArray;    // allocated array
        VBaseLineLosses: pDoubleArray;
        VBaseLoadLosses: pDoubleArray;
        VBaseNoLoadLosses: pDoubleArray;
        VBaseLoad: pDoubleArray;
        VBaseList: pDoubleArray;
        VBaseCount: Integer;
        MaxVBaseCount: Integer;

       { Arrays for phase voltage report  }
        VphaseMax: pDoubleArray;
        VPhaseMin: pDoubleArray;
        VPhaseAccum: pDoubleArray;
        VPhaseAccumCount: pIntegerArray;
        VPhaseReportFileIsOpen: Boolean;

       {Demand Interval File variables}
        This_Meter_DIFileIsOpen: Boolean;


        procedure Integrate(Reg: Integer; const Deriv: Double; const Interval: Double);
        procedure SetDragHandRegister(Reg: Integer; const Value: Double);
        function Accumulate_Load(pLoad: TLoadObj; var TotalZonekW, TotalZonekvar, TotalLoad_EEN, TotalLoad_UE: Double): Double;
        procedure Accumulate_Gen(pGen: TGeneratorObj; var TotalZonekW, TotalZonekvar: Double);
        procedure CalcBusCoordinates(StartBranch: TCktTreeNode; FirstCoordRef, SecondCoordRef, LineCount: Integer);
        function AddToVoltBaseList(BusRef: Integer): Integer;
        function MakeDIFileName: String;
        function MakeVPhaseReportFileName: String;
        procedure AssignVoltBaseRegisterNames;

        procedure TotalupDownstreamCustomers;


    PROTECTED

        procedure OpenDemandIntervalFile;
        procedure WriteDemandIntervalData;
        procedure CloseDemandIntervalFile;
        procedure AppendDemandIntervalFile;

    PUBLIC
        RegisterNames: array[1..NumEMregisters] of String;

        BranchList: TCktTree;      // Pointers to all circuit elements in meter's zone
        SequenceList: TDSSPointerList;  // Pointers to branches in sequence from meter to ends
        LoadList: TDSSPointerList;  // Pointers to Loads in the Meter zone to aid reliability calcs

        Registers: TRegisterArray;
        Derivatives: TRegisterArray;
        TotalsMask: TRegisterArray;

        // Reliability data for Head of Zone
        SAIFI: Double;     // For this Zone - based on number of customers
        SAIFIkW: Double;     // For this Zone - based on kW load
        SAIDI: Double;
        CAIDI: Double;
        CustInterrupts: Double;

        // Source reliability
        Source_NumInterruptions: Double; // Annual interruptions for upline circuit
        Source_IntDuration: Double; // Aver interruption duration of upline circuit

        SectionCount: Integer;
        ActiveSection: Integer;  // For COM interface to index into FeederSections array
        FeederSections: pFeederSections;
        ZonePCE: Array of string;

        constructor Create(ParClass: TDSSClass; const EnergyMeterName: String);
        destructor Destroy; OVERRIDE;

        procedure MakePosSequence; OVERRIDE;  // Make a positive Sequence Model, reset nphases
        procedure RecalcElementData; OVERRIDE;
        procedure CalcYPrim; OVERRIDE;
        procedure GetCurrents(Curr: pComplexArray); OVERRIDE; //Get present value of terminal Curr
        procedure GetInjCurrents(Curr: pComplexArray); OVERRIDE;   // Returns Injextion currents

        function CheckBranchList(code: Integer): Boolean;
        procedure ResetRegisters;
        procedure TakeSample; OVERRIDE;
        procedure SaveRegisters;
        procedure MakeMeterZoneLists;
        procedure ZoneDump;
        procedure InterpolateCoordinates;

        procedure AllocateLoad;
        procedure ReduceZone;  // Reduce Zone by eliminating buses and merging lines
        procedure SaveZone(const dirname: String);
        procedure GetPCEatZone(const allowEmpty: Boolean = False);

        procedure CalcReliabilityIndices(AssumeRestoration: Boolean);

        function GetPropertyValue(Index: Integer): String; OVERRIDE;
        procedure InitPropertyValues(ArrayOffset: Integer); OVERRIDE;
        procedure DumpProperties(F: TFileStream; Complete: Boolean); OVERRIDE;

    end;

var
    ActiveEnergyMeterObj: TEnergyMeterObj;
  { RegisterNameList      :TCommandList; }

{*******************************************************************************
*    Nomenclature:                                                             *
*                  OV_ Overloads                                               *
*                  VR_ Voltage report                                          *
*                  DI_ Demand interval                                         *
*                  SI_ System Demand interval                                  *
*                  TDI_ DI Totals                                              *
*                  FM_  Meter Totals                                           *
*                  SM_  System Mater                                           *
*                  EMT_  Energy Meter Totals                                   *
*                  PHV_  Phase Voltage Report                                  *
*     These prefixes are applied to the variables of each file mapped into     *
*     Memory using the MemoryMap_Lib                                           *
********************************************************************************
}
    OV_MHandle: TBytesStream;  // a. Handle to the file in memory
    VR_MHandle: TBytesStream;
    DI_MHandle: TBytesStream;
    SDI_MHandle: TBytesStream;
    TDI_MHandle: TBytesStream;
    SM_MHandle: TBytesStream;
    EMT_MHandle: TBytesStream;
    PHV_MHandle: TBytesStream;
    FM_MHandle: TBytesStream;

//*********** Flags for appending Files*****************************************
    OV_Append: Boolean;
    VR_Append: Boolean;
    DI_Append: Boolean;
    SDI_Append: Boolean;
    TDI_Append: Boolean;
    SM_Append: Boolean;
    EMT_Append: Boolean;
    PHV_Append: Boolean;
    FM_Append: Boolean;

implementation

uses
    ParserDel,
    DSSClassDefs,
    DSSGlobals,
    Bus,
    Sysutils,
    MathUtil,
    UCMatrix,
    Utilities,
    PCElement,
    StackDef,
    Circuit,
    Line,
    LineUnits,
    ReduceAlgs,
{$IFNDEF FPC}
    Windows,
{$ENDIF}
    Math,
    MemoryMap_Lib;

 //     {$DEFINE DEBUG}
{$UNDEF DEBUG}


const
    NumPropsThisClass = 24;

var

    Delta_Hrs: Double;
   // adjacency lists for PC and PD elements at each bus, built for faster searches
    BusAdjPC: TAdjArray; // also includes shunt PD elements
    BusAdjPD: TAdjArray;


//- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
function jiIndex(i, j: Integer): Integer; inline;
begin
    Result := (j - 1) * 3 + i;
end;

//- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
constructor TEnergyMeter.Create;  // Creates superstructure FOR all EnergyMeter objects

begin
    inherited Create;
    Class_Name := 'EnergyMeter';
    DSSClassType := DSSClassType + ENERGY_METER;

    ActiveElement := 0;

     {Initialice demand interval options to off}
    FSaveDemandInterval := FALSE;
    FDI_Verbose := FALSE;

    Do_OverloadReport := FALSE;  // FSaveDemandInterval must be true for this to have an effect
    OverLoadFileIsOpen := FALSE;
    VoltageFileIsOpen := FALSE;


    Do_VoltageExceptionReport := FALSE;

    DI_Dir := '';

    DefineProperties;

    CommandList := TCommandList.Create(Slice(PropertyName^, NumProperties));
    CommandList.Abbrev := TRUE;


    GeneratorClass := DSSClassList.Get(ClassNames.Find('generator'));

    SystemMeter := TSystemMeter.Create;
    OV_MHandle := NIL;
    VR_MHandle := NIL;
    DI_MHandle := NIL;
    SDI_MHandle := NIL;
    TDI_MHandle := NIL;
    SM_MHandle := NIL;
    EMT_MHandle := NIL;
    PHV_MHandle := NIL;
    FM_MHandle := NIL;
end;

//- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
destructor TEnergyMeter.Destroy;

begin
    SystemMeter.Free;
    if OV_MHandle <> NIL then
        OV_MHandle.Free;
    if VR_MHandle <> NIL then
        VR_MHandle.Free;
    if DI_MHandle <> NIL then
        DI_MHandle.Free;
    if SDI_MHandle <> NIL then
        SDI_MHandle.Free;
    if TDI_MHandle <> NIL then
        TDI_MHandle.Free;
    if SM_MHandle <> NIL then
        SM_MHandle.Free;
    if EMT_MHandle <> NIL then
        EMT_MHandle.Free;
    if PHV_MHandle <> NIL then
        PHV_MHandle.Free;
    if FM_MHandle <> NIL then
        FM_MHandle.Free;
    // ElementList and  CommandList freed in inherited destroy
    inherited Destroy;
end;


//- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
procedure TEnergyMeter.DefineProperties;
begin

    Numproperties := NumPropsThisClass;
    CountProperties;   // Get inherited property count
    AllocatePropertyArrays;

     // Define Property names

    PropertyName^[1] := 'element';
    PropertyName^[2] := 'terminal';
    PropertyName^[3] := 'action';
    PropertyName^[4] := 'option';
    PropertyName^[5] := 'kVAnormal';
    PropertyName^[6] := 'kVAemerg';
    PropertyName^[7] := 'peakcurrent';
    PropertyName^[8] := 'Zonelist';
    PropertyName^[9] := 'LocalOnly';
    PropertyName^[10] := 'Mask';
    PropertyName^[11] := 'Losses';
    PropertyName^[12] := 'LineLosses';
    PropertyName^[13] := 'XfmrLosses';
    PropertyName^[14] := 'SeqLosses';
    PropertyName^[15] := '3phaseLosses';
    PropertyName^[16] := 'VbaseLosses'; // segregate losses by voltage base
    PropertyName^[17] := 'PhaseVoltageReport'; // Compute Avg phase voltages in zone
    PropertyName^[18] := 'Int_Rate';
    PropertyName^[19] := 'Int_Duration';
    PropertyName^[20] := 'SAIFI';    // Read only
    PropertyName^[21] := 'SAIFIkW';    // Read only
    PropertyName^[22] := 'SAIDI';    // Read only
    PropertyName^[23] := 'CAIDI';    // Read only
    PropertyName^[24] := 'CustInterrupts';    // Read only

{     PropertyName^[11] := 'Feeder';  **** removed - not used}

    PropertyHelp[1] := 'Name (Full Object name) of element to which the monitor is connected.';
    PropertyHelp[2] := 'Number of the terminal of the circuit element to which the monitor is connected. ' +
        '1 or 2, typically.';
    PropertyHelp[3] := '{Clear (reset) | Save | Take | Zonedump | Allocate | Reduce} ' + CRLF + CRLF +
        '(A)llocate = Allocate loads on the meter zone to match PeakCurrent.' + CRLF +
        '(C)lear = reset all registers to zero' + CRLF +
        '(R)educe = reduces zone by merging lines (see Set Keeplist & ReduceOption)' + CRLF +
        '(S)ave = saves the current register values to a file.' + CRLF +
        '   File name is "MTR_metername.CSV".' + CRLF +
        '(T)ake = Takes a sample at present solution' + CRLF +
        '(Z)onedump = Dump names of elements in meter zone to a file' + CRLF +
        '   File name is "Zone_metername.CSV".';
    PropertyHelp[4] := 'Enter a string ARRAY of any combination of the following. Options processed left-to-right:' + CRLF + CRLF +
        '(E)xcess : (default) UE/EEN is estimate of energy over capacity ' + CRLF +
        '(T)otal : UE/EEN is total energy after capacity exceeded' + CRLF +
        '(R)adial : (default) Treats zone as a radial circuit' + CRLF +
        '(M)esh : Treats zone as meshed network (not radial).' + CRLF +
        '(C)ombined : (default) Load UE/EEN computed from combination of overload and undervoltage.' + CRLF +
        '(V)oltage : Load UE/EEN computed based on voltage only.' + CRLF + CRLF +
        'Example: option=(E, R)';
    PropertyHelp[5] := 'Upper limit on kVA load in the zone, Normal configuration. Default is 0.0 (ignored). ' +
        'Overrides limits on individual lines for overload EEN. ' +
        'With "LocalOnly=Yes" option, uses only load in metered branch.';
    PropertyHelp[6] := 'Upper limit on kVA load in the zone, Emergency configuration. Default is 0.0 (ignored). ' +
        'Overrides limits on individual lines for overload UE. ' +
        'With "LocalOnly=Yes" option, uses only load in metered branch.';
    PropertyHelp[7] := 'ARRAY of current magnitudes representing the peak currents measured at this location ' +
        'for the load allocation function.  Default is (400, 400, 400). Enter one current for each phase';
    PropertyHelp[8] := 'ARRAY of full element names for this meter''s zone.  Default is for meter to find it''s own zone. ' +
        'If specified, DSS uses this list instead.  Can access the names in a single-column text file.  Examples: ' + crlf + crlf +
        'zonelist=[line.L1, transformer.T1, Line.L3] ' + CRLF +
        'zonelist=(file=branchlist.txt)';
    PropertyHelp[9] := '{Yes | No}  Default is NO.  If Yes, meter considers only the monitored element ' +
        'for EEN and UE calcs.  Uses whole zone for losses.';
    PropertyHelp[10] := 'Mask for adding registers whenever all meters are totalized.  Array of floating point numbers ' +
        'representing the multiplier to be used for summing each register from this meter. ' +
        'Default = (1, 1, 1, 1, ... ).  You only have to enter as many as are changed (positional). ' +
        'Useful when two meters monitor same energy, etc.';
    PropertyHelp[11] := '{Yes | No}  Default is YES. Compute Zone losses. If NO, then no losses at all are computed.';
    PropertyHelp[12] := '{Yes | No}  Default is YES. Compute Line losses. If NO, then none of the losses are computed.';
    PropertyHelp[13] := '{Yes | No}  Default is YES. Compute Transformer losses. If NO, transformers are ignored in loss calculations.';
    PropertyHelp[14] := '{Yes | No}  Default is YES. Compute Sequence losses in lines and segregate by line mode losses and zero mode losses.';
    PropertyHelp[15] := '{Yes | No}  Default is YES. Compute Line losses and segregate by 3-phase and other (1- and 2-phase) line losses. ';
    PropertyHelp[16] := '{Yes | No}  Default is YES. Compute losses and segregate by voltage base. If NO, then voltage-based tabulation is not reported.';
    PropertyHelp[17] := '{Yes | No}  Default is NO.  Report min, max, and average phase voltages for the zone and tabulate by voltage base. ' +
        'Demand Intervals must be turned on (Set Demand=true) and voltage bases must be defined for this property to take effect. ' +
        'Result is in a separate report file.';
    PropertyHelp[18] := 'Average number of annual interruptions for head of the meter zone (source side of zone or feeder).';
    PropertyHelp[19] := 'Average annual duration, in hr, of interruptions for head of the meter zone (source side of zone or feeder).';
    PropertyHelp[20] := '(Read only) Makes SAIFI result available via return on query (? energymeter.myMeter.SAIFI.';
    PropertyHelp[21] := '(Read only) Makes SAIFIkW result available via return on query (? energymeter.myMeter.SAIFIkW.';
    PropertyHelp[22] := '(Read only) Makes SAIDI result available via return on query (? energymeter.myMeter.SAIDI.';
    PropertyHelp[23] := '(Read only) Makes CAIDI result available via return on query (? energymeter.myMeter.CAIDI.';
    PropertyHelp[24] := '(Read only) Makes Total Customer Interrupts value result available via return on query (? energymeter.myMeter.CustInterrupts.';
(**** Not used in present version
      PropertyHelp[11]:= '{Yes/True | No/False}  Default is NO. If set to Yes, a Feeder object is created corresponding to ' +
                         'the energymeter.  Feeder is enabled if Radial=Yes; diabled if Radial=No.  Feeder is ' +
                         'synched automatically with the meter zone.  Do not create feeders for zones in meshed transmission systems.';
*****)

    ActiveProperty := NumPropsThisClass;
    inherited DefineProperties;  // Add defs of inherited properties to bottom of list

end;

//- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
function TEnergyMeter.NewObject(const ObjName: String): Integer;
begin
   // create a new object of this class and add to list
    with ActiveCircuit do
    begin
        ActiveCktElement := TEnergyMeterObj.Create(Self, ObjName);
        Result := AddObjectToList(ActiveDSSObject);
    end;
end;

//- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
function TEnergyMeter.Edit: Integer;

var
    ParamPointer: Integer;
    ParamName: String;
    Param: String;

    DoRecalc: Boolean;

begin

  // continue parsing WITH contents of Parser
  // continue parsing WITH contents of Parser
    ActiveEnergyMeterObj := ElementList.Active;
    ActiveCircuit.ActiveCktElement := ActiveEnergyMeterObj;

    Result := 0;

    DoRecalc := FALSE;

    with ActiveEnergyMeterObj do
    begin

        MeteredElementChanged := FALSE;
        ParamPointer := 0;
        ParamName := Parser.NextParam;
        Param := Parser.StrValue;
        while Length(Param) > 0 do
        begin
            if (Length(ParamName) = 0) then
                Inc(ParamPointer)
            else
                ParamPointer := CommandList.GetCommand(ParamName);

            if (ParamPointer > 0) and (ParamPointer <= NumProperties) then
                PropertyValue[ParamPointer] := Param;

            case ParamPointer of
                0:
                    DoSimpleMsg('Unknown parameter "' + ParamName + '" for Object "' + Class_Name + '.' + Name + '"', 520);
                1:
                    ElementName := lowercase(param);
                2:
                    MeteredTerminal := Parser.IntValue;
                3:
                begin  {Actions}
                    param := lowercase(param);
                    case param[1] of
                        'a':
                            AllocateLoad;
                        'c':
                            ResetRegisters;
                        'r':
                            ReduceZone;
                        's':
                            SaveRegisters;
                        't':
                            TakeSample;
                        'z':
                            ZoneDump;
                    end;
                end;
                4:
                    ProcessOptions(Param);
                5:
                    MaxZonekVA_Norm := Parser.DblValue;
                6:
                    MaxZonekVA_Emerg := Parser.DblValue;
                7:
                    parser.ParseAsVector(Fnphases, SensorCurrent);   // Inits to zero
                8:
                    InterpretAndAllocStrArray(Param, DefinedZoneListSize, DefinedZoneList);
                9:
                    LocalOnly := InterpretYesNo(Param);
                10:
                    InterpretRegisterMaskArray(TotalsMask);
                11:
                    FLosses := InterpretYesNo(Param);
                12:
                    FLineLosses := InterpretYesNo(Param);
                13:
                    FXfmrLosses := InterpretYesNo(Param);
                14:
                    FSeqLosses := InterpretYesNo(Param);
                15:
                    F3PhaseLosses := InterpretYesNo(Param);
                16:
                    FVBaseLosses := InterpretYesNo(Param);
                17:
                    FPhaseVoltageReport := InterpretYesNo(Param);
                18:
                    Source_NumInterruptions := Parser.dblvalue; // Annual interruptions for upline circuit
                19:
                    Source_IntDuration := Parser.dblValue; // hours
                20:
                    PropertyValue[20] := '';  // placeholder, do nothing just throw value away if someone tries to set it.
                21:
                    PropertyValue[21] := '';  // placeholder, do nothing just throw value away if someone tries to set it.
                22:
                    PropertyValue[22] := '';  // placeholder, do nothing just throw value away if someone tries to set it.
                23:
                    PropertyValue[23] := '';  // placeholder, do nothing just throw value away if someone tries to set it.
                24:
                    PropertyValue[24] := '';  // placeholder, do nothing just throw value away if someone tries to set it.
            else
                ClassEdit(ActiveEnergyMeterObj, ParamPointer - NumPropsthisClass)
            end;

            case ParamPointer of
                1, 2:
                begin
                    MeteredElementChanged := TRUE;
                    DoRecalc := TRUE;
                end;
            end;

            ParamName := Parser.NextParam;
            Param := Parser.StrValue;
        end;

        if DoRecalc then
            RecalcElementData;   // When some basic data have changed
    end;
end;

//- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
function TEnergyMeter.MakeLike(const EnergyMeterName: String): Integer;
var
    OtherEnergyMeter: TEnergyMeterObj;
    i: Integer;
begin
    Result := 0;
   {See IF we can find this EnergyMeter name in the present collection}
    OtherEnergyMeter := Find(EnergyMeterName);
    if OtherEnergyMeter <> NIL then
        with ActiveEnergyMeterObj do
        begin

            NPhases := OtherEnergyMeter.Fnphases;
            NConds := OtherEnergyMeter.Fnconds; // Force Reallocation of terminal stuff

            ElementName := OtherEnergyMeter.ElementName;
            MeteredElement := OtherEnergyMeter.MeteredElement;  // Pointer to target circuit element
            MeteredTerminal := OtherEnergyMeter.MeteredTerminal;
            ExcessFlag := OtherEnergyMeter.ExcessFlag;

            MaxZonekVA_Norm := OtherEnergyMeter.MaxZonekVA_Norm;
            MaxZonekVA_Emerg := OtherEnergyMeter.MaxZonekVA_emerg;

       // Reliability
            Source_NumInterruptions := OtherEnergyMeter.Source_NumInterruptions;
            Source_IntDuration := OtherEnergyMeter.Source_IntDuration;

            FreeStringArray(DefinedZoneList, DefinedZoneListSize);
            DefinedZoneListSize := OtherEnergyMeter.DefinedZoneListSize;
            DefinedZoneList := AllocStringArray(DefinedZoneListSize);
       // Copy Strings over (actually incr ref count on string)
            for i := 1 to DefinedZoneListSize do
                DefinedZoneList^[i] := OtherEnergyMeter.DefinedZoneList^[i];

            LocalOnly := OtherEnergyMeter.LocalOnly;
            VoltageUEOnly := OtherEnergyMeter.VoltageUEOnly;

       {Boolean Flags}
            FLosses := OtherEnergyMeter.FLosses;
            FLineLosses := OtherEnergyMeter.FLineLosses;
            FXfmrLosses := OtherEnergyMeter.FXfmrLosses;
            FSeqLosses := OtherEnergyMeter.FSeqLosses;
            F3PhaseLosses := OtherEnergyMeter.F3PhaseLosses;
            FVBaseLosses := OtherEnergyMeter.FVBaseLosses;
            FPhaseVoltageReport := OtherEnergyMeter.FPhaseVoltageReport;

            for i := 1 to ParentClass.NumProperties do
         // Skip Read Only properties
                if i < 20 then
                    PropertyValue[i] := OtherEnergyMeter.PropertyValue[i];

        end
    else
        DoSimpleMsg('Error in EnergyMeter MakeLike: "' + EnergyMeterName + '" Not Found.', 521);

end;

{--------------------------------------------------------------------------}
procedure TEnergyMeter.ResetMeterZonesAll;  // Force all EnergyMeters in the circuit to reset their meter zones

var
    mtr: TEnergyMeterObj;
    pCktElement: TDSSCktElement;
    PDElem: TPDElement;
    PCElem: TPCElement;
    i: Integer;

begin
    with ActiveCircuit do
    begin
        if Energymeters.ListSize = 0 then
            Exit;  // Do not do anything

    // initialize the Checked Flag FOR all circuit Elements
        pCktElement := CktElements.First;
        while (pCktElement <> NIL) do
        begin
            with pCktElement do
            begin
                Checked := FALSE;
                IsIsolated := TRUE;
                for i := 1 to NTerms do
                    Terminals^[i].Checked := FALSE;
            end;
            pCktElement := CktElements.Next;
        end;

    {Clear some things that will be set by the Meter Zone}
        PDElem := PDElements.First;
        while PDElem <> NIL do
        begin
            PDElem.MeterObj := NIL;
            PDElem.SensorObj := NIL;
            PDElem.ParentPDElement := NIL;
            PDElem := PDElements.Next;
        end;

        PCElem := PCElements.First;
        while PCElem <> NIL do
        begin
            PCElem.MeterObj := NIL;
            PCElem.SensorObj := NIL;
            PCElem := PCElements.Next;
        end;

    // Set up the bus adjacency lists for faster searches to build meter zone lists.
        BuildActiveBusAdjacencyLists(BusAdjPD, BusAdjPC);

    {Set Hasmeter flag for all cktelements}
        SetHasMeterFlag;
        SensorClass.SetHasSensorFlag;  // Set all Sensor branch flags, too.

    // initialize the Checked Flag for all Buses
        for i := 1 to NumBuses do
            Buses^[i].BusChecked := FALSE;

        for i := 1 to EnergyMeters.ListSize do
        begin
            mtr := EnergyMeters.Get(i);
            if Mtr.Enabled then
                mtr.MakeMeterZoneLists;
        end;

        FreeAndNilBusAdjacencyLists(BusAdjPD, BusAdjPC);
    end;
end;

{--------------------------------------------------------------------------}
procedure TEnergyMeter.ResetAll;  // Force all EnergyMeters in the circuit to reset

var
    mtr: TEnergyMeterObj;
    CasePath: String;

begin

    if DIFilesAreOpen then
        CloseAllDIFiles;

    if FSaveDemandInterval then
    begin

        CasePath := OutputDirectory + ActiveCircuit.CaseName;
          {Make directories to save data}

        if not DirectoryExists(CasePath) then
        begin
            try
                mkDir(CasePath);
            except
                On E: Exception do
                    DoSimpleMsg('Error making  Directory: "' + CasePath + '". ' + E.Message, 522);
            end;
        end;
        DI_Dir := CasePath + PathDelim + 'DI_yr_' + Trim(IntToStr(ActiveCircuit.Solution.Year));
        if not DirectoryExists(DI_Dir) then
        begin
            try
                mkDir(DI_Dir);
            except
                On E: Exception do
                    DoSimpleMsg('Error making Demand Interval Directory: "' + DI_Dir + '". ' + E.Message, 523);
            end;
        end;

        CreateFDI_Totals;
    end;

    mtr := ActiveCircuit.EnergyMeters.First;
    while mtr <> NIL do
    begin
        mtr.ResetRegisters;
        mtr := ActiveCircuit.EnergyMeters.Next;
    end;

    SystemMeter.Reset;


      // Reset Generator Objects, too
    GeneratorClass.ResetRegistersAll;

    if DSS_CAPI_LEGACY_MODELS then
    begin
        StorageClass.ResetRegistersAll;
        PVSystemClass.ResetRegistersAll;
    end
    else
    begin
        Storage2Class.ResetRegistersAll;
        PVSystem2Class.ResetRegistersAll;
    end;
end;

{--------------------------------------------------------------------------}
procedure TEnergyMeter.SampleAll;  // Force all EnergyMeters in the circuit to take a sample

var
    mtr: TEnergyMeterObj;
    i: Integer;

begin

    mtr := ActiveCircuit.EnergyMeters.First;
    while mtr <> NIL do
    begin
        if mtr.enabled then
            mtr.TakeSample;
        mtr := ActiveCircuit.EnergyMeters.Next;
    end;

    SystemMeter.TakeSample;

    if FSaveDemandInterval then
    begin  {Write Totals Demand interval file}
        with ActiveCircuit.Solution do
            WriteintoMem(TDI_MHandle, DynaVars.dblHour);
        for i := 1 to NumEMRegisters do
            WriteintoMem(TDI_MHandle, DI_RegisterTotals[i]);
        WriteintoMemStr(TDI_MHandle, Char(10));
        ClearDI_Totals;
        if OverLoadFileIsOpen then
            WriteOverloadReport;
        if VoltageFileIsOpen then
            WriteVoltageReport;
    end;

      // Sample Generator ans Storage Objects, too
    GeneratorClass.SampleAll;
    
    if DSS_CAPI_LEGACY_MODELS then
    begin
        StorageClass.SampleAll; // samples energymeter part of storage elements (not update)
        PVSystemClass.SampleAll;
    end
    else
    begin
        Storage2Class.SampleAll; // samples energymeter part of storage elements (not update)
        PVSystem2Class.SampleAll;
    end;
   
end;

{--------------------------------------------------------------------------}
procedure TEnergyMeter.SaveAll;  // Force all EnergyMeters in the circuit to take a sample

var
    mtr: TEnergyMeterObj;

begin
    mtr := ActiveCircuit.EnergyMeters.First;
    while mtr <> NIL do
    begin
        if mtr.enabled then
            mtr.SaveRegisters;
        mtr := ActiveCircuit.EnergyMeters.Next;
    end;

    SystemMeter.Save;

end;


//- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
//      TEnergyMeter Obj
//- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

constructor TEnergyMeterObj.Create(ParClass: TDSSClass; const EnergyMeterName: String);

var
    i: Integer;

begin
    inherited Create(ParClass);
    Name := LowerCase(EnergyMeterName);
    DSSObjType := ParClass.DSSClassType; //ENERGY_METER;

    NPhases := 3;  // Directly set conds and phases
    Fnconds := 3;
    Nterms := 1;  // this forces allocation of terminals and conductors in base class
    ExcessFlag := TRUE;  // Default to Excess energy FOR UE
    ElementName := 'Vsource.' + TDSSCktElement(ActiveCircuit.CktElements.Get(1)).Name; // Default to first circuit element (source)
    MeteredElement := NIL;
    BranchList := NIL;  // initialize to NIL, set later when inited
    SequenceList := NIL;
    LoadList := NIL;

    This_Meter_DIFileIsOpen := FALSE;
    VPhaseReportFileIsOpen := FALSE;

    InitPropertyValues(0);

     // Max zone kW limits ignored unless the user provides a rating
    MaxZonekVA_Norm := 0.0;
    MaxZonekVA_Emerg := 0.0;

     // Zone reliability variables
    SAIFI := 0.0;     // For this Zone
    SAIFIkW := 0.0;
    SAIDI := 0.0;
    CAIDI := 0.0;
    CustInterrupts := 0.0;
    Source_NumInterruptions := 0.0; // Annual interruptions for upline circuit
    Source_IntDuration := 0.0; // Aver interruption duration of upline circuit


    ZoneIsRadial := TRUE;
    DefinedZoneList := NIL;
    DefinedZoneListSize := 0;

    FLosses := TRUE;   {Loss Reporting switches}
    FLineLosses := TRUE;
    FXfmrLosses := TRUE;
    FSeqLosses := TRUE;
    F3PhaseLosses := TRUE;
    FVBaseLosses := TRUE;
    FPhaseVoltageReport := FALSE;
    VbaseList := NIL;
    VBaseTotalLosses := NIL;
    VBaseLineLosses := NIL;
    VBaseLoadLosses := NIL;
    VBaseNoLoadLosses := NIL;
    VBaseLoad := NIL;
    VBaseCount := 0;
    MaxVBaseCount := (NumEMRegisters - Reg_VBaseStart) div 5;
    ReallocMem(VBaseList, MaxVBaseCount * SizeOf(VBaseList^[1]));
    ReallocMem(VBaseTotalLosses, MaxVBaseCount * SizeOf(VBaseTotalLosses^[1]));
    ReallocMem(VBaseLineLosses, MaxVBaseCount * SizeOf(VBaseLineLosses^[1]));
    ReallocMem(VBaseLoadLosses, MaxVBaseCount * SizeOf(VBaseLoadLosses^[1]));
    ReallocMem(VBaseNoLoadLosses, MaxVBaseCount * SizeOf(VBaseNoLoadLosses^[1]));
    ReallocMem(VBaseLoad, MaxVBaseCount * SizeOf(VBaseLoad^[1]));

//  Init pointers to Nil before allocating
    VphaseMax := NIL;
    VPhaseMin := NIL;
    VPhaseAccum := NIL;
    VPhaseAccumCount := NIL;

     // Arrays for phase voltage report
    ReallocMem(VphaseMax, MaxVBaseCount * 3 * SizeOf(Double));
    ReallocMem(VPhaseMin, MaxVBaseCount * 3 * SizeOf(Double));
    ReallocMem(VPhaseAccum, MaxVBaseCount * 3 * SizeOf(Double));
    ReallocMem(VPhaseAccumCount, MaxVBaseCount * 3 * SizeOf(Integer));

    LocalOnly := FALSE;
    VoltageUEOnly := FALSE;

//*************No append files by default***************************************
    OV_Append := FALSE;
    VR_Append := FALSE;
    DI_Append := FALSE;
    SDI_Append := FALSE;
    TDI_Append := FALSE;
    SM_Append := FALSE;
    EMT_Append := FALSE;
    PHV_Append := FALSE;
    FM_Append := FALSE;

     // Set Register names  that correspond to the register quantities
    RegisterNames[1] := 'kWh';
    RegisterNames[2] := 'kvarh';
    RegisterNames[3] := 'Max kW';
    RegisterNames[4] := 'Max kVA';
    RegisterNames[5] := 'Zone kWh';
    RegisterNames[6] := 'Zone kvarh';
    RegisterNames[7] := 'Zone Max kW';
    RegisterNames[8] := 'Zone Max kVA';
    RegisterNames[9] := 'Overload kWh Normal';
    RegisterNames[10] := 'Overload kWh Emerg';
    RegisterNames[11] := 'Load EEN';
    RegisterNames[12] := 'Load UE';
    RegisterNames[13] := 'Zone Losses kWh';
    RegisterNames[14] := 'Zone Losses kvarh';
    RegisterNames[15] := 'Zone Max kW Losses';
    RegisterNames[16] := 'Zone Max kvar Losses';
    RegisterNames[17] := 'Load Losses kWh';
    RegisterNames[18] := 'Load Losses kvarh';
    RegisterNames[19] := 'No Load Losses kWh';
    RegisterNames[20] := 'No Load Losses kvarh';
    RegisterNames[21] := 'Max kW Load Losses';
    RegisterNames[22] := 'Max kW No Load Losses';
    RegisterNames[23] := 'Line Losses';
    RegisterNames[24] := 'Transformer Losses';

    RegisterNames[25] := 'Line Mode Line Losses';
    RegisterNames[26] := 'Zero Mode Line Losses';

    RegisterNames[27] := '3-phase Line Losses';
    RegisterNames[28] := '1- and 2-phase Line Losses';

    RegisterNames[29] := 'Gen kWh';
    RegisterNames[30] := 'Gen kvarh';
    RegisterNames[31] := 'Gen Max kW';
    RegisterNames[32] := 'Gen Max kVA';
     {Registers for capturing losses by base voltage, names assigned later}
    for i := Reg_VBaseStart + 1 to NumEMRegisters do
        RegisterNames[i] := '';

    ResetRegisters;
    for i := 1 to NumEMRegisters do
        TotalsMask[i] := 1.0;

    AllocateSensorArrays;

    for i := 1 to Fnphases do
        SensorCurrent^[i] := 400.0;

    FeederSections := NIL;
    ActiveSection := 0;

    SetLength(ZonePCE, 1);
    ZonePCE[0] := '';

    // RecalcElementData;
end;

//- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
destructor TEnergyMeterObj.Destroy;
var
    i: Integer;
begin
    if Assigned(VBaseList) then
        Reallocmem(VBaseList, 0);
    if Assigned(VBaseTotalLosses) then
        Reallocmem(VBaseTotalLosses, 0);
    if Assigned(VBaseLineLosses) then
        Reallocmem(VBaseLineLosses, 0);
    if Assigned(VBaseLoadLosses) then
        Reallocmem(VBaseLoadLosses, 0);
    if Assigned(VBaseNoLoadLosses) then
        Reallocmem(VBaseNoLoadLosses, 0);
    if Assigned(VBaseLoad) then
        Reallocmem(VBaseLoad, 0);
     // Arrays for phase voltage report
    if Assigned(VphaseMax) then
        ReallocMem(VphaseMax, 0);
    if Assigned(VPhaseMin) then
        ReallocMem(VPhaseMin, 0);
    if Assigned(VPhaseAccum) then
        ReallocMem(VPhaseAccum, 0);
    if Assigned(VPhaseAccumCount) then
        ReallocMem(VPhaseAccumCount, 0);

    for i := 1 to NumEMRegisters do
        RegisterNames[i] := '';
    if Assigned(BranchList) then
        BranchList.Free;
    if Assigned(SequenceList) then
        SequenceList.Free;
    if Assigned(LoadList) then
        LoadList.Free;
    FreeStringArray(DefinedZoneList, DefinedZoneListSize);

    if Assigned(FeederSections) then
        Reallocmem(FeederSections, 0);

    inherited destroy;
end;

//- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
procedure TEnergyMeterObj.RecalcElementData;

var
    DevIndex: Integer;

begin
    Devindex := GetCktElementIndex(ElementName);   // Global function
    if DevIndex > 0 then
    begin  // Monitored element must already exist
        MeteredElement := ActiveCircuit.CktElements.Get(DevIndex); // Get pointer to metered element
         {MeteredElement must be a PDElement}
        if not (MeteredElement is TPDElement) then
        begin
            MeteredElement := NIL;   // element not found
            DoErrorMsg('EnergyMeter: "' + Self.Name + '"', 'Circuit Element "' + ElementName + '" is not a Power Delivery (PD) element.',
                ' Element must be a PD element.', 525);
            Exit;
        end;


        if MeteredTerminal > MeteredElement.Nterms then
        begin
            DoErrorMsg('EnergyMeter: "' + Name + '"',
                'Terminal no. "' + IntToStr(MeteredTerminal) + '" does not exist.',
                'Respecify terminal no.', 524);
        end
        else
        begin

            if MeteredElementChanged then
            begin
               // Sets name of i-th terminal's connected bus in monitor's buslist
               // This value will be used to set the NodeRef array (see TakeSample)
                Setbus(1, MeteredElement.GetBus(MeteredTerminal));
                Nphases := MeteredElement.NPhases;
                Nconds := MeteredElement.Nconds;
                AllocateSensorArrays;

                 // If we come through here, throw branchlist away
                if BranchList <> NIL then
                    BranchList.Free;
                BranchList := NIL;
            end;

        end;
    end
    else
    begin
        MeteredElement := NIL;   // element not found
        DoErrorMsg('EnergyMeter: "' + Self.Name + '"', 'Circuit Element "' + ElementName + '" Not Found.',
            ' Element must be defined previously.', 525);
    end;
end;

procedure TEnergyMeterobj.MakePosSequence;
begin
    if MeteredElement <> NIL then
    begin
        Setbus(1, MeteredElement.GetBus(MeteredTerminal));
        Nphases := MeteredElement.NPhases;
        Nconds := MeteredElement.Nconds;
        AllocateSensorArrays;
        if BranchList <> NIL then
            BranchList.Free;
        BranchList := NIL;
    end;
    inherited;
end;

function TEnergyMeterObj.MakeVPhaseReportFileName: String;
begin
    Result := EnergyMeterClass.DI_Dir + PathDelim + Name + '_PhaseVoltageReport.CSV';
end;

//- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
procedure TEnergyMeterObj.ResetRegisters;

var
    i: Integer;

begin
    for i := 1 to NumEMregisters do
        Registers[i] := 0.0;
    for i := 1 to NumEMregisters do
        Derivatives[i] := 0.0;
   {Initialize DragHand registers to some big negative number}
    Registers[Reg_MaxkW] := -1.0e50;
    Registers[Reg_MaxkVA] := -1.0e50;
    Registers[Reg_ZoneMaxkW] := -1.0e50;
    Registers[Reg_ZoneMaxkVA] := -1.0e50;
    Registers[Reg_MaxLoadLosses] := -1.0e50;
    Registers[Reg_MaxNoLoadLosses] := -1.0e50;
    Registers[Reg_LossesMaxkW] := -1.0e50;
    Registers[Reg_LossesMaxkvar] := -1.0e50;

    Registers[Reg_GenMaxkW] := -1.0e50;
    Registers[Reg_GenMaxkVA] := -1.0e50;

    FirstSampleAfterReset := TRUE;  // initialize for trapezoidal integration
   // Removed .. open in solution loop See Solve Yearly If EnergyMeterClass.SaveDemandInterval Then OpenDemandIntervalFile;

end;


//- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
procedure TEnergyMeterObj.CalcYPrim;

begin

 // YPrim is all zeros.  Just leave as NIL so it is ignored.

end;

//- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
procedure TEnergyMeterObj.SaveRegisters;

var
    CSVName: String;
    F: TFileStream = nil;
    i: Integer;
    sout: String;
begin

    try
        CSVName := 'MTR_' + Name + '.CSV';
        F := TFileStream.Create(GetOutputDirectory + CSVName, fmCreate);
        GlobalResult := CSVName;
        SetLastResultFile(CSVName);

    except
        On E: Exception do
        begin
            DoSimpleMsg('Error opening Meter File "' + CRLF + CSVName + '": ' + E.Message, 526);
            FreeAndNil(F);
            Exit;
        end
    end;

    try
//       FSWriteln(F,'**** NEW RECORD ****');
        WriteStr(sout, 'Year, ', ActiveCircuit.Solution.Year: 0, ',');
        FSWriteLn(F, sout);
        for i := 1 to NumEMregisters do
        begin
            WriteStr(sout, '"', RegisterNames[i], '",', Registers[i]: 0: 0);
            FSWriteLn(F, sout);
        end;
    finally
        F.Free();
    end;

end;

//- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
procedure TEnergyMeterObj.Integrate(Reg: Integer; const Deriv: Double; const Interval: Double);

begin
    if ActiveCircuit.TrapezoidalIntegration then
    begin
        {Trapezoidal Rule Integration}
        if not FirstSampleAfterReset then
            Registers[Reg] := Registers[Reg] + 0.5 * Interval * (Deriv + Derivatives[Reg]);
    end
    else
    begin {Plain Euler integration}
        Registers[Reg] := Registers[Reg] + Interval * Deriv;
    end;

{ Set the derivatives so that the proper value shows up in Demand Interval Files
  and prepare for next time step in Trapezoidal integration }
    Derivatives[Reg] := Deriv;


end;

//- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
procedure TEnergyMeterObj.TakeSample;
// Update registers from metered zone
// Assumes one time period has taken place since last sample.

var
    i, j, idx: Integer;

    S_Local,
    S_Totallosses,
    S_LoadLosses,
    S_NoLoadLosses,
    TotalLoadLosses,
    TotalNoLoadLosses,
    TotalLineLosses,
    TotalTransformerLosses,
    TotalLineModeLosses,    // Lines only  for now
    TotalZeroModeLosses,
    Total3phaseLosses,
    Total1phaseLosses,
    TotalLosses: Complex;

    CktElem,
    ParenElem: TPDElement;
    PCelem: TPCElement;
    pLoad: TLoadobj;
    pGen: TGeneratorObj;
   // doubles
    MaxExcesskWNorm,
    MaxExcesskWEmerg,
    EEN,
    UE,
    ZonekW,
    TotalZonekw,
    TotalZonekvar,
    TotalLoad_EEN,
    TotalLoad_UE,
    TotalGenkw,
    TotalGenkvar,
    LoadkVA,
    GenkVA,
    S_Local_kVA,
    load_kw: Double;
    S_PosSeqLosses: Complex;
    S_ZeroSeqLosses: Complex;
    S_NegSeqLosses: Complex;

    puV: Double;

begin
    if not CheckBranchList(545) then
        Exit;

// Compute energy in branch  to which meter is connected

     //----MeteredElement.ActiveTerminalIdx := MeteredTerminal;  // needed for Excess kVA calcs
    S_Local := CmulReal(MeteredElement.Power[MeteredTerminal], 0.001);
    S_Local_kVA := Cabs(S_Local);
    Delta_Hrs := ActiveCircuit.Solution.IntervalHrs;
    Integrate(Reg_kWh, S_Local.re, Delta_Hrs);   // Accumulate the power
    Integrate(Reg_kvarh, S_Local.im, Delta_Hrs);
    SetDragHandRegister(Reg_MaxkW, S_Local.re);   // 3-10-04 removed abs()
    SetDragHandRegister(Reg_MaxkVA, S_Local_kVA);

// Compute Maximum overload energy in all branches in zone
// and mark all load downline from an overloaded branch as unserved
// If localonly, check only metered element

    TotalLosses := CZERO;     // Initialize loss accumulators
    TotalLoadLosses := CZERO;
    TotalNoLoadLosses := CZERO;
    TotalLineLosses := CZERO;
    TotalLineModeLosses := CZERO;
    TotalZeroModeLosses := CZERO;
    Total3phaseLosses := CZERO;
    Total1phaseLosses := CZERO;
    TotalTransformerLosses := CZERO;

     // Init all voltage base loss accumulators
    for i := 1 to MaxVBaseCount do
    begin
        VBaseTotalLosses^[i] := 0.0;
        VBaseLineLosses^[i] := 0.0;
        VBaseLoadLosses^[i] := 0.0;
        VBaseNoLoadLosses^[i] := 0.0;
        VBaseLoad^[i] := 0.0;
    end;

     // Phase Voltage arrays
    if FPhaseVoltageReport then
        for i := 1 to MaxVBaseCount do
            if VBaseList^[i] > 0.0 then
            begin
                for j := 1 to 3 do
                begin
                    VphaseMax^[jiIndex(j, i)] := 0.0;
                    VphaseMin^[jiIndex(j, i)] := 9999.0;
                    VphaseAccum^[jiIndex(j, i)] := 0.0;
                    VphaseAccumCount^[jiIndex(j, i)] := 0;   // Keep track of counts for average
                end;
            end;

    CktElem := BranchList.First;
    MaxExcesskWNorm := 0.0;
    MaxExcesskWEmerg := 0.0;

     {--------------------------------------------------------------------------}
     {------------------------ Local Zone  Only --------------------------------}
     {--------------------------------------------------------------------------}
    if LocalOnly then
    begin
        CktElem := MeteredElement as TPDElement;
        MaxExcesskWNorm := Abs(CktElem.ExcesskVANorm[MeteredTerminal].re);
        MaxExcesskWEmerg := Abs(CktElem.ExcesskVAEmerg[MeteredTerminal].re);
    end
    else
     {--------------------------------------------------------------------------}
     {--------Cyle Through Entire Zone Setting EEN/UE --------------------------}
     {--------------------------------------------------------------------------}
        while CktElem <> NIL do
        begin       // loop thru all ckt elements on zone

            with CktElem do
            begin
                ActiveTerminalIdx := BranchList.Presentbranch.FromTerminal;
         // Invoking this property sets the Overload_UE flag in the PD Element
                EEN := Abs(ExcesskVANorm[ActiveTerminalIdx].re);
                UE := Abs(ExcesskVAEmerg[ActiveTerminalIdx].re);
            end;

         {For radial circuits just keep the maximum overload; for mesh, add 'em up}
            if (ZoneIsRadial) then
            begin
                if UE > MaxExcesskWEmerg then
                    MaxExcesskWEmerg := UE;
                if EEN > MaxExcesskWNorm then
                    MaxExcesskWNorm := EEN;
            end
            else
            begin
                MaxExcesskWEmerg := MaxExcesskWEmerg + UE;
                MaxExcesskWNorm := MaxExcesskWNorm + EEN;
            end;

         // Even if this branch is not overloaded, if the parent element is overloaded
         // mark load on this branch as unserved also
         // Use the larger of the two factors
            ParenElem := BranchList.Parent;
            if (ParenElem <> NIL) then
            begin
                CktElem.OverLoad_EEN := Max(CktElem.Overload_EEN, ParenElem.Overload_EEN);
                CktElem.OverLoad_UE := Max(CktElem.OverLoad_UE, ParenElem.OverLoad_UE);
            end;

         // Mark loads (not generators) by the degree of overload if the meter's zone is to be considered radial
         // This overrides and supercedes the load's own determination of unserved based on voltage
         // If voltage only is to be used for Load UE/EEN, don't mark (set to 0.0 and load will calc UE based on voltage)
            PCElem := Branchlist.FirstObject;
            while (PCElem <> NIL) do
            begin
                if (PCElem.DSSObjType and CLASSMASK) = LOAD_ELEMENT then
                begin
                    pLoad := PCElem as TLoadObj;
                    if (CktElem.Overload_EEN > 0.0) and (ZoneIsRadial) and not (VoltageUEOnly) then
                        pLoad.EEN_Factor := CktElem.Overload_EEN
                    else
                        pLoad.EEN_Factor := 0.0;

                    if (CktElem.Overload_UE > 0.0) and (ZoneIsRadial) and not (VoltageUEOnly) then
                        pLoad.UE_Factor := CktElem.Overload_UE
                    else
                        pLoad.UE_Factor := 0.0;
                end;
                PCElem := BranchList.NextObject
            end;

            CktElem := BranchList.GoForward;
        end;


     // Get the Losses, and unserved bus energies
    TotalZonekw := 0.0;
    TotalZonekvar := 0.0;
    TotalLoad_EEN := 0.0;
    TotalLoad_UE := 0.0;
    TotalGenkw := 0.0;
    TotalGenkvar := 0.0;


     {--------------------------------------------------------------------------}
     {--------       Cycle Through Zone Accumulating Load and Losses    --------}
     {--------------------------------------------------------------------------}
    CktElem := BranchList.First;
    while (CktElem <> NIL) do
    begin
        PCElem := Branchlist.FirstObject;
        while (PCElem <> NIL) do
        begin
            case (PCElem.DSSObjType and CLASSMASK) of
                LOAD_ELEMENT:
                    if not LocalOnly then
                    begin   // Dont check for load EEN/UE if Local only
                        pLoad := PCElem as TLoadObj;
                        load_kw := Accumulate_Load(pLoad, TotalZonekW, TotalZonekvar, TotalLoad_EEN, TotalLoad_UE);
                        if FVbaseLosses then
                            with BranchList.PresentBranch do
                                if VoltBaseIndex > 0 then
                                    VBaseLoad^[VoltBaseIndex] := VBaseLoad^[VoltBaseIndex] + load_kw;
                    end;
                GEN_ELEMENT:
                begin
                    pGen := PCElem as TGeneratorObj;
                    Accumulate_Gen(pGen, TotalGenkW, TotalGenkvar);
                end;
            else
                {Ignore other types of PC Elements}
            end;
            PCElem := BranchList.NextObject
        end;

        if Flosses then
        begin  // Compute and Report Losses

           {Get losses from the present circuit element}
            CktElem.GetLosses(S_TotalLosses, S_LoadLosses, S_NoLoadLosses);  // returns watts, vars
           {Convert to kW}
            CmulRealAccum(S_TotalLosses, 0.001);
            CmulRealAccum(S_LoadLosses, 0.001);
            CmulRealAccum(S_NoLoadLosses, 0.001);
           {Update accumulators}
            Caccum(TotalLosses, S_TotalLosses); // Accumulate total losses in meter zone
            Caccum(TotalLoadLosses, S_LoadLosses);  // Accumulate total load losses in meter zone
            Caccum(TotalNoLoadLosses, S_NoLoadLosses); // Accumulate total no load losses in meter zone

           {Line and Transformer Elements}
            if IsLineElement(Cktelem) and FLineLosses then
            begin
                Caccum(TotalLineLosses, S_TotalLosses); // Accumulate total losses in meter zone
                if FseqLosses then
                begin
                    CktElem.GetSeqLosses(S_PosSeqLosses, S_NegSeqLosses, S_ZeroSeqLosses);
                    Caccum(S_PosSeqLosses, S_NegSeqLosses);  // add line modes together
                    CmulRealAccum(S_PosSeqLosses, 0.001); // convert to kW
                    CmulRealAccum(S_ZeroSeqLosses, 0.001);
                    Caccum(TotalLineModeLosses, S_PosSeqLosses);
                    Caccum(TotalZeroModeLosses, S_ZeroSeqLosses);
                end;
               {Separate Line losses into 3- and "1-phase" losses}
                if F3PhaseLosses then
                begin
                    if Cktelem.NPhases = 3 then
                        Caccum(Total3phaseLosses, S_TotalLosses)
                    else
                        Caccum(Total1phaseLosses, S_TotalLosses);
                end;
            end
            else
            if IsTransformerElement(Cktelem) and FXfmrLosses then
            begin
                Caccum(TotalTransformerLosses, S_TotalLosses); // Accumulate total losses in meter zone
            end;

            if FVbaseLosses then
                with BranchList.PresentBranch do
                    if VoltBaseIndex > 0 then
                    begin
                        VBaseTotalLosses^[VoltBaseIndex] := VBaseTotalLosses^[VoltBaseIndex] + S_TotalLosses.re;
                        if IsLineElement(CktElem) then
                            VBaseLineLosses^[VoltBaseIndex] := VBaseLineLosses^[VoltBaseIndex] + S_TotalLosses.re
                        else
                        if IsTransformerElement(CktElem) then
                        begin
                            VBaseLoadLosses^[VoltBaseIndex] := VBaseLoadLosses^[VoltBaseIndex] + S_LoadLosses.re;
                            VBaseNoLoadLosses^[VoltBaseIndex] := VBaseNoLoadLosses^[VoltBaseIndex] + S_NoLoadLosses.re
                        end;
                    end;

           // Compute min, max, and average pu voltages for 1st 3 phases  (nodes designated 1, 2, or 3)
            if FPhaseVoltageReport then
                with BranchList.PresentBranch do
                    if VoltBaseIndex > 0 then
                        with ActiveCircuit do
                            if Buses^[FromBusReference].kVBase > 0.0 then
                            begin
                                for i := 1 to Buses^[FromBusReference].NumNodesThisBus do
                                begin
                                    j := Buses^[FromBusReference].GetNum(i);
                                    if (j > 0) and (j < 4) then
                                    begin
                                        puV := Cabs(Solution.NodeV^[Buses^[FromBusReference].GetRef(i)]) / Buses^[FromBusReference].kVBase;
                                        idx := jiIndex(j, VoltBaseIndex);
                                        if puV > VphaseMax^[idx] then
                                        begin
                                            VphaseMax^[jiIndex(j, VoltBaseIndex)] := puV;
                         // VmaxBus := FromBusReference;
                                        end;

                                        if puV < VphaseMin^[idx] then
                                        begin
                                            VphaseMin^[jiIndex(j, VoltBaseIndex)] := puV;
                         // VminBus := FromBusReference;
                                        end;

                                        DblInc(VphaseAccum^[jiIndex(j, VoltBaseIndex)], puV);
                                        Inc(VphaseAccumCount^[jiIndex(j, VoltBaseIndex)]);   // Keep track of counts for average
                                    end;
                                end;
                            end;
        end;  {If FLosses}

        CktElem := BranchList.GoForward;
    end;

     {NOTE: Integrate proc automatically sets derivatives array}
    Integrate(Reg_LoadEEN, TotalLoad_EEN, Delta_Hrs);
    Integrate(Reg_LoadUE, TotalLoad_UE, Delta_Hrs);

     {Accumulate losses in appropriate registers}
    Integrate(Reg_ZoneLosseskWh, TotalLosses.re, Delta_Hrs);
    Integrate(Reg_ZoneLosseskvarh, TotalLosses.im, Delta_Hrs);
    Integrate(Reg_LoadLosseskWh, TotalLoadLosses.re, Delta_Hrs);
    Integrate(Reg_LoadLosseskvarh, TotalLoadLosses.im, Delta_Hrs);
    Integrate(Reg_NoLoadLosseskWh, TotalNoLoadLosses.re, Delta_Hrs);
    Integrate(Reg_NoLoadLosseskvarh, TotalNoLoadLosses.im, Delta_Hrs);
    Integrate(Reg_LineLosseskWh, TotalLineLosses.re, Delta_Hrs);
    Integrate(Reg_LineModeLineLoss, TotalLineModeLosses.re, Delta_Hrs);
    Integrate(Reg_ZeroModeLineLoss, TotalZeroModeLosses.re, Delta_Hrs);
    Integrate(Reg_3_phaseLineLoss, Total3phaseLosses.re, Delta_Hrs);
    Integrate(Reg_1_phaseLineLoss, Total1phaseLosses.re, Delta_Hrs);
    Integrate(Reg_TransformerLosseskWh, TotalTransformerLosses.re, Delta_Hrs);
    for i := 1 to MaxVBaseCount do
    begin
        Integrate(Reg_VbaseStart + i, VBaseTotalLosses^[i], Delta_Hrs);
        Integrate(Reg_VbaseStart + 1 * MaxVBaseCount + i, VBaseLineLosses^[i], Delta_Hrs);
        Integrate(Reg_VbaseStart + 2 * MaxVBaseCount + i, VBaseLoadLosses^[i], Delta_Hrs);
        Integrate(Reg_VbaseStart + 3 * MaxVBaseCount + i, VBaseNoLoadLosses^[i], Delta_Hrs);
        Integrate(Reg_VbaseStart + 4 * MaxVBaseCount + i, VBaseLoad^[i], Delta_Hrs);
    end;


     {--------------------------------------------------------------------------}
     {---------------   Total Zone Load and Generation -------------------------}
     {--------------------------------------------------------------------------}

    Integrate(Reg_ZonekWh, TotalZonekW, Delta_Hrs);
    Integrate(Reg_Zonekvarh, TotalZonekvar, Delta_Hrs);
    Integrate(Reg_GenkWh, TotalGenkW, Delta_Hrs);
    Integrate(Reg_Genkvarh, TotalGenkvar, Delta_Hrs);
    GenkVA := Sqrt(Sqr(TotalGenkvar) + Sqr(TotalGenkW));
    LoadkVA := Sqrt(Sqr(TotalZonekvar) + Sqr(TotalZonekW));

     {--------------------------------------------------------------------------}
     {---------------   Set Drag Hand Registers  ------------------------------}
     {--------------------------------------------------------------------------}

    SetDragHandRegister(Reg_LossesMaxkW, Abs(TotalLosses.Re));
    SetDragHandRegister(Reg_LossesMaxkvar, Abs(TotalLosses.im));
    SetDragHandRegister(Reg_MaxLoadLosses, Abs(TotalLoadLosses.Re));
    SetDragHandRegister(Reg_MaxNoLoadLosses, Abs(TotalNoLoadLosses.Re));
    SetDragHandRegister(Reg_ZoneMaxkW, TotalZonekW); // Removed abs()  3-10-04
    SetDragHandRegister(Reg_ZoneMaxkVA, LoadkVA);
     {Max total generator registers}
    SetDragHandRegister(Reg_GenMaxkW, TotalGenkW); // Removed abs()  3-10-04
    SetDragHandRegister(Reg_GenMaxkVA, GenkVA);

     {--------------------------------------------------------------------------}
     {---------------------   Overload Energy  ---------------------------------}
     {--------------------------------------------------------------------------}
     {Overload energy for the entire zone}
    if LocalOnly then
        ZonekW := S_Local.Re
    else
        ZonekW := TotalZonekW;

     {Either the max excess kW of any PD element or the excess over zone limits}

     {regs 9 and 10}
     {Fixed these formulas 2-7-07 per discussions with Daniel Brooks }
    if (MaxZonekVA_Norm > 0.0) then
    begin
        if (S_Local_KVA = 0.0) then
            S_Local_KVA := MaxZonekVA_Norm;
        Integrate(Reg_OverloadkWhNorm, Max(0.0, (ZonekW * (1.0 - MaxZonekVA_Norm / S_Local_KVA))), Delta_Hrs);
    end
    else
    begin
        Integrate(Reg_OverloadkWhNorm, MaxExcesskWNorm, Delta_Hrs);
    end;

    if (MaxZonekVA_Emerg > 0.0) then
    begin
        if (S_Local_KVA = 0.0) then
            S_Local_KVA := MaxZonekVA_Emerg;
        Integrate(Reg_OverloadkWhEmerg, Max(0.0, (ZonekW * (1.0 - MaxZonekVA_Emerg / S_Local_KVA))), Delta_Hrs);
    end
    else
    begin
        Integrate(Reg_OverloadkWhEmerg, MaxExcesskWEmerg, Delta_Hrs);
    end;

    FirstSampleAfterReset := FALSE;
    if EnergyMeterClass.SaveDemandInterval then
        WriteDemandIntervalData;
end;

{---------------------------------------------------------------------------------}

procedure TEnergyMeterObj.TotalUpDownstreamCustomers;
var
    i: Integer;
  {, Accumulator}
 // PresentNode: TCktTreeNode;
    CktElem: TPDElement;

begin
    if not CheckBranchList(529) then
        Exit;

    {Init totsls and checked flag}
    CktElem := SequenceList.First;
    while CktElem <> NIL do
    begin
        CktElem.Checked := FALSE;
        CktElem.BranchTotalCustomers := 0;
        CktElem := SequenceList.Next;
    end;

  {This algorithm could be made more efficient with a Sequence list}
    (*********
     For i := 1 to Branchlist.ZoneEndsList.NumEnds Do
     Begin
       {Busref := } Branchlist.ZoneEndsList.Get(i, PresentNode);
       If PresentNode <> Nil Then
       Begin
          CktElem     := PresentNode.CktObject;
          if Not CktElem.Checked  then    // don't do a zone end element more than once
          Begin
            CktElem.Checked := TRUE;
            Accumulator := CktElem.NumCustomers;
            Repeat  {Trace back to the source}

                Inc(CktElem.TotalCustomers, Accumulator);
                PresentNode := PresentNode.ParentBranch;
                If PresentNode=Nil Then Break;
                CktElem     := PresentNode.CktObject;
                If not CktElem.Checked Then Begin   // avoid double counting
                   Inc(Accumulator, CktElem.NumCustomers);
                   CktElem.Checked := TRUE;
                End;

            Until FALSE;
          End;
       End;
     End; {For}
     *******)

     // Backward Sweep  -  Order is guaranteed to process end branches first
     // sum numcustomers branch by branch
    for i := SequenceList.ListSize downto 1 do
    begin
        CktElem := SequenceList.Get(i);
        if not CktElem.Checked then    // Avoid double counting
            with CktElem do
            begin
                Checked := TRUE;
                Inc(BranchTotalCustomers, BranchNumCustomers);
                if ParentPDElement <> NIL then
                    Inc(ParentPDElement.BranchTotalCustomers, BranchTotalCustomers);
            end;
    end;  {For i}

end;

//- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
procedure TEnergyMeter.SetHasMeterFlag;
// Set the HasMeter Flag for all cktElement;
var
    i: Integer;
    ThisMeter: TEnergyMeterObj;
    CktElem: TDSSCktElement;

begin
   {Initialize all to FALSE}
    with  ActiveCircuit do
    begin
        CktElem := PDElements.First;
        while CktElem <> NIL do
        begin
            CktElem.HasEnergyMeter := FALSE;
            CktElem := PDElements.Next;
        end;  {WHILE}
    end; {WITH}

    for i := 1 to ActiveCircuit.EnergyMeters.ListSize do
    begin
        ThisMeter := ActiveCircuit.EnergyMeters.Get(i);
        with ThisMeter do
            if MeteredElement <> NIL then
                MeteredElement.HasEnergyMeter := TRUE;
    end;   {FOR}
end;

//- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

//- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
procedure TEnergyMeterObj.MakeMeterZoneLists;

// This gets fired off whenever the buslists are rebuilt
// Must be updated whenever there is a change in the circuit

var

    TestBusNum,
    ZoneListCounter: Integer;
    j, iTerm,
    iPC, iPD: Integer;
    ActiveBranch: TDSSCktElement;
    TestElement: TPDElement;
    pPCelem: TPCElement;
    pLoad: TLoadObj;
    IsFeederEnd: Boolean;
    adjLst: TList;
    PCElementType: Cardinal;

begin

    ZoneListCounter := 0;
    VBasecount := 0; {Build the voltage base list over in case a base added or deleted}
    for j := 1 to MaxVBaseCount do
        VBaseList^[j] := 0.0;

  // Make a new branch list
    if BranchList <> NIL then
        BranchList.Free;
    BranchList := TCktTree.Create;     {Instantiates ZoneEndsList, too}

  // Get Started
    if Assigned(MeteredElement) then
        BranchList.New := MeteredElement
    else
    begin   // oops
        DoSimpleMsg('Metered Element for EnergyMeter ' + Name + ' not defined.', 527);
        Exit;
    end;

  {Initialize SensorObj property of the first branch to this TMeterElement Object.
   Before starting, all sensorObj definitions are cleared in PCElements and PDElements. The
   SensorObj property is passed down to the Load objects for LoadAllocation and State Estimation
  }
    if MeteredElement is TPDElement then
        with TPDElement(MeteredElement) do
        begin
            SensorObj := Self;
            MeterObj := Self;
        end
    else
    if MeteredElement is TPCElement then
        with TPCElement(MeteredElement) do
        begin
            SensorObj := Self;
            MeterObj := Self;
        end;


    MeteredElement.Terminals^[MeteredTerminal].Checked := TRUE;
    with BranchList.PresentBranch do
    begin
    // This bus is the head of the feeder or zone; do not mark as radial bus
        FromBusReference := MeteredElement.Terminals^[MeteredTerminal].BusRef;
        ActiveCircuit.Buses^[FromBusReference].DistFromMeter := 0.0;
        VoltBaseIndex := AddToVoltBaseList(FromBusReference);
        FromTerminal := MeteredTerminal;
        if MeteredElement is TPDElement then
            TPDElement(MeteredElement).FromTerminal := MeteredTerminal;
    end;

  // Check off this element so we don't use it  again
    with MeteredElement do
    begin
        Checked := TRUE;
        IsIsolated := FALSE;
    end;

  // Make SequenceList for use in reliability calcs or anything that
  // needs to run through the tree quickly in a radial sequence
    if Assigned(SequenceList) then
        SequenceList.Free;
    SequenceList := TDSSPointerList.Create(1024); //make it a big initial allocation
    if Assigned(LoadList) then
        LoadList.Free;
    LoadList := TDSSPointerList.Create(1024); //make it a big initial allocation

  // Now start looking for other branches
  // Finds any branch connected to the TestBranch and adds it to the list
  // Goes until end of circuit, another energy meter, an open terminal, or disabled device.
    ActiveBranch := MeteredElement;

  { ****************  MAIN LOOP *****************************}
    while ActiveBranch <> NIL do
    begin

        Sequencelist.Add(ActiveBranch); // When done, this should be the correct order.

        with BranchList.PresentBranch do
        begin
            IsLoopedHere := FALSE;
            IsParallel := FALSE;
            IsDangling := TRUE;  // Unless we find something connected to it
            VoltBaseIndex := AddToVoltBaseList(FromBusReference);
        end;

        TPDElement(ActiveBranch).BranchNumCustomers := 0;   // Init counter

        for iTerm := 1 to ActiveBranch.Nterms do
        begin
            if not ActiveBranch.Terminals^[iTerm].Checked then
                with ActiveCircuit do
                begin
        // Now find all loads and generators connected to the bus on this end of branch
        // attach them as generic objects to cktTree node.
                    TestBusNum := ActiveBranch.Terminals^[iTerm].BusRef;
                    with BranchList.PresentBranch do
                    begin
                        ToBusReference := TestBusNum;   // Add this as a "to" bus reference
                        if isLineElement(ActiveBranch)   // Convert to consistent units (km)
                        then
                            Buses^[TestBusNum].DistFromMeter := Buses^[FromBusReference].DistFromMeter + TLineObj(ActiveBranch).Len * ConvertLineUnits(TLineObj(ActiveBranch).LengthUnits, UNITS_KM)
                        else
                            Buses^[TestBusNum].DistFromMeter := Buses^[FromBusReference].DistFromMeter;
                    end;

                    adjLst := BusAdjPC[TestBusNum];
                    for iPC := 0 to adjLst.Count - 1 do
                    begin
                        pPCelem := adjLst[iPC];
            //  IF pPCelem.Enabled Then Begin   only enabled elements in the search list
                        if not pPCelem.Checked then
                        begin
                            ; // skip ones we already checked
                            BranchList.PresentBranch.IsDangling := FALSE;   // Something is connected here
                // Is this a load or a generator or a Capacitor or reactor??
                            PCElementType := (pPCelem.DSSObjType and CLASSMASK);
                            if (PCElementType = LOAD_ELEMENT) or (PCElementType = GEN_ELEMENT) or (PCElementType = PVSYSTEM_ELEMENT) or (PCElementType = STORAGE_ELEMENT) or (PCElementType = CAP_ELEMENT)  // Capacitor and Reactor put on the PC list if IsShunt=TRUE
                                or (PCElementType = REACTOR_ELEMENT) then
                            begin
                                BranchList.NewObject := pPCelem; // This adds element to the Shunt list in CktTree
                                pPCelem.Checked := TRUE;    // So we don't pick this element up again
                                pPCelem.IsIsolated := FALSE;
                                pPCelem.ActiveTerminalIdx := 1;
                      {Totalize Number of Customers if Load Type}
                                if (pPCelem is TLoadObj) then
                                begin
                                    pLoad := pPCelem as TLoadObj;
                                    Inc(TPDElement(ActiveBranch).BranchNumCustomers, pLoad.NumCustomers);
                                    LoadList.Add(pPCElem);  // Add to list of loads in this zone.)
                                end;
                      {If object does not have a sensor attached, it acquires the sensor of its parent branch}
                                if not pPCelem.HasSensorObj then
                                    pPCelem.SensorObj := TPDElement(ActiveBranch).SensorObj;
                                pPCelem.MeterObj := Self;
                            end; {IF}
                        end;
                    end;

        // Now find all branches connected to this bus that we haven't found already
        // Do not include in this zone if branch has open terminals or has another meter

                    if DefinedZoneListSize = 0 then
                    begin  // Search tree for connected branches (default)
                        IsFeederEnd := TRUE;
                        adjLst := BusAdjPD[TestBusNum];
                        for iPD := 0 to adjLst.Count - 1 do
                        begin
                            TestElement := adjLst[iPD];  // Only enabled objects are in this list
            // **** See ResetMeterZonesAll
                            if not (TestElement = ActiveBranch) then  // Skip self
                                if not TestElement.HasEnergyMeter then
                                begin  // Stop at other meters  so zones don't interfere
                                    for j := 1 to TestElement.Nterms do
                                    begin     // Check each terminal
                                        if TestBusNum = TestElement.Terminals^[j].BusRef then
                                        begin
                                            BranchList.PresentBranch.IsDangling := FALSE; // We found something it was connected to
                    {Check for loops and parallel branches and mark them}
                                            if (TestElement.Checked) then     {This branch is on some meter's list already }
                                                with BranchList.PresentBranch do
                                                begin
                                                    IsLoopedHere := TRUE; {It's a loop}
                                                    LoopLineObj := TestElement;
                                                    if IsLineElement(ActiveBranch) and IsLineElement(TestElement) then
                                                        if CheckParallel(ActiveBranch, TestElement) then
                                                            IsParallel := TRUE; {It's paralleled with another line}
                                                end
                                            else
                                            begin  // push TestElement onto stack and set properties
                                                IsFeederEnd := FALSE;  // for interpolation
                                                BranchList.AddNewChild(TestElement, TestBusNum, j);  // Add new child to the branchlist
                                                with TestElement do
                                                begin
                                                    Terminals^[j].Checked := TRUE;
                                                    FromTerminal := j;
                                                    Checked := TRUE;
                                                    IsIsolated := FALSE;
                                  {Branch inherits sensor of upline branch if it doesn't have its own}
                                                    if not HasSensorObj then
                                                        SensorObj := TPDElement(ActiveBranch).SensorObj;
                                                    MeterObj := Self;   // Set meterobj to this meter
                                                    ParentPDElement := TPDElement(ActiveBranch);  // record the parent so we can easily back up for reconductoring, etc.
                                                end;
                                                Break;
                                            end; {Else}
                                        end; {IF TestBusNum}
                                    end;  {FOR terminals}
                                end; {ELSE}
                        end; {FOR iPD}

                        if IsFeederEnd then
                            BranchList.ZoneEndsList.Add(BranchList.PresentBranch, TestBusNum);
             {This is an end of the feeder and testbusnum is the end bus}
                    end
                    else
                    begin   // Zone is manually specified; Just add next element in list as a child
                        Inc(ZoneListCounter);
                        while ZoneListCounter <= DefinedZoneListSize do
                        begin
                            if SetElementActive(DefinedZoneList^[ZoneListCounter]) = 0 then
                                Inc(ZoneListCounter) // Not Found. Let's search for another
                            else
                            begin
                                TestElement := ActiveCktElement as TPDElement;
                                if not TestElement.Enabled then
                                    Inc(ZoneListCounter)  // Lets ignore disabled devices
                                else
                                begin
                                    if (TestElement.DSSObjType and BaseClassMask) <> PD_ELEMENT then
                                        Inc(ZoneListCounter)  // Lets ignore non-PD elements
                                    else
                                        BranchList.AddNewChild(TestElement, 0, 0); // add it as a child to the previous element
                                    Break;                                         // Can't do reductions if manually spec'd
                                end;
                            end;
                        end; // while
                    end;
                end;  {WITH Active Circuit}
        end;   {FOR iTerm}

        ActiveBranch := BranchList.GoForward;   // Sets PresentBranch
  { ****************  END MAIN LOOP *****************************}
    end;

    TotalupDownstreamCustomers;

    AssignVoltBaseRegisterNames;
end;

{--------------------------------------------------------------------------}
procedure TEnergyMeterObj.GetCurrents(Curr: pComplexArray);  //Get present value of terminal Curr FOR reports

var
    i: Integer;

begin
    for i := 1 to Fnconds do
        Curr^[i] := CZERO;
end;

{--------------------------------------------------------------------------}
procedure TEnergyMeterObj.GetInjCurrents(Curr: pComplexArray);

var
    i: Integer;

begin
    for i := 1 to Fnconds do
        Curr^[i] := CZERO;
end;

{--------------------------------------------------------------------------}

procedure TEnergyMeterObj.ZoneDump;

var
    CSVName: String;
    F: TFileStream = nil;
    pdelem: TPDelement;
    LoadElem: TDSSCktElement;

begin

    try

        CSVName := 'Zone_' + Name + '.CSV';
        F := TFileStream.Create(GetOutputDirectory + CSVName, fmCreate);

        GlobalResult := CSVName;
        SetLastResultFile(CSVName);

    except

        On E: Exception do
        begin
            DoSimpleMsg('Error opening File "' + CSVName + '": ' + E.Message, 528);
            FreeAndNil(F);
            Exit;
        end;

    end;

    try
        FSWriteln(F, 'Level, Branch, Bus1, Bus2, Distance');
        if BranchList <> NIL then
        begin
            PDElem := BranchList.First;
            while PDElem <> NIL do
                with ActiveCircuit do
                begin
                    FSWriteln(F, Format('%d, %s.%s, %s, %s, %10.4f',
                        [BranchList.Level, PDelem.ParentClass.Name, PDelem.Name,
                        PDelem.FirstBus, PDelem.NextBus,
                  {BusList.Get(BranchList.PresentBranch.ToBusReference),}
                        Buses^[BranchList.PresentBranch.ToBusReference].DistFromMeter]));
                    LoadElem := Branchlist.FirstObject;
                    while LoadElem <> NIL do
                    begin
                        FSWrite(F, '-1, ');
                        FSWriteln(F, Format('%s.%s, %s', [LoadElem.ParentClass.Name, LoadElem.Name, LoadElem.Firstbus{ActiveCircuit.BusList.Get(BranchList.PresentBranch.ToBusReference)}]));
                        LoadElem := BranchList.NextObject
                    end;
                    PDElem := BranchList.GoForward;
                end;
        end;

    finally
        FreeAndNil(F);
    end;
end;


//- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
procedure TEnergyMeterObj.DumpProperties(F: TFileStream; Complete: Boolean);

var
    i: Integer;
    pdelem: TPDelement;
    LoadElem: TDSSCktElement;


begin
    inherited DumpProperties(F, complete);

    with ParentClass do
        for i := 1 to NumProperties do
            case i of
                4:
                begin     // option
                    FSWrite(F, '~ ', PropertyName^[i], '=(');
                    if ExcessFlag then
                        FSWrite(F, 'E,')
                    else
                        FSWrite(F, 'T,');
                    if ZoneIsRadial then
                        FSWrite(F, ' R,')
                    else
                        FSWrite(F, ' M,');
                    if VoltageUEOnly then
                        FSWrite(F, ' V')
                    else
                        FSWrite(F, ' C');
                    FSWriteln(F, ')');
                end;
                7:
                    FSWriteln(F, '~ ' + PropertyName^[i] + '=(' + PropertyValue[i] + ')');
            else
                FSWriteln(F, '~ ' + PropertyName^[i] + '=' + PropertyValue[i]);
            end;

    if complete then
    begin

        FSWriteln(F, 'Registers');
        for i := 1 to NumEMregisters do
        begin
            FSWriteln(F, Format('"%s" = %.0g', [RegisterNames[i], Registers[i]]));
        end;
        FSWriteln(F);

        FSWriteln(F, 'Branch List:');
        if BranchList <> NIL then
        begin
            PDElem := BranchList.First;
            while PDElem <> NIL do
            begin
                FSWriteln(F, 'Circuit Element = ', PDelem.Name);
                LoadElem := Branchlist.FirstObject;
                while LoadElem <> NIL do
                begin
                    FSWriteln(F, '   Shunt Element = ' + LoadElem.ParentClass.name + '.' + LoadElem.Name);
                    LoadElem := BranchList.NextObject
                end;
                PDElem := BranchList.GoForward;
            end;
        end;

    end;

end;

procedure TEnergyMeter.ProcessOptions(const Opts: String);

var
    {S1,} S2: String;
begin

    AuxParser.CmdString := Opts;  // Load up aux Parser

    {Loop until no more options found}
    with ActiveEnergymeterObj do
        repeat
            {S1 := }AuxParser.NextParam; // ignore any parameter name  not expecting any
            S2 := lowercase(AuxParser.StrValue);
            if Length(S2) > 0 then
                case s2[1] of
                    'e':
                        ExcessFlag := TRUE;
                    't':
                        ExcessFlag := FALSE;
                    'r':
                        ZoneIsRadial := TRUE;
                    'm':
                        ZoneIsRadial := FALSE;
                    'c':
                        VoltageUEOnly := FALSE;
                    'v':
                        VoltageUEOnly := TRUE;
                end;
        until Length(S2) = 0;

end;

function TEnergyMeterObj.AddToVoltBaseList(BusRef: Integer): Integer;
{Add to VoltBase list if not already there and return index}
var
    i: Integer;

begin
    with ActiveCircuit.Buses^[BusRef] do
    begin
        for i := 1 to VBaseCount do
        begin
            if abs(1.0 - kVBase / VBaseList^[i]) < 0.01 then
            begin    // < 1% difference
                Result := i;
                Exit;
            end;
        end;

        if (kvBase > 0.0) and (VBaseCount < MaxVBaseCount) then
        begin
            Inc(VBaseCount);
            VBaseList^[VBasecount] := {ActiveCircuit.Buses^[BusRef].}kVBase;
            result := VBaseCount;
        end
        else
            Result := 0;
    end;

end;

procedure TEnergyMeterObj.AllocateLoad;

var
    ConnectedPhase: Integer;
    CktElem: TPDElement;
    LoadElem: TLoadobj;

begin


{PREREQUISITE: EXECUTE CALCALLOCATIONFACTORS FOR ALL ENERGYMETERS AND SENSORS}
{****Done in calling procedure  now ***   CalcAllocationFactors;}     {for this meter. Inherited from Meterelement}
{See ExecHelper}

    { Now go through the meter's zone and adjust the loads.

      While the AllocationFactor property is adjusted for all loads, it will only
      have an effect on loads defined with either the XFKVA property or the
      kWh property.

      Loads have a SensorObj property that points to its upstream sensor that has the adjustments for
      the allocation factors.  This is established in the MakeMeterZoneLists proc in this Unit.

      Sensors consist of EnergyMeters, which drive the load allocation process and Sensor objects that
      are simply voltage and current measuring points.  A Sensor may be attached to a line or transformer
      or it may be connected directly to a load.
     }


    CktElem := BranchList.First;
    while CktElem <> NIL do
    begin
        LoadElem := Branchlist.FirstObject;
        while (LoadElem <> NIL) do
        begin
            if (LoadElem.DSSObjType and CLASSMASK) = LOAD_ELEMENT then  // only for loads not other shunts
                case LoadElem.NPhases of
                 {For Single phase loads, allocate based on phase factor, else average factor}
                    1:
                        with LoadElem do
                        begin
                            ConnectedPhase := ActiveCircuit.MapNodeToBus^[NodeRef^[1]].NodeNum;
                            if (ConnectedPhase > 0) and (ConnectedPhase < 4)   // Restrict to phases 1..3
                            then
                                AllocationFactor := AllocationFactor * SensorObj.PhsAllocationFactor^[ConnectedPhase];
                        end;
                else
                    with LoadElem do
                        AllocationFactor := AllocationFactor * SensorObj.AvgAllocFactor;
                end;  {CASE}
            LoadElem := BranchList.NextObject    {Next load at this bus}
        end;   {While Loadelem}
        CktElem := BranchList.GoForward;    {Go on down the tree}
    end;  {While CktElem}

end;

procedure TEnergyMeterObj.InitPropertyValues(ArrayOffset: Integer);
var
    i: Integer;
    S: String;
begin

    PropertyValue[1] := ''; //'element';
    PropertyValue[2] := '1'; //'terminal';
    PropertyValue[3] := 'clear'; //'action';
    PropertyValue[4] := '(E, R, C)'; //'Option';
    PropertyValue[5] := '0.0'; //'kWnormal';
    PropertyValue[6] := '0.0'; //'kwEmerg';
    PropertyValue[7] := '(400, 400, 400)'; //'PeakCurrent';
    PropertyValue[8] := ''; // ZoneList
    PropertyValue[9] := 'No';
     {Define mask as 1 for all registers}
    S := '[';
    for i := 1 to NumEMregisters do
        S := S + '1 ';
    PropertyValue[10] := S + ']';
    PropertyValue[11] := 'Yes';
    PropertyValue[12] := 'Yes';
    PropertyValue[13] := 'Yes';
    PropertyValue[14] := 'Yes';
    PropertyValue[15] := 'Yes'; // segregate losses by voltage base
    PropertyValue[16] := 'Yes';
    PropertyValue[17] := 'No';
    PropertyValue[18] := '0';
    PropertyValue[19] := '0';
    PropertyValue[20] := '0';
    PropertyValue[21] := '0';
    PropertyValue[22] := '0';
    PropertyValue[23] := '0';
    PropertyValue[24] := '0';


    inherited  InitPropertyValues(NumPropsThisClass);

end;

procedure TEnergyMeterObj.Accumulate_Gen;
var
    S: Complex;
begin
     //----pGen.ActiveTerminalIdx := 1;
    S := Cnegate(CmulReal(pGen.Power[1], 0.001));
    TotalZonekw := TotalZonekW + S.re;
    TotalZonekvar := TotalZonekvar + S.im;

end;

function TEnergyMeterObj.Accumulate_Load(pLoad: TLoadObj;
    var TotalZonekW, TotalZonekvar, TotalLoad_EEN, TotalLoad_UE: Double): Double;
var
    S_Load: Complex;
    kW_Load: Double;
    Load_EEN,
    Load_UE: Double;
begin
    with   pLoad do
    begin
       //----ActiveTerminalIdx := 1;
        S_Load := CmulReal(pLoad.Power[1], 0.001);   // Get Power in Terminal 1
        kW_Load := S_Load.re;
        Result := kw_Load;

       {Accumulate load in zone}
        TotalZonekw := TotalZonekW + kW_Load;
        TotalZonekvar := TotalZonekvar + S_Load.im;

       {always integrate even if the value is 0.0
        otherwise the Integrate function is not correct}
       {Invoking the ExceedsNormal and Unserved Properties causes the factors to be computed}
        if ExcessFlag then
        begin   // Return Excess load as EEN/UE
            if (ExceedsNormal) then
                Load_EEN := kW_Load * EEN_Factor
            else
                Load_EEN := 0.0;
            if (Unserved) then
                Load_UE := kW_Load * UE_Factor
            else
                Load_UE := 0.0;
        end
        else
        begin    // Return TOTAL load as EEN/UE
            if (ExceedsNormal) then
                Load_EEN := kW_Load
            else
                Load_EEN := 0.0;
            if (Unserved) then
                Load_UE := kW_Load
            else
                Load_UE := 0.0;
        end;

        TotalLoad_EEN := TotalLoad_EEN + Load_EEN;
        TotalLoad_UE := TotalLoad_UE + Load_UE;

    end; {WITH}
end;


procedure TEnergyMeterObj.ReduceZone;

{Reduce the zone by merging lines}

begin
 // Make  sure zone list is built
    if not assigned(BranchList) then
        MakeMeterZoneLists;

    case ActiveCircuit.ReductionStrategy of

        rsShortlines:
            DoReduceShortLines(BranchList);    {See ReduceAlgs.Pas}
         {rsTapEnds:       DoReduceTapEnds (BranchList);}
        rsMergeParallel:
            DoMergeParallelLines(BranchList);
        rsDangling:
            DoReduceDangling(BranchList);
        rsBreakLoop:
            DoBreakLoops(BranchList);
        rsSwitches:
            DoReduceSwitches(BranchList);
        rsLaterals:
            DoRemoveAll_1ph_Laterals(BranchList);

    else
       {Default}
        DoReduceDefault(BranchList);
    end;
end;

function TEnergyMeterObj.CheckBranchList(code: Integer): Boolean;
begin
    if not Assigned(BranchList) then
    begin
        Result := FALSE;
        DoSimpleMsg('Meter Zone Lists need to be built. Do Solve or Makebuslist first!', code);
        Exit;
    end;
    Result := TRUE;
end;


procedure TEnergyMeterObj.InterpolateCoordinates;
{Start at the ends of the zone and work toward the start
 interpolating between known coordinates}
var
    i, BusRef,
    FirstCoordRef, SecondCoordRef,
    Linecount: Integer;
    PresentNode, StartNode: TCktTreeNode;
    CktElem: TDSSCktElement;

begin
    if not CheckBranchList(529) then
        Exit;

    with ActiveCircuit do
    begin

        for i := 1 to Branchlist.ZoneEndsList.NumEnds do
        begin
            Busref := Branchlist.ZoneEndsList.Get(i, PresentNode);

            FirstCoordRef := BusRef;
            SecondCoordRef := FirstCoordRef;  {so compiler won't issue stupid warning}
       {Find a bus with a coordinate}
            if not Buses^[BusRef].CoordDefined then
            begin
                while not Buses^[PresentNode.FromBusReference].CoordDefined do
                begin
                    PresentNode := PresentNode.ParentBranch;
                    if PresentNode = NIL then
                        Break;
                end;
                if PresentNode <> NIL then
                    FirstCoordRef := PresentNode.FromBusReference;
            end;

            while PresentNode <> NIL do
            begin
          {Back up until we find another Coord defined}
                LineCount := 0;   {number of line segments in this segment}
                StartNode := PresentNode;
                CktElem := PresentNode.CktObject;
                if FirstCoordRef <> PresentNode.FromBusReference then
                begin   {Handle special case for end branch}
                    if Buses^[PresentNode.FromBusReference].CoordDefined then
                        FirstCoordRef := PresentNode.FromBusReference
                    else
                        Inc(LineCount);
                end;

                repeat
                    CktElem.Checked := TRUE;
                    PresentNode := PresentNode.ParentBranch;
                    if PresentNode = NIL then
                        Break;
                    CktElem := PresentNode.CktObject;
                    SecondCoordRef := PresentNode.FromBusReference;
                    Inc(LineCount);
                until Buses^[SecondCoordRef].CoordDefined or CktElem.Checked;

                if (PresentNode <> NIL) and (LineCount > 1) then
                    if Buses^[SecondCoordRef].CoordDefined then
                    begin
                        CalcBusCoordinates(StartNode, FirstCoordRef, SecondCoordRef, LineCount);
                    end
                    else
                        Break; {While - went as far as we could go this way}

                FirstCoordRef := SecondCoordRef;
            end;

        end; {For}

    end; {With}

end;

procedure TEnergyMeterObj.CalcBusCoordinates(StartBranch: TCktTreeNode;
    FirstCoordRef, SecondCoordref, LineCount: Integer);

var
    X, Y, Xinc, Yinc: Double;
begin

    if LineCount = 1 then
        Exit;  {Nothing to do!}

    with ActiveCircuit do
    begin
        Xinc := (Buses^[FirstCoordref].X - Buses^[SecondCoordRef].X) / LineCount;
        Yinc := (Buses^[FirstCoordref].Y - Buses^[SecondCoordRef].Y) / LineCount;

        X := Buses^[FirstCoordref].X;
        Y := Buses^[FirstCoordref].Y;

       {Either start with the "to" end of StartNode or the "from" end;}
        if FirstCoordRef <> StartBranch.FromBusReference then
        begin  // Start with "to" end
            X := X - Xinc;
            Y := Y - Yinc;
            Buses^[StartBranch.FromBusReference].X := X;
            Buses^[StartBranch.FromBusReference].Y := Y;
            Buses^[StartBranch.FromBusReference].CoordDefined := TRUE;
            Dec(LineCount);
        end;

        while LineCount > 1 do
        begin
            X := X - Xinc;
            Y := Y - Yinc;
            StartBranch := StartBranch.ParentBranch; // back up the tree
            Buses^[StartBranch.FromBusReference].X := X;
            Buses^[StartBranch.FromBusReference].Y := Y;
            Buses^[StartBranch.FromBusReference].CoordDefined := TRUE;
            Dec(LineCount);
        end;

    end;
end;


{--------------------------- CalcReliabilityIndices ----------------------------}

procedure TEnergyMeterObj.CalcReliabilityIndices(AssumeRestoration: Boolean);
var
    PD_Elem: TPDElement;
    pSection: ^TFeederSection;
    idx: Integer;
    pBus: TDSSBus;
    dblNcusts: Double;
    dblkW: Double;

begin

    if not Assigned(SequenceList) then
    begin
        DoSimpleMsg('Energymeter.' + Name + ' Zone not defined properly.', 52901);
        Exit;
    end;

    // Zero reliability accumulators
    for idx := SequenceList.ListSize downto 1 do
        TPDElement(SequenceList.Get(idx)).ZeroReliabilityAccums;

    // Backward sweep calculating failure rates
    for idx := SequenceList.ListSize downto 1 do
    begin
        with TPDElement(SequenceList.Get(idx)) do
        begin
            CalcFltRate;    // Calc failure rate for this element
            AccumFltRate;
        end;
    end;

    // Forward sweep to get number of interruptions
       // Initialize number of interruptions and Duration
    PD_Elem := TPDElement(SequenceList.Get(1));
    pBus := ActiveCircuit.Buses^[PD_Elem.Terminals^[PD_Elem.FromTerminal].BusRef];
    pBus.Bus_Num_Interrupt := Source_NumInterruptions;
    pBus.BusCustInterrupts := Source_NumInterruptions * pBus.BusTotalNumCustomers;
    pBus.Bus_Int_Duration := Source_IntDuration;

       // init for defining sections
    SectionCount := 0;
    pBus.BusSectionID := SectionCount; // section before 1st OCP device is zero

    for idx := 1 to SequenceList.ListSize do
        TPDElement(SequenceList.Get(idx)).CalcNum_Int(SectionCount, AssumeRestoration);

    if SectionCount = 0 then
    begin // Error - no OCP devices
        DoSimpleMsg
        ('Error: No Overcurrent Protection device (Relay, Recloser, or Fuse) defined. Aborting Reliability calc.',
            52902);
        Exit;
    end;

       // Now have number of sections  so allocate FeederSections array
    ReallocMem(FeederSections, SizeOf(FeederSections^[1]) * SectionCount);
    for idx := 1 to SectionCount do
        with FeederSections^[idx] do      // Initialize all Section data
        begin
            OCPDeviceType := 0; // 1=Fuse; 2=Recloser; 3=Relay
            AverageRepairTime := 0.0;
            SumFltRatesXRepairHrs := 0.0;
            SumBranchFltRates := 0.0;
            NCustomers := 0;
            TotalCustomers := 0;
            SectFaultRate := 0.0;
            NBranches := 0;
            SeqIndex := 0;
        end;

    // Now do Backward sweep calculating N*Fault rates
    for idx := SequenceList.ListSize downto 1 do
    begin
        PD_Elem := SequenceList.Get(idx);
        PD_Elem.CalcCustInterrupts;

        if PD_Elem.BranchSectionID <= 0 then
            continue;

        // Populate the Section properties
        pSection := @FeederSections^[PD_Elem.BranchSectionID];
        Inc(pSection.NCustomers, PD_Elem.BranchNumCustomers); // Sum up num Customers on this Section
        Inc(pSection.NBranches, 1); // Sum up num branches on this Section
        pBus := ActiveCircuit.Buses^[PD_Elem.Terminals^[PD_Elem.ToTerminal].BusRef];
        DblInc(pSection.SumBranchFltRates, pBus.Bus_Num_Interrupt * PD_Elem.BranchFltRate);
        DblInc(pSection.SumFltRatesXRepairHrs, (pBus.Bus_Num_Interrupt * PD_Elem.BranchFltRate * PD_Elem.HrsToRepair));
        if PD_Elem.HasOCPDevice then
        begin  // set Section properties
            pSection.OCPDeviceType := GetOCPDeviceType(PD_Elem);
            pSection.SeqIndex := idx;  // index of pdelement with OCP device at head of section
            pSection.TotalCustomers := PD_Elem.BranchTotalCustomers;
            pSection.SectFaultRate := PD_Elem.AccumulatedBrFltRate;
        end;

{$IFDEF DEBUG}
        if idx = SequenceList.ListSize then
            WriteDLLDebugFile
            ('Meter, SectionID, BranchName, FaultRate, AccumulatedBrFltRate, BranchFltRate, RepairHrs, NCustomers, Num_Interrupt');
        with FeederSections^[PD_Elem.BranchSectionID] do
            WriteDLLDebugFile
            (Format('%s.%s, %d, %s.%s, %.11g, %.11g, %.11g, %.11g, %d, %.11g ',
                [ParentClass.Name, Name, PD_Elem.BranchSectionID,
                PD_Elem.ParentClass.Name, PD_Elem.Name, PD_Elem.FaultRate,
                PD_Elem.AccumulatedBrFltRate, PD_Elem.BranchFltRate,
                PD_Elem.HrsToRepair, PD_Elem.BranchNumCustomers,
                pBus.Bus_Num_Interrupt]));
{$ENDIF}

    end;

        {Compute Avg Interruption duration of each Section }
    for idx := 1 to SectionCount do
        with FeederSections^[idx] do
            AverageRepairTime := SumFltRatesXRepairHrs / SumBranchFltRates;

        { Set Bus_int_Duration}

    with ActiveCircuit do
        for idx := 1 to NumBuses do
        begin
            pBus := Buses^[idx];
            if pBus.BusSectionID > 0 then
                pBus.Bus_Int_Duration := Source_IntDuration + FeederSections^
                    [pBus.BusSectionID].AverageRepairTime;
        end;

{$IFDEF DEBUG}
    WriteDLLDebugFile
    ('Meter, SectionID, NBranches, NCustomers, AvgRepairHrs, AvgRepairMins, FailureRate*RepairtimeHrs, SumFailureRates');
    for idx := 1 to SectionCount do
        with FeederSections^[idx] do
            WriteDLLDebugFile(Format('%s.%s, %d, %d, %d, %.11g, %.11g, %.11g, %.11g ',
                [ParentClass.Name, Name, idx, NBranches, NCustomers, AverageRepairTime,
                AverageRepairTime * 60.0, SumFltRatesXRepairHrs, SumBranchFltRates]));
{$ENDIF}

       {Compute SAIFI based on numcustomers and load kW}
       {SAIFI is weighted by specified load weights}
       {SAIFI is for the EnergyMeter Zone}
    SAIFI := 0.0;
    CAIDI := 0.0;
    SAIFIkW := 0.0;
    dblNcusts := 0.0;
    dblkW := 0.0;
    CustInterrupts := 0.0;

       // Use LoadList for SAIFI calculation
    with ActiveCircuit do
    begin
        for idx := 1 to LoadList.ListSize do  // all loads in meter zone
        begin
      // Compute CustInterrupts based on interrupts at each load
            with TLoadObj(LoadList.Get(idx)) do
            begin
                pBus := Buses^[Terminals^[1].BusRef]; // pointer to Load's bus
                CustInterrupts := CustInterrupts + (NumCustomers * RelWeighting * pBus.Bus_Num_Interrupt);
                SAIFIkW := SAIFIkW + kWBase * RelWeighting * pBus.Bus_Num_Interrupt;
                DblInc(dblNcusts, NumCustomers * RelWeighting);
          // total up weighted numcustomers
                DblInc(dblkW, kWBase * RelWeighting); // total up weighted kW
          // Set BusCustDurations for Branch reliability export
                pBus.BusCustDurations := (pBus.BusTotalNumCustomers + NumCustomers) *
                    RelWeighting * pBus.Bus_Int_Duration * pBus.Bus_Num_Interrupt;
            end;
        end;
    end;

     // Compute SAIDI from Sections list
    SAIDI := 0.0;
    for idx := 1 to SectionCount do
        with FeederSections^[idx] do
        begin
            SAIDI := SAIDI + SectFaultRate * AverageRepairTime * TotalCustomers;
        end;

    if dblNcusts > 0.0 then
    begin
        SAIFI := CustInterrupts / dblNcusts; // Normalize to total number of customers
        SAIDI := SAIDI / dblNcusts; // Normalize to total number of customers
    end;

    if SAIFI > 0.0 then
        CAIDI := SAIDI / SAIFI;

    if dblkW > 0.0 then
        SAIFIkW := SAIFIkW / dblkW; // Normalize to total kW

end;

{-------------------------------------------------------------------------------}
function TEnergyMeterObj.GetPropertyValue(Index: Integer): String;
begin
    case Index of
        4, 7:
            Result := '(';
    else
        Result := '';
    end;

    case Index of
        4:
        begin     // option
            if ExcessFlag then
                Result := Result + 'E,'
            else
                Result := Result + 'T,';
            if ZoneIsRadial then
                Result := Result + ' R,'
            else
                Result := Result + ' M,';
            if VoltageUEOnly then
                Result := Result + ' V'
            else
                Result := Result + ' C';
        end;
        20:
            Result := Format('%.11g', [SAIFI]);
        21:
            Result := Format('%.11g', [SAIFIkW]);
        22:
            Result := Format('%.11g', [SAIDI]);
        23:
            Result := Format('%.11g', [CAIDI]);
        24:
            Result := Format('%.11g', [CustInterrupts]);
    else
        Result := Result + inherited GetPropertyValue(index);
    end;

    case Index of
        4, 7:
            Result := Result + ')';
    else
    end;
end;

procedure TEnergyMeterObj.SaveZone(const dirname: String);
var
    cktElem, shuntElement: TDSSCktElement;
    LoadElement: TLoadObj;
    pControlElem: TDSSCktElement;
    FBranches, FShunts, FLoads, FGens, FCaps, FXfmrs: TFileStream;
    NBranches, NShunts, Nloads, NGens, NCaps, NXfmrs: Integer;
begin
 {We are in the directory indicated by dirname}

{Run down the zone and write each element into a file}

    if BranchList = NIL then
        Exit;

    FBranches := nil;
    FShunts := nil;
    FLoads := nil;
    FGens := nil;
    FCaps := nil; 
    FXfmrs := nil;
        
    {Open some files:}

    try
        FBranches := TFileStream.Create(CurrentDSSDir + 'Branches.dss', fmCreate);     // Both lines and transformers
        NBranches := 0;
    except
        On E: Exception do
        begin
            DoSimpleMsg('Error creating Branches.dss for Energymeter: ' + Self.Name + '. ' + E.Message, 530);
            FreeAndNil(FBranches);
            Exit;
        end;
    end;

    try
        FXfmrs := TFileStream.Create(CurrentDSSDir + 'Transformers.dss', fmCreate);     // Both lines and transformers
        NXfmrs := 0;
    except
        On E: Exception do
        begin
            DoSimpleMsg('Error creating Transformers.dss for Energymeter: ' + Self.Name + '. ' + E.Message, 53001);
            FreeAndNil(FXfmrs);
            Exit;
        end;
    end;

    try
        FShunts := TFileStream.Create(CurrentDSSDir + 'Shunts.dss', fmCreate);
        NShunts := 0;
    except
        On E: Exception do
        begin
            DoSimpleMsg('Error creating Shunts.dss for Energymeter: ' + Self.Name + '. ' + E.Message, 531);
            FreeAndNil(FShunts);
            Exit;
        end;
    end;

    try
        FLoads := TFileStream.Create(CurrentDSSDir + 'Loads.dss', fmCreate);
        Nloads := 0;
    except
        On E: Exception do
        begin
            DoSimpleMsg('Error creating Loads.dss for Energymeter: ' + Self.Name + '. ' + E.Message, 532);
            FreeAndNil(FLoads);
            Exit;
        end;
    end;

    try
        FGens := TFileStream.Create(CurrentDSSDir + 'Generators.dss', fmCreate);
        NGens := 0;
    except
        On E: Exception do
        begin
            DoSimpleMsg('Error creating Generators.dss for Energymeter: ' + Self.Name + '. ' + E.Message, 533);
            FreeAndNil(FGens);
            Exit;
        end;
    end;

    try
        FCaps := TFileStream.Create(CurrentDSSDir + 'Capacitors.dss', fmCreate);
        Ncaps := 0;
    except
        On E: Exception do
        begin
            DoSimpleMsg('Error creating Capacitors.dss for Energymeter: ' + Self.Name + '. ' + E.Message, 534);
            FreeAndNil(FCaps);
            Exit;
        end;
    end;


    cktElem := BranchList.First;
    with ActiveCircuit do
        while cktElem <> NIL do
        begin
            if CktElem.Enabled then
            begin
                ActiveCktElement := cktElem;

                if (CktElem.DSSObjType and Classmask) = XFMR_ELEMENT then
                begin
                    Inc(NXfmrs);
                    WriteActiveDSSObject(FXfmrs, 'New');     // sets HasBeenSaved := TRUE
                    if cktElem.HasControl then
                    begin
                        pControlElem := cktElem.ControlElementList.First;
                        while pControlElem <> NIL do
                        begin
                            ActiveCktElement := pControlElem;
                            WriteActiveDSSObject(FXfmrs, 'New');  //  regulator control ...Also, relays, switch controls
                            pControlElem := cktElem.ControlElementList.Next;
                        end;
                    end;
                end
                else
                begin  {Mostly LINE elements}
                    Inc(NBranches);
                    WriteActiveDSSObject(FBranches, 'New');     // sets HasBeenSaved := TRUE
                    if cktElem.HasControl then
                    begin
                        pControlElem := cktElem.ControlElementList.First;
                        while pControlElem <> NIL do
                        begin
                            ActiveCktElement := pControlElem;
                            WriteActiveDSSObject(FBranches, 'New');  //  regulator control ...Also, relays, switch controls
                            pControlElem := cktElem.ControlElementList.Next;
                        end;
                    end;
                end;


                shuntElement := Branchlist.FirstObject;
                while shuntElement <> NIL do
                begin
                    ActiveCktElement := shuntElement;
                    if (shuntElement.DSSObjType and Classmask) = LOAD_ELEMENT then
                    begin
                        LoadElement := TLoadObj(shuntElement);
                        if LoadElement.HasBeenAllocated then
                        begin
                   {Manually set the allocation factor so it shows up}
                            Parser.CmdString := 'allocationfactor=' + Format('%-.4g', [LoadElement.AllocationFactor]);
                            LoadElement.Edit;
                        end;
                        ActiveCktElement := shuntElement; // reset in case Edit mangles it
                        Inc(NLoads);
                        WriteActiveDSSObject(FLoads, 'New');
                    end
                    else
                    if (shuntElement.DSSObjType and Classmask) = GEN_ELEMENT then
                    begin
                        Inc(NGens);
                        WriteActiveDSSObject(FGens, 'New');
                        if shuntElement.HasControl then
                        begin
                            pControlElem := shuntElement.ControlElementList.First;
                            while pControlElem <> NIL do
                            begin
                                ActiveCktElement := pControlElem;
                                WriteActiveDSSObject(FGens, 'New');
                                pControlElem := shuntElement.ControlElementList.Next;
                            end;
                        end;
                    end
                    else
                    if (shuntElement.DSSObjType and Classmask) = CAP_ELEMENT then
                    begin
                        Inc(NCaps);
                        WriteActiveDSSObject(FCaps, 'New');
                        if shuntElement.HasControl then
                        begin
                            pControlElem := shuntElement.ControlElementList.First;
                            while pControlElem <> NIL do
                            begin
                                ActiveCktElement := pControlElem;
                                WriteActiveDSSObject(FCaps, 'New');
                                pControlElem := shuntElement.ControlElementList.Next;
                            end;
                        end;
                    end
                    else
                    begin
                        Inc(NShunts);
                        WriteActiveDSSObject(Fshunts, 'New');
                    end;
                    shuntElement := BranchList.NextObject
                end;
            end; {if enabled}

            cktElem := BranchList.GoForward;
        end;{WHILE}

    FreeAndNil(FBranches);
    FreeAndNil(FXfmrs);
    FreeAndNil(Fshunts);
    FreeAndNil(FLoads);
    FreeAndNil(FGens);
    FreeAndNil(FCaps);

 {If any records were written to the file, record their relative names}
    if NBranches > 0 then
        SavedFileList.Add(dirname + PathDelim + 'Branches.dss')
    else
        DeleteFile('Branches.dss');
    if NXfmrs > 0 then
        SavedFileList.Add(dirname + PathDelim + 'Transformers.dss')
    else
        DeleteFile('Transformers.dss');
    if NShunts > 0 then
        SavedFileList.Add(dirname + PathDelim + 'Shunts.dss')
    else
        DeleteFile('Shunts.dss');
    if NLoads > 0 then
        SavedFileList.Add(dirname + PathDelim + 'Loads.dss')
    else
        DeleteFile('Loads.dss');
    if NGens > 0 then
        SavedFileList.Add(dirname + PathDelim + 'Generators.dss')
    else
        DeleteFile('Generators.dss');
    if NCaps > 0 then
        SavedFileList.Add(dirname + PathDelim + 'Capacitors.dss')
    else
        DeleteFile('Capacitors.dss');
end;


procedure TEnergyMeterObj.GetPCEatZone(const allowEmpty: Boolean);
var
    cktElem,
    shuntElement: TDSSCktElement;
    pMeter: TEnergyMeterObj;
begin
    //TODO: if performance ever becomes an issue, rewrite to use a temporary list,
    //      or try overallocating the array first
    if ActiveCircuit = NIL then
        Exit;

    if not allowEmpty then
    begin
        SetLength(ZonePCE, 1);
        ZonePCE[0] := '';
    end
    else
        SetLength(ZonePCE, 0);

    if BranchList = NIL then
        Exit;

    with ActiveCircuit do
    begin
        cktElem := BranchList.First;
        while cktElem <> NIL do
        begin
            if CktElem.Enabled Then
            begin
                ActiveCktElement := cktElem;
                shuntElement := Branchlist.FirstObject;
                while shuntElement <> NIL do
                begin
                    ActiveCktElement := shuntElement;
                    SetLength(ZonePCE, length(ZonePCE) + 1);
                    ZonePCE[high(ZonePCE)] := shuntElement.DSSClassName + '.' + shuntElement.Name;
                    shuntElement := BranchList.NextObject;
                end;
            end;
            cktElem := BranchList.GoForward;
        end;
    end;

    if (Length(ZonePCE) = 0) and (not allowEmpty) then
    begin
        SetLength(ZonePCE, 1);
        ZonePCE[0] := '';
    end;
end;

procedure TEnergyMeterObj.SetDragHandRegister(Reg: Integer; const Value: Double);
begin
    if Value > Registers[reg] then
    begin
        Registers[reg] := Value;
        Derivatives[reg] := Value;  // Use this for   demand interval data;
    end;
end;

procedure TEnergyMeterObj.CloseDemandIntervalFile;
var
    i: Integer;
begin

    try
        if This_Meter_DIFileIsOpen then
        begin
            if DI_MHandle <> NIL then
                CloseMHandler(DI_MHandle, MakeDIFileName, DI_Append);
            DI_MHandle := NIL;
            This_Meter_DIFileIsOpen := FALSE;
            if PHV_MHandle <> NIL then
                if VPhaseReportFileIsOpen then
                    CloseMHandler(PHV_MHandle, MakeVPhaseReportFileName, PHV_Append);
            PHV_MHandle := NIL;
            VPhaseReportFileIsOpen := FALSE;
        end;
    except
        ON E: Exception do
            DoSimpleMsg('Error Closing Demand Interval file for Meter "' + Name + '"', 534);
    end;


     {Write Registers to Totals File}
    with EnergyMeterClass do
    begin
        WriteintoMemStr(EMT_MHandle, '"' + Self.Name + '"');
        for i := 1 to NumEMregisters do
            WriteintoMem(EMT_MHandle, Registers[i]);
        WriteintoMemStr(EMT_MHandle, Char(10));
    end;
end;

procedure TEnergyMeterObj.OpenDemandIntervalFile;
var
    i, j: Integer;
    vbase: Double;

begin

    try
        if This_Meter_DIFileIsOpen then
            CloseDemandIntervalFile;

        if (EnergyMeterClass.DI_Verbose) then
        begin

            This_Meter_DIFileIsOpen := TRUE;
            if DI_MHandle <> NIL then
                DI_MHandle.free;
            DI_MHandle := Create_Meter_Space('"Hour"');
            for i := 1 to NumEMRegisters do
                WriteintoMemStr(DI_MHandle, ', "' + RegisterNames[i] + '"');
            WriteintoMemStr(DI_MHandle, Char(10));

         {Phase Voltage Report, if requested}
            if FPhaseVoltageReport then
            begin
                if PHV_MHandle <> NIL then
                    PHV_MHandle.Free;
                PHV_MHandle := Create_Meter_Space('"Hour"');
                VPhaseReportFileIsOpen := TRUE;
                for i := 1 to MaxVBaseCount do
                begin
                    vbase := VBaseList^[i] * SQRT3;
                    if Vbase > 0.0 then
                    begin
                        for j := 1 to 3 do
                            WriteintoMemStr(PHV_MHandle, Format(', %.3gkV_Phs_%d_Max', [vbase, j]));
                        for j := 1 to 3 do
                            WriteintoMemStr(PHV_MHandle, Format(', %.3gkV_Phs_%d_Min', [vbase, j]));
                        for j := 1 to 3 do
                            WriteintoMemStr(PHV_MHandle, Format(', %.3gkV_Phs_%d_Avg', [vbase, j]));
                    end;
                end;
                WriteintoMemStr(PHV_MHandle, ', Min Bus, MaxBus' + Char(10));
            end;

        end;
    except
        On E: Exception do
            DosimpleMsg('Error opening demand interval file "' + Name + '.CSV' + ' for writing.' + CRLF + E.Message, 535);
    end;

end;

procedure TEnergyMeterObj.WriteDemandIntervalData;
var
    i, j: Integer;

    function MyCount_Avg(const Value: Double; const count: Integer): Double;
    begin
        if Count = 0 then
            Result := 0.0
        else
            Result := Value / count;
    end;

begin
    if EnergyMeterClass.DI_Verbose and This_Meter_DIFileIsOpen then
    begin
        with ActiveCircuit.Solution do
            WriteintoMem(DI_MHandle, DynaVars.dblHour);
        for i := 1 to NumEMRegisters do
            WriteintoMem(DI_MHandle, Derivatives[i]);
        WriteIntoMemStr(DI_MHandle, Char(10));
    end;

      {Add to Class demand interval registers}
    with EnergyMeterClass do
        for i := 1 to NumEMRegisters do
            DI_RegisterTotals[i] := DI_RegisterTotals[i] + Derivatives[i] * TotalsMask[i];


      {Phase Voltage Report, if requested}
    if VPhaseReportFileIsOpen then
    begin
        with ActiveCircuit.Solution do
            WriteintoMem(PHV_MHandle, DynaVars.dblHour);
        for i := 1 to MaxVBaseCount do
            if VBaseList^[i] > 0.0 then
            begin
                for j := 1 to 3 do
                    WriteintoMem(PHV_MHandle, 0.001 * VPhaseMax^[jiIndex(j, i)]);
                for j := 1 to 3 do
                    WriteintoMem(PHV_MHandle, 0.001 * VPhaseMin^[jiIndex(j, i)]);
                for j := 1 to 3 do
                    WriteintoMem(PHV_MHandle, 0.001 * MyCount_Avg(VPhaseAccum^[jiIndex(j, i)], VPhaseAccumCount^[jiIndex(j, i)]));
            end;
        WriteintoMemStr(PHV_MHandle, Char(10));
    end;

end;

procedure TEnergyMeter.CloseAllDIFiles;
var
    mtr: TEnergyMeterObj;

begin
    if FSaveDemandInterval then
    begin
        {While closing DI files, write all meter registers to one file}
        try
            CreateMeterTotals;
        except
            On E: Exception do
                DoSimpleMsg('Error on Rewrite of totals file: ' + E.Message, 536);
        end;

        {Close all the DI file for each meter}
        mtr := ActiveCircuit.EnergyMeters.First;
        while mtr <> NIL do
        begin
            if mtr.enabled then
                mtr.CloseDemandIntervalFile;
            mtr := ActiveCircuit.EnergyMeters.Next;
        end;

        WriteTotalsFile;  // Sum all energymeter registers to "Totals.CSV"
        SystemMeter.CloseDemandIntervalFile;
        SystemMeter.Save;
        if EMT_MHandle <> NIL then
            CloseMHandler(EMT_MHandle, DI_Dir + PathDelim + 'EnergyMeterTotals.CSV', EMT_Append);
        EMT_MHandle := NIL;
        if TDI_MHandle <> NIL then
            CloseMHandler(TDI_MHandle, DI_Dir + PathDelim + 'DI_Totals.CSV', TDI_Append);
        TDI_MHandle := NIL;
        DIFilesAreOpen := FALSE;
        if OverloadFileIsOpen then
        begin
            if OV_MHandle <> NIL then
                CloseMHandler(OV_MHandle, EnergyMeterClass.DI_Dir + PathDelim + 'DI_Overloads.CSV', OV_Append);
            OV_MHandle := NIL;
            OverloadFileIsOpen := FALSE;
        end;
        if VoltageFileIsOpen then
        begin
            if VR_MHandle <> NIL then
                CloseMHandler(VR_MHandle, EnergyMeterClass.DI_Dir + PathDelim + 'DI_VoltExceptions.CSV', VR_Append);
            VR_MHandle := NIL;
            VoltageFileIsOpen := FALSE;
        end;
    end;
end;

procedure TEnergyMeterObj.AppendDemandIntervalFile;

var
    FileNm: String;
begin

  {Only called if "SaveDemandInterval"}

    if This_Meter_DIFileIsOpen then
        Exit;

    try
        if Energymeterclass.FDI_Verbose then
        begin
            FileNm := MakeDIFileName;   // Creates directory if it doesn't exist
            if FileExists(FileNm) then
                DI_Append := TRUE
            else
                DI_Append := FALSE;
            if DI_MHandle <> NIL then
                DI_MHandle.Free;
            DI_MHandle := Create_Meter_Space(' ');
            This_Meter_DIFileIsOpen := TRUE;
        end;
    except
        On E: Exception do
            DosimpleMsg('Error opening demand interval file "' + Name + '.CSV' + ' for appending.' + CRLF + E.Message, 537);
    end;
end;

procedure TEnergyMeterObj.AssignVoltBaseRegisterNames;
var
    i, ireg: Integer;
    vbase: Double;
begin
    ireg := 1;
    for i := 1 to MaxVBaseCount do
    begin
        if VBaseList^[i] > 0.0 then
        begin
            vbase := VBaseList^[i] * SQRT3;
            RegisterNames[i + Reg_VBaseStart] := Format('%.3g kV Losses', [vbase]);
            RegisterNames[i + 1 * MaxVBaseCount + Reg_VBaseStart] := Format('%.3g kV Line Loss', [vbase]);
            RegisterNames[i + 2 * MaxVBaseCount + Reg_VBaseStart] := Format('%.3g kV Load Loss', [vbase]);
            RegisterNames[i + 3 * MaxVBaseCount + Reg_VBaseStart] := Format('%.3g kV No Load Loss', [vbase]);
            RegisterNames[i + 4 * MaxVBaseCount + Reg_VBaseStart] := Format('%.3g kV Load Energy', [vbase])
        end
        else
        begin
            RegisterNames[i + Reg_VBaseStart] := Format('Aux%d', [ireg]);
            Inc(ireg);
            RegisterNames[i + 1 * MaxVBaseCount + Reg_VBaseStart] := Format('Aux%d', [ireg]);
            Inc(ireg);
            RegisterNames[i + 2 * MaxVBaseCount + Reg_VBaseStart] := Format('Aux%d', [ireg]);
            Inc(ireg);
            RegisterNames[i + 3 * MaxVBaseCount + Reg_VBaseStart] := Format('Aux%d', [ireg]);
            Inc(ireg);
            RegisterNames[i + 4 * MaxVBaseCount + Reg_VBaseStart] := Format('Aux%d', [ireg]);
            Inc(ireg);
        end;
    end;
    for i := 1 + Reg_VBaseStart + 5 * MaxVBaseCount to NumEMRegisters do
    begin
        RegisterNames[i] := Format('Aux%d', [ireg]);
        Inc(ireg);
    end;
end;

procedure TEnergyMeter.AppendAllDIFiles;
var
    mtr: TEnergyMeterObj;
    Filenm: String;

begin
    if FSaveDemandInterval then
    begin

        ClearDI_Totals;  // clears accumulator arrays

        mtr := ActiveCircuit.EnergyMeters.First;
        while mtr <> NIL do
        begin
            if mtr.enabled then
                mtr.AppendDemandIntervalFile;
            mtr := ActiveCircuit.EnergyMeters.Next;
        end;

        SystemMeter.AppendDemandIntervalFile;

          {Open FDI_Totals}
        try
            FileNm := DI_Dir + PathDelim + 'DI_Totals.CSV';
              {File Must Exist}
            if FileExists(FileNm) then
                TDI_Append := TRUE;
            CreateFDI_Totals;
        except
            On E: Exception do
                DosimpleMsg('Error opening demand interval file "' + Name + '.CSV' + ' for appending.' + CRLF + E.Message, 538);
        end;

        DIFilesAreOpen := TRUE;

    end;{IF}
end;

function TEnergyMeterObj.MakeDIFileName: String;
begin
    Result := EnergyMeterClass.DI_Dir + PathDelim + Self.Name + '.CSV';
end;

procedure TEnergyMeter.Set_SaveDemandInterval(const Value: Boolean);
begin
    FSaveDemandInterval := Value;
    ResetAll;
end;

function TEnergyMeter.Get_SaveDemandInterval: Boolean;
begin
    Result := FSaveDemandInterval;
end;

procedure TEnergyMeter.WriteOverloadReport;
var
    PDelem: TPDelement;
    EmergAmps,
    NormAmps,
    Cmax: Double;
    ClassName: String;
    RSignal: TXYCurveObj;
    i, j, k,
    RatingIdx: Integer;
    cVector,
    cBuffer: pDoubleArray;

begin
{
  Scans the active circuit for overloaded PD elements and writes each to a file
  This is called only if in Demand Interval (DI) mode and the file is open.
}
{    Prepares everything for using seasonal ratings if required}
    RatingIdx := -1;
    if SeasonalRating then
    begin
        if SeasonSignal <> '' then
        begin
            RSignal := XYCurveClass.Find(SeasonSignal);
            if RSignal <> NIL then
                RatingIdx := trunc(RSignal.GetYValue(ActiveCircuit.Solution.DynaVars.intHour))
            else
                SeasonalRating := FALSE;   // The XYCurve defined doesn't exist
        end
        else
            SeasonalRating := FALSE;    // The user didn't define the seasonal signal
    end;

 { CHECK PDELEMENTS ONLY}
    PDelem := ActiveCircuit.PDElements.First;
    while PDelem <> NIL do
    begin
        if (PDelem.Enabled) and (not PDelem.IsShunt) then
        begin   // Ignore shunts

            if (PdElem.Normamps > 0.0) or (PdElem.Emergamps > 0.0) then
            begin
                PDelem.ComputeIterminal;
                Cmax := PDelem.MaxTerminalOneImag; // For now, check only terminal 1 for overloads
                
             // Section introduced in 02/20/2019 for allowing the automatic change of ratings
             // when the seasonal ratings option is active
                ClassName := lowercase(PDElem.DSSClassName);
                if SeasonalRating and (ClassName = 'line') and (PDElem.NumAmpRatings > 1) then
                begin
                    if (RatingIdx > PDElem.NumAmpRatings) or (RatingIdx < 0) then
                    begin
                        NormAmps := PDElem.NormAmps;
                        EmergAmps := pdelem.EmergAmps;
                    end
                    else
                    begin
                        NormAmps := PDElem.AmpRatings[RatingIdx];
                        EmergAmps := PDElem.AmpRatings[RatingIdx];
                    end;
                end
                else
                begin
                    NormAmps := PDElem.NormAmps;
                    EmergAmps := pdelem.EmergAmps;
                end;


                if (Cmax > NormAmps) or (Cmax > EmergAmps) then
                begin

              // Gets the currents for the active Element
                    cBuffer := Allocmem(sizeof(cBuffer^[1]) * PDElem.NPhases * PDElem.NTerms);
                    PDElem.Get_Current_Mags(cBuffer);
                    cVector := Allocmem(sizeof(cBuffer^[1]) * 3); // for storing
                    for i := 1 to 3 do
                        cVector^[i] := 0.0;
                    if PDElem.NPhases < 3 then
                    begin
                        ClassName := PDElem.FirstBus;
                        j := ansipos('.', ClassName);     // Removes the name of the bus
                        ClassName := ClassName.Substring(j);
                        for i := 1 to 3 do
                        begin
                            j := ansipos('.', ClassName);   // goes for the phase Number
                            if j = 0 then
                            begin
                                k := strtoint(ClassName);
                                cVector^[k] := cBuffer^[i];
                                break
                            end
                            else
                            begin
                                k := strtoint(ClassName.Substring(0, j - 1));
                                cVector^[k] := cBuffer^[i];
                                ClassName := ClassName.Substring(j);
                            end;
                        end;
                    end
                    else
                    begin
                        for i := 1 to 3 do
                            cVector^[i] := cBuffer^[i];
                    end;

                    with ActiveCircuit.Solution do
                        WriteintoMem(OV_MHandle, DynaVars.dblHour);
                    WriteintoMemStr(OV_MHandle, ', ' + FullName(PDelem));
                    WriteintoMem(OV_MHandle, PDElem.NormAmps);
                    WriteintoMem(OV_MHandle, pdelem.EmergAmps);
                    if PDElem.Normamps > 0.0 then
                        WriteintoMem(OV_MHandle, Cmax / PDElem.Normamps * 100.0)
                    else
                        WriteintoMem(OV_MHandle, 0.0);
                    if PDElem.Emergamps > 0.0 then
                        WriteintoMem(OV_MHandle, Cmax / PDElem.Emergamps * 100.0)
                    else
                        WriteintoMem(OV_MHandle, 0.0);
                    with ActiveCircuit do // Find bus of first terminal
                        WriteintoMem(OV_MHandle, Buses^[MapNodeToBus^[PDElem.NodeRef^[1]].BusRef].kVBase);
              // Adds the currents in Amps per phase at the end of the report
                    for i := 1 to 3 do
                        WriteintoMem(OV_MHandle, cVector^[i]);

                    WriteintoMemStr(OV_MHandle, ' ' + Char(10));

                end;
            end; { }
        end;
        PDelem := ActiveCircuit.PDElements.Next;
    end;
end;

procedure TEnergyMeter.ClearDI_Totals;
var
    i: Integer;
begin
    for i := 1 to NumEMRegisters do
        DI_RegisterTotals[i] := 0.0;
end;

procedure TEnergyMeter.CreateFDI_Totals;
var
    i: Integer;
    mtr: TEnergyMeterObj;

begin
    try
        if TDI_MHandle <> NIL then
            TDI_MHandle.Free;
        TDI_MHandle := Create_Meter_Space('Time');
        mtr := ActiveCircuit.EnergyMeters.First;  // just get the first one
        if Assigned(mtr) then
        begin
            for i := 1 to NumEMRegisters do
            begin
                WriteintoMemStr(TDI_MHandle, ', "' + mtr.RegisterNames[i] + '"');
            end;
        end;
        WriteintoMemStr(TDI_MHandle, Char(10));
    except
        On E: Exception do
            DoSimpleMsg('Error creating: "' + DI_Dir + PathDelim + 'DI_Totals.CSV": ' + E.Message, 539)
    end;
end;

{ TSystemMeter }

procedure TSystemMeter.AppendDemandIntervalFile;
var
    FileNm: String;
begin

  {Only called if "SaveDemandInterval"}

    if This_Meter_DIFileIsOpen then
        Exit;

    try
        FileNm := EnergyMeterClass.Di_Dir + PathDelim + 'DI_SystemMeter.CSV';
      {File Must Exist}
        if FileExists(FileNm) then
        begin
//        DI_MMFView:=  MapFile2Memory(EnergyMeterClass.DI_Dir+ PathDelim + 'DI_SystemMeter.CSV', DI_MMFHandle);
//        DI_Cursor :=  GetMMFCursor(DI_MMFView);
        end
        else
            OpenDemandIntervalFile;
        This_Meter_DIFileIsOpen := TRUE;
    except
        On E: Exception do
        begin
            DosimpleMsg('Error opening demand interval file "' + FileNm + ' for appending.' + CRLF + E.Message, 540);
        end;
    end;

end;

procedure TSystemMeter.Clear;
begin
    kWh := 0.0;
    kvarh := 0.0;
    peakkW := 0.0;
    peakkVA := 0.0;
    Losseskwh := 0.0;
    Losseskvarh := 0.0;
    PeakLosseskW := 0.0;
    dkWh := 0.0;
    dkvarh := 0.0;
    dLosseskwh := 0.0;
    dLosseskvarh := 0.0;
    FirstSampleAfterReset := TRUE;
end;

procedure TSystemMeter.CloseDemandIntervalFile;
var
    File_Path: String;
begin
    if This_Meter_DIFileIsOpen then
    begin
        File_Path := EnergyMeterClass.DI_Dir + PathDelim + 'DI_SystemMeter.CSV';
        CloseMHandler(SDI_MHandle, File_Path, SDI_Append);
        SDI_MHandle := NIL;
        This_Meter_DIFileIsOpen := FALSE;
    end;
end;

constructor TSystemMeter.Create;
begin
    Clear;
    This_Meter_DIFileIsOpen := FALSE;
end;

destructor TSystemMeter.Destroy;
begin
    inherited;

end;

procedure TSystemMeter.Integrate(var Reg: Double; Value: Double; var Deriv: Double);
begin
    if ActiveCircuit.TrapezoidalIntegration then
    begin
        {Trapezoidal Rule Integration}
        if not FirstSampleAfterReset then
            Reg := Reg + 0.5 * ActiveCircuit.Solution.IntervalHrs * (Value + Deriv);
    end
    else   {Plain Euler integration}
        Reg := Reg + ActiveCircuit.Solution.IntervalHrs * Value;

    Deriv := Value;

end;

procedure TSystemMeter.OpenDemandIntervalFile;
begin
    try
        if This_Meter_DIFileIsOpen then
            SDI_MHandle.Free;
        This_Meter_DIFileIsOpen := TRUE;
        if SDI_MHandle <> NIL then
            SDI_MHandle.free;
        SDI_MHandle := Create_Meter_Space('"Hour", ');
        WriteintoMemStr(SDI_MHandle, 'kWh, kvarh, "Peak kW", "peak kVA", "Losses kWh", "Losses kvarh", "Peak Losses kW"' + Char(10));

    except
        On E: Exception do
            DosimpleMsg('Error opening demand interval file "DI_SystemMeter.CSV"  for writing.' + CRLF + E.Message, 541);
    end;


end;

procedure TSystemMeter.Reset;
begin
    Clear;
   // removed - open in solution If EnergyMeterClass.SaveDemandInterval Then OpenDemandIntervalFile;
end;

procedure TSystemMeter.Save;
var
    F: TFileStream;
    CSVName, Folder: String;
begin
    try
        CSVName := 'SystemMeter.CSV';
       {If we are doing a simulation and saving interval data, create this in the
        same directory as the demand interval data}
        if energyMeterClass.SaveDemandInterval then
            Folder := energyMeterClass.DI_DIR + PathDelim
        else
            Folder := GetOutputDirectory;
        GlobalResult := CSVName;
        SetLastResultFile(CSVName);

    except
        On E: Exception do
        begin
            DoSimpleMsg('Error opening System Meter File "' + CRLF + CSVName + '": ' + E.Message, 542);
            Exit;
        end
    end;

    try
        if SM_MHandle <> NIL then
            SM_MHandle.Free;
        SM_MHandle := Create_Meter_Space('Year, ');
        WriteintoMemStr(SM_MHandle, 'kWh, kvarh, "Peak kW", "peak kVA", "Losses kWh", "Losses kvarh", "Peak Losses kW"' + Char(10));
        WriteintoMemStr(SM_MHandle, inttostr(ActiveCircuit.Solution.Year));
        WriteRegisters(F);
        WriteintoMemStr(SM_MHandle, Char(10));

    finally
        CloseMHandler(SM_MHandle, Folder + CSVName, SM_Append);
        SM_MHandle := NIL;
    end;
end;

procedure TSystemMeter.TakeSample;

begin

  {Get total system energy out of the sources}

    cPower := CmulReal(GetTotalPowerFromSources, 0.001);  // convert to kW

    Integrate(kWh, cPower.re, dkwh);
    Integrate(kvarh, cPower.im, dkvarh);

    PeakkW := Max(cPower.re, PeakkW);
    Peakkva := Max(Cabs(cPower), Peakkva);

  {Get total circuit losses}
    cLosses := ActiveCircuit.Losses;  // PD Elements except shunts
    cLosses := CmulReal(cLosses, 0.001);  // convert to kW

    Integrate(Losseskwh, cLosses.re, dLosseskwh);
    Integrate(Losseskvarh, cLosses.im, dLosseskvarh);

    PeakLosseskW := Max(cLosses.re, PeakLosseskW);

    FirstSampleAfterReset := FALSE;
    if This_Meter_DIFileIsOpen then
        WriteDemandIntervalData;

end;

procedure TEnergyMeter.CreateMeterTotals;
var
    i: Integer;
    mtr: TEnergyMeterObj;
begin
    if EMT_MHandle <> NIL then
        EMT_MHandle.Free;
    EMT_MHandle := Create_Meter_Space('Name');
    mtr := ActiveCircuit.EnergyMeters.First;
    if Assigned(mtr) then
        for i := 1 to NumEMRegisters do
            WriteintoMemStr(EMT_MHandle, ', "' + mtr.RegisterNames[i] + '"');
    WriteintoMemStr(EMT_MHandle, Char(10));
end;

procedure TSystemMeter.WriteDemandIntervalData;
begin
    with ActiveCircuit.Solution do
        WriteintoMem(SDI_MHandle, DynaVars.dblHour);
    WriteintoMem(SDI_MHandle, cPower.re);
    WriteintoMem(SDI_MHandle, cPower.im);
    WriteintoMem(SDI_MHandle, peakkW);
    WriteintoMem(SDI_MHandle, peakkVA);
    WriteintoMem(SDI_MHandle, cLosses.re);
    WriteintoMem(SDI_MHandle, cLosses.im);
    WriteintoMem(SDI_MHandle, PeakLosseskW);
    WriteintoMemStr(SDI_MHandle, Char(10));

end;

procedure TSystemMeter.WriteRegisterNames(F: TFileStream);
begin
// Does nothing
end;

procedure TSystemMeter.WriteRegisters(F: TFileStream);
begin
    WriteintoMem(SM_MHandle, kWh);
    WriteintoMem(SM_MHandle, kvarh);
    WriteintoMem(SM_MHandle, peakkW);
    WriteintoMem(SM_MHandle, peakkVA);
    WriteintoMem(SM_MHandle, Losseskwh);
    WriteintoMem(SM_MHandle, Losseskvarh);
    WriteintoMem(SM_MHandle, PeakLosseskW);

end;

procedure TEnergyMeter.Set_DI_Verbose(const Value: Boolean);
begin
    FDI_Verbose := Value;
    ResetAll;
end;

function TEnergyMeter.Get_DI_Verbose: Boolean;
begin
    Result := FDI_Verbose;
end;

procedure TEnergyMeter.WriteTotalsFile;
var
    mtr: TEnergyMeterObj;
    Regsum: TRegisterArray;
    i: Integer;
begin
  {Sum up all registers of all meters and write to Totals.CSV}
    for i := 1 to NumEMRegisters do
        RegSum[i] := 0.0;

    mtr := ActiveCircuit.EnergyMeters.First;
    while mtr <> NIL do
    begin
        if mtr.enabled then
            with Mtr do
                for i := 1 to NumEMRegisters do
                    Regsum[i] := Regsum[i] + Registers[i] * TotalsMask[i];

        mtr := ActiveCircuit.EnergyMeters.Next;
    end;

    try     // Writes the file
        if FM_MHandle <> NIL then
            FM_MHandle.Free;
        FM_MHandle := Create_Meter_Space('Year');
        mtr := ActiveCircuit.EnergyMeters.First;
        if assigned(mtr) then
            for i := 1 to NumEMRegisters do
                WriteintoMemStr(FM_MHandle, ', "' + mtr.RegisterNames[i] + '"'); //Write(F,', "', mtr.RegisterNames[i],'"');
        WriteintoMemStr(FM_MHandle, Char(10));

        WriteintoMemStr(FM_MHandle, inttostr(ActiveCircuit.Solution.Year));
        for i := 1 to NumEMRegisters do
            WriteintoMem(FM_MHandle, Double(RegSum[i]));
        WriteintoMemStr(FM_MHandle, Char(10));
        CloseMHandler(FM_MHandle, DI_Dir + PathDelim + 'Totals.CSV', FM_Append);
        FM_MHandle := NIL;

    except
        On E: Exception do
            DosimpleMsg('Error writing demand interval file Totals.CSV.' + CRLF + E.Message, 543);
    end;

end;

procedure TEnergyMeter.WriteVoltageReport;
var
    i, j: Integer;
    Vmagpu: Double;
    UnderCount: Integer;
    OverCount: Integer;
    OverVmax: Double;
    UnderVmin: Double;
    MinBus: Integer;
    MaxBus: Integer;
    BusCounted: Boolean;

begin
     {For any bus with a defined voltage base, test for > Vmax or < Vmin}

    OverCount := 0;
    UnderCount := 0;
    MinBus := 0;
    MaxBus := 0;

    with ActiveCircuit do
    begin
        OverVmax := NormalMinVolts;
        UnderVmin := NormalMaxVolts;
        for i := 1 to NumBuses do
            with Buses^[i] do
            begin
                BusCounted := FALSE;
                if kVBase > 1.0 then          // Primary Nodes first
                begin
                    for j := 1 to NumNodesThisBus do
                    begin
                        Vmagpu := Cabs(Solution.NodeV^[GetRef(j)]) / kvbase * 0.001;
                        if Vmagpu > 0.1 then
                        begin // ignore neutral buses
                            if Vmagpu < underVmin then
                            begin
                                UnderVmin := Vmagpu;
                                MinBus := i;
                            end;

                            if Vmagpu > OverVMax then
                            begin
                                OverVMax := Vmagpu;
                                MaxBus := i;
                            end;

                            if (Vmagpu < NormalMinVolts) then
                            begin
                                if not BusCounted then
                                begin     // Don't count more than once
                                    Inc(UnderCount);
                                    BusCounted := TRUE;
                                end;
                            end
                            else
                            if (Vmagpu > NormalMaxVolts) then
                            begin
                                if not BusCounted then
                                begin
                                    Inc(OverCount);
                                    BusCounted := TRUE;
                                end;
                            end;
                        end;
                    end;
                end;
            end; {For i}

        with Solution do
            WriteintoMem(VR_MHandle, DynaVars.dblHour);
        WriteintoMemStr(VR_MHandle, ', ' + inttostr(UnderCount));
        WriteintoMem(VR_MHandle, UnderVmin);
        WriteintoMemStr(VR_MHandle, ', ' + inttostr(OverCount));
        WriteintoMem(VR_MHandle, OverVmax);
        WriteintoMemStr(VR_MHandle, ', ' + BusList.Get(minbus));
        WriteintoMemStr(VR_MHandle, ', ' + Buslist.Get(maxbus));

     // Klugy but it works
     // now repeat for buses under 1 kV
        OverCount := 0;
        UnderCount := 0;
        MinBus := 0;
        MaxBus := 0;

        OverVmax := NormalMinVolts;
        UnderVmin := NormalMaxVolts;
        for i := 1 to NumBuses do
            with Buses^[i] do
            begin
                BusCounted := FALSE;
                if (kVBase > 0.0) and (kVBase <= 1.0) then
                begin
                    for j := 1 to NumNodesThisBus do
                    begin
                        Vmagpu := Cabs(Solution.NodeV^[GetRef(j)]) / kvbase * 0.001;
                        if Vmagpu > 0.1 then
                        begin // ignore neutral buses
                            if Vmagpu < underVmin then
                            begin
                                UnderVmin := Vmagpu;
                                MinBus := i;
                            end;

                            if Vmagpu > OverVMax then
                            begin
                                OverVMax := Vmagpu;
                                MaxBus := i;
                            end;

                            if (Vmagpu < NormalMinVolts) then
                            begin
                                if not BusCounted then
                                begin     // Don't count more than once
                                    Inc(UnderCount);
                                    BusCounted := TRUE;
                                end;
                            end
                            else
                            if (Vmagpu > NormalMaxVolts) then
                            begin
                                if not BusCounted then
                                begin
                                    Inc(OverCount);
                                    BusCounted := TRUE;
                                end;
                            end;
                        end;
                    end;
                end;
            end; {For i}

        WriteintoMemStr(VR_MHandle, ', ' + inttostr(UnderCount));
        WriteintoMem(VR_MHandle, UnderVmin);
        WriteintoMemStr(VR_MHandle, ', ' + inttostr(OverCount));
        WriteintoMem(VR_MHandle, OverVmax);
        WriteintoMemStr(VR_MHandle, ', ' + BusList.Get(minbus));
        WriteintoMemStr(VR_MHandle, ', ' + Buslist.Get(maxbus));
        WriteintoMemStr(VR_MHandle, Char(10));
    end;

end;

procedure TEnergyMeter.InterpretRegisterMaskArray(var Mask: TRegisterArray);

var
    i, n: Integer;
begin
    n := Parser.ParseAsVector(NumEMRegisters, @Mask);
    for i := n + 1 to NumEMRegisters do
        Mask[i] := 1.0;  // Set the rest to 1
end;

procedure TEnergyMeter.OpenAllDIFiles;
{Similar to Append, by creates the files.}

var
    mtr: TEnergyMeterObj;
  // Filenm:String;
begin

    if FSaveDemandInterval then
    begin

        ClearDI_Totals;  // clears accumulator arrays

        mtr := ActiveCircuit.EnergyMeters.First;
        while mtr <> NIL do
        begin
            if mtr.enabled then
                mtr.OpenDemandIntervalFile;
            mtr := ActiveCircuit.EnergyMeters.Next;
        end;

        SystemMeter.OpenDemandIntervalFile;

          {Optional Exception Reporting}
        if Do_OverloadReport then
            OpenOverloadReportFile;
        if Do_VoltageExceptionReport then
            OpenVoltageReportFile;

          {Open FDI_Totals}
        try
            CreateFDI_Totals;

        except
            On E: Exception do
                DosimpleMsg('Error creating the memory space for demand interval "' + Name + '.CSV' + ' for appending.' + CRLF + E.Message, 538);
        end;

        DIFilesAreOpen := TRUE;

    end;{IF}


end;

procedure TEnergyMeter.OpenOverloadReportFile;
begin
    try
        if OverloadFileIsOpen then
            OV_MHandle.Free;
        OverloadFileIsOpen := TRUE;
        if OV_MHandle <> NIL then
            OV_MHandle.free;
        OV_MHandle := Create_Meter_Space('"Hour", "Element", "Normal Amps", "Emerg Amps", "% Normal", "% Emerg", "kVBase", "I1(A)", "I2(A)", "I3(A)"' + Char(10));
    except
        On E: Exception do
            DosimpleMsg('Error creating memory space (Overload report) for writing.' + CRLF + E.Message, 541);
    end;

end;

procedure TEnergyMeter.OpenVoltageReportFile;
begin
    try
        if VoltageFileIsOpen then
            VR_MHandle.Free;
        VoltageFileIsOpen := TRUE;
        if VR_MHandle <> NIL then
            VR_MHandle.free;
        VR_MHandle := Create_Meter_Space('"Hour", "Undervoltages", "Min Voltage", "Overvoltage", "Max Voltage", "Min Bus", "Max Bus"');
        WriteintoMemStr(VR_MHandle, ', "LV Undervoltages", "Min LV Voltage", "LV Overvoltage", "Max LV Voltage", "Min LV Bus", "Max LV Bus"' + Char(10));
    except
        On E: Exception do
            DosimpleMsg('Error creating memory space (Voltage report) for writing.' + CRLF + E.Message, 541);
    end;

end;

initialization

  {RegisterNameList := TCommandList.Create(['kWh', 'kvarh', 'Max kW', 'Max kVA', 'Zone kWh',
  'Zone kvarh', 'Zone Max kW','Zone Max kVA','Overload kWh Normal','Overload kWh Emerg','Load EEN',
  'Load UE', 'Zone Losses kWh', 'Zone Losses kvarh', 'Zone Max kW Losses', 'Zone Max kvar Losses',
  'Gen kWh', 'Gen kvarh', 'Gen Max kW', 'Gen Max kVA']); }


finalization

end.
