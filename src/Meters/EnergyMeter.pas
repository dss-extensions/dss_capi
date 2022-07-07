unit EnergyMeter;

{
  ----------------------------------------------------------
  Copyright (c) 2008-2015, Electric Power Research Institute, Inc.
  All rights reserved.
  ----------------------------------------------------------
}
//     This class of device accumulates the energy of the voltage and current in the
//     terminal of the device to which it is connected.
//
//     It is an intelligent energy meter capable of measuring losses of all
//     devices within its "zone".
//
//     The Zone is determined automatically after a circuit change.  The Zone starts on the
//     opposite side of the branch on which the meter is located and continues in the same
//     direction through the network until
//       a) an open point is encountered
//       b) an open terminal or switch is encountered
//       c) another energy meter is encountered
//       d) a branch that is already included in a zone is encountered
//
//     It keeps track of kwh, kvarh, UE,  EEN, Losses, etc., having registers FOR each
//     of these quantities.
//
//     In EEN/UE calculations, line overload takes precedence.
//
//     If the Max Zone kW limits are specified, then these replace the line overload UE/EEN numbers.
//     These limits were added so that the user can override line limits in cases
//     such as networks where it is difficult to judge the UE from the individual
//     line limits.
//
//     Only the maximum |kVA| overload is accumulated, not all.  Loads downline from
//     an overload are marked WITH a factor representing the degree of overload.  This
//     is used to compute EEN/UE FOR loads.
//
//     FOR low voltages, the full kW FOR loads below the emergency min voltage are counted.
//     The EEN is proportioned based on how low the voltage is.
//
//     Emergency min voltage must be less than normal min voltage.

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
    UComplex, DSSUcomplex,
    Load,
    Generator,
    XYCurve,
    Command,
    Classes;

const
    NumEMVbase = 7;
    NumEMRegisters = 32 + 5 * NumEMVbase;   // Total Number of energy meter registers
    
    // Fixed Registers
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
{$SCOPEDENUMS ON}
    TEnergyMeterProp = (
        INVALID = 0,
        element = 1,
        terminal = 2,
        action = 3,
        option = 4,
        kVAnormal = 5,
        kVAemerg = 6,
        peakcurrent = 7,
        Zonelist = 8,
        LocalOnly = 9,
        Mask = 10,
        Losses = 11,
        LineLosses = 12,
        XfmrLosses = 13,
        SeqLosses = 14,
        __3phaseLosses = 15,
        VbaseLosses = 16, // segregate losses by voltage base
        PhaseVoltageReport = 17, // Compute Avg phase voltages in zone
        Int_Rate = 18,
        Int_Duration = 19,
        SAIFI = 20, // Read only
        SAIFIkW = 21, // Read only
        SAIDI = 22, // Read only
        CAIDI = 23, // Read only
        CustInterrupts = 24 // Read only
    );
{$SCOPEDENUMS OFF}

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
    FeederSectionArray = array[0..100] of TFeederSection;   // Dummy dimension
    //  --------- Feeder Section Definition -----------

    TEnergyMeter = class;

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
        DSS: TDSSContext;

        procedure Clear;
        procedure Integrate(var Reg: Double; Value: Double; var Deriv: Double);
        procedure WriteRegisters();

    PROTECTED

        procedure OpenDemandIntervalFile;
        procedure WriteDemandIntervalData;
        procedure CloseDemandIntervalFile;
        procedure AppendDemandIntervalFile;

    PUBLIC

        procedure TakeSample;
        procedure Reset;
        procedure Save;

        constructor Create(EnergyMeterClass: TEnergyMeter);
        destructor Destroy; OVERRIDE;
    end;

    TEnergyMeter = class(TMeterClass)    // derive strait from base class
    PRIVATE
        FSaveDemandInterval: Boolean;
        FDI_Verbose: Boolean;

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
        procedure Set_DI_Verbose(const Value: Boolean);
        function Get_DI_Verbose: Boolean;

    PROTECTED
        // Moved from global unit vars

        Delta_Hrs: Double;
        // adjacency lists for PC and PD elements at each bus, built for faster searches
        BusAdjPC: TAdjArray; // also includes shunt PD elements
        BusAdjPD: TAdjArray;

    // ********************************************************************************
    // *    Nomenclature:                                                             *
    // *                  OV_ Overloads                                               *
    // *                  VR_ Voltage report                                          *
    // *                  DI_ Demand interval                                         *
    // *                  SI_ System Demand interval                                  *
    // *                  TDI_ DI Totals                                              *
    // *                  FM_  Meter Totals                                           *
    // *                  SM_  System Mater                                           *
    // *                  EMT_  Energy Meter Totals                                   *
    // *                  PHV_  Phase Voltage Report                                  *
    // *     These prefixes are applied to the variables of each file mapped into     *
    // *     Memory using the MemoryMap_Lib                                           *
    // ********************************************************************************
   
    PUBLIC
        OV_MHandle: TBytesStream;  // a. Handle to the file in memory
        VR_MHandle: TBytesStream;
        OV_Append: Boolean;
        VR_Append: Boolean;
        SDI_Append: Boolean;
        TDI_Append: Boolean;
        SM_Append: Boolean;
        EMT_Append: Boolean;
        FM_Append: Boolean;

        
    PROTECTED
        SDI_MHandle: TBytesStream;
        TDI_MHandle: TBytesStream;
        SM_MHandle: TBytesStream;
        EMT_MHandle: TBytesStream;
        FM_MHandle: TBytesStream;


        procedure DefineProperties; override;
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

        constructor Create(dssContext: TDSSContext);
        destructor Destroy; OVERRIDE;

        function BeginEdit(ptr: Pointer; SetActive_: Boolean=True): Pointer; override;
        function EndEdit(ptr: Pointer; const NumChanges: integer): Boolean; override;
        Function NewObject(const ObjName: String; Activate: Boolean = True): Pointer; OVERRIDE;

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
        LocalOnly,
        FLosses,
        FLineLosses,
        FXfmrLosses,
        FSeqLosses,
        F3PhaseLosses,
        FVBaseLosses,
        FPhaseVoltageReport: LongBool;

        DefinedZoneList: TStringList;

        // Limits on the entire load in the zone for networks where UE cannot be determined
        // by the individual branches
        MaxZonekVA_Norm: Double;
        MaxZonekVA_Emerg: Double;

        // Voltage bases in the Meter Zone
        VBaseTotalLosses: pDoubleArray;    // allocated array
        VBaseLineLosses: pDoubleArray;
        VBaseLoadLosses: pDoubleArray;
        VBaseNoLoadLosses: pDoubleArray;
        VBaseLoad: pDoubleArray;
        VBaseList: pDoubleArray;
        VBaseCount: Integer;
        MaxVBaseCount: Integer;

        // Arrays for phase voltage report
        VphaseMax: pDoubleArray;
        VPhaseMin: pDoubleArray;
        VPhaseAccum: pDoubleArray;
        VPhaseAccumCount: pIntegerArray;
        VPhaseReportFileIsOpen: Boolean;

        // Demand Interval File variables
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
    // ********************************************************************************
    // *    Nomenclature:                                                             *
    // *                  OV_ Overloads                                               *
    // *                  VR_ Voltage report                                          *
    // *                  DI_ Demand interval                                         *
    // *                  SI_ System Demand interval                                  *
    // *                  TDI_ DI Totals                                              *
    // *                  FM_  Meter Totals                                           *
    // *                  SM_  System Mater                                           *
    // *                  EMT_  Energy Meter Totals                                   *
    // *                  PHV_  Phase Voltage Report                                  *
    // *     These prefixes are applied to the variables of each file mapped into     *
    // *     Memory using the MemoryMap_Lib                                           *
    // ********************************************************************************

        DI_MHandle: TBytesStream;
        PHV_MHandle: TBytesStream;

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

        //*********** Flags for appending Files*****************************************
        DI_Append: Boolean;
        PHV_Append: Boolean;

        constructor Create(ParClass: TDSSClass; const EnergyMeterName: String);
        destructor Destroy; OVERRIDE;
        procedure PropertySideEffects(Idx: Integer; previousIntVal: Integer = 0); override;
        procedure MakeLike(OtherPtr: Pointer); override;

        procedure MakePosSequence(); OVERRIDE;  // Make a positive Sequence Model, reset nphases
        procedure RecalcElementData; OVERRIDE;
        procedure CalcYPrim; OVERRIDE;
        procedure GetCurrents(Curr: pComplexArray); OVERRIDE; //Get present value of terminal Curr
        function CheckBranchList(code: Integer): Boolean;
        procedure ResetRegisters;
        procedure TakeSample; OVERRIDE;
        procedure SaveRegisters;
        procedure MakeMeterZoneLists;
        procedure ZoneDump;
        procedure InterpolateCoordinates;

        procedure AllocateLoad;
        procedure ReduceZone;  // Reduce Zone by eliminating buses and merging lines
        procedure SaveZone();
        procedure GetPCEatZone(const allowEmpty: Boolean = False);

        procedure CalcReliabilityIndices(AssumeRestoration: Boolean);

        procedure DumpProperties(F: TFileStream; Complete: Boolean; Leaf: Boolean = False); OVERRIDE;

    end;

implementation

uses
    BufStream,
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
    Math,
    MemoryMap_Lib,
    DSSHelper,
    DSSObjectHelper,
    TypInfo;

type
    TObj = TEnergyMeterObj;
    TProp = TEnergyMeterProp;
{$PUSH}
{$Z4} // keep enums as int32 values
    TEnergyMeterAction = (
        Allocate = 0,
        Clear = 1,
        Reduce = 2,
        Save = 3,
        Take = 4,
        ZoneDump = 5
    );
{$POP}

const
    NumPropsThisClass = Ord(High(TProp));
var
    PropInfo: Pointer = NIL;    
    ActionEnum: TDSSEnum;

function jiIndex(i, j: Integer): Integer; inline;
begin
    Result := (j - 1) * 3 + i;
end;

constructor TEnergyMeter.Create(dssContext: TDSSContext);
begin
    if PropInfo = NIL then
    begin
        PropInfo := TypeInfo(TProp);
        ActionEnum := TDSSEnum.Create('EnergyMeter: Action', True, 1, 2, 
            ['Allocate', 'Clear', 'Reduce', 'Save', 'Take', 'ZoneDump'],
            [0, 1, 2, 3, 4, 5]);
    end;

    inherited Create(dssContext, ENERGY_METER, 'EnergyMeter');

    // Initialize demand interval options to off
    FSaveDemandInterval := FALSE;
    FDI_Verbose := FALSE;

    Do_OverloadReport := FALSE;  // FSaveDemandInterval must be true for this to have an effect
    OverLoadFileIsOpen := FALSE;
    VoltageFileIsOpen := FALSE;

    Do_VoltageExceptionReport := FALSE;

    DI_Dir := '';

    SystemMeter := TSystemMeter.Create(self);
    OV_MHandle := NIL;
    VR_MHandle := NIL;
    SDI_MHandle := NIL;
    TDI_MHandle := NIL;
    SM_MHandle := NIL;
    EMT_MHandle := NIL;
    FM_MHandle := NIL;
end;

destructor TEnergyMeter.Destroy;
begin
    SystemMeter.Free;
    if OV_MHandle <> NIL then
        OV_MHandle.Free;
    if VR_MHandle <> NIL then
        VR_MHandle.Free;
    if SDI_MHandle <> NIL then
        SDI_MHandle.Free;
    if TDI_MHandle <> NIL then
        TDI_MHandle.Free;
    if SM_MHandle <> NIL then
        SM_MHandle.Free;
    if EMT_MHandle <> NIL then
        EMT_MHandle.Free;
    if FM_MHandle <> NIL then
        FM_MHandle.Free;
    inherited Destroy;
end;

procedure DoAction(Obj: TObj; action: TEnergyMeterAction);
begin
    case action of
        TEnergyMeterAction.Allocate:
            Obj.AllocateLoad;
        TEnergyMeterAction.Clear:
            Obj.ResetRegisters;
        TEnergyMeterAction.Reduce:
            Obj.ReduceZone;
        TEnergyMeterAction.Save:
            Obj.SaveRegisters;
        TEnergyMeterAction.Take:
            Obj.TakeSample;
        TEnergyMeterAction.Zonedump:
            Obj.ZoneDump;
    end;
end;

procedure SetOptions(Obj: TObj; Value: TStringList);
var
    i: Integer;
begin
    for i := 0 to Value.Count - 1 do
        case AnsiLowerCase(Value.Strings[i][1])[1] of
            'e':
                Obj.ExcessFlag := TRUE;
            't':
                Obj.ExcessFlag := FALSE;
            'r':
                Obj.ZoneIsRadial := TRUE;
            'm':
                Obj.ZoneIsRadial := FALSE;
            'c':
                Obj.VoltageUEOnly := FALSE;
            'v':
                Obj.VoltageUEOnly := TRUE;
        end;
    Value.Free;
end;

function GetOptions(Obj: TObj; Index: Integer): TStringList;
begin
    Result := TStringList.Create();
    
    if Obj.ExcessFlag then
        Result.Add('E')
    else
        Result.Add('T');
    
    if Obj.ZoneIsRadial then
        Result.Add('R')
    else
        Result.Add('M');
    
    if Obj.VoltageUEOnly then
        Result.Add('V')
    else
        Result.Add('C');
end;

procedure TEnergyMeter.DefineProperties;
var 
    obj: TObj = NIL; // NIL (0) on purpose
begin
    Numproperties := NumPropsThisClass;
    CountPropertiesAndAllocate();
    PopulatePropertyNames(0, NumPropsThisClass, PropInfo);

    PropertyType[ord(TProp.peakcurrent)] := TPropertyType.DoubleVArrayProperty;
    PropertyOffset[ord(TProp.peakcurrent)] := ptruint(@obj.SensorCurrent);
    PropertyOffset2[ord(TProp.peakcurrent)] := ptruint(@obj.Fnphases);

    PropertyType[ord(TProp.Mask)] := TPropertyType.DoubleFArrayProperty;
    PropertyOffset[ord(TProp.Mask)] := ptruint(@obj.TotalsMask); //TODO: validate the actual values?
    PropertyOffset2[ord(TProp.Mask)] := NumEMRegisters;

    // boolean properties
    PropertyType[ord(TProp.LocalOnly)] := TPropertyType.BooleanProperty;
    PropertyType[ord(TProp.Losses)] := TPropertyType.BooleanProperty;
    PropertyType[ord(TProp.LineLosses)] := TPropertyType.BooleanProperty;
    PropertyType[ord(TProp.XfmrLosses)] := TPropertyType.BooleanProperty;
    PropertyType[ord(TProp.SeqLosses)] := TPropertyType.BooleanProperty;
    PropertyType[ord(TProp.__3PhaseLosses)] := TPropertyType.BooleanProperty;
    PropertyType[ord(TProp.VBaseLosses)] := TPropertyType.BooleanProperty;
    PropertyType[ord(TProp.PhaseVoltageReport)] := TPropertyType.BooleanProperty;
    PropertyOffset[ord(TProp.LocalOnly)] := ptruint(@obj.LocalOnly);
    PropertyOffset[ord(TProp.Losses)] := ptruint(@obj.FLosses);
    PropertyOffset[ord(TProp.LineLosses)] := ptruint(@obj.FLineLosses);
    PropertyOffset[ord(TProp.XfmrLosses)] := ptruint(@obj.FXfmrLosses);
    PropertyOffset[ord(TProp.SeqLosses)] := ptruint(@obj.FSeqLosses);
    PropertyOffset[ord(TProp.__3PhaseLosses)] := ptruint(@obj.F3PhaseLosses);
    PropertyOffset[ord(TProp.VBaseLosses)] := ptruint(@obj.FVBaseLosses);
    PropertyOffset[ord(TProp.PhaseVoltageReport)] := ptruint(@obj.FPhaseVoltageReport);

    // object reference
    PropertyType[ord(TProp.element)] := TPropertyType.DSSObjectReferenceProperty;
    PropertyOffset[ord(TProp.element)] := ptruint(@obj.MeteredElement);
    PropertyOffset2[ord(TProp.element)] := 0;
    //PropertyFlags[ord(TProp.element)] := [TPropertyFlag.CheckForVar]; // not required for general cktelements

    // integer properties
    PropertyType[ord(TProp.terminal)] := TPropertyType.IntegerProperty;
    PropertyOffset[ord(TProp.terminal)] := ptruint(@obj.MeteredTerminal);

    // read-only doubles
    PropertyFlags[ord(TProp.SAIFI)] := [TPropertyFlag.SilentReadOnly];
    PropertyFlags[ord(TProp.SAIFIkW)] := [TPropertyFlag.SilentReadOnly];
    PropertyFlags[ord(TProp.SAIDI)] := [TPropertyFlag.SilentReadOnly];
    PropertyFlags[ord(TProp.CAIDI)] := [TPropertyFlag.SilentReadOnly];
    PropertyFlags[ord(TProp.CustInterrupts)] := [TPropertyFlag.SilentReadOnly];
    PropertyOffset[ord(TProp.SAIFI)] := ptruint(@obj.SAIFI);
    PropertyOffset[ord(TProp.SAIFIkW)] := ptruint(@obj.SAIFIkW);
    PropertyOffset[ord(TProp.SAIDI)] := ptruint(@obj.SAIDI);
    PropertyOffset[ord(TProp.CAIDI)] := ptruint(@obj.CAIDI);
    PropertyOffset[ord(TProp.CustInterrupts)] := ptruint(@obj.CustInterrupts);

    // double properties (default type)
    PropertyOffset[ord(TProp.kVAnormal)] := ptruint(@obj.MaxZonekVA_Norm);
    PropertyOffset[ord(TProp.kVAemerg)] := ptruint(@obj.MaxZonekVA_Emerg);
    PropertyOffset[ord(TProp.Int_Rate)] := ptruint(@obj.Source_NumInterruptions);
    PropertyOffset[ord(TProp.Int_Duration)] := ptruint(@obj.Source_IntDuration);

    // string list
    PropertyType[ord(TProp.Zonelist)] := TPropertyType.StringListProperty;
    PropertyOffset[ord(TProp.Zonelist)] := ptruint(@obj.DefinedZoneList);

    // custom list
    PropertyType[ord(TProp.option)] := TPropertyType.StringListProperty;
    PropertyOffset[ord(TProp.option)] := 1; // dummy value
    PropertyWriteFunction[ord(TProp.option)] := @SetOptions;
    PropertyReadFunction[ord(TProp.option)] := @GetOptions;
    PropertyFlags[ord(TProp.option)] := [TPropertyFlag.WriteByFunction, TPropertyFlag.ReadByFunction];

    // enum action
    PropertyType[ord(TProp.Action)] := TPropertyType.StringEnumActionProperty;
    PropertyOffset[ord(TProp.Action)] := ptruint(@DoAction); 
    PropertyOffset2[ord(TProp.Action)] := PtrInt(ActionEnum);

    ActiveProperty := NumPropsThisClass;
    inherited DefineProperties;
end;

function TEnergyMeter.NewObject(const ObjName: String; Activate: Boolean): Pointer;
var
    Obj: TObj;
begin
    Obj := TObj.Create(Self, ObjName);
    if Activate then 
        ActiveCircuit.ActiveCktElement := Obj;
    Obj.ClassIndex := AddObjectToList(Obj, Activate);
    Result := Obj;
end;

procedure TEnergyMeterObj.PropertySideEffects(Idx: Integer; previousIntVal: Integer);
var
    i: Integer;
begin
    case Idx of
        1, 2:
        begin
            MeteredElementChanged := TRUE;
            Include(Flags, Flg.NeedsRecalc);
        end;
        ord(TProp.Mask):
            for i := previousIntVal + 1 to NumEMRegisters do
                TotalsMask[i] := 1.0;  // Set the rest to 1
    end;
    inherited PropertySideEffects(Idx, previousIntVal);
end;

function TEnergyMeter.BeginEdit(ptr: Pointer; SetActive_: Boolean): Pointer;
var
    Obj: TObj;
begin
    Obj := TObj(inherited BeginEdit(ptr, SetActive_));
    if SetActive_ then
        DSS.ActiveEnergyMeterObj := Obj;
    Obj.MeteredElementChanged := FALSE;
    Result := Obj;
end;

function TEnergyMeter.EndEdit(ptr: Pointer; const NumChanges: integer): Boolean;
begin
    with TObj(ptr) do
    begin
        if flg.NeedsRecalc in Flags then
            RecalcElementData;   // When some basic data have changed
        Exclude(Flags, Flg.EditionActive);
    end;
    Result := True;
end;

procedure TEnergyMeterObj.MakeLike(OtherPtr: Pointer);
var
    Other: TObj;
    i: Integer;
begin
    inherited MakeLike(OtherPtr);
    Other := TObj(OtherPtr);
    FNPhases := Other.Fnphases;
    NConds := Other.Fnconds; // Force Reallocation of terminal stuff

    MeteredElement := Other.MeteredElement;  // Pointer to target circuit element
    MeteredTerminal := Other.MeteredTerminal;
    ExcessFlag := Other.ExcessFlag;

    MaxZonekVA_Norm := Other.MaxZonekVA_Norm;
    MaxZonekVA_Emerg := Other.MaxZonekVA_emerg;

    // Reliability
    Source_NumInterruptions := Other.Source_NumInterruptions;
    Source_IntDuration := Other.Source_IntDuration;

    DefinedZoneList.Clear;
    // Copy Strings over (actually incr ref count on string)
    for i := 0 to (Other.DefinedZoneList.Count - 1) do
        DefinedZoneList.Add(Other.DefinedZoneList[i]);

    LocalOnly := Other.LocalOnly;
    VoltageUEOnly := Other.VoltageUEOnly;

    // Boolean Flags
    FLosses := Other.FLosses;
    FLineLosses := Other.FLineLosses;
    FXfmrLosses := Other.FXfmrLosses;
    FSeqLosses := Other.FSeqLosses;
    F3PhaseLosses := Other.F3PhaseLosses;
    FVBaseLosses := Other.FVBaseLosses;
    FPhaseVoltageReport := Other.FPhaseVoltageReport;
end;

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
        if Energymeters.Count = 0 then
            Exit;  // Do not do anything

        // initialize the Checked Flag FOR all circuit Elements
        pCktElement := CktElements.First;
        while (pCktElement <> NIL) do
        begin
            with pCktElement do
            begin
                Exclude(Flags, Flg.Checked);
                Include(Flags, Flg.IsIsolated);
                for i := 1 to NTerms do
                    TerminalsChecked[i - 1] := FALSE;
            end;
            pCktElement := CktElements.Next;
        end;

        // Clear some things that will be set by the Meter Zone
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
        BuildActiveBusAdjacencyLists(ActiveCircuit, BusAdjPD, BusAdjPC);

        // Set Hasmeter flag for all cktelements
        SetHasMeterFlag;
        DSS.SensorClass.SetHasSensorFlag;  // Set all Sensor branch flags, too.

        // initialize the Checked Flag for all Buses
        for i := 1 to NumBuses do
            Buses^[i].BusChecked := FALSE;

        for i := 1 to EnergyMeters.Count do
        begin
            mtr := EnergyMeters.Get(i);
            //if Mtr.Enabled then
            mtr.MakeMeterZoneLists;
        end;

        FreeAndNilBusAdjacencyLists(BusAdjPD, BusAdjPC);
    end;
end;

procedure TEnergyMeter.ResetAll;  // Force all EnergyMeters in the circuit to reset
var
    mtr: TEnergyMeterObj;
    CasePath: String;
begin
    if DSS.DIFilesAreOpen then
        CloseAllDIFiles;

    if FSaveDemandInterval then
    begin
        CasePath := DSS.OutputDirectory + DSS.ActiveCircuit.CaseName;
        
        //Make directories to save data
        if not DirectoryExists(CasePath) then
        begin
            try
                mkDir(CasePath);
            except
                On E: Exception do
                    DoSimpleMsg('Error making  Directory: "%s". %s', [CasePath, E.Message], 522);
            end;
        end;
        DI_Dir := CasePath + PathDelim + 'DI_yr_' + Trim(IntToStr(ActiveCircuit.Solution.Year));
        if not DirectoryExists(DI_Dir) then
        begin
            try
                mkDir(DI_Dir);
            except
                On E: Exception do
                    DoSimpleMsg('Error making Demand Interval Directory: "%s". %s', [DI_Dir, E.Message], 523);
            end;
        end;

        CreateFDI_Totals;
    end;

    mtr := DSS.ActiveCircuit.EnergyMeters.First;
    while mtr <> NIL do
    begin
        mtr.ResetRegisters;
        mtr := DSS.ActiveCircuit.EnergyMeters.Next;
    end;

    SystemMeter.Reset;

    // Reset Generator Objects, too
    DSS.GeneratorClass.ResetRegistersAll;

    if DSS_CAPI_LEGACY_MODELS then
    begin
        DSS.StorageClass.ResetRegistersAll;
        DSS.PVSystemClass.ResetRegistersAll;
    end
    else
    begin
        DSS.Storage2Class.ResetRegistersAll;
        DSS.PVSystem2Class.ResetRegistersAll;
    end;
end;

procedure TEnergyMeter.SampleAll;  // Force all EnergyMeters in the circuit to take a sample
var
    mtr: TEnergyMeterObj;
    i: Integer;
begin
    mtr := DSS.ActiveCircuit.EnergyMeters.First;
    while mtr <> NIL do
    begin
        if mtr.enabled then
            mtr.TakeSample;
        mtr := DSS.ActiveCircuit.EnergyMeters.Next;
    end;

    SystemMeter.TakeSample;

    if FSaveDemandInterval then
    begin  
        // Write Totals Demand interval file
        with DSS.ActiveCircuit.Solution do
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
    DSS.GeneratorClass.SampleAll;
    
    if DSS_CAPI_LEGACY_MODELS then
    begin
        DSS.StorageClass.SampleAll; // samples energymeter part of storage elements (not update)
        DSS.PVSystemClass.SampleAll;
    end
    else
    begin
        DSS.Storage2Class.SampleAll; // samples energymeter part of storage elements (not update)
        DSS.PVSystem2Class.SampleAll;
    end;
end;

procedure TEnergyMeter.SaveAll;  // Force all EnergyMeters in the circuit to take a sample
var
    mtr: TEnergyMeterObj;
begin
    mtr := DSS.ActiveCircuit.EnergyMeters.First;
    while mtr <> NIL do
    begin
        if mtr.enabled then
            mtr.SaveRegisters;
        mtr := DSS.ActiveCircuit.EnergyMeters.Next;
    end;

    SystemMeter.Save;
end;

constructor TEnergyMeterObj.Create(ParClass: TDSSClass; const EnergyMeterName: String);
var
    i: Integer;
begin
    inherited Create(ParClass);
    Name := AnsiLowerCase(EnergyMeterName);
    DSSObjType := ParClass.DSSClassType; //ENERGY_METER;

    FNPhases := 3;  // Directly set conds and phases
    Fnconds := 3;
    Nterms := 1;  // this forces allocation of terminals and conductors in base class
    ExcessFlag := TRUE;  // Default to Excess energy FOR UE
    MeteredElement := TDSSCktElement(ActiveCircuit.CktElements.Get(1)); // Default to first circuit element (source)
    BranchList := NIL;  // initialize to NIL, set later when inited
    SequenceList := NIL;
    LoadList := NIL;

    This_Meter_DIFileIsOpen := FALSE;
    VPhaseReportFileIsOpen := FALSE;

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
    DefinedZoneList := TStringList.Create();

    FLosses := TRUE;   // Loss Reporting switches
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
    //TODO: 
    // 1. These seem to be always false
    // 2. If not always false, check below. Since these vars were globals in the 
    //    original codebase, this means that a new meter resets the append status for 
    //    all meters.

    DI_Append := FALSE;
    PHV_Append := FALSE;
    with DSS.EnergyMeterClass do
    begin
        OV_Append := FALSE;
        VR_Append := FALSE;
        SDI_Append := FALSE;
        TDI_Append := FALSE;
        SM_Append := FALSE;
        EMT_Append := FALSE;
        FM_Append := FALSE;
    end;
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
    // Registers for capturing losses by base voltage, names assigned later
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
    
    DefinedZoneList.Free;

    if Assigned(FeederSections) then
        Reallocmem(FeederSections, 0);

    inherited destroy;
end;

procedure TEnergyMeterObj.RecalcElementData;
begin
    Exclude(Flags, Flg.NeedsRecalc);
    if MeteredElement <> NIL then
    begin  // Monitored element must already exist
        // MeteredElement must be a PDElement
        if not (MeteredElement is TPDElement) then
        begin
            MeteredElement := NIL;   // element not found
            DoErrorMsg(
                Format(_('EnergyMeter: "%s"'), [Self.Name]), 
                Format(_('Circuit Element "%s" is not a Power Delivery (PD) element.'), [MeteredElement.Name]),
                _('Element must be a PD element.'), 525);
            Exit;
        end;

        if MeteredTerminal > MeteredElement.Nterms then
        begin
            DoErrorMsg(
                Format(_('EnergyMeter: "%s"'), [Self.Name]), 
                Format(_('Terminal no. "%d" does not exist.'), [MeteredTerminal]),
                _('Respecify terminal no.'), 524);
        end
        else
        begin
            if MeteredElementChanged then
            begin
               // Sets name of i-th terminal's connected bus in monitor's buslist
               // This value will be used to set the NodeRef array (see TakeSample)
                Setbus(1, MeteredElement.GetBus(MeteredTerminal));
                FNphases := MeteredElement.NPhases;
                Nconds := MeteredElement.Nconds;
                AllocateSensorArrays;

                 // If we come through here, throw branchlist away
                if BranchList <> NIL then
                    BranchList.Free;
                BranchList := NIL;
            end;
        end;
        Exit;
    end;

    // element not found/set
    DoErrorMsg(Format(_('EnergyMeter: "%s"'), [Self.Name]), 
        _('Circuit Element not set.'),
        _('Element must be defined previously.'), 525);
end;

procedure TEnergyMeterobj.MakePosSequence();
begin
    if MeteredElement <> NIL then
    begin
        Setbus(1, MeteredElement.GetBus(MeteredTerminal));
        FNphases := MeteredElement.NPhases;
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
    Result := DSS.EnergyMeterClass.DI_Dir + PathDelim + Name + '_PhaseVoltageReport' + DSS._Name + '.csv';
end;

procedure TEnergyMeterObj.ResetRegisters;
var
    i: Integer;
begin
    for i := 1 to NumEMregisters do
        Registers[i] := 0.0;
    for i := 1 to NumEMregisters do
        Derivatives[i] := 0.0;
    // Initialize DragHand registers to some big negative number
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

procedure TEnergyMeterObj.CalcYPrim;
begin
    // YPrim is all zeros.  Just leave as NIL so it is ignored.
end;

procedure TEnergyMeterObj.SaveRegisters;
var
    CSVName: String;
    F: TFileStream = nil;
    i: Integer;
    sout: String;
begin
    try
        CSVName := 'MTR_' + Name + '.csv';
        F := TBufferedFileStream.Create(DSS.OutputDirectory + CSVName, fmCreate);
        DSS.GlobalResult := CSVName;
        SetLastResultFile(DSS, CSVName);

    except
        On E: Exception do
        begin
            DoSimpleMsg('Error opening Meter File "%s": %s', [CSVName, E.Message], 526);
            FreeAndNil(F);
            Exit;
        end
    end;

    try
//       FSWriteln(F,'**** NEW RECORD ****');
        WriteStr(sout, 'Year, ', DSS.ActiveCircuit.Solution.Year: 0, ',');
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

procedure TEnergyMeterObj.Integrate(Reg: Integer; const Deriv: Double; const Interval: Double);
begin
    if DSS.ActiveCircuit.TrapezoidalIntegration then
    begin
        // Trapezoidal Rule Integration
        if not FirstSampleAfterReset then
            Registers[Reg] := Registers[Reg] + 0.5 * Interval * (Deriv + Derivatives[Reg]);
    end
    else
    begin // Plain Euler integration
        Registers[Reg] := Registers[Reg] + Interval * Deriv;
    end;

    // Set the derivatives so that the proper value shows up in Demand Interval Files
    // and prepare for next time step in Trapezoidal integration 
    Derivatives[Reg] := Deriv;
end;

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
    Delta_hrs_local: Double;
begin
    if not CheckBranchList(545) then
        Exit;

    // Compute energy in branch  to which meter is connected

     //----MeteredElement.ActiveTerminalIdx := MeteredTerminal;  // needed for Excess kVA calcs
    S_Local := MeteredElement.Power[MeteredTerminal] * 0.001;
    S_Local_kVA := Cabs(S_Local);
    DSS.EnergyMeterClass.Delta_Hrs := DSS.ActiveCircuit.Solution.IntervalHrs;
    Integrate(Reg_kWh, S_Local.re, DSS.EnergyMeterClass.Delta_Hrs);   // Accumulate the power
    Integrate(Reg_kvarh, S_Local.im, DSS.EnergyMeterClass.Delta_Hrs);
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

    //--------------------------------------------------------------------------
    //------------------------ Local Zone  Only --------------------------------
    //--------------------------------------------------------------------------
    if LocalOnly then
    begin
        CktElem := MeteredElement as TPDElement;
        MaxExcesskWNorm := Abs(CktElem.ExcesskVANorm[MeteredTerminal].re);
        MaxExcesskWEmerg := Abs(CktElem.ExcesskVAEmerg[MeteredTerminal].re);
    end
    else
        //--------------------------------------------------------------------------
        //--------Cyle Through Entire Zone Setting EEN/UE --------------------------
        //--------------------------------------------------------------------------
        while CktElem <> NIL do
        begin       // loop thru all ckt elements on zone

            with CktElem do
            begin
                ActiveTerminalIdx := BranchList.Presentbranch.FromTerminal;
                // Invoking this property sets the Overload_UE flag in the PD Element
                EEN := Abs(ExcesskVANorm[ActiveTerminalIdx].re);
                UE := Abs(ExcesskVAEmerg[ActiveTerminalIdx].re);
            end;

            // For radial circuits just keep the maximum overload; for mesh, add 'em up
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


    //--------------------------------------------------------------------------
    //--------       Cycle Through Zone Accumulating Load and Losses    --------
    //--------------------------------------------------------------------------
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
                //Ignore other types of PC Elements
            end;
            PCElem := BranchList.NextObject
        end;

        if Flosses then
        begin  // Compute and Report Losses

            // Get losses from the present circuit element
            CktElem.GetLosses(S_TotalLosses, S_LoadLosses, S_NoLoadLosses);  // returns watts, vars
            // Convert to kW
            S_TotalLosses *= 0.001;
            S_LoadLosses *= 0.001;
            S_NoLoadLosses *= 0.001;
            // Update accumulators
            TotalLosses += S_TotalLosses; // Accumulate total losses in meter zone
            TotalLoadLosses += S_LoadLosses;  // Accumulate total load losses in meter zone
            TotalNoLoadLosses += S_NoLoadLosses; // Accumulate total no load losses in meter zone

            // Line and Transformer Elements
            if IsLineElement(Cktelem) and FLineLosses then
            begin
                TotalLineLosses += S_TotalLosses; // Accumulate total losses in meter zone
                if FseqLosses then
                begin
                    CktElem.GetSeqLosses(S_PosSeqLosses, S_NegSeqLosses, S_ZeroSeqLosses);
                    S_PosSeqLosses += S_NegSeqLosses;  // add line modes together
                    S_PosSeqLosses *= 0.001; // convert to kW
                    S_ZeroSeqLosses *= 0.001;
                    TotalLineModeLosses += S_PosSeqLosses;
                    TotalZeroModeLosses += S_ZeroSeqLosses;
                end;
                // Separate Line losses into 3- and "1-phase" losses
                if F3PhaseLosses then
                begin
                    if Cktelem.NPhases = 3 then
                        Total3phaseLosses += S_TotalLosses
                    else
                        Total1phaseLosses += S_TotalLosses;
                end;
            end
            else
            if IsTransformerElement(Cktelem) and FXfmrLosses then
            begin
                TotalTransformerLosses += S_TotalLosses; // Accumulate total losses in meter zone
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
                                        puV := Cabs(Solution.NodeV^[Buses^[FromBusReference].RefNo[i]]) / Buses^[FromBusReference].kVBase;
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
        end; // If FLosses

        CktElem := BranchList.GoForward;
    end;

    Delta_hrs_local := DSS.EnergyMeterClass.Delta_Hrs;
    
    // NOTE: Integrate proc automatically sets derivatives array
    Integrate(Reg_LoadEEN, TotalLoad_EEN, Delta_hrs_local);
    Integrate(Reg_LoadUE, TotalLoad_UE, Delta_hrs_local);

    // Accumulate losses in appropriate registers
    Integrate(Reg_ZoneLosseskWh, TotalLosses.re, Delta_hrs_local);
    Integrate(Reg_ZoneLosseskvarh, TotalLosses.im, Delta_hrs_local);
    Integrate(Reg_LoadLosseskWh, TotalLoadLosses.re, Delta_hrs_local);
    Integrate(Reg_LoadLosseskvarh, TotalLoadLosses.im, Delta_hrs_local);
    Integrate(Reg_NoLoadLosseskWh, TotalNoLoadLosses.re, Delta_hrs_local);
    Integrate(Reg_NoLoadLosseskvarh, TotalNoLoadLosses.im, Delta_hrs_local);
    Integrate(Reg_LineLosseskWh, TotalLineLosses.re, Delta_hrs_local);
    Integrate(Reg_LineModeLineLoss, TotalLineModeLosses.re, Delta_hrs_local);
    Integrate(Reg_ZeroModeLineLoss, TotalZeroModeLosses.re, Delta_hrs_local);
    Integrate(Reg_3_phaseLineLoss, Total3phaseLosses.re, Delta_hrs_local);
    Integrate(Reg_1_phaseLineLoss, Total1phaseLosses.re, Delta_hrs_local);
    Integrate(Reg_TransformerLosseskWh, TotalTransformerLosses.re, Delta_hrs_local);
    for i := 1 to MaxVBaseCount do
    begin
        Integrate(Reg_VbaseStart + i, VBaseTotalLosses^[i], Delta_hrs_local);
        Integrate(Reg_VbaseStart + 1 * MaxVBaseCount + i, VBaseLineLosses^[i], Delta_hrs_local);
        Integrate(Reg_VbaseStart + 2 * MaxVBaseCount + i, VBaseLoadLosses^[i], Delta_hrs_local);
        Integrate(Reg_VbaseStart + 3 * MaxVBaseCount + i, VBaseNoLoadLosses^[i], Delta_hrs_local);
        Integrate(Reg_VbaseStart + 4 * MaxVBaseCount + i, VBaseLoad^[i], Delta_hrs_local);
    end;


    //--------------------------------------------------------------------------
    //---------------   Total Zone Load and Generation -------------------------
    //--------------------------------------------------------------------------

    Integrate(Reg_ZonekWh, TotalZonekW, Delta_hrs_local);
    Integrate(Reg_Zonekvarh, TotalZonekvar, Delta_hrs_local);
    Integrate(Reg_GenkWh, TotalGenkW, Delta_hrs_local);
    Integrate(Reg_Genkvarh, TotalGenkvar, Delta_hrs_local);
    GenkVA := Sqrt(Sqr(TotalGenkvar) + Sqr(TotalGenkW));
    LoadkVA := Sqrt(Sqr(TotalZonekvar) + Sqr(TotalZonekW));

    //--------------------------------------------------------------------------
    //---------------   Set Drag Hand Registers  -------------------------------
    //--------------------------------------------------------------------------

    SetDragHandRegister(Reg_LossesMaxkW, Abs(TotalLosses.Re));
    SetDragHandRegister(Reg_LossesMaxkvar, Abs(TotalLosses.im));
    SetDragHandRegister(Reg_MaxLoadLosses, Abs(TotalLoadLosses.Re));
    SetDragHandRegister(Reg_MaxNoLoadLosses, Abs(TotalNoLoadLosses.Re));
    SetDragHandRegister(Reg_ZoneMaxkW, TotalZonekW); // Removed abs()  3-10-04
    SetDragHandRegister(Reg_ZoneMaxkVA, LoadkVA);
    // Max total generator registers
    SetDragHandRegister(Reg_GenMaxkW, TotalGenkW); // Removed abs()  3-10-04
    SetDragHandRegister(Reg_GenMaxkVA, GenkVA);

    //--------------------------------------------------------------------------
    //---------------------   Overload Energy  ---------------------------------
    //--------------------------------------------------------------------------
    // Overload energy for the entire zone
    if LocalOnly then
        ZonekW := S_Local.Re
    else
        ZonekW := TotalZonekW;

    // Either the max excess kW of any PD element or the excess over zone limits

    // regs 9 and 10
    // Fixed these formulas 2-7-07 per discussions with Daniel Brooks
    if (MaxZonekVA_Norm > 0.0) then
    begin
        if (S_Local_KVA = 0.0) then
            S_Local_KVA := MaxZonekVA_Norm;
        Integrate(Reg_OverloadkWhNorm, Max(0.0, (ZonekW * (1.0 - MaxZonekVA_Norm / S_Local_KVA))), Delta_Hrs_local);
    end
    else
    begin
        Integrate(Reg_OverloadkWhNorm, MaxExcesskWNorm, Delta_hrs_local);
    end;

    if (MaxZonekVA_Emerg > 0.0) then
    begin
        if (S_Local_KVA = 0.0) then
            S_Local_KVA := MaxZonekVA_Emerg;
        Integrate(Reg_OverloadkWhEmerg, Max(0.0, (ZonekW * (1.0 - MaxZonekVA_Emerg / S_Local_KVA))), Delta_Hrs_local);
    end
    else
    begin
        Integrate(Reg_OverloadkWhEmerg, MaxExcesskWEmerg, Delta_hrs_local);
    end;

    FirstSampleAfterReset := FALSE;
    if DSS.EnergyMeterClass.SaveDemandInterval then
        WriteDemandIntervalData;
end;

procedure TEnergyMeterObj.TotalUpDownstreamCustomers;
var
    i: Integer;
    //, Accumulator
    // PresentNode: TCktTreeNode;
    CktElem: TPDElement;
begin
    if not CheckBranchList(529) then
        Exit;

    // Init totals and checked flag
    CktElem := SequenceList.First;
    while CktElem <> NIL do
    begin
        Exclude(CktElem.Flags, Flg.Checked);
        CktElem.BranchTotalCustomers := 0;
        CktElem := SequenceList.Next;
    end;

    // This algorithm could be made more efficient with a Sequence list
    
    // For i := 1 to Branchlist.ZoneEndsList.NumEnds Do
    // Begin
    //   {Busref := } Branchlist.ZoneEndsList.Get(i, PresentNode);
    //   If PresentNode <> Nil Then
    //   Begin
    //      CktElem     := PresentNode.CktObject;
    //      if Not CktElem.Checked  then    // don't do a zone end element more than once
    //      Begin
    //        CktElem.Checked := TRUE;
    //        Accumulator := CktElem.NumCustomers;
    //        Repeat  // Trace back to the source
    //            Inc(CktElem.TotalCustomers, Accumulator);
    //            PresentNode := PresentNode.ParentBranch;
    //            If PresentNode=Nil Then Break;
    //            CktElem     := PresentNode.CktObject;
    //            If not CktElem.Checked Then Begin   // avoid double counting
    //               Inc(Accumulator, CktElem.NumCustomers);
    //               CktElem.Checked := TRUE;
    //            End;
    //        Until FALSE;
    //      End;
    //   End;
    // End;

    // Backward Sweep  -  Order is guaranteed to process end branches first
    // sum numcustomers branch by branch
    for i := SequenceList.Count downto 1 do
    begin
        CktElem := SequenceList.Get(i);
        if not (Flg.Checked in CktElem.Flags) then    // Avoid double counting
            with CktElem do
            begin
                Include(Flags, Flg.Checked);
                Inc(BranchTotalCustomers, BranchNumCustomers);
                if ParentPDElement <> NIL then
                    Inc(ParentPDElement.BranchTotalCustomers, BranchTotalCustomers);
            end;
    end;
end;

procedure TEnergyMeter.SetHasMeterFlag;
// Set the HasMeter Flag for all cktElement;
var
    i: Integer;
    ThisMeter: TEnergyMeterObj;
    CktElem: TDSSCktElement;
begin
    // Initialize all to FALSE
    with  ActiveCircuit do
    begin
        CktElem := PDElements.First;
        while CktElem <> NIL do
        begin
            Exclude(CktElem.Flags, Flg.HasEnergyMeter);
            CktElem := PDElements.Next;
        end;
    end;

    for i := 1 to DSS.ActiveCircuit.EnergyMeters.Count do
    begin
        ThisMeter := DSS.ActiveCircuit.EnergyMeters.Get(i);
        with ThisMeter do
            if Enabled and (MeteredElement <> NIL) then
                Include(MeteredElement.Flags, Flg.HasEnergyMeter);
    end;
end;

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
    BusAdjPC: TAdjArray;
    BusAdjPD: TAdjArray;
begin
    ZoneListCounter := 0;
    VBasecount := 0; // Build the voltage base list over in case a base added or deleted
    for j := 1 to MaxVBaseCount do
        VBaseList^[j] := 0.0;

    // Make a new branch list
    if BranchList <> NIL then
        BranchList.Free;
    
    if Enabled then
        BranchList := TCktTree.Create // Instantiates ZoneEndsList, too
    else
    begin
        BranchList := NIL;
        Exit;
    end;

    // Get Started
    if Assigned(MeteredElement) then
        BranchList.Add(MeteredElement)
    else
    begin   // oops
        DoSimpleMsg('Metered Element for EnergyMeter %s not defined.', [Name], 527);
        Exit;
    end;

    // Initialize SensorObj property of the first branch to this TMeterElement Object.
    // Before starting, all sensorObj definitions are cleared in PCElements and PDElements. The
    // SensorObj property is passed down to the Load objects for LoadAllocation and State Estimation
  
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


    MeteredElement.TerminalsChecked[MeteredTerminal - 1] := TRUE;
    with BranchList.PresentBranch do
    begin
        // This bus is the head of the feeder or zone; do not mark as radial bus
        FromBusReference := MeteredElement.Terminals[MeteredTerminal - 1].BusRef;
        DSS.ActiveCircuit.Buses^[FromBusReference].DistFromMeter := 0.0;
        VoltBaseIndex := AddToVoltBaseList(FromBusReference);
        FromTerminal := MeteredTerminal;
        if MeteredElement is TPDElement then
            TPDElement(MeteredElement).FromTerminal := MeteredTerminal;
    end;

    // Check off this element so we don't use it  again
    with MeteredElement do
    begin
        Include(Flags, Flg.Checked);
        Exclude(Flags, Flg.IsIsolated);
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

    // ****************  MAIN LOOP *****************************
    BusAdjPC := DSS.EnergyMeterClass.BusAdjPC;
    BusAdjPD := DSS.EnergyMeterClass.BusAdjPD;
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
            if not ActiveBranch.TerminalsChecked[iTerm - 1] then
                with ActiveCircuit do
                begin
                    // Now find all loads and generators connected to the bus on this end of branch
                    // attach them as generic objects to cktTree node.
                    TestBusNum := ActiveBranch.Terminals[iTerm - 1].BusRef;
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
                        if not (Flg.Checked in pPCelem.Flags) then
                        begin
                            ; // skip ones we already checked
                            BranchList.PresentBranch.IsDangling := FALSE;   // Something is connected here
                            // Is this a load or a generator or a Capacitor or reactor??
                            PCElementType := (pPCelem.DSSObjType and CLASSMASK);
                            if (PCElementType = LOAD_ELEMENT) or (PCElementType = GEN_ELEMENT) or (PCElementType = PVSYSTEM_ELEMENT) or (PCElementType = STORAGE_ELEMENT) or (PCElementType = CAP_ELEMENT)  // Capacitor and Reactor put on the PC list if IsShunt=TRUE
                                or (PCElementType = REACTOR_ELEMENT) then
                            begin
                                BranchList.AddNewObject(pPCelem); // This adds element to the Shunt list in CktTree
                                Include(pPCelem.Flags, Flg.Checked);    // So we don't pick this element up again
                                Exclude(pPCelem.Flags, Flg.IsIsolated);
                                pPCelem.ActiveTerminalIdx := 1;
                                // Totalize Number of Customers if Load Type
                                if (pPCelem is TLoadObj) then
                                begin
                                    pLoad := pPCelem as TLoadObj;
                                    Inc(TPDElement(ActiveBranch).BranchNumCustomers, pLoad.NumCustomers);
                                    LoadList.Add(pPCElem);  // Add to list of loads in this zone.)
                                end;
                                // If object does not have a sensor attached, it acquires the sensor of its parent branch
                                if not (Flg.HasSensorObj in pPCelem.Flags) then
                                    pPCelem.SensorObj := TPDElement(ActiveBranch).SensorObj;
                                pPCelem.MeterObj := Self;
                            end;
                        end;
                    end;

                    // Now find all branches connected to this bus that we haven't found already
                    // Do not include in this zone if branch has open terminals or has another meter

                    if DefinedZoneList.Count = 0 then
                    begin  // Search tree for connected branches (default)
                        IsFeederEnd := TRUE;
                        adjLst := BusAdjPD[TestBusNum];
                        for iPD := 0 to adjLst.Count - 1 do
                        begin
                            TestElement := adjLst[iPD];  // Only enabled objects are in this list
                            // **** See ResetMeterZonesAll
                            if not (TestElement = ActiveBranch) then  // Skip self
                                if not (Flg.HasEnergyMeter in TestElement.Flags) then
                                begin  // Stop at other meters  so zones don't interfere
                                    for j := 1 to TestElement.Nterms do
                                    begin     // Check each terminal
                                        if TestBusNum = TestElement.Terminals[j - 1].BusRef then
                                        begin
                                            BranchList.PresentBranch.IsDangling := FALSE; // We found something it was connected to
                                            // Check for loops and parallel branches and mark them
                                            if (Flg.Checked in TestElement.Flags) then // This branch is on some meter's list already
                                                with BranchList.PresentBranch do
                                                begin
                                                    IsLoopedHere := TRUE; // It's a loop
                                                    LoopLineObj := TestElement;
                                                    if IsLineElement(ActiveBranch) and IsLineElement(TestElement) then
                                                        if CheckParallel(ActiveBranch, TestElement) then
                                                            IsParallel := TRUE; // It's paralleled with another line
                                                end
                                            else
                                            begin  // push TestElement onto stack and set properties
                                                IsFeederEnd := FALSE;  // for interpolation
                                                BranchList.AddNewChild(TestElement, TestBusNum, j);  // Add new child to the branchlist
                                                with TestElement do
                                                begin
                                                    TerminalsChecked[j - 1] := TRUE;
                                                    FromTerminal := j;
                                                    Include(Flags, Flg.Checked);
                                                    Exclude(Flags, Flg.IsIsolated);
                                                    // Branch inherits sensor of upline branch if it doesn't have its own
                                                    if not (Flg.HasSensorObj in Flags) then
                                                        SensorObj := TPDElement(ActiveBranch).SensorObj;
                                                    MeterObj := Self;   // Set meterobj to this meter
                                                    ParentPDElement := TPDElement(ActiveBranch);  // record the parent so we can easily back up for reconductoring, etc.
                                                end;
                                                Break;
                                            end;
                                        end; // IF TestBusNum
                                    end;  // FOR terminals
                                end; // ELSE
                        end; // FOR iPD

                        if IsFeederEnd then
                            BranchList.ZoneEndsList.Add(BranchList.PresentBranch, TestBusNum);
                            // This is an end of the feeder and testbusnum is the end bus
                    end
                    else
                    begin   // Zone is manually specified; Just add next element in list as a child
                        Inc(ZoneListCounter);
                        while ZoneListCounter <= DefinedZoneList.Count do
                        begin
                            if SetElementActive(DefinedZoneList[ZoneListCounter - 1]) = 0 then
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
                end;  // WITH Active Circuit
        end;  // FOR iTerm

        ActiveBranch := BranchList.GoForward;   // Sets PresentBranch
        // ****************  END MAIN LOOP *****************************
    end;

    TotalupDownstreamCustomers;

    AssignVoltBaseRegisterNames;
end;

procedure TEnergyMeterObj.GetCurrents(Curr: pComplexArray);  //Get present value of terminal Curr FOR reports
var
    i: Integer;
begin
    for i := 1 to Fnconds do
        Curr^[i] := CZERO;
end;

procedure TEnergyMeterObj.ZoneDump;
var
    CSVName: String;
    F: TFileStream = nil;
    pdelem: TPDelement;
    LoadElem: TDSSCktElement;
begin
    try
        CSVName := 'Zone_' + Name + '.csv';
        F := TBufferedFileStream.Create(DSS.OutputDirectory + CSVName, fmCreate);

        DSS.GlobalResult := CSVName;
        SetLastResultFile(DSS, CSVName);
    except

        On E: Exception do
        begin
            DoSimpleMsg('Error opening File "%s": %s', [CSVName, E.Message], 528);
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
                        // BusList.NameOfIndex(BranchList.PresentBranch.ToBusReference),
                        Buses^[BranchList.PresentBranch.ToBusReference].DistFromMeter]));
                    BranchList.PresentBranch.ResetToBusList;
                    LoadElem := Branchlist.FirstObject;
                    while LoadElem <> NIL do
                    begin
                        FSWrite(F, '-1, ');
                        FSWriteln(F, Format('%s.%s, %s', [LoadElem.ParentClass.Name, LoadElem.Name, LoadElem.Firstbus{ActiveCircuit.BusList.NameOfIndex(BranchList.PresentBranch.ToBusReference)}]));
                        LoadElem := BranchList.NextObject
                    end;
                    PDElem := BranchList.GoForward;
                end;
        end;

    finally
        FreeAndNil(F);
    end;
end;

procedure TEnergyMeterObj.DumpProperties(F: TFileStream; Complete: Boolean; Leaf: Boolean);
var
    i: Integer;
    pdelem: TPDelement;
    LoadElem: TDSSCktElement;
begin
    inherited DumpProperties(F, complete);

    with ParentClass do
        for i := 1 to NumProperties do
            FSWriteln(F, '~ ' + PropertyName^[i] + '=' + PropertyValue[i]);

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
                    FSWriteln(F, '   Shunt Element = ' + LoadElem.FullName);
                    LoadElem := BranchList.NextObject
                end;
                PDElem := BranchList.GoForward;
            end;
        end;
    end;
end;

function TEnergyMeterObj.AddToVoltBaseList(BusRef: Integer): Integer;
// Add to VoltBase list if not already there and return index
var
    i: Integer;
begin
    with DSS.ActiveCircuit.Buses^[BusRef] do
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
    // PREREQUISITE: EXECUTE CALCALLOCATIONFACTORS FOR ALL ENERGYMETERS AND SENSORS
    // ****Done in calling procedure  now ***   CalcAllocationFactors;     for this meter. Inherited from Meterelement
    // See ExecHelper

    // Now go through the meter's zone and adjust the loads.

    // While the AllocationFactor property is adjusted for all loads, it will only
    // have an effect on loads defined with either the XFKVA property or the
    // kWh property.

    // Loads have a SensorObj property that points to its upstream sensor that has the adjustments for
    // the allocation factors.  This is established in the MakeMeterZoneLists proc in this Unit.

    // Sensors consist of EnergyMeters, which drive the load allocation process and Sensor objects that
    // are simply voltage and current measuring points.  A Sensor may be attached to a line or transformer
    // or it may be connected directly to a load.

    CktElem := BranchList.First;
    while CktElem <> NIL do
    begin
        LoadElem := Branchlist.FirstObject;
        while (LoadElem <> NIL) do
        begin
            if (LoadElem.DSSObjType and CLASSMASK) = LOAD_ELEMENT then  // only for loads not other shunts
                case LoadElem.NPhases of
                    // For Single phase loads, allocate based on phase factor, else average factor
                    1:
                        with LoadElem do
                        begin
                            ConnectedPhase := DSS.ActiveCircuit.MapNodeToBus^[NodeRef^[1]].NodeNum;
                            if (ConnectedPhase > 0) and (ConnectedPhase < 4)   // Restrict to phases 1..3
                            then
                                if SensorObj.NPhases = 1 then
                                    AllocationFactor := AllocationFactor * SensorObj.PhsAllocationFactor^[1]
                                else
                                    AllocationFactor := AllocationFactor * SensorObj.PhsAllocationFactor^[ConnectedPhase];
                        end;
                else
                    with LoadElem do
                        AllocationFactor := AllocationFactor * SensorObj.AvgAllocFactor;
                end;
            LoadElem := BranchList.NextObject // Next load at this bus
        end;
        CktElem := BranchList.GoForward; // Go on down the tree
    end;
end;

procedure TEnergyMeterObj.Accumulate_Gen;
var
    S: Complex;
begin
     //----pGen.ActiveTerminalIdx := 1;
    S := -pGen.Power[1] * 0.001;
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
    with pLoad do
    begin
       //----ActiveTerminalIdx := 1;
        S_Load := pLoad.Power[1] * 0.001;   // Get Power in Terminal 1
        kW_Load := S_Load.re;
        Result := kw_Load;

        // Accumulate load in zone
        TotalZonekw := TotalZonekW + kW_Load;
        TotalZonekvar := TotalZonekvar + S_Load.im;

        // always integrate even if the value is 0.0
        // otherwise the Integrate function is not correct
       
        // Invoking the ExceedsNormal and Unserved Properties causes the factors to be computed
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

    end;
end;


procedure TEnergyMeterObj.ReduceZone;
// Reduce the zone by merging lines
begin
     // Make  sure zone list is built
    if not assigned(BranchList) then
        MakeMeterZoneLists;

    case DSS.ActiveCircuit.ReductionStrategy of

        rsShortlines:
            DoReduceShortLines(DSS, BranchList); // See ReduceAlgs.Pas
        // rsTapEnds:       DoReduceTapEnds (BranchList);
        rsMergeParallel:
            DoMergeParallelLines(DSS, BranchList);
        rsDangling:
            DoReduceDangling(DSS, BranchList);
        rsBreakLoop:
            DoBreakLoops(DSS, BranchList);
        rsSwitches:
            DoReduceSwitches(DSS, BranchList);
        rsLaterals:
            DoRemoveAll_1ph_Laterals(DSS, BranchList);

    else
        // Default
        DoReduceDefault(DSS, BranchList);
    end;
end;

function TEnergyMeterObj.CheckBranchList(code: Integer): Boolean;
begin
    if not Assigned(BranchList) then
    begin
        Result := FALSE;
        DoSimpleMsg(_('Meter Zone Lists need to be built. Do Solve or Makebuslist first!'), code);
        Exit;
    end;
    Result := TRUE;
end;


procedure TEnergyMeterObj.InterpolateCoordinates;
// Start at the ends of the zone and work toward the start
// interpolating between known coordinates
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
            SecondCoordRef := FirstCoordRef;  // so compiler won't issue stupid warning
            // Find a bus with a coordinate
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
                // Back up until we find another Coord defined
                LineCount := 0; // number of line segments in this segment
                StartNode := PresentNode;
                CktElem := PresentNode.CktObject;
                if FirstCoordRef <> PresentNode.FromBusReference then
                begin 
                    // Handle special case for end branch
                    if Buses^[PresentNode.FromBusReference].CoordDefined then
                        FirstCoordRef := PresentNode.FromBusReference
                    else
                        Inc(LineCount);
                end;

                repeat
                    Include(CktElem.Flags, Flg.Checked);
                    PresentNode := PresentNode.ParentBranch;
                    if PresentNode = NIL then
                        Break;
                    CktElem := PresentNode.CktObject;
                    SecondCoordRef := PresentNode.FromBusReference;
                    Inc(LineCount);
                until Buses^[SecondCoordRef].CoordDefined or (Flg.Checked in CktElem.Flags);

                if (PresentNode <> NIL) and (LineCount > 1) then
                    if Buses^[SecondCoordRef].CoordDefined then
                    begin
                        CalcBusCoordinates(StartNode, FirstCoordRef, SecondCoordRef, LineCount);
                    end
                    else
                        Break; // While - went as far as we could go this way

                FirstCoordRef := SecondCoordRef;
            end;
        end; // For
    end; // With
end;

procedure TEnergyMeterObj.CalcBusCoordinates(StartBranch: TCktTreeNode;
    FirstCoordRef, SecondCoordref, LineCount: Integer);
var
    X, Y, Xinc, Yinc: Double;
begin
    if LineCount = 1 then
        Exit;  // Nothing to do!

    with ActiveCircuit do
    begin
        Xinc := (Buses^[FirstCoordref].X - Buses^[SecondCoordRef].X) / LineCount;
        Yinc := (Buses^[FirstCoordref].Y - Buses^[SecondCoordRef].Y) / LineCount;

        X := Buses^[FirstCoordref].X;
        Y := Buses^[FirstCoordref].Y;

        // Either start with the "to" end of StartNode or the "from" end;
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
        DoSimpleMsg('%s Zone not defined properly.', [FullName], 52901);
        Exit;
    end;

    // Zero reliability accumulators
    for idx := SequenceList.Count downto 1 do
        TPDElement(SequenceList.Get(idx)).ZeroReliabilityAccums;

    // Backward sweep calculating failure rates
    for idx := SequenceList.Count downto 1 do
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
    pBus := DSS.ActiveCircuit.Buses^[PD_Elem.Terminals[PD_Elem.FromTerminal - 1].BusRef];
    pBus.Bus_Num_Interrupt := Source_NumInterruptions;
    pBus.BusCustInterrupts := Source_NumInterruptions * pBus.BusTotalNumCustomers;
    pBus.Bus_Int_Duration := Source_IntDuration;

    // init for defining sections
    SectionCount := 0;
    pBus.BusSectionID := SectionCount; // section before 1st OCP device is zero

    for idx := 1 to SequenceList.Count do
        TPDElement(SequenceList.Get(idx)).CalcNum_Int(SectionCount, AssumeRestoration);

    if SectionCount = 0 then
    begin // Error - no OCP devices
        DoSimpleMsg
        (_('Error: No Overcurrent Protection device (Relay, Recloser, or Fuse) defined. Aborting Reliability calc.'),
            52902);
        Exit;
    end;

    // Now have number of sections  so allocate FeederSections array
    ReallocMem(FeederSections, SizeOf(FeederSections^[1]) * (SectionCount + 1));
    for idx := 0 to SectionCount do
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
    for idx := SequenceList.Count downto 1 do
    begin
        PD_Elem := SequenceList.Get(idx);
        PD_Elem.CalcCustInterrupts;

        if PD_Elem.BranchSectionID <= 0 then
            continue;

        // Populate the Section properties
        pSection := @FeederSections^[PD_Elem.BranchSectionID];
        Inc(pSection.NCustomers, PD_Elem.BranchNumCustomers); // Sum up num Customers on this Section
        Inc(pSection.NBranches, 1); // Sum up num branches on this Section
        pBus := DSS.ActiveCircuit.Buses^[PD_Elem.Terminals[PD_Elem.ToTerminal - 1].BusRef];
        DblInc(pSection.SumBranchFltRates, pBus.Bus_Num_Interrupt * PD_Elem.BranchFltRate);
        DblInc(pSection.SumFltRatesXRepairHrs, (pBus.Bus_Num_Interrupt * PD_Elem.BranchFltRate * PD_Elem.HrsToRepair));
        if Flg.HasOCPDevice in PD_Elem.Flags then
        begin  // set Section properties
            pSection.OCPDeviceType := GetOCPDeviceType(PD_Elem);
            pSection.SeqIndex := idx;  // index of pdelement with OCP device at head of section
            pSection.TotalCustomers := PD_Elem.BranchTotalCustomers;
            pSection.SectFaultRate := PD_Elem.AccumulatedBrFltRate;
        end;

{$IFDEF DEBUG}
        if idx = SequenceList.Count then
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

    // Compute Avg Interruption duration of each Section  except 0 Section
    for idx := 1 to SectionCount do
        with FeederSections^[idx] do
            AverageRepairTime := SumFltRatesXRepairHrs / SumBranchFltRates;

    // Set Bus_int_Duration
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
    for idx := 0 to SectionCount do
        with FeederSections^[idx] do
            WriteDLLDebugFile(Format('%s.%s, %d, %d, %d, %.11g, %.11g, %.11g, %.11g ',
                [ParentClass.Name, Name, idx, NBranches, NCustomers, AverageRepairTime,
                AverageRepairTime * 60.0, SumFltRatesXRepairHrs, SumBranchFltRates]));
{$ENDIF}

    // Compute SAIFI based on numcustomers and load kW
    // SAIFI is weighted by specified load weights
    // SAIFI is for the EnergyMeter Zone
    SAIFI := 0.0;
    CAIDI := 0.0;
    SAIFIkW := 0.0;
    dblNcusts := 0.0;
    dblkW := 0.0;
    CustInterrupts := 0.0;

    // Use LoadList for SAIFI calculation
    with ActiveCircuit do
    begin
        for idx := 1 to LoadList.Count do  // all loads in meter zone
        begin
            // Compute CustInterrupts based on interrupts at each load
            with TLoadObj(LoadList.Get(idx)) do
            begin
                pBus := Buses^[Terminals[0].BusRef]; // pointer to Load's bus
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
    for idx := 1 to SectionCount do // ignore idx=0
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

procedure TEnergyMeterObj.SaveZone();
var
    cktElem, shuntElement: TDSSCktElement;
    LoadElement: TLoadObj;
    pControlElem: TDSSCktElement;
    FBranches, FShunts, FLoads, FGens, FCaps, FXfmrs: TFileStream;
    NBranches, NShunts, Nloads, NGens, NCaps, NXfmrs: Integer;
    dirname: String;
begin
    // We are in the directory indicated by DSS.CurrentDSSDir

    // Run down the zone and write each element into a file

    if BranchList = NIL then
        Exit;

    FBranches := nil;
    FShunts := nil;
    FLoads := nil;
    FGens := nil;
    FCaps := nil; 
    FXfmrs := nil;
        
    // Open some files:

    try
        FBranches := TBufferedFileStream.Create(DSS.CurrentDSSDir + 'Branches.dss', fmCreate);     // Both lines and transformers
        NBranches := 0;
    except
        On E: Exception do
        begin
            DoSimpleMsg('Error creating Branches.dss for Energymeter: %s. %s', [Self.Name, E.Message], 530);
            FreeAndNil(FBranches);
            Exit;
        end;
    end;

    try
        FXfmrs := TBufferedFileStream.Create(DSS.CurrentDSSDir + 'Transformers.dss', fmCreate);     // Both lines and transformers
        NXfmrs := 0;
    except
        On E: Exception do
        begin
            DoSimpleMsg('Error creating Transformers.dss for Energymeter: %s. %s', [Self.Name, E.Message], 53001);
            FreeAndNil(FXfmrs);
            Exit;
        end;
    end;

    try
        FShunts := TBufferedFileStream.Create(DSS.CurrentDSSDir + 'Shunts.dss', fmCreate);
        NShunts := 0;
    except
        On E: Exception do
        begin
            DoSimpleMsg('Error creating Shunts.dss for Energymeter: %s. %s', [Self.Name, E.Message], 531);
            FreeAndNil(FShunts);
            Exit;
        end;
    end;

    try
        FLoads := TBufferedFileStream.Create(DSS.CurrentDSSDir + 'Loads.dss', fmCreate);
        Nloads := 0;
    except
        On E: Exception do
        begin
            DoSimpleMsg('Error creating Loads.dss for Energymeter: %s. %s', [Self.Name, E.Message], 532);
            FreeAndNil(FLoads);
            Exit;
        end;
    end;

    try
        FGens := TBufferedFileStream.Create(DSS.CurrentDSSDir + 'Generators.dss', fmCreate);
        NGens := 0;
    except
        On E: Exception do
        begin
            DoSimpleMsg('Error creating Generators.dss for Energymeter: %s. %s', [Self.Name, E.Message], 533);
            FreeAndNil(FGens);
            Exit;
        end;
    end;

    try
        FCaps := TBufferedFileStream.Create(DSS.CurrentDSSDir + 'Capacitors.dss', fmCreate);
        Ncaps := 0;
    except
        On E: Exception do
        begin
            DoSimpleMsg('Error creating Capacitors.dss for Energymeter: %s. %s', [Self.Name, E.Message], 534);
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
                    WriteActiveDSSObject(DSS, FXfmrs, 'New');     // sets HasBeenSaved := TRUE
                    if Flg.HasControl in cktElem.Flags then
                    begin
                        pControlElem := cktElem.ControlElementList.First;
                        while pControlElem <> NIL do
                        begin
                            ActiveCktElement := pControlElem;
                            WriteActiveDSSObject(DSS, FXfmrs, 'New');  //  regulator control ...Also, relays, switch controls
                            pControlElem := cktElem.ControlElementList.Next;
                        end;
                    end;
                end
                else
                begin  
                    // Mostly LINE elements
                    Inc(NBranches);
                    WriteActiveDSSObject(DSS, FBranches, 'New');     // sets HasBeenSaved := TRUE
                    if Flg.HasControl in cktElem.Flags then
                    begin
                        pControlElem := cktElem.ControlElementList.First;
                        while pControlElem <> NIL do
                        begin
                            ActiveCktElement := pControlElem;
                            WriteActiveDSSObject(DSS, FBranches, 'New');  //  regulator control ...Also, relays, switch controls
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
                            // Manually set the allocation factor so it shows up
                            DSS.Parser.CmdString := 'allocationfactor=' + Format('%-.4g', [LoadElement.AllocationFactor]);
                            LoadElement.Edit(DSS.Parser);
                        end;
                        ActiveCktElement := shuntElement; // reset in case Edit mangles it
                        Inc(NLoads);
                        WriteActiveDSSObject(DSS, FLoads, 'New');
                    end
                    else
                    if (shuntElement.DSSObjType and Classmask) = GEN_ELEMENT then
                    begin
                        Inc(NGens);
                        WriteActiveDSSObject(DSS, FGens, 'New');
                        if Flg.HasControl in shuntElement.Flags then
                        begin
                            pControlElem := shuntElement.ControlElementList.First;
                            while pControlElem <> NIL do
                            begin
                                ActiveCktElement := pControlElem;
                                WriteActiveDSSObject(DSS, FGens, 'New');
                                pControlElem := shuntElement.ControlElementList.Next;
                            end;
                        end;
                    end
                    else
                    if (shuntElement.DSSObjType and Classmask) = CAP_ELEMENT then
                    begin
                        Inc(NCaps);
                        WriteActiveDSSObject(DSS, FCaps, 'New');
                        if Flg.HasControl in shuntElement.Flags then
                        begin
                            pControlElem := shuntElement.ControlElementList.First;
                            while pControlElem <> NIL do
                            begin
                                ActiveCktElement := pControlElem;
                                WriteActiveDSSObject(DSS, FCaps, 'New');
                                pControlElem := shuntElement.ControlElementList.Next;
                            end;
                        end;
                    end
                    else
                    begin
                        Inc(NShunts);
                        WriteActiveDSSObject(DSS, Fshunts, 'New');
                    end;
                    shuntElement := BranchList.NextObject
                end;
            end; // if enabled

            cktElem := BranchList.GoForward;
        end; // WHILE

    FreeAndNil(FBranches);
    FreeAndNil(FXfmrs);
    FreeAndNil(Fshunts);
    FreeAndNil(FLoads);
    FreeAndNil(FGens);
    FreeAndNil(FCaps);

    // If any records were written to the file, record their names (will be post-processed)
    dirname := DSS.CurrentDSSDir; // PathDelim already included here

    if NBranches > 0 then
        DSS.SavedFileList.Add(dirname + 'Branches.dss')
    else
        DeleteFile('Branches.dss');
    if NXfmrs > 0 then
        DSS.SavedFileList.Add(dirname + 'Transformers.dss')
    else
        DeleteFile('Transformers.dss');
    if NShunts > 0 then
        DSS.SavedFileList.Add(dirname + 'Shunts.dss')
    else
        DeleteFile('Shunts.dss');
    if NLoads > 0 then
        DSS.SavedFileList.Add(dirname + 'Loads.dss')
    else
        DeleteFile('Loads.dss');
    if NGens > 0 then
        DSS.SavedFileList.Add(dirname + 'Generators.dss')
    else
        DeleteFile('Generators.dss');
    if NCaps > 0 then
        DSS.SavedFileList.Add(dirname + 'Capacitors.dss')
    else
        DeleteFile('Capacitors.dss');
end;

procedure TEnergyMeterObj.GetPCEatZone(const allowEmpty: Boolean);
var
    cktElem,
    shuntElement: TDSSCktElement;
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
                    ZonePCE[high(ZonePCE)] := shuntElement.FullName;
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
                CloseMHandler(DSS, DI_MHandle, MakeDIFileName, DI_Append);
            This_Meter_DIFileIsOpen := FALSE;
            if PHV_MHandle <> NIL then
                if VPhaseReportFileIsOpen then
                    CloseMHandler(DSS, PHV_MHandle, MakeVPhaseReportFileName, PHV_Append);
            VPhaseReportFileIsOpen := FALSE;
        end;
    except
        ON E: Exception do
            DoSimpleMsg('Error Closing Demand Interval file for Meter "%s"', [Name], 534);
    end;

    // Write Registers to Totals File
    with DSS.EnergyMeterClass do
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

        if (DSS.EnergyMeterClass.DI_Verbose) then
        begin
            This_Meter_DIFileIsOpen := TRUE;
            if DI_MHandle <> NIL then
                DI_MHandle.free;
            DI_MHandle := Create_Meter_Space('"Hour"');
            for i := 1 to NumEMRegisters do
                WriteintoMemStr(DI_MHandle, ', "' + RegisterNames[i] + '"');
            WriteintoMemStr(DI_MHandle, Char(10));

            // Phase Voltage Report, if requested
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
            DoSimpleMsg('Error opening demand interval file "%s.csv" for writing. %s', [Name + DSS._Name, CRLF + E.Message], 535);
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
    if DSS.EnergyMeterClass.DI_Verbose and This_Meter_DIFileIsOpen then
    begin
        with DSS.ActiveCircuit.Solution do
            WriteintoMem(DI_MHandle, DynaVars.dblHour);
        for i := 1 to NumEMRegisters do
            WriteintoMem(DI_MHandle, Derivatives[i]);
        WriteIntoMemStr(DI_MHandle, Char(10));
    end;

    // Add to Class demand interval registers
    with DSS.EnergyMeterClass do
        for i := 1 to NumEMRegisters do
            DI_RegisterTotals[i] := DI_RegisterTotals[i] + Derivatives[i] * TotalsMask[i];


    // Phase Voltage Report, if requested
    if VPhaseReportFileIsOpen then
    begin
        with DSS.ActiveCircuit.Solution do
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
        // While closing DI files, write all meter registers to one file
        try
            CreateMeterTotals;
        except
            On E: Exception do
                DoSimpleMsg('Error on Rewrite of totals file: %s', [E.Message], 536);
        end;

        // Close all the DI file for each meter
        mtr := DSS.ActiveCircuit.EnergyMeters.First;
        while mtr <> NIL do
        begin
            if mtr.enabled then
                mtr.CloseDemandIntervalFile;
            mtr := DSS.ActiveCircuit.EnergyMeters.Next;
        end;

        WriteTotalsFile;  // Sum all energymeter registers to "Totals_{}.csv"
        SystemMeter.CloseDemandIntervalFile;
        SystemMeter.Save;
        if EMT_MHandle <> NIL then
            CloseMHandler(DSS, EMT_MHandle, DI_Dir + PathDelim + 'EnergyMeterTotals' + DSS._Name + '.csv', EMT_Append);
        if TDI_MHandle <> NIL then
            CloseMHandler(DSS, TDI_MHandle, DI_Dir + PathDelim + 'DI_Totals' + DSS._Name + '.csv', TDI_Append);
        DSS.DIFilesAreOpen := FALSE;
        if OverloadFileIsOpen then
        begin
            if OV_MHandle <> NIL then
                CloseMHandler(DSS, OV_MHandle, DSS.EnergyMeterClass.DI_Dir + PathDelim + 'DI_Overloads' + DSS._Name + '.csv', OV_Append);
            OverloadFileIsOpen := FALSE;
        end;
        if VoltageFileIsOpen then
        begin
            if VR_MHandle <> NIL then
                CloseMHandler(DSS, VR_MHandle, DSS.EnergyMeterClass.DI_Dir + PathDelim + 'DI_VoltExceptions' + DSS._Name + '.csv', VR_Append);
            VoltageFileIsOpen := FALSE;
        end;
    end;
end;

procedure TEnergyMeterObj.AppendDemandIntervalFile;
var
    FileNm: String;
begin
    // Only called if "SaveDemandInterval"

    if This_Meter_DIFileIsOpen then
        Exit;

    try
        if DSS.Energymeterclass.FDI_Verbose then
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
            DoSimpleMsg('Error opening demand interval file "%s.csv" for appending. %s', [Name + DSS._Name, CRLF + E.Message], 537);
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

        mtr := DSS.ActiveCircuit.EnergyMeters.First;
        while mtr <> NIL do
        begin
            if mtr.enabled then
                mtr.AppendDemandIntervalFile;
            mtr := DSS.ActiveCircuit.EnergyMeters.Next;
        end;

        SystemMeter.AppendDemandIntervalFile;

        // Open FDI_Totals
        try
            FileNm := DI_Dir + PathDelim + 'DI_Totals' + DSS._Name + '.csv';
            
            // File Must Exist
            if FileExists(FileNm) then
                TDI_Append := TRUE;
            CreateFDI_Totals;
        except
            On E: Exception do
                DoSimpleMsg('Error opening demand interval file "%s.csv" for appending.', [Name + DSS._Name, CRLF + E.Message], 538);
        end;

        DSS.DIFilesAreOpen := TRUE;

    end;
end;

function TEnergyMeterObj.MakeDIFileName: String;
begin
    Result := DSS.EnergyMeterClass.DI_Dir + PathDelim + Self.Name + DSS._Name + '.csv';
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
    dVector,
    dBuffer: pDoubleArray;

begin
// Scans the active circuit for overloaded PD elements and writes each to a file
// This is called only if in Demand Interval (DI) mode and the file is open.

    // Prepares everything for using seasonal ratings if required
    RatingIdx := -1;
    if DSS.SeasonalRating then
    begin
        if DSS.SeasonSignal <> '' then
        begin
            RSignal := DSS.XYCurveClass.Find(DSS.SeasonSignal);
            if RSignal <> NIL then
                RatingIdx := trunc(RSignal.GetYValue(ActiveCircuit.Solution.DynaVars.intHour))
            else
                DSS.SeasonalRating := FALSE;   // The XYCurve defined doesn't exist
        end
        else
            DSS.SeasonalRating := FALSE;    // The user didn't define the seasonal signal
    end;

    // CHECK PDELEMENTS ONLY
    PDelem := DSS.ActiveCircuit.PDElements.First;
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
                ClassName := AnsiLowerCase(PDElem.DSSClassName);
                if DSS.SeasonalRating and (ClassName = 'line') and (PDElem.NumAmpRatings > 1) then
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
                    dBuffer := Allocmem(sizeof(Double) * PDElem.NPhases * PDElem.NTerms);
                    PDElem.Get_Current_Mags(dBuffer);
                    dVector := Allocmem(sizeof(Double) * 3); // for storing
                    for i := 1 to 3 do
                        dVector^[i] := 0.0;
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
                                dVector^[k] := dBuffer^[i];
                                break
                            end
                            else
                            begin
                                k := strtoint(ClassName.Substring(0, j - 1));
                                dVector^[k] := dBuffer^[i];
                                ClassName := ClassName.Substring(j);
                            end;
                        end;
                    end
                    else
                    begin
                        for i := 1 to 3 do
                            dVector^[i] := dBuffer^[i];
                    end;

                    with DSS.ActiveCircuit.Solution do
                        WriteintoMem(OV_MHandle, DynaVars.dblHour);
                    WriteintoMemStr(OV_MHandle, ', ' + EncloseQuotes(PDelem.FullName));
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
                        WriteintoMem(OV_MHandle, dVector^[i]);

                    WriteintoMemStr(OV_MHandle, ' ' + Char(10));

                end;
            end;
        end;
        PDelem := DSS.ActiveCircuit.PDElements.Next;
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
        mtr := DSS.ActiveCircuit.EnergyMeters.First;  // just get the first one
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
            DoSimpleMsg('Error creating: "%sDI_Totals%s.csv": %s', [DI_Dir + PathDelim, DSS._Name, E.Message], 539)
    end;
end;

procedure TSystemMeter.AppendDemandIntervalFile;
var
    FileNm: String;
begin
    // Only called if "SaveDemandInterval"

    if This_Meter_DIFileIsOpen then
        Exit;

    try
        FileNm := DSS.EnergyMeterClass.Di_Dir + PathDelim + 'DI_SystemMeter' + DSS._Name + '.csv';
        // File Must Exist
        if FileExists(FileNm) then
        begin
//        DI_MMFView:=  MapFile2Memory(EnergyMeterClass.DI_Dir+ PathDelim + 'DI_SystemMeter' + DSS._Name + '.csv', DI_MMFHandle);
//        DI_Cursor :=  GetMMFCursor(DI_MMFView);
        end
        else
            OpenDemandIntervalFile;
        This_Meter_DIFileIsOpen := TRUE;
    except
        On E: Exception do
        begin
            DosimpleMsg(DSS, 'Error opening demand interval file "%s" for appending. %s', [FileNm, CRLF + E.Message], 540);
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
    if This_Meter_DIFileIsOpen then with DSS.EnergyMeterClass do
    begin
        File_Path := DSS.EnergyMeterClass.DI_Dir + PathDelim + 'DI_SystemMeter' + DSS._Name + '.csv';
        CloseMHandler(DSS, SDI_MHandle, File_Path, DSS.EnergyMeterClass.SDI_Append);
        This_Meter_DIFileIsOpen := FALSE;
    end;
end;

constructor TSystemMeter.Create(EnergyMeterClass: TEnergyMeter);
begin
    DSS := EnergyMeterClass.DSS;
    Clear;
    This_Meter_DIFileIsOpen := FALSE;
    with EnergyMeterClass do
    begin
        SDI_MHandle := NIL;
        TDI_MHandle := NIL;
        VR_MHandle := NIL;
        OV_MHandle := NIL;
    end;
end;

destructor TSystemMeter.Destroy;
begin
    with DSS.EnergyMeterClass do
    begin
        if SDI_MHandle <> NIL then
            FreeAndNil(SDI_MHandle);
        if TDI_MHandle <> NIL then
            FreeAndNil(TDI_MHandle);
        if VR_MHandle <> NIL then
            FreeAndNil(VR_MHandle);
        if OV_MHandle <> NIL then
            FreeAndNil(OV_MHandle);
    end;
    inherited;
end;

procedure TSystemMeter.Integrate(var Reg: Double; Value: Double; var Deriv: Double);
begin
    if DSS.ActiveCircuit.TrapezoidalIntegration then
    begin
        // Trapezoidal Rule Integration
        if not FirstSampleAfterReset then
            Reg := Reg + 0.5 * DSS.ActiveCircuit.Solution.IntervalHrs * (Value + Deriv);
    end
    else   
        // Plain Euler integration
        Reg := Reg + DSS.ActiveCircuit.Solution.IntervalHrs * Value;

    Deriv := Value;
end;

procedure TSystemMeter.OpenDemandIntervalFile;
begin
    try
        with DSS.EnergyMeterClass do
        begin
            if This_Meter_DIFileIsOpen then
                SDI_MHandle.Free;
            This_Meter_DIFileIsOpen := TRUE;
            if SDI_MHandle <> NIL then
                SDI_MHandle.free;
            SDI_MHandle := Create_Meter_Space('"Hour", ');
            WriteintoMemStr(SDI_MHandle, 'kWh, kvarh, "Peak kW", "peak kVA", "Losses kWh", "Losses kvarh", "Peak Losses kW"' + Char(10));
        end;
    except
        On E: Exception do
            DoSimpleMsg(DSS, 'Error opening demand interval file "DI_SystemMeter%s.csv" for writing.', [DSS._Name, CRLF + E.Message], 541);
    end;
end;

procedure TSystemMeter.Reset;
begin
    Clear;
   // removed - open in solution If EnergyMeterClass.SaveDemandInterval Then OpenDemandIntervalFile;
end;

procedure TSystemMeter.Save;
var
    CSVName, Folder: String;
begin
    try
        CSVName := 'SystemMeter' + DSS._Name + '.csv';
        // If we are doing a simulation and saving interval data, create this in the
        // same directory as the demand interval data
        if DSS.energyMeterClass.SaveDemandInterval then
            Folder := DSS.energyMeterClass.DI_DIR + PathDelim
        else
            Folder := DSS.OutputDirectory;
        DSS.GlobalResult := CSVName;
        SetLastResultFile(DSS, CSVName);

    except
        On E: Exception do
        begin
            DoSimpleMsg(DSS, 'Error opening System Meter File "%s": %s', [CSVName, E.Message], 542);
            Exit;
        end
    end;

    with DSS.EnergyMeterClass do
        try
            if SM_MHandle <> NIL then
                FreeAndNil(SM_MHandle);
            SM_MHandle := Create_Meter_Space('Year, ');
            WriteintoMemStr(SM_MHandle, 'kWh, kvarh, "Peak kW", "peak kVA", "Losses kWh", "Losses kvarh", "Peak Losses kW"' + Char(10));
            WriteintoMemStr(SM_MHandle, inttostr(DSS.ActiveCircuit.Solution.Year));
            WriteRegisters();
            WriteintoMemStr(SM_MHandle, Char(10));

        finally
            CloseMHandler(DSS, SM_MHandle, Folder + CSVName, SM_Append);
        end;
end;

procedure TSystemMeter.TakeSample;
begin
    // Get total system energy out of the sources

    cPower := GetTotalPowerFromSources(DSS) * 0.001;  // convert to kW

    Integrate(kWh, cPower.re, dkwh);
    Integrate(kvarh, cPower.im, dkvarh);

    PeakkW := Max(cPower.re, PeakkW);
    Peakkva := Max(Cabs(cPower), Peakkva);

    // Get total circuit losses
    cLosses := DSS.ActiveCircuit.Losses;  // PD Elements except shunts
    cLosses := cLosses * 0.001;  // convert to kW

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
    mtr := DSS.ActiveCircuit.EnergyMeters.First;
    if Assigned(mtr) then
        for i := 1 to NumEMRegisters do
            WriteintoMemStr(EMT_MHandle, ', "' + mtr.RegisterNames[i] + '"');
    WriteintoMemStr(EMT_MHandle, Char(10));
end;

procedure TSystemMeter.WriteDemandIntervalData;
begin
    with DSS.EnergyMeterClass do
    begin
        with DSS.ActiveCircuit.Solution do
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
end;

procedure TSystemMeter.WriteRegisters();
begin
    with DSS.EnergyMeterClass do
    begin
        WriteintoMem(SM_MHandle, kWh);
        WriteintoMem(SM_MHandle, kvarh);
        WriteintoMem(SM_MHandle, peakkW);
        WriteintoMem(SM_MHandle, peakkVA);
        WriteintoMem(SM_MHandle, Losseskwh);
        WriteintoMem(SM_MHandle, Losseskvarh);
        WriteintoMem(SM_MHandle, PeakLosseskW);
    end;
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
    // Sum up all registers of all meters and write to Totals.csv
    for i := 1 to NumEMRegisters do
        RegSum[i] := 0.0;

    mtr := DSS.ActiveCircuit.EnergyMeters.First;
    while mtr <> NIL do
    begin
        if mtr.enabled then
            with Mtr do
                for i := 1 to NumEMRegisters do
                    Regsum[i] := Regsum[i] + Registers[i] * TotalsMask[i];

        mtr := DSS.ActiveCircuit.EnergyMeters.Next;
    end;

    try     // Writes the file
        if FM_MHandle <> NIL then
            FM_MHandle.Free;
        FM_MHandle := Create_Meter_Space('Year');
        mtr := DSS.ActiveCircuit.EnergyMeters.First;
        if assigned(mtr) then
            for i := 1 to NumEMRegisters do
                WriteintoMemStr(FM_MHandle, ', "' + mtr.RegisterNames[i] + '"'); //Write(F,', "', mtr.RegisterNames[i],'"');
        WriteintoMemStr(FM_MHandle, Char(10));

        WriteintoMemStr(FM_MHandle, inttostr(ActiveCircuit.Solution.Year));
        for i := 1 to NumEMRegisters do
            WriteintoMem(FM_MHandle, Double(RegSum[i]));
        WriteintoMemStr(FM_MHandle, Char(10));
        CloseMHandler(DSS, FM_MHandle, DI_Dir + PathDelim + 'Totals' + DSS._Name + '.csv', FM_Append);
    except
        On E: Exception do
            DoSimpleMsg('Error writing demand interval file Totals%s.csv. %s', [DSS._Name, CRLF + E.Message], 543);
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
    // For any bus with a defined voltage base, test for > Vmax or < Vmin

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
                        Vmagpu := Cabs(Solution.NodeV^[RefNo[j]]) / kvbase * 0.001;
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
            end; // For i

        with Solution do
            WriteintoMem(VR_MHandle, DynaVars.dblHour);
        WriteintoMemStr(VR_MHandle, ', ' + inttostr(UnderCount));
        WriteintoMem(VR_MHandle, UnderVmin);
        WriteintoMemStr(VR_MHandle, ', ' + inttostr(OverCount));
        WriteintoMem(VR_MHandle, OverVmax);
        WriteintoMemStr(VR_MHandle, ', ' + BusList.NameOfIndex(minbus));
        WriteintoMemStr(VR_MHandle, ', ' + BusList.NameOfIndex(maxbus));

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
                        Vmagpu := Cabs(Solution.NodeV^[RefNo[j]]) / kvbase * 0.001;
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
            end; // For i

        WriteintoMemStr(VR_MHandle, ', ' + inttostr(UnderCount));
        WriteintoMem(VR_MHandle, UnderVmin);
        WriteintoMemStr(VR_MHandle, ', ' + inttostr(OverCount));
        WriteintoMem(VR_MHandle, OverVmax);
        WriteintoMemStr(VR_MHandle, ', ' + BusList.NameOfIndex(minbus));
        WriteintoMemStr(VR_MHandle, ', ' + BusList.NameOfIndex(maxbus));
        WriteintoMemStr(VR_MHandle, Char(10));
    end;
end;

procedure TEnergyMeter.OpenAllDIFiles;
// Similar to Append, by creates the files.
var
    mtr: TEnergyMeterObj;
  // Filenm:String;
begin
    if FSaveDemandInterval then
    begin
        ClearDI_Totals;  // clears accumulator arrays

        mtr := DSS.ActiveCircuit.EnergyMeters.First;
        while mtr <> NIL do
        begin
            if mtr.enabled then
                mtr.OpenDemandIntervalFile;
            mtr := DSS.ActiveCircuit.EnergyMeters.Next;
        end;

        SystemMeter.OpenDemandIntervalFile;

        // Optional Exception Reporting
        if Do_OverloadReport then
            OpenOverloadReportFile;
        if Do_VoltageExceptionReport then
            OpenVoltageReportFile;

        // Open FDI_Totals
        try
            CreateFDI_Totals;

        except
            On E: Exception do
                DoSimpleMsg('Error creating the memory space for demand interval "%s.csv" for appending.', [Name + DSS._Name, CRLF + E.Message], 538);
        end;

        DSS.DIFilesAreOpen := TRUE;

    end;
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
            DosimpleMsg('Error creating memory space (Overload report) for writing: %s', [E.Message], 541);
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
            DosimpleMsg('Error creating memory space (Voltage report) for writing: %s', [E.Message], 541);
    end;
end;

finalization
    ActionEnum.Free;
end.
