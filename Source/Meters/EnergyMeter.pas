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
}

{$WARN UNIT_PLATFORM OFF}

interface

Uses DSSClass,
     MeterClass,
     MeterElement,
     CktElement,
     PDElement,
     arrayDef,
     PointerList,
     CktTree,
     ucomplex,
     Feeder,
     Load,
     Generator,
     Command;

Const
    NumEMVbase = 7;
    NumEMRegisters = 32 + 5 * NumEMVbase;   // Total Number of energy meter registers
    {Fixed Registers}
     Reg_kWh               = 1;
     Reg_kvarh             = 2;
     Reg_MaxkW             = 3;
     Reg_MaxkVA            = 4;
     Reg_ZonekWh           = 5;
     Reg_Zonekvarh         = 6;
     Reg_ZoneMaxkW         = 7;
     Reg_ZoneMaxkVA        = 8;
     Reg_OverloadkWhNorm   = 9;    // Max overload
     Reg_OverloadkWhEmerg  = 10;
     Reg_LoadEEN           = 11;
     Reg_LoadUE            = 12;  // Energy served below normal voltage
     Reg_ZoneLosseskWh     = 13;
     Reg_ZoneLosseskvarh   = 14;
     Reg_LossesMaxkW       = 15;
     Reg_LossesMaxkvar     = 16;
     Reg_LoadLosseskWh     = 17;
     Reg_LoadLosseskvarh   = 18;
     Reg_NoLoadLosseskWh   = 19;
     Reg_NoLoadLosseskvarh = 20;
     Reg_MaxLoadLosses     = 21;
     Reg_MaxNoLoadLosses   = 22;
     Reg_LineLosseskWh     = 23;
     Reg_TransformerLosseskWh = 24;
     Reg_LineModeLineLoss  = 25;    // for 3-phase feeder lines
     Reg_ZeroModeLineLoss  = 26;
     Reg_3_phaseLineLoss   = 27;
     Reg_1_phaseLineLoss   = 28;
     Reg_GenkWh            = 29;
     Reg_Genkvarh          = 30;
     Reg_GenMaxkW          = 31;
     Reg_GenMaxkVA         = 32;
     Reg_VBaseStart        = 32;  // anchor for the voltage base loss registers

Type
   TRegisterArray = Array[1..NumEMregisters] of Double;

    //  --------- Feeder Section Definition -----------
    TFeederSection=Record
        OCPDeviceType        : Integer;  // 1=Fuse; 2=Recloser; 3=Relay
        NCustomers           : Integer;
        NBranches            : Integer;
        TotalCustomers       : Integer;
        SeqIndex             : Integer;  // index of pdelement with OCP device at head of section
        AverageRepairTime    : double;
        SectFaultRate       : Double;
        SumFltRatesXRepairHrs: double;
        SumBranchFltRates    : double;
    End;

    pFeederSections = ^FeederSectionArray;
    FeederSectionArray = Array[1..1] of TFeederSection;
    //  --------- Feeder Section Definition -----------

   TSystemMeter = Class(Tobject)
     private
        kWh, dkWh,
        kvarh, dkvarh,
        peakkW,
        peakkVA,
        Losseskwh,  dLosseskWh,
        Losseskvarh, dlosseskvarh,
        PeakLosseskW            :Double;
        FirstSampleAfterReset,
        This_Meter_DIFileIsOpen :Boolean;
        SystemDIFile            :TextFile;
        cPower, cLosses         :Complex;

        Procedure Clear;
        Procedure Integrate(Var Reg:Double; Value:Double; Var Deriv:Double; ActorID: integer);
        Procedure WriteRegisters(Var F:TextFile; ActorID: Integer);
        Procedure WriteRegisterNames(Var F:TextFile);

     protected

        Procedure OpenDemandIntervalFile(ActorID:integer);
        Procedure WriteDemandIntervalData(ActorID:integer);
        Procedure CloseDemandIntervalFile(ActorID: integer);
        Procedure AppendDemandIntervalFile(ActorID: integer);

     public

       Procedure TakeSample(ActorID:Integer);
       Procedure Reset;
       Procedure Save(ActorID: integer);

       constructor Create;
       destructor Destroy; override;

   end;

   TEnergyMeter = class(TMeterClass)    // derive strait from base class
     private
        GeneratorClass        : TGenerator;
        FSaveDemandInterval   : Boolean;
        FDI_Verbose           : Boolean;
        FOverLoadFile         : Textfile;
        FVoltageFile          : TextFile;

        PROCEDURE ProcessOptions(Const Opts:String);
        procedure Set_SaveDemandInterval(ActorID: integer; const Value: Boolean);
        function Get_SaveDemandInterval(ActorID: integer):Boolean;
        Procedure CreateMeterTotals(ActorID:integer);
        Procedure CreateFDI_Totals(ActorID: integer);
        Procedure ClearDI_Totals;
        Procedure WriteTotalsFile(ActorID: integer);
        Procedure OpenOverloadReportFile(ActorID: integer);
        Procedure OpenVoltageReportFile(ActorID: integer);
        Procedure WriteOverloadReport(ActorID : Integer);
        Procedure WriteVoltageReport(ActorID:integer);
        Procedure InterpretRegisterMaskArray(Var Mask:TRegisterArray; ActorID : Integer);
        procedure Set_DI_Verbose(ActorID:integer; const Value: Boolean);
        function  Get_DI_Verbose(ActorID: integer): Boolean;

     Protected
        Procedure DefineProperties;
        Function  MakeLike(Const EnergyMeterName:String):Integer;   Override;
        procedure SetHasMeterFlag(ActorID: integer);

     public

       DI_RegisterTotals      :TRegisterArray;
       DI_Dir                 :String;
       FDI_Totals             :TextFile;
       FMeterTotals           :TextFile;

       SystemMeter                :TSystemMeter;
       Do_OverloadReport          :Boolean;
       Do_VoltageExceptionReport  :Boolean;
       OverLoadFileIsOpen         :Boolean;
       VoltageFileIsOpen          :Boolean;


       constructor Create;
       destructor Destroy;     override;

       Function Edit(ActorID : Integer):Integer;  override;     // uses global parser
       Function Init(Handle:Integer; ActorID : Integer):Integer;            override;
       Function NewObject(const ObjName:String):Integer; override;

       Procedure ResetMeterZonesAll(ActorID : Integer);
       Procedure ResetAll(ActorID : Integer);  Override;  // Reset all meters in active circuit to zero
       Procedure SampleAll(ActorID : Integer); Override;   // Force all meters in active circuit to sample
       Procedure SaveAll(ActorID : Integer);   Override;

       Procedure AppendAllDIFiles(ActorID:integer);
       Procedure OpenAllDIFiles(ActorID:Integer);
       Procedure CloseAllDIFiles(ActorID:Integer);

       Property SaveDemandInterval[ActorID: integer]:Boolean Read Get_SaveDemandInterval Write Set_SaveDemandInterval;
       Property DI_Verbose[ActorID: integer]:Boolean Read Get_DI_Verbose Write Set_DI_Verbose;

   end;

   TEnergyMeterObj = class(TMeterElement)
      Private
       FirstSampleAfterReset  :Boolean;
       ExcessFlag             :Boolean;
       ZoneIsRadial           :Boolean;
       VoltageUEOnly          :Boolean;
       LocalOnly              :Boolean;
       HasFeeder              :Boolean;

       FLosses                :Boolean;
       FLineLosses            :Boolean;
       FXfmrLosses            :Boolean;
       FSeqLosses             :Boolean;
       F3PhaseLosses          :Boolean;
       FVBaseLosses           :Boolean;
       FPhaseVoltageReport    :Boolean;

       FeederObj              :TFeederObj;   // not used at present
       DefinedZoneList        :pStringArray;
       DefinedZoneListSize    :Integer;

       {Limits on the entire load in the zone for networks where UE cannot be determined
        by the individual branches}
       MaxZonekVA_Norm        :Double;
       MaxZonekVA_Emerg       :Double;

       {Voltage bases in the Meter Zone}
       VBaseTotalLosses       :pDoubleArray;    // allocated array
       VBaseLineLosses        :pDoubleArray;
       VBaseLoadLosses        :pDoubleArray;
       VBaseNoLoadLosses      :pDoubleArray;
       VBaseLoad              :pDoubleArray;
       VBaseList              :pDoubleArray;
       VBaseCount             :Integer;
       MaxVBaseCount          :Integer;

       { Arrays for phase voltage report  }
       VphaseMax              :pDoubleArray;
       VPhaseMin              :pDoubleArray;
       VPhaseAccum            :pDoubleArray;
       VPhaseAccumCount       :pIntegerArray;
       VPhase_File            :TextFile;
       VPhaseReportFileIsOpen :Boolean;

       {Demand Interval File variables}
       DI_File                :TextFile;
       This_Meter_DIFileIsOpen:Boolean;


       Procedure Integrate(Reg:Integer; const Deriv:Double; Const Interval:Double; ActorID: integer);
       Procedure SetDragHandRegister( Reg:Integer; const Value:Double);
       Function  Accumulate_Load(pLoad:TLoadObj; var TotalZonekW, TotalZonekvar, TotalLoad_EEN, TotalLoad_UE:Double; ActorID : Integer):double;
       Procedure Accumulate_Gen(pGen:TGeneratorObj; var TotalZonekW, TotalZonekvar:Double;ActorID:integer);
       Procedure CalcBusCoordinates(StartBranch:TCktTreeNode; FirstCoordRef, SecondCoordRef, LineCount:Integer);
       Function  AddToVoltBaseList(BusRef:Integer; ActorID: Integer):Integer;
       Function  MakeDIFileName(ActorID:integer):String;
       Function  MakeVPhaseReportFileName(ActorID: integer):String;
       Procedure AssignVoltBaseRegisterNames;

    // Not used   Procedure MakeFeederObj;
    // Not used   Procedure RemoveFeederObj;
       Procedure TotalupDownstreamCustomers;



      Protected

        Procedure OpenDemandIntervalFile(ActorID: integer);
        Procedure WriteDemandIntervalData(ActorID: integer);
        Procedure CloseDemandIntervalFile(ActorID: integer);
        Procedure AppendDemandIntervalFile(ActorID:integer);

      Public
        RegisterNames: Array[1..NumEMregisters] of String;

        BranchList            : TCktTree;      // Pointers to all circuit elements in meter's zone
        SequenceList          : PointerList.TPointerList;  // Pointers to branches in sequence from meter to ends
        LoadList              : PointerList.TPointerList;  // Pointers to Loads in the Meter zone to aid reliability calcs

        Registers             : TRegisterArray;
        Derivatives           : TRegisterArray;
        TotalsMask            : TRegisterArray;

        // Reliability data for Head of Zone
        SAIFI                 : Double;     // For this Zone - based on number of customers
        SAIFIkW               : Double;     // For this Zone - based on kW load
        SAIDI                 : Double;
        CAIDI                 : Double;
        CustInterrupts        : Double;

        // Source reliability
        Source_NumInterruptions : Double; // Annual interruptions for upline circuit
        Source_IntDuration      : Double; // Aver interruption duration of upline circuit

        SectionCount          : Integer;
        ActiveSection         : Integer;  // For COM interface to index into FeederSections array
        FeederSections        : pFeederSections;


        constructor Create(ParClass:TDSSClass; const EnergyMeterName:String);
        destructor Destroy; override;

        PROCEDURE MakePosSequence(ActorID : Integer); Override;  // Make a positive Sequence Model, reset nphases
        Procedure RecalcElementData(ActorID : Integer);Override;
        Procedure CalcYPrim(ActorID : Integer);Override;
        Procedure GetCurrents(Curr: pComplexArray; ActorID : Integer); Override; //Get present value of terminal Curr
        Procedure GetInjCurrents(Curr: pComplexArray; ActorID : Integer); Override;   // Returns Injextion currents

        Procedure ResetRegisters;
        Procedure TakeSample(ActorID : Integer);Override;
        Procedure SaveRegisters(ActorID: integer);
        Procedure MakeMeterZoneLists(ActorID:Integer);
        Procedure ZoneDump(ActorID:Integer);
        Procedure InterpolateCoordinates;
        Procedure EnableFeeder;

        Procedure AllocateLoad(ActorID: integer);
        Procedure ReduceZone(ActorID: integer);  // Reduce Zone by eliminating buses and merging lines
        Procedure SaveZone(const dirname:String);

        Procedure CalcReliabilityIndices(AssumeRestoration:Boolean; ActorID : Integer);

        FUNCTION  GetPropertyValue(Index:Integer):String;Override;
        PROCEDURE InitPropertyValues(ArrayOffset:Integer);Override;
        PROCEDURE DumpProperties(Var F:TextFile; Complete:Boolean);Override;

   end;

VAR
   ActiveEnergyMeterObj  :TEnergyMeterObj;
  { RegisterNameList      :TCommandList; }


implementation
USES  ParserDel, DSSClassDefs, DSSGlobals, Bus,  MathUtil,  UCMatrix,
      Utilities, PCElement,  StackDef, Circuit, Line, LineUnits,
      ReduceAlgs, Math, MemoryMap_Lib, Sysutils,
      {$IFDEF MSWINDOWS}
      Windows,
      {$ENDIF}
      Classes;

//{$UNDEF DEBUG}

Const NumPropsThisClass = 24;

VAR

   Delta_Hrs : Double;
   // adjacency lists for PC and PD elements at each bus, built for faster searches
   BusAdjPC : TAdjArray; // also includes shunt PD elements
   BusAdjPD : TAdjArray;



//- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
FUNCTION  jiIndex(i, j:Integer):Integer; Inline;
Begin
    Result := (j-1)*3 + i;
End;

//- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
constructor TEnergyMeter.Create;  // Creates superstructure FOR all EnergyMeter objects
var
  idx : Integer;
Begin
     Inherited Create;
     Class_Name := 'EnergyMeter';
     DSSClassType := DSSClassType + ENERGY_METER;

     ActiveElement := 0;

     {Initialice demand interval options to off}
     FSaveDemandInterval := FALSE;
     FDI_Verbose         := FALSE;

     OverLoadFileIsOpen               :=  FALSE;
     VoltageFileIsOpen                :=  FALSE;



     Do_OverloadReport                :=  FALSE;
     Do_VoltageExceptionReport        :=  FALSE;

     DI_Dir := '';
     
     DefineProperties;

     CommandList := TCommandList.Create(Slice(PropertyName^, NumProperties));
     CommandList.Abbrev := TRUE;

//{$IFDEF MSWINDOWS}
     GeneratorClass := DSSClassList[ActiveActor].Get(ClassNames[ActiveActor].Find('generator'));
//{$ENDIF}

     SystemMeter := TSystemMeter.Create;

End;

//- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Destructor TEnergyMeter.Destroy;

Begin
    SystemMeter.Free;

    // ElementList and  CommandList freed in inherited destroy
    Inherited Destroy;
End;


//- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Procedure TEnergyMeter.DefineProperties;
Begin

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
     PropertyHelp[2] := 'Number of the terminal of the circuit element to which the monitor is connected. '+
                    '1 or 2, typically.';
     PropertyHelp[3] := '{Clear (reset) | Save | Take | Zonedump | Allocate | Reduce} ' + CRLF + CRLF +
                    '(A)llocate = Allocate loads on the meter zone to match PeakCurrent.' + CRLF +
                    '(C)lear = reset all registers to zero' + CRLF +
                    '(R)educe = reduces zone by merging lines (see Set Keeplist & ReduceOption)' + CRLF +
                    '(S)ave = saves the current register values to a file.' + CRLF +
                    '   File name is "MTR_metername.CSV".' +CRLF +
                    '(T)ake = Takes a sample at present solution' + CRLF +
                    '(Z)onedump = Dump names of elements in meter zone to a file' + CRLF +
                    '   File name is "Zone_metername.CSV".';
      PropertyHelp[4] := 'Enter a string ARRAY of any combination of the following. Options processed left-to-right:' + CRLF + CRLF +
                    '(E)xcess : (default) UE/EEN is estimate of energy over capacity ' + CRLF +
                    '(T)otal : UE/EEN is total energy after capacity exceeded'+ CRLF +
                    '(R)adial : (default) Treats zone as a radial circuit'+ CRLF +
                    '(M)esh : Treats zone as meshed network (not radial).' +CRLF+
                    '(C)ombined : (default) Load UE/EEN computed from combination of overload and undervoltage.'+ CRLF +
                    '(V)oltage : Load UE/EEN computed based on voltage only.'+CRLF+CRLF+
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
                         'If specified, DSS uses this list instead.  Can access the names in a single-column text file.  Examples: ' + crlf + crlf+
                         'zonelist=[line.L1, transformer.T1, Line.L3] ' + CRLF +
                         'zonelist=(file=branchlist.txt)';
      PropertyHelp[9] := '{Yes | No}  Default is NO.  If Yes, meter considers only the monitored element ' +
                         'for EEN and UE calcs.  Uses whole zone for losses.';
      PropertyHelp[10]:= 'Mask for adding registers whenever all meters are totalized.  Array of floating point numbers ' +
                         'representing the multiplier to be used for summing each register from this meter. ' +
                         'Default = (1, 1, 1, 1, ... ).  You only have to enter as many as are changed (positional). ' +
                         'Useful when two meters monitor same energy, etc.';
      PropertyHelp[11]:= '{Yes | No}  Default is YES. Compute Zone losses. If NO, then no losses at all are computed.';
      PropertyHelp[12]:= '{Yes | No}  Default is YES. Compute Line losses. If NO, then none of the losses are computed.';
      PropertyHelp[13]:= '{Yes | No}  Default is YES. Compute Transformer losses. If NO, transformers are ignored in loss calculations.';
      PropertyHelp[14]:= '{Yes | No}  Default is YES. Compute Sequence losses in lines and segregate by line mode losses and zero mode losses.';
      PropertyHelp[15]:= '{Yes | No}  Default is YES. Compute Line losses and segregate by 3-phase and other (1- and 2-phase) line losses. ';
      PropertyHelp[16]:= '{Yes | No}  Default is YES. Compute losses and segregate by voltage base. If NO, then voltage-based tabulation is not reported.';
      PropertyHelp[17]:= '{Yes | No}  Default is NO.  Report min, max, and average phase voltages for the zone and tabulate by voltage base. ' +
                         'Demand Intervals must be turned on (Set Demand=true) and voltage bases must be defined for this property to take effect. '+
                         'Result is in a separate report file.';
      PropertyHelp[18]:= 'Average number of annual interruptions for head of the meter zone (source side of zone or feeder).';
      PropertyHelp[19]:= 'Average annual duration, in hr, of interruptions for head of the meter zone (source side of zone or feeder).';
      PropertyHelp[20]:= '(Read only) Makes SAIFI result available via return on query (? energymeter.myMeter.SAIFI.';
      PropertyHelp[21]:= '(Read only) Makes SAIFIkW result available via return on query (? energymeter.myMeter.SAIFIkW.';
      PropertyHelp[22]:= '(Read only) Makes SAIDI result available via return on query (? energymeter.myMeter.SAIDI.';
      PropertyHelp[23]:= '(Read only) Makes CAIDI result available via return on query (? energymeter.myMeter.CAIDI.';
      PropertyHelp[24]:= '(Read only) Makes Total Customer Interrupts value result available via return on query (? energymeter.myMeter.CustInterrupts.';
(**** Not used in present version
      PropertyHelp[11]:= '{Yes/True | No/False}  Default is NO. If set to Yes, a Feeder object is created corresponding to ' +
                         'the energymeter.  Feeder is enabled if Radial=Yes; diabled if Radial=No.  Feeder is ' +
                         'synched automatically with the meter zone.  Do not create feeders for zones in meshed transmission systems.';
*****)

     ActiveProperty := NumPropsThisClass;
     inherited DefineProperties;  // Add defs of inherited properties to bottom of list

End;

//- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Function TEnergyMeter.NewObject(const ObjName:String):Integer;
Begin
   // create a new object of this class and add to list
    WITH ActiveCircuit[ActiveActor] Do
    Begin
      ActiveCktElement := TEnergyMeterObj.Create(Self, ObjName);
      Result           := AddObjectToList(ActiveDSSObject[ActiveActor]);
    End;
End;

//- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Function TEnergyMeter.Edit(ActorID : Integer):Integer;

VAR
   ParamPointer :Integer;
   ParamName    :String;
   Param        :String;

   DoRecalc     :Boolean;

Begin

  // continue parsing WITH contents of Parser
  // continue parsing WITH contents of Parser
  ActiveEnergyMeterObj           := ElementList.Active;
  ActiveCircuit[ActorID].ActiveCktElement := ActiveEnergyMeterObj;

  Result   := 0;

  DoRecalc := FALSE;

  WITH ActiveEnergyMeterObj DO
  Begin

     MeteredElementChanged := FALSE;
     ParamPointer := 0;
     ParamName    := Parser[ActorID].NextParam;
     Param        := Parser[ActorID].StrValue;
     WHILE Length(Param)>0 DO
     Begin
         IF   (Length(ParamName) = 0)
         THEN Inc(ParamPointer)
         ELSE ParamPointer := CommandList.GetCommand(ParamName);

         IF (ParamPointer>0) and (ParamPointer<=NumProperties)
         THEN PropertyValue[ParamPointer] := Param;

         CASE ParamPointer OF
            0: DoSimpleMsg('Unknown parameter "' + ParamName + '" for Object "' + Class_Name +'.'+ Name + '"', 520);
            1: ElementName := lowercase(param);
            2: MeteredTerminal := Parser[ActorID].IntValue;
            3: Begin  {Actions}
                  param := lowercase(param);
                  CASE param[1] of
                    'a': AllocateLoad(ActorID);
                    'c': ResetRegisters;
                    'r': ReduceZone(ActorID);
                    's': SaveRegisters(ActorID);
                    't': TakeSample(ActorID);
                    'z': ZoneDump(ActorID);
                  End;
               End;
            4: ProcessOptions(Param);
            5: MaxZonekVA_Norm  := Parser[ActorID].DblValue;
            6: MaxZonekVA_Emerg := Parser[ActorID].DblValue;
            7: parser[ActorID].ParseAsVector(Fnphases, SensorCurrent);   // Inits to zero
            8: InterpretAndAllocStrArray(Param, DefinedZoneListSize, DefinedZoneList);
            9: LocalOnly := InterpretYesNo(Param);
           10: InterpretRegisterMaskArray(TotalsMask, ActorID);
           11: FLosses        := InterpretYesNo(Param);
           12: FLineLosses    := InterpretYesNo(Param);
           13: FXfmrLosses    := InterpretYesNo(Param);
           14: FSeqLosses     := InterpretYesNo(Param);
           15: F3PhaseLosses  := InterpretYesNo(Param);
           16: FVBaseLosses   := InterpretYesNo(Param);
           17: FPhaseVoltageReport  := InterpretYesNo(Param);
           18: Source_NumInterruptions  := Parser[ActorID].dblvalue; // Annual interruptions for upline circuit
           19: Source_IntDuration       := Parser[ActorID].dblValue; // hours
           20: PropertyValue[20] := '';  // placeholder, do nothing just throw value away if someone tries to set it.
           21: PropertyValue[21] := '';  // placeholder, do nothing just throw value away if someone tries to set it.
           22: PropertyValue[22] := '';  // placeholder, do nothing just throw value away if someone tries to set it.
           23: PropertyValue[23] := '';  // placeholder, do nothing just throw value away if someone tries to set it.
           24: PropertyValue[24] := '';  // placeholder, do nothing just throw value away if someone tries to set it.
           (****11: HasFeeder := InterpretYesNo(Param); ***)
         ELSE
           ClassEdit(ActiveEnergyMeterObj, ParamPointer - NumPropsthisClass)
         End;

         CASE ParamPointer OF
             1,2: Begin
                     MeteredElementChanged := TRUE;
                     DoRecalc := TRUE;
                  End;
             (****11: If HasFeeder Then DoRecalc := True Else RemoveFeederObj; *)
         END;

         ParamName := Parser[ActorID].NextParam;
         Param := Parser[ActorID].StrValue;
     End;

     If DoRecalc Then RecalcElementData(ActorID);   // When some basic data have changed
  End;
End;

//- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Function TEnergyMeter.MakeLike(Const EnergyMeterName:String):Integer;
VAR
   OtherEnergyMeter:TEnergyMeterObj;
   i:Integer;
Begin
   Result := 0;
   {See IF we can find this EnergyMeter name in the present collection}
   OtherEnergyMeter := Find(EnergyMeterName);
   IF OtherEnergyMeter<>NIL Then
   WITH ActiveEnergyMeterObj DO
   Begin

       NPhases       := OtherEnergyMeter.Fnphases;
       NConds  := OtherEnergyMeter.Fnconds; // Force Reallocation of terminal stuff

       ElementName     := OtherEnergyMeter.ElementName;
       MeteredElement  := OtherEnergyMeter.MeteredElement;  // Pointer to target circuit element
       MeteredTerminal := OtherEnergyMeter.MeteredTerminal;
       ExcessFlag      := OtherEnergyMeter.ExcessFlag;

       MaxZonekVA_Norm  := OtherEnergyMeter.MaxZonekVA_Norm;
       MaxZonekVA_Emerg := OtherEnergyMeter.MaxZonekVA_emerg;

       // Reliability
       Source_NumInterruptions := OtherEnergyMeter.Source_NumInterruptions;
       Source_IntDuration := OtherEnergyMeter.Source_IntDuration;

       FreeStringArray(DefinedZoneList, DefinedZoneListSize);
       DefinedZoneListSize    := OtherEnergyMeter.DefinedZoneListSize;
       DefinedZoneList        := AllocStringArray(DefinedZoneListSize);
       // Copy Strings over (actually incr ref count on string)
       For i := 1 to DefinedZoneListSize Do  DefinedZoneList^[i] := OtherEnergyMeter.DefinedZoneList^[i];

       LocalOnly       := OtherEnergyMeter.LocalOnly;
       VoltageUEOnly   := OtherEnergyMeter.VoltageUEOnly;

       {Boolean Flags}
       FLosses        := OtherEnergyMeter.FLosses;
       FLineLosses    := OtherEnergyMeter.FLineLosses;
       FXfmrLosses    := OtherEnergyMeter.FXfmrLosses;
       FSeqLosses     := OtherEnergyMeter.FSeqLosses;
       F3PhaseLosses  := OtherEnergyMeter.F3PhaseLosses;
       FVBaseLosses   := OtherEnergyMeter.FVBaseLosses;
       FPhaseVoltageReport  := OtherEnergyMeter.FPhaseVoltageReport;

       FOR i := 1 to ParentClass.NumProperties Do
         // Skip Read Only properties
         If i<20 Then PropertyValue[i] := OtherEnergyMeter.PropertyValue[i];

   End
   ELSE  DoSimpleMsg('Error in EnergyMeter MakeLike: "' + EnergyMeterName + '" Not Found.', 521);

End;

//- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Function TEnergyMeter.Init(Handle:Integer; ActorID : Integer):Integer;

Begin
   DoSimpleMsg('Need to implement TEnergyMeter.Init', -1);
   Result := 0;
End;

{--------------------------------------------------------------------------}
Procedure TEnergyMeter.ResetMeterZonesAll(ActorID : Integer);  // Force all EnergyMeters in the circuit to reset their meter zones

VAR
  mtr :TEnergyMeterObj;
  pCktElement  :TDSSCktElement;
  PDElem       :TPDElement;
  PCElem       :TPCElement;
  i        :Integer;

Begin
  WITH ActiveCircuit[ActorID] Do Begin
    If Energymeters.ListSize=0 Then Exit;  // Do not do anything

    // initialize the Checked Flag FOR all circuit Elements
    pCktElement := CktElements.First;
    WHILE (pCktElement <> NIL) Do Begin
      With pCktElement Do Begin
        Checked := False;
        IsIsolated := TRUE;
        For i := 1 to NTerms Do Terminals^[i].Checked := FALSE;
      End;
      pCktElement := CktElements.Next;
    End;

    {Clear some things that will be set by the Meter Zone}
    PDElem := PDElements.First;
    while PDElem <> Nil do Begin
      PDElem.MeterObj := Nil;
      PDElem.SensorObj := Nil;
      PDElem.ParentPDElement := Nil;
      PDElem := PDElements.Next;
    End;

    PCElem := PCElements.First;
    while PCElem <> Nil do Begin
      PCElem.MeterObj := Nil;
      PCElem.SensorObj := Nil;
      PCElem := PCElements.Next;
    End;

    // Set up the bus adjacency lists for faster searches to build meter zone lists.
    BuildActiveBusAdjacencyLists (BusAdjPD, BusAdjPC,ActorID);

    {Set Hasmeter flag for all cktelements}
    SetHasMeterFlag(ActorID);
    SensorClass[ActorID].SetHasSensorFlag;  // Set all Sensor branch flags, too.

    // initialize the Checked Flag for all Buses
    FOR i := 1 to NumBuses Do Buses^[i].BusChecked := False;

    FOR i := 1 TO EnergyMeters.ListSize DO Begin
      mtr :=  EnergyMeters.Get(i);
      IF Mtr.Enabled Then mtr.MakeMeterZoneLists(ActorID);
    END;

    FreeAndNilBusAdjacencyLists (BusAdjPD, BusAdjPC);
  End;
End;

{--------------------------------------------------------------------------}
Procedure TEnergyMeter.ResetAll(ActorID : Integer);  // Force all EnergyMeters in the circuit to reset

VAR
   mtr:TEnergyMeterObj;
   CasePath:String;

Begin

      If DIFilesAreOpen[ActorID] Then CloseAllDIFiles(ActorID);

      If FSaveDemandInterval Then   Begin

          CasePath := OutputDirectory[ActorID] + ActiveCircuit[ActorID].CaseName;
          {Make directories to save data}

            If not DirectoryExists(CasePath) Then Begin
              Try
                 mkDir(CasePath);
              Except
                 On E:Exception Do DoSimpleMsg('Error making  Directory: "'+CasePath+'". ' + E.Message, 522);
              End;
            End;
            DI_Dir  := CasePath+'\DI_yr_' + Trim( IntToStr(ActiveCircuit[ActorID].Solution.Year));
            If not DirectoryExists(DI_Dir) Then Begin
              Try
                 mkDir(DI_Dir);
              Except
                 On E:Exception Do DoSimpleMsg('Error making Demand Interval Directory: "'+DI_Dir+'". ' + E.Message, 523);
              End;
            End;

            CreateFDI_Totals(ActorID);
      End;

      mtr := ActiveCircuit[ActorID].EnergyMeters.First;
      WHILE mtr<>NIL DO
      Begin
          mtr.ResetRegisters;
          mtr := ActiveCircuit[ActorID].EnergyMeters.Next;
      End;

      SystemMeter.Reset;


      // Reset Generator Objects, too
      GeneratorClass.ResetRegistersAll(ActorID);
      StorageClass[ActorID].ResetRegistersAll;
      PVSystemClass[ActorID].ResetRegistersAll;


End;

{--------------------------------------------------------------------------}
Procedure TEnergyMeter.SampleAll(ActorID : Integer);  // Force all EnergyMeters in the circuit to take a sample

VAR
   mtr:TEnergyMeterObj;
   i:Integer;

Begin

      mtr := ActiveCircuit[ActorID].EnergyMeters.First;
      WHILE mtr<>NIL DO Begin
          IF mtr.enabled Then mtr.TakeSample(ActorID);
          mtr := ActiveCircuit[ActorID].EnergyMeters.Next;
      End;

      SystemMeter.TakeSample(ActorID);

      If FSaveDemandInterval Then Begin  {Write Totals Demand interval file}
        With ActiveCircuit[ActorID].Solution  Do WriteintoMem(TDI_MHandle[ActorID],DynaVars.dblHour);
        For i := 1 to NumEMRegisters Do WriteintoMem(TDI_MHandle[ActorID],DI_RegisterTotals[i]);
        WriteintoMemStr(TDI_MHandle[ActorID], Char(10));
        ClearDI_Totals;
        if OverLoadFileIsOpen then WriteOverloadReport(ActorID);
        If VoltageFileIsOpen  then WriteVoltageReport(ActorID);
      End;

      // Sample Generator ans Storage Objects, too
      GeneratorClass.SampleAll(ActorID);
      StorageClass[ActorID].SampleAll(ActorID);  // samples energymeter part of storage elements (not update)
      PVSystemClass[ActorID].SampleAll(ActorID);

End;

{--------------------------------------------------------------------------}
Procedure TEnergyMeter.SaveAll(ActorID : Integer);  // Force all EnergyMeters in the circuit to take a sample

VAR
   mtr:TEnergyMeterObj;

Begin
    mtr := ActiveCircuit[ActorID].EnergyMeters.First;
    WHILE mtr<>NIL DO
    Begin
        IF mtr.enabled Then mtr.SaveRegisters(ActorID);
        mtr := ActiveCircuit[ActorID].EnergyMeters.Next;
    End;

    SystemMeter.Save(ActorID);

End;


//- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
//      TEnergyMeter Obj
//- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

constructor TEnergyMeterObj.Create(ParClass:TDSSClass; const EnergyMeterName:String);

VAR
   i :Integer;

Begin
     Inherited Create(ParClass);
     Name       := LowerCase(EnergyMeterName);
     DSSObjType := ParClass.DSSClassType; //ENERGY_METER;

     NPhases        := 3;  // Directly set conds and phases
     Fnconds        := 3;
     Nterms         := 1;  // this forces allocation of terminals and conductors in base class
     ExcessFlag     := True;  // Default to Excess energy FOR UE
     ElementName    := 'Vsource.'+TDSSCktElement(ActiveCircuit[ActiveActor].CktElements.Get(1)).Name; // Default to first circuit element (source)
     MeteredElement := NIL;
     BranchList     := NIL;  // initialize to NIL, set later when inited
     SequenceList   := Nil;
     LoadList       := Nil;

     This_Meter_DIFileIsOpen := FALSE;
     VPhaseReportFileIsOpen  := FALSE;

     InitPropertyValues(0);

     // Max zone kW limits ignored unless the user provides a rating
     MaxZonekVA_Norm     := 0.0;
     MaxZonekVA_Emerg    := 0.0;

     // Zone reliability variables
     SAIFI   := 0.0;     // For this Zone
     SAIFIkW := 0.0;
     SAIDI   := 0.0;
     CAIDI   := 0.0;
     CustInterrupts := 0.0;
     Source_NumInterruptions  := 0.0; // Annual interruptions for upline circuit
     Source_IntDuration       := 0.0; // Aver interruption duration of upline circuit


     ZoneIsRadial        := True;
     HasFeeder           := FALSE; // Not used; leave as False
     FeederObj           := Nil;  // initialize to not assigned
     DefinedZoneList     := NIL;
     DefinedZoneListSize := 0;

     FLosses             := TRUE;   {Loss Reporting switches}
     FLineLosses         := TRUE;
     FXfmrLosses         := TRUE;
     FSeqLosses          := TRUE;
     F3PhaseLosses       := TRUE;
     FVBaseLosses        := TRUE;
     FPhaseVoltageReport := FALSE;
     VbaseList           := NIL;
     VBaseTotalLosses    := NIL;
     VBaseLineLosses     := NIL;
     VBaseLoadLosses     := NIL;
     VBaseNoLoadLosses   := NIL;
     VBaseLoad           := NIL;
     VBaseCount          := 0;
     MaxVBaseCount       := (NumEMRegisters - Reg_VBaseStart) div 5;
     ReallocMem(VBaseList, MaxVBaseCount * SizeOf(VBaseList^[1]));
     ReallocMem(VBaseTotalLosses, MaxVBaseCount * SizeOf(VBaseTotalLosses^[1]));
     ReallocMem(VBaseLineLosses, MaxVBaseCount * SizeOf(VBaseLineLosses^[1]));
     ReallocMem(VBaseLoadLosses, MaxVBaseCount * SizeOf(VBaseLoadLosses^[1]));
     ReallocMem(VBaseNoLoadLosses, MaxVBaseCount * SizeOf(VBaseNoLoadLosses^[1]));
     ReallocMem(VBaseLoad, MaxVBaseCount * SizeOf(VBaseLoad^[1]));

     // Arrays for phase voltage report
     ReallocMem(VphaseMax, MaxVBaseCount * 3 * SizeOf(double));
     ReallocMem(VPhaseMin, MaxVBaseCount * 3 * SizeOf(double));
     ReallocMem(VPhaseAccum, MaxVBaseCount * 3 * SizeOf(double));
     ReallocMem(VPhaseAccumCount, MaxVBaseCount * 3 * SizeOf(Integer));

     LocalOnly           := FALSE;
     VoltageUEOnly       := FALSE;

//*************No append files by default***************************************
     OV_Append[ActiveActor]              :=  False;
     VR_Append[ActiveActor]              :=  False;
     DI_Append[ActiveActor]              :=  False;
     SDI_Append[ActiveActor]             :=  False;
     TDI_Append[ActiveActor]             :=  False;
     SM_Append[ActiveActor]              :=  False;
     EMT_Append[ActiveActor]             :=  False;
     PHV_Append[ActiveActor]             :=  False;
     FM_Append[ActiveActor]              :=  False;

     // Set Register names  that correspond to the register quantities
     RegisterNames[1]  := 'kWh';
     RegisterNames[2]  := 'kvarh';
     RegisterNames[3]  := 'Max kW';
     RegisterNames[4]  := 'Max kVA';
     RegisterNames[5]  := 'Zone kWh';
     RegisterNames[6]  := 'Zone kvarh';
     RegisterNames[7]  := 'Zone Max kW';
     RegisterNames[8]  := 'Zone Max kVA';
     RegisterNames[9]  := 'Overload kWh Normal';
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
     for i:= Reg_VBaseStart + 1 to NumEMRegisters do RegisterNames[i] := '';

     ResetRegisters;
     For i := 1 to NumEMRegisters Do TotalsMask[i] := 1.0;

     AllocateSensorArrays;

     FOR i := 1 to Fnphases Do SensorCurrent^[i] := 400.0;

     FeederSections := Nil;
     ActiveSection  := 0;

     OV_MHandle[ActiveActor]      :=  nil;
     VR_MHandle[ActiveActor]      :=  nil;
     DI_MHandle[ActiveActor]      :=  nil;
     SDI_MHandle[ActiveActor]     :=  nil;
     TDI_MHandle[ActiveActor]     :=  nil;
     SM_MHandle[ActiveActor]      :=  nil;
     EMT_MHandle[ActiveActor]     :=  nil;
     PHV_MHandle[ActiveActor]     :=  nil;
     FM_MHandle[ActiveActor]      :=  nil;

    // RecalcElementData;
End;

//- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
destructor TEnergyMeterObj.Destroy;
var
   i :Integer;
Begin
    If Assigned (VBaseList)        then Reallocmem(VBaseList, 0);
    If Assigned (VBaseTotalLosses) then Reallocmem(VBaseTotalLosses, 0);
    If Assigned (VBaseLineLosses)  then Reallocmem(VBaseLineLosses, 0);
    If Assigned (VBaseLoadLosses)  then Reallocmem(VBaseLoadLosses, 0);
    If Assigned (VBaseNoLoadLosses)then Reallocmem(VBaseNoLoadLosses, 0);
    If Assigned (VBaseLoad)        then Reallocmem(VBaseLoad, 0);
     // Arrays for phase voltage report
    If Assigned (VphaseMax)   then  ReallocMem(VphaseMax, 0);
    If Assigned (VPhaseMin)   then  ReallocMem(VPhaseMin, 0);
    If Assigned (VPhaseAccum) then  ReallocMem(VPhaseAccum, 0);
    If Assigned (VPhaseAccumCount) then  ReallocMem(VPhaseAccumCount, 0);

    for i := 1 to NumEMRegisters do RegisterNames[i] := '';
    If Assigned(BranchList)   Then BranchList.Free;
    If Assigned(SequenceList) Then SequenceList.Free;
    If Assigned(LoadList)     Then LoadList.Free;
    FreeStringArray(DefinedZoneList, DefinedZoneListSize);

    If Assigned (FeederSections) Then Reallocmem(FeederSections, 0);

    if OV_MHandle[ActiveActor] <> nil then OV_MHandle[ActiveActor].Free;
    if VR_MHandle[ActiveActor] <> nil then VR_MHandle[ActiveActor].Free;
    if DI_MHandle[ActiveActor] <> nil then DI_MHandle[ActiveActor].Free;
    if SDI_MHandle[ActiveActor] <> nil then SDI_MHandle[ActiveActor].Free;
    if TDI_MHandle[ActiveActor] <> nil then TDI_MHandle[ActiveActor].Free;
    if SM_MHandle[ActiveActor] <> nil then SM_MHandle[ActiveActor].Free;
    if EMT_MHandle[ActiveActor] <> nil then EMT_MHandle[ActiveActor].Free;
    if PHV_MHandle[ActiveActor] <> nil then PHV_MHandle[ActiveActor].Free;
    if FM_MHandle[ActiveActor] <> nil then FM_MHandle[ActiveActor].Free;

    Inherited destroy;
End;

//- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Procedure TEnergyMeterObj.RecalcElementData(ActorID : Integer);

VAR
   DevIndex :Integer;

Begin
     Devindex := GetCktElementIndex(ElementName);   // Global function
     IF DevIndex>0 Then Begin  // Monitored element must already exist
         MeteredElement := ActiveCircuit[ActorID].CktElements.Get(DevIndex); // Get pointer to metered element
         {MeteredElement must be a PDElement}
         If NOT (MeteredElement is TPDElement) Then Begin
            MeteredElement := NIL;   // element not found
            DoErrorMsg('EnergyMeter: "' + Self.Name + '"', 'Circuit Element "'+ ElementName + '" is not a Power Delivery (PD) element.',
                        ' Element must be a PD element.', 525);
            Exit;
         End;


         IF MeteredTerminal>MeteredElement.Nterms  Then Begin
             DoErrorMsg('EnergyMeter: "' + Name + '"',
                             'Terminal no. "' + IntToStr(MeteredTerminal)+'" does not exist.',
                             'Respecify terminal no.', 524);
         END
         ELSE Begin

             If MeteredElementChanged Then Begin
               // Sets name of i-th terminal's connected bus in monitor's buslist
               // This value will be used to set the NodeRef array (see TakeSample)
                 Setbus(1, MeteredElement.GetBus(MeteredTerminal));
                 Nphases := MeteredElement.NPhases;
                 Nconds  := MeteredElement.Nconds;
                 AllocateSensorArrays;

                 // If we come through here, throw branchlist away
                 IF BranchList <> NIL Then BranchList.Free;
                 BranchList := Nil;
             End;

             (****If HasFeeder Then MakeFeederObj;  // OK to call multiple times  *)

         END;
     END
     ELSE Begin
        MeteredElement := NIL;   // element not found
        DoErrorMsg('EnergyMeter: "' + Self.Name + '"', 'Circuit Element "'+ ElementName + '" Not Found.',
                        ' Element must be defined previously.', 525);
     END;
End;

procedure TEnergyMeterobj.MakePosSequence(ActorID : Integer);
begin
  if MeteredElement <> Nil then begin
    Setbus(1, MeteredElement.GetBus(MeteredTerminal));
    Nphases := MeteredElement.NPhases;
    Nconds  := MeteredElement.Nconds;
    AllocateSensorArrays;
    IF BranchList <> NIL Then BranchList.Free;
    BranchList := Nil;
  end;
  (***If HasFeeder Then MakeFeederObj;*)
  Inherited;
end;

function TEnergyMeterObj.MakeVPhaseReportFileName(ActorID: integer): String;
begin
    Result := EnergyMeterClass[ActorID].DI_Dir + '\' + Name + '_PhaseVoltageReport_' + inttostr(ActorID) + '.CSV';
end;

//- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Procedure TEnergyMeterObj.ResetRegisters;

VAR
   i : Integer;

Begin
   FOR i := 1 to NumEMregisters Do Registers[i]   := 0.0;
   FOR i := 1 to NumEMregisters Do Derivatives[i] := 0.0;
   {Initialize DragHand registers to some big negative number}
   Registers[Reg_MaxkW]           := -1.0e50;
   Registers[Reg_MaxkVA]          := -1.0e50;
   Registers[Reg_ZoneMaxkW]       := -1.0e50;
   Registers[Reg_ZoneMaxkVA]      := -1.0e50;
   Registers[Reg_MaxLoadLosses]   := -1.0e50;
   Registers[Reg_MaxNoLoadLosses] := -1.0e50;
   Registers[Reg_LossesMaxkW]     := -1.0e50;
   Registers[Reg_LossesMaxkvar]   := -1.0e50;

   Registers[Reg_GenMaxkW]        := -1.0e50;
   Registers[Reg_GenMaxkVA]       := -1.0e50;

   FirstSampleAfterReset := True;  // initialize for trapezoidal integration
   // Removed .. open in solution loop See Solve Yearly If EnergyMeterClass.SaveDemandInterval Then OpenDemandIntervalFile;

End;


//- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Procedure TEnergyMeterObj.CalcYPrim(ActorID : Integer);

Begin

 // YPrim is all zeros.  Just leave as NIL so it is ignored.

End;

//- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Procedure TEnergyMeterObj.SaveRegisters(ActorID: integer);

VAR
  CSVName :String;
  F       :TextFile;
  i       :Integer;

Begin

  Try
       CSVName := 'MTR_' + Name + '.CSV';
       AssignFile(F, GetOutputDirectory + CSVName);
       Rewrite(F);
       GlobalResult := CSVName;
       SetLastResultFile(CSVName);

  Except
      On E: Exception DO  Begin
       DoSimpleMsg('Error opening Meter File "' + CRLF + CSVName + '": ' + E.Message, 526);
       Exit;
      End
  End;

 Try
//       Writeln(F,'**** NEW RECORD ****');
       Writeln(F, 'Year, ', ActiveCircuit[ActorID].Solution.Year:0,',');
       FOR i := 1 to NumEMregisters Do
         Writeln(F, '"', RegisterNames[i], '",', Registers[i]:0:0);
 Finally
       CloseFile(F);
 End;

END;

//- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Procedure TEnergyMeterObj.Integrate(Reg:Integer; const Deriv:Double; Const Interval:Double; ActorID: integer);

Begin
     IF ActiveCircuit[ActorID].TrapezoidalIntegration THEN Begin
        {Trapezoidal Rule Integration}
         If Not FirstSampleAfterReset Then Registers[Reg] := Registers[Reg] + 0.5 * Interval * (Deriv + Derivatives[Reg]);
     End
     ELSE  Begin {Plain Euler integration}
         Registers[Reg] := Registers[Reg] + Interval * Deriv;
     End;

{ Set the derivatives so that the proper value shows up in Demand Interval Files
  and prepare for next time step in Trapezoidal integration }
     Derivatives[Reg] := Deriv;



End;

//- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Procedure TEnergyMeterObj.TakeSample(ActorID : Integer);
// Update registers from metered zone
// Assumes one time period has taken place since last sample.

VAR
   i,j, idx :Integer;
   
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
   TotalLosses           :Complex;

   CktElem,
   ParenElem :TPDElement;
   PCelem    :TPCElement;
   pLoad     :TLoadobj;
   pGen      :TGeneratorObj;
   // doubles
   MaxExcesskWNorm ,
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
   load_kw         :Double;
   S_PosSeqLosses  :Complex;
   S_ZeroSeqLosses :Complex;
   S_NegSeqLosses  :Complex;

   puV  :Double;

Begin

// Compute energy in branch  to which meter is connected

     //----MeteredElement.ActiveTerminalIdx := MeteredTerminal;  // needed for Excess kVA calcs
     S_Local     := CmulReal(MeteredElement.Power[MeteredTerminal,ActorID], 0.001);
     S_Local_kVA := Cabs(S_Local);
     Delta_Hrs   := ActiveCircuit[ActorID].Solution.IntervalHrs;
     Integrate(Reg_kWh,   S_Local.re, Delta_Hrs, ActorID);   // Accumulate the power
     Integrate(Reg_kvarh, S_Local.im, Delta_Hrs, ActorID);
     SetDragHandRegister( Reg_MaxkW,  S_Local.re);   // 3-10-04 removed abs()
     SetDragHandRegister( Reg_MaxkVA, S_Local_kVA);

// Compute Maximum overload energy in all branches in zone
// and mark all load downline from an overloaded branch as unserved
// If localonly, check only metered element

     TotalLosses         := CZERO;     // Initialize loss accumulators
     TotalLoadLosses     := CZERO;
     TotalNoLoadLosses   := CZERO;
     TotalLineLosses     := CZERO;
     TotalLineModeLosses := CZERO;
     TotalZeroModeLosses := CZERO;
     Total3phaseLosses   := CZERO;
     Total1phaseLosses   := CZERO;
     TotalTransformerLosses   := CZERO;

     // Init all voltage base loss accumulators
     For i := 1 to MaxVBaseCount Do Begin
       VBaseTotalLosses^[i]  := 0.0;
       VBaseLineLosses^[i]   := 0.0;
       VBaseLoadLosses^[i]   := 0.0;
       VBaseNoLoadLosses^[i] := 0.0;
       VBaseLoad^[i]         := 0.0;
     end;

     // Phase Voltage arrays
     If FPhaseVoltageReport then
     For i := 1 to MaxVBaseCount Do
     If VBaseList^[i] > 0.0 then Begin
        For j := 1 to 3 Do  Begin
          VphaseMax^[jiIndex(j, i)]        := 0.0;
          VphaseMin^[jiIndex(j, i)]        := 9999.0;
          VphaseAccum^[jiIndex(j, i)]      := 0.0;
          VphaseAccumCount^[jiIndex(j, i)] := 0;   // Keep track of counts for average
        End;
     End;

     CktElem           := BranchList.First;
     MaxExcesskWNorm   := 0.0;
     MaxExcesskWEmerg  := 0.0;

     {--------------------------------------------------------------------------}
     {------------------------ Local Zone  Only --------------------------------}
     {--------------------------------------------------------------------------}
     IF LocalOnly THEN Begin
           CktElem :=  MeteredElement as TPDElement;
           MaxExcesskWNorm   := Abs(CktElem.ExcesskVANorm[MeteredTerminal,ActorID].re);
           MaxExcesskWEmerg  := Abs(CktElem.ExcesskVAEmerg[MeteredTerminal,ActorID].re);
     End ELSE
     {--------------------------------------------------------------------------}
     {--------Cyle Through Entire Zone Setting EEN/UE --------------------------}
     {--------------------------------------------------------------------------}
     WHILE CktElem <> NIL Do
     Begin       // loop thru all ckt elements on zone

         With CktElem Do Begin
           ActiveTerminalIdx := BranchList.Presentbranch.FromTerminal;
         // Invoking this property sets the Overload_UE flag in the PD Element
           EEN  := Abs(ExcesskVANorm[ActiveTerminalIdx,ActorID].re);
           UE   := Abs(ExcesskVAEmerg[ActiveTerminalIdx,ActorID].re);
         End;

         {For radial circuits just keep the maximum overload; for mesh, add 'em up}
         IF   (ZoneIsRadial)  THEN Begin
             IF UE  > MaxExcesskWEmerg Then MaxExcesskWEmerg := UE;
             IF EEN > MaxExcesskWNorm  Then MaxExcesskWNorm  := EEN;
         End
         ELSE Begin
              MaxExcesskWEmerg := MaxExcesskWEmerg + UE;
              MaxExcesskWNorm  := MaxExcesskWNorm  + EEN;
         End;

         // Even if this branch is not overloaded, if the parent element is overloaded
         // mark load on this branch as unserved also
         // Use the larger of the two factors
         ParenElem := BranchList.Parent;
         IF  (ParenElem <> NIL) Then Begin
              CktElem.OverLoad_EEN := Maxvalue([CktElem.Overload_EEN, ParenElem.Overload_EEN]);
              CktElem.OverLoad_UE  := Maxvalue([CktElem.OverLoad_UE,  ParenElem.OverLoad_UE]);
         End;

         // Mark loads (not generators) by the degree of overload if the meter's zone is to be considered radial
         // This overrides and supercedes the load's own determination of unserved based on voltage
         // If voltage only is to be used for Load UE/EEN, don't mark (set to 0.0 and load will calc UE based on voltage)
         PCElem := Branchlist.FirstObject;
         WHILE (PCElem <> NIL) Do  Begin
             IF (PCElem.DSSObjType and CLASSMASK) = LOAD_ELEMENT Then Begin
                 pLoad := PCElem as TLoadObj;
                 IF   (CktElem.Overload_EEN > 0.0) And (ZoneIsRadial) and Not (VoltageUEOnly)
                 Then pLoad.EEN_Factor := CktElem.Overload_EEN
                 Else pLoad.EEN_Factor := 0.0;

                 IF   (CktElem.Overload_UE  > 0.0) And (ZoneIsRadial) and Not (VoltageUEOnly)
                 Then pLoad.UE_Factor  := CktElem.Overload_UE
                 Else pLoad.UE_Factor  := 0.0;
             End;
             PCElem := BranchList.NextObject
         End;

     CktElem := BranchList.GoForward;
     End;


     // Get the Losses, and unserved bus energies
     TotalZonekw   := 0.0;
     TotalZonekvar := 0.0;
     TotalLoad_EEN := 0.0;
     TotalLoad_UE  := 0.0;
     TotalGenkw    := 0.0;
     TotalGenkvar  := 0.0;


     {--------------------------------------------------------------------------}
     {--------       Cycle Through Zone Accumulating Load and Losses    --------}
     {--------------------------------------------------------------------------}
     CktElem := BranchList.First;
     WHILE (CktElem <> NIL) Do  Begin
         PCElem := Branchlist.FirstObject;
         WHILE (PCElem <> NIL) Do  Begin
             CASE (PCElem.DSSObjType and CLASSMASK) OF
                LOAD_ELEMENT: If Not LocalOnly Then Begin   // Dont check for load EEN/UE if Local only
                               pLoad := PCElem as TLoadObj;
                               load_kw := Accumulate_Load(pLoad, TotalZonekW, TotalZonekvar, TotalLoad_EEN, TotalLoad_UE, ActorID);
                               if FVbaseLosses then with BranchList.PresentBranch do
                                 if VoltBaseIndex > 0 then
                                    VBaseLoad^[VoltBaseIndex] := VBaseLoad^[VoltBaseIndex]  + load_kw;
                              END;
                GEN_ELEMENT:  Begin
                               pGen := PCElem as TGeneratorObj;
                               Accumulate_Gen(pGen, TotalGenkW, TotalGenkvar,ActorID);
                              End;
             ELSE
                {Ignore other types of PC Elements}
             END;
          PCElem := BranchList.NextObject
         End;

         If Flosses then Begin  // Compute and Report Losses

           {Get losses from the present circuit element}
           CktElem.GetLosses(S_TotalLosses, S_LoadLosses, S_NoLoadLosses, ActorID);  // returns watts, vars
           {Convert to kW}
           CmulRealAccum(S_TotalLosses,  0.001);
           CmulRealAccum(S_LoadLosses,   0.001);
           CmulRealAccum(S_NoLoadLosses, 0.001);
           {Update accumulators}
           Caccum(TotalLosses,       S_TotalLosses); // Accumulate total losses in meter zone
           Caccum(TotalLoadLosses,   S_LoadLosses);  // Accumulate total load losses in meter zone
           Caccum(TotalNoLoadLosses, S_NoLoadLosses); // Accumulate total no load losses in meter zone

           {Line and Transformer Elements}
           If IsLineElement(Cktelem) and FLineLosses then Begin
               Caccum(TotalLineLosses,   S_TotalLosses); // Accumulate total losses in meter zone
               If FseqLosses then  Begin
                   CktElem.GetSeqLosses(S_PosSeqLosses, S_NegSeqLosses, S_ZeroSeqLosses, ActorID);
                   Caccum(S_PosSeqLosses, S_NegSeqLosses);  // add line modes together
                   CmulRealAccum(S_PosSeqLosses,  0.001); // convert to kW
                   CmulRealAccum(S_ZeroSeqLosses, 0.001);
                   Caccum(TotalLineModeLosses,  S_PosSeqLosses );
                   Caccum(TotalZeroModeLosses,  S_ZeroSeqLosses);
               End;
               {Separate Line losses into 3- and "1-phase" losses}
               If F3PhaseLosses then Begin
                   If Cktelem.NPhases = 3 then Caccum(Total3phaseLosses,  S_TotalLosses )
                                          Else Caccum(Total1phaseLosses,  S_TotalLosses );
               End;
           End
           Else If IsTransformerElement(Cktelem) and FXfmrLosses then Begin
               Caccum(TotalTransformerLosses,  S_TotalLosses); // Accumulate total losses in meter zone
           End;

           If FVbaseLosses Then
           With BranchList.PresentBranch do
             If VoltBaseIndex >0  then Begin
                VBaseTotalLosses^[VoltBaseIndex]    := VBaseTotalLosses^[VoltBaseIndex]  + S_TotalLosses.re;
                if IsLineElement(CktElem) then
                  VBaseLineLosses^[VoltBaseIndex]   := VBaseLineLosses^[VoltBaseIndex]   + S_TotalLosses.re
                else if IsTransformerElement(CktElem) then begin
                  VBaseLoadLosses^[VoltBaseIndex]   := VBaseLoadLosses^[VoltBaseIndex]   + S_LoadLosses.re;
                  VBaseNoLoadLosses^[VoltBaseIndex] := VBaseNoLoadLosses^[VoltBaseIndex] + S_NoLoadLosses.re
                end;
             End;

           // Compute min, max, and average pu voltages for 1st 3 phases  (nodes designated 1, 2, or 3)
           If FPhaseVoltageReport then
           With BranchList.PresentBranch do
             If VoltBaseIndex > 0  then With ActiveCircuit[ActorID] Do
             If Buses^[FromBusReference].kVBase > 0.0
             Then Begin
                For i := 1 to Buses^[FromBusReference].NumNodesThisBus
                Do Begin
                    j := Buses^[FromBusReference].GetNum(i);
                    If (j>0) and (j<4) then Begin
                      puV := Cabs(Solution.NodeV^[Buses^[FromBusReference].GetRef(i)])/Buses^[FromBusReference].kVBase;
                      idx := jiIndex(j, VoltBaseIndex);
                      If puV > VphaseMax^[idx] Then
                      Begin
                         VphaseMax^[jiIndex(j, VoltBaseIndex)] := puV;
                         // VmaxBus := FromBusReference;
                      End;

                      If puV < VphaseMin^[idx] Then
                      Begin
                         VphaseMin^[jiIndex(j, VoltBaseIndex)] := puV;
                         // VminBus := FromBusReference;
                      End;

                      DblInc(VphaseAccum^[jiIndex(j, VoltBaseIndex)],  puV);
                      Inc(VphaseAccumCount^[jiIndex(j, VoltBaseIndex)]);   // Keep track of counts for average
                    End;
                End;
             End;
         End;  {If FLosses}

     CktElem := BranchList.GoForward;
     End;

     {NOTE: Integrate proc automatically sets derivatives array}
     Integrate(Reg_LoadEEN, TotalLoad_EEN, Delta_Hrs, ActorID);
     Integrate(Reg_LoadUE , TotalLoad_UE,  Delta_Hrs, ActorID);

     {Accumulate losses in appropriate registers}
     Integrate(Reg_ZoneLosseskWh,     TotalLosses.re,          Delta_Hrs, ActorID);
     Integrate(Reg_ZoneLosseskvarh,   TotalLosses.im,          Delta_Hrs, ActorID);
     Integrate(Reg_LoadLosseskWh,     TotalLoadLosses.re,      Delta_Hrs, ActorID);
     Integrate(Reg_LoadLosseskvarh,   TotalLoadLosses.im,      Delta_Hrs, ActorID);
     Integrate(Reg_NoLoadLosseskWh,   TotalNoLoadLosses.re,    Delta_Hrs, ActorID);
     Integrate(Reg_NoLoadLosseskvarh, TotalNoLoadLosses.im,    Delta_Hrs, ActorID);
     Integrate(Reg_LineLosseskWh,     TotalLineLosses.re,      Delta_Hrs, ActorID);
     Integrate(Reg_LineModeLineLoss,  TotalLineModeLosses.re,  Delta_Hrs, ActorID);
     Integrate(Reg_ZeroModeLineLoss,  TotalZeroModeLosses.re,  Delta_Hrs, ActorID);
     Integrate(Reg_3_phaseLineLoss,   Total3phaseLosses.re,    Delta_Hrs, ActorID);
     Integrate(Reg_1_phaseLineLoss,   Total1phaseLosses.re,    Delta_Hrs, ActorID);
     Integrate(Reg_TransformerLosseskWh,  TotalTransformerLosses.re,  Delta_Hrs, ActorID);
     for i  := 1 to MaxVBaseCount do begin
        Integrate(Reg_VbaseStart + i, VBaseTotalLosses^[i],  Delta_Hrs, ActorID);
        Integrate(Reg_VbaseStart + 1 * MaxVBaseCount + i, VBaseLineLosses^[i],    Delta_Hrs, ActorID);
        Integrate(Reg_VbaseStart + 2 * MaxVBaseCount + i, VBaseLoadLosses^[i],    Delta_Hrs, ActorID);
        Integrate(Reg_VbaseStart + 3 * MaxVBaseCount + i, VBaseNoLoadLosses^[i],  Delta_Hrs, ActorID);
        Integrate(Reg_VbaseStart + 4 * MaxVBaseCount + i, VBaseLoad^[i],          Delta_Hrs, ActorID);
     end;


     {--------------------------------------------------------------------------}
     {---------------   Total Zone Load and Generation -------------------------}
     {--------------------------------------------------------------------------}

     Integrate(Reg_ZonekWh,   TotalZonekW,   Delta_Hrs, ActorID);
     Integrate(Reg_Zonekvarh, TotalZonekvar, Delta_Hrs, ActorID);
     Integrate(Reg_GenkWh,    TotalGenkW,    Delta_Hrs, ActorID);
     Integrate(Reg_Genkvarh,  TotalGenkvar,  Delta_Hrs, ActorID);
     GenkVA  := Sqrt(Sqr(TotalGenkvar)  + Sqr(TotalGenkW));
     LoadkVA := Sqrt(Sqr(TotalZonekvar) + Sqr(TotalZonekW));

     {--------------------------------------------------------------------------}
     {---------------   Set Drag Hand Registers  ------------------------------}
     {--------------------------------------------------------------------------}

     SetDragHandRegister(Reg_LossesMaxkW,    Abs(TotalLosses.Re));
     SetDragHandRegister(Reg_LossesMaxkvar,  Abs(TotalLosses.im));
     SetDragHandRegister(Reg_MaxLoadLosses,  Abs(TotalLoadLosses.Re));
     SetDragHandRegister(Reg_MaxNoLoadLosses,Abs(TotalNoLoadLosses.Re));
     SetDragHandRegister(Reg_ZoneMaxkW,      TotalZonekW ); // Removed abs()  3-10-04
     SetDragHandRegister(Reg_ZoneMaxkVA ,    LoadkVA  );
     {Max total generator registers}
     SetDragHandRegister(Reg_GenMaxkW,       TotalGenkW); // Removed abs()  3-10-04
     SetDragHandRegister(Reg_GenMaxkVA ,     GenkVA  );

     {--------------------------------------------------------------------------}
     {---------------------   Overload Energy  ---------------------------------}
     {--------------------------------------------------------------------------}
     {Overload energy for the entire zone}
     If LocalOnly Then ZonekW := S_Local.Re
                  Else ZonekW := TotalZonekW;

     {Either the max excess kW of any PD element or the excess over zone limits}

     {regs 9 and 10}
     {Fixed these formulas 2-7-07 per discussions with Daniel Brooks }
     If  (MaxZonekVA_Norm > 0.0) Then Begin
          IF (S_Local_KVA =0.0) Then S_Local_KVA := MaxZonekVA_Norm;
          Integrate(Reg_OverloadkWhNorm, Maxvalue([0.0, (ZonekW * (1.0 -  MaxZonekVA_Norm / S_Local_KVA))]) , Delta_Hrs, ActorID);
     End Else Begin
          Integrate(Reg_OverloadkWhNorm, MaxExcesskWNorm,  Delta_Hrs, ActorID);
     End;

     If  (MaxZonekVA_Emerg > 0.0)Then Begin
          IF (S_Local_KVA =0.0) Then S_Local_KVA := MaxZonekVA_Emerg;
          Integrate(Reg_OverloadkWhEmerg, Maxvalue([0.0, (ZonekW * (1.0 - MaxZonekVA_Emerg/ S_Local_KVA))]), Delta_Hrs, ActorID);
     End Else Begin
          Integrate(Reg_OverloadkWhEmerg, MaxExcesskWEmerg,  Delta_Hrs, ActorID);
     End;

    FirstSampleAfterReset := False;
    IF EnergyMeterClass[ActorID].SaveDemandInterval[ActorID] Then WriteDemandIntervalData(ActorID);
End;

{---------------------------------------------------------------------------------}

procedure TEnergyMeterObj.TotalUpDownstreamCustomers;
Var
  i      :integer;
  {, Accumulator}
 // PresentNode: TCktTreeNode;
  CktElem:TPDElement;

begin

    If Not Assigned(BranchList) Then
    Begin
        DoSimpleMsg('Meter Zone Lists need to be built. Do Solve or Makebuslist first!', 529);
        Exit;
    End;

    {Init totsls and checked flag}
    CktElem := SequenceList.First;
    While CktElem <> Nil do  Begin
        CktElem.Checked        := FALSE;
        CktElem.BranchTotalCustomers := 0;
        CktElem := SequenceList.Next;
    End;

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
     For i := SequenceList.ListSize  downto 1 do
     Begin
            CktElem := SequenceList.Get(i);
            If Not CktElem.Checked  Then    // Avoid double counting
            WITH CktElem Do
            Begin
                 Checked := TRUE;
                 Inc(BranchTotalCustomers, BranchNumCustomers);
                 If ParentPDElement <> Nil Then
                    Inc(ParentPDElement.BranchTotalCustomers, BranchTotalCustomers);
            End;
     End;  {For i}

end;

//- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Procedure TEnergyMeter.SetHasMeterFlag(ActorID: integer);
// Set the HasMeter Flag for all cktElement;
VAR
   i:Integer;
   ThisMeter:TEnergyMeterObj;
   CktElem:TDSSCktElement;

Begin
   {Initialize all to FALSE}
   With  ActiveCircuit[ActorID] Do Begin
     CktElem := PDElements.First;
     While CktElem <> Nil Do Begin
        CktElem.HasEnergyMeter := FALSE;
        CktElem := PDElements.Next;
     End;  {WHILE}
   End; {WITH}

   FOR i := 1 to ActiveCircuit[ActorID].EnergyMeters.ListSize DO Begin
       ThisMeter := ActiveCircuit[ActorID].EnergyMeters.Get(i);
       With ThisMeter Do If MeteredElement <> Nil Then MeteredElement.HasEnergyMeter := TRUE;
   End;   {FOR}
End;

//- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

//- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Procedure TEnergyMeterObj.MakeMeterZoneLists(ActorID:Integer);

// This gets fired off whenever the buslists are rebuilt
// Must be updated whenever there is a change in the circuit

Var

  TestBusNum,
  ZoneListCounter :Integer ;
  j, iTerm,
  iPC, iPD      :Integer;
  ActiveBranch  :TDSSCktElement;
  TestElement   :TPDElement;
  pPCelem       :TPCElement;
  pLoad         :TLoadObj;
  IsFeederEnd   :Boolean;
  adjLst        :TList;
  PCElementType :Cardinal;

Begin

  ZoneListCounter := 0;
  VBasecount      := 0; {Build the voltage base list over in case a base added or deleted}
  for j := 1 to MaxVBaseCount  do VBaseList^[j] := 0.0;

  // Make a new branch list
  IF BranchList <> NIL Then BranchList.Free;
  BranchList := TCktTree.Create;     {Instantiates ZoneEndsList, too}

  // Get Started
  If Assigned(MeteredElement) Then BranchList.New := MeteredElement
  Else Begin   // oops
    DoSimpleMsg('Metered Element for EnergyMeter '+Name+' not defined.', 527);
    Exit;
  End;

  {Initialize SensorObj property of the first branch to this TMeterElement Object.
   Before starting, all sensorObj definitions are cleared in PCElements and PDElements. The
   SensorObj property is passed down to the Load objects for LoadAllocation and State Estimation
  }
  If MeteredElement is TPDElement then
    With TPDElement(MeteredElement) Do Begin
       SensorObj := Self;
       MeterObj  := Self;
    End
  Else If MeteredElement is TPCElement then
    With TPCElement(MeteredElement) Do Begin
       SensorObj := Self;
       MeterObj  := Self;
    End;


  MeteredElement.Terminals^[MeteredTerminal].Checked := TRUE;
  With BranchList.PresentBranch Do Begin
    // This bus is the head of the feeder or zone; do not mark as radial bus
      FromBusReference := MeteredElement.Terminals^[MeteredTerminal].BusRef;
      ActiveCircuit[ActorID].Buses^[FromBusReference].DistFromMeter := 0.0;
      VoltBaseIndex    := AddToVoltBaseList(FromBusReference, ActorID);
      FromTerminal     := MeteredTerminal;
      If MeteredElement is TPDElement Then TPDElement(MeteredElement).FromTerminal := MeteredTerminal;
  End;

  // Check off this element so we don't use it  again
  With MeteredElement Do Begin
      Checked    := True;
      IsIsolated := FALSE;
  End;

  // Make SequenceList for use in reliability calcs or anything that
  // needs to run through the tree quickly in a radial sequence
  If Assigned(SequenceList) Then  SequenceList.Free;
  SequenceList := PointerList.TPointerList.Create(1024); //make it a big initial allocation
  If Assigned(LoadList) Then  LoadList.Free;
  LoadList := PointerList.TPointerList.Create(1024); //make it a big initial allocation

  // Now start looking for other branches
  // Finds any branch connected to the TestBranch and adds it to the list
  // Goes until end of circuit, another energy meter, an open terminal, or disabled device.
  ActiveBranch := MeteredElement;

  { ****************  MAIN LOOP *****************************}
  WHILE ActiveBranch <> NIL DO Begin

    Sequencelist.Add(ActiveBranch); // When done, this should be the correct order.

    With BranchList.PresentBranch Do Begin
        IsLoopedHere  := FALSE;
        IsParallel    := FALSE;
        IsDangling    := TRUE;  // Unless we find something connected to it
        VoltBaseIndex := AddToVoltBaseList(FromBusReference,ActorID);
    End;

    TPDElement(ActiveBranch).BranchNumCustomers := 0;   // Init counter

    FOR iTerm := 1 to ActiveBranch.Nterms Do Begin
      IF NOT ActiveBranch.Terminals^[iTerm].Checked Then WITH ActiveCircuit[ActorID] Do Begin
        // Now find all loads and generators connected to the bus on this end of branch
        // attach them as generic objects to cktTree node.
        TestBusNum := ActiveBranch.Terminals^[iTerm].BusRef;
        With BranchList.PresentBranch Do Begin
            ToBusReference := TestBusNum;   // Add this as a "to" bus reference
            If isLineElement(ActiveBranch)   // Convert to consistent units (km)
              then Buses^[TestBusNum].DistFromMeter := Buses^[FromBusReference].DistFromMeter
                    + TLineObj(ActiveBranch).Len * ConvertLineUnits(TLineObj(ActiveBranch).LengthUnits, UNITS_KM)
              else Buses^[TestBusNum].DistFromMeter := Buses^[FromBusReference].DistFromMeter;
        End;

        adjLst := BusAdjPC[TestBusNum];
        For iPC := 0 to adjLst.Count - 1 do Begin
            pPCelem := adjLst[iPC];
            //  IF pPCelem.Enabled Then Begin   only enabled elements in the search list
            if NOT pPCelem.Checked Then Begin; // skip ones we already checked
                BranchList.PresentBranch.IsDangling := FALSE;   // Something is connected here
                // Is this a load or a generator or a Capacitor or reactor??
                PCElementType := (pPCelem.DSSObjType and CLASSMASK);
                IF (PCElementType = LOAD_ELEMENT)
                OR (PCElementType = GEN_ELEMENT)
                OR (PCElementType = CAP_ELEMENT)
                OR (PCElementType = REACTOR_ELEMENT) Then Begin
                      BranchList.NewObject      := pPCelem;
                      pPCelem.Checked           := TRUE;  // So we don't pick this element up again
                      pPCelem.IsIsolated        := FALSE;
                      pPCelem.ActiveTerminalIdx := 1;
                      {Totalize Number of Customers if Load Type}
                      If (pPCelem is TLoadObj) then Begin
                          pLoad := pPCelem As TLoadObj;
                          Inc(TPDElement(ActiveBranch).BranchNumCustomers, pLoad.NumCustomers);
                          LoadList.Add(pPCElem);  // Add to list of loads in this zone.)
                      End;
                      {If object does not have a sensor attached, it acquires the sensor of its parent branch}
                      If Not pPCelem.HasSensorObj then pPCelem.SensorObj := TPDElement(ActiveBranch).SensorObj;
                      pPCelem.MeterObj := Self;
                End; {IF}
            End;
        End;

        // Now find all branches connected to this bus that we haven't found already
        // Do not include in this zone if branch has open terminals or has another meter

        IF DefinedZoneListSize = 0 THEN Begin  // Search tree for connected branches (default)
          IsFeederEnd := TRUE;
          adjLst := BusAdjPD[TestBusNum];
          for iPD := 0 to adjLst.Count - 1 do begin
            TestElement := adjLst[iPD];  // Only enabled objects are in this list
            // **** See ResetMeterZonesAll
            IF Not (TestElement = ActiveBranch) Then  // Skip self
              IF Not TestElement.HasEnergyMeter THEN Begin  // Stop at other meters  so zones don't interfere
                FOR j := 1 to TestElement.Nterms Do Begin     // Check each terminal
                  IF TestBusNum = TestElement.Terminals^[j].BusRef THEN Begin
                    BranchList.PresentBranch.IsDangling := FALSE; // We found something it was connected to
                    {Check for loops and parallel branches and mark them}
                      IF (TestElement.Checked) Then     {This branch is on some meter's list already }
                          With BranchList.PresentBranch Do Begin
                              IsLoopedHere := TRUE; {It's a loop}
                              LoopLineObj  := TestElement;
                              If IsLineElement(ActiveBranch) and IsLineElement(TestElement) Then
                                If CheckParallel(ActiveBranch, TestElement) Then IsParallel := TRUE; {It's paralleled with another line}
                          End
                          Else Begin  // push TestElement onto stack and set properties
                              IsFeederEnd := FALSE;  // for interpolation
                              BranchList.AddNewChild( TestElement, TestBusNum, j);  // Add new child to the branchlist
                              With TestElement Do Begin
                                  Terminals^[j].Checked := TRUE;
                                  FromTerminal := j;
                                  Checked      := TRUE;
                                  IsIsolated   := FALSE;
                                  {Branch inherits sensor of upline branch if it doesn't have its own}
                                  If Not HasSensorObj  then SensorObj :=  TPDElement(ActiveBranch).SensorObj;
                                  MeterObj := Self;   // Set meterobj to this meter
                                  ParentPDElement := TPDElement(ActiveBranch);  // record the parent so we can easily back up for reconductoring, etc.
                              End;
                              Break;
                          End; {Else}
                  END; {IF TestBusNum}
                END;  {FOR terminals}
              END; {ELSE}
          End; {FOR iPD}

          If IsFeederEnd then BranchList.ZoneEndsList.Add (BranchList.PresentBranch, TestBusNum);
             {This is an end of the feeder and testbusnum is the end bus}
        END
        ELSE Begin   // Zone is manually specified; Just add next element in list as a child
            Inc(ZoneListCounter);
            WHILE ZoneListCounter <= DefinedZoneListSize Do Begin
                IF SetElementActive(DefinedZoneList^[ZoneListCounter]) = 0 THEN
                      Inc(ZoneListCounter) // Not Found. Let's search for another
                ELSE Begin
                    TestElement := ActiveCktElement as TPDElement;
                    IF Not TestElement.Enabled THEN
                        Inc(ZoneListCounter)  // Lets ignore disabled devices
                    ELSE Begin
                        IF (TestElement.DSSObjType and BaseClassMask) <> PD_ELEMENT THEN
                            Inc(ZoneListCounter)  // Lets ignore non-PD elements
                        ELSE
                            BranchList.AddNewChild(TestElement, 0, 0); // add it as a child to the previous element
                        Break;                                         // Can't do reductions if manually spec'd
                    END;
                END;
            END; // while
        END;
      End;  {WITH Active Circuit}
    End;   {FOR iTerm}

    ActiveBranch := BranchList.GoForward;   // Sets PresentBranch
  { ****************  END MAIN LOOP *****************************}
  End;

  TotalupDownstreamCustomers;

  (****If HasFeeder Then FeederObj.InitializeFeeder(BranchList);   // Synchronize the feeder definition *)

  AssignVoltBaseRegisterNames;
End;

{--------------------------------------------------------------------------}
Procedure TEnergyMeterObj.GetCurrents(Curr: pComplexArray; ActorID : Integer);  //Get present value of terminal Curr FOR reports

Var i:Integer;

Begin
     FOR i := 1 to Fnconds DO Curr^[i] := CZERO;
END;

{--------------------------------------------------------------------------}
Procedure TEnergyMeterObj.GetInjCurrents(Curr: pComplexArray; ActorID : Integer);

Var i:Integer;

Begin
     FOR i := 1 to Fnconds DO Curr^[i] := CZERO;
END;

{--------------------------------------------------------------------------}

Procedure TEnergyMeterObj.ZoneDump(ActorID:Integer);

Var
    CSVName :String;
    F       :TextFile;
    pdelem  :TPDelement;
    LoadElem:TDSSCktElement;

Begin

     TRY

       CSVName := 'Zone_' + Name + '.CSV';
       AssignFile(F, GetOutputDirectory + CSVName);
       Rewrite(F);

       GlobalResult := CSVName;
       SetLastResultFile(CSVName);

     EXCEPT

       On E: Exception DO Begin
         DoSimpleMsg('Error opening File "' + CSVName + '": ' + E.Message, 528);
         Exit;
       End;

     END;

     TRY
         Writeln(F, 'Level, Branch, Bus1, Bus2, Distance');
         IF BranchList<>NIL
         Then Begin
           PDElem := BranchList.First;
           WHILE PDElem <> NIL Do With ActiveCircuit[ActorID] Do
           Begin
               Writeln(F, Format('%d, %s.%s, %s, %s, %10.4f',
                 [BranchList.Level, PDelem.ParentClass.Name, PDelem.Name,
                  PDelem.FirstBus, PDelem.NextBus,
                  {BusList.Get(BranchList.PresentBranch.ToBusReference),}
                  Buses^[BranchList.PresentBranch.ToBusReference].DistFromMeter]));
               LoadElem := Branchlist.FirstObject;
               WHILE LoadElem <> NIL Do
               Begin
                     Writeln(F, '-1, ', Format('%s.%s, %s', [LoadElem.ParentClass.Name, LoadElem.Name, LoadElem.Firstbus{ActiveCircuit[ActorID].BusList.Get(BranchList.PresentBranch.ToBusReference)}]));
                     LoadElem := BranchList.NextObject
               End;
           PDElem := BranchList.GoForward;
           End;
         End;

     FINALLY
         Closefile(F);
     END;
END;


//- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Procedure TEnergyMeterObj.DumpProperties(Var F:TextFile; Complete:Boolean);

VAR
   i:Integer;
   pdelem:TPDelement;
   LoadElem:TDSSCktElement;


Begin
    Inherited DumpProperties(F, complete);

    WITH ParentClass Do
     FOR i := 1 to NumProperties Do
     Case i of
         4: Begin     // option
                Write(F,'~ ',PropertyName^[i],'=(');
                IF ExcessFlag Then Write(F, 'E,') Else Write(F, 'T,');
                IF ZoneIsRadial Then Write(F, ' R,') Else Write(F, ' M,');
                IF VoltageUEOnly Then Write(F, ' V') Else Write(F, ' C');
                Writeln(F,')');
            End;
         7: Writeln(F,'~ ',PropertyName^[i],'=(',PropertyValue[i],')');
     Else
        Writeln(F,'~ ',PropertyName^[i],'=',PropertyValue[i]);
     End;

    IF complete Then
     Begin

       Writeln(F, 'Registers');
       FOR i := 1 to NumEMregisters Do
       Begin
            Writeln(F, '"', RegisterNames[i],'" = ', Registers[i]:0:0);
       End;
       Writeln(F);

       Writeln(F, 'Branch List:');
       IF BranchList<>NIL
       Then Begin
         PDElem := BranchList.First;
         WHILE PDElem <> NIL Do
         Begin
             Writeln(F, 'Circuit Element = ', PDelem.Name);
             LoadElem := Branchlist.FirstObject;
             WHILE LoadElem <> NIL Do
             Begin
                   Writeln(F, '   Shunt Element = ', LoadElem.ParentClass.name, '.', LoadElem.Name);
                   LoadElem := BranchList.NextObject
             End;
             PDElem := BranchList.GoForward;
         End;
       End;

    End;

End;

PROCEDURE TEnergyMeter.ProcessOptions(const Opts: String);

VAR
   S1, S2 :String;
begin

    AuxParser.CmdString := Opts;  // Load up aux Parser

    {Loop until no more options found}
    WITH ActiveEnergymeterObj DO
    REPEAT
         S1 := AuxParser.NextParam; // ignore any parameter name  not expecting any
         S2 := lowercase(AuxParser.StrValue);
         IF Length(S2)>0 THEN
         CASE s2[1] of
              'e': ExcessFlag    := TRUE;
              't': ExcessFlag    := FALSE;
              'r': ZoneIsRadial  := TRUE;
              'm': ZoneIsRadial  := FALSE;
              'c': VoltageUEOnly := FALSE;
              'v': VoltageUEOnly := TRUE;
         End;
    UNTIL Length(S2)=0;

end;

function TEnergyMeterObj.AddToVoltBaseList(BusRef: Integer; ActorID:Integer): Integer;
{Add to VoltBase list if not already there and return index}
var
   i  :Integer;

begin
   With ActiveCircuit[ActorID].Buses^[BusRef]  Do Begin
     for i  := 1 to VBaseCount do  Begin
       if abs(1.0 - kVBase / VBaseList^[i]) < 0.01  then
       Begin    // < 1% difference
          Result := i;
          Exit;
       End;
     End;

     if (kvBase > 0.0) And (VBaseCount < MaxVBaseCount) Then
     Begin
         Inc(VBaseCount);
         VBaseList^[VBasecount] := {ActiveCircuit[ActorID].Buses^[BusRef].}kVBase;
         result := VBaseCount;
     End
     Else Result := 0;
   End;

end;

PROCEDURE TEnergyMeterObj.AllocateLoad(ActorID: integer);

VAR
   ConnectedPhase  :Integer;
   CktElem   :TPDElement;
   LoadElem  :TLoadobj;

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


     CktElem     := BranchList.First;
     WHILE CktElem <> NIL Do Begin
         LoadElem := Branchlist.FirstObject;
         WHILE (LoadElem <> NIL) Do Begin
             If (LoadElem.DSSObjType and CLASSMASK) = LOAD_ELEMENT  Then  // only for loads not other shunts
               CASE LoadElem.NPhases of
                 {For Single phase loads, allocate based on phase factor, else average factor}
                  1: WITH LoadElem Do Begin
                          ConnectedPhase := ActiveCircuit[ActorID].MapNodeToBus^[NodeRef^[1]].NodeNum;
                          IF  (ConnectedPhase > 0) and (ConnectedPhase < 4)   // Restrict to phases 1..3
                          THEN AllocationFactor := AllocationFactor * SensorObj.PhsAllocationFactor^[ConnectedPhase];
                     End;
               ELSE
                  WITH LoadElem Do AllocationFactor := AllocationFactor * SensorObj.AvgAllocFactor;
               End;  {CASE}
         LoadElem := BranchList.NextObject    {Next load at this bus}
         End;   {While Loadelem}
       CktElem := BranchList.GoForward;    {Go on down the tree}
     End;  {While CktElem}

end;

procedure TEnergyMeterObj.InitPropertyValues(ArrayOffset: Integer);
Var i:integer;
    S:String;
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
     For i := 1 to NumEMregisters do  S := S + '1 ';
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

PROCEDURE TEnergyMeterObj.Accumulate_Gen(pGen:TGeneratorObj; var TotalZonekW, TotalZonekvar:Double;ActorID:integer);
Var
   S:Complex;
begin
     //----pGen.ActiveTerminalIdx := 1;
     S := Cnegate(CmulReal(pGen.Power[1,ActorID], 0.001));
     TotalZonekw   := TotalZonekW   + S.re;
     TotalZonekvar := TotalZonekvar + S.im;

end;

function TEnergyMeterObj.Accumulate_Load(pLoad:TLoadObj;
  var TotalZonekW, TotalZonekvar, TotalLoad_EEN, TotalLoad_UE:Double; ActorID : Integer):double;
Var
   S_Load  :Complex;
   kW_Load :Double;
   Load_EEN,
   Load_UE  :Double;
begin
  WITH   pLoad  Do
  Begin
       //----ActiveTerminalIdx := 1;
       S_Load        := CmulReal(pLoad.Power[1,ActorID], 0.001);   // Get Power in Terminal 1
       kW_Load       := S_Load.re;
       Result := kw_Load;

       {Accumulate load in zone}
       TotalZonekw   := TotalZonekW   + kW_Load;
       TotalZonekvar := TotalZonekvar + S_Load.im;

       {always integrate even if the value is 0.0
        otherwise the Integrate function is not correct}
       {Invoking the ExceedsNormal and Unserved Properties causes the factors to be computed}
       IF ExcessFlag THEN
         Begin   // Return Excess load as EEN/UE
             If   (Get_ExceedsNormal(ActorID)) Then Load_EEN := kW_Load * EEN_Factor
                                  Else Load_EEN := 0.0;
             If   (Unserved[ActorID])      Then Load_UE  := kW_Load * UE_Factor
                                  Else Load_UE  := 0.0;
         End
       ELSE
         Begin    // Return TOTAL load as EEN/UE
             If   (Get_ExceedsNormal(ActorID)) Then Load_EEN := kW_Load
                                  Else Load_EEN := 0.0;
             If   (Unserved[ActorID])      Then Load_UE  := kW_Load
                                  Else Load_UE  := 0.0;
         End;

       TotalLoad_EEN := TotalLoad_EEN + Load_EEN;
       TotalLoad_UE  := TotalLoad_UE  + Load_UE;

  END; {WITH}
end;


PROCEDURE TEnergyMeterObj.ReduceZone(ActorID: integer);

{Reduce the zone by merging lines}

begin
 // Make  sure zone list is built
    If not assigned(BranchList) Then MakeMeterZoneLists(ActorID);

    Case ActiveCircuit[ActorID].ReductionStrategy of

         rsStubs:         DoReduceStubs(BranchList);    {See ReduceAlgs.Pas}
         {rsTapEnds:       DoReduceTapEnds (BranchList);}
         rsMergeParallel: DoMergeParallelLines(BranchList);
         rsDangling:      DoReduceDangling(BranchList);
         rsBreakLoop:     DoBreakLoops(BranchList);
         rsSwitches:      DoReduceSwitches(BranchList);
         rsLaterals:      DoReduceLaterals(BranchList);
    Else
       {Default}
       DoReduceDefault(BranchList);
    End;
(* Feeder Code removed
    // Resynchronize with Feeders
    If HasFeeder Then FeederObj.InitializeFeeder (Branchlist);
*)

end;

procedure TEnergyMeterObj.InterpolateCoordinates;
{Start at the ends of the zone and work toward the start
 interpolating between known coordinates}
Var
  i, BusRef,
  FirstCoordRef, SecondCoordRef,
  Linecount:integer;
  PresentNode, StartNode: TCktTreeNode;
  CktElem:TDSSCktElement;

begin
  If Not Assigned(BranchList) Then
  Begin
      DoSimpleMsg('Meter Zone Lists need to be built. Do Solve or Makebuslist first!', 529);
      Exit;
  End;
  With ActiveCircuit[ActiveActor] Do
   Begin

     For i := 1 to Branchlist.ZoneEndsList.NumEnds Do
     Begin
       Busref := Branchlist.ZoneEndsList.Get(i, PresentNode);

       FirstCoordRef := BusRef;
       SecondCoordRef :=   FirstCoordRef;  {so compiler won't issue stupid warning}
       {Find a bus with a coordinate}
       If Not Buses^[BusRef].CoordDefined Then
       Begin
          While Not Buses^[PresentNode.FromBusReference].CoordDefined Do
          Begin
             PresentNode := PresentNode.ParentBranch;
             If PresentNode=Nil Then Break;
          End;
          If PresentNode<> Nil then  FirstCoordRef := PresentNode.FromBusReference;
       End;

       While PresentNode <> Nil Do
       Begin
          {Back up until we find another Coord defined}
          LineCount := 0;   {number of line segments in this segment}
          StartNode := PresentNode;
          CktElem   := PresentNode.CktObject;
          If FirstCoordRef <> PresentNode.FromBusReference then
            Begin   {Handle special case for end branch}
                If  Buses^[PresentNode.FromBusReference].CoordDefined Then
                    FirstCoordRef := PresentNode.FromBusReference
                Else Inc(LineCount);
            End;

          Repeat
              CktElem.Checked := True;
              PresentNode := PresentNode.ParentBranch;
              If PresentNode=Nil Then Break;
              CktElem     := PresentNode.CktObject;
              SecondCoordRef := PresentNode.FromBusReference;
              Inc(LineCount);
          Until Buses^[SecondCoordRef].CoordDefined or CktElem.Checked;

          If (PresentNode<>Nil) and (LineCount>1) Then
           IF Buses^[SecondCoordRef].CoordDefined Then
             Begin
               CalcBusCoordinates(StartNode,  FirstCoordRef, SecondCoordRef, LineCount);
             End
            Else Break; {While - went as far as we could go this way}

          FirstCoordRef := SecondCoordRef;
       End;

     End; {For}

  End; {With}

end;

procedure TEnergyMeterObj.CalcBusCoordinates(StartBranch:TCktTreeNode;
  FirstCoordRef,  SecondCoordref, LineCount: Integer);

Var
   X, Y, Xinc, Yinc:Double;
begin

     If LineCount = 1 then Exit;  {Nothing to do!}

     With ActiveCircuit[ActiveActor] Do
     Begin
       Xinc := (Buses^[FirstCoordref].X - Buses^[SecondCoordRef].X) / LineCount;
       Yinc := (Buses^[FirstCoordref].Y - Buses^[SecondCoordRef].Y) / LineCount;

       X := Buses^[FirstCoordref].X;
       Y := Buses^[FirstCoordref].Y;

       {***Debug}
   (*    If ((X<10.0) and (y<10.0)) or
          ((Buses^[SecondCoordRef].X<10.0) and (Buses^[SecondCoordRef].Y<10.0)) Then
       Begin
          X := y;  // Stopping point
       End;
     *)
     
     {Either start with the "to" end of StartNode or the "from" end;}
       If FirstCoordRef <> StartBranch.FromBusReference Then
       Begin  // Start with "to" end
          X:= X- Xinc;
          Y:= Y- Yinc;
          Buses^[StartBranch.FromBusReference].X := X;
          Buses^[StartBranch.FromBusReference].Y := Y;
          Buses^[StartBranch.FromBusReference].CoordDefined := True;
          Dec(LineCount);
       End;

       While LineCount>1 Do
       Begin
          X:= X- Xinc;
          Y:= Y- Yinc;
          StartBranch := StartBranch.ParentBranch; // back up the tree
          Buses^[StartBranch.FromBusReference].X := X;
          Buses^[StartBranch.FromBusReference].Y := Y;
          Buses^[StartBranch.FromBusReference].CoordDefined := True;
          Dec(LineCount);
       End;

     End;
end;




{-------------------------------------------------------------------------------}
procedure TEnergyMeterObj.CalcReliabilityIndices(AssumeRestoration:Boolean; ActorID : Integer);
Var
    PD_Elem : TPDElement;
    pLoad   : TLoadObj;
    idx     : Integer;
    pBus    : TDSSBus;
    dblNcusts  : Double;
    dblkW      : Double;

begin

       If not assigned (SequenceList) Then
       Begin
           DoSimpleMsg('Energymeter.' + Name + ' Zone not defined properly.', 52901);
           Exit;
       End;

    // Zero reliability accumulators
       For idx := SequenceList.ListSize downto 1 Do
           TPDElement(SequenceList.Get(idx)).ZeroReliabilityAccums ;

    // Backward sweep calculating failure rates
       For idx := SequenceList.ListSize downto 1 Do
           Begin
               With TPDElement(SequenceList.Get(idx)) do
               Begin
                   CalcFltRate;    // Calc failure rate for this element
                   AccumFltRate;
               End;
           End;

    // Forward sweep to get number of interruptions
       // Initialize number of interruptions and Duration
       PD_Elem := TPDElement(SequenceList.Get(1));
       pBus    := ActiveCircuit[ActorID].Buses^[PD_Elem.Terminals^[PD_Elem.FromTerminal].BusRef];
       pBus.Bus_Num_Interrupt  := Source_NumInterruptions;
       pBus.BusCustInterrupts  := Source_NumInterruptions * pBus.BusTotalNumCustomers;
       pBus.Bus_Int_Duration   := Source_IntDuration;

       // init for defining sections
       SectionCount := 0;
       pBus.BusSectionID := SectionCount; // section before 1st OCP device is zero

       For idx := 1 to SequenceList.ListSize Do
           TPDElement(SequenceList.Get(idx)).CalcNum_Int(SectionCount, AssumeRestoration);

       If SectionCount=0 Then
       Begin   // Error - no OCP devices
             DoSimpleMsg('Error: No Overcurrent Protection device (Relay, Recloser, or Fuse) defined. Aborting Reliability calc.', 52902);
             Exit;
       End;

       // Now have number of sections  so allocate FeederSections array
       Reallocmem(FeederSections, Sizeof(FeederSections^[1])*SectionCount);
       for idx := 1 to SectionCount do
            With FeederSections^[idx] Do
            Begin
                OCPDeviceType         := 0;    // 1=Fuse; 2=Recloser; 3=Relay
                AverageRepairTime     := 0.0;
                SumFltRatesXRepairHrs := 0.0;
                SumBranchFltRates     := 0.0;
                NCustomers            := 0;
                TotalCustomers        := 0;
                SectFaultRate         := 0.0;
                NBranches             := 0;
                SeqIndex              := 0;
            End;


    // Now do Backward sweep calculating N*Fault rates
       For idx := SequenceList.ListSize downto 1 Do
           Begin
               PD_Elem := SequenceList.Get(idx);
               PD_Elem.CalcCustInterrupts;

              With FeederSections^[PD_Elem.BranchSectionID] Do
              Begin
                Inc(NCustomers, PD_Elem.BranchNumCustomers);
                // Sum up num Customers on this Section
                Inc(NBranches, 1); // Sum up num branches on this Section
                pBus := ActiveCircuit[ActorID].Buses^
                  [PD_Elem.Terminals^[PD_Elem.ToTerminal].BusRef];
                DblInc(SumBranchFltRates, pBus.Bus_Num_Interrupt * PD_Elem.BranchFltRate);
                DblInc(SumFltRatesXRepairHrs,
                  (pBus.Bus_Num_Interrupt * PD_Elem.BranchFltRate * PD_Elem.HrsToRepair));
                If PD_Elem.HasOCPDevice Then
                Begin
                  OCPDeviceType := GetOCPDeviceType(PD_Elem);
                  SeqIndex := idx;  // index of pdelement with OCP device at head of section
                  TotalCustomers := PD_Elem.BranchTotalCustomers;
                  SectFaultRate := PD_Elem.AccumulatedBrFltRate;
                End;
              End;

          {$IFDEF DEBUG}
              If idx = SequenceList.ListSize then
                WriteDLLDebugFile
                  ('Meter, SectionID, BranchName, FaultRate, AccumulatedBrFltRate, BranchFltRate, RepairHrs, NCustomers, Num_Interrupt');
              With FeederSections^[PD_Elem.BranchSectionID] Do
                WriteDLLDebugFile
                  (Format('%s.%s, %d, %s.%s, %.11g, %.11g, %.11g, %.11g, %d, %.11g ',
                  [ParentClass.Name, Name, PD_Elem.BranchSectionID,
                  PD_Elem.ParentClass.Name, PD_Elem.Name, PD_Elem.FaultRate,
                  PD_Elem.AccumulatedBrFltRate, PD_Elem.BranchFltRate,
                  PD_Elem.HrsToRepair, PD_Elem.BranchNumCustomers,
                  pBus.Bus_Num_Interrupt]));
          {$ENDIF}
           End;

          { Compute Avg Interruption duration of each Section }
          for idx := 1 to SectionCount do
            With FeederSections^[idx] Do
              AverageRepairTime := SumFltRatesXRepairHrs / SumBranchFltRates;

          { Set Bus_int_Duration }

          With ActiveCircuit[ActorID] do
            for idx := 1 to NumBuses do
            Begin
              pBus := Buses^[idx];
              If pBus.BusSectionID > 0 Then
                pBus.Bus_Int_Duration := Source_IntDuration + FeederSections^
                  [pBus.BusSectionID].AverageRepairTime;
            End;

        {$IFDEF DEBUG}
          WriteDLLDebugFile
            ('Meter, SectionID, NBranches, NCustomers, AvgRepairHrs, AvgRepairMins, FailureRate*RepairtimeHrs, SumFailureRates');
          for idx := 1 to SectionCount do
            With FeederSections^[idx] Do
              WriteDLLDebugFile(Format('%s.%s, %d, %d, %d, %.11g, %.11g, %.11g, %.11g ',
                [ParentClass.Name, Name, idx, NBranches, NCustomers, AverageRepairTime,
                AverageRepairTime * 60.0, SumFltRatesXRepairHrs, SumBranchFltRates]));
        {$ENDIF}

        { Compute SAIFI based on numcustomers and load kW }
        { SAIFI is weighted by specified load weights }
        { SAIFI is for the EnergyMeter Zone }
        SAIFI := 0.0;
        CAIDI := 0.0;
        SAIFIkW := 0.0;
        CustInterrupts := 0.0;
        dblNcusts := 0.0;
        dblkW := 0.0;

        // Use LoadList for SAIFI calculation
        WITH ActiveCircuit[ActorID] do
          For idx := 1 to LoadList.ListSize Do // all loads in meter zone
          Begin
            pLoad := TLoadObj(LoadList.Get(idx));
            WITH pLoad Do
            Begin
              pBus := Buses^[Terminals^[1].BusRef]; // pointer to bus
              CustInterrupts := CustInterrupts + NumCustomers * RelWeighting *
                pBus.Bus_Num_Interrupt;
              SAIFIkW := SAIFIkW + kWBase * RelWeighting * pBus.Bus_Num_Interrupt;
              DblInc(dblNcusts, NumCustomers * RelWeighting);
              // total up weighted numcustomers
              DblInc(dblkW, kWBase * RelWeighting); // total up weighted kW
              // Set BusCustDurations for Branch reliability export
              pBus.BusCustDurations := (pBus.BusTotalNumCustomers + NumCustomers) *
                     RelWeighting * pBus.Bus_Int_Duration * pBus.Bus_Num_Interrupt;
            End;
          End;

        // Compute SAIDI from Sections list
        SAIDI := 0.0;
        For idx := 1 to SectionCount Do
          With FeederSections^[idx] Do
            Begin
              SAIDI := SAIDI +  SectFaultRate * AverageRepairTime * TotalCustomers  ;
            End;

        If dblNcusts > 0.0 Then
          Begin
            SAIFI := CustInterrupts / dblNcusts; // Normalize to total number of customers
            SAIDI := SAIDI / dblNcusts; // Normalize to total number of customers
          End;
        If SAIFI > 0.0 Then
          CAIDI := SAIDI / SAIFI;

        If dblkW > 0.0 Then
          SAIFIkW := SAIFIkW / dblkW; // Normalize to total kW

end;

{-------------------------------------------------------------------------------}
function TEnergyMeterObj.GetPropertyValue(Index: Integer): String;
begin
        Case Index of
          4,7: Result := '(';
        Else
            Result := '';
        End;

        CASE Index of
           4: Begin     // option
                IF ExcessFlag Then Result := Result +'E,' Else Result := Result +'T,';
                IF ZoneIsRadial Then Result := Result +' R,' Else Result := Result +' M,';
                IF VoltageUEOnly then Result := Result +' V' Else Result := Result +' C';
              End;
           20: Result := Format('%.11g',[SAIFI]);
           21: Result := Format('%.11g',[SAIFIkW]);
           22: Result := Format('%.11g',[SAIDI]);
           23: Result := Format('%.11g',[CAIDI]);
           24: Result := Format('%.11g',[CustInterrupts]);
        ELSE
           Result := Result + Inherited GetPropertyValue(index);
        END;

        Case Index of
          4,7: Result := Result + ')';
        Else
        End;
end;

procedure TEnergyMeterObj.SaveZone(const dirname:String);

Var cktElem, shuntElement:TDSSCktElement;
    LoadElement:TLoadObj;
    pControlElem : TDSSCktElement;
    FBranches, FShunts, FLoads, FGens, FCaps, FXfmrs: TextFile;
    NBranches, NShunts, Nloads, NGens, NCaps, NXfmrs: Integer;


begin
 {We are in the directory indicated by dirname}

{Run down the zone and write each element into a file}

   IF BranchList<>NIL Then
   Begin
    {Open some files:}

     Try
         AssignFile(FBranches, 'Branches.dss');     // Both lines and transformers
         Rewrite(FBranches);
         NBranches := 0;
     Except
         On E:Exception Do Begin
             DoSimpleMsg('Error creating Branches.dss for Energymeter: ' + Self.Name+'. '+E.Message , 530);
             CloseFile(FBranches);
             Exit;
         End;
     End;

     Try
         AssignFile(FXfmrs, 'Transformers.dss');     // Both lines and transformers
         Rewrite(FXfmrs);
         NXfmrs := 0;
     Except
         On E:Exception Do Begin
             DoSimpleMsg('Error creating Transformers.dss for Energymeter: ' + Self.Name+'. '+E.Message , 53001);
             CloseFile(FXfmrs);
             Exit;
         End;
     End;

     Try
         AssignFile(FShunts, 'Shunts.dss');
         Rewrite(FShunts);
         NShunts := 0;
     Except
         On E:Exception Do Begin
             DoSimpleMsg('Error creating Shunts.dss for Energymeter: ' + Self.Name+'. '+E.Message , 531);
             CloseFile(FShunts);
             Exit;
         End;
     End;

     Try
         AssignFile(FLoads, 'Loads.dss');
         Rewrite(FLoads);
         Nloads := 0;
     Except
         On E:Exception Do Begin
             DoSimpleMsg('Error creating Loads.dss for Energymeter: ' + Self.Name+'. '+E.Message , 532);
             CloseFile(FLoads);
             Exit;
         End;
     End;

     Try
         AssignFile(FGens, 'Generators.dss');
         Rewrite(FGens);
         NGens := 0;
     Except
         On E:Exception Do Begin
             DoSimpleMsg('Error creating Generators.dss for Energymeter: ' + Self.Name+'. '+E.Message , 533);
             CloseFile(FGens);
             Exit;
         End;
     End;

     Try
         AssignFile(FCaps, 'Capacitors.dss');
         Rewrite(FCaps);
         Ncaps := 0;
     Except
         On E:Exception Do Begin
             DoSimpleMsg('Error creating Capacitors.dss for Energymeter: ' + Self.Name+'. '+E.Message, 534 );
             CloseFile(FCaps);
             Exit;
         End;
     End;


     cktElem := BranchList.First;
     With ActiveCircuit[ActiveActor] Do
     WHILE cktElem <> NIL Do
       Begin
         If CktElem.Enabled Then Begin
           ActiveCktElement := cktElem;

           If (CktElem.DSSObjType and Classmask) = XFMR_ELEMENT  Then
           Begin
             Inc(NXfmrs);
             WriteActiveDSSObject(FXfmrs, 'New');     // sets HasBeenSaved := TRUE
             If cktElem.HasControl Then Begin
                pControlElem := cktElem.ControlElementList.First;
                while pControlElem <> nil do
                Begin
                     ActiveCktElement := pControlElem;
                     WriteActiveDSSObject(FXfmrs, 'New');  //  regulator control ...Also, relays, switch controls
                     pControlElem := cktElem.ControlElementList.Next;
                End;
             End;
           End
           Else Begin  {Mostly LINE elements}
             Inc(NBranches);
             WriteActiveDSSObject(FBranches, 'New');     // sets HasBeenSaved := TRUE
             If cktElem.HasControl Then Begin
                pControlElem := cktElem.ControlElementList.First;
                while pControlElem <> nil do
                Begin
                     ActiveCktElement := pControlElem;
                     WriteActiveDSSObject(FBranches, 'New');  //  regulator control ...Also, relays, switch controls
                     pControlElem := cktElem.ControlElementList.Next;
                End;
             End;
           End;



           shuntElement := Branchlist.FirstObject;
           While shuntElement <> Nil Do
             Begin
                 ActiveCktElement := shuntElement;
                 If (shuntElement.DSSObjType and Classmask)=LOAD_ELEMENT Then  Begin
                     LoadElement := TLoadObj(shuntElement);
                     If LoadElement.HasBeenAllocated Then Begin
                       {Manually set the allocation factor so it shows up}
                       Parser[ActiveActor].CmdString := 'allocationfactor='+Format('%-.4g',[LoadElement.AllocationFactor]);
                       LoadElement.Edit(ActiveActor);
                     End;
                     ActiveCktElement := shuntElement; // reset in case Edit mangles it
                     Inc(NLoads);
                     WriteActiveDSSObject(FLoads, 'New');
                 End Else If (shuntElement.DSSObjType and Classmask)=GEN_ELEMENT Then Begin
                     Inc(NGens);
                     WriteActiveDSSObject(FGens, 'New');
                     If shuntElement.HasControl Then Begin
                        pControlElem := shuntElement.ControlElementList.First;
                        while pControlElem <>Nil do
                        Begin
                            ActiveCktElement := pControlElem;
                            WriteActiveDSSObject(FGens, 'New');
                            pControlElem := shuntElement.ControlElementList.Next;
                        End;
                     End;
                 End
                 Else If (shuntElement.DSSObjType and Classmask)=CAP_ELEMENT Then Begin
                     Inc(NCaps);
                     WriteActiveDSSObject(FCaps, 'New');
                     If shuntElement.HasControl Then Begin
                        pControlElem := shuntElement.ControlElementList.First;
                        while pControlElem <> Nil do
                        Begin
                            ActiveCktElement := pControlElem;
                            WriteActiveDSSObject(FCaps, 'New');
                            pControlElem := shuntElement.ControlElementList.Next;
                        End;
                     End;
                 End Else Begin
                   Inc(NShunts);
                   WriteActiveDSSObject(Fshunts, 'New');
                 End;
               shuntElement := BranchList.NextObject
             End;
          End; {if enabled}

        cktElem := BranchList.GoForward;
       End;{WHILE}

     CloseFile(FBranches);
     CloseFile(FXfmrs);
     CloseFile(Fshunts);
     CloseFile(FLoads);
     CloseFile(FGens);
     CloseFile(FCaps);

     {If any records were written to the file, record their relative names}
     If NBranches>0  Then SavedFileList[ActiveActor].Add (dirname + '\Branches.dss') else DeleteFile('Branches.dss');
     If NXfmrs>0  Then SavedFileList[ActiveActor].Add (dirname + '\Transformers.dss') else DeleteFile('Transformers.dss');
     If NShunts>0 Then SavedFileList[ActiveActor].Add (dirname + '\Shunts.dss') else DeleteFile('Shunts.dss');
     If NLoads>0  Then SavedFileList[ActiveActor].Add (dirname + '\Loads.dss') else DeleteFile('Loads.dss');
     If NGens>0   Then SavedFileList[ActiveActor].Add (dirname + '\Generators.dss') else DeleteFile('Generators.dss');
     If NCaps>0   Then SavedFileList[ActiveActor].Add (dirname + '\Capacitors.dss') else DeleteFile('Capacitors.dss');

   End; {IF}

end;


procedure TEnergyMeterObj.SetDragHandRegister(Reg: Integer;  const Value: Double);
begin
    If  Value > Registers[reg] Then Begin
       Registers[reg]   := Value;
       Derivatives[reg] := Value;  // Use this for   demand interval data;
    End;
end;

procedure TEnergyMeterObj.CloseDemandIntervalFile(ActorID: integer);
Var
  i   :   integer;

begin

  Try
     IF This_Meter_DIFileIsOpen Then Begin
       if DI_MHandle[ActorID] <> nil then
         CloseMHandler(DI_MHandle[ActorID], MakeDIFileName(ActorID), DI_Append[ActorID]);
       DI_MHandle[ActorID] :=  nil;
       This_Meter_DIFileIsOpen := FALSE;
       if PHV_MHandle <> nil then
        If VPhaseReportFileIsOpen then CloseMHandler(PHV_MHandle[ActorID], MakeVPhaseReportFileName(ActorID), PHV_Append[ActorID]) ;
       PHV_MHandle[ActorID]  :=  nil;
       VPhaseReportFileIsOpen := FALSE;
     End;
  Except
     ON E:Exception Do DoSimpleMsg('Error Closing Demand Interval file for Meter "'+Name+'"', 534   );
  End;


     {Write Registers to Totals File}
     with EnergyMeterClass[ActorID] do
     begin
       WriteintoMemStr(EMT_MHandle[ActorID],'"' + Self.Name + '"');
       For i := 1 to NumEMregisters Do WriteintoMem(EMT_MHandle[ActorID],Registers[i]);
       WriteintoMemStr(EMT_MHandle[ActorID], Char(10));
     end;
end;

procedure TEnergyMeterObj.OpenDemandIntervalFile(ActorID: integer);
Var i,j     : Integer;
    vbase   : double;

begin

  Try
      IF This_Meter_DIFileIsOpen Then CloseDemandIntervalFile(ActorID);

      If (EnergyMeterClass[ActorID].DI_Verbose[ActorID]) Then Begin

          This_Meter_DIFileIsOpen :=  TRUE;
          if DI_MHandle[ActorID] <> nil then DI_MHandle[ActorID].free;
          DI_MHandle[ActorID]  :=  Create_Meter_Space('"Hour"');
          For i := 1 to NumEMRegisters Do WriteintoMemStr(DI_MHandle[ActorID],', "' + RegisterNames[i] + '"');
          WriteintoMemStr(DI_MHandle[ActorID], Char(10));

         {Phase Voltage Report, if requested}
          If FPhaseVoltageReport Then Begin
              if PHV_MHandle[ActorID] <> nil then PHV_MHandle[ActorID].free;
              PHV_MHandle[ActorID] :=  Create_Meter_Space('"Hour"');
              VPhaseReportFileIsOpen := TRUE;
              For i := 1 to MaxVBaseCount Do Begin
                vbase := VBaseList^[i] * SQRT3;
                If Vbase > 0.0 then   Begin
                  For j := 1 to 3 Do WriteintoMemStr(PHV_MHandle[ActorID], Format(', %.3gkV_Phs_%d_Max', [vbase, j]));
                  For j := 1 to 3 Do WriteintoMemStr(PHV_MHandle[ActorID], Format(', %.3gkV_Phs_%d_Min', [vbase, j]));
                  For j := 1 to 3 Do WriteintoMemStr(PHV_MHandle[ActorID], Format(', %.3gkV_Phs_%d_Avg', [vbase, j]));
                End;
              End;
              WriteintoMemStr(PHV_MHandle[ActorID], ', Min Bus, MaxBus' + Char(10));
          End;

      End;
  Except
      On E:Exception Do DosimpleMsg('Error opening demand interval file "' + Name + '.CSV' +' for writing.'+CRLF+E.Message, 535);
  End;

end;

procedure TEnergyMeterObj.WriteDemandIntervalData(ActorID: integer);
Var i,j   : Integer;

     Function MyCount_Avg(const Value:Double; const count:Integer): double;
     Begin
         If Count=0 then Result := 0.0
         Else            Result := Value/count;
     End;

begin
      If EnergyMeterClass[ActorID].DI_Verbose[ActorID] and This_Meter_DIFileIsOpen Then Begin
          With ActiveCircuit[ActorID].Solution Do  WriteintoMem(DI_MHandle[ActorID], DynaVars.dblHour);
          For i := 1 to NumEMRegisters Do WriteintoMem(DI_MHandle[ActorID], Derivatives[i]);
          WriteIntoMemStr(DI_MHandle[ActorID],Char(10));
      End;

      {Add to Class demand interval registers}
      With EnergyMeterClass[ActorID] Do For i := 1 to NumEMRegisters Do DI_RegisterTotals[i] := DI_RegisterTotals[i] + Derivatives[i]*TotalsMask[i];


      {Phase Voltage Report, if requested}
      If VPhaseReportFileIsOpen Then Begin
          With ActiveCircuit[ActorID].Solution Do WriteintoMem(PHV_MHandle[ActorID], DynaVars.dblHour);
          For i := 1 to MaxVBaseCount Do
          If VBaseList^[i] > 0.0 then  Begin
              For j := 1 to 3 Do WriteintoMem(PHV_MHandle[ActorID], 0.001 * VPhaseMax^[jiIndex(j, i)]);
              For j := 1 to 3 Do WriteintoMem(PHV_MHandle[ActorID], 0.001 * VPhaseMin^[jiIndex(j, i)]);
              For j := 1 to 3 Do WriteintoMem(PHV_MHandle[ActorID], 0.001 * MyCount_Avg(VPhaseAccum^[jiIndex(j, i)], VPhaseAccumCount^[jiIndex(j, i)]));
          End;
          WriteintoMemStr(PHV_MHandle[ActorID], Char(10));
      End;

end;

procedure TEnergyMeter.CloseAllDIFiles(ActorID:Integer);
VAR
   mtr:TEnergyMeterObj;

Begin
      If FSaveDemandInterval Then Begin
        {While closing DI files, write all meter registers to one file}
        Try
            CreateMeterTotals(ActorID);
        Except
            On E:Exception Do DoSimpleMsg('Error on Rewrite of totals file: '+E.Message, 536);
        End;

        {Close all the DI file for each meter}
        mtr := ActiveCircuit[ActorID].EnergyMeters.First;
        WHILE mtr<>NIL DO Begin
            IF mtr.enabled Then mtr.CloseDemandIntervalFile(ActorID);
            mtr := ActiveCircuit[ActorID].EnergyMeters.Next;
        End;

        WriteTotalsFile(ActorID);  // Sum all energymeter registers to "Totals.CSV"
        SystemMeter.CloseDemandIntervalFile(ActorID);
        SystemMeter.Save(ActorID);
        if EMT_MHandle[ActorID] <> nil then
          CloseMHandler(EMT_MHandle[ActorID], DI_Dir + '\EnergyMeterTotals_' + inttostr(ActorID) + '.CSV', EMT_Append[ActorID]);
        EMT_MHandle[ActorID] :=  nil;
        if TDI_MHandle[ActorID] <> nil then
          CloseMHandler(TDI_MHandle[ActorID], DI_Dir+'\DI_Totals_' + inttostr(ActorID) + '.CSV', TDI_Append[ActorID]);
        TDI_MHandle[ActorID] :=  nil;
        DIFilesAreOpen[ActorID] := FALSE;
        if OverloadFileIsOpen then Begin
          if OV_MHandle[ActorID] <> nil then
            CloseMHandler(OV_MHandle[ActorID],EnergyMeterClass[ActorID].DI_Dir+'\DI_Overloads_' + inttostr(ActorID) + '.CSV', OV_Append[ActorID]);
          OV_MHandle[ActorID]  :=  nil;
          OverloadFileIsOpen := FALSE;
        End;
        if VoltageFileIsOpen then Begin
          if VR_MHandle[ActorID] <> nil then
            CloseMHandler(VR_MHandle[ActorID],EnergyMeterClass[ActorID].DI_Dir+'\DI_VoltExceptions_' + inttostr(ActorID) + '.CSV', VR_Append[ActorID]);
          VR_MHandle[ActorID]  := nil;
          VoltageFileIsOpen := FALSE;
        End;
      End;
end;

procedure TEnergyMeterObj.AppendDemandIntervalFile(ActorID:integer);

Var
    FileNm:String;
begin

  {Only called if "SaveDemandInterval"}

  If This_Meter_DIFileIsOpen Then Exit;

  Try
      If Energymeterclass[ActorID].FDI_Verbose Then Begin
          FileNm := MakeDIFileName(ActorID);   // Creates directory if it doesn't exist
          If FileExists(FileNm) Then DI_Append[ActorID]  :=  True
          Else DI_Append[ActorID] :=  False;
          if DI_MHandle[ActorID] <> nil then DI_MHandle[ActorID].free;
          DI_MHandle[ActorID]  :=  Create_Meter_Space(' ');
          This_Meter_DIFileIsOpen := TRUE;
      End;
  Except
      On E:Exception Do DosimpleMsg('Error opening demand interval file "'+Name+'.CSV' +' for appending.'+CRLF+E.Message, 537);
  End;
end;

procedure TEnergyMeterObj.AssignVoltBaseRegisterNames;
var
  i, ireg: Integer;
  vbase: double;
begin
  ireg := 1;
  for i := 1 to MaxVBaseCount  do begin
    if VBaseList^[i] > 0.0 then begin
      vbase := VBaseList^[i]* SQRT3;
      RegisterNames[i + Reg_VBaseStart] := Format('%.3g kV Losses', [vbase]);
      RegisterNames[i + 1 * MaxVBaseCount + Reg_VBaseStart] := Format('%.3g kV Line Loss', [vbase]);
      RegisterNames[i + 2 * MaxVBaseCount + Reg_VBaseStart] := Format('%.3g kV Load Loss', [vbase]);
      RegisterNames[i + 3 * MaxVBaseCount + Reg_VBaseStart] := Format('%.3g kV No Load Loss', [vbase]);
      RegisterNames[i + 4 * MaxVBaseCount + Reg_VBaseStart] := Format('%.3g kV Load Energy', [vbase])
    end else begin
      RegisterNames[i + Reg_VBaseStart] := Format('Aux%d',[ireg]);
      Inc (ireg);
      RegisterNames[i + 1 * MaxVBaseCount + Reg_VBaseStart] := Format('Aux%d',[ireg]);
      Inc (ireg);
      RegisterNames[i + 2 * MaxVBaseCount + Reg_VBaseStart] := Format('Aux%d',[ireg]);
      Inc (ireg);
      RegisterNames[i + 3 * MaxVBaseCount + Reg_VBaseStart] := Format('Aux%d',[ireg]);
      Inc (ireg);
      RegisterNames[i + 4 * MaxVBaseCount + Reg_VBaseStart] := Format('Aux%d',[ireg]);
      Inc (ireg);
    end;
  end;
  for i := 1 + Reg_VBaseStart + 5 * MaxVBaseCount to NumEMRegisters do begin
    RegisterNames[i] := Format('Aux%d',[ireg]);
    Inc (ireg);
  end;
end;

procedure TEnergyMeter.AppendAllDIFiles(ActorID:integer);
VAR
   mtr:TEnergyMeterObj;
   Filenm:String;

Begin
      If FSaveDemandInterval Then  Begin

          ClearDI_Totals;  // clears accumulator arrays

          mtr := ActiveCircuit[ActorID].EnergyMeters.First;
          WHILE mtr<>NIL DO Begin
              IF mtr.enabled Then mtr.AppendDemandIntervalFile(ActorID);
              mtr := ActiveCircuit[ActorID].EnergyMeters.Next;
          End;

          SystemMeter.AppendDemandIntervalFile(ActorID);

          {Open FDI_Totals}
          Try
              FileNm :=  DI_Dir+'\DI_Totals_' + inttostr(ActorID) + '.CSV';
              {File Must Exist}
              If FileExists(FileNm) Then  TDI_Append[ActorID] := True;
              CreateFDI_Totals(ActorID);
          Except
              On E:Exception Do DosimpleMsg('Error opening demand interval file "'+Name+'.CSV' +' for appending.'+CRLF+E.Message, 538);
          End;

          DIFilesAreOpen[ActorID] := TRUE;

      End;{IF}
end;

function TEnergyMeterObj.MakeDIFileName(ActorID:integer): String;
begin
    Result := EnergyMeterClass[ActorID].DI_Dir + '\' + Self.Name + '_' + inttostr(ActorID) + '.CSV';
end;

procedure TEnergyMeter.Set_SaveDemandInterval(ActorID: integer; const Value: Boolean);
begin
  FSaveDemandInterval := Value;
  ResetAll(ActorID);
end;

Function TEnergyMeter.Get_SaveDemandInterval(ActorID: integer):Boolean;
begin
  Result:=FSaveDemandInterval;
end;

procedure TEnergyMeter.WriteOverloadReport(ActorID : Integer);
Var
   PDelem   :TPDelement;
   Cmax     :double;
   mtr      : TEnergyMeterObj;

begin
{
  Scans the active circuit for overloaded PD elements and writes each to a file
  This is called only if in Demand Interval (DI) mode and the file is open.
}

 { CHECK PDELEMENTS ONLY}
     PDelem :=  ActiveCircuit[ActorID].PDElements.First;
     WHILE PDelem<>nil DO Begin
       IF (PDelem.Enabled)and (Not PDelem.IsShunt)  THEN Begin   // Ignore shunts

          IF (PdElem.Normamps > 0.0) OR (PdElem.Emergamps>0.0) THEN Begin
             PDelem.ComputeIterminal(ActorID);
             Cmax := PDelem.MaxTerminalOneImag(ActorID); // For now, check only terminal 1 for overloads
             IF (Cmax > PDElem.NormAmps) OR (Cmax > pdelem.EmergAmps) THEN Begin
                  With ActiveCircuit[ActorID].Solution Do WriteintoMem(OV_MHandle[ActorID],DynaVars.dblHour);
                  WriteintoMemStr(OV_MHandle[ActorID],', ' + FullName(PDelem));
                  WriteintoMem(OV_MHandle[ActorID],PDElem.NormAmps);
                  WriteintoMem(OV_MHandle[ActorID],pdelem.EmergAmps);
                 IF PDElem.Normamps > 0.0  THEN WriteintoMem(OV_MHandle[ActorID],Cmax/PDElem.Normamps*100.0)
                                           ELSE WriteintoMem(OV_MHandle[ActorID], 0.0);
                 IF PDElem.Emergamps > 0.0 THEN WriteintoMem(OV_MHandle[ActorID],Cmax/PDElem.Emergamps*100.0)
                                           ELSE WriteintoMem(OV_MHandle[ActorID], 0.0);
                 With ActiveCircuit[ActorID] Do // Find bus of first terminal
                   WriteintoMem(OV_MHandle[ActorID],Buses^[MapNodeToBus^[PDElem.NodeRef^[1]].BusRef].kVBase);
                 WriteintoMemStr(OV_MHandle[ActorID], ' ' + char(10));
             END;
          End; { }
       End;
        PDelem := ActiveCircuit[ActorID].PDElements.Next;
     End;
end;

procedure TEnergyMeter.ClearDI_Totals;
Var i:integer;
begin
   For i := 1 to NumEMRegisters Do DI_RegisterTotals[i] := 0.0;
end;

procedure TEnergyMeter.CreateFDI_Totals(ActorID: integer);
Var i:Integer;
    mtr:TEnergyMeterObj;

begin
 Try
    if TDI_MHandle[ActorID] <> nil then TDI_MHandle[ActorID].free;
    TDI_MHandle[ActorID] :=  Create_Meter_Space('Time');
    mtr := ActiveCircuit[ActorID].EnergyMeters.First;  // just get the first one
    if Assigned(mtr) then
    begin
      For i := 1 to NumEMRegisters Do
      begin
        WriteintoMemStr(TDI_MHandle[ActorID],', "' + mtr.RegisterNames[i] +'"');
      end;
    end;
    WriteintoMemStr(TDI_MHandle[ActorID], Char(10));
 Except
    On E:Exception Do DoSimpleMsg('Error creating: "'+DI_Dir+'\DI_Totals_' + inttostr(ActorID) + '.CSV": '+E.Message, 539)
 End;
end;

{ TSystemMeter }

procedure TSystemMeter.AppendDemandIntervalFile(ActorID: integer);
Var
    FileNm:String;
begin

  {Only called if "SaveDemandInterval"}

  If This_Meter_DIFileIsOpen Then Exit;

  Try
      FileNm := EnergyMeterClass[ActorID].Di_Dir + '\DI_SystemMeter_' + inttostr(ActorID) + '.CSV';
      AssignFile(SystemDIFile, FileNm );
      {File Must Exist}
      If FileExists(FileNm) Then
      Begin
//        DI_MMFView:=  MapFile2Memory(EnergyMeterClass.DI_Dir+'\DI_SystemMeter.CSV', DI_MMFHandle);
//        DI_Cursor :=  GetMMFCursor(DI_MMFView);
      End
      Else OpenDemandIntervalFile(ActorID);
      This_Meter_DIFileIsOpen := TRUE;
  Except
      On E:Exception Do DosimpleMsg('Error opening demand interval file "'+FileNm +' for appending.'+CRLF+E.Message, 540);
  End;

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

procedure TSystemMeter.CloseDemandIntervalFile(ActorID: integer);
var
  File_Path : string;
  mtr       : TEnergyMeterObj;
begin
     IF This_Meter_DIFileIsOpen Then Begin
       File_Path  :=  EnergyMeterClass[ActorID].DI_Dir+'\DI_SystemMeter_' + inttostr(ActorID) + '.CSV';
       CloseMHandler(SDI_MHandle[ActorID], File_Path, SDI_Append[ActorID]);
       SDI_MHandle[ActorID]  :=  nil;
       This_Meter_DIFileIsOpen := FALSE;
     End;
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

procedure TSystemMeter.Integrate(Var Reg:Double; Value:Double; Var Deriv:Double; ActorID: integer);
begin
     IF ActiveCircuit[ActorID].TrapezoidalIntegration THEN
      Begin
        {Trapezoidal Rule Integration}
        If Not FirstSampleAfterReset Then Reg := Reg + 0.5 * ActiveCircuit[ActorID].Solution.IntervalHrs * (Value + Deriv);
      End
     ELSE   {Plain Euler integration}
         Reg := Reg + ActiveCircuit[ActorID].Solution.IntervalHrs * Value;

     Deriv := Value;

end;

procedure TSystemMeter.OpenDemandIntervalFile(ActorID:integer);
var
  F_header  : string;
  mtr       : TEnergyMeterObj;
begin
  Try
      IF This_Meter_DIFileIsOpen Then SDI_MHandle[ActorID].Free;
      This_Meter_DIFileIsOpen	:=	TRUE;
      if SDI_MHandle[ActorID] <> nil then SDI_MHandle[ActorID].free;
      SDI_MHandle[ActorID]  :=  Create_Meter_Space('"Hour", ');
      WriteintoMemStr(SDI_MHandle[ActorID], 'kWh, kvarh, "Peak kW", "peak kVA", "Losses kWh", "Losses kvarh", "Peak Losses kW"' + Char(10));

  Except
      On E:Exception Do DosimpleMsg('Error opening demand interval file "DI_SystemMeter.CSV"  for writing.'+CRLF+E.Message, 541);
  End;


end;

procedure TSystemMeter.Reset;
begin
    Clear;
   // removed - open in solution If EnergyMeterClass.SaveDemandInterval Then OpenDemandIntervalFile;
end;

procedure TSystemMeter.Save(ActorID: integer);
Var  F:Textfile;
     CSVName, Folder:String;
     mtr    :   TEnergyMeterObj;
begin
 Try
       CSVName := 'SystemMeter_' + inttostr(ActorID) + '.CSV';
       {If we are doing a simulation and saving interval data, create this in the
        same directory as the demand interval data}
       If  energyMeterClass[ActorID].SaveDemandInterval[ActorID] Then
          Folder := energyMeterClass[ActorID].DI_DIR + '\'
       Else
          Folder := GetOutputDirectory;
       GlobalResult := CSVName;
       SetLastResultFile(CSVName);

  Except
      On E: Exception DO
      Begin
       DoSimpleMsg('Error opening System Meter File "' + CRLF + CSVName + '": ' + E.Message, 542);
       Exit;
      End
  End;

 Try
      if SM_MHandle[ActorID] <> nil then SM_MHandle[ActorID].free;
      SM_MHandle[ActorID]  :=  Create_Meter_Space('Year, ');
      WriteintoMemStr(SM_MHandle[ActorID], 'kWh, kvarh, "Peak kW", "peak kVA", "Losses kWh", "Losses kvarh", "Peak Losses kW"' + Char(10));
      WriteintoMemStr(SM_MHandle[ActorID], inttostr(ActiveCircuit[ActorID].Solution.Year));
      WriteRegisters(F,ActorID);
      WriteintoMemStr(SM_MHandle[ActorID], Char(10));

 Finally
      CloseMHandler(SM_MHandle[ActorID], Folder + CSVName, SM_Append[ActorID]);
      SM_MHandle[ActorID]  :=  nil;
 End;
end;

procedure TSystemMeter.TakeSample(ActorID:Integer);

begin

  {Get total system energy out of the sources}

  cPower := CmulReal(GetTotalPowerFromSources(ActorID), 0.001);  // convert to kW

  Integrate(kWh, cPower.re, dkwh, ActorID);
  Integrate(kvarh, cPower.im, dkvarh, ActorID);
  
	{$IFNDEF FPC} // TODO TEMc
  PeakkW := Max(cPower.re, PeakkW);
  Peakkva := Max(Cabs(cPower), Peakkva);
	{$ELSE}
	PeakkW := 0.0;
	PeakkVA := 0.0;
	{$ENDIF}

  {Get total circuit losses}
   cLosses := ActiveCircuit[ActorID].Losses[ActorID];  // PD Elements except shunts
   cLosses := CmulReal(cLosses, 0.001);  // convert to kW

   Integrate(Losseskwh, cLosses.re, dLosseskwh, ActorID);
   Integrate(Losseskvarh, cLosses.im, dLosseskvarh, ActorID);
   
	{$IFNDEF FPC} // TODO TEMc
  PeakLosseskW := Max(cLosses.re, PeakLosseskW);
	{$ELSE}
	PeakLosseskW := 0.0;
	{$ENDIF}

  FirstSampleAfterReset := FALSE;
  IF This_Meter_DIFileIsOpen then WriteDemandIntervalData(ActorID);

end;

procedure TEnergyMeter.CreateMeterTotals(ActorID:integer);
Var 
    i:Integer;
    mtr:TEnergyMeterObj;
begin
    if EMT_MHandle[ActorID] <> nil then EMT_MHandle[ActorID].free;
    EMT_MHandle[ActorID] :=  Create_Meter_Space('Name');
    mtr := ActiveCircuit[ActorID].EnergyMeters.First;
    if Assigned(mtr) then
      For i := 1 to NumEMRegisters Do WriteintoMemStr(EMT_MHandle[ActorID],', "' + mtr.RegisterNames[i] + '"');
    WriteintoMemStr(EMT_MHandle[ActorID], Char(10));
end;

procedure TSystemMeter.WriteDemandIntervalData(ActorID:integer);
var
  mtr   :   TEnergyMeterObj;
begin
   With ActiveCircuit[ActorID].Solution Do WriteintoMem(SDI_MHandle[ActorID],DynaVars.dblHour);
   WriteintoMem(SDI_MHandle[ActorID],cPower.re);
   WriteintoMem(SDI_MHandle[ActorID],cPower.im);
   WriteintoMem(SDI_MHandle[ActorID],peakkW);
   WriteintoMem(SDI_MHandle[ActorID],peakkVA);
   WriteintoMem(SDI_MHandle[ActorID],cLosses.re);
   WriteintoMem(SDI_MHandle[ActorID],cLosses.im);
   WriteintoMem(SDI_MHandle[ActorID],PeakLosseskW);
   WriteintoMemStr(SDI_MHandle[ActorID],Char(10));

end;

procedure TSystemMeter.WriteRegisterNames(var F:TextFile);
begin
// Does nothing
end;

procedure TSystemMeter.WriteRegisters(var F: TextFile; ActorID: Integer);
var
  mtr   :   TEnergyMeterObj;
begin
     WriteintoMem(SM_MHandle[ActorID], kWh);
     WriteintoMem(SM_MHandle[ActorID], kvarh);
     WriteintoMem(SM_MHandle[ActorID], peakkW);
     WriteintoMem(SM_MHandle[ActorID], peakkVA);
     WriteintoMem(SM_MHandle[ActorID], Losseskwh);
     WriteintoMem(SM_MHandle[ActorID], Losseskvarh);
     WriteintoMem(SM_MHandle[ActorID], PeakLosseskW);

end;

procedure TEnergyMeter.Set_DI_Verbose(ActorID: integer;const Value: Boolean);
begin
  FDI_Verbose := Value;
  ResetAll(ActorID);
end;

function TEnergyMeter.Get_DI_Verbose(ActorID: integer): Boolean;
begin
  Result:=FDI_Verbose;
end;

procedure TEnergyMeter.WriteTotalsFile(ActorID: integer);
Var
   mtr:TEnergyMeterObj;
   Regsum:TRegisterArray;
   i :Integer;
   F:Textfile;

begin
  {Sum up all registers of all meters and write to Totals.CSV}
  For i := 1 to NumEMRegisters Do RegSum[i] := 0.0;

  mtr := ActiveCircuit[ActorID].EnergyMeters.First;
  WHILE mtr<>NIL DO Begin
      IF mtr.enabled Then With Mtr Do
         For i := 1 to NumEMRegisters Do
            Regsum[i] := Regsum[i] + Registers[i] * TotalsMask[i] ;

      mtr := ActiveCircuit[ActorID].EnergyMeters.Next;
  End;

  Try     // Writes the file
        if FM_MHandle[ActorID] <> nil then FM_MHandle[ActorID].free;
        FM_MHandle[ActorID]  :=  Create_Meter_Space('Year');
        mtr := ActiveCircuit[ActorID].EnergyMeters.First;
        if assigned(mtr) then
           For i := 1 to NumEMRegisters Do WriteintoMemStr(FM_MHandle[ActorID], ', "' + mtr.RegisterNames[i] + '"'); //Write(F,', "', mtr.RegisterNames[i],'"');
        WriteintoMemStr(FM_MHandle[ActorID], Char(10));

        WriteintoMemStr(FM_MHandle[ActorID], inttostr(ActiveCircuit[ActorID].Solution.Year));
        For i := 1 to NumEMRegisters Do WriteintoMem(FM_MHandle[ActorID],Double(RegSum[i]));
        WriteintoMemStr(FM_MHandle[ActorID], Char(10));
        CloseMHandler(FM_MHandle[ActorID], DI_Dir + '\Totals_' + inttostr(ActorID) + '.CSV', FM_Append[ActorID]);
        FM_MHandle[ActorID]  := nil;

  Except
      On E:Exception Do DosimpleMsg('Error writing demand interval file Totals_' + inttostr(ActorID) + '.CSV.'+CRLF+E.Message, 543);
  End;
  
end;

procedure TEnergyMeter.WriteVoltageReport(ActorID:integer);
var
  i, j       :Integer;
  Vmagpu     :Double;
  UnderCount :Integer;
  OverCount  :integer;
  OverVmax   :Double;
  UnderVmin  :Double;
  MinBus     :Integer;
  MaxBus     :Integer;
  BusCounted :Boolean;
  mtr        :TEnergyMeterObj;

begin
     {For any bus with a defined voltage base, test for > Vmax or < Vmin}

     OverCount  := 0;
     UnderCount := 0;
     MinBus := 0;
     MaxBus := 0;

     With ActiveCircuit[ActorID] Do Begin
       OverVmax   := NormalMinVolts;
       UnderVmin  := NormalMaxVolts;
       For i := 1 to NumBuses do
       With Buses^[i] Do
       Begin
           BusCounted := FALSE;
           If kVBase > 1.0 Then          // Primary Nodes first
           Begin
               For j := 1 to NumNodesThisBus Do
               Begin
                  Vmagpu := Cabs(Solution.NodeV^[GetRef(j)])/kvbase * 0.001;
                  If Vmagpu > 0.1 then Begin // ignore neutral buses
                     If Vmagpu < underVmin Then
                     Begin
                        UnderVmin := Vmagpu;
                        MinBus := i;
                     End;

                     If Vmagpu > OverVMax Then
                     Begin
                        OverVMax := Vmagpu;
                        MaxBus := i;
                     End;

                     If (Vmagpu < NormalMinVolts) Then Begin
                         If Not BusCounted Then Begin     // Don't count more than once
                             Inc(UnderCount);
                             BusCounted := TRUE;
                         End;
                     End Else if (Vmagpu > NormalMaxVolts) then Begin
                         If Not BusCounted Then Begin
                             Inc(OverCount);
                             BusCounted := TRUE;
                         End;
                     End;
                  End;
               End;
           End;
       End; {For i}

       With Solution Do WriteintoMem(VR_MHandle[ActorID],DynaVars.dblHour);
       WriteintoMemStr(VR_MHandle[ActorID],', ' + inttostr(UnderCount));
       WriteintoMem(VR_MHandle[ActorID], UnderVmin);
       WriteintoMemStr(VR_MHandle[ActorID],', ' + inttostr(OverCount));
       WriteintoMem(VR_MHandle[ActorID], OverVmax);
       WriteintoMemStr(VR_MHandle[ActorID],', ' + BusList.Get(minbus));
       WriteintoMemStr(VR_MHandle[ActorID],', ' + Buslist.Get(maxbus));

     // Klugy but it works
     // now repeat for buses under 1 kV
       OverCount  := 0;
       UnderCount := 0;
       MinBus := 0;
       MaxBus := 0;

       OverVmax   := NormalMinVolts;
       UnderVmin  := NormalMaxVolts;
       For i := 1 to NumBuses do
       With Buses^[i] Do
       Begin
           BusCounted := FALSE;
           If (kVBase > 0.0) and (kVBase <= 1.0) Then
           Begin
               For j := 1 to NumNodesThisBus Do
               Begin
                  Vmagpu := Cabs(Solution.NodeV^[GetRef(j)])/kvbase * 0.001;
                  If Vmagpu > 0.1 then Begin // ignore neutral buses
                     If Vmagpu < underVmin Then
                     Begin
                        UnderVmin := Vmagpu;
                        MinBus := i;
                     End;

                     If Vmagpu > OverVMax Then
                     Begin
                        OverVMax := Vmagpu;
                        MaxBus := i;
                     End;

                     If (Vmagpu < NormalMinVolts) Then Begin
                         If Not BusCounted Then Begin     // Don't count more than once
                             Inc(UnderCount);
                             BusCounted := TRUE;
                         End;
                     End Else if (Vmagpu > NormalMaxVolts) then Begin
                         If Not BusCounted Then Begin
                             Inc(OverCount);
                             BusCounted := TRUE;
                         End;
                     End;
                  End;
               End;
           End;
       End; {For i}

       WriteintoMemStr(VR_MHandle[ActorID],', ' + inttostr(UnderCount));
       WriteintoMem(VR_MHandle[ActorID], UnderVmin);
       WriteintoMemStr(VR_MHandle[ActorID],', ' + inttostr(OverCount));
       WriteintoMem(VR_MHandle[ActorID], OverVmax);
       WriteintoMemStr(VR_MHandle[ActorID],', ' + BusList.Get(minbus));
       WriteintoMemStr(VR_MHandle[ActorID],', ' + Buslist.Get(maxbus));
       WriteintoMemStr(VR_MHandle[ActorID],Char(10));
    End;

end;

procedure TEnergyMeter.InterpretRegisterMaskArray(Var Mask: TRegisterArray; ActorID : Integer);

Var i,n:integer;
begin
     n := Parser[ActorID].ParseAsVector(NumEMRegisters, @Mask);
     For i := n+1 to NumEMRegisters Do Mask[i] := 1.0;  // Set the rest to 1
end;

(* Feeder object code commented out
procedure TEnergyMeterObj.MakeFeederObj;
begin
  If Assigned(MeteredElement) Then Begin

    FeederClass.NewObject(Name);  // NewObject creates only if not existent. Else Inits  and desynchs
    FeederObj := ActiveCircuit[ActiveActor].ActiveCktElement as TFeederObj;
    FeederObj.SetBus (1, MeteredElement.GetBus(MeteredTerminal));
    FeederObj.Nphases := MeteredElement.NPhases;
    FeederObj.Nconds  := MeteredElement.Nconds;
    FeederObj.Enabled := ActiveCircuit[ActiveActor].RadialSolution;

  End
  Else DoSimpleMsg('Error: Attempted to make Feeder Obj without instantiating Metered Element in Energymeter.'+name,544);
end;
*)
(*  Feeder object code commented out
procedure TEnergyMeterObj.RemoveFeederObj;
begin

    If Assigned(FeederObj) Then Begin
       FeederObj.Enabled := FALSE;
       FeederObj.SetCktElementFeederFlags (FALSE);
    End;

end;
*)

procedure TEnergyMeterObj.EnableFeeder;
// HasFeeder has to be true before feederObj will be re-enabled.
begin
(*  Feeder object code commented out  HasFeeder can never be true
    If HasFeeder Then Begin
        If Not Assigned(FeederObj) Then MakeFeederObj
        Else FeederObj.Enabled := TRUE;
        FeederObj.SetCktElementFeederFlags (TRUE);
    End;
*)
end;

procedure TEnergyMeter.OpenAllDIFiles(ActorID:integer);
{Similar to Append, by creates the files.}

VAR
   mtr:TEnergyMeterObj;
  // Filenm:String;
begin

      If FSaveDemandInterval Then  Begin

          ClearDI_Totals;  // clears accumulator arrays

          mtr := ActiveCircuit[ActorID].EnergyMeters.First;
          WHILE mtr<>NIL DO Begin
              IF mtr.enabled Then mtr.OpenDemandIntervalFile(ActorID);
              mtr := ActiveCircuit[ActorID].EnergyMeters.Next;
          End;

          SystemMeter.OpenDemandIntervalFile(ActorID);

          {Optional Exception Reporting}
          if Do_OverloadReport         then OpenOverloadReportFile(ActorID);
          If Do_VoltageExceptionReport then OpenVoltageReportFile(ActorID);

          {Open FDI_Totals}
          Try
             CreateFDI_Totals(ActorID);

          Except
              On E:Exception Do DosimpleMsg('Error creating the memory space for demand interval "'+Name+'.CSV' +' for appending.'+CRLF+E.Message, 538);
          End;

          DIFilesAreOpen[ActorID] := TRUE;

      End;{IF}


end;

procedure TEnergyMeter.OpenOverloadReportFile(ActorID: integer);
begin
  Try
      IF OverloadFileIsOpen Then OV_MHandle[ActorID].Free;
      OverloadFileIsOpen := TRUE;
      if OV_MHandle[ActorID] <> nil then OV_MHandle[ActorID].free;
      OV_MHandle[ActorID]  :=  Create_Meter_Space('"Hour", "Element", "Normal Amps", "Emerg Amps", "% Normal", "% Emerg", "kVBase"' + Char(10));
  Except
      On E:Exception Do DosimpleMsg('Error creating memory space (Overload report) for writing.'+CRLF+E.Message, 541);
  End;

end;

procedure TEnergyMeter.OpenVoltageReportFile(ActorID: integer);
begin
  Try
      IF VoltageFileIsOpen Then VR_MHandle[ActorID].Free;
      VoltageFileIsOpen := TRUE;
      if VR_MHandle[ActorID] <> nil then VR_MHandle[ActorID].free;
      VR_MHandle[ActorID]  :=  Create_Meter_Space('"Hour", "Undervoltages", "Min Voltage", "Overvoltage", "Max Voltage", "Min Bus", "Max Bus"');
      WriteintoMemStr(VR_MHandle[ActorID],', "LV Undervoltages", "Min LV Voltage", "LV Overvoltage", "Max LV Voltage", "Min LV Bus", "Max LV Bus"' + Char(10));
  Except
      On E:Exception Do DosimpleMsg('Error creating memory space (Voltage report) for writing.'+CRLF+E.Message, 541);
  End;

end;

initialization

  {RegisterNameList := TCommandList.Create(['kWh', 'kvarh', 'Max kW', 'Max kVA', 'Zone kWh',
  'Zone kvarh', 'Zone Max kW','Zone Max kVA','Overload kWh Normal','Overload kWh Emerg','Load EEN',
  'Load UE', 'Zone Losses kWh', 'Zone Losses kvarh', 'Zone Max kW Losses', 'Zone Max kvar Losses',
  'Gen kWh', 'Gen kvarh', 'Gen Max kW', 'Gen Max kVA']); }


Finalization

end.

