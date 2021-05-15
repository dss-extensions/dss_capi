unit fMonitor;

{
  ----------------------------------------------------------
  Copyright (c) 2008-2019, Electric Power Research Institute, Inc.
  All rights reserved.
  ----------------------------------------------------------
}

{
   Change Log
   By Ying @UCF 10/27/2017

}

interface

USES
     Command, MeterClass, Meterelement, DSSClass, Arraydef, ucomplex,
     utilities, Classes, DSSPointerList,
     VLNodeVars,LD_fm_infos;
TYPE
    TFMonitorStrBuffer = Array[1..256] of AnsiChar;
// = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = =
   {This has to be named TDSSMonitor because Delphi has a TMonitor Class and the compiler will get confused}
   TDSSFMonitor = class(TMeterClass)
     private

     protected
        Procedure DefineProperties;
        Function  MakeLike(const MonitorName:String):Integer;  Override;
     public


       constructor Create;
       destructor  Destroy; override;

       Function Edit:Integer;                 override;     // uses global parser
       Function NewObject(const ObjName:String):Integer;  override;

       Procedure ResetAll;   Override;
       Procedure SampleAll;  Override;  // Force all monitors to take a sample
       //Procedure SampleAllMode5;  // Sample just Mode 5 monitors
       Procedure SaveAll;    Override;   // Force all monitors to save their buffers to disk
       {update FM leader information}
       Procedure  update_sys_ld_info; //all FMs

   end;

// = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = =
   TFMonitorObj = class(TMeterElement)
     private
       //pTVLeaderVars : TDSSPointerList;   //save all nodes information
       //VL_node : TVLnodeVars;//  leader node of this cluster
       nodes : integer; //nodes of this cluster  \\default nodes := 33;
       pCommMatrix : pSmallIntArray;// communication matrix of this cluster
       P_trans_ref : double; // Power Ref on metered elemet, if mode =1 real power of this cluster will be used
       p_mode : integer;
       tempCplx : complex;

       BufferSize      :Integer;
       Hour            :Integer;
       Sec             :Double;    // last time entered in the buffer
       MonBuffer       :pSingleArray;
       Bufptr          :Integer;  // point to present (last) element in buffer must be incremented to add

       CurrentBuffer   :pComplexArray;
       VoltageBuffer   :pComplexArray;

       NumStateVars    :Integer;
       StateBuffer     :pDoubleArray;

       FlickerBuffer   :pComplexArray; // store phase voltages in polar form
                                       // then convert to re=flicker level, update every time step
                                       //             and im=Pst, update every 10 minutes
       SolutionBuffer  :pDoubleArray;

       IncludeResidual :Boolean;
       VIpolar         :Boolean;
       Ppolar          :Boolean;

       FileSignature   :Integer;
       FileVersion     :Integer;

       BaseFrequency   :Double;
       {-------------}
       F_Value_one   : double;//test;
       //Voltages
       F_Value_one_V    : pDoubleArray; //Measured voltage by each FMonitor(p.u.)
       F_Value_one_S  : pComplexArray; //Measured apparent power for each phase by each Fmonitor
       Fvalue_P : Double;  //This variable is used to store total measure three-phase active power of any Fmonitor
       Fvalue_Q : Double;  //This variable is used to store total measure three-phase reactive power of any Fmonitor

       F_P_one, F_Q_one :double;//measured power
       P_ref_one :double; //the ref Power for this point

       Node_num : Integer;  // Node number within the cluster
       V_Sensor : Integer;  // Voltage sensor enable variable
       P_Sensor : Integer;   // Power sensor enable variable
       Cluster_num : integer;  // the group number for this
       Total_Clusters : Integer; //Total Number of the Groups in a circuit

       //communication time
       T_intvl_smpl : double; //Sampling interval.
       MaxLocalMem : integer; //Max number of local memory, no large than 99
       Smpl_stps : integer; // T_Comm/ ActiveCircuit.Solution.Dynavars.h.
       pCommDelayMatrix : pDoubleArray;  //
       pCommDelaySteps : pSmallIntArray;// Communication delay step matrix of this cluster

       //delay to uppper level
       up_dly : double;    //in seconds
       nup_dlys : integer; //nup_dlys := up_dly / t_intvl_smpl;
       virtual_Ld_Nd : integer;// denotes which node talks to upper level
                               // default by 1;

       Bus_code  :Integer;
       NodeNum : Integer;
       Node_Ref : Integer;
       {------------}
       BufferFile      :String;  // Name of file for catching buffer overflow

       IsFileOpen      :Boolean;
       ValidMonitor    :Boolean;
       IsProcessed     :Boolean;

       //Procedure AddDblsToBuffer(Dbl:pDoubleArray; Ndoubles:Integer);
       //Procedure AddDblToBuffer(const Dbl:Double);

       //Procedure DoFlickerCalculations;  // call from CloseMonitorStream
       Procedure Set_nodes_for_fm(intNodes : integer);//initiate the structure of this FMon
       Procedure Set_CommVector( strParam: string);
       Procedure Set_volt_lmt_clstr( strParam: string);

       Procedure Set_CommDelayVector( strParam: string);
       Procedure ResetDelaySteps(iNodeNum: integer);

       Procedure Set_ElemTable_line( strParam: string);
       Procedure Init_nodeFM(iNodeNum : integer);
       //Procedure push_voltage;
       Procedure  Get_PDElem_terminal_voltage(nd_num_in_cluster: integer ;devName: string; Tern_num: integer); //
       procedure  Calc_Alpha_for_PDNode(NodeNum :Integer);
       Procedure  update_all_nodes_info;
       Function   AvgPmax : double;
       Function   AvgQmax : double;
       Procedure  Get_PQ_DI(i_NodeNum : integer);
       Function  Calc_Grdt_for_Alpha(NodeNuminClstr, phase_num,ActorID:Integer):Double;
       Function  Calc_Grdt_for_Alpha_vivj(NodeNuminClstr, phase_num,ActorID : Integer):Double;

       Function  Calc_GP_AlphaP(NodeNuminClstr, phase_num, ActorID:Integer):Double;
       Function  Get_power_trans :Double;
      public
       pNodeFMs : pNodeArray;

       Mode          :Integer;
       //MonitorStream :TMemoryStream;
       SampleCount   :Integer;  // This is the number of samples taken
       {-- overview information about this cluster--}
       ld_fm_info : array [0..3] of TLD_fm_infos;

       constructor Create(ParClass:TDSSClass; const MonitorName:String);
       destructor Destroy; override;

       PROCEDURE MakePosSequence;    Override;  // Make a positive Sequence Model, reset nphases
       Procedure RecalcElementData;  Override;
       Procedure CalcYPrim;          Override;    // Always Zero for a monitor
       //Procedure TakeSample;         Override; // Go add a sample to the buffer
       {}
       //Procedure GetFvalue;
       //Function PhaseDetect(Bus_name:string):Integer;
       {}
       Procedure ResetIt;
       Procedure Save;     // Saves present buffer to file
       //Procedure PostProcess; // calculates Pst or other post-processing
       //

       Function Calc_sum_dij_Alphaj(NodeNumofDG, phase_num,ActorID:Integer):Double;
       //unified voltage (1-V_dg)^2
       Function Calc_Alpha_M2(NodeNumofDG, phase_num:Integer;  dbNodeRef: integer; Bii,beta,Volt_Trhd: double):Double;
       //minimize loss
       Function Calc_Alpha_L(NodeNumofDG, phase_num:Integer;  dbNodeRef: integer; Bii,beta,Volt_Trhd: double):Double;
       Function Calc_Alpha_L_vivj(NodeNumofDG, phase_num:Integer;  dbNodeRef: integer; Bii,beta,Volt_Trhd: double):Double;
       Function Calc_Alpha_LnM2(NodeNumofDG, phase_num:Integer;  dbNodeRef: integer; Bii,beta,Volt_Trhd: double):Double;

       Function Calc_AlphaP(NodeNuminClstr, phase_num, ActorID:Integer):Double;
       Function Get_P_mode( ActorID : Integer):integer;

       //Zero seq.
       Function Calc_fm_ul_0(NodeNumofDG, phase_num:Integer; dbNodeRef: integer; Bii,beta,Volt_Trhd: double): double;
       Function Calc_fm_us_0(NodeNumofDG, phase_num:Integer; dbNodeRef: integer; Bii,beta,Volt_Trhd: double): double;
       Procedure Agnt_smpl(NodeNumofDG, phase_num,ActorID:Integer); // sample data of this node at each  t_intvl_smpl
       Procedure Init_delay_array(NodeNumofDG, ActorID:Integer);
       {For real power control-dynamic simu}
       Function Calc_ul_P(NodeNuminClstr, phase_num:Integer):Double;
       Function Calc_Gradient_ct_P(NodeNuminClstr, phase_num, ActorID:Integer):Double;  // curtailment
       {--}
       Procedure  update_ld_info( ActorID: integer); //all nodes in the cluster
       Procedure  update_ld_dly( ActorID: integer); // all nodes in this cluster with delay
       //       Procedure TranslateToCSV(Show:Boolean);

       Procedure GetCurrents(Curr: pComplexArray);                Override; // Get present value of terminal Curr
       Procedure GetInjCurrents(Curr: pComplexArray);             Override;   // Returns Injextion currents
       PROCEDURE InitPropertyValues(ArrayOffset:Integer);         Override;
       Procedure DumpProperties(F: TFileStream; Complete:Boolean);Override;
       function  Get_FileName: String;

      // Property CSVFileName:String Read Get_FileName;
   end;

// = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = =

VAR
    ActiveFMonitorObj:TFMonitorObj;

{--------------------------------------------------------------------------}
implementation

USES

    ParserDel, DSSClassDefs, DSSGlobals, Circuit, CktTree, CktElement,Transformer, PCElement, PDElement,
    Sysutils, ucmatrix, showresults, mathUtil , TOPExport, Dynamics, PstCalc,
    {}  Terminal,   Generic5OrderMach,   strutils,
    Capacitor,Load;

CONST
    SEQUENCEMASK = 17;
    MAGNITUDEMASK = 32;
    POSSEQONLYMASK = 64;
    MODEMASK = 15;

    NumPropsThisClass = 24;//22;//21;//20;//17; //12;// 9; //8;//7;//add P_ref_one
    NumSolutionVars = 12;

VAR
    StrBuffer:TFMonitorStrBuffer;

{--------------------------------------------------------------------------}
constructor TDSSFMonitor.Create;  // Creates superstructure for all Monitor objects

Begin
     Inherited Create;

     Class_name   := 'FMonitor';
     DSSClassType := DSSClassType + FMON_ELEMENT;

     DefineProperties;

     CommandList := TCommandList.Create(Slice(PropertyName^, NumProperties));
     CommandList.Abbrev := TRUE;
End;

{--------------------------------------------------------------------------}
destructor TDSSFMonitor.Destroy;

Begin
     Inherited Destroy;
End;

//- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Procedure TDSSFMonitor.DefineProperties;
Begin

     Numproperties := NumPropsThisClass;
     CountProperties;   // Get inherited property count
     AllocatePropertyArrays;

     // Define Property names

     PropertyName[1] := 'element';
     PropertyName[2] := 'terminal';
     PropertyName[3] := 'mode';
     PropertyName[4] := 'action';  // buffer=clear|save
     PropertyName[5] := 'residual';  // buffer=clear|save
     PropertyName[6] := 'VIPolar';  // V I in mag and angle rather then re and im
     PropertyName[7] := 'PPolar';  // Power in power PF rather then power and vars
     PropertyName[8] := 'P_trans_ref';
     PropertyName[9] := 'V_Sensor';
     PropertyName[10] := 'P_Sensor';
     PropertyName[11] := 'Node_num';
     PropertyName[12] := 'Cluster_num';
     PropertyName[13] := 'Total_Clusters';
     PropertyName[14] := 'Nodes';
     PropertyName[15] := 'CommVector';
     PropertyName[16] := 'ElemTableLine';
     PropertyName[17] := 'P_Mode';  //real power control mode
     PropertyName[18] := 'CommDelayVector';
     PropertyName[19] := 'T_intvl_smpl';  //real power control mode
     PropertyName[20] := 'MaxLocalMem';
     PropertyName[21] := 'Volt_limits_pu';// set limits for this cluster {0,1.05, 0.95}
     PropertyName[22] := 'b_Curt_Ctrl';// set P curtailment on/off
     PropertyName[23] := 'up_dly';// delay time to communicate to upper level
     PropertyName[24] := 'virtual_ld_node';// delay time to communicate to upper level

     PropertyHelp[1] := 'Name (Full Object name) of element to which the monitor is connected.';
     PropertyHelp[2] := 'Number of the terminal of the circuit element to which the monitor is connected. '+
                    '1 or 2, typically. For monitoring states, attach monitor to terminal 1.';
     PropertyHelp[3] := 'Bitmask integer designating the values the monitor is to capture: '+CRLF+
                    '0 = Voltages and currents' + CRLF+
                    '1 = Powers'+CRLF+
                    '2 = Tap Position (Transformers only)'+CRLF+
                    '3 = State Variables (PCElements only)' +CRLF+
                    '4 = Flicker level and severity index (Pst) for voltages. No adders apply.' +CRLF+
                    '    Flicker level at simulation time step, Pst at 10-minute time step.' +CRLF+
                    '5 = Solution variables (Iterations, etc).' +CRLF+CRLF+
                    'Normally, these would be actual phasor quantities from solution.' + CRLF+
                    '6 = Capacitor Switching (Capacitors only)'+CRLF+
                    'Combine with adders below to achieve other results for terminal quantities:' + CRLF+
                    '+16 = Sequence quantities' + CRLF+
                    '+32 = Magnitude only' + CRLF+
                    '+64 = Positive sequence only or avg of all phases' + CRLF+
                     CRLF +
                    'Mix adder to obtain desired results. For example:' + CRLF+
                    'Mode=112 will save positive sequence voltage and current magnitudes only' + CRLF+
                    'Mode=48 will save all sequence voltages and currents, but magnitude only.';
     PropertyHelp[4] := '{Clear | Save | Take | Process}' + CRLF +
                        '(C)lears or (S)aves current buffer.' + CRLF +
                        '(T)ake action takes a sample.'+ CRLF +
                        '(P)rocesses the data taken so far (e.g. Pst for mode 4).' + CRLF + CRLF +
                        'Note that monitors are automatically reset (cleared) when the Set Mode= command is issued. '+
                        'Otherwise, the user must explicitly reset all monitors (reset monitors command) or individual ' +
                        'monitors with the Clear action.';
     PropertyHelp[5] := '{Yes/True | No/False} Default = No.  Include Residual cbannel (sum of all phases) for voltage and current. ' +
                        'Does not apply to sequence quantity modes or power modes.';
     PropertyHelp[6] := '{Yes/True | No/False} Default = YES. Report voltage and current in polar form (Mag/Angle). (default)  Otherwise, it will be real and imaginary.';
     PropertyHelp[7] := '{Yes/True | No/False} Default = YES. Report power in Apparent power, S, in polar form (Mag/Angle).(default)  Otherwise, is P and Q';
     PropertyHelp[8] := 'P_trans_ref: P ref value for metered element(unit kW)';
     PropertyHelp[9] := 'V_Sensor'+ CRLF+
                         'Enable voltage sensor';
     PropertyHelp[10] := 'P_Sensor'+ CRLF+
                         'Enable power sensor';
     PropertyHelp[11] := 'Node_num' + CRLF+
                          'Assign a node number within a cluster';
     PropertyHelp[12] := 'Cluster_num';
     PropertyHelp[13] := 'Total_Clusters.' + CRLF+
                          'Define the total number of groups in a circuit' + CRLF+
                          'Just use for the first defined FMonitor';
     PropertyHelp[14] := 'Nodes connected to this FMonitor. Example:(Nodes=33)';
     PropertyHelp[15] := 'CommVector of this FMonitor. ' + CRLF+
                          'The first entry of this vector is the number of '+ CRLF+
                          'Example:(CommVector={2,1,1,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0})'+ CRLF+
                          'The example show node #2 can communicate to node #1,#2,#3';
     PropertyHelp[16] := 'ElemTableLine of the each node within this cluster. ' + CRLF+
                         'The first entry of this vector is the number of node within cluster '+ CRLF+
                         'The second entry of this vector is element name '+ CRLF+
                         'The third entry of this vector is terminal number '+ CRLF+
                         'The fourth entry of this vector is voltage sensor '+ CRLF+
                         'Example:(ElemTable={2,Line.1,1,1})'+ CRLF+
                         'The example show node #2 Element';
     PropertyHelp[17] :=  '0 = real Power controlled by each p_ref on each DG' + CRLF+
                          '1 = real Power on MeteredElem controlled by DGs according to P_trans_ref'+CRLF+
                          '2 = Not defined'+CRLF+
                          '3 = Not defined'  ;

     PropertyHelp[18] :=  'CommDelayVector of this FMonitor. ' + CRLF+
                          'The first entry of this vector is the number of the node.'+ CRLF+
                          'Example:(CommVector={2,t1,0,t2,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0})'+ CRLF+
                          'The example show node #2 can communicate to node #1 and #3 with time delay t1 and t2 seperately';
     PropertyHelp[19] :=  'T_intvl_smpl: '+ CRLF+
                          'The imformation of each agent will be sampled at each T_comm time. Unit is second.'
                          + CRLF+'T_intvl_smpl is also the minimal communication time between neighbor nodes.'
                          + CRLF+'If T_intvl_smpl=0.0, no delay for the communication is enabled in the simulation.';
     PropertyHelp[20] := 'MaxLocalMem: the max number of local memory siza. No larger than 99';
     PropertyHelp[21] := 'Volt_limits_pu: exmaple "Volt_limits_pu={a0,a1, a2}"'
                          + CRLF+'a0: the phase number, 0 means pos. seq; a1: upper voltage limit of this cluster, usually 1.05;'
                          + CRLF+'a2: upper voltage limit of this cluster, usually 0.95';// set limits for this cluster {0,1.05, 0.95}
     PropertyHelp[22] := 'b_Curt_Ctrl:set P curtailment on/off;'// set P curtailment on/off
                          + CRLF+ 'b_Curt_Ctrl=True: P curtailment will be implemented according to the system voltage (default);'
                          + CRLF+ 'b_Curt_Ctrl=False: P curtailment will not be implemented.';
     PropertyHelp[23] := 'up_dly: delay time to upper level. For example: "up_dly := 0.05"'
                         + CRLF+ 'It can be used to simulate the time delay between clusters';
     PropertyName[24] := 'virtual_ld_node: which node talks to upper level. virtual_ld_node=1';
     ActiveProperty := NumPropsThisClass;
     inherited DefineProperties;  // Add defs of inherited properties to bottom of list

End;

{--------------------------------------------------------------------------}
Function TDSSFMonitor.NewObject(const ObjName:String):Integer;
Begin
    // Make a new Monitor and add it to Monitor class list
    With ActiveCircuit Do
    Begin
      ActiveCktElement := TFMonitorObj.Create(Self, ObjName);
      Result := AddObjectToList(ActiveDSSObject);
    End;
End;
{--------------------------------------------------------------------------}
Function TDSSFMonitor.Edit:Integer;
VAR
   ParamPointer:Integer;
   ParamName:String;
   Param:String;
   recalc: integer;
   i : integer;
Begin

  // continue parsing with contents of Parser
  // continue parsing with contents of Parser
  ActiveFMonitorObj := ElementList.Active;
  ActiveCircuit.ActiveCktElement := ActiveFMonitorObj;

  Result := 0;
  recalc:=0;

  WITH ActiveFMonitorObj DO Begin

     ParamPointer := 0;
     ParamName := Parser.NextParam;
     Param := Parser.StrValue;
     WHILE Length(Param)>0 DO Begin
         IF Length(ParamName) = 0 THEN Inc(ParamPointer)
         ELSE ParamPointer := CommandList.GetCommand(ParamName);

         If (ParamPointer>0) and (ParamPointer<=NumProperties) Then PropertyValue[ParamPointer]:= Param;
         inc (recalc);

         CASE ParamPointer OF
            0: DoSimpleMsg('Unknown parameter "' + ParamName + '" for Object "' + Class_Name +'.'+ Name + '"', 661);
            1: Begin
                 ElementName := ConstructElemName(lowercase(param));   // subtitute @var values if any
                 PropertyValue[1] := ElementName;
               End;
            2: MeteredTerminal := Parser.IntValue;
            3: Mode := Parser.IntValue;
            4: Begin
                  param := lowercase(param);
                  Case param[1] of
                    's':Save;
                    'c','r':ResetIt;
                    't': TakeSample;
                    //'p': begin PostProcess; dec(recalc) end
                  End;
               End;  // buffer
            5: IncludeResidual := InterpretYesNo(Param);
            6: VIpolar := InterpretYesNo(Param);
            7: Ppolar := InterpretYesNo(Param);
            8: p_trans_ref := 1000 * Parser.dblValue;//kW for ref, unit of p_trans_ref is 'W'
            9: V_Sensor := Parser.IntValue;//Voltage Sensor: Binary
            10: P_Sensor := Parser.IntValue;//Power sensor : Binary
            //11: Node_num := Parser.IntValue;//Node number : integer
            12: Cluster_num := Parser.IntValue;//group number: integer
            13: Total_Clusters := Parser.IntValue;//Total number of the groups: integer
            14: Set_nodes_for_fm(Parser.IntValue);//Nodes. Innitiate the structure
            15: Set_CommVector(Param);//
            16: Set_ElemTable_line(Param);//
            17: p_mode :=   Parser.IntValue;
            18: Set_CommDelayVector(Param);//
            19: begin
                  T_intvl_smpl :=  Parser.dblValue; //
                  for i := 1 to nodes do
                          ResetDelaySteps(i);
                end ;
            20: MaxLocalMem := Parser.IntValue;
            21: Set_volt_lmt_clstr(Param);
            22: ld_fm_info[0].b_curt_ctrl := InterpretYesNo(Param); //curtailment
            23: begin
                up_dly := Parser.dblValue;
                if t_intvl_smpl<>0.0 then
                begin
                    if frac (up_dly /t_intvl_smpl)<>0.0 then
                            nUp_dlys := trunc(up_dly /t_intvl_smpl)
                            else  nUp_dlys := trunc(up_dly /t_intvl_smpl)+1;
                end
                    else nUp_dlys :=0;
            end;
            24: virtual_Ld_Nd := Parser.IntValue;
         ELSE
           // Inherited parameters
           ClassEdit( ActiveFMonitorObj, ParamPointer - NumPropsthisClass)
         End;

         ParamName := Parser.NextParam;
         Param := Parser.StrValue;
     End;

     if recalc > 0 then RecalcElementData;
  End;

End;

{--------------------------------------------------------------------------}
Procedure TDSSFMonitor.ResetAll;  // Force all monitors in the circuit to reset

VAR
   Mon:TFMonitorObj;

Begin
      Mon := ActiveCircuit.FMonitors.First;
      WHILE Mon<>Nil DO
      Begin
          If Mon.enabled Then Mon.ResetIt;
          Mon := ActiveCircuit.FMonitors.Next;
      End;

End;

{--------------------------------------------------------------------------}
Procedure TDSSFMonitor.SampleAll;  // Force all monitors in the circuit to take a sample

VAR
   Mon:TFMonitorObj;
// sample all monitors except mode 5 monitors
Begin
      Mon := ActiveCircuit.FMonitors.First;
      WHILE Mon<>Nil DO  Begin
          If Mon.enabled Then
             If Mon.Mode <> 5 then Mon.TakeSample;
          Mon := ActiveCircuit.FMonitors.Next;
      End;
      //ProcessFvalue;
End;
//This function is used to measure total net power of a cluster
Procedure  TDSSFMonitor.update_sys_ld_info; //all FMs
VAR
   FMon:TFMonitorObj;
   vtemp, dv_lwst : double;
// sample all monitors except mode 5 monitors
Begin
     ActiveCircuit.Solution.LD_FM[0].volt_hghst := -999999;
     ActiveCircuit.Solution.LD_FM[0].volt_lwst  := 9999999;
     FMon := ActiveCircuit.FMonitors.First;
      WHILE FMon<>Nil DO  Begin
          //update all agents information:
              //synchronous: voltage to agents
              //asynchronous: aphga, ahphaP, highest/lowest voltage
          If FMon.enabled Then
              //FMon.update_ld_info;
              FMon.update_ld_dly; //with delay
          //
          {Update cluster info to center}
          if ActiveCircuit.Solution.LD_FM[0].volt_hghst < FMon.ld_fm_info[0].volt_hghst then
          begin
               ActiveCircuit.Solution.LD_FM[0].volt_hghst  := FMon.ld_fm_info[0].volt_hghst;
               ActiveCircuit.Solution.LD_FM[0].ndnum_hghst := FMon.ld_fm_info[0].ndnum_hghst;
               ActiveCircuit.Solution.LD_FM[0].clstr_num_hghst := FMon.Cluster_num;
               ActiveCircuit.Solution.LD_FM[0].volt_hgh_lmt := fmon.ld_fm_info[0].volt_hgh_lmt;
               ActiveCircuit.Solution.LD_FM[0].b_ctrl_hghst := FMon.ld_fm_info[0].b_ctrl_hghst;
          end;
          if ActiveCircuit.Solution.LD_FM[0].volt_lwst > FMon.ld_fm_info[0].volt_lwst then
          begin
               ActiveCircuit.Solution.LD_FM[0].volt_lwst  := FMon.ld_fm_info[0].volt_lwst;
               ActiveCircuit.Solution.LD_FM[0].ndnum_lwst := FMon.ld_fm_info[0].ndnum_lwst;
               ActiveCircuit.Solution.LD_FM[0].clstr_num_lwst := FMon.Cluster_num;
               ActiveCircuit.Solution.LD_FM[0].volt_lw_lmt := fmon.ld_fm_info[0].volt_lw_lmt;
               ActiveCircuit.Solution.LD_FM[0].b_ctrl_lwst := FMon.ld_fm_info[0].b_ctrl_lwst;
          end;

          {---- curtailment ----- bCurtl := t/f for overall system ------}
          //curtailment is needed or not
            if FMon.ld_fm_info[0].volt_hghst > 1.0 then
               FMon.ld_fm_info[0].b_ctrl_hghst := true     //need curtailment
               else FMon.ld_fm_info[0].b_ctrl_hghst := false;
          {---- each cluster may have their own ---}
          FMon := ActiveCircuit.FMonitors.Next;
      End;
      //curtailment is needed or not
          vtemp := ( activecircuit.Solution.LD_FM[0].volt_hghst - activecircuit.Solution.LD_FM[0].volt_lwst );//p.u.
          dv_lwst := activecircuit.Solution.LD_FM[0].volt_lwst - ActiveCircuit.Solution.LD_FM[0].volt_lw_lmt;//0.95; // must greater than 0.0
         if  (dv_lwst<0.0) then
          begin
            activecircuit.Solution.bCurtl := true;  //curtailment
          end
          else
          begin
              activecircuit.Solution.bCurtl := false;//dont need curtailment
          end;

End;

{--------------------------------------------------------------------------}
{
Procedure TDSSFMonitor.PostProcessAll;
VAR
   Mon:TFMonitorObj;
Begin
   Mon := ActiveCircuit.FMonitors.First;
   WHILE Mon<>Nil DO Begin
       If Mon.Enabled Then Mon.PostProcess;
       Mon := ActiveCircuit.FMonitors.Next;
   End;
End;
}
{--------------------------------------------------------------------------}
Procedure TDSSFMonitor.SaveAll;     // Force all monitors in the circuit to save their buffers to disk

VAR
   Mon:TFMonitorObj;

Begin
   Mon := ActiveCircuit.FMonitors.First;
   WHILE Mon<>Nil DO Begin
       If Mon.Enabled Then Mon.Save;
       Mon := ActiveCircuit.FMonitors.Next;
   End;
End;

{--------------------------------------------------------------------------}
Function TDSSFMonitor.MakeLike(const MonitorName:String):Integer;
VAR
   OtherMonitor:TFMonitorObj;
   i:Integer;
Begin
   Result := 0;
   {See if we can find this Monitor name in the present collection}
   OtherMonitor := Find(MonitorName);
   IF OtherMonitor<>Nil THEN
   WITH ActiveFMonitorObj DO Begin

       NPhases := OtherMonitor.Fnphases;
       NConds  := OtherMonitor.Fnconds; // Force Reallocation of terminal stuff

       Buffersize := OtherMonitor.Buffersize;
       ElementName:= OtherMonitor.ElementName;
       MeteredElement:= OtherMonitor.MeteredElement;  // Pointer to target circuit element
       MeteredTerminal:= OtherMonitor.MeteredTerminal;
       Mode := OtherMonitor.Mode;
       IncludeResidual := OtherMonitor.IncludeResidual;

       For i := 1 to ParentClass.NumProperties Do PropertyValue[i] := OtherMonitor.PropertyValue[i];

       BaseFrequency:= OtherMonitor.BaseFrequency;

   End
   ELSE  DoSimpleMsg('Error in Monitor MakeLike: "' + MonitorName + '" Not Found.', 662);

End;

{==========================================================================}
{                    TFMonitorObj                                           }
{==========================================================================}



{--------------------------------------------------------------------------}
constructor TFMonitorObj.Create(ParClass:TDSSClass; const MonitorName:String);
var
 i : integer;
Begin
     Inherited Create(ParClass);
     Name := LowerCase(MonitorName);

     Nphases := 3;  // Directly set conds and phases
     Fnconds := 3;
     Nterms  := 1;  // this forces allocation of terminals and conductors
                         // in base class

     pNodeFMs := nil;
     //pNodeFMs := AllocMem(Sizeof(pNodeFMs^[1] )* 1);

     {Current Buffer has to be big enough to hold all terminals}
     CurrentBuffer := Nil;
     VoltageBuffer := Nil;
     StateBuffer   := Nil;
     FlickerBuffer := Nil;
     SolutionBuffer:= Nil;

     Basefrequency := 60.0;
     Hour          := 0;
     Sec           := 0.0;

     Mode := 0;  // Standard Mode: V & I, complex values

     BufferSize := 1024;       // Makes a 4K buffer
     MonBuffer  := AllocMem(Sizeof(MonBuffer^[1]) * BufferSize);
     BufPtr     := 0;

     ElementName    := TDSSCktElement(ActiveCircuit.CktElements.Get(1)).Name; // Default to first circuit element (source)
     MeteredElement := nil;
     Bufferfile     := '';

     //MonitorStream := TMemoryStream.Create; // Create memory stream

     IsFileOpen      := FALSE;
     MeteredTerminal := 1;
     IncludeResidual := FALSE;
     VIPolar         := TRUE;
     Ppolar          := TRUE;
     FileSignature   := 43756;
     FileVersion     := 1;
     SampleCount     := 0;
     IsProcessed     := FALSE;

     DSSObjType := ParClass.DSSClassType; //MON_ELEMENT;

     InitPropertyValues(0);
     Nodes := 33;//default Nodes in one cluster

     ReAllocMem(pCommMatrix,  Nodes *Nodes * sizeof(pCommMatrix^[1]));
     {}
      ReAllocMem(f_Value_one_V, 999 * sizeof(f_Value_one_V^[1]));
      ReAllocMem(F_Value_one_S, 999 * sizeof(F_Value_one_S^[1]));
     {}
      T_intvl_smpl := 0;
      MaxLocalMem := 10;
      ReAllocMem(pCommDelayMatrix,  Nodes *Nodes * sizeof(pCommDelayMatrix^[1]));
      ReAllocMem(pCommDelaySteps,  Nodes *Nodes * sizeof(pCommDelaySteps^[1]));

     {leader information}
     for i := 0 to 3 do
     begin

      ld_fm_info[i].ndnum_hghst := 0;
      ld_fm_info[i].b_ctrl_hghst := false;        //small number that can never be true
      ld_fm_info[i].volt_hghst := -1.0;
      ld_fm_info[i].volt_hgh_lmt := 1.05;
      ld_fm_info[i].Pinjec_hghst := 0.0;
      ld_fm_info[i].ndnum_lwst := 0;
      ld_fm_info[i].b_ctrl_lwst := false;
      ld_fm_info[i].volt_lw_lmt := 0.95;
      ld_fm_info[i].volt_lwst := 9999999999.0;   //large nunber can never be true
      ld_fm_info[i].Pinjec_lwst := 0.0;
      ld_fm_info[i].volt_avg := 0.0;
      ld_fm_info[i].total_pg := 0.0;
      ld_fm_info[i].total_pl := 0.0;
      ld_fm_info[i].b_Curt_Ctrl := true;
     end;
     virtual_Ld_Nd := 1;
     nUp_dlys := 0;
     //bCurtl_Clstr := false;
     {end of leader initialization}
End;

destructor TFMonitorObj.Destroy;
Begin
     //MonitorStream.Free;
     ElementName := '';
     Bufferfile := '';
     ReAllocMem(MonBuffer,0);
     ReAllocMem(StateBuffer,0);
     ReAllocMem(CurrentBuffer,0);
     ReAllocMem(VoltageBuffer,0);
     ReAllocMem(FlickerBuffer,0);
     ReAllocMem(SolutionBuffer,0);
     {}
     if Assigned(f_Value_one_V) then ReallocMem(f_Value_one_V, 0);
     if Assigned(F_Value_one_S) then ReallocMem(F_Value_one_S, 0);
     {}
     ReAllocMem(pNodeFMs,0);
     ReAllocMem(pCommMatrix,0);
     ReAllocMem(pCommDelayMatrix, 0);
     ReAllocMem(pCommDelaySteps, 0);
     Inherited Destroy;
End;


{--------------------------------------------------------------------------}
Procedure ConvertBlanks(Var s:String);
VAR
    BlankPos:Integer;

Begin
     { Convert spaces to Underscores }
     BlankPos := Pos(' ', S);
     WHILE BlankPos>0 DO Begin
         S[BlankPos] := '_';
         BlankPos := Pos(' ', S);
     End;
End;

{--------------------------------------------------------------------------}
Procedure TFMonitorObj.RecalcElementData;

VAR
   DevIndex :Integer;
   i : integer;
Begin
         ValidMonitor := FALSE;
         Devindex := GetCktElementIndex(ElementName);                   // Global function
         IF DevIndex>0 THEN Begin                                       // Monitored element must already exist
             MeteredElement := ActiveCircuit.CktElements.Get(DevIndex);
             Case (Mode and MODEMASK) of
                2: Begin                                                // Must be transformer
                          If (MeteredElement.DSSObjType And CLASSMASK) <> XFMR_ELEMENT Then Begin
                            DoSimpleMsg(MeteredElement.Name + ' is not a transformer!', 663);
                            Exit;
                          End;
                   End;
                3: Begin                                                // Must be PCElement
                          If (MeteredElement.DSSObjType And BASECLASSMASK) <> PC_ELEMENT Then Begin
                            DoSimpleMsg(MeteredElement.Name + ' must be a power conversion element (Load or Generator)!', 664);
                            Exit;
                          End;
                   End;
                6: begin                                                // Checking Caps Tap
                          If (MeteredElement.DSSObjType And CLASSMASK) <> CAP_ELEMENT Then Begin
                            DoSimpleMsg(MeteredElement.Name + ' is not a capacitor!', 2016001);
                            Exit;
                          End;
                   end;



             End;

             IF MeteredTerminal>MeteredElement.Nterms THEN Begin
                 DoErrorMsg('FMonitor: "' + Name + '"',
                                 'Terminal no. "' +'" does not exist.',
                                 'Respecify terminal no.', 665);
             End
             ELSE Begin
                 Nphases := MeteredElement.NPhases;
                 Nconds  := MeteredElement.NConds;

               // Sets name of i-th terminal's connected bus in monitor's buslist
               // This value will be used to set the NodeRef array (see TakeSample)
                 Setbus(1, MeteredElement.GetBus(MeteredTerminal));


                 Case (Mode and MODEMASK) of
                      3: Begin
                             NumStateVars := TPCElement(MeteredElement).Numvariables;
                             ReallocMem(StateBuffer, Sizeof(StateBuffer^[1])*NumStatevars);
                         End;
                      4: Begin
                             ReallocMem(FlickerBuffer, Sizeof(FlickerBuffer^[1])*Nphases);
                         End;
                      5: Begin
                             ReallocMem(SolutionBuffer, Sizeof(SolutionBuffer^[1])*NumSolutionVars);
                         End;
                 Else
                     ReallocMem(CurrentBuffer, SizeOf(CurrentBuffer^[1])*MeteredElement.Yorder);
                     ReallocMem(VoltageBuffer, SizeOf(VoltageBuffer^[1])*MeteredElement.NConds);
                 End;

                 //ClearMonitorStream;

                 ValidMonitor := TRUE;

             End;

         End
         ELSE Begin
            MeteredElement := nil;   // element not found
            DoErrorMsg('Monitor: "' + Self.Name + '"', 'Circuit Element "'+ ElementName + '" Not Found.',
                            ' Element must be defined previously.', 666);
         End;
         {}
         {}
End;

procedure TFMonitorObj.MakePosSequence;
begin
  if MeteredElement <> Nil then begin
    Setbus(1, MeteredElement.GetBus(MeteredTerminal));
    Nphases := MeteredElement.NPhases;
    Nconds  := MeteredElement.Nconds;
    Case (Mode and MODEMASK) of
      3: Begin
             NumStateVars := TPCElement(MeteredElement).Numvariables;
             ReallocMem(StateBuffer, Sizeof(StateBuffer^[1])*NumStatevars);
         End;
      4: Begin
              ReallocMem(FlickerBuffer, Sizeof(FlickerBuffer^[1])*Nphases);
         End;
      5: Begin
             ReallocMem(SolutionBuffer, Sizeof(SolutionBuffer^[1])*NumSolutionVars);
         End;
      Else
         ReallocMem(CurrentBuffer, SizeOf(CurrentBuffer^[1])*MeteredElement.Yorder);
         ReallocMem(VoltageBuffer, SizeOf(VoltageBuffer^[1])*MeteredElement.NConds);
      End;
    //ClearMonitorStream;
    ValidMonitor := TRUE;
  end;
  Inherited;
end;


{--------------------------------------------------------------------------}
Procedure TFMonitorObj.CalcYPrim;
Begin

  {A Monitor is a zero current source; Yprim is always zero.}
  // leave YPrims as nil and they will be ignored
  // Yprim is zeroed when created.  Leave it as is.
End;

{--------------------------------------------------------------------------}
Procedure TFMonitorObj.Save;

// Saves present buffer to monitor file, resets bufferptrs and continues

Begin

     //If NOT IsFileOpen THEN OpenMonitorStream; // Position to end of stream

     {Write present monitor buffer to monitorstream}
     //MonitorStream.Write(MonBuffer^, SizeOF(MonBuffer^[1]) * BufPtr);

     BufPtr := 0; // reset Buffer for next

End;
{--------------------------------------------------------------------------}
 Procedure TFMonitorObj.Set_nodes_for_fm(intNodes : integer);
var
  i,j: Integer;
begin
  //
      Nodes := intNodes;//initalize the size according to nodes
      if pNodeFMs<>nil then ReAllocMem(pNodeFMs,0);
      if pCommMatrix<>nil then ReAllocMem(pCommMatrix,0);
      if pCommDelayMatrix<>nil then ReAllocMem(pCommDelayMatrix,0);
      if pCommDelaySteps<>nil then ReAllocMem(pCommDelaySteps,0);
       //
      pNodeFMs := AllocMem( Sizeof(pNodeFMs^[1] )* intNodes);
      ReAllocMem(pCommMatrix, intNodes *intNodes * sizeof(pCommMatrix^[1]));
      ReAllocMem(pCommDelayMatrix, intNodes *intNodes * sizeof(pCommDelayMatrix^[1]));
      ReAllocMem(pCommDelaySteps, intNodes *intNodes * sizeof(pCommDelaySteps^[1]));
      for i:=1 to nodes do
         for j := 1 to nodes do
               pCommDelayMatrix^[nodes*(i-1)+j] := 0.0;

end;
{--------------------------------------------------------------------------}
Procedure TFMonitorObj.Set_volt_lmt_clstr( strParam: string);
var
    i:Integer;
    Datahgh, datalw:double;
    iPhasenum : integer;
begin
    AuxParser.CmdString := strParam;  // Load up Parser
    AuxParser.NextParam; // the first entry is the No. of iNode
    iPhasenum := AuxParser.IntValue; //node number defined in cluster
    AuxParser.NextParam; // high limit
    Datahgh :=  AuxParser.DblValue;
    AuxParser.NextParam; // low limit
    Datalw :=  AuxParser.DblValue;

    case iPhaseNum of
    0: begin
           ld_fm_info[0].volt_hgh_lmt := Datahgh;
           ld_fm_info[0].volt_lw_lmt := Datalw;
      end;
    1: ;
    2: ;
    3: ;
    end;
end;
Procedure TFMonitorObj.Set_CommVector( strParam: string);
VAR
    TempStr,
    DataStr       :String;
    i,j,
    iMin,                      // the min if Nodes or the length of the vector
    iNodeNum      : integer;
Begin

    AuxParser.CmdString := strParam;  // Load up Parser
    //iMin := min(Nodes, )
    {Loop for no more than the expected number of windings;  Ignore omitted values}

            AuxParser.NextParam; // the first entry is the No. of iNode
            iNodeNum := AuxParser.IntValue; //node number defined in cluster
          FOR i := 2 to (Nodes + 1) Do  Begin
               AuxParser.NextParam; // ignore any parameter name  not expecting any
               DataStr := AuxParser.StrValue;
               IF Length(DataStr) > 0 THEN pCommMatrix^[(iNodeNum-1)*Nodes+ i-1] := AuxParser.intValue;
          End;

// Updates the value of the property for future queries
// Added y Davis 02072019
    TempStr     :=  '';
    for j := 1 to Nodes do
    Begin
      iNodeNum    :=  j;
      TempStr :=  TempStr + inttostr(iNodeNum) + ',';
      for i := 2 to (Nodes + 1) do
        TempStr :=  TempStr + inttostr(pCommMatrix^[(iNodeNum-1)*Nodes+ i-1]) + ',';

      TempStr :=  TempStr + '|';
    End;
    ActiveDSSObject.PropertyValue[15]  :=  TempStr;


end;

Procedure TFMonitorObj.Set_CommDelayVector( strParam: string);
VAR
    TEmpStr,
    DataStr       :String;
    i, j,
    iNodeNum      : integer;
Begin

    AuxParser.CmdString := strParam;  // Load up Parser
    //iMin := min(Nodes, )
    {Loop for no more than the expected number of windings;  Ignore omitted values}

            AuxParser.NextParam; // the first entry is the No. of iNode
            iNodeNum := AuxParser.IntValue; //node number defined in cluster
          FOR i := 2 to (Nodes + 1) Do  Begin
               AuxParser.NextParam; // ignore any parameter name  not expecting any
               DataStr := AuxParser.StrValue;
               IF Length(DataStr) > 0 THEN pCommDelayMatrix^[(iNodeNum-1)*Nodes+ i-1] := AuxParser.DblValue;
          End;

    ResetDelaySteps(iNodeNum);  //Use pCommDelayMatrix^ to calculate pCommDelaySteps^

// Updates the value of the property for future queries
// Added y Davis 02072019
    TempStr     :=  '';
    for j := 1 to Nodes do
    Begin
      iNodeNum    :=  j;
      TempStr :=  TempStr + inttostr(iNodeNum) + ',';
      for i := 2 to (Nodes + 1) do
        TempStr :=  TempStr + floattostr(pCommDelayMatrix^[(iNodeNum-1)*Nodes+ i-1]) + ',';

      TempStr :=  TempStr + '|';
    End;
    ActiveDSSObject.PropertyValue[18]  :=  TempStr;
end;
{--------------------------------------------------------------------------}
Procedure TFMonitorObj.Set_ElemTable_line( strParam: string);
VAR
    TempStr,
    DataStr   :String;
    i         :Integer;
    iNodeNum  : integer;
Begin
    AuxParser.CmdString        := strParam;  // Load up Parser
    AuxParser.NextParam; // the first entry is the number of the iNode
    iNodeNum                                := AuxParser.IntValue; //node number defined in the cluster
    AuxParser.NextParam; // the first entry is the number of the iNode
    pNodeFMs^[iNodeNum].vl_strBusName       := AuxParser.strValue; //node number defined in the cluster
    AuxParser.NextParam;
    pNodeFMs^[iNodeNum].vl_strMeasuredName  := AuxParser.StrValue; //Element name load into data str
               //
               //pNodeFMs^[iNodeNum].vl_strName_dg := pNodeFMs^[iNodeNum].vl_strMeasuredName;
               //
    AuxParser.NextParam;
    pNodeFMs^[iNodeNum].vl_terminalNum := AuxParser.IntValue;  //Terminal number load into data str
    AuxParser.NextParam;
    pNodeFMs^[iNodeNum].vl_V_ref_dg := 1000*AuxParser.dblValue;
    AuxParser.NextParam;
    pNodeFMs^[iNodeNum].vl_kc_ul_dg := AuxParser.dblValue;
               //2.402
    Init_nodeFM(iNodeNum);
// Updates the value of the property for future queries
// Added y Davis 02072019
    TempStr     :=  '';
    for i := 1 to iNodeNum do
    Begin
      TempStr :=  TempStr + inttostr(i)                 + ',' +
                  pNodeFMs^[i].vl_strBusName            + ',' +
                  pNodeFMs^[i].vl_strMeasuredName       + ',' +
                  inttostr(pNodeFMs^[i].vl_terminalNum) + ',' +
                  floattostr(pNodeFMs^[i].vl_V_ref_dg)  + ',' +
                  floattostr(pNodeFMs^[iNodeNum].vl_kc_ul_dg) + '|';
    End;
    ActiveDSSObject.PropertyValue[16]  :=  TempStr;
end;
Procedure TFMonitorObj.Get_PQ_DI(i_NodeNum : integer);
var
    Devindex,i, j, num: integer;
    pElement  :TDSSCktElement   ;
    pLoad: TLoadObj;
    cBuffer:pComplexArray;
begin

    with  pNodeFMs^[i_NodeNum] do
    beGIn
        case ldType of
            0:// one 3 phase or 2 phase load
            begin
                 pElement :=  ActiveCircuit.PCElements.Get(ldidx);
                 num := pElement.NPhases;
                 cBuffer := Allocmem(sizeof(cBuffer^[1])*num);
                 pElement.GetPhasePower(cBuffer);// power

                 for j := 1 to num do
                 begin
                      i := pElement.Terminals[0].TermNodeRef[j - 1];
                      case activecircuit.MapNodeToBus^[i].NodeNum of
                            1: begin vl_P_Di1 := cBuffer^[1].re; vl_Q_Di1 := cBuffer^[1].im;  end;
                            2: begin vl_P_Di1 := cBuffer^[2].re; vl_Q_Di1 := cBuffer^[2].im;  end;
                            3: begin vl_P_Di1 := cBuffer^[3].re; vl_Q_Di1 := cBuffer^[3].im;  end;
                      end;
                 end;
            end;
            1,2,3:
            begin
                 pElement := nil;
                 if ldidx1>0 then
                 begin
                     pElement :=  ActiveCircuit.PCElements.Get(ldidx1);
                     num := pElement.NPhases;
                     cBuffer := Allocmem(sizeof(cBuffer^[1])*num);
                     pElement.GetPhasePower(cBuffer);// power

                     for j := 1 to num do
                     begin
                          i := pElement.Terminals[0].TermNodeRef[j - 1];
                          case activecircuit.MapNodeToBus^[i].NodeNum of
                                1: begin vl_P_Di1 := cBuffer^[1].re; vl_Q_Di1 := cBuffer^[1].im;  end;
                                2: begin vl_P_Di1 := cBuffer^[1].re; vl_Q_Di1 := cBuffer^[1].im;  end;
                                3: begin vl_P_Di1 := cBuffer^[1].re; vl_Q_Di1 := cBuffer^[1].im;  end;
                          end;
                     end;
                 end;
                 if ldidx2>0 then
                 begin
                     pElement :=  ActiveCircuit.PCElements.Get(ldidx2);
                     num := pElement.NPhases;
                     cBuffer := Allocmem(sizeof(cBuffer^[1])*num);
                     pElement.GetPhasePower(cBuffer);// power

                     for j := 1 to num do
                     begin
                          i := pElement.Terminals[0].TermNodeRef[j - 1];
                          case activecircuit.MapNodeToBus^[i].NodeNum of
                                1: begin vl_P_Di2 := cBuffer^[1].re; vl_Q_Di2 := cBuffer^[1].im;  end;
                                2: begin vl_P_Di2 := cBuffer^[1].re; vl_Q_Di2 := cBuffer^[1].im;  end;
                                3: begin vl_P_Di2 := cBuffer^[1].re; vl_Q_Di2 := cBuffer^[1].im;  end;
                          end;
                     end;
                 end;
                 if ldidx3>0 then
                 begin
                     pElement :=  ActiveCircuit.PCElements.Get(ldidx3);
                     num := pElement.NPhases;
                     cBuffer := Allocmem(sizeof(cBuffer^[1])*num);
                     pElement.GetPhasePower(cBuffer);// power

                     for j := 1 to num do
                     begin
                          i := pElement.Terminals[0].TermNodeRef[j - 1];
                          case activecircuit.MapNodeToBus^[i].NodeNum of
                                1: begin vl_P_Di3 := cBuffer^[1].re; vl_Q_Di3 := cBuffer^[1].im;  end;
                                2: begin vl_P_Di3 := cBuffer^[1].re; vl_Q_Di3 := cBuffer^[2].im;  end;
                                3: begin vl_P_Di3 := cBuffer^[1].re; vl_Q_Di3 := cBuffer^[3].im;  end;
                          end;
                     end;
                 end;
            end
//            2:
//            begin
//
//            end;
//            3:
//            begin
//
//            end
            else
        end;
    end;
end;

Procedure TFMonitorObj.Init_nodeFM(iNodeNum : integer);
var
    strTemp : string;
    Devindex, i, j: integer;
    PCindex_ld :integer;
    pElem  :TDSSCktElement   ;

    pDG : TGeneric5Obj;
    pLd : TLoadObj;
    //pPDElem : TPDElement;
    lstPC :TAdjArray;
    num : integer;
    ctmp : complex;
    cBuffer:pComplexArray;
begin

    //init all info of this node
    with  pNodeFMs^[iNodeNum] do
    beGIn
    //1
        strTemp := lowercase( vl_strBusName);
        Bus_Idx := ActiveCircuit.BusList.Find(strTemp);

        Devindex := GetCktElementIndex(vl_strMeasuredName);                   // Global function
        IF DevIndex>0 THEN Begin                                       // Monitored element must already exist
             pElem := ActiveCircuit.CktElements.Get(DevIndex);
        end;
        vl_ndphases := pElem.NPhases;
        vl_basevolt := ActiveCircuit.Buses[bus_idx].kVBase*1000;
        vl_phase_num_dg := -1; //-1 -- no dg under this nodes;0 --3 phases by default

        for j := 1 to 3 do vl_nodeType_phase[j] := 2;// by default not dg

        WITH ActiveCircuit
        Do Begin
             pElem := PCElements.First;
             PCindex_ld := PCElements.activeindex;
             WHILE pElem <> nil
             Do Begin
               IF pElem.Enabled  THEN
               begin
                    if pElem.ActiveTerminal.BusRef =  Bus_Idx then
                    begin
                      if {$IFDEF FPC}ANSIContainsText{$ELSE}ContainsText{$ENDIF}(lowercase(pElem.DSSClassName),'generic5') then
                       begin

                                  //vl_nodeType should be define per phase
                                    vl_ndphases_dg := pElem.NPhases; //1 or 3
                                                                     // under 1 bus, there can be either 3 phase DG or 1 phase dg
                                                                     //set Cluster defination for DG
                                                                     // one 3phase dg
                                                                     // 1,2 or 3  1-phase dgs under each phase
                                    pDG := TGeneric5Obj(pElem);
                                    num :=  trunc(pDG.Get_Variable(30)); //ctrl_mode
                                   case num of
                                    1:  vl_nodeType_phase[1] := 1;
                                    2:  vl_nodeType_phase[2] := 1;
                                    3:  vl_nodeType_phase[3] := 1;
                                   else  for j := 1 to 3 do vl_nodeType_phase[j] := 1; // //ctrl_mode = 4 or 0
                                   end;
                                   //pDG
                                   if (pDG.FMonObj = nil) then // first cluster
                                   begin

                                       pDG.Set_Variable(28,cluster_num); //28:  TPCElement(self).cluster_num :=  trunc(Value);
                                                                         //if cluster_num >= 1 then      // assign the virtue leader to this DG
                                       pDG.FMonObj := self;    //FMonObj := ActiveCircuit.Fmonitors.Get(cluster_num); cluster_num can not be used for 'Get'
                                       pDG.Set_Variable(29,iNodeNum); //29:  TPCElement(self).NdNumInCluster := trunc(Value) ;
                                       pDG.Set_Variable(30,1);         //TPCElement(self).nVLeaders := trunc(Value) ;
                                   end else
                                   begin    // the second virtual leader // which means if the 2nd one will always be the one being overwritten

                                       if (cluster_num <> Round(pDG.get_Variable(28))) then
                                       begin
                                            pDG.Set_Variable(31,cluster_num); //28:  TPCElement(self).cluster_num2 :=  trunc(Value);
                                                                         //if cluster_num >= 1 then      // assign the virtue leader to this DG
                                            pDG.FMonObj2 := self;             //FMonObj := ActiveCircuit.Fmonitors.Get(cluster_num); cluster_num can not be used for 'Get'
                                            pDG.Set_Variable(32,iNodeNum);    //29:  TPCElement(self).NdNumInCluster2 := trunc(Value) ;
                                            pDG.Set_Variable(30,2);           //  TPCElement(self).nVLeaders := trunc(Value) ;
                                       end else
                                       begin
                                            //
                                       end;
                                   end;
                                   vl_phase_num_dg := 0; //3 phases by default
                                   if vl_ndphases_dg=1 then
                                      vl_phase_num_dg := trunc(pDG.Get_Variable(30));  //if vl_nodeType=1, and vl_ndphases=1,phase_num =1,2,3 0- this node has 3 phases
                                                                         // 30, ctrl_mode, is the phase number of this Generic5

                       end;

                       ldType := -1;
                       ldIdx :=  -1;
                       ldIdx1 :=  -1;
                       ldIdx2 :=  -1;
                       ldIdx3 :=  -1;

                       if {$IFDEF FPC}ANSIContainsText{$ELSE}ContainsText{$ENDIF}(lowercase(pElem.DSSClassName),'load') then
                       begin

                            num := pElem.NPhases;
                            pLd := TLoadObj(pElem);
                            //Devindex :=
                            if num = 3 then
                            begin
                                  ldIdx :=  PCindex_ld ;
                                  ldType := 0;
                            end else if num=2 then

                            begin
                                  ldIdx :=  PCindex_ld ;
                                  ldType := 0;

                            end else if num=1 then

                            begin
                                 i := pElem.Terminals[0].TermNodeRef[0];
                                       case activecircuit.MapNodeToBus^[i].NodeNum of
                                             1: ldIdx1 :=  PCindex_ld ;
                                             2: ldIdx2 :=  PCindex_ld ;
                                             3: ldIdx3 :=  PCindex_ld ;
                                       end;
                                 if ldType<1 then
                                       ldType := 1
                                 else if ldType>=1 then
                                    ldType := ldType + 1;
                                 if ldType>=3 then
                                      ldType := 3 ;
                            end;
                            //

//                            cBuffer := Allocmem(sizeof(cBuffer^[1])*num);
//                            pElem.GetPhasePower(cBuffer);// power
//                            for j := 1 to num do
//                                     begin
//                                       i := pElem.Terminals[0].TermNodeRef[j - 1];
//                                       case activecircuit.MapNodeToBus^[i].NodeNum of
//                                       1: begin vl_P_Di1 := cBuffer^[1].re; vl_Q_Di1 := cBuffer^[1].im;  end;
//                                       2: begin vl_P_Di1 := cBuffer^[2].re; vl_Q_Di1 := cBuffer^[2].im;  end;
//                                       3: begin vl_P_Di1 := cBuffer^[3].re; vl_Q_Di1 := cBuffer^[3].im;  end;
//                                       end;
//                                     end;
//
//                            Reallocmem(cBuffer,0);
                         end;
                    end;
              End;
              pElem := PCElements.Next;
              PCindex_ld := PCElements.activeindex;
            End;
      end;
        //lstPC := ActiveCircuit.GetBusAdjacentPCLists;
        //activecircuit.MapNodeToBus
        //num := lstPC.NumShuntObjects;

   //2


         // will be overwritten if this nodeFM is a 1-phase Generic5
        ////// update cluster_num and node number of this DG in this cluster
        {if vl_nodeType=1 then   // this only work for DG
        begin
             //set
             pDG := TGeneric5Obj(pElement);
             //pDG
             if pDG.FMonObj = nil then // first cluster
             begin

                 pDG.Set_Variable(28,cluster_num); //28:  TPCElement(self).cluster_num :=  trunc(Value);
                                                   //if cluster_num >= 1 then      // assign the virtue leader to this DG
                 pDG.FMonObj := self;    //FMonObj := ActiveCircuit.Fmonitors.Get(cluster_num); cluster_num can not be used for 'Get'
                 pDG.Set_Variable(29,iNodeNum); //29:  TPCElement(self).NdNumInCluster := trunc(Value) ;
                 pDG.Set_Variable(30,1);         //TPCElement(self).nVLeaders := trunc(Value) ;
             end else
             begin    // the second virtual leader // which means if the 2nd one will always be the one being overwritten

                 pDG.Set_Variable(31,cluster_num); //28:  TPCElement(self).cluster_num2 :=  trunc(Value);
                                                   //if cluster_num >= 1 then      // assign the virtue leader to this DG
                 pDG.FMonObj2 := self;             //FMonObj := ActiveCircuit.Fmonitors.Get(cluster_num); cluster_num can not be used for 'Get'
                 pDG.Set_Variable(32,iNodeNum);    //29:  TPCElement(self).NdNumInCluster2 := trunc(Value) ;
                 pDG.Set_Variable(30,2);           //  TPCElement(self).nVLeaders := trunc(Value) ;
             end;
             if vl_ndphases_dg=1 then
                vl_phase_num_dg := trunc(pDG.Get_Variable(30));  //if vl_nodeType=1, and vl_ndphases=1,phase_num =1,2,3 0- this node has 3 phases
                                                       // 30, ctrl_mode, is the phase number of this Generic5
        end;  }
        {if vl_nodeType=2 then   // line and tranformer
        begin
             //set
             //pPDelem := TPDElement(pElement);
             //pDG
        end;
        }
        ///
        vl_V_ref1_dg:= vl_V_ref_dg;//:=1000*2.4; 1000*2.4; V_ref2:=1000*2.4; V_ref3 :=1000*2.4;// norminal value with respect to p.u. 1  //must be set by initialization
        vl_V_ref2_dg:= vl_V_ref_dg;
        vl_V_ref3_dg:= vl_V_ref_dg;
        //kcq := 1.0; // the step size gain of agent i //has to be defined befor used

        ///  other properties if needed
        //ndphases : integer; //how many phases of this device on this node; not those of node
        vl_CC_switch_dg :=false; // cooperate control switch. true, cooperate control is on
        vl_PF_flag_dg :=0 ;//1, real power control is on
        vl_QV_flag_dg :=0;//1, volt/var control is on
        vl_Alpha_dg:=0;
        vl_Alpha1_dg:=0;vl_Alpha2_dg:=0;vl_Alpha3_dg:=0;
        vl_Gradient_dg:=0;vl_Gradient1_dg:=0;vl_Gradient2_dg:=0;vl_Gradient3_dg:=0;
        vl_AlphaP_dg:=0;
        vl_AlphaP1_dg:=0;vl_AlphaP2_dg:=0;vl_AlphaP3_dg:=0;
        vl_GradientP_dg:=0;vl_GradientP1_dg:=0;vl_GradientP2_dg:=0;vl_GradientP3_dg:=0;
        vl_Pmax_dg:=0; vl_Qmax_dg:=0;
        vl_Pmax_phase_dg:=0; vl_Qmax_phase_dg :=0;
        vl_V_base_dg:=1000*2.4;
        vl_V:=1000*2.4;vl_V1:=1000*2.4;vl_V2:=1000*2.4;vl_V3:=1000*2.4;
        vl_Q_Di := 0.0;
        vl_Q_Di1 := 0.0;
        vl_Q_Di2 := 0.0;
        vl_Q_Di3 := 0.0;
        vl_P_Di := 0.0;
        vl_P_Di1 := 0.0;
        vl_P_Di2 := 0.0;
        vl_P_Di3 := 0.0;
        vl_smplCnt := 0;
        vl_crnt_smp_time := 0.0;
    end;
end;
Procedure  TFMonitorObj.Get_PDElem_terminal_voltage(nd_num_in_cluster: integer ;devName: string; Tern_num: integer );
var
    tempTerminal : TPowerTerminal;
    i ,Devindex, j: integer;
    tempElement  :TDSSCktElement ;
    //VAR
   //pElem:TDSSCktElement;
    phase_num:Integer ;
    vabs : double;
    //V012 :TSymCompArray5;
    V012 : Array[0..2] of Complex;
    VaVbVc :Array[1..3] of Complex;
begin
        Devindex := GetCktElementIndex(devName);                   // Global function
         IF DevIndex>0 THEN Begin                                       // Monitored element must already exist
             tempElement := ActiveCircuit.CktElements.Get(DevIndex);
         end;
              //
         tempTerminal := tempElement.Terminals[Tern_num - 1];
         for j:=1 to tempElement.NPhases do// how many phases of this element
         begin
              i := tempTerminal.TermNodeRef[j - 1];  // global node number
              phase_num := ActiveCircuit.MapNodeToBus^[i].NodeNum;
              vabs := cabs (activecircuit.Solution.NodeV^[i]);
              if phase_num=1 then // phase A
              begin
                   pnodefms^[nd_num_in_cluster].vl_V1 := vabs ;
                   pnodefms^[nd_num_in_cluster].vl_V_1c :=   activecircuit.Solution.NodeV^[i];
              end
              else if phase_num=2 then    //phase B
              begin
                    pnodefms^[nd_num_in_cluster].vl_V2 := vabs;
                    pnodefms^[nd_num_in_cluster].vl_V_2c :=   activecircuit.Solution.NodeV^[i];
              end
              else if phase_num=3 then    //phase c
              begin
                    pnodefms^[nd_num_in_cluster].vl_V3 := vabs;
                    pnodefms^[nd_num_in_cluster].vl_V_3c :=   activecircuit.Solution.NodeV^[i];
              end;
         end;
         if tempElement.NPhases=3  then
         begin
            VaVbVc[1] := pnodefms^[nd_num_in_cluster].vl_V_1c;//phase A
            VaVbVc[2] := pnodefms^[nd_num_in_cluster].vl_V_2c;
            VaVbVc[3] := pnodefms^[nd_num_in_cluster].vl_V_3c;
            Phase2SymComp(@VaVbVc, @V012);  // Convert abc voltages to 012
            pnodefms^[nd_num_in_cluster].vl_V := cabs(V012[1]);  //pos. seq. Voltage
         end;

end;

Procedure  TFMonitorObj.update_all_nodes_info; //PD nodes
var
  i : integer;
begin
     for i := 1 to nodes do
     begin
          with pnodeFMs^[i] do
          begin
              //if vl_nodeType = 1 then // generic dg
              //begin
                   // do nothing; because the info is updated from the other end (sent by DGs)
              //end
              //else if vl_nodeType = 2 then // PD elements: lines, xformers, etc..
              //begin
                   //Update_PD_Node_Info(i,vl_strMeasuredName,vl_terminalNum);
                   Get_PDElem_terminal_voltage(i,vl_strMeasuredName,vl_terminalNum,ActorID ) ;
                   //Calc_Alpha_for_PDNode(i ); // calc all alpha, alpha1, alpha2, alph3 of all pdelement nodes
              //end;
          end;
      end;

end;
{--------------------------------------------------------------------------}
Procedure TFMonitorObj.ResetDelaySteps(iNodeNum: integer);
Var
    j,tmp : integer;
begin
          //calc delay array
              for j := 1 to nodes do
              begin
                  if (T_intvl_smpl=0.0) or (pCommDelayMatrix^[(iNodeNum-1)*nodes+j]=0.0) then
                      pCommDelaySteps^[(iNodeNum-1)*nodes+j] := 0
                  else
                  begin
                      tmp := trunc(pCommDelayMatrix^[(iNodeNum-1)*nodes+j] /T_intvl_smpl);
                      if frac(pCommDelayMatrix^[(iNodeNum-1)*nodes+j] /T_intvl_smpl)=0.0 then
                          pCommDelaySteps^[(iNodeNum-1)*nodes+j] := tmp
                      else
                          pCommDelaySteps^[(iNodeNum-1)*nodes+j] := tmp + 1;
                      //How many delays for communication
                  end;
              end
end;

Procedure TFMonitorObj.ResetIt;
Var
    iTmp : integer;
Begin
     BufPtr := 0;
     //ClearMonitorStream;
     if ActiveCircuit.Solution.DynaVars.SolutionMode = DYNAMICMODE then
     begin
          //calc Delay_stps for sampling
          if T_intvl_smpl = 0.0 then
              Smpl_stps := 0  //No delay.
          else
          begin
              iTmp := Trunc(T_intvl_smpl / ActiveCircuit.Solution.Dynavars.h);
              if frac(T_intvl_smpl / ActiveCircuit.Solution.Dynavars.h)=0.0 then
                     Smpl_stps := iTmp
                else
                     Smpl_stps := iTmp +1 ;// uper
          end;
          for iTmp := 1 to Nodes do
          begin
                pnodeFMs^[iTmp].vl_smplCnt := 0;
                pNodeFMs^[iTmp].vl_crnt_smp_time := ActiveCircuit.Solution.DynaVars.intHour*3600+ActiveCircuit.Solution.DynaVars.t ;
                Init_delay_array(iTmp); // in DYNAMICMODE, init alpha array
          end;

     end;
End;

{--------------------------------------------------------------------------}
{
Procedure TFMonitorObj.PostProcess;
Begin
  if IsProcessed = FALSE then begin
    if (mode = 4) and (MonitorStream.Position > 0) then DoFlickerCalculations;
  end;
  IsProcessed := TRUE;
End;

{--------------------------------------------------------------------------}
{
Procedure TFMonitorObj.GetFvalue;
VAR
    dHour             :Double;
    dSum              :Double;
    i,j               :Integer;
    IsPower           :Boolean;
    IsSequence        :Boolean;
    NumVI             :Integer;
    Offset            :Integer;
    ResidualCurr      :Complex;
    ResidualVolt      :Complex;
    Sum               :Complex;
    V012,I012         :Array[1..3] of Complex;

     Monitor : TFMonitorObj;


Begin

   If Not (ValidMonitor and Enabled) Then Exit;

   //inc(SampleCount);

   Hour := ActiveCircuit.Solution.DynaVars.intHour;
   Sec :=  ActiveCircuit.Solution.Dynavars.t;

   Offset := (MeteredTerminal-1)  * MeteredElement.NConds;

   //Save time unless Harmonics mode and then save Frequency and Harmonic
   WITH ActiveCircuit.Solution Do
     IF IsHarmonicModel Then Begin
         AddDblsToBuffer(@Frequency, 1);  // put freq in hour slot as a double
         AddDblsToBuffer(@Harmonic ,1);  // stick harmonic in time slot in buffer
     End
     ELSE Begin
         dHour := Hour;      // convert to double
         AddDblsToBuffer(@dHour, 1);  // put hours in buffer as a double
         AddDblsToBuffer(@Sec, 1);  // stick time in sec in buffer
     End;

   CASE  (Mode AND MODEMASK) of

     0,1:       // Voltage, current. Powers
       Begin

            // MeteredElement.GetCurrents(CurrentBuffer);
            // To save some time, call ComputeITerminal
            MeteredElement.ComputeIterminal;   // only does calc if needed
            For i := 1 to MeteredElement.Yorder Do CurrentBuffer^[i] := MeteredElement.Iterminal^[i];

            TRY

                Bus_code := Activeterminal.BusRef;
                Monitor := ActiveCircuit.FMonitors.Active;
              //Detect non-existed phase======================================================
              for j := 1 to 3 do
                 begin
                   NodeNum := ActiveCircuit.Buses^[Bus_code].Find(j);
                   if NodeNum=0 then
                   begin
                    F_Value_one_V^[j] := 9999;
                   end
                   else
                   begin

                    for i := 1 to Fnphases do
                       begin
                         Node_Ref := Monitor.ActiveTerminal.TermNodeRef[i - 1];
                         if Node_Ref=NodeNum then  F_Value_one_V^[j] := cabs(ActiveCircuit.Solution.NodeV^[NodeRef^[i]])/ActiveCircuit.Solution.NodeVbase^[NodeRef^[i]];
                       end;

                   end;
                 end;

              FOR i := 1 to Fnconds DO
              Begin
                // It is the index of the terminal into the system node list
                  //Meteredelement.
                  f_value_one := cabs(ActiveCircuit.Solution.NodeV^[NodeRef^[1]]); //A value of voltage on first phase
                  f_value_one := f_value_one/ActiveCircuit.Solution.NodeVbase^[NodeRef^[i]] ;
                  VoltageBuffer^[i] := ActiveCircuit.Solution.NodeV^[NodeRef^[i]];
              End;
            EXCEPT
               On E:Exception Do DoSimpleMsg(E.Message + CRLF + 'NodeRef is invalid. Try solving a snapshot or direct before solving in a mode that takes a monitor sample.', 672);
            END;
       End;

     2: Begin     // Monitor Transformer Tap Position

              With TTransfObj(MeteredElement) Do Begin
                   //AddDblToBuffer(PresentTap[MeteredTerminal]);
              End;
              Exit;  // Done with this mode now.
        End;

     3: Begin   // Pick up device state variables
              TPCElement(MeteredElement).GetAllVariables(StateBuffer);
              AddDblsToBuffer(StateBuffer, NumStateVars);
              Exit; // Done with this mode now
        End;

     4: Begin   // RMS phase voltages for flicker evaluation
            TRY
              FOR i := 1 to Fnphases DO Begin
                  FlickerBuffer^[i] := ActiveCircuit.Solution.NodeV^[NodeRef^[i]];
              End;
            EXCEPT
               On E:Exception Do DoSimpleMsg(E.Message + CRLF + 'NodeRef is invalid. Try solving a snapshot or direct before solving in a mode that takes a monitor sample.', 672);
            END;
        End;

     5: Begin
            (* Capture Solution Variables *)
            With ActiveCircuit.Solution Do Begin
             SolutionBuffer^[1]   :=  Iteration;
             SolutionBuffer^[2]   :=  ControlIteration;
             SolutionBuffer^[3]   :=  MaxIterations;
             SolutionBuffer^[4]   :=  MaxControlIterations;
             If ConvergedFlag then SolutionBuffer^[5] := 1 else SolutionBuffer^[5] := 0;
             SolutionBuffer^[6]   :=  IntervalHrs;
             SolutionBuffer^[7]   :=  SolutionCount;
             SolutionBuffer^[8]   :=  Mode;
             SolutionBuffer^[9]   :=  Frequency;
             SolutionBuffer^[10]  :=  Year;
             SolutionBuffer^[11]  :=  Time_Solve;
             SolutionBuffer^[12]  :=  Time_Step;
            End;

        End;

     6: Begin     // Monitor Transformer Tap Position

              With TCapacitorObj(MeteredElement) Do Begin
                  for i := 1 to NumSteps do
                    begin
                      //AddDblToBuffer(States[i]);
                    end;
              End;
              Exit;  // Done with this mode now.
        End;
        //
     Else Exit  // Ignore invalid mask

   End;


   IF ((Mode AND SEQUENCEMASK)>0) And (Fnphases=3)
   THEN Begin  // Convert to Symmetrical components
       Phase2SymComp(VoltageBuffer, @V012);
       Phase2SymComp(@CurrentBuffer^[Offset + 1], @I012);
       NumVI      := 3;
       IsSequence := TRUE;
       // Replace voltage and current buffer with sequence quantities
       FOR i := 1 to 3 DO VoltageBuffer^[i]         := V012[i];
       FOR i := 1 to 3 DO CurrentBuffer[Offset + i] := I012[i];
   End
   ELSE Begin
       NumVI      :=Fnconds;
       IsSequence := FALSE;
   End;

   IsPower := False;  // Init so compiler won't complain
   CASE  (Mode AND MODEMASK) of
     0: Begin        // Convert to Mag, Angle  // and compute residual if required
          IsPower := FALSE;
          IF IncludeResidual THEN Begin
             If VIPolar Then Begin
                 ResidualVolt := ResidualPolar(@VoltageBuffer^[1], Fnphases);
                 ResidualCurr := ResidualPolar(@CurrentBuffer^[Offset+1], Fnphases);
             End Else Begin
                 ResidualVolt := Residual(@VoltageBuffer^[1], Fnphases);
                 ResidualCurr := Residual(@CurrentBuffer^[Offset+1], Fnphases);
             End;
          End;
          If VIPolar Then Begin
             ConvertComplexArrayToPolar(VoltageBuffer, NumVI);
             ConvertComplexArrayToPolar(@CurrentBuffer^[Offset+1], NumVI );    // Corrected 3-11-13

             ConvertComplexArrayToPolar(F_Value_one_S, NumVI);

          End;


          //Calulate power in mode 0
          CalckPowers(F_Value_one_S, VoltageBuffer, @CurrentBuffer^[Offset+1], NumVI);
          IF (IsSequence OR ActiveCircuit.PositiveSequence) THEN  CmulArray(F_Value_one_S, 3.0, NumVI); // convert to total power
           Fvalue_P :=0 ;
           Fvalue_Q :=0 ;
          FOR i := 1 to NumVI DO
          begin
            Fvalue_P := Fvalue_P + F_Value_one_S^[i].re;
            Fvalue_Q := Fvalue_Q + F_Value_one_S^[i].im;
          end;


        End;
     1: Begin     // Convert Voltage Buffer to power kW, kvar or Mag/Angle
          CalckPowers(VoltageBuffer, VoltageBuffer, @CurrentBuffer^[Offset+1], NumVI);
          IF (IsSequence OR ActiveCircuit.PositiveSequence) THEN  CmulArray(VoltageBuffer, 3.0, NumVI); // convert to total power
          f_p_one :=0 ;
          f_q_one :=0 ;
          FOR i := 1 to NumVI DO
          begin
            f_p_one := f_p_one + VoltageBuffer^[i].re;
            f_q_one := f_q_one + VoltageBuffer^[i].im;
          end;

          If Ppolar Then ConvertComplexArrayToPolar(VoltageBuffer, NumVI);
          IsPower := TRUE;
        End;
     4: Begin
          IsPower := FALSE;
          ConvertComplexArrayToPolar(FlickerBuffer, Fnphases);
        End
   Else
   End;


End;
 }
{}
{--------------------------------------------------------------------------}

{--------------------------------------------------------------------------}
{
Procedure TFMonitorObj.AddDblsToBuffer( Dbl:pDoubleArray; Ndoubles:Integer);

VAR
   i:Integer;

Begin
   //FOR i := 1 to Ndoubles DO AddDblToBuffer(Dbl^[i]);
End;
}

{--------------------------------------------------------------------------}
{
Procedure TFMonitorObj.AddDblToBuffer(const Dbl:Double);

Begin
    // first check to see if there's enough room
    // if not, save to monitorstream first.
    IF BufPtr=BufferSize THEN Save;
    Inc(BufPtr);
    MonBuffer^[BufPtr]:=Dbl;
End;
}
{
Procedure TFMonitorObj.DoFlickerCalculations;
var
  FSignature  :Integer;
  Fversion    :Integer;
  RecordSize  :Cardinal;
  RecordBytes :Cardinal;
  SngBuffer   :Array[1..100] of Single;
  hr          :single;
  s           :single;
  N           :Integer;
  Npst        :Integer;
  i, p        :Integer;
  bStart      :Integer;
  data        :Array of pSingleArray; // indexed from zero (time) to FnPhases
  pst         :Array of pSingleArray; // indexed from zero to FnPhases - 1
  ipst        :integer;
  tpst        :single;
  defaultpst  :single;
  Vbase       :single;
  busref      :integer;
begin
  N := SampleCount;
  With MonitorStream Do Begin
    Seek(0, soFromBeginning);  // Start at the beginning of the Stream
    Read( Fsignature, Sizeof(Fsignature));
    Read( Fversion,   Sizeof(Fversion));
    Read( RecordSize, Sizeof(RecordSize));
    Read( Mode,       Sizeof(Mode));
    Read( StrBuffer,  Sizeof(StrBuffer));
    bStart := Position;
  End;
  RecordBytes := Sizeof(SngBuffer[1]) * RecordSize;
  Try
    // read rms voltages out of the monitor stream into arrays
    SetLength (data, Fnphases + 1);
    SetLength (pst, Fnphases);
    for p := 0 to FnPhases do data[p] := AllocMem (Sizeof(SngBuffer[1]) * N);
    i := 1;
    while Not (MonitorStream.Position>=MonitorStream.Size) do Begin
      With MonitorStream Do Begin
        Read( hr, SizeOf(hr));
        Read( s,  SizeOf(s));
        Read(SngBuffer, RecordBytes);
        data[0][i] := s + 3600.0 * hr;
        for p := 1 to FnPhases do data[p][i] := SngBuffer[2*p - 1];
        i := i + 1;
      End;
    End;

    // calculate the flicker level and pst
    Npst := 1 + Trunc (data[0][N] / 600.0); // pst updates every 10 minutes or 600 seconds
    for p := 0 to FnPhases-1 do begin
      pst[p] := AllocMem (Sizeof(SngBuffer[1]) * Npst);
      busref := MeteredElement.Terminals[MeteredTerminal].BusRef;
      Vbase := 1000.0 * ActiveCircuit.Buses^[busref].kVBase;
      FlickerMeter (N, BaseFrequency, Vbase, data[0], data[p+1], pst[p]);
    end;

    // stuff the flicker level and pst back into the monitor stream
    with MonitorStream do begin
      Position := bStart;
      tpst:=0.0;
      ipst:=0;
      defaultpst:=0;
      for i := 1 to N do begin
        if (data[0][i] - tpst) >= 600.0 then begin
          inc(ipst);
          tpst:=data[0][i];
        end;
        Position:=Position + 2 * SizeOf(hr); // don't alter the time
        for p := 1 to FnPhases do begin
          Write (data[p][i], sizeof(data[p][i]));
          if (ipst > 0) and (ipst <= Npst) then
            Write (pst[p-1][ipst], sizeof(pst[p-1][ipst]))
          else
            Write (defaultpst, sizeof(defaultpst))
        end;
      end;
    end;
  Finally
    for p := 0 to FnPhases do ReAllocMem (data[p], 0);
    for p := 0 to FnPhases-1 do ReAllocMem (pst[p], 0);
  end;
end;
}
{--------------------------------------------------------------------------}
{--------------------------------------------------------------------------}
Procedure TFMonitorObj.GetCurrents(Curr: pComplexArray);  //Get present value of terminal Curr for reports
VAR
   i:Integer;
Begin

{
  Revised 12-7-99 to return Zero current instead of Monitored element current because
 it was messing up Newton iteration.
}

  For i := 1 to Fnconds Do Curr^[i] := CZERO;

End;

Procedure TFMonitorObj.GetInjCurrents(Curr: pComplexArray);
Var i:Integer;
Begin
     FOR i := 1 to Fnconds DO Curr^[i] := CZERO;
End;

{--------------------------------------------------------------------------}
Procedure TFMonitorObj.DumpProperties(F: TFileStream; Complete:Boolean);

VAR
   i, k:Integer;

Begin
    Inherited DumpProperties(F,Complete);

    With ParentClass Do
     For i := 1 to NumProperties Do
       Begin
          FSWriteln(F,'~ ' + PropertyName^[i] + '=' + PropertyValue[i]);
       End;


    If Complete Then Begin
      FSWriteln(F);
      FSWriteln(F,'// BufferSize=',BufferSize:0);
      FSWriteln(F,'// Hour=',Hour:0);
      FSWriteln(F,'// Sec=',Sec:0);
      FSWriteln(F,'// BaseFrequency=',BaseFrequency:0:1);
      FSWriteln(F,'// Bufptr=',BufPtr:0);
      FSWriteln(F,'// Buffer=');
      k:=0;
      FOR i := 1 to BufPtr DO Begin
        FSWrite(F, MonBuffer^[i]:0:1,', ');
        Inc(k);
        IF k=(2 + Fnconds*4) THEN Begin
          FSWriteln(F);
          k:=0;
        End;
      End;
      FSWriteln(F);
    End;

End;

procedure TFMonitorObj.InitPropertyValues(ArrayOffset: Integer);
begin

     PropertyValue[1] := ''; //'element';
     PropertyValue[2] := '1'; //'terminal';
     PropertyValue[3] := '0'; //'mode';
     PropertyValue[4] := ''; // 'action';  // buffer=clear|save|take|process
     PropertyValue[5] := 'NO';
     PropertyValue[6] := 'YES';
     PropertyValue[7] := 'YES';
     {}
     PropertyValue[8] := '0';//ref power
     PropertyValue[9] := '0';// default voltage sensor value
     PropertyValue[10] := '0';// default power sensor value
     PropertyValue[11] := '1';// default node number value
     PropertyValue[12] := '1';// default by group 1
     PropertyValue[13] := '1';// default number of groups
     {}
  inherited  InitPropertyValues(NumPropsThisClass);

end;


{--------------------------------------------------------------------------}


{----------------------------------------------------------------------}

//
procedure  TFMonitorObj.Calc_Alpha_for_PDNode(NodeNum:Integer);
//must be called after self gradient calc
// self gradient calc is in 'Update _pd_node_info'
var
  j,phase_num : integer;
  sum_Sij_j : double;
  TempAlpha : double;
begin
     //if phase_num=0 then
     //     result := 0.0 //alpha
     //else if phase_num=1 then
     //     result:= 0.0  //alpha1
     //     else if phase_num=2 then
     //          result := 0.0 //alpha2
     //          else if phase_num=3 then
     //               result := 0.0; //alpha3
     //update nodes information before calculating alpha_i
     //update_all_nodes_info;

     // calclate alpha
     with pnodeFMs^[NodeNum] do
      for phase_num:= 1 to vl_ndphases_dg do
         case phase_num of
         1: begin //phase A
                 TempAlpha := 0.0;//init as zero
                 sum_Sij_j := 0.0;
                 for j := 1 to Nodes do
                 //for j := 1 to NodeNumofDG-1 do
                 begin
                          //if pnodeFMs^[j].vl_phase_num=phase_num then
                          //begin
                                TempAlpha := TempAlpha + pcommmatrix^[(NodeNum-1)*nodes +j]*pnodeFMs^[j].vl_Alpha1_dg   ;
                                sum_Sij_j := sum_Sij_j + pcommmatrix^[(NodeNum-1)*nodes +j];
                          //end;
                 end;
                 //for j := NodeNumofDG + 1 to Nodes do
                 //begin
                 //         Alpha1 := Alpha1 + pcommmatrix^[(NodeNumofDG-1)*nodes +j]*pnodeFMs^[j].Alpha1   ;
                 //         sum_Sij_j := sum_Sij_j + pcommmatrix^[(NodeNumofDG-1)*nodes +j];
                 //end;
                 vl_Alpha1_dg := TempAlpha / sum_Sij_j;
                 vl_Alpha1_dg := vl_Alpha1_dg - vl_kcq_dg* vl_gradient1_dg;
                 //result := Alpha1;
            end;
         2: begin //phase B
                 TempAlpha := 0.0;//init as zero
                 sum_Sij_j := 0.0;
                 for j := 1 to Nodes do
                 //for j := 1 to NodeNumofDG-1 do
                 begin
                          TempAlpha := TempAlpha + pcommmatrix^[(NodeNum-1)*nodes +j]*pnodeFMs^[j].vl_Alpha2_dg   ;
                          sum_Sij_j := sum_Sij_j + pcommmatrix^[(NodeNum-1)*nodes +j];
                 end;
                 //for j := NodeNumofDG + 1 to Nodes do
                 //begin
                 //         Alpha2 := Alpha2 + pcommmatrix^[(NodeNumofDG-1)*nodes +j]*pnodeFMs^[j].Alpha2   ;
                 //         sum_Sij_j := sum_Sij_j + pcommmatrix^[(NodeNumofDG-1)*nodes +j];
                 //end;
                 vl_Alpha2_dg := TempAlpha / sum_Sij_j;
                 vl_Alpha2_dg := vl_Alpha2_dg - vl_kcq_dg* vl_gradient2_dg;
                 //result := Alpha2;
            end;
         3: begin //phase C
                 TempAlpha := 0.0;//init as zero
                 sum_Sij_j := 0.0;
                 for j := 1 to Nodes do
                 //for j := 1 to NodeNumofDG-1 do
                 begin
                          TempAlpha := TempAlpha + pcommmatrix^[(NodeNum-1)*nodes +j]*pnodeFMs^[j].vl_Alpha3_dg   ;
                          sum_Sij_j := sum_Sij_j + pcommmatrix^[(NodeNum-1)*nodes +j];
                 end;
                 //for j := NodeNumofDG + 1 to Nodes do
                 //begin
                 //         Alpha3 := Alpha3 + pcommmatrix^[(NodeNumofDG-1)*nodes +j]*pnodeFMs^[j].Alpha3   ;
                 //         sum_Sij_j := sum_Sij_j + pcommmatrix^[(NodeNumofDG-1)*nodes +j];
                 //end;
                 vl_Alpha3_dg := TempAlpha / sum_Sij_j;
                 vl_Alpha3_dg := vl_Alpha3_dg - vl_kcq_dg* vl_gradient3_dg;
                 //result := Alpha3;
            end;
         0: begin //pos seq value
                 TempAlpha := 0.0;//init as zero
                 sum_Sij_j := 0.0;
                 for j := 1 to Nodes do
                 //for j := 1 to NodeNumofDG-1 do
                 begin
                          TempAlpha := TempAlpha + pcommmatrix^[(NodeNum-1)*nodes +j]*pnodeFMs^[j].vl_Alpha_dg   ;
                          sum_Sij_j := sum_Sij_j + pcommmatrix^[(NodeNum-1)*nodes +j];
                 end;
                 //for j := NodeNumofDG + 1 to Nodes do
                 //begin
                 //         Alpha := Alpha + pcommmatrix^[(NodeNumofDG-1)*nodes +j]*pnodeFMs^[j].Alpha   ;
                 //         sum_Sij_j := sum_Sij_j + pcommmatrix^[(NodeNumofDG-1)*nodes +j];
                 //end;
                 vl_Alpha_dg := TempAlpha / sum_Sij_j;
                 vl_Alpha_dg := vl_Alpha_dg - vl_kcq_dg* vl_gradient_dg;
                 //result := Alpha;
            end;
         else
         end;
end;
{------------------------}
//////////////////////////
Function TFMonitorObj.Get_P_mode( ActorID : Integer):integer;
begin
     result := p_mode;
end;
// AlphaP Gradient or Pref
Function TFMonitorObj.get_power_trans:Double;  // NodeNuminClstr: node number in cluster
var
  i,j,k: integer;
  pTerminal : TPowerTerminal;
  Curr: pComplexArray;
begin
      //tempCplx := MeteredElement.power[MeteredTerminal];
      TPDElement(MeteredElement).GetCurrents(MeteredElement.Iterminal); //Curr
      pTerminal := MeteredElement.Terminals[MeteredTerminal - 1];
      tempCplx := CZERO;
      k := (MeteredTerminal -1)*MeteredElement.NConds;
      for j:=1 to MeteredElement.NConds do// how many conds of this element
             begin
                  i := pTerminal.TermNodeRef[j - 1];  // global node number
                  CACCUM( tempCplx , cmul(activecircuit.Solution.NodeV^[i], Conjg(MeteredElement.Iterminal[k+j])));//power
             end;
      result :=  tempCplx.re ;
end;
//calculate the gradient for alpha i
Function  TFMonitorObj.Calc_Grdt_for_Alpha(NodeNuminClstr, phase_num,ActorID : Integer):Double;
var
      Vtemp, ctmp : complex;
      tmp : double;
      Gij, Bij, Gii, Bii : double;
      Devindex, i, j, k, jTempTerminal: integer;
      pElem  :TDSSCktElement   ;
      nodeRefi : integer;// ref number of this node
      nodeRefj : integer;// ref number of the upper node
      den : double;
begin
      //pNodeFMs^[NodeNuminClstr].vl_strMeasuredName;//element followed by this bus
      Devindex := GetCktElementIndex(pNodeFMs^[NodeNuminClstr].vl_strMeasuredName);
      IF DevIndex>0 THEN Begin                                       // Monitored element must already exist
          pElem := ActiveCircuit.CktElements.Get(DevIndex);
      end;
      if pElem <> nil //want to get voltages from the other side of the device
      then Begin
          With ActiveCircuit.solution Do
              For i := 1 to pElem.Yorder Do
                  pElem.Vterminal^[i] := NodeV^[pElem.NodeRef^[i]];
      End
          else result := 0.0 ;
      //k is the terminal number of this end
      k :=  pNodeFMs^[NodeNuminClstr].vl_terminalNum;
      //this is the other end jTempTerminal
      if k=1 then jTempTerminal := 2 else jTempTerminal := 1;
      //k := (iTempTerminal -1)*MeteredElement.NConds;
      //
      //find the voltage of this phase on this terminal
      for i:=1 to pElem.NPhases do// how many conds of this element
      begin
           j := pElem.Terminals[jTempTerminal - 1].TermNodeRef[i - 1];
           if activecircuit.MapNodeToBus^[j].NodeNum = phase_num then
           begin
                nodeRefj := j;                                   // node ref of the other end of this element and this phase
                vTemp := activecircuit.Solution.NodeV^[nodeRefj];
                nodeRefi := pElem.Terminals[k - 1].TermNodeRef[i - 1]; // node ref of this node
           end;
      end;
      if phase_num=0 then //  cannot deal with pos seq
      begin

      end;
      //
      ctmp :=  activecircuit.Solution.Get_Yij(nodeRefi,nodeRefj) ;
      Gij := ctmp.re;
      Bij := ctmp.im;
      ctmp :=  activecircuit.Solution.Get_Yij(nodeRefi,nodeRefi) ;
            //ctmp := activecircuit.Solution.NodeYii^[nodeRefi];
      Gii := ctmp.re;
      Bii := ctmp.im;

      with pNodeFMs^[NodeNuminClstr] do
      case phase_num of //pos seq
         0: begin
              result := vl_gradient_dg; //  can not deal with that
            end;
         1: begin
              {den := vl_Q_DG1 - vl_Q_Di1- vl_V1*vl_V1* Bii;   // pos ctrl: Bii use the first one
              tmp := vl_V1-cabs(vTemp);

              vl_gradient1_dg := vl_V1 * tmp;

              if abs(den)<epsilon then vl_gradient1_dg := 0.0
              else
                vl_gradient1_dg := vl_gradient1_dg /(den);
              }
              den := vl_Q_DG1 - vl_Q_Di1- vl_V1*vl_V1* Bii;   // pos ctrl: Bii use the first one
              tmp := (vl_V1-cabs(vTemp)*cos(cang(vTemp)-cang(vl_V_1c)));

              vl_gradient1_dg := vl_V1 * tmp;

              if abs(den)<epsilon then vl_gradient1_dg := 0.0
              else
                vl_gradient1_dg := vl_gradient1_dg /(den);

              den := vl_P_DG1 - vl_P_Di1- vl_V1*vl_V1* Gii;
              tmp := vl_V1*cabs(vTemp)*sin(cang(vTemp)-cang(vl_V_1c)) ;
              if abs(den)<epsilon then tmp :=0 else
                tmp := tmp/den;
              vl_gradient1_dg :=vl_gradient1_dg - tmp;

              {
              den := vl_V1*(Gij * sin(cang(vl_V_1c)-cang(vTemp)) - Bij * cos(cang(vl_V_1c)-cang(vTemp)));
              tmp := cabs(vTemp) - vl_V1*cos(cang(vTemp)-cang(vl_V_1c));
              if abs(den)<epsilon then tmp :=0 else
                tmp := tmp/den;
              vl_gradient1_dg :=vl_gradient1_dg + tmp;

              den := Gij * cos(cang(vl_V_1c)-cang(vTemp)) + Bij * sin(cang(vl_V_1c)-cang(vTemp));
              tmp := sin(cang(vTemp)-cang(vl_V_1c));
              if abs(den)<epsilon then tmp :=0 else
                tmp := tmp/den;
              vl_gradient1_dg :=vl_gradient1_dg - tmp;
              }

              tmp := vl_Qmax_phase_dg * Gij;
              vl_gradient1_dg := vl_gradient1_dg * tmp;
              vl_gradient1_dg := vl_gradient1_dg /(vl_V_ref1_dg*vl_V_ref1_dg);//PU value??
             //////////////////////////////////////////////////////////
             //vl_gradient_dg := (vl_V_ref_dg-vl_v)*vl_V/(den)/(vl_V_ref_dg*vl_V_ref_dg);  //*vl_Qmax, 0311-by dahei
             //       vl_gradient_dg := (beta*vl_V_ref_dg*vl_V_ref_dg* abs(Bii)*100/j)*vl_gradient_dg;
             ////////////////////////////////////////////////////////////////////////////////////
              result := vl_gradient1_dg; //
            end;
         2: begin
               {den := vl_Q_DG2 - vl_Q_Di2- vl_V2*vl_V2* Bii;   // pos ctrl: Bii use the first one
              tmp := vl_V2-cabs(vTemp);

              vl_gradient2_dg := vl_V2 * tmp;

              if abs(den)<epsilon then vl_gradient2_dg := 0.0
              else
                vl_gradient2_dg := vl_gradient2_dg /(den);

              }
              den := vl_Q_DG2 - vl_Q_Di2- vl_V2*vl_V2* Bii;   // pos ctrl: Bii use the first one
              tmp := (vl_V2-cabs(vTemp)*cos(cang(vTemp)-cang(vl_V_2c)));

              vl_gradient2_dg := vl_V2 * tmp;

              if abs(den)<epsilon then vl_gradient2_dg := 0.0
              else
                vl_gradient2_dg := vl_gradient2_dg /(den);

              den := vl_P_DG2 - vl_P_Di2- vl_V2*vl_V2* Gii;
              tmp := vl_V2*cabs(vTemp)*sin(cang(vTemp)-cang(vl_V_2c)) ;
              if abs(den)<epsilon then tmp :=0 else
                tmp := tmp/den;
              vl_gradient2_dg :=vl_gradient2_dg - tmp;

              {
              den := vl_V2*(Gij * sin(cang(vl_V_2c)-cang(vTemp)) - Bij * cos(cang(vl_V_2c)-cang(vTemp)));
              tmp := cabs(vTemp) - vl_V2*cos(cang(vTemp)-cang(vl_V_2c));
              if abs(den)<epsilon then tmp :=0 else
                tmp := tmp/den;
              vl_gradient2_dg :=vl_gradient2_dg + tmp;

              den := Gij * cos(cang(vl_V_2c)-cang(vTemp)) + Bij * sin(cang(vl_V_2c)-cang(vTemp));
              tmp := sin(cang(vTemp)-cang(vl_V_2c));
              if abs(den)<epsilon then tmp :=0 else
                tmp := tmp/den;
              vl_gradient2_dg :=vl_gradient2_dg - tmp;
               }

              tmp := vl_Qmax_phase_dg * Gij;
              vl_gradient2_dg := vl_gradient2_dg * tmp;
              vl_gradient2_dg := vl_gradient2_dg /(vl_V_ref2_dg*vl_V_ref2_dg);//PU value??

            end;
         3: begin
              {den := vl_Q_DG3 - vl_Q_Di3- vl_V3*vl_V3* Bii;   // pos ctrl: Bii use the first one
              tmp := vl_V3-cabs(vTemp) ;

              vl_gradient3_dg := vl_V3 * tmp;

              if abs(den)<epsilon then vl_gradient3_dg := 0.0
              else
                vl_gradient3_dg := vl_gradient3_dg /(den);

               }
              den := vl_Q_DG3 - vl_Q_Di3- vl_V3*vl_V3* Bii;   // pos ctrl: Bii use the first one
              tmp := (vl_V3-cabs(vTemp)*cos(cang(vTemp)-cang(vl_V_3c)));

              vl_gradient3_dg := vl_V3 * tmp;

              if abs(den)<epsilon then vl_gradient3_dg := 0.0
              else
                vl_gradient3_dg := vl_gradient3_dg /(den);

              den := vl_P_DG3 - vl_P_Di3- vl_V3*vl_V3* Gii;
              tmp := vl_V3*cabs(vTemp)*sin(cang(vTemp)-cang(vl_V_3c)) ;
              if abs(den)<epsilon then tmp :=0 else
                tmp := tmp/den;
              vl_gradient3_dg :=vl_gradient3_dg - tmp;

              {
              den := vl_V3*(Gij * sin(cang(vl_V_3c)-cang(vTemp)) - Bij * cos(cang(vl_V_3c)-cang(vTemp)));
              tmp := cabs(vTemp) - vl_V3*cos(cang(vTemp)-cang(vl_V_3c));
              if abs(den)<epsilon then tmp :=0 else
                tmp := tmp/den;
              vl_gradient3_dg :=vl_gradient3_dg + tmp;

              den := Gij * cos(cang(vl_V_3c)-cang(vTemp)) + Bij * sin(cang(vl_V_3c)-cang(vTemp));
              tmp := sin(cang(vTemp)-cang(vl_V_3c));
              if abs(den)<epsilon then tmp :=0 else
                tmp := tmp/den;
              vl_gradient3_dg :=vl_gradient3_dg - tmp;
              }

              tmp := vl_Qmax_phase_dg * Gij;
              vl_gradient3_dg := vl_gradient3_dg * tmp;
              vl_gradient3_dg := vl_gradient3_dg /(vl_V_ref3_dg*vl_V_ref3_dg);//PU value??

            end;
         end;
      //result :=  tempCplx.re ;

end;

//calculate the gradient for alpha i
Function  TFMonitorObj.Calc_Grdt_for_Alpha_vivj(NodeNuminClstr, phase_num,ActorID : Integer):Double;
var
      Vtemp, ctmp : complex;
      tmp : double;
      Gij, Bij, Gii, Bii : double;
      Devindex, i, j, k, jTempTerminal: integer;
      pElem  :TDSSCktElement   ;
      nodeRefi : integer;// ref number of this node
      nodeRefj : integer;// ref number of the upper node
      den : double;
begin
      //pNodeFMs^[NodeNuminClstr].vl_strMeasuredName;//element followed by this bus
      Devindex := GetCktElementIndex(pNodeFMs^[NodeNuminClstr].vl_strMeasuredName);
      IF DevIndex>0 THEN Begin                                       // Monitored element must already exist
          pElem := ActiveCircuit.CktElements.Get(DevIndex);
      end;
      if pElem <> nil //want to get voltages from the other side of the device
      then Begin
          With ActiveCircuit.solution Do
              For i := 1 to pElem.Yorder Do
                  pElem.Vterminal^[i] := NodeV^[pElem.NodeRef^[i]];
      End
          else result := 0.0 ;
      //k is the terminal number of this end
      k :=  pNodeFMs^[NodeNuminClstr].vl_terminalNum;
      //this is the other end jTempTerminal
      if k=1 then jTempTerminal := 2 else jTempTerminal := 1;
      //k := (iTempTerminal -1)*MeteredElement.NConds;
      //
      //find the voltage of this phase on this terminal
      for i:=1 to pElem.NPhases do// how many conds of this element
      begin
           j := pElem.Terminals[jTempTerminal - 1].TermNodeRef[i - 1];
           if activecircuit.MapNodeToBus^[j].NodeNum = phase_num then
           begin
                nodeRefj := j;                                   // node ref of the other end of this element and this phase
                vTemp := activecircuit.Solution.NodeV^[nodeRefj];
                nodeRefi := pElem.Terminals[k - 1].TermNodeRef[i - 1]; // node ref of this node
           end;
      end;
      if phase_num=0 then //  cannot deal with pos seq
      begin

      end;
      //
      ctmp :=  activecircuit.Solution.Get_Yij(nodeRefi,nodeRefj) ;
      Gij := ctmp.re;
      Bij := ctmp.im;
      ctmp :=  activecircuit.Solution.Get_Yij(nodeRefi,nodeRefi) ;
            //ctmp := activecircuit.Solution.NodeYii^[nodeRefi];
      Gii := ctmp.re;
      Bii := ctmp.im;

      with pNodeFMs^[NodeNuminClstr] do
      case phase_num of //pos seq
         0: begin
              result := vl_gradient_dg; //  can not deal with that
            end;
         1: begin
              den := vl_Q_DG1 - vl_Q_Di1- vl_V1*vl_V1* Bii;   // pos ctrl: Bii use the first one
              tmp := vl_V1-cabs(vTemp);

              vl_gradient1_dg := vl_V1 * tmp;

              if abs(den)<epsilon then vl_gradient1_dg := 0.0
              else
                vl_gradient1_dg := vl_gradient1_dg /(den);

              tmp := vl_Qmax_phase_dg * Gij;
              vl_gradient1_dg := vl_gradient1_dg * tmp;
              vl_gradient1_dg := vl_gradient1_dg /(vl_V_ref1_dg*vl_V_ref1_dg);//PU value??
             //////////////////////////////////////////////////////////
             //vl_gradient_dg := (vl_V_ref_dg-vl_v)*vl_V/(den)/(vl_V_ref_dg*vl_V_ref_dg);  //*vl_Qmax, 0311-by dahei
             //       vl_gradient_dg := (beta*vl_V_ref_dg*vl_V_ref_dg* abs(Bii)*100/j)*vl_gradient_dg;
             ////////////////////////////////////////////////////////////////////////////////////
              result := vl_gradient1_dg; //
            end;
         2: begin
              den := vl_Q_DG2 - vl_Q_Di2- vl_V2*vl_V2* Bii;   // pos ctrl: Bii use the first one
              tmp := vl_V2-cabs(vTemp);

              vl_gradient2_dg := vl_V2 * tmp;

              if abs(den)<epsilon then vl_gradient2_dg := 0.0
              else
                vl_gradient2_dg := vl_gradient2_dg /(den);

              tmp := vl_Qmax_phase_dg * Gij;
              vl_gradient2_dg := vl_gradient2_dg * tmp;
              vl_gradient2_dg := vl_gradient2_dg /(vl_V_ref2_dg*vl_V_ref2_dg);//PU value??

            end;
         3: begin
              den := vl_Q_DG3 - vl_Q_Di3- vl_V3*vl_V3* Bii;   // pos ctrl: Bii use the first one
              tmp := vl_V3-cabs(vTemp) ;

              vl_gradient3_dg := vl_V3 * tmp;

              if abs(den)<epsilon then vl_gradient3_dg := 0.0
              else
                vl_gradient3_dg := vl_gradient3_dg /(den);

              tmp := vl_Qmax_phase_dg * Gij;
              vl_gradient3_dg := vl_gradient3_dg * tmp;
              vl_gradient3_dg := vl_gradient3_dg /(vl_V_ref3_dg*vl_V_ref3_dg);//PU value??

            end;
         end;
      //result :=  tempCplx.re ;

end;


Function TFMonitorObj.Calc_GP_AlphaP(NodeNuminClstr, phase_num, ActorID:Integer):Double;  // NodeNuminClstr: node number in cluster
var
  PGtemp, ptemp : double;

begin
       //tempCplx
      ptemp := get_power_trans;  // get power on trans point
      //tempCplx := cnegate(tempCplx);//
      //if pNodeFMs^[NodeNuminClstr].vl_V_ref_dg<>0 then
      //  PGtemp  := -(p_trans_ref - ptemp )/Activecircuit.solution.t;//pNodeFMs^[NodeNuminClstr].vl_V_ref_dg // pu value?
      //else
      //if pNodeFMs^[NodeNuminClstr].vl_Pmax_dg<>0.0 then
        //PGtemp  := -(p_trans_ref - ptemp )//-(p_trans_ref - ptemp )/pNodeFMs^[NodeNuminClstr].vl_Pmax_dg  // should use the total load
        //else  PGtemp  := 0.0 ;
      PGtemp  := -(p_trans_ref - ptemp );
      if PGtemp>0 then PGtemp := 1
      else if PGtemp<0 then PGtemp := -1
      else PGtemp := 0.0;
         case phase_num of //pos seq
         0: begin
              result := PGtemp; //
            end;
         1: begin
              result := PGtemp; //
            end;
         2: begin
              result := PGtemp; //
            end;
         3: begin
              result := PGtemp; //
            end;
         end;
end;
//////////////////////////
{-------------------------}
Function TFMonitorObj.Calc_AlphaP(NodeNuminClstr, phase_num,ActorID:Integer):Double;  // NodeNuminClstr: node number in cluster
var
  j : integer;
  den_dij,TempAlpha : Double;

begin
     //alphaP = sum (alphaP) + Beta * Gp

     with pNodeFMs^[NodeNuminClstr] do
      begin
         case phase_num of //pos seq
         0:  begin
              //1.calculate d_ij*alpha_j summation
                den_dij := 0;
                TempAlpha := 0;
                for j := 1 to Nodes do
                   begin
                            if (pnodeFMs^[j].vl_ndphases_dg = 3) then   //only 3 phase nodes
                            begin
                                    //if pnodeFMs^[j].vl_nodeType = 1 then // only DG nodes
                                    //begin
                                        den_dij := den_dij+pCommMatrix^[(NodeNuminClstr-1)*Nodes+ j];
                                        TempAlpha := TempAlpha + pcommmatrix^[(NodeNuminClstr-1)*nodes +j]*pnodeFMs^[j].vl_AlphaP_dg;
                                    //end;
                            end;
                   end;
                if den_dij=0  then  TempAlpha := 0.0 else
                begin
                     TempAlpha := TempAlpha/den_dij;
                end;
                vl_gradient_dg := Calc_GP_AlphaP(NodeNuminClstr,phase_num);
                vl_alphaP_dg :=TempAlpha + vl_kcd_dg * vl_gradient_dg/activecircuit.Solution.Iteration;
                if vl_alphaP_dg >1 then vl_alphaP_dg := 1;
                if vl_alphaP_dg <-1 then vl_alphaP_dg := -1;
                result := vl_alphaP_dg;
            end;
         1:  begin
              //1.calculate d_ij*alpha_j summation
                den_dij := 0;
                TempAlpha := 0;
                for j := 1 to Nodes do
                   begin
                            if (pnodeFMs^[j].vl_ndphases_dg = 3)     //only count dgs with 3 phases or 1 phase that is same number
                            or (pnodeFMs^[j].vl_phase_num_dg = phase_num) then
                            begin
                              //if pnodeFMs^[j].vl_nodeType = 1 then // only DG nodes
                              //begin
                                  den_dij := den_dij+pCommMatrix^[(NodeNuminClstr-1)*Nodes+ j]  ;
                                  TempAlpha := TempAlpha + pcommmatrix^[(NodeNuminClstr-1)*nodes +j]*pnodeFMs^[j].vl_AlphaP1_dg;
                              //end;
                            end;
                   end;
                if den_dij=0  then  TempAlpha := 0.0 else
                begin
                     TempAlpha := TempAlpha/den_dij;
                end;
                vl_gradientP1_dg := Calc_GP_AlphaP(NodeNuminClstr,phase_num);
                vl_alphaP1_dg :=TempAlpha + vl_kcd_dg * vl_gradientP1_dg/activecircuit.Solution.Iteration;
                if vl_alphaP1_dg >1 then vl_alphaP1_dg := 1;
                if vl_alphaP1_dg <-1 then vl_alphaP1_dg := -1;
                result := vl_alphaP1_dg;
            end;
         2: begin
                 //1.calculate d_ij*alpha_j summation
                  den_dij := 0;
                  TempAlpha := 0;
                  for j := 1 to Nodes do
                     begin
                              if (pnodeFMs^[j].vl_ndphases_dg = 3)     //only count dgs with 3 phases or 1 phase that is same number
                              or (pnodeFMs^[j].vl_phase_num_dg = phase_num) then
                              begin
                                //if pnodeFMs^[j].vl_nodeType = 1 then // only DG nodes
                                //begin
                                    den_dij := den_dij+pCommMatrix^[(NodeNuminClstr-1)*Nodes+ j];
                                    TempAlpha := TempAlpha + pcommmatrix^[(NodeNuminClstr-1)*nodes +j]*pnodeFMs^[j].vl_AlphaP2_dg;
                                //end;
                              end;
                     end;
                  if den_dij=0  then  TempAlpha := 0.0 else
                  begin
                      TempAlpha := TempAlpha/den_dij;
                  end;
                  vl_gradientP2_dg := Calc_GP_AlphaP(NodeNuminClstr,phase_num);
                  vl_alphaP2_dg :=TempAlpha + vl_kcd_dg * vl_gradientP2_dg/activecircuit.Solution.Iteration;
                  if vl_alphaP2_dg >1 then vl_alphaP2_dg := 1;
                  if vl_alphaP2_dg <-1 then vl_alphaP2_dg := -1;
                  result := vl_alphaP2_dg;
            end;
         3:  begin
                  //1.calculate d_ij*alpha_j summation
                  den_dij := 0;
                  TempAlpha := 0;
                  //for i := 1 to nodes do den_dij := den_dij+pCommMatrix^[(NodeNumofDG-1)*Nodes+ i]; //4.is this correct

                  for j := 1 to Nodes do
                     begin
                              if (pnodeFMs^[j].vl_ndphases_dg = 3)     //only count dgs with 3 phases or 1 phase that is same number
                              or (pnodeFMs^[j].vl_phase_num_dg = phase_num) then
                              begin
                                //if pnodeFMs^[j].vl_nodeType = 1 then // only DG nodes
                                //begin
                                  den_dij := den_dij+pCommMatrix^[(NodeNuminClstr-1)*Nodes+ j];
                                  TempAlpha := TempAlpha + pcommmatrix^[(NodeNuminClstr-1)*nodes +j]*pnodeFMs^[j].vl_AlphaP3_dg;
                                //end;
                              end;
                     end;
                   if den_dij=0  then  TempAlpha := 0.0 else
                   begin
                       TempAlpha := TempAlpha/den_dij;
                  end;
                  vl_gradientP3_dg := Calc_GP_AlphaP(NodeNuminClstr,phase_num);
                  vl_alphaP3_dg :=TempAlpha + vl_kcd_dg * vl_gradientP3_dg/activecircuit.Solution.Iteration;
                  if vl_alphaP3_dg >1 then vl_alphaP3_dg := 1;
                  if vl_alphaP3_dg <-1 then vl_alphaP3_dg := -1;
                  result := vl_alphaP3_dg;
            end;
        end;
     end;
end;
{----------------------------------------------------------------------------------}
//only work for Generic5 nodefm
Function TFMonitorObj.Calc_Alpha_M2(NodeNumofDG, phase_num:Integer; dbNodeRef: integer; Bii,beta,Volt_Trhd: double):Double;
//NodeNumofDG = NodeNuminClstr
var
  i,j : integer;
  sum_Sij_j, den : double;
  alpha:double;
  den_dij,dii,TempAlpha : Double;
  tmp1, tmp2, tmp3, tmp4, tmp5, tmp6, tmp7: double;

begin
     //update_all_nodes_info;
     //needs to update Q_Di here(Find a way)
     {

     1.need beta or kcq here
     2. is Bii calculated for each phases
     3.what is phase 0?
     4.in the function look at communication vector

     }

     //j := ActiveCircuit.Solution.Iteration;
      
      update_all_nodes_info;     // update voltages on all buses

      with pNodeFMs^[NodeNumofDG] do
      begin
         case phase_num of //pos seq
         0:  begin
              //1.calculate d_ij*alpha_j summation
                den_dij := 0;
                TempAlpha := 0;


                for j := 1 to Nodes do
                   begin
                            if (pnodeFMs^[j].vl_ndphases_dg = 3) then   //only 3 phase nodes
                            begin
                                    //if pnodeFMs^[j].vl_nodeType = 1 then // only DG nodes
                                    //begin
                                        den_dij := den_dij+pCommMatrix^[(NodeNumofDG-1)*Nodes+ j];
                                        TempAlpha := TempAlpha + pcommmatrix^[(NodeNumofDG-1)*nodes +j]*pnodeFMs^[j].vl_Alpha_dg;
                                    //end;
                            end;
                            
                   end;
                if den_dij=0  then  TempAlpha := 0.0 else
                begin
                     TempAlpha := TempAlpha/den_dij;

                    //2.calculate gradient----------------
                    //Bii := ActiveCircuit.Solution.NodeYii^[dbNodeRef].im;
                    //Load.ActiveLoadObj.kvarBase;
                    den := vl_Q_DG - vl_Q_Di- vl_V*vl_V* Bii;   // pos ctrl: Bii use the first one
                    if abs(den)<epsilon then den := epsilon ;
                    vl_gradient_dg := (vl_V_ref_dg-vl_v)*vl_V/(den)/(vl_V_ref_dg*vl_V_ref_dg);  //*vl_Qmax, 0311-by dahei
                    j :=  ActiveCircuit.Solution.Iteration;
                    vl_gradient_dg := (beta*vl_V_ref_dg*vl_V_ref_dg* abs(Bii)*100/j)*vl_gradient_dg;
                    if abs(vl_V_ref_dg-vl_v)<= Volt_Trhd*vl_V_ref_dg then vl_gradient_dg := 0.0;

                    //calculate final alpha----------------
                end;
                alpha :=vl_kc_ul_dg * TempAlpha +  vl_gradient_dg;
                if alpha >1 then alpha := 1;
                if alpha <-1 then alpha := -1;
                vl_Alpha_dg := alpha;
                result := alpha;
            end;
         1:  begin
              //1.calculate d_ij*alpha_j summation
                den_dij := 0;
                TempAlpha := 0;
                //for i := 1 to nodes do den_dij := den_dij+pCommMatrix^[(NodeNumofDG-1)*Nodes+ i]; //4.is this correct
                //tmp1 := Nodes;
                for j := 1 to Nodes do
                   begin
                            if (pnodeFMs^[j].vl_ndphases_dg = 3)     //only count dgs with 3 phases or 1 phase that is same number
                            //or (pnodeFMs^[j].vl_phase_num_dg = phase_num) then
                            or (pnodeFMs^[j].vl_nodeType_phase[1] = 1) then //this phase has DG
                            begin
                              //if pnodeFMs^[j].vl_nodeType = 1 then // only DG nodes
                              //begin
                                  den_dij := den_dij+pCommMatrix^[(NodeNumofDG-1)*Nodes+ j]  ;
                                  TempAlpha := TempAlpha + pcommmatrix^[(NodeNumofDG-1)*nodes +j]*pnodeFMs^[j].vl_Alpha1_dg;
                              //end;
                            end;

                   end;
                if den_dij=0  then  TempAlpha := 0.0 else
                begin
                     TempAlpha := TempAlpha/den_dij;
                    //2.calculate gradient----------------
                    //Bii := ActiveCircuit.Solution.NodeYii^[dbNodeRef].im;
                    den := vl_Q_DG1 - vl_Q_Di1- vl_V1*vl_V1* Bii;   // pos ctrl: Bii use the first one
                    if abs(den)<epsilon then den := epsilon ;

                    tmp1 :=  vl_Q_DG1;
                    tmp2 :=  vl_Q_Di1;
                    tmp3 :=  vl_V1;
                    tmp4 :=  vl_Qmax_dg  ;
                    tmp5 :=  vl_V_ref1_dg;
                    j :=  ActiveCircuit.Solution.Iteration;
                    vl_gradient1_dg := (vl_V_ref1_dg-vl_v1)*vl_V1/(den)/(vl_V_ref1_dg*vl_V_ref1_dg);   //*vl_Qmax, 0311-by dahei
                    vl_gradient1_dg := (beta*vl_V_ref1_dg*vl_V_ref1_dg* abs(Bii)*100/j)*vl_gradient1_dg;
                    //vl_gradient1 := (beta)*vl_gradient1;
                    if abs(vl_V_ref1_dg-vl_v1)<= Volt_Trhd*vl_V_ref1_dg then vl_gradient1_dg := 0.0;
                    //calculate final alpha----------------
                end;
                vl_alpha1_dg :=TempAlpha +  vl_gradient1_dg;
                tmp6 := vl_gradient1_dg;
                tmp7 := vl_alpha1_dg;
                if vl_alpha1_dg >1 then vl_alpha1_dg := 1;
                if vl_alpha1_dg <-1 then vl_alpha1_dg := -1;
                result := vl_alpha1_dg;
            end;
         2: begin
                 //1.calculate d_ij*alpha_j summation
                  den_dij := 0;
                  TempAlpha := 0;
                  //for i := 1 to nodes do den_dij := den_dij+pCommMatrix^[(NodeNumofDG-1)*Nodes+ i]; //4.is this correct

                  for j := 1 to Nodes do
                     begin
                              if (pnodeFMs^[j].vl_ndphases_dg = 3)     //only count dgs with 3 phases or 1 phase that is same number
                              //or (pnodeFMs^[j].vl_phase_num_dg = phase_num) then
                              or (pnodeFMs^[j].vl_nodeType_phase[2] = 1) then //this phase has DG
                              begin
                                //if pnodeFMs^[j].vl_nodeType = 1 then // only DG nodes
                                //begin
                                    den_dij := den_dij+pCommMatrix^[(NodeNumofDG-1)*Nodes+ j];
                                    TempAlpha := TempAlpha + pcommmatrix^[(NodeNumofDG-1)*nodes +j]*pnodeFMs^[j].vl_Alpha2_dg;
                                //end;
                              end;
                     end;
                  if den_dij=0  then  TempAlpha := 0.0 else
                  begin
                      TempAlpha := TempAlpha/den_dij;

                      //2.calculate gradient----------------
                      //Bii := ActiveCircuit.Solution.NodeYii^[dbNodeRef].im;
                      den := vl_Q_DG2 - vl_Q_Di2- vl_V2*vl_V2* Bii;   // pos ctrl: Bii use the first one
                      if abs(den)<epsilon then den := epsilon ;
                      j :=  ActiveCircuit.Solution.Iteration;
                      vl_gradient2_dg := (vl_V_ref2_dg-vl_v2)*vl_V2/(den)/(vl_V_ref2_dg*vl_V_ref2_dg);        //*vl_Qmax
                      vl_gradient2_dg := (beta*vl_V_ref2_dg*vl_V_ref2_dg* abs(Bii)*100/j)*vl_gradient2_dg;
                      //vl_gradient2 := (beta)*vl_gradient2;
                       if abs(vl_V_ref2_dg-vl_v2)<= Volt_Trhd*vl_V_ref2_dg then vl_gradient2_dg := 0.0;
                      //calculate final alpha----------------
                  end;
                  vl_alpha2_dg :=TempAlpha +  vl_gradient2_dg;
                  if vl_alpha2_dg >1 then vl_alpha2_dg := 1;
                  if vl_alpha2_dg <-1 then vl_alpha2_dg := -1;
                  result := vl_alpha2_dg;
            end;
         3:  begin
                  //1.calculate d_ij*alpha_j summation
                  den_dij := 0;
                  TempAlpha := 0;
                  //for i := 1 to nodes do den_dij := den_dij+pCommMatrix^[(NodeNumofDG-1)*Nodes+ i]; //4.is this correct

                  for j := 1 to Nodes do
                     begin
                              if (pnodeFMs^[j].vl_ndphases_dg = 3)     //only count dgs with 3 phases or 1 phase that is same number
                              //or (pnodeFMs^[j].vl_phase_num_dg = phase_num) then
                              or (pnodeFMs^[j].vl_nodeType_phase[3] = 1) then //this phase has DG
                              begin
                                //if pnodeFMs^[j].vl_nodeType = 1 then // only DG nodes
                                //begin
                                  den_dij := den_dij+pCommMatrix^[(NodeNumofDG-1)*Nodes+ j];
                                  TempAlpha := TempAlpha + pcommmatrix^[(NodeNumofDG-1)*nodes +j]*pnodeFMs^[j].vl_Alpha3_dg;
                                //end;
                              end;
                     end;
                   if den_dij=0  then  TempAlpha := 0.0 else
                   begin
                       TempAlpha := TempAlpha/den_dij;

                      //2.calculate gradient----------------
                      //Bii := ActiveCircuit.Solution.NodeYii^[dbNodeRef].im;
                      den := vl_Q_DG3 - vl_Q_Di3- vl_V3*vl_V3* Bii;   // pos ctrl: Bii use the first one
                      if abs(den)<epsilon then den := epsilon ;
                      j :=  ActiveCircuit.Solution.Iteration;
                      vl_gradient3_dg := (vl_V_ref3_dg-vl_v3)*vl_V3/(den)/(vl_V_ref3_dg*vl_V_ref3_dg);        //*vl_Qmax
                      vl_gradient3_dg := (beta* vl_V_ref3_dg*vl_V_ref3_dg*abs(Bii)*100/j)*vl_gradient3_dg;
                      //vl_gradient3 := (beta)*vl_gradient3;
                      if abs(vl_V_ref3_dg-vl_v3)<= Volt_Trhd*vl_V_ref3_dg then vl_gradient3_dg := 0.0;
                      //calculate final alpha----------------
                  end;
                  vl_alpha3_dg :=TempAlpha +  vl_gradient3_dg;
                  if vl_alpha3_dg >1 then vl_alpha3_dg := 1;
                  if vl_alpha3_dg <-1 then vl_alpha3_dg := -1;
                  result := vl_alpha3_dg;
            end;
        end;
     end;
     //result := 0;
end;
//will be call in Generic5
//calculate subgradient for DG 'NodeNumofDG' phase 'phase_num'

Function TFMonitorObj.Calc_Alpha_LnM2(NodeNumofDG, phase_num:Integer; dbNodeRef: integer; Bii,beta,Volt_Trhd: double):Double;
Var
  Lambda0, Lambda, tmp, tmp1 : double;
begin
     Lambda0 := 0.1;
     Lambda := 1.0;
     tmp := 0.0;
     //tmp := Calc_Alpha_L(NodeNumofDG, phase_num, dbNodeRef, Bii,beta,Volt_Trhd) ;
     tmp1 := Calc_Alpha_M2(NodeNumofDG, phase_num, dbNodeRef, Bii,beta,Volt_Trhd) ;
     result := (1-Lambda) *tmp + Lambda * tmp1;
     //result := Calc_Alpha_L_vivj(NodeNumofDG, phase_num, dbNodeRef, Bii,beta,Volt_Trhd) ;
end;
Function TFMonitorObj.Calc_Alpha_L_vivj(NodeNumofDG, phase_num:Integer; dbNodeRef: integer; Bii,beta,Volt_Trhd: double ):Double;
Var
  i,j : integer;
  sum_Sij_j, den : double;
  alpha:double;
  den_dij,dii,TempAlpha ,tmp: Double;
  dynBeta : double;
  //tmp1, tmp2, tmp3, tmp4, tmp5, tmp6, tmp7: double;
begin
      update_all_nodes_info;     // update voltages on all buses
      Get_PQ_DI( NodeNumofDG) ; // load measure
      // calclate alpha
     //with pnodeFMs^[NodeNumofDG] do
         case phase_num of
         1: begin //phase A
              //1.calculate d_ij*alpha_j summation
                den_dij := 0;
                TempAlpha := 0;
                //for i := 1 to nodes do den_dij := den_dij+pCommMatrix^[(NodeNumofDG-1)*Nodes+ i]; //4.is this correct
                //tmp1 := Nodes;
                for j := 1 to Nodes do
                   begin
                            if (pnodeFMs^[j].vl_ndphases_dg = 3)     //only count dgs with 3 phases or 1 phase that is same number
                            //or (pnodeFMs^[j].vl_phase_num_dg = phase_num) then
                            or (pnodeFMs^[j].vl_nodeType_phase[1] = 1) then //this phase has DG
                            begin
                              //if pnodeFMs^[j].vl_nodeType = 1 then // only DG nodes
                              //begin
                                  den_dij := den_dij+pCommMatrix^[(NodeNumofDG-1)*Nodes+ j]  ;
                                  TempAlpha := TempAlpha + pcommmatrix^[(NodeNumofDG-1)*nodes +j]*pnodeFMs^[j].vl_Alpha1_dg;
                              //end;
                            end;

                   end;
                if den_dij=0  then  TempAlpha := 0.0 else
                begin
                     TempAlpha := TempAlpha/den_dij;
                    //2.calculate gradient----------------
                    //vl_gradient1_dg := (vl_V_ref1_dg-vl_v1)*vl_V1/(den)/(vl_V_ref1_dg*vl_V_ref1_dg);   //*vl_Qmax, 0311-by dahei
                    //vl_gradient1_dg := (beta*vl_V_ref1_dg*vl_V_ref1_dg* abs(Bii)*100/j)*vl_gradient1_dg;
                end;
                 with pnodeFMs^[NodeNumofDG] do
                begin
                    tmp := Calc_Grdt_for_Alpha_vivj(NodeNumofDG, phase_num);//  vl_gradient1_dg updated inside
                    if vl_Qmax_phase_dg <> 0 then
                        dynBeta := (beta* abs(Bii)*100/j) *vl_V_ref1_dg *vl_V_ref1_dg
                                        /vl_Qmax_phase_dg;
                    vl_alpha1_dg :=TempAlpha  + dynBeta * vl_gradient1_dg;
                    if vl_alpha1_dg >1 then vl_alpha1_dg := 1;
                    if vl_alpha1_dg <-1 then vl_alpha1_dg := -1;
                end;
                result := pnodeFMs^[NodeNumofDG].vl_alpha1_dg;
            end;
         2: begin //phase B
              //1.calculate d_ij*alpha_j summation
                den_dij := 0;
                TempAlpha := 0;
                //for i := 1 to nodes do den_dij := den_dij+pCommMatrix^[(NodeNumofDG-1)*Nodes+ i]; //4.is this correct
                //tmp1 := Nodes;
                for j := 1 to Nodes do
                   begin
                            if (pnodeFMs^[j].vl_ndphases_dg = 3)     //only count dgs with 3 phases or 1 phase that is same number
                            //or (pnodeFMs^[j].vl_phase_num_dg = phase_num) then
                            or (pnodeFMs^[j].vl_nodeType_phase[2] = 1) then //this phase has DG
                            begin
                              //if pnodeFMs^[j].vl_nodeType = 1 then // only DG nodes
                              //begin
                                  den_dij := den_dij+pCommMatrix^[(NodeNumofDG-1)*Nodes+ j]  ;
                                  TempAlpha := TempAlpha + pcommmatrix^[(NodeNumofDG-1)*nodes +j]*pnodeFMs^[j].vl_Alpha2_dg;
                              //end;
                            end;

                   end;
                if den_dij=0  then  TempAlpha := 0.0 else
                begin
                     TempAlpha := TempAlpha/den_dij;
                    //2.calculate gradient----------------
                    //vl_gradient1_dg := (vl_V_ref1_dg-vl_v1)*vl_V1/(den)/(vl_V_ref1_dg*vl_V_ref1_dg);   //*vl_Qmax, 0311-by dahei
                    //vl_gradient1_dg := (beta*vl_V_ref1_dg*vl_V_ref1_dg* abs(Bii)*100/j)*vl_gradient1_dg;
                end;
                with pnodeFMs^[NodeNumofDG] do
                begin
                  tmp := Calc_Grdt_for_Alpha_vivj(NodeNumofDG, phase_num);
                  if vl_Qmax_phase_dg <> 0 then
                    dynBeta := (beta* abs(Bii)*100/j)/vl_Qmax_phase_dg*vl_V_ref2_dg *vl_V_ref2_dg; //
                  vl_alpha2_dg :=TempAlpha  + dynBeta * vl_gradient2_dg;
                  if vl_alpha2_dg >1 then vl_alpha2_dg := 1;
                  if vl_alpha2_dg <-1 then vl_alpha2_dg := -1;
                end;
                result := pnodeFMs^[NodeNumofDG].vl_alpha2_dg;
            end;
         3: begin //phase C
                 //1.calculate d_ij*alpha_j summation
                den_dij := 0;
                TempAlpha := 0;
                //for i := 1 to nodes do den_dij := den_dij+pCommMatrix^[(NodeNumofDG-1)*Nodes+ i]; //4.is this correct
                //tmp1 := Nodes;
                for j := 1 to Nodes do
                   begin
                            if (pnodeFMs^[j].vl_ndphases_dg = 3)     //only count dgs with 3 phases or 1 phase that is same number
                            //or (pnodeFMs^[j].vl_phase_num_dg = phase_num) then
                            or (pnodeFMs^[j].vl_nodeType_phase[3] = 1) then //this phase has DG
                            begin
                              //if pnodeFMs^[j].vl_nodeType = 1 then // only DG nodes
                              //begin
                                  den_dij := den_dij+pCommMatrix^[(NodeNumofDG-1)*Nodes+ j]  ;
                                  TempAlpha := TempAlpha + pcommmatrix^[(NodeNumofDG-1)*nodes +j]*pnodeFMs^[j].vl_Alpha3_dg;
                              //end;
                            end;

                   end;
                if den_dij=0  then  TempAlpha := 0.0 else
                begin
                     TempAlpha := TempAlpha/den_dij;
                    //2.calculate gradient----------------
                    //vl_gradient1_dg := (vl_V_ref1_dg-vl_v1)*vl_V1/(den)/(vl_V_ref1_dg*vl_V_ref1_dg);   //*vl_Qmax, 0311-by dahei
                    //vl_gradient1_dg := (beta*vl_V_ref1_dg*vl_V_ref1_dg* abs(Bii)*100/j)*vl_gradient1_dg;
                end;
                with pnodeFMs^[NodeNumofDG] do
                begin
                    tmp := Calc_Grdt_for_Alpha_vivj(NodeNumofDG, phase_num);
                    if vl_Qmax_phase_dg <> 0 then
                        dynBeta := (beta* abs(Bii)*100/j)/vl_Qmax_phase_dg*vl_V_ref3_dg *vl_V_ref3_dg   ; //
                    vl_alpha3_dg :=TempAlpha  + dynBeta * vl_gradient3_dg;
                    if vl_alpha3_dg >1 then vl_alpha3_dg := 1;
                    if vl_alpha3_dg <-1 then vl_alpha3_dg := -1;
                end;
                result := pnodeFMs^[NodeNumofDG].vl_alpha3_dg;
            end;
         0: begin //pos seq value

            end;
         else
         end;
end;

Function TFMonitorObj.Calc_Alpha_L(NodeNumofDG, phase_num:Integer; dbNodeRef: integer; Bii,beta,Volt_Trhd: double):Double;
Var
  i,j : integer;
  sum_Sij_j, den : double;
  alpha:double;
  den_dij,dii,TempAlpha ,tmp: Double;
  dynBeta : double;
  //tmp1, tmp2, tmp3, tmp4, tmp5, tmp6, tmp7: double;
begin
      update_all_nodes_info;     // update voltages on all buses
      Get_PQ_DI( NodeNumofDG,ActorID ) ; // load measure
      // calclate alpha
     //with pnodeFMs^[NodeNumofDG] do
         case phase_num of
         1: begin //phase A
              //1.calculate d_ij*alpha_j summation
                den_dij := 0;
                TempAlpha := 0;
                //for i := 1 to nodes do den_dij := den_dij+pCommMatrix^[(NodeNumofDG-1)*Nodes+ i]; //4.is this correct
                //tmp1 := Nodes;
                for j := 1 to Nodes do
                   begin
                            if (pnodeFMs^[j].vl_ndphases_dg = 3)     //only count dgs with 3 phases or 1 phase that is same number
                            //or (pnodeFMs^[j].vl_phase_num_dg = phase_num) then
                            or (pnodeFMs^[j].vl_nodeType_phase[1] = 1) then //this phase has DG
                            begin
                              //if pnodeFMs^[j].vl_nodeType = 1 then // only DG nodes
                              //begin
                                  den_dij := den_dij+pCommMatrix^[(NodeNumofDG-1)*Nodes+ j]  ;
                                  TempAlpha := TempAlpha + pcommmatrix^[(NodeNumofDG-1)*nodes +j]*pnodeFMs^[j].vl_Alpha1_dg;
                              //end;
                            end;

                   end;
                if den_dij=0  then  TempAlpha := 0.0 else
                begin
                     TempAlpha := TempAlpha/den_dij;
                    //2.calculate gradient----------------
                    //vl_gradient1_dg := (vl_V_ref1_dg-vl_v1)*vl_V1/(den)/(vl_V_ref1_dg*vl_V_ref1_dg);   //*vl_Qmax, 0311-by dahei
                    //vl_gradient1_dg := (beta*vl_V_ref1_dg*vl_V_ref1_dg* abs(Bii)*100/j)*vl_gradient1_dg;
                end;
                 with pnodeFMs^[NodeNumofDG] do
                begin
                    tmp := Calc_Grdt_for_Alpha(NodeNumofDG, phase_num);//  vl_gradient1_dg updated inside
                    if vl_Qmax_phase_dg <> 0 then
                        dynBeta := (beta* abs(Bii)*100/j) *vl_V_ref1_dg *vl_V_ref1_dg
                                        /vl_Qmax_phase_dg;
                    vl_alpha1_dg :=TempAlpha  + dynBeta * vl_gradient1_dg;
                    if vl_alpha1_dg >1 then vl_alpha1_dg := 1;
                    if vl_alpha1_dg <-1 then vl_alpha1_dg := -1;
                end;
                result := pnodeFMs^[NodeNumofDG].vl_alpha1_dg;
            end;
         2: begin //phase B
              //1.calculate d_ij*alpha_j summation
                den_dij := 0;
                TempAlpha := 0;
                //for i := 1 to nodes do den_dij := den_dij+pCommMatrix^[(NodeNumofDG-1)*Nodes+ i]; //4.is this correct
                //tmp1 := Nodes;
                for j := 1 to Nodes do
                   begin
                            if (pnodeFMs^[j].vl_ndphases_dg = 3)     //only count dgs with 3 phases or 1 phase that is same number
                            //or (pnodeFMs^[j].vl_phase_num_dg = phase_num) then
                            or (pnodeFMs^[j].vl_nodeType_phase[2] = 1) then //this phase has DG
                            begin
                              //if pnodeFMs^[j].vl_nodeType = 1 then // only DG nodes
                              //begin
                                  den_dij := den_dij+pCommMatrix^[(NodeNumofDG-1)*Nodes+ j]  ;
                                  TempAlpha := TempAlpha + pcommmatrix^[(NodeNumofDG-1)*nodes +j]*pnodeFMs^[j].vl_Alpha2_dg;
                              //end;
                            end;

                   end;
                if den_dij=0  then  TempAlpha := 0.0 else
                begin
                     TempAlpha := TempAlpha/den_dij;
                    //2.calculate gradient----------------
                    //vl_gradient1_dg := (vl_V_ref1_dg-vl_v1)*vl_V1/(den)/(vl_V_ref1_dg*vl_V_ref1_dg);   //*vl_Qmax, 0311-by dahei
                    //vl_gradient1_dg := (beta*vl_V_ref1_dg*vl_V_ref1_dg* abs(Bii)*100/j)*vl_gradient1_dg;
                end;
                with pnodeFMs^[NodeNumofDG] do
                begin
                  tmp := Calc_Grdt_for_Alpha(NodeNumofDG, phase_num);
                  if vl_Qmax_phase_dg <> 0 then
                    dynBeta := (beta* abs(Bii)*100/j)/vl_Qmax_phase_dg*vl_V_ref2_dg *vl_V_ref2_dg; //
                  vl_alpha2_dg :=TempAlpha  + dynBeta * vl_gradient2_dg;
                  if vl_alpha2_dg >1 then vl_alpha2_dg := 1;
                  if vl_alpha2_dg <-1 then vl_alpha2_dg := -1;
                end;
                result := pnodeFMs^[NodeNumofDG].vl_alpha2_dg;
            end;
         3: begin //phase C
                 //1.calculate d_ij*alpha_j summation
                den_dij := 0;
                TempAlpha := 0;
                //for i := 1 to nodes do den_dij := den_dij+pCommMatrix^[(NodeNumofDG-1)*Nodes+ i]; //4.is this correct
                //tmp1 := Nodes;
                for j := 1 to Nodes do
                   begin
                            if (pnodeFMs^[j].vl_ndphases_dg = 3)     //only count dgs with 3 phases or 1 phase that is same number
                            //or (pnodeFMs^[j].vl_phase_num_dg = phase_num) then
                            or (pnodeFMs^[j].vl_nodeType_phase[3] = 1) then //this phase has DG
                            begin
                              //if pnodeFMs^[j].vl_nodeType = 1 then // only DG nodes
                              //begin
                                  den_dij := den_dij+pCommMatrix^[(NodeNumofDG-1)*Nodes+ j]  ;
                                  TempAlpha := TempAlpha + pcommmatrix^[(NodeNumofDG-1)*nodes +j]*pnodeFMs^[j].vl_Alpha3_dg;
                              //end;
                            end;

                   end;
                if den_dij=0  then  TempAlpha := 0.0 else
                begin
                     TempAlpha := TempAlpha/den_dij;
                    //2.calculate gradient----------------
                    //vl_gradient1_dg := (vl_V_ref1_dg-vl_v1)*vl_V1/(den)/(vl_V_ref1_dg*vl_V_ref1_dg);   //*vl_Qmax, 0311-by dahei
                    //vl_gradient1_dg := (beta*vl_V_ref1_dg*vl_V_ref1_dg* abs(Bii)*100/j)*vl_gradient1_dg;
                end;
                with pnodeFMs^[NodeNumofDG] do
                begin
                    tmp := Calc_Grdt_for_Alpha(NodeNumofDG, phase_num);
                    if vl_Qmax_phase_dg <> 0 then
                        dynBeta := (beta* abs(Bii)*100/j)/vl_Qmax_phase_dg*vl_V_ref3_dg *vl_V_ref3_dg   ; //
                    vl_alpha3_dg :=TempAlpha  + dynBeta * vl_gradient3_dg;
                    if vl_alpha3_dg >1 then vl_alpha3_dg := 1;
                    if vl_alpha3_dg <-1 then vl_alpha3_dg := -1;
                end;
                result := pnodeFMs^[NodeNumofDG].vl_alpha3_dg;
            end;
         0: begin //pos seq value

            end;
         else
         end;
end;

{----------------------------------------------------------------------------------}
Function TFMonitorObj.Calc_sum_dij_Alphaj(NodeNumofDG, phase_num,ActorID:Integer):Double;
var
  j : integer;
  sum_Sij_j : double;
begin
     update_all_nodes_info;

     // calclate alpha
     with pnodeFMs^[NodeNumofDG] do
         case phase_num of
         1: begin //phase A
                 vl_Alpha1_dg := 0.0;//init as zero
                 sum_Sij_j := 0.0;
                 //for j := 1 to Nodes do
                 for j := 1 to NodeNumofDG-1 do
                 begin
                          vl_Alpha1_dg := vl_Alpha1_dg + pcommmatrix^[(NodeNumofDG-1)*nodes +j]*pnodeFMs^[j].vl_Alpha1_dg   ;
                          sum_Sij_j := sum_Sij_j + pcommmatrix^[(NodeNumofDG-1)*nodes +j];
                 end;
                 for j := NodeNumofDG + 1 to Nodes do
                 begin
                          vl_Alpha1_dg := vl_Alpha1_dg + pcommmatrix^[(NodeNumofDG-1)*nodes +j]*pnodeFMs^[j].vl_Alpha1_dg   ;
                          sum_Sij_j := sum_Sij_j + pcommmatrix^[(NodeNumofDG-1)*nodes +j];
                 end;
                 vl_Alpha1_dg := vl_Alpha1_dg / sum_Sij_j;
                 //Alpha1 := Alpha1 - kcq* gradient1;
                 result := vl_Alpha1_dg;
            end;
         2: begin //phase B
                 vl_Alpha2_dg := 0.0;//init as zero
                 sum_Sij_j := 0.0;
                 //for j := 1 to Nodes do
                 for j := 1 to NodeNumofDG-1 do
                 begin
                          vl_Alpha2_dg := vl_Alpha2_dg + pcommmatrix^[(NodeNumofDG-1)*nodes +j]*pnodeFMs^[j].vl_Alpha2_dg   ;
                          sum_Sij_j := sum_Sij_j + pcommmatrix^[(NodeNumofDG-1)*nodes +j];
                 end;
                 for j := NodeNumofDG + 1 to Nodes do
                 begin
                          vl_Alpha2_dg := vl_Alpha2_dg + pcommmatrix^[(NodeNumofDG-1)*nodes +j]*pnodeFMs^[j].vl_Alpha2_dg   ;
                          sum_Sij_j := sum_Sij_j + pcommmatrix^[(NodeNumofDG-1)*nodes +j];
                 end;
                 vl_Alpha2_dg := vl_Alpha2_dg / sum_Sij_j;
                 //Alpha2 := Alpha2 - kcq* gradient2;
                 result := vl_Alpha2_dg;
            end;
         3: begin //phase C
                 vl_Alpha3_dg := 0.0;//init as zero
                 sum_Sij_j := 0.0;
                 //for j := 1 to Nodes do
                 for j := 1 to NodeNumofDG-1 do
                 begin
                          vl_Alpha3_dg := vl_Alpha3_dg + pcommmatrix^[(NodeNumofDG-1)*nodes +j]*pnodeFMs^[j].vl_Alpha3_dg   ;
                          sum_Sij_j := sum_Sij_j + pcommmatrix^[(NodeNumofDG-1)*nodes +j];
                 end;
                 for j := NodeNumofDG + 1 to Nodes do
                 begin
                          vl_Alpha3_dg := vl_Alpha3_dg + pcommmatrix^[(NodeNumofDG-1)*nodes +j]*pnodeFMs^[j].vl_Alpha3_dg   ;
                          sum_Sij_j := sum_Sij_j + pcommmatrix^[(NodeNumofDG-1)*nodes +j];
                 end;
                 vl_Alpha3_dg := vl_Alpha3_dg / sum_Sij_j;
                 //Alpha3 := Alpha3 - kcq* gradient3;
                 result := vl_Alpha3_dg;
            end;
         0: begin //pos seq value
                 vl_Alpha_dg := 0.0;//init as zero
                 sum_Sij_j := 0.0;
                 //for j := 1 to Nodes do
                 for j := 1 to NodeNumofDG-1 do
                 begin
                          vl_Alpha_dg := vl_Alpha_dg + pcommmatrix^[(NodeNumofDG-1)*nodes +j]*pnodeFMs^[j].vl_Alpha_dg   ;
                          sum_Sij_j := sum_Sij_j + pcommmatrix^[(NodeNumofDG-1)*nodes +j];
                 end;
                 for j := NodeNumofDG + 1 to Nodes do
                 begin
                          vl_Alpha_dg := vl_Alpha_dg + pcommmatrix^[(NodeNumofDG-1)*nodes +j]*pnodeFMs^[j].vl_Alpha_dg   ;
                          sum_Sij_j := sum_Sij_j + pcommmatrix^[(NodeNumofDG-1)*nodes +j];
                 end;
                 vl_Alpha_dg := vl_Alpha_dg / sum_Sij_j;
                 //Alpha := Alpha - kcq* gradient;
                 result := vl_Alpha_dg;
            end;
         else
         end;
end;
Function TFMonitorObj.AvgPmax : double;
var
     i,k : integer;
begin
     result := 0.0;
     k := 1;
     //nodes;//all nodes included;

     for i := 1 to nodes do
     begin
       with pnodeFMs^[i] do
          if ((vl_PF_flag_dg=1) and (vl_cc_switch_dg=true)) then //
          begin
                    result := result + vl_Pmax_dg ;
                    result := result /k;
                    k := k+1;
          end;
     end;
end;
Function TFMonitorObj.AvgQmax : double;
var
     i,k : integer;
begin
     result := 0.0;
     k := 1;
     //nodes;//all nodes included;
     for i := 1 to nodes do
     begin
        with pnodeFMs^[i] do
          if ((vl_QV_flag_dg=1) and (vl_cc_switch_dg=true)) then //volt/var control is on
          begin
                    result := result + vl_Qmax_dg ;
                    result := result /k;
                    k := k+1;
          end;
     end;
end;
{----------------------------------------------------------------------}
{}
function TFMonitorObj.Get_FileName: String;
begin
  Result := GetOutputDirectory +  CircuitName_ + 'Mon_' + Name + '.csv'
end;

function TFMonitorObj.Calc_fm_ul_0(NodeNumofDG, phase_num:Integer; dbNodeRef: integer; Bii,beta,Volt_Trhd: double): double;
var
  i,j : integer;
  den : double;
  den_dij,TempAlpha : Double;
  tmp : double;
  dly : integer;
begin

      //update_all_nodes_info;     // update voltages on all buses
      //with pNodeFMs^[NodeNumofDG] do
     // begin
   case phase_num of //pos seq
   0:  begin
        //1.calculate d_ij*alpha_j summation
          den_dij := 0;
          TempAlpha := 0;
          {-----------------------------------}
          //no delay
          if T_intvl_smpl=0.0  then
          begin
              for j := 1 to Nodes do
               begin
                   if (pnodeFMs^[j].vl_ndphases_dg = 3)   //only 3 phase nodes
                        and ((pnodeFMs^[j].vl_nodeType_phase[1]+pnodeFMs^[j].vl_nodeType_phase[2]+pnodeFMs^[j].vl_nodeType_phase[3]) = 3)
                   then //this phase has DG
                   begin
                       den_dij := den_dij+pCommMatrix^[(NodeNumofDG-1)*Nodes+ j];
                       TempAlpha := TempAlpha + pcommmatrix^[(NodeNumofDG-1)*nodes
                                      +j]*pnodeFMs^[j].vl_Alpha_dg;
                   end;

               end;
          end
          //with delay
          else begin
              for j := 1 to NodeNumofDG-1 do
               begin
                   if (pnodeFMs^[j].vl_ndphases_dg = 3)   //only 3 phase nodes
                        and ((pnodeFMs^[j].vl_nodeType_phase[1]+pnodeFMs^[j].vl_nodeType_phase[2]+pnodeFMs^[j].vl_nodeType_phase[3]) = 3)
                   then //this phase has DG
                   begin
                       den_dij := den_dij+pCommMatrix^[(NodeNumofDG-1)*Nodes+ j];
                       //how many steps of delay from node j to node 'NodeNumofDG'
                       dly :=  pcommDelaysteps^[(NodeNumofDG-1)*nodes +j];
                       if dly=0 then
                       begin
                          TempAlpha := TempAlpha
                                  + pcommmatrix^[(NodeNumofDG-1)*nodes +j]*pnodeFMs^[j].vl_Alpha_dg;
                       end
                       else begin
                          TempAlpha := TempAlpha
                                + pcommmatrix^[(NodeNumofDG-1)*nodes +j]*pnodeFMs^[j].vl_smpl_dg[1][dly];
                       end;
                   end;

               end;

               j := NodeNumofDG;
                   if (pnodeFMs^[j].vl_ndphases_dg = 3)   //only 3 phase nodes
                        and ((pnodeFMs^[j].vl_nodeType_phase[1]+pnodeFMs^[j].vl_nodeType_phase[2]+pnodeFMs^[j].vl_nodeType_phase[3]) = 3)
                   then //this phase has DG
                   begin
                         den_dij := den_dij+pCommMatrix^[(NodeNumofDG-1)*Nodes+ j];
                         TempAlpha := TempAlpha
                                    + pcommmatrix^[(NodeNumofDG-1)*nodes +j]*pnodeFMs^[j].vl_Alpha_dg;
                    end;

               for j := NodeNumofDG + 1 to Nodes do
               begin
                   if (pnodeFMs^[j].vl_ndphases_dg = 3)   //only 3 phase nodes
                        and ((pnodeFMs^[j].vl_nodeType_phase[1]+pnodeFMs^[j].vl_nodeType_phase[2]+pnodeFMs^[j].vl_nodeType_phase[3]) = 3)
                   then //this phase has DG
                   begin
                       den_dij := den_dij+pCommMatrix^[(NodeNumofDG-1)*Nodes+ j];
                       dly :=  pcommDelaysteps^[(NodeNumofDG-1)*nodes +j];
                       if dly=0 then
                       begin
                          TempAlpha := TempAlpha
                                  + pcommmatrix^[(NodeNumofDG-1)*nodes +j]*pnodeFMs^[j].vl_Alpha_dg;
                       end
                       else begin
                          TempAlpha := TempAlpha
                                + pcommmatrix^[(NodeNumofDG-1)*nodes +j]*pnodeFMs^[j].vl_smpl_dg[1][dly];
                       end;
                   end;

               end;
          end;
          {-----------------------------------}

          if den_dij=0  then  TempAlpha := 0.0 else
          begin
               TempAlpha := TempAlpha/den_dij;
          end;

          tmp := (TempAlpha - pNodeFMs^[NodeNumofDG].vl_Alpha_dg);
          //Tolerance of alpha_i alpha_j
          if abs(tmp)< 0.002 then
               Result := 0.0
          else
               Result := tmp * pNodeFMs^[NodeNumofDG].vl_kc_ul_dg;
      end;

  end;
     //end;
     //result := 0;
end;
Function TFMonitorObj.Calc_fm_us_0(NodeNumofDG, phase_num:Integer; dbNodeRef: integer; Bii,beta,Volt_Trhd: double): double;
var
  i,j : integer;
  den : double;

begin
      //update voltage
      Get_PDElem_terminal_voltage(NodeNumofDG,pNodeFMs^[NodeNumofDG].vl_strMeasuredName,pNodeFMs^[NodeNumofDG].vl_terminalNum,ActorID ) ;
      //calc gradient
      with pNodeFMs^[NodeNumofDG] do
      begin
         case phase_num of //pos seq
         0:  begin
                    den := abs(vl_Q_DG - vl_Q_Di- vl_V*vl_V* Bii);   // pos ctrl: Bii use the first one
                    if abs(den)<epsilon then den := epsilon ;
                    vl_gradient_dg := (vl_V_ref_dg-vl_v)*vl_V/(den)/(vl_V_ref_dg*vl_V_ref_dg);//*vl_Qmax_dg;//
                    vl_gradient_dg := (beta*vl_V_ref_dg*vl_V_ref_dg)* abs(Bii)*vl_gradient_dg;
                if abs(vl_V_ref_dg-vl_v)<= Volt_Trhd*vl_V_ref_dg then vl_gradient_dg := 0.0;
                if vl_gradient_dg >1 then vl_gradient_dg := 1;
                if vl_gradient_dg <-1 then vl_gradient_dg := -1;
                result := vl_gradient_dg ;
            end;

        end;
     end;
end;
Procedure TFMonitorObj.Agnt_smpl(NodeNumofDG, phase_num,ActorID:Integer); //abandoned
Var
  crnt_time: double;
  i : integer;
begin
    //if True then
    if ActiveCircuit.Solution.DynaVars.IterationFlag = 1 then   {1= Same Time Step as last iteration}
    begin
          //
          if pNodeFMs^[NodeNumofDG].vl_SmplCnt=0 then//the first step
          begin
              for i:= 1 to MaxLocalMem do
                  pNodeFMs^[NodeNumofDG].vl_smpl_dg[1][i] := pNodeFMs^[NodeNumofDG].vl_Alpha_dg ;
                  pNodeFMs^[NodeNumofDG].vl_smpl_dg[2][i] := pNodeFMs^[NodeNumofDG].vl_AlphaP_dg;
          end;
          //
          crnt_time := ActiveCircuit.Solution.DynaVars.intHour*3600+ActiveCircuit.Solution.DynaVars.t ;
          //Move the array only at the first time-step

          //if t_k greater or equal to current sample time plus smp interval, do another sample
          if crnt_time >= (T_intvl_smpl +pNodeFMs^[NodeNumofDG].vl_crnt_smp_time) then
          begin
              //if Trunc(crnt_time/T_intvl_smpl) >= pNodeFMs^[NodeNumofDG].vl_SmplCnt +1 then
              //begin
                 //save alf into the first entry of smpl_ary for communication
                 //alpha
                  pNodeFMs^[NodeNumofDG].vl_smpl_dg[1][0] := pNodeFMs^[NodeNumofDG].vl_Alpha_dg;// [0] is the newest value
                                                                                                // [0] and [1] are always the same
                 //alphaP
                  pNodeFMs^[NodeNumofDG].vl_smpl_dg[2][0] := pNodeFMs^[NodeNumofDG].vl_AlphaP_dg;//
                 //0 seq voltage
                  //pNoddeFMs^[NodeNumofDG].vl_smpl_dg[3][0] := pNodeFMs^[NodeNumofDG].vl_V;//

                  for i := 0 to MaxLocalMem-1 do // [0]->[1],[MaxLocalMem-1]->[MaxLocalMem]
                  begin
                       pNodeFMs^[NodeNumofDG].vl_smpl_dg[1][MaxLocalMem-i] := pNodeFMs^[NodeNumofDG].vl_smpl_dg[1][MaxLocalMem-i-1] ;
                       pNodeFMs^[NodeNumofDG].vl_smpl_dg[2][MaxLocalMem-i] := pNodeFMs^[NodeNumofDG].vl_smpl_dg[2][MaxLocalMem-i-1] ;
                  end;

                 //vl_SmplCnt increase
                  inc(pNodeFMs^[NodeNumofDG].vl_SmplCnt);
                  //update vl_crnt_time
                  pNodeFMs^[NodeNumofDG].vl_crnt_smp_time := crnt_time;
              //end;
          end;

    end;
end;
Procedure TFMonitorObj.Init_delay_array(NodeNumofDG, ActorID:Integer);
Var
  i : integer;
begin
              //measure all voltages
              for i := 1 to nodes do
                   Get_PDElem_terminal_voltage(i, pnodefms^[i].vl_strMeasuredName, pnodefms^[i].vl_terminalNum,ActorID ) ;
              // inti delay array
              for i:= 0 to MaxLocalMem do
              begin
                   //alpha
                   pNodeFMs^[NodeNumofDG].vl_smpl_dg[1][i] :=  pNodeFMs^[NodeNumofDG].vl_Alpha_dg;
                   //alphaP
                   pNodeFMs^[NodeNumofDG].vl_smpl_dg[2][i] := pNodeFMs^[NodeNumofDG].vl_AlphaP_dg ;
                   // vl_v, which is the 0 seq. voltage
                   pNodeFMs^[NodeNumofDG].vl_smpl_dg[3][i] := pNodeFMs^[NodeNumofDG].vl_V ;

              end;
end;

Function TFMonitorObj.Calc_Gradient_ct_P(NodeNuminClstr, phase_num, ActorID:Integer):Double;  // NodeNuminClstr: node number in cluster
var
  dvDGtemp : double;
  Grdnt_P : double;
begin
       //tempCplx
       dvDGtemp := (pNodeFMs^[NodeNuminClstr].vl_V - pNodeFMs^[NodeNuminClstr].vl_V_ref_dg)
                /pNodeFMs^[NodeNuminClstr].vl_V_ref_dg; //
       // if this DG is above 1.05, then it should have P curtail gradient
       //if ( ActiveCircuit.Solution.bCurtl=true ) and (dvDGtemp>0.0) then //overall system need control
       if ( ActiveCircuit.Solution.bCurtl=true )  then
       begin
            if ld_fm_info[0].b_Curt_Ctrl=true then // if false, the curtailment will be zero for any node in this cluster
                Grdnt_P := (activecircuit.Solution.LD_FM[0].volt_lwst
                              -1.0)
                              //- activecircuit.Solution.LD_FM[0].volt_lw_lmt);
       end else
             Grdnt_P := 0.0;
      //!!!!!!!!!!!!!
         case phase_num of //pos seq
         0: begin
              result := Grdnt_P; //
            end;
         1: begin
              result := Grdnt_P; //
            end;
         2: begin
              result := Grdnt_P; //
            end;
         3: begin
              result := Grdnt_P; //
            end;
         end;
end;
//////////////////////////
{-------------------------}
Function TFMonitorObj.Calc_ul_P(NodeNuminClstr, phase_num:Integer):Double;  // NodeNuminClstr: node number in cluster //with delay
var
  j : integer;
  den_dij,TempAlphaP : Double;
  dly : integer;

begin
     //alphaP = sum (alphaP) + Beta * Gp

     //with pNodeFMs^[NodeNuminClstr] do
     // begin
         case phase_num of //pos seq
         0:  begin
              //1.calculate d_ij*alpha_j summation
                den_dij := 0;
                TempAlphaP := 0;
                 {-----------------------------------}
          //no delay
                if T_intvl_smpl=0.0  then
                begin
                      for j := 1 to Nodes do
                         begin
                           if (pnodeFMs^[j].vl_ndphases_dg = 3)   //only 3 phase nodes
                              and ((pnodeFMs^[j].vl_nodeType_phase[1]+pnodeFMs^[j].vl_nodeType_phase[2]+pnodeFMs^[j].vl_nodeType_phase[3]) = 3)
                           then  // has 3-phase DG
      //                         if (pnodeFMs^[j].vl_ndphases_dg = 3) then   //only 3 phase nodes
                                  begin
                                          //if pnodeFMs^[j].vl_nodeType = 1 then // only DG nodes
                                          //begin
                                              den_dij := den_dij+pCommMatrix^[(NodeNuminClstr-1)*Nodes+ j];
                                              TempAlphaP := TempAlphaP
                                                    + pcommmatrix^[(NodeNuminClstr-1)*nodes +j]*pnodeFMs^[j].vl_AlphaP_dg;
                                          //end;
                                  end;
                         end;
                end else
           // with delay
                begin
                     for j := 1 to NodeNuminClstr-1 do
                     begin
                         if (pnodeFMs^[j].vl_ndphases_dg = 3)   //only 3 phase nodes
                              and ((pnodeFMs^[j].vl_nodeType_phase[1]+pnodeFMs^[j].vl_nodeType_phase[2]+pnodeFMs^[j].vl_nodeType_phase[3]) = 3)
                         then //has 3-phase  DG
                         begin
                             den_dij := den_dij+pCommMatrix^[(NodeNuminClstr-1)*Nodes+ j];
                             //how many steps of delay from node j to node 'NodeNumofDG'
                             dly :=  pcommDelaysteps^[(NodeNuminClstr-1)*nodes +j];
                             if dly=0 then
                             begin
                                TempAlphaP := TempAlphaP
                                        + pcommmatrix^[(NodeNuminClstr-1)*nodes +j]*pnodeFMs^[j].vl_AlphaP_dg;
                             end
                             else begin
                                TempAlphaP := TempAlphaP
                                      + pcommmatrix^[(NodeNuminClstr-1)*nodes +j]*pnodeFMs^[j].vl_smpl_dg[2][dly];
                             end;
                         end;

                     end;

                     j := NodeNuminClstr;
                         if (pnodeFMs^[j].vl_ndphases_dg = 3)   //only 3 phase nodes
                              and ((pnodeFMs^[j].vl_nodeType_phase[1]+pnodeFMs^[j].vl_nodeType_phase[2]+pnodeFMs^[j].vl_nodeType_phase[3]) = 3)
                         then //has 3-phase DG
                         begin
                               den_dij := den_dij+pCommMatrix^[(NodeNuminClstr-1)*Nodes+ j];
                               TempAlphaP := TempAlphaP
                                          + pcommmatrix^[(NodeNuminClstr-1)*nodes +j]*pnodeFMs^[j].vl_AlphaP_dg;
                          end;

                     for j := NodeNuminClstr + 1 to Nodes do
                     begin
                         if (pnodeFMs^[j].vl_ndphases_dg = 3)   //only 3 phase nodes
                              and ((pnodeFMs^[j].vl_nodeType_phase[1]+pnodeFMs^[j].vl_nodeType_phase[2]+pnodeFMs^[j].vl_nodeType_phase[3]) = 3)
                         then //has 3-phase DG
                         begin
                             den_dij := den_dij+pCommMatrix^[(NodeNuminClstr-1)*Nodes+ j];
                             dly :=  pcommDelaysteps^[(NodeNuminClstr-1)*nodes +j];
                             if dly=0 then
                             begin
                                TempAlphaP := TempAlphaP
                                        + pcommmatrix^[(NodeNuminClstr-1)*nodes +j]*pnodeFMs^[j].vl_AlphaP_dg;
                             end
                             else begin
                                TempAlphaP := TempAlphaP
                                      + pcommmatrix^[(NodeNuminClstr-1)*nodes +j]*pnodeFMs^[j].vl_smpl_dg[2][dly];
                             end;
                         end;

                     end;
                end;
                if den_dij=0  then  TempAlphaP := 0.0 else
                begin
                     TempAlphaP := TempAlphaP/den_dij;   //the average
                end;
              //Tolerance of alphap_i alphap_j
                TempAlphaP := TempAlphaP - pNodeFMs^[NodeNuminClstr].vl_AlphaP_dg; //uL for cooperative control of active power
              if abs(TempAlphaP)< 0.002 then
               Result := 0.0
              else
               Result :=  TempAlphaP;
            end;
         end;
     // end;
end;

Procedure  TFMonitorObj.update_ld_info; //all nodes , p.u. value
var
  i : integer;
  v0_tmp : double;
begin

     ld_fm_info[0].volt_avg := 0.0; //recalculate voltage average
     ld_fm_info[0].volt_lwst  := 999999; //search new value at each round
     ld_fm_info[0].volt_hghst := -99999;
     for i := 1 to nodes do
     begin
           //update all nodes voltage into agents
           Get_PDElem_terminal_voltage(i, pnodefms^[i].vl_strMeasuredName, pnodefms^[i].vl_terminalNum ) ;
           //
           //synchronous sampling
           v0_tmp := pnodefms^[i].vl_V/(pnodefms^[i].vl_basevolt);
           //update highest voltage
           if  ld_fm_info[0].volt_hghst < v0_tmp then
           begin
             ld_fm_info[0].volt_hghst := v0_tmp;
             ld_fm_info[0].ndnum_hghst := i;
           end;
           //update lowest voltage
           if  ld_fm_info[0].volt_lwst > v0_tmp then
           begin
             ld_fm_info[0].volt_lwst := v0_tmp;
             ld_fm_info[0].ndnum_lwst := i;
           end;

           //other information should be updated?
           //
           ld_fm_info[0].volt_avg := ld_fm_info[0].volt_avg + v0_tmp;  //p.u.
      end;

      ld_fm_info[0].volt_avg := ld_fm_info[0].volt_avg / nodes;
end;
Procedure  TFMonitorObj.update_ld_dly( ActorID: integer); //all nodes , p.u. value
var
  i, j , ndlys : integer;
  v0_tmp : double;
  crnt_time: double;
begin
     ld_fm_info[0].volt_avg := 0.0; //recalculate voltage average
     ld_fm_info[0].volt_lwst  := 999999; //search new value at each round
     ld_fm_info[0].volt_hghst := -99999;
     for i := 1 to Nodes do
     begin
           //update vl_v1/v2/v3, vl_v_1c/v_2c/v_3c, update vl_v for node i
           Get_PDElem_terminal_voltage(i, pnodefms^[i].vl_strMeasuredName, pnodefms^[i].vl_terminalNum, ActorID ) ;
           //synchronous sampling
           if t_intvl_smpl=0.0 then
           begin
                 // pNodeFMs^[i].vl_smpl_dg[i][j] is not used
                 v0_tmp := pnodefms^[i].vl_V/(pnodefms^[i].vl_basevolt);
           end else
           begin
           //asynchronous sampling
                 //update pNodeFMs^[i].vl_smpl_dg[i][j] first
                  if pNodeFMs^[i].vl_SmplCnt=0 then//the first step
                         begin
                             for j:= 0 to MaxLocalMem do
                                 //alphas
                                 pNodeFMs^[i].vl_smpl_dg[1][j] := pNodeFMs^[i].vl_Alpha_dg ;
                                 pNodeFMs^[i].vl_smpl_dg[2][j] := pNodeFMs^[i].vl_AlphaP_dg;
                                 //voltage
                                 pNodeFMs^[i].vl_smpl_dg[3][j] := pNodeFMs^[i].vl_V ;  // 0 seq.
                         end;
                 //
                 crnt_time := ActiveCircuit.Solution.DynaVars.intHour*3600+ActiveCircuit.Solution.DynaVars.t ;
                 //Move the array only at the first time-step

                 //if t_k greater or equal to current sample time plus smp interval, do another sample
                 if crnt_time >= (T_intvl_smpl +pNodeFMs^[i].vl_crnt_smp_time) then
                 begin

                     //if Trunc(crnt_time/T_intvl_smpl) >= pNodeFMs^[NodeNumofDG].vl_SmplCnt +1 then
                     //begin
                        //save alf into the first entry of smpl_ary for communication
                        //alpha
                         pNodeFMs^[i].vl_smpl_dg[1][0] := pNodeFMs^[i].vl_Alpha_dg;// [0] is the newest value
                        //alphaP
                         pNodeFMs^[i].vl_smpl_dg[2][0] := pNodeFMs^[i].vl_AlphaP_dg;//
                        // VL_V  //0 seq voltage
                         pNodeFMs^[i].vl_smpl_dg[3][0] := pNodeFMs^[i].vl_V;//


                         //pNoddeFMs^[NodeNumofDG].vl_smpl_dg[3][0] := pNodeFMs^[NodeNumofDG].vl_V;//

                         for j := 0 to MaxLocalMem-1 do // [0]->[1],[MaxLocalMem-1]->[MaxLocalMem]
                         begin
                              pNodeFMs^[i].vl_smpl_dg[1][MaxLocalMem-j] := pNodeFMs^[i].vl_smpl_dg[1][MaxLocalMem-j-1] ;
                              pNodeFMs^[i].vl_smpl_dg[2][MaxLocalMem-j] := pNodeFMs^[i].vl_smpl_dg[2][MaxLocalMem-j-1] ;
                              pNodeFMs^[i].vl_smpl_dg[3][MaxLocalMem-j] := pNodeFMs^[i].vl_smpl_dg[3][MaxLocalMem-j-1]
                         end;

                        //vl_SmplCnt increase
                         inc(pNodeFMs^[i].vl_SmplCnt);
                         //update vl_crnt_time
                         pNodeFMs^[i].vl_crnt_smp_time := crnt_time;
                     //end;
                 end;
                 // delay steps from agent to virtual leader
                 ndlys :=  pcommDelaysteps^[(virtual_Ld_Nd - 1)*Nodes +i];
                 // total delay steps: ndlys+nup_dlys
                 v0_tmp := pnodefms^[i].vl_smpl_dg[3][ndlys+nUp_dlys]/(pnodefms^[i].vl_basevolt);
           end;
           //update highest voltage
            if  ld_fm_info[0].volt_hghst < v0_tmp then
            begin
              ld_fm_info[0].volt_hghst := v0_tmp;
              ld_fm_info[0].ndnum_hghst := i;
            end;
            //update lowest voltage
            if  ld_fm_info[0].volt_lwst > v0_tmp then
            begin
              ld_fm_info[0].volt_lwst := v0_tmp;
              ld_fm_info[0].ndnum_lwst := i;
            end;

           //other information should be updated?
           //
           ld_fm_info[0].volt_avg := ld_fm_info[0].volt_avg + v0_tmp;  //p.u.
     end;
     //avg of valtage
     ld_fm_info[0].volt_avg := ld_fm_info[0].volt_avg / nodes;
end;

initialization
  //WriteDLLDebugFile('Monitor');

end.


