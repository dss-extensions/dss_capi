unit fMonitor;
{$PUSH}
{$RANGECHECKS ON}
//  Copyright (c) 2024 DSS-Extensions contributors
//  Copyright (c) 2008-2019, Electric Power Research Institute, Inc.
//  All rights reserved.

// **Heavily** modified (by Paulo Meira) for DSS-Extensions.
// The original file had a lot of leftover code from Monitor.pas and
// some other issues. It still currently doesn't fit great with the
// other OpenDSS components. A lot of the input properties is still
// missing better validation.
//TODO: dynBeta := (beta* abs(Bii)*100/Nodes) (SVN R3340)
//TODO: MaxLocalMem: Limits in parser and handling in loops


interface

uses
    Command,
    MeterClass,
    Meterelement,
    DSSClass,
    CAPI_Types,
    ucomplex, DSSUComplex,
    utilities,
    Classes,
    DSSPointerList;

type
{$SCOPEDENUMS ON}
    TFMonitorPropLegacy = (
        INVALID = 0,
        element,
        terminal,
        action,
        P_trans_ref,
        Node_num,
        Cluster_num,
        Nodes,
        CommVector,
        ElemTableLine,
        P_Mode,
        CommDelayVector,
        T_intvl_smpl,
        MaxLocalMem,
        Volt_limits_pu,
        b_Curt_Ctrl,
        up_dly,
        virtual_ld_node,
        EGen,
        attack_defense,
        Comm_hide,
        Comm_node_hide
    );
    TFMonitorProp = (
        INVALID = 0,
        Element,
        Terminal,
        Action,
        P_Trans_Ref,
        Node_Num,
        Cluster_Num,
        Nodes,
        CommVector,
        ElemTableLine,
        P_Mode,
        CommDelayVector,
        T_IntVL_Smpl,
        MaxLocalMem,
        Volt_Limits_pu,
        b_Curt_Ctrl,
        Up_Dly,
        Virtual_LD_Node,
        EGen,
        Attack_Defense,
        Comm_Hide,
        Comm_Node_Hide
    );
{$SCOPEDENUMS OFF}

    //value ot save communication delay
    TFMonDelays = array [0..99] of Double;

    // define LD_FM_Arry-by dahei
    TLDs_sys_fms = record
        //properties for Nodes
        // highest voltage node
        // clstr_num_hghst: Integer;
        // ndnum_hghst: Integer;
        b_ctrl_hghst: Boolean; //can contribute more to the high volt problem
        volt_hghst: Double;    //p.u.
        volt_hgh_lmt: Double;  //p.u.
        // Pinjec_hghst: Double; //net P injection on this node
        // lowest voltage node
        // clstr_num_lwst: Integer;
        // ndnum_lwst: Integer;
        // b_ctrl_lwst: Boolean; //can contribute more to the high volt problem
        volt_lwst: Double;   //p.u.
        volt_lw_lmt: Double;  //p.u.
        // Pinjec_lwst: Double; // net P injection on this node
    end;

    TVLNodeVars = record
        
        //properties for Node
        vl_strBusName: String;
        vl_strMeasuredName: String;
        vl_terminalNum: Integer;
        // vl_ndphases: Integer;
        vl_basevolt: Double;
        vl_nodeType_phase: array [1..3] of Smallint;   // set by TFMonitorObj.Init_nodeFM :
                                  //for each phase
                                  //1, dg under it; 2, no dg there
                                  //if a dg is connected, it is 1;

        vl_V, vl_V1, vl_V2, vl_V3: Double;

        Bus_Idx: Integer;      // has to be updated before being used
                                // it is related to YMatrix
        // Yii: array [0..3] of complex; //each phase  123 - ABC,  0 - pos seq
        // Yij: array [0..3] of complex; //each phase  123 - ABC,  0 - pos seq

        // complex voltage
        // vl_V_c, 
        vl_V_1c, vl_V_2c, vl_V_3c: complex;
        
        
        //Properties for DG
        // vl_strName_dg: String;
        //vl_terminalNum : integer;
        vl_ndphases_dg: Integer;   // set by TFMonitorObj.Init_nodeFM: 1,3
        vl_phase_num_dg: Integer; //   set by TFMonitorObj.Init_nodeFM, 123--abc 0- this node has 3-phase
                            //if vl_nodeType=1, and vl_ndphases=1,phase_num =1,2,3
        //SmallIntArray = Array[1..100] of SmallInt;
        //vl_strBusName : string;
        // vl_CC_switch_dg: Boolean;// cooperate control switch. true, cooperate control is on
        // vl_PF_flag_dg: Integer;//1, real power control is on
        // vl_QV_flag_dg: Integer;//1, volt/var control is on
        // vl_volt_thrd_dg: Double;
        //vl_phase_select
        vl_Alpha_dg,
        vl_Alpha1_dg, vl_Alpha2_dg, vl_Alpha3_dg,
        vl_Alpha_dgn,
        vl_Gradient_dg, vl_Gradient1_dg, vl_Gradient2_dg, vl_Gradient3_dg: Double;

        // communication array for alpha and others can be improved
        vl_smpl_dg: array [1..3] of TFMonDelays; //1: alpha; 2: alphaP; 3, bus voltage 0 seq. ; 4,5,6: bus voltage ABC

        //
        vl_SmplCnt: Integer;  //sample count for this agent
        vl_crnt_smp_time: Double; //time for current sample at this agent

        vl_AlphaP_dg,
        vl_AlphaP1_dg, vl_AlphaP2_dg, vl_AlphaP3_dg: Double;
        vl_GradientP_dg, vl_GradientP1_dg, vl_GradientP2_dg, vl_GradientP3_dg: Double;
        vl_Pmax_dg, vl_Qmax_dg,
        vl_Pmax_phase_dg, vl_Qmax_phase_dg: Double;
        // vl_V_base_dg,
        vl_V_ref_dg, vl_V_ref1_dg, vl_V_ref2_dg, vl_V_ref3_dg: Double;// nominal value with respect to p.u. 1  //must be set by initializing routines
        vl_kcq_dg: Double; // the step size gain of agent i //has to be defined befor used
        // vl_p_DG, 
        vl_p_DG1, vl_p_DG2, vl_p_DG3: Double;
        vl_kcd_dg: Double; // the step size gain of agent i //has to be defined befor used
        vl_kc_ul_dg: Double; // the cooperative gain for agent i
        vl_q_DG, vl_q_DG1, vl_q_DG2, vl_q_DG3: Double;
   
        //Properties for Loads
        ldType: Integer; //-1: noload; 0: one 3phase or 2phase load; 1, 2, 3: 1,2 or 3 single loads;
        ldIdx, ldIdx1, ldIdx2, ldIdx3: Integer;
        vl_Q_Di: Double; //all load reactive power except DG
        vl_Q_Di1: Double;
        vl_Q_Di2: Double;
        vl_Q_Di3: Double;
        // vl_P_Di: Double; //all load reactive power except DG
        vl_P_Di1: Double;
        vl_P_Di2: Double;
        vl_P_Di3: Double;
        // vl_NodeRef : integer;// for global use

        // attack and defense
        d_atk, z_dfs: Double;
        z_dfsn: Double;
        d_atk0: Double;
    end;

    TFMonitor = class(TMeterClass)
    PROTECTED
        procedure DefineProperties(); OVERRIDE;
    PUBLIC
        LD_FM: array [0..3] of TLDs_sys_fms;
        bCurtl: Boolean;

        constructor Create(dssContext: TDSSContext);
        destructor Destroy; OVERRIDE;

        function EndEdit(ptr: Pointer; const NumChanges: integer): Boolean; override;
        function NewObject(const ObjName: String; Activate: Boolean = true): Pointer; OVERRIDE;

        procedure ResetAll(); OVERRIDE;

        // update FM leader information
        procedure update_sys_ld_info(); //all FMs
        procedure Calc_P_freq();// calculte frequency for each cluster

        //attack and defense
        procedure update_atks();
        procedure update_defense_layer();

    end;

    // Definition of Fmonitor (virtue leader) Public Data Record
    // Fmonitor public data/state variable structure
    TLD_fm_infos = {$IFNDEF DSS_CAPI_NO_PACKED_RECORDS}packed{$ENDIF} record
        //properties for Nodes
        // highest voltage node
        // ndnum_hghst: Integer;
        b_ctrl_hghst: Boolean; //can contribute more to the high volt problem
        volt_hghst: Double;    //low volt in pu
        volt_hgh_lmt: Double;   //low limit in pu
        // Pinjec_hghst: Double;  //net P injection on this node

         // lowest voltage node
        // ndnum_lwst: Integer;
        // b_ctrl_lwst: Boolean; //can contribute more to the high volt problem
        volt_lwst: Double;  //low volt in pu
        volt_lw_lmt: Double; //low limit in pu
        // Pinjec_lwst: Double; // net P injection on this node

         // overview information
        volt_avg: Double;
        total_pg: Double; //total generation of this cluster
        total_pl: Double; //total load of this cluster

        b_Curt_Ctrl: LongBool;
    end;

    TFMonitorObj = class(TMeterElement)
    PRIVATE
        nodes: Integer; //nodes of this cluster  \\default nodes := 33;
        pCommMatrix: Array of Array of SmallInt;// communication matrix of this cluster
        P_trans_ref: Double; // Power Ref on metered elemet, if mode =1 real power of this cluster will be used
        tempCplx: complex;

        // F_Value_one: Double;//test;
        // Voltages
        // F_Value_one_V: pDoubleArray; //Measured voltage by each FMonitor(p.u.)
        // F_Value_one_S: pComplexArray; //Measured apparent power for each phase by each Fmonitor
        // Fvalue_P: Double;  //This variable is used to store total measure three-phase active power of any Fmonitor
        // Fvalue_Q: Double;  //This variable is used to store total measure three-phase reactive power of any Fmonitor

        // F_P_one, F_Q_one: Double;//measured power
        // P_ref_one: Double; //the ref Power for this point

        Node_num: Integer;  // Node number within the cluster
        Cluster_num: Integer;  // the group number for this

        // communication time
        T_intvl_smpl: Double; //Sampling interval.
        MaxLocalMem: Integer; //Max number of local memory, no large than 99
        Smpl_stps: Integer; // T_Comm/ ActiveCircuit.Solution.Dynavars.h.
        pCommDelayMatrix: Array of Array of Double;  //
        pCommDelaySteps: Array of Array of SmallInt;// Communication delay step matrix of this cluster

        // define properties for equivalent generator for simulate frequency
        // eg_defed : boolean; //moved to public
        kVA_fm, M_fm, D_fm, Tau_fm, Ki_fm,
        Pm_fm,
        init_time,               //default 0.5s to flat the initial condition
        k_dltP: Double;                  // determine the input of PV: u_i = k_dltP * \Delta P + omg_fm
        // delay to uppper level
        up_dly: Double;         //in seconds
        nup_dlys,                //nup_dlys := up_dly / t_intvl_smpl;
        virtual_Ld_Nd: Integer; // denotes which node talks to upper level
                                // default by 1;

        // attack and defense
        d_atk_inited,
        z_dfs_inited: Boolean;        // initializing attack if attack is dynamic
        atk_node_num: Integer;        //default no. 1;
        atk_time,                            //when the attack starts to work, default by 0.5s.
        beta_dfs,                            //defense index
        D_beta,                              //parameter for Kc (gradient control)
        D_p,                                 //attack on gradient control: 1: no attack; -1: make the gradient control work to the oppesite
        dlt_z0: Double;
        pCommHide: Array of Array of SmallInt; // communication matrix of this cluster
        pCommNode_Hide: Array of Array of SmallInt; // communication matrix of this cluster

        // Bus_code,
        // NodeNum,
        // Node_Ref: Integer;

        procedure ResetDelaySteps(iNodeNum: Integer);

        // attack and defense
        procedure update_attack(); // update d_i
        procedure update_defense();// update z_i
        function organise_dfs_node(j: Integer): Double;// update z_i

        procedure Init_nodeFM(iNodeNum: Integer);
        procedure Get_PDElem_terminal_voltage(nd_num_in_cluster: Integer; devName: String; Tern_num: Integer); //
        procedure Calc_Alpha_for_PDNode(NodeNum: Integer);
        procedure update_all_nodes_info();
        // function AvgPmax(): Double;
        // function AvgQmax(): Double;
        procedure Get_PQ_DI(var nodeVars: TVLNodeVars);
        function Calc_Grdt_for_Alpha(NodeNuminClstr, phase_num: Integer): Double;
        function Calc_Grdt_for_Alpha_vivj(NodeNuminClstr, phase_num: Integer): Double;
        function Getgradient(j, phase_num: Integer; Bii, Volt_Trhd: Double): Double;

        function Calc_GP_AlphaP(phase_num: Integer): Double;
        function Get_power_trans(): Double;

        function Coef_Phi(x: Double): Double;  // a coeffient
    PUBLIC
        FMonClass: TFMonitor;
        nodeFMs: Array of TVLNodeVars;
        p_mode: Integer;

        // -- overview information about this cluster --
        ld_fm_info: array [0..3] of TLD_fm_infos;
        // define properties for equivalent generator for simulate frequency
        eg_defed: Boolean;
        dlt_fm, omg_fm: Double;
        comp_omg: Double;

        // define properties for attack and defense
        atk: Boolean; //default = false
        dfs: Boolean; //default = false

        constructor Create(ParClass: TDSSClass; const MonitorName: String);
        destructor Destroy; OVERRIDE;
        procedure PropertySideEffects(Idx: Integer; previousIntVal: Integer; setterFlags: TDSSPropertySetterFlags); override;
        procedure MakeLike(OtherPtr: Pointer); override;
        procedure CustomSetRaw(Idx: Integer; Value: String); override;

        procedure MakePosSequence(); OVERRIDE;  // Make a positive Sequence Model, reset nphases
        procedure RecalcElementData(); OVERRIDE;
        procedure ResetIt();

        // unified voltage (1-V_dg)^2
        function Calc_Alpha_M2(NodeNumofDG, phase_num: Integer; dbNodeRef: Integer; Bii, beta, Volt_Trhd: Double): Double;
        // minimize loss
        function Calc_Alpha_L(NodeNumofDG, phase_num: Integer; dbNodeRef: Integer; Bii, beta, Volt_Trhd: Double): Double;
        function Calc_Alpha_L_vivj(NodeNumofDG, phase_num: Integer; dbNodeRef: Integer; Bii, beta, Volt_Trhd: Double): Double;
        function Calc_Alpha_LnM2(NodeNumofDG, phase_num: Integer; dbNodeRef: Integer; Bii, beta, Volt_Trhd: Double): Double;

        function Calc_AlphaP(NodeNuminClstr, phase_num: Integer): Double;

        // Zero seq.
        function Calc_fm_ul_0(NodeNumofDG, phase_num: Integer; dbNodeRef: Integer; Bii, beta, Volt_Trhd: Double): Double;
        function Calc_fm_us_0(NodeNumofDG, phase_num: Integer; dbNodeRef: Integer; Bii, beta, Volt_Trhd: Double): Double;

        procedure Init_delay_array(NodeNumofDG: Integer);

        // For real power control-dynamic simu
        function Calc_ul_P(NodeNuminClstr, phase_num: Integer): Double;
        function Calc_Gradient_ct_P(NodeNuminClstr, phase_num: Integer): Double;  // curtailment

        procedure update_node_info_each_time_step(); //all nodes in the cluster
        procedure update_ld_dly(); // all nodes in this cluster with delay
        procedure Calc_P_freq_fm();// calculte frequency for each cluster
    end;

implementation

uses
    ParserDel,
    DSSClassDefs,
    DSSGlobals,
    Circuit,
    CktTree,
    CktElement,
    Transformer,
    PCElement,
    PDElement,
    Sysutils,
    ucmatrix,
    showresults,
    mathUtil,
    Dynamics,
    PstCalc,
    Terminal,
    Generic5OrderMach,
    Generic5Helper,
    strutils,
    Capacitor,
    Load,
    DSSHelper,
    DSSObjectHelper,
    TypInfo;

type
    TObj = TFMonitorObj;
    TProp = TFMonitorProp;
    TPropLegacy = TFMonitorPropLegacy;
const
    NumPropsThisClass = Ord(High(TProp));
var
    PropInfo: Pointer = nil;
    PropInfoLegacy: Pointer = nil;
    ActionEnum: TDSSEnum;

constructor TFMonitor.Create(dssContext: TDSSContext);
begin
    if PropInfo = nil then
    begin
        PropInfo := TypeInfo(TProp);
        PropInfoLegacy := TypeInfo(TPropLegacy);
        ActionEnum := TDSSEnum.Create('FMonitor: Action', True, 1, 1, ['Clear', 'Reset'], [0, 0]);
    end;
    inherited Create(dssContext, FMON_ELEMENT, 'FMonitor');

    bCurtl := false;
end;

destructor TFMonitor.Destroy;
begin
    inherited Destroy;
end;

procedure DoAction(obj: TObj; action: Integer);
begin
    if action <> 0 then
        Exit;
    obj.ResetIt();
end;

function getProp_Empty(obj: TObj): String;
begin
    Result := '';
end;

function getProp_Attack_Defense(obj: TObj): String; forward;
function getProp_EGen(obj: TObj): String; forward;
function getProp_Volt_Limits_pu(obj: TObj): String; forward;

procedure TFMonitor.DefineProperties();
var
    obj: TObj = NIL; // NIL (0) on purpose
begin
    Numproperties := NumPropsThisClass;
    CountPropertiesAndAllocate();
    PopulatePropertyNames(0, NumPropsThisClass, PropInfo, PropInfoLegacy);

    // boolean properties
    PropertyType[ord(TProp.b_Curt_Ctrl)] := TPropertyType.BooleanProperty;
    PropertyOffset[ord(TProp.b_Curt_Ctrl)] := PtrInt(@obj.ld_fm_info[0].b_curt_ctrl);

    // integer properties
    PropertyType[ord(TProp.Terminal)] := TPropertyType.IntegerProperty;
    PropertyType[ord(TProp.Cluster_Num)] := TPropertyType.IntegerProperty;
    PropertyType[ord(TProp.P_Mode)] := TPropertyType.IntegerProperty;
    PropertyType[ord(TProp.MaxLocalMem)] := TPropertyType.IntegerProperty;
    PropertyType[ord(TProp.Nodes)] := TPropertyType.IntegerProperty;

    PropertyOffset[ord(TProp.Terminal)] := PtrInt(@obj.MeteredTerminal);
    PropertyOffset[ord(TProp.Cluster_Num)] := PtrInt(@obj.Cluster_num);
    PropertyOffset[ord(TProp.P_Mode)] := PtrInt(@obj.p_mode);
    PropertyOffset[ord(TProp.MaxLocalMem)] := PtrInt(@obj.MaxLocalMem);
    PropertyOffset[ord(TProp.Nodes)] := PtrInt(@obj.Nodes);

    PropertyType[ord(TProp.Node_Num)] := TPropertyType.IntegerProperty;
    PropertyOffset[ord(TProp.Node_Num)] := PtrInt(@obj.Node_num);
    PropertyFlags[ord(TProp.Node_Num)] := [TPropertyFlag.Unused];

    PropertyType[ord(TProp.Virtual_LD_Node)] := TPropertyType.IntegerProperty;
    PropertyOffset[ord(TProp.Virtual_LD_Node)] := PtrInt(@obj.virtual_Ld_Nd);
    PropertyFlags[ord(TProp.Virtual_LD_Node)] := [TPropertyFlag.NonNegative, TPropertyFlag.NonZero, TPropertyFlag.ValueOffset];
    PropertyValueOffset[ord(TProp.Virtual_LD_Node)] := -1; // exposed as 1-based, internally 0-based

    // object reference
    PropertyType[ord(TProp.Element)] := TPropertyType.DSSObjectReferenceProperty;
    PropertyOffset[ord(TProp.Element)] := PtrInt(@obj.MeteredElement);
    PropertyOffset2[ord(TProp.Element)] := 0;
    PropertyFlags[ord(TProp.Element)] := [TPropertyFlag.DynamicDefault]; // TPropertyFlag.CheckForVar]; // not required for general cktelements

    // enum action
    PropertyType[ord(TProp.Action)] := TPropertyType.StringEnumActionProperty;
    try
        PropertyOffset[ord(TProp.Action)] := PtrInt(@DoAction);
    except

    end;
    PropertyOffset2[ord(TProp.Action)] := PtrInt(ActionEnum);

    // double
    PropertyOffset[ord(TProp.P_Trans_Ref)] := PtrInt(@obj.p_trans_ref);
    PropertyScale[ord(TProp.P_Trans_Ref)] := 1000;
    PropertyOffset[ord(TProp.T_IntVL_Smpl)] := PtrInt(@obj.T_intvl_smpl);
    PropertyOffset[ord(TProp.Up_Dly)] := PtrInt(@obj.up_dly);

    // **very** custom, weird stuff

    PropertyType[ord(TProp.Attack_Defense)] := TPropertyType.StringProperty;
    PropertyType[ord(TProp.Comm_Hide)] := TPropertyType.StringProperty;
    PropertyType[ord(TProp.Comm_Node_Hide)] := TPropertyType.StringProperty;
    PropertyType[ord(TProp.CommDelayVector)] := TPropertyType.StringProperty;
    PropertyType[ord(TProp.CommVector)] := TPropertyType.StringProperty;
    PropertyType[ord(TProp.EGen)] := TPropertyType.StringProperty;
    PropertyType[ord(TProp.ElemTableLine)] := TPropertyType.StringProperty;
    PropertyType[ord(TProp.Volt_Limits_pu)] := TPropertyType.StringProperty;

    PropertyOffset[ord(TProp.Attack_Defense)] := 1;
    PropertyOffset[ord(TProp.Comm_Hide)] := 1;
    PropertyOffset[ord(TProp.Comm_Node_Hide)] := 1;
    PropertyOffset[ord(TProp.CommDelayVector)] := 1;
    PropertyOffset[ord(TProp.CommVector)] := 1;
    PropertyOffset[ord(TProp.EGen)] := 1;
    PropertyOffset[ord(TProp.ElemTableLine)] := 1;
    PropertyOffset[ord(TProp.Volt_Limits_pu)] := 1;

    PropertyFlags[ord(TProp.Attack_Defense)] := [TPropertyFlag.CustomSetRaw, TPropertyFlag.CustomGet, TPropertyFlag.ReadByFunction];
    PropertyFlags[ord(TProp.Comm_Hide)] := [TPropertyFlag.CustomSetRaw, TPropertyFlag.CustomGet, TPropertyFlag.ReadByFunction];
    PropertyFlags[ord(TProp.Comm_Node_Hide)] := [TPropertyFlag.CustomSetRaw, TPropertyFlag.CustomGet, TPropertyFlag.ReadByFunction];
    PropertyFlags[ord(TProp.CommDelayVector)] := [TPropertyFlag.CustomSetRaw, TPropertyFlag.CustomGet, TPropertyFlag.ReadByFunction];
    PropertyFlags[ord(TProp.CommVector)] := [TPropertyFlag.CustomSetRaw, TPropertyFlag.CustomGet, TPropertyFlag.ReadByFunction];
    PropertyFlags[ord(TProp.EGen)] := [TPropertyFlag.CustomSetRaw, TPropertyFlag.CustomGet, TPropertyFlag.ReadByFunction];
    PropertyFlags[ord(TProp.ElemTableLine)] := [TPropertyFlag.CustomSetRaw, TPropertyFlag.CustomGet, TPropertyFlag.ReadByFunction];
    PropertyFlags[ord(TProp.Volt_Limits_pu)] := [TPropertyFlag.CustomSetRaw, TPropertyFlag.CustomGet, TPropertyFlag.ReadByFunction];

    PropertyReadFunction[ord(TProp.Attack_Defense)] := @getProp_Attack_Defense;
    PropertyReadFunction[ord(TProp.EGen)] := @getProp_EGen;
    PropertyReadFunction[ord(TProp.Volt_Limits_pu)] := @getProp_Volt_Limits_pu;

    PropertyReadFunction[ord(TProp.Comm_Hide)] := @getProp_Empty;
    PropertyReadFunction[ord(TProp.Comm_Node_Hide)] := @getProp_Empty;
    PropertyReadFunction[ord(TProp.CommDelayVector)] := @getProp_Empty;
    PropertyReadFunction[ord(TProp.CommVector)] := @getProp_Empty;
    PropertyReadFunction[ord(TProp.ElemTableLine)] := @getProp_Empty;
    
    ActiveProperty := NumPropsThisClass;
    inherited DefineProperties();
end;

function getProp_Attack_Defense(obj: TObj): String;
var
    d_atk0: Double;
begin
    if (obj.atk_node_num >= 0) and (obj.atk_node_num < obj.Nodes) then
        d_atk0 := obj.nodeFMs[obj.atk_node_num].d_atk0
    else
        d_atk0 := -999;

    Result := Format('[%s, %s, %g, %d, %g, %g, %g, %g]', [
        ifthen(obj.atk, '"yes"', '"no"'),
        ifthen(obj.dfs, '"yes"', '"no"'),
        obj.atk_time,
        obj.atk_node_num + 1,
        d_atk0,
        obj.beta_dfs,
        obj.D_beta,
        obj.D_p
    ]);
end;

function getProp_EGen(obj: TObj): String;
begin
    Result := Format('[%g, %g, %g, %g, %g, %g, %g]', [
        obj.kVA_fm,
        obj.M_fm,
        obj.D_fm,
        obj.Tau_fm,
        obj.Ki_fm,
        obj.init_time,
        obj.k_dltP
    ]);
end;

function getProp_Volt_Limits_pu(obj: TObj): String;
begin
    Result := Format('[%d, %g, %g]', [
        0, // iPhasenum
        obj.ld_fm_info[0].volt_hgh_lmt,
        obj.ld_fm_info[0].volt_lw_lmt
    ]);
end;

procedure TFMonitorObj.CustomSetRaw(Idx: Integer; Value: String);
var
    i: Integer;
    iNodeNum: Integer; //TODO: validate iNodeNum
    iPhasenum: Integer;
begin
    DSS.AuxParser.SetCmdString(Value);
    DSS.AuxParser.NextParam();

    case TProp(Idx) of
        TProp.Attack_Defense: 
        begin
            atk := InterpretYesNo(DSS.AuxParser.MakeString());
            
            DSS.AuxParser.NextParam();
            dfs := InterpretYesNo(DSS.AuxParser.MakeString());
            
            DSS.AuxParser.NextParam();
            atk_time := DSS.AuxParser.MakeDouble();
            
            DSS.AuxParser.NextParam();
            atk_node_num := DSS.AuxParser.MakeInteger() - 1; // TODO: validate atk_node_num?
            
            DSS.AuxParser.NextParam();
            nodeFMs[atk_node_num].d_atk0 := DSS.AuxParser.MakeDouble();
            
            DSS.AuxParser.NextParam();
            beta_dfs := DSS.AuxParser.MakeDouble();
            
            DSS.AuxParser.NextParam();
            D_beta := DSS.AuxParser.MakeDouble();
            
            DSS.AuxParser.NextParam(); // direction of gradient control
            D_p := DSS.AuxParser.MakeDouble();
        end;
        TProp.Comm_Hide:
        begin
            iNodeNum := DSS.AuxParser.MakeInteger() - 1; //node number defined in cluster

            for i := 0 to Nodes - 1 do
            begin
                DSS.AuxParser.NextParam(); // ignore any parameter name  not expecting any
                if Length(DSS.AuxParser.MakeString()) > 0 then
                    pCommHide[iNodeNum, i] := DSS.AuxParser.MakeInteger();
            end;
        end;
        TProp.Comm_Node_Hide:
        begin
            iNodeNum := DSS.AuxParser.MakeInteger() - 1; //node number defined in cluster

            for i := 0 to Nodes - 1 do
            begin
                DSS.AuxParser.NextParam(); // ignore any parameter name  not expecting any
                if Length(DSS.AuxParser.MakeString()) > 0 then
                    pCommNode_Hide[iNodeNum, i] := DSS.AuxParser.MakeInteger();
            end;
        end;
        TProp.CommDelayVector:
        begin
            iNodeNum := DSS.AuxParser.MakeInteger() - 1; //node number defined in cluster

            for i := 0 to Nodes - 1 do
            begin
                DSS.AuxParser.NextParam(); // ignore any parameter name  not expecting any
                if Length(DSS.AuxParser.MakeString()) > 0 then
                    pCommDelayMatrix[iNodeNum, i] := DSS.AuxParser.MakeDouble();
            end;
            ResetDelaySteps(iNodeNum);  //Use pCommDelayMatrix to calculate pCommDelaySteps
        end;
        TProp.CommVector:
        begin
            iNodeNum := DSS.AuxParser.MakeInteger() - 1; //node number defined in cluster

            for i := 0 to Nodes - 1 do //TODO: check -- out of bounds?
            begin
                DSS.AuxParser.NextParam(); // ignore any parameter name  not expecting any
                if Length(DSS.AuxParser.MakeString()) = 0 then
                    continue;

                pCommMatrix[iNodeNum, i] := DSS.AuxParser.MakeInteger();
                pCommHide[iNodeNum, i] := DSS.AuxParser.MakeInteger();       //default
                pCommNode_Hide[iNodeNum, i] := DSS.AuxParser.MakeInteger();  //default
            end;
        end;
        TProp.EGen:
        begin
            kVA_fm := DSS.AuxParser.MakeDouble();
            
            DSS.AuxParser.NextParam();
            M_fm := DSS.AuxParser.MakeDouble();
            
            DSS.AuxParser.NextParam();
            D_fm := DSS.AuxParser.MakeDouble();
            
            DSS.AuxParser.NextParam();
            Tau_fm := DSS.AuxParser.MakeDouble();
            
            DSS.AuxParser.NextParam();
            Ki_fm := DSS.AuxParser.MakeDouble();
            
            DSS.AuxParser.NextParam(); // init_time
            init_time := DSS.AuxParser.MakeDouble();
            
            DSS.AuxParser.NextParam(); // k_dltP is the coordinator
            k_dltP := DSS.AuxParser.MakeDouble();
            
            eg_defed := (kVA_fm * M_fm * D_fm * Tau_fm * Ki_fm <> 0.0); //eg_defed := false by default
        end;
        TProp.ElemTableLine:
        begin
            iNodeNum := DSS.AuxParser.MakeInteger() - 1; //node number defined in the cluster

            DSS.AuxParser.NextParam(); // the first entry is the number of the iNode
            nodeFMs[iNodeNum].vl_strBusName := DSS.AuxParser.MakeString(); //node number defined in the cluster

            DSS.AuxParser.NextParam();
            nodeFMs[iNodeNum].vl_strMeasuredName := DSS.AuxParser.MakeString(); //Element name load into data str

            // nodeFMs[iNodeNum].vl_strName_dg := nodeFMs[iNodeNum].vl_strMeasuredName;

            DSS.AuxParser.NextParam();
            nodeFMs[iNodeNum].vl_terminalNum := DSS.AuxParser.MakeInteger();  //Terminal number load into data str

            DSS.AuxParser.NextParam();
            nodeFMs[iNodeNum].vl_V_ref_dg := 1000 * DSS.AuxParser.MakeDouble();

            DSS.AuxParser.NextParam();
            nodeFMs[iNodeNum].vl_kc_ul_dg := DSS.AuxParser.MakeDouble();

            Init_nodeFM(iNodeNum);
        end;
        TProp.Volt_Limits_pu:
        begin
            iPhasenum := DSS.AuxParser.MakeInteger(); //node number defined in cluster
            if iPhaseNum = 0 then
            begin
                DSS.AuxParser.NextParam(); // high limit
                ld_fm_info[0].volt_hgh_lmt := DSS.AuxParser.MakeDouble(); // NOTE: Not used
                DSS.AuxParser.NextParam(); // low limit
                ld_fm_info[0].volt_lw_lmt := DSS.AuxParser.MakeDouble();
            end;
        end;
    end;
end;

procedure TFMonitorObj.PropertySideEffects(Idx: Integer; previousIntVal: Integer; setterFlags: TDSSPropertySetterFlags);
var
    i, j: Integer;
begin
    if (Idx < 0) or (Idx > NumPropsThisClass) then
    begin
        inherited PropertySideEffects(Idx, previousIntVal, setterFlags);
        Exit;
    end;

    case TProp(Idx) of
        TProp.Nodes:
        begin
            SetLength(nodeFMs, 0);
            SetLength(pCommMatrix, 0, 0);
            SetLength(pCommDelayMatrix, 0, 0);
            SetLength(pCommDelaySteps, 0, 0);
            SetLength(pCommHide, 0, 0);
            SetLength(pCommNode_Hide, 0, 0);

            SetLength(nodeFMs, Nodes);
            SetLength(pCommMatrix, Nodes, Nodes);
            SetLength(pCommDelayMatrix, Nodes, Nodes);
            SetLength(pCommDelaySteps, Nodes, Nodes);
            SetLength(pCommHide, Nodes, Nodes);
            SetLength(pCommNode_Hide, Nodes, Nodes);

            for i := 0 to Nodes - 1 do
                for j := 0 to Nodes - 1 do
                    pCommDelayMatrix[i, j] := 0.0;
        end;
        TProp.T_IntVL_Smpl:
        begin
            for i := 0 to Nodes - 1 do
                ResetDelaySteps(i);
        end;
        TProp.Up_Dly:
        begin
            if t_intvl_smpl <> 0.0 then
            begin
                if frac(up_dly / t_intvl_smpl) <> 0.0 then
                    nUp_dlys := trunc(up_dly / t_intvl_smpl)
                else
                    nUp_dlys := trunc(up_dly / t_intvl_smpl) + 1;
            end
            else
            begin
                nUp_dlys := 0;
            end;
        end;
    end;

    inherited PropertySideEffects(Idx, previousIntVal, setterFlags);
end;

function TFMonitor.NewObject(const ObjName: String; Activate: Boolean): Pointer;
var
    obj: TObj;
begin
    if not ALTDSS_ENABLE_GENERIC5 then
    begin
        Result := NIL;
        raise Exception.Create(
            'FMonitor is currently disabled. It has not been fully validated. ' +
            'If you wish to enable it, set the environment variable ALTDSS_ENABLE_GENERIC5=1 ' +
            'before loading the AltDSS engine.'
        );
    end;
    obj := TObj.Create(Self, ObjName);
    if Activate then
        ActiveCircuit.SetActiveCktElement(obj);
    obj.ClassIndex := AddObjectToList(obj, Activate);
    Result := obj;
end;

function TFMonitor.EndEdit(ptr: Pointer; const NumChanges: Integer): Boolean;
var
    obj: TObj;
begin
    obj := TObj(ptr);
    obj.RecalcElementData();
    Exclude(obj.Flags, Flg.EditingActive);
    Result := true;
end;

procedure TFMonitor.ResetAll();
var
    FMon: TFMonitorObj;
begin
    for FMon in ElementList do
    begin
        if FMon.Enabled() then
            FMon.ResetIt();
    end;
end;

//This function is used to measure total net power of a cluster
procedure TFMonitor.update_sys_ld_info();
var
    FMon: TFMonitorObj;
    dv_lwst: Double;
begin
    LD_FM[0].volt_hghst := -999999;
    LD_FM[0].volt_lwst := 9999999;
    for FMon in ElementList do
    begin
        //update all agents information:
        //synchronous: voltage to agents
        //asynchronous: aphga, ahphaP, highest/lowest voltage
        if FMon.Enabled() then
        begin
            FMon.update_node_info_each_time_step(); //update old z_dfs, vl_alpha_dgn
            FMon.update_ld_dly(); //with delay
        end;

        // Update cluster info to center
        if LD_FM[0].volt_hghst < FMon.ld_fm_info[0].volt_hghst then
        begin
            LD_FM[0].volt_hghst := FMon.ld_fm_info[0].volt_hghst;
            // LD_FM[0].ndnum_hghst := FMon.ld_fm_info[0].ndnum_hghst;
            // LD_FM[0].clstr_num_hghst := FMon.Cluster_num;
            // LD_FM[0].volt_hgh_lmt := fmon.ld_fm_info[0].volt_hgh_lmt;
            LD_FM[0].b_ctrl_hghst := FMon.ld_fm_info[0].b_ctrl_hghst;
        end;
        if LD_FM[0].volt_lwst > FMon.ld_fm_info[0].volt_lwst then
        begin
            LD_FM[0].volt_lwst := FMon.ld_fm_info[0].volt_lwst;
            // LD_FM[0].ndnum_lwst := FMon.ld_fm_info[0].ndnum_lwst;
            // LD_FM[0].clstr_num_lwst := FMon.Cluster_num;
            LD_FM[0].volt_lw_lmt := fmon.ld_fm_info[0].volt_lw_lmt;
            // LD_FM[0].b_ctrl_lwst := FMon.ld_fm_info[0].b_ctrl_lwst;
        end;

        // ---- curtailment ----- bCurtl := t/f for overall system ------
        //curtailment is needed or not
        if FMon.ld_fm_info[0].volt_hghst > 1.0 then
            FMon.ld_fm_info[0].b_ctrl_hghst := true     //need curtailment
        else
            FMon.ld_fm_info[0].b_ctrl_hghst := false;
        // ---- each cluster may have their own ---
    end;
      //curtailment is needed or not
    // vtemp := (LD_FM[0].volt_hghst - LD_FM[0].volt_lwst);//p.u.
    dv_lwst := LD_FM[0].volt_lwst - LD_FM[0].volt_lw_lmt;//0.95; // must greater than 0.0
    if (dv_lwst < 0.0) then
    begin
        bCurtl := true;  //curtailment
    end
    else
    begin
        bCurtl := false;//dont need curtailment
    end;
end;

procedure TFMonitor.Calc_P_freq();// calculte frequency for each cluster
var
    FMon: TFMonitorObj;
begin
    for FMon in ElementList do
    begin
        if FMon.Enabled() and FMon.eg_defed then
            FMon.Calc_P_freq_fm(); //w
    end;
end;

procedure TFMonitor.update_atks();
var
    FMon: TFMonitorObj;
begin
    for FMon in ElementList do
    begin
        if FMon.Enabled() and FMon.atk then
            FMon.update_attack(); //w
    end;
end;

procedure TFMonitor.update_defense_layer();
var
    FMon: TFMonitorObj;
begin
    for FMon in ElementList do
    begin
        if FMon.Enabled() and FMon.dfs then
            FMon.update_defense(); //w
    end;
end;

procedure TFMonitorObj.MakeLike(OtherPtr: Pointer);
var
    OtherMonitor: TFMonitorObj;
begin
    OtherMonitor := TObj(OtherPtr);
    // See if we can find this Monitor name in the present collection
    FNPhases := OtherMonitor.Fnphases;
    FNConds := OtherMonitor.FNConds; // Force Reallocation of terminal stuff
    MeteredElement := OtherMonitor.MeteredElement;  // Pointer to target circuit element
    MeteredTerminal := OtherMonitor.MeteredTerminal;
end;

constructor TFMonitorObj.Create(ParClass: TDSSClass; const MonitorName: String);
var
    i: Integer;
begin
    inherited Create(ParClass, MonitorName);

    FMonClass := TFMonitor(ParClass);
    FNphases := 3;  // Directly set conds and phases
    FNConds := 3;
    SetNTerms(1);  // this forces allocation of terminals and conductors in base class
    // Current Buffer has to be big enough to hold all terminals
    MeteredElement := TDSSCktElement(ActiveCircuit.CktElements.Get(1)); // Default to first circuit element (source)
    MeteredTerminal := 1;
    DSSObjType := ParClass.DSSClassType; //MON_ELEMENT;
    Nodes := 33;//default Nodes in one cluster
    PropertySideEffects(ord(TProp.Nodes), 0, []);

    // ReAllocMem(f_Value_one_V, 999 * sizeof(f_Value_one_V[1]));
    // ReAllocMem(F_Value_one_S, 999 * sizeof(F_Value_one_S[1]));
    T_intvl_smpl := 0;
    MaxLocalMem := 10;


    // leader information
    for i := 0 to 3 do
    begin
        // ld_fm_info[i].ndnum_hghst := 0;
        ld_fm_info[i].b_ctrl_hghst := false;
        ld_fm_info[i].volt_hghst := -1.0; //small number that can never be true
        // ld_fm_info[i].volt_hgh_lmt := 1.05;
        // ld_fm_info[i].Pinjec_hghst := 0.0;
        // ld_fm_info[i].ndnum_lwst := 0;
        // ld_fm_info[i].b_ctrl_lwst := false;
        ld_fm_info[i].volt_lw_lmt := 0.95;
        ld_fm_info[i].volt_lwst := 9999999999.0; //large nunber can never be true
        // ld_fm_info[i].Pinjec_lwst := 0.0;
        ld_fm_info[i].volt_avg := 0.0;
        ld_fm_info[i].total_pg := 0.0;
        ld_fm_info[i].total_pl := 0.0;
        ld_fm_info[i].b_Curt_Ctrl := false;
    end;
    virtual_Ld_Nd := 0;
    nUp_dlys := 0;
     //bCurtl_Clstr := false;
     // end of initializing leader
     //virtual generator for frequency
    eg_defed := false;
    kVA_fm := 0.0;
    M_fm := 0.0;
    D_fm := 0.0;
    Tau_fm := 0.0;
    Ki_fm := 0.0;
    dlt_fm := 0.0;
    omg_fm := 0.0;
    Pm_fm := 0.0;
    init_time := 0.5;
    comp_omg := 0.0;
     // when the attack time starts
    atk := false;
    atk_time := 0.5;
    atk_node_num := 0;
    d_atk_inited := false;
    z_dfs_inited := false;
    D_beta := 1;
    D_p := 1;
    dlt_z0 := 0.0;
end;

destructor TFMonitorObj.Destroy;
begin
     //MonitorStream.Free;
    // if Assigned(f_Value_one_V) then
    //     ReallocMem(f_Value_one_V, 0);
    // if Assigned(F_Value_one_S) then
    //     ReallocMem(F_Value_one_S, 0);

    inherited Destroy;
end;

procedure TFMonitorObj.RecalcElementData();
begin
    if MeteredElement = NIL then
    begin
        DoErrorMsg(FullName(), 
            'Target circuit element (in "Element" property) for not found or not provided.',
            ' Element must be defined previously.', 666);
        Exit;
    end;
    if not (MeteredElement is TPDElement) then
    begin
        DoErrorMsg(FullName(), 
            'Target circuit element (in "Element" property) is not a PD element.',
            ' Element must be a PD element.', 667);
        Exit;
    end;

    if MeteredTerminal > MeteredElement.NTerms() then
    begin
        DoErrorMsg('FMonitor: "' + Name + '"',
            Format(_('Terminal number %d does not exist.'), [MeteredTerminal]),
            'Respecify terminal number.', 665);
    end
    else
    begin
        FNphases := MeteredElement.NPhases;
        FNConds := MeteredElement.NConds();

        // Sets name of i-th terminal's connected bus in monitor's buslist
        // This value will be used to set the NodeRef array (see TakeSample)
        Setbus(1, MeteredElement.GetBus(MeteredTerminal));
    end;
end;

procedure TFMonitorObj.MakePosSequence();
begin
    if MeteredElement <> nil then
    begin
        Setbus(1, MeteredElement.GetBus(MeteredTerminal));
        FNphases := MeteredElement.NPhases;
        FNConds := MeteredElement.NConds();
    end;
    inherited;
end;

procedure TFMonitorObj.Get_PQ_DI(var nodeVars: TVLNodeVars);
var
    i, j, num: Integer;
    pElement: TDSSCktElement;
    // pLoad: TLoadObj;
    cBuffer: Array of Complex;
    mapnn: Integer;
begin
    with nodeVars do
    begin
        case ldType of
            0:// one 3 phase or 2 phase load
            begin
                pElement := ActiveCircuit.PCElements.Get(ldidx);
                num := pElement.NPhases;
                SetLength(cBuffer, num);
                pElement.GetPhasePower(pComplexArray(@cBuffer[0]));// power

                for j := 0 to num - 1 do
                begin
                    i := pElement.Terminals[0].TermNodeRef[j];
                    mapnn := ActiveCircuit.MapNodeToBus[i].NodeNum - 1;
                    if (mapnn >= 0) and (mapnn < 3) then
                    begin
                        vl_P_Di1 := cBuffer[i].re;
                        vl_Q_Di1 := cBuffer[i].im;
                    end;
                end;
            end;
            1, 2, 3:
            begin
                pElement := nil;
                if ldidx1 > 0 then
                begin
                    pElement := ActiveCircuit.PCElements.Get(ldidx1);
                    num := pElement.NPhases;
                    SetLength(cBuffer, num);
                    pElement.GetPhasePower(pComplexArray(@cBuffer[0]));

                    for j := 0 to num - 1 do
                    begin
                        i := pElement.Terminals[0].TermNodeRef[j];
                        mapnn := ActiveCircuit.MapNodeToBus[i].NodeNum - 1;
                        if (mapnn >= 0) and (mapnn < 3) then
                        begin
                            vl_P_Di1 := cBuffer[i].re;
                            vl_Q_Di1 := cBuffer[i].im;
                        end;
                    end;
                end;
                if ldidx2 > 0 then
                begin
                    pElement := ActiveCircuit.PCElements.Get(ldidx2);
                    num := pElement.NPhases;
                    SetLength(cBuffer, num);
                    pElement.GetPhasePower(pComplexArray(@cBuffer[0]));

                    for j := 0 to num - 1 do
                    begin
                        i := pElement.Terminals[0].TermNodeRef[j];
                        mapnn := ActiveCircuit.MapNodeToBus[i].NodeNum - 1;
                        if (mapnn >= 0) and (mapnn < 3) then
                        begin
                            vl_P_Di2 := cBuffer[i].re;
                            vl_Q_Di2 := cBuffer[i].im;
                        end;
                    end;
                end;
                if ldidx3 > 0 then
                begin
                    pElement := ActiveCircuit.PCElements.Get(ldidx3);
                    num := pElement.NPhases;
                    SetLength(cBuffer, num);
                    pElement.GetPhasePower(pComplexArray(@cBuffer[0]));

                    for j := 0 to num - 1 do
                    begin
                        i := pElement.Terminals[0].TermNodeRef[j];
                        mapnn := ActiveCircuit.MapNodeToBus[i].NodeNum - 1;
                        if (mapnn >= 0) and (mapnn < 3) then
                        begin
                            vl_P_Di3 := cBuffer[i].re;
                            vl_Q_Di3 := cBuffer[i].im;
                        end;
                    end;
                end;
            end
        end;
    end;
end;

procedure TFMonitorObj.Init_nodeFM(iNodeNum: Integer);
var
    strTemp: String;
    Devindex, i, j: Integer;
    PCindex_ld: Integer;
    pElem: TDSSCktElement;

    pDG: TGeneric5Obj;
    num: Integer;
begin
    pElem := nil;
    //init all info of this node
    with nodeFMs[iNodeNum] do
    begin
    //1
        strTemp := lowercase(vl_strBusName);
        Bus_Idx := ActiveCircuit.BusList.Find(strTemp);

        Devindex := GetCktElementIndex(DSS, vl_strMeasuredName);                   // Global function
        if DevIndex > 0 then
        begin                                       // Monitored element must already exist
            pElem := ActiveCircuit.CktElements.Get(DevIndex);
        end;

        if pElem = NIL then
            // TODO: ERROR;
            Exit;

        // vl_ndphases := pElem.NPhases;
        {RANGECHECKS OFF}
        vl_basevolt := ActiveCircuit.Buses[bus_idx].kVBase * 1000;
        {RANGECHECKS ON}
        vl_phase_num_dg := -1; //-1 -- no dg under this nodes;0 --3 phases by default

        for j := 1 to 3 do
            vl_nodeType_phase[j] := 2;// by default not dg

        pElem := ActiveCircuit.PCElements.First();
        PCindex_ld := ActiveCircuit.PCElements.ActiveIndex;
        while pElem <> nil do
        begin
            if not pElem.Enabled() then
            begin
                pElem := ActiveCircuit.PCElements.Next;
                PCindex_ld := ActiveCircuit.PCElements.ActiveIndex;
                continue;
            end;

            if pElem.ActiveTerminal.BusRef = Bus_Idx then
            begin
                if (pElem.DSSObjType and CLASSMASK) = GENERIC5ORDERMACH_ELEMENT then
                begin
                    //vl_nodeType should be define per phase
                    vl_ndphases_dg := pElem.NPhases; //1 or 3
                    // under 1 bus, there can be either 3 phase DG or 1 phase dg
                    //set Cluster defination for DG
                    // one 3phase dg
                    // 1,2 or 3  1-phase dgs under each phase
                    pDG := TGeneric5Obj(pElem);
                    num := pDG.ctrl_mode;
                    if (num > 0) and (num <= 3) then
                    begin
                        vl_nodeType_phase[num] := 1;
                    end
                    else
                    begin
                        for j := 1 to 3 do
                            vl_nodeType_phase[j] := 1; // //ctrl_mode = 4 or 0
                    end;
                    //pDG
                    if (pDG.FMonObj = nil) then // first cluster
                    begin
                        pDG.cluster_num := cluster_num;
                        // assign the virtue leader to this DG
                        pDG.FMonObj := self;
                        //FMonObj := ElementList.Get(cluster_num); cluster_num can not be used for 'Get'
                        pDG.NdNumInCluster := iNodeNum;
                        // pDG.nVLeaders := 1; // NOTE: not used
                    end
                    else
                    // the second virtual leader, which means if the 2nd one will always be the one being overwritten
                    if (cluster_num <> pDG.cluster_num) then
                    begin
                        // pDG.cluster_num2 := cluster_num;  // NOTE: not used
                        // assign the virtue leader to this DG
                        pDG.FMonObj2 := self; 
                        //FMonObj := ElementList.Get(cluster_num); cluster_num can not be used for 'Get'
                        // pDG.NdNumInCluster2 := iNodeNum; // NOTE: not used
                        // pDG.nVLeaders := 2; // NOTE: not used
                    end;
                    vl_phase_num_dg := 0; //3 phases by default
                    if vl_ndphases_dg = 1 then
                    begin
                        // if vl_nodeType=1, and vl_ndphases=1,phase_num =1,2,3 0- this node has 3 phases
                        // ctrl_mode is the phase number of this Generic5
                        vl_phase_num_dg := pDG.ctrl_mode;
                    end;
                end;

                ldType := -1;
                ldIdx := -1;
                ldIdx1 := -1;
                ldIdx2 := -1;
                ldIdx3 := -1;

                if (pElem.DSSObjType and CLASSMASK) = LOAD_ELEMENT then
                begin
                    num := pElem.NPhases;
                    case num of 
                        3:
                        begin
                            ldIdx := PCindex_ld;
                            ldType := 0;
                        end;
                        2:
                        begin
                            ldIdx := PCindex_ld;
                            ldType := 0;
                        end;
                        1:
                        begin
                            i := pElem.Terminals[0].TermNodeRef[0];
                            case ActiveCircuit.MapNodeToBus[i].NodeNum of
                                1:
                                    ldIdx1 := PCindex_ld;
                                2:
                                    ldIdx2 := PCindex_ld;
                                3:
                                    ldIdx3 := PCindex_ld;
                            end;
                            if ldType < 1 then
                                ldType := 1
                            else
                            if ldType >= 1 then
                                ldType := ldType + 1;
                            if ldType >= 3 then
                                ldType := 3;
                        end;
                    end;
                end;
            end;

            pElem := ActiveCircuit.PCElements.Next;
            PCindex_ld := ActiveCircuit.PCElements.ActiveIndex;
        end;


        vl_V_ref1_dg := vl_V_ref_dg;
        vl_V_ref2_dg := vl_V_ref_dg;
        vl_V_ref3_dg := vl_V_ref_dg;

        ///  other properties if needed
        // vl_CC_switch_dg := false; // cooperate control switch. true, cooperate control is on
        // vl_PF_flag_dg := 0;//1, real power control is on
        // vl_QV_flag_dg := 0;//1, volt/var control is on
        // vl_volt_thrd_dg := 0.03;
        vl_Alpha_dg := 0;
        vl_Alpha1_dg := 0;
        vl_Alpha2_dg := 0;
        vl_Alpha3_dg := 0;
        vl_Gradient_dg := 0;
        vl_Gradient1_dg := 0;
        vl_Gradient2_dg := 0;
        vl_Gradient3_dg := 0;
        vl_AlphaP_dg := 0;
        vl_Alpha_dgn := 0;
        vl_AlphaP1_dg := 0;
        vl_AlphaP2_dg := 0;
        vl_AlphaP3_dg := 0;
        vl_GradientP_dg := 0;
        vl_GradientP1_dg := 0;
        vl_GradientP2_dg := 0;
        vl_GradientP3_dg := 0;
        vl_Pmax_dg := 0;
        vl_Qmax_dg := 0;
        vl_Pmax_phase_dg := 0;
        vl_Qmax_phase_dg := 0;
        // vl_V_base_dg := 1000 * 2.4;
        vl_V := 1000 * 2.4;
        vl_V1 := 1000 * 2.4;
        vl_V2 := 1000 * 2.4;
        vl_V3 := 1000 * 2.4;
        vl_Q_Di := 0.0;
        vl_Q_Di1 := 0.0;
        vl_Q_Di2 := 0.0;
        vl_Q_Di3 := 0.0;
        // vl_P_Di := 0.0;
        vl_P_Di1 := 0.0;
        vl_P_Di2 := 0.0;
        vl_P_Di3 := 0.0;
        vl_smplCnt := 0;
        vl_crnt_smp_time := 0.0;
        // attack and defense
        d_atk := 0.0;
        z_dfs := 0.0;
        z_dfsn := 0.0;
        d_atk0 := 0.0;
    end;
end;

procedure TFMonitorObj.Get_PDElem_terminal_voltage(nd_num_in_cluster: Integer; devName: String; Tern_num: Integer);
var
    tempTerminal: TPowerTerminal;
    nn, i, Devindex, j: Integer;
    tempElement: TDSSCktElement;
    phase_num: Integer;
    vabs: Double;
    V012: array[0..2] of Complex;
    VaVbVc: array[1..3] of Complex;
begin
    tempElement := nil;
    Devindex := GetCktElementIndex(DSS, devName); // Global function
    if DevIndex > 0 then
    begin  // Monitored element must already exist
        tempElement := ActiveCircuit.CktElements.Get(DevIndex);
    end;

    //TODO: BUG: no checks for NIL
    tempTerminal := tempElement.Terminals[Tern_num - 1];
    nn := nd_num_in_cluster;
    for j := 0 to tempElement.NPhases - 1 do// how many phases of this element
    begin
        i := tempTerminal.TermNodeRef[j];  // global node number
        phase_num := ActiveCircuit.MapNodeToBus[i].NodeNum;
        vabs := cabs(ActiveCircuit.Solution.NodeV[i]);
        if phase_num = 1 then // phase A
        begin
            nodeFMs[nn].vl_V1 := vabs;
            nodeFMs[nn].vl_V_1c := ActiveCircuit.Solution.NodeV[i];
        end
        else
        if phase_num = 2 then    //phase B
        begin
            nodeFMs[nn].vl_V2 := vabs;
            nodeFMs[nn].vl_V_2c := ActiveCircuit.Solution.NodeV[i];
        end
        else
        if phase_num = 3 then    //phase c
        begin
            nodeFMs[nn].vl_V3 := vabs;
            nodeFMs[nn].vl_V_3c := ActiveCircuit.Solution.NodeV[i];
        end;
    end;
    if tempElement.NPhases = 3 then
    begin
        VaVbVc[1] := nodeFMs[nn].vl_V_1c;//phase A
        VaVbVc[2] := nodeFMs[nn].vl_V_2c;
        VaVbVc[3] := nodeFMs[nn].vl_V_3c;
        Phase2SymComp(pComplexArray(@VaVbVc), pComplexArray(@V012));  // Convert abc voltages to 012
        nodeFMs[nn].vl_V := cabs(V012[1]);  //pos. seq. Voltage
    end;

end;

procedure TFMonitorObj.update_all_nodes_info();
var
    i: Integer;
begin
    for i := 0 to Nodes - 1 do
    begin
        with nodeFMs[i] do
        begin
            Get_PDElem_terminal_voltage(i, vl_strMeasuredName, vl_terminalNum);
        end;
    end;
end;

procedure TFMonitorObj.ResetDelaySteps(iNodeNum: Integer);
var
    j, tmp: Integer;
begin
    //calc delay array
    for j := 0 to Nodes - 1 do
    begin
        if (T_intvl_smpl = 0.0) or (pCommDelayMatrix[iNodeNum, j] = 0.0) then
        begin
            pCommDelaySteps[iNodeNum, j] := 0;
        end
        else
        begin
            tmp := trunc(pCommDelayMatrix[iNodeNum, j] / T_intvl_smpl);
            if frac(pCommDelayMatrix[iNodeNum, j] / T_intvl_smpl) = 0.0 then
                pCommDelaySteps[iNodeNum, j] := tmp
            else
                pCommDelaySteps[iNodeNum, j] := tmp + 1;
                //How many delays for communication
        end;
    end
end;

procedure TFMonitorObj.ResetIt();
var
    iTmp: Integer;
begin
    if ActiveCircuit.Solution.DynaVars.SolutionMode <> TSolveMode.DYNAMICMODE then
        Exit;

    // calc Delay_stps for sampling
    if T_intvl_smpl = 0.0 then
        Smpl_stps := 0  //No delay.
    else
    begin
        iTmp := Trunc(T_intvl_smpl / ActiveCircuit.Solution.Dynavars.h);
        if frac(T_intvl_smpl / ActiveCircuit.Solution.Dynavars.h) = 0.0 then
            Smpl_stps := iTmp
        else
            Smpl_stps := iTmp + 1;// uper
    end;
    for iTmp := 0 to Nodes - 1 do
    begin
        nodeFMs[iTmp].vl_smplCnt := 0;
        nodeFMs[iTmp].vl_crnt_smp_time := ActiveCircuit.Solution.DynaVars.intHour * 3600 + ActiveCircuit.Solution.DynaVars.t;
        Init_delay_array(iTmp); // in DYNAMICMODE, init alpha array
    end;
end;

procedure TFMonitorObj.Calc_Alpha_for_PDNode(NodeNum: Integer);
//must be called after self gradient calc
// self gradient calc is in 'Update _pd_node_info'
var
    j, phase_num: Integer;
    sum_Sij_j: Double;
    TempAlpha: Double;
begin
     // calclate alpha
    with nodeFMs[NodeNum] do
        for phase_num := 1 to vl_ndphases_dg do
            case phase_num of
                1:
                begin //phase A
                    TempAlpha := 0.0;//init as zero
                    sum_Sij_j := 0.0;
                    for j := 0 to Nodes - 1 do
                    begin
                        TempAlpha := TempAlpha + pCommMatrix[NodeNum, j] * nodeFMs[j].vl_Alpha1_dg;
                        sum_Sij_j := sum_Sij_j + pCommMatrix[NodeNum, j];
                    end;
                    vl_Alpha1_dg := TempAlpha / sum_Sij_j;
                    vl_Alpha1_dg := vl_Alpha1_dg - vl_kcq_dg * vl_gradient1_dg;
                end;
                2:
                begin //phase B
                    TempAlpha := 0.0;//init as zero
                    sum_Sij_j := 0.0;
                    for j := 0 to Nodes - 1 do
                    begin
                        TempAlpha := TempAlpha + pCommMatrix[NodeNum, j] * nodeFMs[j].vl_Alpha2_dg;
                        sum_Sij_j := sum_Sij_j + pCommMatrix[NodeNum, j];
                    end;
                    vl_Alpha2_dg := TempAlpha / sum_Sij_j;
                    vl_Alpha2_dg := vl_Alpha2_dg - vl_kcq_dg * vl_gradient2_dg;
                end;
                3:
                begin //phase C
                    TempAlpha := 0.0;//init as zero
                    sum_Sij_j := 0.0;
                    for j := 0 to Nodes - 1 do
                    begin
                        TempAlpha := TempAlpha + pCommMatrix[NodeNum, j] * nodeFMs[j].vl_Alpha3_dg;
                        sum_Sij_j := sum_Sij_j + pCommMatrix[NodeNum, j];
                    end;
                    vl_Alpha3_dg := TempAlpha / sum_Sij_j;
                    vl_Alpha3_dg := vl_Alpha3_dg - vl_kcq_dg * vl_gradient3_dg;
                end;
                0:
                begin //pos seq value
                    TempAlpha := 0.0;//init as zero
                    sum_Sij_j := 0.0;
                    for j := 0 to Nodes - 1 do
                    begin
                        TempAlpha := TempAlpha + pCommMatrix[NodeNum, j] * nodeFMs[j].vl_Alpha_dg;
                        sum_Sij_j := sum_Sij_j + pCommMatrix[NodeNum, j];
                    end;
                    vl_Alpha_dg := TempAlpha / sum_Sij_j;
                    vl_Alpha_dg := vl_Alpha_dg - vl_kcq_dg * vl_gradient_dg;
                end;
            else
            end;
end;

// AlphaP Gradient or Pref
function TFMonitorObj.get_power_trans(): Double;  // NodeNuminClstr: node number in cluster
var
    i, j, k: Integer;
    pTerminal: TPowerTerminal;
begin
    TPDElement(MeteredElement).GetCurrents(MeteredElement.Iterminal); //Curr
    pTerminal := MeteredElement.Terminals[MeteredTerminal - 1];
    tempCplx := 0;
    k := (MeteredTerminal - 1) * MeteredElement.NConds();
    for j := 1 to MeteredElement.NConds() do// how many conds of this element
    begin
        i := pTerminal.TermNodeRef[j - 1];  // global node number
        tempCplx += ActiveCircuit.Solution.NodeV[i] * cong(MeteredElement.Iterminal[k + j])//power
    end;
    result := tempCplx.re;
end;

//calculate the gradient for alpha i
function TFMonitorObj.Calc_Grdt_for_Alpha(NodeNuminClstr, phase_num: Integer): Double;
var
    Vtemp, ctmp: complex;
    tmp: Double;
    Gij, Gii, Bii: Double;
    Devindex, i, j, k, jTempTerminal: Integer;
    pElem: TDSSCktElement;
    nodeRefi: Integer;// ref number of this node
    nodeRefj: Integer;// ref number of the upper node
    den: Double;
begin
    pElem := nil;
    nodeRefi := 0;
    nodeRefj := 0;
    Result := 0.0;
    Devindex := GetCktElementIndex(DSS, nodeFMs[NodeNuminClstr].vl_strMeasuredName); // TODO: remove this kind of construction, use pointers instead
    if DevIndex > 0 then
    begin                                       // Monitored element must already exist
        pElem := ActiveCircuit.CktElements.Get(DevIndex);
    end;
    if pElem <> nil then //want to get voltages from the other side of the device
    begin
        with ActiveCircuit.solution do
            for i := 1 to pElem.Yorder do
                pElem.Vterminal[i] := NodeV[pElem.NodeRef[i]]
    end
    else
    begin
        result := 0.0;
    end;
    //k is the terminal number of this end
    k := nodeFMs[NodeNuminClstr].vl_terminalNum;
    //this is the other end jTempTerminal
    if k = 1 then
        jTempTerminal := 2
    else
        jTempTerminal := 1;
    //find the voltage of this phase on this terminal
    for i := 0 to pElem.NPhases - 1 do // how many conds of this element
    begin
        j := pElem.Terminals[jTempTerminal - 1].TermNodeRef[i];
        if ActiveCircuit.MapNodeToBus[j].NodeNum = phase_num then
        begin
            nodeRefj := j;                                   // node ref of the other end of this element and this phase
            vTemp := ActiveCircuit.Solution.NodeV[nodeRefj];
            nodeRefi := pElem.Terminals[k - 1].TermNodeRef[i]; // node ref of this node
        end;
    end;
    if phase_num = 0 then //  cannot deal with pos seq
    begin
        //TODO: return early?
    end;
    ctmp := ActiveCircuit.Solution.Yij(nodeRefi, nodeRefj);
    Gij := ctmp.re;
    // Bij := ctmp.im;
    ctmp := ActiveCircuit.Solution.Yij(nodeRefi, nodeRefi);
    Gii := ctmp.re;
    Bii := ctmp.im;

    with nodeFMs[NodeNuminClstr] do
        case phase_num of //pos seq
            0:
            begin
                result := vl_gradient_dg; //  can not deal with that
            end;
            1:
            begin
                den := vl_Q_DG1 - vl_Q_Di1 - vl_V1 * vl_V1 * Bii;   // pos ctrl: Bii use the first one
                tmp := (vl_V1 - cabs(vTemp) * cos(cang(vTemp) - cang(vl_V_1c)));

                vl_gradient1_dg := vl_V1 * tmp;

                if abs(den) < epsilon then
                    vl_gradient1_dg := 0.0
                else
                    vl_gradient1_dg := vl_gradient1_dg / (den);

                den := vl_P_DG1 - vl_P_Di1 - vl_V1 * vl_V1 * Gii;
                tmp := vl_V1 * cabs(vTemp) * sin(cang(vTemp) - cang(vl_V_1c));
                if abs(den) < epsilon then
                    tmp := 0
                else
                    tmp := tmp / den;
                vl_gradient1_dg := vl_gradient1_dg - tmp;
                tmp := vl_Qmax_phase_dg * Gij;
                vl_gradient1_dg := vl_gradient1_dg * tmp;
                vl_gradient1_dg := vl_gradient1_dg / (vl_V_ref1_dg * vl_V_ref1_dg);//PU value??
                result := vl_gradient1_dg; //
            end;
            2:
            begin
                den := vl_Q_DG2 - vl_Q_Di2 - vl_V2 * vl_V2 * Bii;   // pos ctrl: Bii use the first one
                tmp := (vl_V2 - cabs(vTemp) * cos(cang(vTemp) - cang(vl_V_2c)));

                vl_gradient2_dg := vl_V2 * tmp;

                if abs(den) < epsilon then
                    vl_gradient2_dg := 0.0
                else
                    vl_gradient2_dg := vl_gradient2_dg / (den);

                den := vl_P_DG2 - vl_P_Di2 - vl_V2 * vl_V2 * Gii;
                tmp := vl_V2 * cabs(vTemp) * sin(cang(vTemp) - cang(vl_V_2c));
                if abs(den) < epsilon then
                    tmp := 0
                else
                    tmp := tmp / den;
                vl_gradient2_dg := vl_gradient2_dg - tmp;

                tmp := vl_Qmax_phase_dg * Gij;
                vl_gradient2_dg := vl_gradient2_dg * tmp;
                vl_gradient2_dg := vl_gradient2_dg / (vl_V_ref2_dg * vl_V_ref2_dg);//PU value??

            end;
            3:
            begin
                den := vl_Q_DG3 - vl_Q_Di3 - vl_V3 * vl_V3 * Bii;   // pos ctrl: Bii use the first one
                tmp := (vl_V3 - cabs(vTemp) * cos(cang(vTemp) - cang(vl_V_3c)));

                vl_gradient3_dg := vl_V3 * tmp;

                if abs(den) < epsilon then
                    vl_gradient3_dg := 0.0
                else
                    vl_gradient3_dg := vl_gradient3_dg / (den);

                den := vl_P_DG3 - vl_P_Di3 - vl_V3 * vl_V3 * Gii;
                tmp := vl_V3 * cabs(vTemp) * sin(cang(vTemp) - cang(vl_V_3c));
                if abs(den) < epsilon then
                    tmp := 0
                else
                    tmp := tmp / den;
                vl_gradient3_dg := vl_gradient3_dg - tmp;

                tmp := vl_Qmax_phase_dg * Gij;
                vl_gradient3_dg := vl_gradient3_dg * tmp;
                vl_gradient3_dg := vl_gradient3_dg / (vl_V_ref3_dg * vl_V_ref3_dg);//PU value??

            end;
        end;
end;

//calculate the gradient for alpha i
function TFMonitorObj.Calc_Grdt_for_Alpha_vivj(NodeNuminClstr, phase_num: Integer): Double;
var
    Vtemp, ctmp: complex;
    tmp: Double;
    Gij, Bii: Double;
    Devindex, i, j, k, jTempTerminal: Integer;
    pElem: TDSSCktElement;
    nodeRefi: Integer;// ref number of this node
    nodeRefj: Integer;// ref number of the upper node
    den: Double;
begin
    pElem := nil;
    nodeRefi := 0;
    nodeRefj := 0;
    Result := 0.0;
    //nodeFMs[NodeNuminClstr].vl_strMeasuredName is ther element followed by this bus
    Devindex := GetCktElementIndex(DSS, nodeFMs[NodeNuminClstr].vl_strMeasuredName);
    if DevIndex > 0 then
    begin // Monitored element must already exist
        pElem := ActiveCircuit.CktElements.Get(DevIndex);
    end;
    if pElem <> nil then //want to get voltages from the other side of the device
    begin
        with ActiveCircuit.solution do
            for i := 1 to pElem.Yorder do
                pElem.Vterminal[i] := NodeV[pElem.NodeRef[i]]
    end
    else
        result := 0.0;
      //k is the terminal number of this end
    k := nodeFMs[NodeNuminClstr].vl_terminalNum;
      //this is the other end jTempTerminal
    if k = 1 then
        jTempTerminal := 2
    else
        jTempTerminal := 1;
      //find the voltage of this phase on this terminal
    for i := 0 to pElem.NPhases - 1 do // how many conds of this element
    begin
        j := pElem.Terminals[jTempTerminal - 1].TermNodeRef[i];
        if ActiveCircuit.MapNodeToBus[j].NodeNum = phase_num then
        begin
            nodeRefj := j; // node ref of the other end of this element and this phase
            vTemp := ActiveCircuit.Solution.NodeV[nodeRefj];
            nodeRefi := pElem.Terminals[k - 1].TermNodeRef[i]; // node ref of this node
        end;
    end;
    if phase_num = 0 then //  cannot deal with pos seq
    begin
        //TODO: return early?
    end;
    ctmp := ActiveCircuit.Solution.Yij(nodeRefi, nodeRefj);
    Gij := ctmp.re;
    // Bij := ctmp.im;
    ctmp := ActiveCircuit.Solution.Yij(nodeRefi, nodeRefi);
    // Gii := ctmp.re;
    Bii := ctmp.im;

    with nodeFMs[NodeNuminClstr] do
        case phase_num of //pos seq
            0:
            begin
                result := vl_gradient_dg; //  can not deal with that
            end;
            1:
            begin
                den := vl_Q_DG1 - vl_Q_Di1 - vl_V1 * vl_V1 * Bii;   // pos ctrl: Bii use the first one
                tmp := vl_V1 - cabs(vTemp);

                vl_gradient1_dg := vl_V1 * tmp;

                if abs(den) < epsilon then
                    vl_gradient1_dg := 0.0
                else
                    vl_gradient1_dg := vl_gradient1_dg / (den);

                tmp := vl_Qmax_phase_dg * Gij;
                vl_gradient1_dg := vl_gradient1_dg * tmp;
                vl_gradient1_dg := vl_gradient1_dg / (vl_V_ref1_dg * vl_V_ref1_dg);//PU value??

                result := vl_gradient1_dg; //
            end;
            2:
            begin
                den := vl_Q_DG2 - vl_Q_Di2 - vl_V2 * vl_V2 * Bii;   // pos ctrl: Bii use the first one
                tmp := vl_V2 - cabs(vTemp);

                vl_gradient2_dg := vl_V2 * tmp;

                if abs(den) < epsilon then
                    vl_gradient2_dg := 0.0
                else
                    vl_gradient2_dg := vl_gradient2_dg / (den);

                tmp := vl_Qmax_phase_dg * Gij;
                vl_gradient2_dg := vl_gradient2_dg * tmp;
                vl_gradient2_dg := vl_gradient2_dg / (vl_V_ref2_dg * vl_V_ref2_dg);//PU value??

            end;
            3:
            begin
                den := vl_Q_DG3 - vl_Q_Di3 - vl_V3 * vl_V3 * Bii;   // pos ctrl: Bii use the first one
                tmp := vl_V3 - cabs(vTemp);

                vl_gradient3_dg := vl_V3 * tmp;

                if abs(den) < epsilon then
                    vl_gradient3_dg := 0.0
                else
                    vl_gradient3_dg := vl_gradient3_dg / (den);

                tmp := vl_Qmax_phase_dg * Gij;
                vl_gradient3_dg := vl_gradient3_dg * tmp;
                vl_gradient3_dg := vl_gradient3_dg / (vl_V_ref3_dg * vl_V_ref3_dg);//PU value??

            end;
        end;
end;

function TFMonitorObj.Calc_GP_AlphaP(phase_num: Integer): Double;  // NodeNuminClstr: node number in cluster
var
    PGtemp, ptemp: Double;
begin
    Result := 0.0;
    ptemp := get_power_trans();  // get power on trans point
    if eg_defed then
        PGtemp := -(p_trans_ref - ptemp) / (kVA_fm * 1000) * k_dltP // D_fm is damping plus droop
    else
        PGtemp := -(p_trans_ref - ptemp) / 1000 * k_dltP; // kVA_fm = 1 kVA

    case phase_num of //pos seq
        0, 1, 2, 3:
        begin
            result := PGtemp;
        end;
    end;
end;

function TFMonitorObj.Calc_AlphaP(NodeNuminClstr, phase_num: Integer): Double;  // NodeNuminClstr: node number in cluster
var
    nn,
    j: Integer;
    den_dij, TempAlpha: Double;
begin
    Result := 0.0;
    // alphaP = avg (alphaP) + Beta * Gp
    nn := NodeNuminClstr;
    case phase_num of //pos seq
        0:
        begin
              //1.calculate d_ij*alpha_j summation
            den_dij := 0;
            TempAlpha := 0;
            for j := 0 to Nodes - 1 do
            begin
                if (nodeFMs[j].vl_ndphases_dg = 3) then   //only 3 phase nodes
                begin
                    den_dij += pCommMatrix[nn, j];
                    TempAlpha := TempAlpha + pCommMatrix[nn, j] * nodeFMs[j].vl_AlphaP_dg;
                end;
            end;
            if den_dij = 0 then
                TempAlpha := 0.0
            else
            begin
                TempAlpha := TempAlpha / den_dij;
            end;
            nodeFMs[nn].vl_gradient_dg := self.Calc_GP_AlphaP(phase_num);
            nodeFMs[nn].vl_alphaP_dg := TempAlpha + nodeFMs[nn].vl_kcd_dg * nodeFMs[nn].vl_gradient_dg / ActiveCircuit.Solution.Iteration;

            //disturbance
            nodeFMs[nn].vl_alphaP_dg := nodeFMs[nn].vl_alphaP_dg;

            if nodeFMs[nn].vl_alphaP_dg > 1 then
                nodeFMs[nn].vl_alphaP_dg := 1;
            if nodeFMs[nn].vl_alphaP_dg < 0 then
                nodeFMs[nn].vl_alphaP_dg := 0;
            result := nodeFMs[nn].vl_alphaP_dg;
        end;
    end;
    with nodeFMs[nn] do
    begin
        case phase_num of //pos seq
            1:
            begin
              //1.calculate d_ij*alpha_j summation
                den_dij := 0;
                TempAlpha := 0;
                for j := 0 to Nodes - 1 do
                begin
                    if (nodeFMs[j].vl_ndphases_dg = 3)     //only count dgs with 3 phases or 1 phase that is same number
                        or (nodeFMs[j].vl_phase_num_dg = phase_num) then
                    begin
                        //if nodeFMs[j].vl_nodeType = 1 then // only DG nodes
                        //begin
                        den_dij += pCommMatrix[nn, j];
                        TempAlpha := TempAlpha + pCommMatrix[nn, j] * nodeFMs[j].vl_AlphaP1_dg;
                        //end;
                    end;
                end;
                if den_dij = 0 then
                    TempAlpha := 0.0
                else
                begin
                    TempAlpha := TempAlpha / den_dij;
                end;
                vl_gradientP1_dg := Calc_GP_AlphaP(phase_num);
                vl_alphaP1_dg := TempAlpha + vl_kcd_dg * vl_gradientP1_dg / ActiveCircuit.Solution.Iteration;
                if vl_alphaP1_dg > 1 then
                    vl_alphaP1_dg := 1;
                if vl_alphaP1_dg < -1 then
                    vl_alphaP1_dg := -1;
                result := vl_alphaP1_dg;
            end;
            2:
            begin
                 //1.calculate d_ij*alpha_j summation
                den_dij := 0;
                TempAlpha := 0;
                for j := 0 to Nodes - 1 do
                begin
                    if (nodeFMs[j].vl_ndphases_dg = 3)     //only count dgs with 3 phases or 1 phase that is same number
                        or (nodeFMs[j].vl_phase_num_dg = phase_num) then
                    begin
                        //if nodeFMs[j].vl_nodeType = 1 then // only DG nodes
                        //begin
                        den_dij += pCommMatrix[nn, j];
                        TempAlpha := TempAlpha + pCommMatrix[nn, j] * nodeFMs[j].vl_AlphaP2_dg;
                        //end;
                    end;
                end;
                if den_dij = 0 then
                    TempAlpha := 0.0
                else
                begin
                    TempAlpha := TempAlpha / den_dij;
                end;
                vl_gradientP2_dg := Calc_GP_AlphaP(phase_num);
                vl_alphaP2_dg := TempAlpha + vl_kcd_dg * vl_gradientP2_dg / ActiveCircuit.Solution.Iteration;
                if vl_alphaP2_dg > 1 then
                    vl_alphaP2_dg := 1;
                if vl_alphaP2_dg < -1 then
                    vl_alphaP2_dg := -1;
                result := vl_alphaP2_dg;
            end;
            3:
            begin
                  //1.calculate d_ij*alpha_j summation
                den_dij := 0;
                TempAlpha := 0;
                for j := 0 to Nodes - 1 do
                begin
                    if (nodeFMs[j].vl_ndphases_dg = 3)     //only count dgs with 3 phases or 1 phase that is same number
                        or (nodeFMs[j].vl_phase_num_dg = phase_num) then
                    begin
                        den_dij += pCommMatrix[nn, j];
                        TempAlpha := TempAlpha + pCommMatrix[nn, j] * nodeFMs[j].vl_AlphaP3_dg;
                    end;
                end;
                if den_dij = 0 then
                    TempAlpha := 0.0
                else
                begin
                    TempAlpha := TempAlpha / den_dij;
                end;
                vl_gradientP3_dg := Calc_GP_AlphaP(phase_num);
                vl_alphaP3_dg := TempAlpha + vl_kcd_dg * vl_gradientP3_dg / ActiveCircuit.Solution.Iteration;
                if vl_alphaP3_dg > 1 then
                    vl_alphaP3_dg := 1;
                if vl_alphaP3_dg < -1 then
                    vl_alphaP3_dg := -1;
                result := vl_alphaP3_dg;
            end;
        end;
    end;
end;

//only work for Generic5 nodefm
function TFMonitorObj.Calc_Alpha_M2(NodeNumofDG, phase_num: Integer; dbNodeRef: Integer; Bii, beta, Volt_Trhd: Double): Double;
//NodeNumofDG = NodeNuminClstr
var
    j, it: Integer;
    den: Double;
    alpha: Double;
    den_dij, TempAlpha: Double;
    // tmp1, tmp2, tmp3, tmp4, tmp5, tmp6, tmp7: Double;
begin
    Result := 0.0;
    update_all_nodes_info();     // update voltages on all buses
    with nodeFMs[NodeNumofDG] do
    begin
        case phase_num of //pos seq
            0:
            begin
              //1.calculate d_ij*alpha_j summation
                den_dij := 0;
                TempAlpha := 0;

                for j := 0 to Nodes - 1 do
                begin
                    if (nodeFMs[j].vl_ndphases_dg = 3) then   //only 3 phase nodes
                    begin
                        //if nodeFMs[j].vl_nodeType = 1 then // only DG nodes
                        //begin
                        den_dij += pCommMatrix[NodeNumofDG, j];
                        TempAlpha := TempAlpha + pCommMatrix[NodeNumofDG, j] * nodeFMs[j].vl_Alpha_dg;
                        //end;
                    end;

                end;
                if den_dij = 0 then
                    TempAlpha := 0.0
                else
                begin
                    TempAlpha := TempAlpha / den_dij;

                    //2.calculate gradient----------------
                    den := vl_Q_DG - vl_Q_Di - vl_V * vl_V * Bii;   // pos ctrl: Bii use the first one
                    if abs(den) < epsilon then
                        den := epsilon;
                    vl_gradient_dg := (vl_V_ref_dg - vl_v) * vl_V / (den) / (vl_V_ref_dg * vl_V_ref_dg);  //*vl_Qmax, 0311-by dahei
                    it := ActiveCircuit.Solution.Iteration;
                    vl_gradient_dg := (beta * vl_V_ref_dg * vl_V_ref_dg * abs(Bii) * 100 / it) * vl_gradient_dg;
                    if abs(vl_V_ref_dg - vl_v) <= Volt_Trhd * vl_V_ref_dg then
                        vl_gradient_dg := 0.0;
                    //calculate final alpha----------------
                end;
                alpha := vl_kc_ul_dg * TempAlpha + vl_gradient_dg;
                if alpha > 1 then
                    alpha := 1;
                if alpha < -1 then
                    alpha := -1;
                vl_Alpha_dg := alpha;
                result := alpha;
            end;
            1:
            begin
                //1.calculate d_ij*alpha_j summation
                den_dij := 0;
                TempAlpha := 0;
                for j := 0 to Nodes - 1 do
                begin
                    if (nodeFMs[j].vl_ndphases_dg = 3)     //only count dgs with 3 phases or 1 phase that is same number
                        or (nodeFMs[j].vl_nodeType_phase[1] = 1) then //this phase has DG
                    begin
                        den_dij += pCommMatrix[NodeNumofDG, j];
                        TempAlpha := TempAlpha + pCommMatrix[NodeNumofDG, j] * nodeFMs[j].vl_Alpha1_dg;
                    end;

                end;
                if den_dij = 0 then
                    TempAlpha := 0.0
                else
                begin
                    TempAlpha := TempAlpha / den_dij;
                    //2.calculate gradient----------------
                    den := vl_Q_DG1 - vl_Q_Di1 - vl_V1 * vl_V1 * Bii;   // pos ctrl: Bii use the first one
                    if abs(den) < epsilon then
                        den := epsilon;

                    // tmp1 := vl_Q_DG1;
                    // tmp2 := vl_Q_Di1;
                    // tmp3 := vl_V1;
                    // tmp4 := vl_Qmax_dg;
                    // tmp5 := vl_V_ref1_dg;
                    j := ActiveCircuit.Solution.Iteration;
                    vl_gradient1_dg := (vl_V_ref1_dg - vl_v1) * vl_V1 / (den) / (vl_V_ref1_dg * vl_V_ref1_dg);   //*vl_Qmax, 0311-by dahei
                    vl_gradient1_dg := (beta * vl_V_ref1_dg * vl_V_ref1_dg * abs(Bii) * 100 / j) * vl_gradient1_dg;
                    //vl_gradient1 := (beta)*vl_gradient1;
                    if abs(vl_V_ref1_dg - vl_v1) <= Volt_Trhd * vl_V_ref1_dg then
                        vl_gradient1_dg := 0.0;
                    //calculate final alpha----------------
                end;
                vl_alpha1_dg := TempAlpha + vl_gradient1_dg;
                // tmp6 := vl_gradient1_dg;
                // tmp7 := vl_alpha1_dg;
                if vl_alpha1_dg > 1 then
                    vl_alpha1_dg := 1;
                if vl_alpha1_dg < -1 then
                    vl_alpha1_dg := -1;
                result := vl_alpha1_dg;
            end;
            2:
            begin
                 //1.calculate d_ij*alpha_j summation
                den_dij := 0;
                TempAlpha := 0;
                for j := 0 to Nodes - 1 do
                begin
                    if (nodeFMs[j].vl_ndphases_dg = 3)     //only count dgs with 3 phases or 1 phase that is same number
                        or (nodeFMs[j].vl_nodeType_phase[2] = 1) then //this phase has DG
                    begin
                        den_dij += pCommMatrix[NodeNumofDG, j];
                        TempAlpha := TempAlpha + pCommMatrix[NodeNumofDG, j] * nodeFMs[j].vl_Alpha2_dg;
                    end;
                end;
                if den_dij = 0 then
                    TempAlpha := 0.0
                else
                begin
                    TempAlpha := TempAlpha / den_dij;

                    //2.calculate gradient----------------
                    den := vl_Q_DG2 - vl_Q_Di2 - vl_V2 * vl_V2 * Bii;   // pos ctrl: Bii use the first one
                    if abs(den) < epsilon then
                        den := epsilon;
                    it := ActiveCircuit.Solution.Iteration;
                    vl_gradient2_dg := (vl_V_ref2_dg - vl_v2) * vl_V2 / (den) / (vl_V_ref2_dg * vl_V_ref2_dg);        //*vl_Qmax
                    vl_gradient2_dg := (beta * vl_V_ref2_dg * vl_V_ref2_dg * abs(Bii) * 100 / it) * vl_gradient2_dg;
                    if abs(vl_V_ref2_dg - vl_v2) <= Volt_Trhd * vl_V_ref2_dg then
                        vl_gradient2_dg := 0.0;
                    //calculate final alpha----------------
                end;
                vl_alpha2_dg := TempAlpha + vl_gradient2_dg;
                if vl_alpha2_dg > 1 then
                    vl_alpha2_dg := 1;
                if vl_alpha2_dg < -1 then
                    vl_alpha2_dg := -1;
                result := vl_alpha2_dg;
            end;
            3:
            begin
                  //1.calculate d_ij*alpha_j summation
                den_dij := 0;
                TempAlpha := 0;
                for j := 0 to Nodes - 1 do
                begin
                    if (nodeFMs[j].vl_ndphases_dg = 3)     //only count dgs with 3 phases or 1 phase that is same number
                        or (nodeFMs[j].vl_nodeType_phase[3] = 1) then //this phase has DG
                    begin
                        den_dij += pCommMatrix[NodeNumofDG, j];
                        TempAlpha := TempAlpha + pCommMatrix[NodeNumofDG, j] * nodeFMs[j].vl_Alpha3_dg;
                    end;
                end;
                if den_dij = 0 then
                    TempAlpha := 0.0
                else
                begin
                    TempAlpha := TempAlpha / den_dij;
                    //2.calculate gradient----------------
                    den := vl_Q_DG3 - vl_Q_Di3 - vl_V3 * vl_V3 * Bii;   // pos ctrl: Bii use the first one
                    if abs(den) < epsilon then
                        den := epsilon;
                    it := ActiveCircuit.Solution.Iteration;
                    vl_gradient3_dg := (vl_V_ref3_dg - vl_v3) * vl_V3 / (den) / (vl_V_ref3_dg * vl_V_ref3_dg);        //*vl_Qmax
                    vl_gradient3_dg := (beta * vl_V_ref3_dg * vl_V_ref3_dg * abs(Bii) * 100 / it) * vl_gradient3_dg;
                    if abs(vl_V_ref3_dg - vl_v3) <= Volt_Trhd * vl_V_ref3_dg then
                        vl_gradient3_dg := 0.0;
                      //calculate final alpha----------------
                end;
                vl_alpha3_dg := TempAlpha + vl_gradient3_dg;
                if vl_alpha3_dg > 1 then
                    vl_alpha3_dg := 1;
                if vl_alpha3_dg < -1 then
                    vl_alpha3_dg := -1;
                result := vl_alpha3_dg;
            end;
        end;
    end;
end;

//will be call in Generic5
//calculate subgradient for DG 'NodeNumofDG' phase 'phase_num'
function TFMonitorObj.Calc_Alpha_LnM2(NodeNumofDG, phase_num: Integer; dbNodeRef: Integer; Bii, beta, Volt_Trhd: Double): Double;
var
    Lambda, tmp, tmp1: Double;
begin
    // Lambda0 := 0.1;
    Lambda := 1.0;
    tmp := 0.0;
    tmp1 := Calc_Alpha_M2(NodeNumofDG, phase_num, dbNodeRef, Bii, beta, Volt_Trhd);
    result := (1 - Lambda) * tmp + Lambda * tmp1; // TODO: BUG: I suspect one of these is supposed to be Lambda0, but tmp=0, so...
end;

function TFMonitorObj.Calc_Alpha_L_vivj(NodeNumofDG, phase_num: Integer; dbNodeRef: Integer; Bii, beta, Volt_Trhd: Double): Double;
var
    j, it: Integer;
    den_dij, TempAlpha: Double;
    dynBeta: Double;
begin
    Result := 0.0;
    dynBeta := 0.0;
    update_all_nodes_info(); // update voltages on all buses
    Get_PQ_DI(nodeFMs[NodeNumofDG]); // load measure
    // calclate alpha
    case phase_num of
        1:
        begin //phase A
            //1.calculate d_ij*alpha_j summation
            den_dij := 0;
            TempAlpha := 0;
            for j := 0 to Nodes - 1 do
            begin
                if (nodeFMs[j].vl_ndphases_dg = 3)     //only count dgs with 3 phases or 1 phase that is same number
                    or (nodeFMs[j].vl_nodeType_phase[1] = 1) then //this phase has DG
                begin
                    den_dij += pCommMatrix[NodeNumofDG, j];
                    TempAlpha := TempAlpha + pCommMatrix[NodeNumofDG, j] * nodeFMs[j].vl_Alpha1_dg;
                end;

            end;
            if den_dij = 0 then
                TempAlpha := 0.0
            else
            begin
                TempAlpha := TempAlpha / den_dij;
                //2.calculate gradient----------------
            end;
            with nodeFMs[NodeNumofDG] do
            begin
                it := ActiveCircuit.Solution.Iteration;
                // it := Nodes; // TODO: replace with the line above (changed SVN R3340)
                Calc_Grdt_for_Alpha_vivj(NodeNumofDG, phase_num);//  vl_gradient1_dg updated inside
                if vl_Qmax_phase_dg <> 0 then
                    dynBeta := (beta * abs(Bii) * 100 / it) * vl_V_ref1_dg * vl_V_ref1_dg / vl_Qmax_phase_dg;
                vl_alpha1_dg := TempAlpha + dynBeta * vl_gradient1_dg;
                if vl_alpha1_dg > 1 then
                    vl_alpha1_dg := 1;
                if vl_alpha1_dg < -1 then
                    vl_alpha1_dg := -1;

                result := vl_alpha1_dg;
            end;
        end;
        2:
        begin //phase B
            //1.calculate d_ij*alpha_j summation
            den_dij := 0;
            TempAlpha := 0;
            for j := 0 to Nodes - 1 do
            begin
                if (nodeFMs[j].vl_ndphases_dg = 3)     //only count dgs with 3 phases or 1 phase that is same number
                    or (nodeFMs[j].vl_nodeType_phase[2] = 1) then //this phase has DG
                begin
                    den_dij += pCommMatrix[NodeNumofDG, j];
                    TempAlpha := TempAlpha + pCommMatrix[NodeNumofDG, j] * nodeFMs[j].vl_Alpha2_dg;
                end;

            end;
            if den_dij = 0 then
            begin
                TempAlpha := 0.0
            end
            else
            begin
                TempAlpha := TempAlpha / den_dij;
            end;
            with nodeFMs[NodeNumofDG] do
            begin
                it := ActiveCircuit.Solution.Iteration;
                // it := Nodes; // TODO: replace with the line above (changed SVN R3340)
                Calc_Grdt_for_Alpha_vivj(NodeNumofDG, phase_num);
                if vl_Qmax_phase_dg <> 0 then
                    dynBeta := (beta * abs(Bii) * 100 / it) / vl_Qmax_phase_dg * vl_V_ref2_dg * vl_V_ref2_dg; //
                vl_alpha2_dg := TempAlpha + dynBeta * vl_gradient2_dg;
                if vl_alpha2_dg > 1 then
                    vl_alpha2_dg := 1;
                if vl_alpha2_dg < -1 then
                    vl_alpha2_dg := -1;
                
                result := vl_alpha2_dg;
            end;
        end;
        3:
        begin //phase C
            //1.calculate d_ij*alpha_j summation
            den_dij := 0;
            TempAlpha := 0;
            for j := 0 to Nodes - 1 do
            begin
                if (nodeFMs[j].vl_ndphases_dg = 3)     //only count dgs with 3 phases or 1 phase that is same number
                    or (nodeFMs[j].vl_nodeType_phase[3] = 1) then //this phase has DG
                begin
                    den_dij += pCommMatrix[NodeNumofDG, j];
                    TempAlpha := TempAlpha + pCommMatrix[NodeNumofDG, j] * nodeFMs[j].vl_Alpha3_dg;
                end;

            end;
            if den_dij = 0 then
            begin
                TempAlpha := 0.0
            end
            else
            begin
                TempAlpha := TempAlpha / den_dij;
            end;
            with nodeFMs[NodeNumofDG] do
            begin
                it := ActiveCircuit.Solution.Iteration;
                // it := Nodes; // TODO: replace with the line above (changed SVN R3340)
                Calc_Grdt_for_Alpha_vivj(NodeNumofDG, phase_num);
                if vl_Qmax_phase_dg <> 0 then
                    dynBeta := (beta * abs(Bii) * 100 / it) / vl_Qmax_phase_dg * vl_V_ref3_dg * vl_V_ref3_dg; //
                vl_alpha3_dg := TempAlpha + dynBeta * vl_gradient3_dg;
                if vl_alpha3_dg > 1 then
                    vl_alpha3_dg := 1;
                if vl_alpha3_dg < -1 then
                    vl_alpha3_dg := -1;

                result := vl_alpha3_dg;
            end;
        end;
        0:
        begin //pos seq value
        end;
    else
    end;
end;

function TFMonitorObj.Calc_Alpha_L(NodeNumofDG, phase_num: Integer; dbNodeRef: Integer; Bii, beta, Volt_Trhd: Double): Double;
var
    j, it: Integer;
    den_dij, TempAlpha: Double;
    // tmp: Double;
    dynBeta: Double;
begin
    Result := 0.0;
    dynBeta := 0.0;
    update_all_nodes_info();     // update voltages on all buses
    Get_PQ_DI(nodeFMs[NodeNumofDG]); // load measure
      // calclate alpha
    case phase_num of
        1:
        begin //phase A
              //1.calculate d_ij*alpha_j summation
            den_dij := 0;
            TempAlpha := 0;
            for j := 0 to Nodes - 1 do
            begin
                if (nodeFMs[j].vl_ndphases_dg = 3)     //only count dgs with 3 phases or 1 phase that is same number
                    or (nodeFMs[j].vl_nodeType_phase[1] = 1) then //this phase has DG
                begin
                    den_dij += pCommMatrix[NodeNumofDG, j];
                    TempAlpha := TempAlpha + pCommMatrix[NodeNumofDG, j] * nodeFMs[j].vl_Alpha1_dg;
                end;

            end;
            if den_dij = 0 then
                TempAlpha := 0.0
            else
            begin
                TempAlpha := TempAlpha / den_dij;
            end;
            with nodeFMs[NodeNumofDG] do
            begin
                // tmp := Calc_Grdt_for_Alpha(NodeNumofDG, phase_num);//  vl_gradient1_dg updated inside
                it := ActiveCircuit.Solution.Iteration;
                // it := Nodes; // TODO: replace with the line above (changed SVN R3340)
                if vl_Qmax_phase_dg <> 0 then
                    dynBeta := (beta * abs(Bii) * 100 / it) * vl_V_ref1_dg * vl_V_ref1_dg / vl_Qmax_phase_dg;
                vl_alpha1_dg := TempAlpha + dynBeta * vl_gradient1_dg;
                if vl_alpha1_dg > 1 then
                    vl_alpha1_dg := 1;
                if vl_alpha1_dg < -1 then
                    vl_alpha1_dg := -1;
    
                result := vl_alpha1_dg;
            end;
        end;
        2:
        begin //phase B
              //1.calculate d_ij*alpha_j summation
            den_dij := 0;
            TempAlpha := 0;
            for j := 0 to Nodes - 1 do
            begin
                if (nodeFMs[j].vl_ndphases_dg = 3)     //only count dgs with 3 phases or 1 phase that is same number
                    or (nodeFMs[j].vl_nodeType_phase[2] = 1) then //this phase has DG
                begin
                    den_dij += pCommMatrix[NodeNumofDG, j];
                    TempAlpha := TempAlpha + pCommMatrix[NodeNumofDG, j] * nodeFMs[j].vl_Alpha2_dg;
                end;

            end;
            if den_dij = 0 then
                TempAlpha := 0.0
            else
            begin
                TempAlpha := TempAlpha / den_dij;
            end;
            with nodeFMs[NodeNumofDG] do
            begin
                it := ActiveCircuit.Solution.Iteration;
                // it := Nodes; // TODO: replace with the line above (changed SVN R3340)
                if vl_Qmax_phase_dg <> 0 then
                    dynBeta := (beta * abs(Bii) * 100 / it) / vl_Qmax_phase_dg * vl_V_ref2_dg * vl_V_ref2_dg; //
                vl_alpha2_dg := TempAlpha + dynBeta * vl_gradient2_dg;
                if vl_alpha2_dg > 1 then
                    vl_alpha2_dg := 1;
                if vl_alpha2_dg < -1 then
                    vl_alpha2_dg := -1;
            end;
            result := nodeFMs[NodeNumofDG].vl_alpha2_dg;
        end;
        3:
        begin //phase C
            //1.calculate d_ij*alpha_j summation
            den_dij := 0;
            TempAlpha := 0;
            for j := 0 to Nodes - 1 do
            begin
                if (nodeFMs[j].vl_ndphases_dg = 3)     //only count dgs with 3 phases or 1 phase that is same number
                    or (nodeFMs[j].vl_nodeType_phase[3] = 1) then //this phase has DG
                begin
                    den_dij += pCommMatrix[NodeNumofDG, j];
                    TempAlpha := TempAlpha + pCommMatrix[NodeNumofDG, j] * nodeFMs[j].vl_Alpha3_dg;
                end;
            end;
            if den_dij = 0 then
                TempAlpha := 0.0
            else
            begin
                TempAlpha := TempAlpha / den_dij;
            end;
            with nodeFMs[NodeNumofDG] do
            begin
                it := ActiveCircuit.Solution.Iteration;
                // it := Nodes; // TODO: replace with the line above (changed SVN R3340)
                if vl_Qmax_phase_dg <> 0 then
                    dynBeta := (beta * abs(Bii) * 100 / it) / vl_Qmax_phase_dg * vl_V_ref3_dg * vl_V_ref3_dg; //
                vl_alpha3_dg := TempAlpha + dynBeta * vl_gradient3_dg;
                if vl_alpha3_dg > 1 then
                    vl_alpha3_dg := 1;
                if vl_alpha3_dg < -1 then
                    vl_alpha3_dg := -1;
            end;
            result := nodeFMs[NodeNumofDG].vl_alpha3_dg;
        end;
        0:
        begin //pos seq value
        end;
    else
    end;
end;

// function TFMonitorObj.AvgPmax(): Double;
// var
//     i, k: Integer;
// begin
//     result := 0.0;
//     k := 1;
//     for i := 0 to Nodes - 1 do
//     begin
//         with nodeFMs[i] do
//             if ((vl_PF_flag_dg = 1) and (vl_cc_switch_dg = true)) then //
//             begin
//                 result := result + vl_Pmax_dg;
//                 result := result / k;
//                 k := k + 1;
//             end;
//     end;
// end;

// function TFMonitorObj.AvgQmax(): Double;
// var
//     i, k: Integer;
// begin
//     result := 0.0;
//     k := 1;
//     for i := 0 to Nodes - 1 do
//     begin
//         with nodeFMs[i] do
//             if ((vl_QV_flag_dg = 1) and (vl_cc_switch_dg = true)) then //volt/var control is on
//             begin
//                 result := result + vl_Qmax_dg;
//                 result := result / k;
//                 k := k + 1;
//             end;
//     end;
// end;

function TFMonitorObj.Calc_fm_ul_0(NodeNumofDG, phase_num: Integer; dbNodeRef: Integer; Bii, beta, Volt_Trhd: Double): Double;
var
    dly,
    j: Integer;
    den_dij, TempAlpha,
    tmp,
    dfs_hide: Double;
begin
    Result := 0.0;
    case phase_num of //pos seq
        0:
        begin
          //1.calculate d_ij*alpha_j summation
            den_dij := 0;
            TempAlpha := 0.0;
            //no delay
            if T_intvl_smpl = 0.0 then
            begin
                // communication
                for j := 0 to Nodes - 1 do
                begin
                    if (nodeFMs[j].vl_ndphases_dg = 3)   //only 3 phase nodes
                        and ((nodeFMs[j].vl_nodeType_phase[1] + nodeFMs[j].vl_nodeType_phase[2] + nodeFMs[j].vl_nodeType_phase[3]) = 3) then //this phase has DG
                    begin
                        den_dij += pCommMatrix[NodeNumofDG, j];

                        if j <> atk_node_num then // regular nodes
                        begin
                             //Sumation of all alpha s
                            TempAlpha := TempAlpha + pCommMatrix[NodeNumofDG, j] * nodeFMs[j].vl_Alpha_dgn;
                        end
                        else
                         // attack and defense -------------------------------------
                        begin   // node under attack
                             //Sumation of all alpha s
                            if (atk = true) and (ActiveCircuit.Solution.DynaVars.t >= atk_time) then
                                TempAlpha := TempAlpha + D_p * pCommMatrix[NodeNumofDG, j] * nodeFMs[j].vl_Alpha_dgn
                            else  // attack starts from here
                                TempAlpha := TempAlpha + pCommMatrix[NodeNumofDG, j] * nodeFMs[j].vl_Alpha_dgn

                        end; // --attack and defense ends---------------------------------
                         // attack and defense
                        if (atk = true) and (ActiveCircuit.Solution.DynaVars.t >= atk_time) then
                            //and (ActiveCircuit.Solution.DynaVars.IterationFlag = 1)
                        begin
                              // if being attacked
                            TempAlpha := TempAlpha + nodeFMs[j].d_atk;      // attack is added on
                        end;
                         // --attack and defense ends---------------------------------
                    end;
                end;

            end
            //with delay
            else
            begin
                for j := 0 to NodeNumofDG - 2 do
                begin
                    if (nodeFMs[j].vl_ndphases_dg = 3)   //only 3 phase nodes
                        and ((nodeFMs[j].vl_nodeType_phase[1] + nodeFMs[j].vl_nodeType_phase[2] + nodeFMs[j].vl_nodeType_phase[3]) = 3) then //this phase has DG
                    begin
                        den_dij += pCommMatrix[NodeNumofDG, j];
                         //how many steps of delay from node j to node 'NodeNumofDG'
                        dly := pcommDelaysteps[NodeNumofDG, j];
                        if dly = 0 then
                        begin
                            TempAlpha := TempAlpha + pCommMatrix[NodeNumofDG, j] * nodeFMs[j].vl_Alpha_dg;
                        end
                        else
                        begin
                            TempAlpha := TempAlpha + pCommMatrix[NodeNumofDG, j] * nodeFMs[j].vl_smpl_dg[1, dly];
                        end;
                    end;

                end;

                j := NodeNumofDG;
                if (nodeFMs[j].vl_ndphases_dg = 3)   //only 3 phase nodes
                    and ((nodeFMs[j].vl_nodeType_phase[1] + nodeFMs[j].vl_nodeType_phase[2] + nodeFMs[j].vl_nodeType_phase[3]) = 3) then //this phase has DG
                begin
                    den_dij += pCommMatrix[NodeNumofDG, j];
                    TempAlpha := TempAlpha + pCommMatrix[NodeNumofDG, j] * nodeFMs[j].vl_Alpha_dg;
                end;

                for j := NodeNumofDG to Nodes - 1 do
                begin
                    if (nodeFMs[j].vl_ndphases_dg = 3)   //only 3 phase nodes
                        and ((nodeFMs[j].vl_nodeType_phase[1] + nodeFMs[j].vl_nodeType_phase[2] + nodeFMs[j].vl_nodeType_phase[3]) = 3) then //this phase has DG
                    begin
                        den_dij += pCommMatrix[NodeNumofDG, j];
                        dly := pcommDelaysteps[NodeNumofDG, j];
                        if dly = 0 then
                        begin
                            TempAlpha := TempAlpha + pCommMatrix[NodeNumofDG, j] * nodeFMs[j].vl_Alpha_dg;
                        end
                        else
                        begin
                            TempAlpha := TempAlpha + pCommMatrix[NodeNumofDG, j] * nodeFMs[j].vl_smpl_dg[1, dly];
                        end;
                    end;

                end;
            end;

            // from sumation to ul
            if den_dij = 0 then
                TempAlpha := 0.0
            else
            begin
                TempAlpha := TempAlpha / den_dij;
            end;
            // if this node is the node under attack, change the sign of that
            if (NodeNumofDG = atk_node_num) and (atk) and (ActiveCircuit.Solution.DynaVars.t >= atk_time) then
            begin
                tmp := (TempAlpha - d_p * nodeFMs[NodeNumofDG].vl_Alpha_dgn);
            end
            else
            begin
                tmp := (TempAlpha - nodeFMs[NodeNumofDG].vl_Alpha_dgn);
            end;

                 // attack and defense
            if (atk) and (ActiveCircuit.Solution.DynaVars.t >= atk_time) then
            begin
                dfs_hide := organise_dfs_node(NodeNumofDG);  // x_i'  =  A_i x + {{ \beta K_i z }}+ \beta B_i x_0 + d_i
                     //TempAlpha := TempAlpha + beta_dfs * tmp;  // defense is added on
                     // --attack and defense ends---------------------------------
                tmp := tmp + beta_dfs * dfs_hide;
            end;
            //Tolerance of alpha_i alpha_j
            if abs(tmp) <= Volt_Trhd * 0.01 then
            begin
                 //Result := 0.0
            end
            else
                Result := tmp * nodeFMs[NodeNumofDG].vl_kc_ul_dg;
            // if there is attck
        end;
    end;
end;

function TFMonitorObj.Calc_fm_us_0(NodeNumofDG, phase_num: Integer; dbNodeRef: Integer; Bii, beta, Volt_Trhd: Double): Double;
var
    nn, i: Integer;
    den: Double;
    // tmp: Double;
    v, vref: Double;
    den_dij, tempUl: Double;
    phi: Double;
begin
    Result := 0.0;
    phi := 0.0;
    //update voltage
    nn := NodeNumofDG;
    Get_PDElem_terminal_voltage(NodeNumofDG, nodeFMs[nn].vl_strMeasuredName, nodeFMs[nn].vl_terminalNum);
    
    //calc gradient
    v := nodeFMs[nn].vl_V;
    vref := nodeFMs[nn].vl_V_ref_dg;

    case phase_num of //pos seq
        0:
        begin
            den := abs(nodeFMs[nn].vl_Q_DG - nodeFMs[nn].vl_Q_Di - v * v * Bii);   // pos ctrl: Bii use the first one
            if abs(den) < epsilon then
                den := epsilon;
            nodeFMs[nn].vl_gradient_dg := (beta * vref * vref) * abs(Bii) * 100 * ((Vref - v) * v / den / (vref * vref));

            //(beta* abs(Bii)*100/j)/vl_Qmax_phase_dg*

            if abs(vref - v) <= Volt_Trhd * vref then
                nodeFMs[nn].vl_gradient_dg := 0.0;
            // tmp := abs(vref - v);
            if nodeFMs[nn].vl_gradient_dg > 1 then
                nodeFMs[nn].vl_gradient_dg := 1;
            if nodeFMs[nn].vl_gradient_dg < -1 then
                nodeFMs[nn].vl_gradient_dg := -1;
            result := nodeFMs[nn].vl_gradient_dg;

            if NodeNumofDG <> atk_node_num then
                Exit;

            if ActiveCircuit.Solution.Dynavars.SolutionMode <> TSolveMode.DYNAMICMODE then
                Exit;

            // the following only works for the node under attack
            // in dynamic simulation
            
            // attack and defense has been set
            if not (atk and dfs) then
                Exit;

            if ActiveCircuit.Solution.Dynavars.t <= atk_time then
                Exit;
            //if current time is over attack time

            
            // if the attack is of the second type, then phi =0
            // if the attack is of the first type, then phi =0
            if (nodeFMs[atk_node_num].d_atk0 <> 0) and (BETA_DFS <> 0) then
            begin
                //Set a coeffient for beta_dfs
                den_dij := 0;
                tempUl := 0.0;
                for i := 0 to Nodes - 1 do
                begin
                    if (nodeFMs[i].vl_ndphases_dg = 3)   //only 3 phase nodes
                        and ((nodeFMs[i].vl_nodeType_phase[1] + nodeFMs[i].vl_nodeType_phase[2] + nodeFMs[i].vl_nodeType_phase[3]) = 3) then //this phase has DG
                    begin
                        //Sumation of all Z and alpha s
                        den_dij += pCommMatrix[nn, i];
                        tempUl := tempUl + pCommMatrix[nn, i] * nodeFMs[i].vl_Alpha_dgn;
                    end;
                end;
                // average
                if den_dij = 0 then
                begin
                    tempUl := 0.0;
                end
                else
                begin
                    tempUl := tempUl / den_dij;
                end;

                tempUl := (tempUl - nodeFMs[nn].vl_Alpha_dgn);
                // calculate phi
                phi := Coef_Phi(abs(tempUl));
            end;
            result := d_p * (1 + phi * beta_dfs) * nodeFMs[nn].vl_gradient_dg;
        end;
    end;
end;

function TFMonitorObj.Coef_Phi(x: Double): Double;
var
    x1, x2, x3,
    y1, y2, y3: Double;
    y0: Double;
    overall: Double;
begin
    overall := d_beta;
    x1 := 0.005;
    x2 := 0.01;
    x3 := 0.05;

    y1 := 1.0;
    y2 := 0.5;
    y3 := 0.0;

    y0 := 1.0;
    if x <= x1 then
        result := x * (y1 - y0) / x1 + y0
    else
    if x <= x2 then
        result := y1 + (x - x1) * (y2 - y1) / (x2 - x1)
    else
    if x <= x3 then
        result := y2 + (x - x2) * (y3 - y2) / (x3 - x2)
    else
        result := y1;

    result := overall * result;
end;

function TFMonitorObj.GetGradient(j, phase_num: Integer; Bii, Volt_Trhd: Double): Double;
var
    v, vref, den: Double;
    tmp: Double;
begin
    Result := 0.0;
    v := nodeFMs[j].vl_V;
    vref := nodeFMs[j].vl_V_ref_dg;

    case phase_num of //pos seq
        0:
        begin
            den := abs(nodeFMs[j].vl_Q_DG - nodeFMs[j].vl_Q_Di - v * V * Bii);   // pos ctrl: Bii use the first one
            if abs(den) < epsilon then
                den := epsilon;
            tmp := (Vref - v) * V / (den) / (vref * vref);
            if tmp > 1 then
                tmp := 1;
            if tmp < -1 then
                tmp := -1;
            result := tmp;
        end;
    end;
end;

procedure TFMonitorObj.Init_delay_array(NodeNumofDG: Integer);
var
    nn, i: Integer;
begin
    //measure all voltages
    for i := 0 to Nodes - 1 do
        Get_PDElem_terminal_voltage(i, nodeFMs[i].vl_strMeasuredName, nodeFMs[i].vl_terminalNum);
    
    // inti delay array
    nn := NodeNumofDG;
    for i := 0 to MaxLocalMem do
    begin
        //alpha
        nodeFMs[nn].vl_smpl_dg[1, i] := nodeFMs[nn].vl_Alpha_dg;
        //alphaP
        nodeFMs[nn].vl_smpl_dg[2, i] := nodeFMs[nn].vl_AlphaP_dg;
        // vl_v, which is the 0 seq. voltage
        nodeFMs[nn].vl_smpl_dg[3, i] := nodeFMs[nn].vl_V;
    end;
end;

function TFMonitorObj.Calc_Gradient_ct_P(NodeNuminClstr, phase_num: Integer): Double;  // NodeNuminClstr: node number in cluster
var
    Grdnt_P: Double;
    // nn: Integer;
begin
    Result := 0.0;
    Grdnt_P := 0.0;
    // nn := NodeNumofDG;
    //tempCplx
    // dvDGtemp := (nodeFMs[nn].vl_V - nodeFMs[nn].vl_V_ref_dg) / nodeFMs[nn].vl_V_ref_dg;
    // if this DG is above 1.05, then it should have P curtail gradient
    //if ( FMonClass.bCurtl ) and (dvDGtemp>0.0) then //overall system need control
    if (FMonClass.bCurtl) then
    begin
        if ld_fm_info[0].b_Curt_Ctrl = true then // if false, the curtailment will be zero for any node in this cluster
            Grdnt_P := (FMonClass.LD_FM[0].volt_lwst - 1.0)
    end
    else
        Grdnt_P := 0.0;
    case phase_num of //pos seq
        0:
        begin
            result := Grdnt_P;
        end;
        1:
        begin
            result := Grdnt_P;
        end;
        2:
        begin
            result := Grdnt_P;
        end;
        3:
        begin
            result := Grdnt_P;
        end;
    end;
end;

function TFMonitorObj.Calc_ul_P(NodeNuminClstr, phase_num: Integer): Double;
// NodeNuminClstr: node number in cluster 
var
    j: Integer;
    den_dij, TempAlphaP: Double;
    dly: Integer;
begin
    Result := 0.0;
    case phase_num of //pos seq
        0:
        begin
            //1.calculate d_ij*alpha_j summation
            den_dij := 0;
            TempAlphaP := 0;
            if T_intvl_smpl = 0.0 then
            begin
                //no delay

                for j := 0 to Nodes - 1 do
                begin
                    if (nodeFMs[j].vl_ndphases_dg = 3)   //only 3 phase nodes
                        and ((nodeFMs[j].vl_nodeType_phase[1] + nodeFMs[j].vl_nodeType_phase[2] + nodeFMs[j].vl_nodeType_phase[3]) = 3) then  // has 3-phase DG
                    begin
                        den_dij += pCommMatrix[NodeNuminClstr, j];
                        TempAlphaP += pCommMatrix[NodeNuminClstr, j] * nodeFMs[j].vl_AlphaP_dg;
                    end;
                end;
            end
            else
            begin
                // with delay
                for j := 0 to NodeNuminClstr - 2 do
                begin
                    if (nodeFMs[j].vl_ndphases_dg = 3)   //only 3 phase nodes
                        and ((nodeFMs[j].vl_nodeType_phase[1] + nodeFMs[j].vl_nodeType_phase[2] + nodeFMs[j].vl_nodeType_phase[3]) = 3) then //has 3-phase  DG
                    begin
                        den_dij += pCommMatrix[NodeNuminClstr, j];
                             //how many steps of delay from node j to node 'NodeNumofDG'
                        dly := pcommDelaysteps[NodeNuminClstr, j];
                        if dly = 0 then
                        begin
                            TempAlphaP += pCommMatrix[NodeNuminClstr, j] * nodeFMs[j].vl_AlphaP_dg;
                        end
                        else
                        begin
                            TempAlphaP += pCommMatrix[NodeNuminClstr, j] * nodeFMs[j].vl_smpl_dg[2, dly];
                        end;
                    end;

                end;

                j := NodeNuminClstr;
                if (nodeFMs[j].vl_ndphases_dg = 3)   //only 3 phase nodes
                    and ((nodeFMs[j].vl_nodeType_phase[1] + nodeFMs[j].vl_nodeType_phase[2] + nodeFMs[j].vl_nodeType_phase[3]) = 3) then //has 3-phase DG
                begin
                    den_dij += pCommMatrix[NodeNuminClstr, j];
                    TempAlphaP += pCommMatrix[NodeNuminClstr, j] * nodeFMs[j].vl_AlphaP_dg;
                end;

                for j := NodeNuminClstr to Nodes - 1 do
                begin
                    if (nodeFMs[j].vl_ndphases_dg = 3)   //only 3 phase nodes
                        and ((nodeFMs[j].vl_nodeType_phase[1] + nodeFMs[j].vl_nodeType_phase[2] + nodeFMs[j].vl_nodeType_phase[3]) = 3) then //has 3-phase DG
                    begin
                        den_dij += pCommMatrix[NodeNuminClstr, j];
                        dly := pcommDelaysteps[NodeNuminClstr, j];
                        if dly = 0 then
                        begin
                            TempAlphaP += pCommMatrix[NodeNuminClstr, j] * nodeFMs[j].vl_AlphaP_dg;
                        end
                        else
                        begin
                            TempAlphaP += pCommMatrix[NodeNuminClstr, j] * nodeFMs[j].vl_smpl_dg[2, dly];
                        end;
                    end;

                end;
            end;
            if den_dij = 0 then
                TempAlphaP := 0.0
            else
            begin
                TempAlphaP /= den_dij;   //the average
            end;
              //Tolerance of alphap_i alphap_j
            TempAlphaP -= nodeFMs[NodeNuminClstr].vl_AlphaP_dg; //uL for cooperative control of active power
            if abs(TempAlphaP) < 0.002 then
                Result := 0.0
            else
                Result := TempAlphaP;
        end;
    end;
end;

procedure TFMonitorObj.update_node_info_each_time_step(); //all nodes , p.u. value
var
    den,
    i: Integer;
begin
    dlt_z0 := 0.0;
    den := 0;
    for i := 0 to Nodes - 1 do
    begin
        if (nodeFMs[i].vl_ndphases_dg = 3)   //only 3 phase nodes
            and ((nodeFMs[i].vl_nodeType_phase[1] + nodeFMs[i].vl_nodeType_phase[2] + nodeFMs[i].vl_nodeType_phase[3]) = 3) then
        begin
            nodeFMs[i].vl_Alpha_dgn := nodeFMs[i].vl_Alpha_dg;
            nodeFMs[i].z_dfsn := nodeFMs[i].z_dfs;
            dlt_z0 := dlt_z0 + nodeFMs[i].vl_Gradient_dg;
        end;
        den := den + 1;
    end;

     //sumation or average
    if den <> 0 then
        dlt_z0 := -dlt_z0 / den; // gredient (v-vref)

end;

//Calculate equivalent omega and delta
procedure TFMonitorObj.Calc_P_freq_fm();
var
    domg, dPm: Double;
    DeltaP, tmp: Double;
begin
    // initializing first time
    if ActiveCircuit.Solution.DynaVars.t < init_time then
        pm_fm := self.Get_power_trans();

    //preparation : calculate Delta P
    tmp := self.Get_power_trans();
    DeltaP := pm_fm - tmp;
    //derivatives
    //ddlt := omg_fm;
    domg := (DeltaP / (kVA_fm * 1000) - D_fm * omg_fm) / M_fm;
    dpm := -ki_fm * omg_fm * (kva_fm * 1000) / tau_fm;
      //integral
    if ActiveCircuit.Solution.Mode() = TSolveMode.DYNAMICMODE then
    begin
        //dlt_fm += ddlt * ActiveCircuit.Solution.DynaVars.h;
        Pm_fm += dpm * ActiveCircuit.Solution.DynaVars.h;
        omg_fm += domg * ActiveCircuit.Solution.DynaVars.h;
    end;
    comp_omg := omg_fm + DeltaP / (kVA_fm * 1000) / D_fm; //comp_omg is (\Delta f + \Delta P / B)
end;

procedure TFMonitorObj.update_ld_dly(); //all nodes , p.u. value
var
    i, j, ndlys: Integer;
    v0_tmp: Double;
    crnt_time: Double;
begin
    ld_fm_info[0].volt_avg := 0.0; //recalculate voltage average
    ld_fm_info[0].volt_lwst := 999999; //search new value at each round
    ld_fm_info[0].volt_hghst := -99999;
    for i := 0 to Nodes - 1 do
    begin
        //update vl_v1/v2/v3, vl_v_1c/v_2c/v_3c, update vl_v for node i
        Get_PDElem_terminal_voltage(i, nodeFMs[i].vl_strMeasuredName, nodeFMs[i].vl_terminalNum);
        
        //synchronous sampling
        if t_intvl_smpl = 0.0 then
        begin
            // nodeFMs[i].vl_smpl_dg[i, j] is not used
            v0_tmp := nodeFMs[i].vl_V / (nodeFMs[i].vl_basevolt);
        end
        else
        begin
            //asynchronous sampling

            //update nodeFMs[i].vl_smpl_dg[i, j] first
            if nodeFMs[i].vl_SmplCnt = 0 then//the first step
            begin
                for j := 0 to MaxLocalMem do
                begin
                    //alphas
                    nodeFMs[i].vl_smpl_dg[1, j] := nodeFMs[i].vl_Alpha_dg;
                    nodeFMs[i].vl_smpl_dg[2, j] := nodeFMs[i].vl_AlphaP_dg;
                    //voltage
                    nodeFMs[i].vl_smpl_dg[3, j] := nodeFMs[i].vl_V;  // 0 seq.
                end;
            end;
            
            crnt_time := ActiveCircuit.Solution.DynaVars.intHour * 3600 + ActiveCircuit.Solution.DynaVars.t;
            //Move the array only at the first time-step

            //if t_k greater or equal to current sample time plus smp interval, do another sample
            if crnt_time >= (T_intvl_smpl + nodeFMs[i].vl_crnt_smp_time) then
            begin
                //save alf into the first entry of smpl_ary for communication
                //alpha
                nodeFMs[i].vl_smpl_dg[1, 0] := nodeFMs[i].vl_Alpha_dg;// [0] is the newest value
                //alphaP
                nodeFMs[i].vl_smpl_dg[2, 0] := nodeFMs[i].vl_AlphaP_dg;//
                // VL_V  //0 seq voltage
                nodeFMs[i].vl_smpl_dg[3, 0] := nodeFMs[i].vl_V;//

                for j := 0 to MaxLocalMem - 1 do // [0]->[1],[MaxLocalMem-1]->[MaxLocalMem]
                begin
                    nodeFMs[i].vl_smpl_dg[1, MaxLocalMem - j] := nodeFMs[i].vl_smpl_dg[1, MaxLocalMem - j - 1];
                    nodeFMs[i].vl_smpl_dg[2, MaxLocalMem - j] := nodeFMs[i].vl_smpl_dg[2, MaxLocalMem - j - 1];
                    nodeFMs[i].vl_smpl_dg[3, MaxLocalMem - j] := nodeFMs[i].vl_smpl_dg[3, MaxLocalMem - j - 1]
                end;

                //vl_SmplCnt increase
                inc(nodeFMs[i].vl_SmplCnt);
                //update vl_crnt_time
                nodeFMs[i].vl_crnt_smp_time := crnt_time;
            end;
            // delay steps from agent to virtual leader
            ndlys := pcommDelaysteps[virtual_Ld_Nd, i];
            // total delay steps: ndlys+nup_dlys
            //if nodeFMs[i].vl_basevolt <> 0.0 then
            v0_tmp := nodeFMs[i].vl_smpl_dg[3, ndlys + nUp_dlys] / (nodeFMs[i].vl_basevolt);
        end;
           //update highest voltage
        if ld_fm_info[0].volt_hghst < v0_tmp then
        begin
            ld_fm_info[0].volt_hghst := v0_tmp;
            // ld_fm_info[0].ndnum_hghst := i;
        end;
            //update lowest voltage
        if ld_fm_info[0].volt_lwst > v0_tmp then
        begin
            ld_fm_info[0].volt_lwst := v0_tmp;
            // ld_fm_info[0].ndnum_lwst := i;
        end;

           //other information should be updated?
           //
        ld_fm_info[0].volt_avg += v0_tmp;  //p.u.
    end;
     //avg of valtage
    ld_fm_info[0].volt_avg /= nodes;
end;

//attack and defense
procedure TFMonitorObj.update_attack(); // update d_i
var
    dlt_d: Double;
    j: Integer;
begin
    //attack and defense at this step
    if not atk then
        exit;

    dlt_d := 0.0; // no dynamic for now

    if atk and (ActiveCircuit.Solution.DynaVars.SolutionMode = TSolveMode.DYNAMICMODE) and (ActiveCircuit.Solution.DynaVars.t >= atk_time) then
    begin
        // initializing first, and only once
        if d_atk_inited = false then
        begin
            for j := 0 to Nodes - 1 do
            begin
                if j = atk_node_num then // only the node being attacked is affected
                begin
                    nodeFMs[j].d_atk := nodeFMs[j].d_atk0;
                end;
            end;
            d_atk_inited := true;
        end;
        // attack
        for j := 0 to Nodes - 1 do
        begin
            if j = atk_node_num then // only the node being attacked is affected
            begin
                nodeFMs[j].d_atk += ActiveCircuit.Solution.DynaVars.h * dlt_d;
            end;
        end;
    end
    else
    begin
        // no attack
        for j := 0 to Nodes - 1 do
            nodeFMs[j].d_atk := 0.0;
    end;
end;

procedure TFMonitorObj.update_defense();// update z_i
var
    dlt_z: Double;
    j: Integer;
    den_dij, den_dij_z: Integer;
    tempZ, tempAlpha: Double;
    i: Integer;
begin
    if not dfs then // if no defense
        Exit;

    den_dij_z := 1;
    if (ActiveCircuit.Solution.DynaVars.SolutionMode <> TSolveMode.DYNAMICMODE) then
        Exit;

    if (ActiveCircuit.Solution.DynaVars.t <= atk_time) then
    begin
        // IF THERE IS NO ATTACK YET, Z FOLLOWS ALPHA
        for j := 0 to Nodes - 1 do
        begin
            nodeFMs[j].z_dfs := nodeFMs[j].vl_alpha_dg; //the let z : alpha
            nodeFMs[j].z_dfsn := nodeFMs[j].z_dfs;
        end;
    end;

    if not (ActiveCircuit.Solution.DynaVars.t >= atk_time) then
        Exit;

    //calculate the initial value for z_dfs
    if not z_dfs_inited then
    begin
        for j := 0 to Nodes - 1 do
        begin
            nodeFMs[j].z_dfs := nodeFMs[j].vl_alpha_dgn; //the let z : alpha
            nodeFMs[j].z_dfsn := nodeFMs[j].z_dfs;
        end;
        // has been initiated
        z_dfs_inited := true;
    end;

    //update for each node
    for j := 0 to Nodes - 1 do
    begin
        // x_i'  =  A_i x + \beta K_i z + \beta B_i x_0 + d_i
        // z_i'  =  H_i Z + \beta G_i x + \beta D_i x_0
        // calculate z_i
        //////////////////////
        // derivative calculation
        dlt_z := dlt_z0; //dlt_z0 will be update at each time step by average of gradient     actually this is -us_i
        // j is the outer loop
        // pCommMatrix is used as the matrix for H_i , G_i, K_i
        den_dij := 0;
        TempZ := 0.0;
        tempAlpha := 0.0;
        for i := 0 to Nodes - 1 do
        begin
            if (nodeFMs[i].vl_ndphases_dg = 3)   //only 3 phase nodes
                and ((nodeFMs[i].vl_nodeType_phase[1] + nodeFMs[i].vl_nodeType_phase[2] + nodeFMs[i].vl_nodeType_phase[3]) = 3) then //this phase has DG
            begin
                //Sumation of all Z and alpha s
                den_dij_z := den_dij + pCommHide[j, i];
                TempZ += pCommHide[j, i] * nodeFMs[i].z_dfsn;

                den_dij += pCommMatrix[j, i];
                tempAlpha += pCommMatrix[j, i] * nodeFMs[i].vl_Alpha_dgn;
            end;
            ///
        end;
        // average
        if den_dij = 0 then
        begin
            TempZ := 0.0;
            TempAlpha := 0.0;
        end
        else
        begin
            TempZ /= den_dij_z;
            TempAlpha /= den_dij;
        end;
        TempZ := (TempZ - nodeFMs[j].z_dfsn);
        TempAlpha := (TempAlpha - nodeFMs[j].vl_Alpha_dgn);

        // z_i'  =  H_i Z + \beta G_i x + \beta D_i x_0
        dlt_z := TempZ + beta_dfs * TempAlpha;//- beta_dfs *dlt_z0;// - nodeFMs[j].z_dfsn/den_dij;// - 0.1* nodeFMs[j].z_dfs ; //+ nodeFMs[j].vl_kcq_dg*dlt_z0 ;
        // integration
        nodeFMs[j].z_dfs := nodeFMs[j].z_dfsn + dlt_z * ActiveCircuit.Solution.DynaVars.h;
    end;
end;

function TFMonitorObj.organise_dfs_node(j: Integer): Double;    // calculate K_i z  // x_i'  =  A_i x + \beta K_i z + \beta B_i x_0 + d_i
var
    i: Integer;
    den_dij: Integer;
    tempZ: Double;
begin
    // x_i'  =  A_i x - \beta K_i z + \beta B_i x_0 + d_i
    // z_i'  =  H_i Z + \beta G_i x + \beta D_i x_0

    // this function is to calculate
    // K_i z
    den_dij := 0;
    TempZ := 0.0;
    for i := 0 to Nodes - 1 do
    begin
        if (nodeFMs[i].vl_ndphases_dg = 3) and  //only 3 phase nodes
            ((nodeFMs[i].vl_nodeType_phase[1] + nodeFMs[i].vl_nodeType_phase[2] + nodeFMs[i].vl_nodeType_phase[3]) = 3) then //this phase has DG
        begin
            den_dij += pCommMatrix[j, i];
            TempZ += pCommMatrix[j, i] * nodeFMs[i].z_dfsn;
        end;
    end;

    //average
    if den_dij = 0 then
        TempZ := 0.0
    else
    begin
        TempZ /= den_dij;
    end;
    result := -(TempZ - nodeFMs[j].z_dfsn);// - nodeFMs[j].z_dfsn/den_dij; // should be ZERO at last
end;

finalization
    ActionEnum.Free;
end.
{$POP}