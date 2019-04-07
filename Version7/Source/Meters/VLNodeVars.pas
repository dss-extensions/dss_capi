unit VLNodeVars;

{
  ----------------------------------------------------------
  Copyright (c) 2017, Electric Power Research Institute, Inc.
  Added by Ying.
  ----------------------------------------------------------

  Definition of Fmonitor (virtue leader) Public Data Record
}

interface

uses
    UComplex;
      //PointerList;

type
    pNodeVar = ^TVLNodeVars;
   {Fmonitor public data/state variable structure}

   //value ot save communication delay
   //TDelays = packed record
    Tdlys = array [0..99] of Double; //max 99

   //end;
    TVLNodeVars = packed record
   //properties for Node
        vl_strBusName: String;
        vl_strMeasuredName: String;
        vl_terminalNum: Integer;
        vl_ndphases: Integer;
        vl_basevolt: Double;
        vl_nodeType_phase: array [1..3] of Smallint;   // set by TFMonitorObj.Init_nodeFM :
                                  //for each phase
                                  //1, dg under it; 2, no dg there
                                  //if a dg is connected, it is 1;

        vl_V, vl_V1, vl_V2, vl_V3: Double;

        Bus_Idx: Integer;      // has to be updated before being used
                                // it is related to YMatrix
        Yii: array [0..3] of complex; //each phase  123 - ABC,  0 - pos seq
        Yij: array [0..3] of complex; //each phase  123 - ABC,  0 - pos seq

        // complex voltage
        vl_V_c, vl_V_1c, vl_V_2c, vl_V_3c: complex;
   //Properties for DG
        vl_strName_dg: String;
        //vl_terminalNum : integer;
        vl_ndphases_dg: Integer;   // set by TFMonitorObj.Init_nodeFM: 1,3
        vl_phase_num_dg: Integer; //   set by TFMonitorObj.Init_nodeFM, 123--abc 0- this node has 3-phase
                            //if vl_nodeType=1, and vl_ndphases=1,phase_num =1,2,3
        //SmallIntArray = Array[1..100] of SmallInt;
        //vl_strBusName : string;
        vl_CC_switch_dg: Boolean;// cooperate control switch. true, cooperate control is on
        vl_PF_flag_dg: Integer;//1, real power control is on
        vl_QV_flag_dg: Integer;//1, volt/var control is on
        //vl_phase_select
        vl_Alpha_dg,
        vl_Alpha1_dg, vl_Alpha2_dg, vl_Alpha3_dg,
        vl_Gradient_dg, vl_Gradient1_dg, vl_Gradient2_dg, vl_Gradient3_dg: Double;

        // communication array for alpha and others can be improved
        vl_smpl_dg: array [1..6] of Tdlys; //1: alpha; 2: alphaP; 3, bus voltage 0 seq. ; 4,5,6: bus voltage ABC

        //
        vl_SmplCnt: Integer;  //sample count for this agent
        vl_crnt_smp_time: Double; //time for current sample at this agent

        vl_AlphaP_dg,
        vl_AlphaP1_dg, vl_AlphaP2_dg, vl_AlphaP3_dg: Double;
        vl_GradientP_dg, vl_GradientP1_dg, vl_GradientP2_dg, vl_GradientP3_dg: Double;
        vl_Pmax_dg, vl_Qmax_dg,
        vl_Pmax_phase_dg, vl_Qmax_phase_dg: Double;
        vl_V_base_dg,
        vl_V_ref_dg, vl_V_ref1_dg, vl_V_ref2_dg, vl_V_ref3_dg: Double;// nominal value with respect to p.u. 1  //must be set by initialization
        vl_kcq_dg: Double; // the step size gain of agent i //has to be defined befor used
        vl_p_DG, vl_p_DG1, vl_p_DG2, vl_p_DG3: Double;
        vl_kcd_dg: Double; // the step size gain of agent i //has to be defined befor used
        vl_kc_ul_dg: Double; // the cooperative gain for agent i
        vl_q_DG, vl_q_DG1, vl_q_DG2, vl_q_DG3: Double;
   //Properties for Loads
        ldType: Integer; //-1: noload; 0: one 3phase or 2phase load; 1, 2, 3: 1,2 or 3 single loads;
        ldIdx, ldIdx1, ldIdx2, ldIdx3: Integer;
        vl_Q_Di: Double; //all load reactive power except DG
        vl_Q_Di1: Double; //
        vl_Q_Di2: Double; //
        vl_Q_Di3: Double; //
        vl_P_Di: Double; //all load reactive power except DG
        vl_P_Di1: Double; //
        vl_P_Di2: Double; //
        vl_P_Di3: Double; //
       // vl_NodeRef : integer;// for global use

    end;
    pNodeArray = ^NodeArray;
    NodeArray = array [1..33] of TVLNodeVars;


implementation

end.
