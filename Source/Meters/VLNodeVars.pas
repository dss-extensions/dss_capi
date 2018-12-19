unit VLNodeVars;

{
  ----------------------------------------------------------
  Copyright (c) 2017, Electric Power Research Institute, Inc.
  Added by Ying.
  ----------------------------------------------------------

  Definition of Fmonitor (virtue leader) Public Data Record
}

interface

Uses
      UComplex;
      //PointerList;

TYPE
   pNodeVar = ^TVLNodeVars;
   {Fmonitor public data/state variable structure}

   //value ot save communication delay
   //TDelays = packed record
   Tdlys = array [0..99] of double; //max 99

   //end;
   TVLNodeVars = packed Record
   //properties for Node
         vl_strBusName : string;
         vl_strMeasuredName : string;
         vl_terminalNum : integer;
         vl_ndphases : integer;
         vl_basevolt : double;
         vl_nodeType_phase : array [1..3] of smallint;   // set by TFMonitorObj.Init_nodeFM :
                                  //for each phase
                                  //1, dg under it; 2, no dg there
                                  //if a dg is connected, it is 1;

         vl_V,vl_V1,vl_V2,vl_V3 : double;

         Bus_Idx: integer;      // has to be updated before being used
                                // it is related to YMatrix
         Yii : array [0..3] of complex; //each phase  123 - ABC,  0 - pos seq
         Yij : array [0..3] of complex; //each phase  123 - ABC,  0 - pos seq

        // complex voltage
        vl_V_c,vl_V_1c,vl_V_2c,vl_V_3c : complex;
   //Properties for DG
        vl_strName_dg : string;
        //vl_terminalNum : integer;
        vl_ndphases_dg : integer;   // set by TFMonitorObj.Init_nodeFM: 1,3
        vl_phase_num_dg : integer; //   set by TFMonitorObj.Init_nodeFM, 123--abc 0- this node has 3-phase
                            //if vl_nodeType=1, and vl_ndphases=1,phase_num =1,2,3
        //SmallIntArray = Array[1..100] of SmallInt;
        //vl_strBusName : string;
        vl_CC_switch_dg : boolean;// cooperate control switch. true, cooperate control is on
        vl_PF_flag_dg : integer;//1, real power control is on
        vl_QV_flag_dg : integer;//1, volt/var control is on
        //vl_phase_select
        vl_Alpha_dg,
        vl_Alpha1_dg,vl_Alpha2_dg,vl_Alpha3_dg,
        vl_Gradient_dg,vl_Gradient1_dg,vl_Gradient2_dg,vl_Gradient3_dg: double;

        // communication array for alpha and others can be improved
        vl_smpl_dg  : array [1..6] of Tdlys; //1: alpha; 2: alphaP; 3, bus voltage 0 seq. ; 4,5,6: bus voltage ABC

        //
        vl_SmplCnt : integer;  //sample count for this agent
        vl_crnt_smp_time : double; //time for current sample at this agent

        vl_AlphaP_dg,
        vl_AlphaP1_dg,vl_AlphaP2_dg,vl_AlphaP3_dg : double;
        vl_GradientP_dg, vl_GradientP1_dg,vl_GradientP2_dg,vl_GradientP3_dg: double;
        vl_Pmax_dg, vl_Qmax_dg,
        vl_Pmax_phase_dg, vl_Qmax_phase_dg : double;
        vl_V_base_dg,
        vl_V_ref_dg, vl_V_ref1_dg, vl_V_ref2_dg, vl_V_ref3_dg : double;// nominal value with respect to p.u. 1  //must be set by initialization
        vl_kcq_dg : double; // the step size gain of agent i //has to be defined befor used
        vl_p_DG,vl_p_DG1,vl_p_DG2,vl_p_DG3 : double;
        vl_kcd_dg : double; // the step size gain of agent i //has to be defined befor used
        vl_kc_ul_dg : double; // the cooperative gain for agent i
        vl_q_DG,vl_q_DG1,vl_q_DG2,vl_q_DG3 : double;
   //Properties for Loads
        ldType : integer; //-1: noload; 0: one 3phase or 2phase load; 1, 2, 3: 1,2 or 3 single loads;
        ldIdx, ldIdx1, ldIdx2, ldIdx3 : integer;
        vl_Q_Di : double; //all load reactive power except DG
        vl_Q_Di1 : double; //
        vl_Q_Di2 : double; //
        vl_Q_Di3 : double; //
        vl_P_Di : double; //all load reactive power except DG
        vl_P_Di1 : double; //
        vl_P_Di2 : double; //
        vl_P_Di3 : double; //
       // vl_NodeRef : integer;// for global use

    End;
    pNodeArray = ^NodeArray;
    NodeArray = Array [1..33] of TVLNodeVars;


implementation

end.
