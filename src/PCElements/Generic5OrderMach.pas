unit Generic5OrderMach;
{$PUSH}
{$RANGECHECKS ON}

// Copyright (c) 2024 DSS-Extensions contributors
// Copyright (c) 2008-2023, Electric Power Research Institute, Inc.

// **Heavily** modified (by Paulo Meira) for DSS-Extensions. 
// The original file had a lot of leftover code from IndMach012.pas,
// and many of the DSS properties could mislead users. At the moment,
// consider this component experimental, and it might be modified in
// the future.

// Original notice:
// November 3, 2017
// Created by
//    Darhey  Xu

interface

uses
    DSSClass,
    PCClass,
    PCElement,
    ucmatrix,
    ucomplex, DSSUComplex,
    ArrayDef,
    // LoadShape,
    Dynamics,
    DSSObject;

const
    Generic5_nOrder = 6;

type
{$SCOPEDENUMS ON}
    TGeneric5PropLegacy = (
        INVALID = 0,
        phases,
        bus1,
        kv,
        kW,
        pf,
        conn,
        kVA,
        P_ref1kW,
        P_ref2kW,
        P_ref3kW,
        V_ref1kVLN,
        V_ref2kVLN,
        V_ref3kVLN,
        P_refkW,
        Q_refkVAr,
        Cluster_num,
        V_refkVLN,
        ctrl_mode,
        QV_flag,
        kcd,
        kcq,
        kqi,
        Q_ref1kVAr,
        Q_ref2kVAr,
        Q_ref3kVAr,
        PmaxkW,
        PminkW,
        PQpriority,
        PmppkW,
        Pfctr1,
        Pfctr2,
        Pfctr3,
        Pfctr4,
        Pfctr5,
        Pfctr6,
        PbiaskW,
        CC_Switch,
        kcq_drp2,
        Volt_Trhd,
        droop
    );
    TGeneric5Prop = (
        INVALID = 0,
        Phases,
        Bus1,
        kV,
        kW,
        PF,
        Conn,
        kVA,
        P_Ref1kW,
        P_Ref2kW,
        P_Ref3kW,
        V_Ref1kVLN,
        V_Ref2kVLN,
        V_Ref3kVLN,
        P_RefkW,
        Q_RefkVAr,
        Cluster_Num,
        V_refkVLN,
        Ctrl_Mode,
        QV_flag,
        kcd,
        kcq,
        kqi,
        Q_ref1kvar,
        Q_ref2kvar,
        Q_ref3kvar,
        PMaxkW,
        PMinkW,
        PQPriority,
        PmppkW,
        Pfctr1,
        Pfctr2,
        Pfctr3,
        Pfctr4,
        Pfctr5,
        Pfctr6,
        PbiaskW,
        CC_Switch,
        kcq_drp2,
        Volt_Trhd,
        Droop
    );

    TGeneric5Variable = (
        INVALID = 0,
        V_DG = 1, //pos seq value
        P_DG = 2,
        Q_DG = 3,
        V_DG1 = 4, //Phase A or the first phase if there are less than 3
        P_DG1 = 5,
        Q_DG1 = 6,
        V_DG2 = 7, //Phase B if exists
        P_DG2 = 8,
        Q_DG2 = 9,
        V_DG3 = 10, //phase C if exists
        P_DG3 = 11,
        Q_DG3 = 12,
        Qmax = 13,
        Qmax_Phase = 14,
        Pmax = 15,
        Pmax_Phase = 16,
        Alpha = 17,
        Alpha1 = 18,
        Alpha2 = 19,
        Alpha3 = 20,
        AlphaP = 21,
        AlphaP1 = 22,
        AlphaP2 = 23,
        AlphaP3 = 24,
        V_ref = 25, //Voltage object
        kVA = 26, //kVArating
        kW = 27,
        cluster_num = 28,
        NdNumInCluster = 29,
        ctrl_mode = 30,
        Gradient = 31,
        Id = 32,
        Iq = 33,
        P_set = 34,
        Frequency = 35,
        Defense = 36
    );
{$SCOPEDENUMS OFF}

    TGeneric5 = class(TPCClass)
    PROTECTED
        cBuffer: TCBuffer24;// Temp buffer for complex math calcs; allows up to 24-phase models.

        procedure DefineProperties(); override;
    PUBLIC
        varNames: ArrayOfString;

        constructor Create(dssContext: TDSSContext);
        destructor Destroy; OVERRIDE;

        function EndEdit(ptr: Pointer; const NumChanges: integer): Boolean; override;
        Function NewObject(const ObjName: String; Activate: Boolean = True): Pointer; OVERRIDE;
    end;

    TSymCompArray = array[0..2] of Complex;

    TGeneric5Obj = class(TPCElement)
    PRIVATE
        Yeq: Complex; // Y at nominal voltage

        // Dynamics variables
        Xp: Double;

        // X,V
        X_var: Array[0..Generic5_nOrder-1] of Double;
        dX_vardt: Array[0..Generic5_nOrder-1] of Double;
        X_varn: Array[0..Generic5_nOrder-1] of Double; // for trapezoidal integration
        dX_vardtn: Array[0..Generic5_nOrder-1] of Double;
        V_in_var: Array[0..Generic5_nOrder-1] of Double;
        pV_f_CC: Array[0..Generic5_nOrder-1] of Double;
        CC_Switch: LongBool;
        // A, B Matrix
        // Amm: Array of Array of Double;
        // Bmn: Array of Array of Double;

        // InDynamics: Boolean; // NOTE: commented since it's not used
        Is1, Ir1, V1, // Keep the last computed voltages and currents
        Is2, Ir2, V2: Complex;

        // Complex variables for dynamics
        Zsp: Complex;

        Id, Iq: Double; //Id related to P ; Iq related to Q
        // flag_dyna_Id_chg: Boolean;

        dIqdt: Double;
        Idn, Iqn: Double; //save last time step for integration.
        // the input for control purpose
        kcd, kcq, kcq_drp2: Double; //the control gain in vi1, vi2
        Volt_Trhd: Double;
        droop: Integer;//droop type: 2, Q = kcq_drp2 * (1-v_dg). others: integral droop with kcq
        //flag_drp2 : integer; //if it is 1, drp2
        kqi: Double; //control gain for Q_ref
        vi1, vi2: Double; //the input of the control
        Id_ref, Iq_ref: Double; // The pursued value of Id related to P ; Iq related to Q

        P_ref, P_RefTotal, Q_ref, Q_RefTotal, V_ref: Double;//Power and voltage goal of the machine
        DPx: Double;

        P_DG, Q_DG: Double; //power of all phases totally in one
        V_DG: Double;// the voltage magetitude of current bus
        Theta_DG: Double; //the voltage angel of DG bus to slack
        QV_flag: Integer; // 0 Q_ref; 1 V_ref
        //QV_flag_0 : integer;
        //QV_switch: integer; //if Q hits limits, PV to PQ, the QV_switch:= 1; each time Edit function runs, check this and set QV_flag back to user set.
        // --for 3 phases--
        //power, voltage, angle,
        P_DG1, P_DG2, P_DG3,
        Q_DG1, Q_DG2, Q_DG3,
        V_DG1, V_DG2, V_DG3,
        V_theta1, V_theta2, V_theta3: Double;//operation values
        Id1, Iq1, Id2, Iq2, Id3, Iq3: Double; //currents
        //set values
        P_ref1, P_ref2, P_ref3,
        Q_ref1, Q_ref2, Q_ref3,
        V_ref1, V_ref2, V_ref3: Double;// set values from outside
        // ------Max Check-------
        //SMax; //  kVArating; // 'g1.kva=100'
        PMax, // Activity power output limit
        PMax_phase, //limit per phase
        PMin, //(0, default) // 'g1.pmax=100'
        Pmin_phase, //
        Qmax, //Reactive power output limit
        Qmax_phase,
        Qmin, //(-Qmax, default)
        Qmin_phase: Double; //
        // IdMax_phase,
        // IqMax_phase: Double;//phase current limit
        PQpriority: Integer; //Active and reactive power control mode, control s
        //'g1.pqvflag=0 Q, 1 P;

        //equivalent frequency
        freq: Double;
        z_dfs_plot: Double;

        Generic5SwitchOpen: Boolean;

        kVArating: Double;
        kVGeneratorBase: Double;
        // Pnominalperphase, Qnominalperphase: Double;

        // MachineON: Boolean; NOTE: this was left unmodified and was also not initialized. Needs to be left as "true" for it to work. Commented the "false" branches in the code
        // ShapeFactor: Complex;
        // ShapeIsActual: Boolean;

        VBase: Double;
        WBase: Double;
        // ---deal with -Update_Pmax_by_Ftrs-
        Pmpp,//Pmpp, default value is 1.0;
        Pbias, //Pbias, default value is 0.0;
        Pfctr1,//factors, default value all are 1.0;
        Pfctr2,
        Pfctr3,
        Pfctr4,
        Pfctr5,
        Pfctr6: Double;

        // ----------------
        //Gradient ; public
        Alpha, Alpha1, Alpha2, Alpha3,
        Gradient, 
        Gradient1, Gradient2, Gradient3: Double;

        AlphaP, AlphaP1, AlphaP2, AlphaP3: Double;// for active P control
        GradientP: Double;

        procedure CalcYPrimMatrix(Ymatrix: TcMatrix);
        procedure CalcGeneric5ModelContribution();
        procedure CalcInjCurrentArray();
        procedure CalcModel(V, I: pComplexArray);
        procedure update_controlinput();
        procedure update_pV_f_CC(); // update cooperate control part, pV_f_CC
        procedure update_pV_f_CC_M2(); // update cooperate control part, pV_f_CC
        procedure Update_PQLimits(); // real time limits check; can also be used in power flow and simulation
        procedure InfoPublish();
    PROTECTED
        FFMonObj, FFMonObj2: TDSSObject;

        procedure GetTerminalCurrents(Curr: pComplexArray); OVERRIDE;

        procedure DoDynamicMode();

    PUBLIC
        ctrl_mode: Integer; //ctrl_mode 0-local droop  V_ref = V_DG_0, P_ref = P_DG_0
        cluster_num: Integer;
        NdNumInCluster: Integer;
        // cluster_num2: Integer;
        // NdNumInCluster2: Integer;

        // DailyDispShapeObj: TLoadShapeObj; // Daily Generator Shape for this load
        // DutyShapeObj: TLoadShapeObj; // Shape for this generator
        // YearlyShapeObj: TLoadShapeObj; // Shape for this Generator

        constructor Create(ParClass: TDSSClass; const Generic5ObjName: String);
        destructor Destroy; OVERRIDE;
        procedure PropertySideEffects(Idx: Integer; previousIntVal: Integer; setterFlags: TDSSPropertySetterFlags); override;
        procedure MakeLike(OtherPtr: Pointer); override;

        procedure RecalcElementData(); OVERRIDE; // Generally called after Edit is complete to recompute variables
        procedure CalcYPrim(); OVERRIDE; // Calculate Primitive Y matrix
        procedure SetConductorClosed(Index: Integer; Value: Boolean); OVERRIDE;
        procedure IntegrateABCD();
        procedure CalcDynamic(var V012, I012: TSymCompArray);
        procedure CalcPFlow(var V012, I012: TSymCompArray);
        // for abc phases: the below 2
        procedure CalcDynamicVIabc(var Vabc, Iabc: pComplexArray);
        procedure CalcPFlowVIabc(var Vabc, Iabc: pComplexArray);

        function InjCurrents(): Integer; OVERRIDE;

        function NumVariables: Integer; OVERRIDE;
        procedure GetAllVariables(var States: ArrayOfDouble); OVERRIDE;
        function GetVariable(i: Integer): Double; OVERRIDE;
        procedure SetVariable(i: Integer; Value: Double); OVERRIDE;
        function VariableName(i: Integer): String; OVERRIDE;

        procedure InitStateVars(); OVERRIDE;
        procedure IntegrateStates(); OVERRIDE;
        procedure InitHarmonics(); OVERRIDE;
        procedure MakePosSequence(); OVERRIDE;
    end;

implementation

uses
    ParserDel,
    DSSClassDefs,
    DSSGlobals,
    Circuit,
    Command,
    Sysutils,
    Math,
    MathUtil,
    Utilities,
    Generic5Helper,
    DSSHelper,
    DSSObjectHelper,
    TypInfo;

type
    TObj = TGeneric5Obj;
    TProp = TGeneric5Prop;
    TPropLegacy = TGeneric5PropLegacy;
    TVar = TGeneric5Variable;
const
    NumPropsThisClass = Ord(High(TProp));
    NumGeneric5Variables = Ord(High(TGeneric5Variable));
    NumOrderX = Generic5_nOrder; //  system order
    NumOrderY = Generic5_nOrder; //  system output Y order
var
    PropInfo: Pointer = NIL;
    PropInfoLegacy: Pointer = NIL;
    VarInfo: Pointer = NIL;

constructor TGeneric5.Create(dssContext: TDSSContext);
var
    i: Integer;
begin
    if PropInfo = NIL then
    begin
        PropInfo := TypeInfo(TProp);
        PropInfoLegacy := TypeInfo(TPropLegacy);
        VarInfo := TypeInfo(TGeneric5Variable);
    end;

    SetLength(varNames, NumGeneric5Variables);
    for i := 1 to NumGeneric5Variables do
        varNames[i - 1] := GetEnumName(VarInfo, i);

    inherited Create(dssContext, GENERIC5ORDERMACH_ELEMENT, 'Generic5');
end;

destructor TGeneric5.Destroy;
begin
    inherited Destroy;
end;

function getPF(obj: TObj): Double;
begin
    Result := PowerFactor(obj.Power(1));
end;

procedure TGeneric5.DefineProperties();
var 
    obj: TObj = NIL; // NIL (0) on purpose
begin
    Numproperties := NumPropsThisClass;
    CountPropertiesAndAllocate();
    PopulatePropertyNames(0, NumPropsThisClass, PropInfo, PropInfoLegacy);

    // bus properties
    PropertyType[ord(TProp.Bus1)] := TPropertyType.BusProperty;
    PropertyOffset[ord(TProp.Bus1)] := 1;
    PropertyFlags[ord(TProp.Bus1)] := [TPropertyFlag.Required];

    // enum properties
    PropertyType[ord(TProp.Conn)] := TPropertyType.MappedStringEnumProperty;
    PropertyOffset[ord(TProp.Conn)] := ptruint(@obj.Connection);
    PropertyOffset2[ord(TProp.Conn)] := PtrInt(DSS.ConnectionEnum);

    // Double properties, including adv. scaled ones
    PropertyOffset[ord(TProp.kcd)] := ptruint(@obj.kcd);
    PropertyOffset[ord(TProp.kcq)] := ptruint(@obj.kcq);
    PropertyOffset[ord(TProp.kqi)] := ptruint(@obj.kqi);
    PropertyOffset[ord(TProp.kV)] := ptruint(@obj.kVGeneratorBase);
    PropertyOffset[ord(TProp.Pfctr1)] := ptruint(@obj.Pfctr1); //for pmpp
    PropertyOffset[ord(TProp.Pfctr2)] := ptruint(@obj.Pfctr2); //for pmpp
    PropertyOffset[ord(TProp.Pfctr3)] := ptruint(@obj.Pfctr3); //for pmpp
    PropertyOffset[ord(TProp.Pfctr4)] := ptruint(@obj.Pfctr4); //for pmpp
    PropertyOffset[ord(TProp.Pfctr5)] := ptruint(@obj.Pfctr5); //for pmpp
    PropertyOffset[ord(TProp.Pfctr6)] := ptruint(@obj.Pfctr6); //for pmpp
    PropertyOffset[ord(TProp.kcq_drp2)] := ptruint(@obj.kcq_drp2);
    PropertyOffset[ord(TProp.Volt_Trhd)] := ptruint(@obj.Volt_Trhd);
    PropertyOffset[ord(TProp.kVA)] := ptruint(@obj.kVArating);

    PropertyOffset[ord(TProp.kW)] := ptruint(@obj.WBase);
    PropertyScale[ord(TProp.kW)] := 1000;

    PropertyOffset[ord(TProp.P_Ref1kW)] := ptruint(@obj.P_ref1);
    PropertyScale[ord(TProp.P_Ref1kW)] := 1000; //for phase ctrl unit kW to W

    PropertyOffset[ord(TProp.P_Ref2kW)] := ptruint(@obj.P_ref2);
    PropertyScale[ord(TProp.P_Ref2kW)] := 1000; //for phase ctrl

    PropertyOffset[ord(TProp.P_Ref3kW)] := ptruint(@obj.P_ref3);
    PropertyScale[ord(TProp.P_Ref3kW)] := 1000; //for phase ctrl

    PropertyOffset[ord(TProp.V_Ref1kVLN)] := ptruint(@obj.V_ref1);
    PropertyScale[ord(TProp.V_Ref1kVLN)] := 1000; //for phase ctrl unit kV to V

    PropertyOffset[ord(TProp.V_Ref2kVLN)] := ptruint(@obj.V_ref2);
    PropertyScale[ord(TProp.V_Ref2kVLN)] := 1000; //for phase ctrl

    PropertyOffset[ord(TProp.V_Ref3kVLN)] := ptruint(@obj.V_ref3);
    PropertyScale[ord(TProp.V_Ref3kVLN)] := 1000; //for phase ctrl

    PropertyOffset[ord(TProp.P_RefkW)] := ptruint(@obj.P_RefTotal);
    PropertyScale[ord(TProp.P_RefkW)] := 1000;//to norm value W from kW(in script)//for avg ctrl    1000*PrefKw/3;

    PropertyOffset[ord(TProp.Q_RefkVAr)] := ptruint(@obj.Q_RefTotal);
    PropertyScale[ord(TProp.Q_RefkVAr)] := 1000;//to VA from kVA(in script) //for avg ctrl 1000*QrefKVAr/3;

    PropertyOffset[ord(TProp.V_refkVLN)] := ptruint(@obj.V_ref);
    PropertyScale[ord(TProp.V_refkVLN)] := 1000;//kV  to V //for avg ctrl 1000*VrefkV;

    PropertyOffset[ord(TProp.Q_ref1kvar)] := ptruint(@obj.Q_ref1);
    PropertyScale[ord(TProp.Q_ref1kvar)] := 1000; //for phase ctrl unit kVar to Var

    PropertyOffset[ord(TProp.Q_ref2kvar)] := ptruint(@obj.Q_ref2);
    PropertyScale[ord(TProp.Q_ref2kvar)] := 1000; //for phase ctrl

    PropertyOffset[ord(TProp.Q_ref3kvar)] := ptruint(@obj.Q_ref3);
    PropertyScale[ord(TProp.Q_ref3kvar)] := 1000; //for phase ctrl

    PropertyOffset[ord(TProp.PMaxkW)] := ptruint(@obj.Pmax);
    PropertyScale[ord(TProp.PMaxkW)] := 1000; //Pmax has to be less then kW in script

    PropertyOffset[ord(TProp.PMinkW)] := ptruint(@obj.Pmin);
    PropertyScale[ord(TProp.PMinkW)] := 1000; //for phase ctrl

    PropertyOffset[ord(TProp.PmppkW)] := ptruint(@obj.Pmpp);
    PropertyScale[ord(TProp.PmppkW)] := 1000; //for pmpp kW

    PropertyOffset[ord(TProp.PbiaskW)] := ptruint(@obj.Pbias);
    PropertyScale[ord(TProp.PbiaskW)] := 1000; //for pmpp

    // boolean properties
    PropertyType[ord(TProp.CC_Switch)] := TPropertyType.BooleanProperty;
    PropertyOffset[ord(TProp.CC_Switch)] := ptruint(@obj.CC_Switch);

    // Integer properties
    PropertyType[ord(TProp.Phases)] := TPropertyType.IntegerProperty;
    PropertyOffset[ord(TProp.Phases)] := ptruint(@obj.FNPhases);
    PropertyFlags[ord(TProp.Phases)] := [TPropertyFlag.NonNegative, TPropertyFlag.NonZero];

    PropertyType[ord(TProp.Cluster_Num)] := TPropertyType.IntegerProperty;
    PropertyOffset[ord(TProp.Cluster_Num)] := ptruint(@obj.Cluster_Num);

    PropertyType[ord(TProp.Ctrl_Mode)] := TPropertyType.IntegerProperty;
    PropertyOffset[ord(TProp.Ctrl_Mode)] := ptruint(@obj.Ctrl_Mode);

    PropertyType[ord(TProp.QV_flag)] := TPropertyType.IntegerProperty;
    PropertyOffset[ord(TProp.QV_flag)] := ptruint(@obj.QV_flag);

    PropertyType[ord(TProp.PQPriority)] := TPropertyType.IntegerProperty;
    PropertyOffset[ord(TProp.PQPriority)] := ptruint(@obj.PQPriority);

    PropertyType[ord(TProp.Droop)] := TPropertyType.IntegerProperty;
    PropertyOffset[ord(TProp.Droop)] := ptruint(@obj.Droop);

    // double
    PropertyType[ord(TProp.PF)] := TPropertyType.DoubleProperty;
    PropertyOffset[ord(TProp.PF)] := 1;  // dummy
    PropertyFlags[ord(TProp.PF)] := [TPropertyFlag.ReadByFunction, TPropertyFlag.SilentReadOnly];
    PropertyReadFunction[ord(TProp.PF)] := @getPF;

    ActiveProperty := NumPropsThisClass;
    inherited DefineProperties;
end;

function TGeneric5.NewObject(const ObjName: String; Activate: Boolean): Pointer;
var
    Obj: TObj;
begin
    Obj := TObj.Create(Self, ObjName);
    if Activate then 
        ActiveCircuit.SetActiveCktElement(Obj);
    Obj.ClassIndex := AddObjectToList(Obj, Activate);
    Result := Obj;
end;

procedure SetNcondsForConnection(obj: TObj);
begin
    case obj.Connection of
        TGeneralConnection.Wye:
            obj.NConds := obj.Fnphases; // Neutral is not connected for induction machine
        TGeneralConnection.Delta:
            case obj.Fnphases of
                1, 2:
                    obj.NConds := obj.Fnphases + 1; // L-L and Open-delta
            else
                obj.NConds := obj.Fnphases; // no neutral for this connection
            end;
    end;
end;

procedure TGeneric5Obj.PropertySideEffects(Idx: Integer; previousIntVal: Integer; setterFlags: TDSSPropertySetterFlags);
begin
    case Idx of
        ord(TProp.PmppkW),
        ord(TProp.Pfctr1),
        ord(TProp.Pfctr2),
        ord(TProp.Pfctr3),
        ord(TProp.Pfctr4),
        ord(TProp.Pfctr5),
        ord(TProp.Pfctr6),
        ord(TProp.PbiaskW):
        begin
            // Update_kWbase_by_Fctrs();// Update Pmax; will cover direct Pmax input by these
            Wbase := (Pmpp + Pbias) * Pfctr1 * Pfctr2 * Pfctr3 * Pfctr4 * Pfctr5 * Pfctr6;
            Update_PQLimits();
        end;
        ord(TProp.Phases):
            // TODO: Set_NPhases side-effects?
            SetNcondsForConnection(self); // Force Reallocation of terminal info
        ord(TProp.kV):
        begin
            case FNphases of
                2, 3:
                    VBase := kVGeneratorBase * InvSQRT3x1000;
            else
                VBase := kVGeneratorBase * 1000.0;
            end;
        end;
        ord(TProp.kW):
        begin
            if (Pmax < 0) or (Pmax > WBase) then
                Pmax := WBase;
            if PQpriority = 1 then
                Pmax := WBase;
        end;
        ord(TProp.PMaxkW):
            PMax_phase := Pmax / fnphases;
        ord(TProp.PMinkW):
            PMin_phase := Pmin / fnphases; // Note: diverges from original (uses Pmax here)
        ord(TProp.P_RefKW):
        begin
            P_ref := P_RefTotal / fnphases;
            P_ref1 := P_ref;
            P_ref2 := P_ref;
            P_ref3 := P_ref;

            if P_RefTotal > WBase then
            begin
                DoSimpleMsg('PRef should be less than or equal to kW.', 562);
            end;

    // if ActiveCircuit.Solution.DynaVars.SolutionMode = DYNAMICMODE then 
    // begin 
    // //if P_ref changed in the dynamic simulation
    // //The sudden change of Id has to be applied
    // // flag_dyna_Id_chg := true;
    // end;
        end;
        ord(TProp.Q_RefkVAr):
        begin
            Q_ref := Q_RefTotal / fnphases;
            Q_ref1 := Q_ref;
            Q_ref2 := Q_ref;
            Q_ref3 := Q_ref;
        end;
        ord(TProp.V_refkVLN):
        begin
            V_ref1 := V_ref;
            V_ref2 := V_ref;
            V_ref3 := V_ref;
        end;
    end;
end;


function TGeneric5.EndEdit(ptr: Pointer; const NumChanges: integer): Boolean;
var
    obj: TObj;
begin
    obj:= TObj(ptr);
    obj.Update_PQLimits();
    obj.RecalcElementData();
    obj.YPrimInvalid := true;
    Exclude(obj.Flags, Flg.EditingActive);
    Result := True;    
end;

procedure TGeneric5Obj.MakeLike(OtherPtr: Pointer);
begin
    DoSimpleMsg('%s: TGeneric5Obj.MakeLike is not implemented. Aborting.', [FullName()], 202406012);
    DSS.SetSolutionAbort(true);
end;

constructor TGeneric5Obj.Create(ParClass: TDSSClass; const Generic5ObjName: String);
var
    i, j: Integer;
begin
    inherited create(ParClass, Generic5ObjName);
    DSSObjType := ParClass.DSSClassType; // Same as Parent Class

    // TODO: BUG: These three are neither initialized nor modified at all in the original code,
    //       but used later on
    Gradient1 := 0;
    Gradient2 := 0;
    Gradient3 := 0;

    // Set some basic circuit element properties
    Connection := TGeneralConnection.Delta; // Delta Default -- override the default in PCE (Wye)
    FNphases := 3; // typical DSS default for a circuit element
    Yorder := 0; // To trigger an initial allocation
    Nterms := 1; // forces allocations of terminal quantities
    WBase := -1;//00; // has to be set in DSS scripts

    FFMonObj := nil;
    FFMonObj2 := nil;
    Yorder := Fnterms * Fnconds;
    // ShapeIsActual := false;
    Generic5SwitchOpen := false;

    kVGeneratorBase := 12.47;

    kVArating := WBase * 1.2e-3;
    // InDynamics := false;

    // A, B, X_var, V_in_var matrix
    // SetLength(Amm, Generic5_nOrder, Generic5_nOrder);
    // SetLength(Bmn, Generic5_nOrder, Generic5_nOrder);
    //Allocate ABXYV
    // A,B matrix, X_var //5 order system
    for i := 0 to Generic5_nOrder - 1 do
    begin
        // for j := 0 to Generic5_nOrder - 1 do
        // begin
        //     Amm[i][j] := 0.0;
        // end;
        // for j := 0 to Generic5_nOrder - 1 do
        // begin
        //     Bmn[i][j] := 0.0;
        //     if j = i then
        //         Bmn[i][j] := 1;
        // end;
        X_var[i] := 0.0;
        dX_vardt[i] := 0.0;// derivatives
        X_varn[i] := 0.0;// for trapezoid
        dX_vardtn[i] := 0.0;// derivatives
    end;
    // Y, V
    for i := 0 to Generic5_nOrder - 1 do
    begin
        V_in_var[i] := 0.0;
        pV_f_cc[i] := 0.0;
    end;
    P_ref := 0;
    V_ref := 1;
    Id := 0;
    Iq := 0;//default current
    Id1 := 0;
    Iq1 := 0;
    Id2 := 0;
    Iq2 := 0;
    Id3 := 0;
    Iq3 := 0;

    kcd := 0.1;
    kcq := 0.1;
    kqi := 0.1; //for local control gain in vi1, vi2
    Volt_Trhd := 0.0;
    Cluster_Num := 0;//by default.
    // Update anything that has to be calculated from property values
    ctrl_mode := 0;// avg contrl by default
    QV_flag := 0;

    //PQ max
    PMax := -1; // Activity power output limit
    PMax_phase := PMax / fnphases; //limit per phase
    PMin := 0; //(0, default)
    Pmin_phase := PMin / fnphases; //
    Qmax := 1000 * kVArating; //Reactive power output limit
    Qmax_phase := Qmax / fnphases;
    Qmin := -Qmax; //(-Qmax, default)
    Qmin_phase := Qmin / fnphases; //
    PQpriority := 1;//P priority
    Pmpp := 1;//Pmpp, default value is 1.0;
    Pbias := 0; //Pbias, default value is 0.0;
    Pfctr1 := 1;//factors, default value all are 1.0;
    Pfctr2 := 1;
    Pfctr3 := 1;
    Pfctr4 := 1;
    Pfctr5 := 1;
    Pfctr6 := 1;

    kcq_drp2 := 0;
    CC_switch := false;
    // flag_dyna_Id_chg := false;

    z_dfs_plot := 0.0;

    // SetNcondsForConnection(self);
    RecalcElementData();
end;

destructor TGeneric5Obj.Destroy;
begin
    inherited Destroy; // This will take care of most common circuit element arrays, etc.
end;

procedure TGeneric5Obj.RecalcElementData();
var
    Rs, Xs, Xr, Xm, ZBase: Double;
    modetest: Boolean;
    numPhase, DotPos: Integer;
    strtemp: String;
begin
    ZBase := Sqr(kVGeneratorBase) / kVArating * 1000.0;
    Rs := 0.0053 * ZBase;
    Xs := 0.106 * ZBase;
    Xr := 0.12 * ZBase;
    Xm := 4.0 * ZBase;

    Xp := Xs + (Xr * Xm) / (Xr + Xm);
    Zsp := Cmplx(Rs, Xp);
    Yeq := Cmplx(0.0, -1.0 / ZBase); // vars only for power flow
    Is1 := 0;
    V1 := 0;
    Is2 := 0;
    V2 := 0;

    Reallocmem(InjCurrent, SizeOf(Complex) * Yorder);

    /// contrl mode
    ///    ctrl_mode =0; phases = 3; // pos avg control---p_ref, V_ref, Q_ref
    ///    ctrl_mode =1; phases = 1; bus1 = 452.1;      ---p_ref1, V_ref1, Q_ref1
    ///    ctrl_mode =2; phases = 1; bus1 = 452.2;      ---p_ref2, V_ref2, Q_ref2
    ///    ctrl_mode =3; phases = 1; bus1 = 452.3;      ---p_ref3, V_ref3, Q_ref3
    ///    ctrl_mode =4; phases = 3; bus1 = 452.2;      ---p_ref1,2,3, V_ref1,2,3, Q_ref1,2,3
    ///
    modetest := false;
    if (((ctrl_mode = 0) or (ctrl_mode = 4)) and (fnphases = 3)) then
        modetest := true
    else
    if (fnphases = 1) then
    begin
        if ((ctrl_mode = 1) or (ctrl_mode = 2) or (ctrl_mode = 3)) then
        begin
            strtemp := FirstBus; //only one
            DotPos := Pos('.', strtemp);
            if DotPos <> 0 then
            begin
                numphase := sysutils.StrToInt(Trim(Copy(strtemp, DotPos + 1, 1))); // Bus Name . node Num
                if numphase = ctrl_mode then
                    modetest := true
            end;
        end;
    end;
    if not modetest then
        DoSimpleMsg('ctrl_mode and bus node connection dont match, see help for generic5.ctrl_mode', 561);
end;

procedure TGeneric5Obj.IntegrateABCD();
var
    h2: Double;
    i, j: Integer;
begin
    if ActiveCircuit.Solution.Dynavars.IterationFlag = 0 then
    begin
        for i := 0 to NumOrderX - 1 do
        begin
            X_varn[i] := X_var[i];
            dX_vardtn[i] := dX_vardt[i];
        end;
    end;

    // update_system_abcd; //Matrix ABCD calculation if they are state-dependant
    update_controlinput(); //vi1, vi2 calculation,
    // co control strategies from network vfi can be done here
    //dX_vardt calculation
    for i := 0 to NumOrderX - 1 do //  NumOrderX, NumOrderY should be less than Generic5_nOrder 5
    begin
        dX_vardt[i] := 0.0;
        for j := 0 to NumOrderY - 1 do
        begin
            // dX_vardt[i] += Amm[i][j] * X_var[j] + Bmn[i][j] * V_in_var[j];
            if i <> j then
                continue;
            dX_vardt[i] += V_in_var[j];
            //cooperate control if exist is involved in
        end;
    end;

    // Trapezoidal Integration
    h2 := ActiveCircuit.Solution.Dynavars.h * 0.5;
    for i := 0 to NumOrderX - 1 do
    begin
        X_var[i] := X_varn[i] + h2 * (dX_vardtn[i] + dX_vardt[i]);
    end;

    //Y=CX to be added
    ///
    ///  the following is to connect with calcDynamic or CalcDynamicVIabc
    ///  because Id, Iq; Id1, Iq1, Id2, Iq2, Id3, Iq3 will be used there
    if ctrl_mode = 0 then //pos seq control
    begin
        Id := X_var[0];//can be put in calcdyna, so the integrate is just for X_var
        Iq := X_var[1];
    end
    else
    begin
        // all other ctrl_mode's are phase control modes
        Id1 := X_var[0];//1st phase, or the only phase if fnphases=1
        Iq1 := X_var[1];//can be put in calcdyna in futher, so the integrate is just for X_var
        Id2 := X_var[2];//2nd phase; zero if single phase
        Iq2 := X_var[3];
        Id3 := X_var[4];//3rd phase; zero if single phase
        Iq3 := X_var[5];
    end;
end;

//This part deals with the control input,  is based on the voltage measurement
procedure TGeneric5Obj.update_controlInput();
var
    j: Integer;
    temp_pref, temp_qref, temp_vref, Pref3: Double;
begin
    temp_qref := 0.0;
    temp_pref := 0.0;
    temp_vref := 0.0;
    ///////////////////////////////////
    //local control input and alpha gradient calculation
    ///////////////////////////////////
    //gradient, gradient1, gradient2, gradient3
    //   gradient will be calculated in FMonitor because of Bii, Q_Di etc
    //V_DG, Q_DG have been updated either in 'init' or in 'calcdynamic'

    if ctrl_mode = 0 then //pos seq control mode
    begin
        //Id
        // P and Q control
        // vi1, vi2 local gradient
        //  vi1, vi2 =0, local gradient calculated outside
        vi1 := 0;
        vi2 := 0; // local gradient calculated IN fMONITOR Node
        // ---if in curtailment P_ref has to be changed here-----
        if (DSS.FMonitorClass.bCurtl) and (FMonObj <> NIL) and (FMonObj.ld_fm_info[0].b_ctrl_hghst) then
        //if (DSS.FMonitorClass.bCurtl) then // this will cause oscillation
        begin
            Pref3 := V_DG * Id; //Here, P_ref will never go out of limits.
            //if cuitailment is needed, update P_ref here; then vi1 will be 0
            //check limits
            if Pref3 > Pmax then
            begin
                Pref3 := Pmax; //set real power change during the simulation
            end
            else
            if Pref3 < Pmin then
            begin
                Pref3 := Pmin;
            end;
            P_ref := Pref3 / 3.0;
        end;
        // --use vi1 to follow p_ref--

        DPx := fnphases * P_ref - P_DG;
        vi1 := 100 * kcd * DPx / V_DG; //pref control is 100 times faster than Curtailment
        //update V_in_var
        V_in_var[0] := vi1;
        V_in_var[1] := vi2;

    end
    else
    begin //phases control mode
        if fnphases = 3 then
        begin
            DPx := P_ref1 - P_DG1;
            vi1 := kcd * DPx / V_DG1;
            //Iq
            if QV_flag = 1 then
            begin
                if not cc_switch then
                begin
                    //droop
                    vi2 := kcq * (V_ref1 - V_DG1) //reactive V_ref control
                end
                else
                begin
                    //gradient
                    vi2 := Qmax_phase / V_DG1 * (-kcq * Gradient1);
                end;
                if ((Q_DG1 >= Qmax_phase) or (Q_DG1 <= Qmin_phase)) then // switch control mode to Q_ref control
                begin
                    // QV_flag := 0;
                    if (Q_DG1 >= Qmax_phase) then
                    begin
                        Q_ref1 := Qmax_phase; // set Q_ref
                        Q_ref2 := Qmax_phase; // set Q_ref
                        Q_ref3 := Qmax_phase; // set Q_ref
                    end
                    else
                    begin
                        Q_ref1 := Qmin_phase;
                        Q_ref2 := Qmin_phase;
                        Q_ref3 := Qmin_phase;
                    end;
                    vi2 := kqi * (Q_ref1 - Q_DG1); //reactive Q_ref control
                end
            end
            else
            begin
                vi2 := kqi * (Q_ref1 - Q_DG1); //reactive Q_ref control
            end;
            //update V_in_var
            V_in_var[0] := vi1;
            V_in_var[1] := vi2;

            DPx := P_ref2 - P_DG2;
            vi1 := kcd * DPx / V_DG2;
            //Iq
            if QV_flag = 1 then
            begin
                if not cc_switch then 
                begin
                    //droop
                    vi2 := kcq * (V_ref2 - V_DG2) //reactive V_ref control
                end
                else
                begin
                    //gradient
                    vi2 := Qmax_phase / V_DG2 * (-kcq * Gradient2);
                end;
                if ((Q_DG2 >= Qmax_phase) or (Q_DG2 <= Qmin_phase)) then // switch control mode to Q_ref control
                begin
                    if (Q_DG2 >= Qmax_phase) then
                    begin
                        Q_ref1 := Qmax_phase; // set Q_ref
                        Q_ref2 := Qmax_phase; // set Q_ref
                        Q_ref3 := Qmax_phase; // set Q_ref
                    end
                    else
                    begin
                        Q_ref1 := Qmin_phase;
                        Q_ref2 := Qmin_phase;
                        Q_ref3 := Qmin_phase;
                    end;
                    vi2 := kqi * (Q_ref2 - Q_DG2); //reactive Q_ref control
                end
            end
            else
            begin
                vi2 := kqi * (Q_ref2 - Q_DG2); //reactive Q_ref control
            end;
            //update V_in_var
            V_in_var[2] := vi1;
            V_in_var[3] := vi2;

            DPx := P_ref3 - P_DG3;
            vi1 := kcd * DPx / V_DG3;
            //Iq
            if QV_flag = 1 then
            begin
                if not cc_switch then
                    //droop
                    vi2 := kcq * (V_ref3 - V_DG3) //reactive V_ref control
                else
                    //gradient
                    vi2 := Qmax_phase / V_DG3 * (-kcq * Gradient3);
                if ((Q_DG3 >= Qmax_phase) or (Q_DG3 <= Qmin_phase)) then // switch control mode to Q_ref control
                begin
                    //QV_flag := 0;
                    if (Q_DG3 >= Qmax_phase) then
                    begin
                        Q_ref1 := Qmax_phase; // set Q_ref
                        Q_ref2 := Qmax_phase; // set Q_ref
                        Q_ref3 := Qmax_phase; // set Q_ref
                    end
                    else
                    begin
                        Q_ref1 := Qmin_phase;
                        Q_ref2 := Qmin_phase;
                        Q_ref3 := Qmin_phase;
                    end;
                    vi2 := kqi * (Q_ref3 - Q_DG3); //reactive Q_ref control
                end
            end
            else
                vi2 := kqi * (Q_ref3 - Q_DG3); //reactive Q_ref control
            //update V_in_var
            V_in_var[4] := vi1;
            V_in_var[5] := vi2;
        end
        else
        if fnphases = 1 then
        begin
            //choose ref
            case ctrl_mode of
                1:
                begin
                    temp_pref := P_ref1;
                    temp_qref := q_ref1;
                    temp_vref := v_ref1;
                end;
                2:
                begin
                    temp_pref := P_ref2;
                    temp_qref := q_ref2;
                    temp_vref := v_ref2;
                end;
                3:
                begin
                    temp_pref := P_ref3;
                    temp_qref := q_ref3;
                    temp_vref := v_ref3;
                end;
            end;

            DPx := temp_pref - P_DG1;
            vi1 := kcd * DPx / V_DG1;
            //Iq
            if QV_flag = 1 then
            begin
                if not cc_switch then //droop
                    vi2 := kcq * (temp_vref - V_DG) //reactive V_ref control
                else
                    vi2 := Qmax_phase / V_DG1 * (-kcq * Gradient1);
                if ((Q_DG1 >= Qmax_phase) or (Q_DG1 <= Qmin_phase)) then // switch control mode to Q_ref control
                begin
                    //QV_flag := 0;
                    if (Q_DG1 >= Qmax_phase) then
                        temp_qref := Qmax_phase // set Q_ref
                    else
                        temp_qref := Qmin_phase;

                    vi2 := kqi * (temp_qref - Q_DG); //reactive Q_ref control
                    //send Qref back
                end
            end
            else
                vi2 := kqi * (temp_qref - Q_DG); //reactive Q_ref control
            //update V_in_var
            V_in_var[0] := vi1;
            V_in_var[1] := vi2;
        end;

    end;
    // --------------------------------
    // cooperate part is done here
    //pVinput[j];
    update_pV_f_CC(); //update pV_f_CC which is cooperate control
    // --------------------------------
    //implement cooperate control
    for j := 0 to NumOrderX - 1 do
        V_in_var[j] += pV_f_CC[j];
end;

procedure TGeneric5Obj.update_pV_f_CC_M2(); //for power flow
var
    j: Integer;
    // num_vleader: Integer;
    Bii: Double;
begin
    if not cc_switch then
    begin
        for j := 0 to NumOrderX - 1 do
            pV_f_CC[j] := 0.0;
        exit;
    end;

    if FMonObj = nil then
        Exit;

    //avg ctrl, under V120, I120

    // num_vleader := 1;
    if ctrl_mode = 0 then
    begin
        //u = gradient + pV_f_CC; pV_f_CC = -alpha + sum(alpha_j)
        Bii := ActiveCircuit.Solution.Bii(NodeRef[1]);
        // Q ctrl with v_ref
        // pV_f_CC[1] := FMonObj.Calc_Alpha_M2(ndNumincluster,0,NodeRef[1],Bii,kcq,Volt_Trhd); // for dIddt, diqdt
        // Q ctrl with loss
        // pV_f_CC[1] := FMonObj.Calc_Alpha_L(ndNumincluster,0,NodeRef[1],Bii,kcq,Volt_Trhd);
        pV_f_CC[1] := FMonObj.Calc_Alpha_LnM2(ndNumincluster, 0, NodeRef[1], Bii, kcq, Volt_Trhd);
        // pV_f_CC[1] := alpha * Qmax / v ;
        //P ctrl
        // pV_f_CC[0] := FMonObj.Calc_AlphaP(ndNumincluster,0); // for dIddt, diqdt
        pV_f_CC[0] := 0;
    end
    else
    begin
        // phases control
        if fnphases = 3 then
        begin
            pV_f_CC[5] := 0.0;
            //u = gradient + pV_f_CC; pV_f_CC = -alpha + sum(alpha_j)
            Bii := ActiveCircuit.Solution.Bii(NodeRef[1]);
            pV_f_CC[1] := FMonObj.Calc_Alpha_M2(ndNumincluster, 1, NodeRef[1], Bii, kcq, Volt_Trhd);
            pV_f_CC[0] := FMonObj.Calc_AlphaP(ndNumincluster, 1);
            Bii := ActiveCircuit.Solution.Bii(NodeRef[2]);
            pV_f_CC[3] := FMonObj.Calc_Alpha_M2(ndNumincluster, 2, NodeRef[2], Bii, kcq, Volt_Trhd);
            pV_f_CC[2] := FMonObj.Calc_AlphaP(ndNumincluster, 2);
            Bii := ActiveCircuit.Solution.Bii(NodeRef[3]);
            pV_f_CC[5] := FMonObj.Calc_Alpha_M2(ndNumincluster, 3, NodeRef[3], Bii, kcq, Volt_Trhd);
            pV_f_CC[4] := FMonObj.Calc_AlphaP(ndNumincluster, 3);
            //pV_f_CC[1-6]； // for dIddt1, diqdt1,dIddt2, diqdt2,dIddt3, diqdt3
        end
        else
        if fnphases = 1 then
        begin
            //if ctrl_mode=1 then
            Bii := ActiveCircuit.Solution.Bii(NodeRef[1]);
            pV_f_CC[1] := FMonObj.Calc_Alpha_M2(ndNumincluster, ctrl_mode, NodeRef[1], Bii, kcq, Volt_Trhd);
            // for dIddt1, diqdt1
            pV_f_CC[0] := FMonObj.Calc_AlphaP(ndNumincluster, ctrl_mode);
        end;
    end;
end;

procedure TGeneric5Obj.update_pV_f_CC(); //used in dynamic mode to update alpha
var
    p_mode,
    j: Integer;
    // num_vleader: Integer;
    Bii,
    us_i, ul_i: Double;
begin
    if not cc_switch then
    begin
        //no control at all
        //local gradient control will be set by communication matrix
        for j := 0 to NumOrderX - 1 do
            pV_f_CC[j] := 0.0;
        exit;
    end;

    if FMonObj = nil then
        Exit;

    // num_vleader := 1;
    if ctrl_mode <> 0 then
        Exit;

    //avg ctrl, under V120, I120
    p_mode := 0;
    if FMonObj <> nil then
        p_mode := FMonObj.p_mode;
    
    //u = gradient + pV_f_CC; pV_f_CC = -alpha + sum(alpha_j)
    
    Bii := ActiveCircuit.Solution.Bii(NodeRef[1]);

    if ActiveCircuit.Solution.DynaVars.SolutionMode = TSolveMode.DYNAMICMODE then
    begin
        //Ip control
        if FMonObj.ld_fm_info[0].b_Curt_Ctrl then // curtailment algorithm
        begin
            ul_i := FMonObj.Calc_ul_P(ndNumincluster, 0);
            us_i := kcd * FMonObj.Calc_Gradient_ct_P(ndNumincluster, 0);
            GradientP := us_i;
            if not cc_switch then //local
                pV_f_CC[0] := 0.0
            else
            begin
                pV_f_CC[0] := ul_i + us_i;
                pV_f_CC[0] := pV_f_CC[0] * Pmax / v_DG;
            end;
        end;

        if (p_mode = 1) and (cc_switch) then //if delta P = p_trans_ref - p_trans
        begin //balance p_trans
            pV_f_CC[0] := FMonObj.Calc_AlphaP(ndNumincluster, 0);//new alfa_p
            pV_f_CC[0] := pV_f_CC[0] - AlphaP; //derivative of alfa_p
            //use us_i to calculate the frequncy
            us_i := -FMonObj.omg_fm; //frequency droop
            pV_f_CC[0] := (pV_f_CC[0] + us_i) * Pmax / v_DG; // derivative of Ip in dynamic mode,
            //use us_i to
        end;

        //Iq control
        ul_i := FMonObj.Calc_fm_ul_0(ndNumincluster, 0, NodeRef[1], Bii, kcq, Volt_Trhd);
        us_i := FMonObj.Calc_fm_us_0(ndNumincluster, 0, NodeRef[1], Bii, kcq, Volt_Trhd);
        Gradient := us_i;

        if FMonObj.ld_fm_info[0].b_Curt_Ctrl then
        begin 
            // if curtailment for this cluster is on
            //Q will try to boost the voltage while P is decreasing
            if (DSS.FMonitorClass.bCurtl) and (Gradient = 0.0) then
                us_i := -GradientP * Pmax / Qmax;
        end;

        if not cc_switch then //local
        begin
            pV_f_CC[1] := us_i;
        end
        else
        begin // cc_switch is on
            pV_f_CC[1] := ul_i + us_i; //cc //attack comes in ul_i (FMonObj.Calc_fm_ul_0)
        end;
        pV_f_CC[1] *= Qmax / v_DG;

    end
    else 
    //power flow
    begin
        //alphaP: p ratio
        if (p_mode = 1) or (FMonObj.ld_fm_info[0].b_Curt_Ctrl) then
            pV_f_CC[0] := FMonObj.Calc_AlphaP(ndNumincluster, 0)
        else
            pV_f_CC[0] := 0.0;
        
        //alpha : q ratio
        pV_f_CC[1] := FMonObj.Calc_Alpha_M2(ndNumincluster, 0, NodeRef[1], Bii, kcq, Volt_Trhd); // for dIddt, diqdt
    end;
end;

procedure TGeneric5Obj.InfoPublish();
begin
    Update_PQLimits();
    if FMonObj = nil then
        Exit;

    with FMonObj.nodeFMs[NdNuminCluster] do
    begin
        case ctrl_mode of
            1:
            begin
                vl_V1 := V_DG1;//Phase A or the first phase if there are less than 3 phases
                vl_P_DG1 := P_DG1;
                vl_Q_DG1 := Q_DG1;
                vl_V_ref1_dg := V_ref1;
            end;
            2:
            begin
                vl_V2 := V_DG2;//Phase B if exists
                vl_P_DG2 := P_DG2;
                vl_Q_DG2 := Q_DG2;
                vl_V_ref2_dg := V_ref2;
            end;
            3:
            begin
                vl_V3 := V_DG3;//Phase c if exists
                vl_P_DG3 := P_DG3;
                vl_Q_DG3 := Q_DG3;
                vl_V_ref3_dg := V_ref3;

            end;
            0:
            begin
                vl_V := V_DG; //0 seq    , will be used in FMonObj.Agnt_smpl
                // vl_P_DG := P_DG;
                vl_Q_DG := Q_DG;
                alpha := Q_DG / Qmax;
                vl_alpha_dg := alpha; // update first, will be used in FMonObj.Agnt_smpl
                //P control
                alphap := P_DG / Pmax;
                vl_alphaP_dg := alphaP;

                vl_V_ref_dg := V_ref;
                if ActiveCircuit.Solution.DynaVars.SolutionMode = TSolveMode.DYNAMICMODE then
                begin
                    z_dfs_plot := z_dfs; // defense value
                end;
            end;
            4:
            begin
                vl_V1 := V_DG1;//Phase A or the first phase if there are less than 3 phases
                vl_P_DG1 := P_DG1;
                vl_Q_DG1 := Q_DG1;
                vl_V2 := V_DG2;//Phase B if exists
                vl_P_DG2 := P_DG2;
                vl_Q_DG2 := Q_DG2;
                vl_V3 := V_DG3;//Phase c if exists
                vl_P_DG3 := P_DG3;
                vl_Q_DG3 := Q_DG3;
                vl_V_ref1_dg := V_ref1;
                vl_V_ref2_dg := V_ref2;
                vl_V_ref3_dg := V_ref3;
            end;
        end;
        vl_Qmax_dg := Qmax;
        vl_Qmax_phase_dg := Qmax_Phase;
        vl_Pmax_dg := Pmax;
        vl_Pmax_phase_dg := Pmax_Phase;
        // vl_CC_switch_dg := CC_switch;
        // vl_QV_flag_dg := QV_flag;
        vl_kcd_dg := kcd;
        vl_kcq_dg := kcq;
        // vl_volt_thrd_dg := Volt_Trhd;
    end;
end;

procedure TGeneric5Obj.Update_PQLimits();
begin
    if PQpriority = 1 then //P prior by default
    begin
        // TODO: BUG: the original comparison probably compares W and kW
        // if (Pmax <= 0) or (Pmax > Wbase) then
        //     Pmax := WBase;// first value is set to be kWbase;   when kWbase is set, Pmax will be update in edit;
        
        // TODO: BUG: why no conditional?
        Pmax := WBase;//if PQpriority=1 then
        if (1000 * kVArating) >= P_DG then //  Pmax P_DG
            Qmax := sqrt(kVArating * 1000 * kVArating * 1000 - P_DG * P_DG) // PMax*PMax)//
        else
            Qmax := epsilon; //error when used as demoninator
        Qmin := -Qmax;
    end
    else
    if PQpriority = 0 then //Q prior
    begin
        Qmax := kVArating;
        Qmin := -kVArating;

        Pmax := min(sqrt(kVArating * 1000 * kVArating * 1000 - Q_DG * Q_DG), Wbase); //which one is smaller
        Pmin := 0;
    end;

    Pmax_phase := Pmax / fnphases;
    Pmin_phase := Pmin / fnphases;
    Qmax_phase := Qmax / fnphases;
    Qmin_phase := Qmin / fnphases;
    //used for limit currents derivative
    // Idmax_phase := Pmax_phase / (Vbase);// vBase := kVGeneratorBase*InvSQRT31000
    // Iqmax_phase := Qmax_phase / (Vbase);
end;

procedure TGeneric5Obj.CalcDynamic(var V012, I012: TSymCompArray);
var
    temp: Double;
begin
    if ctrl_mode <> 0 then
        Exit;

    // InDynamics := true;
    V1 := V012[1]; // Save for variable calcs
    V2 := V012[2];

    V_DG := cabs(V1);
    Theta_DG := cang(V1);

    // P -- P_DG follows ref, and allows sudden change

    P_DG := V_DG * Id; //update P_DG
    // if P_DG, Q_DG exceed the limits
    if P_DG > Pmax then
    begin
        P_DG := Pmax; //set real power change during the simulation
        Id := P_DG / V_DG; //set Id
        Idn := Id;
        X_var[0] := Id;
        X_varn[0] := Idn;
        dX_vardtn[0] := 0.0;
    end
    else
    if P_DG < Pmin then
    begin
        P_DG := Pmin;
        Id := P_DG / V_DG; //set Id
        Idn := Id;
        X_var[0] := Id;
        X_varn[0] := Idn;
        dX_vardtn[0] := 0.0;
    end;

    // Q
    Q_DG := V_DG * Iq;

    if Q_DG >= Qmax then
    begin
        Q_DG := Qmax;
        Iq := Q_DG / V_DG;
        Iqn := Iq;
        X_var[1] := Iq;
        X_varn[1] := Iqn;
        dX_vardtn[0] := 0.0;
    end
    else
    if Q_DG <= Qmin then
    begin
        Q_DG := Qmin;
        Iq := Q_DG / V_DG;
        Iqn := Iq;
        X_var[1] := Iq;
        X_varn[1] := Iqn;
        dX_vardtn[0] := 0.0;
    end;
    
    if Id = 0.0 then
        temp := pi / 2
    else
        temp := arctan(Iq / Id);
    Is1 := PCLX(sqrt(Iq * Iq + Id * Id), Theta_DG - temp) / 3.0 ;//with respect to Q_axis
    Is2 := 0; //force balance
    // rotor current  Ir1= Is1-Vm/jXm
    Ir1 := Is1;
    Ir2 := 0;

    // Iq Iq does not change, Is1 := cmplx(Id, Iq)*1<angle Is2 := 0
    // Id and Iq are divided by/3.0 to be I012
    // Is2 is calculated here(In XY domain), will be used as I012
    // sqrt(Iq*Iq +Id*Id),Theta_DG - arctan(Iq/Id)

    AlphaP := P_DG / Pmax;
    Alpha := Q_DG / Qmax;

    I012[1] := -Is1; // Id and Iq /3.0
    I012[2] := -Is2;
    I012[0] := 0; //force balance
end;

procedure TGeneric5Obj.CalcDynamicVIabc(var Vabc, Iabc: pComplexArray);
//   Vabc is the terminal voltages of the connecting bus
//   Iabc will be returned as the currents injection into network
//   This func will be called after integrate, so 'Id1, Iq1,  Id2, Iq2,  Id3, Iq3' have been integrated for current time step
//   ,whcih means at the end of integration 'Id1, Iq1,  Id2, Iq2,  Id3, Iq3' shoulbe be valued
//   ' P_DG1, P_DG2,P_dg3, Q_DG1, Q_dg2,Q_dg3' will also be calculated in this func
//    'V_DG1, V_DG2,V_dg3'
var
    tempV1, tempV2, tempV3,
    Curr1,
    Curr2,
    Curr3: Complex;
    tempAngleR: Double;
begin
    if ctrl_mode = 0 then // avg ctrl
    begin
        //will never be used
        //will be in CalcDynamic
        Exit;
    end;

    //direct phase ctrl
    if fnphases = 3 then
    begin
        //3-phase ctrl
        // InDynamics := true;
        tempV1 := Vabc[1]; // Save for variable calcs
        tempV2 := Vabc[2];
        tempV3 := Vabc[3];

        V_DG1 := cabs(tempV1);
        V_DG2 := cabs(tempV2);
        V_DG3 := cabs(tempV3);
        V_Theta1 := cang(tempV1);
        V_Theta2 := cang(tempV2);
        V_Theta3 := cang(tempV3);
        ///
        ///  Model currents Iabc injectted into network by Id1, Iq1, Id2, Iq2, Id3, Iq3, Vabc/////
        //Id1, Iq1
        if Id1 = 0.0 then
            tempAngleR := pi / 2
        else
            tempAngleR := arctan(Iq1 / Id1);
        curr1 := PCLX(sqrt(Iq1 * Iq1 + Id1 * Id1), V_Theta1 - tempAngleR);//with respect to Q_axis
        //Id2, Iq2
        if Id2 = 0.0 then
            tempAngleR := pi / 2
        else
            tempAngleR := arctan(Iq2 / Id2);
        curr2 := PCLX(sqrt(Iq2 * Iq2 + Id2 * Id2), V_Theta2 - tempAngleR);//with respect to Q_axis
        //Id3, Iq3
        if Id3 = 0.0 then
            tempAngleR := pi / 2
        else
            tempAngleR := arctan(Iq3 / Id3);
        curr3 := PCLX(sqrt(Iq3 * Iq3 + Id3 * Id3), V_Theta3 - tempAngleR);//with respect to Q_axis
        //////////////////////////////////////////////////////
        ///Update power at current time step
        P_DG1 := V_DG1 * Id1;
        Q_DG1 := V_DG1 * Iq1;
        P_DG2 := V_DG2 * Id2;
        Q_DG2 := V_DG2 * Iq2;
        P_DG3 := V_DG3 * Id3;
        Q_DG3 := V_DG3 * Iq3;
        //sum
        P_DG := P_DG1 + P_DG2 + P_DG3; //element output
        Q_DG := Q_DG1 + Q_DG2 + Q_DG3;
        ////////////////////////////////
        // inject into network
        Iabc[1] := -Curr1;
        Iabc[2] := -Curr2;
        Iabc[3] := -Curr3;
        Exit;
    end;
    
    if fnphases = 1 then
    begin
        //1-phase ctrl
        // InDynamics := true;
        tempV1 := Vabc[1]; // Save for variable calcs

        V_DG1 := cabs(tempV1);
        V_Theta1 := cang(tempV1);
        ///
        ///  Model currents Iabc injectted into network by Id1, Iq1, Id2, Iq2, Id3, Iq3, Vabc/////
        //Id1, Iq1
        if Id1 = 0.0 then
            tempAngleR := pi / 2
        else
            tempAngleR := arctan(Iq1 / Id1);
        curr1 := PCLX(sqrt(Iq1 * Iq1 + Id1 * Id1), V_Theta1 - tempAngleR);//with respect to Q_axis

        //////////////////////////////////////////////////////
        ///Update power at current time step
        P_DG1 := V_DG1 * Id1;
        Q_DG1 := V_DG1 * Iq1;
        //sum
        P_DG := P_DG1;//
        Q_DG := Q_DG1;
        ////////////////////////////////
        // inject into network
        Iabc[1] := -Curr1;
        Exit;
    end;

    //no consideration for 2-phase DG
end;

procedure TGeneric5Obj.CalcPFlow(var V012, I012: TSymCompArray);
var
    Curr: Complex;
    p_mode: Integer;
begin
    if ctrl_mode <> 0 then
        Exit;
    
    //duplicate all codes as avg ctrl, under V120, I120

    V1 := V012[1]; // Save for variable calcs
    V2 := V012[2];
    if cabs(V1) = 0.0 then
        V1 := 1; //in Case the first step
    // InDynamics := false;
    // Guess at a new var output value
    V_DG := cabs(V1);
    Theta_DG := cang(V1);
    //this should be the the online system index, has to be improved by

    // ----real power is control by Pref in DG----
    update_pV_f_CC(); //AlphaP, Alpha
    p_mode := 0;
    if FMonObj <> nil then
        p_mode := FMonObj.p_mode;
    if (p_mode = 1) and (cc_switch) then //if delta P = p_trans_ref - p_trans
    begin //balance p_trans
        AlphaP := pV_f_CC[0]; //alpha_p
        P_DG := Pmax * AlphaP;
    end
    else
    begin
        P_DG := fnphases * P_ref; // local

    end;
    if (P_DG > Pmax) then
    begin
        P_DG := Pmax;
    end
    else
    if (P_DG < Pmin) then
    begin
        P_DG := Pmin;
    end;
    AlphaP := P_DG / Pmax;
    // --- real power is controled above --
    if QV_flag = 0 then //P_ref, Q_ref
        Curr := cong(Cmplx(P_DG / 3.0, Q_ref) / V1)
    else
    //P_ref, V_ref
    begin
        if ActiveCircuit.Solution.Iteration = 1 then
        begin
            Iq := 0; //In power flow, start value of Iq for each power flow
        end;
        Alpha := pV_f_CC[1]; // only when not dynamode
        Q_DG := Qmax * Alpha;
        Curr := cong(Cmplx(P_DG / 3.0, Q_DG / 3.0) / V1);
    end;

    I012[1] := -Curr; // Save for variable calcs
    I012[2] := 0;//force to be balanced output DG
    I012[0] := 0;
end;

procedure TGeneric5Obj.CalcPFlowVIabc(var Vabc, Iabc: pComplexArray);
var
    tempV1, tempV2, tempV3,
    Curr1,
    Curr2,
    Curr3: Complex;
    temp_pref, temp_qref, temp_vref, temp_alpha: Double;
    // flmt: Double;
    p_mode: Integer;
begin
    temp_qref := 0.0;
    temp_pref := 0.0;
    temp_alpha := 0.0;
    // flmt := 0.9;
    Update_PQLimits(); //  Pmax_phase, Qmax_phase will be used in the following steps
    update_pV_f_CC_M2();  
    // pV_f_CC, updated from virtual leader
    // Q ctrl: 3-phase,  pV_f_CC[1], [4], [6]
    // 1-phase,  pV_f_CC[1]
    // P ctrl: 3-phase,  pV_f_CC[0], [3], [5]
    // 1-phase,  pV_f_CC[0]

    if fnphases = 3 then
    begin
        tempV1 := Vabc[1]; // Save for variable calcs //assume Vabc[1][2][3] is ABC!
        tempV2 := Vabc[2];
        tempV3 := Vabc[3];
        if cabs(tempV1) = 0 then
            tempV1 := 1;
        if cabs(tempV2) = 0 then
            tempV2 := 1;
        if cabs(tempV3) = 0 then
            tempV3 := 1;
    end
    else
    if fnphases = 1 then
    begin
        tempV1 := Vabc[1]; // Save for variable calcs //assume Vabc[1][2][3] is ABC!
        tempV2 := 1;
        tempV3 := 1;
    end;

    V_DG1 := cabs(tempV1); // Save for variable calcs
    V_DG2 := cabs(tempV2);
    V_DG3 := cabs(tempV3);
    // ----real power is control by Pref in DG----
    P_DG1 := P_ref1;
    P_DG2 := P_ref2;
    P_DG3 := P_ref3;

    //alpha is implemented in M2
    p_mode := 0;
    if FMonObj <> nil then
        p_mode := FMonObj.p_mode;

    if (p_mode = 1) and cc_switch then //if delta P = p_trans_ref - p_trans
    begin
        case ctrl_mode of
            1:
            begin
                AlphaP1 := pV_f_CC[0];
                p_DG1 := p_DG1 + Pmax_phase * AlphaP1;
            end;
            2:
            begin
                AlphaP2 := pV_f_CC[0];//if single phase only pV_f_CC[0] and pV_f_CC[1]
                p_DG2 := p_DG2 + Pmax_phase * AlphaP2;
            end;
            3:
            begin
                AlphaP3 := pV_f_CC[0]; //if single phase only pV_f_CC[0] and pV_f_CC[1]
                p_DG3 := p_DG3 + Pmax_phase * AlphaP3;
            end;
            4:
            begin
                AlphaP1 := pV_f_CC[0];
                p_DG1 := p_DG1 + Pmax_phase * AlphaP1;
                AlphaP2 := pV_f_CC[2];
                p_DG2 := p_DG2 + Pmax_phase * AlphaP2;
                AlphaP3 := pV_f_CC[4];
                p_DG3 := p_DG3 + Pmax_phase * AlphaP3;
            end;
        end;
        if (p_DG1 > Pmax_phase) then
        begin
            P_DG1 := Pmax_phase;
        end
        else
        if (P_DG1 < Pmin_phase) then
        begin
            P_DG1 := Pmin_phase;
        end;
        if (P_DG2 > Pmax_phase) then
        begin
            P_DG2 := Pmax_phase;
        end
        else
        if (P_DG2 < Pmin_phase) then
        begin
            P_DG2 := Pmin_phase;
        end;
        if (P_DG3 > Pmax_phase) then
        begin
            P_DG3 := Pmax_phase;
        end
        else
        if (P_DG3 < Pmin_phase) then
        begin
            P_DG3 := Pmin_phase;
        end;
        Update_PQLimits(); //  Qmax_phase will be updated accordingly
    end;
    // calc P_DG
    case ctrl_mode of
        1:
        begin
            P_DG := P_dg1;
        end;
        2:
        begin
            P_DG := P_dg2;
        end;
        3:
        begin
            P_DG := P_dg3;
        end;
        4:
        begin
            P_DG := P_DG1 + P_DG2 + P_DG3;
        end;
    end;

    // Q Control

    if ctrl_mode = 0 then
    begin
        //will never be used
        //will be in CalcPFlow
        Exit;
    end; // avg ctrl
    
    //direct phase ctrl
    if fnphases = 3 then
    begin
        //3-phase ctrl
        // InDynamics := false;

        //if (P_Mode = 1) and  then
        //real power control

        // Guess at a new var output value
        if QV_flag = 0 then //P_ref, Q_ref
        begin
            Curr1 := cong(Cmplx(P_dg1, Q_ref1) / tempV1); //currents A,B,C
            Curr2 := cong(Cmplx(P_dg2, Q_ref2) / tempV2);
            Curr3 := cong(Cmplx(P_dg3, Q_ref3) / tempV3);
        end
        else //P_ref, V_ref
        begin
            //phase A
            //1st iteration Iq := 0;
            if ActiveCircuit.Solution.Iteration = 1 then
            begin
                Iq1 := 0; //In power flow, start value of Iq for each power flow
                Iq2 := 0;
                Iq3 := 0;
            end; //should be taken care of here
            if not cc_switch then
            begin
                //droop
                //Q_DG starts from 0
                ///////////integral droop
                dIqdt := kcq * (V_ref1 - V_DG1) / ActiveCircuit.Solution.Iteration;
                if abs(V_ref1 - v_DG1) <= Volt_Trhd * V_ref1 then
                    dIqdt := 0.0;

                Iq1 := Iq1 + dIqdt;
                Q_DG1 := V_DG1 * Iq1;
                if droop = 2 then

                    Q_DG1 := kcq_drp2 * (V_ref1 - V_DG1) * 1000 * kVArating / 0.05 / V_ref1;
            end
            // gradient control
            else
            begin
                // cooperative control
                Alpha1 := pV_f_CC[1];
                Q_DG1 := Qmax_phase * Alpha1;
            end;
            // ----------------
            //phase B
            if not cc_switch then //droop
            begin
                ///////////integral droop
                dIqdt := kcq * (V_ref2 - V_DG2) / ActiveCircuit.Solution.Iteration; // ref control
                if abs(V_ref2 - v_DG2) <= Volt_Trhd * V_ref2 then
                    dIqdt := 0.0;
                Iq2 := Iq2 + dIqdt; //In power flow, Iq starts from 0;
                Q_DG2 := V_DG2 * Iq2;
                if droop = 2 then
                    Q_DG2 := kcq_drp2 * (V_ref2 - V_DG2) * 1000 * kVArating / 0.05 / V_ref1;
            end
            // gradient control
            else
            begin
                Alpha2 := pV_f_CC[3];
                Q_DG2 := Qmax_phase * Alpha2;
            end;
            // ----------------
            //phase C
            if not cc_switch then //droop
            begin
                ///////////integral droop
                dIqdt := kcq * (V_ref3 - V_DG3) / ActiveCircuit.Solution.Iteration;
                if abs(V_ref3 - v_DG3) <= Volt_Trhd * V_ref3 then
                    dIqdt := 0.0;
                Iq3 := Iq3 + dIqdt; //In power flow, Iq starts from 0;
                Q_DG3 := V_DG3 * Iq3;
                if droop = 2 then
                    Q_DG3 := kcq_drp2 * (V_ref3 - V_DG3) * 1000 * kVArating / 0.05 / V_ref1;
            end
            else
            begin
                // gradient control
                Alpha3 := pV_f_CC[5];
                Q_DG3 := Qmax_phase * Alpha3;
            end;

            /// code bellow is for each phase working seperately
            if (Q_DG1 > Qmax_phase) then
            begin
                Q_DG1 := Qmax_phase;
            end
            else
            if (Q_DG1 < Qmin_phase) then
            begin
                Q_DG1 := Qmin_phase;
            end;
            Curr1 := cong(Cmplx(P_dg1, Q_DG1) / tempV1);
            if (Q_DG2 > Qmax_phase) then
            begin
                Q_DG2 := Qmax_phase;
            end
            else
            if (Q_DG2 < Qmin_phase) then
            begin
                Q_DG2 := Qmin_phase;
            end;
            Curr2 := cong(Cmplx(P_dg2, Q_DG2) / tempV2);
            if (Q_DG3 > Qmax_phase) then
            begin
                Q_DG3 := Qmax_phase;
            end
            else
            if (Q_DG3 < Qmin_phase) then
            begin
                Q_DG3 := Qmin_phase;
            end;
            Curr3 := cong(Cmplx(P_dg3, Q_DG3) / tempV3);
        end;
        Q_DG := Q_DG1 + Q_DG2 + Q_DG3;

        Iabc[1] := -Curr1; // Save for variable calcs
        Iabc[2] := -Curr2;
        Iabc[3] := -Curr3;
    end
    else
    if fnphases = 1 then
    begin
        //1-phase ctrl

        //tempV1 := Vabc[1]; // Save for variable calcs //assume Vabc[1][2][3] is ABC!

        V_DG2 := V_DG1; // Save for variable calcs, just in case of other use
        V_DG3 := V_DG1;

        // InDynamics := false;
        // Guess at a new var output value
        case ctrl_mode of
            1:
            begin
                temp_pref := P_dg1;
                temp_qref := q_ref1;
                temp_vref := v_ref1;
                Alpha1 := pV_f_CC[1]; //1 phase, only first one. coincident with dynamic calc
                temp_alpha := alpha1;
            end;
            2:
            begin
                temp_pref := P_dg2;
                temp_qref := q_ref2;
                temp_vref := v_ref2;
                Alpha2 := pV_f_CC[1];
                temp_alpha := alpha2;
            end;
            3:
            begin
                temp_pref := P_dg3;
                temp_qref := q_ref3;
                temp_vref := v_ref3;
                Alpha3 := pV_f_CC[1];
                temp_alpha := alpha3;
            end;
        end;
        if QV_flag = 0 then //P_ref, Q_ref
        begin
            Curr1 := cong(Cmplx(temp_pref, temp_qref) / tempV1); //currents A,B,C
        end
        else //P_ref, V_ref
        begin // QV_flag=1
            //phase 1
            //1st ireration Iq := 0;
            if ActiveCircuit.Solution.Iteration = 1 then
            begin
                Iq1 := 0; //In power flow, start value of Iq for each power flow
            end;
            if not cc_switch then //droop
            begin
                ///////////integral droop
                dIqdt := kcq * (temp_vref - V_DG1) / ActiveCircuit.Solution.Iteration;
                if abs(V_ref1 - v_DG1) <= Volt_Trhd * V_ref1 then
                    dIqdt := 0.0;
                Iq1 := Iq1 + dIqdt; //In power flow, Iq starts from 0;
                temp_qref := V_DG1 * Iq1;
                if droop = 2 then
                    temp_qref := kcq_drp2 * (temp_vref - V_DG1) * 1000 * kVArating / 0.05 / V_ref1;
            end
            else
            begin
                // gradient control
                temp_qref := Qmax_phase * temp_alpha;
            end;
            // ----------------

            if (temp_qref > Qmax_phase) then // switch control mode to Q_ref control
                temp_qref := Qmax_phase
            else
            if (temp_qref < Qmin_phase) then
                temp_qref := Qmin_phase;
            Curr1 := cong(Cmplx(temp_pref, temp_qref) / tempV1);
            case ctrl_mode of
                1:
                begin
                    Q_DG1 := temp_qref;
                    alpha1 := temp_alpha;
                end;
                2:
                begin
                    Q_DG2 := temp_qref;
                    alpha2 := temp_alpha;
                end;
                3:
                begin
                    Q_DG3 := temp_qref;
                    alpha3 := temp_alpha;
                end;
            end;
        //no consideration for 2-phase DG
        end;
        Iabc[1] := -Curr1; // Save for variable calcs
    end; //phase =1
    //direct phase ctrl
end;

procedure TGeneric5Obj.InitStateVars();
var
    i: Integer;
    V012,
    I012: TSymCompArray;
    Vabc: array[1..3] of Complex;
    cBuffer: pComplexArray;
begin
    YPrimInvalid := true; // Force rebuild of YPrims

    // Compute nominal Positive sequence voltage behind transient reactance

    // if MachineON then
    // begin
        Yeq := Cinv(Zsp);
        ComputeIterminal();
        case Fnphases of
            1:
            begin
                for i := 1 to FNphases do
                    Vabc[i] := ActiveCircuit.Solution.NodeV[NodeRef[i]] // Wye Voltage
            end;
            3:
            begin
                // Calculate E1 based on Pos Seq only
                Phase2SymComp(ITerminal, pComplexArray(@I012));   // terminal currents

                // Voltage behind Zsp  (transient reactance), volts
                for i := 1 to FNphases do
                    Vabc[i] := ActiveCircuit.Solution.NodeV[NodeRef[i]]; // Wye Voltage
                Phase2SymComp(pComplexArray(@Vabc), pComplexArray(@V012));
            end;
        else
            DoSimpleMsg('Dynamics mode is implemented only for 1- or 3-phase Motors. %s has %d phases.', [FullName(), Fnphases], 5672);
            DSS.SetSolutionAbort(true);
        end;
    // end;

    /// from here, let us deal with ctrl_mode and everything  related to control

    if ctrl_mode = 0 then //Pos seq contrl
    begin
        V_DG := Cabs(V012[1]);// Pos Seq Control
        Theta_DG := Cang(V012[1]);
        P_DG := 0 - Power(1).re;
        Q_DG := 0 - Power(1).im;
        P_ref := P_DG / 3;
        Q_ref := Q_DG / 3;

        // Previously in InitModel

        //duplicate all codes as avg ctrl
        Id := P_DG / V_DG; //make sure V_DG has been calc beforehand
        Iq := Q_DG / V_DG;
        Idn := Id;
        Iqn := Iq;
        Id_ref := Id;// local; may need to be changed in futher
        Iq_ref := Iq;//
        //  P_ref :=  Id_ref *v_DG;//local
        //  V_ref := v_DG;//local
        // -initiate ABCD XY-
        X_var[0] := Id;
        X_var[1] := Iq;

        dIqdt := 0;
    end
    else
    begin //ctrl_mode <> 0   =1,2,3,4
        //Vabc
        V_DG1 := cabs(Vabc[1]); // Save for variable calcs
        V_DG2 := cabs(Vabc[2]);
        V_DG3 := cabs(Vabc[3]);
        cBuffer := Allocmem(sizeof(Complex) * fnPhases);//define cBuffer
        GetPhasePower(cBuffer);

        P_DG1 := 0.0 - cBuffer[1].re; //first phase or the only one
        Q_DG1 := 0.0 - cBuffer[1].im;
        Id1 := P_DG1 / V_DG1;
        Iq1 := Q_DG1 / V_DG1;
        // initiate ABCD XY
        X_var[0] := Id1;
        X_var[1] := Iq1;
        if fnphases = 3 then //for 3 phase control the bellow is needed
        begin
            P_DG2 := 0.0 - cBuffer[2].re;
            Q_DG2 := 0.0 - cBuffer[2].im;
            P_DG3 := 0.0 - cBuffer[3].re;
            Q_DG3 := 0.0 - cBuffer[3].im;
            Id2 := P_DG2 / V_DG2;
            Iq2 := Q_DG2 / V_DG2;
            Id3 := P_DG3 / V_DG3;
            Iq3 := Q_DG3 / V_DG3;
            X_var[2] := Id2;
            X_var[3] := Iq2;
            X_var[4] := Id3;
            X_var[5] := Iq3;
        end;
        Reallocmem(cBuffer, 0);//free cBuffer
    end;
    Update_PQLimits();
end;

procedure TGeneric5Obj.CalcYPrimMatrix(Ymatrix: TcMatrix);
var
    Y, Yij, Yadder: Complex;
    i, j: Integer;
    FreqMultiplier: Double;
begin
    FYprimFreq := ActiveCircuit.Solution.Frequency();
    FreqMultiplier := FYprimFreq / BaseFrequency; // ratio to adjust reactances for present solution frequency

    if ActiveCircuit.Solution.IsDynamicModel or ActiveCircuit.Solution.IsHarmonicModel then
    // for Dynamics and Harmonics modes use constant equivalent Y
    begin
        // if MachineON then
        Y := Yeq; // L-N value computed in initial condition routines
        // else
        // Y := Cmplx(EPSILON, 0.0);

        if Connection = TGeneralConnection.Delta then
            Y := Y / 3.0; // Convert to delta impedance
        Y.im := Y.im / FreqMultiplier; // adjust for frequency
        Yij := -Y;
        for i := 1 to Fnphases do
        begin
            case Connection of
                TGeneralConnection.Wye:
                begin
                    Ymatrix.SetElement(i, i, Y); // sets the element
                end;
                TGeneralConnection.Delta:
                begin // Delta connection
                    Yadder := Y * 1.000001; // to prevent floating delta
                    Ymatrix.SetElement(i, i, Y + Yadder); // add a little bit to diagonal
                    Ymatrix.AddElement(i, i, Y); // put it in again
                    for j := 1 to i - 1 do
                        Ymatrix.SetElemsym(i, j, Yij);
                end;
            end;
        end;
        Exit;
    end;

    //  Typical code for a regular power flow  model
    //  Borrowed from Generator object

    // Yeq is typically expected as the equivalent line-neutral admittance

    Y := Yeq; // Yeq is L-N quantity

    // ****** Need to modify the base admittance for real harmonics calcs
    Y.im := Y.im / FreqMultiplier;

    case Connection of
        TGeneralConnection.Wye:
            for i := 1 to Fnphases do
            begin
                YMatrix.SetElement(i, i, Y);
            end;
        TGeneralConnection.Delta:
        begin
            Y := (Y / 3.0); // Convert to delta impedance
            Yij := -Y;
            for i := 1 to Fnphases do
            begin
                j := i + 1;
                if j > Fnconds then
                    j := 1; // wrap around for closed connections
                YMatrix.AddElement(i, i, Y);
                YMatrix.AddElement(j, j, Y);
                YMatrix.AddElemSym(i, j, Yij);
            end;
        end;
    end;
end;

procedure TGeneric5Obj.CalcYPrim();
var
    i: Integer;
begin
    if (Yprim = NIL) OR (Yprim.order <> Yorder) OR (Yprim_Shunt = NIL) OR (Yprim_Series = NIL) then // YPrimInvalid
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

    // call helper routine to compute YPrim_Shunt
    CalcYPrimMatrix(YPrim_Shunt);

    // Set YPrim_Series based on a small fraction of the diagonals of YPrim_shunt
    // so that CalcVoltages doesn't fail
    // This is just one of a number of possible strategies but seems to work most of the time
    for i := 1 to Yorder do
        Yprim_Series.SetElement(i, i, Yprim_Shunt.Getelement(i, i) * 1.0e-10);

    // copy YPrim_shunt into YPrim; That's all that is needed for most PC Elements
    YPrim.CopyFrom(YPrim_Shunt);

    // Account for Open Conductors -- done in base class
    inherited CalcYPrim();
end;

procedure TGeneric5Obj.CalcModel(V, I: pComplexArray); // given voltages returns currents
var
    V012, I012: TSymCompArray;
begin
    if ctrl_mode = 0 then
    begin
        // Convert abc voltages to 012
        Phase2SymComp(V, pComplexArray(@V012));

        // compute I012
        if ActiveCircuit.Solution.DynaVars.SolutionMode = TSolveMode.DYNAMICMODE then
            CalcDynamic(V012, I012)
        else // All other modes are power flow modes
            CalcPFlow(V012, I012);

        SymComp2Phase(I, pComplexArray(@I012));       // convert back to I abc
    end // avg ctrl
    else //direct phase ctrl
    begin
        if fnphases = 3 then
        begin
            //3-phase ctrl
            // use Vterminal Iterminal directly instead of computing 120

            if ActiveCircuit.Solution.DynaVars.SolutionMode = TSolveMode.DYNAMICMODE then
                CalcDynamicVIabc(V, I) //if ((ctrl_mode=4)and (fnphases=3))
            else 
                //All other modes are power flow modes
                CalcPflowVIabc(V, I); //if ((ctrl_mode=4)and (fnphases=3))
        end
        else
        if fnphases = 1 then
        begin
            //1-phase ctrl
            // use Vterminal Iterminal directly instead of computing 120
            // actually there is no 120 for single phase

            if ActiveCircuit.Solution.DynaVars.SolutionMode = TSolveMode.DYNAMICMODE then
                CalcDynamicVIabc(V, I) //if (fnphases=1)
            else // All other modes are power flow modes
                CalcPflowVIabc(V, I); // //if (fnphases=1)
        end
        else
        begin
            //no consideration for 2-phase DG
        end;
    end;

    if FMonObj <> nil then
        infoPublish();
end;

procedure TGeneric5Obj.DoDynamicMode();
var
    i: Integer;
begin
    // Start off by getting the current in the admittance branch of the model
    CalcYPrimContribution(InjCurrent); // Init InjCurrent Array
    // Inj = -Itotal (in) - Yprim*Vtemp
    CalcModel(Vterminal, Iterminal);
    SetITerminalUpdated(TRUE);
    for i := 1 to Nphases do
        InjCurrent[i] -= ITerminal[i];
end;

procedure TGeneric5Obj.CalcGeneric5ModelContribution();
var
    i: Integer;
begin
    SetITerminalUpdated(FALSE);
    if ActiveCircuit.Solution.IsDynamicModel then
    begin
        DoDynamicMode();
        Exit;
    end;
    if ActiveCircuit.Solution.IsHarmonicModel and (ActiveCircuit.Solution.Frequency() <> ActiveCircuit.Fundamental) then
    begin
        DoSimpleMsg('%s: TGeneric5Obj.CalcGeneric5ModelContribution is not implemented for HarmonicMode. Aborting.', [FullName()], 202406013);
        DSS.SetSolutionAbort(true);
        Exit;
    end;

    CalcYPrimContribution(InjCurrent); // Init InjCurrent Array
    CalcModel(Vterminal, Iterminal);
    SetITerminalUpdated(TRUE);

    for i := 1 to Nphases do
        InjCurrent[i] -= ITerminal[i];
end;

procedure TGeneric5Obj.CalcInjCurrentArray();
begin
    // If the element is open, just zero the array and return
    if Generic5SwitchOpen then
    begin
        ZeroInjCurrent();
        Exit
    end;
    // otherwise, go to a routine that manages the calculation
    CalcGeneric5ModelContribution();
end;

procedure TGeneric5Obj.GetTerminalCurrents(Curr: pComplexArray);
begin
    with ActiveCircuit.Solution do
    begin
        if IterminalSolutionCount <> ActiveCircuit.Solution.SolutionCount then
        begin
            // recalc the contribution
            if not Generic5SwitchOpen then
                CalcGeneric5ModelContribution(); // Adds totals in Iterminal as a side effect
        end;
        inherited GetTerminalCurrents(Curr); // add in inherited contribution
    end;
end;

function TGeneric5Obj.InjCurrents(): Integer;
// Required function for managing computing of InjCurrents
begin
    // call the main function for doing calculation
    CalcInjCurrentArray(); // Difference between currents in YPrim and total terminal current
    // Add into System Injection Current Array
    Result := inherited InjCurrents();
end;

procedure TGeneric5Obj.InitHarmonics;
begin
    YPrimInvalid := true; // Force rebuild of YPrims
end;

procedure TGeneric5Obj.IntegrateStates();
begin
    // Compute Derivatives and then integrate
    ComputeIterminal();
    // Pshaft := P_DG; // P_DG is calculated in CalcDynamic or CalcDynamicVIabc
    IntegrateABCD();
end;

function TGeneric5Obj.NumVariables: Integer;
begin
    Result := NumGeneric5Variables;
end;

function TGeneric5Obj.VariableName(i: Integer): String;
begin
    Result := 'ERROR';
    if (i < 1) or (i > NumGeneric5Variables) then
        Exit;
    Result := TGeneric5(ParentClass).varNames[i - 1];
end;

function TGeneric5Obj.GetVariable(i: Integer): Double;
begin
    Result := -9999.99; // Error Value
    if (i < 1) or (i > NumGeneric5Variables) then
    begin
        DoSimpleMsg('%s: invalid variable index %d.', [FullName(), i], 565);
        Exit; // No variables to set
    end;

    case TVar(i) of
        TVar.V_DG:
            Result := V_DG;
        TVar.P_DG:
            Result := P_DG / 1000;//kW
        TVar.Q_DG:
            Result := Q_DG / 1000;
        TVar.V_DG1:
            Result := V_DG1;//Phase A or the first phase if there are less than 3 phases
        TVar.P_DG1:
            Result := P_DG1 / 1000;
        TVar.Q_DG1:
            Result := Q_DG1 / 1000;
        TVar.V_DG2:
            Result := V_DG2;//Phase B if exists
        TVar.P_DG2:
            Result := P_DG2 / 1000;
        TVar.Q_DG2:
            Result := Q_DG2 / 1000;
        TVar.V_DG3:
            Result := V_DG3;//Phase c if exists
        TVar.P_DG3:
            Result := P_DG3 / 1000;
        TVar.Q_DG3:
            Result := Q_DG3 / 1000;
        TVar.Qmax:
            Result := Qmax / 1000;
        TVar.Qmax_Phase:
            Result := Qmax_Phase / 1000;
        TVar.Pmax:
            Result := Pmax / 1000;
        TVar.Pmax_Phase:
            Result := Pmax_Phase / 1000;
        TVar.Alpha:
            Result := Alpha;
        TVar.Alpha1:
            Result := Alpha1;
        TVar.Alpha2:
            Result := Alpha2;
        TVar.Alpha3:
            Result := Alpha3;
        TVar.AlphaP:
            Result := AlphaP;
        TVar.AlphaP1:
            Result := AlphaP1;
        TVar.AlphaP2:
            Result := AlphaP2;
        TVar.AlphaP3:
            Result := AlphaP3;
        TVar.V_ref:
            Result := V_ref;
        TVar.kVA:
            // TODO: BUG: probably wrong in the original version (26: Result := MachineData.kVArating/1000 ;)
            Result := kVArating;
        TVar.kW:
            // TODO: BUG: probably wrong in the original version (27: Result := kWbase/1000;)
            Result := Wbase / 1000;
        TVar.cluster_num:
            Result := cluster_num;
        TVar.NdNumInCluster:
            Result := NdNumInCluster + 1;
        TVar.ctrl_mode:
            Result := ctrl_mode;
        TVar.Gradient:
            Result := Gradient;
        TVar.Id:
            Result := Id;
        TVar.Iq:
            Result := Iq;
        TVar.P_set:
            Result := P_ref * 3.0;
        TVar.Frequency:
        begin
            freq := ActiveCircuit.Solution.Frequency();
            if FMonObj <> nil then
                freq := freq + FMonObj.omg_fm;
            Result := freq;
        end;
        TVar.Defense:
        begin
            result := 0.0;
            if FMonObj <> nil then
                Result := z_dfs_plot;
        end;
    end;
end;

procedure TGeneric5Obj.SetVariable(i: Integer; Value: Double);
begin
    if (i < 1) or (i > NumGeneric5Variables) then
    begin
        DoSimpleMsg('%s: invalid variable index %d.', [FullName(), i], 565);
        Exit; // No variables to set
    end;

    case TVar(i) of
        TVar.V_DG:
            V_DG := Value;
        TVar.P_DG:
            P_DG := Value;
        TVar.Q_DG:
            Q_DG := Value;
        TVar.V_DG1:
            V_DG1 := Value;//Phase A or the first phase if there are less than 3 phases
        TVar.P_DG1:
            P_DG1 := Value;
        TVar.Q_DG1:
            Q_DG1 := Value;
        TVar.V_DG2:
            V_DG2 := Value;//Phase B if exists
        TVar.P_DG2:
            P_DG2 := Value;
        TVar.Q_DG2:
            Q_DG2 := Value;
        TVar.V_DG3:
            V_DG3 := Value;//Phase c if exists
        TVar.P_DG3:
            P_DG3 := Value;
        TVar.Q_DG3:
            Q_DG3 := Value;
        TVar.Qmax:
            Qmax := Value;
        TVar.Qmax_Phase:
            Qmax_Phase := Value;
        TVar.Pmax:
            Pmax := Value;
        TVar.Pmax_Phase:
            Pmax_Phase := Value;
        TVar.Alpha:
            Alpha := Value;
        TVar.Alpha1:
            Alpha1 := Value;
        TVar.Alpha2:
            Alpha2 := Value;
        TVar.Alpha3:
            Alpha3 := Value;
        TVar.AlphaP:
            AlphaP := Value;
        TVar.AlphaP1:
            AlphaP1 := Value;
        TVar.AlphaP2:
            AlphaP2 := Value;
        TVar.AlphaP3:
            AlphaP3 := Value;
        TVar.V_ref:
            V_ref := Value;
        TVar.kVA:
            kVArating := Value;
        TVar.kW:
            WBase := Value * 1000;
        TVar.cluster_num:
            cluster_num := trunc(Value);
        TVar.NdNumInCluster:
            NdNumInCluster := trunc(Value) - 1;
    else
        DoSimpleMsg('%s: variable %d is read-only.', [FullName(), i], 568);
        Exit; // No variables to set
    end;
    // Do Nothing for other variables: they are read only
end;

procedure TGeneric5Obj.GetAllVariables(var States: ArrayOfDouble);
var
    i: Integer;
begin
    for i := 1 to NumGeneric5Variables do
        States[i - 1] := GetVariable(i);
end;

procedure TGeneric5Obj.MakePosSequence;
begin
    DoSimpleMsg('%s: TGeneric5Obj.MakePosSequence is not implemented. Aborting.', [FullName()], 202406011);
    DSS.SetSolutionAbort(true);
end;

procedure TGeneric5Obj.SetConductorClosed(Index: Integer; Value: Boolean);
// Routine for handling Open/Close procedures
begin
    inherited;

    Generic5SwitchOpen := Value;
end;

end.
{$POP}