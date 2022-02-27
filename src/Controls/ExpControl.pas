unit ExpControl;

{
  ----------------------------------------------------------
  Copyright (c) 2015-2016, University of Pittsburgh
  Copyright (c) 2019-2020, Battelle Memorial Institute
  All rights reserved.
  ----------------------------------------------------------

  Notes: adapted and simplified from InvControl for adaptive controller research
}

INTERFACE

  uses
    gqueue, Command,
    ControlClass, ControlElem, CktElement, DSSClass, PVSystem, Arraydef, UComplex, DSSUcomplex,
    utilities, Dynamics, DSSPointerList, Classes, StrUtils;

  type
{$SCOPEDENUMS ON}
    TExpControlProp = (
        INVALID = 0,
        PVSystemList = 1,
        Vreg = 2,
        Slope = 3,
        VregTau = 4,
        Qbias = 5,
        VregMin = 6,
        VregMax = 7,
        QmaxLead = 8,
        QmaxLag = 9,
        EventLog = 10,
        DeltaQ_factor = 11,
        PreferQ = 12,
        Tresponse = 13
     );
{$SCOPEDENUMS OFF}

    TExpControl = class(TControlClass)
      protected
        procedure DefineProperties; override;
      public
        constructor Create(dssContext: TDSSContext);
        destructor Destroy; override;

        Function NewObject(const ObjName: String; Activate: Boolean = True): Pointer; override;
        PROCEDURE UpdateAll;
    end;

    TExpControlObj = class(TControlElem)
      private
            ControlActionHandle: Integer;
            ControlledElement: Array of TPVSystemObj;    // list of pointers to controlled PVSystem elements
            MonitoredElement : TDSSCktElement;  // First PVSystem element for now

            // PVSystemList information
            FListSize:Integer;
            FPVSystemPointerList: TDSSPointerList;

            // working storage for each PV system under management
            FPriorVpu: Array of Double;
            FPresentVpu: Array of Double;
            FPendingChange: Array of Integer;
            FVregs: Array of Double;
            FLastIterQ: Array of Double; // for DeltaQFactor
            FLastStepQ: Array of Double; // for FOpenTau
            FTargetQ: Array of Double;
            FWithinTol: Array of Boolean;

            // temp storage for biggest PV system, not each one
            cBuffer : Array of Complex;

            // user-supplied parameters (also PVSystemList and EventLog)
            FVregInit: Double;
            FQbias: Double;
            FdeltaQ_factor: Double;
            FVoltageChangeTolerance: Double; // no user adjustment
            FVarChangeTolerance: Double;     // no user adjustment
            FPreferQ: Boolean;
            
            FTresponse, FOpenTau: Double;

            PROCEDURE Set_PendingChange(Value: Integer;DevIndex: Integer);
            FUNCTION  Get_PendingChange(DevIndex: Integer):Integer;
            PROCEDURE UpdateExpControl(i:integer);
    public
            FPVSystemNameList:TStringList;
            QmaxLead: Double;
            QmaxLag: Double;
            VregMin: Double;
            VregMax: Double;
            VregTau: Double;
            QVSlope: Double; // originally FSlope

            constructor Create(ParClass:TDSSClass; const ExpControlName:String);
            destructor  Destroy; override;
            procedure PropertySideEffects(Idx: Integer; previousIntVal: Integer = 0); override;
            procedure MakeLike(OtherPtr: Pointer); override;

            // PROCEDURE Set_Enabled(Value: Boolean);Override;
            PROCEDURE MakePosSequence(); Override;  // Make a positive Sequence Model
            PROCEDURE RecalcElementData; Override;

            // Sample control quantities and set action times in Control Queue
            PROCEDURE Sample;  Override;

            // Do the action that is pending from last sample
            PROCEDURE DoPendingAction(Const Code, ProxyHdl: Integer); Override;

            PROCEDURE Reset; Override;  // Reset to initial defined state

            FUNCTION MakePVSystemList:Boolean;

            Property PendingChange[DevIndex: Integer]: Integer Read Get_PendingChange Write Set_PendingChange;

   end;

IMPLEMENTATION

USES
    Sysutils, DSSClassDefs, DSSGlobals, Circuit,  uCmatrix, MathUtil, Math,
    DSSHelper,
    DSSObjectHelper,
    TypInfo;

type 
    TObj = TExpControlObj;
    TProp = TExpControlProp;
const
    NumPropsThisClass = Ord(High(TProp));
    NONE = 0;
    CHANGEVARLEVEL = 1;
var
    PropInfo: Pointer;    

constructor TExpControl.Create(dssContext: TDSSContext);
begin
    if PropInfo = NIL then
        PropInfo := TypeInfo(TProp);

    inherited Create(dssContext, EXP_CONTROL, 'ExpControl');
end;

destructor TExpControl.Destroy;
Begin
  Inherited Destroy;
End;

PROCEDURE TExpControl.DefineProperties;
var 
    obj: TObj = NIL; // NIL (0) on purpose
Begin
    Numproperties := NumPropsThisClass;
    CountPropertiesAndAllocate();
    PopulatePropertyNames(0, NumPropsThisClass, PropInfo);
    
    // string lists
    PropertyType[ord(TProp.PVSystemList)] := TPropertyType.StringListProperty;
    PropertyOffset[ord(TProp.PVSystemList)] := ptruint(@obj.FPVSystemNameList);

    // boolean properties
    PropertyType[ord(TProp.EventLog)] := TPropertyType.BooleanProperty;
    PropertyType[ord(TProp.PreferQ)] := TPropertyType.BooleanProperty;
    PropertyOffset[ord(TProp.EventLog)] := ptruint(@obj.ShowEventLog);
    PropertyOffset[ord(TProp.PreferQ)] := ptruint(@obj.FPreferQ);

    // double properties (default type)
    PropertyOffset[ord(TProp.Qbias)] := ptruint(@obj.FQbias);
    PropertyOffset[ord(TProp.DeltaQ_factor)] := ptruint(@obj.FdeltaQ_factor);
    
    // advanced doubles
    PropertyOffset[ord(TProp.Vreg)] := ptruint(@obj.FVregInit);
    PropertyFlags[ord(TProp.Vreg)] := [TPropertyFlag.IgnoreInvalid, TPropertyFlag.NonNegative];

    PropertyOffset[ord(TProp.Slope)] := ptruint(@obj.QVSlope);
    PropertyFlags[ord(TProp.Slope)] := [TPropertyFlag.IgnoreInvalid, TPropertyFlag.NonNegative, TPropertyFlag.NonZero];

    PropertyOffset[ord(TProp.VregTau)] := ptruint(@obj.VregTau);
    PropertyFlags[ord(TProp.VregTau)] := [TPropertyFlag.IgnoreInvalid, TPropertyFlag.NonNegative];

    PropertyOffset[ord(TProp.VregMin)] := ptruint(@obj.VregMin);
    PropertyFlags[ord(TProp.VregMin)] := [TPropertyFlag.IgnoreInvalid, TPropertyFlag.NonNegative];

    PropertyOffset[ord(TProp.VregMax)] := ptruint(@obj.VregMax);
    PropertyFlags[ord(TProp.VregMax)] := [TPropertyFlag.IgnoreInvalid, TPropertyFlag.NonNegative];

    PropertyOffset[ord(TProp.QmaxLead)] := ptruint(@obj.QmaxLead);
    PropertyFlags[ord(TProp.QmaxLead)] := [TPropertyFlag.IgnoreInvalid, TPropertyFlag.NonNegative, TPropertyFlag.NonZero];

    PropertyOffset[ord(TProp.QmaxLag)] := ptruint(@obj.QmaxLag);
    PropertyFlags[ord(TProp.QmaxLag)] := [TPropertyFlag.IgnoreInvalid, TPropertyFlag.NonNegative, TPropertyFlag.NonZero];

    PropertyOffset[ord(TProp.Tresponse)] := ptruint(@obj.FTresponse);
    PropertyFlags[ord(TProp.Tresponse)] := [TPropertyFlag.IgnoreInvalid, TPropertyFlag.NonNegative, TPropertyFlag.NonZero];

    ActiveProperty  := NumPropsThisClass;
    inherited DefineProperties;
End;

FUNCTION TExpControl.NewObject(const ObjName: String; Activate: Boolean): Pointer;
var
    Obj: TObj;
begin
    Obj := TObj.Create(Self, ObjName);
    if Activate then 
        ActiveCircuit.ActiveCktElement := Obj;
    Obj.ClassIndex := AddObjectToList(Obj, Activate);
    Result := Obj;
end;

procedure TExpControlObj.PropertySideEffects(Idx: Integer; previousIntVal: Integer);
begin
    case Idx of 
        ord(TProp.PVSystemList):
        begin
          FPVSystemPointerList.Clear; // clear this for resetting on first sample
          FListSize := FPVSystemNameList.count;
        end;
    end;
    inherited PropertySideEffects(Idx, previousIntVal);
end;

procedure TExpControlObj.MakeLike(OtherPtr: Pointer);
VAR
    Other: TObj;
    i: Integer;
Begin
    inherited MakeLike(OtherPtr);
    Other := TObj(OtherPtr);
    FNPhases := Other.Fnphases;
    NConds:= Other.Fnconds; // Force Reallocation of terminal stuff
    for i := 1 to FPVSystemPointerList.Count do
    begin
        ControlledElement[i] := Other.ControlledElement[i];
        FWithinTol[i]:= Other.FWithinTol[i];
    end;
    FListSize := Other.FListSize;
    FVoltageChangeTolerance := Other.FVoltageChangeTolerance;
    FVarChangeTolerance := Other.FVarChangeTolerance;
    FVregInit := Other.FVregInit;
    QVSlope := Other.QVSlope;
    VregTau := Other.VregTau;
    FQbias := Other.FQbias;
    VregMin := Other.VregMin;
    VregMax := Other.VregMax;
    QmaxLead:= Other.QmaxLead;
    QmaxLag := Other.QmaxLag;
    FdeltaQ_factor := Other.FdeltaQ_factor;
    FPreferQ := Other.FPreferQ;
End;

constructor TExpControlObj.Create(ParClass:TDSSClass; const ExpControlName:String);
Begin
     Inherited Create(ParClass);
     Name                     := LowerCase(ExpControlName);
     DSSObjType               := ParClass.DSSClassType;
     
     //  Control elements are zero current sources that attach to a terminal of a
     //  power-carrying device, but do not alter voltage or current flow.
     //  Define a default number of phases and conductors here and update in
     //  RecalcElementData routine if necessary. This allocates arrays for voltages
     //  and currents and gives more direct access to the values, if needed
     FNPhases := 3;  // Directly set conds and phases
     Fnconds := 3;
     Nterms  := 1;  // this forces allocation of terminals and conductors
     // This general feature should not be used for ExpControl,
     // because it controls more than one PVSystem

     ShowEventLog := FALSE;

     ControlledElement := nil;
     FPVSystemNameList := nil;
     FPVSystemPointerList := nil;
     cBuffer := nil;
     FPriorVpu := nil;
     FPresentVpu := nil;
     FPendingChange := nil;
     FLastIterQ := nil;
     FLastStepQ := nil;
     FTargetQ := nil;
     FWithinTol := nil;

     FVoltageChangeTolerance:=0.0001;  // per-unit
     FVarChangeTolerance:=0.0001;  // per-unit

     FPVSystemNameList := TSTringList.Create;
     FPVSystemPointerList := TDSSPointerList.Create(20);  // Default size and increment

     // user parameters for dynamic Vreg
     FVregInit := 1.0; // 0 means to find it during initialization
     QVSlope := 50.0;
     VregTau := 1200.0;
     FVregs := nil;
     FQbias := 0.0;
     VregMin := 0.95;
     VregMax := 1.05;
     QmaxLead := 0.44;
     QmaxLag := 0.44;
     FdeltaQ_factor := 0.7; // only on control iterations, not the final solution
     FPreferQ := FALSE;
     FTresponse := 0.0;
     FOpenTau := 0.0;

     //generic for control
     FPendingChange         := nil;
End;

destructor TExpControlObj.Destroy;
Begin
     Finalize(ControlledElement);
     Finalize(cBuffer);
     Finalize(FPriorVpu);
     Finalize(FPresentVpu);
     Finalize(FPendingChange);
     Finalize(FLastIterQ);
     Finalize(FLastStepQ);
     Finalize(FTargetQ);
     Finalize(FWithinTol);
     Finalize(FVregs);
     Inherited Destroy;
End;

PROCEDURE TExpControlObj.RecalcElementData;
VAR
   i      :Integer;
   maxord :Integer;
Begin
    FOpenTau := FTresponse / 2.3026;
    IF FPVSystemPointerList.Count = 0 Then  MakePVSystemList;

    IF FPVSystemPointerList.Count > 0  Then begin
    {Setting the terminal of the ExpControl device to same as the 1st PVSystem element}
         MonitoredElement :=  TDSSCktElement(FPVSystemPointerList.Get(1));   // Set MonitoredElement to 1st PVSystem in lise
         Setbus(1, MonitoredElement.Firstbus);
    End;

    maxord := 0; // will be the size of cBuffer
    for i := 1 to FPVSystemPointerList.Count do begin
        // User ControlledElement[] as the pointer to the PVSystem elements
         ControlledElement[i] :=  TPVSystemObj(FPVSystemPointerList.Get(i));  // pointer to i-th PVSystem
         FNphases := ControlledElement[i].NPhases;  // TEMC TODO - what if these are different sizes (same concern exists with InvControl)
         Nconds  := Nphases;
         if (ControlledElement[i] = nil) then
            DoErrorMsg(Format(_('ExpControl: "%s"'), [Self.Name]),
              Format(_('Controlled Element "%s" not found.'), [FPVSystemNameList.Strings[i-1]]),
              _('PVSystem object must be defined previously.'), 361);
         if ControlledElement[i].Yorder > maxord then maxord := ControlledElement[i].Yorder;
         ControlledElement[i].ActiveTerminalIdx := 1; // Make the 1 st terminal active
    end;
    if maxord > 0 then SetLength(cBuffer, SizeOF(Complex) * maxord);
End;

procedure TExpControlObj.MakePosSequence();
// ***  This assumes the PVSystem devices have already been converted to pos seq
begin
  IF FPVSystemPointerList.Count = 0 Then  RecalcElementData;
  // TEMC - from here to inherited was copied from InvControl
  FNphases := 3;
  Nconds := 3;
  Setbus(1, MonitoredElement.GetBus(ElementTerminal));
  IF FPVSystemPointerList.Count > 0  Then Begin
    // Setting the terminal of the ExpControl device to same as the 1st PVSystem element
    // This sets it to a realistic value to avoid crashes later
    MonitoredElement :=  TDSSCktElement(FPVSystemPointerList.Get(1));   // Set MonitoredElement to 1st PVSystem in lise
    Setbus(1, MonitoredElement.Firstbus);
    FNphases := MonitoredElement.NPhases;
    Nconds := Nphases;
  End;
  inherited;
end;

PROCEDURE TExpControlObj.DoPendingAction;
VAR
  i             :Integer;
  Qset, DeltaQ  :Double;
  Qmaxpu, Qpu   :Double;
  Qbase         :Double;
  Qinvmaxpu     :Double;
  Plimit        :Double;
  dt            :Double;
  PVSys         :TPVSystemObj;
BEGIN
  for i := 1 to FPVSystemPointerList.Count do begin
    PVSys := ControlledElement[i];   // Use local variable in loop
    if PendingChange[i] = CHANGEVARLEVEL then begin
      PVSys.VWmode := FALSE;
      PVSys.ActiveTerminalIdx := 1; // Set active terminal of PVSystem to terminal 1
      PVSys.Varmode := VARMODEKVAR;  // Set var mode to VARMODEKVAR to indicate we might change kvar
      FTargetQ[i] := 0.0;
      Qbase  := PVSys.kVARating;
      Qinvmaxpu := PVSys.kvarLimit / Qbase;
      Qpu := PVSys.Presentkvar / Qbase; // no change for now

      if (FWithinTol[i]=False) then begin
        // look up Qpu from the slope crossing at Vreg, and add the bias
        Qpu := -QVSlope * (FPresentVpu[i] - FVregs[i]) + FQbias;
        If ShowEventLog Then AppendtoEventLog('ExpControl.' + Self.Name+','+PVSys.Name,
          Format(' Setting Qpu= %.5g at FVreg= %.5g, Vpu= %.5g', [Qpu, FVregs[i],FPresentVpu[i]]));
      end;

      // apply limits on Qpu, then define the target in kVAR
      PVSys.SetNominalPVSystemOuput; // as does InvControl
      if FPreferQ then
        Qmaxpu := 1.0
      else
        Qmaxpu := Sqrt(1 - Sqr(PVSys.PresentkW/Qbase)); // dynamic headroom
      if Qmaxpu > Qinvmaxpu then Qmaxpu := Qinvmaxpu;
      if Abs(Qpu) > Qmaxpu then Qpu := QmaxPu * Sign(Qpu);
      if Qpu < -QmaxLead then Qpu := -QmaxLead;
      if Qpu > QmaxLag then Qpu := QmaxLag;
      FTargetQ[i] := Qbase * Qpu;
      if FPreferQ then begin
        Plimit := Qbase * Sqrt (1 - Qpu * Qpu);
        if Plimit < PVSys.PresentkW then begin
          If ShowEventLog Then AppendtoEventLog('ExpControl.' + Self.Name+','+PVSys.Name,
            Format(' curtailing %.3f to %.3f kW', [PVSys.PresentkW, Plimit]));
          PVSys.PresentkW := Plimit;
          PVSys.puPmpp := Plimit/PVSys.Pmpp;
        end;
      end;

      // put FTargetQ through the low-pass open-loop filter
      if FOpenTau > 0.0 then begin
        dt :=  ActiveCircuit.Solution.Dynavars.h;
        FTargetQ[i] := FLastStepQ[i] + (FTargetQ[i] - FLastStepQ[i]) * (1 - Exp (-dt / FOpenTau)); // TODO - precalculate?
      end;

      // only move the non-bias component by deltaQ_factor in this control iteration
      DeltaQ := FTargetQ[i] - FLastIterQ[i];
      Qset := FLastIterQ[i] + DeltaQ * FdeltaQ_factor;
 //     Qset := FQbias * Qbase;
      If PVSys.Presentkvar <> Qset Then PVSys.Presentkvar := Qset;
      If ShowEventLog Then AppendtoEventLog('ExpControl.' + Self.Name +','+ PVSys.Name,
                             Format(' Setting PVSystem output kvar= %.5g',
                             [PVSys.Presentkvar]));
      FLastIterQ[i] := Qset;
      FPriorVpu[i] := FPresentVpu[i];
      ActiveCircuit.Solution.LoadsNeedUpdating := TRUE;
      // Force recalc of power parms
      Set_PendingChange(NONE,i);
    end
  end;
end;

PROCEDURE TExpControlObj.Sample;
VAR
  i,j                 :Integer;
  basekV, Vpresent    :Double;
  Verr, Qerr          :Double;
  PVSys               :TPVSystemObj;
begin
  // If list is not defined, go make one from all PVSystem in circuit
  IF FPVSystemPointerList.Count=0 Then RecalcElementData;

  If (FListSize>0) then Begin
    // If an ExpControl controls more than one PV, control each one
    // separately based on the PVSystem's terminal voltages, etc.
    for i := 1 to FPVSystemPointerList.Count do begin
      PVSys := ControlledElement[i];   // Use local variable in loop
      // Calculate the present average voltage  magnitude
      PVSys.ComputeVTerminal;
      for j := 1 to PVSys.Yorder do cBuffer[j] := PVSys.Vterminal^[j];
      BasekV := ActiveCircuit.Buses^[PVSys.terminals[0].busRef].kVBase;
      Vpresent := 0;
      For j := 1 to PVSys.NPhases Do Vpresent := Vpresent + Cabs(cBuffer[j]);
      FPresentVpu[i] := (Vpresent / PVSys.NPhases) / (basekV * 1000.0);
      // if initializing with Vreg=0 in static mode, we want to FIND Vreg
      if (ActiveCircuit.Solution.ControlMode = CTRLSTATIC) and (FVregInit <= 0.0) then
        FVregs[i] := FPresentVpu[i];
      // both errors are in per-unit
      Verr := Abs(FPresentVpu[i] - FPriorVpu[i]);
      Qerr := Abs(PVSys.Presentkvar - FTargetQ[i]) / PVSys.kVARating;
      // process the sample
      if (PVSys.InverterON = FALSE) and (PVSys.VarFollowInverter = TRUE) then begin // not injecting
        if (VregTau > 0.0) and (FVregs[i] <= 0.0) then
          FVregs[i] := FPresentVpu[i]; // wake up to the grid voltage, otherwise track it while not injecting
        continue;
      end;
      PVSys.VWmode := FALSE;
      if ((Verr > FVoltageChangeTolerance) or (Qerr > FVarChangeTolerance) or
          (ActiveCircuit.Solution.ControlIteration = 1)) then begin
        FWithinTol[i] := False;
        Set_PendingChange(CHANGEVARLEVEL,i);
        With  ActiveCircuit.Solution.DynaVars Do
          ControlActionHandle := ActiveCircuit.ControlQueue.Push (intHour, t + TimeDelay, PendingChange[i], 0, Self);
        If ShowEventLog Then AppendtoEventLog('ExpControl.' + Self.Name+' '+PVSys.Name, Format
          (' outside Hit Tolerance, Verr= %.5g, Qerr=%.5g', [Verr,Qerr]));
      end else begin
        FWithinTol[i] := True;
        If ShowEventLog Then AppendtoEventLog('ExpControl.' + Self.Name+' '+PVSys.Name, Format
          (' within Hit Tolerance, Verr= %.5g, Qerr=%.5g', [Verr,Qerr]));
      end;
    end;  {For}
  end; {If FlistSize}
end;

Function TExpControlObj.MakePVSystemList:Boolean;
VAR
   PVSysClass:TDSSClass;
   PVSys:TPVsystemObj;
   i:Integer;
begin
  Result := FALSE;
  PVSysClass := GetDSSClassPtr(DSS, 'PVsystem');
  If FListSize > 0 Then Begin    // Name list is defined - Use it
    SetLength(ControlledElement,FListSize+1);  // Use this as the main pointer to PVSystem Elements
    SetLength(FPriorVpu, FListSize+1);
    SetLength(FPresentVpu, FListSize+1);
    SetLength(FPendingChange,FListSize+1);
    SetLength(FLastIterQ,FListSize+1);
    SetLength(FLastStepQ,FListSize+1);
    SetLength(FTargetQ,FListSize+1);
    SetLength(FWithinTol, FListSize+1);
    SetLength(FVregs, FListSize+1);
    For i := 1 to FListSize Do Begin
      PVSys := PVSysClass.Find(FPVSystemNameList.Strings[i-1]);
      If Assigned(PVSys) and PVSys.Enabled Then FPVSystemPointerList.Add(PVSys);
    End;
  End Else Begin
     {Search through the entire circuit for enabled pvsysten objects and add them to the list}
         For i := 1 to PVSysClass.ElementCount Do Begin
            PVSys :=  PVSysClass.ElementList.Get(i);
            If PVSys.Enabled Then FPVSystemPointerList.Add(PVSys);
            FPVSystemNameList.Add(PVSys.Name);
         End;
         FListSize := FPVSystemPointerList.Count;

         SetLength(ControlledElement,FListSize+1);

         SetLength(FPriorVpu, FListSize+1);
         SetLength(FPresentVpu, FListSize+1);

         SetLength(FPendingChange,FListSize+1);
         SetLength(FLastIterQ,FListSize+1);
         SetLength(FLastStepQ,FListSize+1);
         SetLength(FTargetQ,FListSize+1);
         SetLength(FWithinTol, FListSize+1);
         SetLength(FVregs, FListSize+1);
    End;  {Else}

  //Initialize arrays
  For i := 1 to FlistSize Do begin
//    PVSys := PVSysClass.Find(FPVSystemNameList.Strings[i-1]);
//    Set_NTerms(PVSys.NTerms); // TODO - what is this for?
    FPriorVpu[i] := 0.0;
    FPresentVpu[i] := 0.0;
    FLastIterQ[i] := -1.0;
    FLastStepQ[i] := -1.0;
    FTargetQ[i] :=0.0;
    FWithinTol[i] := False;
    FVregs[i] := FVregInit;
    FPendingChange[i] := NONE;
  end; {For}
  RecalcElementData;
  If FPVSystemPointerList.Count>0 Then Result := TRUE;
end;

procedure TExpControlObj.Reset;
begin
  // inherited;
end;

//procedure TExpControlObj.Set_Enabled(Value: Boolean);
//begin
//    inherited;
//    // Reset controlled PVSystems to original PF
//end;

procedure TExpControlObj.Set_PendingChange(Value: Integer;DevIndex: Integer);
begin
  FPendingChange[DevIndex] := Value;
  DblTraceParameter := Value;
end;

procedure TExpControlObj.UpdateExpControl(i:integer);
Var
  j                      : Integer;
  PVSys                  : TPVSystemObj;
  dt, Verr: Double; // for DYNAMICVREG
begin
  for j := 1 to FPVSystemPointerList.Count do begin
    PVSys := ControlledElement[j];
    FLastStepQ[j] := PVSys.Presentkvar;
    if VregTau > 0.0 then begin
      dt :=  ActiveCircuit.Solution.Dynavars.h;
      Verr := FPresentVpu[j] - FVregs[j];
      FVregs[j] := FVregs[j] + Verr * (1 - Exp (-dt / VregTau)); // TODO - precalculate?
    end else begin
      Verr := 0.0;
    end;
    if FVregs[j] < VregMin then FVregs[j] := VregMin;
    if FVregs[j] > VregMax then FVregs[j] := VregMax;
    PVSys.Set_Variable(5,FVregs[j]);
    If ShowEventLog Then AppendtoEventLog('ExpControl.' + Self.Name+','+PVSys.Name,
      Format(' Setting new Vreg= %.5g Vpu=%.5g Verr=%.5g',
      [FVregs[j], FPresentVpu[j], Verr]));
  end;
end;

FUNCTION TExpControlObj.Get_PendingChange(DevIndex: Integer):Integer;
begin
  Result := FPendingChange[DevIndex];
end;

//Called at end of main power flow solution loop
PROCEDURE TExpControl.UpdateAll;
VAR
  i : Integer;
Begin
  For i := 1 to ElementList.Count  Do
    With TExpControlObj(ElementList.Get(i)) Do
      If Enabled Then UpdateExpControl(i);
End;

initialization
    PropInfo := NIL;
end.
