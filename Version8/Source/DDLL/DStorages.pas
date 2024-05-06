unit DStorages;

interface

function StoragesI(mode:longint;arg:longint):longint;cdecl;
function StoragesF(mode:longint;arg:double):double;cdecl;
function StoragesS(mode:longint;arg:pAnsiChar):pAnsiChar;cdecl;
procedure StoragesV(mode:longint; var myPointer: Pointer; var myType, mySize: longint);cdecl;

implementation

uses {$IFNDEF FPC_DLL}ComServ, {$ENDIF}Storage, Variants, PointerList, DSSGlobals, CktElement, SysUtils;

// Wrapper for concentating all the integer-based IO for the Storage Obj mimicking COM
function StoragesI(mode:longint;arg:longint):longint;cdecl;
Var
   pStorageElem:TStorageObj;

Begin
  Result:=0; // Default return value
  case mode of
    0 : Begin           // Storages.First
          If ActiveCircuit[ActiveActor] <> Nil Then
          Begin
            pStorageElem := ActiveCircuit[ActiveActor].StorageElements.First;
            If pStorageElem <> Nil Then
            Begin
              Repeat
                If pStorageElem.Enabled
                Then Begin
                  ActiveCircuit[ActiveActor].ActiveCktElement := pStorageElem;
                  Result := 1;
                End
                Else pStorageElem := ActiveCircuit[ActiveActor].StorageElements.Next;
              Until (Result = 1) or (pStorageElem = nil);
            End
          End;
    End;
    1 : Begin           // Storages.Next
          If ActiveCircuit[ActiveActor] <> Nil Then
          Begin
            pStorageElem := ActiveCircuit[ActiveActor].StorageElements.Next;
            If pStorageElem <> Nil Then
            Begin
              Repeat
                If pStorageElem.Enabled
                Then
                Begin
                  ActiveCircuit[ActiveActor].ActiveCktElement := pStorageElem;
                  Result := ActiveCircuit[ActiveActor].StorageElements.ActiveIndex;
                End
                Else pStorageElem := ActiveCircuit[ActiveActor].StorageElements.Next;
              Until (Result > 0) or (pStorageElem = nil);
            End
          End;
    End;
    2 : Begin           // Storages.Count
          If Assigned(ActiveCircuit[ActiveActor]) Then
            Result := ActiveCircuit[ActiveActor].StorageElements.ListSize;
    End;
    3 : Begin           // Storages.Idx read
          If Assigned(ActiveCircuit[ActiveActor]) Then
            Result := ActiveCircuit[ActiveActor].StorageElements.ActiveIndex
    End;
    4 : Begin           // Storages.Idx write
          if ActiveCircuit[ActiveActor] <> Nil then
          Begin
            pStorageElem := ActiveCircuit[ActiveActor].StorageElements.Get(arg);
            If pStorageElem <> Nil Then ActiveCircuit[ActiveActor].ActiveCktElement := pStorageElem;
          End;
    End;
    5 : Begin           // Storages.State Read
          If ActiveCircuit[ActiveActor] <> Nil Then
          Begin
            pStorageElem := ActiveCircuit[ActiveActor].StorageElements.Active;
            If pStorageElem <> Nil Then
            Begin
              Result := pStorageElem.StorageState;
            End;
          End;
    End;
    6 : Begin           // Storages.State Write
          {  Legal States
           STORE_CHARGING    = -1;
           STORE_IDLING      =  0;
           STORE_DISCHARGING =  1;
          }
          If ActiveCircuit[ActiveActor] <> Nil Then
          Begin
            pStorageElem := ActiveCircuit[ActiveActor].StorageElements.Active;
            If pStorageElem <> Nil Then
            Begin
              pStorageElem.StorageState := arg;
            End;
          End;
    End;
    7 : Begin           // Storages.ControlMode Read
          If ActiveCircuit[ActiveActor] <> Nil Then
          Begin
            pStorageElem := ActiveCircuit[ActiveActor].StorageElements.Active;
            If pStorageElem <> Nil Then
            Begin
              if pStorageElem.GFM_mode then
                Result := 1
            End;
          End;
    End;
    8 : Begin           // Storages.ControlMode Write
          If ActiveCircuit[ActiveActor] <> Nil Then
          Begin
            pStorageElem := ActiveCircuit[ActiveActor].StorageElements.Active;
            If pStorageElem <> Nil Then
            Begin
              pStorageElem.GFM_mode := False;
              if arg <> 0 then
                pStorageElem.GFM_mode := True
            End;
          End;
    End;
    9 : Begin           // Storages.SafeMode
          If ActiveCircuit[ActiveActor] <> Nil Then
          Begin
            pStorageElem := ActiveCircuit[ActiveActor].StorageElements.Active;
            If pStorageElem <> Nil Then
            Begin
              if pStorageElem.myDynVars.SafeMode then
                Result := 1;
            End;
          End;
    End;
    10: Begin           // Storages.VarFollowInverter Read
          If ActiveCircuit[ActiveActor] <> Nil Then
          Begin
            pStorageElem := ActiveCircuit[ActiveActor].StorageElements.Active;
            If pStorageElem <> Nil Then
            Begin
              if pStorageElem.FVarFollowInverter then
                Result := 1;
            End;
          End;
    End;
    11: Begin           // Storages.VarFollowInverter Write
          If ActiveCircuit[ActiveActor] <> Nil Then
          Begin
            pStorageElem := ActiveCircuit[ActiveActor].StorageElements.Active;
            If pStorageElem <> Nil Then
            Begin
              pStorageElem.FVarFollowInverter := False;
              if arg <> 0 then
                 pStorageElem.FVarFollowInverter := True
            End;
          End;
    End
    else
    Begin
      Result := -1; // Just sent the wrong command
    End;
  end;
End;

// Wrapper for concentating all the double-based IO for the Storage Obj mimicking COM
function StoragesF(mode:longint;arg:double):double;cdecl;
Var
   pStorage:TStorageObj;

Begin
  Result:=0; // Default return value
  case mode of
    0 : Begin           // Storages.puSOC Read
          If ActiveCircuit[ActiveActor] <> Nil Then
          Begin
            pStorage := ActiveCircuit[ActiveActor].StorageElements.Active;
            If pStorage <> Nil Then
            Begin
              Result := pStorage.Storagevars.kWhStored/pStorage.StorageVars.kWhRating;
            End;
          End;
    End;
    1 : Begin           // Storages.puSOC Write
          If ActiveCircuit[ActiveActor] <> Nil Then
          Begin
            pStorage := ActiveCircuit[ActiveActor].StorageElements.Active;
            If pStorage <> Nil Then
            Begin
              pStorage.Storagevars.kWhStored := pStorage.StorageVars.kWhRating * arg;
            End;
          End;
    End;
    2 : Begin           // Storages.AmpLimit Read
          If ActiveCircuit[ActiveActor] <> Nil Then
          Begin
            pStorage := ActiveCircuit[ActiveActor].StorageElements.Active;
            If pStorage <> Nil Then
            Begin
              Result := pStorage.myDynVars.ILimit;
            End;
          End;
    End;
    3 : Begin           // Storages.AmpLimit Write
          If ActiveCircuit[ActiveActor] <> Nil Then
          Begin
            pStorage := ActiveCircuit[ActiveActor].StorageElements.Active;
            If pStorage <> Nil Then
            Begin
              pStorage.myDynVars.ILimit := arg;
            End;
          End;
    End;
    4 : Begin           // Storages.AmpLimitGain Read
          If ActiveCircuit[ActiveActor] <> Nil Then
          Begin
            pStorage := ActiveCircuit[ActiveActor].StorageElements.Active;
            If pStorage <> Nil Then
            Begin
              Result := pStorage.myDynVars.VError;
            End;
          End;
    End;
    5 : Begin           // Storages.AmpLimitGain Write
          If ActiveCircuit[ActiveActor] <> Nil Then
          Begin
            pStorage := ActiveCircuit[ActiveActor].StorageElements.Active;
            If pStorage <> Nil Then
            Begin
              pStorage.myDynVars.VError := arg;
            End;
          End;
    End;
    6 : Begin            // Storages.ChargeTrigger Read
          If ActiveCircuit[ActiveActor] <> Nil Then
          Begin
            pStorage := ActiveCircuit[ActiveActor].StorageElements.Active;
            If pStorage <> Nil Then
            Begin
              Result := pStorage.ChargeTrigger;
            End;
          End;
    End;
    7 : Begin            // Storages.ChargeTrigger Write
          If ActiveCircuit[ActiveActor] <> Nil Then
          Begin
            pStorage := ActiveCircuit[ActiveActor].StorageElements.Active;
            If pStorage <> Nil Then
            Begin
              pStorage.ChargeTrigger := arg;
            End;
          End;
    End;
    8 : Begin            // Storages.DisChargeTrigger Read
          If ActiveCircuit[ActiveActor] <> Nil Then
          Begin
            pStorage := ActiveCircuit[ActiveActor].StorageElements.Active;
            If pStorage <> Nil Then
            Begin
              Result := pStorage.DisChargeTrigger;
            End;
          End;
    End;
    9 : Begin            // Storages.DisChargeTrigger Write
          If ActiveCircuit[ActiveActor] <> Nil Then
          Begin
            pStorage := ActiveCircuit[ActiveActor].StorageElements.Active;
            If pStorage <> Nil Then
            Begin
              pStorage.DisChargeTrigger := arg;
            End;
          End;
    End;
    10: Begin            // Storages.EffCharge Read
          If ActiveCircuit[ActiveActor] <> Nil Then
          Begin
            pStorage := ActiveCircuit[ActiveActor].StorageElements.Active;
            If pStorage <> Nil Then
            Begin
              Result := pStorage.pctChargeEff;
            End;
          End;
    End;
    11: Begin            // Storages.EffCharge Write
          If ActiveCircuit[ActiveActor] <> Nil Then
          Begin
            pStorage := ActiveCircuit[ActiveActor].StorageElements.Active;
            If pStorage <> Nil Then
            Begin
              pStorage.pctChargeEff := arg;
            End;
          End;
    End;
    12: Begin            // Storages.EffDisCharge Read
          If ActiveCircuit[ActiveActor] <> Nil Then
          Begin
            pStorage := ActiveCircuit[ActiveActor].StorageElements.Active;
            If pStorage <> Nil Then
            Begin
              Result := pStorage.pctDischargeEff;
            End;
          End;
    End;
    13: Begin            // Storages.EffDisCharge Write
          If ActiveCircuit[ActiveActor] <> Nil Then
          Begin
            pStorage := ActiveCircuit[ActiveActor].StorageElements.Active;
            If pStorage <> Nil Then
            Begin
              pStorage.pctDischargeEff := arg;
            End;
          End;
    End;
    14: Begin            // Storages.kP Read
        If ActiveCircuit[ActiveActor] <> Nil Then
        Begin
          pStorage := ActiveCircuit[ActiveActor].StorageElements.Active;
          If pStorage <> Nil Then
          Begin
            Result := pStorage.myDynVars.kP * 1e3;
          End;
        End;
    End;
    15: Begin            // Storages.kP Write
          If ActiveCircuit[ActiveActor] <> Nil Then
          Begin
            pStorage := ActiveCircuit[ActiveActor].StorageElements.Active;
            If pStorage <> Nil Then
            Begin
              pStorage.myDynVars.kP := arg / 1e3;
            End;
          End;
    End;
    16: Begin            // Storages.kV Read
          If ActiveCircuit[ActiveActor] <> Nil Then
          Begin
            pStorage := ActiveCircuit[ActiveActor].StorageElements.Active;
            If pStorage <> Nil Then
            Begin
              Result := pStorage.PresentkV;
            End;
          End;
    End;
    17: Begin            // Storages.kV Write
          If ActiveCircuit[ActiveActor] <> Nil Then
          Begin
            pStorage := ActiveCircuit[ActiveActor].StorageElements.Active;
            If pStorage <> Nil Then
            Begin
              pStorage.PresentkV := arg;
            End;
          End;
    End;
    18: Begin            // Storages.kVA Read
          If ActiveCircuit[ActiveActor] <> Nil Then
          Begin
            pStorage := ActiveCircuit[ActiveActor].StorageElements.Active;
            If pStorage <> Nil Then
            Begin
              Result := pStorage.StorageVars.FkVArating;
            End;
          End;
    End;
    19: Begin            // Storages.kVA Write
          If ActiveCircuit[ActiveActor] <> Nil Then
          Begin
            pStorage := ActiveCircuit[ActiveActor].StorageElements.Active;
            If pStorage <> Nil Then
            Begin
              pStorage.StorageVars.FkVArating := arg;
            End;
          End;
    End;
    20: Begin            // Storages.kvar Read
          If ActiveCircuit[ActiveActor] <> Nil Then
          Begin
            pStorage := ActiveCircuit[ActiveActor].StorageElements.Active;
            If pStorage <> Nil Then
            Begin
              Result := pStorage.kvarRequested;
            End;
          End;
    End;
    21: Begin            // Storages.kvar Write
          If ActiveCircuit[ActiveActor] <> Nil Then
          Begin
            pStorage := ActiveCircuit[ActiveActor].StorageElements.Active;
            If pStorage <> Nil Then
            Begin
              pStorage.kvarRequested := arg;
            End;
          End;
    End;
    22: Begin            // Storages.kVDC Read
          If ActiveCircuit[ActiveActor] <> Nil Then
          Begin
            pStorage := ActiveCircuit[ActiveActor].StorageElements.Active;
            If pStorage <> Nil Then
            Begin
              Result := pStorage.myDynVars.RatedVDC / 1e3;
            End;
          End;
    End;
    23: Begin             // Storages.kVDC Write
          If ActiveCircuit[ActiveActor] <> Nil Then
          Begin
            pStorage := ActiveCircuit[ActiveActor].StorageElements.Active;
            If pStorage <> Nil Then
            Begin
              pStorage.myDynVars.RatedVDC := arg * 1e3;
            End;
          End;
    End;
    24: Begin            // Storages.kW Read
          If ActiveCircuit[ActiveActor] <> Nil Then
          Begin
            pStorage := ActiveCircuit[ActiveActor].StorageElements.Active;
            If pStorage <> Nil Then
            Begin
              Result := pStorage.kW;
            End;
          End;
    End;
    25: Begin            // Storages.kW Write
          If ActiveCircuit[ActiveActor] <> Nil Then
          Begin
            pStorage := ActiveCircuit[ActiveActor].StorageElements.Active;
            If pStorage <> Nil Then
            Begin
              pStorage.kW := arg;
            End;
          End;
    End;
    26: Begin           // Storages.kWhRated Read
          If ActiveCircuit[ActiveActor] <> Nil Then
          Begin
            pStorage := ActiveCircuit[ActiveActor].StorageElements.Active;
            If pStorage <> Nil Then
            Begin
              Result := pStorage.StorageVars.kWhrating;
            End;
          End;
    End;
    27: Begin           // Storages.kWhRated Write
          If ActiveCircuit[ActiveActor] <> Nil Then
          Begin
            pStorage := ActiveCircuit[ActiveActor].StorageElements.Active;
            If pStorage <> Nil Then
            Begin
              pStorage.StorageVars.kWhrating := arg;
            End;
          End;
    End;
    28: Begin           // Storages.kWRated Read
          If ActiveCircuit[ActiveActor] <> Nil Then
          Begin
            pStorage := ActiveCircuit[ActiveActor].StorageElements.Active;
            If pStorage <> Nil Then
            Begin
              Result := pStorage.StorageVars.kWrating;
            End;
          End;
    End;
    29: Begin           // Storages.kWRated Write
          If ActiveCircuit[ActiveActor] <> Nil Then
          Begin
            pStorage := ActiveCircuit[ActiveActor].StorageElements.Active;
            If pStorage <> Nil Then
            Begin
              pStorage.StorageVars.kWrating := arg;
            End;
          End;
    End;
    30: Begin           // Storages.LimitCurrent Read
          If ActiveCircuit[ActiveActor] <> Nil Then
          Begin
            pStorage := ActiveCircuit[ActiveActor].StorageElements.Active;
            If pStorage <> Nil Then
            Begin
              if pStorage.IsCurrentLimited then
                Result := 1;
            End;
          End;
    End;
    31: Begin           // Storages.LimitCurrent Write
          If ActiveCircuit[ActiveActor] <> Nil Then
          Begin
            pStorage := ActiveCircuit[ActiveActor].StorageElements.Active;
            If pStorage <> Nil Then
            Begin
              pStorage.IsCurrentLimited := False;
              if arg <> 0 then
                pStorage.IsCurrentLimited := True;
            End;
          End;
    End;
    32: Begin           // Storages.PF Read
          If ActiveCircuit[ActiveActor] <> Nil Then
          Begin
            pStorage := ActiveCircuit[ActiveActor].StorageElements.Active;
            If pStorage <> Nil Then
            Begin
              Result := pStorage.PFnominal;
            End;
          End;
    End;
    33: Begin           // Storages.PF Write
          If ActiveCircuit[ActiveActor] <> Nil Then
          Begin
            pStorage := ActiveCircuit[ActiveActor].StorageElements.Active;
            If pStorage <> Nil Then
            Begin
              pStorage.PFnominal := arg;
            End;
          End;
    End;
    34: Begin           // Storages.PITol Read
          If ActiveCircuit[ActiveActor] <> Nil Then
          Begin
            pStorage := ActiveCircuit[ActiveActor].StorageElements.Active;
            If pStorage <> Nil Then
            Begin
              Result := pStorage.myDynVars.CtrlTol * 100;
            End;
          End;
    End;
    35: Begin           // Storages.PITol Write
          If ActiveCircuit[ActiveActor] <> Nil Then
          Begin
            pStorage := ActiveCircuit[ActiveActor].StorageElements.Active;
            If pStorage <> Nil Then
            Begin
              pStorage.myDynVars.CtrlTol := arg / 100;
            End;
          End;
    End;
    36: Begin           // Storages.SafeVoltage Read
          If ActiveCircuit[ActiveActor] <> Nil Then
          Begin
            pStorage := ActiveCircuit[ActiveActor].StorageElements.Active;
            If pStorage <> Nil Then
            Begin
              Result := pStorage.myDynVars.SMThreshold;
            End;
          End;
    End;
    37: Begin           // Storages.SafeVoltage Write
          If ActiveCircuit[ActiveActor] <> Nil Then
          Begin
            pStorage := ActiveCircuit[ActiveActor].StorageElements.Active;
            If pStorage <> Nil Then
            Begin
              pStorage.myDynVars.SMThreshold := arg;
            End;
          End;
    End;
    38: Begin            // Storages.TimeChargeTrig Read
          If ActiveCircuit[ActiveActor] <> Nil Then
          Begin
            pStorage := ActiveCircuit[ActiveActor].StorageElements.Active;
            If pStorage <> Nil Then
            Begin
              Result := pStorage.ChargeTime;
            End;
          End;
    End;
    39: Begin            // Storages.TimeChargeTrig Write
          If ActiveCircuit[ActiveActor] <> Nil Then
          Begin
            pStorage := ActiveCircuit[ActiveActor].StorageElements.Active;
            If pStorage <> Nil Then
            Begin
              pStorage.ChargeTime := arg;
            End;
          End;
    End
    Else
      Result  := -1.0; // The user sent the wrong command code
  end;
End;

// Wrapper for concentating all the string-based IO for the Storage Obj mimicking COM
function StoragesS(mode:longint;arg:pAnsiChar):pAnsiChar;cdecl;
Var
   pStorage       :TStorageObj;
   activesave     :integer;
   S              :String;
   Found          :Boolean;

Begin
  Result:= pAnsiChar(AnsiString('')); // Default return value
  case mode of
    0 : Begin                   // Storages.Name Read
          If ActiveCircuit[ActiveActor] <> Nil Then
          Begin
            pStorage := ActiveCircuit[ActiveActor].StorageElements.Active;
            If pStorage <> Nil Then
            Begin
              Result := pAnsiChar(AnsiString(pStorage.Name));
            End
          End;
    End;
    1 : Begin                   // Storages.Name Write
          IF ActiveCircuit[ActiveActor] <> NIL
          THEN
          Begin      // Search list of Storages in active circuit for name
            WITH ActiveCircuit[ActiveActor].StorageElements DO
            Begin
              S := string(arg);  // Convert to Pascal String
              Found := FALSE;
              ActiveSave := ActiveIndex;
              pStorage := First;
              While pStorage <> NIL Do
              Begin
                IF (CompareText(pStorage.Name, S) = 0)
                THEN
                Begin
                  ActiveCircuit[ActiveActor].ActiveCktElement := pStorage;
                  Found := TRUE;
                  Break;
                End;
                pStorage := Next;
              End;
              IF NOT Found
              THEN
              Begin
                DoSimpleMsg('Storage "'+S+'" Not Found in Active Circuit.', 5003);
                pStorage := Get(ActiveSave);    // Restore active Storage
                ActiveCircuit[ActiveActor].ActiveCktElement := pStorage;
              End;
            End;
          End;
    End
    Else
      Result:=pAnsiChar(AnsiString('Error, parameter not valid'));
  end;
End;

// Wrapper for concentating all the array-like IO structures for the Storage Obj mimicking COM
procedure StoragesV(mode:longint; var myPointer: Pointer; var myType, mySize: longint);cdecl;
Var
  StorageElem :TStorageObj;
  k           :Integer;

Begin
  case mode of
    0 : Begin                   // Storages.AllNames
          myType  :=  4;        // String
          setlength(myStrArray,0);
          IF ActiveCircuit[ActiveActor] <> Nil THEN
          Begin
            WITH ActiveCircuit[ActiveActor] DO
            Begin
              If StorageElements.ListSize>0 Then
              Begin
                k:=0;
                StorageElem := StorageElements.First;
                WHILE StorageElem<>Nil DO
                Begin
                  WriteStr2Array(StorageElem.Name);
                  WriteStr2Array(Char(0));
                  Inc(k);
                  StorageElem := StorageElements.Next;
                End;
              End;
            End;
          End;
          if (length(myStrArray) = 0) then
            WriteStr2Array('None');
          myPointer :=  @(myStrArray[0]);
          mySize    :=  Length(myStrArray);
    End;
    1 : Begin                   // Storages.RegisterNames
          myType  :=  4;        // String
          setlength(myStrArray,0);
          For k := 0 to  NumStorageRegisters - 1  Do
          Begin
             WriteStr2Array(StorageClass[ActiveActor].RegisterNames[k + 1]);
             WriteStr2Array(Char(0));
          End;
          if (length(myStrArray) = 0) then
            WriteStr2Array('None');
          myPointer :=  @(myStrArray[0]);
          mySize    :=  Length(myStrArray);
    End;
    2 : Begin                   // Storages.RegisterValues
          myType    :=  2;      // Double
          setlength(myDBLArray, 1);
          myDBLArray[0] := 0;
          IF ActiveCircuit[ActiveActor] <> Nil THEN
          Begin
            StorageElem :=  TStorageObj(ActiveCircuit[ActiveActor].StorageElements.Active);
            If StorageElem <> Nil Then
            Begin
              setlength(myIntArray, numStorageRegisters);
              FOR k := 0 to numStorageRegisters-1 DO
              Begin
                  myDBLArray[k] := StorageElem.Registers[k+1];
              End;
            End
          End;
          myPointer :=  @(myDBLArray[0]);
          mySize    :=  SizeOf(myDBLArray[0]) * Length(myDBLArray);
    End
    Else
    Begin
      myType  :=  4;        // String
      setlength(myStrArray, 0);
      WriteStr2Array('Error, parameter not recognized');
      myPointer :=  @(myStrArray[0]);
      mySize    :=  Length(myStrArray);
    End;
  end;

End;

end.
