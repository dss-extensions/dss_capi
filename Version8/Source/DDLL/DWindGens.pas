unit DWindGens;

interface

function WindGensI(mode:longint;arg:longint):longint;cdecl;
function WindGensF(mode:longint;arg:double):double;cdecl;
function WindGensS(mode:longint;arg:pAnsiChar):pAnsiChar;cdecl;
procedure WindGensV(mode:longint; var myPointer: Pointer; var myType, mySize: longint);cdecl;

implementation

uses {$IFNDEF FPC_DLL}ComServ, {$ENDIF} DSSGlobals, WindGen, Variants, Pointerlist, Sysutils;

// Wrapper for concentating all the integer-based IO for the WindGen Obj mimicking COM
function WindGensI(mode:longint;arg:longint):longint;cdecl;
Var
  WindGenElem   :TWindGenObj;
  pList         :TPointerList;

Begin
  Result:=0; // Default return value
  case mode of
    0 : Begin           // WindGens.First
          If ActiveCircuit[ActiveActor] <> Nil Then
          Begin
            If WindGenClass[ActiveActor].ElementList.ListSize > 0 Then
            Begin
              pList := WindGenClass[ActiveActor].ElementList;
              WindGenElem := pList.First;
              Repeat
                If WindGenElem.Enabled
                Then Begin
                  ActiveCircuit[ActiveActor].ActiveCktElement := WindGenElem;
                  Result := 1;
                End
                Else WindGenElem := pList.Next;
              Until (Result = 1) or (WindGenElem = nil);
            End
            Else
                Result := 0;  // signify no more
          End;
    End;
    1 : Begin           // WindGens.Next
          If ActiveCircuit[ActiveActor] <> Nil Then
          Begin
            If WindGenClass[ActiveActor].ElementList.ListSize > 0 Then
            Begin
              pList := WindGenClass[ActiveActor].ElementList;
              WindGenElem := pList.First;
              Repeat
                If WindGenElem.Enabled
                Then
                Begin
                  ActiveCircuit[ActiveActor].ActiveCktElement := WindGenElem;
                  Result := pList.ActiveIndex;
                End
                Else WindGenElem := pList.Next;
              Until (Result > 0) or (WindGenElem = nil);
            End
            Else
              Result := 0;  // signify no more
          End;
    End;
    2 : Begin           // WindGens.Count
          If Assigned(ActiveCircuit[ActiveActor]) Then
            Result := WindGenClass[ActiveActor].ElementList.ListSize;
    End;
    3 : Begin           // WindGens.Idx Read
          if ActiveCircuit[ActiveActor] <> Nil then
             Result := WindGenClass[ActiveActor].ElementList.ActiveIndex
    End;
    4 : Begin           // WindGens.Idx Write
          if ActiveCircuit[ActiveActor] <> Nil then
          Begin
            WindGenElem := WindGenClass[ActiveActor].ElementList.Get(arg);
            If WindGenElem <> Nil Then ActiveCircuit[ActiveActor].ActiveCktElement := WindGenElem;
          End;
    End;
    5 : Begin           // WindGens.N_WTG Read
          WindGenElem := WindGenClass[ActiveActor].GetActiveObj;
          if WindGenElem <> nil then
          Begin
            Result := WindGenElem.WindModelDyn.N_WTG
          End;
    End;
    6 : Begin           // WindGens.N_WTG Write
          WindGenElem := WindGenClass[ActiveActor].GetActiveObj;
          if WindGenElem <> nil then
          Begin
            WindGenElem.WindModelDyn.N_WTG := arg
          End;
    End;
    7 : Begin           // WindGens.NPoles Read
          WindGenElem := WindGenClass[ActiveActor].GetActiveObj;
          if WindGenElem <> nil then
          Begin
            Result := Trunc(WindGenElem.WindGenVars.Poles)
          End;
    End;
    8 : Begin           // WindGens.NPoles Write
          WindGenElem := WindGenClass[ActiveActor].GetActiveObj;
          if WindGenElem <> nil then
          Begin
            WindGenElem.WindGenVars.Poles := arg
          End;
    End;
    9 : Begin           // WindGens.QFlag Read
          WindGenElem := WindGenClass[ActiveActor].GetActiveObj;
          if WindGenElem <> nil then
          Begin
            Result := WindGenElem.WindModelDyn.QFlg
          End;
    End;
    10: Begin           // WindGens.QFlag Write
          WindGenElem := WindGenClass[ActiveActor].GetActiveObj;
          if WindGenElem <> nil then
          Begin
            WindGenElem.WindModelDyn.QFlg := arg
          End;
    End;
    11: Begin           // WindGens.QMode Read
          WindGenElem := WindGenClass[ActiveActor].GetActiveObj;
          if WindGenElem <> nil then
          Begin
            Result := WindGenElem.WindModelDyn.QMode
          End;
    End;
    12: Begin           // WindGens.QMode Write
          WindGenElem := WindGenClass[ActiveActor].GetActiveObj;
          if WindGenElem <> nil then
          Begin
            WindGenElem.WindModelDyn.QMode := arg
          End;
    End
    Else
      Result := -1; // The user is asking for the wrong command
  end;

End;


// Wrapper for concentating all the double-based IO for the WindGen Obj mimicking COM
function WindGensF(mode:longint;arg:double):double;cdecl;
Var
  WindGenElem   :TWindGenObj;

Begin
  Result:=0; // Default return value
  case mode of
    0 : Begin           // WindGens.Ag Read
          WindGenElem := WindGenClass[ActiveActor].GetActiveObj;
          if WindGenElem <> nil then
          Begin
            Result := WindGenElem.WindGenVars.ag
          End;
    End;
    1 : Begin           // WindGens.Ag Write
          WindGenElem := WindGenClass[ActiveActor].GetActiveObj;
          if WindGenElem <> nil then
          Begin
            WindGenElem.WindGenVars.ag  := arg
          End;
    End;
    2 : Begin           // WindGens.Cp Read
          WindGenElem := WindGenClass[ActiveActor].GetActiveObj;
          if WindGenElem <> nil then
          Begin
            Result := WindGenElem.WindGenVars.Cp
          End;
    End;
    3 : Begin           // WindGens.Cp Write
          WindGenElem := WindGenClass[ActiveActor].GetActiveObj;
          if WindGenElem <> nil then
          Begin
            WindGenElem.WindGenVars.Cp := arg
          End;
    End;
    4 : Begin           // WindGens.kV Read
          WindGenElem := WindGenClass[ActiveActor].GetActiveObj;
          if WindGenElem <> nil then
          Begin
            Result := WindGenElem.PresentkV
          End;
    End;
    5 : Begin           // WindGens.kV Write
          WindGenElem := WindGenClass[ActiveActor].GetActiveObj;
          if WindGenElem <> nil then
          Begin
            WindGenElem.PresentkV := arg
          End;
    End;
    6 : Begin           // WindGens.kVA Read
          WindGenElem := WindGenClass[ActiveActor].GetActiveObj;
          if WindGenElem <> nil then
          Begin
            Result := WindGenElem.WindGenVars.kVArating
          End;
    End;
    7 : Begin           // WindGens.kVA Write
          WindGenElem := WindGenClass[ActiveActor].GetActiveObj;
          if WindGenElem <> nil then
          Begin
            WindGenElem.WindGenVars.kVArating := arg;
            WindGenElem.WindModelDyn.EditProp(13,floattostr(arg));
          End;
    End;
    8 : Begin           // WindGens.kvar Read
          WindGenElem := WindGenClass[ActiveActor].GetActiveObj;
          if WindGenElem <> nil then
          Begin
            Result := WindGenElem.Presentkvar
          End;
    End;
    9 : Begin           // WindGens.kvar Write
          WindGenElem := WindGenClass[ActiveActor].GetActiveObj;
          if WindGenElem <> nil then
          Begin
            WindGenElem.Presentkvar := arg
          End;
    End;
    10: Begin           // WindGens.kW Read
          WindGenElem := WindGenClass[ActiveActor].GetActiveObj;
          if WindGenElem <> nil then
          Begin
            Result := WindGenElem.PresentkW
          End;
    End;
    11: Begin           // WindGens.kW Write
          WindGenElem := WindGenClass[ActiveActor].GetActiveObj;
          if WindGenElem <> nil then
          Begin
            WindGenElem.PresentkW := arg
          End;
    End;
    12: Begin           // WindGens.Lamda Read
          WindGenElem := WindGenClass[ActiveActor].GetActiveObj;
          if WindGenElem <> nil then
          Begin
            Result := WindGenElem.WindGenVars.Lamda
          End;
    End;
    13: Begin           // WindGens.Lamda Write
          WindGenElem := WindGenClass[ActiveActor].GetActiveObj;
          if WindGenElem <> nil then
          Begin
            WindGenElem.WindGenVars.Lamda := arg
          End;
    End;
    14: Begin           // WindGens.pd Read
          WindGenElem := WindGenClass[ActiveActor].GetActiveObj;
          if WindGenElem <> nil then
          Begin
            Result := WindGenElem.WindGenVars.pd
          End;
    End;
    15: Begin           // WindGens.pd Write
          WindGenElem := WindGenClass[ActiveActor].GetActiveObj;
          if WindGenElem <> nil then
          Begin
            WindGenElem.WindGenVars.pd  := arg
          End;
    End;
    16: Begin           // WindGens.PF Read
          WindGenElem := WindGenClass[ActiveActor].GetActiveObj;
          if WindGenElem <> nil then
          Begin
            Result := WindGenElem.PFNominal
          End;
    End;
    17: Begin           // WindGens.PF Write
          WindGenElem := WindGenClass[ActiveActor].GetActiveObj;
          if WindGenElem <> nil then
          Begin
            WindGenElem.PFNominal := arg
          End;
    End;
    18: Begin           // WindGens.Pss Read
          WindGenElem := WindGenClass[ActiveActor].GetActiveObj;
          if WindGenElem <> nil then
          Begin
            Result := WindGenElem.WindModelDyn.Pss
          End;
    End;
    19: Begin           // WindGens.Pss Write
          WindGenElem := WindGenClass[ActiveActor].GetActiveObj;
          if WindGenElem <> nil then
          Begin
            WindGenElem.WindModelDyn.Pss := arg
          End;
    End;
    20: Begin           // WindGens.Qss Read
          WindGenElem := WindGenClass[ActiveActor].GetActiveObj;
          if WindGenElem <> nil then
          Begin
            Result := WindGenElem.WindModelDyn.Qss
          End;
    End;
    21: Begin           // WindGens.Qss Write
          WindGenElem := WindGenClass[ActiveActor].GetActiveObj;
          if WindGenElem <> nil then
          Begin
            WindGenElem.WindModelDyn.Qss := arg
          End;
    End;
    22: Begin           // WindGens.Rad Read
          WindGenElem := WindGenClass[ActiveActor].GetActiveObj;
          if WindGenElem <> nil then
          Begin
            Result := WindGenElem.WindGenVars.Rad
          End;
    End;
    23: Begin           // WindGens.Rad Write
          WindGenElem := WindGenClass[ActiveActor].GetActiveObj;
          if WindGenElem <> nil then
          Begin
            WindGenElem.WindGenVars.Rad := arg
          End;
    End;
    24: Begin           // WindGens.RThev Read
          WindGenElem := WindGenClass[ActiveActor].GetActiveObj;
          if WindGenElem <> nil then
          Begin
            Result := WindGenElem.WindModelDyn.Rthev
          End;
    End;
    25: Begin           // WindGens.RThev Write
          WindGenElem := WindGenClass[ActiveActor].GetActiveObj;
          if WindGenElem <> nil then
          Begin
            WindGenElem.WindModelDyn.Rthev := arg
          End;
    End;
    26: Begin           // WindGens.VCutOut Read
          WindGenElem := WindGenClass[ActiveActor].GetActiveObj;
          if WindGenElem <> nil then
          Begin
            Result := WindGenElem.WindGenVars.VCutout
          End;
    End;
    27: Begin           // WindGens.VCutOut Write
          WindGenElem := WindGenClass[ActiveActor].GetActiveObj;
          if WindGenElem <> nil then
          Begin
            WindGenElem.WindGenVars.VCutout := arg
          End;
    End;
    28: Begin           // WindGens.VCutIn Read
          WindGenElem := WindGenClass[ActiveActor].GetActiveObj;
          if WindGenElem <> nil then
          Begin
            Result := WindGenElem.WindGenVars.VCutin
          End;
    End;
    29: Begin           // WindGens.VCutIn Write
          WindGenElem := WindGenClass[ActiveActor].GetActiveObj;
          if WindGenElem <> nil then
          Begin
            WindGenElem.WindGenVars.VCutin := arg
          End;
    End;
    30: Begin           // WindGens.Vss Read
          WindGenElem := WindGenClass[ActiveActor].GetActiveObj;
          if WindGenElem <> nil then
          Begin
            Result := WindGenElem.WindModelDyn.Vss
          End;
    End;
    31: Begin           // WindGens.Vss Write
          WindGenElem := WindGenClass[ActiveActor].GetActiveObj;
          if WindGenElem <> nil then
          Begin
            WindGenElem.WindModelDyn.Vss := arg
          End;
    End;
    32: Begin           // WindGens.WindSpeed Read
          WindGenElem := WindGenClass[ActiveActor].GetActiveObj;
          if WindGenElem <> nil then
          Begin
            Result := WindGenElem.WindModelDyn.vwind
          End;
    End;
    33: Begin           // WindGens.WindSpeed Write
          WindGenElem := WindGenClass[ActiveActor].GetActiveObj;
          if WindGenElem <> nil then
          Begin
            WindGenElem.WindModelDyn.vwind := arg
          End;
    End;
    34: Begin           // WindGens.XThev Read
          WindGenElem := WindGenClass[ActiveActor].GetActiveObj;
          if WindGenElem <> nil then
          Begin
            Result := WindGenElem.WindModelDyn.Xthev
          End;
    End;
    35: Begin           // WindGens.XThev Write
          WindGenElem := WindGenClass[ActiveActor].GetActiveObj;
          if WindGenElem <> nil then
          Begin
            WindGenElem.WindModelDyn.Xthev := arg
          End;
    End
    Else
      Result := -1.0; // We got the wrong command
  end;

End;

// Wrapper for concentating all the string-based IO for the WindGen Obj mimicking COM
function WindGensS(mode:longint;arg:pAnsiChar):pAnsiChar;cdecl;
Var
  WindGenElem   :TWindGenObj;
  k             :Integer;
  pList         :TPointerList;
  activesave    :integer;
  S             : String;
  Found         :Boolean;

Begin
  Result:= pAnsiChar(AnsiString('')); // Default return value
  case mode of
    0 : Begin                   // WindGen.Name Read
          WindGenElem := WindGenClass[ActiveActor].GetActiveObj;
          if WindGenElem <> nil then
          Begin
            Result := pAnsiChar(AnsiString(WindGenElem.Name))
          End;
    End;
    1 : Begin                   // WindGen.Name Write
          IF ActiveCircuit[ActiveActor] <> NIL
          THEN
          Begin      // Search list of Storages in active circuit for name
            If WindGenClass[ActiveActor].ElementList.ListSize > 0 Then
            Begin
              S := string(arg);  // Convert to Pascal String
              Found := FALSE;
              pList := WindGenClass[ActiveActor].ElementList;
              activesave :=  pList.ActiveIndex;
              WindGenElem := pList.First;
              While WindGenElem <> NIL Do
              Begin
                IF (CompareText(WindGenElem.Name, S) = 0)
                THEN
                Begin
                  ActiveCircuit[ActiveActor].ActiveCktElement := WindGenElem;
                  Found := TRUE;
                  Break;
                End;
                WindGenElem := pList.Next;
              End;
              IF NOT Found
              THEN
              Begin
                DoSimpleMsg('WindGen "'+S+'" Not Found in Active Circuit.', 20003);
                WindGenElem := pList.Get(activesave);    // Restore active Storage
                ActiveCircuit[ActiveActor].ActiveCktElement := WindGenElem;
              End;
            End;
          End;
    End;
  end;
End;

// Wrapper for concentating all the array-like IO structures for the Windgen Obj mimicking COM
procedure WindGensV(mode:longint; var myPointer: Pointer; var myType, mySize: longint);cdecl;
Var
  WindGenElem   :TWindGenObj;
  k             :Integer;
  pList         :TPointerList;

Begin
  case mode of
    0 : Begin                   // WindGen.AllNames
          myType  :=  4;        // String
          setlength(myStrArray,0);
          IF ActiveCircuit[ActiveActor] <> Nil THEN
          Begin
            WITH ActiveCircuit[ActiveActor] DO
            Begin

              If WindGenClass[ActiveActor].ElementList.ListSize > 0 Then
              Begin
                pList := WindGenClass[ActiveActor].ElementList;
                k:=0;
                WindGenElem := pList.First;
                WHILE WindGenElem<>Nil DO
                Begin
                  WriteStr2Array(WindGenElem.Name);
                  WriteStr2Array(Char(0));
                  Inc(k);
                  WindGenElem := pList.Next;
                End;
              End;

            End;
          End;
          if (length(myStrArray) = 0) then
            WriteStr2Array('None');
          myPointer :=  @(myStrArray[0]);
          mySize    :=  Length(myStrArray);
    End;
    1 : Begin                   // WindGen.RegisterNames
          myType  :=  4;        // String
          setlength(myStrArray,0);
          For k := 0 to  NumWGenRegisters - 1  Do
          Begin
             WriteStr2Array(WindGenClass[ActiveActor].RegisterNames[k + 1]);
             WriteStr2Array(Char(0));
          End;
          if (length(myStrArray) = 0) then
            WriteStr2Array('None');
          myPointer :=  @(myStrArray[0]);
          mySize    :=  Length(myStrArray);
    End;
    2 : Begin                   // WindGen.RegisterValues
          myType    :=  2;      // Double
          setlength(myDBLArray, 1);
          myDBLArray[0] := 0;
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
