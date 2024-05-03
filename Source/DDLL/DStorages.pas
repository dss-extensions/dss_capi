unit DStorages;

interface

function StoragesI(mode:longint;arg:longint):longint;cdecl;
function StoragesF(mode:longint;arg:double):double;cdecl;
function StoragesS(mode:longint;arg:pAnsiChar):pAnsiChar;cdecl;
procedure StoragesV(mode:longint; var myPointer: Pointer; var myType, mySize: longint);cdecl;

implementation

uses {$IFNDEF FPC_DLL}ComServ, {$ENDIF}Storage, Variants, PointerList, DSSGlobals, CktElement;

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

    End;
  end;
End;

// Wrapper for concentating all the string-based IO for the Storage Obj mimicking COM
function StoragesS(mode:longint;arg:pAnsiChar):pAnsiChar;cdecl;
Begin

End;

// Wrapper for concentating all the array-like IO structures for the Storage Obj mimicking COM
procedure StoragesV(mode:longint; var myPointer: Pointer; var myType, mySize: longint);cdecl;
Begin

End;

end.
