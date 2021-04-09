unit ImplStorages;

{$WARN SYMBOL_PLATFORM OFF}

interface

uses
  ComObj, ActiveX, OpenDSSengine_TLB, StdVcl;

type
  TStorages = class(TAutoObject, IStorages)
  protected
    function Get_AllNames: OleVariant; safecall;

  end;

implementation

uses ComServ, DSSGlobals, Storage, Variants, Sysutils;

function TStorages.Get_AllNames: OleVariant;
Var
  StorageElem:TStorageObj;
  k:Integer;

Begin
    Result := VarArrayCreate([0, 0], varOleStr);
    Result[0] := 'NONE';
    IF ActiveCircuit[ActiveActor] <> Nil THEN
     WITH ActiveCircuit[ActiveActor] DO
     If StorageElements.ListSize>0 Then
     Begin
       VarArrayRedim(result, StorageElements.ListSize-1);
       k:=0;
       StorageElem := StorageElements.First;
       WHILE StorageElem<>Nil DO  Begin
          Result[k] := StorageElem.Name;
          Inc(k);
          StorageElem := StorageElements.Next;
       End;
     End;

end;

initialization
  TAutoObjectFactory.Create(ComServer, TStorages, Class_Storages,
    ciInternal, tmApartment);
end.
