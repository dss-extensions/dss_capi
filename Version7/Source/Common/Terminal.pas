unit Terminal;

{
  ----------------------------------------------------------
  Copyright (c) 2008-2015, Electric Power Research Institute, Inc.
  All rights reserved.
  ----------------------------------------------------------
}

{ Definition of classes for all terminals of a DSS element}

interface

uses
    Conductor,
    Arraydef;

type
    TPowerTerminal = class(TObject)
    PRIVATE
        FNumCond: Integer;
        ActiveConductor: Integer;
        procedure Set_ActiveConductor(Value: Integer);
    PUBLIC
        BusRef: Integer;
        TermNodeRef: pIntegerArray;   // Need to get to this fast
        Conductors: pTConductorArray;
        Checked: Boolean;
        constructor Create(Ncond: Integer);
        destructor Destroy; OVERRIDE;
        property Conductor: Integer READ ActiveConductor WRITE set_ActiveConductor;
    end;

    pTerminalList = ^TerminalList;
    TerminalList = array[1..3] of TPowerTerminal;

  {
   Control Terminal is managed by override functions in classes that are derived from this class
  }

implementation

uses
    SysUtils;


{TPowerTerminal}

constructor TPowerTerminal.Create(Ncond: Integer);
var
    i: Integer;
begin
    inherited Create;
    FNumCond := NCond;
    BusRef := -1; // signify not set
    TermNodeRef := AllocMem(SizeOf(TermNodeRef^[1]) * FNumCond);
    Conductors := AllocMem(SizeOf(Conductors^[1]) * FNumCond);
    for i := 1 to FNumCond do
        Conductors^[i] := Tconductor.Create;
    ActiveConductor := 1;
end;

destructor TPowerTerminal.Destroy;

var
    i: Integer;

begin
    for i := 1 to FNumCond do
        Conductors^[i].Free;
    Reallocmem(Conductors, 0);
    Reallocmem(TermNodeRef, 0);
    inherited  Destroy;
end;

//- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
procedure TPowerTerminal.Set_ActiveConductor(value: Integer);
begin
    if (Value > 0) and (Value <= FNumCond) then
        ActiveConductor := Value;
end;


end.
