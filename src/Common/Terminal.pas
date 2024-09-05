unit Terminal;

// ----------------------------------------------------------
// Copyright (c) 2008-2015, Electric Power Research Institute, Inc.
// All rights reserved.
// ----------------------------------------------------------

// Definition of classes for all terminals of a DSS element

interface

uses
    Arraydef;

type
    TPowerTerminal = object
    PUBLIC
        BusRef: Integer;
        TermNodeRef: array of Integer;   // Need to get to this fast
        ConductorsClosed: array of Boolean;
        procedure Init(Ncond: Integer);
    end;

    TerminalArray = array of TPowerTerminal;

//   Control Terminal is managed by override functions in classes that are derived from this class

implementation

uses
    SysUtils;

procedure TPowerTerminal.Init(Ncond: Integer);
var
    i: Integer;
begin
    BusRef := -1; // signify not set
    SetLength(TermNodeRef, NCond);
    SetLength(ConductorsClosed, NCond);
    for i := 1 to NCond do
        ConductorsClosed[i - 1] := True;
end;

end.
