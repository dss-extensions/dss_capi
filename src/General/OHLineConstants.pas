unit OHLineConstants;

// ----------------------------------------------------------
// Copyright (c) 2008-2015, Electric Power Research Institute, Inc.
// All rights reserved.
// ----------------------------------------------------------

// Manages the geometry data and calculates the impedance matrices for an overhead line
interface

uses
    CAPI_Types,
    Ucmatrix,
    UComplex, DSSUcomplex,
    LineUnits,
    LineConstants;

type
    TOHLineConstants = class(TLineConstants)
    PUBLIC
        constructor Create(NConductors: Integer);
        destructor Destroy; OVERRIDE;
    end;

implementation

constructor TOHLineConstants.Create(NConductors: Integer);
begin
    inherited Create(NConductors);
end;

destructor TOHLineConstants.Destroy;
begin
    inherited;
end;

end.
