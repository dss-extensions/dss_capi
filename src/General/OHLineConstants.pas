unit OHLineConstants;

{
  ----------------------------------------------------------
  Copyright (c) 2008-2015, Electric Power Research Institute, Inc.
  All rights reserved.
  ----------------------------------------------------------
}

// Manages the geometry data and calculates the impedance matrices for an overhead line
interface

uses
    Arraydef,
    Ucmatrix,
    UComplex, DSSUcomplex,
    LineUnits,
    LineConstants;

type
    TOHLineConstants = class(TLineConstants)
    PRIVATE

    PROTECTED

    PUBLIC
        constructor Create(NumConductors: Integer);
        destructor Destroy; OVERRIDE;
    end;

implementation

constructor TOHLineConstants.Create(NumConductors: Integer);
begin
    inherited Create(NumConductors);
end;

destructor TOHLineConstants.Destroy;
begin
    inherited;
end;

end.
