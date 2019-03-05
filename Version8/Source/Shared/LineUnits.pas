unit LineUnits;

 {
  ----------------------------------------------------------
  Copyright (c) 2008-2015, Electric Power Research Institute, Inc.
  All rights reserved.
  ----------------------------------------------------------
}

interface

const
    UNITS_MAXNUM = 9;
    UNITS_NONE = 0;
    UNITS_MILES = 1;
    UNITS_KFT = 2;
    UNITS_KM = 3;
    UNITS_M = 4;
    UNITS_FT = 5;
    UNITS_IN = 6;
    UNITS_CM = 7;
    UNITS_MM = 8;

function GetUnitsCode(const S: String): Integer;
function LineUnitsStr(Units: Integer): String;

// Conversion to and from meters and per meter
function To_Meters(Units: Integer): Double;
function To_per_Meter(Units: Integer): Double;
function From_per_Meter(Units: Integer): Double;
function From_Meters(Units: Integer): Double;

function ConvertLineUnits(FromUnits, ToUnits: Integer): Double;

implementation

uses
    Sysutils;

function GetUnitsCode(const S: String): Integer;

var
    Stest: String;

begin

    Result := 0;
    Stest := Copy(S, 1, 2);  // copy first 2 chars for MOST OF the test
    if CompareText(Stest, 'no') = 0 then
        Result := UNITS_NONE      // no units specified
    else
    if CompareText(Stest, 'mi') = 0 then
        Result := UNITS_MILES      // per mile
    else
    if CompareText(Stest, 'kf') = 0 then
        Result := UNITS_KFT  // per 1000 ft (kft)
    else
    if CompareText(Stest, 'km') = 0 then
        Result := UNITS_KM  // per km
    else
    if CompareText(Stest, 'm') = 0 then
        Result := UNITS_M  // per meter
    else
    if CompareText(Stest, 'me') = 0 then
        Result := UNITS_M  // per meter
    else
    if CompareText(Stest, 'ft') = 0 then
        Result := UNITS_FT
    else
    if CompareText(Stest, 'in') = 0 then
        Result := UNITS_IN
    else
    if CompareText(Stest, 'cm') = 0 then
        Result := UNITS_CM
    else
    if CompareText(Stest, 'mm') = 0 then
        Result := UNITS_MM;
end;


function LineUnitsStr(Units: Integer): String;
begin

    case Units of
        0:
            Result := 'none';
        UNITS_MILES:
            Result := 'mi';
        UNITS_KFT:
            Result := 'kft';
        UNITS_KM:
            Result := 'km';
        UNITS_M:
            Result := 'm';
        UNITS_FT:
            Result := 'ft';
        UNITS_IN:
            Result := 'in';
        UNITS_CM:
            Result := 'cm';
        UNITS_MM:
            Result := 'mm';
    else
        Result := 'none';
    end;
end;

function To_Meters(Units: Integer): Double;
begin

    case Units of
        UNITS_MILES:
            Result := 1609.3;
        UNITS_KFT:
            Result := 304.8;
        UNITS_KM:
            Result := 1000.0;
        UNITS_M:
            Result := 1.0;
        UNITS_FT:
            Result := 0.3048;
        UNITS_IN:
            Result := 0.0254;
        UNITS_CM:
            Result := 0.01;
        UNITS_MM:
            Result := 0.001;
    else
        Result := 1.0;
    end;
end;


function To_per_Meter(Units: Integer): Double;
begin
    Result := 1.0 / To_Meters(Units);
end;

function From_per_Meter(Units: Integer): Double;
begin
    Result := To_Meters(Units);
end;

function From_Meters(Units: Integer): Double;
begin
    Result := 1.0 / To_Meters(Units);
end;

function ConvertLineUnits(FromUnits, ToUnits: Integer): Double;
begin
    if ((FromUnits = UNITS_NONE) or (ToUnits = UNITS_NONE)) then
        Result := 1.0 // Don't know what to convert
    else
        Result := From_Meters(ToUnits) * To_Meters(FromUnits);
end;

end.
