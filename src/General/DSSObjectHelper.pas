unit DSSObjectHelper;

interface

uses 
    DSSObject,
    DSSClass,
    ParserDel,
    Circuit,
    ArrayDef;

type
    TDSSClassHelper = class helper for TDSSClass
    private
        function GetAuxParser: TParser; inline;
        function GetCircuit: TDSSCircuit; inline;
        function GetParser: TParser; inline;
        function GetDSSObject: TDSSObject; inline;
    protected
        property ActiveCircuit: TDSSCircuit read GetCircuit;
        property ActiveDSSObject: TDSSObject read GetDSSObject;
        property AuxParser: TParser read GetAuxParser;
        property Parser: TParser read GetParser;
    end;

    TDSSObjectHelper = class helper for TDSSObject
    private
        function GetAuxParser: TParser; inline;
        function GetCircuit: TDSSCircuit; inline;
        function GetParser: TParser; inline;
    protected
        property ActiveCircuit: TDSSCircuit read GetCircuit;
        property AuxParser: TParser read GetAuxParser;
        property Parser: TParser read GetParser;
    public
        function InterpretDblArray(const s: String; MaxValues: Integer; ResultArray: pDoubleArray): Integer;inline;
        function InterpretIntArray(const s: String; MaxValues: Integer; ResultArray: pIntegerArray): Integer;inline;
    end;

implementation

uses
    DSSHelper,
    Utilities;
        
function TDSSClassHelper.GetCircuit: TDSSCircuit;
begin
    Result := DSS.ActiveCircuit;
end;

function TDSSClassHelper.GetParser: TParser; inline;
begin
    Result := DSS.Parser;
end;

function TDSSClassHelper.GetAuxParser: TParser; inline;
begin
    Result := DSS.AuxParser;
end;


function TDSSClassHelper.GetDSSObject: TDSSObject; inline;
begin
    Result := DSS.ActiveDSSObject;
end;

function TDSSObjectHelper.GetCircuit: TDSSCircuit;
begin
    Result := DSS.ActiveCircuit;
end;

function TDSSObjectHelper.GetAuxParser: TParser; inline;
begin
    Result := DSS.AuxParser;
end;

function TDSSObjectHelper.GetParser: TParser; inline;
begin
    Result := DSS.Parser;
end;

function TDSSObjectHelper.InterpretDblArray(const s: String; MaxValues: Integer; ResultArray: pDoubleArray): Integer;inline;
begin
    Result := Utilities.InterpretDblArray(DSS, s, MaxValues, ResultArray)
end;

function TDSSObjectHelper.InterpretIntArray(const s: String; MaxValues: Integer; ResultArray: pIntegerArray): Integer;inline;
begin
    Result := Utilities.InterpretIntArray(DSS, s, MaxValues, ResultArray)
end;


end.