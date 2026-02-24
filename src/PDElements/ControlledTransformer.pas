unit ControlledTransformer;

interface

uses 
    PDElement,
    DSSClass,
    DSSUcomplex,
    CAPI_Types;

type
    TControlledTransformerObj = class (TPDElement)
    public
        NumWindings: Integer;
        BHPoints: Integer;
        BHCurrent: PDoubleArray;
        BHFlux: PDoubleArray;

        function RotatePhases(iPhs: Integer): Integer; virtual; abstract;
        procedure GetWindingVoltages(iWind: Integer; VBuffer: pComplexArray); virtual; abstract;
        procedure GetAllWindingCurrents(CurrBuffer: pComplexArray); virtual; abstract;
        function TapPosition(iWind: Integer): Integer;

        function PresentTap(i: Integer): Double; virtual; abstract;
        procedure SetPresentTap(i: Integer; const Value: Double); virtual; abstract;
        function MinTap(i: Integer): Double; virtual; abstract;
        function MaxTap(i: Integer): Double; virtual; abstract;
        function TapIncrement(i: Integer): Double; virtual; abstract;
        function NumTaps(i: Integer): Integer; virtual; abstract;
        function WdgConnection(i: Integer): Integer; virtual; abstract;
        function BaseVoltage(i: Integer): Double; virtual; abstract;

        constructor Create(ParClass: TDSSClass; objName: String);
        destructor Destroy; override;
        procedure MakeLike(OtherPtr: Pointer); override;
    end;

implementation

type
    TObj = TControlledTransformerObj;

constructor TControlledTransformerObj.Create(ParClass: TDSSClass; objName: String);
begin
    inherited Create(ParClass, objName);

    BHPoints := 0;
    BHCurrent := NIL;
    BHFlux := NIL;
end;

procedure TControlledTransformerObj.MakeLike(OtherPtr: Pointer);
var
    Other: TObj;
    i: Integer;
begin
    inherited MakeLike(OtherPtr);

    Other := TObj(OtherPtr);

    BHPoints := Other.BHPoints;
    BHCurrent := AllocMem(SizeOf(Double) * BHPoints);
    BHFlux := AllocMem(SizeOf(Double) * BHPoints);
    for i := 1 to BHPoints do
        BHCurrent[i] := Other.BHCurrent[i];
    for i := 1 to BHPoints do
        BHFlux[i] := Other.BHFlux[i];
end;

destructor TControlledTransformerObj.Destroy;
begin
    Reallocmem(BHCurrent, 0);
    Reallocmem(BHFlux, 0);

    inherited Destroy;
end;

function TControlledTransformerObj.TapPosition(iWind: Integer): Integer;
// Assumes 0  is 1.0 per unit tap
begin
    Result := Round((PresentTap(iWind) - (MaxTap(iWind) + MinTap(iWind)) / 2.0) / TapIncrement(iWind));
end;


end.