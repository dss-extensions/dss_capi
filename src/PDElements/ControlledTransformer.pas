unit ControlledTransformer;

interface

uses 
    PDElement,
    DSSClass,
    DSSUcomplex;

type
    TControlledTransformerObj = class (TPDElement)
    public
        NumWindings: Integer;

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

        constructor Create(ParClass: TDSSClass);
        destructor Destroy; override;
    end;

implementation

constructor TControlledTransformerObj.Create(ParClass: TDSSClass);
begin
    inherited Create(ParClass);
end;

destructor TControlledTransformerObj.Destroy;
begin
    inherited Destroy;
end;

function TControlledTransformerObj.TapPosition(iWind: Integer): Integer;
// Assumes 0  is 1.0 per unit tap
begin
    Result := Round((PresentTap(iWind) - (MaxTap(iWind) + MinTap(iWind)) / 2.0) / TapIncrement(iWind));
end;


end.