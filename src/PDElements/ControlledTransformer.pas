unit ControlledTransformer;

interface

uses 
    PDElement,
    DSSClass,
    DSSUcomplex;

type
    TControlledTransformerObj = class (TPDElement)
    protected
        function Get_PresentTap(i: Integer): Double; virtual; abstract;
        procedure Set_PresentTap(i: Integer; const Value: Double); virtual; abstract;
        function Get_MinTap(i: Integer): Double; virtual; abstract;
        function Get_MaxTap(i: Integer): Double; virtual; abstract;
        function Get_TapIncrement(i: Integer): Double; virtual; abstract;
        function Get_NumTaps(i: Integer): Integer; virtual; abstract;
        function Get_WdgConnection(i: Integer): Integer; virtual; abstract;
        function Get_BaseVoltage(i: Integer): Double; virtual; abstract;
    public
        NumWindings: Integer;

        function RotatePhases(iPhs: Integer): Integer; virtual; abstract;
        procedure GetWindingVoltages(iWind: Integer; VBuffer: pComplexArray); virtual; abstract;
        procedure GetAllWindingCurrents(CurrBuffer: pComplexArray); virtual; abstract;
        function TapPosition(iWind: Integer): Integer;

        property PresentTap[i: Integer]: Double READ Get_PresentTap WRITE Set_PresentTap;
        property Mintap[i: Integer]: Double READ Get_MinTap;
        property Maxtap[i: Integer]: Double READ Get_MaxTap;
        property TapIncrement[i: Integer]: Double READ Get_TapIncrement;
        property NumTaps[i: Integer]: Integer READ Get_NumTaps;
        property WdgConnection[i: Integer]: Integer READ Get_WdgConnection;
        property BaseVoltage[i: Integer]: Double READ Get_BaseVoltage;

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
    Result := Round((PresentTap[iWind] - (Maxtap[iWind] + Mintap[iWind]) / 2.0) / TapIncrement[iWind]);
end;


end.