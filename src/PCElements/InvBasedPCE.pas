unit InvBasedPCE;

// InvBasedPCE is an abstract class for grouping inverter based functions.
// In the upstream OpenDSS, this is included in the base PCElement, 
// but it feels better to create a dedicated class, even in the restrictive
// environment of the Object Pascal language.

interface

uses
    DSSClass,
    DynEqPCE,
    InvDynamics;

type
    TInvBasedPCEClass = class(TDynEqPCEClass)
    PROTECTED
        // procedure DefineProperties; override;

    PUBLIC
        constructor Create(dssContext: TDSSContext; DSSClsType: Integer; DSSClsName: String);
        destructor Destroy; OVERRIDE;
    end;

    TInvBasedPCE = class(TDynEqPCE)
    PUBLIC
        dynVars: TInvDynamicVars;
        GFM_Mode: LongBool; // To indicate if the PCE (normally IBR) is working in Grod forming inverter mode
        InverterON: Boolean;
        varMode: Integer; // 0=constant PF; 1=kvar specified
        VWMode: Boolean; //boolean indicating if under volt-watt control mode from InvControl (not ExpControl)
        VVMode: Boolean; //boolean indicating if under volt-var mode from InvControl
        WVMode: Boolean; //boolean indicating if under watt-var mode from InvControl
        WPMode: Boolean; //boolean indicating if under watt-pf mode from InvControl
        DRCMode: Boolean; //boolean indicating if under DRC mode from InvControl
        AVRMode: Boolean; //boolean indicating whether under AVR mode from ExpControl (or InvControl, but that does not seem to be implemented yet)

        constructor Create(ParClass: TDSSClass);
        destructor Destroy; OVERRIDE;

        function IsPVSystem(): Boolean; virtual;
        function IsStorage(): Boolean; virtual;
        function GetPFPriority(): Boolean; virtual;
        procedure SetPFPriority(value: Boolean); virtual; abstract;
        function CheckOLInverter(): Boolean; virtual; abstract;
        function UsingCIMDynamics(): Boolean;
    end;

implementation

uses
    PCElement,
    DSSClassDefs,
    DSSGlobals,
    Utilities,
    DSSHelper;

constructor TInvBasedPCEClass.Create(dssContext: TDSSContext; DSSClsType: Integer; DSSClsName: String);
begin
    inherited Create(dssContext, DSSClsType, DSSClsName);
end;

destructor TInvBasedPCEClass.Destroy;
begin
    inherited Destroy;
end;

constructor TInvBasedPCE.Create(ParClass: TDSSClass);
begin
    inherited Create(ParClass);

    GFM_Mode := FALSE;
end;

destructor TInvBasedPCE.Destroy;
begin
    inherited Destroy;
end;

function TInvBasedPCE.IsPVSystem(): Boolean;
begin
    Result := False;
end;

function TInvBasedPCE.IsStorage(): Boolean;
begin
    Result := False;
end;

function TInvBasedPCE.GetPFPriority(): Boolean;
begin
    Result := False;
end;

function TInvBasedPCE.UsingCIMDynamics(): Boolean;
begin
    Result := VWMode or VVMode or WVMode or AVRMode or DRCMode; // WPMode not in CIM Dynamics
end;


end.