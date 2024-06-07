unit Generic5Helper;

// Copyright (c) 2024 DSS-Extensions contributors

// A class helper added to simplify the integration between Generic5 and FMonitor.

interface

uses 
    Generic5OrderMach,
    FMonitor;

type
    TGeneric5ObjHelper = class helper for TGeneric5Obj
    private
        function Get_FMonObj(): TFMonitorObj;
        function Get_FMonObj2(): TFMonitorObj;
        procedure Set_FMonObj(value: TFMonitorObj);
        procedure Set_FMonObj2(value: TFMonitorObj);
    public
        property FMonObj: TFMonitorObj read Get_FMonObj write Set_FMonObj;
        property FMonObj2: TFMonitorObj read Get_FMonObj2 write Set_FMonObj2;
    end;

implementation

function TGeneric5ObjHelper.Get_FMonObj(): TFMonitorObj;
begin
    result := TFMonitorObj(FFMonObj);
end;

function TGeneric5ObjHelper.Get_FMonObj2(): TFMonitorObj;
begin
    result := TFMonitorObj(FFMonObj2);
end;

procedure TGeneric5ObjHelper.Set_FMonObj(value: TFMonitorObj);
begin
    FFMonObj := value;
end;

procedure TGeneric5ObjHelper.Set_FMonObj2(value: TFMonitorObj);
begin
    FFMonObj2 := value;
end;

end.