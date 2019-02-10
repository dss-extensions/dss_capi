unit ImplDSSProperty;

{
  ----------------------------------------------------------
  Copyright (c) 2008-2015, Electric Power Research Institute, Inc.
  All rights reserved.
  ----------------------------------------------------------
}
{
  10-1-2009 Revised so it works on all DSSobjects

}

interface

uses
    ComObj,
    ActiveX,
    OpenDSSEngine_TLB,
    StdVcl;

type
    TDSSProperty = class(TAutoObject, IDSSProperty)
    PROTECTED
        function Get_Description: Widestring; SAFECALL;
        function Get_Name: Widestring; SAFECALL;
        function Get_Val: Widestring; SAFECALL;
        procedure Set_Val(const Value: Widestring); SAFECALL;
    end;

implementation

uses
    ComServ,
    DSSClass,
    DSSGlobals,
    ImplGlobals,
    Executive,
    SysUtils;

function TDSSProperty.Get_Description: Widestring;
begin
    Result := '';
    if (ActiveCircuit <> NIL) and (FPropIndex <> 0) {and (FPropClass <> Nil)} then
        with  ActiveDSSObject.ParentClass do
            if FPropIndex <= NumProperties then
                Result := PropertyHelp^[FPropIndex];

end;

function TDSSProperty.Get_Name: Widestring;
begin
    Result := '';
    if (ActiveCircuit <> NIL) and (FPropIndex <> 0) {and (FPropClass <> Nil)} then
        with  ActiveDSSObject.ParentClass do
            if FPropIndex <= NumProperties then
                Result := PropertyName^[FPropIndex];

end;


function TDSSProperty.Get_Val: Widestring;
begin
    Result := '';
    if (ActiveCircuit <> NIL) then
        with ActiveDSSObject do
            if FPropIndex <= ParentClass.NumProperties then
                Result := PropertyValue[ParentClass.PropertyIdxMap[FPropIndex]];

end;

procedure TDSSProperty.Set_Val(const Value: Widestring);
begin
    if (ActiveCircuit <> NIL) then
        with ActiveDSSObject do
            if FPropIndex <= ParentClass.NumProperties then
                DSSExecutive.Command := 'Edit ' + ParentClass.Name + '.' + Name + ' ' +
                    ParentClass.PropertyName^[FPropIndex] + '=' +
                    String(Value);
end;


initialization
    TAutoObjectFactory.Create(ComServer, TDSSProperty, Class_DSSProperty, ciInternal, tmApartment);
end.
