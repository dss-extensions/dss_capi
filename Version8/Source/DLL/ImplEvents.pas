unit ImplEvents;

{
  ----------------------------------------------------------
  Copyright (c) 2008-2015, Electric Power Research Institute, Inc.
  All rights reserved.
  ----------------------------------------------------------
}
{$WARN SYMBOL_PLATFORM OFF}

interface

uses
    ComObj,
    ActiveX,
    AxCtrls,
    Classes,
    OpenDSSengine_TLB,
    StdVcl;

type
    TDSSEvents = class(TAutoObject, IConnectionPointContainer, IDSSEvents)
    PRIVATE
        FConnectionPoints: TConnectionPoints;
        function GetSinks: TInterfaceList;
    PUBLIC
        procedure Initialize; OVERRIDE;
        procedure Fire_InitControls;
        procedure Fire_StepControls;
        procedure Fire_CheckControls;
    PROTECTED
        property ConnectionPoints: TConnectionPoints READ FConnectionPoints
            IMPLEMENTS IConnectionPointContainer;
    end;

implementation

uses
    ComServ,
    Dialogs;

procedure TDSSEvents.Initialize;
begin
    inherited Initialize;
    FConnectionPoints := TConnectionPoints.Create(Self);
    if AutoFactory.EventTypeInfo <> NIL then
        FConnectionPoints.CreateConnectionPoint(AutoFactory.EventIID, ckMulti, EventConnect);
end;

function TDSSEvents.GetSinks: TInterfaceList;
var
    connections: IenumConnections;
    conPoint: IconnectionPoint;
    ConnectData: tConnectData;
    NoFetched: Cardinal;
begin
    result := tInterfaceList.Create;
    (self as IConnectionPointContainer).FindConnectionPoint(DIID_IDSSEventsEvents, conPoint);
    conPoint.EnumConnections(connections);
    if connections <> NIL then
        while connections.Next(1, ConnectData, @NoFetched) = S_OK do
            if ConnectData.pUnk <> NIL then
                result.Add(ConnectData.pUnk)
end;

procedure TDSSEvents.Fire_InitControls;
var
    SinkList: TInterfaceList;
    i: Integer;
begin
    SinkList := GetSinks;
    for i := 0 to SinkList.Count - 1 do
        (SinkList.Items[i] as IDSSEventsEvents).InitControls;
    SinkList.Free;
end;

procedure TDSSEvents.Fire_StepControls;
var
    SinkList: TInterfaceList;
    i: Integer;
begin
    SinkList := GetSinks;
    for i := 0 to SinkList.Count - 1 do
        (SinkList.Items[i] as IDSSEventsEvents).StepControls;
    SinkList.Free;
end;

procedure TDSSEvents.Fire_CheckControls;
var
    SinkList: TInterfaceList;
    i: Integer;
begin
    SinkList := GetSinks;
    for i := 0 to SinkList.Count - 1 do
        (SinkList.Items[i] as IDSSEventsEvents).CheckControls;
    SinkList.Free;
end;

initialization
    TAutoObjectFactory.Create(ComServer, TDSSEvents, Class_DSSEvents,
        ciInternal, tmApartment);
end.
