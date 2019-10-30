unit GISCommands;

interface

uses
  DSSGlobals,
  Windows,
  SysUtils,
  System.Classes,
  ShellApi,
  djson,
//   TCP Indy libraries
  IdBaseComponent,
  IdComponent,
  IdTCPConnection,
  IdTCPClient,
  IdThreadComponent;

function start_openDSSGIS(): boolean;
function show_busGIS(BusName   : string): string;

const GUEST_PORT = 20011;

var
  GISTCPClient          : TIdTCPClient;  // ... TIdThreadComponent
  GISThreadComponent    : TIdThreadComponent;

implementation

{*******************************************************************************
*             Starts openDSS-GIS and gets connected as client                  *
*******************************************************************************}

function start_openDSSGIS(): boolean;
Begin
  Result  :=  False;
  if DSS_GIS_Installed then
  Begin
    ShellExecute(0, 'open',pWidechar(DSS_GIS_path), nil, nil, SW_SHOWNORMAL);
    sleep(5000);
    // ... create TIdTCPClient
    GISTCPClient                 := TIdTCPClient.Create();
    // ... set properties
    GISTCPClient.Host            := 'localhost';
    GISTCPClient.Port            := GUEST_PORT;
    GISTCPClient.ReadTimeout     := 1000;
    GISThreadComponent           := TIdThreadComponent.Create();
    try
      GISTCPClient.Connect;
      IsGISON      :=  True;
    except
      on E: Exception do begin
        IsGISON      :=  False;
      end;
    end;
    Result  :=  IsGISON;
  end;
End;

{*******************************************************************************
*                         Shows the given bus on the map                       *
*******************************************************************************}
function show_busGIS(BusName   : string): string;
Var
  TCPJSON       : TdJSON;
  i             : Integer;
  lat,
  long          : Double;
  msg           : String;
Begin
  if IsGISON then
  Begin
    SetActiveBus(BusName);
     If (ActiveCircuit[ActiveActor] <> Nil) Then
     begin
        With ActiveCircuit[ActiveActor] Do
        Begin
          IF (ActiveBusIndex > 0) and (ActiveBusIndex <= Numbuses) Then
          IF (Buses^[ActiveCircuit[ActiveActor].ActiveBusIndex].Coorddefined) Then
          Begin
             lat  := Buses^[ActiveCircuit[ActiveActor].ActiveBusIndex].x;
             long := Buses^[ActiveCircuit[ActiveActor].ActiveBusIndex].y;
          end;
        End;
        if (lat > 0) and (long > 0) then
        Begin
          msg     :=  '{"command":"showlocation","coords":{"longitude":' + floattostr(long) +',"latitude":' + floattostr(lat) + '}}';
          GISTCPClient.IOHandler.WriteLn(msg);
          msg     :=  GISTCPClient.IOHandler.ReadLn(#10,200);
          TCPJSON :=  TdJSON.Parse(msg);
          Result  :=  TCPJSON['showlocation'].AsString;
        end  
        else
          Result  :=  'One or both of the GIS coordinates are incorrect';
     end;
  End;

End;

end.
