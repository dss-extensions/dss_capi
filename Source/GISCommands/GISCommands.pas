unit GISCommands;

interface

uses
  Tlhelp32,
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
  IdThreadComponent,

  TCP_IP;

function start_openDSSGIS(): boolean;
function show_busGIS(BusName   : string): string;
function Get_routeGIS():  string;
function Get_edgesGIS():  string;
function Get_distanceGIS():  string;
function Show_routeGIS():  string;
function Get_JSONrouteGIS():  string;
function WindowLR():  string;
function WindowRL():  string;

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
  if Not IsGISON then
  Begin
    if DSS_GIS_Installed then
    Begin
      if Not processExists('OpenDSSGIS.exe') then
      begin
        // Starts OpenDSS-GIS if is not running
        ShellExecute(0, 'open',pWidechar(DSS_GIS_path), nil, nil, SW_SHOWNORMAL);
        sleep(5000);
      end;
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
  End
  else
    Result  :=  IsGISON;
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
  InMsg           : String;
Begin
  if IsGISON then
  Begin
    SetActiveBus(BusName);
     If (ActiveCircuit[ActiveActor] <> Nil) Then
     begin
        With ActiveCircuit[ActiveActor] Do
        Begin
          IF (ActiveBusIndex > 0) and (ActiveBusIndex <= Numbuses) Then
          IF (Buses^[ActiveCircuit[ActiveActor].ActiveBusIndex].GISCoorddefined) Then
          Begin
            lat  := Buses^[ActiveCircuit[ActiveActor].ActiveBusIndex].lat;
            long := Buses^[ActiveCircuit[ActiveActor].ActiveBusIndex].long;
            InMsg:=  '{"command":"showlocation","coords":{"longitude":' + floattostr(long) +',"latitude":' + floattostr(lat) + '}}';
            try
              GISTCPClient.IOHandler.WriteLn(InMsg);
              InMsg   :=  GISTCPClient.IOHandler.ReadLn(#10,200);
              TCPJSON :=  TdJSON.Parse(InMsg);
              Result  :=  TCPJSON['showlocation'].AsString;
            except
              on E: Exception do begin
                IsGISON     :=  False;
                Result      :=  'Error while communicating to OpenDSS-GIS';
              end;
            end;
          end
          else
            Result  :=  'One or both of the GIS coordinates are incorrect or not defined';
        End;
     end;
  End
  else
    result  :=  'OpenDSS-GIS is not installed or initialized';

End;

{*******************************************************************************
*                 Request to calculate a route between 2 buses                 *
*******************************************************************************}

function Get_routeGIS():  string;
var
  TCPJSON       : TdJSON;
  JSONCmd,
  InMsg,
  busName   : String;
  TryCom,
  error     : Boolean;
  i         : Integer;
  lat,
  long      : Double;
Begin
  if IsGISON then
  Begin
    error     :=  False;
    JSONCmd   :=  '{"command":"route","coords":[';
    for i := 1 to 2 do                                                  // to extract both buses
    begin
      Parser[ActiveActor].NextParam;
      busName   :=  Parser[ActiveActor].StrValue;
      SetActiveBus(busName);
      If (ActiveCircuit[ActiveActor] <> Nil) and Not error Then         // is everything fine?
      begin
        With ActiveCircuit[ActiveActor] Do
        Begin
          IF (ActiveBusIndex > 0) and (ActiveBusIndex <= Numbuses) Then
          IF (Buses^[ActiveCircuit[ActiveActor].ActiveBusIndex].GISCoorddefined) Then
          Begin
            lat     := Buses^[ActiveCircuit[ActiveActor].ActiveBusIndex].lat;
            long    := Buses^[ActiveCircuit[ActiveActor].ActiveBusIndex].long;
            JSONCmd := JSONCmd + '{"longitude":' + floattostr(long) + ',"latitude":' + floattostr(lat) + '},';
          End
          else
            error :=  True;
        End;
      end;
    end;
    if Not error then                                                 // No error so far
    begin
      JSONCmd   :=  JSONCmd.Substring(0,length(JSONCmd)-1) + ']}';
      TryCom    :=  True;
      i         :=  0;
      while TryCom do
      Begin
        try
          GISTCPClient.IOHandler.WriteLn(JSONCmd);
          InMsg   :=  GISTCPClient.IOHandler.ReadLn(#10,200);
          TCPJSON :=  TdJSON.Parse(InMsg);
          InMsg   :=  TCPJSON['route'].AsString;
          if InMsg = 'done' then                                      // Route calculated successfully
            Trycom  :=  False
          else
          begin
            // If the route wasn't calculated because the server was busy, it tries up to 5 times
            // with 300 ms interval, if after that the server is still busy, return error message
            sleep(300);
            inc(i);
            if i > 5 then
              Trycom  :=  False;
          end;
        except
          on E: Exception do begin
            IsGISON     :=  False;
            Result      :=  'Error while communicating to OpenDSS-GIS';
          end;
        end;
      End;
      Result  :=  InMsg;
    end
    else
      Result  :=  'One or more buses have no GIS coordinates';
  End
  else
    result  :=  'OpenDSS-GIS is not installed or initialized'
End;

{*******************************************************************************
*  Request to coordiantes of the edges that define the last route calculated   *
*******************************************************************************}
function Get_edgesGIS():  string;
var
  Coords,
  TCPJSON       : TdJSON;
  JSONCmd,
  TempStr,
  InMsg         : String;

Begin
  if IsGISON then
  Begin
    JSONCmd   :=  '{"command":"jsonroute"}';
    try
      GISTCPClient.IOHandler.WriteLn(JSONCmd);
      InMsg   :=  GISTCPClient.IOHandler.ReadLn(#10,2000);
      TCPJSON :=  TdJSON.Parse(InMsg);
      TempStr :=  '[';
      for Coords in TCPJSON['jsonroute'] do
      begin
          TempStr         :=  TempStr + Coords['latitude'].AsString + ',' + Coords['longitude'].AsString + ',';
      End;
      Result  :=  TempStr.substring(0,(length(TempStr) - 1)) + ']';
      except
      on E: Exception do begin
        IsGISON     :=  False;
        Result      :=  'Error while communicating to OpenDSS-GIS';
      end;
    end;
  End
  else
    result  :=  'OpenDSS-GIS is not installed or initialized'
End;

{*******************************************************************************
*                Gets the distance of the last route calculated                *
*******************************************************************************}
function Get_distanceGIS():  string;
var
  TCPJSON       : TdJSON;
  JSONCmd,
  TempStr,
  InMsg         : String;

begin
  if IsGISON then
  Begin
    JSONCmd   :=  '{"command":"distance"}';
    try
      GISTCPClient.IOHandler.WriteLn(JSONCmd);
      InMsg   :=  GISTCPClient.IOHandler.ReadLn(#10,2000);
      TCPJSON :=  TdJSON.Parse(InMsg);
      TempStr :=  TCPJSON['distance'].AsString + ' ' + TCPJSON['units'].AsString;
      Result  :=  TempStr;
      except
      on E: Exception do begin
        IsGISON     :=  False;
        Result      :=  'Error while communicating to OpenDSS-GIS';
      end;
    end;

  End
  else
    result  :=  'OpenDSS-GIS is not installed or initialized'
end;

{*******************************************************************************
*                 Shows on the map the last route calculated                   *
*******************************************************************************}

function Show_routeGIS():  string;
var
  TCPJSON       : TdJSON;
  JSONCmd,
  TempStr,
  InMsg         : String;

Begin
  if IsGISON then
  Begin
    JSONCmd   :=  '{"command":"showroute"}';
    try
      GISTCPClient.IOHandler.WriteLn(JSONCmd);
      InMsg   :=  GISTCPClient.IOHandler.ReadLn(#10,2000);
      TCPJSON :=  TdJSON.Parse(InMsg);
      TempStr :=  TCPJSON['showroute'].AsString;
      Result  :=  TempStr;
      except
      on E: Exception do begin
        IsGISON     :=  False;
        Result      :=  'Error while communicating to OpenDSS-GIS';
      end;
    end;

  End
  else
    result  :=  'OpenDSS-GIS is not installed or initialized'

End;

{*******************************************************************************
*       Exports to a file the last route calculated in JSON format             *
*******************************************************************************}

function Get_JSONrouteGIS():  string;
Begin

End;

{*******************************************************************************
*            Distributes the windows leaving OpenDSS on the left               *
*******************************************************************************}

function WindowLR():  string;
Begin

End;

{*******************************************************************************
*            Distributes the windows leaving OpenDSS on the right              *
*******************************************************************************}

function WindowRL():  string;
Begin

End;

end.
