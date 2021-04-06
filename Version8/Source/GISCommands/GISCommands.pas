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
  VCl.forms,
  Line,
  Utilities,
  ArrayDef,
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
function ReSizeWindow():  string;
function GISDrawCircuit():  string;
function show_lineGIS(LineName   : string): string;
function export_mapGIS(): string;
function find_treesGIS(LineName : string): string;
Procedure get_line_Coords(LineName : string);
function set_map_View(myView : string): string;
function clear_map(): string;
function Draw_line_GIS(): string;
function Zoom_area_GIS(): string;
function GISPlotfile(myPath : string):  string;
function show_LatLong(): string;
function GISPlotPoints(myPath : string):  string;
function GISPlotPoint(const myShape : string):  string;
function GISLoadBus(const myBus : string): string;
function GISShowBuffer():  string;
function GISFormat(const FormatFrom,FormatTo,Coords : string): string;
function GISBatchFormat(const FormatFrom,FormatTo,mypath : string): string;
function GISClose(): string;
function Get_distance():  string;
function GISStartSelect():  string;
function GISStopSelect():  string;
function GISGetSelect():  string;
function GISStartDrawLine():  string;
function GISStopDrawLine():  string;
function GISGetPolyline(): string;

var
  GISTCPClient          : TIdTCPClient;  // ... TIdThreadComponent
  GISThreadComponent    : TIdThreadComponent;
  myCoords          : array of double;

implementation

{*******************************************************************************
*             Starts openDSS-GIS and gets connected as client                  *
*******************************************************************************}

function start_openDSSGIS(): boolean;
var
  myPath,
  myFolder  : String;
Begin
  Result  :=  False;

  if DSS_GIS_Installed then
  Begin
    myPath    :=  StringReplace(DSS_GIS_path,'\\','\',[rfReplaceAll, rfIgnoreCase]);
    myPath    :=  StringReplace(myPath,'"','',[rfReplaceAll, rfIgnoreCase]);
    myFolder  :=  ExtractFilePath(myPath);

    if Not processExists('OpenDSSGIS.exe') then
    begin
     // Starts OpenDSS-GIS if is not running
      ShellExecute(0, 'open',pChar(myPath), nil, pChar(myFolder), SW_SHOWNORMAL);
      sleep(5000);
      IsGISON      :=  False;
    end;
    if Not IsGISON then
    Begin
      // ... create TIdTCPClient
      GISTCPClient                 := TIdTCPClient.Create();
      // ... set properties
      GISTCPClient.Host            := 'localhost';
      GISTCPClient.Port            := DSSGISPort;
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
    end
    else
      Result  :=  IsGISON;
  End;

End;

{*******************************************************************************
*                            Closes OpenDSS-GIS                                *
*******************************************************************************}
function GISClose(): string;
var
  myError   : Integer;
Begin
  myError := ShellExecute(0, nil,'taskkill.exe','/IM "OpenDSSGIS.exe" /F', 'C:\Windows\System32', SW_HIDE);
  if myError > 32 then
    Result := 'OpenDSS-GIS closed successfuly'
  else
    Result := 'An error has occurred while closing OpenDSS-GIS';
  IsGISON      :=  False;
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
*                  Shows the given location using LatLong                      *
*******************************************************************************}
function show_LatLong(): string;
Var
  TCPJSON       : TdJSON;
  i             : Integer;
  lat,
  long          : Double;
  InMsg           : String;
Begin
  if IsGISON then
  Begin

    lat  := GISCoords^[1];
    long := GISCoords^[2];
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
            Trycom      :=  False;
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
    JSONCmd   :=  '{"command":"routedistance"}';
    try
      GISTCPClient.IOHandler.WriteLn(JSONCmd);
      InMsg   :=  GISTCPClient.IOHandler.ReadLn(#10,2000);
      TCPJSON :=  TdJSON.Parse(InMsg);
      TempStr :=  TCPJSON['routedistance'].AsString + ' ' + TCPJSON['units'].AsString;
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
var
  F             :TextFile;
  JSONCmd,
  FileName,
  InMsg         : String;
Begin
  if IsGISON then
  Begin
    JSONCmd   :=  '{"command":"jsonscript"}';
    try
      GISTCPClient.IOHandler.WriteLn(JSONCmd);
      InMsg   :=  GISTCPClient.IOHandler.ReadLn(#10,20000);

      FileName := GetOutputDirectory + CircuitName_[ActiveActor] + 'JSONScript_route.txt';  // Explicitly define directory

      Assignfile(F, FileName);
      ReWrite(F);
      Write(F,inMsg);
      CloseFile(F);

      Result  :=  FileName;
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
*            Distributes the windows leaving OpenDSS on the left               *
*******************************************************************************}

function WindowLR():  string;
var
  TCPJSON       : TdJSON;
  ScrSize   : Integer;
  InMsg,
  TempStr,
  JSONCmd   : String;
Begin
  if IsGISON then
  Begin
    JSONCmd   := '{"command":"resizewindow","coords":{"left":' + inttostr(Screen.Width div 2) +
                  ',"top":0,"right":' + inttostr(Screen.Width) + ',"bottom":' + inttostr(Screen.Height - 40) + '}}';
    try
      GISTCPClient.IOHandler.WriteLn(JSONCmd);
      InMsg   :=  GISTCPClient.IOHandler.ReadLn(#10,2000);
      TCPJSON :=  TdJSON.Parse(InMsg);
      TempStr :=  TCPJSON['resizewindow'].AsString;
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
*            Distributes the windows leaving OpenDSS to the right              *
*******************************************************************************}

function WindowRL():  string;
var
  TCPJSON       : TdJSON;
  ScrSize   : Integer;
  InMsg,
  TempStr,
  JSONCmd   : String;
Begin
  if IsGISON then
  Begin
    JSONCmd   := '{"command":"resizewindow","coords":{"left":0,"top":0,"right":' +
                  inttostr(Screen.Width div 2) + ',"bottom":' + inttostr(Screen.Height - 40) + '}}';
    try
      GISTCPClient.IOHandler.WriteLn(JSONCmd);
      InMsg   :=  GISTCPClient.IOHandler.ReadLn(#10,2000);
      TCPJSON :=  TdJSON.Parse(InMsg);
      TempStr :=  TCPJSON['resizewindow'].AsString;
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
*    Resizes the OpenDSS-GIS window using the coordinates given by the user    *
*******************************************************************************}

function ReSizeWindow():  string;
var
  TCPJSON       : TdJSON;
  j,
  ScrSize       : Integer;
  InMsg,
  TempStr,
  JSONCmd       : String;
  TStrArr       : Array of String;

Begin
  if IsGISON then
  Begin
    setlength(TStrArr,4);
    TStrArr[0]  :=  ',"top":';
    TStrArr[1]  :=  ',"right":';
    TStrArr[2]  :=  ',"bottom":';
    TStrArr[3]  :=  '}}';

    JSONCmd :=  '{"command":"resizewindow","coords":{"left":';
    for j := 0 to High(TStrArr) do
    Begin
      Parser[ActiveActor].NextParam;
      JSONCmd   := JSONCmd + Parser[ActiveActor].StrValue + TStrArr[j];
    End;
    try
      GISTCPClient.IOHandler.WriteLn(JSONCmd);
      InMsg   :=  GISTCPClient.IOHandler.ReadLn(#10,2000);
      TCPJSON :=  TdJSON.Parse(InMsg);
      TempStr :=  TCPJSON['resizewindow'].AsString;
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
*      Generates the file required by DSS-GIS to draw the model on the map     *
*******************************************************************************}

function GISDrawCircuit():  string;
Var
  LineElem      : TLineObj;
  TxtRow,
  myBus         : string;
  k             : Integer;
  F             : TextFile;
  InMsg,
  TempStr,
  JSONCmd       : String;
  TCPJSON       : TdJSON;
  Add2file      : Boolean;

Begin
  IF ActiveCircuit[ActiveActor] <> Nil THEN
  Begin
    if IsGISON then
    Begin
        WITH ActiveCircuit[ActiveActor] DO
        begin
          If Lines.ListSize > 0 Then
          Begin
            Assignfile(F, 'GIS_desc.csv');
            ReWrite(F);
            LineElem := Lines.First;
            WHILE LineElem<>Nil DO
            Begin
              TxtRow    :=  '';
              Add2File  :=  True;
              for k := 1 to 2 do
              Begin
                myBus   :=  StripExtension(LineElem.GetBus(k));
                DSSGlobals.SetActiveBus(myBus);
                IF (Buses^[ActiveCircuit[ActiveActor].ActiveBusIndex].GISCoordDefined) Then
                Begin
                  TxtRow  :=  TxtRow + floattostr(Buses^[ActiveCircuit[ActiveActor].ActiveBusIndex].Long) +
                  ',' + floattostr(Buses^[ActiveCircuit[ActiveActor].ActiveBusIndex].Lat) + ',';
                End;
                Add2File  :=  Add2File and (Buses^[ActiveCircuit[ActiveActor].ActiveBusIndex].Long <> 0) and (Buses^[ActiveCircuit[ActiveActor].ActiveBusIndex].Lat <> 0);
              End;
              if Add2File then
                Writeln(F, TxtRow);
              LineElem := Lines.Next;

            End;
            CloseFile(F);
            JSONCmd :=  '{"command":"plotcircuit","path":"' +
            OutputDirectory[ActiveActor] + 'GIS_desc.csv","color":"' + GISColor +
            '","thickness":' + GISThickness + '}';
            // Sends the command to OpenDSS-GIS
            try
              GISTCPClient.IOHandler.WriteLn(JSONCmd);
              InMsg   :=  GISTCPClient.IOHandler.ReadLn(#10,5000);
              TCPJSON :=  TdJSON.Parse(InMsg);
              TempStr :=  TCPJSON['plotcircuit'].AsString;
              Result  :=  TempStr;
              except
              on E: Exception do begin
                IsGISON     :=  False;
                Result      :=  'Error while communicating to OpenDSS-GIS';
              end;
            end;

          End;
        end;
    End
    else
      result  :=  'OpenDSS-GIS is not installed or initialized'
  End;
end;

{*******************************************************************************
*                         Shows the given line on the map                       *
*******************************************************************************}
function show_lineGIS(LineName   : string): string;
Var
  TCPJSON       : TdJSON;
  activesave,
  i             : Integer;
  InMsg         : String;
  pLine         : TLineObj;
Begin
  if IsGISON then
  Begin
  // First have to find the line

    If (ActiveCircuit[ActiveActor] <> Nil) Then
    begin
      get_line_Coords(LineName);

      InMsg:=  '{"command":"showline","coords":{"long1":' + floattostr(myCoords[0]) +',"lat1":' + floattostr(myCoords[1]) +
                ',"long2":' + floattostr(myCoords[2]) + ',"lat2":'+ floattostr(myCoords[3]) + '}}';
      try
        GISTCPClient.IOHandler.WriteLn(InMsg);
        InMsg   :=  GISTCPClient.IOHandler.ReadLn(#10,200);
        TCPJSON :=  TdJSON.Parse(InMsg);
        Result  :=  TCPJSON['showline'].AsString;
      except
        on E: Exception do begin
          IsGISON     :=  False;
          Result      :=  'Error while communicating to OpenDSS-GIS';
        end;
      end;

    end;

  End
  else
    result  :=  'OpenDSS-GIS is not installed or initialized';

End;

{*******************************************************************************
*             Exports the current map view into the models folder              *
*******************************************************************************}
function export_mapGIS(): string;
Var
  TCPJSON       : TdJSON;
  activesave,
  i             : Integer;
  InMsg         : String;
  Found         : Boolean;
  pLine         : TLineObj;
Begin
  if IsGISON then
  Begin
    InMsg:=  '{"command":"exportmap", "path":"' + OutputDirectory[ActiveActor] + '"}';
    try
      GISTCPClient.IOHandler.WriteLn(InMsg);
      InMsg   :=  GISTCPClient.IOHandler.ReadLn(#10,200);
      TCPJSON :=  TdJSON.Parse(InMsg);
      Result  :=  TCPJSON['exportmap'].AsString;
    except
      on E: Exception do begin
        IsGISON     :=  False;
        Result      :=  'Error while communicating to OpenDSS-GIS';
      end;
    end;

  end
  else
    result  :=  'OpenDSS-GIS is not installed or initialized';
End;

{*******************************************************************************
*             Commands OpenDSS-GIS to verify if there are trees                *
*                     intersecting with the given line                         *
*******************************************************************************}
function find_treesGIS(LineName : string): string;
Var
  TCPJSON       : TdJSON;
  activesave,
  i             : Integer;
  InMsg         : String;
  Found         : Boolean;
  pLine         : TLineObj;
Begin
  if IsGISON then
  Begin
  // to be implemented

    If (ActiveCircuit[ActiveActor] <> Nil) Then
    begin
      get_line_Coords(LineName);

      InMsg:=  '{"command":"findtrees","coords":{"long1":' + floattostr(myCoords[0]) +',"lat1":' + floattostr(myCoords[1]) +
                ',"long2":' + floattostr(myCoords[2]) + ',"lat2":'+ floattostr(myCoords[3]) + '}}';

      try
        GISTCPClient.IOHandler.WriteLn(InMsg);
        InMsg   :=  GISTCPClient.IOHandler.ReadLn(#10,200);
        TCPJSON :=  TdJSON.Parse(InMsg);
        Result  :=  TCPJSON['findtrees'].AsString;
      except
        on E: Exception do begin
          IsGISON     :=  False;
          Result      :=  'Error while communicating to OpenDSS-GIS';
        end;
      end;
    end;
    result  := 'No';
  end
  else
    result  :=  'OpenDSS-GIS is not installed or initialized';
End;
{*******************************************************************************
*             Commands OpenDSS-GIS to update the map view to the               *
*                             one given by the user                            *
*******************************************************************************}
function set_map_View(myView : string): string;
Var
  TCPJSON       : TdJSON;
  activesave,
  i             : Integer;
  InMsg         : String;
  Found         : Boolean;
  pLine         : TLineObj;
Begin
  if IsGISON then
  Begin
    InMsg:=  '{"command":"mapview","mymap":"' + myView + '"}';
    try
      GISTCPClient.IOHandler.WriteLn(InMsg);
      InMsg   :=  GISTCPClient.IOHandler.ReadLn(#10,200);
      TCPJSON :=  TdJSON.Parse(InMsg);
      Result  :=  TCPJSON['mapview'].AsString;
    except
      on E: Exception do begin
        IsGISON     :=  False;
        Result      :=  'Error while communicating to OpenDSS-GIS';
      end;
    end;
  end
  else
    result  :=  'OpenDSS-GIS is not installed or initialized';
End;

{*******************************************************************************
*      Commands OpenDSS-GIS to remove all previous lines/draws from the map    *
*******************************************************************************}
function clear_map(): string;
Var
  TCPJSON       : TdJSON;
  activesave,
  i             : Integer;
  InMsg         : String;
  Found         : Boolean;
  pLine         : TLineObj;
Begin
  if IsGISON then
  Begin
    InMsg:=  '{"command":"clearmap"}';
    try
      GISTCPClient.IOHandler.WriteLn(InMsg);
      InMsg   :=  GISTCPClient.IOHandler.ReadLn(#10,200);
      TCPJSON :=  TdJSON.Parse(InMsg);
      Result  :=  TCPJSON['clearmap'].AsString;
    except
      on E: Exception do begin
        IsGISON     :=  False;
        Result      :=  'Error while communicating to OpenDSS-GIS';
      end;
    end;
  end
  else
    result  :=  'OpenDSS-GIS is not installed or initialized';
End;

{*******************************************************************************
*                 Draws a line in the map at the given coordinates             *
*******************************************************************************}
function Draw_line_GIS(): string;
Var
  TCPJSON       : TdJSON;
  activesave,
  i             : Integer;
  InMsg         : String;
  Found         : Boolean;
  pLine         : TLineObj;

Begin
  if IsGISON then
  Begin

    InMsg:=  '{"command":"drawline","coords":{"long1":' + floattostr(GISCoords^[1]) +',"lat1":' + floattostr(GISCoords^[2]) +
              ',"long2":' + floattostr(GISCoords^[3]) + ',"lat2":'+ floattostr(GISCoords^[4]) + '},"color":"' + GISColor +
              '","thickness":' + GISThickness + '}';
      try
        GISTCPClient.IOHandler.WriteLn(InMsg);
        InMsg   :=  GISTCPClient.IOHandler.ReadLn(#10,200);
        TCPJSON :=  TdJSON.Parse(InMsg);
        Result  :=  TCPJSON['drawline'].AsString;
      except
        on E: Exception do begin
          IsGISON     :=  False;
         Result      :=  'Error while communicating to OpenDSS-GIS';
        end;
      end;
  end
  else
    result  :=  'OpenDSS-GIS is not installed or initialized';
End;

{*******************************************************************************
*          Zooms the map at the area described by the given coordinates        *
*******************************************************************************}
function Zoom_area_GIS(): string;
Var
  TCPJSON       : TdJSON;
  activesave,
  i             : Integer;
  InMsg         : String;
  Found         : Boolean;
  pLine         : TLineObj;
Begin
  if IsGISON then
  Begin
    InMsg:=  '{"command":"zoommap","coords":{"long1":' + floattostr(GISCoords^[1]) +',"lat1":' + floattostr(GISCoords^[2]) +
              ',"long2":' + floattostr(GISCoords^[3]) + ',"lat2":'+ floattostr(GISCoords^[4]) + '}}';
    try
      GISTCPClient.IOHandler.WriteLn(InMsg);
      InMsg   :=  GISTCPClient.IOHandler.ReadLn(#10,200);
      TCPJSON :=  TdJSON.Parse(InMsg);
      Result  :=  TCPJSON['zoommap'].AsString;
    except
      on E: Exception do begin
        IsGISON     :=  False;
        Result      :=  'Error while communicating to OpenDSS-GIS';
      end;
    end;
  end
  else
    result  :=  'OpenDSS-GIS is not installed or initialized';
End;

{*******************************************************************************
*            request the calculation of the distance between 2 points          *
*******************************************************************************}
function Get_distance(): string;
Var
  TCPJSON       : TdJSON;
  activesave,
  i             : Integer;
  InMsg         : String;
  Found         : Boolean;
  pLine         : TLineObj;
Begin
  if IsGISON then
  Begin
    InMsg:=  '{"command":"distance","coords":{"long1":' + floattostr(GISCoords^[1]) +',"lat1":' + floattostr(GISCoords^[2]) +
              ',"long2":' + floattostr(GISCoords^[3]) + ',"lat2":'+ floattostr(GISCoords^[4]) + '}}';
    try
      GISTCPClient.IOHandler.WriteLn(InMsg);
      InMsg   :=  GISTCPClient.IOHandler.ReadLn(#10,200);
      TCPJSON :=  TdJSON.Parse(InMsg);
      Result  :=  TCPJSON['distance'].AsString;
    except
      on E: Exception do begin
        IsGISON     :=  False;
        Result      :=  'Error while communicating to OpenDSS-GIS';
      end;
    end;
  end
  else
    result  :=  'OpenDSS-GIS is not installed or initialized';
End;

{*******************************************************************************
*                 Commands OpenDSS-GIS to start select mode                    *
*******************************************************************************}
function GISStartSelect(): string;
Var
  TCPJSON       : TdJSON;
  activesave,
  i             : Integer;
  InMsg         : String;
  Found         : Boolean;
  pLine         : TLineObj;
Begin
  if IsGISON then
  Begin
    InMsg:=  '{"command":"select"}';
    try
      GISTCPClient.IOHandler.WriteLn(InMsg);
      InMsg   :=  GISTCPClient.IOHandler.ReadLn(#10,200);
      TCPJSON :=  TdJSON.Parse(InMsg);
      Result  :=  TCPJSON['select'].AsString;
    except
      on E: Exception do begin
        IsGISON     :=  False;
        Result      :=  'Error while communicating to OpenDSS-GIS';
      end;
    end;
  end
  else
    result  :=  'OpenDSS-GIS is not installed or initialized';
End;

{*******************************************************************************
*              Commands OpenDSS-GIS to start line drawing mode                 *
*******************************************************************************}
function GISStartDrawLine(): string;
Var
  TCPJSON       : TdJSON;
  activesave,
  i             : Integer;
  InMsg         : String;
  Found         : Boolean;
  pLine         : TLineObj;
Begin
  if IsGISON then
  Begin
    InMsg:=  '{"command":"drawlines"}';
    try
      GISTCPClient.IOHandler.WriteLn(InMsg);
      InMsg   :=  GISTCPClient.IOHandler.ReadLn(#10,200);
      TCPJSON :=  TdJSON.Parse(InMsg);
      Result  :=  TCPJSON['drawlines'].AsString;
    except
      on E: Exception do begin
        IsGISON     :=  False;
        Result      :=  'Error while communicating to OpenDSS-GIS';
      end;
    end;
  end
  else
    result  :=  'OpenDSS-GIS is not installed or initialized';
End;

{*******************************************************************************
*            Commands OpenDSS-GIS to stop the line drawing mode                *
*******************************************************************************}
function GISStopDrawLine(): string;
Var
  TCPJSON       : TdJSON;
  activesave,
  i             : Integer;
  InMsg         : String;
  Found         : Boolean;
  pLine         : TLineObj;
Begin
  if IsGISON then
  Begin
    InMsg:=  '{"command":"stopdrawlines"}';
    try
      GISTCPClient.IOHandler.WriteLn(InMsg);
      InMsg   :=  GISTCPClient.IOHandler.ReadLn(#10,200);
      TCPJSON :=  TdJSON.Parse(InMsg);
      Result  :=  TCPJSON['stopdrawlines'].AsString;
    except
      on E: Exception do begin
        IsGISON     :=  False;
        Result      :=  'Error while communicating to OpenDSS-GIS';
      end;
    end;
  end
  else
    result  :=  'OpenDSS-GIS is not installed or initialized';
End;

{*******************************************************************************
*                  Commands OpenDSS-GIS to stop select mode                    *
*******************************************************************************}
function GISStopSelect(): string;
Var
  TCPJSON       : TdJSON;
  activesave,
  i             : Integer;
  InMsg         : String;
  Found         : Boolean;
  pLine         : TLineObj;
Begin
  if IsGISON then
  Begin
    InMsg:=  '{"command":"stopselect"}';
    try
      GISTCPClient.IOHandler.WriteLn(InMsg);
      InMsg   :=  GISTCPClient.IOHandler.ReadLn(#10,200);
      TCPJSON :=  TdJSON.Parse(InMsg);
      Result  :=  TCPJSON['stopselect'].AsString;
    except
      on E: Exception do begin
        IsGISON     :=  False;
        Result      :=  'Error while communicating to OpenDSS-GIS';
      end;
    end;
  end
  else
    result  :=  'OpenDSS-GIS is not installed or initialized';
End;

{*******************************************************************************
*      gets the boundaries for the latest selection made in OpenDSS-GIS        *
*******************************************************************************}
function GISGetSelect(): string;
Var
  TCPJSON       : TdJSON;
  activesave,
  i             : Integer;
  InMsg         : String;
  Found         : Boolean;
  pLine         : TLineObj;
Begin
  if IsGISON then
  Begin
    InMsg:=  '{"command":"getselect"}';
    try
      GISTCPClient.IOHandler.WriteLn(InMsg);
      InMsg   :=  GISTCPClient.IOHandler.ReadLn(#10,200);
      TCPJSON :=  TdJSON.Parse(InMsg);
      Result  :=  TCPJSON['getselect'].AsString;
    except
      on E: Exception do begin
        IsGISON     :=  False;
        Result      :=  'Error while communicating to OpenDSS-GIS';
      end;
    end;
  end
  else
    result  :=  'OpenDSS-GIS is not installed or initialized';
End;

{*******************************************************************************
*          gets the coords for the latest polyline drawn in OpenDSS-GIS        *
*******************************************************************************}
function GISGetPolyline(): string;
Var
  TCPJSON       : TdJSON;
  activesave,
  i             : Integer;
  InMsg         : String;
  Found         : Boolean;
  pLine         : TLineObj;
Begin
  if IsGISON then
  Begin
    InMsg:=  '{"command":"getpolyline"}';
    try
      GISTCPClient.IOHandler.WriteLn(InMsg);
      InMsg   :=  GISTCPClient.IOHandler.ReadLn(#10,200);
      TCPJSON :=  TdJSON.Parse(InMsg);
      Result  :=  TCPJSON['getpolyline'].AsString;
    except
      on E: Exception do begin
        IsGISON     :=  False;
        Result      :=  'Error while communicating to OpenDSS-GIS';
      end;
    end;
  end
  else
    result  :=  'OpenDSS-GIS is not installed or initialized';
End;

{*******************************************************************************
*       Commands OpenDSS-GIS to draw the content of a file over the map        *
*******************************************************************************}
function GISPlotfile(myPath : string):  string;
Var
  TxtRow,
  myBus         : string;
  k             : Integer;
  F             : TextFile;
  InMsg,
  TempStr,
  JSONCmd       : String;
  TCPJSON       : TdJSON;

Begin
  IF ActiveCircuit[ActiveActor] <> Nil THEN
  Begin
    if IsGISON then
    Begin
      JSONCmd :=  '{"command":"plotfromfile","path":"' +
      myPath + '"}';
      // Sends the command to OpenDSS-GIS
      try
        GISTCPClient.IOHandler.WriteLn(JSONCmd);
        InMsg   :=  GISTCPClient.IOHandler.ReadLn(#10,5000);
        TCPJSON :=  TdJSON.Parse(InMsg);
        TempStr :=  TCPJSON['plotfromfile'].AsString;
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
end;

{*******************************************************************************
*     Commands OpenDSS-GIS to draw the points within a file over the map       *
*******************************************************************************}
function GISPlotPoints(myPath : string):  string;
Var
  TxtRow,
  myBus         : string;
  k             : Integer;
  F             : TextFile;
  InMsg,
  TempStr,
  JSONCmd       : String;
  TCPJSON       : TdJSON;

Begin
  IF ActiveCircuit[ActiveActor] <> Nil THEN
  Begin
    if IsGISON then
    Begin
      JSONCmd :=  '{"command":"plotpoints","path":"' +
      myPath + '"}';
      // Sends the command to OpenDSS-GIS
      try
        GISTCPClient.IOHandler.WriteLn(JSONCmd);
        InMsg   :=  GISTCPClient.IOHandler.ReadLn(#10,5000);
        TCPJSON :=  TdJSON.Parse(InMsg);
        TempStr :=  TCPJSON['plotpoints'].AsString;
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
end;

{*******************************************************************************
*         Commands OpenDSS-GIS to draw a marker at specific coordinates        *
*******************************************************************************}
function GISPlotPoint(const myShape : string):  string;
Var
  TCPJSON       : TdJSON;
  myShpCode,
  activesave,
  i             : Integer;
  InMsg         : String;
  Found         : Boolean;
  pLine         : TLineObj;

Begin
  if IsGISON then
  Begin
    case myShape[1] of            // Parse the shape specified
      'c' : myShpCode :=0;
      '+' : myShpCode :=1;
      'd' : myShpCode :=2;
      's' : myShpCode :=3;
      't' : myShpCode :=4;
      'x' : myShpCode :=5;
    end;
    InMsg:=  '{"command":"plotpoint","coords":{"long":' + floattostr(GISCoords^[1]) +',"lat":' + floattostr(GISCoords^[2]) +
              '},"color":"' + GISColor +
              '","thickness":' + GISThickness + ',"shape":' + inttostr(myShpCode) + '}';
    try
      GISTCPClient.IOHandler.WriteLn(InMsg);
      InMsg   :=  GISTCPClient.IOHandler.ReadLn(#10,1000);
      TCPJSON :=  TdJSON.Parse(InMsg);
      Result  :=  TCPJSON['plotpoint'].AsString;
    except
      on E: Exception do begin
        IsGISON     :=  False;
       Result      :=  'Error while communicating to OpenDSS-GIS';
      end;
    end;
  end
  else
    result  :=  'OpenDSS-GIS is not installed or initialized';
End;

{*******************************************************************************
* Commands openDSS-GIS to convert the coords given in a file into a new format *
*******************************************************************************}
function GISBatchFormat(const FormatFrom,FormatTo,mypath : string): string;
Var
  myEnd,
  myStart       : Integer;
  TCPJSON       : TdJSON;
  InMsg         : String;
Begin
    InMsg:=  '{"command":"batchformat","from":"' + FormatFrom + '","to":"' + FormatTo + '",' +
              '"path":"' + mypath + '"}';
    try
      GISTCPClient.IOHandler.WriteLn(InMsg);
      InMsg   :=  GISTCPClient.IOHandler.ReadLn(#10,1000);
      myStart :=  ansipos('path":"',InMsg) + 6;
      myEnd   :=  ansipos('"}',InMsg) - 1;
      Result  :=  InMsg.Substring(myStart, myEnd - myStart);
    except
      on E: Exception do begin
        IsGISON     :=  False;
       Result      :=  'Error while communicating to OpenDSS-GIS';
      end;
    end;
End;

{*******************************************************************************
*      Commands openDSS-GIS to convert the coords given into a new format      *
*******************************************************************************}
function GISFormat(const FormatFrom,FormatTo,Coords : string): string;
Var
  TCPJSON       : TdJSON;
  InMsg         : String;
Begin
    InMsg:=  '{"command":"format","from":"' + FormatFrom + '","to":"' + FormatTo + '",' +
              '"coords":"' + Coords + '"}';
    try
      GISTCPClient.IOHandler.WriteLn(InMsg);
      InMsg   :=  GISTCPClient.IOHandler.ReadLn(#10,1000);
      TCPJSON :=  TdJSON.Parse(InMsg);
      Result  :=  TCPJSON['coords'].AsString;
    except
      on E: Exception do begin
        IsGISON     :=  False;
       Result      :=  'Error while communicating to OpenDSS-GIS';
      end;
    end;
End;

{*******************************************************************************
*          Returns a string with the content of the coordiantes buffer         *
*******************************************************************************}
function GISShowBuffer():  string;
var
  idx : Integer;
Begin
  Result  :=  '';
  for idx := 1 to 4 do
    Result  :=  Result + floattostr(GISCoords^[idx]) + ',';
End;
{*******************************************************************************
*  Loads the bus coordiantes into the first 2 places fo the coordiantes buffer *
*  shifting it down                                                            *
*******************************************************************************}
function GISLoadBus(const myBus : string): string;
var
  myLat,
  myLong  : Double;
Begin
  If (ActiveCircuit[ActiveActor] <> Nil) Then With ActiveCircuit[ActiveActor] Do
  Begin
    DSSGlobals.SetActiveBus(StripExtension(myBus));
    IF (ActiveBusIndex > 0) and (ActiveBusIndex <= Numbuses) Then
    Begin
      IF (Buses^[ActiveCircuit[ActiveActor].ActiveBusIndex].Coorddefined) Then
      Begin
        myLong := Buses^[ActiveCircuit[ActiveActor].ActiveBusIndex].long;
        myLat :=  Buses^[ActiveCircuit[ActiveActor].ActiveBusIndex].lat;
      End;
      GISCoords^[3] :=  GISCoords^[1];
      GISCoords^[4] :=  GISCoords^[2];
      GISCoords^[1] :=  myLong;
      GISCoords^[2] :=  myLat;
      Result  :=  'done'
    End
    else
      Result  :=  'Invalid bus name';
  End
  else
    Result  :=  'There is no active circuit';
End;
{*******************************************************************************
*             Loads the line Long-lat into the global array "myCoords"         *
*******************************************************************************}
Procedure get_line_Coords(LineName : string);
var
  TCPJSON       : TdJSON;
  activesave,
  i             : Integer;
  myBuses       : Array of String;
  S,
  InMsg         : String;
  Found         : Boolean;
  pLine         : TLineObj;
begin
  setlength(myCoords,4);
  setlength(myBuses,2);

  S          := LineName;  // Convert to Pascal String
  Found      := FALSE;

  WITH ActiveCircuit[ActiveActor].Lines DO
  Begin
    ActiveSave := ActiveIndex;
    pLine      := First;
    While pLine <> NIL Do
    Begin
       IF (CompareText(pLine.Name, S) = 0)
       THEN Begin
           ActiveCircuit[ActiveActor].ActiveCktElement := pLine;
           Found := TRUE;
           Break;
       End;
       pLine := Next;
    End;
  End;
  // Get the names of the buses for the line
  With ActiveCircuit[ActiveActor] Do
  Begin
    For i := 1 to  2 Do
    Begin
       myBuses[i-1] := StripExtension(pLine.GetBus(i));
    End;

  // Get the coords of the buses
    For i := 0 to  1 Do
    Begin
      SetActiveBus(myBuses[i]);
      IF (ActiveBusIndex > 0) and (ActiveBusIndex <= Numbuses) Then
      Begin
        IF (Buses^[ActiveCircuit[ActiveActor].ActiveBusIndex].GISCoorddefined) Then
        Begin
          myCoords[i*2]     :=  Buses^[ActiveCircuit[ActiveActor].ActiveBusIndex].long;
          myCoords[i*2 + 1] :=  Buses^[ActiveCircuit[ActiveActor].ActiveBusIndex].lat;
        End;
      End;
    End;
  End;

end;

end.
