unit GISCommands;

{
  ----------------------------------------------------------
  Copyright (c) 2008-2021, Electric Power Research Institute, Inc.
  All rights reserved.
  ----------------------------------------------------------
}

interface

uses
  Command,
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
  DSSForms,
  ExecHelper,
//   TCP Indy libraries
  IdBaseComponent,
  IdComponent,
  IdTCPConnection,
  IdTCPClient,
  IdThreadComponent,
  TCP_IP;

CONST
        NumGISOptions = 35;

FUNCTION DoGISCmd:string;

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
function GetRouteSegDistances(): string;
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
function GISGetAddress(): string;

var
  GISTCPClient          : TIdTCPClient;  // ... TIdThreadComponent
  GISThreadComponent    : TIdThreadComponent;
  myCoords              : array of double;
  GISOption,
  GISHelp               : Array[1..NumGISOptions] of String;
  GISCommandList        : TCommandList;

implementation

PROCEDURE DefineOptions;

Begin


      GISOption[ 1] := 'Start';
      GISOption[ 2] := 'ShowBus';
      GISOption[ 3] := 'FindRoute';
      GISOption[ 4] := 'GetRoute';
      GISOption[ 5] := 'RouteDistance';
      GISOption[ 6] := 'ShowRoute';
      GISOption[ 7] := 'JSONRoute';
      GISOption[ 8] := 'WindowDistribLR';
      GISOption[ 9] := 'WindowDistribRL';
      GISOption[10] := 'WindowSize';
      GISOption[11] := 'PlotCircuit';
      GISOption[12] := 'showLine';
      GISOption[13] := 'ExportMap';
      GISOption[14] := 'RouteSegDistances';
      GISOption[15] := 'MapView';
      GISOption[16] := 'ClearMap';
      GISOption[17] := 'DrawLine';
      GISOption[18] := 'ZoomMap';
      GISOption[19] := 'PlotFile';
      GISOption[20] := 'GoTo';
      GISOption[21] := 'PlotPoints';
      GISOption[22] := 'PlotPoint';
      GISOption[23] := 'LoadBus';
      GISOption[24] := 'ShowCoords';
      GISOption[25] := 'Format';
      GISOption[26] := 'BatchFormat';
      GISOption[27] := 'Close';
      GISOption[28] := 'Distance';
      GISOption[29] := 'Select';
      GISOption[30] := 'StopSelect';
      GISOption[31] := 'GetSelect';
      GISOption[32] := 'DrawLines';
      GISOption[33] := 'StopDraw';
      GISOption[34] := 'GetPolyline';
      GISOption[35] := 'GetAddress';


       GISHelp[1] := 'Starts OpenDSS-GIS only if it is installed in the local machine';
       GISHelp[2] := 'Shows the bus specified on the map, however, the following conditions need to be fulfilled:' + CRLF +
                           CRLF +
                           '1. OpenDSS-GIS must be installed' + CRLF +
                           '2. OpenDSS-GIS must be initialized (use GISStart command)' + CRLF +
                           '3. The model needs to have the correct GISCoords file';
       GISHelp[3] := 'Finds a route between the given buses using roads and geographical information. The buses are described as an array' +
                           ' as follows: GISFindRoute [b1 b2], do not include phases. The following conditions need to be fulfilled:' + CRLF +
                           CRLF +
                           '1. OpenDSS-GIS must be installed' + CRLF +
                           '2. OpenDSS-GIS must be initialized (use GISStart command)' + CRLF +
                           '3. The model needs to have the correct GISCoords file';
       GISHelp[4] := 'Returns the GIS coords of the route between 2 buses step by step, however, the following conditions need to be fulfilled:' + CRLF +
                           CRLF +
                           '1. OpenDSS-GIS must be installed' + CRLF +
                           '2. OpenDSS-GIS must be initialized (use GISStart command)' + CRLF +
                           '3. GISFindRoute has been executed at some point before this command (at least once)' + CRLF +
                           '4. The model needs to have the correct GISCoords file';
       GISHelp[5] := 'Returns the distance (value units) of the last route calculated between 2 buses, however, the following conditions need to be fulfilled:' + CRLF +
                           CRLF +
                           '1. OpenDSS-GIS must be installed' + CRLF +
                           '2. OpenDSS-GIS must be initialized (use GISStart command)' + CRLF +
                           '3. GISFindRoute has been executed at some point before this command (at least once)' + CRLF +
                           '4. The model needs to have the correct GISCoords file';
       GISHelp[6] := 'Shows the last route calculated between 2 buses in OpenDSS-GIS, however, the following conditions need to be fulfilled:' + CRLF +
                           CRLF +
                           '1. OpenDSS-GIS must be installed' + CRLF +
                           '2. OpenDSS-GIS must be initialized (use GISStart command)' + CRLF +
                           '3. GISFindRoute has been executed at some point before this command (at least once)' + CRLF +
                           '4. The model needs to have the correct GISCoords file';
       GISHelp[7] := 'Returns the JSON script describing the last route calculated between 2 buses, however, the following conditions need to be fulfilled:' + CRLF +
                           CRLF +
                           '1. OpenDSS-GIS must be installed' + CRLF +
                           '2. OpenDSS-GIS must be initialized (use GISStart command)' + CRLF +
                           '3. GISFindRoute has been executed at some point before this command (at least once)' + CRLF +
                           '4. The model needs to have the correct GISCoords file';
       GISHelp[8] := 'Redistributes the windows horizontally leaving OpenDSS to the left of the screen and OpenDSS-GIS to the right, however, the following conditions need to be fulfilled:' + CRLF +
                           CRLF +
                           '1. OpenDSS-GIS must be installed' + CRLF +
                           '2. OpenDSS-GIS must be initialized (use GISStart command)';
       GISHelp[9] := 'Redistributes the windows horizontally leaving OpenDSS to the right of the screen and OpenDSS-GIS to the left, however, the following conditions need to be fulfilled:' + CRLF +
                           CRLF +
                           '1. OpenDSS-GIS must be installed' + CRLF +
                           '2. OpenDSS-GIS must be initialized (use GISStart command)';
       GISHelp[10] := 'Resizes the OpenDSS-GIS window, the coordiantes need to be given as: Left, Top, Right, Bottom. For example:'+CRLF+CRLF+
                           'GISWindowSize 0 0 800 800' + CRLF + CRLF +
                           'The following conditions need to be fulfilled:' + CRLF + CRLF +
                           '1. OpenDSS-GIS must be installed' + CRLF +
                           '2. OpenDSS-GIS must be initialized (use GISStart command)';
       GISHelp[11] := 'Draws the circuit on top of the map displayed in OpenDSS-GIS. The following conditions need to be fulfilled:' + CRLF +
                           CRLF +
                           '1. OpenDSS-GIS must be installed' + CRLF +
                           '2. OpenDSS-GIS must be initialized (use GISStart command)' + CRLF +
                           '3. The model needs to have the correct GISCoords file';
       GISHelp[12] := 'Shows the line specified int he argument using OpenDSS-GIS. The following conditions need to be fulfilled:' + CRLF +
                           CRLF +
                           '1. OpenDSS-GIS must be installed' + CRLF +
                           '2. OpenDSS-GIS must be initialized (use GISStart command)' + CRLF +
                           '3. The model needs to have the correct GISCoords file';
       GISHelp[13] := 'Exports the current map view into the models folder as a PNG file. The following conditions need to be fulfilled:' + CRLF +
                           CRLF +
                           '1. OpenDSS-GIS must be installed' + CRLF +
                           '2. OpenDSS-GIS must be initialized (use GISStart command)' + CRLF +
                           '3. The model needs to have the correct GISCoords file';
       GISHelp[14] := 'Returns Tree/No tree if a tree intersects with the line given in the argument. The following conditions need to be fulfilled:' + CRLF +
                           CRLF +
                           '1. OpenDSS-GIS must be installed' + CRLF +
                           '2. OpenDSS-GIS must be initialized (use GISStart command)' + CRLF +
                           '3. The model needs to have the correct GISCoords file';
       GISHelp[15] := 'Chenges the map view in OpenDSS-GIS using one of the following arguments:' + CRLF + CRLF +
                           '- Streets' +  CRLF +
                           '- StreetsVector' +  CRLF +
                           '- StreetsNight' +  CRLF +
                           '- Satellite' +  CRLF +
                           '- SatelliteLabels' +  CRLF +
                           '- SatelliteLabelsVector' +  CRLF +
                           '- DarkGrayCanvas' +  CRLF +
                           '- LightGrayCanvas' +  CRLF +
                           '- LightGrayCanvasVector' +  CRLF +
                           '- Navigation' +  CRLF +
                           '- OpenStreetMap' +  CRLF + CRLF +
                           'The following conditions need to be fulfilled:' + CRLF +
                           CRLF +
                           '1. OpenDSS-GIS must be installed' + CRLF +
                           '2. OpenDSS-GIS must be initialized (use GISStart command)' + CRLF +
                           '3. The model needs to have the correct GISCoords file';
       GISHelp[16] := 'Clears the Map by removing all the previous draws. The following conditions need to be fulfilled:' + CRLF +
                           CRLF +
                           '1. OpenDSS-GIS must be installed' + CRLF +
                           '2. OpenDSS-GIS must be initialized (use GISStart command)' + CRLF +
                           '3. The model needs to have the correct GISCoords file';
       GISHelp[17] := 'Draws a line at the given coordinates using the color and thickness (pix) specified.' + CRLF +
                           'The line features can be defined using GISCoords, GISColor and GISThickness from the exective options.' + CRLF +
                           'The following conditions need to be fulfilled:' + CRLF + CRLF +
                           '1. OpenDSS-GIS must be installed' + CRLF +
                           '2. OpenDSS-GIS must be initialized (use GISStart command)' + CRLF +
                           '3. The model needs to have the correct GISCoords file';
       GISHelp[18] := 'Zooms the map at the area specified at GISCoords.' + CRLF +
                           'The following conditions need to be fulfilled:' + CRLF + CRLF +
                           '1. OpenDSS-GIS must be installed' + CRLF +
                           '2. OpenDSS-GIS must be initialized (use GISStart command)' + CRLF +
                           '3. The model needs to have the correct GISCoords file';
       GISHelp[19] := 'Plots the content of the file specified in the argument on top of the current map.' + CRLF +
                           'With this function it is expected that the content of the file describes lines, their color and thickness.' + CRLF +
                           'The following conditions need to be fulfilled:' + CRLF + CRLF +
                           '1. OpenDSS-GIS must be installed' + CRLF +
                           '2. OpenDSS-GIS must be initialized (use GISStart command)' + CRLF +
                           '3. The model needs to have the correct GISCoords file';
       GISHelp[20] := 'Shows the location  in the map at given the coordinates.' +
                           'The coordiantes must be defined using GISCoords.' + CRLF +
                           'The following conditions need to be fulfilled:' + CRLF + CRLF +
                           '1. OpenDSS-GIS must be installed' + CRLF +
                           '2. OpenDSS-GIS must be initialized (use GISStart command)';
       GISHelp[21] := 'Plots the content of the file specified in the argument on top of the current map.' + CRLF +
                           'This function plots the content as points, the point shape, color and size must be specified in the file.' + CRLF +
                           'The following conditions need to be fulfilled:' + CRLF + CRLF +
                           '1. OpenDSS-GIS must be installed' + CRLF +
                           '2. OpenDSS-GIS must be initialized (use GISStart command)' + CRLF +
                           '3. The model needs to have the correct GISCoords file';
       GISHelp[22] := 'plots the shape specified in the argument in the map at given the coordinates.' +
                           'The coordiantes must be defined using GISCoords, the size and color can be specified through the options GISColor and GISThickness.' + CRLF +
                           'The shape can be one fo the following:' + CRLF + CRLF +
                           '  Circle' + CRLF +
                           '  + ' + CRLF +
                           '  Diamond' + CRLF +
                           '  Square' + CRLF +
                           '  Triangle' + CRLF +
                           '  x' + CRLF + CRLF +
                           'The following conditions need to be fulfilled:' + CRLF + CRLF +
                           '1. OpenDSS-GIS must be installed' + CRLF +
                           '2. OpenDSS-GIS must be initialized (use GISStart command)';
       GISHelp[23] := 'Uploads the coordinates of the bus specified in the argument to the coordinates buffer by pushing the previous down.' +
                           ' The coordinates buffer has 4 positions, the coordinates of the bus specified will be at the first 2 positions.' + CRLF +
                           'The following conditions need to be fulfilled:' + CRLF + CRLF +
                           '1. OpenDSS-GIS must be installed' + CRLF +
                           '2. OpenDSS-GIS must be initialized (use GISStart command)' + CRLF +
                           '3. The model needs to have the correct GISCoords file';
       GISHelp[24] := 'Returns the content of the coordinates buffer.' + CRLF +
                           'The following conditions need to be fulfilled:' + CRLF + CRLF +
                           '1. OpenDSS-GIS must be installed' + CRLF +
                           '2. OpenDSS-GIS must be initialized (use GISStart command)' + CRLF +
                           '3. The model needs to have the correct GISCoords file';
       GISHelp[25] := 'Formats the coordinates located at the first 2 places of the coordiantes buffer. The first argument indicates ' +
                           'the original format and the second argument the destination format. The format can be one of the following:' + CRLF + CRLF +
                           '- LatLong (latitude, Longitude - WGS84))' + CRLF +
                           '- DMS (Degrees, minutes, seconds) ' + CRLF +
                           '- UTM (Universal Transverse Mercator)' + CRLF +
                           '- USNG' + CRLF + CRLF +
                           'The following conditions need to be fulfilled:' + CRLF + CRLF +
                           '1. OpenDSS-GIS must be installed' + CRLF +
                           '2. OpenDSS-GIS must be initialized (use GISStart command)' + CRLF +
                           '3. The model needs to have the correct GISCoords file';
       GISHelp[26] := 'Formats the coordinates within the file specified. The first argument indicates ' +
                           'the original format and the second argument the destination format. The third argument is the path to the source file' +
                           ' containing the coordinates, which should be organized in 2 columns comma separated. The format can be one of the following:' + CRLF + CRLF +
                           '- LatLong (latitude, Longitude - WGS84))' + CRLF +
                           '- DMS (Degrees, minutes, seconds) ' + CRLF +
                           '- UTM (Universal Transverse Mercator)' + CRLF +
                           '- USNG' + CRLF + CRLF +
                           'The following conditions need to be fulfilled:' + CRLF + CRLF +
                           '1. OpenDSS-GIS must be installed' + CRLF +
                           '2. OpenDSS-GIS must be initialized (use GISStart command)' + CRLF +
                           '3. The model needs to have the correct GISCoords file';
       GISHelp[27] := 'Closses all the instances of OpenDSS-GIS';
       GISHelp[28] := 'Returns the distance in meters between the coordinates in the buffer.' + CRLF +
                           'The following conditions need to be fulfilled:' + CRLF + CRLF +
                           '1. OpenDSS-GIS must be installed' + CRLF +
                           '2. OpenDSS-GIS must be initialized (use GISStart command)' + CRLF +
                           '3. The model needs to have the correct GISCoords file';
       GISHelp[29] := 'Commands OpenDSS-GIS to start the selection mode for allowing users to draw an area on the map.' + CRLF +
                           'The following conditions need to be fulfilled:' + CRLF + CRLF +
                           '1. OpenDSS-GIS must be installed' + CRLF +
                           '2. OpenDSS-GIS must be initialized (use GISStart command)' + CRLF +
                           '3. The model needs to have the correct GISCoords file';
       GISHelp[30] := 'Stops the latest select command sent to OpenDSS-GIS. Clears the map from selections and stores the selection coords in OpenDSS-GIS.' + CRLF +
                           'The following conditions need to be fulfilled:' + CRLF + CRLF +
                           '1. OpenDSS-GIS must be installed' + CRLF +
                           '2. OpenDSS-GIS must be initialized (use GISStart command)' + CRLF +
                           '3. The model needs to have the correct GISCoords file';
       GISHelp[31] := 'Requests the boundaries of the latest selection. The boundaties are returned as XMin, YMin, XMax and YMax in WGS84 coords format.' + CRLF +
                           'The following conditions need to be fulfilled:' + CRLF + CRLF +
                           '1. OpenDSS-GIS must be installed' + CRLF +
                           '2. OpenDSS-GIS must be initialized (use GISStart command)' + CRLF +
                           '3. The model needs to have the correct GISCoords file';
       GISHelp[32] := 'Commands OpenDSS-GIS to start line drawing mode for allowing the user to draw a polyline over the map.' + CRLF +
                           'The following conditions need to be fulfilled:' + CRLF + CRLF +
                           '1. OpenDSS-GIS must be installed' + CRLF +
                           '2. OpenDSS-GIS must be initialized (use GISStart command)' + CRLF +
                           '3. The model needs to have the correct GISCoords file';
       GISHelp[33] := 'Stops the latest lien drawing mode in OpenDSS-GIS. Clears the map and stores the coordinates of the polyline drawn by the user (if any).' + CRLF +
                           'The following conditions need to be fulfilled:' + CRLF + CRLF +
                           '1. OpenDSS-GIS must be installed' + CRLF +
                           '2. OpenDSS-GIS must be initialized (use GISStart command)' + CRLF +
                           '3. The model needs to have the correct GISCoords file';
       GISHelp[34] := 'Requests the coordinates of the latest polyline drawn by the user to OpenDSS-GIS. The are returned in coordiante pairs (Longitude, latitude) in WGS84 coords format.' + CRLF +
                           'The following conditions need to be fulfilled:' + CRLF + CRLF +
                           '1. OpenDSS-GIS must be installed' + CRLF +
                           '2. OpenDSS-GIS must be initialized (use GISStart command)' + CRLF +
                           '3. The model needs to have the correct GISCoords file';
       GISHelp[35] := 'Returns the address calculated at the coordinates given in GISCoords. The address is returned in a JSON string.' + CRLF +
                           'The following conditions need to be fulfilled:' + CRLF + CRLF +
                           '1. OpenDSS-GIS must be installed' + CRLF +
                           '2. OpenDSS-GIS must be initialized (use GISStart command)' + CRLF +
                           '3. The model needs to have the correct GISCoords file';

End;

FUNCTION DoGISCmd:string;
Var

  formatFrom,
  formatto,
  ParamName,
  Param          : String;
  ParamPointer,
  i              : Integer;
  DblBuffer      : Array[0..50] of Double;

Begin

  {Get next parameter on command line}
  Result        :=  'Unknown GIS command;';
  ParamPointer  := 0;
  ParamName     := Uppercase(Parser[ActiveActor].NextParam);
  Param         := Uppercase(Parser[ActiveActor].StrValue);
  {Interpret Parameter}
  IF   (Length(Param) <> 0)  THEN
  begin
     ParamPointer := GISCommandList.Getcommand (Param);

    {Check options requiring a solution and abort if no solution or circuit}
     CASE ParamPointer of
      1:  if start_openDSSGIS() then Result  :=  'GIS Started succesfully' else Result :=  'Error, check if OpenDSS-GIS is running and your firewall setup';
      2:  begin
            Parser[ActiveActor].NextParam;
            Result  :=  show_busGIS(Parser[ActiveActor].StrValue);
          end;
      3:  Result  :=  Get_routeGIS();
      4:  Result  :=  Get_edgesGIS();
      5:  Result  :=  Get_distanceGIS();
      6:  Result  :=  Show_routeGIS();
      7:  Result  :=  Get_JSONrouteGIS();
      8:  begin
            if Not isDLL then
            Begin
              Result  := WindowLR();
              ControlPanel.ResizeWindow(0);
            end
            else
              Result  :=  'Available only for the EXE interface'
          End;
      9:  Begin
            if Not IsDLL then
            begin
              Result  :=  WindowRL();
              ControlPanel.ResizeWindow(1);
            end
            else
              Result  :=  'Available only for the EXE interface'
          End;
      10: Begin
            Result  :=  ReSizeWindow();
          End;
      11: Result   :=  GISDrawCircuit;           // Draws the circuit on top of the map in DSS-GIS
      12: begin
            Parser[ActiveActor].NextParam;
            Result  :=  show_lineGIS(Parser[ActiveActor].StrValue);
          end;
      13: Result   :=  export_mapGIS();           // exports the current map view into the model's folder
      14: begin
            Result  :=  GetRouteSegDistances();   // returns the distances of all the segments of the last route estimated
          end;
      15: begin
            Parser[ActiveActor].NextParam;
            Result  :=  set_map_View(lowercase(Parser[ActiveActor].StrValue));
          end;
      16: Result  :=  clear_map();
      17: Result  :=  Draw_line_GIS();
      18: Result  :=  Zoom_area_GIS();
      19: begin
            Parser[ActiveActor].NextParam;
            Result  :=  GISPlotfile(lowercase(Parser[ActiveActor].StrValue));
          end;
      20: Begin
            Result  :=  show_LatLong();
          end;
      21: begin
            Parser[ActiveActor].NextParam;
            Result  :=  GISPlotPoints(lowercase(Parser[ActiveActor].StrValue));
          end;
      22: begin
            Parser[ActiveActor].NextParam;
            Result  :=  GISPlotPoint(lowercase(Parser[ActiveActor].StrValue));
          end;
      23: begin
            Parser[ActiveActor].NextParam;
            Result  :=  GISLoadBus(lowercase(Parser[ActiveActor].StrValue));
          end;
      24: begin
            Result  :=  GISShowBuffer();
          end;
      25: begin
            Parser[ActiveActor].NextParam;
            FormatFrom :=  lowercase(Parser[ActiveActor].StrValue);
            Parser[ActiveActor].NextParam;
            FormatTo :=  lowercase(Parser[ActiveActor].StrValue);
            Parser[ActiveActor].NextParam;
            Result  :=  GISFormat(FormatFrom,FormatTo,Parser[ActiveActor].StrValue);
          end;
      26: begin
            Parser[ActiveActor].NextParam;
            FormatFrom :=  lowercase(Parser[ActiveActor].StrValue);
            Parser[ActiveActor].NextParam;
            FormatTo :=  lowercase(Parser[ActiveActor].StrValue);
            Parser[ActiveActor].NextParam;
            Result  :=  GISBatchFormat(FormatFrom,FormatTo,Parser[ActiveActor].StrValue);
          end;
      27: Result :=  GISClose();
      28: Result :=  Get_distance();
      29: Result :=  GISStartSelect();
      30: Result :=  GISStopSelect();
      31: Result :=  GISGetSelect();
      32: Result :=  GISStartDrawLine();
      33: Result :=  GISStopDrawLine();
      34: Result :=  GISGetPolyline();
      35: Result :=  GISGetAddress();
      else
     END;
  end;
End;

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
    InMsg:=  '{"command":"route","coords":{"long1":' + floattostr(GISCoords^[1]) +',"lat1":' + floattostr(GISCoords^[2]) +
              ',"long2":' + floattostr(GISCoords^[3]) + ',"lat2":'+ floattostr(GISCoords^[4]) + '}}';
    try
      GISTCPClient.IOHandler.WriteLn(InMsg);
      InMsg   :=  GISTCPClient.IOHandler.ReadLn(#10,1000);
      TCPJSON :=  TdJSON.Parse(InMsg);
      Result  :=  TCPJSON['route'].AsString;
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
      TempStr :=  TCPJSON['coords'].AsString;
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
      TempStr :=  TCPJSON['routedistance'].AsString;
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
    JSONCmd   :=  '{"command":"showroute"},"color":"' + GISColor +
            '","thickness":' + GISThickness + '}';
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
*             Commands OpenDSS-GIS to return the distances for every           *
*                     step of the last route estimated                         *
*******************************************************************************}
function GetRouteSegDistances(): string;
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
      InMsg:=  '{"command":"getdistances"}';
      try
        GISTCPClient.IOHandler.WriteLn(InMsg);
        InMsg   :=  GISTCPClient.IOHandler.ReadLn(#10,200);
        TCPJSON :=  TdJSON.Parse(InMsg);
        Result  :=  TCPJSON['getdistances'].AsString;
      except
        on E: Exception do begin
          IsGISON     :=  False;
          Result      :=  'Error while communicating to OpenDSS-GIS';
        end;
      end;
    end;
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
*                     gets the address at the given coords                     *
*******************************************************************************}
function GISGetAddress(): string;
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
    InMsg:=  '{"command":"address","coords":{"long":' + floattostr(GISCoords^[1]) +',"lat":' + floattostr(GISCoords^[2]) + '}}';
    try
      GISTCPClient.IOHandler.WriteLn(InMsg);
      InMsg   :=  GISTCPClient.IOHandler.ReadLn(#10,1000);
      Result  :=  InMsg;
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
      GISCoords^[1] :=  myLat;
      GISCoords^[2] :=  myLong;
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

Procedure DisposeStrings;
Var i:Integer;

Begin
    For i := 1 to NumGISOptions Do Begin
       GISOption[i] := '';
       GISHelp[i]   := '';
   End;

End;

Initialization

    DefineOptions;

    GISCommandList := TCommandList.Create(GISOption);
    GISCommandList.Abbrev := True;

Finalization

    DisposeStrings;
    GISCommandList.Free;

end.
