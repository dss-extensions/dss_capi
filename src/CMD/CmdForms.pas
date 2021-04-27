unit CmdForms;

{
  ----------------------------------------------------------
  Copyright (c) 2008-2015, Electric Power Research Institute, Inc.
  All rights reserved.
  ----------------------------------------------------------
}

{
    08/17/2016  Created from OpenDSS
 ----------------------------------------------------------
  Copyright (c) 2016 Battelle Memorial Institute
 ----------------------------------------------------------
}

interface

uses
    Classes;

procedure CreateControlPanel;
procedure ExitControlPanel;
procedure InitProgressForm;
procedure ProgressCaption(const S: String);
procedure ProgressFormCaption(const S: String);
procedure ProgressHide;
procedure ShowControlPanel;
procedure ShowHelpForm;
procedure ShowAboutBox;
procedure ShowPropEditForm;
procedure ShowPctProgress(Count: Integer);
procedure ShowMessageForm(S: TStrings);
function DSSMessageDlg(const Msg: String; err: Boolean): Integer;
procedure DSSInfoMessageDlg(const Msg: String);
function GetDSSExeFile: String;
procedure CloseDownForms;
procedure ShowTreeView(const Fname: String);
function MakeChannelSelection(NumFieldsToSkip: Integer; const Filename: String): Boolean;
procedure ShowHeapUsage; // copied from Lazarus form; not used in command line yet

implementation

uses
    ExecCommands,
    ExecOptions,
    ShowOptions,
    ExportOptions,
    DSSGlobals,
    DSSClass,
    DSSClassDefs,
    ParserDel,
    Sysutils,
    Strutils,
    ArrayDef;

const
    colwidth = 25;
    numcols = 4;  // for listing commands to the console

procedure ShowHeapUsage;
var
    hstat: TFPCHeapStatus;
    s: String;
begin
    hstat := GetFPCHeapStatus;
    s := Format('Heap Memory Used: %dK', [hstat.CurrHeapUsed div 1024]);
    DSSInfoMessageDlg(s);
end;

procedure InitProgressForm;
begin
end;

procedure ShowPctProgress(Count: Integer);
begin
end;

procedure ProgressCaption(const S: String);
begin
    if NoFormsAllowed then
        Exit;
    Writeln('Progress: ', S);
end;

procedure ProgressFormCaption(const S: String);
begin
    if NoFormsAllowed then
        Exit;
    Writeln('Progress: ', S);
end;

procedure ProgressHide;
begin
end;

procedure ShowAboutBox;
begin
    writeln('OpenDSS (Electric Power Distribution System Simulator), DSS C-API library version');
    writeln(VersionString);
    writeln('Copyright (c) 2008-2019, Electric Power Research Institute, Inc.');
    writeln('Copyright (c) 2016-2017, Battelle Memorial Institute');
    writeln('Copyright (c) 2017-2021, Paulo Meira');
    writeln('All rights reserved.');
end;

procedure ShowTreeView(const Fname: String);
begin
end;

function GetDSSExeFile: String;
begin
    Result := 'todo'; // ExtractFilePath (Application.ExeName);
end;


function DSSMessageDlg(const Msg: String; err: Boolean): Integer;
begin
    result := 0;
{$IFDEF DSS_CAPI}
    if DSS_CAPI_EARLY_ABORT then
        // If the result is handled outside and this is not and error message,
        // we can let the caller decide if the error should halt or not
        result := -1; 
{$ENDIF}
    if NoFormsAllowed then
    begin
{$IFDEF DSS_CAPI}
        if err then
        begin
            // If this is an error message, We need to pass the message somehow. 
            // Decided to use the error interface here and, if early abort is on,
            // set the global Redirect_Abort.
            DoSimpleMsg(Msg, 65535);
            if DSS_CAPI_EARLY_ABORT then
                Redirect_Abort := True;
        end;
{$ENDIF}
        Exit;
    end;
    if err then
        write('** Error: ');
    writeln(Msg);
end;

procedure DSSInfoMessageDlg(const Msg: String);
begin
    if NoFormsAllowed then
        Exit;
    writeln(Msg);
end;

procedure CreateControlPanel;
begin
end;

procedure ExitControlPanel;
begin
end;

procedure ShowControlPanel;
begin
end;

function CompareClassNames(Item1, Item2: Pointer): Integer;
begin
    Result := CompareText(TDSSClass(Item1).name, TDSSClass(Item2).name);
end;

procedure AddHelpForClasses(BaseClass: Word; bProperties: Boolean);
var
    HelpList: TList;
    pDSSClass: TDSSClass;
    i, j: Integer;
begin
    HelpList := TList.Create();
    pDSSClass := DSSClassList.First;
    while pDSSClass <> NIL do
    begin
        if (pDSSClass.DSSClassType and BASECLASSMASK) = BaseClass then
            HelpList.Add(pDSSClass);
        pDSSClass := DSSClassList.Next;
    end;
    HelpList.Sort(@CompareClassNames);

    for i := 1 to HelpList.Count do
    begin
        pDSSClass := HelpList.Items[i - 1];
        writeln(pDSSClass.name);
        if bProperties = TRUE then
            for j := 1 to pDSSClass.NumProperties do
                writeln('  ', pDSSClass.PropertyName[j], ': ', pDSSClass.PropertyHelp^[j]);
    end;
    HelpList.Free;
end;

procedure ShowGeneralHelp;
begin
    writeln('For specific help, enter:');
    writeln('  "help command [cmd]" lists all executive commands, or');
    writeln('                       if [cmd] provided, details on that command');
    writeln('  "help option [opt]"  lists all simulator options, or');
    writeln('                       if [opt] provided, details on that option');
    writeln('  "help show [opt]"    lists the options to "show" various outputs, or');
    writeln('                       if [opt] provided, details on that output');
    writeln('  "help export [fmt]"  lists the options to "export" in various formats, or');
    writeln('                       if [fmt] provided, details on that format');
    writeln('  "help class [cls]"   lists the names of all available circuit model classes, or');
    writeln('                       if [cls] provided, details on that class');
    writeln('You may truncate any help topic name, which returns all matching entries');
end;

procedure ShowAnyHelp(const num: Integer; cmd: pStringArray; hlp: pStringArray; const opt: String);
var
    i: Integer;
    lst: TStringList;
begin
    if Length(opt) < 1 then
    begin
        lst := TStringList.Create;
        for i := 1 to num do
            lst.Add(PadRight(cmd[i], colwidth));
        lst.Sort;
        for i := 1 to num do
            if ((i mod numcols) = 0) then
                writeln(lst[i - 1])
            else
                write(lst[i - 1] + ' ');
        lst.Free;
    end
    else
    begin
        for i := 1 to num do
        begin
            if AnsiStartsStr(opt, LowerCase(cmd[i])) then
            begin
                writeln(UpperCase(cmd[i]));
                writeln('======================');
                writeln(hlp[i]);
            end;
        end;
    end;
end;

procedure ShowClassHelp(const opt: String);
var
    pDSSClass: TDSSClass;
    i: Integer;
begin
    if Length(opt) > 0 then
    begin
        pDSSClass := DSSClassList.First;
        while pDSSClass <> NIL do
        begin
            if AnsiStartsStr(opt, LowerCase(pDSSClass.name)) then
            begin
                writeln(UpperCase(pDSSClass.name));
                writeln('======================');
                for i := 1 to pDSSClass.NumProperties do
                    writeln('  ', pDSSClass.PropertyName[i], ': ', pDSSClass.PropertyHelp^[i]);
            end;
            pDSSClass := DSSClassList.Next;
        end;
    end
    else
    begin
        writeln('== Power Delivery Elements ==');
        AddHelpForClasses(PD_ELEMENT, FALSE);
        writeln('== Power Conversion Elements ==');
        AddHelpForClasses(PC_ELEMENT, FALSE);
        writeln('== Control Elements ==');
        AddHelpForClasses(CTRL_ELEMENT, FALSE);
        writeln('== Metering Elements ==');
        AddHelpForClasses(METER_ELEMENT, FALSE);
        writeln('== Supporting Elements ==');
        AddHelpForClasses(0, FALSE);
        writeln('== Other Elements ==');
        AddHelpForClasses(NON_PCPD_ELEM, FALSE);
    end;
end;

{$DEFINE EXPORT_HELP}
{$IFDEF EXPORT_HELP}
function StringToMD(const s: String): String;
begin
    Result := s;
    Result := StringReplace(Result, CRLF, '<br>', [rfReplaceAll]);
    Result := StringReplace(Result, '|', '\|', [rfReplaceAll]);
    Result := StringReplace(Result, '*', '\*', [rfReplaceAll]);
    Result := StringReplace(Result, '~', '\~', [rfReplaceAll]);
end;

procedure AddHelpForClassesMD(BaseClass: Word);
var
    HelpList: TList;
    pDSSClass: TDSSClass;
    i, j: Integer;
begin
    HelpList := TList.Create();
    pDSSClass := DSSClassList.First;
    while pDSSClass <> NIL do
    begin
        if (pDSSClass.DSSClassType and BASECLASSMASK) = BaseClass then
            HelpList.Add(pDSSClass);
        pDSSClass := DSSClassList.Next;
    end;
    HelpList.Sort(@CompareClassNames);


    for i := 1 to HelpList.Count do
    begin
        pDSSClass := HelpList.Items[i - 1];
        writeln('#### `', pDSSClass.name, '` properties');
        writeln();
        writeln('| Number | Name | Description |');
        writeln('| - | - | - |');
        
        for j := 1 to pDSSClass.NumProperties do
            writeln('| ', IntToStr(j),  ' | ', pDSSClass.PropertyName[j], ' | ', StringToMD(pDSSClass.PropertyHelp^[j]), ' |');
                
        writeln();
        writeln();
    end;
    
    HelpList.Free;
end;

procedure ShowAnyHelpMD(const num: Integer; cmd: pStringArray; hlp: pStringArray; what: String);
var
    i, j: Integer;
    lst: TStringList;
begin
    lst := TStringList.Create;
    for i := 1 to num do
        lst.Add(cmd[i]);

    lst.Sort;
    
    writeln('| ', what,  ' | Description |');
    writeln('| - | - |');
    for i := 1 to num do
        for j := 1 to num do
            if cmd[j] = lst[i - 1] then
            begin
                writeln('| ', cmd[j], ' | ',  StringToMD(hlp[j]), ' |');
                break;
            end;
    
    lst.Free;
    writeln();
end;

procedure ShowAllHelpMD();
//var
//    pDSSClass: TDSSClass;
//    i: Integer;
begin
    writeln('# DSS Extensions: OpenDSS Commands and Properties');
    writeln();
    writeln('---');
    writeln();
    writeln('**This document was generated from:** `', VersionString, '`');
    writeln();
    writeln('*Generated with the legacy models disabled (i.e. OpenDSS v9+ compatibility mode).*');
    writeln();
    writeln('---');
    writeln();
    writeln('## About this');
    writeln();
    writeln('This is a document automatically generated from the commands, options and properties for the DSS language (script level) exposed in the DSS Extensions version of the OpenDSS engine. A separate document will be developed in the future to detail **API** functions and general usage recommendations for the projects under DSS Extensions.');
    writeln();
    writeln('Since the extensive majority of properties and elements are compatible, this document can be useful when using either the official OpenDSS implementation or the DSS Extensions version (DSS C-API engine), consumed through the projects DSS Python (`dss_python`), OpenDSSDirect.py, OpenDSSDirect.jl, DSS Sharp (`dss_sharp`), and DSS MATLAB (`dss_matlab`).  If you are using the official OpenDSS, when in doubt check the official documentation and/or source code.');
    writeln();
    writeln('As a final note, keep in mind that not all commands are implemented in the DSS Extensions engine, interactive commands like plots are missing (on purpose).');
    writeln();
    
    writeln('---');
    writeln('## Commands');
    writeln();
    ShowAnyHelpMD(NumExecCommands, pStringArray(@ExecCommand), pStringArray(@CommandHelp), 'Command');
    
    writeln('---');
    writeln('## Execution Options');
    writeln();
    ShowAnyHelpMD(NumExecOptions, pStringArray(@ExecOption), pStringArray(@OptionHelp), 'Option');
    
    writeln('---');
    writeln('## `Show` options');
    writeln();
    ShowAnyHelpMD(NumShowOptions, pStringArray(@ShowOption), pStringArray(@ShowHelp), 'Option');
    
    writeln('---');
    writeln('## `Export` options');
    writeln();
    ShowAnyHelpMD(NumExportOptions, pStringArray(@ExportOption), pStringArray(@ExportHelp), 'Option');
    
    writeln('---');
    writeln('## Elements');
    writeln();
    writeln('---');
    writeln('### Power Delivery Elements');
    writeln();
    AddHelpForClassesMD(PD_ELEMENT);
    writeln('---');
    writeln('### Power Conversion Elements');
    writeln();
    AddHelpForClassesMD(PC_ELEMENT);
    writeln('---');
    writeln('###  Control Elements');
    writeln();
    AddHelpForClassesMD(CTRL_ELEMENT);
    writeln('---');
    writeln('### Metering Elements');
    writeln();
    AddHelpForClassesMD(METER_ELEMENT);
    writeln('---');
    writeln('### Supporting Elements');
    writeln();
    AddHelpForClassesMD(0);
    writeln('---');
    writeln('### Other Elements');
    writeln();
    AddHelpForClassesMD(NON_PCPD_ELEM);
end;
{$ENDIF}

procedure ShowHelpForm;
var
    Param, OptName: String;
begin
    Parser.NextParam;
    Param := LowerCase(Parser.StrValue);
    Parser.NextParam;
    OptName := LowerCase(Parser.StrValue);
    
{$IFDEF EXPORT_HELP}
    if ANSIStartsStr('markdown', param) then
        ShowAllHelpMD()
    else
{$ENDIF}
    if ANSIStartsStr('com', param) then
        ShowAnyHelp(NumExecCommands, pStringArray(@ExecCommand), pStringArray(@CommandHelp), OptName)
    else
    if ANSIStartsStr('op', param) then
        ShowAnyHelp(NumExecOptions, pStringArray(@ExecOption), pStringArray(@OptionHelp), OptName)
    else
    if ANSIStartsStr('sh', param) then
        ShowAnyHelp(NumShowOptions, pStringArray(@ShowOption), pStringArray(@ShowHelp), OptName)
    else
    if ANSIStartsStr('e', param) then
        ShowAnyHelp(NumExportOptions, pStringArray(@ExportOption), pStringArray(@ExportHelp), OptName)
    else
    if ANSIStartsStr('cl', param) then
        ShowClassHelp(OptName)
    else
        ShowGeneralHelp;
end;

procedure ShowMessageForm(S: TStrings);
begin
    if NoFormsAllowed then
        Exit;
    writeln(s.text);
end;

procedure ShowPropEditForm;
begin
end;

procedure CloseDownForms;
begin
end;

function MakeChannelSelection(NumFieldsToSkip: Integer; const Filename: String): Boolean;
begin
    Result := FALSE;
end;

end.
