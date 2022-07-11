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

type
{$SCOPEDENUMS ON}
    DSSMessageType = (
        Error = -1,
        General = 0,
        Info = 1,
        Help = 2,
        Progress = 3,
        ProgressCaption = 4,
        ProgressFormCaption = 5,
        ProgressPercent = 6,
        FireOffEditor = 7
    );
{$SCOPEDENUMS OFF}


procedure InitProgressForm;
procedure ProgressCaption(const S: String);
procedure ProgressFormCaption(const S: String);
procedure ProgressHide;
procedure ShowHelpForm(dssContext: TObject);
procedure ShowAboutBox;
procedure ShowPropEditForm;
procedure ShowPctProgress(Count: Integer);
procedure ShowMessageForm(S: TStrings);
function DSSMessageDlg(const Msg: String; err: Boolean): Integer;
procedure DSSInfoMessageDlg(const Msg: String);
procedure CloseDownForms;
procedure ShowTreeView(const Fname: String);

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
    ArrayDef,
    DSSHelper,
    DSSPointerList,
    Utilities;

const
    colwidth = 25;
    numcols = 4;  // for listing commands to the console


procedure WriteLnCB(s: String; mtype: DSSMessageType);
begin
    if (@DSSPrime.DSSMessageCallback) <> NIL then
        DSSPrime.DSSMessageCallback(DSSPrime, PChar(s), ord(mtype))
    else
        WriteLn(s);
end;

procedure InitProgressForm;
begin
    if (@DSSPrime.DSSMessageCallback) <> NIL then
    begin
        DSSPrime.DSSMessageCallback(DSSPrime, PChar('0'), ord(DSSMessageType.ProgressPercent));
        Exit;
    end;
end;

procedure ShowPctProgress(Count: Integer);
begin
    if (@DSSPrime.DSSMessageCallback) <> NIL then
    begin
        DSSPrime.DSSMessageCallback(DSSPrime, PChar(IntToStr(Count)), ord(DSSMessageType.ProgressPercent));
        Exit;
    end;
end;

procedure ProgressCaption(const S: String);
begin
    if NoFormsAllowed then
        Exit;
    
    if (@DSSPrime.DSSMessageCallback) <> NIL then
    begin
        DSSPrime.DSSMessageCallback(DSSPrime, PChar(S), ord(DSSMessageType.ProgressCaption));
        Exit;
    end;
    Writeln('Progress: ', S);
end;

procedure ProgressFormCaption(const S: String);
begin
    if NoFormsAllowed then
        Exit;

    if (@DSSPrime.DSSMessageCallback) <> NIL then
    begin
        DSSPrime.DSSMessageCallback(DSSPrime, PChar(S), ord(DSSMessageType.ProgressFormCaption));
        Exit;
    end;
    Writeln('Progress: ', S);
end;

procedure ProgressHide;
begin
    if NoFormsAllowed then
        Exit;

    if (@DSSPrime.DSSMessageCallback) <> NIL then
        DSSPrime.DSSMessageCallback(DSSPrime, PChar('-1'), ord(DSSMessageType.ProgressPercent));
end;

procedure ShowAboutBox;
begin
    WriteLnCB(
        'DSS C-API library version' + CRLF +
        VersionString + CRLF +
        'An alternative implementation of OpenDSS' + CRLF + 
        'OpenDSS is EPRI''s Electric Power Distribution System Simulator' + CRLF +
        'Copyright (c) 2008-2022, Electric Power Research Institute, Inc.' + CRLF +
        'Copyright (c) 2016-2021, Battelle Memorial Institute' + CRLF +
        'Copyright (c) 2017-2022, Paulo Meira, DSS Extensions contributors' + CRLF +
        'All rights reserved.' + CRLF +
        'Please check the repository commit history and specific files for detailed credits.', 
        DSSMessageType.Info
    );
end;

procedure ShowTreeView(const Fname: String);
begin
end;

function DSSMessageDlg(const Msg: String; err: Boolean): Integer;
begin
    result := 0;

    if DSS_CAPI_EARLY_ABORT then
        // If the result is handled outside and this is not and error message,
        // we can let the caller decide if the error should halt or not
        result := -1; 

    if NoFormsAllowed then
    begin
        if err then
        begin
            // If this is an error message, We need to pass the message somehow. 
            // Decided to use the error interface here and, if early abort is on,
            // set the global Redirect_Abort.
            DoSimpleMsg(DSSPrime, Msg, 65535);
            if DSS_CAPI_EARLY_ABORT then
                DSSPrime.Redirect_Abort := True;
        end;

        Exit;
    end;

    if (@DSSPrime.DSSMessageCallback) <> NIL then
    begin
        if err then
            DSSPrime.DSSMessageCallback(DSSPrime, PChar(Msg), ord(DSSMessageType.Error))
        else
            DSSPrime.DSSMessageCallback(DSSPrime, PChar(Msg), ord(DSSMessageType.General));
        
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

    WriteLnCB(Msg, DSSMessageType.Info);
end;

function CompareClassNames(Item1, Item2: Pointer): Integer;
begin
    Result := CompareText(TDSSClass(Item1).name, TDSSClass(Item2).name);
end;

procedure AddHelpForClasses(DSSClassList: TDSSPointerList; BaseClass: Word; bProperties: Boolean);
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

    if (@DSSPrime.DSSMessageCallback) <> NIL then
    begin
        for i := 1 to HelpList.Count do
        begin
            pDSSClass := HelpList.Items[i - 1];
            DSSPrime.DSSMessageCallback(DSSPrime, PChar(pDSSClass.name), ord(DSSMessageType.Help));
            if bProperties = TRUE then
                for j := 1 to pDSSClass.NumProperties do
                    DSSPrime.DSSMessageCallback(DSSPrime, PChar('  ' + pDSSClass.PropertyName[j] + ': ' + pDSSClass.GetPropertyHelp(j)), ord(DSSMessageType.Help));
        end;
    end
    else
    begin
        for i := 1 to HelpList.Count do
        begin
            pDSSClass := HelpList.Items[i - 1];
            WriteLnCB(pDSSClass.name, DSSMessageType.Help);

            if bProperties = TRUE then
                for j := 1 to pDSSClass.NumProperties do
                    WriteLnCB('  ' + pDSSClass.PropertyName[j] + ': ' + pDSSClass.GetPropertyHelp(j), DSSMessageType.Help);
        end;
    end;

    HelpList.Free;
end;

procedure ShowGeneralHelp;
begin
    WriteLnCB(
        _('For specific help, enter:') + CRLF + 
        _('  "help command [cmd]" lists all executive commands, or') + CRLF + 
        _('                       if [cmd] provided, details on that command') + CRLF + 
        _('  "help option [opt]"  lists all simulator options, or') + CRLF + 
        _('                       if [opt] provided, details on that option') + CRLF + 
        _('  "help show [opt]"    lists the options to "show" various outputs, or') + CRLF + 
        _('                       if [opt] provided, details on that output') + CRLF + 
        _('  "help export [fmt]"  lists the options to "export" in various formats, or') + CRLF + 
        _('                       if [fmt] provided, details on that format') + CRLF + 
        _('  "help class [cls]"   lists the names of all available circuit model classes, or') + CRLF + 
        _('                       if [cls] provided, details on that class') + CRLF + 
        _('You may truncate any help topic name, which returns all matching entries'),
        DSSMessageType.Help
    );
end;

procedure ShowAnyHelp(cmd: ArrayOfString; const opt: String; const prefix: String);
var
    i: Integer;
    lst: TStringList;
    msg: String = '';
begin
    if Length(opt) < 1 then
    begin
        Writeln('if Length(opt) < 1 then');
        lst := TStringList.Create;
        for i := 0 to High(cmd) do
            lst.Add(PadRight(cmd[i], colwidth));
        lst.Sort;
        for i := 1 to Length(cmd) do
            if ((i mod numcols) = 0) then
            begin
                msg := msg + lst[i - 1];
                WriteLnCB(msg, DSSMessageType.Help);
                msg := '';
            end
            else
                msg := msg + lst[i - 1] + ' ';
                
            if length(msg) > 0 then
                WriteLnCB(msg, DSSMessageType.Help);

        lst.Free;
        Exit;
    end;

    for i := 0 to High(cmd) do
    begin
        if AnsiStartsStr(opt, AnsiLowerCase(cmd[i])) then
        begin
            WriteLnCB(AnsiUpperCase(cmd[i]), DSSMessageType.Help);
            WriteLnCB('======================', DSSMessageType.Help);
            WriteLnCB(DSSHelp(prefix + '.' + cmd[i]), DSSMessageType.Help);
            WriteLnCB(msg, DSSMessageType.Help);
        end;
    end;
end;

procedure ShowClassHelp(DSSClassList: TDSSPointerList; const opt: String);
var
    pDSSClass: TDSSClass;
    i: Integer;
begin
    if Length(opt) > 0 then
    begin
        pDSSClass := DSSClassList.First;
        while pDSSClass <> NIL do
        begin
            if AnsiStartsStr(opt, AnsiLowerCase(pDSSClass.name)) then
            begin
                WriteLnCB(AnsiUpperCase(pDSSClass.name), DSSMessageType.Help);
                WriteLnCB('======================', DSSMessageType.Help);
                for i := 1 to pDSSClass.NumProperties do
                    WriteLnCB('  ' + pDSSClass.PropertyName[i] + ': ' + pDSSClass.GetPropertyHelp(i), DSSMessageType.Help);
            end;
            pDSSClass := DSSClassList.Next;
        end;
        Exit;
    end;

    WriteLnCB(_('== Power Delivery Elements =='), DSSMessageType.Help);
    AddHelpForClasses(DSSClassList, PD_ELEMENT, FALSE);
    WriteLnCB(_('== Power Conversion Elements =='), DSSMessageType.Help);
    AddHelpForClasses(DSSClassList, PC_ELEMENT, FALSE);
    WriteLnCB(_('== Control Elements =='), DSSMessageType.Help);
    AddHelpForClasses(DSSClassList, CTRL_ELEMENT, FALSE);
    WriteLnCB(_('== Metering Elements =='), DSSMessageType.Help);
    AddHelpForClasses(DSSClassList, METER_ELEMENT, FALSE);
    WriteLnCB(_('== Supporting Elements =='), DSSMessageType.Help);
    AddHelpForClasses(DSSClassList, 0, FALSE);
    WriteLnCB(_('== Other Elements =='), DSSMessageType.Help);
    AddHelpForClasses(DSSClassList, NON_PCPD_ELEM, FALSE);
end;

function StringToHTML(const s: String): String; //TODO
begin
    Result := s;
    Result := StringReplace(Result, CRLF, '<br>', [rfReplaceAll]);
    // Result := StringReplace(Result, '|', '\|', [rfReplaceAll]);
    // Result := StringReplace(Result, '*', '\*', [rfReplaceAll]);
    // Result := StringReplace(Result, '~', '\~', [rfReplaceAll]);
end;

procedure AddHelpForClassesMD(DSS: TDSSContext; BaseClass: Word);
var
    HelpList: TList;
    pDSSClass: TDSSClass;
    i, j: Integer;
begin
    HelpList := TList.Create();
    pDSSClass := DSS.DSSClassList.First;
    while pDSSClass <> NIL do
    begin
        if (pDSSClass.DSSClassType and BASECLASSMASK) = BaseClass then
            HelpList.Add(pDSSClass);
        pDSSClass := DSS.DSSClassList.Next;
    end;
    HelpList.Sort(@CompareClassNames);


    for i := 1 to HelpList.Count do
    begin
        pDSSClass := HelpList.Items[i - 1];
        WriteLnCB(Format(_('#### `%s` properties'), [pDSSClass.name]), DSSMessageType.Help);
        WriteLnCB('', DSSMessageType.Help);

        WriteLnCB('<table>', DSSMessageType.Help);
        WriteLnCB(Format('<tr><th>%s</th><th>%s</th><th>%s</th><th>Migrated?</th></tr>', [_('Number'), _('Name'), _('Description')]), DSSMessageType.Help);

        for j := 1 to pDSSClass.NumProperties do
            WriteLnCB(Format(
                    '<tr><td>%d</td><td>%s</td><td>%s</td><td>%s</td></tr>', 
                [j, pDSSClass.PropertyName[j], StringToHTML(pDSSClass.GetPropertyHelp(j)), 'MIGRATED:' + StrYorN(pDSSClass.PropertyOffset[j] >= 0)]
            ), DSSMessageType.Help);
                
        WriteLnCB('</table>', DSSMessageType.Help);
        WriteLnCB('', DSSMessageType.Help);
    end;
    
    HelpList.Free;
end;

procedure ShowAnyHelpMD(cmd: ArrayOfString; what: String);
var
    i, j: Integer;
    lst: TStringList;
begin
    lst := TStringList.Create;
    for i := 0 to High(cmd) do
        lst.Add(cmd[i]);

    lst.Sort;
    
    WriteLnCB('<table>', DSSMessageType.Help);
    WriteLnCB(Format('<tr><th>%s</th><th>%s</th></tr>', [what,  _('Description')]), DSSMessageType.Help);
    for i := 0 to High(cmd) do
        for j := 0 to High(cmd) do
            if cmd[j] = lst[i] then
            begin
                WriteLnCB(Format('<tr><td>%s</td><td>%s</td></tr>', [cmd[j], StringToHTML(DSSHelp(what + '.' + cmd[j]))]), DSSMessageType.Help);
                break;
            end;
    
    WriteLnCB('</table>', DSSMessageType.Help);
    lst.Free;
    WriteLnCB('', DSSMessageType.Help);
end;

procedure ShowAllHelpMD(DSS: TDSSContext); // TODO: use plain HTML for everything instead?
begin
    WriteLnCB(_('# DSS Extensions: OpenDSS Commands and Properties'), DSSMessageType.Help);
    WriteLnCB('', DSSMessageType.Help);
    WriteLnCB('---', DSSMessageType.Help);
    WriteLnCB('', DSSMessageType.Help);
    WriteLnCB(Format(_('**This document was generated from:** `%s`'), [VersionString]), DSSMessageType.Help);
    WriteLnCB('', DSSMessageType.Help);
    WriteLnCB(_('*Generated with the legacy models disabled (i.e. OpenDSS v9+ compatibility mode).*'), DSSMessageType.Help);
    WriteLnCB('', DSSMessageType.Help);
    WriteLnCB('---', DSSMessageType.Help);
    WriteLnCB('', DSSMessageType.Help);
    WriteLnCB(_('## About this'), DSSMessageType.Help);
    WriteLnCB('', DSSMessageType.Help);
    WriteLnCB(_('This is a document automatically generated from the commands, options and properties for the DSS language (script level) exposed in the DSS Extensions version of the OpenDSS engine. A separate document will be developed in the future to detail **API** functions and general usage recommendations for the projects under DSS Extensions.'), DSSMessageType.Help);
    WriteLnCB('', DSSMessageType.Help);
    WriteLnCB(_('Since the extensive majority of properties and elements are compatible, this document can be useful when using either the official OpenDSS implementation or the DSS Extensions version (DSS C-API engine), consumed through the projects DSS Python (`dss_python`), OpenDSSDirect.py, OpenDSSDirect.jl, DSS Sharp (`dss_sharp`), and DSS MATLAB (`dss_matlab`).  If you are using the official OpenDSS, when in doubt check the official documentation and/or source code.'), DSSMessageType.Help);
    WriteLnCB('', DSSMessageType.Help);
    WriteLnCB(_('As a final note, keep in mind that not all commands are implemented in the DSS Extensions engine, interactive commands like plots are missing (on purpose).'), DSSMessageType.Help);
    WriteLnCB('', DSSMessageType.Help);
    
    WriteLnCB('---', DSSMessageType.Help);
    WriteLnCB(_('## Commands'), DSSMessageType.Help);
    WriteLnCB('', DSSMessageType.Help);

    ShowAnyHelpMD(DSS.DSSExecutive.ExecCommand, 'Command');
    
    WriteLnCB('---', DSSMessageType.Help);
    WriteLnCB(_('## Execution Options'), DSSMessageType.Help);
    WriteLnCB('', DSSMessageType.Help);
    ShowAnyHelpMD( DSS.DSSExecutive.ExecOption, 'Executive');
    
    WriteLnCB('---', DSSMessageType.Help);
    WriteLnCB(_('## `Show` options'), DSSMessageType.Help);
    WriteLnCB('', DSSMessageType.Help);
    ShowAnyHelpMD(DSS.DSSExecutive.ShowOption, 'ShowOption');
    
    WriteLnCB('---', DSSMessageType.Help);
    WriteLnCB(_('## `Export` options'), DSSMessageType.Help);
    WriteLnCB('', DSSMessageType.Help);
    ShowAnyHelpMD(DSS.DSSExecutive.ExportOption, 'ExportOption');
    
    WriteLnCB('---', DSSMessageType.Help);
    WriteLnCB(_('## Elements'), DSSMessageType.Help);
    WriteLnCB('', DSSMessageType.Help);
    WriteLnCB('---', DSSMessageType.Help);
    WriteLnCB(_('### Power Delivery Elements'), DSSMessageType.Help);
    WriteLnCB('', DSSMessageType.Help);
    AddHelpForClassesMD(DSS, PD_ELEMENT);
    WriteLnCB('---', DSSMessageType.Help);
    WriteLnCB(_('### Power Conversion Elements'), DSSMessageType.Help);
    WriteLnCB('', DSSMessageType.Help);
    AddHelpForClassesMD(DSS, PC_ELEMENT);
    WriteLnCB('---', DSSMessageType.Help);
    WriteLnCB(_('###  Control Elements'), DSSMessageType.Help);
    WriteLnCB('', DSSMessageType.Help);
    AddHelpForClassesMD(DSS, CTRL_ELEMENT);
    WriteLnCB('---', DSSMessageType.Help);
    WriteLnCB(_('### Metering Elements'), DSSMessageType.Help);
    WriteLnCB('', DSSMessageType.Help);
    AddHelpForClassesMD(DSS, METER_ELEMENT);
    WriteLnCB('---', DSSMessageType.Help);
    WriteLnCB(_('### Supporting Elements'), DSSMessageType.Help);
    WriteLnCB('', DSSMessageType.Help);
    AddHelpForClassesMD(DSS, 0);
    WriteLnCB('---', DSSMessageType.Help);
    WriteLnCB(_('### Other Elements'), DSSMessageType.Help);
    WriteLnCB('', DSSMessageType.Help);
    AddHelpForClassesMD(DSS, NON_PCPD_ELEM);
end;

procedure ShowHelpForm(dssContext: TObject);
var
    Param, OptName: String;
    DSS: TDSSContext;
begin
    DSS := TDSSContext(dssContext);
    DSS.Parser.NextParam;
    Param := AnsiLowerCase(DSS.Parser.StrValue);
    DSS.Parser.NextParam;
    OptName := AnsiLowerCase(DSS.Parser.StrValue);
    
    if ANSIStartsStr('markdown', param) then
        ShowAllHelpMD(DSS)
    else
    if ANSIStartsStr('com', param) then
        ShowAnyHelp(DSS.DSSExecutive.ExecCommand, OptName, 'Command')
    else
    if ANSIStartsStr('op', param) then
        ShowAnyHelp(DSS.DSSExecutive.ExecOption, OptName, 'Executive')
    else
    if ANSIStartsStr('sh', param) then
        ShowAnyHelp(DSS.DSSExecutive.ShowOption, OptName, 'ShowOption')
    else
    if ANSIStartsStr('e', param) then
        ShowAnyHelp(DSS.DSSExecutive.ExportOption, OptName, 'ExportOption')
    else
    if ANSIStartsStr('cl', param) then
        ShowClassHelp(DSS.DSSClassList, OptName)
    else
        ShowGeneralHelp;
end;

procedure ShowMessageForm(S: TStrings);
begin
    if NoFormsAllowed then
        Exit;
    WriteLnCB(s.text, DSSMessageType.General);
end;

procedure ShowPropEditForm;
begin
    // DoSimpleMsg(DSSPrime, 'Not implemented', 999);
end;

procedure CloseDownForms;
begin
end;

end.
