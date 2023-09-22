unit DDSS;

interface

function DSSI(mode:longint;arg:longint):longint;cdecl;
function DSSS(mode:longint;arg:pAnsiChar):pAnsiChar;cdecl;
procedure DSSV(mode:longint; var myPointer: Pointer; var myType, mySize: longint);cdecl;

implementation

uses DSSClassDefs,
     DSSGlobals,
     {$IFDEF FPC}
     CmdForms,
     {$ELSE}
     DSSForms,
     Forms,
     ScriptFormNormal,
     {$ENDIF}
     DSSClass,
     Exechelper,
     sysUtils,
     Executive,
     Variants,
     ParserDel,
     ExecCommands, ExecOptions;

function DSSI(mode:longint;arg:longint):longint;cdecl;
begin
  Result:=0;
  case mode of
  0: begin  // DSS.NumCircuits
      Result := ActiveCircuit[ActiveActor].NumCircuits;
  end;
  1: begin  // DSS.ClearAll
      DoClearAllCmd;
  end;
  2: begin
    {$IFDEF FPC}
    Result:=0; // edit form not supported in FPC, but don't throw an error for trying...
    {$ELSE}
    If Not Assigned (MainEditFormNormal) Then
    Begin
          MainEditFormNormal := TMainEditFormnormal.Create(Nil);
          MainEditFormNormal.Caption := 'OpenDSS Script Form';
          MainEditFormNormal.isMainWindow := TRUE;
    End;
    MainEditFormNormal.Show;
    {$ENDIF}
  end;
  3: begin  // DSS.Start
    Result :=  1;
  end;
  4: begin  // DSS.NumClasses
    Result := NumIntrinsicClasses;
  end;
  5: begin  // DSS.NumUserClasses
    Result := NumUserClasses;
  end;
  6: begin  // DSS.Reset
        {Put any code here necessary to reset for specific systems};
  end;
  7: begin  // DSS.Allowforms read
     if NoFormsAllowed then Result:=1
     else Result:=0;
  end;
  8: begin  // DSS.Allowforms write
     If arg=0 Then NoFormsAllowed := TRUE  // Only set to False
     else NoFormsAllowed := FALSE;
//     If NoFormsAllowed Then CloseDownForms;  // DSSForms
  end
  else
      Result:=-1;
  end;
end;

//********************************String Type properties***************************
function DSSS(mode:longint;arg:pAnsiChar):pAnsiChar;cdecl;
begin
  Result:=pAnsiChar(AnsiString('0')); // Default return value
  case mode of
  0: begin  // DSS.NewCircuit
     MakeNewCircuit(arg);
     Result := pAnsiChar(AnsiString('New Circuit'));
  end;
  1: begin  // DSS.Version
     Result := pAnsiChar(AnsiString(VersionString +'; License Status: Open '));
  end;
  2: begin  // DSS.DataPath read
     Result := pAnsiChar(AnsiString(DataDirectory[ActiveActor]));
  end;
  3: begin  // DSS.DataPath write
     SetDataPath(arg);
  end;
  4: begin  // DSS.DefaultEditor
     Result := pAnsiChar(AnsiString(DSSGlobals.DefaultEditor));
  end
  else
      Result:= pAnsiChar(AnsiString('Error, parameter not valid'));
  end;
end;

//********************************Variant Type properties***************************
procedure DSSV(mode:longint; var myPointer: Pointer; var myType, mySize: longint);cdecl;

Var
  i   : Integer;

begin
  case mode of
  0: begin  // DSS.Classes
    myType  :=  4;        // String
    setlength(myStrArray, 0);
    For i := 1 to NumIntrinsicClasses Do
    Begin
      WriteStr2Array(TDSSClass(DssClassList[ActiveActor].Get(i)).Name);
      WriteStr2Array(Char(0));
    End;
    myPointer :=  @(myStrArray[0]);
    mySize    :=  Length(myStrArray);
  end;
  1: begin  // DSS.UserClasses
    myType  :=  4;        // String
    setlength(myStrArray, 0);
    If NumUserClasses > 0 Then
    Begin
      For i := NumIntrinsicClasses+1 To DSSClassList[ActiveActor].ListSize   Do
      Begin
        WriteStr2Array(TDSSClass(DssClassList[ActiveActor].Get(i)).Name);
        WriteStr2Array(Char(0));
      End;
    End
    Else WriteStr2Array('');
    myPointer :=  @(myStrArray[0]);
    mySize    :=  Length(myStrArray);
  end
  else
    myType  :=  4;        // String
    setlength(myStrArray, 0);
    WriteStr2Array('Error, parameter not recognized');
    myPointer :=  @(myStrArray[0]);
    mySize    :=  Length(myStrArray);
  end;
end;


end.
