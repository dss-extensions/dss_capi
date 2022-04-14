unit ExecHelper;
{
  ----------------------------------------------------------
  Copyright (c) 2008-2015, Electric Power Research Institute, Inc.
  All rights reserved.
  ----------------------------------------------------------
}

// Functions for performing DSS Exec Commands and Options

{$MODE DELPHI}

interface

uses 
    Executive,
    Classes;

type 
    TExecHelper = class helper for TExecutive
    public
        FUNCTION DoNewCmd:Integer;
        FUNCTION DoEditCmd:Integer;
        FUNCTION DoBatchEditCmd:Integer;
        FUNCTION DoSelectCmd:Integer;
        FUNCTION DoMoreCmd:Integer;
        FUNCTION DoRedirect(IsCompile:Boolean):Integer;
        FUNCTION DoSaveCmd:Integer;
        FUNCTION DoSampleCmd:Integer;


        FUNCTION DoSolveCmd:Integer;
        FUNCTION DoEnableCmd:Integer;
        FUNCTION DoDisableCmd:Integer;

        FUNCTION DoOpenCmd:Integer;
        FUNCTION DoResetCmd:Integer;
        FUNCTION DoNextCmd:Integer;
        FUNCTION DoFormEditCmd:Integer;  
        FUNCTION DoClassesCmd:Integer;
        FUNCTION DoUserClassesCmd:Integer;
        FUNCTION DoHelpCmd:Integer;
        FUNCTION DoClearCmd:Integer;
{$IFDEF DSS_CAPI_PM}
        FUNCTION DoClearAllCmd:Integer;
{$ENDIF}
        FUNCTION DoReduceCmd:Integer;
        FUNCTION DoInterpolateCmd:Integer;

        FUNCTION DoCloseCmd:Integer;
        FUNCTION DoResetMonitors:Integer;

        FUNCTION DoFileEditCmd:Integer;
        FUNCTION DoQueryCmd:Integer;
        FUNCTION DoResetMeters:Integer;
        PROCEDURE DoAboutBox;
        FUNCTION  DoSetVoltageBases:Integer;
        FUNCTION DoSetkVBase: Integer;

        PROCEDURE DoLegalVoltageBases;
        PROCEDURE DoAutoAddBusList(Const S:String);
        PROCEDURE DoKeeperBusList(Const S:String);
        PROCEDURE DoSetReduceStrategy(Const S:String);
        PROCEDURE DoSetAllocationFactors(Const X:Double);
        PROCEDURE DoSetCFactors(Const X:Double);

        FUNCTION DovoltagesCmd(Const PerUnit:Boolean): Integer;
        FUNCTION DocurrentsCmd :Integer;
        FUNCTION DopowersCmd(Total: Integer) :Integer;
        FUNCTION DoseqvoltagesCmd :Integer;
        FUNCTION DoseqcurrentsCmd :Integer;
        FUNCTION DoseqpowersCmd :Integer;
        FUNCTION DolossesCmd :Integer;
        FUNCTION DophaselossesCmd :Integer;
        FUNCTION DocktlossesCmd :Integer;
        FUNCTION DoAllocateLoadsCmd :Integer;
        FUNCTION DoHarmonicsList(const S:String):Integer;
        FUNCTION DoMeterTotals:Integer;
        FUNCTION DoCapacityCmd:Integer;
        FUNCTION DoZscCmd(Zmatrix:Boolean): Integer;
        FUNCTION DoZsc10Cmd: Integer;
        FUNCTION DoZscRefresh:Integer;
        FUNCTION DoZsc012Cmd: Integer;

        FUNCTION DoBusCoordsCmd(SwapXY:Boolean):Integer;
        FUNCTION DoUuidsCmd:Integer;
        FUNCTION DoSetLoadAndGenKVCmd:Integer;
        FUNCTION DoVarValuesCmd:Integer;
        FUNCTION DoVarNamesCmd :Integer;

        FUNCTION DoMakePosSeq:Integer;
        FUNCTION DoAlignFileCmd:Integer;
        FUNCTION DoRotateCmd:Integer;
        FUNCTION DoVDiffCmd:Integer;
        FUNCTION DoSummaryCmd:Integer;
        Function DoDistributeCmd:Integer;
        FUNCTION DoDI_PlotCmd:Integer;
        FUNCTION DoCompareCasesCmd:Integer;
        FUNCTION DoYearlyCurvesCmd:Integer;
        FUNCTION DoVisualizeCmd:Integer;
        FUNCTION DoCloseDICmd:Integer;
        FUNCTION DoADOScmd:Integer;
        FUNCTION DoEstimateCmd:Integer;
        FUNCTION DoReconductorCmd:Integer;
        FUNCTION DoAddMarkerCmd:Integer;
        FUNCTION DoCvrtLoadshapesCmd:Integer;
        FUNCTION DoNodeDiffCmd:Integer;
        FUNCTION DoRephaseCmd:Integer;
        FUNCTION DoSetBusXYCmd:Integer;
        FUNCTION DoUpdateStorageCmd:Integer;
        FUNCTION DoPstCalc:Integer;
        FUNCTION DoValVarCmd:Integer;
        FUNCTION DoLambdaCalcs:Integer;
        FUNCTION DoVarCmd:Integer;
        FUNCTION DoNodeListCmd:Integer;
        FUNCTION DoRemoveCmd:Integer;

        PROCEDURE DoSetNormal(pctNormal:Double);

        PROCEDURE Set_Time;

        PROCEDURE ParseObjName(const fullname:String; VAR objname, propname:String);

        PROCEDURE GetObjClassAndName(VAR ObjClass,ObjName:String);

        FUNCTION AddObject(const ObjType, name:String):Integer;
        FUNCTION EditObject(const ObjType, name:String):Integer;

        PROCEDURE SetActiveCircuit(const cktname:String);

        FUNCTION SetActiveCktElement:Integer;

        FUNCTION DoPropertyDump:Integer;

    private
    
        procedure MarkCapandReactorBuses;
    end;

implementation

USES Command, ArrayDef, ParserDel, SysUtils, DSSClassDefs, DSSGlobals,
     Circuit, Monitor, {ShowResults, ExportResults,}
     DSSClass, DSSObject, Utilities, Solution,
     EnergyMeter, Generator, LoadShape, Load, PCElement,   CktElement,
     UComplex, DSSUcomplex,  mathutil,  Bus,  SolutionAlgs,
     CmdForms, ExecCommands,
     Dynamics, Capacitor, Reactor, Line, Lineunits, Math,
     CktElementClass, Sensor, ExportCIMXML, NamedObject,
     RegExpr,PstCalc,
     PDELement, ReduceAlgs, Ucmatrix, 
     BufStream, 
     fpjson, DSSHelper;

Var
   SaveCommands, DistributeCommands,  DI_PlotCommands,
   ReconductorCommands, RephaseCommands, AddMarkerCommands,
   SetBusXYCommands, PstCalcCommands, RemoveCommands   :TCommandList;

procedure TExecHelper.GetObjClassAndName(VAR ObjClass,ObjName:String);
VAR
   ParamName:String;
   Param:String;

begin
    //   We're looking for Object Definition:
    //
    //    ParamName = 'object' IF given
    //     and the name of the object
    //
    //     Object=Capacitor.C1
    //    or just Capacitor.C1
    //
    //   If no dot, last class is assumed

    ObjClass := '';
    ObjName := '';
    ParamName := AnsiLowerCase(DSS.Parser.NextParam);
    Param := DSS.Parser.StrValue;
    IF Length(ParamName)>0 THEN
    Begin   // IF specified, must be object or an abbreviation
        IF ComparetextShortest(ParamName, 'object')<>0 THEN
        Begin
            DoSimpleMsg(DSS, 'object=Class.Name expected as first parameter in command. %s', [CRLF + DSS.Parser.CmdString], 240);
            Exit;
        End;
    End;
    ParseObjectClassandName(DSS, Param, ObjClass, ObjName);     // see DSSGlobals
End;


function TExecHelper.DoNewCmd:Integer;

// Process the New Command
// new type=xxxx name=xxxx  editstring

// IF the device being added already exists, the default behavior is to
// treat the New command as an Edit command.  This may be overridden
// by setting the DuplicatesAllowed VARiable to true, in which CASE,
// the New command always results in a new device being added.

VAR
   ObjClass, ObjName:String;
   handle:Integer;

begin
     Result := 0;
     Handle := 0;

     GetObjClassAndName(ObjClass, ObjName);

     IF CompareText(ObjClass,'solution') = 0
     THEN Begin
         DoSimpleMsg(DSS, _('You cannot create new Solution objects through the command interface.'), 241);
         Exit;
     End;

     IF   CompareText(ObjClass,'circuit') = 0
     THEN Begin
            MakeNewCircuit(DSS, ObjName);  // Make a new circuit
            ClearEventLog(DSS);      // Start the event log in the current directory
            ClearErrorLog(DSS);
          End
     ELSE    // Everything else must be a circuit element or DSS Object
        Begin
          Handle := AddObject(ObjClass, ObjName);
        End;

     IF Handle=0 THEN Result := 1;
     
End;

function TExecHelper.DoEditCmd:Integer;

// edit type=xxxx name=xxxx  editstring
VAR
   ObjType, ObjName:String;

begin
     Result := 0;

     GetObjClassAndName(ObjType, ObjName);

     IF CompareText(ObjType, 'circuit')=0 THEN
     Begin
                 // Do nothing
     End
     ELSE
     begin
        // Everything ELSE must be a circuit element
        Result := EditObject(ObjType, ObjName);

     End;
end;

function TExecHelper.DoBatchEditCmd:Integer;
// batchedit type=xxxx name=pattern  editstring
VAR
   ObjType, Pattern:String;
   RegEx1: TRegExpr;
   pObj: TDSSObject;
   Params: Integer;
Begin
  Result := 0;
  GetObjClassAndName(ObjType, Pattern);
  IF CompareText(ObjType, 'circuit')=0 THEN Begin
    // Do nothing
  End ELSE begin
    DSS.LastClassReferenced := DSS.ClassNames.Find(ObjType);

    CASE DSS.LastClassReferenced of
      0: Begin
        DoSimpleMsg(DSS, 'BatchEdit Command: Object Type "%s" not found. %s', [ObjType, CRLF + DSS.Parser.CmdString], 267);
        Exit;
        End;{Error}
    ELSE
      Params:=DSS.Parser.Position;
      DSS.ActiveDSSClass := DSS.DSSClassList.Get(DSS.LastClassReferenced);
      RegEx1:=TRegExpr.Create;
      RegEx1.ModifierI := True; // equivalent to RegEx1.Options:=[preCaseLess]
      RegEx1.Expression:=UTF8String(Pattern);
      If DSS.ActiveDSSClass.First>0 then pObj:=DSS.ActiveDSSObject else pObj := Nil;
      while pObj <> Nil do begin
        if RegEx1.Exec(UTF8String(pObj.Name)) then begin
          DSS.Parser.Position:=Params;
          DSS.ActiveDSSClass.Edit(DSS.Parser);
        end;
        If DSS.ActiveDSSClass.Next>0 then pObj:=DSS.ActiveDSSObject else pObj := Nil;
      end;
      RegEx1.Free;
    End;
  End;
End;

function TExecHelper.DoRedirect(IsCompile:Boolean):Integer;
//  This routine should be recursive
//  So you can redirect input an arbitrary number of times

// If Compile, makes directory of the file the new home directory
// If not Compile (is simple redirect), return to where we started

VAR
    Fin: TextFile;
    InputLine, CurrDir, SaveDir, ReDirFileExp: String;
    LocalCompFileName: String;
    InBlockComment: Boolean;
    strings: TStringList;
    gotTheFile: Boolean;
    stringIdx: Integer;
    LineNum: Integer = 0;
    Fstream: TStream;
Begin
    gotTheFile := False;
    strings := nil;
    Result := 0;
    InBlockComment := FALSE;  // Discarded off stack upon return
    // Therefore extent of block comment does not extend beyond a file
    // Going back up the redirect stack

    if InZip then
    begin
        // Get next parm and try to interpret as a file name
        DSS.Parser.NextParam;

        if DSS.Parser.StrValue = '' then 
            exit;  // ignore altogether IF null filename

        try
            Fstream := GetZipStream(DSS.Parser.StrValue);
        except
            on E: Exception do
            begin
                DoSimpleMsg(DSS, 'Redirect File "%s" could not be read: %s', [DSS.Parser.StrValue, E.Message], 2202);
                DSS.SolutionAbort := TRUE;
                Exit;
            end;
        end;


        strings := TStringList.Create;
        strings.LoadFromStream(Fstream);
        Fstream.Free;
        ReDirFileExp := DSS.inZipPath + DSS.Parser.StrValue;
        gotTheFile := True;
        SaveDir := DSS.inZipPath;
    end
    else
    begin
        // Get next parm and try to interpret as a file name
        DSS.Parser.NextParam;
        
        // Expanded path is required later as other Free Pascal functions 
        // may fail with relative paths
        ReDirFileExp := ExpandFileName(DSS.Parser.StrValue);
        
        // First check if we need to workaround the SetCurrentDir issues
        if (not DSS_CAPI_ALLOW_CHANGE_DIR) then
        begin
            ReDirFileExp := ExpandFileName(AdjustInputFilePath(DSS, DSS.Parser.StrValue));
        end;
        
        DSS.ReDirFile := ReDirFileExp;// DSS.Parser.StrValue;
        if DSS.ReDirFile = '' then 
            exit;  // ignore altogether IF null filename

        SaveDir := DSS.CurrentDSSDir;
    end;
   

    if (not gotTheFile) and (FileExists(DSS.ReDirFile)) then
    begin
        // If the usual Pascal text file is broken, 
        // try a stream via a TStringList object
        try
            strings := TStringList.Create;
            strings.LoadFromFile(DSS.ReDirFile);
            if IsCompile Then 
            begin
                DSS.LastFileCompiled := DSS.ReDirFile;
                LocalCompFileName := DSS.ReDirFile;
            end;
            gotTheFile := True;
        except
            FreeAndNil(strings);
        end;
    end;
    
    if not gotTheFile then
    begin
        try
            // First try, using the provided name directly
            AssignFile(Fin, DSS.ReDirFile);
            Reset(Fin);
            if IsCompile Then 
            begin
                DSS.LastFileCompiled := DSS.ReDirFile;
                LocalCompFileName := DSS.ReDirFile;
            end;
            gotTheFile := True;
        except
            // intentionally blank
        end;
    end;
    
    // For full backwards compatibility
    DSS.ReDirFile := ReDirFileExp;

    if not gotTheFile then
    begin
        // Try the expanded name
        if DSS.ReDirFile = '' then
            exit;

        try
            AssignFile(Fin, DSS.ReDirFile);
            Reset(Fin);
            if IsCompile Then 
            begin
                DSS.LastFileCompiled := DSS.ReDirFile;
                LocalCompFileName := DSS.ReDirFile;
            end;
            gotTheFile := True;
        except
            // intentionally blank
        end;
    end;

    if not gotTheFile and FileExists(DSS.ReDirFile) then
    begin
        // If the usual Pascal text file is broken, 
        // try a stream via a TStringList object
        try
            strings := TStringList.Create;
            strings.LoadFromFile(DSS.ReDirFile);
            if IsCompile Then 
            begin
                DSS.LastFileCompiled := DSS.ReDirFile;
                LocalCompFileName := DSS.ReDirFile;
            end;
            gotTheFile := True;
        except
            FreeAndNil(strings);
        end;
    end;

    if not gotTheFile then
    begin
        // Couldn't find file
        // Try appending a '.dss' to the file name
        // If it doesn't already have an extension
        IF Pos('.', DSS.ReDirFile)=0 THEN 
        Begin
            DSS.ReDirFile := DSS.ReDirFile + '.dss';
            LocalCompFileName := DSS.ReDirFile;
            TRY
                AssignFile(Fin, DSS.ReDirFile);
                Reset(Fin);
            EXCEPT
                DoSimpleMsg(DSS, 'Redirect file not found: "%s"', [DSS.Parser.StrValue], 242);
                DSS.SolutionAbort := TRUE;
                Exit;
            End;
            gotTheFile := True;
        End;
    end;

    if not gotTheFile then
    begin
        DoSimpleMsg(DSS, 'Redirect file not found: "%s"', [DSS.Parser.StrValue], 243);
        DSS.SolutionAbort := True;
        exit;  // Already had an extension, so just bail
    end;
    
    // For full backwards compatibility
    DSS.ReDirFile := ReDirFileExp;

    // OK, we finally got one open, so we're going to continue
    TRY
        TRY
            // Change Directory to path specified by file in CASE that
            // loads in more files
            CurrDir := ExtractFileDir(DSS.ReDirFile);
            if not InZip then
            begin
                DSS.SetCurrentDSSDir(CurrDir);
                If IsCompile Then 
                    SetDataPath(DSS, CurrDir);  // change datadirectory
            end
            else
            begin
                SetInZipPath(CurrDir);
            end;

            DSS.Redirect_Abort := False;
            DSS.In_Redirect := True;

            if strings = nil then 
            begin
                // Traditional TextFile is used
                while not ( (EOF(Fin)) or (DSS.Redirect_Abort) ) do
                begin
                    Readln(Fin, InputLine);
                    Inc(LineNum);
                    if Length(InputLine) > 0 then
                    begin
                        if not InBlockComment then     // look for '/*'  at baginning of line
                            case InputLine[1] of
                                '/': 
                                    if (Length(InputLine) > 1) and (InputLine[2]='*') then
                                        InBlockComment := TRUE;
                            end;

                        if not InBlockComment then   // process the command line
                            if not DSS.SolutionAbort then 
                                Set_Command(InputLine, LineNum)
                            else 
                                DSS.Redirect_Abort := True;  // Abort file if solution was aborted

                        // in block comment ... look for */   and cancel block comment (whole line)
                        if InBlockComment then
                            if Pos('*/', Inputline) > 0 then
                                InBlockComment := FALSE;

                    end;
                    
                End // WHILE Not ( (EOF(Fin)) or (Redirect_Abort) ) DO
            end 
            else
            begin
                // The string list is used
                for stringIdx := 0 to (strings.Count - 1) do
                begin
                    if DSS.Redirect_Abort then 
                        break;
                        
                    LineNum := stringIdx + 1;
                    InputLine := strings[stringIdx];
                    if Length(InputLine) > 0 then
                    BEGIN
                        if Not InBlockComment then     // look for '/*'  at baginning of line
                            case InputLine[1] of
                                '/': 
                                    if (Length(InputLine) > 1) and (InputLine[2]='*') then
                                        InBlockComment := TRUE;
                            end;

                        If Not InBlockComment Then   // process the command line
                            If Not DSS.SolutionAbort Then 
                                Set_Command(InputLine, LineNum)
                            Else 
                                DSS.Redirect_Abort := True;  // Abort file if solution was aborted

                        // in block comment ... look for */   and cancel block comment (whole line)
                        if InBlockComment then
                            if Pos('*/', Inputline) > 0 then
                                InBlockComment := FALSE;
                    END;
                End; // for stringIdx := 1 to strings.Count do
            end;

            IF DSS.ActiveCircuit <> Nil THEN 
                DSS.ActiveCircuit.CurrentDirectory := CurrDir + PathDelim;

        EXCEPT On E: Exception DO
            DoErrorMsg(DSS, _('DoRedirect: Error Processing Input Stream in Compile/Redirect.'),
                        E.Message,
                        Format(_('Error in File: "%s" or Filename itself.'), [DSS.ReDirFile]), 244);
        END;
    FINALLY
        if gotTheFile and (DSS.Redirect_Abort or (DSS.ErrorNumber <> 0)) then
        begin
            DSS.LastErrorMessage := DSS.LastErrorMessage + CRLF + 
              Format(_('[file: "%s", line: %d]'), [ReDirFileExp, LineNum]);
        end;

        if strings <> nil then
            FreeAndNil(strings)
        else
            CloseFile(Fin);
            
        DSS.In_Redirect := False;
        DSS.ParserVars.Add('@lastfile', DSS.ReDirFile) ;

        if not InZip then
        begin        
            If IsCompile Then
            Begin
                SetDataPath(DSS, CurrDir); // change datadirectory
                DSS.LastCommandWasCompile := True;
                DSS.ParserVars.Add('@lastcompilefile', LocalCompFileName); // will be last one off the stack
            End
            Else 
            Begin
                DSS.SetCurrentDSSDir(SaveDir);    // set back to where we were for redirect, but not compile
                DSS.ParserVars.Add('@lastredirectfile', DSS.ReDirFile);
            End;
        end
        else
        begin
            if not IsCompile then
                SetInZipPath(SaveDir);
        end;
    END;
End;

function TExecHelper.DoSelectCmd:Integer;

// select active object
// select element=elementname terminal=terminalnumber
VAR
   ObjClass, ObjName,
   Param:String;

begin
     Result := 1;

     GetObjClassAndName(ObjClass, ObjName);  // Parse Object class and name

     If (Length(ObjClass)=0) and (Length(ObjName)=0) Then Exit;  // select active obj if any

     IF CompareText(ObjClass, 'circuit')=0 THEN
     Begin
           SetActiveCircuit(ObjName);
     End
     ELSE
     begin
        // Everything else must be a circuit element
        IF Length(ObjClass)>0 THEN SetObjectClass(DSS, ObjClass);

        DSS.ActiveDSSClass := DSS.DSSClassList.Get(DSS.LastClassReferenced);
        IF DSS.ActiveDSSClass<>Nil THEN
        Begin
          IF Not DSS.ActiveDSSClass.SetActive(Objname) THEN
          Begin // scroll through list of objects untill a match
            DoSimpleMsg(DSS, 'Error! Object "%s" not found. %s', [ObjName, CRLF + DSS.Parser.CmdString], 245);
            Result := 0;
          End
          ELSE
          WITH DSS.ActiveCircuit Do
          Begin
             CASE DSS.ActiveDSSObject.DSSObjType OF
                  DSS_OBJECT: ;  // do nothing for general DSS object

             ELSE Begin   // for circuit types, set DSS.ActiveCircuit Element, too
                   ActiveCktElement := DSS.ActiveDSSClass.GetActiveObj;
                   // Now check for active terminal designation
                   DSS.Parser.NextParam;
                   Param := DSS.Parser.StrValue;
                   If Length(Param)>0
                   THEN ActiveCktElement.ActiveTerminalIdx := DSS.Parser.Intvalue
                   ELSE ActiveCktElement.ActiveTerminalIdx := 1;  {default to 1}
                   With ActiveCktElement Do SetActiveBus(DSS, StripExtension(Getbus(ActiveTerminalIdx)));
                  End;
             End;
          End;
        End
        ELSE Begin
          DoSimpleMsg(DSS, _('Error! Active object type/class is not set.'), 246);
          Result := 0;
        End;

     End;
end;

function TExecHelper.DoMoreCmd:Integer;
// more editstring  (assumes active circuit element)
Begin
    if DSS.ActiveDSSClass <> NIL then 
        Result := DSS.ActiveDSSClass.Edit(DSS.Parser)
    else
        Result := 0;
End;

FUNCTION TExecHelper.DoSaveCmd:Integer;
// Save current values in both monitors and Meters
VAR
   pMon :TMonitorObj;
   pMtr :TEnergyMeterObj;
   i    :Integer;

   ParamPointer :Integer;
   ParamName,
   Param        :String;
   ObjClass     :String;
   SaveDir      :String;
   saveFile     :String;
   DSSClass     :TDSSClass;
Begin
     Result := 0;
     ObjClass := '';
     SaveDir := DSS.OutputDirectory; {CurrentDSSDir;}
     SaveFile := '';
     ParamPointer := 0;
     ParamName := DSS.Parser.NextParam;
     Param := DSS.Parser.StrValue;
     WHILE Length(Param)>0 DO
     Begin
         IF   (Length(ParamName) = 0)  THEN Inc(ParamPointer)
         ELSE ParamPointer := SaveCommands.GetCommand(ParamName);

         CASE ParamPointer OF
           1: ObjClass := DSS.Parser.StrValue;
           2: Savefile := DSS.Parser.StrValue;   // File name for saving  a class
           3: SaveDir := DSS.Parser.StrValue;
         ELSE

         End;

         ParamName := DSS.Parser.NextParam;
         Param := DSS.Parser.StrValue;
     End;

   DSS.InShowResults := True;
   If (Length(ObjClass)=0) or (CompareTextShortest( ObjClass, 'meters')=0 ) then Begin
   // Save monitors and Meters

     WITH DSS.ActiveCircuit.Monitors Do
     FOR i := 1 to Count Do
     Begin
         pMon := Get(i);
         pMon.Save;
     End;

     WITH DSS.ActiveCircuit.EnergyMeters Do
     FOR i := 1 to Count Do
     Begin
         pMtr := Get(i);
         pMtr.SaveRegisters;
     End;

     Exit;
   End;
   If CompareTextShortest( ObjClass, 'circuit')=0 then  Begin
      IF not DSS.ActiveCircuit.Save(SaveDir) Then Result := 1;
      Exit;
   End;
   If CompareTextShortest( ObjClass, 'voltages')=0 then  Begin
      DSS.ActiveCircuit.Solution.SaveVoltages;
      Exit;
   End;

   {Assume that we have a class name for a DSS Class}
   DSSClass :=  GetDSSClassPtr(DSS, ObjClass);
   If DSSClass <> Nil Then Begin
     IF Length(SaveFile)=0 Then SaveFile := objClass;
     IF Length(SaveDir)>0 Then begin
       If not DirectoryExists(SaveDir) Then
          Try
             mkDir(SaveDir);
          Except
             On E:Exception Do DoSimpleMsg(DSS, 'Error making Directory: "%s". %s', [SaveDir, E.Message], 247);
          End;
       SaveFile := SaveDir + PathDelim + SaveFile;
     End;
     WriteClassFile(DSS, DSSClass, SaveFile, FALSE); // just write the class with no checks
   End;

   SetLastResultFile(DSS, SaveFile);
   DSS.GlobalResult := SaveFile;
end;

function TExecHelper.DoClearCmd:Integer;
Begin
    DSS.DSSExecutive.Clear;
    Result := 0;
End;
{$IFDEF DSS_CAPI_PM}
function TExecHelper.DoClearAllCmd:Integer;
Begin
    DSS.DSSExecutive.ClearAll;
    Result := 0;
end;
{$ENDIF}
function TExecHelper.DoHelpCmd:Integer;
Begin
    ShowHelpForm(DSS); // DSSForms Unit
    Result := 0;
End;
function TExecHelper.DoSampleCmd:Integer;
// Force all monitors and meters in active circuit to take a sample
Begin
    DSS.MonitorClass.SampleAll;
    DSS.EnergyMeterClass.SampleAll;  // gets generators too
    Result := 0;
End;
function TExecHelper.DoSolveCmd:Integer;
Begin
    Result := 0;
    DSS.ActiveCircuit.Solution.Solve();
End;

function TExecHelper.SetActiveCktElement:Integer;
// Parses the object off the line and sets it active as a circuitelement.
VAR
   ObjType, ObjName:String;

begin
     Result := 0;

     GetObjClassAndName(ObjType, ObjName);

     IF CompareText(ObjType, 'circuit')=0 THEN
     Begin
                 // Do nothing
     End
     ELSE
     begin
        IF CompareText(ObjType, DSS.ActiveDSSClass.Name)<>0 THEN
             DSS.LastClassReferenced := DSS.ClassNames.Find(ObjType);

        CASE DSS.LastClassReferenced of
          0: Begin
                 DoSimpleMsg(DSS, 'Object Type "%s" not found. %s', [ObjType, CRLF + DSS.Parser.CmdString], 253);
                 Result := 0;
                 Exit;
             End;{Error}
        ELSE

        // intrinsic and user Defined models
           DSS.ActiveDSSClass := DSS.DSSClassList.Get(DSS.LastClassReferenced);
           IF DSS.ActiveDSSClass.SetActive(ObjName) THEN
           WITH DSS.ActiveCircuit Do
           Begin // scroll through list of objects until a match
             CASE DSS.ActiveDSSObject.DSSObjType OF
                    DSS_OBJECT: DoSimpleMsg(DSS, 'Error in SetActiveCktElement: Object not a circuit Element. %s', [CRLF + DSS.Parser.CmdString], 254);
             ELSE Begin
                    ActiveCktElement := DSS.ActiveDSSClass.GetActiveObj;
                    Result:=1;
                  End;
             End;
           End;
        End;
     End;
End;


function TExecHelper.DoEnableCmd:Integer;
Var Objtype, ObjName:String;
    ClassPtr:TDSSClass;
    CktElem:TDSSCktElement;
    i:Integer;
begin
  //   Result := SetActiveCktElement;
  //  IF Result>0 THEN DSS.ActiveCircuit.ActiveCktElement.Enabled := True;

     Result := 0;

     GetObjClassAndName(ObjType, ObjName);

     IF CompareText(ObjType, 'circuit')=0 THEN
     Begin
                 // Do nothing
     End
     ELSE
     If Length(ObjType)>0 Then Begin
      // only applies to CktElementClass objects
       ClassPtr := GetDSSClassPtr(DSS, ObjType);
       If ClassPtr<> Nil Then begin
         If (ClassPtr.DSSClassType and BASECLASSMASK) > 0  then Begin
              // Everything else must be a circuit element
             If CompareText(ObjName,'*') = 0 Then Begin
               // Enable all elements of this class
               For i := 1 to ClassPtr.ElementCount Do Begin
                 CktElem := ClassPtr.ElementList.Get(i);
                 CktElem.Enabled := TRUE;
               End;

             End
             Else begin
              // just load up the parser and call the edit routine for the object in question

              DSS.Parser.CmdString := 'Enabled=true';  // Will only work for CktElements
              Result := EditObject(ObjType, ObjName);
             End;
         End;
       End;
     End;
end;

function TExecHelper.DoDisableCmd:Integer;
Var Objtype, ObjName:String;
    ClassPtr:TDSSClass;
    CktElem:TDSSCktElement;
    i:Integer;
Begin
     Result := 0;

     GetObjClassAndName(ObjType, ObjName);

     IF CompareText(ObjType, 'circuit')=0 THEN
     Begin
                 // Do nothing
     End
     ELSE
     If Length(ObjType)>0 Then Begin
      // only applies to CktElementClass objects
       ClassPtr := GetDSSClassPtr(DSS, ObjType);
       If ClassPtr<> Nil Then begin
         If (ClassPtr.DSSClassType and BASECLASSMASK) > 0  then Begin
              // Everything else must be a circuit element
             If CompareText(ObjName,'*') = 0 Then Begin
               // Disable all elements of this class
               For i := 1 to ClassPtr.ElementCount Do Begin
                 CktElem := ClassPtr.ElementList.Get(i);
                 CktElem.Enabled := FALSE;
               End;

             End
             Else begin
              // just load up the parser and call the edit routine for the object in question

              DSS.Parser.CmdString := 'Enabled=false';  // Will only work for CktElements
              Result := EditObject(ObjType, ObjName);
             End;
         End;
       End;
     End;
//     Result := SetActiveCktElement;
//     IF Result>0 THEN DSS.ActiveCircuit.ActiveCktElement.Enabled := False;
End;

function TExecHelper.DoPropertyDump:Integer;

VAR
   pObject:TDSSObject;
   F: TFileStream = nil;
   SingleObject, Debugdump, IsSolution:Boolean;
   i:Integer;
   FileName:String;
   Param, Param2, ObjClass, ObjName:String;
begin
 Result := 0;
 SingleObject := False;
 IsSolution := False;
 DebugDump := False;
 ObjClass := ' ';  // make sure these have at least one character
 ObjName := ' ';
 
 // Continue parsing command line - check for object name
 DSS.Parser.NextParam;
 Param := DSS.Parser.StrValue;
 IF Length(Param)>0 THEN
 begin
    IF CompareText(Param, 'commands')=0 THEN
    Begin
        DumpAllDSSCommands(DSS, FileName);
{$IFDEF DSS_CAPI}DSS.GlobalResult := FileName;{$ENDIF}
        FireOffEditor(DSS, FileName);
        Exit;
    End;

    // dump bus names hash list
    if CompareText(Param, 'buslist')=0 then
    Begin
        FileName := DSS.OutputDirectory +  'Bus_Hash_List.txt';
        DSS.ActiveCircuit.BusList.DumpToFile(FileName);
{$IFDEF DSS_CAPI}DSS.GlobalResult := FileName;{$ENDIF}
        FireOffEditor(DSS, FileName);
        Exit;
    End;

    // dump device names hash list
    if CompareText(Param, 'devicelist')=0 then
    Begin
        FileName := DSS.OutputDirectory +  'Device_Hash_List.txt';
        DSS.ActiveCircuit.DeviceList.DumpToFile(FileName);
{$IFDEF DSS_CAPI}DSS.GlobalResult := FileName;{$ENDIF}
        FireOffEditor(DSS, FileName);
        Exit;
    End;

    IF CompareText(Copy(AnsiLowerCase(Param),1,5), 'alloc')=0 THEN
    Begin
        FileName :=DSS.OutputDirectory + 'AllocationFactors.txt';
        DumpAllocationFactors(DSS, FileName);
{$IFDEF DSS_CAPI}DSS.GlobalResult := FileName;{$ENDIF}
        FireOffEditor(DSS, FileName);
        Exit;
    End;

    IF CompareText(Param,'debug')=0 THEN
       DebugDump := TRUE
    ELSE
    begin
       IF CompareText(Param,'solution')=0 THEN
         Begin
          // Assume active circuit solution IF not qualified
          // DSS.ActiveDSSClass := DSS.SolutionClass;
          // DSS.ActiveDSSObject := DSS.ActiveCircuit.Solution;
          IsSolution := TRUE;
         End
       ELSE
         Begin
            SingleObject := TRUE;
             // Check to see IF we want a debugdump on this object
            DSS.Parser.NextParam;
            Param2 := DSS.Parser.StrValue;
            IF CompareText(Param2,'debug')=0 THEN DebugDump := TRUE;
            // Set active Element to be value in Param
            DSS.Parser.CmdString := '"' + Param + '"';  // put param back into DSS.Parser
            GetObjClassAndName( ObjClass, ObjName);
            // IF DoSelectCmd=0 THEN Exit;  8-17-00
            IF SetObjectClass(DSS, ObjClass)
            THEN Begin
              DSS.ActiveDSSClass := DSS.DSSClassList.Get(DSS.LastClassReferenced);
              IF DSS.ActiveDSSClass = NIL Then Exit;
            End
            ELSE Exit;
         End;
    End;
 End;

  TRY
      F := TBufferedFileStream.Create(DSS.OutputDirectory + DSS.CircuitName_ + 'PropertyDump.txt', fmCreate);
  EXCEPT
      On E:Exception DO
      Begin
        DoErrorMsg(DSS, 
            Format(_('DoPropertyDump - opening %s DSS_PropertyDump.txt for writing in %s'), [DSS.OutputDirectory, Getcurrentdir]), 
                E.Message, _('Disk protected or other file error'), 255);
        Exit;
      End;
  End;

  TRY
      IF SingleObject THEN
      begin
        // IF ObjName='*' then we dump all objects of this class
        CASE ObjName[1] of
           '*':Begin
                  FOR i := 1 to DSS.ActiveDSSClass.ElementCount Do
                  Begin
                      DSS.ActiveDSSClass.Active := i;
                      DSS.ActiveDSSObject.DumpProperties(F, DebugDump);
                  End;
               End;
        ELSE
           IF Not DSS.ActiveDSSClass.SetActive(Objname)
           THEN Begin
               DoSimpleMsg(DSS, 'Error! Object "%s" not found.', [ObjName], 256) ;
               Exit;
           End
           ELSE DSS.ActiveDSSObject.DumpProperties(F, DebugDump);  // Dump only properties of active circuit element
        END;

      End
      ELSE IF IsSolution THEN  Begin
         DSS.ActiveCircuit.Solution.DumpProperties(F, DebugDump);
      End
      ELSE begin
        // Dump general Circuit stuff

        IF DebugDump THEN DSS.ActiveCircuit.DebugDump(F);
        // Dump circuit objects
        TRY
          pObject := DSS.ActiveCircuit.CktElements.First;
          WHILE pObject <> Nil DO
          Begin
              pObject.DumpProperties(F, DebugDump, True);
              pObject := DSS.ActiveCircuit.CktElements.Next;
          End;
          pObject := DSS.DSSObjs.First;
          WHILE pObject <> Nil DO
          Begin
              pObject.DumpProperties(F, DebugDump, True);
              pObject := DSS.DSSObjs.Next;
          End;
        EXCEPT
            On E:Exception DO
              DoErrorMsg(DSS, _('DoPropertyDump - Problem writing file.'), E.Message, 
              _('File may be read only, in use, or disk full?'), 257);
        End;

        DSS.ActiveCircuit.Solution.DumpProperties(F,DebugDump);
      End;

  FINALLY

         FreeAndNil(F);
  END;  {TRY}

  FileName := DSS.OutputDirectory + DSS.CircuitName_ + 'PropertyDump.txt';
{$IFDEF DSS_CAPI}DSS.GlobalResult := FileName;{$ENDIF}
  FireOffEditor(DSS, FileName);
end;

procedure TExecHelper.Set_Time;
// for interpreting time specified as an array "hour, sec"
VAR
   TimeArray:Array[1..2] of double;
Begin
     DSS.Parser.ParseAsVector(2, pDoubleArray(@TimeArray));
     WITH DSS.ActiveCircuit.Solution DO
     Begin
        DynaVars.intHour := Round(TimeArray[1]);
        DynaVars.t := TimeArray[2];
        Update_dblHour;
     End;
End;

procedure TExecHelper.SetActiveCircuit(const cktname:String);
VAR
   pCkt:TDSSCircuit;
begin
   pCkt := DSS.Circuits.First;
   WHILE pCkt<>nil DO
   Begin
       IF AnsiCompareText(pCkt.Name, cktname)=0 THEN
       Begin
           DSS.ActiveCircuit := pCkt;
           Exit;
       End;
       pCkt := DSS.Circuits.Next;
   End;

   // IF none is found, just leave as is after giving error

   DoSimpleMsg(DSS, 'Error! No circuit named "%s" found. Active circuit not changed.', [cktname], 258);
End;


procedure TExecHelper.DoLegalVoltageBases;
VAR
   Dummy :pDoubleArray;
   i,
   Num   :Integer;
begin
     Dummy := AllocMem(Sizeof(Double) * 1000); // Big Buffer
     Num   := DSS.Parser.ParseAsVector(1000, Dummy);
     // Parsing zero-fills the array

     // LegalVoltageBases is a zero-terminated array, so we have to allocate
     // one more than the number of actual values

     WITH DSS.ActiveCircuit Do
     Begin
       Reallocmem(LegalVoltageBases, Sizeof(LegalVoltageBases^[1])*(Num+1));
       FOR i := 1 to Num+1 Do LegalVoltageBases^[i] := Dummy^[i];
     End;

     Reallocmem(Dummy, 0);
End;

function TExecHelper.DoOpenCmd:Integer;
// Opens a terminal and conductor of a ckt Element
VAR
   retval    :Integer;
   Terminal  :Integer;
   Conductor :Integer;

// syntax:  "Open class.name term=xx cond=xx"
//  IF cond is omitted, all conductors are opened.
Begin
  retval := SetActiveCktElement;
  IF retval>0 THEN
  Begin
        DSS.Parser.NextParam;
        Terminal  := DSS.Parser.IntValue;
        DSS.Parser.NextParam;
        Conductor := DSS.Parser.IntValue;

        With DSS.ActiveCircuit Do
        Begin
              ActiveCktElement.ActiveTerminalIdx := Terminal;
              ActiveCktElement.Closed[Conductor] := FALSE;
              With ActiveCktElement Do SetActiveBus(DSS, StripExtension(Getbus(ActiveTerminalIdx)));
        End;
  End
  ELSE
  Begin
       DoSimpleMsg(DSS, 'Error in Open Command: Circuit Element not found. %s', [CRLF+ DSS.Parser.CmdString], 259);
  End;
  Result := 0;
End;

function TExecHelper.DoCloseCmd:Integer;
// Closes a terminal and conductor of a ckt Element
VAR
   retval:Integer;
   Terminal:Integer;
   Conductor:Integer;

// syntax:  "Close class.name term=xx cond=xx"
//  IF cond is omitted, all conductors are opened
Begin
  retval := SetActiveCktElement;
  IF retval>0 THEN
    Begin
       DSS.Parser.NextParam;                 
       Terminal  := DSS.Parser.IntValue;
       DSS.Parser.NextParam;
       Conductor := DSS.Parser.IntValue;

        With DSS.ActiveCircuit Do
         Begin
          ActiveCktElement.ActiveTerminalIdx := Terminal;
          ActiveCktElement.Closed[Conductor] := TRUE;
          With ActiveCktElement Do SetActiveBus(DSS, StripExtension(Getbus(ActiveTerminalIdx)));
         End;

    End
  ELSE
  Begin
       DoSimpleMsg(DSS, 'Error in Close Command: Circuit Element not found. %s', [CRLF+ DSS.Parser.CmdString], 260);
  End;
  Result := 0;
end;

function TExecHelper.DoResetCmd:Integer;
VAR
    Param  :String;
Begin
    Result := 0;
    // Get next parm and try to interpret as a file name
    DSS.Parser.NextParam;
    Param := AnsiUpperCase(DSS.Parser.StrValue);
    IF Length(Param) = 0
       THEN Begin
            DoResetMonitors;
            DoResetMeters;
            DoResetFaults(DSS);
            DoResetControls(DSS);
            ClearEventLog(DSS);
            ClearErrorLog(DSS);
            DoResetKeepList(DSS);
       End
    ELSE
      CASE Param[1] of
       'M': CASE Param[2] of
               'O'{MOnitor}:  DoResetMonitors;
               'E'{MEter}:    DoResetMeters;
            END;
       'F'{Faults}:   DoResetFaults(DSS);
       'C'{Controls}: DoResetControls(DSS);
       'E'{EventLog and ErrorLog}: Begin ClearEventLog(DSS);  ClearErrorLog(DSS); End;
       'K': DoResetKeepList(DSS);

      ELSE

         DoSimpleMsg(DSS, 'Unknown argument to Reset Command: "%s"', [Param], 261);

      End;
end;

procedure TExecHelper.MarkCapandReactorBuses;
Var
    pClass:TDSSClass;
    pCapElement:TCapacitorObj;
    pReacElement:TReactorObj;
    ObjRef:Integer;
begin
    // Mark all buses as keepers if there are capacitors or reactors on them
    pClass :=  GetDSSClassPtr(DSS, 'capacitor');
    If pClass<>Nil then
    Begin
       ObjRef := pClass.First;
       While Objref>0 Do
       Begin
          pCapElement := TCapacitorObj(DSS.ActiveDSSObject);
          If pCapElement.IsShunt Then
          Begin
             If pCapElement.Enabled Then  DSS.ActiveCircuit.Buses^[pCapElement.Terminals[0].Busref].Keep := TRUE;
          End;
          ObjRef := pClass.Next;
       End;
    End;

    // Now Get the Reactors
    pClass :=  GetDSSClassPtr(DSS, 'reactor');
    If pClass<>Nil then
    Begin
       ObjRef := pClass.First;
       While Objref>0 Do
       Begin
          pReacElement := TReactorObj(DSS.ActiveDSSObject);
          If pReacElement.IsShunt Then
          Try
             If pReacElement.Enabled Then DSS.ActiveCircuit.Buses^[pReacElement.Terminals[0].Busref].Keep := TRUE;
          Except
             On E:Exception Do Begin
               DoSimpleMsg(DSS, '%s %s Reactor=%s Bus No.=%d ',[E.Message, CRLF, pReacElement.Name, pReacElement.NodeRef^[1] ], 9999);
               Break;
             End;
          End;
          ObjRef := pClass.Next;
       End;
    End;
end;

function TExecHelper.DoReduceCmd:Integer;
VAR
    MetObj:TEnergyMeterObj;
    MeterClass: TEnergyMeter;
    Param  :String;
    DevClassIndex:Integer;

Begin
    Result := 0;
    // Get next parm and try to interpret as a file name
    DSS.Parser.NextParam;
    Param := AnsiUpperCase(DSS.Parser.StrValue);

    // Mark Capacitor and Reactor buses as Keep so we don't lose them
    MarkCapandReactorBuses;

    IF Length(Param) = 0  Then Param := 'A';
    CASE Param[1] of
     'A': Begin
              metobj := DSS.ActiveCircuit.EnergyMeters.First;
              While metobj <> nil Do
              Begin
                MetObj.ReduceZone;
                MetObj := DSS.ActiveCircuit.EnergyMeters.Next;
              End;
          End;

    ELSE
       // Reduce a specific meter
       DevClassIndex := DSS.ClassNames.Find('energymeter');
       IF DevClassIndex > 0 THEN
       Begin
          MeterClass := DSS.DSSClassList.Get(DevClassIndex);
          If MeterClass.SetActive (Param) Then   // Try to set it active
          Begin
            MetObj := MeterClass.GetActiveObj;
            MetObj.ReduceZone;
          End
          Else DoSimpleMsg(DSS, 'EnergyMeter "%s" not found.', [Param], 262);
       End;
    End;
end;

function TExecHelper.DoResetMonitors:Integer;
VAR
   pMon:TMonitorObj;

begin
     WITH DSS.ActiveCircuit DO
     begin
        pMon := Monitors.First;
        WHILE pMon<>nil DO
        Begin
            pMon.ResetIt;
            pMon := Monitors.Next;
        End;
        Result :=0;

     End;
end;

function TExecHelper.DoFileEditCmd: Integer;
var
    Param: String;
begin
    Result := 0;
    // Get next parm and try to interpret as a file name
    DSS.Parser.NextParam;
    Param := DSS.Parser.StrValue;

    if FileExists(Param) then
        FireOffEditor(DSS, Param)
    else
    begin
       DSS.GlobalResult := Format(_('File "%s" does not exist.'), [param]);
       Result := 1;
    end;
end;

procedure TExecHelper.ParseObjName(const fullname:String; VAR objname, propname:String);
// Parse strings such as
//
//    1. Classname.Objectname,Property    (full name)
//    2. Objectname.Property   (classname omitted)
//    3. Property           (classname and objectname omitted
//
VAR
  DotPos1, DotPos2:Integer;
Begin
     DotPos1 := Pos('.',fullname);
     CASE Dotpos1 of

        0: Begin
               Objname  := '';
               PropName := FullName;
           End;

       ELSE begin
          PropName := Copy(FullName,Dotpos1+1,(Length(FullName)-DotPos1));
          DotPos2  := Pos('.', PropName);
          CASE DotPos2 of

             0: Begin
                    ObjName := Copy(FullName,1,DotPos1-1);
                End;
            ELSE
            Begin
                ObjName  := Copy(FullName,1,Dotpos1+DotPos2-1);
                PropName := Copy(PropName,Dotpos2+1,(Length(PropName)-DotPos2));
            End;

          End;

       End;
     End;
End;

function TExecHelper.DoQueryCmd:Integer;
// ? Command
// Syntax:  ? Line.Line1.R1
VAR
   Param, ObjName, PropName:String;
   PropIndex:Integer;
begin
     Result := 0;
     DSS.Parser.NextParam;
     Param := DSS.Parser.StrValue;

     ParseObjName(Param, ObjName, PropName);

     IF CompareText(ObjName,'solution')=0 THEN
     Begin  // special for solution
         // DSS.ActiveDSSClass  := DSS.SolutionClass;
         //DSS.ActiveDSSObject := DSS.ActiveCircuit.Solution;
         DSS.GlobalPropertyValue := 'Property Unknown';
     End ELSE
     Begin
         // Set Object Active
         DSS.Parser.cmdstring := '"' + Objname + '"';
         DoSelectCmd;
          // Put property value in global VARiable
          PropIndex := DSS.ActiveDSSClass.Propertyindex(PropName);
          IF PropIndex>0 THEN
              DSS.GlobalPropertyValue := DSS.ActiveDSSObject.GetPropertyValue(PropIndex)
          ELSE
              DSS.GlobalPropertyValue := 'Property Unknown';
     End;

     DSS.GlobalResult := DSS.GlobalPropertyValue;

     If DSS.LogQueries Then WriteQueryLogFile(DSS, param, DSS.GlobalResult); // write time-stamped query

End;

function TExecHelper.DoResetMeters:Integer;
Begin
     Result := 0;
     DSS.EnergyMeterClass.ResetAll
End;


function TExecHelper.DoNextCmd:Integer;
VAR
    Param  :String;
Begin
    Result := 0;

    // Get next parm and try to interpret as a file name
    DSS.Parser.NextParam;
    Param := DSS.Parser.StrValue;

    With DSS.ActiveCircuit.Solution Do
    CASE UpCase(Param[1]) of

       'Y'{Year}:  Year := Year + 1;
       'H'{Hour}:  Inc(DynaVars.intHour);
       'T'{Time}:  Increment_time;
    ELSE

    END;
end;

procedure TExecHelper.DoAboutBox;
Begin
    If NoFormsAllowed Then Exit;
    ShowAboutBox;
End;

function TExecHelper.DoSetVoltageBases:integer;
Begin
   Result := 0;
   DSS.ActiveCircuit.Solution.SetVoltageBases;
End;
function TExecHelper.AddObject(const ObjType, Name: String): Integer;
var
    Obj: TDSSObject = NIL;
begin
   Result :=0;

   // Search for class IF not already active
   // IF nothing specified, LastClassReferenced remains
   IF   CompareText(Objtype, DSS.ActiveDSSClass.Name) <> 0
   THEN DSS.LastClassReferenced := DSS.ClassNames.Find(ObjType);

   CASE DSS.LastClassReferenced of
     0: Begin
            DoSimpleMsg(DSS, 'New Command: Object Type "%s" not found. %s', [ObjType, CRLF + DSS.Parser.CmdString], 263);
            Result := 0;
            Exit;
        End;{Error}
   ELSE

     // intrinsic and user Defined models
     // Make a new circuit element
        DSS.ActiveDSSClass := DSS.DSSClassList.Get(DSS.LastClassReferenced);

      // Name must be supplied
        IF   Length(Name) = 0
        THEN Begin
            DoSimpleMsg(DSS, 'Object Name Missing %s', [CRLF + DSS.Parser.CmdString], 264);
            Exit;
        End;


        // now let's make a new object or set an existing one active, whatever the case
        CASE  DSS.ActiveDSSClass.DSSClassType Of
            // These can be added WITHout having an active circuit
            // Duplicates not allowed in general DSS objects;
             DSS_OBJECT :  IF  NOT  DSS.ActiveDSSClass.SetActive(Name)
                           THEN Begin
                               Obj := DSS.ActiveDSSClass.NewObject(Name, True, Result);
                               DSS.DSSObjs.Add(Obj);  // Stick in pointer list to keep track of it
                           End;
        ELSE
            // These are circuit elements
            IF   DSS.ActiveCircuit = nil
            THEN Begin
                 DoSimpleMsg(DSS, _('You Must Create a circuit first: "new circuit.yourcktname"'), 265);
                 Exit;
            End;

          // IF Object already exists.  Treat as an Edit IF dulicates not allowed
            IF    DSS.ActiveCircuit.DuplicatesAllowed THEN
             Begin
                 Obj := DSS.ActiveDSSClass.NewObject(Name, True, Result); // Returns index into this class
                 DSS.ActiveCircuit.AddCktElement(TDSSCktElement(Obj));   // Adds active object to active circuit
             End
            ELSE
             Begin      // Check to see if we can set it active first
                IF   Not DSS.ActiveDSSClass.SetActive(Name)  THEN
                 Begin
                   Obj := DSS.ActiveDSSClass.NewObject(Name, True, Result);   // Returns index into this class
                   DSS.ActiveCircuit.AddCktElement(TDSSCktElement(Obj));   // Adds active object to active circuit
                 End
                ELSE
                 Begin
                    DoSimpleMsg(DSS, 'Warning: Duplicate new element definition: "%s.%s". Element being redefined.', [DSS.ActiveDSSClass.Name, Name], 266);
                 End;
             End;

        End;
        DSS.ActiveDSSClass.Edit(DSS.Parser);    // Process remaining instructions on the command line
  End;
End;


function TExecHelper.EditObject(const ObjType, Name:String):Integer;
begin
    Result :=0;
    DSS.LastClassReferenced := DSS.ClassNames.Find(ObjType);

    if DSS.LastClassReferenced = 0 then
    begin
      DoSimpleMsg(DSS, 'Edit Command: Object Type "%s" not found. %s', [ObjType, CRLF + DSS.Parser.CmdString], 267);
      Result := 0;
      Exit;
    end;

    // intrinsic and user Defined models
    // Edit the DSS object
    DSS.ActiveDSSClass := DSS.DSSClassList.Get(DSS.LastClassReferenced);
    IF DSS.ActiveDSSClass.SetActive(Name) THEN
    Begin
        Result := DSS.ActiveDSSClass.Edit(DSS.Parser);   // Edit the active object
    End;
 end;

function TExecHelper.DoSetkVBase: Integer;
var
   ParamName, BusName:String;
   kVValue :Double;
begin
   // Parse off next two items on line
   ParamName := DSS.Parser.NextParam;
   BusName   := AnsiLowerCase(DSS.Parser.StrValue);

   ParamName := DSS.Parser.NextParam;
   kVValue   := DSS.Parser.DblValue;

   // Now find the bus and set the value

   WITH DSS.ActiveCircuit Do
   Begin
      ActiveBusIndex := BusList.Find(BusName);

      IF   ActiveBusIndex > 0
      THEN Begin
           IF    Comparetext(ParamName, 'kvln') = 0
           THEN  Buses^[ActiveBusIndex].kVBase := kVValue
           ELSE  Buses^[ActiveBusIndex].kVBase := kVValue / SQRT3;
           Result := 0;
           Solution.VoltageBaseChanged := TRUE;
           // Solution.SolutionInitialized := FALSE;  // Force reinitialization
      End
      ELSE Begin
           Result := 1;
           AppendGlobalResult(DSS, 'Bus ' + BusName + ' not found.');
      End;
   End;
End;


PROCEDURE TExecHelper.DoAutoAddBusList(const S: String);
VAR
   ParmName,
   Param, S2    :String;
   F: TStream = nil;
begin
     DSS.ActiveCircuit.AutoAddBusList.Clear;

     // Load up auxiliary parser to reparse the array list or file name
     DSS.AuxParser.CmdString := S;
     ParmName := DSS.AuxParser.NextParam ;
     Param := DSS.AuxParser.StrValue;

     // Syntax can be either a list of bus names or a file specification:  File= ...

     If CompareText(Parmname, 'file') = 0
     THEN Begin
         // load the list from a file
         TRY
             F := DSS.GetROFileStream(Param);
             WHILE (F.Position+1) < F.Size Do
             Begin
                  FSReadln(F, S2);
                  DSS.AuxParser.CmdString := S2;
                  ParmName := DSS.AuxParser.NextParam ;
                  Param := DSS.AuxParser.StrValue;
                  IF Length(Param) > 0 THEN 
                      DSS.ActiveCircuit.AutoAddBusList.Add(Param);
             End;
             FreeAndNil(F);

         EXCEPT
             On E:Exception Do 
                DoSimpleMsg(DSS, 'Error trying to read bus list file: %s', [E.message], 268);
         END;
     End
     ELSE 
     begin
       // Parse bus names off of array list
       WHILE Length(Param) > 0 Do
       BEGIN
            DSS.ActiveCircuit.AutoAddBusList.Add(Param);
            DSS.AuxParser.NextParam;
            Param := DSS.AuxParser.StrValue;
       END;

     End;
end;

procedure TExecHelper.DoKeeperBusList(Const S:String);
// Set Keep flag on buses found in list so they aren't eliminated by some reduction
// algorithm.  This command is cumulative. To clear flag, use Reset Keeplist
VAR
   ParmName,
   Param, S2    :String;
   F: TStream = nil;
   iBus :Integer;
begin
     // Load up auxiliary parser to reparse the array list or file name
     DSS.AuxParser.CmdString := S;
     ParmName := DSS.AuxParser.NextParam ;
     Param := DSS.AuxParser.StrValue;

     // Syntax can be either a list of bus names or a file specification:  File= ...
     If CompareText(Parmname, 'file') = 0  THEN
      Begin
         // load the list from a file
         TRY
             F := DSS.GetROFileStream(Param);
             WHILE (F.Position + 1) < F.Size Do
             Begin         // Fixed 7/8/01 to handle all sorts of bus names
                  FSReadln(F, S2);
                  DSS.AuxParser.CmdString := S2;
                  ParmName := DSS.AuxParser.NextParam ;
                  Param := DSS.AuxParser.StrValue;
                  IF   Length(Param) > 0
                  THEN With DSS.ActiveCircuit Do
                    Begin
                      iBus := BusList.Find(Param);
                      If iBus>0 Then Buses^[iBus].Keep := TRUE;
                    End;
             End;
             FreeAndNil(F);

         EXCEPT
             On E:Exception Do
                 DoSimpleMsg(DSS, 'Error trying to read bus list file "%s": %s', [param, E.message], 269);
         END;


     End
     ELSE begin
       // Parse bus names off of array list
       WHILE Length(Param) > 0 Do
       BEGIN
            With DSS.ActiveCircuit Do
            Begin
              iBus := BusList.Find(Param);
              If iBus>0 Then Buses^[iBus].Keep := TRUE;
            End;

            DSS.AuxParser.NextParam;
            Param := DSS.AuxParser.StrValue;
       END;

     End;
end;

function TExecHelper.DocktlossesCmd: Integer;
Var
   LossValue :complex;
begin
     Result := 0;
     IF DSS.ActiveCircuit <> Nil THEN
      Begin
         DSS.GlobalResult := '';
         LossValue := DSS.ActiveCircuit.Losses;
         DSS.GlobalResult := Format('%10.5g, %10.5g',[LossValue.re * 0.001,  LossValue.im*0.001]);
      End
    ELSE  DSS.GlobalResult := 'No Active Circuit.';
end;

function TExecHelper.DocurrentsCmd: Integer;
VAR
  cBuffer: pComplexArray;
  NValues, i: Integer;

Begin
    Result := 0;

  If DSS.ActiveCircuit <> Nil Then
     WITH DSS.ActiveCircuit.ActiveCktElement DO
     Begin
         NValues := NConds*Nterms;
         DSS.GlobalResult := '';
         cBuffer := Allocmem(sizeof(Complex)*NValues);
         GetCurrents(cBuffer);
         For i := 1 to  NValues DO
         Begin
            DSS.GlobalResult := DSS.GlobalResult + Format('%10.5g, %6.1f,',[cabs(cBuffer^[i]), Cdang(cBuffer^[i])]);
         End;
         Reallocmem(cBuffer,0);
     End
  Else
     DSS.GlobalResult := 'No Active Circuit.';
end;

function TExecHelper.DoNodeListCmd: Integer;
VAR
  NValues, i: Integer;
  CktElementName: String;
begin
  Result := 0;

  If DSS.ActiveCircuit <> Nil Then
  Begin
    DSS.Parser.NextParam;
    CktElementName := DSS.Parser.StrValue ;

    If Length(CktElementName) > 0  Then  SetObject(DSS, CktElementName);

    If Assigned(DSS.ActiveCircuit.ActiveCktElement) Then
     WITH DSS.ActiveCircuit.ActiveCktElement DO
     Begin
         NValues := NConds*Nterms;
         DSS.GlobalResult := '';
         For i := 1 to  NValues DO
         Begin
            DSS.GlobalResult := DSS.GlobalResult + Format('%d, ',[GetNodeNum(DSS, NodeRef^[i]) ]);
         End;
     End
  Else
     DSS.GlobalResult := 'No Active Circuit.';
  End;
end;


function TExecHelper.DolossesCmd: Integer;
Var
   LossValue :complex;
begin
    Result := 0;
     IF DSS.ActiveCircuit <> Nil THEN
      WITH DSS.ActiveCircuit DO
      Begin
        If ActiveCktElement<>Nil THEN
        Begin
         DSS.GlobalResult := '';
         LossValue := ActiveCktElement.Losses;
         DSS.GlobalResult := Format('%10.5g, %10.5g', [LossValue.re * 0.001, LossValue.im * 0.001]);
        End;
      End
    ELSE DSS.GlobalResult := 'No Active Circuit.';
end;

function TExecHelper.DophaselossesCmd: Integer;

// Returns Phase losses in kW, kVar

VAR
  cBuffer:pComplexArray;
  NValues, i : Integer;

begin
 Result := 0;

 IF DSS.ActiveCircuit <> Nil THEN

  WITH DSS.ActiveCircuit.ActiveCktElement DO
  Begin
      NValues := NPhases;
      cBuffer := Allocmem(sizeof(Complex)*NValues);
      DSS.GlobalResult := '';
      GetPhaseLosses( NValues, cBuffer);
      For i := 1 to  NValues DO Begin
         DSS.GlobalResult := DSS.GlobalResult + Format('%10.5g, %10.5g,',[ cBuffer^[i].re*0.001, cBuffer^[i].im*0.001]);
      End;
      Reallocmem(cBuffer,0);
  End
 ELSE DSS.GlobalResult := 'No Active Circuit.'



end;

function TExecHelper.DopowersCmd(Total: Integer): Integer;
VAR
  cBuffer     : pComplexArray;
  NValues,
  myInit,
  myEnd,
  j,
  i           : Integer;
  myBuffer    : Array of Complex;

Begin
  // If Total = 0, returns the powers per phase
  // If Total = 1, returns the power sum at each terminal

 Result := 0;
  IF DSS.ActiveCircuit <> Nil THEN
    WITH DSS.ActiveCircuit.ActiveCktElement DO
    Begin
      NValues := NConds*Nterms;
      DSS.GlobalResult := '';
      cBuffer := Allocmem(sizeof(Complex)*NValues);
      GetPhasePower(cBuffer);
      if Total = 0 then
      Begin
        For i := 1 to  NValues DO Begin
           DSS.GlobalResult := DSS.GlobalResult+ Format('%10.5g, %10.5g,', [cBuffer^[i].re*0.001, cBuffer^[i].im*0.001]);
        End;
      End
      else
      Begin
        setlength(myBuffer,Nterms);
        for j := 1 to Nterms do
        Begin
          myBuffer[j - 1] :=  cmplx(0.0, 0.0);
          myInit          :=  (j - 1) * NConds + 1;
          myEnd           :=  NConds * j;
          For i := myInit to myEnd DO
          Begin
            myBuffer[j - 1] := myBuffer[j - 1] + cBuffer^[i];
          End;
          DSS.GlobalResult := DSS.GlobalResult+ Format('%10.5g, %10.5g,', [myBuffer[j - 1].re*0.001, myBuffer[j - 1].im*0.001]);
        End;
      End;
      Reallocmem(cBuffer,0);
  End
 ELSE DSS.GlobalResult := 'No Active Circuit';
end;

function TExecHelper.DoseqcurrentsCmd: Integer;
// All sequence currents of active ciruit element
// returns magnitude only.

VAR
  Nvalues,i,j,k:Integer;
  IPh, I012 : Array[1..3] of Complex;
  cBuffer:pComplexArray;

begin
   Result := 0;
   IF DSS.ActiveCircuit <> Nil THEN
     WITH DSS.ActiveCircuit DO
     Begin
       If ActiveCktElement<>Nil THEN
       WITH ActiveCktElement DO
       Begin
        DSS.GlobalResult := '';
        IF Nphases<3
        THEN  For i := 0 to  3*Nterms-1 DO DSS.GlobalResult := DSS.GlobalResult + ' -1.0,'  // Signify n/A
        ELSE Begin
          NValues := NConds * Nterms;
          cBuffer := Allocmem(sizeof(Complex)*NValues);
          GetCurrents(cBuffer);
          For j := 1 to Nterms Do
          Begin
            k := (j-1)*NConds;
            For i := 1 to  3 DO
            Begin
              Iph[i] := cBuffer^[k+i];
            End;
            Phase2SymComp(pComplexArray(@Iph), pComplexArray(@I012));
            For i := 1 to 3 DO
            Begin
              DSS.GlobalResult := DSS.GlobalResult + Format('%10.5g, ',[Cabs(I012[i])]);
            End;
          End;
          Reallocmem(cBuffer,0);
        End; // ELSE
       End; // WITH ActiveCktElement
     End // IF/WITH DSS.ActiveCircuit
   ELSE DSS.GlobalResult := 'No Active Circuit';
end;

function TExecHelper.DoSeqpowersCmd: Integer;
// All seq Powers of active 3-phase ciruit element
// returns kW + j kvar

VAR
  Nvalues,i,j,k :Integer;
  S:Complex;
  VPh, V012 : Array[1..3] of Complex;
  IPh, I012 : Array[1..3] of Complex;
  cBuffer:pComplexArray;

begin
 Result := 0;
 IF DSS.ActiveCircuit <> Nil THEN
   WITH DSS.ActiveCircuit DO Begin
     If ActiveCktElement<>Nil THEN
     WITH ActiveCktElement DO Begin
      DSS.GlobalResult := '';
      IF NPhases < 3 THEN
         For i := 0 to 2*3*Nterms-1 DO DSS.GlobalResult := DSS.GlobalResult + '-1.0, '  // Signify n/A
      ELSE Begin
        NValues := NConds * Nterms;
        cBuffer := Allocmem(sizeof(Complex)*NValues);
        GetCurrents(cBuffer);
        FOR j := 1 to Nterms Do Begin
         k :=(j-1) * NConds;
         FOR i := 1 to  3 DO Begin
            Vph[i] := Solution.NodeV^[Terminals[j - 1].TermNodeRef[i - 1]];
         End;
         For i := 1 to  3 DO Begin
           Iph[i] := cBuffer^[k+i];
         End;
         Phase2SymComp(pComplexArray(@Iph), pComplexArray(@I012));
         Phase2SymComp(pComplexArray(@Vph), pComplexArray(@V012));
         For i := 1 to 3 DO  Begin
           S := V012[i] * cong(I012[i]);
           DSS.GlobalResult := DSS.GlobalResult+ Format('%10.5g, %10.5g,',[S.re*0.003, S.im*0.003]); // 3-phase kW conversion
         End;
        End;
      End;
      Reallocmem(cBuffer,0);
     End;
   End
 ELSE DSS.GlobalResult := 'No Active Circuit';
end;

function TExecHelper.DoseqvoltagesCmd: Integer;

// All voltages of active ciruit element
// magnitude only
// returns a set of seq voltages (3) for each terminal

VAR
  Nvalues,i,j,k,n:Integer;
  VPh, V012 : Array[1..3] of Complex;
  S:String;

Begin
  Result := 0;
  Nvalues := -1; // unassigned, for exception message
  n := -1; // unassigned, for exception message
  IF   DSS.ActiveCircuit <> Nil THEN
   WITH DSS.ActiveCircuit DO
   Begin
     If ActiveCktElement<>Nil THEN
     WITH ActiveCktElement DO
     If Enabled Then
     Begin
     TRY
      Nvalues := NPhases;
      DSS.GlobalResult :='';
      IF Nvalues < 3 THEN
         For i := 1 to 3*Nterms DO DSS.GlobalResult := DSS.GlobalResult + '-1.0, '  // Signify n/A
      ELSE
      begin
       FOR j := 1 to Nterms Do
       begin
          k :=(j-1)*NConds;
          FOR i := 1 to 3 DO
          Begin
             Vph[i] := Solution.NodeV^[NodeRef^[i+k]];
          End;
          Phase2SymComp(pComplexArray(@Vph), pComplexArray(@V012));   // Compute Symmetrical components

          For i := 1 to 3 DO  // Stuff it in the result
          Begin
             DSS.GlobalResult := DSS.GlobalResult + Format('%10.5g, ',[Cabs(V012[i])]);
          End;

       End;
      End;

      EXCEPT
         On E:Exception Do
         Begin
            S:= E.message + CRLF +
                'Element='+ ActiveCktElement.Name + CRLF+
                'Nvalues=' + IntToStr(NValues) + CRLF +
                'Nterms=' + IntToStr(Nterms) + CRLF +
                'NConds =' + IntToStr(NConds) + CRLF +
                'noderef=' + IntToStr(N) ;
            DoSimpleMsg(S, 270);
          End;
      END;
     End
     Else
         DSS.GlobalResult := _('Element Disabled');  // Disabled

   End
  ELSE DSS.GlobalResult := _('No Active Circuit');



End;

function TExecHelper.DovoltagesCmd(Const PerUnit:Boolean): Integer;
// Bus Voltages at active terminal

VAR
  i:Integer;
  Volts:Complex;
  ActiveBus:TDSSBus;
  VMag:Double;

begin
    Result := 0;
    IF DSS.ActiveCircuit <> Nil THEN
      WITH DSS.ActiveCircuit DO
      Begin
        If ActiveBusIndex<>0 THEN
        Begin
         ActiveBus := Buses^[ActiveBusIndex];
         DSS.GlobalResult := '';
         FOR i := 1 to  ActiveBus.NumNodesThisBus DO
         Begin
            Volts := Solution.NodeV^[ActiveBus.GetRef(i)];
            Vmag := Cabs(Volts);
            If PerUnit and (ActiveBus.kvbase>0.0) Then Begin
                  Vmag := Vmag *0.001/ActiveBus.kVBase;
                  DSS.GlobalResult := DSS.GlobalResult + Format('%10.5g, %6.1f, ', [Vmag, CDang(Volts)]);
            End 
            Else  DSS.GlobalResult := DSS.GlobalResult + Format('%10.5g, %6.1f, ', [Vmag, CDang(Volts)]);
         End;
        End
        Else DSS.GlobalResult := 'No Active Bus.';
      End
    ELSE DSS.GlobalResult := 'No Active Circuit.';
end;

function TExecHelper.DoZscCmd(Zmatrix:Boolean): Integer;
// Bus Short Circuit matrix

VAR
  i,j:Integer;
  ActiveBus:TDSSBus;
  Z:Complex;

begin
    Result := 0;
    IF DSS.ActiveCircuit <> Nil THEN
      WITH DSS.ActiveCircuit DO
      Begin
        If ActiveBusIndex<>0 THEN
        Begin
         ActiveBus := Buses^[ActiveBusIndex];
         DSS.GlobalResult := '';
         If not assigned(ActiveBus.Zsc) Then Exit;
         With ActiveBus Do
         FOR i := 1 to  NumNodesThisBus DO Begin
            For j := 1 to  NumNodesThisBus Do  begin
             If ZMatrix Then Z := Zsc.GetElement(i,j)
             Else Z := Ysc.GetElement(i,j);
             DSS.GlobalResult := DSS.GlobalResult + Format('%-.5g, %-.5g,   ', [Z.re, Z.im]);

            End;

         End;
        End
        Else DSS.GlobalResult := 'No Active Bus.';
      End
    ELSE DSS.GlobalResult := 'No Active Circuit.';
end;

function TExecHelper.DoZsc012Cmd: Integer;
// Bus Short Circuit matrix
VAR
    // i: Integer;
    ActiveBus: TDSSBus;
    Z0, Z1, Z2: Complex;
    // Temp1, Temp2: pComplexArray;
    Zsc012Temp: TcMatrix;
Begin
    Result := 0;
    if DSS.ActiveCircuit = NIL then
    begin
        DSS.GlobalResult := 'No Active Circuit.';
        Exit;
    end;
    if (DSS.ActiveCircuit.ActiveBusIndex <= 0) or (DSS.ActiveCircuit.ActiveBusIndex > DSS.ActiveCircuit.NumBuses) then
    begin
        DSS.GlobalResult := 'No Active Bus.';
        Exit;
    end;
    
    ActiveBus := DSS.ActiveCircuit.Buses^[DSS.ActiveCircuit.ActiveBusIndex];
    DSS.GlobalResult := '';
    If not assigned(ActiveBus.Zsc) Then Exit;

    if ActiveBus.NumNodesThisBus <> 3 then
    begin
        DSS.GlobalResult :=  'Not a 3-phase bus. Cannot compute Symmetrical Component matrix.';
        Exit;
    end;

    With ActiveBus Do
    Begin
        // Compute ZSC012 for 3-phase buses else leave it zeros
        // ZSC012 = Ap2s Zsc As2p
        Zsc012Temp:= Zsc.MtrxMult(As2p);  // temp for intermediate result
        if Assigned(ZSC012) then ZSC012.Free;
        ZSC012 := Ap2s.MtrxMult(Zsc012Temp);
        // Cleanup
        Zsc012Temp.Free;
        // Just return diagonal elements only
        Z0 := Zsc012.GetElement(1,1);
        Z1 := Zsc012.GetElement(2,2);
        Z2 := Zsc012.GetElement(3,3);
        DSS.GlobalResult := DSS.GlobalResult + Format('Z0, (%-.5g, +j %-.5g), ', [Z0.re, Z0.im]) + CRLF;
        DSS.GlobalResult := DSS.GlobalResult + Format('Z1, (%-.5g, +j %-.5g), ', [Z1.re, Z1.im]) + CRLF;
        DSS.GlobalResult := DSS.GlobalResult + Format('Z2, (%-.5g, +j %-.5g), ', [Z2.re, Z2.im]);
    end;
end;

function TExecHelper.DoZsc10Cmd: Integer;
// Bus Short Circuit matrix
VAR
  ActiveBus:TDSSBus;
  Z:Complex;
begin
    Result := 0;
    IF DSS.ActiveCircuit <> Nil THEN
      WITH DSS.ActiveCircuit DO
      Begin
        If ActiveBusIndex<>0 THEN
        Begin
         ActiveBus := Buses^[ActiveBusIndex];
         DSS.GlobalResult := '';
         If not assigned(ActiveBus.Zsc) Then Exit;
         With ActiveBus Do begin
             Z := Zsc1;
             DSS.GlobalResult := DSS.GlobalResult + Format('Z1, %-.5g, %-.5g, ', [Z.re, Z.im]) + CRLF;
             
             Z := Zsc0;
             DSS.GlobalResult := DSS.GlobalResult + Format('Z0, %-.5g, %-.5g, ', [Z.re, Z.im]);
         End;

        End
        Else DSS.GlobalResult := 'No Active Bus.';
      End
    ELSE DSS.GlobalResult := 'No Active Circuit.';
end;


function TExecHelper.DoAllocateLoadsCmd: Integer;
// Requires an EnergyMeter Object at the head of the feeder
// Adjusts loads defined by connected kVA or kWh billing
VAR
   pMeter :TEnergyMeterObj;
   pSensor:TSensorObj;
   iterCount :Integer;

begin
    Result := 0;
    WITH DSS.ActiveCircuit Do
    Begin
         LoadMultiplier := 1.0;   // Property .. has side effects
         With Solution Do
         Begin
             If Mode <> TSolveMode.SNAPSHOT Then Mode := TSolveMode.SNAPSHOT;   // Resets meters, etc. if not in snapshot mode
             Solve;  // Make guess based on present allocationfactors
         End;

         // Allocation loop -- make MaxAllocationIterations iterations
         FOR iterCount := 1 to DSS.MaxAllocationIterations Do begin
           // Do EnergyMeters
           pMeter := EnergyMeters.First;
           WHILE pMeter <> NIL Do Begin
              pMeter.CalcAllocationFactors;
              pMeter := EnergyMeters.Next;
           End;

           // Now do other Sensors
           pSensor := Sensors.First;
           WHILE pSensor <> NIL Do Begin
              pSensor.CalcAllocationFactors;
              pSensor := Sensors.Next;
           End;

            // Now let the EnergyMeters run down the circuit setting the loads
            pMeter := EnergyMeters.First;
            WHILE pMeter <> NIL Do Begin
                pMeter.AllocateLoad;
                pMeter := EnergyMeters.Next;
            End;
            Solution.Solve;  {Update the solution}

         End;
    End;
end;

procedure TExecHelper.DoSetAllocationFactors(const X: Double);

VAR
   pLoad :TLoadObj;

begin
    IF   X <= 0.0
    THEN DoSimpleMsg(DSS, _('Allocation Factor must be greater than zero.'), 271)
    ELSE WITH DSS.ActiveCircuit Do
    Begin
         pLoad := Loads.First;
         WHILE pLoad <> NIL Do
         Begin
             pLoad.kVAAllocationFactor := X;
             pLoad := Loads.Next;
         End;
    End;
end;

procedure TExecHelper.DoSetCFactors(const X: Double);

VAR
   pLoad :TLoadObj;

begin
    IF   X <= 0.0
    THEN DoSimpleMsg(DSS, _('CFactor must be greater than zero.'), 271)
    ELSE WITH DSS.ActiveCircuit Do
    Begin
         pLoad := Loads.First;
         WHILE pLoad <> NIL Do
         Begin
             pLoad.CFactor := X;
             pLoad := Loads.Next;
         End;
    End;
end;

function TExecHelper.DoHarmonicsList(const S:String):Integer;

VAR
   Dummy :pDoubleArray;
   i,
   Num   :Integer;

Begin
   Result := 0;

   WITH DSS.ActiveCircuit.Solution Do
   IF CompareText(S, 'ALL') = 0 THEN DoAllHarmonics := TRUE
   ELSE Begin
       DoAllHarmonics := FALSE;

       Dummy := AllocMem(Sizeof(Double) * 100); // Big Buffer
       Num   := DSS.Parser.ParseAsVector(100, Dummy);
       {Parsing zero-fills the array}

       HarmonicListSize := Num;
       Reallocmem(HarmonicList, SizeOf(HarmonicList^[1]) * HarmonicListSize);
       FOR i := 1 to HarmonicListSize Do HarmonicList^[i] := Dummy^[i];

       Reallocmem(Dummy, 0);
   End;
End;


function TExecHelper.DoFormEditCmd:Integer;

begin
    Result := 0;
    If NoFormsAllowed Then Exit;
    DoSelectCmd;  // Select ActiveObject
    IF DSS.ActiveDSSObject <> NIL THEN  begin
         ShowPropEditForm;

    End
    ELSE   Begin
       DoSimpleMsg(DSS, _('Element not found.'), 272);
       Result := 1;
    End;
End;


function TExecHelper.DoMeterTotals:Integer;
Var
   i: Integer;
Begin
    Result := 0;
    If DSS.ActiveCircuit <> Nil Then
      Begin
       DSS.ActiveCircuit.TotalizeMeters;
        // Now export to global result
        For i := 1 to NumEMregisters Do
          Begin
            AppendGlobalResult(DSS, Format('%-.6g',[DSS.ActiveCircuit.RegisterTotals[i]]));
          End;
      End;
End;

function TExecHelper.DoCapacityCmd:Integer;

Var
   ParamPointer     :integer;
   Param, ParamName :String;

Begin
  Result := 0;

     ParamPointer := 0;
     ParamName := DSS.Parser.NextParam;
     Param := DSS.Parser.StrValue;
     WHILE Length(Param)>0 DO BEGIN
         IF Length(ParamName) = 0 THEN Inc(ParamPointer)
         ELSE Case ParamName[1] of
                 's':ParamPointer := 1;
                 'i':ParamPointer := 2;
              ELSE
                  ParamPointer := 0;
              END;

         CASE ParamPointer OF
            0: DoSimpleMsg(DSS, 'Unknown parameter "%s" for Capacity Command', [ParamName], 273);
            1: DSS.ActiveCircuit.CapacityStart := DSS.Parser.DblValue;
            2: DSS.ActiveCircuit.CapacityIncrement := DSS.Parser.DblValue;

         ELSE

         END;

         ParamName := DSS.Parser.NextParam;
         Param := DSS.Parser.StrValue;
     END;

    WITH DSS.ActiveCircuit Do
    IF ComputeCapacity Then Begin   // Totalizes EnergyMeters at End

       DSS.GlobalResult := Format('%-.6g', [(DSS.ActiveCircuit.RegisterTotals[3] + DSS.ActiveCircuit.RegisterTotals[19]) ] );  // Peak KW in Meters
       AppendGlobalResult(DSS, Format('%-.6g', [LoadMultiplier]));
    End;
End;

function TExecHelper.DoClassesCmd:Integer;

VAR  i:Integer;
Begin
     For i := 1 to DSS.NumIntrinsicClasses Do Begin
       AppendGlobalResult(DSS, TDSSClass(DSS.DSSClassList.Get(i)).Name);
     End;
     Result := 0;
End;

function TExecHelper.DoUserClassesCmd:Integer;
Begin
    Result := 0;
    AppendGlobalResult(DSS, 'No User Classes Defined.');
End;

function TExecHelper.DoZscRefresh:Integer;

Var j:Integer;

Begin
   Result := 1;

   Try

     WITH DSS.ActiveCircuit, DSS.ActiveCircuit.Solution Do
     Begin
       FOR j := 1 to NumNodes Do Currents^[j] := cZERO;  // Clear Currents array

       IF (ActiveBusIndex > 0) and (ActiveBusIndex <= Numbuses) Then Begin
          If not assigned(Buses^[ActiveBusIndex].Zsc) Then Buses^[ActiveBusIndex].AllocateBusQuantities ;
          Solution.ComputeYsc(ActiveBusIndex);      // Compute YSC for active Bus
          Result := 0;
       End;
     End;

   Except
       On E:Exception Do DoSimpleMsg(DSS, 'ZscRefresh Error: %s', [E.message], 274);
   End;
end;

function TExecHelper.DoVarValuesCmd:Integer;
Var
   i: Integer;
  // PcElem:TPCElement;
begin
    Result := 0;
    If DSS.ActiveCircuit <> Nil Then
    With DSS.ActiveCircuit Do
      Begin
         // Check if PCElement
         CASE (ActiveCktElement.DSSObjType and BASECLASSMASK) OF
           PC_ELEMENT: With ActiveCktElement as TPCElement Do
                       Begin
                         For i := 1 to NumVariables Do
                         AppendGlobalResult(DSS, Format('%-.6g',[Variable[i]]));
                       End;
         Else
             AppendGlobalResult(DSS, 'Null');
         End;
      End;
end;

function TExecHelper.DoValVarCmd:Integer;
// Get value of specified variable by name of index
Var
    ParamName, Param :String;
    VarIndex :Integer;
    PropIndex :Integer;
    PCElem :TPCElement;

begin
    Result := 0;

    // Check to make sure this is a PC Element. If not, return null string in global result

    If (DSS.ActiveCircuit.ActiveCktElement.DSSObjType And BASECLASSMASK) <> PC_ELEMENT Then

       DSS.GlobalResult := ''

    Else begin
        PCElem :=  DSS.ActiveCircuit.ActiveCktElement As TPCElement;

        // Get next parameter on command line

        ParamName := AnsiUpperCase(DSS.Parser.NextParam);
        Param := DSS.Parser.StrValue;

        PropIndex := 1;
        If Length(ParamName) > 0 Then
          CASE ParamName[1] of
              'N': PropIndex := 1;
              'I': PropIndex := 2;
          END;

        VarIndex := 0;

        CASE PropIndex of
            1: VarIndex := PCElem.LookupVariable(Param);  // Look up property index
            2: VarIndex := DSS.Parser.IntValue ;
        END;

        If (VarIndex>0) and (VarIndex<=PCElem.NumVariables) Then

           DSS.GlobalResult := Format('%.8g',[PCElem.Variable[VarIndex] ])

        Else DSS.GlobalResult := '';   {Invalid var name or index}

    End;
end;

function TExecHelper.DoVarNamesCmd :Integer;

Var
   i: Integer;
begin
    Result := 0;
    If DSS.ActiveCircuit <> Nil Then
    With DSS.ActiveCircuit Do
      Begin
         {Check if PCElement}
         CASE (ActiveCktElement.DSSObjType and BASECLASSMASK) OF
           PC_ELEMENT: With (ActiveCktElement as TPCElement) Do
                       Begin
                         For i := 1 to NumVariables Do
                         AppendGlobalResult(DSS, VariableName(i));
                       End;
         Else
             AppendGlobalResult(DSS, 'Null');
         End;
      End;
end;

function TExecHelper.DoBusCoordsCmd(SwapXY:Boolean):Integer;
// Format of File should be
//
//   Busname, x, y
//
//   (x, y are real values)
//
//   If SwapXY is true, x and y values are swapped
var
   strings: TStringList = nil;
   FStream: TStream = NIL;
   Param,
   BusName : String;
   iB      : Integer;
   iLine   : Integer;
   stringIdx: Integer;
begin
    Result := 0;

    // Get next parameter on command line

    DSS.Parser.NextParam;
    Param := DSS.Parser.StrValue;

    Try
      iLine := -1;
      Try
            strings := TStringList.Create;
            iLine := 0;

            Fstream := DSS.GetROFileStream(Param);
            strings.LoadFromStream(Fstream);
            for stringIdx := 0 to (strings.Count - 1) do
            Begin
                Inc(iLine);
                With DSS.AuxParser Do Begin      // User Auxparser to parse line
                   CmdString := strings[stringIdx];
                   NextParam;  BusName := StrValue;
                   iB := DSS.ActiveCircuit.Buslist.Find(BusName);
                   If iB >0 Then  Begin
                       With DSS.ActiveCircuit.Buses^[iB] Do Begin     // Returns TBus object
                         NextParam;  If SwapXY Then y := DblValue else x := DblValue;
                         NextParam;  If SwapXY Then x := DblValue else y := DblValue;
                         CoordDefined := TRUE;
                       End;
                   End;
              End;
              // Else just ignore a bus that's not in the circuit
          End;


      Except
          // **CHANGE THIS ERROR MESSAGE**
          ON E:Exception Do 
          Begin
              If iLine = -1 Then 
                  DoSimpleMsg(DSS, 'Bus Coordinate file "%s" could not be read: %s', [Param, E.Message], 275)
              Else 
                  DoSimpleMsg(DSS, 'Bus Coordinate file: Error Reading Line %d; %s', [Iline, E.Message], 275);          
          End;
      End;

    Finally
        FreeAndNil(strings);
        FreeAndNil(FStream);
    End;
end;

function TExecHelper.DoMakePosSeq:Integer;
var
   CktElem:TDSSCktElement;
begin
    Result := 0;

    DSS.ActiveCircuit.PositiveSequence := TRUE;

    CktElem := DSS.ActiveCircuit.CktElements.First;
    While CktElem<>Nil Do
    Begin
       CktElem.MakePosSequence();
       CktElem := DSS.ActiveCircuit.CktElements.Next;
    End;
end;

procedure TExecHelper.DoSetReduceStrategy(Const S:String);

   Function AtLeast(i,j:Integer):Integer;
   Begin If j<i Then Result := i Else Result := j; End;

Begin
     DSS.ActiveCircuit.ReductionStrategyString := S;

     DSS.ActiveCircuit.ReductionStrategy := rsDefault;
     IF Length(S)=0 Then Exit;  {No option given}

     DSS.AuxParser.CmdString := DSS.Parser.Remainder;  // so we don't mess up Set Command

     Case AnsiUpperCase(S)[1] of

       'B': DSS.ActiveCircuit.ReductionStrategy := rsBreakLoop;
       'D': DSS.ActiveCircuit.ReductionStrategy := rsDefault;  {Default}
       'E': DSS.ActiveCircuit.ReductionStrategy := rsDangling;  {Ends}
       'L': Begin {Laterals}
               DSS.ActiveCircuit.ReductionStrategy := rsLaterals;
            End;
       'M': DSS.ActiveCircuit.ReductionStrategy := rsMergeParallel;
       (*
       'T': Begin          removed 2-28-2018
              DSS.ActiveCircuit.ReductionStrategy := rsTapEnds;
              DSS.ActiveCircuit.ReductionMaxAngle := 15.0;  {default}
              If Length(param2) > 0 Then  DSS.ActiveCircuit.ReductionMaxAngle := DSS.AuxParser.DblValue;
            End;
            *)
       'S': Begin  {Shortlines or Switch}
              IF CompareTextShortest(S, 'SWITCH')=0 Then Begin
                  DSS.ActiveCircuit.ReductionStrategy := rsSwitches;
              End ELSE Begin
                  DSS.ActiveCircuit.ReductionStrategy := rsShortlines;
                  { DSS.ActiveCircuit.ReductionZmag is now set in main ExecOptions     }
              End;
            End;
     ELSE
         DoSimpleMsg(DSS, 'Unknown Reduction Strategy: "%s".', [S], 276);
     End;
end;

function TExecHelper.DoInterpolateCmd:Integer;

{Interpolate bus coordinates in meter zones}

VAR
    MetObj:TEnergyMeterObj;
    MeterClass: TEnergyMeter;
    Param  :String;
    DevClassIndex:Integer;
    CktElem:TDSSCktElement;

Begin
    Result := 0;

    DSS.Parser.NextParam;
    Param := AnsiUpperCase(DSS.Parser.StrValue);

    // initialize the Checked Flag FOR all circuit Elements
    With DSS.ActiveCircuit Do
    Begin
     CktElem := CktElements.First;
     WHILE  (CktElem <> NIL) Do
     Begin
         Exclude(CktElem.Flags, Flg.Checked);
         CktElem := CktElements.Next;
     End;
    End;


    IF Length(Param) = 0  Then Param := 'A';
    CASE Param[1] of
     'A': Begin
              metobj := DSS.ActiveCircuit.EnergyMeters.First;
              While metobj <> nil Do
              Begin
                MetObj.InterpolateCoordinates;
                MetObj := DSS.ActiveCircuit.EnergyMeters.Next;
              End;
          End;

    ELSE
       {Interpolate a specific meter}
       DevClassIndex := DSS.ClassNames.Find('energymeter');
       IF DevClassIndex > 0 THEN
       Begin
          MeterClass := DSS.DSSClassList.Get(DevClassIndex);
          If MeterClass.SetActive (Param) Then   // Try to set it active
          Begin
            MetObj := MeterClass.GetActiveObj;
            MetObj.InterpolateCoordinates;
          End
          Else DoSimpleMsg(DSS, 'EnergyMeter "%s" not found.', [Param], 277);
       End;
    End;
end;

function TExecHelper.DoAlignFileCmd:Integer;
{Rewrites designated file, aligning the fields into columns}
Var
    Param  :String;

Begin
  Result := 0;
  DSS.Parser.NextParam;
  Param := DSS.Parser.StrValue;


  If FileExists(Param) Then
    Begin
     If Not RewriteAlignedFile(DSS, Param) Then Result := 1;
    End
  Else
    Begin
     DoSimpleMsg(DSS, 'File "%s" does not exist.', [Param], 278);
     Result := 1;
    End;

  If Result=0 Then FireOffEditor(DSS, DSS.GlobalResult);
end; {DoAlignfileCmd}

procedure TExecHelper.DoSetNormal(pctNormal:Double);

Var i:Integer;
    pLine:TLineObj;

Begin
    If DSS.ActiveCircuit <> Nil Then Begin
       pctNormal := pctNormal * 0.01;  // local copy only
       For i := 1 to DSS.ActiveCircuit.Lines.Count Do  Begin
         pLine := DSS.ActiveCircuit.Lines.Get(i);
         pLine.Normamps := pctNormal * pLine.EmergAmps;
       End;
    End;
End;

function TExecHelper.DoRotateCmd:Integer;
// rotate about the center of the coordinates
Var
        i:Integer;
        Angle, xmin,xmax, ymin, ymax, xc, yc:Double;
         a, vector: Complex;
Begin
    Result := 0;
    If DSS.ActiveCircuit <> NIl then begin
        DSS.Parser.NextParam;
        Angle := DSS.Parser.DblValue * PI/180.0;   // Deg to rad

        a := cmplx(cos(Angle), Sin(Angle));
        With DSS.ActiveCircuit Do Begin
            Xmin := 1.0e50;
            Xmax := -1.0e50;
            Ymin := 1.0e50;
            Ymax := -1.0e50;
            For i := 1 to Numbuses Do Begin
                If Buses^[i].CoordDefined Then Begin
                    With  Buses^[i] Do Begin
                      Xmax := Max(Xmax, x);
                      XMin := Min(Xmin, x);
                      ymax := Max(ymax, y);
                      yMin := Min(ymin, y);
                    End;
                End;
            End;

            Xc := (Xmax + Xmin) / 2.0;
            Yc := (Ymax + Ymin) / 2.0;

            For i := 1 to Numbuses Do Begin
                If Buses^[i].CoordDefined Then Begin
                    With  Buses^[i] Do Begin
                         vector := cmplx(x-xc,y-yc);
                         Vector := Vector * a;
                         x := xc+vector.re;
                         y := yc+vector.im;
                    End;
                End;
            End;
        End;
    end;
end;


function TExecHelper.DoVDiffCmd:Integer;
Var
    Fin: TBufferedFileStream = nil; 
    Fout: TFileStream = nil;
    sout: String;
    BusName, Line:String;
    i,  node, busIndex:Integer;
    Vmag, Diff:Double;
Begin
   Result := 0;
   If FileExists(DSS.OutputDirectory {CurrentDSSDir} + DSS.CircuitName_ + 'SavedVoltages.txt') Then Begin
   try
      try
         Fin := TBufferedFileStream.Create(DSS.OutputDirectory {CurrentDSSDir} + DSS.CircuitName_ + 'SavedVoltages.txt', fmOpenRead or fmShareDenyWrite);
         Fout := TBufferedFileStream.Create(DSS.OutputDirectory {CurrentDSSDir} + DSS.CircuitName_ + 'VDIFF.txt', fmCreate);

         while (Fin.Position + 1) < Fin.Size do 
         begin
             FSReadln(Fin, Line);
             DSS.AuxParser.CmdString := Line;
             DSS.AuxParser.NextParam;
             BusName := DSS.AuxParser.StrValue;
             If Length(BusName) > 0 Then 
             Begin
                 BusIndex := DSS.ActiveCircuit.BusList.Find(BusName);
                 If BusIndex>0 Then
                 Begin
                     DSS.AuxParser.Nextparam;
                     node := DSS.AuxParser.Intvalue;
                     With  DSS.ActiveCircuit.Buses^[BusIndex] Do
                     For i := 1 to NumNodesThisBus Do
                     Begin
                         If GetNum(i)=node then
                         Begin
                             DSS.AuxParser.Nextparam;
                             Vmag := DSS.AuxParser.Dblvalue;
                             Diff := Cabs(DSS.ActiveCircuit.Solution.NodeV^[GetRef(i)]) - Vmag;
                             If Vmag<>0.0 then 
                             Begin
                                WriteStr(sout, BusName,'.',node,', ', (Diff / Vmag * 100.0):7:2,', %');
                                FSWriteln(Fout, sout);
                             End
                             Else 
                             begin
                                WriteStr(sout, BusName,'.',node,', ', format('%-.5g',[Diff]),', Volts');
                                FSWriteln(Fout, sout);
                             end;
                         End;
                     End;
                 End;
             End;
         End;

    Except
          On E:Exception Do Begin
           DoSimpleMsg(DSS, 'Error opening Saved Voltages or VDIFF File: %s', [E.message], 280);
           Exit;
          End;
    End;

  Finally
   FreeAndNil(Fin);
   FreeAndNil(Fout);
   FireOffEditor(DSS, DSS.OutputDirectory {CurrentDSSDir} + DSS.CircuitName_ + 'VDIFF.txt');
  End;
  End
  Else
      DoSimpleMsg(DSS, _('Error: No Saved Voltages.'), 281);
end;

function TExecHelper.DoSummaryCmd:Integer;

// Returns summary in global result String

Var
   S:String;
   cLosses,
   cPower :Complex;

Begin
  Result := 0;
     S := '';
     IF DSS.ActiveCircuit.Issolved Then S := S + 'Status = SOLVED' + CRLF
     Else Begin
       S := S + 'Status = NOT Solved' + CRLF;
     End;
     S := S + 'Solution Mode = ' + DSS.SolveModeEnum.OrdinalToString(ord(DSS.ActiveCircuit.Solution.mode)) + CRLF;
     S := S + 'Number = ' + IntToStr(DSS.ActiveCircuit.Solution.NumberofTimes) + CRLF;
     S := S + 'Load Mult = '+ Format('%5.3f', [DSS.ActiveCircuit.LoadMultiplier]) + CRLF;
     S := S + 'Devices = '+ Format('%d', [DSS.ActiveCircuit.NumDevices]) + CRLF;
     S := S + 'Buses = ' + Format('%d', [DSS.ActiveCircuit.NumBuses]) + CRLF;
     S := S + 'Nodes = ' + Format('%d', [DSS.ActiveCircuit.NumNodes]) + CRLF;
     S := S + 'Control Mode =' + DSS.ControlModeEnum.OrdinalToString(DSS.ActiveCircuit.Solution.Controlmode) + CRLF;
     S := S + 'Total Iterations = '+IntToStr(DSS.ActiveCircuit.Solution.Iteration) + CRLF;
     S := S + 'Control Iterations = '+IntToStr(DSS.ActiveCircuit.Solution.ControlIteration) + CRLF;
     S := S + 'Max Sol Iter = ' +IntToStr(DSS.ActiveCircuit.Solution.MostIterationsDone ) + CRLF;
     S := S + ' ' + CRLF;
     S := S + ' - Circuit Summary -' + CRLF;
     S := S + ' ' + CRLF;
     If DSS.ActiveCircuit <> Nil Then begin
         S := S + Format('Year = %d ',[DSS.ActiveCircuit.Solution.Year]) + CRLF;
         S := S + Format('Hour = %d ',[DSS.ActiveCircuit.Solution.DynaVars.intHour]) + CRLF;
         S := S + 'Max pu. voltage = '+Format('%-.5g ',[GetMaxPUVoltage(DSS)]) + CRLF;
         S := S + 'Min pu. voltage = '+Format('%-.5g ',[GetMinPUVoltage(DSS, TRUE)]) + CRLF;
         cPower :=  GetTotalPowerFromSources(DSS) * 0.000001;  // MVA
         S := S + Format('Total Active Power:   %-.6g MW',[cpower.re]) + CRLF;
         S := S + Format('Total Reactive Power: %-.6g Mvar',[cpower.im]) + CRLF;
         cLosses := DSS.ActiveCircuit.Losses * 0.000001;
         If cPower.re <> 0.0 Then S := S + Format('Total Active Losses:   %-.6g MW, (%-.4g %%)',[cLosses.re,(Closses.re/cPower.re*100.0)]) + CRLF
                             Else S := S + 'Total Active Losses:   ****** MW, (**** %%)' + CRLF;
         S := S + Format('Total Reactive Losses: %-.6g Mvar',[cLosses.im]) + CRLF;
         S := S + Format('Frequency = %-g Hz',[DSS.ActiveCircuit.Solution.Frequency]) + CRLF;
         S := S + 'Mode = '+DSS.SolveModeEnum.OrdinalToString(ord(DSS.ActiveCircuit.Solution.mode)) + CRLF;
         S := S + 'Control Mode = ' + DSS.ControlModeEnum.OrdinalToString(DSS.ActiveCircuit.Solution.Controlmode) + CRLF;
         S := S + 'Load Model = '+ DSS.DefaultLoadModelEnum.OrdinalToString(DSS.ActiveCircuit.Solution.LoadModel) + CRLF;
     End;

     DSS.GlobalResult := S;
End;

function TExecHelper.DoDistributeCmd:Integer;
Var
   ParamPointer :Integer;
   ParamName,
   Param:String;

   DoGenerators :Boolean;

   kW, PF :double;
   Skip:Integer;
   How,
   FilName:String;

Begin
     Result := 0;
     ParamPointer := 0;
     {Defaults}
     kW := 1000.0;
     How := 'Proportional';
     Skip := 1;
     PF := 1.0;
     FilName := 'DistGenerators.dss';
     DoGenerators := TRUE;

     ParamName := DSS.Parser.NextParam;
     Param := DSS.Parser.StrValue;
     WHILE Length(Param)>0 DO
     Begin
         IF   (Length(ParamName) = 0)
         THEN Inc(ParamPointer)
         ELSE ParamPointer := DistributeCommands.GetCommand(ParamName);

         CASE ParamPointer OF
           1: kW := DSS.Parser.DblValue;
           2: How := DSS.Parser.StrValue;
           3: Skip := DSS.Parser.IntValue;
           4: PF := DSS.Parser.DblValue;
           5: FilName := DSS.Parser.StrValue;
           6: kW := DSS.Parser.DblValue * 1000.0;
           7: if (AnsiUpperCase(Param)[1]='L') then DoGenerators := FALSE Else DoGenerators := TRUE;  // Load or Generator

         ELSE
             // ignore unnamed and extra parms
         End;

         ParamName := DSS.Parser.NextParam;
         Param := DSS.Parser.StrValue;
     End;

     if Not DoGenerators then FilName := 'DistLoads.dss' ;

     MakeDistributedGenerators(DSS, kW, PF, How, Skip, FilName, DoGenerators);  // in Utilities

End;

function TExecHelper.DoDI_PlotCmd:Integer;
Var
    ParamName, Param:String;
    ParamPointer, i:Integer;
    CaseName:String;
    MeterName:String;
    CaseYear:integer;
    dRegisters: Array[1..NumEMRegisters] of Double;
    iRegisters:Array of Integer;
    NumRegs:Integer;
    PeakDay:Boolean;
Begin
     IF DSS.DIFilesAreOpen Then DSS.EnergyMeterClass.CloseAllDIFiles;

     // Defaults
     NumRegs:=1;
     SetLength(IRegisters, NumRegs);
     iRegisters[0] := 9;
     PeakDay := FALSE;
     CaseYear := 1;
     CaseName := '';
     MeterName := 'DI_Totals';

     ParamPointer := 0;
     ParamName := DSS.Parser.NextParam;
     Param := DSS.Parser.StrValue;
     WHILE Length(Param)>0 DO
     Begin
         IF   (Length(ParamName) = 0) THEN Inc(ParamPointer)
         ELSE ParamPointer := DI_PlotCommands.GetCommand(ParamName);

         CASE ParamPointer OF
           1: CaseName := Param;
           2: CaseYear := DSS.Parser.Intvalue;
           3: Begin
                 NumRegs := DSS.Parser.ParseAsVector(NumEMREgisters, pDoubleArray(@dRegisters));
                 SetLength(iRegisters, NumRegs);
                 For i := 1 to NumRegs Do iRegisters[i-1] := Round(dRegisters[i]);
              End;
           4: PeakDay := InterpretYesNo(Param);
           5: MeterName := Param;

         ELSE
             // ignore unnamed and extra parms
         End;

         ParamName := DSS.Parser.NextParam;
         Param := DSS.Parser.StrValue;
     End;

     //TODO!!! DSSPlotObj.DoDI_Plot(CaseName, CaseYear, iRegisters, PeakDay, MeterName);

     iRegisters := Nil;
     Result := 0;
end;

function TExecHelper.DoCompareCasesCmd:Integer;
Var
    ParamName, Param: String;
    ParamPointer: Integer;
    UnKnown: Boolean;
    Reg: Integer;
    CaseName1, CaseName2, WhichFile: String;
Begin
     IF DSS.DIFilesAreOpen Then DSS.EnergyMeterClass.CloseAllDIFiles;
     CaseName1 := 'base';
     CaseName2 := '';
     Reg := 9;    // Overload EEN
     WhichFile := 'Totals';

     ParamPointer := 0;
     ParamName := AnsiUpperCase(DSS.Parser.NextParam);
     Param := DSS.Parser.StrValue;
     WHILE Length(Param)>0 DO
     Begin
         Unknown := False;
         IF   (Length(ParamName) = 0) THEN Inc(ParamPointer)

         ELSE Begin
             If  CompareTextShortest(ParamName, 'CASE1')=0 then ParamPointer:=1
             ELSE If  CompareTextShortest(ParamName, 'CASE2')=0 then ParamPointer:=2
             ELSE If  CompareTextShortest(ParamName, 'REGISTER')=0 then ParamPointer:=3
             ELSE If  CompareTextShortest(ParamName, 'METER')=0 then ParamPointer:=4
             ELSE Unknown := TRUE;
         End;


         If Not Unknown then
         CASE ParamPointer OF
           1: CaseName1 := Param;
           2: CaseName2 := Param;
           3: Reg := DSS.Parser.IntValue;
           4: WhichFile := Param;
         ELSE
             // ignore unnamed and extra parms
         End;

         ParamName := AnsiUpperCase(DSS.Parser.NextParam);
         Param := DSS.Parser.StrValue;
     End;

     //TODO!!! DSSPlotObj.DoCompareCases(CaseName1, CaseName2, WhichFile,  Reg);
     Result := 0;
end;

function TExecHelper.DoYearlyCurvesCmd:Integer;
Var
    ParamName, Param: String;
    ParamPointer, i: Integer;
    UnKnown: Boolean;
    CaseNames: TStringList;
    dRegisters: Array[1..NumEMRegisters] of Double;
    iRegisters: Array of Integer;
    Nregs: Integer;
    WhichFile: String;
Begin
     IF DSS.DIFilesAreOpen Then DSS.EnergyMeterClass.CloseAllDIFiles;

     Nregs := 1;
     SetLength(iRegisters, Nregs);
     CaseNames := TStringList.Create;
     CaseNames.Clear;
     WhichFile := 'Totals';

     ParamPointer := 0;
     ParamName := DSS.Parser.NextParam;
     Param := DSS.Parser.StrValue;
     WHILE Length(Param)>0 DO
     Begin
         Unknown := False;
         IF   (Length(ParamName) = 0) THEN Inc(ParamPointer)

         ELSE Case AnsiUpperCase(ParamName)[1] of
                    'C':ParamPointer := 1;
                    'R':ParamPointer := 2;
                    'M':ParamPointer := 3; {meter=}
              ELSE
                   Unknown := TRUE;
              END;

         If Not Unknown then
         CASE ParamPointer OF
           1: Begin  // List of case names
                DSS.AuxParser.CmdString := Param;
                DSS.AuxParser.NextParam;
                Param := DSS.AuxParser.StrValue;
                While Length(Param)>0 Do Begin
                    CaseNames.Add(Param);
                    DSS.AuxParser.NextParam;
                    Param := DSS.AuxParser.StrValue;
                End;
              End;
           2: Begin
                NRegs := DSS.Parser.ParseAsVector(NumEMRegisters, pDoubleArray(@dRegisters));
                SetLength(iRegisters, Nregs);
                For i := 1 to NRegs Do iRegisters[i-1] := Round(dRegisters[i]);
              end;
           3: WhichFile := Param ;
         ELSE
             // ignore unnamed and extra parms
         End;

         ParamName := DSS.Parser.NextParam;
         Param := DSS.Parser.StrValue;
     End;

     //TODO!!! DSSPlotObj.DoYearlyCurvePlot(CaseNames, WhichFile,  iRegisters);

     iRegisters := Nil;
     CaseNames.Free;
     Result := 0;
End;

function TExecHelper.DoVisualizeCmd:Integer;
Var
    DevIndex: integer;
    Param: String;
    ParamName: String;
    ParamPointer: Integer;
    Unknown: Boolean;
    Quantity: String;
    ElemName: String;
    plotParamsStr: String;
    pElem: TDSSObject;
    plotParams: TJSONObject = NIL;
Begin
    Result := 0;
     // Abort if no circuit or solution
    If not assigned(DSS.ActiveCircuit) Then
    Begin
         DoSimpleMsg(DSS, _('No circuit created.'), 24721);
         Exit;
    End;
    If not assigned(DSS.ActiveCircuit.Solution) OR not assigned(DSS.ActiveCircuit.Solution.NodeV) Then
    Begin
         DoSimpleMsg(DSS, _('The circuit must be solved before you can do this.'), 24722);
         Exit;
    End;
    Quantity := 'Current';
    ElemName := '';
    // Parse rest of command line
    ParamPointer := 0;
    ParamName := AnsiUpperCase(DSS.Parser.NextParam);
    Param := DSS.Parser.StrValue;
    WHILE Length(Param)>0 DO
    Begin
        Unknown := False;
        if (Length(ParamName) = 0) then
            Inc(ParamPointer)
        ELSE 
        begin
            If  CompareTextShortest(ParamName, 'WHAT')=0 then ParamPointer:=1
            ELSE If  CompareTextShortest(ParamName, 'ELEMENT')=0 then ParamPointer:=2
            ELSE 
              Unknown := TRUE;
        End;
        If Not Unknown then
        CASE ParamPointer OF
          1: Case AnsiLowerCase(Param)[1] of
               'c':  Quantity := 'Current';
               'v':  Quantity := 'Voltage';
               'p':  Quantity := 'Power';
              End;
          2: ElemName := Param;
        ELSE
            // ignore unnamed and extra parms
        End;
        ParamName := AnsiUpperCase(DSS.Parser.NextParam);
        Param := DSS.Parser.StrValue;
    End;
    // --------------------------------------------------------------
    Devindex := GetCktElementIndex(DSS, ElemName); // Global function
    if DevIndex > 0 then
    begin  //  element must already exist
        pElem := DSS.ActiveCircuit.CktElements.Get(DevIndex);
        if not (pElem is TDSSCktElement) then
        begin
            DoSimpleMsg(DSS, '"%s" must be a circuit element type!', [pElem.Name], 282);   // Wrong type
            Exit;
        end;
    end 
    else
    begin
        DoSimpleMsg(DSS, 'Requested Circuit Element: "%s" not found.', [ElemName], 282); // Did not find it ..
        Exit;
    end;

    try
        // pElem.ComputeIterminal(); 
        // pElem.ComputeVTerminal();
        plotParams := TJSONObject.Create([
            'PlotType', 'Visualize',
            'ElementName', pElem.Name,
            'ElementType', pElem.DSSClassName,
            'Quantity', Quantity
        ]);
        // plotParams.CompressedJSON := True;
        plotParamsStr := plotParams.FormatJSON();
        if (@DSS.DSSPlotCallback) <> NIL then
          DSS.DSSPlotCallback(DSS, PChar(plotParamsStr));
    finally
        FreeAndNil(plotParams);
    end;    
End;

function TExecHelper.DoCloseDICmd:Integer;

Begin
    Result  := 0;
    DSS.EnergyMeterClass.CloseAllDIFiles;
End;

function TExecHelper.DoADOScmd:Integer;

Begin
    Result  := 0;
    DoDOScmd(DSS, DSS.Parser.Remainder);
End;

function TExecHelper.DoEstimateCmd:Integer;



Begin
    Result := 0;

    // Load current Estimation is driven by Energy Meters at head of feeders.
    DoAllocateLoadsCmd;

     // Let's look to see how well we did
     If not DSS.AutoShowExport Then DSS.DSSExecutive.Command := 'Set showexport=yes';
     DSS.DSSExecutive.Command := 'Export Estimation';
end;



function TExecHelper.DoReconductorCmd:Integer;

Var
     Param       :String;
     ParamName   :String;
     ParamPointer:Integer;
     Line1, Line2,
     Linecode,
     Geometry,
     EditString,
     MyEditString:String;
     LineCodeSpecified,
     GeometrySpecified :Boolean;
     pLine1, pLine2 :TLineObj;
     LineClass :TLine;
     TraceDirection :Integer;
     NPhases: Integer;


Begin
     Result := 0;
     ParamPointer := 0;
     LineCodeSpecified := FALSE;
     GeometrySpecified := FALSE;
     Line1 := '';
     Line2 := '';
     MyEditString := '';
     NPhases := 0; // no filtering by number of phases
     ParamName := DSS.Parser.NextParam;
     Param := DSS.Parser.StrValue;
     while Length(Param) > 0 do Begin
       IF Length(ParamName) = 0 THEN Inc(ParamPointer)
       ELSE ParamPointer := ReconductorCommands.GetCommand(ParamName);

       Case ParamPointer of
          1: Line1 := Param;
          2: Line2 := Param;
          3: Begin Linecode := Param; LineCodeSpecified := TRUE; GeometrySpecified := FALSE; End;
          4: Begin Geometry := Param; LineCodeSpecified := FALSE; GeometrySpecified := TRUE; End;
          5: MyEditString := Param;
          6: Nphases := DSS.Parser.IntValue;
       Else
          DoSimpleMsg(DSS, 'Error: Unknown Parameter on command line: %s', [Param], 28701);
       End;

      ParamName := DSS.Parser.NextParam;
      Param := DSS.Parser.StrValue;
     End;

     {Check for Errors}

     {If user specified full line name, get rid of "line."}
     Line1 := StripClassName(Line1);
     Line2 := StripClassName(Line2);

     If (Length(Line1)=0) or (Length(Line2)=0) then Begin
       DoSimpleMsg(DSS, _('Both Line1 and Line2 must be specified!'), 28702);
       Exit;
     End;

     If (Not LineCodeSpecified) and (Not GeometrySpecified) then Begin
       DoSimpleMsg(DSS, _('Either a new LineCode or a Geometry must be specified!'), 28703);
       Exit;
     End;

     LineClass := DSS.DSSClassList.Get(DSS.ClassNames.Find('Line'));
     pLine1 := LineClass.Find(Line1);
     pLine2 := LineCLass.Find(Line2);

     If (pLine1 = Nil) or (pLine2=NIL) then Begin
       If pLine1=Nil then DoSimpleMsg(DSS, 'Line.%s not found.', [Line1], 28704)
       Else If pLine2=Nil then DoSimpleMsg(DSS, 'Line.%s not found.', [Line2], 28704);
       Exit;
     End;

     {Now check to make sure they are in the same meter's zone}
     If (pLine1.MeterObj=Nil) or (pLine2.MeterObj=Nil)  then Begin
       DoSimpleMsg(DSS, _('Error: Both Lines must be in the same EnergyMeter zone. One or both are not in any meter zone.'), 28705);
       Exit;
     End;

     If pLine1.MeterObj<>pline2.MeterObj then Begin
       DoSimpleMsg(DSS, 'Error: Line1 is in %s zone while Line2 is in %s zone. Both must be in the same Zone.', 
                   [pLine1.MeterObj.FullName, pLine2.MeterObj.FullName], 28706);
       Exit;
     End;

     {Since the lines can be given in either order, Have to check to see which direction they are specified and find the path between them}
     TraceDirection := 0;
     If IsPathBetween(pLine1, pLine2) then TraceDirection := 1;
     If IsPathBetween(pLine2, pLine1) then TraceDirection := 2;

     If LineCodeSpecified Then EditString := 'Linecode=' + LineCode
     Else                      EditString := 'Geometry=' + Geometry;

     // Append MyEditString onto the end of the edit string to change the linecode  or geometry
     EditString := Format('%s  %s',[EditString, MyEditString]);

     case TraceDirection of
          1: TraceAndEdit(DSS, pLine1, pLine2, NPhases, Editstring);
          2: TraceAndEdit(DSS, pLine2, pLine1, NPhases, Editstring);
     Else
         DoSimpleMsg(DSS, _('Traceback path not found between Line1 and Line2.'), 28707);
         Exit;
     end;
end;

function TExecHelper.DoAddMarkerCmd:Integer;
Var
   ParamPointer :Integer;
   ParamName,
   Param:String;
   BusMarker:TBusMarker;

Begin
     Result := 0;
     ParamPointer := 0;

     BusMarker := TBusMarker.Create;
     DSS.ActiveCircuit.BusMarkerList.Add(BusMarker);

     ParamName := DSS.Parser.NextParam;
     Param := DSS.Parser.StrValue;
     WHILE Length(Param)>0 DO
     Begin
         IF   (Length(ParamName) = 0)
         THEN Inc(ParamPointer)
         ELSE ParamPointer := AddmarkerCommands.GetCommand(ParamName);

         With BusMarker Do
         CASE ParamPointer OF
           1: BusName := Param;
           2: AddMarkerCode := DSS.Parser.IntValue;
           3: AddMarkerColor:= InterpretColorName(DSS, Param);
           4: AddMarkerSize := DSS.Parser.IntValue;

         ELSE
             // ignore unnamed and extra parms
         End;

         ParamName := DSS.Parser.NextParam;
         Param := DSS.Parser.StrValue;
     End;
end;

function TExecHelper.DoSetLoadAndGenKVCmd:Integer;
VAR
  pLoad :TLoadObj;
  pGen :TGeneratorObj;
  pBus :TDSSBus;
  sBus : String;
  iBus, i : integer;
  kvln : double;
Begin
  Result := 0;
  pLoad := DSS.ActiveCircuit.Loads.First;
  WHILE pLoad <> NIL Do Begin
    sBus := StripExtension (pLoad.GetBus(1));
    iBus := DSS.ActiveCircuit.BusList.Find (sBus);
    pBus := DSS.ActiveCircuit.Buses^[iBus];
    kvln := pBus.kVBase;
    if (pLoad.Connection = TLoadConnection.Delta) Or (pLoad.NPhases = 3) then
      pLoad.kVLoadBase := kvln * sqrt (3.0)
    else
      pLoad.kVLoadBase := kvln;
    
    pLoad.PropertySideEffects(ord(TLoadProp.kV));
    pLoad.RecalcElementData;
    pLoad := DSS.ActiveCircuit.Loads.Next;
  End;

  For i := 1 to DSS.ActiveCircuit.Generators.Count Do Begin
    pGen := DSS.ActiveCircuit.Generators.Get(i);
    sBus := StripExtension (pGen.GetBus(1));
    iBus := DSS.ActiveCircuit.BusList.Find (sBus);
    pBus := DSS.ActiveCircuit.Buses^[iBus];
    kvln := pBus.kVBase;
    if (pGen.Connection = 1) Or (pGen.NPhases > 1) then
      pGen.PresentKV := kvln * sqrt (3.0)
    else
      pGen.PresentKV := kvln;
    pGen.RecalcElementData;
  End;
end;

function TExecHelper.DoUuidsCmd:Integer;
Var
  F: TStream = nil;
  Param, S, NameVal, UuidVal, DevClass, DevName: String;
  pName: TNamedObject;
  idx: integer;
Begin
  DSS.CIMExporter.StartUuidList(DSS.ActiveCircuit.NumBuses + 2 * DSS.ActiveCircuit.NumDevices);
  Result := 0;
  DSS.Parser.NextParam;
  Param := DSS.Parser.StrValue;
  Try
    F := DSS.GetROFileStream(Param);
    DSS.AuxParser.Delimiters := ',';
    While (F.Position + 1) < F.Size Do 
    Begin
      FSReadln(F, S);
      With DSS.AuxParser Do Begin
        pName := nil;
        CmdString := S;
        NextParam;  NameVal := StrValue;
        NextParam;  UuidVal := StrValue;
        // format the UUID properly
        if Pos ('{', UuidVal) < 1 then
          UuidVal := '{' + UuidVal + '}';
        if Pos ('=', NameVal) > 0 then 
        begin  // it's a non-identified object in OpenDSS
          DSS.CIMExporter.AddHashedUuid(NameVal, UuidVal);
        end 
        else 
        begin  // find this as a descendant of TNamedObject
          pName := nil;
          ParseObjectClassAndName(DSS, NameVal, DevClass, DevName);
          IF CompareText (DevClass, 'circuit')=0 THEN 
          begin
            pName := DSS.ActiveCircuit
          end 
          else 
          if CompareText (DevClass, 'Bus')=0 then 
          begin
            idx := DSS.ActiveCircuit.BusList.Find(DevName);
            if idx > 0 then pName := DSS.ActiveCircuit.Buses^[idx];
          end else 
          begin
            DSS.LastClassReferenced := DSS.ClassNames.Find (DevClass);
            DSS.ActiveDSSClass := DSS.DSSClassList.Get(DSS.LastClassReferenced);
            if DSS.ActiveDSSClass <> nil then
              if DSS.ActiveDSSClass.SetActive (DevName) then
                pName := DSS.ActiveDSSClass.GetActiveObj;
          end;
          // re-assign its UUID
          if pName <> nil then pName.UUID := StringToUuid(UuidVal);
        end;
      End;
    End;
  Finally
    DSS.AuxParser.ResetDelims;
    FreeAndNil(F);
  End;
End;

function TExecHelper.DoCvrtLoadshapesCmd:Integer;
Var
   pLoadshape :TLoadShapeObj;
   iLoadshape :Integer;
   LoadShapeClass :TLoadShape;
   //ParamName      :String;
   Param          :String;
   Action         :String;
   F: TFileStream = nil;
   Fname          :String;

Begin
    DSS.Parser.NextParam;
    Param := DSS.Parser.StrValue;

    If length(param)=0 then  Param := 's';

    {Double file or Single file?}
    CASE AnsiLowerCase(param)[1] of
        'd': Action := 'action=dblsave';
    ELSE
        Action := 'action=sngsave';   // default
    END;

     LoadShapeClass := GetDSSClassPtr(DSS, 'loadshape') as TLoadShape;

     Fname := DSS.OutputDirectory {CurrentDSSDir} + 'ReloadLoadshapes.dss';
     F := TBufferedFileStream.Create(Fname, fmCreate);

     iLoadshape := LoadShapeClass.First;
     while iLoadshape > 0 do  Begin
        pLoadShape := LoadShapeClass.GetActiveObj;
        DSS.Parser.CmdString := Action;
        pLoadShape.Edit(DSS.Parser);
        FSWriteln(F, Format('New %s Npts=%d Interval=%.8g %s', [pLoadShape.FullName, pLoadShape.NumPoints, pLoadShape.Interval, DSS.GlobalResult]));
        iLoadshape := LoadShapeClass.Next;
     End;

     FreeAndNil(F);
     FireOffEditor(DSS, Fname);
     Result := 0;
End;

function TExecHelper.DoNodeDiffCmd:Integer;

Var
   ParamName      :String;
   Param          :String;
   sNode1, sNode2   :String;
   SBusName       :String;
   V1, V2,
   VNodeDiff      :Complex;
   iBusidx        :Integer;
   B1ref          :integer;
   B2ref          :Integer;
   NumNodes       :Integer;
   NodeBuffer     :Array[1..50] of Integer;


begin
    Result := 0;
    ParamName := DSS.Parser.NextParam;
    Param := DSS.Parser.StrValue;
    sNode1 := Param;
    If Pos('2',ParamName)>0 then sNode2 := Param;

    ParamName := DSS.Parser.NextParam;
    Param := DSS.Parser.StrValue;
    sNode2 := Param;
    If Pos('1',ParamName)>0 then sNode1 := Param;

    // Get first node voltage
    NodeBuffer[1] := 1;
    sBusName := DSS.AuxParser.ParseAsBusName(sNode1, numNodes, pIntegerArray(@NodeBuffer));
    iBusidx := DSS.ActiveCircuit.Buslist.Find(sBusName);
    If iBusidx>0 Then Begin
        B1Ref := DSS.ActiveCircuit.Buses^[iBusidx].Find(NodeBuffer[1])
    End Else Begin
        DoSimpleMsg(DSS, 'Bus %s not found.', [sBusName], 28709);
        Exit;
    End;

    V1 := DSS.ActiveCircuit.Solution.NodeV^[B1Ref];

    // Get 2nd node voltage
    NodeBuffer[1] := 1;
    sBusName := DSS.AuxParser.ParseAsBusName(sNode2, numNodes, pIntegerArray(@NodeBuffer));
    iBusidx := DSS.ActiveCircuit.Buslist.Find(sBusName);
    If iBusidx>0 Then Begin
        B2Ref := DSS.ActiveCircuit.Buses^[iBusidx].Find(NodeBuffer[1])
    End Else Begin
        DoSimpleMsg(DSS, 'Bus %s not found.', [sBusName], 28710);
        Exit;
    End;

    V2 := DSS.ActiveCircuit.Solution.NodeV^[B2Ref];

    VNodeDiff := V1 - V2;
    DSS.GlobalResult := Format('%.7g, V,    %.7g, deg  ',[Cabs(VNodeDiff), CDang(VNodeDiff) ]);
end;

function TExecHelper.DoRephaseCmd:Integer;
Var
     Param          :String;
     ParamName      :String;
     ParamPointer   :Integer;
     StartLine      :String;
     NewPhases      :String;
     MyEditString   :String;
     ScriptfileName :String;
     pStartLine     :TLineObj;
     LineClass      :TLine;
     TransfStop     :Boolean;

Begin
     Result       := 0;
     ParamPointer := 0;
     MyEditString := '';
     ScriptfileName := 'RephaseEditScript.dss';
     TransfStop     := TRUE;  // Stop at Transformers

     ParamName      := DSS.Parser.NextParam;
     Param          := DSS.Parser.StrValue;
     while Length(Param) > 0 do Begin
       IF Length(ParamName) = 0 THEN Inc(ParamPointer)
       ELSE ParamPointer := RephaseCommands.GetCommand(ParamName);

       Case ParamPointer of
          1: StartLine := Param;
          2: NewPhases := Param;
          3: MyEditString := Param;
          4: ScriptFileName := Param;
          5: TransfStop := InterpretYesNo(Param);
       Else
          DoSimpleMsg(DSS, 'Error: Unknown Parameter on command line: %s', [Param], 28711);
       End;

      ParamName := DSS.Parser.NextParam;
      Param := DSS.Parser.StrValue;
     End;

     LineClass := DSS.DSSClassList.Get(DSS.ClassNames.Find('Line'));
     pStartLine := LineClass.Find(StripClassName(StartLine));
     If pStartLine=Nil then  Begin
         DoSimpleMsg(DSS, 'Starting Line (%s) not found.', [StartLine], 28712);
         Exit;
     End;
     {Check for some error conditions and abort if necessary}
     If pStartLine.MeterObj=Nil then  Begin
         DoSimpleMsg(DSS, _('Starting Line must be in an EnergyMeter zone.'), 28713);
         Exit;
     End;

     If not (pStartLine.MeterObj is TEnergyMeterObj) then  Begin
         DoSimpleMsg(DSS, _('Starting Line must be in an EnergyMeter zone.'), 28714);
         Exit;
     End;

     GoForwardandRephase(DSS, pStartLine, NewPhases, MyEditString, ScriptfileName, TransfStop);
end;

function TExecHelper.DoSetBusXYCmd:Integer;

Var
     Param          :String;
     ParamName      :String;
     ParamPointer   :Integer;
     BusName        :String;
     Xval           :Double;
     Yval           :Double;
     iB             :Integer;

begin
     Result := 0;
     ParamName      := DSS.Parser.NextParam;
     Param          := DSS.Parser.StrValue;
     ParamPointer   := 0;
     Xval := 0.0;  Yval := 0.0;
     while Length(Param) > 0 do Begin
       IF Length(ParamName) = 0 THEN Inc(ParamPointer)
       ELSE ParamPointer := SetBusXYCommands.GetCommand(ParamName);

       Case ParamPointer of
          1: BusName := Param;
          2: Xval := DSS.Parser.DblValue;
          3: Yval := DSS.Parser.DblValue;
       Else
          DoSimpleMsg(DSS, 'Error: Unknown Parameter on command line: %s', [Param], 28721);
       End;

       iB := DSS.ActiveCircuit.Buslist.Find(BusName);
       If iB >0 Then  Begin
           With DSS.ActiveCircuit.Buses^[iB] Do Begin     // Returns TBus object
             x := Xval;
             y := Yval;
             CoordDefined := TRUE;
           End;
       End Else Begin
           DoSimpleMsg(DSS, 'Error: Bus "%s" not found.', [BusName], 28722);
       End;

      ParamName := DSS.Parser.NextParam;
      Param := DSS.Parser.StrValue;
     End;
end;

function TExecHelper.DoUpdateStorageCmd:Integer;

Begin
    if DSS_CAPI_LEGACY_MODELS then
        DSS.StorageClass.UpdateAll
    else
        DSS.Storage2Class.UpdateAll;

    Result := 0;
End;

function TExecHelper.DoPstCalc;

Var
     Param          :String;
     ParamName      :String;
     ParamPointer   :Integer;
     Npts           :Integer;
     Varray         :pDoubleArray;
     CyclesPerSample:Integer;
     Lamp           :Integer;
     PstArray       :pDoubleArray;
     nPst           :Integer;
     i              :integer;
     S              :String;
     Freq           :Double;

begin
     Result := 0;
     Varray   := nil;
     PstArray := nil;
     Npts   := 0;
     Lamp   := 120;  // 120 or 230
     CyclesPerSample := 60;
     Freq := DSS.DefaultBaseFreq;

     ParamName      := DSS.Parser.NextParam;
     Param          := DSS.Parser.StrValue;
     ParamPointer   := 0;
     while Length(Param) > 0 do Begin
         IF    Length(ParamName) = 0 THEN Inc(ParamPointer)
         ELSE  ParamPointer := PstCalcCommands.GetCommand(ParamName);
         // 'Npts', 'Voltages', 'cycles', 'lamp'
         Case ParamPointer of
            1: Begin
                 Npts  := DSS.Parser.IntValue;
                 Reallocmem(Varray, SizeOf(Varray^[1])*Npts);
               End;
            2: Npts    := InterpretDblArray(DSS, Param, Npts, Varray);
            3: CyclesPerSample := Round(DSS.ActiveCircuit.Solution.Frequency * DSS.Parser.dblvalue);
            4: Freq   := DSS.Parser.DblValue;
            5: Lamp    := DSS.Parser.IntValue;
         Else
            DoSimpleMsg(DSS, 'Error: Unknown Parameter on command line: %s', [Param], 28722);
         End;

        ParamName := DSS.Parser.NextParam;
        Param := DSS.Parser.StrValue;
     End;

     If Npts>10 Then
     begin
         nPst := PstRMS(PstArray, Varray, Freq, CyclesPerSample, Npts, Lamp);
         // put resulting pst array in the result string
         S := '';
         For i := 1 to nPst Do  S := S + Format('%.8g, ', [PstArray^[i]]);
         DSS.GlobalResult := S;
     End
     Else DoSimpleMsg(DSS, _('Insuffient number of points for Pst Calculation.'), 28723);


     Reallocmem(Varray,   0);   // discard temp arrays
     Reallocmem(PstArray, 0);
End;

function TExecHelper.DoLambdaCalcs:Integer;
// Execute fault rate and bus number of interruptions calc
Var pMeter : TEnergyMeterObj;
    i      : Integer;
    //ParamName,
    Param  : String;
    AssumeRestoration : Boolean;

Begin
      Result := 0;

// Do for each Energymeter object in active circuit
      pMeter := DSS.ActiveCircuit.EnergyMeters.First;
      If pMeter=nil Then Begin
        DoSimpleMsg(DSS, _('No EnergyMeter Objects Defined. EnergyMeter objects required for this function.'), 28724);
        Exit;
      End;

      DSS.Parser.NextParam;
      Param := DSS.Parser.StrValue ;

      If Length(Param)>0 Then
          Assumerestoration := InterpretYesNo(param)
      Else
          Assumerestoration := False;

       // initialize bus quantities
       With DSS.ActiveCircuit Do
       For i := 1 to NumBuses Do
         With Buses^[i] Do Begin
            BusFltRate        := 0.0;
            Bus_Num_Interrupt := 0.0;
         End;

      while pMeter <> Nil do Begin
         pMeter.CalcReliabilityIndices(AssumeRestoration);
         pMeter := DSS.ActiveCircuit.EnergyMeters.Next;
      End;
End;

function TExecHelper.DoVarCmd:Integer;
{Process Script variables}

VAR
   ParamName:String;
   Param:String;
   Str  : String;
   iVar : Integer;
begin
     Result := 0;

     ParamName := DSS.Parser.NextParam;
     Param := DSS.Parser.StrValue;

      If Length(Param)=0 Then  // show all vars
      Begin
          {
          MsgStrings := TStringList.Create;
          MsgStrings.Add('Variable, Value');
          for iVar := 1 to DSS.ParserVars.NumVariables  do
              MsgStrings.Add(DSS.ParserVars.VarString[iVar] );
          ShowMessageForm(MsgStrings);
          MsgStrings.Free;}
          Str := _('Variable, Value') + CRLF;
          for iVar := 1 to DSS.ParserVars.NumVariables do
            Str := Str + DSS.ParserVars.VarString[iVar]+CRLF;

          DSS.GlobalResult := Str;
      End Else if Length(ParamName)=0 then   // show value of this var
      Begin
           DSS.GlobalResult := Param;  // DSS.Parser substitutes @var with value
      End
      Else Begin
           WHILE Length(ParamName)>0 Do Begin
               case ParamName[1] of
                  '@': DSS.ParserVars.Add(ParamName, Param);
               else
                   DoSimpleMsg(DSS, 'Illegal Variable Name: %s; Must begin with "@"', [ParamName], 28725);
                   Exit;
               end;
               ParamName := DSS.Parser.NextParam;
               Param := DSS.Parser.StrValue;
           End;

      End;
end;

function TExecHelper.DoRemoveCmd:Integer;
var
    ParamName: String;
    Param: String;
    ParamPointer: Integer;
    DeviceIndex: Integer;

    FElementName: String;
    FKeepLoad: Boolean;
    FEditString: String;

    elem: TDSSObject;
    pPDElem: TPDelement;
    pMeter: TEnergyMeterObj;
    FMeterName: String;
begin
    Result := 0;
    if DSS.ActiveCircuit = nil then
    begin
        DoSimpleMsg(DSS, _('Error: There is no active circuit!'), 28998);
        Exit;
    end;
    
    FElementName := '';
    FEditString := '';
    FKeepLoad := TRUE;
    ParamPointer := 0;
    
    ParamName := DSS.Parser.NextParam;
    Param := DSS.Parser.StrValue;
    
    while Length(Param) > 0 do
    begin
        if Length(ParamName) = 0 then 
            Inc(ParamPointer)
        else 
            ParamPointer := RemoveCommands.GetCommand(ParamName);

        case ParamPointer of
            1: FElementName := Param; {ElementName}
            2: FkeepLoad := InterpretYesNo(Param); {KeepLoad}
            3: FEditString := Param; {EditString}
        end;

        ParamName := DSS.Parser.NextParam;
        Param := DSS.Parser.StrValue;
    end;
    
    // Check for existence of FelementName
    DeviceIndex := GetCktElementIndex(DSS, FElementName);
    if DeviceIndex = 0 then
    begin
         DoSimpleMsg(DSS,
            Format('Error: Element %s does not exist in this circuit.', [FelementName]),
            28726
        );
        Exit;
    end;

    // first, checks if the element is not linked to an energy meter, if it does, abort (added 01/06/2021 -DM)
    elem := DSS.ActiveCircuit.CktElements.Get(DeviceIndex);
    with DSS.ActiveCircuit do
    begin
        pMeter := EnergyMeters.First;
        while pMeter <> NIL do
        begin
            if pMeter.MeteredElement = elem then
            begin
                DoSimpleMsg(DSS,
                    Format('Error: Element %s is tied to an Energy Meter.', [FelementName]),
                    28800
                );
                Exit;
            end;
            pMeter := EnergyMeters.Next;
        end;
    end;
    
    // Set CktElement active
    SetObject(DSS, FelementName);
    if not (DSS.ActiveCircuit.ActiveCktElement is TPDElement) then 
    begin
        DoSimpleMsg(DSS,
            Format('Error: Element "%s" is not a power delivery element (PDElement)', [FelementName]), 
            28728
        );
        Exit;
    end;

    // Get Energymeter associated with this element.
    pPDElem := DSS.ActiveCircuit.ActiveCktElement as TPDElement;
    if pPDElem.SensorObj = NIL then 
    begin
        DoSimpleMsg(DSS,
            'Element "%s" is not in a meter zone! Add an Energymeter. ', 
            [pPDelem.FullName],
        287261);
        Exit;
    end;
    
    FMeterName := pPDElem.SensorObj.FullName;
    SetObject(DSS, FMeterName);

    if not (DSS.ActiveCircuit.ActiveCktElement is TEnergyMeterObj) then 
    begin
        DoSimpleMsg(DSS,
            'Error: The Sensor Object for "%s" is not an EnergyMeter object', [FelementName],
            28727
        );
        Exit;
    end;
    
    pMeter := DSS.ActiveCircuit.ActiveCktElement as TEnergyMeterObj;
    // in ReduceAlgs
    DoRemoveBranches(DSS, pMeter.BranchList, pPDelem, FKeepLoad, FEditString);
end;

initialization
    // Initialize Command lists

    SaveCommands := TCommandList.Create(['class', 'file', 'dir', 'keepdisabled'], True);
    DI_PlotCommands := TCommandList.Create(['case','year','registers','peak','meter']);
    DistributeCommands := TCommandList.Create(['kW','how','skip','pf','file','MW','what'], True);
    ReconductorCommands := TCommandList.Create(['Line1', 'Line2', 'LineCode', 'Geometry', 'EditString', 'Nphases'], True);
    RephaseCommands := TCommandList.Create(['StartLine', 'PhaseDesignation', 'EditString', 'ScriptFileName', 'StopAtTransformers'], True);
    AddMarkerCommands := TCommandList.Create(['Bus', 'code', 'color', 'size'], True);
    SetBusXYCommands := TCommandList.Create(['Bus', 'x', 'y'], True);
    PstCalcCommands := TCommandList.Create(['Npts', 'Voltages', 'dt', 'Frequency', 'lamp'], True);
    RemoveCommands := TCommandList.Create(['ElementName', 'KeepLoad', 'Editstring'], True);

finalization

    DistributeCommands.Free;
    DI_PlotCommands.Free;
    SaveCommands.Free;
    AddMarkerCommands.Free;
    ReconductorCommands.Free;
    RephaseCommands.Free;
    SetBusXYCommands.Free;
    PstCalcCommands.Free;
    RemoveCommands.Free;

end.