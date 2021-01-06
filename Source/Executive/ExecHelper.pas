unit ExecHelper;
{
  ----------------------------------------------------------
  Copyright (c) 2008-2015, Electric Power Research Institute, Inc.
  All rights reserved.
  ----------------------------------------------------------
}

{Functions for performing DSS Exec Commands and Options}
{
 8-17-00  Updated Property Dump to handle wildcards
 10-23-00 Fixed EnergyMeters iteration error in DoAllocateLoadsCmd
 7/6/01  Fixed autobuslist command parsing of file
 7/19/01 Added DoMeterTotals
 8/1/01 Revised the Capacity Command return values
 9/12/02 Added Classes and UserClasses
 3/29/03 Implemented DoPlotCmd and Buscoords
 4/24/03  Implemented Keep list and other stuff related to circuit reduction
}

{$WARN UNIT_PLATFORM OFF}

interface


         FUNCTION DoNewCmd:Integer;
         FUNCTION DoEditCmd:Integer;
         FUNCTION DoBatchEditCmd:Integer;
         FUNCTION DoSelectCmd:Integer;
         FUNCTION DoMoreCmd:Integer;
         FUNCTION DoRedirect(IsCompile:Boolean):Integer;
         FUNCTION DoSaveCmd:Integer;
         FUNCTION DoSampleCmd(ActorID : Integer):Integer;


         FUNCTION DoSolveCmd:Integer;
         FUNCTION DoEnableCmd:Integer;
         FUNCTION DoDisableCmd:Integer;

         FUNCTION DoOpenCmd:Integer;
         FUNCTION DoResetCmd(ActorID : Integer):Integer;
         FUNCTION DoNextCmd:Integer;
         FUNCTION DoFormEditCmd:Integer;  
         FUNCTION DoClassesCmd:Integer;
         FUNCTION DoUserClassesCmd:Integer;
         FUNCTION DoHelpCmd:Integer;
         FUNCTION DoClearCmd:Integer;
         FUNCTION DoClearAllCmd:Integer;
         FUNCTION DoReduceCmd:Integer;
         FUNCTION DoInterpolateCmd:Integer;

         FUNCTION DoCloseCmd:Integer;
         FUNCTION DoResetMonitors(ActorID : Integer):Integer;

         FUNCTION DoFileEditCmd:Integer;
         FUNCTION DoQueryCmd:Integer;
         FUNCTION DoResetMeters(ActorID : Integer):Integer;
         PROCEDURE DoAboutBox;
         FUNCTION  DoSetVoltageBases(ActorID : Integer):Integer;
         FUNCTION DoSetkVBase: Integer;

         PROCEDURE DoLegalVoltageBases;
         PROCEDURE DoAutoAddBusList(Const S:String);
         PROCEDURE DoKeeperBusList(Const S:String);
         PROCEDURE DoSetReduceStrategy(Const S:String);
         PROCEDURE DoSetAllocationFactors(Const X:Double);
         PROCEDURE DoSetCFactors(Const X:Double);

         FUNCTION DovoltagesCmd(Const PerUnit:Boolean): Integer;
         FUNCTION DocurrentsCmd :Integer;
         FUNCTION DopowersCmd(Total : Integer) :Integer;
         FUNCTION DoseqvoltagesCmd :Integer;
         FUNCTION DoseqcurrentsCmd :Integer;
         FUNCTION DoseqpowersCmd :Integer;
         FUNCTION DolossesCmd :Integer;
         FUNCTION DophaselossesCmd :Integer;
         FUNCTION DocktlossesCmd :Integer;
         FUNCTION DoAllocateLoadsCmd(ActorID : Integer) :Integer;
         FUNCTION DoHarmonicsList(const S:String):Integer;
         FUNCTION DoMeterTotals:Integer;
         FUNCTION DoCapacityCmd:Integer;
         FUNCTION DoZscCmd(Zmatrix:Boolean): Integer;
         FUNCTION DoZsc10Cmd: Integer;
         FUNCTION DoZscRefresh(ActorID : Integer):Integer;
         FUNCTION DoZsc012Cmd: Integer;


         FUNCTION DoBusCoordsCmd(SwapXY:Boolean; CoordType  : Integer):Integer;
         FUNCTION DoUuidsCmd:Integer;
         FUNCTION DoSetLoadAndGenKVCmd:Integer;
         FUNCTION DoVarValuesCmd:Integer;
         FUNCTION DoVarNamesCmd :Integer;

         FUNCTION DoMakePosSeq:Integer;
         FUNCTION DoAlignFileCmd:Integer;
         FUNCTION DoTOPCmd:Integer;
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
         FUNCTION DoUpdateStorage2Cmd:Integer;
         FUNCTION DoPstCalc:Integer;
         FUNCTION DoValVarCmd:Integer;
         FUNCTION DoLambdaCalcs:Integer;
         FUNCTION DoVarCmd:Integer;
         FUNCTION DoNodeListCmd:Integer;
         FUNCTION DoRemoveCmd:Integer;

         function DoFNCSPubCmd:Integer;

         PROCEDURE DoSetNormal(pctNormal:Double);


         PROCEDURE Set_Time;

         PROCEDURE ParseObjName(const fullname:String; VAR objname, propname:String);

         PROCEDURE GetObjClassAndName(VAR ObjClass,ObjName:String);

         FUNCTION AddObject(const ObjType, name:String):Integer;
         FUNCTION EditObject(const ObjType, name:String):Integer;

         PROCEDURE SetActiveCircuit(const cktname:String);

         FUNCTION SetActiveCktElement:Integer;

         FUNCTION DoPropertyDump:Integer;



implementation

USES Command, ArrayDef, ParserDel, SysUtils, DSSClassDefs, DSSGlobals,
     Circuit, Monitor, {ShowResults, ExportResults,}
     DSSClass, DSSObject, Utilities, Solution,
     EnergyMeter, Generator, LoadShape, Load, PCElement,   CktElement,
     uComplex,  mathutil,  Bus,  SolutionAlgs,
     {$IFNDEF FPC}DSSForms,DssPlot,{$ELSE}CmdForms,{$ENDIF} ExecCommands, Executive,
     Dynamics, Capacitor, Reactor, Line, Lineunits, Math,
     Classes,  CktElementClass, Sensor,  ExportCIMXML, NamedObject,
     {$IFNDEF FPC}RegularExpressionsCore,{$ELSE}RegExpr,{$ENDIF} PstCalc,
     PDELement, ReduceAlgs{$IFDEF FPC}, Fncs{$ENDIF}, Ucmatrix;

Var
   SaveCommands, DistributeCommands,  DI_PlotCommands,
   ReconductorCommands, RephaseCommands, AddMarkerCommands,
   SetBusXYCommands, PstCalcCommands, RemoveCommands, FNCSPubCommands :TCommandList;



//----------------------------------------------------------------------------
PROCEDURE GetObjClassAndName(VAR ObjClass,ObjName:String);
VAR
   ParamName:String;
   Param:String;

Begin

{
  We're looking for Object Definition:

      ParamName = 'object' IF given
     and the name of the object

     Object=Capacitor.C1
    or just Capacitor.C1

  If no dot, last class is assumed
}
      ObjClass := '';
      ObjName := '';
      ParamName := LowerCase(Parser[ActiveActor].NextParam);
      Param := Parser[ActiveActor].StrValue;
      IF Length(ParamName)>0 THEN  Begin   // IF specified, must be object or an abbreviation
        IF ComparetextShortest(ParamName, 'object')<>0 THEN  Begin
          DoSimpleMsg('object=Class.Name expected as first parameter in command.'+ CRLF + parser[ActiveActor].CmdString, 240);
          Exit;
        End;
      End;

      ParseObjectClassandName(Param, ObjClass, ObjName);     // see DSSGlobals

End;


//----------------------------------------------------------------------------
FUNCTION DoNewCmd:Integer;

// Process the New Command
// new type=xxxx name=xxxx  editstring

// IF the device being added already exists, the default behavior is to
// treat the New command as an Edit command.  This may be overridden
// by setting the DuplicatesAllowed VARiable to true, in which CASE,
// the New command always results in a new device being added.

VAR
   ObjClass, ObjName:String;
   handle:Integer;

Begin

     Result := 0;
     Handle := 0;

     GetObjClassAndName(ObjClass, ObjName);

     IF CompareText(ObjClass,'solution') = 0
     THEN Begin
         DoSimpleMsg('You cannot create new Solution objects through the command interface.', 241);
         Exit;
     End;

     IF   CompareText(ObjClass,'circuit') = 0
     THEN Begin
            MakeNewCircuit(ObjName);  // Make a new circuit
            ClearEventLog;      // Start the event log in the current directory
            ClearErrorLog;
          End
     ELSE    // Everything else must be a circuit element or DSS Object
        Begin
          Handle := AddObject(ObjClass, ObjName);
        End;

     IF Handle=0 THEN Result := 1;
     
End;

//----------------------------------------------------------------------------
FUNCTION DoEditCmd:Integer;

// edit type=xxxx name=xxxx  editstring
VAR
   ObjType, ObjName:String;

Begin

     Result := 0;

     GetObjClassAndName(ObjType, ObjName);

     IF CompareText(ObjType, 'circuit')=0 THEN
     Begin
                 // Do nothing
     End
     ELSE
     Begin

        // Everything ELSE must be a circuit element
        Result := EditObject(ObjType, ObjName);

     End;

End;

//----------------------------------------------------------------------------
FUNCTION DoBatchEditCmd:Integer;
// batchedit type=xxxx name=pattern  editstring
{$IFDEF FPC}
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
  End ELSE Begin

    LastClassReferenced[ActiveActor] := ClassNames[ActiveActor].Find(ObjType);

    CASE LastClassReferenced[ActiveActor] of
      0: Begin
        DoSimpleMsg('BatchEdit Command: Object Type "' + ObjType + '" not found.'+ CRLF + parser[ActiveActor].CmdString, 267);
        Exit;
        End;{Error}
    ELSE
      Params:=Parser[ActiveActor].Position;
      ActiveDSSClass[ActiveActor] := DSSClassList[ActiveActor].Get(LastClassReferenced[ActiveActor]);
      RegEx1:=TRegExpr.Create;
//      RegEx1.Options:=[preCaseLess];
      RegEx1.Expression:=UTF8String(Pattern);
      If ActiveDSSClass[ActiveActor].First>0 then pObj:=ActiveDSSObject[ActiveActor] else pObj := Nil;
      while pObj <> Nil do begin
        if RegEx1.Exec(UTF8String(pObj.Name)) then begin
          Parser[ActiveActor].Position:=Params;
          ActiveDSSClass[ActiveActor].Edit(ActiveActor);
        end;
        If ActiveDSSClass[ActiveActor].Next>0 then pObj:=ActiveDSSObject[ActiveActor] else pObj := Nil;
      end;
      RegEx1.Free;
    End;
  End;
End;
{$ELSE}
VAR
   ObjType, Pattern:String;
   RegEx1: TPerlRegEx;
   pObj: TDSSObject;
   Params: Integer;
   iElement: Integer;
Begin
  Result := 0;
  GetObjClassAndName(ObjType, Pattern);
  IF CompareText(ObjType, 'circuit')=0 THEN Begin
    // Do nothing
  End ELSE Begin

    LastClassReferenced[ActiveActor] := ClassNames[ActiveActor].Find(ObjType);

    CASE LastClassReferenced[ActiveActor] of
      0: Begin
        DoSimpleMsg('BatchEdit Command: Object Type "' + ObjType + '" not found.'+ CRLF + parser[ActiveActor].CmdString, 267);
        Exit;
        End;{Error}
    ELSE
      Params:=Parser[ActiveActor].Position;
      ActiveDSSClass[ActiveActor] := DSSClassList[ActiveActor].Get(LastClassReferenced[ActiveActor]);
      RegEx1:=TPerlRegEx.Create;
      RegEx1.Options:=[preCaseLess];
      RegEx1.RegEx:=Pattern; // AnsiString(Pattern);
      If ActiveDSSClass[ActiveActor].First>0 then pObj:=ActiveDSSObject[ActiveActor] else pObj := Nil;
      while pObj <> Nil do begin
        RegEx1.Subject:= pObj.Name; //(pObj.Name);
        if RegEx1.Match then begin
          Parser[ActiveActor].Position:=Params;
          ActiveDSSClass[ActiveActor].Edit(ActiveActor);
        end;
        If ActiveDSSClass[ActiveActor].Next>0 then pObj:=ActiveDSSObject[ActiveActor] else pObj := Nil;
      end;
    End;
  End;
End;
{$ENDIF}

//----------------------------------------------------------------------------
FUNCTION DoRedirect(IsCompile:Boolean):Integer;

//  This routine should be recursive
//  So you can redirect input an arbitrary number of times

// If Compile, makes directory of the file the new home directory
// If not Compile (is simple redirect), return to where we started

VAR
    Fin:TextFile;
    ParamName,  InputLine, CurrDir, SaveDir : String;
    LocalCompFileName  : String;
    InBlockComment : Boolean;

Begin
    Result := 0;
    InBlockComment := FALSE;  // Discareded off stack upon return
    // Therefore extent of block comment does not extend beyond a file
    // Going back up the redirect stack

    // Get next parm and try to interpret as a file name
    ParamName := Parser[ActiveActor].NextParam;
    ReDirFile := ExpandFileName(Parser[ActiveActor].StrValue);

    IF ReDirFile <> '' THEN
    Begin

      SaveDir :=  GetCurrentDir;

      TRY
          AssignFile(Fin, ReDirFile);
          Reset(Fin);
          If IsCompile Then Begin
             LastFileCompiled := ReDirFile;
             LocalCompFileName:= ReDirFile;
          End;
      EXCEPT

         // Couldn't find file  Try appending a '.dss' to the file name
         // If it doesn't already have an extension

         IF   Pos('.', ReDirFile)=0
         THEN Begin
            ReDirFile := ReDirFile + '.dss';
            TRY
                AssignFile(Fin, ReDirFile);
                Reset(Fin);
            EXCEPT
                DoSimpleMsg('Redirect File: "' + ReDirFile + '" Not Found.', 242);
                SolutionAbort := TRUE;
                Exit;
            End;
         End
         ELSE Begin
               DoSimpleMsg('Redirect File: "'+ReDirFile+'" Not Found.', 243);
               SolutionAbort := True;
               Exit;  // Already had an extension, so just Bail
         End;

      END;

    // OK, we finally got one open, so we're going to continue
       TRY
          TRY
             // Change Directory to path specified by file in CASE that
             // loads in more files
             CurrDir := ExtractFileDir(ReDirFile);
             SetCurrentDir(CurrDir);
             If  IsCompile Then   SetDataPath(CurrDir);  // change datadirectory

             Redirect_Abort := False;
             In_Redirect    := True;

             WHILE Not ( (EOF(Fin)) or (Redirect_Abort) ) DO
               Begin
                  Readln(Fin, InputLine);
                  if Length(InputLine) > 0 then
                  BEGIN
                      if Not InBlockComment then     // look for '/*'  at baginning of line
                        case InputLine[1] of
                           '/': if (Length(InputLine) > 1) and (InputLine[2]='*')then
                                InBlockComment := TRUE;
                        end;

                      If Not InBlockComment Then   // process the command line
                        If Not SolutionAbort Then ProcessCommand(InputLine)
                                             Else Redirect_Abort := True;  // Abort file if solution was aborted

                      // in block comment ... look for */   and cancel block comment (whole line)
                      if InBlockComment then
                        if Pos('*/', Inputline)>0 then
                                InBlockComment := FALSE;
                  END;
               End;

             IF ActiveCircuit[ActiveActor] <> Nil THEN ActiveCircuit[ActiveActor].CurrentDirectory := CurrDir +'\';

          EXCEPT
             On E: Exception DO
                DoErrorMsg('DoRedirect'+CRLF+'Error Processing Input Stream in Compile/Redirect.',
                            E.Message,
                            'Error in File: "' + ReDirFile + '" or Filename itself.', 244);
          END;
      FINALLY
          CloseFile(Fin);
          In_Redirect := False;
          ParserVars.Add('@lastfile', ReDirFile) ;

          If  IsCompile Then   Begin
            SetDataPath(CurrDir); // change datadirectory
            LastCommandWasCompile := True;
            ParserVars.Add('@lastcompilefile', LocalCompFileName); // will be last one off the stack
          End
          Else Begin
              SetCurrentDir(SaveDir);    // set back to where we were for redirect, but not compile
              ParserVars.Add('@lastredirectfile', ReDirFile);
          End;
      END;

    End;  // ELSE ignore altogether IF null filename


End;

//----------------------------------------------------------------------------
FUNCTION DoSelectCmd:Integer;

// select active object
// select element=elementname terminal=terminalnumber
VAR
   ObjClass, ObjName,
   ParamName, Param:String;

Begin

     Result := 1;

     GetObjClassAndName(ObjClass, ObjName);  // Parse Object class and name

     If (Length(ObjClass)=0) and (Length(ObjName)=0) Then Exit;  // select active obj if any

     IF CompareText(ObjClass, 'circuit')=0 THEN
     Begin
           SetActiveCircuit(ObjName);
     End
     ELSE
     Begin

        // Everything else must be a circuit element
        IF Length(ObjClass)>0 THEN SetObjectClass(ObjClass);

        ActiveDSSClass[ActiveActor] := DSSClassList[ActiveActor].Get(LastClassReferenced[ActiveActor]);
        IF ActiveDSSClass[ActiveActor]<>Nil THEN
        Begin
          IF Not ActiveDSSClass[ActiveActor].SetActive(Objname) THEN
          Begin // scroll through list of objects untill a match
            DoSimpleMsg('Error! Object "' + ObjName + '" not found.'+ CRLF + parser[ActiveActor].CmdString, 245);
            Result := 0;
          End
          ELSE
          WITH ActiveCircuit[ActiveActor] Do
          Begin
             CASE ActiveDSSObject[ActiveActor].DSSObjType OF
                  DSS_OBJECT: ;  // do nothing for general DSS object

             ELSE Begin   // for circuit types, set ActiveCircuit[ActiveActor] Element, too
                   ActiveCktElement := ActiveDSSClass[ActiveActor].GetActiveObj;
                   // Now check for active terminal designation
                   ParamName := LowerCase(Parser[ActiveActor].NextParam);
                   Param := Parser[ActiveActor].StrValue;
                   If Length(Param)>0
                   THEN ActiveCktElement.ActiveTerminalIdx := Parser[ActiveActor].Intvalue
                   ELSE ActiveCktElement.ActiveTerminalIdx := 1;  {default to 1}
                   With ActiveCktElement Do SetActiveBus(StripExtension(Getbus(ActiveTerminalIdx)));
                  End;
             End;
          End;
        End
        ELSE Begin
          DoSimpleMsg('Error! Active object type/class is not set.', 246);
          Result := 0;
        End;

     End;

End;

//----------------------------------------------------------------------------
FUNCTION DoMoreCmd:Integer;

// more editstring  (assumes active circuit element)
Begin
      IF ActiveDSSClass[ActiveActor]<>nil THEN Result := ActiveDSSClass[ActiveActor].Edit(ActiveActor)
                             ELSE Result := 0;
End;


//----------------------------------------------------------------------------
FUNCTION DoSaveCmd:Integer;

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
     SaveDir := '';
     SaveFile := '';
     ParamPointer := 0;
     ParamName := Parser[ActiveActor].NextParam;
     Param := Parser[ActiveActor].StrValue;
     WHILE Length(Param)>0 DO
     Begin
         IF   (Length(ParamName) = 0)  THEN Inc(ParamPointer)
         ELSE ParamPointer := SaveCommands.GetCommand(ParamName);

         CASE ParamPointer OF
           1: ObjClass := Parser[ActiveActor].StrValue;
           2: Savefile := Parser[ActiveActor].StrValue;   // File name for saving  a class
           3: SaveDir := Parser[ActiveActor].StrValue;
         ELSE

         End;

         ParamName := Parser[ActiveActor].NextParam;
         Param := Parser[ActiveActor].StrValue;
     End;

   InShowResults := True;
   If (Length(ObjClass)=0) or (CompareTextShortest( ObjClass, 'meters')=0 ) then Begin
   // Save monitors and Meters

     WITH ActiveCircuit[ActiveActor].Monitors Do
     FOR i := 1 to ListSize Do
     Begin
         pMon := Get(i);
         pMon.Save;
     End;

     WITH ActiveCircuit[ActiveActor].EnergyMeters Do
     FOR i := 1 to ListSize Do
     Begin
         pMtr := Get(i);
         pMtr.SaveRegisters(ActiveActor);
     End;

     Exit;
   End;
   If CompareTextShortest( ObjClass, 'circuit')=0 then  Begin
      IF not ActiveCircuit[ActiveActor].Save(SaveDir) Then Result := 1;
      Exit;
   End;
   If CompareTextShortest( ObjClass, 'voltages')=0 then  Begin
      ActiveCircuit[ActiveActor].Solution.SaveVoltages;
      Exit;
   End;

   {Assume that we have a class name for a DSS Class}
   DSSClass :=  GetDSSClassPtr(ObjClass);
   If DSSClass <> Nil Then Begin
     IF Length(SaveFile)=0 Then SaveFile := objClass;
     IF Length(SaveDir)>0 Then begin
       If not DirectoryExists(SaveDir) Then
          Try
             mkDir(SaveDir);
          Except
             On E:Exception Do DoSimpleMsg('Error making Directory: "'+SaveDir+'". ' + E.Message, 247);
          End;
       SaveFile := SaveDir+'\'+SaveFile;
     End;
     WriteClassFile(DSSClass, SaveFile, FALSE); // just write the class with no checks
   End;

   SetLastResultFile( SaveFile);
   GlobalResult := SaveFile;

End;


//----------------------------------------------------------------------------
FUNCTION DoClearCmd:Integer;

Begin

      DSSExecutive[ActiveActor].Clear;

      Result := 0;

End;
//----------------------------------------------------------------------------
FUNCTION DoClearAllCmd:Integer;

Begin

      DSSExecutive[ActiveActor].ClearAll;

      Result := 0;

End;
//----------------------------------------------------------------------------
FUNCTION DoHelpCmd:Integer;
Begin
    ShowHelpForm; // DSSForms Unit
    Result := 0;
End;


//----------------------------------------------------------------------------
FUNCTION DoSampleCmd(ActorID : Integer):Integer;

// FORce all monitors and meters in active circuit to take a sample


Begin

   MonitorClass[ActorID].SampleAll(ActorID);

   EnergyMeterClass[ActorID].SampleAll(ActorID);  // gets generators too



   Result := 0;

End;


//----------------------------------------------------------------------------
FUNCTION DoSolveCmd:Integer;
Begin
   // just invoke solution obj's editor to pick up parsing and execute rest of command
   ActiveSolutionObj := ActiveCircuit[ActiveActor].Solution;
   Result := SolutionClass[ActiveActor].Edit(ActiveActor);
End;


//----------------------------------------------------------------------------
FUNCTION SetActiveCktElement:Integer;

// Parses the object off the line and sets it active as a circuitelement.

VAR
   ObjType, ObjName:String;

Begin

     Result := 0;

     GetObjClassAndName(ObjType, ObjName);

     IF CompareText(ObjType, 'circuit')=0 THEN
     Begin
                 // Do nothing
     End
     ELSE
     Begin

        IF CompareText(ObjType, ActiveDSSClass[ActiveActor].Name)<>0 THEN
             LastClassReferenced[ActiveActor] := ClassNames[ActiveActor].Find(ObjType);

        CASE LastClassReferenced[ActiveActor] of
          0: Begin
                 DoSimpleMsg('Object Type "' + ObjType + '" not found.'+ CRLF + parser[ActiveActor].CmdString, 253);
                 Result := 0;
                 Exit;
             End;{Error}
        ELSE

        // intrinsic and user Defined models
           ActiveDSSClass[ActiveActor] := DSSClassList[ActiveActor].Get(LastClassReferenced[ActiveActor]);
           IF ActiveDSSClass[ActiveActor].SetActive(ObjName) THEN
           WITH ActiveCircuit[ActiveActor] Do
           Begin // scroll through list of objects until a match
             CASE ActiveDSSObject[ActiveActor].DSSObjType OF
                    DSS_OBJECT: DoSimpleMsg('Error in SetActiveCktElement: Object not a circuit Element.'+ CRLF + parser[ActiveActor].CmdString, 254);
             ELSE Begin
                    ActiveCktElement := ActiveDSSClass[ActiveActor].GetActiveObj;
                    Result:=1;
                  End;
             End;
           End;
        End;
     End;
End;


//----------------------------------------------------------------------------
FUNCTION DoEnableCmd:Integer;

Var Objtype, ObjName:String;
    ClassPtr:TDSSClass;
    CktElem:TDSSCktElement;
    i:Integer;


Begin

  //   Result := SetActiveCktElement;
  //  IF Result>0 THEN ActiveCircuit[ActiveActor].ActiveCktElement.Enabled := True;

     Result := 0;

     GetObjClassAndName(ObjType, ObjName);

     IF CompareText(ObjType, 'circuit')=0 THEN
     Begin
                 // Do nothing
     End
     ELSE
     If Length(ObjType)>0 Then Begin
      // only applies to CktElementClass objects
       ClassPtr := GetDSSClassPtr(ObjType);
       If ClassPtr<> Nil Then Begin

         If (ClassPtr.DSSClassType and BASECLASSMASK) > 0  then Begin
              // Everything else must be a circuit element
             If CompareText(ObjName,'*') = 0 Then Begin
               // Enable all elements of this class
               For i := 1 to ClassPtr.ElementCount Do Begin
                 CktElem := ClassPtr.ElementList.Get(i);
                 CktElem.Enabled := TRUE;
               End;

             End
             Else Begin

              // just load up the parser and call the edit routine for the object in question

              Parser[ActiveActor].CmdString := 'Enabled=true';  // Will only work for CktElements
              Result := EditObject(ObjType, ObjName);
             End;
         End;
       End;
     End;

End;

//----------------------------------------------------------------------------
FUNCTION DoDisableCmd:Integer;

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
       ClassPtr := GetDSSClassPtr(ObjType);
       If ClassPtr<> Nil Then Begin

         If (ClassPtr.DSSClassType and BASECLASSMASK) > 0  then Begin
              // Everything else must be a circuit element
             If CompareText(ObjName,'*') = 0 Then Begin
               // Disable all elements of this class
               For i := 1 to ClassPtr.ElementCount Do Begin
                 CktElem := ClassPtr.ElementList.Get(i);
                 CktElem.Enabled := FALSE;
               End;

             End
             Else Begin

              // just load up the parser and call the edit routine for the object in question

              Parser[ActiveActor].CmdString := 'Enabled=false';  // Will only work for CktElements
              Result := EditObject(ObjType, ObjName);
             End;
         End;
       End;
     End;

//     Result := SetActiveCktElement;
//     IF Result>0 THEN ActiveCircuit[ActiveActor].ActiveCktElement.Enabled := False;
End;

//----------------------------------------------------------------------------
FUNCTION DoPropertyDump:Integer;

VAR
   pObject:TDSSObject;
   F:TextFile;
   SingleObject, Debugdump, IsSolution:Boolean;
   i:Integer;
   FileName:String;
   ParamName:String;
   Param, Param2, ObjClass, ObjName:String;

Begin

 Result := 0;
 SingleObject := False;
 IsSolution := False;
 DebugDump := False;
 ObjClass := ' ';  // make sure these have at least one character
 ObjName := ' ';
 
 // Continue parsing command line - check for object name
 ParamName := Parser[ActiveActor].NextParam;
 Param := Parser[ActiveActor].StrValue;
 IF Length(Param)>0 THEN
 Begin

    IF CompareText(Param, 'commands')=0 THEN
    If Not NoFormsAllowed Then Begin
        DumpAllDSSCommands(FileName);
        FireOffEditor(FileName);
        Exit;
    End;

    {dump bus names hash list}
    if CompareText(Param, 'buslist')=0 then
    If Not NoFormsAllowed Then Begin
        FileName := GetOutputDirectory +  'Bus_Hash_List.Txt';
        ActiveCircuit[ActiveActor].BusList.DumpToFile(FileName);
        FireOffEditor(FileName);
        Exit;
    End;

    {dump device names hash list}
    if CompareText(Param, 'devicelist')=0 then
    If Not NoFormsAllowed Then Begin
        FileName := GetOutputDirectory +  'Device_Hash_List.Txt';
        ActiveCircuit[ActiveActor].DeviceList.DumpToFile(FileName);
        FireOffEditor(FileName);
        Exit;
    End;

    IF CompareText(Copy(lowercase(Param),1,5), 'alloc')=0 THEN
    Begin
        FileName :=GetOutputDirectory + 'AllocationFactors.Txt';
        DumpAllocationFactors(FileName);
        FireOffEditor(FileName);
        Exit;
    End;

    IF CompareText(Param,'debug')=0 THEN
       DebugDump := TRUE
    ELSE
    Begin

       IF CompareText(Param,'solution')=0 THEN
         Begin
          // Assume active circuit solution IF not qualified
          ActiveDSSClass[ActiveActor] := SolutionClass[ActiveActor];
          ActiveDSSObject[ActiveActor] := ActiveCircuit[ActiveActor].Solution;
          IsSolution := TRUE;
         End
       ELSE
         Begin
            SingleObject := TRUE;
           // Check to see IF we want a debugdump on this object
            ParamName := Parser[ActiveActor].NextParam;
            Param2 := Parser[ActiveActor].StrValue;
            IF CompareText(Param2,'debug')=0 THEN DebugDump := TRUE;
            // Set active Element to be value in Param
            Parser[ActiveActor].CmdString := '"' + Param + '"';  // put param back into parser
            GetObjClassAndName( ObjClass, ObjName);
            // IF DoSelectCmd=0 THEN Exit;  8-17-00
            IF SetObjectClass(ObjClass)
            THEN Begin
              ActiveDSSClass[ActiveActor] := DSSClassList[ActiveActor].Get(LastClassReferenced[ActiveActor]);
              IF ActiveDSSClass[ActiveActor] = NIL Then Exit;
            End
            ELSE Exit;
         End;
    End;
 End;

  TRY
      AssignFile(F, GetOutputDirectory + CircuitName_[ActiveActor] + 'PropertyDump.Txt');
      Rewrite(F);
  EXCEPT
      On E:Exception DO
      Begin
        DoErrorMsg('DoPropertyDump - opening '+ GetOutputDirectory +' DSS_PropertyDump.txt for writing in '+Getcurrentdir, E.Message, 'Disk protected or other file error', 255);
        Exit;
      End;
  End;


  TRY

      IF SingleObject THEN
      Begin

        {IF ObjName='*' then we dump all objects of this class}
        CASE ObjName[1] of
           '*':Begin
                  FOR i := 1 to ActiveDSSClass[ActiveActor].ElementCount Do
                  Begin
                      ActiveDSSClass[ActiveActor].Active := i;
                      ActiveDSSObject[ActiveActor].DumpProperties(F, DebugDump);
                  End;
               End;
        ELSE
           IF Not ActiveDSSClass[ActiveActor].SetActive(Objname)
           THEN Begin
               DoSimpleMsg('Error! Object "' + ObjName + '" not found.', 256) ;
               Exit;
           End
           ELSE ActiveDSSObject[ActiveActor].DumpProperties(F, DebugDump);  // Dump only properties of active circuit element
        END;

      End
      ELSE IF IsSolution THEN  Begin
         ActiveDSSObject[ActiveActor].DumpProperties(F, DebugDump);
      End
      ELSE Begin

        // Dump general Circuit stuff

        IF DebugDump THEN ActiveCircuit[ActiveActor].DebugDump(F);
        // Dump circuit objects
        TRY
          pObject := ActiveCircuit[ActiveActor].CktElements.First;
          WHILE pObject <> Nil DO
          Begin
              pObject.DumpProperties(F, DebugDump);
              pObject := ActiveCircuit[ActiveActor].CktElements.Next;
          End;
          pObject := DSSObjs[ActiveActor].First;
          WHILE pObject <> Nil DO
          Begin
              pObject.DumpProperties(F, DebugDump);
              pObject := DSSObjs[ActiveActor].Next;
          End;
        EXCEPT
            On E:Exception DO
              DoErrorMsg('DoPropertyDump - Problem writing file.', E.Message, 'File may be read only, in use, or disk full?', 257);
        End;

        ActiveCircuit[ActiveActor].Solution.DumpProperties(F,DebugDump);
      End;

  FINALLY

         CloseFile(F);
  END;  {TRY}

  FireOffEditor(GetOutputDirectory + CircuitName_[ActiveActor] + 'PropertyDump.Txt');

End;



//----------------------------------------------------------------------------
PROCEDURE Set_Time;

// for interpreting time specified as an array "hour, sec"
VAR

   TimeArray:Array[1..2] of double;

Begin
     Parser[ActiveActor].ParseAsVector(2, @TimeArray);
     WITH ActiveCircuit[ActiveActor].Solution DO
     Begin
        DynaVars.intHour := Round(TimeArray[1]);
        DynaVars.t := TimeArray[2];
        Update_dblHour;
     End;
End;

//----------------------------------------------------------------------------
PROCEDURE SetActiveCircuit(const cktname:String);

VAR
   pCkt:TDSSCircuit;
Begin

   pCkt := Circuits.First;
   WHILE pCkt<>nil DO
   Begin
       IF CompareText(pCkt.Name, cktname)=0 THEN
       Begin
           ActiveCircuit[ActiveActor] := pCkt;
           Exit;
       End;
       pCkt := Circuits.Next;
   End;

   // IF none is found, just leave as is after giving error

   DoSimpleMsg('Error! No circuit named "' + cktname + '" found.' + CRLF +
               'Active circuit not changed.', 258);
End;

{-------------------------------------------}
PROCEDURE DoLegalVoltageBases;

VAR
   Dummy :pDoubleArray;
   i,
   Num   :Integer;

Begin

     Dummy := AllocMem(Sizeof(Double) * 200); // Big Buffer
     Num   := Parser[ActiveActor].ParseAsVector(200, Dummy);
     {Parsing zero-fills the array}

     {LegalVoltageBases is a zero-terminated array, so we have to allocate
      one more than the number of actual values}

     WITH ActiveCircuit[ActiveActor] Do
     Begin
       Reallocmem(LegalVoltageBases, Sizeof(LegalVoltageBases^[1])*(Num+1));
       FOR i := 1 to Num+1 Do LegalVoltageBases^[i] := Dummy^[i];
     End;

     Reallocmem(Dummy, 0);
End;



//----------------------------------------------------------------------------
FUNCTION DoOpenCmd:Integer;
// Opens a terminal and conductor of a ckt Element
VAR
   retval    :Integer;
   Terminal  :Integer;
   Conductor :Integer;
   ParamName :string;

// syntax:  "Open class.name term=xx cond=xx"
//  IF cond is omitted, all conductors are opened.

Begin
  retval := SetActiveCktElement;
  IF retval>0 THEN
  Begin
        ParamName := Parser[ActiveActor].NextParam;
        Terminal  := Parser[ActiveActor].IntValue;
        ParamName := Parser[ActiveActor].NextParam;
        Conductor := Parser[ActiveActor].IntValue;

        With ActiveCircuit[ActiveActor] Do
        Begin
              ActiveCktElement.ActiveTerminalIdx := Terminal;
              ActiveCktElement.Closed[Conductor,ActiveActor] := FALSE;
              With ActiveCktElement Do SetActiveBus(StripExtension(Getbus(ActiveTerminalIdx)));
        End;
  End
  ELSE
  Begin
       DoSimpleMsg('Error in Open Command: Circuit Element Not Found.' +CRLF+ Parser[ActiveActor].CmdString, 259);
  End;
  Result := 0;
End;



//----------------------------------------------------------------------------
FUNCTION DoCloseCmd:Integer;
// Closes a terminal and conductor of a ckt Element
VAR
   retval:Integer;
   Terminal:Integer;
   Conductor:Integer;
   ParamName : string;

// syntax:  "Close class.name term=xx cond=xx"
//  IF cond is omitted, all conductors are opened

Begin
  retval := SetActiveCktElement;
  IF retval>0 THEN
    Begin
       ParamName := Parser[ActiveActor].NextParam;
       Terminal  := Parser[ActiveActor].IntValue;
       ParamName := Parser[ActiveActor].NextParam;
       Conductor := Parser[ActiveActor].IntValue;

        With ActiveCircuit[ActiveActor] Do
         Begin
          ActiveCktElement.ActiveTerminalIdx := Terminal;
          ActiveCktElement.Closed[Conductor,ActiveActor] := TRUE;
          With ActiveCktElement Do SetActiveBus(StripExtension(Getbus(ActiveTerminalIdx)));
         End;

    End
  ELSE
  Begin
       DoSimpleMsg('Error in Close Command: Circuit Element Not Found.' +CRLF+ Parser[ActiveActor].CmdString, 260);
  End;
  Result := 0;

End;

//----------------------------------------------------------------------------
FUNCTION DoResetCmd(ActorID : Integer):Integer;
VAR
    ParamName, Param  :String;

Begin
    Result := 0;

    // Get next parm and try to interpret as a file name
    ParamName := Parser[ActorID].NextParam;
    Param := UpperCase(Parser[ActorID].StrValue);
    IF Length(Param) = 0
       THEN Begin
            DoResetMonitors(ACtorID);
            DoResetMeters(ActorID);
            DoResetFaults ;
            DoResetControls;
            ClearEventLog;
            ClearErrorLog;
            DoResetKeepList;
       End
    ELSE
      CASE Param[1] of
       'M': CASE Param[2] of
               'O'{MOnitor}:  DoResetMonitors(ActiveActor);
               'E'{MEter}:    DoResetMeters(ActiveActor);
            END;
       'F'{Faults}:   DoResetFaults;
       'C'{Controls}: DoResetControls;
       'E'{EventLog and ErrorLog}: Begin ClearEventLog;  ClearErrorLog; End;
       'K': DoResetKeepList;

      ELSE

         DoSimpleMsg('Unknown argument to Reset Command: "'+ Param+'"', 261);

      End;

End;

procedure MarkCapandReactorBuses;
Var
    pClass:TDSSClass;
    pCapElement:TCapacitorObj;
    pReacElement:TReactorObj;
    ObjRef:Integer;

begin
{Mark all buses as keepers if there are capacitors or reactors on them}
    pClass :=  GetDSSClassPtr('capacitor');
    If pClass<>Nil then
    Begin
       ObjRef := pClass.First;
       While Objref>0 Do
       Begin
          pCapElement := TCapacitorObj(ActiveDSSObject[ActiveActor]);
          If pCapElement.IsShunt Then
          Begin
             If pCapElement.Enabled Then  ActiveCircuit[ActiveActor].Buses^[pCapElement.Terminals^[1].Busref].Keep := TRUE;
          End;
          ObjRef := pClass.Next;
       End;
    End;

    {Now Get the Reactors}

    pClass :=  GetDSSClassPtr('reactor');
    If pClass<>Nil then
    Begin
       ObjRef := pClass.First;
       While Objref>0 Do
       Begin
          pReacElement := TReactorObj(ActiveDSSObject[ActiveActor]);
          If pReacElement.IsShunt Then
          Try
             If pReacElement.Enabled Then ActiveCircuit[ActiveActor].Buses^[pReacElement.Terminals^[1].Busref].Keep := TRUE;
          Except
             On E:Exception Do Begin
               DoSimpleMsg(Format('%s %s Reactor=%s Bus No.=%d ',[E.Message, CRLF, pReacElement.Name, pReacElement.NodeRef^[1] ]), 9999);
               Break;
             End;
          End;
          ObjRef := pClass.Next;
       End;
    End;
end;

//----------------------------------------------------------------------------
FUNCTION DoReduceCmd:Integer;
VAR
    MetObj:TEnergyMeterObj;
    MeterClass: TEnergyMeter;
    ParamName, Param  :String;
    DevClassIndex:Integer;

Begin
    Result := 0;
    // Get next parm and try to interpret as a file name
    ParamName := Parser[ActiveActor].NextParam;
    Param := UpperCase(Parser[ActiveActor].StrValue);

    {Mark Capacitor and Reactor buses as Keep so we don't lose them}
    MarkCapandReactorBuses;

    IF Length(Param) = 0  Then Param := 'A';
    CASE Param[1] of
     'A': Begin
              metobj := ActiveCircuit[ActiveActor].EnergyMeters.First;
              While metobj <> nil Do
              Begin
                MetObj.ReduceZone(ActiveActor);
                MetObj := ActiveCircuit[ActiveActor].EnergyMeters.Next;
              End;
          End;

    ELSE
       {Reduce a specific meter}
       DevClassIndex := ClassNames[ActiveActor].Find('energymeter');
       IF DevClassIndex > 0 THEN
       Begin
          MeterClass := DSSClassList[ActiveActor].Get(DevClassIndex);
          If MeterClass.SetActive (Param) Then   // Try to set it active
          Begin
            MetObj := MeterClass.GetActiveObj;
            MetObj.ReduceZone(ActiveActor);
          End
          Else DoSimpleMsg('EnergyMeter "'+Param+'" not found.', 262);
       End;
    End;

End;

//----------------------------------------------------------------------------
FUNCTION DoResetMonitors(ActorID : Integer):Integer;
VAR
   pMon:TMonitorObj;

Begin

     WITH ActiveCircuit[ActorID] DO
     Begin

        pMon := Monitors.First;
        WHILE pMon<>nil DO
        Begin
            pMon.ResetIt(ActorID);
            pMon := Monitors.Next;
        End;
        Result :=0;

     End;

End;

//----------------------------------------------------------------------------
FUNCTION DoFileEditCmd:Integer;

VAR
    ParamName, Param  :String;

Begin
    Result := 0;

    // Get next parm and try to interpret as a file name
    ParamName := Parser[ActiveActor].NextParam;
    Param := Parser[ActiveActor].StrValue;

    IF  FileExists(Param) THEN FireOffEditor(Param)
    ELSE Begin
       GlobalResult := 'File "'+param+'" does not exist.';
       Result := 1;
    End;
End;

//----------------------------------------------------------------------------
PROCEDURE ParseObjName(const fullname:String; VAR objname, propname:String);

{ Parse strings such as

    1. Classname.Objectname,Property    (full name)
    2. Objectname.Property   (classname omitted)
    3. Property           (classname and objectname omitted
}

VAR
  DotPos1, DotPos2:Integer;

Begin
     DotPos1 := Pos('.',fullname);
     CASE Dotpos1 of

        0: Begin
               Objname  := '';
               PropName := FullName;
           End;

       ELSE Begin

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

FUNCTION DoQueryCmd:Integer;
{ ? Command }
{ Syntax:  ? Line.Line1.R1}
VAR
   ParamName:String;
   Param, ObjName, PropName:String;
   PropIndex:Integer;


Begin

     Result := 0;
     ParamName := Parser[ActiveActor].NextParam;
     Param := Parser[ActiveActor].StrValue;

     ParseObjName(Param, ObjName, PropName);

     IF CompareText(ObjName,'solution')=0 THEN
     Begin  // special for solution
         ActiveDSSClass[ActiveActor]  := SolutionClass[ActiveActor];
         ActiveDSSObject[ActiveActor] := ActiveCircuit[ActiveActor].Solution;
     End ELSE
     Begin
         // Set Object Active
         parser[ActiveActor].cmdstring := '"' + Objname + '"';
         DoSelectCmd;
     End;

     // Put property value in global VARiable
     PropIndex := ActiveDSSClass[ActiveActor].Propertyindex(PropName);
     IF PropIndex>0 THEN
        GlobalPropertyValue := ActiveDSSObject[ActiveActor].GetPropertyValue(PropIndex)
     ELSE
        GlobalPropertyValue := 'Property Unknown';

     GlobalResult := GlobalPropertyValue;

     If LogQueries Then WriteQueryLogFile(param, GlobalResult); // write time-stamped query

End;

//----------------------------------------------------------------------------
FUNCTION DoResetMeters(ActorID : Integer):Integer;

Begin
     Result := 0;
     EnergyMeterClass[ActorID].ResetAll(ActorID)
End;


//----------------------------------------------------------------------------
FUNCTION DoNextCmd:Integer;
VAR
    ParamName, Param  :String;

Begin
    Result := 0;

    // Get next parm and try to interpret as a file name
    ParamName := Parser[ActiveActor].NextParam;
    Param := Parser[ActiveActor].StrValue;

    With ActiveCircuit[ActiveActor].Solution Do
    CASE UpCase(Param[1]) of

       'Y'{Year}:  Year := Year + 1;
       'H'{Hour}:  Inc(DynaVars.intHour);
       'T'{Time}:  Increment_time;
    ELSE

    END;

End;

//----------------------------------------------------------------------------
PROCEDURE DoAboutBox;

Begin

 If NoFormsAllowed Then Exit;

 ShowAboutBox;


End;

//----------------------------------------------------------------------------
FUNCTION DoSetVoltageBases(ActorID : Integer):integer;


Begin

   Result := 0;

   ActiveCircuit[ActiveActor].Solution.SetVoltageBases(ActorID);

End;
//----------------------------------------------------------------------------
FUNCTION AddObject(const ObjType, Name:String):Integer;


Begin

   Result :=0;

   // Search for class IF not already active
   // IF nothing specified, LastClassReferenced remains
   IF   CompareText(Objtype, ActiveDssClass[ActiveActor].Name) <> 0
   THEN LastClassReferenced[ActiveActor] := ClassNames[ActiveActor].Find(ObjType);

   CASE LastClassReferenced[ActiveActor] of
     0: Begin
            DoSimpleMsg('New Command: Object Type "' + ObjType + '" not found.' + CRLF + parser[ActiveActor].CmdString, 263);
            Result := 0;
            Exit;
        End;{Error}
   ELSE

     // intrinsic and user Defined models
     // Make a new circuit element
        ActiveDSSClass[ActiveActor] := DSSClassList[ActiveActor].Get(LastClassReferenced[ActiveActor]);

      // Name must be supplied
        IF   Length(Name) = 0
        THEN Begin
            DoSimpleMsg('Object Name Missing'+ CRLF + parser[ActiveActor].CmdString, 264);
            Exit;
        End;


   // now let's make a new object or set an existing one active, whatever the case
        CASE  ActiveDSSClass[ActiveActor].DSSClassType Of
            // These can be added WITHout having an active circuit
            // Duplicates not allowed in general DSS objects;
            // If the name is the same, Edit is executed instead of New
             DSS_OBJECT :  IF  NOT  ActiveDSSClass[ActiveActor].SetActive(Name)
                           THEN Begin
                               Result := ActiveDSSClass[ActiveActor].NewObject(Name);
                               DSSObjs[ActiveActor].Add(ActiveDSSObject[ActiveActor]);  // Stick in pointer list to keep track of it
                           End;
        ELSE
            // These are circuit elements
            IF   ActiveActor = 0
            THEN Begin
                 DoSimpleMsg('You Must Create a circuit first: "new circuit.yourcktname"', 265);
                 Exit;
            End;

          // IF Object already exists.  Treat as an Edit IF dulicates not allowed
            IF    ActiveCircuit[ActiveActor].DuplicatesAllowed THEN
             Begin
                 Result := ActiveDSSClass[ActiveActor].NewObject(Name); // Returns index into this class
                 ActiveCircuit[ActiveActor].AddCktElement(Result);   // Adds active object to active circuit
             End
            ELSE
             Begin      // Check to see if we can set it active first
                IF   Not ActiveDSSClass[ActiveActor].SetActive(Name)  THEN
                 Begin
                   Result := ActiveDSSClass[ActiveActor].NewObject(Name);   // Returns index into this class
                   ActiveCircuit[ActiveActor].AddCktElement(Result);   // Adds active object to active circuit
                 End
                ELSE
                 Begin
                    DoSimpleMsg('Warning: Duplicate new element definition: "'+ ActiveDSSClass[ActiveActor].Name+'.'+Name+'"'+
                                 CRLF+ 'Element being redefined.', 266);
                 End;
             End;

        End;

        // ActiveDSSObject now points to the object just added
        // IF a circuit element, ActiveCktElement in ActiveCircuit[ActiveActor] is also set

        If Result>0 Then ActiveDSSObject[ActiveActor].ClassIndex := Result;

        ActiveDSSClass[ActiveActor].Edit(ActiveActor);    // Process remaining instructions on the command line

  End;
End;


//----------------------------------------------------------------------------
FUNCTION EditObject(const ObjType, Name:String):Integer;

Begin

   Result :=0;
   LastClassReferenced[ActiveActor] := ClassNames[ActiveActor].Find(ObjType);

   CASE LastClassReferenced[ActiveActor] of
     0: Begin
            DoSimpleMsg('Edit Command: Object Type "' + ObjType + '" not found.'+ CRLF + parser[ActiveActor].CmdString, 267);
            Result := 0;
            Exit;
        End;{Error}
   ELSE

   // intrinsic and user Defined models
   // Edit the DSS object
      ActiveDSSClass[ActiveActor] := DSSClassList[ActiveActor].Get(LastClassReferenced[ActiveActor]);
      IF ActiveDSSClass[ActiveActor].SetActive(Name) THEN
      Begin
          Result := ActiveDSSClass[ActiveActor].Edit(ActiveActor);   // Edit the active object
      End;
   End;

End;

//----------------------------------------------------------------------------
FUNCTION DoSetkVBase: Integer;

VAR
   ParamName, BusName:String;
   kVValue :Double;

Begin

// Parse off next two items on line
   ParamName := Parser[ActiveActor].NextParam;
   BusName   := LowerCase(Parser[ActiveActor].StrValue);

   ParamName := Parser[ActiveActor].NextParam;
   kVValue   := Parser[ActiveActor].DblValue;

   // Now find the bus and set the value

   WITH ActiveCircuit[ActiveActor] Do
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
           AppendGlobalResult('Bus ' + BusName + ' Not Found.');
      End;
   End;



End;



//----------------------------------------------------------------------------
PROCEDURE DoAutoAddBusList(const S: String);

VAR
   ParmName,
   Param, S2    :String;
   F :Textfile;


begin

     ActiveCircuit[ActiveActor].AutoAddBusList.Clear;

     // Load up auxiliary parser to reparse the array list or file name
     Auxparser[ActiveActor].CmdString := S;
     ParmName := Auxparser[ActiveActor].NextParam ;
     Param := AuxParser[ActiveActor].StrValue;

     {Syntax can be either a list of bus names or a file specification:  File= ...}

     If CompareText(Parmname, 'file') = 0
     THEN Begin
         // load the list from a file

         TRY
             AssignFile(F, Param);
             Reset(F);
             WHILE Not EOF(F) Do
             Begin         // Fixed 7/8/01 to handle all sorts of bus names
                  Readln(F, S2);
                  Auxparser[ActiveActor].CmdString := S2;
                  ParmName := Auxparser[ActiveActor].NextParam ;
                  Param := AuxParser[ActiveActor].StrValue;
                  IF   Length(Param) > 0
                  THEN ActiveCircuit[ActiveActor].AutoAddBusList.Add(Param);
             End;
             CloseFile(F);

         EXCEPT
             On E:Exception Do DoSimpleMsg('Error trying to read bus list file. Error is: '+E.message, 268);
         END;


     End
     ELSE Begin

       // Parse bus names off of array list
       WHILE Length(Param) > 0 Do
       BEGIN
            ActiveCircuit[ActiveActor].AutoAddBusList.Add(Param);
            AuxParser[ActiveActor].NextParam;
            Param := AuxParser[ActiveActor].StrValue;
       END;

     End;

end;

//----------------------------------------------------------------------------
PROCEDURE DoKeeperBusList(Const S:String);


// Created 4/25/03

{Set Keep flag on buses found in list so they aren't eliminated by some reduction
 algorithm.  This command is cumulative. To clear flag, use Reset Keeplist}

VAR
   ParmName,
   Param, S2    :String;
   F :Textfile;
   iBus :Integer;

begin

     // Load up auxiliary parser to reparse the array list or file name
     Auxparser[ActiveActor].CmdString := S;
     ParmName := Auxparser[ActiveActor].NextParam ;
     Param := AuxParser[ActiveActor].StrValue;

     {Syntax can be either a list of bus names or a file specification:  File= ...}

     If CompareText(Parmname, 'file') = 0  THEN
      Begin
         // load the list from a file

         TRY
             AssignFile(F, Param);
             Reset(F);
             WHILE Not EOF(F) Do
             Begin         // Fixed 7/8/01 to handle all sorts of bus names
                  Readln(F, S2);
                  Auxparser[ActiveActor].CmdString := S2;
                  ParmName := Auxparser[ActiveActor].NextParam ;
                  Param := AuxParser[ActiveActor].StrValue;
                  IF   Length(Param) > 0
                  THEN With ActiveCircuit[ActiveActor] Do
                    Begin
                      iBus := BusList.Find(Param);
                      If iBus>0 Then Buses^[iBus].Keep := TRUE;
                    End;
             End;
             CloseFile(F);

         EXCEPT
             On E:Exception Do DoSimpleMsg('Error trying to read bus list file "+param+". Error is: '+E.message, 269);
         END;


     End
     ELSE Begin

       // Parse bus names off of array list
       WHILE Length(Param) > 0 Do
       BEGIN
            With ActiveCircuit[ActiveActor] Do
            Begin
              iBus := BusList.Find(Param);
              If iBus>0 Then Buses^[iBus].Keep := TRUE;
            End;

            AuxParser[ActiveActor].NextParam;
            Param := AuxParser[ActiveActor].StrValue;
       END;

     End;

end;

//----------------------------------------------------------------------------
FUNCTION DocktlossesCmd: Integer;
Var
   LossValue :complex;
begin
     Result := 0;
     IF ActiveCircuit[ActiveActor] <> Nil THEN
      Begin
         GlobalResult := '';
         LossValue := ActiveCircuit[ActiveActor].Losses[ActiveActor];
         GlobalResult := Format('%10.5g, %10.5g',[LossValue.re * 0.001,  LossValue.im*0.001]);
      End
    ELSE  GlobalResult := 'No Active Circuit.';


end;

FUNCTION DocurrentsCmd: Integer;
VAR
  cBuffer: pComplexArray;
  NValues, i: Integer;

Begin
    Result := 0;

  If ActiveCircuit[ActiveActor] <> Nil Then
     WITH ActiveCircuit[ActiveActor].ActiveCktElement DO
     Begin
         NValues := NConds*Nterms;
         GlobalResult := '';
         cBuffer := Allocmem(sizeof(cBuffer^[1])*NValues);
         GetCurrents(cBuffer, ActiveActor);
         For i := 1 to  NValues DO
         Begin
            GlobalResult := GlobalResult + Format('%10.5g, %6.1f,',[cabs(cBuffer^[i]), Cdang(cBuffer^[i])]);
         End;
         Reallocmem(cBuffer,0);
     End
  Else
     GlobalResult := 'No Active Circuit.';


end;

FUNCTION DoNodeListCmd: Integer;
VAR
  NValues, i: Integer;
  CktElementName, S : String;


Begin

  Result := 0;

  If ActiveCircuit[ActiveActor] <> Nil Then
  Begin
    S := Parser[ActiveActor].NextParam;
    CktElementName := Parser[ActiveActor].StrValue ;

    If Length(CktElementName) > 0  Then  SetObject(CktElementName);

    If Assigned(ActiveCircuit[ActiveActor].ActiveCktElement) Then
     WITH ActiveCircuit[ActiveActor].ActiveCktElement DO
     Begin
         NValues := NConds*Nterms;
         GlobalResult := '';
         For i := 1 to  NValues DO
         Begin
            GlobalResult := GlobalResult + Format('%d, ',[GetNodeNum(NodeRef^[i]) ]);
         End;
     End
  Else
     GlobalResult := 'No Active Circuit.';
  End;


end;


FUNCTION DolossesCmd: Integer;
Var
   LossValue :complex;
begin
    Result := 0;
     IF ActiveCircuit[ActiveActor] <> Nil THEN
      WITH ActiveCircuit[ActiveActor] DO
      Begin
        If ActiveCktElement<>Nil THEN
        Begin
         GlobalResult := '';
         LossValue := ActiveCktElement.Losses[ActiveActor];
         GlobalResult := Format('%10.5g, %10.5g', [LossValue.re * 0.001, LossValue.im * 0.001]);
        End;
      End
    ELSE GlobalResult := 'No Active Circuit.';

end;

FUNCTION DophaselossesCmd: Integer;

// Returns Phase losses in kW, kVar

VAR
  cBuffer:pComplexArray;
  NValues, i : Integer;

Begin

 Result := 0;

 IF ActiveCircuit[ActiveActor] <> Nil THEN

  WITH ActiveCircuit[ActiveActor].ActiveCktElement DO
  Begin
      NValues := NPhases;
      cBuffer := Allocmem(sizeof(cBuffer^[1])*NValues);
      GlobalResult := '';
      GetPhaseLosses( NValues, cBuffer,ActiveActor);
      For i := 1 to  NValues DO Begin
         GlobalResult := GlobalResult + Format('%10.5g, %10.5g,',[ cBuffer^[i].re*0.001, cBuffer^[i].im*0.001]);
      End;
      Reallocmem(cBuffer,0);
  End
 ELSE GlobalResult := 'No Active Circuit.'



end;

FUNCTION DopowersCmd(Total : Integer): Integer;
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
  IF ActiveCircuit[ActiveActor] <> Nil THEN
    WITH ActiveCircuit[ActiveActor].ActiveCktElement DO
    Begin
      NValues := NConds*Nterms;
      GlobalResult := '';
      cBuffer := Allocmem(sizeof(cBuffer^[1])*NValues);
      GetPhasePower(cBuffer, ActiveActor);
      if Total = 0 then
      Begin
        For i := 1 to  NValues DO Begin
           GlobalResult := GlobalResult+ Format('%10.5g, %10.5g,', [cBuffer^[i].re*0.001, cBuffer^[i].im*0.001]);
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
            myBuffer[j - 1] :=  cadd(myBuffer[j - 1], cBuffer^[i]);
          End;
          GlobalResult := GlobalResult+ Format('%10.5g, %10.5g,', [myBuffer[j - 1].re*0.001, myBuffer[j - 1].im*0.001]);
        End;
      End;
      Reallocmem(cBuffer,0);
    End
  ELSE GlobalResult := 'No Active Circuit';


end;

FUNCTION DoseqcurrentsCmd: Integer;
// All sequence currents of active ciruit element
// returns magnitude only.

VAR
  Nvalues,i,j,k:Integer;
  IPh, I012 : Array[1..3] of Complex;
  cBuffer:pComplexArray;

Begin

   Result := 0;
   IF ActiveCircuit[ActiveActor] <> Nil THEN
     WITH ActiveCircuit[ActiveActor] DO
     Begin
       If ActiveCktElement<>Nil THEN
       WITH ActiveCktElement DO
       Begin
        GlobalResult := '';
        IF Nphases<3
        THEN  For i := 0 to  3*Nterms-1 DO GlobalResult := GlobalResult + ' -1.0,'  // Signify n/A
        ELSE Begin
          NValues := NConds * Nterms;
          cBuffer := Allocmem(sizeof(cBuffer^[1])*NValues);
          GetCurrents(cBuffer, ActiveActor);
          For j := 1 to Nterms Do
          Begin
            k := (j-1)*NConds;
            For i := 1 to  3 DO
            Begin
              Iph[i] := cBuffer^[k+i];
            End;
            Phase2SymComp(@Iph, @I012);
            For i := 1 to 3 DO
            Begin
              GlobalResult := GlobalResult + Format('%10.5g, ',[Cabs(I012[i])]);
            End;
          End;
          Reallocmem(cBuffer,0);
        End; {ELSE}
       End; {WITH ActiveCktElement}
     End   {IF/WITH ActiveCircuit[ActiveActor]}
   ELSE GlobalResult := 'No Active Circuit';


end;

FUNCTION DoSeqpowersCmd: Integer;
// All seq Powers of active 3-phase ciruit element
// returns kW + j kvar

VAR
  Nvalues,i,j,k :Integer;
  S:Complex;
  VPh, V012 : Array[1..3] of Complex;
  IPh, I012 : Array[1..3] of Complex;
  cBuffer:pComplexArray;

Begin

 Result := 0;
 IF ActiveCircuit[ActiveActor] <> Nil THEN
   WITH ActiveCircuit[ActiveActor] DO Begin
     If ActiveCktElement<>Nil THEN
     WITH ActiveCktElement DO Begin
      GlobalResult := '';
      IF NPhases < 3 THEN
         For i := 0 to 2*3*Nterms-1 DO GlobalResult := GlobalResult + '-1.0, '  // Signify n/A
      ELSE Begin
        NValues := NConds * Nterms;
        cBuffer := Allocmem(sizeof(cBuffer^[1])*NValues);
        GetCurrents(cBuffer, ActiveActor);
        FOR j := 1 to Nterms Do Begin
         k :=(j-1) * NConds;
         FOR i := 1 to  3 DO Begin
            Vph[i] := Solution.NodeV^[Terminals^[j].TermNodeRef^[i]];
         End;
         For i := 1 to  3 DO Begin
           Iph[i] := cBuffer^[k+i];
         End;
         Phase2SymComp(@Iph, @I012);
         Phase2SymComp(@Vph, @V012);
         For i := 1 to 3 DO  Begin
           S := Cmul(V012[i], conjg(I012[i]));
           GlobalResult := GlobalResult+ Format('%10.5g, %10.5g,',[S.re*0.003, S.im*0.003]); // 3-phase kW conversion
         End;
        End;
      End;
      Reallocmem(cBuffer,0);
     End;
   End
 ELSE GlobalResult := 'No Active Circuit';


end;

FUNCTION DoseqvoltagesCmd: Integer;

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
  IF   ActiveCircuit[ActiveActor] <> Nil THEN
   WITH ActiveCircuit[ActiveActor] DO
   Begin
     If ActiveCktElement<>Nil THEN
     WITH ActiveCktElement DO
     If Enabled Then
     Begin
     TRY
      Nvalues := NPhases;
      GlobalResult :='';
      IF Nvalues < 3 THEN
         For i := 1 to 3*Nterms DO GlobalResult := GlobalResult + '-1.0, '  // Signify n/A
      ELSE
      Begin

       FOR j := 1 to Nterms Do
       Begin

          k :=(j-1)*NConds;
          FOR i := 1 to 3 DO
          Begin
             Vph[i] := Solution.NodeV^[NodeRef^[i+k]];
          End;
          Phase2SymComp(@Vph, @V012);   // Compute Symmetrical components

          For i := 1 to 3 DO  // Stuff it in the result
          Begin
             GlobalResult := GlobalResult + Format('%10.5g, ',[Cabs(V012[i])]);
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
         GlobalResult := 'Element Disabled';  // Disabled

   End
  ELSE GlobalResult := 'No Active Circuit';



End;

//----------------------------------------------------------------------------
FUNCTION DovoltagesCmd(Const PerUnit:Boolean): Integer;
// Bus Voltages at active terminal

VAR
  i:Integer;
  Volts:Complex;
  ActiveBus:TDSSBus;
  VMag:Double;

Begin

    Result := 0;
    IF ActiveCircuit[ActiveActor] <> Nil THEN
      WITH ActiveCircuit[ActiveActor] DO
      Begin
        If ActiveBusIndex<>0 THEN
        Begin
         ActiveBus := Buses^[ActiveBusIndex];
         GlobalResult := '';
         FOR i := 1 to  ActiveBus.NumNodesThisBus DO
         Begin
            Volts := Solution.NodeV^[ActiveBus.GetRef(i)];
            Vmag := Cabs(Volts);
            If PerUnit and (ActiveBus.kvbase>0.0) Then Begin
                  Vmag := Vmag *0.001/ActiveBus.kVBase;
                  GlobalResult := GlobalResult + Format('%10.5g, %6.1f, ', [Vmag, CDang(Volts)]);
            End 
            Else  GlobalResult := GlobalResult + Format('%10.5g, %6.1f, ', [Vmag, CDang(Volts)]);
         End;
        End
        Else GlobalResult := 'No Active Bus.';
      End
    ELSE GlobalResult := 'No Active Circuit.';

end;

//----------------------------------------------------------------------------
FUNCTION DoZscCmd(Zmatrix:Boolean): Integer;
// Bus Short Circuit matrix

VAR
  i,j:Integer;
  ActiveBus:TDSSBus;
  Z:Complex;

Begin

    Result := 0;
    IF ActiveCircuit[ActiveActor] <> Nil THEN
      WITH ActiveCircuit[ActiveActor] DO
      Begin
        If ActiveBusIndex<>0 THEN
        Begin
         ActiveBus := Buses^[ActiveBusIndex];
         GlobalResult := '';
         If not assigned(ActiveBus.Zsc) Then Exit;
         With ActiveBus Do
         FOR i := 1 to  NumNodesThisBus DO Begin
            For j := 1 to  NumNodesThisBus Do  Begin

             If ZMatrix Then Z := Zsc.GetElement(i,j)
             Else Z := Ysc.GetElement(i,j);
             GlobalResult := GlobalResult + Format('%-.5g, %-.5g,   ', [Z.re, Z.im]);

            End;

         End;
        End
        Else GlobalResult := 'No Active Bus.';
      End
    ELSE GlobalResult := 'No Active Circuit.';

end;


//----------------------------------------------------------------------------
FUNCTION DoZsc012Cmd: Integer;
// Bus Short Circuit matrix

VAR
  i:Integer;
  ActiveBus:TDSSBus;
  Z0, Z1, Z2:Complex;
  Temp1, Temp2 : pComplexArray;
  Zsc012Temp   :TcMatrix;
Begin

    Result := 0;
    IF ActiveCircuit[ActiveActor] <> Nil THEN
      WITH ActiveCircuit[ActiveActor] DO
      Begin
        If ActiveBusIndex <> 0 THEN
        Begin
         ActiveBus := Buses^[ActiveBusIndex];
         GlobalResult := '';
         If not assigned(ActiveBus.Zsc) Then Exit;

         With ActiveBus Do
           If NumNodesThisBus = 3 Then
             Begin

        // Compute ZSC012 for 3-phase buses else leave it zeros
        // ZSC012 = Ap2s Zsc As2p
                  Zsc012Temp:= Zsc.MtrxMult(As2p);  // temp for intermediate result
                  if Assigned(ZSC012) then ZSC012.Free;
                  ZSC012 := Ap2s.MtrxMult(Zsc012Temp);
                  // Cleanup
                  Zsc012Temp.Free;


              {Just return diagonal elements only}

                 Z0 := Zsc012.GetElement(1,1);
                 Z1 := Zsc012.GetElement(2,2);
                 Z2 := Zsc012.GetElement(3,3);
                 GlobalResult := GlobalResult + Format('Z0, (%-.5g, +j %-.5g), ', [Z0.re, Z0.im]) + CRLF;
                 GlobalResult := GlobalResult + Format('Z1, (%-.5g, +j %-.5g), ', [Z1.re, Z1.im]) + CRLF;
                 GlobalResult := GlobalResult + Format('Z2, (%-.5g, +j %-.5g), ', [Z2.re, Z2.im]);

             End
           Else GlobalResult :=  'Not a 3-phase bus. Cannot compute Symmetrical Component matrix.';

        End
        Else GlobalResult := 'No Active Bus.';
      End
    ELSE GlobalResult := 'No Active Circuit.';

end;

//----------------------------------------------------------------------------
FUNCTION DoZsc10Cmd: Integer;
// Bus Short Circuit matrix

VAR
  ActiveBus:TDSSBus;
  Z:Complex;

Begin

    Result := 0;
    IF ActiveCircuit[ActiveActor] <> Nil THEN
      WITH ActiveCircuit[ActiveActor] DO
      Begin
        If ActiveBusIndex<>0 THEN
        Begin
         ActiveBus := Buses^[ActiveBusIndex];
         GlobalResult := '';
         If not assigned(ActiveBus.Zsc) Then Exit;
         With ActiveBus Do Begin

             Z := Zsc1;
             GlobalResult := GlobalResult + Format('Z1, %-.5g, %-.5g, ', [Z.re, Z.im]) + CRLF;
             
             Z := Zsc0;
             GlobalResult := GlobalResult + Format('Z0, %-.5g, %-.5g, ', [Z.re, Z.im]);
         End;

        End
        Else GlobalResult := 'No Active Bus.';
      End
    ELSE GlobalResult := 'No Active Circuit.';

end;


//----------------------------------------------------------------------------
FUNCTION DoAllocateLoadsCmd(ActorID : Integer): Integer;

{ Requires an EnergyMeter Object at the head of the feeder
  Adjusts loads defined by connected kVA or kWh billing
}

VAR
   pMeter :TEnergyMeterObj;
   pSensor:TSensorObj;
   iterCount :Integer;

begin
    Result := 0;
    WITH ActiveCircuit[ActorID] Do
    Begin
         LoadMultiplier := 1.0;   // Property .. has side effects
         With Solution Do
         Begin
             If Mode <> SNAPSHOT Then Mode := SNAPSHOT;   // Resets meters, etc. if not in snapshot mode
             Solve(ActorID);  {Make guess based on present allocationfactors}
         End;

         {Allocation loop -- make MaxAllocationIterations iterations}
         FOR iterCount := 1 to MaxAllocationIterations Do Begin

           {Do EnergyMeters}
           pMeter := EnergyMeters.First;
           WHILE pMeter <> NIL Do Begin
              pMeter.CalcAllocationFactors(ActorID);
              pMeter := EnergyMeters.Next;
           End;

           {Now do other Sensors}
           pSensor := Sensors.First;
           WHILE pSensor <> NIL Do Begin
              pSensor.CalcAllocationFactors(ActorID);
              pSensor := Sensors.Next;
           End;

           {Now let the EnergyMeters run down the circuit setting the loads}
            pMeter := EnergyMeters.First;
            WHILE pMeter <> NIL Do Begin
                pMeter.AllocateLoad(ActorID);
                pMeter := EnergyMeters.Next;
            End;
            Solution.Solve(ActorID);  {Update the solution}

         End;
    End;
end;

//----------------------------------------------------------------------------
PROCEDURE DoSetAllocationFactors(const X: Double);

VAR
   pLoad :TLoadObj;

begin
    IF   X <= 0.0
    THEN DoSimpleMsg('Allocation Factor must be greater than zero.', 271)
    ELSE WITH ActiveCircuit[ActiveActor] Do
    Begin
         pLoad := Loads.First;
         WHILE pLoad <> NIL Do
         Begin
             pLoad.kVAAllocationFactor := X;
             pLoad := Loads.Next;
         End;
    End;
end;

PROCEDURE DoSetCFactors(const X: Double);

VAR
   pLoad :TLoadObj;

begin
    IF   X <= 0.0
    THEN DoSimpleMsg('CFactor must be greater than zero.', 271)
    ELSE WITH ActiveCircuit[ActiveActor] Do
    Begin
         pLoad := Loads.First;
         WHILE pLoad <> NIL Do
         Begin
             pLoad.CFactor := X;
             pLoad := Loads.Next;
         End;
    End;
end;

//----------------------------------------------------------------------------
FUNCTION DoHarmonicsList(const S:String):Integer;

VAR
   Dummy :pDoubleArray;
   i,
   Num   :Integer;

Begin
   Result := 0;

   WITH ActiveCircuit[ActiveActor].Solution Do
   IF CompareText(S, 'ALL') = 0 THEN DoAllHarmonics := TRUE
   ELSE Begin
       DoAllHarmonics := FALSE;

       Dummy := AllocMem(Sizeof(Dummy^[1]) * 100); // Big Buffer
       Num   := Parser[ActiveActor].ParseAsVector(100, Dummy);
       {Parsing zero-fills the array}

       HarmonicListSize := Num;
       Reallocmem(HarmonicList, SizeOf(HarmonicList^[1]) * HarmonicListSize);
       FOR i := 1 to HarmonicListSize Do HarmonicList^[i] := Dummy^[i];

       Reallocmem(Dummy, 0);
   End;
End;


//----------------------------------------------------------------------------
FUNCTION DoFormEditCmd:Integer;

Begin

    Result := 0;
    If NoFormsAllowed Then Exit;
    DoSelectCmd;  // Select ActiveObject
    IF ActiveDSSObject[ActiveActor] <> NIL THEN  Begin

         ShowPropEditForm;

    End
    ELSE   Begin
       DoSimpleMsg('Element Not Found.', 272);
       Result := 1;
    End;
End;


//----------------------------------------------------------------------------
FUNCTION DoMeterTotals:Integer;
Var
   i: Integer;
Begin
    Result := 0;
    If ActiveCircuit[ActiveActor] <> Nil Then
      Begin
       ActiveCircuit[ActiveActor].TotalizeMeters;
        // Now export to global result
        For i := 1 to NumEMregisters Do
          Begin
            AppendGlobalResult(Format('%-.6g',[ActiveCircuit[ActiveActor].RegisterTotals[i]]));
          End;
      End;
End;

//----------------------------------------------------------------------------
FUNCTION DoCapacityCmd:Integer;

Var
   ParamPointer     :integer;
   Param, ParamName :String;

Begin
  Result := 0;

     ParamPointer := 0;
     ParamName := Parser[ActiveActor].NextParam;
     Param := Parser[ActiveActor].StrValue;
     WHILE Length(Param)>0 DO BEGIN
         IF Length(ParamName) = 0 THEN Inc(ParamPointer)
         ELSE Case ParamName[1] of
                 's':ParamPointer := 1;
                 'i':ParamPointer := 2;
              ELSE
                  ParamPointer := 0;
              END;

         CASE ParamPointer OF
            0: DoSimpleMsg('Unknown parameter "'+ParamName+'" for Capacity Command', 273);
            1: ActiveCircuit[ActiveActor].CapacityStart := Parser[ActiveActor].DblValue;
            2: ActiveCircuit[ActiveActor].CapacityIncrement := Parser[ActiveActor].DblValue;

         ELSE

         END;

         ParamName := Parser[ActiveActor].NextParam;
         Param := Parser[ActiveActor].StrValue;
     END;

    WITH ActiveCircuit[ActiveActor] Do
    IF ComputeCapacity(ActiveActor) Then Begin   // Totalizes EnergyMeters at End

       GlobalResult := Format('%-.6g', [(ActiveCircuit[ActiveActor].RegisterTotals[3] + ActiveCircuit[ActiveActor].RegisterTotals[19]) ] );  // Peak KW in Meters
       AppendGlobalResult(Format('%-.6g', [LoadMultiplier]));
    End;
End;

//----------------------------------------------------------------------------
FUNCTION DoClassesCmd:Integer;

VAR  i:Integer;
Begin
     For i := 1 to NumIntrinsicClasses Do Begin
       AppendGlobalResult(TDSSClass(DSSClassList[ActiveActor].Get(i)).Name);
     End;
     Result := 0;
End;

//----------------------------------------------------------------------------
FUNCTION DoUserClassesCmd:Integer;
VAR  i:Integer;
Begin
    Result := 0;
    IF NumUserClasses=0 Then Begin
        AppendGlobalResult('No User Classes Defined.');
    End
    ELSE
     For i := NumIntrinsicClasses+1 to DSSClassList[ActiveActor].ListSize Do Begin
       AppendGlobalResult(TDSSClass(DSSClassList[ActiveActor].Get(i)).Name);
     End;
End;

//----------------------------------------------------------------------------
FUNCTION DoZscRefresh(ActorID : Integer):Integer;

Var j:Integer;

Begin
   Result := 1;

   Try

     WITH ActiveCircuit[ActiveActor], ActiveCircuit[ActiveActor].Solution Do
     Begin
       FOR j := 1 to NumNodes Do Currents^[j] := cZERO;  // Clear Currents array

       IF (ActiveBusIndex > 0) and (ActiveBusIndex <= Numbuses) Then Begin
          If not assigned(Buses^[ActiveBusIndex].Zsc) Then Buses^[ActiveBusIndex].AllocateBusQuantities ;
          SolutionAlgs.ComputeYsc(ActiveBusIndex, ActorID);      // Compute YSC for active Bus
          Result := 0;
       End;
     End;

   Except
       On E:Exception Do DoSimpleMsg('ZscRefresh Error: ' + E.message + CRLF , 274);
   End;


End;


FUNCTION DoVarValuesCmd:Integer;

Var
   i: Integer;
  // PcElem:TPCElement;
Begin

    Result := 0;
    If ActiveCircuit[ActiveActor] <> Nil Then
    With ActiveCircuit[ActiveActor] Do
      Begin
         {Check if PCElement}
         CASE (ActiveCktElement.DSSObjType and BASECLASSMASK) OF
           PC_ELEMENT: With ActiveCktElement as TPCElement Do
                       Begin
                         For i := 1 to NumVariables Do
                         AppendGlobalResult(Format('%-.6g',[Variable[i]]));
                       End;
         Else
             AppendGlobalResult('Null');
         End;
      End;

End;

FUNCTION DoValVarCmd:Integer;

{Geg value of specified variable by name of index,}
Var
    ParamName, Param :String;
    VarIndex :Integer;
    PropIndex :Integer;
    PCElem :TPCElement;

Begin

    Result := 0;

    {Check to make sure this is a PC Element. If not, return null string in global result}

    If (ActiveCircuit[ActiveActor].ActiveCktElement.DSSObjType And BASECLASSMASK) <> PC_ELEMENT Then

       GlobalResult := ''

    Else Begin

        PCElem :=  ActiveCircuit[ActiveActor].ActiveCktElement As TPCElement;

        {Get next parameter on command line}

        ParamName := UpperCase(Parser[ActiveActor].NextParam);
        Param := Parser[ActiveActor].StrValue;

        PropIndex := 1;
        If Length(ParamName) > 0 Then
          CASE ParamName[1] of
              'N': PropIndex := 1;
              'I': PropIndex := 2;
          END;

        VarIndex := 0;

        CASE PropIndex of
            1: VarIndex := PCElem.LookupVariable(Param);  // Look up property index
            2: VarIndex := Parser[ActiveActor].IntValue ;
        END;

        If (VarIndex>0) and (VarIndex<=PCElem.NumVariables) Then

           GlobalResult := Format('%.8g',[PCElem.Variable[VarIndex] ])

        Else GlobalResult := '';   {Invalid var name or index}

    End;


End;

FUNCTION DoVarNamesCmd :Integer;

Var
   i: Integer;
Begin

    Result := 0;
    If ActiveCircuit[ActiveActor] <> Nil Then
    With ActiveCircuit[ActiveActor] Do
      Begin
         {Check if PCElement}
         CASE (ActiveCktElement.DSSObjType and BASECLASSMASK) OF
           PC_ELEMENT: With (ActiveCktElement as TPCElement) Do
                       Begin
                         For i := 1 to NumVariables Do
                         AppendGlobalResult(VariableName(i));
                       End;
         Else
             AppendGlobalResult('Null');
         End;
      End;

End;

FUNCTION DoBusCoordsCmd(SwapXY:Boolean; CoordType  : Integer):Integer;

{
 Format of File should be

   Busname, x, y

   (x, y are real values)

   If SwapXY is true, x and y values are swapped

}

Var

   F : TextFile;
   ParamName, Param,
   S,
   BusName : String;
   iB      : Integer;
   iLine   : Integer;

Begin
    Result := 0;

    {Get next parameter on command line}

    ParamName := Parser[ActiveActor].NextParam;
    Param := Parser[ActiveActor].StrValue;

    Try
      iLine := -1;
      Try
         AssignFile(F, Param);
         Reset(F);
         iLine := 0;
         While not EOF(F) Do
          Begin
             Inc(iLine);
             Readln(F, S);      // Read line in from file

             With AuxParser[ActiveActor] Do Begin      // User Auxparser to parse line
                   CmdString := S;
                   NextParam;  BusName := StrValue;
                   iB := ActiveCircuit[ActiveActor].Buslist.Find(BusName);
                   If iB >0 Then  Begin
                    With ActiveCircuit[ActiveActor].Buses^[iB] Do Begin     // Returns TBus object
                        if CoordType = 0 then                                   // Standard buscoords
                        Begin
                          NextParam;  If SwapXY Then y := DblValue else x := DblValue;
                          NextParam;  If SwapXY Then x := DblValue else y := DblValue;
                          CoordDefined := TRUE;
                        End
                        else
                        Begin                                                   // GIS coords
                          NextParam;  lat   := DblValue;
                          NextParam;  long  := DblValue;
                          GISCoorddefined   :=  TRUE;
                        End;
                    End;
                   End;
              End;
              {Else just ignore a bus that's not in the circuit}
          End;

      Except
      {**CHANGE THIS ERROR MESSAGE**}
          ON E:Exception Do Begin
              If iLine = -1 Then DoSimpleMsg('Bus Coordinate file: "' + Param + '" not found; ' + E.Message , 275)
              Else DoSimpleMsg('Bus Coordinate file: Error Reading Line ' + InttoStr(Iline)+'; ' + E.Message , 275);
          End;
      End;

    Finally
        CloseFile(F);
    End;

End;

FUNCTION DoMakePosSeq:Integer;

Var
   CktElem:TDSSCktElement;

Begin
    Result := 0;

    ActiveCircuit[ActiveActor].PositiveSequence := TRUE;

    CktElem := ActiveCircuit[ActiveActor].CktElements.First;
    While CktElem<>Nil Do
    Begin
       CktElem.MakePosSequence(ActiveActor);
       CktElem := ActiveCircuit[ActiveActor].CktElements.Next;
    End;

End;


PROCEDURE DoSetReduceStrategy(Const S:String);


   Function AtLeast(i,j:Integer):Integer;
   Begin If j<i Then Result := i Else Result := j; End;

Begin
     ActiveCircuit[ActiveActor].ReductionStrategyString := S;

     ActiveCircuit[ActiveActor].ReductionStrategy := rsDefault;
     IF Length(S)=0 Then Exit;  {No option given}

     Case UpperCase(S)[1] of

       'B': ActiveCircuit[ActiveActor].ReductionStrategy := rsBreakLoop;
       'D': ActiveCircuit[ActiveActor].ReductionStrategy := rsDefault;  {Default}
       'E': ActiveCircuit[ActiveActor].ReductionStrategy := rsDangling;  {Ends}
       'L': Begin {Laterals}
               ActiveCircuit[ActiveActor].ReductionStrategy := rsLaterals;
            End;
       'M': ActiveCircuit[ActiveActor].ReductionStrategy := rsMergeParallel;
        {*
       'T': Begin
              ActiveCircuit[ActiveActor].ReductionStrategy := rsTapEnds;
              ActiveCircuit[ActiveActor].ReductionMaxAngle := 15.0;
              If Length(param2) > 0 Then  ActiveCircuit[ActiveActor].ReductionMaxAngle := Auxparser.DblValue;
            End;
            *}
       'S': Begin  {Shortlines or Switch}
              IF CompareTextShortest(S, 'SWITCH')=0 Then Begin
                  ActiveCircuit[ActiveActor].ReductionStrategy := rsSwitches;
              End ELSE Begin
                 { ActiveCircuit.ReductionZmag is now set in main ExecOptions     }
                  ActiveCircuit[ActiveActor].ReductionStrategy := rsShortlines;
              End;
            End;
     ELSE
         DoSimpleMsg('Unknown Reduction Strategy: "' + S + '".', 276);
     End;

End;

FUNCTION DoInterpolateCmd:Integer;

{Interpolate bus coordinates in meter zones}

VAR
    MetObj:TEnergyMeterObj;
    MeterClass: TEnergyMeter;
    ParamName, Param  :String;
    DevClassIndex:Integer;
    CktElem:TDSSCktElement;

Begin
    Result := 0;

    ParamName := Parser[ActiveActor].NextParam;
    Param := UpperCase(Parser[ActiveActor].StrValue);

    // initialize the Checked Flag FOR all circuit Elements
    With ActiveCircuit[ActiveActor] Do
    Begin
     CktElem := CktElements.First;
     WHILE  (CktElem <> NIL) Do
     Begin
         CktElem.Checked := False;
         CktElem := CktElements.Next;
     End;
    End;


    IF Length(Param) = 0  Then Param := 'A';
    CASE Param[1] of
     'A': Begin
              metobj := ActiveCircuit[ActiveActor].EnergyMeters.First;
              While metobj <> nil Do
              Begin
                MetObj.InterpolateCoordinates;
                MetObj := ActiveCircuit[ActiveActor].EnergyMeters.Next;
              End;
          End;

    ELSE
       {Interpolate a specific meter}
       DevClassIndex := ClassNames[ActiveActor].Find('energymeter');
       IF DevClassIndex > 0 THEN
       Begin
          MeterClass := DSSClassList[ActiveActor].Get(DevClassIndex);
          If MeterClass.SetActive (Param) Then   // Try to set it active
          Begin
            MetObj := MeterClass.GetActiveObj;
            MetObj.InterpolateCoordinates;
          End
          Else DoSimpleMsg('EnergyMeter "'+Param+'" not found.', 277);
       End;
    End;

End;

FUNCTION DoAlignFileCmd:Integer;
{Rewrites designated file, aligning the fields into columns}
Var
    ParamName, Param  :String;

Begin
  Result := 0;
  ParamName := Parser[ActiveActor].NextParam;
  Param := Parser[ActiveActor].StrValue;


  If FileExists(Param) Then
    Begin
     If Not RewriteAlignedFile(Param) Then Result := 1;
    End
  Else
    Begin
     DoSimpleMsg('File "'+Param+'" does not exist.', 278);
     Result := 1;
    End;

  If Result=0 Then FireOffEditor(GlobalResult);

End; {DoAlignfileCmd}

FUNCTION DoTOPCmd:Integer;
{ Sends Monitors, Loadshapes, GrowthShapes, or TCC Curves to TOP as an STO file}

Var
    ParamName, Param, ObjName  :String;

Begin
    Result := 0;
    ParamName := Parser[ActiveActor].NextParam;
    Param := UpperCase(Parser[ActiveActor].StrValue);

    ParamName := Parser[ActiveActor].NextParam;
    ObjName := UpperCase(Parser[ActiveActor].StrValue);

    If Length(ObjName)=0 Then ObjName := 'ALL';


    Case  Param[1] of
        'L': LoadShapeClass[ActiveActor].TOPExport(ObjName);
        'T': TshapeClass[ActiveActor].TOPExport(ObjName);
        {
          'G': GrowthShapeClass.TOPExportAll;
          'T': TCC_CurveClass.TOPExportAll;
        }
    ELSE
        MonitorClass[ActiveActor].TOPExport(ObjName);
    End;


End;

Procedure DoSetNormal(pctNormal:Double);

Var i:Integer;
    pLine:TLineObj;

Begin
    If ActiveCircuit[ActiveActor] <> Nil Then Begin
       pctNormal := pctNormal * 0.01;  // local copy only
       For i := 1 to ActiveCircuit[ActiveActor].Lines.ListSize Do  Begin
         pLine := ActiveCircuit[ActiveActor].Lines.Get(i);
         pLine.Normamps := pctNormal * pLine.EmergAmps;
       End;
    End;
End;

FUNCTION DoRotateCmd:Integer;

{rotate about the center of the coordinates}

Var
        i:Integer;
        Angle, xmin,xmax, ymin, ymax, xc, yc:Double;
         ParamName:String;
         a, vector: Complex;

Begin
    Result := 0;
    If ActiveCircuit[ActiveActor] <> NIl then Begin

        ParamName := Parser[ActiveActor].NextParam;
        Angle := Parser[ActiveActor].DblValue * PI/180.0;   // Deg to rad

        a := cmplx(cos(Angle), Sin(Angle));
        With ActiveCircuit[ActiveActor] Do Begin
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
                         Vector := Cmul(Vector, a);
                         x := xc+vector.re;
                         y := yc+vector.im;
                    End;
                End;
            End;
        End;
    end;

End;


FUNCTION DoVDiffCmd:Integer;
Var
        Fin, Fout :TextFile;
        BusName, Line:String;
        i,  node, busIndex:Integer;
        Vmag, Diff:Double;

Begin
   Result := 0;
   If FileExists(CircuitName_[ActiveActor] + 'SavedVoltages.Txt') Then Begin
   Try
    Try

         AssignFile(Fin, CircuitName_[ActiveActor] + 'SavedVoltages.Txt');
         Reset(Fin);

         AssignFile(Fout, CircuitName_[ActiveActor] + 'VDIFF.txt');
         Rewrite(Fout);

         While Not EOF(Fin) Do Begin
             Readln(Fin, Line);
             Auxparser[ActiveActor].CmdString := Line;
             AuxParser[ActiveActor].NextParam;
             BusName := Auxparser[ActiveActor].StrValue;
             If Length(BusName) > 0 Then Begin
                 BusIndex := ActiveCircuit[ActiveActor].BusList.Find(BusName);
                 If BusIndex>0 Then Begin
                     AuxParser[ActiveActor].Nextparam;
                     node := AuxParser[ActiveActor].Intvalue;
                     With  ActiveCircuit[ActiveActor].Buses^[BusIndex] Do
                     For i := 1 to NumNodesThisBus Do Begin
                         If GetNum(i)=node then Begin
                             AuxParser[ActiveActor].Nextparam;
                             Vmag := AuxParser[ActiveActor].Dblvalue;
                             Diff := Cabs(ActiveCircuit[ActiveActor].Solution.NodeV^[GetRef(i)]) - Vmag;
                             If Vmag<>0.0 then Begin
                                Writeln(Fout, BusName,'.',node,', ', (Diff / Vmag * 100.0):7:2,', %');
                             End
                             Else Writeln(Fout, BusName,'.',node,', ', format('%-.5g',[Diff]),', Volts');
                         End;
                     End;

                 End;
             End;
         End;

      
    Except
          On E:Exception Do Begin
           DoSimpleMsg('Error opening Saved Voltages or VDIFF File: '+E.message, 280);
           Exit;
          End;

    End;


  Finally

   CloseFile(Fin);
   CloseFile(Fout);

   FireOffEditor(CircuitName_[ActiveActor] + 'VDIFF.txt');

  End;

  End
  Else  DoSimpleMsg('Error: No Saved Voltages.', 281);

End;

FUNCTION DoSummaryCmd:Integer;

// Returns summary in global result String

Var
   S:String;
   cLosses,
   cPower :Complex;

Begin
  Result := 0;
     S := '';
     IF ActiveCircuit[ActiveActor].Issolved Then S := S + 'Status = SOLVED' + CRLF
     Else Begin
       S := S + 'Status = NOT Solved' + CRLF;
     End;
     S := S + 'Solution Mode = ' + GetSolutionModeID + CRLF;
     S := S + 'Number = ' + IntToStr(ActiveCircuit[ActiveActor].Solution.NumberofTimes) + CRLF;
     S := S + 'Load Mult = '+ Format('%5.3f', [ActiveCircuit[ActiveActor].LoadMultiplier]) + CRLF;
     S := S + 'Devices = '+ Format('%d', [ActiveCircuit[ActiveActor].NumDevices]) + CRLF;
     S := S + 'Buses = ' + Format('%d', [ActiveCircuit[ActiveActor].NumBuses]) + CRLF;
     S := S + 'Nodes = ' + Format('%d', [ActiveCircuit[ActiveActor].NumNodes]) + CRLF;
     S := S + 'Control Mode =' + GetControlModeID + CRLF;
     S := S + 'Total Iterations = '+IntToStr(ActiveCircuit[ActiveActor].Solution.Iteration) + CRLF;
     S := S + 'Control Iterations = '+IntToStr(ActiveCircuit[ActiveActor].Solution.ControlIteration) + CRLF;
     S := S + 'Max Sol Iter = ' +IntToStr(ActiveCircuit[ActiveActor].Solution.MostIterationsDone ) + CRLF;
     S := S + ' ' + CRLF;
     S := S + ' - Circuit Summary -' + CRLF;
     S := S + ' ' + CRLF;
     If ActiveCircuit[ActiveActor] <> Nil Then Begin

         S := S + Format('Year = %d ',[ActiveCircuit[ActiveActor].Solution.Year]) + CRLF;
         S := S + Format('Hour = %d ',[ActiveCircuit[ActiveActor].Solution.DynaVars.intHour]) + CRLF;
         S := S + 'Max pu. voltage = '+Format('%-.5g ',[GetMaxPUVoltage]) + CRLF;
         S := S + 'Min pu. voltage = '+Format('%-.5g ',[GetMinPUVoltage(TRUE)]) + CRLF;
         cPower :=  CmulReal(GetTotalPowerFromSources(ActiveActor), 0.000001);  // MVA
         S := S + Format('Total Active Power:   %-.6g MW',[cpower.re]) + CRLF;
         S := S + Format('Total Reactive Power: %-.6g Mvar',[cpower.im]) + CRLF;
         cLosses := CmulReal(ActiveCircuit[ActiveActor].Losses[ActiveActor], 0.000001);
         If cPower.re <> 0.0 Then S := S + Format('Total Active Losses:   %-.6g MW, (%-.4g %%)',[cLosses.re,(Closses.re/cPower.re*100.0)]) + CRLF
                             Else S := S + 'Total Active Losses:   ****** MW, (**** %%)' + CRLF;
         S := S + Format('Total Reactive Losses: %-.6n Mvar',[cLosses.im]) + CRLF;
         S := S + Format('Frequency = %-g Hz',[ActiveCircuit[ActiveActor].Solution.Frequency]) + CRLF;
         S := S + 'Mode = '+GetSolutionModeID + CRLF;
         S := S + 'Control Mode = '+GetControlModeID + CRLF;
         S := S + 'Load Model = '+GetLoadModel + CRLF;
     End;

     GlobalResult := S;
End;

Function DoDistributeCmd:Integer;
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

     ParamName := Parser[ActiveActor].NextParam;
     Param := Parser[ActiveActor].StrValue;
     WHILE Length(Param)>0 DO
     Begin
         IF   (Length(ParamName) = 0)
         THEN Inc(ParamPointer)
         ELSE ParamPointer := DistributeCommands.GetCommand(ParamName);

         CASE ParamPointer OF
           1: kW := Parser[ActiveActor].DblValue;
           2: How := Parser[ActiveActor].StrValue;
           3: Skip := Parser[ActiveActor].IntValue;
           4: PF := Parser[ActiveActor].DblValue;
           5: FilName := Parser[ActiveActor].StrValue;
           6: kW := Parser[ActiveActor].DblValue * 1000.0;
           7: if (Uppercase(Param)[1]='L') then DoGenerators := FALSE Else DoGenerators := TRUE;  // Load or Generator

         ELSE
             // ignore unnamed and extra parms
         End;

         ParamName := Parser[ActiveActor].NextParam;
         Param := Parser[ActiveActor].StrValue;
     End;

     if Not DoGenerators then FilName := 'DistLoads.dss' ;

     MakeDistributedGenerators(kW, PF, How, Skip, FilName, DoGenerators);  // in Utilities

End;

FUNCTION DoDI_PlotCmd:Integer;
{$IF not (defined(DLL_ENGINE) or defined(FPC))}
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
{$ENDIF}
Begin
{$IF not (defined(DLL_ENGINE) or defined(FPC))}
     IF DIFilesAreOpen[ActiveActor] Then EnergyMeterClass[ActiveActor].CloseAllDIFiles(ActiveActor);
     If Not Assigned(DSSPlotObj) Then DSSPlotObj := TDSSPlot.Create;
     {Defaults}
     NumRegs:=1;
     SetLength(IRegisters, NumRegs);
     iRegisters[0] := 9;
     PeakDay := FALSE;
     CaseYear := 1;
     CaseName := '';
     MeterName := 'DI_Totals';

     ParamPointer := 0;
     ParamName := Parser[ActiveActor].NextParam;
     Param := Parser[ActiveActor].StrValue;
     WHILE Length(Param)>0 DO
     Begin
         IF   (Length(ParamName) = 0) THEN Inc(ParamPointer)
         ELSE ParamPointer := DI_PlotCommands.GetCommand(ParamName);

         CASE ParamPointer OF
           1: CaseName := Param;
           2: CaseYear := Parser[ActiveActor].Intvalue;
           3: Begin
                 NumRegs := Parser[ActiveActor].ParseAsVector(NumEMREgisters, @dRegisters);
                 SetLength(iRegisters, NumRegs);
                 For i := 1 to NumRegs Do iRegisters[i-1] := Round(dRegisters[i]);
              End;
           4: PeakDay := InterpretYesNo(Param);
           5: MeterName := Param;

         ELSE
             // ignore unnamed and extra parms
         End;

         ParamName := Parser[ActiveActor].NextParam;
         Param := Parser[ActiveActor].StrValue;
     End;
     DSSPlotObj.DoDI_Plot(CaseName, CaseYear, iRegisters, PeakDay, MeterName);
     iRegisters := Nil;
{$ENDIF}
     Result := 0;
End;

FUNCTION DoCompareCasesCmd:Integer;
{$IFNDEF DLL_ENGINE}
Var
    ParamName, Param:String;
    ParamPointer:Integer;
    UnKnown:Boolean;
    Reg:Integer;
    CaseName1,
    CaseName2, WhichFile:String;
{$ENDIF}
Begin
{$IF not (defined(DLL_ENGINE) or defined(FPC))}
     IF DIFilesAreOpen[ActiveActor] Then EnergyMeterClass[ActiveActor].CloseAllDIFiles(ActiveActor);
     If Not Assigned(DSSPlotObj) Then DSSPlotObj := TDSSPlot.Create;
     CaseName1 := 'base';
     CaseName2 := '';
     Reg := 9;    // Overload EEN
     WhichFile := 'Totals';

     ParamPointer := 0;
     ParamName := UpperCase(Parser[ActiveActor].NextParam);
     Param := Parser[ActiveActor].StrValue;
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
           3: Reg := Parser[ActiveActor].IntValue;
           4: WhichFile := Param;
         ELSE
             // ignore unnamed and extra parms
         End;

         ParamName := UpperCase(Parser[ActiveActor].NextParam);
         Param := Parser[ActiveActor].StrValue;
     End;
     DSSPlotObj.DoCompareCases(CaseName1, CaseName2, WhichFile,  Reg);
{$ENDIF}
     Result := 0;
End;

FUNCTION DoYearlyCurvesCmd:Integer;
{$IF not (defined(DLL_ENGINE) or defined(FPC))}
Var
    ParamName, Param:String;
    ParamPointer, i:Integer;
    UnKnown:Boolean;
    CaseNames:TStringList;
    dRegisters:Array[1..NumEMRegisters] of Double;
    iRegisters:Array of Integer;
    Nregs:Integer;
    WhichFile:String;
{$ENDIF}
Begin
{$IF not (defined(DLL_ENGINE) or defined(FPC))}
     IF DIFilesAreOpen[ActiveActor] Then EnergyMeterClass[ActiveActor].CloseAllDIFiles(ActiveActor);
     If Not Assigned(DSSPlotObj) Then DSSPlotObj := TDSSPlot.Create;

     Nregs := 1;
     SetLength(iRegisters, Nregs);
     CaseNames := TStringList.Create;
     CaseNames.Clear;
     WhichFile := 'Totals';


     ParamPointer := 0;
     ParamName := Parser[ActiveActor].NextParam;
     Param := Parser[ActiveActor].StrValue;
     WHILE Length(Param)>0 DO
     Begin
         Unknown := False;
         IF   (Length(ParamName) = 0) THEN Inc(ParamPointer)

         ELSE Case Uppercase(ParamName)[1] of
                    'C':ParamPointer := 1;
                    'R':ParamPointer := 2;
                    'M':ParamPointer := 3; {meter=}
              ELSE
                   Unknown := TRUE;
              END;

         If Not Unknown then
         CASE ParamPointer OF
           1: Begin  // List of case names
                AuxParser[ActiveActor].CmdString := Param;
                AuxParser[ActiveActor].NextParam;
                Param := AuxParser[ActiveActor].StrValue;
                While Length(Param)>0 Do Begin
                    CaseNames.Add(Param);
                    AuxParser[ActiveActor].NextParam;
                    Param := AuxParser[ActiveActor].StrValue;
                End;
              End;
           2: Begin
                NRegs := Parser[ActiveActor].ParseAsVector(NumEMRegisters, @dRegisters);
                SetLength(iRegisters, Nregs);
                For i := 1 to NRegs Do iRegisters[i-1] := Round(dRegisters[i]);
              end;
           3: WhichFile := Param ;
         ELSE
             // ignore unnamed and extra parms
         End;

         ParamName := Parser[ActiveActor].NextParam;
         Param := Parser[ActiveActor].StrValue;
     End;
     DSSPlotObj.DoYearlyCurvePlot(CaseNames, WhichFile,  iRegisters);
     iRegisters := Nil;
     CaseNames.Free;
{$ENDIF}
     Result := 0;
End;

FUNCTION DoVisualizeCmd:Integer;
{$IF not defined(FPC)}
Var
    DevIndex    :integer;
    Param       :String;
    ParamName   :String;
    ParamPointer:Integer;
    Unknown     :Boolean;
    Quantity    :Integer;
    ElemName    :String;
    pElem       :TDSSObject;
{$ENDIF}
Begin
     Result := 0;
{$IF not defined(FPC)}
     // Abort if no circuit or solution
     If not assigned(ActiveCircuit[ActiveActor]) Then
     Begin
          DoSimpleMsg('No circuit created.',24721);
          Exit;
     End;
     If not assigned(ActiveCircuit[ActiveActor].Solution) OR not assigned(ActiveCircuit[ActiveActor].Solution.NodeV) Then
     Begin
          DoSimpleMsg('The circuit must be solved before you can do this.',24722);
          Exit;
     End;
     Quantity := vizCURRENT;
     Quantity := 1;
     ElemName := '';
      {Parse rest of command line}
     ParamPointer := 0;
     ParamName := UpperCase(Parser[ActiveActor].NextParam);
     Param := Parser[ActiveActor].StrValue;
     WHILE Length(Param)>0 DO
     Begin
         Unknown := False;
         IF   (Length(ParamName) = 0) THEN Inc(ParamPointer)

         ELSE Begin
             If  CompareTextShortest(ParamName, 'WHAT')=0 then ParamPointer:=1
             ELSE If  CompareTextShortest(ParamName, 'ELEMENT')=0 then ParamPointer:=2
             ELSE Unknown := TRUE;
         End;

         If Not Unknown then
         CASE ParamPointer OF
           1: Case Lowercase(Param)[1] of
{$IFDEF MSWINDOWS}
                'c':  Quantity := vizCURRENT;
                'v':  Quantity := vizVOLTAGE;
                'p':  Quantity := vizPOWER;
{$ELSE}
                'c':  Quantity := 1;
                'v':  Quantity := 2;
                'p':  Quantity := 3;
{$ENDIF}
               End;
           2: ElemName := Param;
         ELSE
             // ignore unnamed and extra parms
         End;

         ParamName := UpperCase(Parser[ActiveActor].NextParam);
         Param := Parser[ActiveActor].StrValue;
     End;  {WHILE}

     {--------------------------------------------------------------}

     Devindex := GetCktElementIndex(ElemName); // Global function
     IF DevIndex > 0 THEN Begin  //  element must already exist
        pElem := ActiveCircuit[ActiveActor].CktElements.Get(DevIndex);
        If pElem is TDSSCktElement Then Begin
           DSSPlotObj.DoVisualizationPlot(TDSSCktElement(pElem), Quantity);
        End Else Begin
          DoSimpleMsg(pElem.Name + ' must be a circuit element type!', 282);   // Wrong type
        End;
     End Else Begin
        DoSimpleMsg('Requested Circuit Element: "' + ElemName + '" Not Found.',282 ); // Did not find it ..
     End;
{$ENDIF}
End;

FUNCTION DoCloseDICmd:Integer;

Begin
    Result  := 0;
    EnergyMeterClass[ActiveActor].CloseAllDIFiles(ActiveActor);
End;

FUNCTION DoADOScmd:Integer;

Begin
    Result  := 0;
    DoDOScmd(Parser[ActiveActor].Remainder);
End;

FUNCTION DoEstimateCmd:Integer;



Begin
    Result := 0;

    {Load current Estimation is driven by Energy Meters at head of feeders.}
    DoAllocateLoadsCmd(ActiveActor);

    {Let's look to see how well we did}
     If not AutoShowExport Then DSSExecutive[ActiveActor].Command := 'Set showexport=yes';
     DSSExecutive[ActiveActor].Command := 'Export Estimation';

End;



FUNCTION DoReconductorCmd:Integer;

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
     ParamName := Parser[ActiveActor].NextParam;
     Param := Parser[ActiveActor].StrValue;
     while Length(Param) > 0 do Begin
       IF Length(ParamName) = 0 THEN Inc(ParamPointer)
       ELSE ParamPointer := ReconductorCommands.GetCommand(ParamName);

       Case ParamPointer of
          1: Line1 := Param;
          2: Line2 := Param;
          3: Begin Linecode := Param; LineCodeSpecified := TRUE; GeometrySpecified := FALSE; End;
          4: Begin Geometry := Param; LineCodeSpecified := FALSE; GeometrySpecified := TRUE; End;
          5: MyEditString := Param;
          6: Nphases := Parser[ActiveActor].IntValue;
       Else
          DoSimpleMsg('Error: Unknown Parameter on command line: '+Param, 28701);
       End;

      ParamName := Parser[ActiveActor].NextParam;
      Param := Parser[ActiveActor].StrValue;
     End;

     {Check for Errors}

     {If user specified full line name, get rid of "line."}
     Line1 := StripClassName(Line1);
     Line2 := StripClassName(Line2);

     If (Length(Line1)=0) or (Length(Line2)=0) then Begin
       DoSimpleMsg('Both Line1 and Line2 must be specified!', 28702);
       Exit;
     End;

     If (Not LineCodeSpecified) and (Not GeometrySpecified) then Begin
       DoSimpleMsg('Either a new LineCode or a Geometry must be specified!', 28703);
       Exit;
     End;

     LineClass := DSSClassList[ActiveActor].Get(ClassNames[ActiveActor].Find('Line'));
     pLine1 := LineClass.Find(Line1);
     pLine2 := LineCLass.Find(Line2);

     If (pLine1 = Nil) or (pLine2=NIL) then Begin
       If pLine1=Nil then doSimpleMsg('Line.'+Line1+' not found.', 28704)
       Else If pLine2=Nil then doSimpleMsg('Line.'+Line2+' not found.', 28704);
       Exit;
     End;

     {Now check to make sure they are in the same meter's zone}
     If (pLine1.MeterObj=Nil) or (pLine2.MeterObj=Nil)  then Begin
       DoSimpleMsg('Error: Both Lines must be in the same EnergyMeter zone. One or both are not in any meter zone.', 28705);
       Exit;
     End;

     If pLine1.MeterObj<>pline2.MeterObj then Begin
       DoSimpleMsg('Error: Line1 is in EnergyMeter.'+pLine1.MeterObj.Name+
                   ' zone while Line2 is in EnergyMeter.'+pLine2.MeterObj.Name+ ' zone. Both must be in the same Zone.', 28706);
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
          1: TraceAndEdit(pLine1, pLine2, NPhases, Editstring);
          2: TraceAndEdit(pLine2, pLine1, NPhases, Editstring);
     Else
         DoSimpleMsg('Traceback path not found between Line1 and Line2.', 28707);
         Exit;
     end;

End;

FUNCTION DoAddMarkerCmd:Integer;
Var
   ParamPointer :Integer;
   ParamName,
   Param:String;
   BusMarker:TBusMarker;

Begin
     Result := 0;
     ParamPointer := 0;

     BusMarker := TBusMarker.Create;
     ActiveCircuit[ActiveActor].BusMarkerList.Add(BusMarker);

     ParamName := Parser[ActiveActor].NextParam;
     Param := Parser[ActiveActor].StrValue;
     WHILE Length(Param)>0 DO
     Begin
         IF   (Length(ParamName) = 0)
         THEN Inc(ParamPointer)
         ELSE ParamPointer := AddmarkerCommands.GetCommand(ParamName);

         With BusMarker Do
         CASE ParamPointer OF
           1: BusName := Param;
           2: AddMarkerCode := Parser[ActiveActor].IntValue;
           3: AddMarkerColor:= InterpretColorName(Param);
           4: AddMarkerSize := Parser[ActiveActor].IntValue;

         ELSE
             // ignore unnamed and extra parms
         End;

         ParamName := Parser[ActiveActor].NextParam;
         Param := Parser[ActiveActor].StrValue;
     End;

End;

FUNCTION DoSetLoadAndGenKVCmd:Integer;
VAR
  pLoad :TLoadObj;
  pGen :TGeneratorObj;
  pBus :TDSSBus;
  sBus : String;
  iBus, i : integer;
  kvln : double;
Begin
  Result := 0;
  pLoad := ActiveCircuit[ActiveActor].Loads.First;
  WHILE pLoad <> NIL Do Begin
    ActiveLoadObj := pLoad; // for UpdateVoltageBases to work
    sBus := StripExtension (pLoad.GetBus(1));
    iBus := ActiveCircuit[ActiveActor].BusList.Find (sBus);
    pBus := ActiveCircuit[ActiveActor].Buses^[iBus];
    kvln := pBus.kVBase;
    if (pLoad.Connection = 1) Or (pLoad.NPhases = 3) then
      pLoad.kVLoadBase := kvln * sqrt (3.0)
    else
      pLoad.kVLoadBase := kvln;
    pLoad.UpdateVoltageBases;
    pLoad.RecalcElementData(ActiveActor);
    pLoad := ActiveCircuit[ActiveActor].Loads.Next;
  End;

  For i := 1 to ActiveCircuit[ActiveActor].Generators.ListSize Do Begin
    pGen := ActiveCircuit[ActiveActor].Generators.Get(i);
    sBus := StripExtension (pGen.GetBus(1));
    iBus := ActiveCircuit[ActiveActor].BusList.Find (sBus);
    pBus := ActiveCircuit[ActiveActor].Buses^[iBus];
    kvln := pBus.kVBase;
    if (pGen.Connection = 1) Or (pGen.NPhases > 1) then
      pGen.PresentKV := kvln * sqrt (3.0)
    else
      pGen.PresentKV := kvln;
    pGen.RecalcElementData(ActiveActor);
  End;

End;

FUNCTION DoUuidsCmd:Integer;
Var
  F:TextFile;
  ParamName, Param, S, NameVal, UuidVal, DevClass, DevName: String;
  pName: TNamedObject;
  idx: integer;
begin
  StartUuidList (ActiveCircuit[ActiveActor].NumBuses + 2 * ActiveCircuit[ActiveActor].NumDevices);
  Result := 0;
  ParamName := Parser[ActiveActor].NextParam;
  Param := Parser[ActiveActor].StrValue;
  Try
    AssignFile(F, Param);
    Reset(F);
    AuxParser[ActiveActor].Delimiters := ',';
    While not EOF(F) Do Begin
      Readln(F, S);
      With AuxParser[ActiveActor] Do Begin
        pName := nil;
        CmdString := S;
        NextParam;  NameVal := StrValue;
        NextParam;  UuidVal := StrValue;
        // format the UUID properly
        if Pos ('{', UuidVal) < 1 then
          UuidVal := '{' + UuidVal + '}';
        if Pos ('=', NameVal) > 0 then begin  // it's a non-identified object in OpenDSS
          AddHashedUuid (NameVal, UuidVal);
        end else begin  // find this as a descendant of TNamedObject
          pName := nil;
          ParseObjectClassAndName (NameVal, DevClass, DevName);
          IF CompareText (DevClass, 'circuit')=0 THEN begin
            pName := ActiveCircuit[ActiveActor]
          end else if CompareText (DevClass, 'Bus')=0 then begin
            idx := ActiveCircuit[ActiveActor].BusList.Find (DevName);
            pName := ActiveCircuit[ActiveActor].Buses^[idx];
          end else begin
          LastClassReferenced[ActiveActor] := ClassNames[ActiveActor].Find (DevClass);
          ActiveDSSClass[ActiveActor] := DSSClassList[ActiveActor].Get(LastClassReferenced[ActiveActor]);
          if ActiveDSSClass[ActiveActor] <> nil then
            if ActiveDSSClass[ActiveActor].SetActive (DevName) then
              pName := ActiveDSSClass[ActiveActor].GetActiveObj;
          end;
          // re-assign its UUID
          if pName <> nil then pName.UUID := StringToUuid (UuidVal);
        end;
      End;
    End;
  Finally
    AuxParser[ActiveActor].ResetDelims;
    CloseFile(F);
  End;
End;

FUNCTION DoCvrtLoadshapesCmd:Integer;
Var
   pLoadshape :TLoadShapeObj;
   iLoadshape :Integer;
   LoadShapeClass :TLoadShape;
   ParamName      :String;
   Param          :String;
   Action         :String;
   F              :TextFile;
   Fname          :String;

Begin
    ParamName := Parser[ActiveActor].NextParam;
    Param := Parser[ActiveActor].StrValue;

    If length(param)=0 then  Param := 's';

    {Double file or Single file?}
    CASE lowercase(param)[1] of
        'd': Action := 'action=dblsave';
    ELSE
        Action := 'action=sngsave';   // default
    END;

     LoadShapeClass := GetDSSClassPtr('loadshape') as TLoadShape;

     Fname := 'ReloadLoadshapes.DSS';
     AssignFile(F, Fname);
     Rewrite(F);

     iLoadshape := LoadShapeClass.First;
     while iLoadshape > 0 do  Begin
        pLoadShape := LoadShapeClass.GetActiveObj;
        Parser[ActiveActor].CmdString := Action;
        pLoadShape.Edit(ActiveActor);
        Writeln(F, Format('New Loadshape.%s Npts=%d Interval=%.8g %s',[pLoadShape.Name, pLoadShape.NumPoints, pLoadShape.Interval, GlobalResult]));
        iLoadshape := LoadShapeClass.Next;
     End;

     CloseFile(F);
     FireOffEditor(Fname);
     Result := 0;
End;

FUNCTION DoNodeDiffCmd:Integer;

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


Begin

    Result := 0;
    ParamName := Parser[ActiveActor].NextParam;
    Param := Parser[ActiveActor].StrValue;
    sNode1 := Param;
    If Pos('2',ParamName)>0 then sNode2 := Param;

    ParamName := Parser[ActiveActor].NextParam;
    Param := Parser[ActiveActor].StrValue;
    sNode2 := Param;
    If Pos('1',ParamName)>0 then sNode1 := Param;

    // Get first node voltage
    AuxParser[ActiveActor].Token := sNode1;
    NodeBuffer[1] := 1;
    sBusName := AuxParser[ActiveActor].ParseAsBusName (numNodes,  @NodeBuffer,ActiveActor);
    iBusidx := ActiveCircuit[ActiveActor].Buslist.Find(sBusName);
    If iBusidx>0 Then Begin
        B1Ref := ActiveCircuit[ActiveActor].Buses^[iBusidx].Find(NodeBuffer[1])
    End Else Begin
        DoSimpleMsg(Format('Bus %s not found.',[sBusName]), 28709);
        Exit;
    End;

    V1 := ActiveCircuit[ActiveActor].Solution.NodeV^[B1Ref];

    // Get 2nd node voltage
    AuxParser[ActiveActor].Token := sNode2;
    NodeBuffer[1] := 1;
    sBusName := AuxParser[ActiveActor].ParseAsBusName (numNodes,  @NodeBuffer,ActiveActor);
    iBusidx := ActiveCircuit[ActiveActor].Buslist.Find(sBusName);
    If iBusidx>0 Then Begin
        B2Ref := ActiveCircuit[ActiveActor].Buses^[iBusidx].Find(NodeBuffer[1])
    End Else Begin
        DoSimpleMsg(Format('Bus %s not found.',[sBusName]), 28710);
        Exit;
    End;

    V2 := ActiveCircuit[ActiveActor].Solution.NodeV^[B2Ref];

    VNodeDiff := CSub(V1, V2);
    GlobalResult := Format('%.7g, V,    %.7g, deg  ',[Cabs(VNodeDiff), CDang(VNodeDiff) ]);

End;

FUNCTION DoRephaseCmd:Integer;
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
     ScriptfileName := 'RephaseEditScript.DSS';
     TransfStop     := TRUE;  // Stop at Transformers

     ParamName      := Parser[ActiveActor].NextParam;
     Param          := Parser[ActiveActor].StrValue;
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
          DoSimpleMsg('Error: Unknown Parameter on command line: '+Param, 28711);
       End;

      ParamName := Parser[ActiveActor].NextParam;
      Param := Parser[ActiveActor].StrValue;
     End;

     LineClass := DSSClassList[ActiveActor].Get(ClassNames[ActiveActor].Find('Line'));
     pStartLine := LineClass.Find(StripClassName(StartLine));
     If pStartLine=Nil then  Begin
         DosimpleMsg('Starting Line ('+StartLine+') not found.', 28712);
         Exit;
     End;
     {Check for some error conditions and abort if necessary}
     If pStartLine.MeterObj=Nil then  Begin
         DosimpleMsg('Starting Line must be in an EnergyMeter zone.', 28713);
         Exit;
     End;

     If not (pStartLine.MeterObj is TEnergyMeterObj) then  Begin
         DosimpleMsg('Starting Line must be in an EnergyMeter zone.', 28714);
         Exit;
     End;

     GoForwardandRephase(pStartLine, NewPhases, MyEditString, ScriptfileName, TransfStop);

End;

FUNCTION DoSetBusXYCmd:Integer;

Var
     Param          :String;
     ParamName      :String;
     ParamPointer   :Integer;
     BusName        :String;
     Xval           :Double;
     Yval           :Double;
     iB             :Integer;

Begin

     Result := 0;
     ParamName      := Parser[ActiveActor].NextParam;
     Param          := Parser[ActiveActor].StrValue;
     ParamPointer   := 0;
     Xval := 0.0;  Yval := 0.0;
     while Length(Param) > 0 do Begin
       IF Length(ParamName) = 0 THEN Inc(ParamPointer)
       ELSE ParamPointer := SetBusXYCommands.GetCommand(ParamName);

       Case ParamPointer of
          1: BusName := Param;
          2: Xval := Parser[ActiveActor].DblValue;
          3: Yval := Parser[ActiveActor].DblValue;
       Else
          DoSimpleMsg('Error: Unknown Parameter on command line: '+Param, 28721);
       End;

       iB := ActiveCircuit[ActiveActor].Buslist.Find(BusName);
       If iB >0 Then  Begin
           With ActiveCircuit[ActiveActor].Buses^[iB] Do Begin     // Returns TBus object
             x := Xval;
             y := Yval;
             CoordDefined := TRUE;
           End;
       End Else Begin
           DosimpleMsg('Error: Bus "' + BusName + '" Not Found.', 28722);
       End;

      ParamName := Parser[ActiveActor].NextParam;
      Param := Parser[ActiveActor].StrValue;
     End;


End;


FUNCTION DoFNCSPubCmd:Integer;
{$IFDEF FPC}
Var
  Param          :String;
  ParamName      :String;
  ParamPointer   :Integer;
  FileName       :String;
Begin
  Result := 0;
  ParamName      := Parser[ActiveActor].NextParam;
  Param          := Parser[ActiveActor].StrValue;
  ParamPointer   := 0;
  while Length(Param) > 0 do Begin
    IF Length(ParamName) = 0 THEN Inc(ParamPointer)
    ELSE ParamPointer := FNCSPubCommands.GetCommand(ParamName);

    Case ParamPointer of
       1: FileName := Param;
    Else
       DoSimpleMsg('Error: Unknown Parameter on command line: '+Param, 28728);
    End;
    ParamName := Parser[ActiveActor].NextParam;
    Param := Parser[ActiveActor].StrValue;
  End;
  if Assigned (ActiveFNCS) then begin
    if ActiveFNCS.IsReady then begin
      ActiveFNCS.ReadFncsPubConfig (FileName);
    end;
  end;
{$ELSE}
Begin
  DoSimpleMsg('Error: FNCS only supported in the Free Pascal version', 28728);
{$ENDIF}
End;


FUNCTION DoUpdateStorageCmd:Integer;

Begin
       StorageClass[ActiveActor].UpdateAll(ActiveActor);
       Result := 0;
End;

FUNCTION DoUpdateStorage2Cmd:Integer;

Begin
       StorageClass[ActiveActor].UpdateAll(ActiveActor);
       Result := 0;
End;

FUNCTION DoPstCalc;

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

Begin

     Result := 0;
     Varray   := nil;
     PstArray := nil;
     Npts   := 0;
     Lamp   := 120;  // 120 or 230
     CyclesPerSample := 60;
     Freq := DefaultBaseFreq;

     ParamName      := Parser[ActiveActor].NextParam;
     Param          := Parser[ActiveActor].StrValue;
     ParamPointer   := 0;
     while Length(Param) > 0 do Begin
         IF    Length(ParamName) = 0 THEN Inc(ParamPointer)
         ELSE  ParamPointer := PstCalcCommands.GetCommand(ParamName);
         // 'Npts', 'Voltages', 'cycles', 'lamp'
         Case ParamPointer of
            1: Begin
                 Npts  := Parser[ActiveActor].IntValue;
                 Reallocmem(Varray, SizeOf(Varray^[1])*Npts);
               End;
            2: Npts    := InterpretDblArray(Param, Npts, Varray);
            3: CyclesPerSample := Round(ActiveCircuit[ActiveActor].Solution.Frequency * Parser[ActiveActor].dblvalue);
            4: Freq   := Parser[ActiveActor].DblValue;
            5: Lamp    := Parser[ActiveActor].IntValue;
         Else
            DoSimpleMsg('Error: Unknown Parameter on command line: '+Param, 28722);
         End;

        ParamName := Parser[ActiveActor].NextParam;
        Param := Parser[ActiveActor].StrValue;
     End;

     If Npts>10 Then
     Begin

         nPst := PstRMS(PstArray, Varray, Freq, CyclesPerSample, Npts, Lamp);
         // put resulting pst array in the result string
         S := '';
         For i := 1 to nPst Do  S := S + Format('%.8g, ', [PstArray^[i]]);
         GlobalResult := S;
     End
     Else DoSimpleMsg('Insuffient number of points for Pst Calculation.', 28723);


     Reallocmem(Varray,   0);   // discard temp arrays
     Reallocmem(PstArray, 0);
End;

FUNCTION DoLambdaCalcs:Integer;
{Execute fault rate and bus number of interruptions calc}

Var pMeter : TEnergyMeterObj;
    i      : Integer;
    ParamName,
    Param  : String;
    AssumeRestoration : Boolean;

Begin
      Result := 0;

// Do for each Energymeter object in active circuit
      pMeter := ActiveCircuit[ActiveActor].EnergyMeters.First;
      If pMeter=nil Then Begin
        DoSimpleMsg('No EnergyMeter Objects Defined. EnergyMeter objects required for this function.',28724);
        Exit;
      End;

      ParamName := Parser[ActiveActor].NextParam;
      Param := Parser[ActiveActor].StrValue ;

      If Length(Param)>0 Then
          Assumerestoration := InterpretYesNo(param)
      Else
          Assumerestoration := False;

       // initialize bus quantities
       With ActiveCircuit[ActiveActor] Do
       For i := 1 to NumBuses Do
         With Buses^[i] Do Begin
            BusFltRate        := 0.0;
            Bus_Num_Interrupt := 0.0;
         End;

      while pMeter <> Nil do Begin
         pMeter.CalcReliabilityIndices(AssumeRestoration, ActiveActor);
         pMeter := ActiveCircuit[ActiveActor].EnergyMeters.Next;
      End;
End;

FUNCTION DoVarCmd:Integer;
{Process Script variables}

VAR
   ParamName:String;
   Param:String;
   Str  : String;
   iVar : Integer;
   MsgStrings : TStringList;

Begin

     Result := 0;

     ParamName := Parser[ActiveActor].NextParam;
     Param := Parser[ActiveActor].StrValue;

      If Length(Param)=0 Then  // show all vars
      Begin
          If NoFormsAllowed Then Exit;
          {
          MsgStrings := TStringList.Create;
          MsgStrings.Add('Variable, Value');
          for iVar := 1 to ParserVars.NumVariables  do
              MsgStrings.Add(ParserVars.VarString[iVar] );
          ShowMessageForm(MsgStrings);
          MsgStrings.Free;}
          Str := 'Variable, Value' + CRLF;
          for iVar := 1 to ParserVars.NumVariables do
            Str := Str + ParserVars.VarString[iVar]+CRLF;

          DoSimpleMsg(Str, 999345);


      End Else if Length(ParamName)=0 then   // show value of this var
      Begin
           GlobalResult := Param;  // Parser substitutes @var with value
      End
      Else Begin
           WHILE Length(ParamName)>0 Do Begin
               case ParamName[1] of
                  '@': ParserVars.Add(ParamName, Param);
               else
                   DosimpleMsg('Illegal Variable Name: ' + ParamName + '; Must begin with "@"', 28725);
                   Exit;
               end;
               ParamName := Parser[ActiveActor].NextParam;
               Param := Parser[ActiveActor].StrValue;
           End;

      End;


End;

FUNCTION DoRemoveCmd:Integer;
Var
   ParamName    :String;
   Param        :String;
   Str          :String;
   i,
   ParamPointer :Integer;
   DeviceIndex  :Integer;

   FElementName :String;
   ElmFound,
   FKeepLoad    :Boolean;
   FEditString  :String;

   pPDElem      :TPDelement;
   pMeter       :TEnergyMeterObj;
   FMeterName   :String;


Begin

     Result := 0;

     FKeepLoad := TRUE;
     ParamPointer := 0;

     ParamName := Parser[ActiveActor].NextParam;
     Param := Parser[ActiveActor].StrValue;

     WHILE Length(Param)>0 DO
     Begin
         IF   (Length(ParamName) = 0)  THEN Inc(ParamPointer)
         ELSE ParamPointer := RemoveCommands.GetCommand(ParamName);

         CASE ParamPointer OF
           1: FElementName := Param; {ElementName}
           2: FkeepLoad := InterpretYesNo(Param); {KeepLoad}
           3: FEditString := Param; {EditString}
         ELSE

         End;

         ParamName := Parser[ActiveActor].NextParam;
         Param := Parser[ActiveActor].StrValue;
     End;

     // Check for existence of FelementName
     DeviceIndex := GetCktElementIndex(FElementName);
     if DeviceIndex = 0  then
     Begin
         DoSimpleMsg('Error: Element '+ FelementName + ' does not exist in this circuit.', 28726);
     End
     Else
     Begin // Element exists  GO!
     // first, checks if the element is not linked to an energy meter, if it does, abort (added 01/06/2020 -DM)
        ElmFound  :=  False;
        If ActiveCircuit[ActiveActor] <> Nil Then
        With ActiveCircuit[ActiveActor] Do
        Begin
          pMeter := EnergyMeters.First;
          for i := 1 to EnergyMeters.ListSize do
          Begin
            if AnsiLowerCase(pMeter.ElementName) = AnsiLowerCase(FElementName) then
            Begin
              ElmFound   := True;
              break;
            End
            Else pMeter := EnergyMeters.Next;
          End;
        End;
        if not ElmFound then
        Begin
        // Set CktElement active
          SetObject(FelementName);

        // Get Energymeter associated with this element.
          if ActiveCircuit[ActiveActor].ActiveCktElement is TPDElement then Begin
            pPDElem := ActiveCircuit[ActiveActor].ActiveCktElement as TPDElement;
            if pPDElem.SensorObj = Nil then DoSimpleMsg(Format('Element %s.%s is not in a meter zone! Add an Energymeter. ',[pPDelem.Parentclass.Name, pPDelem.name  ]),287261)
            Else Begin
              FMeterName := Format('%s.%s',[pPDElem.SensorObj.ParentClass.Name, pPDElem.SensorObj.Name]);
              SetObject(FMeterName);

              if ActiveCircuit[ActiveActor].ActiveCktElement is TEnergyMeterObj then Begin
                  pMeter := ActiveCircuit[ActiveActor].ActiveCktElement as TEnergyMeterObj;
                  // in ReduceAlgs
                  DoRemoveBranches(pMeter.BranchList, pPDelem, FKeepLoad, FEditString);
              End
              Else DoSimpleMsg('Error: The Sensor Object for '+ FelementName + ' is not an EnergyMeter object', 28727);
            End;
          End
          Else DoSimpleMsg('Error: Element '+ FelementName + ' is not a power delivery element (PDElement)', 28728);
        End
        Else DoSimpleMsg('Error: Element '+ FelementName + ' is tied to an Energy Meter.', 28800);

     End;


End;

initialization

{Initialize Command lists}

    SaveCommands := TCommandList.Create(['class', 'file', 'dir', 'keepdisabled']);
    SaveCommands.Abbrev := True;
    DI_PlotCommands := TCommandList.Create(['case','year','registers','peak','meter']);
    DistributeCommands := TCommandList.Create(['kW','how','skip','pf','file','MW','what']);
    DistributeCommands.Abbrev := True;

    ReconductorCommands := TCommandList.Create(['Line1', 'Line2', 'LineCode', 'Geometry', 'EditString', 'Nphases']);
    ReconductorCommands.Abbrev := True;

    RephaseCommands := TCommandList.Create(['StartLine', 'PhaseDesignation', 'EditString', 'ScriptFileName', 'StopAtTransformers']);
    RephaseCommands.Abbrev := True;

    AddMarkerCommands := TCommandList.Create(['Bus', 'code', 'color', 'size']);
    AddMarkerCommands.Abbrev := True;

    SetBusXYCommands := TCommandList.Create(['Bus', 'x', 'y']);
    SetBusXYCommands.Abbrev := True;

    PstCalcCommands := TCommandList.Create(['Npts', 'Voltages', 'dt', 'Frequency', 'lamp']);
    PstCalcCommands.abbrev := True;

    FNCSPubCommands := TCommandList.Create(['Fname']);
    FNCSPubCommands.abbrev := True;

    RemoveCommands := TCommandList.Create(['ElementName', 'KeepLoad', 'Editstring']);
    RemoveCommands.abbrev := True;

finalization

    DistributeCommands.Free;
    DI_PlotCommands.Free;
    SaveCommands.Free;
    AddMarkerCommands.Free;
    ReconductorCommands.Free;
    RephaseCommands.Free;
    SetBusXYCommands.Free;
    PstCalcCommands.Free;
    FNCSPubCommands.Free;
    RemoveCommands.Free;

end.




