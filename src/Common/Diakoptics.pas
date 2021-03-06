unit Diakoptics;
{
   ----------------------------------------------------------
  Copyright (c) 2008-2019, Electric Power Research Institute, Inc.
  All rights reserved.
  ----------------------------------------------------------
}

{

}
interface

uses
  DSSClass, Circuit, Solution, DSSGlobals, SysUtils, DSSClassDefs, EnergyMeter,
  SolutionAlgs, Line,
  {$IFDEF FPC}CmdForms{$ELSE}DSSForms, ScriptEdit{$ENDIF};

Function Solve_Diakoptics(DSS: TDSSContext):Integer;
Function ADiakoptics_Tearing(DSS: TDSSContext): Integer;
procedure ADiakopticsInit(DSS: TDSSContext);
function Calc_C_Matrix(DSS: TDSSContext; PLinks : PString; NLinks  : Integer):Integer;
function Calc_ZLL(DSS: TDSSContext; PLinks : PString; NLinks  : Integer):Integer;
procedure Calc_ZCC(DSS: TDSSContext; Links : Integer);
procedure Calc_Y4(DSS: TDSSContext);
procedure SendIdx2Actors(DSS: TDSSContext);
Function get_Statistics(DSS: TDSSContext): String;

implementation

Uses
  DSSHelper, ExecHelper, Executive, ParserDel, YMatrix, KLUSolve, Ucomplex, Sparse_Math,
  UcMatrix, math,
  Dynamics; // For TSolveMode



{*******************************************************************************
*              This is the A-Diakoptics algorithm executed by the              *
*                        Coordinator (Actor = 1)                               *
*******************************************************************************}

Function Solve_Diakoptics(DSS: TDSSContext):Integer;
var
  i,
  j,
  k         : Integer;
  Vpartial  : TSparse_Complex; //TODO: migrate to KLUSolveX
Begin
  {Space left empty to implement the simplified Diakoptics algorithm}
  With DSS.ActiveCircuit, Solution do
  Begin
     // This is the coordinator actor in A-Diakoptics mode
    FOR i := 1 TO NumberOfTimes Do
    Begin
      if AD_Init then
      Begin
      // Loads the partial solution considering the previous iteration
        VPartial  :=  Contours.Transpose();
        VPartial  :=  Vpartial.multiply(V_0);
        Vpartial  :=  Y4.multiply(VPartial);
        Ic        :=  Contours.multiply(VPartial);
      // Ready to go
      End
      else
      Begin
      // Setups the other actors to match the options of the coordinator
        for j := 1 to DSS.NumOfActors - 1 do
        Begin
          DSS.Children[j].ActiveCircuit.Solution.Mode                  :=  Mode;
          DSS.Children[j].ActiveCircuit.solution.DynaVars.h            :=  DynaVars.h;
          DSS.Children[j].ActiveCircuit.solution.DynaVars.intHour      :=  DynaVars.intHour;
          DSS.Children[j].ActiveCircuit.solution.DynaVars.t            :=  DynaVars.t;
          DSS.Children[j].ActiveCircuit.solution.MaxIterations         :=  MaxIterations;
          DSS.Children[j].ActiveCircuit.solution.MaxControlIterations  :=  MaxControlIterations;
          DSS.Children[j].ActiveCircuit.solution.ControlMode           :=  ControlMode;
          DSS.Children[j].ActiveCircuit.solution.NumberOfTimes         :=  1;
        End;
        AD_Init   :=  True;
      End;
    // Starts the simulation
      for j := 1 to DSS.NumOfActors - 1 do
      Begin
        DSS.CmdResult := DSS.Children[j].DSSExecutive.DoSolveCmd();
      End;
      Wait4Actors(DSS, AD_Actors);
      // The other routines
      DSS.ActorPctProgress  :=  (i*100) div NumberofTimes;
      if DSS.SolutionAbort then
        break
      else
        DSS.ActiveCircuit.IsSolved :=  True;
      Increment_time;
    End;
  End;

  if DSS.SolutionAbort then 
    DSS.ActiveCircuit.IsSolved :=  False;
  DSS.ActiveChild := DSS;
  DSS.ActiveChildIndex := 0;
  Result  :=  0;
End;

{*******************************************************************************
*              Returns a string with the partitioning statistics               *
*                  It only works if the partitioning was succesful             *
*******************************************************************************}
Function get_Statistics(DSS: TDSSContext): String;
var
  unbalance,
  ASize     : Array of single;
  idx       : Integer;
  GReduct,
  MaxImbal,
  AvgImbal  : Double;
Begin
  setlength(ASize,0);
  for idx := 1 to DSS.NumOfActors-1 do
  Begin
    setlength(ASize,length(ASize) + 1);
    ASize[high(ASize)]  :=  DSS.Children[idx].ActiveCircuit.NumNodes;
  End;
  GReduct  :=  (1 - (MaxValue(ASize)/DSS.ActiveCircuit.NumNodes))*100;   // The biggest actor
  setlength(unbalance,length(ASize));
  for idx := 0 to High(ASize) do
    unbalance[idx]  :=  (1 - (ASize[idx]/MaxValue(ASize))) * 100; // All the unbalances
  MaxImbal  :=  MaxValue(unbalance);                              // Max imbalance
  AvgImbal  :=  mean(unbalance);                                  // Average
  // publishes the results
  Result    :=  CRLF +
                'Circuit reduction    (%): ' + floattostrf(GReduct, ffgeneral, 4 ,2) + CRLF +
                'Max imbalance       (%): ' + floattostrf(MaxImbal, ffgeneral, 4 ,2) + CRLF +
                'Average imbalance(%): ' + floattostrf(AvgImbal, ffgeneral, 4 ,2) + CRLF;
End;


{*******************************************************************************
*              Sets the memory index for each actor so they can write          *
*                   directly into the coordinator's Voltage vector             *
*******************************************************************************}
procedure SendIdx2Actors(DSS: TDSSContext);
VAR
   i,j,k      : Integer;
   BusName    : String;
   AllNNames  : Array of String;
Begin
// Gets the names of the nodes in the interconnected system
  setlength(AllNNames,0);
  With DSS.ActiveCircuit do
  Begin
       FOR i := 1 to NumBuses DO
       Begin
           BusName := BusList.Get(i);
           FOR j := 1 to Buses^[i].NumNodesThisBus DO
           Begin
                setlength(AllNNames,(length(AllNNames) + 1));
                AllNNames[high(AllNNames)] := BusName + '.' + IntToStr(Buses^[i].GetNum(j));
           End;
       End;
  End;
// Sets the index for each actor
// The feeder head first
  DSS.ActiveCircuit.VIndex :=  0;
// Then checks the rest of the actors
  for i := 1 to DSS.NumOfActors - 1 do
  Begin
    BusName :=  DSS.Children[i].ActiveCircuit.BusList.Get(1) + '.1';
    // Looks for the node within all the Node Names in the interconnected model
    for j := 0 to High(AllNNames) do
      if BusName = AllNNames[j] then  
        Break;
    DSS.Children[i].ActiveCircuit.VIndex :=  j;
  End;
  // Initializes the Ic vector with zeros
  DSS.ActiveCircuit.Ic.sparse_matrix_Cmplx(length(AllNNames),1);
  DSS.ActiveCircuit.V_0.sparse_matrix_Cmplx(length(AllNNames),1);
  for i := 0 to High(AllNNames) do
  Begin
    DSS.ActiveCircuit.Ic.Insert(i,0,cZERO);
    DSS.ActiveCircuit.V_0.Insert(i,0,cZERO);
  End;
End;

{*******************************************************************************
*              Inverts ZCC to obtain its admittance equivalent Y4              *
*                      This is the heart of ADiakoptics                        *
*******************************************************************************}
procedure Calc_Y4(DSS: TDSSContext);
var
  value     : Complex;
  NumRows,
  NumCols,
  col,
  idx       : Integer;
  TempMat   : TcMatrix;
Begin
  WITH DSS.ActiveCircuit, Solution DO
  Begin
    //  Moves ZCC into an equivalent compatible with TcMatrix
    TempMat  :=  TCmatrix.CreateMatrix(ZCC.NRows);
    for idx := 0 to High(ZCC.CData) do
    Begin
      TempMat.SetElement(ZCC.CData[idx].Row + 1,ZCC.CData[idx].Col + 1,ZCC.CData[idx].Value);
    End;
    //  Inverts the ZCC equivalent
    TempMat.Invert;
    Y4.sparse_matrix_Cmplx(ZCC.NRows,ZCC.NCols);
    NumRows   :=  ZCC.NRows - 1;
    NumCols   :=  ZCC.NCols - 1;
    // Moves the inverse into Y4 for furhter use
    for idx := 0 to NumRows do
    Begin
      for col := 0 to NumCols do
      Begin
        value :=  TempMat.GetElement(idx+1,col+1);
        if (value.re <> 0) and (value.re <> 0) then
          Y4.insert(idx,col,value);
      End;
    End;
  End;
End;

{*******************************************************************************
*              Calculates the Connections matrix ZCC in the                    *
*                      contours-contours domain                                *
*******************************************************************************}
procedure Calc_ZCC(DSS: TDSSContext; Links : Integer);
var
  row,
  col,
  idx3,
  idx2,
  idx       : Integer;
  NNodes    : LongWord;
  CVector,
  ZVector   : pComplexArray;
  Ctemp     : Complex;
Begin
  WITH DSS.ActiveCircuit, Solution DO
  Begin
    GetSize(hY, @NNodes);
    col       :=  NNodes;
    dec(Links);
    ZCT.sparse_matrix_Cmplx(Links*3,col);
    CVector   :=  Allocmem(SizeOf(CVector^[1]) * (col+1));
    ZVector   :=  Allocmem(SizeOf(ZVector^[1]) * (col+1));
    idx3      :=  Links*3 - 1;

    for idx2 := 0 to idx3 do
    Begin

      for idx := 1 to col do CVector^[idx] :=  cZERO;  // Makes it zero

      for idx := 1 to length(Contours.CData) do
      Begin
        if Contours.CData[idx - 1].col = idx2 then
        Begin
          row                 := Contours.CData[idx - 1].row + 1;
          CVector^[row]       := Contours.CData[idx-1].Value;
        End;
      End;
      SolveSparseSet(hy,@ZVector^[1],@CVector^[1]);

      for idx := 1 to col do           // inserts result into the ZCT matrix
      Begin
        CTemp   :=  ZVector^[idx];
        if (CTemp.re <> 0) and (CTemp.im <> 0) then
           ZCT.insert(idx2,(idx-1),ZVector[idx]);
      End;
      idx :=  col;
    End;

    ZCC   :=  ZCT.multiply(Contours);    // Calculates ZCC with no Link impedances
    ZCC   :=  ZCC.Add(ZLL);              // Adds the link impedance

    FreeMem(CVector);
    FreeMem(ZVector);
  End;
End;

{*******************************************************************************
*                   Calculates the contours matrix based                       *
*             on the location in the graph of the link branches                *
*             if there is an error returns <> 0                                *
*******************************************************************************}
function Calc_C_Matrix(DSS: TDSSContext; PLinks : PString; NLinks  : Integer):Integer;
var
  LIdx,k,l,
  j,CDirection,
  NumPhases,
  i           : Integer;
  Elem_Buses,
  Node_Names  : Array of String;
  temp        : String;
  Go_Flag     : Boolean;
Begin
  DSS := DSS.GetPrime();
  DSS.ActiveChild := DSS;
  DSS.ActiveChildIndex := 0;
  WITH DSS.ActiveCircuit DO
  Begin
    Result    :=  0;
    setlength(Elem_Buses,2);
    setlength(Node_Names,0);
    FOR i := 1 to NumNodes DO
    Begin
      setlength(Node_Names,(length(Node_Names) + 1));
      With MapNodeToBus^[i] do
        Node_Names[High(Node_names)] := Format('%s.%-d',[lowercase(BusList.Get(Busref)), NodeNum]);
    End;

    Contours.sparse_matrix_Cmplx(length(Node_Names),(NLinks - 1)*3);

    for LIdx := 1 to (NLinks - 1) do
    Begin

      inc(PLinks);                  // Pointing to the Next link branch (starting in 1)
      temp      :=  string(PLinks^);
      j         :=  ansipos('.',temp);
      temp      :=  lowercase(copy(temp,0,(j - 1)));
      if (temp = 'line') then
      Begin
        i         :=  SetElementActive(string(PLinks^));
        // Gest the names of the buses fot this PDElement
        // If it is something different from a Transformer reports an error
        // Since a link branch cannot be a transformer
        For i := 1 to  ActiveCktElement.Nterms Do
        Begin
          Elem_Buses[i-1] := ActiveCktElement.GetBus(i);
          j               :=  ansipos('.',Elem_Buses[i-1]);
          if j <> 0 then
            Elem_Buses[i-1] :=  copy(Elem_Buses[i-1],0,j)
          else
            Elem_Buses[i-1] :=  Elem_Buses[i-1] + '.';
        End;
        //  Marks the connection point in the contours matrix
        NumPhases   :=  ActiveCktElement.NPhases;
        For l :=  1 to NumPhases Do
        Begin

          for i := 0 to 1 do
          Begin

            temp    :=  Elem_Buses[i] + inttostr(l);
            Go_Flag :=  True;
            j       :=  0;
            while Go_Flag and (j <= High(Node_Names)) do
            Begin

              k       :=  ansipos(temp,Node_Names[j]);
              if k  <>  0 then
              Begin
                if i  = 0 then CDirection :=  1
                else CDirection :=  -1;
                Contours.insert(j,((l-1) + (LIdx -1)*3),cmplx(CDirection,0));
                Go_Flag :=  False;
              End;
              inc(j);

            End;

          End;
        End;
      End
      else
      Begin
        Result  :=  -1; // There was an error when selecting the link branches (MeTIS)
        break;  // Abort
      End;
    End;
    // More error checking
    if Result = 0 then
      if Contours.NZero <> 0 then Result  :=  0
      else Result :=  1;

  End;
End;

{*******************************************************************************
*            Calculates the Link branches matrix for further use                *
*                if there is an error returns <> 0                             *
*******************************************************************************}
Function Calc_ZLL(DSS: TDSSContext; PLinks : PString; NLinks  : Integer):Integer;
var
  NValues,
  idx, k, j,
  row,col,
  count,
  i         : Integer;
  cValues   : pComplexArray;
  ErrorFlag : Boolean;
  localMat  : TSparse_Complex;
  LinkPrim  : TcMatrix;
Begin
  dec(NLinks);
  ErrorFlag :=  False;
  LinkPrim  :=  TCmatrix.CreateMatrix(3);
  DSS := DSS.GetPrime();
  DSS.ActiveChild := DSS;
  DSS.ActiveChildIndex := 0;
  WITH DSS.ActiveCircuit DO
  Begin
    ZLL.Sparse_matrix_Cmplx(NLinks*3,NLinks*3);
    for i := 1 to NLinks do
    Begin
      inc(PLinks);
      idx         :=  SetElementActive(string(PLinks^));

      If ActiveCktElement<>Nil THEN
        WITH ActiveCktElement Do
        Begin
          NValues := SQR(Yorder);
          cValues := GetYprimValues(ALL_YPRIM);  // Get pointer to complex array of values
          If cValues <> Nil Then
          Begin
            k     :=  1;
            idx   :=  (i-1)*3;
            row   :=  1;
            col   :=  1;
            count :=  0;
            // Extracts the YPrim of the Link branch
            FOR j := 1 to  (NValues div 4) DO
            Begin
              LinkPrim.SetElement(row,col,cValues^[k]);
              inc(count);
              if count > 2 then
              Begin
                inc(row);
                Col   :=  1;
                Count :=  0;
                k     :=  k + 4;
              End
              else
              Begin
                inc(Col);
                inc(k);
              End;
            End;
            // Inverts the Y primitive
            LinkPrim.Invert;
            // Inserts the Z primitive values into ZLL
            row   :=  0;
            col   :=  0;
            count :=  0;
            FOR j := 1 to  (NValues div 4) DO
            Begin
              ZLL.insert((row + idx),(col + idx),LinkPrim.GetElement(row+1,col+1));
              inc(count);
              if count > 2 then
              Begin
                inc(row);
                Col   :=  0;
                Count :=  0;
              End
              else
                inc(Col);
            End;
          End
          else
            ErrorFlag :=  True;

        End

    End;
    if ErrorFlag then Result  :=  1
    else Result  :=  0;

  End;
End;


{*******************************************************************************
*           Tears the system using considering the number of                   *
*           circuits specified by the user                                     *
*******************************************************************************}
Function ADiakoptics_Tearing(DSS: TDSSContext): Integer;
var
  Prev_Mode: TSolveMode;                              // Stores the previous solution mode
  Num_Ckts    : Integer;                  // Stores the number of Sub-Circuits created
Begin
  DSS := DSS.GetPrime();
  DSS.ActiveChild := DSS;
  DSS.ActiveChildIndex := 0;

  With DSS.ActiveCircuit, Solution do
  Begin
    Num_Ckts                      :=  Tear_Circuit();
    Prev_mode                     :=  Dynavars.SolutionMode;
    Dynavars.SolutionMode         :=  TSolveMode.SNAPSHOT;
    DSS.DSSExecutive.Command          :=  'set controlmode=off';
    Ymatrix.BuildYMatrix(DSS, WHOLEMATRIX, FALSE);
//    DoSolveCmd();
    if not DSS.SolutionAbort then
    Begin
      Save_SubCircuits();
      Dynavars.SolutionMode         :=  Prev_mode;  // Goes back to the previous solution mode
      DSS.ActiveCircuit.Num_SubCkts  :=  Num_Ckts;
      DSS.GlobalResult                  :=  'Sub-Circuits Created: ' + inttostr(Num_Ckts);
      Result                        :=  0;
    End
    else
    Begin
      DSS.GlobalResult                  := 'There was an error when tearing the circuit ';
      Result                        :=  1;
    End;
  End;
End;

{*******************************************************************************
*            Generates the subsystems, actors and memory space                 *
*                     For using the A-Diakoptics parallelism                   *
*******************************************************************************}
procedure ADiakopticsInit(DSS: TDSSContext);
var
  EMeter      : TEnergyMeterObj;
  j,
  Local_State,
  Num_States,
  ErrorCode,
  DIdx,
  Diak_Actors : Integer;
  Dir, Proj_Dir,
  prog_Str,
  ErrorStr,
  FileRoot    : String;
  Links       : Array of String;                        // List of the Link Branches
  MQuit       : Boolean;                                // To quit the State Machine
  {$IFNDEF FPC}
  ScriptEd    : TScriptEdit;
  {$ENDIF}

Begin
  // The program is built as a state machine to facilitate the error detection
  // and quitting the routines after an error is detected wihtout killing the prog
  MQuit       :=  False;
  Num_States  :=  9;                          // Number of states of the machine
  Local_State :=  0;                          // Current state
  prog_str    :=  'A-Diakoptics initialization summary:' + CRLF + CRLF;

  DSS := DSS.GetPrime();
  DSS.ActiveChild := DSS;
  DSS.ActiveChildIndex := 0;

  // Checks if the number of actors is within a reasonable limit
  if DSS.ActiveCircuit.Num_SubCkts > (CPU_Cores - 2) then
    DSS.ActiveCircuit.Num_SubCkts := CPU_Cores - 2;

  while(not MQuit) do
  Begin
    case Local_State of
      0: Begin                       // Create subcircuits

        prog_Str := prog_str + '- Creating Sub-Circuits...' + CRLF;

        ErrorCode :=  ADiakoptics_Tearing(DSS);
        if ErrorCode <> 0 then 
            ErrorStr := 'Error' + CRLF + 'The circuit cannot be decomposed' + CRLF
        else
            ErrorStr :=  '  ' + inttostr(DSS.ActiveCircuit.Num_SubCkts) + ' Sub-Circuits Created' + CRLF;

        prog_Str :=  prog_str + ErrorStr;

      End;
      1:  Begin                      // Saves the Link Branch list locally

        Diak_Actors :=  DSS.ActiveCircuit.Num_SubCkts + 1;
        prog_Str :=  prog_str + '- Indexing link branches...';

        setlength(Links,length(DSS.ActiveCircuit.Link_Branches));
        for DIdx := 0 to High(Links) do Links[DIdx]   :=  DSS.ActiveCircuit.Link_Branches[DIdx];

        prog_Str :=  prog_str + 'Done';
        ErrorCode :=  0;          // No error handling here

      End;
      2:  Begin                      // Compile subsystems

        ErrorCode   :=  0;
        prog_Str    :=  prog_str + CRLF + '- Setting up the Actors...';
        // Clears everything to create the actors and compile the subsystems
        DSS.Parallel_enabled                :=  False;
        DSS.DSSExecutive.ClearAll;
        Fileroot                        :=  OutputDirectory {CurrentDSSDir};    //  Gets the current directory
        DSS.SolutionAbort                   :=  False;

        // Compiles the interconnected Circuit for further calculations on actor 1
        ActiveActor                     :=  1;
        Proj_Dir                        :=  'compile "' + Fileroot + 'Torn_Circuit' + PathDelim + 'master_interconnected.dss"';
        DSS.DssExecutive.Command            :=  'set controlmode=Off';
        // Disables the Energymeters for the zones
        with DSS.ActiveCircuit, Solution do
        Begin
          EMeter    := EnergyMeters.First;
          while EMeter  <> Nil do
          begin
            j               :=  ansipos('zone_',EMeter.Name);
            if j <> 0 then  EMeter.Enabled  :=  False;
            EMeter          :=  EnergyMeters.Next;
          end;
        End;
        Ymatrix.BuildYMatrix(DSS, WHOLEMATRIX, FALSE);
        DSS.DSSExecutive.DoSolveCmd();

        // Creates the other actors
        for DIdx := 2 to Diak_Actors do
        Begin
          //New_Actor_Slot();

          if DIdx = 2 then 
            Dir :=  ''
          else
            Dir :=  'zone_' + inttostr(DIdx - 1) + PathDelim;

          Proj_Dir :=  'compile "' + Fileroot + PathDelim + 'Torn_Circuit' + PathDelim + Dir + 'master.dss"';
          DSS.DssExecutive.Command := Proj_Dir;
          if DIdx > 2 then
            DSS.DssExecutive.Command  := Links[DIdx - 2] + '.enabled=False';

          DSS.DssExecutive.Command :=  'set controlmode=Off';
          DSS.DssExecutive.DoSolveCmd();
          if DSS.SolutionAbort then
          Begin
            ErrorCode :=  1;
            Break;
          End;
        End;
        if ErrorCode <> 0 then ErrorStr := 'Error' + CRLF
                        + 'One or sub-systems cannot be compiled' + CRLF
        else ErrorStr :=  'Done';
        prog_Str    :=  prog_str + ErrorStr;

      end;
      3:  Begin                      // Creates the contours matrix
        prog_Str    :=  prog_str + CRLF + '- Building Contours...';
        // Builds the contour matrix
        ErrorCode :=  Calc_C_Matrix(DSS, @Links[0], length(Links));
        if ErrorCode <> 0 then ErrorStr := 'Error' + CRLF
                        + 'One or more link branches are not lines' + CRLF
        else ErrorStr :=  'Done';
        prog_Str    :=  prog_str + ErrorStr;

      end;
      4: Begin                       // Builds the ZLL matrix

        prog_Str    :=  prog_str + CRLF + '- Building ZLL...';
        ErrorCode :=  Calc_ZLL(DSS, @Links[0],length(Links));
        if ErrorCode <> 0 then ErrorStr := 'Error'
        else ErrorStr :=  'Done';
        prog_Str    :=  prog_str + ErrorStr;

      end;
      5:  Begin
        // Opens the link branches in the interconnected Circuit and recalculates the YBus
        // The opening happens by replacing the line with a very high series impedance
        prog_Str    :=  prog_str + CRLF + '- Opening link branches...';
        for DIdx := 1 to High(Links) do
        Begin
          DSS.DssExecutive.Command    :=  Links[DIdx] + '.r0=10000000';
          DSS.DssExecutive.Command    :=  Links[DIdx] + '.r1=10000000';
          DSS.DssExecutive.Command    :=  Links[DIdx] + '.x0=0';
          DSS.DssExecutive.Command    :=  Links[DIdx] + '.x1=0';
        End;
        Ymatrix.BuildYMatrix(DSS, WHOLEMATRIX, FALSE);
        prog_Str      :=  prog_str + 'Done';
        ErrorCode     :=  0;  // No error handling here

      end;
      6:  Begin // Builds the ZCC matrix

        prog_Str      :=  prog_str + CRLF + '- Building ZCC...';
        Calc_ZCC(DSS, length(Links));
        prog_Str      :=  prog_str + 'Done';

      End;
      7:  Begin // Inverts ZCC to get Y4

        prog_Str      :=  prog_str + CRLF + '- Building Y4 ...';
        Calc_Y4(DSS);
        prog_Str      :=  prog_str + 'Done';
        // Moves back the link branches list into actor 1 for further use
        setlength(DSS.ActiveCircuit.Link_Branches,length(Links));
        for DIdx := 0 to High(Links) do 
            DSS.ActiveCircuit.Link_Branches[DIdx] := Links[DIdx];
      End;
      8:  Begin                      // Sends the index to the actors for uploading info
        prog_Str      :=  prog_str + CRLF + '- Assigning indexes to actors ...';
        SendIdx2Actors(DSS);
        prog_Str      :=  prog_str + 'Done';
      End;
      9:  Begin                      // Prints the statistics of the partitioning
        prog_Str      :=  prog_str + CRLF + CRLF + 'Partitioning statistics';
        prog_Str      :=  prog_str + get_Statistics(DSS);
      End
      else
      Begin

      End;
    end;
    inc(Local_State);
    MQuit := (Local_State > Num_States) or (ErrorCode <> 0);
  End;

  DSS := DSS.GetPrime();
  if ErrorCode <> 0 then
  Begin
    ErrorStr          := 'One or more errors found';
    DSS.ADiakoptics       :=  False;
  End
  else
  Begin
    ErrorStr          :=  'A-Diakoptics initialized';
    DSS.Parallel_enabled  :=  True;
    DSS.ADiakoptics       :=  True;
  End;
//TODO?  ProgressCmd   :=  True;
  prog_Str      :=  CRLF + prog_str + CRLF + ErrorStr + CRLF;
  DSS.GlobalResult  :=  ErrorStr;

  //TODO: check -- the previous GlobalResult is ignored here...
  {$IFNDEF FPC}
  if not IsDLL
  then
    ScriptEd.PublishMessage(prog_Str)
  else
    DSS.GlobalResult  :=  prog_str;
  {$ELSE}
    DSS.GlobalResult  :=  prog_str;
  {$ENDIF}
  // TEMc: TODO: should we report something here under FPC?
  // Davis: Done: This will add the needed report

  DSS.SolutionAbort :=  False;

End;

end.
