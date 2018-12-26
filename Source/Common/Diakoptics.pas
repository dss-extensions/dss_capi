unit Diakoptics;

interface

uses
  Circuit, Solution, DSSGlobals, SysUtils, DSSClassDefs, 
  {$IFDEF FPC}CmdForms{$ELSE}DSSForms, ScriptEdit{$ENDIF};

Function Solve_Diakoptics():Integer;
Function ADiakoptics_Tearing(): Integer;
procedure ADiakopticsInit();
function Calc_C_Matrix(PLinks : PString; NLinks  : Integer):Integer;
function Calc_ZLL(PLinks : PString; NLinks  : Integer):Integer;
procedure Calc_ZCC();

implementation

Uses
  ExecHelper, Executive, ParserDel, YMatrix, KLUSolve, Ucomplex, Sparse_Math,
  UcMatrix;

Function Solve_Diakoptics():Integer;
Begin
  {Space left empty to implement the simplified Diakoptics algorithm}
  With ActiveCircuit[1].Solution do
  Begin

  End;
  Result  :=  0;
End;


{*******************************************************************************
*              Calculates the Connections matrix ZCC in the                    *
*                      contours-contours domain                                *
*******************************************************************************}
procedure Calc_ZCC();
var
  idx       : Integer;
  MSize     : LongWord;
  CVector,
  ZVector   : pNodeVArray;
Begin
  WITH ActiveCircuit[1], ActiveCircuit[1].Solution DO
  Begin
    GetSize(hY, @MSize);
    for idx := 1 to MSize do CVector^[idx] :=  cmplx(0,0);

    


  End;
End;

{*******************************************************************************
*                   Calculates the contours matrix based                       *
*             on the location in the graph of the link branches                *
*             if there is an error returns 0, otherwise 1                      *
*******************************************************************************}
function Calc_C_Matrix(PLinks : PString; NLinks  : Integer):Integer;
var
  LIdx,k,l,
  j,CDirection,
  i           : Integer;
  Elem_Buses,
  Node_Names  : Array of String;
  temp        : String;
  Go_Flag     : Boolean;

  myFile      : TextFile;         // For debugging
Begin
  ActiveActor   :=  1;
  WITH ActiveCircuit[ActiveActor] DO
  Begin

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

      i         :=  SetElementActive(string(PLinks^));
      // Gest the names of the buses fot this PDElement
      For i := 1 to  ActiveCktElement.Nterms Do
      Begin
        Elem_Buses[i-1] := ActiveCktElement.GetBus(i);
        j               :=  ansipos('.',Elem_Buses[i-1]);
        if j <> 0 then
          Elem_Buses[i-1] :=  copy(Elem_Buses[i-1],0,j);
      End;
      //  Marks the connection point in the contours matrix
      For l :=  1 to ActiveCktElement.NPhases Do
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
              Contours.insert(j,(l-1),cmplx(CDirection,0));
              Go_Flag :=  False;
            End;
            inc(j);

          End;

        End;

      End;

    End;
    if Contours.NZero <> 0 then Result  :=  0
    else Result :=  1;
  End;



End;

{*******************************************************************************
*            Calculates the Link brnahes matrix for further use                *
*                if there is an error returns 0, otherwise 1                   *
*******************************************************************************}
Function Calc_ZLL(PLinks : PString; NLinks  : Integer):Integer;
var
  NValues,
  idx, k, j,
  row,col,
  count,
  i         : Integer;
  cValues   : pComplexArray;
  ErrorFlag : Boolean;
  localMat  : TSparse_Complex;
  LinkPrim : TcMatrix;
Begin
  dec(NLinks);
  ErrorFlag :=  False;
  LinkPrim  :=  TCmatrix.CreateMatrix(3);
  ActiveActor   :=  1;
  WITH ActiveCircuit[ActiveActor] DO
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
*           available CPUs as reference                                        *
*******************************************************************************}
Function ADiakoptics_Tearing(): Integer;
var
  Prev_Mode,                              // Stores the previous solution mode
  Num_Ckts    : Integer;                  // Stores the number of Sub-Circuits created
Begin
  With ActiveCircuit[ActiveActor].Solution do
  Begin
    Num_Ckts                      :=  ActiveCircuit[ActiveActor].Tear_Circuit();
    Prev_mode                     :=  Dynavars.SolutionMode;
    Dynavars.SolutionMode         :=  0;          // Shapshot mode
    solve(ActiveActor);
    ActiveCircuit[ActiveActor].Save_SubCircuits();
    Dynavars.SolutionMode         :=  Prev_mode;  // Goes back to the previous solution mode
    ActiveCircuit[1].Num_SubCkts  :=  Num_Ckts;
    GlobalResult                  := 'Sub-Circuits Created: ' + inttostr(Num_Ckts);
    Result                        :=  0;          // No error handling here
  End;
End;

{*******************************************************************************
*            Generates the subsystems, actors and memory space                 *
*                     For using the A-Diakoptics parallelism                   *
*******************************************************************************}
procedure ADiakopticsInit();
var
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
  Num_States  :=  6;
  Local_State :=  0;
  prog_str  :=  'A-Diakoptics initialization sumary:' + CRLF + CRLF;
  ActiveActor                     :=  1;
  if ActiveCircuit[1].Num_SubCkts > (CPU_Cores - 2) then
    ActiveCircuit[1].Num_SubCkts    :=  CPU_Cores - 2;

  while(not MQuit) do
  Begin
    case Local_State of
      0: Begin                       // Create subcircuits
        prog_Str    :=  prog_str + '- Creating SubCircuits...' + CRLF;

        ErrorCode   :=  ADiakoptics_Tearing();
        prog_Str    :=  prog_str + '  ' + inttostr(ActiveCircuit[1].Num_SubCkts) + ' Sub-Circuits Created' + CRLF;

      End;
      1:  Begin                      // Saves the Link Branch list locally
        Diak_Actors                     :=  ActiveCircuit[1].Num_SubCkts + 1;
        prog_Str    :=  prog_str + '- Indexing link branches...';

        setlength(Links,length(ActiveCircuit[1].Link_Branches));
        for DIdx := 0 to High(Links) do Links[DIdx]   :=  ActiveCircuit[1].Link_Branches[DIdx];

        prog_Str    :=  prog_str + 'Done';
        ErrorCode   :=  0;          // No error handling here

      End;
      2:  Begin                      // Compile subsystems
        prog_Str    :=  prog_str + CRLF + '- Setting up the Actors...';
        // Clears everything to craete the actors and compile the subsystems
        DSSExecutive.ClearAll;
        Fileroot                        :=  GetCurrentDir;    //  Gets the current directory
        SolutionAbort                   :=  False;
        DssExecutive.Command            :=  'ClearAll';

        // Compiles the interconnected Circuit for further calculations on actor 1
        ActiveActor                     :=  1;
        Proj_Dir                        :=  'compile "' + Fileroot + '\Torn_Circuit\master_interconnected.dss"';
        DssExecutive.Command            :=  Proj_Dir;

        // Creates the other actors
        for DIdx := 2 to Diak_Actors do
        Begin
          inc(NumOfActors);
          ActiveActor           :=  NumOfActors;
          ActorCPU[ActiveActor] :=  ActiveActor -1;
          DSSExecutive          :=  TExecutive.Create;  // Make a DSS object
          Parser[ActiveActor]   :=  TParser.Create;
          DSSExecutive.CreateDefaultDSSItems;
          Parallel_enabled      :=  False;

          if DIdx = 2 then  Dir :=  ''
          else  Dir :=  'zone_' + inttostr(DIdx - 1) + '\';
          Proj_Dir              :=  'compile "' + Fileroot + '\Torn_Circuit\' + Dir + 'master.dss"';
          DssExecutive.Command  := Proj_Dir;
          if DIdx > 2 then
            DssExecutive.Command  := Links[DIdx - 2] + '.enabled=False';
          DssExecutive.Command  :=  'solve';
        End;
        prog_Str    :=  prog_str + 'Done';
        ErrorCode   :=  0;
      end;
      3:  Begin                      // Creates the contours matrix
        ActiveActor                     :=  1;
        prog_Str    :=  prog_str + CRLF + '- Building Contour matrix...';
        // Builds the contour matrix
        ErrorCode :=  Calc_C_Matrix(@Links[0], length(Links));
        if ErrorCode <> 0 then ErrorStr := 'Error'
        else ErrorStr :=  'Done';
        prog_Str    :=  prog_str + ErrorStr;

      end;
      4: Begin                       // Builds the ZLL matrix
        prog_Str    :=  prog_str + CRLF + '- Building ZLL...';
        ErrorCode :=  Calc_ZLL(@Links[0],length(Links));
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
          DssExecutive.Command    :=  Links[DIdx] + '.r0=1000000000';
          DssExecutive.Command    :=  Links[DIdx] + '.r1=1000000000';
          DssExecutive.Command    :=  Links[DIdx] + '.x0=0';
          DssExecutive.Command    :=  Links[DIdx] + '.x1=0';
        End;
        Ymatrix.BuildYMatrix(WHOLEMATRIX, FALSE, ActiveActor);
        prog_Str      :=  prog_str + 'Done';
        ErrorCode     :=  0;          // No error handling here
      end;
      6:  Begin                      // Builds the ZCC matrix
        prog_Str      :=  prog_str + CRLF + '- Building ZCC...';
        Calc_ZCC();
        prog_Str      :=  prog_str + 'Done' + CRLF;

      End
      else
      Begin

      End;
    end;
    inc(Local_State);
    MQuit := (Local_State > Num_States) or (ErrorCode <> 0);
  End;

  ActiveActor                     :=  1;
  if ErrorCode <> 0 then ErrorStr := 'One or more errors found'
  else  ErrorStr  :=  'A-Diakoptics initialized';

  prog_Str      :=  prog_str + CRLF + ErrorStr + CRLF;
  GlobalResult  :=  ErrorStr;

  {$IFNDEF FPC}
  if not IsDLL
  then
    ScriptEd.PublishMessage(prog_Str)
  else
    GlobalResult  :=  prog_str;
  {$ELSE}
    GlobalResult  :=  prog_str;
  {$ENDIF}
  // TEMc: TODO: should we report something here under FPC?
  // Davis: Done: This will add the needed report
  SolutionAbort :=  False;

End;

end.
