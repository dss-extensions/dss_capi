unit Diakoptics;

// Copyright (c) 2008-2021, Electric Power Research Institute, Inc.
// All rights reserved.

interface

uses
    DSSClass,
    Circuit,
    Solution,
    SysUtils,
    DSSClassDefs,
    EnergyMeter,
    SolutionAlgs,
    Line,
    CmdForms;

function Solve_Diakoptics(DSS: TDSSContext): Integer;
function ADiakoptics_Tearing(DSS: TDSSContext; AddISrc: Boolean): Integer;
procedure ADiakopticsInit(DSS: TDSSContext);
function Calc_C_Matrix(DSS: TDSSContext; PLinks: PString; NLinks: Integer): Integer;
function Calc_ZLL(DSS: TDSSContext; PLinks: PString; NLinks: Integer): Integer;
procedure Calc_ZCC(DSS: TDSSContext; Links: Integer);
procedure Calc_Y4(DSS: TDSSContext);
procedure SendIdx2Actors(DSS: TDSSContext);
function get_Statistics(DSS: TDSSContext): String;

implementation

uses
    DSSGlobals,
    DSSHelper,
    ExecHelper,
    Executive,
    YMatrix,
    KLUSolve,
    UComplex,
    DSSUcomplex,
    Sparse_Math,
    UcMatrix,
    math,
    Dynamics; // For TSolveMode

// This is the A-Diakoptics algorithm executed by the Coordinator (Actor = 1)
function Solve_Diakoptics(DSS: TDSSContext): Integer;
var
    i, myRow: Integer;
    Vpartial: TSparse_Complex; //TODO: migrate to KLUSolveX
begin
    // Space left empty to implement the simplified Diakoptics algorithm
    with DSS.ActiveCircuit, Solution do
    begin
        // Solves the partial systems to find the voltages at the edges of the sub-systems
        SendCmd2Actors(SOLVE_AD1);

        Vpartial := Tsparse_Complex.Create;
        Vpartial.sparse_matrix_Cmplx(Contours.NCols, 1);
        // Does the voltage diff calculation using the partial results
        myRow := 0;
        for i := 0 to (Contours.NCols - 1) do
        begin
            VPartial.insert(i, 0, NodeV^[Contours.CData[myRow].Row + 1] - NodeV^[Contours.CData[myRow + 1].Row + 1]);
            myRow := myRow + 2;
        end;
        // Loads the partial solution considering the previous iteration
        Vpartial := Y4.multiply(VPartial);
        Ic := Contours.multiply(VPartial);  // Calculates the new Injecting Currents

        // Commands the actors to complement the solution
        SendCmd2Actors(SOLVE_AD2);
    end;
    DSS.ActiveCircuit.IsSolved := TRUE;
    DSS.ActiveCircuit.BusNameRedefined := FALSE;
    if DSS.SolutionAbort then
        DSS.ActiveCircuit.IsSolved := FALSE;
    DSS.ActiveChild := DSS;
    DSS.ActiveChildIndex := 0;
    Result := 0;
    Vpartial.Free;
end;

// Returns a string with the partitioning statistics
// It only works if the partitioning was succesful
function get_Statistics(DSS: TDSSContext): String;
var
    unbalance,
    ASize: array of Single;
    idx: Integer;
    GReduct,
    MaxImbal,
    AvgImbal: Double;
begin
    setlength(ASize, 0);
    for idx := 1 to DSS.NumOfActors - 1 do
    begin
        setlength(ASize, length(ASize) + 1);
        ASize[high(ASize)] := DSS.Children[idx].ActiveCircuit.NumNodes;
    end;
    GReduct := (1 - (MaxValue(ASize) / DSS.ActiveCircuit.NumNodes)) * 100;   // The biggest actor
    setlength(unbalance, length(ASize));
    for idx := 0 to High(ASize) do
        unbalance[idx] := (1 - (ASize[idx] / MaxValue(ASize))) * 100; // All the unbalances
    MaxImbal := MaxValue(unbalance);                              // Max imbalance
    AvgImbal := mean(unbalance);                                  // Average
  // publishes the results
    Result := CRLF +
        Format(_('Circuit reduction (%%): %4.2f'), [GReduct]) + CRLF +
        Format(_('Max imbalance (%%): %4.2f'), [MaxImbal]) + CRLF +
        Format(_('Average imbalance(%): %4.2f'), [AvgImbal]) + CRLF;
end;


// Sets the memory index for each actor so they can write
// directly into the coordinator's Voltage vector
procedure SendIdx2Actors(DSS: TDSSContext);
var
    i, j: Integer;
    BusName: String;
    AllNNames: array of String;
begin
// Gets the names of the nodes in the interconnected system
    setlength(AllNNames, 0);
    with DSS.ActiveCircuit do
    begin
        for i := 1 to NumBuses do
        begin
            BusName := BusList.NameOfIndex(i);
            for j := 1 to Buses^[i].NumNodesThisBus do
            begin
                setlength(AllNNames, (length(AllNNames) + 1));
                AllNNames[high(AllNNames)] := BusName + '.' + IntToStr(Buses^[i].GetNum(j));
            end;
        end;
    end;
    // Sets the index for each actor
    // The feeder head first
    DSS.Children[1].ActiveCircuit.VIndex := 0;
    // Then checks the rest of the actors
    for i := 2 to DSS.NumOfActors - 1 do
    begin
        BusName := DSS.Children[i].ActiveCircuit.BusList.NameOfIndex(1) + '.1';
        // Looks for the node within all the Node Names in the interconnected model
        for j := 0 to High(AllNNames) do
            if BusName = AllNNames[j] then
                Break;
        DSS.Children[i].ActiveCircuit.VIndex := j;
    end;
    // Initializes the Ic vector with zeros
    DSS.ActiveCircuit.Ic.sparse_matrix_Cmplx(length(AllNNames), 1);
    // DSS.ActiveCircuit.V_0.sparse_matrix_Cmplx(length(AllNNames), 1);
    for i := 0 to High(AllNNames) do
    begin
        DSS.ActiveCircuit.Ic.Insert(i, 0, cZERO);
        // DSS.ActiveCircuit.V_0.Insert(i, 0, cZERO);
    end;
end;

// Inverts ZCC to obtain its admittance equivalent Y4 
// This is the heart of A-Diakoptics
procedure Calc_Y4(DSS: TDSSContext);
var
    value: Complex;
    NumRows,
    NumCols,
    col,
    idx: Integer;
    TempMat: TcMatrix;
begin
    with DSS.ActiveCircuit, Solution do
    begin
        // Moves ZCC into an equivalent compatible with TcMatrix
        TempMat := TCmatrix.CreateMatrix(ZCC.NRows);
        for idx := 0 to High(ZCC.CData) do
        begin
            TempMat.SetElement(ZCC.CData[idx].Row + 1, ZCC.CData[idx].Col + 1, ZCC.CData[idx].Value);
        end;
        // Inverts the ZCC equivalent
        TempMat.Invert;
        Y4.sparse_matrix_Cmplx(ZCC.NRows, ZCC.NCols);
        NumRows := ZCC.NRows - 1;
        NumCols := ZCC.NCols - 1;
    // Moves the inverse into Y4 for furhter use
        for idx := 0 to NumRows do
        begin
            for col := 0 to NumCols do
            begin
                value := TempMat.GetElement(idx + 1, col + 1);
                if (value.re <> 0) and (value.re <> 0) then
                    Y4.insert(idx, col, value);
            end;
        end;
    end;
end;

// Calculates the Connections matrix ZCC in the
// contours-contours domain
procedure Calc_ZCC(DSS: TDSSContext; Links: Integer);
var
    row,
    col,
    idx3,
    idx2,
    idx: Integer;
    NNodes: Longword;
    CVector,
    ZVector: pComplexArray;
    Ctemp: Complex;
begin
    with DSS.ActiveCircuit, Solution do
    begin
        GetSize(hY, @NNodes);
        col := NNodes;
        dec(Links);
        ZCT.sparse_matrix_Cmplx(col, Links * 3);
        CVector := Allocmem(SizeOf(CVector^[1]) * (col + 1));
        ZVector := Allocmem(SizeOf(ZVector^[1]) * (col + 1));
        idx3 := Links * 3 - 1;

        for idx2 := 0 to idx3 do
        begin
            for idx := 1 to col do
                CVector^[idx] := cZERO;  // Makes it zero

            for idx := 1 to length(Contours.CData) do
            begin
                if Contours.CData[idx - 1].col = idx2 then
                begin
                    row := Contours.CData[idx - 1].row + 1;
                    CVector^[row] := Contours.CData[idx - 1].Value;
                end;
            end;
            SolveSparseSet(hy, pComplexArray(@ZVector^[1]), pComplexArray(@CVector^[1]));

            for idx := 1 to col do           // inserts result into the ZCT matrix
            begin
                CTemp := ZVector^[idx];
                if (CTemp.re <> 0) and (CTemp.im <> 0) then
                    ZCT.insert((idx - 1), idx2, ZVector[idx]);
            end;
            idx := col;
        end;
        // At this point we have calculated the right side of the equation
        // ZCC = CTZ(TT)C -> Z(TT)C
        // It is needed transpose the contours matrix and multiply it
        ContoursT := Contours.Transpose();
        ZCC := ContoursT.multiply(ZCT);   // Calculates ZCC with no Link impedances
        ZCC := ZCC.Add(ZLL);              // Adds the link impedance

        FreeMem(CVector);
        FreeMem(ZVector);
    end;
end;

// Calculates the contours matrix based
// on the location in the graph of the link branches
// if there is an error returns <> 0
function Calc_C_Matrix(DSS: TDSSContext; PLinks: PString; NLinks: Integer): Integer;
var
    LIdx, k, l,
    j, CDirection,
    NumPhases,
    i: Integer;
    Elem_Buses,
    Node_Names: array of String;
    temp: String;
    Go_Flag: Boolean;
begin
    DSS := DSS.GetPrime();
    DSS.ActiveChild := DSS;
    DSS.ActiveChildIndex := 0;
    with DSS.ActiveCircuit do
    begin
        Result := 0;
        setlength(Elem_Buses, 2);
        setlength(Node_Names, 0);
        for i := 1 to NumNodes do
        begin
            setlength(Node_Names, (length(Node_Names) + 1));
            with MapNodeToBus^[i] do
                Node_Names[High(Node_names)] := Format('%s.%-d', [AnsiLowerCase(BusList.NameOfIndex(Busref)), NodeNum]);
        end;

        Contours.sparse_matrix_Cmplx(length(Node_Names), (NLinks - 1) * 3);

        for LIdx := 1 to (NLinks - 1) do
        begin
            inc(PLinks); // Pointing to the Next link branch (starting in 1)
            temp := String(PLinks^);
            j := ansipos('.', temp);
            temp := AnsiLowerCase(copy(temp, 0, (j - 1)));
            if (temp = 'line') then
            begin
                i := SetElementActive(String(PLinks^));
                // Get the names of the buses fot this PDElement
                // If it is something different from a Transformer reports an error
                // Since a link branch cannot be a transformer
                for i := 1 to ActiveCktElement.Nterms do
                begin
                    Elem_Buses[i - 1] := ActiveCktElement.GetBus(i);
                    j := ansipos('.', Elem_Buses[i - 1]);
                    if j <> 0 then
                        Elem_Buses[i - 1] := copy(Elem_Buses[i - 1], 0, j)
                    else
                        Elem_Buses[i - 1] := Elem_Buses[i - 1] + '.';
                end;
                //  Marks the connection point in the contours matrix
                NumPhases := ActiveCktElement.NPhases;
                for l := 1 to NumPhases do
                begin
                    for i := 0 to 1 do
                    begin
                        temp := Elem_Buses[i] + inttostr(l);
                        Go_Flag := TRUE;
                        j := 0;
                        while Go_Flag and (j <= High(Node_Names)) do
                        begin
                            k := ansipos(temp, Node_Names[j]);
                            if k <> 0 then
                            begin
                                if i = 0 then
                                    CDirection := 1
                                else
                                    CDirection := -1;
                                Contours.insert(j, ((l - 1) + (LIdx - 1) * 3), cmplx(CDirection, 0));
                                Go_Flag := FALSE;
                            end;
                            inc(j);

                        end;

                    end;
                end;
            end
            else
            begin
                Result := -1; // There was an error when selecting the link branches (MeTIS)
                break;  // Abort
            end;
        end;
        // More error checking
        if Result = 0 then
            if Contours.NZero <> 0 then
                Result := 0
            else
                Result := 1;
    end;
end;

// Calculates the Link branches matrix for further use
// if there is an error returns <> 0
function Calc_ZLL(DSS: TDSSContext; PLinks: PString; NLinks: Integer): Integer;
var
    NValues,
    idx, k, j,
    row, col,
    count,
    i: Integer;
    cValues: pComplexArray;
    ErrorFlag: Boolean;
    LinkPrim: TcMatrix;
begin
    dec(NLinks);
    ErrorFlag := FALSE;
    LinkPrim := TCmatrix.CreateMatrix(3);
    DSS := DSS.GetPrime();
    DSS.ActiveChild := DSS;
    DSS.ActiveChildIndex := 0;
    with DSS.ActiveCircuit do
    begin
        ZLL.Sparse_matrix_Cmplx(NLinks * 3, NLinks * 3);
        for i := 1 to NLinks do
        begin
            inc(PLinks);
            idx := SetElementActive(String(PLinks^));

            if ActiveCktElement <> NIL then
                with ActiveCktElement do
                begin
                    NValues := SQR(Yorder);
                    cValues := GetYprimValues(ALL_YPRIM);  // Get pointer to complex array of values
                    if cValues <> NIL then
                    begin
                        k := 1;
                        idx := (i - 1) * 3;
                        row := 1;
                        col := 1;
                        count := 0;
                        // Extracts the YPrim of the Link branch
                        for j := 1 to (NValues div 4) do
                        begin
                            LinkPrim.SetElement(row, col, cValues^[k]);
                            inc(count);
                            if count > 2 then
                            begin
                                inc(row);
                                Col := 1;
                                Count := 0;
                                k := k + 4;
                            end
                            else
                            begin
                                inc(Col);
                                inc(k);
                            end;
                        end;
                        // Inverts the Y primitive
                        LinkPrim.Invert;
                        // Inserts the Z primitive values into ZLL
                        row := 0;
                        col := 0;
                        count := 0;
                        for j := 1 to (NValues div 4) do
                        begin
                            ZLL.insert((row + idx), (col + idx), LinkPrim.GetElement(row + 1, col + 1));
                            inc(count);
                            if count > 2 then
                            begin
                                inc(row);
                                Col := 0;
                                Count := 0;
                            end
                            else
                                inc(Col);
                        end;
                    end
                    else
                        ErrorFlag := TRUE;

                end

        end;
        if ErrorFlag then
            Result := 1
        else
            Result := 0;

    end;
end;


// Tears the system using considering the number of
// circuits specified by the user
// The flag AddISrc indicates if its necessary to create
// Isources at the edges of the link branches, the ISource
// magnitude is equal to 0.000001, angle 0 (for A-Diakoptics)
function ADiakoptics_Tearing(DSS: TDSSContext; AddISrc: Boolean): Integer;
var
    Prev_Mode: TSolveMode; // Stores the previous solution mode
    Num_Ckts: Integer; // Stores the number of Sub-Circuits created
begin
    with DSS.ActiveCircuit, Solution do
    begin
        Num_Ckts := Tear_Circuit();
        Prev_mode := Dynavars.SolutionMode;
        Dynavars.SolutionMode := TSolveMode.SNAPSHOT;
        DSS.DSSExecutive.Command := 'set controlmode=off';
        Ymatrix.BuildYMatrix(DSS, WHOLEMATRIX, FALSE);
        // DoSolveCmd();
        if not DSS.SolutionAbort then
        begin
            Save_SubCircuits(AddISrc);
            Dynavars.SolutionMode := Prev_mode;  // Goes back to the previous solution mode
            DSS.ActiveCircuit.Num_SubCkts := Num_Ckts;
            DSS.GlobalResult := Format(_('Sub-Circuits Created: %d'), [Num_Ckts]);
            Result := 0;
        end
        else
        begin
            DSS.GlobalResult := _('There was an error when tearing the circuit ');
            Result := 1;
        end;
    end;
end;

// Generates the subsystems, actors and memory space
// For using the A-Diakoptics parallelism
procedure ADiakopticsInit(DSS: TDSSContext);
var
    EMeter: TEnergyMeterObj;
    j,
    Local_State,
    Num_States,
    ErrorCode,
    DIdx,
    Diak_Actors: Integer;
    Dir, Proj_Dir,
    prog_Str,
    ErrorStr,
    FileRoot: String;
    Links: array of String;                        // List of the Link Branches
    MQuit: Boolean;                                // To quit the State Machine
    ChDSS: TDSSContext = NIL;
begin
    ErrorCode := 0;
    Diak_Actors := 0;
    // The program is built as a state machine to facilitate the error detection
    // and quitting the routines after an error is detected wihtout killing the prog
    MQuit := FALSE;
    Num_States := 9;                          // Number of states of the machine
    Local_State := 0;                          // Current state
    prog_str := _('A-Diakoptics initialization summary:') + CRLF + CRLF;

    DSS := DSS.GetPrime();
    DSS.ActiveChild := DSS;
    DSS.ActiveChildIndex := 0;

    // Checks if the number of actors is within a reasonable limit
    if DSS.ActiveCircuit.Num_SubCkts > (CPU_Cores - 2) then
        DSS.ActiveCircuit.Num_SubCkts := CPU_Cores - 2;

    while (not MQuit) do
    begin
        case Local_State of
            0:
            begin // Create subcircuits
                prog_Str := prog_str + _('- Creating Sub-Circuits...') + CRLF;

                ErrorCode := ADiakoptics_Tearing(DSS, FALSE);
                if ErrorCode <> 0 then
                    ErrorStr := _('Error: The circuit cannot be decomposed') + CRLF
                else
                    ErrorStr := Format(_('  %d Sub-Circuits Created'), [DSS.ActiveCircuit.Num_SubCkts]) + CRLF;

                prog_Str := prog_str + ErrorStr;
            end;
            1:
            begin // Saves the Link Branch list locally
                Diak_Actors := DSS.ActiveCircuit.Num_SubCkts + 1;
                prog_Str := prog_str + _('- Indexing link branches...');

                setlength(Links, length(DSS.ActiveCircuit.Link_Branches));
                for DIdx := 0 to High(Links) do
                    Links[DIdx] := DSS.ActiveCircuit.Link_Branches[DIdx];

                prog_Str := prog_str + _('Done');
                ErrorCode := 0; // No error handling here
            end;
            2:
            begin // Compile subsystems
                ErrorCode := 0;
                prog_Str := prog_str + CRLF + _('- Setting up the Actors...');
                // Clears everything to create the actors and compile the subsystems
                DSS.Parallel_enabled := FALSE;
                DSS.DSSExecutive.ClearAll;
                Fileroot := DSS.OutputDirectory {CurrentDSSDir};    //  Gets the current directory
                DSS.SolutionAbort := FALSE;

                // Compiles the interconnected Circuit for further calculations on actor 1
                Proj_Dir := 'compile "' + Fileroot + 'Torn_Circuit' + PathDelim + 'Master_interconnected.dss"';
                DSS.DssExecutive.Command := Proj_Dir;
                DSS.DssExecutive.Command := 'set controlmode=Off';
                // Disables the Energymeters for the zones
                with DSS.ActiveCircuit, Solution do
                begin
                    EMeter := EnergyMeters.First;
                    while EMeter <> NIL do
                    begin
                        j := ansipos('zone_', EMeter.Name);
                        if j <> 0 then
                            EMeter.Enabled := FALSE;
                        EMeter := EnergyMeters.Next;
                    end;
                end;
                Ymatrix.BuildYMatrix(DSS, WHOLEMATRIX, FALSE);
                DSS.DSSExecutive.DoSolveCmd();

                // Creates the other actors
                for DIdx := 2 to Diak_Actors do
                begin
                    New_Actor_Slot(DSS);
                    
                    ChDSS := DSS.Children[DIdx - 1];
                    if DIdx = 2 then
                        Dir := ''
                    else
                        Dir := 'zone_' + inttostr(DIdx - 1) + PathDelim;

                    Proj_Dir := 'compile "' + Fileroot + PathDelim + 'Torn_Circuit' + PathDelim + Dir + 'Master.dss"';
                    ChDSS.DssExecutive.Command := Proj_Dir;
                    if DIdx > 2 then
                        ChDSS.DssExecutive.Command := Links[DIdx - 2] + '.enabled=False';

                    ChDSS.DssExecutive.Command := 'set controlmode=Off';
                    ChDSS.DssExecutive.DoSolveCmd();
                    if ChDSS.SolutionAbort then
                    begin
                        ErrorCode := 1;
                        Break;
                    end;
                end;
                if ErrorCode <> 0 then
                    ErrorStr := _('Error') + CRLF + _('One or sub-systems cannot be compiled') + CRLF
                else
                    ErrorStr := _('Done');
                prog_Str := prog_str + ErrorStr;
            end;
            3:
            begin
                // Opens the link branches in the interconnected Circuit and recalculates the YBus
                // The opening happens by replacing the line with a very high series impedance
                prog_Str := prog_str + CRLF + _('- Opening link branches...');
                for DIdx := 1 to High(Links) do
                begin
                    DSS.ActiveCircuit.SetElementActive(String(Links[DIdx]));
                    DSS.ActiveCircuit.ActiveCktElement.Enabled := FALSE;
                end;
                DSS.ActiveCircuit.BusNameRedefined := FALSE;
                Ymatrix.BuildYMatrix(DSS,WHOLEMATRIX, FALSE);
                prog_Str := prog_str + _('Done');
                ErrorCode := 0; // No error handling here
            end;
            4:
            begin // Creates the contours matrix
                prog_Str := prog_str + CRLF + _('- Building Contours...');
                // Builds the contour matrix
                ErrorCode := Calc_C_Matrix(DSS, @Links[0], length(Links));
                if ErrorCode <> 0 then
                    ErrorStr := _('Error') + CRLF + _('One or more link branches are not lines') + CRLF
                else
                    ErrorStr := _('Done');
                prog_Str := prog_str + ErrorStr;
            end;
            5:
            begin // Builds the ZLL matrix
                prog_Str := prog_str + CRLF + _('- Building ZLL...');
                ErrorCode := Calc_ZLL(DSS, @Links[0], length(Links));
                if ErrorCode <> 0 then
                    ErrorStr := _('Error')
                else
                    ErrorStr := _('Done');
                prog_Str := prog_str + ErrorStr;
            end;
            6:
            begin // Builds the ZCC matrix
                prog_Str := prog_str + CRLF + _('- Building ZCC...');
                Calc_ZCC(DSS, length(Links));
                prog_Str := prog_str + _('Done');
            end;
            7:
            begin // Inverts ZCC to get Y4
                prog_Str := prog_str + CRLF + _('- Building Y4 ...');
                Calc_Y4(DSS);
                prog_Str := prog_str + _('Done');
                // Moves back the link branches list into actor 1 for further use
                setlength(DSS.ActiveCircuit.Link_Branches, length(Links));
                for DIdx := 0 to High(Links) do
                    DSS.ActiveCircuit.Link_Branches[DIdx] := Links[DIdx];
            end;
            8:
            begin // Sends the index to the actors for uploading info
                prog_Str := prog_str + CRLF + _('- Assigning indexes to actors ...');
                SendIdx2Actors(DSS);
                prog_Str := prog_str + _('Done');
            end;
            9:
            begin // Prints the statistics of the partitioning
                prog_Str := prog_str + CRLF + CRLF + _('Partitioning statistics');
                prog_Str := prog_str + get_Statistics(DSS);
                // Assigns the processor per actor
                for DIdx := 1 to DSS.NumOfActors do
                begin
                    ChDSS := DSS.Children[DIdx - 1];//TODO: check
                    ChDSS.CPU := DIdx;
                    if ChDSS.ActorThread <> NIL then
                    begin
                        ChDSS.ActorThread.CPU := ChDSS.CPU;
                        // ChDSS.ActorThread.Priority := tpTimeCritical;
                    end;
                end;
                // Compiles the interconnected Circuit for further calculations on actor 1
                prog_Str := prog_str + CRLF + _('- Closing link branches...');
                for DIdx := 1 to High(Links) do
                begin
                    DSS.ActiveCircuit.SetElementActive(String(Links[DIdx]));
                    DSS.ActiveCircuit.ActiveCktElement.Enabled := TRUE;
                end;
                DSS.ActiveCircuit.BusNameRedefined := FALSE;
                Ymatrix.BuildYMatrix(DSS, WHOLEMATRIX, FALSE);

                DSS.ActiveCircuit.Solution.SendCmd2Actors(INIT_ADIAKOPTICS);
                DSS.ActiveCircuit.Solution.ADiak_init := TRUE;
            end
        end;
        inc(Local_State);
        MQuit := (Local_State > Num_States) or (ErrorCode <> 0);
    end;

    if ErrorCode <> 0 then
    begin
        ErrorStr := _('One or more errors found');
        DSS.ActiveCircuit.Solution.ADiakoptics := FALSE;
    end
    else
    begin
        ErrorStr := _('A-Diakoptics initialized');
        DSS.Parallel_enabled := TRUE;
        DSS.ActiveCircuit.Solution.ADiakoptics := TRUE;
        DSS.ActiveCircuit.Solution.ADiak_Init := FALSE; // Needed to force the subzones to remove VSource.Source
    end;
    //TODO?  ProgressCmd := TRUE;
    prog_Str := CRLF + prog_str + CRLF + ErrorStr + CRLF;
    DSS.GlobalResult := ErrorStr;

    //TODO: ScriptEd.PublishMessage(prog_Str)
    //TODO: check -- the previous GlobalResult is ignored here...
    DSS.GlobalResult := prog_str;
    DSS.SolutionAbort := FALSE;
end;

end.
