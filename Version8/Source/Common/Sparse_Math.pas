{*******************************************************************************
  Electric Power Resarch Insitute EPRI 2018

 Library created to handle sparse matrix linear algebra ops, the arguments need
 to be sparse and provided in compressed coordiante format
 Created by Davis Montenegro for EPRI 08/01/2018
 based on the code provided by Sudarshan Khasnis in Java at
 https://www.geeksforgeeks.org/operations-sparse-matrices/
*******************************************************************************}

unit Sparse_Math;

interface

uses
{$IFDEF MSWINDOWS}
    dialogs,
{$ENDIF}
    UComplex,
    Ucmatrix;

type
    TCmplx_Data = record
        Row,
        Col: Integer;
        Value: Complex;
    end;


type
    TData = array of Integer;
    PData = ^TData;

    TComplex = array of TCmplx_Data;
    PComplex = ^TComplex;

    TComplexArr = array of Complex;
    PComplexArr = ^TComplexArr;


type

    Tsparse_matrix = class(Tobject)
    PRIVATE
        row,
        col,
        len: Integer;

        function checkifexists(r, c: Integer): Integer;
        procedure getrow(index: Integer; cols, vals: PData);
        function R_equal(acols, avals, bcols, bvals: PData): Boolean;

    PUBLIC
        data: array of array of Integer;
        procedure sparse_matrix(r, c: Integer);
        function insert(r, c, val: Integer): Integer;
        function add(b: Tsparse_matrix): Tsparse_matrix;
        function Transpose(): Tsparse_matrix;
        function multiply(b: Tsparse_matrix): Tsparse_matrix;
        procedure reset();
        function NZero(): Integer;
        function NCols(): Integer;
        function NRows(): Integer;
        function Rank(): Integer;
    end;

    Tsparse_Complex = class(Tobject)
    PRIVATE
        row,
        col,
        len: Integer;

        function checkifexists(r, c: Integer): Integer;
        procedure getrow(index: Integer; cols: PData; vals: PComplexArr);
        function getvalue(row, col: Integer): Complex;
        function R_equal(acols, bcols: PData; avals, bvals: PComplexArr): Boolean;

    PUBLIC
        CData: array of TCmplx_Data;
        procedure sparse_matrix_Cmplx(r, c: Integer);
        function insert(r, c: Integer; val: Complex): Integer;
        function add(b: Tsparse_Complex): Tsparse_Complex;
        function Transpose(): Tsparse_Complex;
        function TransposeConj(): Tsparse_Complex;
        function multiply(b: Tsparse_Complex): Tsparse_Complex;
        procedure reset();
        function NZero(): Integer;
        function NCols(): Integer;
        function NRows(): Integer;
        function Rank(): Integer;


    end;

implementation

// Evaluates of both rows are equal
function Tsparse_matrix.R_equal(acols, avals, bcols, bvals: PData): Boolean;
var
    idx,
    rlen: Integer;
begin
    Result := FALSE;                        // In case they are not equal
    if length(acols^) = length(bcols^) then   // If they have the same # of Cols
    begin
        rlen := 0;                            // First, verify if the cols are the same
        for idx := 0 to high(acols^) do
            if (acols^[idx] - bcols^[idx]) <> 0 then
                inc(rlen);
        if rlen = 0 then
            Result := TRUE;
    end;
end;
// Gets the columns and values at each columns for the row specified
procedure Tsparse_matrix.getrow(index: Integer; cols, vals: PData);
var
    rowcols,
    rowvals: TData;
    j: Integer;
begin
    setlength(rowcols, 0);
    setlength(rowvals, 0);

    for j := 0 to (len - 1) do
    begin
        if data[j][0] = index then
        begin
            setlength(rowcols, length(rowcols) + 1);
            setlength(rowvals, length(rowvals) + 1);

            rowcols[high(rowcols)] := data[j][1];
            rowvals[high(rowvals)] := data[j][2];
        end;
    end;

    cols^ := rowcols;
    vals^ := rowvals;
end;

function Tsparse_matrix.Rank(): Integer;   // Added 08/16/2018 by DM for calculating the
var                                        // Rank of the sparse matrix
    i,
    j: Integer;
    flag: Boolean;
    acols,                                   // Row under evaluation
    avals,
    bcols,                                   // Reference row
    bvals: TData;
begin
    Result := 0;
    for i := 0 to (row - 1) do
    begin
        getrow(i, @acols, @avals);
        if i > 0 then
        begin
            j := i - 1;
            flag := TRUE;
            while flag and (j >= 0) do
            begin
                getrow(j, @bcols, @bvals);    // sweeps the matrix bottom up
                flag := not R_equal(@acols, @avals, @bcols, @bvals);
                dec(j);
            end;
            if flag then
                inc(Result);
        end
        else
            inc(Result);
    end;
end;

function Tsparse_matrix.NCols(): Integer;
begin
    Result := Col;
end;

function Tsparse_matrix.NRows(): Integer;
begin
    Result := Row;
end;

function Tsparse_matrix.checkifexists(r, c: Integer): Integer;
var
    i: Integer;
begin
    Result := -1;                 // Default in case the value doesn't exist
    if len > 0 then
    begin
        for i := 0 to (len - 1) do
        begin
            if (data[i][0] = r) and (data[i][1] = c) then
                Result := i;              // If the value exists returns the index ( >=0 )
        end;
    end;
end;

procedure Tsparse_matrix.sparse_matrix(r, c: Integer);
begin
    row := r;    // Initialize row
    col := c;    // Initialize Col
    len := 0;    // Initialize length to 0
    setlength(data, 0);
end;

//Inserts elements into the sparse matrix
function Tsparse_matrix.insert(r, c, val: Integer): Integer;
var
    lrow,
    lcol: Integer;  // To store the current lenght of the data matrix
begin
    Result := 1;

    lrow := checkifexists(r, c);
    if lrow >= 0 then
    begin
        data[lrow][2] := val;    // Assigns the new value to the existing cell
    end
    else
    begin
      // Reshapes the memory space
        lrow := length(data);
        setlength(data, lrow + 1);
        setlength(data[lrow], 3);
      // Adds the data to the new memory space
        data[high(data)][0] := r;
        data[high(data)][1] := c;
        data[high(data)][2] := val;
        inc(len);

        if col < c then
            col := c;
        if row < r then
            row := r;
    end;

end;

// Adds another sparse matrix to this matrix
function Tsparse_matrix.add(b: Tsparse_matrix): Tsparse_matrix;
var
    addeval,
    apos,
    bpos: Integer;

begin
    // Creates a memory space to store the result
    Result := Tsparse_matrix.create;
    // First checks if the matrices have the same dimensions
    if (row <> b.row) or (col <> b.col) then
    begin
        Result.sparse_matrix(1, 1);
        Result.insert(0, 0, -1);
    end
    else
    begin
        apos := 0;
        bpos := 0;

        Result.sparse_matrix(row, col);

        while (apos < len) and (bpos < b.len) do
        begin
            if (data[apos][0] > b.data[bpos][0]) or ((data[apos][0] = b.data[bpos][0]) and (data[apos][1] > b.data[bpos][1])) then
            begin
                Result.insert(b.data[bpos][0], b.data[bpos][1], b.data[bpos][2]);
                inc(bpos)
            end
            else
            begin
                if (data[apos][0] < b.data[bpos][0]) or ((data[apos][0] = b.data[bpos][0]) and (data[apos][1] < b.data[bpos][1])) then
                begin
                    Result.insert(data[apos][0], data[apos][1], data[apos][2]);
                    inc(apos)
                end
                else
                begin
                    addeval := data[apos][2] + b.data[bpos][2];
                    if addeval <> 0 then
                        Result.insert(data[apos][0], data[apos][1], addeval);
                    inc(apos);
                    inc(bpos);
                end;
            end;
        end;
      // Inserts the remaining elements
        while (apos < (len - 1)) do
        begin
            Result.insert(data[apos][0], data[apos][1], data[apos + 1][2]);
            inc(apos)
        end;
        while (bpos < (b.len - 1)) do
        begin
            Result.insert(b.data[bpos][0], b.data[bpos][1], b.data[bpos + 1][2]);
            inc(bpos)
        end;

    end;

end;

// Transposes the sparse matrix
function Tsparse_matrix.Transpose(): Tsparse_matrix;
var
    Count,
    Index: array of Integer;
    i,
    rpos: Integer;

begin
    // Creates a memory space to store the result
    Result := Tsparse_matrix.create;
    // new matrix with inversed row X col
    Result.sparse_matrix(col, row);
    // same number of elements
    for i := 1 to len do
        Result.insert(i, 0, 0);

    setlength(Count, col + 1);
    setlength(Index, col + 1);
    // Initialize all to 0
    for i := 0 to col do
        Count[i] := 0;
    for i := 0 to (len - 1) do
        inc(Count[data[i][1]]);
    // to count number of elements having col smaller
    // than particular i
    // as there is no col with value < 1
    Index[0] := 0;
    // initialize rest of the indices

    for i := 1 to col do
        Index[i] := Index[i - 1] + Count[i - 1];

    for i := 0 to (len - 1) do
    begin
      // insert a data at rpos and increment its value
        rpos := index[data[i][1]];
        inc(index[data[i][1]]);
      // transpose row=col
        Result.data[rpos][0] := data[i][1];

      // transpose col=row
        Result.data[rpos][1] := data[i][0];

      // same value
        Result.data[rpos][2] := data[i][2];
    end;

    // the above method ensures
    // sorting of transpose matrix
    // according to row-col value

end;

// Multiplies another sparse matrix by this matrix
function Tsparse_matrix.multiply(b: Tsparse_matrix): Tsparse_matrix;
var
    sum,
    c,
    tempa,
    tempb,
    r,
    apos,
    bpos: Integer;
begin
    // Creates a memory space to store the result
    Result := Tsparse_matrix.create;
    // First checks if the matrices have the right dimensions
    if col <> b.row then
    begin
        Result.sparse_matrix(1, 1);
        Result.insert(0, 0, -1);    //Invalid multiplication
    end
    else
    begin
        // transpose b to compare row
        // and col values and to add them at the end
        b := b.Transpose();
        // result matrix of dimension row X b.col
        // however b has been transposed, hence row X b.row
        Result.sparse_matrix(row, b.row);
        // iterate over all elements of A (this matrix)
        apos := 0;
        while apos < len do
        begin
            r := data[apos][0];
          // iterate over all elements of B
            bpos := 0;
            while bpos < b.len do
            begin
            // current column of result matrix
            // data[][0] used as b is transposed
                c := b.data[bpos][0];

            // temporary pointers created to add all
            // multiplied values to obtain current
            // element of result matrix
                tempa := apos;
                tempb := bpos;

                sum := 0;

            // iterate over all elements with
            // same row and col value
            // to calculate result[r]

                while (tempa < len) and (data[tempa][0] = r) and (tempb < b.len) and (b.data[tempb][0] = c) do
                begin
                    if (data[tempa][1] < b.data[tempb][1]) then
                        inc(tempa)   //skip a
                    else
                    begin
                        if (data[tempa][1] > b.data[tempb][1]) then
                            inc(tempb)  //skip b
                        else
                        begin
                  // same col, so multiply and increment
                            sum := sum + data[tempa][2] * b.data[tempb][2];
                            inc(tempa);
                            inc(tempb);
                        end;
                    end;
                end;
            // insert sum obtained in result[r]
            // if its not equal to 0
                if sum <> 0 then
                    result.insert(r, c, sum);

                while (bpos < b.len) and (b.data[bpos][0] = c) do
                    inc(bpos);    // Jump to next column
            end;
            while (apos < len) and (data[apos][0] = r) do
                inc(apos);    // Jump to next row
        end;
    end;
end;

  // Resets the sparse matrix (makes it empty)
procedure Tsparse_matrix.reset();
begin
    setlength(data, 0);
    len := 0;
end;
  // Returns the lenght of the sparse matrix (number of non-zero elements)
function Tsparse_matrix.NZero(): Integer;
begin
    Result := len;
end;


//******************************************************************************
//*   Complex sparse matrices
//******************************************************************************

// Evaluates of both rows are equal
function Tsparse_Complex.R_equal(acols, bcols: PData; avals, bvals: PComplexArr): Boolean;
var
    idx,
    rlen: Integer;
begin
    Result := FALSE;                        // In case they are not equal
    if length(acols^) = length(bcols^) then   // If they have the same # of Cols
    begin
        rlen := 0;                            // First, verify if the cols are the same
        for idx := 0 to high(acols^) do
            if (acols^[idx] - bcols^[idx]) <> 0 then
                inc(rlen);
        if rlen = 0 then
            Result := TRUE;
    end;
end;

  // Returns the value contained at the specific position
function Tsparse_Complex.getvalue(row, col: Integer): Complex;
var
    go_flag: Boolean;
    i: Integer;
begin
    Result := cmplx(0, 0);
    go_flag := TRUE;
    i := 0;
    while go_flag do
    begin
        if (CData[i].Row = row) and (CData[i].Col = col) then
        begin
            Result := CData[i].Value;
            go_flag := FALSE;
        end
        else
        begin
            inc(i);
            if i > High(CData) then
                go_flag := FALSE;
        end;
    end;

end;

// Gets the columns and values at each columns for the row specified
procedure Tsparse_Complex.getrow(index: Integer; cols: PData; vals: PComplexArr);
var
    rowcols: TData;
    rowvals: TComplexArr;
    j: Integer;
begin
    setlength(rowcols, 0);
    setlength(rowvals, 0);

    for j := 0 to (len - 1) do
    begin
        if CData[j].Row = index then
        begin
            setlength(rowcols, length(rowcols) + 1);
            setlength(rowvals, length(rowvals) + 1);

            rowcols[high(rowcols)] := CData[j].Col;
            rowvals[high(rowvals)] := CData[j].Value;
        end;
    end;

    cols^ := rowcols;
    vals^ := rowvals;
end;

function Tsparse_Complex.Rank(): Integer;   // Added 08/16/2018 by DM for calculating the
var                                        // Rank of the sparse matrix
    i,
    j: Integer;
    flag: Boolean;
    acols,                                   // Row under evaluation
    bcols: TData;                             // Reference row
    avals,
    bvals: TComplexArr;

begin
    Result := 0;
    for i := 0 to (row - 1) do
    begin
        getrow(i, @acols, @avals);
        if i > 0 then
        begin
            j := i - 1;
            flag := TRUE;
            while flag and (j >= 0) do
            begin
                getrow(j, @bcols, @bvals);    // sweeps the matrix bottom up
                flag := not R_equal(@acols, @avals, @bcols, @bvals);
                dec(j);
            end;
            if flag then
                inc(Result);
        end
        else
            inc(Result);
    end;
end;

function Tsparse_Complex.NCols(): Integer;
begin
    Result := Col;
end;

function Tsparse_Complex.NRows(): Integer;
begin
    Result := Row;
end;

function Tsparse_Complex.checkifexists(r, c: Integer): Integer;
var
    i: Integer;
begin
    Result := -1;                 // Default in case the value doesn't exist
    if len > 0 then
    begin
        for i := 0 to (len - 1) do
        begin
            if (CData[i].Row = r) and (CData[i].Col = c) then
                Result := i;              // If the value exists returns the index ( >=0 )
        end;
    end;
end;

procedure Tsparse_Complex.sparse_matrix_Cmplx(r, c: Integer);
begin
    row := r;    // Initialize row
    col := c;    // Initialize Col
    len := 0;    // Initialize length to 0
    setlength(CData, 0);
end;

//Inserts elements into the sparse matrix
function Tsparse_Complex.insert(r, c: Integer; val: Complex): Integer;
var
    lrow,
    lcol: Integer;  // To store the current lenght of the data matrix
begin
    Result := 1;

    lrow := checkifexists(r, c);
    if lrow >= 0 then
    begin
        CData[lrow].Value := val;    // Assigns the new value to the existing cell
    end
    else
    begin
      // Reshapes the memory space
        lrow := length(CData);
        setlength(CData, lrow + 1);
      // Adds the data to the new memory space
        CData[high(CData)].Row := r;
        CData[high(CData)].Col := c;
        CData[high(CData)].Value := val;
        inc(len);

        if col < c then
            col := c;
        if row < r then
            row := r;
    end;

end;

// Adds another sparse matrix to this matrix
function Tsparse_Complex.add(b: Tsparse_Complex): Tsparse_Complex;
var
    addeval: Complex;
    apos,
    bpos: Integer;

begin
    // Creates a memory space to store the result
    Result := Tsparse_Complex.create;
    // First checks if the matrices have the same dimensions
    if (row <> b.row) or (col <> b.col) then
    begin
        Result.sparse_matrix_Cmplx(1, 1);
        Result.insert(0, 0, cmplx(-1, 0));
    end
    else
    begin
        apos := 0;
        bpos := 0;

        Result.sparse_matrix_Cmplx(row, col);

        while (apos < len) and (bpos < b.len) do
        begin
            if (CData[apos].Row > b.CData[bpos].Row) or ((CData[apos].Row = b.CData[bpos].Row) and (CData[apos].Col > b.CData[bpos].Col)) then
            begin
                Result.insert(b.CData[bpos].Row, b.CData[bpos].Col, b.CData[bpos].Value);
                inc(bpos)
            end
            else
            begin
                if (CData[apos].Row < b.CData[bpos].Row) or ((CData[apos].Row = b.CData[bpos].Row) and (CData[apos].Col < b.CData[bpos].Col)) then
                begin
                    Result.insert(CData[apos].Row, CData[apos].Col, CData[apos].Value);
                    inc(apos)
                end
                else
                begin
                    addeval := cadd(CData[apos].Value, b.CData[bpos].Value);
                    if (addeval.re <> 0) and (addeval.im <> 0) then
                        Result.insert(CData[apos].Row, CData[apos].Col, addeval);
                    inc(apos);
                    inc(bpos);
                end;
            end;
        end;
      // Inserts the remaining elements
        while (apos < (len - 1)) do
        begin
            Result.insert(CData[apos].Row, CData[apos].Col, CData[apos + 1].Value);
            inc(apos)
        end;
        while (bpos < (b.len - 1)) do
        begin
            Result.insert(b.CData[bpos].Row, b.CData[bpos].Col, b.CData[bpos + 1].Value);
            inc(bpos)
        end;

    end;

end;

// Transposes the sparse matrix
function Tsparse_Complex.Transpose(): Tsparse_Complex;
var
    Count,
    Index: array of Integer;
    i,
    j,
    k,
    rpos: Integer;

begin
    // Creates a memory space to store the result
    Result := Tsparse_Complex.create;
    // new matrix with inversed row X col
    Result.sparse_matrix_Cmplx(col, row);
    // same number of elements
    j := 0;
    k := 0;
    for i := 1 to len do
    begin
        Result.insert(j, k, cZERO);
        inc(k);
        if k = row then
        begin
            inc(j);
            k := 0;
        end;

    end;

    setlength(Count, col + 1);
    setlength(Index, col + 1);
    // Initialize all to 0
    for i := 0 to col do
        Count[i] := 0;
    for i := 0 to (len - 1) do
        inc(Count[CData[i].Col]);
    // to count number of elements having col smaller
    // than particular i
    // as there is no col with value < 1
    Index[0] := 0;
    // initialize rest of the indices

    for i := 1 to col do
        Index[i] := Index[i - 1] + Count[i - 1];

    for i := 0 to (len - 1) do
    begin
      // insert a data at rpos and increment its value
        rpos := index[CData[i].Col];
        inc(index[CData[i].Col]);
      // transpose row=col
        Result.CData[rpos].Row := CData[i].Col;

      // transpose col=row
        Result.CData[rpos].Col := CData[i].Row;

      // same value
        Result.CData[rpos].Value := CData[i].Value;
    end;

    // the above method ensures
    // sorting of transpose matrix
    // according to row-col value

end;

// Transposes and conjugates the sparse matrix
function Tsparse_Complex.TransposeConj(): Tsparse_Complex;
var
    Count,
    Index: array of Integer;
    i,
    rpos: Integer;

begin
    // Creates a memory space to store the result
    Result := Tsparse_Complex.create;
    // new matrix with inversed row X col
    Result.sparse_matrix_Cmplx(col, row);
    // same number of elements
    for i := 1 to len do
        Result.insert(i, 0, cmplx(0, 0));

    setlength(Count, col + 1);
    setlength(Index, col + 1);
    // Initialize all to 0
    for i := 0 to col do
        Count[i] := 0;
    for i := 0 to (len - 1) do
        inc(Count[CData[i].Col]);
    // to count number of elements having col smaller
    // than particular i
    // as there is no col with value < 1
    Index[0] := 0;
    // initialize rest of the indices

    for i := 1 to col do
        Index[i] := Index[i - 1] + Count[i - 1];

    for i := 0 to (len - 1) do
    begin
      // insert a data at rpos and increment its value
        rpos := index[CData[i].Col];
        inc(index[CData[i].Col]);
      // transpose row=col
        Result.CData[rpos].Row := CData[i].Col;

      // transpose col=row
        Result.CData[rpos].Col := CData[i].Row;

      // same value
        Result.CData[rpos].Value := Conjg(CData[i].Value);
    end;

    // the above method ensures
    // sorting of transpose matrix
    // according to row-col value

end;

// Multiplies another sparse matrix by this matrix
function Tsparse_Complex.multiply(b: Tsparse_Complex): Tsparse_Complex;
var
    sum: Complex;
    c,
    tempa,
    tempb,
    r,
    apos,
    bpos: Integer;
begin
    // Creates a memory space to store the result
    Result := Tsparse_Complex.create;
    // First checks if the matrices have the right dimensions
    if col <> b.row then
    begin
        Result.sparse_matrix_Cmplx(1, 1);
        Result.insert(0, 0, cmplx(-1, 0));    //Invalid multiplication
    end
    else
    begin
        // transpose b to compare row
        // and col values and to add them at the end
        b := b.Transpose();
        // result matrix of dimension row X b.col
        // however b has been transposed, hence row X b.row
        Result.sparse_matrix_Cmplx(row, b.row);
        // iterate over all elements of A (this matrix)
        apos := 0;
        while apos < len do
        begin
            r := CData[apos].Row;
          // iterate over all elements of B
            bpos := 0;
            while bpos < b.len do
            begin
            // current column of result matrix
            // data[][0] used as b is transposed
                c := b.CData[bpos].Row;

            // temporary pointers created to add all
            // multiplied values to obtain current
            // element of result matrix
                tempa := apos;
                tempb := bpos;

                sum := cmplx(0, 0);

            // iterate over all elements with
            // same row and col value
            // to calculate result[r]

                while (tempa < len) and (CData[tempa].Row = r) and (tempb < b.len) and (b.CData[tempb].Row = c) do
                begin
                    if (CData[tempa].Col < b.CData[tempb].Col) then
                        inc(tempa)   //skip a
                    else
                    begin
                        if (CData[tempa].Col > b.CData[tempb].Col) then
                            inc(tempb)  //skip b
                        else
                        begin
                  // same col, so multiply and increment
                            sum := cadd(sum, cmul(CData[tempa].Value, b.CData[tempb].Value));
                            inc(tempa);
                            inc(tempb);
                        end;
                    end;
                end;
            // insert sum obtained in result[r]
            // if its not equal to 0
                if (sum.re <> 0) and (sum.im <> 0) then
                    result.insert(r, c, sum);

                while (bpos < b.len) and (b.CData[bpos].Row = c) do
                    inc(bpos);    // Jump to next column
            end;
            while (apos < len) and (CData[apos].Row = r) do
                inc(apos);    // Jump to next row
        end;
    end;
end;

  // Resets the sparse matrix (makes it empty)
procedure Tsparse_Complex.reset();
begin
    setlength(CData, 0);
    len := 0;
end;
  // Returns the lenght of the sparse matrix (number of non-zero elements)
function Tsparse_Complex.NZero(): Integer;
begin
    Result := len;
end;

end.
