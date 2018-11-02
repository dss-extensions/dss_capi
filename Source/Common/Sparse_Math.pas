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
  TCmplx_Data = Record
    Row,
    Col   :   Integer;
    Value :   Complex;
  End;


  TYPE
    TData = array of integer;
    PData = ^TData;

    TComplex = array of TCmplx_Data;
    PComplex = ^TComplex;

    TComplexArr = array of Complex;
    PComplexArr = ^TComplexArr;


  TYPE

    Tsparse_matrix = class(Tobject)
    private
      row,
      col,
      len   : Integer;

      function checkifexists(r, c: Integer): Integer;
      procedure getrow(index   : Integer; cols, vals : PData);
      function R_equal(acols,avals,bcols,bvals : PData): Boolean;

    Public
      data  : array of array of integer;
      procedure sparse_matrix(r, c : Integer);
      function insert(r, c, val : Integer): Integer;
      function add(b : Tsparse_matrix): Tsparse_matrix;
      function Transpose(): Tsparse_matrix;
      function multiply(b  : Tsparse_matrix): Tsparse_matrix;
      procedure reset();
      function NZero(): Integer;
      function NCols(): Integer;
      function NRows(): Integer;
      function Rank(): Integer;
  end;

    Tsparse_Complex = class(Tobject)
    private
      row,
      col,
      len   : Integer;

      function checkifexists(r, c: Integer): Integer;
      procedure getrow(index   : Integer; cols : PData; vals : PComplexArr);
      function getvalue(row, col : Integer): Complex;
      function R_equal(acols,bcols: PData; avals,bvals : PComplexArr): Boolean;

    Public
      CData  : array of TCmplx_Data;
      procedure sparse_matrix_Cmplx(r, c : Integer);
      function insert(r, c: Integer; val : Complex): Integer;
      function add(b : Tsparse_Complex):Tsparse_Complex;
      function Transpose(): Tsparse_Complex;
      function TransposeConj(): Tsparse_Complex;
      function multiply(b  : Tsparse_Complex): Tsparse_Complex;
      procedure reset();
      function NZero(): Integer;
      function NCols(): Integer;
      function NRows(): Integer;
      function Rank(): Integer;



  end;

implementation

// Evaluates of both rows are equal
  function Tsparse_matrix.R_equal(acols,avals,bcols,bvals : PData): Boolean;
  var
    idx,
    rlen     : Integer;
  Begin
    Result  :=  False;                        // In case they are not equal
    if length(acols^) = length(bcols^) then   // If they have the same # of Cols
    Begin
      rlen  :=  0;                            // First, verify if the cols are the same
      for idx := 0 to high(acols^) do
        if (acols^[idx] - bcols^[idx]) <> 0 then inc(rlen);
      if rlen = 0 then Result  := True;
    End;
  End;
// Gets the columns and values at each columns for the row specified
  procedure Tsparse_matrix.getrow(index   : Integer; cols, vals : PData);
  var
    rowcols,
    rowvals     : TData;
    j           : Integer;
  Begin
      setlength(rowcols,0);
      setlength(rowvals,0);

      for j := 0 to (len - 1) do
      Begin
        if data[j][0] = index then
        Begin
          setlength(rowcols,length(rowcols) + 1);
          setlength(rowvals,length(rowvals) + 1);

          rowcols[high(rowcols)] := data[j][1];
          rowvals[high(rowvals)] := data[j][2];
        End;
      End;

      cols^  :=  rowcols;
      vals^  :=  rowvals;
  End;

  function Tsparse_matrix.Rank(): Integer;   // Added 08/16/2018 by DM for calculating the
  var                                        // Rank of the sparse matrix
    i,
    j           : Integer;
    flag        : Boolean;
    acols,                                   // Row under evaluation
    avals,
    bcols,                                   // Reference row
    bvals       : TData;
  Begin
    Result  :=  0;
    for i := 0 to (row-1) do
    Begin
      getrow(i,@acols,@avals);
      if i > 0 then
      Begin
        j     :=  i - 1;
        flag  :=  True;
        while flag and (j >= 0) do
        Begin
          getrow(j,@bcols,@bvals);    // sweeps the matrix bottom up
          flag  :=  Not R_equal(@acols,@avals,@bcols,@bvals);
          dec(j);
        End;
        if flag then inc(Result);
      End
      else
        inc(Result);
    End;
  End;

  function Tsparse_matrix.NCols(): Integer;
  Begin
    Result  := Col;
  End;

  function Tsparse_matrix.NRows(): Integer;
  Begin
    Result  := Row;
  End;

  function Tsparse_matrix.checkifexists(r, c: Integer): Integer;
  var
    i     : Integer;
  Begin
    Result  :=  -1;                 // Default in case the value doesn't exist
    if len > 0 then
    Begin
      for i := 0 to (len - 1) do
      Begin
        if (data[i][0] = r) and (data[i][1] = c) then
          Result  :=  i;              // If the value exists returns the index ( >=0 )
      End;
    End;
  End;

  procedure Tsparse_matrix.sparse_matrix(r, c : Integer);
  Begin
    row :=  r;    // Initialize row
    col :=  c;    // Initialize Col
    len :=  0;    // Initialize length to 0
    setlength(data,0);
  End;

//Inserts elements into the sparse matrix
  function Tsparse_matrix.insert(r, c, val : Integer): Integer;
  var
    lrow,
    lcol  : Integer;  // To store the current lenght of the data matrix
  Begin
    Result  :=  1;

    lrow  :=  checkifexists(r,c);
    if lrow >= 0 then
    Begin
      data[lrow][2] :=  val;    // Assigns the new value to the existing cell
    End
    else
    Begin
      // Reshapes the memory space
      lrow  :=  length(data);
      setlength(data, lrow + 1);
      setlength(data[lrow], 3);
      // Adds the data to the new memory space
      data[high(data)][0] :=  r;
      data[high(data)][1] :=  c;
      data[high(data)][2] :=  val;
      inc(len);

      if col < c then col :=  c;
      if row < r then row :=  r;
    End;

  End;

// Adds another sparse matrix to this matrix
  function Tsparse_matrix.add(b : Tsparse_matrix):Tsparse_matrix;
  var
    addeval,
    apos,
    bpos    : Integer;

  Begin
    // Creates a memory space to store the result
    Result := Tsparse_matrix.create;
    // First checks if the matrices have the same dimensions
    if (row <> b.row) or (col <> b.col)  then
    Begin
      Result.sparse_matrix(1,1);
      Result.insert(0,0,-1);
    End
    else
    Begin
      apos  :=  0;
      bpos  :=  0;

      Result.sparse_matrix(row,col);

      while (apos < len) and (bpos < b.len) do
      Begin
        if (data[apos][0] > b.data[bpos][0]) or ((data[apos][0] = b.data[bpos][0]) and (data[apos][1] > b.data[bpos][1])) then
        Begin
          Result.insert(b.data[bpos][0],b.data[bpos][1],b.data[bpos][2]);
          inc(bpos)
        End
        else
        Begin
          if (data[apos][0] < b.data[bpos][0]) or ((data[apos][0] = b.data[bpos][0]) and (data[apos][1] < b.data[bpos][1])) then
          Begin
            Result.insert(data[apos][0],data[apos][1],data[apos][2]);
            inc(apos)
          End
          else
          Begin
            addeval :=  data[apos][2] + b.data[bpos][2];
            if addeval <> 0 then
              Result.insert(data[apos][0],data[apos][1],addeval);
            inc(apos);
            inc(bpos);
          End;
        End;
      End;
      // Inserts the remaining elements
      while (apos < (len - 1)) do
      Begin
        Result.insert(data[apos][0],data[apos][1],data[apos + 1][2]);
        inc(apos)
      End;
      while (bpos < (b.len - 1)) do
      Begin
        Result.insert(b.data[bpos][0],b.data[bpos][1],b.data[bpos + 1][2]);
        inc(bpos)
      End;

    End;

  End;

// Transposes the sparse matrix
  function Tsparse_matrix.Transpose(): Tsparse_matrix;
  var
    Count,
    Index     : array of Integer;
    i,
    rpos      : Integer;

  Begin
    // Creates a memory space to store the result
    Result := Tsparse_matrix.create;
    // new matrix with inversed row X col
    Result.sparse_matrix(col,row);
    // same number of elements
    for i :=  1 to  len do
      Result.insert(i,0,0);

    setlength(Count,col + 1);
    setlength(Index,col + 1);
    // Initialize all to 0
    for i := 0 to col do
      Count[i]  :=  0;
    for i := 0 to (len - 1) do
      inc(Count[data[i][1]]);
    // to count number of elements having col smaller
    // than particular i
    // as there is no col with value < 1
    Index[0]  :=  0;
    // initialize rest of the indices

    for i := 1 to col do
      Index[i]  :=  Index[i - 1] + Count[i - 1];

    for i := 0 to (len - 1) do
    Begin
      // insert a data at rpos and increment its value
      rpos  :=  index[data[i][1]];
      inc(index[data[i][1]]);
      // transpose row=col
      Result.data[rpos][0]  :=  data[i][1];

      // transpose col=row
      Result.data[rpos][1]  :=  data[i][0];

      // same value
      Result.data[rpos][2]  :=  data[i][2];
    End;

    // the above method ensures
    // sorting of transpose matrix
    // according to row-col value

  End;

// Multiplies another sparse matrix by this matrix
  function Tsparse_matrix.multiply(b  : Tsparse_matrix): Tsparse_matrix;
  var
    sum,
    c,
    tempa,
    tempb,
    r,
    apos,
    bpos    : Integer;
  Begin
    // Creates a memory space to store the result
    Result := Tsparse_matrix.create;
    // First checks if the matrices have the right dimensions
    if col <> b.row then
    Begin
      Result.sparse_matrix(1,1);
      Result.insert(0,0,-1);    //Invalid multiplication
    End
    else
    Begin
        // transpose b to compare row
        // and col values and to add them at the end
        b :=  b.Transpose();
        // result matrix of dimension row X b.col
        // however b has been transposed, hence row X b.row
        Result.sparse_matrix(row,b.row);
        // iterate over all elements of A (this matrix)
        apos  :=  0;
        while apos < len do
        Begin
          r :=  data[apos][0];
          // iterate over all elements of B
          bpos  :=  0;
          while bpos < b.len do
          Begin
            // current column of result matrix
            // data[][0] used as b is transposed
            c :=  b.data[bpos][0];

            // temporary pointers created to add all
            // multiplied values to obtain current
            // element of result matrix
            tempa :=  apos;
            tempb :=  bpos;

            sum :=  0;

            // iterate over all elements with
            // same row and col value
            // to calculate result[r]

            while (tempa < len) and (data[tempa][0] = r) and (tempb < b.len) and (b.data[tempb][0] = c) do
            Begin
              if (data[tempa][1] < b.data[tempb][1]) then
                inc(tempa)   //skip a
              else
              Begin
                if (data[tempa][1] > b.data[tempb][1]) then
                  inc(tempb)  //skip b
                else
                Begin
                  // same col, so multiply and increment
                  sum :=  sum + data[tempa][2]*b.data[tempb][2];
                  inc(tempa);
                  inc(tempb);
                End;
              End;
            End;
            // insert sum obtained in result[r]
            // if its not equal to 0
            if sum <> 0 then
              result.insert(r,c,sum);

            while (bpos < b.len) and (b.data[bpos][0] = c) do
              inc(bpos);    // Jump to next column
          End;
          while (apos < len) and (data[apos][0] = r) do
            inc(apos);    // Jump to next row
        End;
    End;
  End;

  // Resets the sparse matrix (makes it empty)
  procedure Tsparse_matrix.reset();
  Begin
    setlength(data,0);
    len :=  0;
  End;
  // Returns the lenght of the sparse matrix (number of non-zero elements)
  function Tsparse_matrix.NZero(): Integer;
  Begin
    Result  :=  len;
  End;






//******************************************************************************
//*   Complex sparse matrices
//******************************************************************************

// Evaluates of both rows are equal
  function Tsparse_Complex.R_equal(acols,bcols: PData; avals,bvals : PComplexArr): Boolean;
  var
    idx,
    rlen     : Integer;
  Begin
    Result  :=  False;                        // In case they are not equal
    if length(acols^) = length(bcols^) then   // If they have the same # of Cols
    Begin
      rlen  :=  0;                            // First, verify if the cols are the same
      for idx := 0 to high(acols^) do
        if (acols^[idx] - bcols^[idx]) <> 0 then inc(rlen);
      if rlen = 0 then Result  := True;
    End;
  End;

  // Returns the value contained at the specific position
  function Tsparse_Complex.getvalue(row, col : Integer): Complex;
  var
    go_flag : Boolean;
    i       : Integer;
  Begin
    Result  :=  cmplx(0,0);
    go_flag :=  True;
    i       :=  0;
    while go_flag do
    Begin
      if (CData[i].Row = row) and (CData[i].Col = col) then
      Begin
        Result  :=  CData[i].Value;
        go_flag :=  False;
      End
      else
      Begin
        inc(i);
        if i > High(CData) then go_flag :=  False;
      End;
    End;

  End;

// Gets the columns and values at each columns for the row specified
  procedure Tsparse_Complex.getrow(index   : Integer; cols : PData; vals : PComplexArr);
  var
    rowcols     : TData;
    rowvals     : TComplexArr;
    j           : Integer;
  Begin
      setlength(rowcols,0);
      setlength(rowvals,0);

      for j := 0 to (len - 1) do
      Begin
        if CData[j].Row = index then
        Begin
          setlength(rowcols,length(rowcols) + 1);
          setlength(rowvals,length(rowvals) + 1);

          rowcols[high(rowcols)] := CData[j].Col;
          rowvals[high(rowvals)] := CData[j].Value;
        End;
      End;

      cols^  :=  rowcols;
      vals^  :=  rowvals;
  End;

  function Tsparse_Complex.Rank(): Integer;   // Added 08/16/2018 by DM for calculating the
  var                                        // Rank of the sparse matrix
    i,
    j           : Integer;
    flag        : Boolean;
    acols,                                   // Row under evaluation
    bcols       : TData;                             // Reference row
    avals,
    bvals       : TComplexArr;

  Begin
    Result  :=  0;
    for i := 0 to (row-1) do
    Begin
      getrow(i,@acols,@avals);
      if i > 0 then
      Begin
        j     :=  i - 1;
        flag  :=  True;
        while flag and (j >= 0) do
        Begin
          getrow(j,@bcols,@bvals);    // sweeps the matrix bottom up
          flag  :=  Not R_equal(@acols,@avals,@bcols,@bvals);
          dec(j);
        End;
        if flag then inc(Result);
      End
      else
        inc(Result);
    End;
  End;

  function Tsparse_Complex.NCols(): Integer;
  Begin
    Result  := Col;
  End;

  function Tsparse_Complex.NRows(): Integer;
  Begin
    Result  := Row;
  End;

  function Tsparse_Complex.checkifexists(r, c: Integer): Integer;
  var
    i     : Integer;
  Begin
    Result  :=  -1;                 // Default in case the value doesn't exist
    if len > 0 then
    Begin
      for i := 0 to (len - 1) do
      Begin
        if (CData[i].Row = r) and (CData[i].Col = c) then
          Result  :=  i;              // If the value exists returns the index ( >=0 )
      End;
    End;
  End;

  procedure Tsparse_Complex.sparse_matrix_Cmplx(r, c : Integer);
  Begin
    row :=  r;    // Initialize row
    col :=  c;    // Initialize Col
    len :=  0;    // Initialize length to 0
    setlength(CData,0);
  End;

//Inserts elements into the sparse matrix
  function Tsparse_Complex.insert(r, c: Integer; val : Complex): Integer;
  var
    lrow,
    lcol  : Integer;  // To store the current lenght of the data matrix
  Begin
    Result  :=  1;

    lrow  :=  checkifexists(r,c);
    if lrow >= 0 then
    Begin
      CData[lrow].Value :=  val;    // Assigns the new value to the existing cell
    End
    else
    Begin
      // Reshapes the memory space
      lrow  :=  length(CData);
      setlength(CData, lrow + 1);
      // Adds the data to the new memory space
      CData[high(CData)].Row :=  r;
      CData[high(CData)].Col :=  c;
      CData[high(CData)].Value :=  val;
      inc(len);

      if col < c then col :=  c;
      if row < r then row :=  r;
    End;

  End;

// Adds another sparse matrix to this matrix
  function Tsparse_Complex.add(b : Tsparse_Complex):Tsparse_Complex;
  var
    addeval : Complex;
    apos,
    bpos    : Integer;

  Begin
    // Creates a memory space to store the result
    Result := Tsparse_Complex.create;
    // First checks if the matrices have the same dimensions
    if (row <> b.row) or (col <> b.col)  then
    Begin
      Result.sparse_matrix_Cmplx(1,1);
      Result.insert(0,0,cmplx(-1,0));
    End
    else
    Begin
      apos  :=  0;
      bpos  :=  0;

      Result.sparse_matrix_Cmplx(row,col);

      while (apos < len) and (bpos < b.len) do
      Begin
        if (CData[apos].Row > b.CData[bpos].Row) or ((CData[apos].Row = b.CData[bpos].Row) and (CData[apos].Col > b.CData[bpos].Col)) then
        Begin
          Result.insert(b.CData[bpos].Row,b.CData[bpos].Col,b.CData[bpos].Value);
          inc(bpos)
        End
        else
        Begin
          if (CData[apos].Row < b.CData[bpos].Row) or ((CData[apos].Row = b.CData[bpos].Row) and (CData[apos].Col < b.CData[bpos].Col)) then
          Begin
            Result.insert(CData[apos].Row,CData[apos].Col,CData[apos].Value);
            inc(apos)
          End
          else
          Begin
            addeval :=  cadd(CData[apos].Value,b.CData[bpos].Value);
            if (addeval.re <> 0) and (addeval.im <> 0) then
              Result.insert(CData[apos].Row,CData[apos].Col,addeval);
            inc(apos);
            inc(bpos);
          End;
        End;
      End;
      // Inserts the remaining elements
      while (apos < (len - 1)) do
      Begin
        Result.insert(CData[apos].Row,CData[apos].Col,CData[apos + 1].Value);
        inc(apos)
      End;
      while (bpos < (b.len - 1)) do
      Begin
        Result.insert(b.CData[bpos].Row,b.CData[bpos].Col,b.CData[bpos + 1].Value);
        inc(bpos)
      End;

    End;

  End;

// Transposes the sparse matrix
  function Tsparse_Complex.Transpose(): Tsparse_Complex;
  var
    Count,
    Index     : array of Integer;
    i,
    rpos      : Integer;

  Begin
    // Creates a memory space to store the result
    Result := Tsparse_Complex.create;
    // new matrix with inversed row X col
    Result.sparse_matrix_Cmplx(col,row);
    // same number of elements
    for i :=  1 to  len do
      Result.insert(i,0,cmplx(0,0));

    setlength(Count,col + 1);
    setlength(Index,col + 1);
    // Initialize all to 0
    for i := 0 to col do
      Count[i]  :=  0;
    for i := 0 to (len - 1) do
      inc(Count[CData[i].Col]);
    // to count number of elements having col smaller
    // than particular i
    // as there is no col with value < 1
    Index[0]  :=  0;
    // initialize rest of the indices

    for i := 1 to col do
      Index[i]  :=  Index[i - 1] + Count[i - 1];

    for i := 0 to (len - 1) do
    Begin
      // insert a data at rpos and increment its value
      rpos  :=  index[CData[i].Col];
      inc(index[CData[i].Col]);
      // transpose row=col
      Result.CData[rpos].Row  :=  CData[i].Col;

      // transpose col=row
      Result.CData[rpos].Col  :=  CData[i].Row;

      // same value
      Result.CData[rpos].Value  :=  CData[i].Value;
    End;

    // the above method ensures
    // sorting of transpose matrix
    // according to row-col value

  End;

// Transposes and conjugates the sparse matrix
  function Tsparse_Complex.TransposeConj(): Tsparse_Complex;
  var
    Count,
    Index     : array of Integer;
    i,
    rpos      : Integer;

  Begin
    // Creates a memory space to store the result
    Result := Tsparse_Complex.create;
    // new matrix with inversed row X col
    Result.sparse_matrix_Cmplx(col,row);
    // same number of elements
    for i :=  1 to  len do
      Result.insert(i,0,cmplx(0,0));

    setlength(Count,col + 1);
    setlength(Index,col + 1);
    // Initialize all to 0
    for i := 0 to col do
      Count[i]  :=  0;
    for i := 0 to (len - 1) do
      inc(Count[CData[i].Col]);
    // to count number of elements having col smaller
    // than particular i
    // as there is no col with value < 1
    Index[0]  :=  0;
    // initialize rest of the indices

    for i := 1 to col do
      Index[i]  :=  Index[i - 1] + Count[i - 1];

    for i := 0 to (len - 1) do
    Begin
      // insert a data at rpos and increment its value
      rpos  :=  index[CData[i].Col];
      inc(index[CData[i].Col]);
      // transpose row=col
      Result.CData[rpos].Row  :=  CData[i].Col;

      // transpose col=row
      Result.CData[rpos].Col  :=  CData[i].Row;

      // same value
      Result.CData[rpos].Value  :=  Conjg(CData[i].Value);
    End;

    // the above method ensures
    // sorting of transpose matrix
    // according to row-col value

  End;

// Multiplies another sparse matrix by this matrix
  function Tsparse_Complex.multiply(b  : Tsparse_Complex): Tsparse_Complex;
  var
    sum     : Complex;
    c,
    tempa,
    tempb,
    r,
    apos,
    bpos    : Integer;
  Begin
    // Creates a memory space to store the result
    Result := Tsparse_Complex.create;
    // First checks if the matrices have the right dimensions
    if col <> b.row then
    Begin
      Result.sparse_matrix_Cmplx(1,1);
      Result.insert(0,0,cmplx(-1,0));    //Invalid multiplication
    End
    else
    Begin
        // transpose b to compare row
        // and col values and to add them at the end
        b :=  b.Transpose();
        // result matrix of dimension row X b.col
        // however b has been transposed, hence row X b.row
        Result.sparse_matrix_Cmplx(row,b.row);
        // iterate over all elements of A (this matrix)
        apos  :=  0;
        while apos < len do
        Begin
          r :=  CData[apos].Row;
          // iterate over all elements of B
          bpos  :=  0;
          while bpos < b.len do
          Begin
            // current column of result matrix
            // data[][0] used as b is transposed
            c :=  b.CData[bpos].Row;

            // temporary pointers created to add all
            // multiplied values to obtain current
            // element of result matrix
            tempa :=  apos;
            tempb :=  bpos;

            sum :=  cmplx(0,0);

            // iterate over all elements with
            // same row and col value
            // to calculate result[r]

            while (tempa < len) and (CData[tempa].Row = r) and (tempb < b.len) and (b.CData[tempb].Row = c) do
            Begin
              if (CData[tempa].Col < b.CData[tempb].Col) then
                inc(tempa)   //skip a
              else
              Begin
                if (CData[tempa].Col > b.CData[tempb].Col) then
                  inc(tempb)  //skip b
                else
                Begin
                  // same col, so multiply and increment
                  sum :=  cadd(sum,cmul(CData[tempa].Value,b.CData[tempb].Value));
                  inc(tempa);
                  inc(tempb);
                End;
              End;
            End;
            // insert sum obtained in result[r]
            // if its not equal to 0
            if (sum.re <> 0) and (sum.im <> 0) then
              result.insert(r,c,sum);

            while (bpos < b.len) and (b.CData[bpos].Row = c) do
              inc(bpos);    // Jump to next column
          End;
          while (apos < len) and (CData[apos].Row = r) do
            inc(apos);    // Jump to next row
        End;
    End;
  End;

  // Resets the sparse matrix (makes it empty)
  procedure Tsparse_Complex.reset();
  Begin
    setlength(CData,0);
    len :=  0;
  End;
  // Returns the lenght of the sparse matrix (number of non-zero elements)
  function Tsparse_Complex.NZero(): Integer;
  Begin
    Result  :=  len;
  End;

end.
