unit LoadShape;
{
  ----------------------------------------------------------
  Copyright (c) 2008-2021, Electric Power Research Institute, Inc.
  All rights reserved.
  ----------------------------------------------------------
}

{  8-18-00 Added call to InterpretDblArrayto allow File=Syntax }

{$HINTS OFF}
Interface

{The LoadShape object is a general DSS object used by all circuits
 as a reference for obtaining yearly, daily, and other load shapes.

 The values are set by the normal New and Edit procedures for any DSS object.

 The values are retrieved by setting the Code Property in the LoadShape Class.
 This sets the active LoadShape object to be the one referenced by the Code Property;

 Then the values of that code can be retrieved via the public variables.  Or you
 can pick up the ActiveLoadShapeObj object and save the direct reference to the object.

 Loadshapes default to fixed interval data.  If the Interval is specified to be 0.0,
 then both time and multiplier data are expected.  If the Interval is  greater than 0.0,
 the user specifies only the multipliers.  The Hour command is ignored and the files are
 assumed to contain only the multiplier data.

 The user may place the data in CSV or binary files as well as passing through the
 command interface. Obviously, for large amounts of data such as 8760 load curves, the
 command interface is cumbersome.  CSV files are text separated by commas, one interval to a line.
 There are two binary formats permitted: 1) a file of Singles; 2) a file of Doubles.

 For fixed interval data, only the multiplier is expected.  Therefore, the CSV format would
 contain only one number per line.  The two binary formats are packed.

 For variable interval data, (hour, multiplier) pairs are expected in both formats.

 The Mean and Std Deviation are automatically computed upon demand when new series of points is entered.

 The data may also be entered in unnormalized form.  The normalize=Yes command will force normalization.  That
 is, the multipliers are scaled so that the maximum value is 1.0.

 }

USES
   Command, DSSClass, DSSObject, UcMatrix, ucomplex, Arraydef, {$IFDEF FPC}resource{$ELSE}Windows{$ENDIF};


TYPE

   TLoadShape = class(TDSSClass)
     private


       Function Get_Code:String;  // Returns active LoadShape string
       Procedure Set_Code(const Value:String);  // sets the  active LoadShape

       Procedure DoCSVFile(Const FileName:String);
       Procedure Do2ColCSVFile(Const FileName:String);  // for P and Q pairs
       Procedure DoSngFile(Const FileName:String);
       Procedure DoDblFile(Const FileName:String);
     Protected
       Procedure DefineProperties;
       Function MakeLike(Const ShapeName:String):Integer;  Override;
     public
       constructor Create;
       destructor Destroy; override;

       Function Edit(ActorID : Integer):Integer; override;     // uses global parser
       Function Init(Handle:Integer; ActorID : Integer):Integer; override;
       Function NewObject(const ObjName:String):Integer; override;

       Function Find(const ObjName:String):Pointer; override;  // Find an obj of this class by name

       Procedure TOPExport(ObjName:String);
       function CreateMMF(const S: String; Destination : Integer): integer;
       // Set this property to point ActiveLoadShapeObj to the right value
       Property Code:String Read Get_Code  Write Set_Code;

   end;

   TLoadShapeObj = class(TDSSObject)
     private
        LastValueAccessed,
        FNumPoints          : Integer;  // Number of points in curve
        ArrayPropertyIndex  : Integer;

        iMaxP               : integer;

        FStdDevCalculated   : Boolean;
        MaxQSpecified       : Boolean;
        FMean,
        FStdDev             : Double;

        // Function Get_FirstMult:Double;
        // Function Get_NextMult :Double;
        Function Get_Interval :Double;
        procedure Set_NumPoints(const Value: Integer);
        Procedure SaveToDblFile;
        Procedure SaveToSngFile;
        Procedure CalcMeanandStdDev;
        function Get_Mean: Double;
        function Get_StdDev: Double;
        procedure Set_Mean(const Value: Double);
        procedure Set_StdDev(const Value: Double);  // Normalize the curve presently in memory
        Procedure SetMaxPandQ;

      public

        Interval            : Double;  //=0.0 then random interval     (hr)
        Hours,          // Time values (hr) if Interval > 0.0  Else nil
        PMultipliers,
        QMultipliers        : pDoubleArray;  // Multipliers

        MaxP,
        MaxQ,
        BaseP,
        BaseQ               : Double;

        Enabled,
        UseActual           : Boolean;

        {***********************************************************************
        *                    Memory mapping variables                          *
        ************************************************************************}

        UseMMF              : Boolean;            // Flag to indicated that the user wants to use MMF
        myMMF,                                    // Handle for the memory map (P)
        myFile,                                   // Handle for the file to be mapped (P)
        myQMMF,                                   // Handle for the memory map (Q)
        myQFile             : THandle;            // Handle for the file to be mapped (Q)
        myFileSizeQ,                              // File size of the file opened (P)
        myFileSize          : Cardinal;           // File size of the file opened (P)
        myFileCmdQ,
        myFileCmd           : String;             // The file definition added by the user (for moving the data window)
        myViewQ,                                  // Current view of the file mapped (Bytes - Q)
        myView              : pByte;              // Current view of the file mapped (Bytes - P)
        myFileType,                               // The file type (P)
        myFileTypeQ,                              // The file type (Q)
        myColumn,                                 // The column to read (P)
        myColumnQ,                                // The column to read (Q)
        myLineLen,                                // The size of the char line (P)
        myLineLenQ,                               // The size of the char line (Q)
        myDataSize,                               // The total data size expected (P)
        myDataSizeQ,                              // The total data size expected (Q)
        MyViewLenQ,                               // Memory View size in bytes (Q)
        MyViewLen           : Integer;            // Memory View size in bytes (P)

        //**********************************************************************

        constructor Create(ParClass:TDSSClass; const LoadShapeName:String);
        destructor  Destroy; override;

        Function  GetMult(hr:double):Complex;           // Get multiplier at specified time
        Function  Mult(i:Integer):Double;               // get multiplier by index
        Function  Hour(i:Integer):Double;               // get hour corresponding to point index
        Procedure Normalize;
        // Loads the current view of the MMF into memory for further use
        Procedure LoadMMFView(const Parmname : string; MMF: THandle; Destination : Integer);
        procedure LoadFileFeatures(ShapeType : Integer);


        FUNCTION  GetPropertyValue(Index:Integer):String;Override;
        PROCEDURE InitPropertyValues(ArrayOffset:Integer);Override;
        PROCEDURE DumpProperties(Var F:TextFile; Complete:Boolean);Override;

        Property NumPoints :Integer Read FNumPoints Write Set_NumPoints;
        Property PresentInterval :Double Read Get_Interval;
        Property Mean :Double   Read Get_Mean Write Set_Mean;
        Property StdDev :Double  Read Get_StdDev Write Set_StdDev;

        {Property FirstMult :Double Read Get_FirstMult;}
        {Property NextMult  :Double Read Get_NextMult;}

   end;

VAR
   ActiveLoadShapeObj:TLoadShapeObj;

implementation

USES  ParserDel,  DSSClassDefs, DSSGlobals, Sysutils,  MathUtil, Utilities, Classes, TOPExport, Math, PointerList;

Const NumPropsThisClass = 21;

//- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
constructor TLoadShape.Create;  // Creates superstructure for all Line objects
BEGIN
     Inherited Create;
     Class_Name := 'LoadShape';
     DSSClassType := DSS_OBJECT;

     ActiveElement := 0;

     DefineProperties;

          CommandList := TCommandList.Create(Slice(PropertyName^, NumProperties));
     CommandList.Abbrev := TRUE;
END;

//- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Destructor TLoadShape.Destroy;

BEGIN
    // ElementList and  CommandList freed in inherited destroy
    Inherited Destroy;
END;
//- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Procedure TLoadShape.DefineProperties;
Begin

     Numproperties := NumPropsThisClass;
     CountProperties;   // Get inherited property count
     AllocatePropertyArrays;


     // Define Property names
     PropertyName[1] := 'npts';           // Number of points to expect
     PropertyName[2] := 'interval';       // default = 1.0;
     PropertyName[3] := 'mult';           // vector of power multiplier values
     PropertyName[4] := 'hour';           // vextor of hour values
     PropertyName[5] := 'mean';           // set the mean (otherwise computed)
     PropertyName[6] := 'stddev';         // set the std dev (otherwise computed)
     PropertyName[7] := 'csvfile';        // Switch input to a csvfile
     PropertyName[8] := 'sngfile';        // switch input to a binary file of singles
     PropertyName[9] := 'dblfile';        // switch input to a binary file of singles
     PropertyName[10] := 'action';        // actions  Normalize
     PropertyName[11] := 'qmult';         // Q multiplier
     PropertyName[12] := 'UseActual';     // Flag to signify to use actual value
     PropertyName[13] := 'Pmax';          // MaxP value
     PropertyName[14] := 'Qmax';          // MaxQ
     PropertyName[15] := 'sinterval';     // Interval in seconds
     PropertyName[16] := 'minterval';     // Interval in minutes
     PropertyName[17] := 'Pbase';         // for normalization, use peak if 0
     PropertyName[18] := 'Qbase';         // for normalization, use peak if 0
     PropertyName[19] := 'Pmult';         // synonym for Mult
     PropertyName[20] := 'PQCSVFile';     // Redirect to a file with p, q pairs
     PropertyName[21] := 'MemoryMapping'; // Enable/disable using Memory mapping for this shape

     // define Property help values

     PropertyHelp[1] := 'Max number of points to expect in load shape vectors. This gets reset to the number of multiplier values found (in files only) if less than specified.';     // Number of points to expect
     PropertyHelp[2] := 'Time interval for fixed interval data, hrs. Default = 1. '+
                        'If Interval = 0 then time data (in hours) may be at either regular or  irregular intervals and time value must be specified using either the Hour property or input files. ' +
                        'Then values are interpolated when Interval=0, but not for fixed interval data.  ' +CRLF+CRLF+
                        'See also "sinterval" and "minterval".'; // default = 1.0;
     PropertyHelp[3] := 'Array of multiplier values for active power (P) or other key value (such as pu V for Vsource). ' + CRLF+  CRLF +
                        'You can also use the syntax: '+CRLF+  CRLF +
                        'mult = (file=filename)     !for text file one value per line'+CRLF+
                        'mult = (dblfile=filename)  !for packed file of doubles'+CRLF+
                        'mult = (sngfile=filename)  !for packed file of singles '+CRLF+
                        'mult = (file=MyCSVFile.CSV, col=3, header=yes)  !for multicolumn CSV files '+CRLF+CRLF+
                        'Note: this property will reset Npts if the  number of values in the files are fewer.' + CRLF+CRLF+
                        'Same as Pmult';     // vector of power multiplier values
     PropertyHelp[4] := 'Array of hour values. Only necessary to define for variable interval data (Interval=0).'+
                    ' If you set Interval>0 to denote fixed interval data, DO NOT USE THIS PROPERTY. ' +
                    'You can also use the syntax: '+CRLF+
                        'hour = (file=filename)     !for text file one value per line'+CRLF+
                        'hour = (dblfile=filename)  !for packed file of doubles'+CRLF+
                        'hour = (sngfile=filename)  !for packed file of singles ';     // vextor of hour values
     PropertyHelp[5] := 'Mean of the active power multipliers.  This is computed on demand the first time a '+
                     'value is needed.  However, you may set it to another value independently. '+
                     'Used for Monte Carlo load simulations.';     // set the mean (otherwise computed)
     PropertyHelp[6] := 'Standard deviation of active power multipliers.  This is computed on demand the first time a '+
                     'value is needed.  However, you may set it to another value independently.'+
                     'Is overwritten if you subsequently read in a curve' + CRLF + CRLF +
                     'Used for Monte Carlo load simulations.';   // set the std dev (otherwise computed)
     PropertyHelp[7] := 'Switch input of active power load curve data to a CSV text file '+
                        'containing (hour, mult) points, or simply (mult) values for fixed time interval data, one per line. ' +
                        'NOTE: This action may reset the number of points to a lower value.';   // Switch input to a csvfile
     PropertyHelp[8] := 'Switch input of active power load curve data to a binary file of singles '+
                        'containing (hour, mult) points, or simply (mult) values for fixed time interval data, packed one after another. ' +
                        'NOTE: This action may reset the number of points to a lower value.';  // switch input to a binary file of singles
     PropertyHelp[9] := 'Switch input of active power load curve data to a binary file of doubles '+
                        'containing (hour, mult) points, or simply (mult) values for fixed time interval data, packed one after another. ' +
                        'NOTE: This action may reset the number of points to a lower value.';   // switch input to a binary file of singles
     PropertyHelp[10] := '{NORMALIZE | DblSave | SngSave} After defining load curve data, setting action=normalize '+
                     'will modify the multipliers so that the peak is 1.0. ' +
                     'The mean and std deviation are recomputed.' +  CRLF + CRLF +
                     'Setting action=DblSave or SngSave will cause the present mult and qmult values to be written to ' +
                     'either a packed file of double or single. The filename is the loadshape name. The mult array will have a '+
                     '"_P" appended on the file name and the qmult array, if it exists, will have "_Q" appended.'; // Action
     PropertyHelp[11] := 'Array of multiplier values for reactive power (Q).  You can also use the syntax: '+CRLF+
                        'qmult = (file=filename)     !for text file one value per line'+CRLF+
                        'qmult = (dblfile=filename)  !for packed file of doubles'+CRLF+
                        'qmult = (sngfile=filename)  !for packed file of singles ' + CRLF +     // vector of qmultiplier values
                        'qmult = (file=MyCSVFile.CSV, col=4, header=yes)  !for multicolumn CSV files ';
     PropertyHelp[12] := '{Yes | No* | True | False*} If true, signifies to Load, Generator, Vsource, or other objects to ' +
                         'use the return value as the actual kW, kvar, kV, or other value rather than a multiplier. ' +
                         'Nominally for AMI Load data but may be used for other functions.';
     PropertyHelp[13] := 'kW value at the time of max power. Is automatically set upon reading in a loadshape. '+
                         'Use this property to override the value automatically computed or to retrieve the value computed.';
     PropertyHelp[14] := 'kvar value at the time of max kW power. Is automatically set upon reading in a loadshape. '+
                         'Use this property to override the value automatically computed or to retrieve the value computed.';
     PropertyHelp[15] := 'Specify fixed interval in SECONDS. Alternate way to specify Interval property.';
     PropertyHelp[16] := 'Specify fixed interval in MINUTES. Alternate way to specify Interval property.';
     PropertyHelp[17] := 'Base P value for normalization. Default is zero, meaning the peak will be used.';
     PropertyHelp[18] := 'Base Q value for normalization. Default is zero, meaning the peak will be used.';
     PropertyHelp[19] := 'Synonym for "mult".';
     PropertyHelp[20] := 'Switch input to a CSV text file containing (active, reactive) power (P, Q) multiplier pairs, one per row. '+CRLF+
                         'If the interval=0, there should be 3 items on each line: (hour, Pmult, Qmult)';
     PropertyHelp[21] := '{Yes | No* | True | False*} Enables the memory mapping functionality for dealing with large amounts of load shapes. '+CRLF+
                         'By defaul is False. Use it to accelerate the model loading when the containing a large number of load shapes.';

     ActiveProperty := NumPropsThisClass;
     inherited DefineProperties;  // Add defs of inherited properties to bottom of list

End;

//- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Function TLoadShape.NewObject(const ObjName:String):Integer;
BEGIN
  // create a new object of this class and add to list
  With ActiveCircuit[ActiveActor] Do
  Begin
  ActiveDSSObject[ActiveActor] := TLoadShapeObj.Create(Self, ObjName);
  Result := AddObjectToList(ActiveDSSObject[ActiveActor]);
  End;
END;


{*******************************************************************************
*      Loads the mapped file features into local variables for further use     *
********************************************************************************}
Procedure TLoadShapeObj.LoadFileFeatures(ShapeType : Integer);
VAR
  LocalCol,
  myType    : Integer;
  ParmName,
  Param     : String;
BEGIN

  AuxParser[ActiveActor].CmdString := myFileCmd;
  ParmName := AuxParser[ActiveActor].NextParam ;
  Param := AuxParser[ActiveActor].StrValue;
  LocalCol := 1;

  If CompareText(Parmname, 'file') = 0  THEN
  Begin
     {Default values}
    myType  :=  0;
     // Default options

    // Look for other options  (may be in either order)
    ParmName := AuxParser[ActiveActor].NextParam ;
    Param := AuxParser[ActiveActor].StrValue;
    While Length(Param) > 0 Do  Begin
        If CompareTextShortest(ParmName, 'column')= 0 Then LocalCol := AuxParser[ActiveActor].IntValue;
        ParmName := AuxParser[ActiveActor].NextParam ;
        Param := AuxParser[ActiveActor].StrValue;
    End;
  End

  ELSE   If CompareText(Parmname, 'dblfile') = 0  THEN myType  :=  1
  ELSE   If CompareText(Parmname, 'sngfile') = 0  THEN myType  :=  2;
  if ShapeType = 0 then // P
  Begin
    myFileType    :=  myType;
    myColumn      :=  LocalCol;
  End
  else
  Begin
    myFileTypeQ   :=  myType;
    myColumnQ     :=  LocalCol;
  End;

END;

{*******************************************************************************
*         Uploads the active MMF view into memory for further use              *
********************************************************************************}
Procedure TLoadShapeObj.LoadMMFView(const Parmname : string; MMF: THandle; Destination : Integer);
// Destination
//  0   : P
//  1   : Q
{$IFDEF FPC}
begin 
end;
{$ELSE}
var
  FirstPos  : Integer;
  myLastCh  : Byte;

BEGIN
  // processes the view depending on the file type
  FirstPos      :=  1;
  if Destination = 0 then
  Begin
    myView        := PByte(MapViewOfFile(MMF,FILE_MAP_READ,0,0,myViewlen));
    If CompareText(Parmname, 'file') = 0  THEN // starndard csv file
    Begin
      myLastCh      :=  myView[FirstPos];
      while myLastCh <> $0A do
      Begin
       inc(FirstPos);
       myLastCh      := myView[FirstPos];
      End;
      myLineLen     :=  FirstPos + 1;
    End
    // DBL file
    ELSE If (Length(Parmname)>0) and (CompareTextShortest(Parmname, 'dblfile') = 0) THEN myLineLen  :=  sizeof(double)
    // SGL file
    ELSE IF (Length(Parmname)>0) and (CompareTextShortest(Parmname, 'sngfile') = 0) THEN myLineLen  :=  sizeof(single);
  End
  else
  Begin
    myViewQ       := PByte(MapViewOfFile(MMF,FILE_MAP_READ,0,0,myViewlen));
    If CompareText(Parmname, 'file') = 0  THEN // starndard csv file
    Begin
      myLastCh      :=  myViewQ[FirstPos];
      while myLastCh <> $0A do
      Begin
       inc(FirstPos);
       myLastCh      := myViewQ[FirstPos];
      End;
      myLineLenQ    :=  FirstPos + 1;
    End
    // DBL file
    ELSE If (Length(Parmname)>0) and (CompareTextShortest(Parmname, 'dblfile') = 0) THEN myLineLenQ :=  sizeof(double)
    // SGL file
    ELSE IF (Length(Parmname)>0) and (CompareTextShortest(Parmname, 'sngfile') = 0) THEN myLineLenQ :=  sizeof(single);
  End;

END;
{$ENDIF}

{*******************************************************************************
*   Creates the Memory mapping for the file specified, Destination is used to  *
*   Indicate the destinaton (0 = P, 1 = Q)                                     *
********************************************************************************}

function TLoadShape.CreateMMF(const S: String; Destination : Integer): integer;
{$IFDEF FPC}
begin Result:=-1
end;
{$ELSE}
VAR

  ParmName,
  Param     : string;
  myLastCh  : Byte;
  i         : Integer;
  myLocalMMF: THandle;

Begin

  WITH ActiveLoadShapeObj DO
  BEGIN
    try

      AuxParser[ActiveActor].CmdString := S;
      ParmName       := AuxParser[ActiveActor].NextParam ;
      Param          := AuxParser[ActiveActor].StrValue;

      if fileexists(pChar(Param)) then
      Begin
        if Destination = 0 then
        Begin
          // Creating mapping for P
          // Opens the file for this instance
          myFile        := CreateFile(PChar(Param),GENERIC_READ,FILE_SHARE_READ,nil,OPEN_EXISTING,FILE_ATTRIBUTE_NORMAL,0);
          // Creates the memory map for the file
          myMMF         :=  CreateFileMapping(myFile,nil,PAGE_READONLY,0,0,nil);
          myLocalMMF    :=  myMMF;  // Assignment for working locally
          myFileCmd     :=  S;
          myFileSize    :=  GetFileSize(myFile,nil);
          myViewLen     :=  myFileSize;
        End
        else
        Begin
          // Creating mapping for Q
          // Opens the file for this instance
          myQFile       := CreateFile(PChar(Param),GENERIC_READ,FILE_SHARE_READ,nil,OPEN_EXISTING,FILE_ATTRIBUTE_NORMAL,0);
          // Creates the memory map for the file
          myQMMF        :=  CreateFileMapping(myQFile,nil,PAGE_READONLY,0,0,nil);
          myLocalMMF    :=  myQMMF; // Assignment for working locally
          myFileCmdQ    :=  S;
          myFileSizeQ   :=  GetFileSize(myFile,nil);
          myViewLenQ    :=  myFileSizeQ;
        End;

        LoadMMFView(ParmName, myLocalMMF, Destination);
        Result        :=  0;
      End
      else
      Begin
        DoSimpleMsg(Format('The file "%s" does not exist. Process cancelled.', [Param]), 800002);
        Result      :=  -1;
      End;
    Except
      DoSimpleMsg(Format('There was a proble mapping file "%s". Process cancelled.', [Param]), 800001);
      Result      :=  -1;
    end;

  END;

End;
{$ENDIF}

//- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Function TLoadShape.Edit(ActorID : Integer):Integer;
VAR
   MMFError,
   ParamPointer : Integer;
   ParamName,
   Param        : String;

BEGIN
  Result := 0;
  // continue parsing with contents of Parser
  ActiveLoadShapeObj := ElementList.Active;
  ActiveDSSObject[ActorID] := ActiveLoadShapeObj;

  WITH ActiveLoadShapeObj DO BEGIN

     ParamPointer := 0;
     ParamName := Parser[ActorID].NextParam;
     Param := Parser[ActorID].StrValue;
     WHILE Length(Param)>0 DO BEGIN
         IF Length(ParamName) = 0 THEN Inc(ParamPointer)
         ELSE ParamPointer := CommandList.GetCommand(ParamName);
 
         If (ParamPointer>0) and (ParamPointer<=NumProperties) Then PropertyValue[ParamPointer]:= Param;

         CASE ParamPointer OF
            0:  DoSimpleMsg('Unknown parameter "' + ParamName + '" for Object "' + Class_Name +'.'+ Name + '"', 610);
            1:  NumPoints := Parser[ActorID].Intvalue;
            2:  Interval := Parser[ActorID].DblValue;
            3:  BEGIN
                  if UseMMF then      // A different procedure if the user is working with MMF
                  begin
                    MMFError  :=  CreateMMF(Param, 0); // Creates MMF for P
                    if MMFError = 0 then
                    Begin
                      LoadFileFeatures(0);
                      myDataSize            :=  NumPoints;
                      ReAllocmem(PMultipliers, sizeof(PMultipliers^[1]) * 2);
                    End;
                  end
                  else
                  Begin               // Otherwise, follow the traditional technique for loading up load shapes
                   ReAllocmem(PMultipliers, Sizeof(PMultipliers^[1])*NumPoints);
                   // Allow possible Resetting (to a lower value) of num points when specifying multipliers not Hours
                   NumPoints := InterpretDblArray(Param, NumPoints, PMultipliers);   // Parser.ParseAsVector(Npts, Multipliers);
                  END;
                END;
            4:  BEGIN
                  ReAllocmem(Hours, Sizeof(Hours^[1])*NumPoints);
                  NumPoints := InterpretDblArray(Param, NumPoints, Hours);   // Parser.ParseAsVector(Npts, Hours);
                  Interval := 0.0;
                END;
            5:  Mean := Parser[ActorID].DblValue;
            6:  StdDev := Parser[ActorID].DblValue;
            7:  DoCSVFile(Param);
            8:  DoSngFile(Param);
            9:  DoDblFile(Param);
           10:  CASE lowercase(Param)[1] of
                 'n': Normalize;
                 'd': SaveToDblFile;
                 's': SaveToSngFile;
                END;
           11:  BEGIN
                  if UseMMF then      // A different procedure if the user is working with MMF
                  begin
                    MMFError  :=  CreateMMF(Param, 1);  // Creates MMF for Q
                    if MMFError = 0 then
                    Begin
                      LoadFileFeatures(1);
                      if Assigned(PMultipliers) then
                        myDataSizeQ         :=  myDataSize
                      else
                        myDataSizeQ           :=  NumPoints;
                      ReAllocmem(QMultipliers, sizeof(QMultipliers^[1]) * 2);
                    End;
                  end
                  else
                  Begin               // Otherwise, follow the traditional technique for loading up load shapes
                    ReAllocmem(QMultipliers, Sizeof(QMultipliers^[1])*NumPoints);
                    NumPoints := InterpretDblArray(Param, NumPoints, QMultipliers);   // Parser.ParseAsVector(Npts, Multipliers);
                  end;
                END;
           12:  UseActual := InterpretYesNo(Param);
           13:  MaxP := Parser[ActorID].DblValue;
           14:  MaxQ := Parser[ActorID].DblValue;
           15:  Interval := Parser[ActorID].DblValue / 3600.0;  // Convert seconds to hr
           16:  Interval := Parser[ActorID].DblValue / 60.0;  // Convert minutes to hr
           17:  BaseP := Parser[ActorID].DblValue;
           18:  BaseQ := Parser[ActorID].DblValue;
           19:  BEGIN   // same as mult
                  if UseMMF then      // A different procedure if the user is working with MMF
                  begin
                    MMFError  :=  CreateMMF(Param, 0); // Creates MMF for P
                    if MMFError = 0 then
                    Begin
                      LoadFileFeatures(0);
                      myDataSize            :=  NumPoints;
                      ReAllocmem(PMultipliers, sizeof(PMultipliers^[1]) * 2);
                    End;
                  end
                  else
                  Begin               // Otherwise, follow the traditional technique for loading up load shapes
                   ReAllocmem(PMultipliers, Sizeof(PMultipliers^[1])*NumPoints);
                   // Allow possible Resetting (to a lower value) of num points when specifying multipliers not Hours
                   NumPoints := InterpretDblArray(Param, NumPoints, PMultipliers);   // Parser.ParseAsVector(Npts, Multipliers);
                  END;
                END;
           20:  Do2ColCSVFile(Param);
           21:  UseMMF := InterpretYesNo(Param);

         ELSE
           // Inherited parameters
             ClassEdit( ActiveLoadShapeObj, ParamPointer - NumPropsThisClass)
         END;

         CASE ParamPointer OF
           3,7,8,9,11: Begin
                            FStdDevCalculated := FALSE;   // now calculated on demand
                            ArrayPropertyIndex := ParamPointer;
                            NumPoints := FNumPoints;  // Keep Properties in order for save command
                       END;
           14: MaxQSpecified := TRUE;

         END;

         ParamName := Parser[ActorID].NextParam;
         Param := Parser[ActorID].StrValue;
     END; {WHILE}

     If Assigned(PMultipliers) then  SetMaxPandQ;
  END; {WITH}
END;

function TLoadShape.Find(const ObjName: String): Pointer;
begin
      If (Length(ObjName)=0) or (CompareText(ObjName, 'none')=0) Then Result := Nil
      Else Result := Inherited Find(ObjName);
end;

//- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Function TLoadShape.MakeLike(Const ShapeName:String):Integer;
VAR
   OtherLoadShape:TLoadShapeObj;
   i:Integer;
BEGIN
   Result := 0;
   {See if we can find this line code in the present collection}
   OtherLoadShape := Find(ShapeName);
   IF OtherLoadShape<>Nil THEN
    WITH ActiveLoadShapeObj DO BEGIN
        NumPoints := OtherLoadShape.NumPoints;
        Interval := OtherLoadShape.Interval;
        ReallocMem(PMultipliers, SizeOf(PMultipliers^[1])*NumPoints);
        FOR i := 1 To NumPoints DO PMultipliers^[i] := OtherLoadShape.PMultipliers^[i];
        If Assigned(OtherLoadShape.Qmultipliers) Then Begin
          ReallocMem(QMultipliers, SizeOf(QMultipliers^[1])*NumPoints);
          FOR i := 1 To NumPoints DO QMultipliers^[i] := OtherLoadShape.QMultipliers^[i];
        End;
        IF Interval>0.0 THEN ReallocMem(Hours, 0)
        ELSE BEGIN
          ReallocMem(Hours, SizeOf(Hours^[1])*NumPoints);
          FOR i := 1 To NumPoints DO Hours^[i] := OtherLoadShape.Hours^[i];
        END;
        SetMaxPandQ;
        UseActual := OtherLoadShape.UseActual;
        UseMMF    := OtherLoadShape.UseMMF;
        BaseP     := OtherLoadShape.BaseP;
        BaseQ     := OtherLoadShape.BaseQ;


       { MaxP :=  OtherLoadShape.MaxP;
        MaxQ :=  OtherLoadShape.MaxQ;
        Mean :=  OtherLoadShape.Mean;
        StdDev := OtherLoadShape.StdDev;
       }

       For i := 1 to ParentClass.NumProperties Do PropertyValue[i] := OtherLoadShape.PropertyValue[i];
    END
   ELSE  DoSimpleMsg('Error in LoadShape MakeLike: "' + ShapeName + '" Not Found.', 611);


END;

//- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Function TLoadShape.Init(Handle:Integer; ActorID : Integer):Integer;

BEGIN
   DoSimpleMsg('Need to implement TLoadShape.Init', -1);
   REsult := 0;
END;

//- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Function TLoadShape.Get_Code:String;  // Returns active line code string
VAR
  LoadShapeObj:TLoadShapeObj;

BEGIN

  LoadShapeObj := ElementList.Active;
  Result := LoadShapeObj.Name;

END;

//- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Procedure TLoadShape.Set_Code(const Value:String);  // sets the  active LoadShape

VAR
  LoadShapeObj:TLoadShapeObj;
  
BEGIN

    ActiveLoadShapeObj := Nil;
    LoadShapeObj := ElementList.First;
    WHILE LoadShapeObj<>Nil DO BEGIN

       IF CompareText(LoadShapeObj.Name, Value)=0 THEN BEGIN
          ActiveLoadShapeObj := LoadShapeObj;
          Exit;
       END;

       LoadShapeObj := ElementList.Next;
    END;

    DoSimpleMsg('LoadShape: "' + Value + '" not Found.', 612);

END;

//- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
procedure TLoadShape.Do2ColCSVFile(const FileName: String);
{
   Process 2-column CSV file (3-col if time expected)
}
VAR
    F         : Textfile;
    MMFError,
    i         : Integer;
    s         : String;

BEGIN
  TRY
    AssignFile(F,FileName);
    Reset(F);
  EXCEPT
    DoSimpleMsg('Error Opening File: "' + FileName, 613);
    CloseFile(F);
    Exit;
  END;

  WITH ActiveLoadShapeObj DO
  BEGIN
    if UseMMF then      // A different procedure if the user is working with MMF
    begin
      CloseFile(F);
      myDataSize            :=  NumPoints;
      myFileCmd             :=  'file=' + FileName + ' column=1';      // Command for P
      MMFError              :=  CreateMMF(myFileCmd, 0);               // Creates MMF for the whole file
      myViewQ               :=  myView;
      if MMFError = 0 then
      Begin
        LoadFileFeatures(0);                                             // Features for P
        myFileCmd             :=  'file=' + FileName + ' column=2';      // Command for Q
        LoadFileFeatures(1);                                             // Features for Q
        myDataSize            :=  NumPoints;
        myLineLenQ            :=  myLineLen;
        ReAllocmem(PMultipliers, sizeof(PMultipliers^[1]) * 2);
        ReAllocmem(QMultipliers, sizeof(QMultipliers^[1]) * 2);
      End;
    END

    else
    Begin
      TRY
        // Allocate both P and Q multipliers
        ReAllocmem(PMultipliers, Sizeof(PMultipliers^[1])*NumPoints);
        ReAllocmem(QMultipliers, Sizeof(QMultipliers^[1])*NumPoints);
        IF Interval=0.0 THEN ReAllocmem(Hours, Sizeof(Hours^[1])*NumPoints);
        i := 0;
        WHILE (NOT EOF(F)) AND (i<FNumPoints) DO
        BEGIN
          Inc(i);
          Readln(F, s); // read entire line  and parse with AuxParser
          {AuxParser allows commas or white space}
          With AuxParser[ActiveActor] Do Begin
              CmdString := s;
              IF Interval=0.0 THEN Begin
                 NextParam; Hours^[i] := DblValue;
              End;
              NextParam; PMultipliers^[i] := DblValue;  // first parm
              NextParam; QMultipliers^[i] := DblValue;  // second parm
          End;
        END;
        CloseFile(F);
        If i<>FNumPoints Then NumPoints := i;

      EXCEPT
        On E:Exception Do Begin
          DoSimpleMsg('Error Processing CSV File: "' + FileName + '. ' + E.Message , 614);
          CloseFile(F);
          Exit;
        End;
      END;
    End;

  END;
end;

Procedure TLoadShape.DoCSVFile(Const FileName:String);

VAR

    MMFError,
    i         : Integer;
    s         : String;
    F         : Textfile;

BEGIN

  TRY
    AssignFile(F,FileName);
    Reset(F);
  EXCEPT
    DoSimpleMsg('Error Opening File: "' + FileName, 613);
    CloseFile(F);
    Exit;
  END;
  WITH ActiveLoadShapeObj DO
  BEGIN
    if UseMMF then      // A different procedure if the user is working with MMF
    begin
      CloseFile(F);
      s                     :=  'file=' + FileName;
      MMFError  :=  CreateMMF(s, 0); // Creates MMF for P
      if MMFError = 0 then
      Begin
        LoadFileFeatures(0);
        myDataSize            :=  NumPoints;
        ReAllocmem(PMultipliers, sizeof(PMultipliers^[1]) * 2);
      End;
    END
    else
    Begin
      TRY
       ReAllocmem(PMultipliers, Sizeof(PMultipliers^[1])*NumPoints);
       IF Interval=0.0 THEN ReAllocmem(Hours, Sizeof(Hours^[1])*NumPoints);
       i := 0;
        WHILE (NOT EOF(F)) AND (i<FNumPoints) DO
        BEGIN
          Inc(i);
          Readln(F, s); // read entire line  and parse with AuxParser
          {AuxParser allows commas or white space}
          With AuxParser[ActiveActor] Do Begin
            CmdString := s;
            IF Interval=0.0 THEN Begin
               NextParam; Hours^[i] := DblValue;
            End;
            NextParam; PMultipliers^[i] := DblValue;
          End;
        END;
        CloseFile(F);
        If i<>FNumPoints Then NumPoints := i;

      EXCEPT
        On E:Exception Do Begin
          DoSimpleMsg('Error Processing CSV File: "' + FileName + '. ' + E.Message , 614);
          CloseFile(F);
          Exit;
        End;
      END;
    End;
  END;

END;

//- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Procedure TLoadShape.DoSngFile(Const FileName:String);
VAR
  s         : string;
  F         : File of Single;
  Hr,M      : Single;
  mMFError,
  i         : Integer;

BEGIN
  TRY
    AssignFile(F,FileName);
    Reset(F);
  EXCEPT
    DoSimpleMsg('Error Opening File: "' + FileName, 615);
    CloseFile(F);
    Exit;
  END;

  WITH ActiveLoadShapeObj DO
  BEGIN
    if UseMMF then      // A different procedure if the user is working with MMF
    begin
      CloseFile(F);
      s                     :=  'sngfile=' + FileName;
      MMFError  :=  CreateMMF(s, 0); // Creates MMF for P
      if MMFError = 0 then
      Begin
        LoadFileFeatures(0);
        myDataSize            :=  NumPoints;
        ReAllocmem(PMultipliers, sizeof(PMultipliers^[1]) * 2);
      End;
    END
    else
    Begin
      TRY
        ReAllocmem(PMultipliers, Sizeof(PMultipliers^[1])*NumPoints);
        IF Interval=0.0 THEN ReAllocmem(Hours, Sizeof(Hours^[1])*NumPoints);
        i := 0;
        WHILE (NOT EOF(F)) AND (i<FNumPoints) DO
        BEGIN
          Inc(i);
          IF Interval=0.0 THEN BEGIN Read(F, Hr); Hours^[i] := Hr; END;
          Read(F, M ); PMultipliers^[i] := M;
        END;
        CloseFile(F);
        If i<>FNumPoints Then NumPoints := i;
      EXCEPT
        DoSimpleMsg('Error Processing LoadShape File: "' + FileName, 616);
        CloseFile(F);
        Exit;
      END;
    End;
  END;
END;

//- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Procedure TLoadShape.DoDblFile(Const FileName:String);
VAR
  s         : string;
  F         : File of double;
  MMFError,
  i         : Integer;

BEGIN
  TRY
    AssignFile(F,FileName);
    Reset(F);
  EXCEPT
    DoSimpleMsg('Error Opening File: "' + FileName, 617);
    CloseFile(F);
    Exit;
  END;

  WITH ActiveLoadShapeObj DO
  BEGIN
    if UseMMF then      // A different procedure if the user is working with MMF
    begin
      CloseFile(F);
      s                     :=  'dblfile=' + FileName;
      MMFError  :=  CreateMMF(s, 0); // Creates MMF for P
      if MMFError = 0 then
      Begin
        LoadFileFeatures(0);
        myDataSize            :=  NumPoints;
        ReAllocmem(PMultipliers, sizeof(PMultipliers^[1]) * 2);
      End;
    END
    else
    Begin
      TRY
        ReAllocmem(PMultipliers, Sizeof(PMultipliers^[1])*NumPoints);
        IF Interval=0.0 THEN ReAllocmem(Hours, Sizeof(Hours^[1])*NumPoints);
        i := 0;
        WHILE (NOT EOF(F)) AND (i<FNumPoints) DO BEGIN
          Inc(i);
          IF Interval=0.0 THEN Read(F, Hours^[i]);
          Read(F, PMultipliers^[i]);
        END;
        CloseFile(F);
        If i<>FNumPoints Then NumPoints := i;
      EXCEPT
        DoSimpleMsg('Error Processing LoadShape File: "' + FileName, 618);
        CloseFile(F);
        Exit;
      END;
    End;
  END;

END;

//- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
//      TLoadShape Obj
//- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

constructor TLoadShapeObj.Create(ParClass:TDSSClass; const LoadShapeName:String);

BEGIN
     Inherited Create(ParClass);
     Name := LowerCase(LoadShapeName);
     DSSObjType := ParClass.DSSClassType;

     LastValueAccessed  :=  1;

     FNumPoints         :=  0;
     Interval           :=  1.0;  // hr
     Hours              :=  Nil;
     PMultipliers       :=  Nil;
     QMultipliers       :=  Nil;
     MaxP               :=  1.0;
     MaxQ               :=  0.0;
     BaseP              :=  0.0;
     BaseQ              :=  0.0;
     UseActual          :=  FALSE;
     UseMMF             :=  FALSE;  // No memory mapping by default
     MaxQSpecified      :=  FALSE;
     FStdDevCalculated  :=  FALSE;  // calculate on demand
     Enabled            :=  TRUE;

     myViewLen          :=  1000;   // 1kB by default, it may change for not missing a row

     ArrayPropertyIndex :=  0;

     InitPropertyValues(0);

END;

//- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
destructor TLoadShapeObj.Destroy;
BEGIN
    If Assigned(Hours)        Then  ReallocMem(Hours, 0);
    If Assigned(PMultipliers) Then  ReallocMem(PMultipliers, 0);
    If Assigned(QMultipliers) Then  ReallocMem(QMultipliers, 0);
{$IFNDEF FPC}
    if UseMMF then
    Begin
      UnmapViewOfFile(myView);
      CloseHandle(myMMF);
      CloseHandle(myFile);
      CloseHandle(myQMMF);
      CloseHandle(myQFile);
    End;
{$ENDIF}
    Inherited destroy;
END;

//- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Function TLoadShapeObj.GetMult(hr:double):Complex;

// This function returns a multiplier for the given hour.
// If no points exist in the curve, the result is  1.0
// If there are fewer points than requested, the curve is simply assumed to repeat
// Thus a daily load curve can suffice for a yearly load curve:  You just get the
// same day over and over again.
// The value returned is the nearest to the interval requested.  Thus if you request
// hour=12.25 and the interval is 1.0, you will get interval 12.

VAR
  FileType    : string;
  MMFound     : Boolean;
  LocalPage,
  UpLimit,
  LowLimit,
  Index,
  j,
  i           : Integer;

  Function Set_Result_im(const realpart:double):Double;
  {Set imaginary part of Result when Qmultipliers not defined}
  Begin
    If UseActual Then Set_Result_im := 0.0       // if actual, assume zero
                Else Set_Result_im := realpart; // same as real otherwise
  End;

BEGIN

  Result.re := 1.0;
  Result.im := 1.0;    // default return value if no points in curve

  IF FNumPoints > 0 THEN         // Handle Exceptional cases
    IF FNumPoints = 1 THEN
    Begin
      Result.re := PMultipliers^[1];
      If Assigned(QMultipliers) Then Result.im := QMultipliers^[1]
      Else Result.im := Set_Result_im(Result.re);
    End
    ELSE
    BEGIN
      IF Interval > 0.0 THEN                                      // Using Interval
      BEGIN
        Index         :=  round(hr/Interval);
        if UseMMF then
        BEGIN
          IF Index > myDataSize Then Index := Index Mod myDataSize;  // Wrap around using remainder
          IF Index = 0 THEN Index := myDataSize;
          Result.re     :=  InterpretDblArrayMMF(myView, myFileType, myColumn, Index, myLineLen);
          If Assigned(QMultipliers) Then Result.im := InterpretDblArrayMMF(myViewQ, myFileTypeQ, myColumnQ, Index, myLineLenQ)
          Else  Result.im  := Set_Result_im(Result.re );
        END
        ELSE
        BEGIN
          IF Index > FNumPoints Then Index := Index Mod FNumPoints;  // Wrap around using remainder
          IF Index = 0 THEN Index := FNumPoints;
          Result.re     :=  PMultipliers^[Index];
          If Assigned(QMultipliers) Then Result.im := QMultipliers^[Index]
          Else  Result.im  := Set_Result_im(Result.re );
        END;
      END
      ELSE
      BEGIN
      // For random interval
        { Start with previous value accessed under the assumption that most
          of the time, this function will be called sequentially}
        {Normalize Hr to max hour in curve to get wraparound}
        If Hr > Hours^[FNumPoints] Then
          Hr := Hr - Trunc(Hr/Hours^[FNumPoints])*Hours^[FNumPoints];


        IF Hours^[LastValueAccessed] > Hr THEN LastValueAccessed := 1;  // Start over from beginning
        FOR i := LastValueAccessed + 1 TO FNumPoints DO
        BEGIN
          IF Abs(Hours^[i]-Hr)<0.00001 THEN  // If close to an actual point, just use it.
          BEGIN
            if UseMMF then
            BEGIN
              Result.re     :=  InterpretDblArrayMMF(myView, myFileType, myColumn, Index, myLineLen);
              If Assigned(QMultipliers) Then Result.im := InterpretDblArrayMMF(myViewQ, myFileTypeQ, myColumnQ, Index, myLineLenQ)
              Else  Result.im  := Set_Result_im(Result.re );
            END
            ELSE
            BEGIN
              Result.re := PMultipliers^[i];
              If Assigned(QMultipliers) Then Result.im := QMultipliers^[i]
              Else  Result.im := Set_Result_im(Result.re);
            END;
            LastValueAccessed := i;
            Exit;
          END
          ELSE IF Hours^[i]>Hr THEN      // Interpolate for multiplier
          BEGIN
            LastValueAccessed := i-1;
            if UseMMF then
            BEGIN
              Result.re     :=  InterpretDblArrayMMF(myView, myFileType, myColumn, LastValueAccessed, myLineLen) +
                        (Hr - Hours^[LastValueAccessed]) / (Hours^[i] - Hours^[LastValueAccessed])*
                        (InterpretDblArrayMMF(myView, myFileType, myColumn, i, myLineLen) -
                        InterpretDblArrayMMF(myView, myFileType, myColumn, LastValueAccessed, myLineLen));
              If Assigned(QMultipliers) Then
                  Result.im := InterpretDblArrayMMF(myViewQ, myFileTypeQ, myColumnQ, LastValueAccessed, myLineLenQ) +
                        (Hr - Hours^[LastValueAccessed]) / (Hours^[i] - Hours^[LastValueAccessed])*
                        (InterpretDblArrayMMF(myViewQ, myFileTypeQ, myColumnQ, i, myLineLenQ) -
                        InterpretDblArrayMMF(myViewQ, myFileTypeQ, myColumnQ, LastValueAccessed, myLineLenQ))
              Else  Result.im  := Set_Result_im(Result.re );
            END
            ELSE
            BEGIN
              Result.re := PMultipliers^[LastValueAccessed] +
                       (Hr - Hours^[LastValueAccessed]) / (Hours^[i] - Hours^[LastValueAccessed])*
                       (PMultipliers^[i] -PMultipliers^[LastValueAccessed]);
              If Assigned(QMultipliers) Then
                  Result.im := QMultipliers^[LastValueAccessed] +
                       (Hr - Hours^[LastValueAccessed]) / (Hours^[i] - Hours^[LastValueAccessed])*
                       (QMultipliers^[i] -QMultipliers^[LastValueAccessed])
              Else Result.im := Set_Result_im(Result.re);
            END;
            Exit ;
          END;
        END;
        // If we fall through the loop, just use last value
        LastValueAccessed := FNumPoints-1;
        Result.re := PMultipliers^[FNumPoints];
        If Assigned(QMultipliers) Then Result.im := QMultipliers^[FNumPoints]
        Else  Result.im := Set_Result_im(Result.re);
      END;
    END;

END;


//- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Procedure TLoadShapeObj.Normalize;
// normalize this load shape
VAR
  MaxMult:Double;

  Procedure DoNormalize(Multipliers:pDoubleArray);
  Var i:Integer;
  Begin
    If FNumPoints>0 THEN BEGIN
      if MaxMult <= 0.0 then begin
        MaxMult := Abs(Multipliers^[1]);
        FOR i := 2 TO FNumPoints DO   MaxMult := Max(MaxMult, Abs(Multipliers^[i]));
      end;
      IF MaxMult = 0.0 THEN MaxMult := 1.0; // Avoid divide by zero
      FOR i := 1 TO FNumPoints DO Multipliers^[i] := Multipliers^[i]/MaxMult;
    END;
  End;

BEGIN
  if UseMMF then
    DoSimpleMsg('Normalize is not possible when working in memory mapping mode"', 2000001)
  else
  Begin
    MaxMult:=BaseP;
    DoNormalize(PMultipliers);
    If Assigned(QMultipliers) Then Begin
      MaxMult:=BaseQ;
      DoNormalize(QMultipliers);
    end;
    UseActual := FALSE;  // not likely that you would want to use the actual if you normalized it.
  end;
END;

//- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Procedure TLoadShapeObj.CalcMeanandStdDev;

BEGIN

  If FNumPoints > 0 Then
    if Interval > 0.0 then
      RCDMeanandStdDev(PMultipliers, FNumPoints, FMean, FStdDev)
    else
      CurveMeanAndStdDev (PMultipliers, Hours, FNumPoints, FMean, FStdDev);

    PropertyValue[5] := Format('%.8g', [FMean]);
    PropertyValue[6] := Format('%.8g', [FStdDev]);

    FStdDevCalculated := TRUE;
   { No Action is taken on Q multipliers}
END;

(*
Function TLoadShapeObj.Get_FirstMult:Double;
Begin

  If Npts>0 Then Begin
     Result :=  Multipliers^[1];
     LastValueAccessed := 1;
  End
  Else
      Result := 0.0;

End;

Function TLoadShapeObj.Get_NextMult :Double;
Begin

  If Npts>0 Then Begin
     Inc(LastValueAccessed);
     If LastValueAccessed>Npts Then Begin
         Result := 0.0;
         Dec(LastValueAccessed);
     End
     Else Begin
          Result :=  Multipliers^[LastValueAccessed];
     End;
  End Else
      Result := 0.0;

End;
*)
Function TLoadShapeObj.Get_Interval :Double;
Begin

     If Interval>0.0 Then Result := Interval
     Else Begin
         If LastValueAccessed>1 Then
            Result := Hours^[LastValueAccessed] - Hours^[LastValueAccessed - 1]
         Else
            Result := 0.0;
     End;


End;

function TLoadShapeObj.Get_Mean: Double;
begin
     If Not FStdDevCalculated then  CalcMeanandStdDev;
     Result := FMean;
end;

function TLoadShapeObj.Get_StdDev: Double;
begin
    If Not FStdDevCalculated then  CalcMeanandStdDev;
    Result := FStdDev;
end;

Function TLoadShapeObj.Mult(i:Integer) :Double;
Begin

  If (i <= FNumPoints) and (i > 0) Then
  Begin
    if UseMMF then Result := InterpretDblArrayMMF(myView, myFileType, myColumn, i, myLineLen)
    else Result := PMultipliers^[i];
    LastValueAccessed := i;
  End
  Else
    Result := 0.0;

End;

FUNCTION TLoadShapeObj.Hour(i:Integer) :Double;
Begin

   If Interval = 0 Then Begin
     If (i<= FNumPoints) and (i>0) Then Begin
      Result := Hours^[i];
      LastValueAccessed := i;
     End Else
      Result := 0.0;
   End Else Begin
       Result := Hours^[i] * Interval;
       LastValueAccessed := i;
   End;

End;


PROCEDURE TLoadShapeObj.DumpProperties(var F: TextFile; Complete: Boolean);

Var
   i :Integer;

Begin
    Inherited DumpProperties(F, Complete);

    WITH ParentClass Do
     FOR i := 1 to NumProperties Do
     Begin
          Writeln(F,'~ ',PropertyName^[i],'=',PropertyValue[i]);
     End;


end;

FUNCTION TLoadShapeObj.GetPropertyValue(Index: Integer): String;
begin
  Result := '';

  CASE Index of
    2: Result := Format('%.8g', [Interval]);
    3: Begin
        if UseMMF then Result := '(' + myFileCmd + ')'
        else  Result := GetDSSArray_Real( FNumPoints, PMultipliers);
      End;
    4: IF Hours <> Nil THEN Result := GetDSSArray_Real( FNumPoints, Hours) ;
    5: Result := Format('%.8g', [Mean ]);
    6: Result := Format('%.8g', [StdDev ]);
    11: Begin
        IF Assigned(QMultipliers) Then
          if UseMMF then Result := '(' + myFileCmdQ + ')'
          else Result := GetDSSArray_Real( FNumPoints, QMultipliers);
      End;
    12: If UseActual then Result := 'Yes' else Result := 'No';
    13: Result := Format('%.8g', [MaxP ]);
    14: Result := Format('%.8g', [MaxQ ]);
    15: Result := Format('%.8g', [Interval * 3600.0 ]);
    16: Result := Format('%.8g', [Interval * 60.0 ]);
    17: Result := Format('%.8g', [BaseP ]);
    18: Result := Format('%.8g', [BaseQ ]);
    19: Begin
        if UseMMF then Result := '(' + myFileCmd + ')'
        else  Result := GetDSSArray_Real( FNumPoints, PMultipliers);
      End;
    21: If UseMMF then Result := 'Yes' else Result := 'No';
  ELSE
     Result := Inherited GetPropertyValue(index);
  END;

end;

procedure TLoadShapeObj.InitPropertyValues(ArrayOffset: Integer);
begin

     PropertyValue[1] := '0';     // Number of points to expect
     PropertyValue[2] := '1'; // default = 1.0 hr;
     PropertyValue[3] := '';     // vector of multiplier values
     PropertyValue[4] := '';     // vextor of hour values
     PropertyValue[5] := '0';     // set the mean (otherwise computed)
     PropertyValue[6] := '0';   // set the std dev (otherwise computed)
     PropertyValue[7] := '';   // Switch input to a csvfile
     PropertyValue[8] := '';  // switch input to a binary file of singles
     PropertyValue[9] := '';   // switch input to a binary file of singles
     PropertyValue[10] := ''; // action option .
     PropertyValue[11] := ''; // Qmult.
     PropertyValue[12] := 'No';
     PropertyValue[13] := '0';
     PropertyValue[14] := '0';
     PropertyValue[15] := '3600';   // seconds
     PropertyValue[16] := '60';     // minutes
     PropertyValue[17] := '0';
     PropertyValue[18] := '0';
     PropertyValue[19] := '';   // same as 3
     PropertyValue[20] := '';  // switch input to csv file of P, Q pairs
     PropertyValue[21] := 'No';  // memory mapped load shape



    inherited  InitPropertyValues(NumPropsThisClass);

end;

procedure TLoadShape.TOPExport(ObjName:String);

Var
   NameList, CNames:TStringList;
   Vbuf, CBuf:pDoubleArray;
   Obj:TLoadShapeObj;
   MaxPts, i, j:Integer;
   MaxTime, MinInterval, Hr_Time :Double;
   ObjList:TPointerList;

begin
     TOPTransferFile.FileName := GetOutputDirectory + 'TOP_LoadShape.STO';
     TRY
         TOPTransferFile.Open;
     EXCEPT
        ON E:Exception Do
        Begin
          DoSimpleMsg('TOP Transfer File Error: '+E.message, 619);
          TRY
              TopTransferFile.Close;
          EXCEPT
              {OK if Error}
          End;
          Exit;
        End;
     END;

     {Send only fixed interval data}

     ObjList := TPointerList.Create(10);
     NameList := TStringList.Create;
     CNames := TStringList.Create;

     {Make a List of fixed interval data where the interval is greater than 1 minute}
     IF CompareText(ObjName, 'ALL')=0 Then Begin
       Obj := ElementList.First;
       While Obj <>  Nil Do Begin
          If Obj.Interval>(1.0/60.0) Then ObjList.Add(Obj);
          Obj := ElementList.Next;
       End;
     End
     ELSE Begin
       Obj := Find(ObjName);
       If Obj <>  Nil Then Begin
          If Obj.Interval>(1.0/60.0) Then ObjList.Add(Obj)
          Else DoSimpleMsg('Loadshape.'+ObjName+' is not hourly fixed interval.', 620);
       End
       Else Begin
           DoSimpleMsg('Loadshape.'+ObjName+' not found.', 621);
       End;

     End;

     {If none found, exit}
     If ObjList.ListSize >0 Then Begin

       {Find Max number of points}
       MaxTime := 0.0;
       MinInterval := 1.0;
       Obj := ObjList.First;
       While Obj <>  Nil Do Begin
          MaxTime :=  Max(MaxTime, Obj.NumPoints*Obj.Interval) ;
          MinInterval := Min(MinInterval, Obj.Interval);
          NameList.Add(Obj.Name);
          Obj := ObjList.Next;
       End;
      // SetLength(Xarray, maxPts);
       MaxPts := Round(MaxTime/MinInterval);

       TopTransferFile.WriteHeader(0.0, MaxTime, MinInterval, ObjList.ListSize, 0, 16,  'OpenDSS(TM), EPRI (R)');
       TopTransferFile.WriteNames(NameList, CNames);

       Hr_Time := 0.0;

       VBuf := AllocMem(Sizeof(VBuf^[1])* ObjList.ListSize);
       CBuf := AllocMem(Sizeof(VBuf^[1])* 1);   // just a dummy -- Cbuf is ignored here

       For i := 1 to MaxPts Do Begin
          For j := 1 to ObjList.ListSize Do Begin
              Obj := ObjList.Get(j);
              VBuf^[j] :=  Obj.GetMult (Hr_Time).Re;
          End;
          TopTransferFile.WriteData(HR_Time, Vbuf, Cbuf);
          HR_Time := HR_Time + MinInterval;
       End;

       TopTransferFile.Close;
       TopTransferFile.SendToTop;
       Reallocmem(Vbuf,0);
       Reallocmem(Cbuf,0);
     End;
     
     ObjList.Free;
     NameList.Free;
     CNames.Free;
end;

procedure TLoadShapeObj.SaveToDblFile;

Var
  myDBL       : Double;
  F           : File of Double;
  i           : Integer;
  Fname       : String;

begin
   If Assigned(PMultipliers) then  Begin
    TRY
      FName := Format('%s_P.dbl',[Name]);
      AssignFile(F, Fname);
      Rewrite(F);
      if UseMMF then
      Begin
        For i := 1 to NumPoints Do
        Begin
          myDBL    :=  InterpretDblArrayMMF(myView, myFileType, myColumn, i, myLineLen);
          Write(F, myDBL);
        End;
      End
      else
        For i := 1 to NumPoints Do  Write(F, PMultipliers^[i]);
      GlobalResult := 'mult=[dblfile='+FName+']';
    FINALLY
      CloseFile(F);
    END;

      If Assigned(QMultipliers) then  Begin
        TRY
          FName := Format('%s_Q.dbl',[Name]);
          AssignFile(F, Fname);
          Rewrite(F);
          if UseMMF then
          Begin
            For i := 1 to NumPoints Do
            Begin
              myDBL    :=  InterpretDblArrayMMF(myViewQ, myFileTypeQ, myColumnQ, i, myLineLenQ);
              Write(F, myDBL);
            End;
          End
          else
            For i := 1 to NumPoints Do  Write(F, QMultipliers^[i]);
          AppendGlobalResult(' Qmult=[dblfile='+FName+']');
        FINALLY
          CloseFile(F);
        END;
      End;

   End
   Else DoSimpleMsg('Loadshape.'+Name + ' P multipliers not defined.', 622);
end;

procedure TLoadShapeObj.SaveToSngFile;

Var

  F           : File of Single;
  i           : Integer;
  Fname       : String;
  Temp        : Single;

begin
  If Assigned(PMultipliers) then
  Begin
    TRY
      FName := Format('%s_P.sng',[Name]);
      AssignFile(F, Fname);
      Rewrite(F);
      For i := 1 to NumPoints Do
      Begin
          if UseMMF then Temp :=  InterpretDblArrayMMF(myView, myFileType, myColumn, i, myLineLen)
          else Temp := PMultipliers^[i] ;
          Write(F, Temp);
      End;
      GlobalResult := 'mult=[sngfile='+FName+']';
    FINALLY
      CloseFile(F);
    END;

    If Assigned(QMultipliers) then  Begin
      TRY
        FName := Format('%s_Q.sng',[Name]);
        AssignFile(F, Fname);
        Rewrite(F);
        For i := 1 to NumPoints Do
        Begin
            if UseMMF then Temp :=  InterpretDblArrayMMF(myViewQ, myFileTypeQ, myColumnQ, i, myLineLenQ)
            else Temp := QMultipliers^[i] ;
            Write(F, Temp);
        End;
        AppendGlobalResult(' Qmult=[sngfile='+FName+']');
      FINALLY
        CloseFile(F);
      END;
    End;

  End
  Else DoSimpleMsg('Loadshape.'+Name + ' P multipliers not defined.', 623);

end;

procedure TLoadShapeObj.SetMaxPandQ;
begin
  if not UseMMF then
  Begin
    iMaxP := iMaxAbsdblArrayValue(NumPoints, PMultipliers);
    If iMaxP > 0 Then Begin
      MaxP := PMultipliers^[iMaxP];
      If Not MaxQSpecified Then
        If Assigned(QMultipliers) then  MaxQ := QMultipliers^[iMaxP]
        else MaxQ := 0.0;
    End;
  End;
end;

procedure TLoadShapeObj.Set_Mean(const Value: Double);
begin
      FStdDevCalculated := TRUE;
      FMean := Value;
end;

procedure TLoadShapeObj.Set_NumPoints(const Value: Integer);
begin

        PropertyValue[1] := IntToStr(Value);   // Update property list variable

        // Reset array property values to keep them in propoer order in Save

        If ArrayPropertyIndex>0   Then  PropertyValue[ArrayPropertyIndex] := PropertyValue[ArrayPropertyIndex];
        If Assigned(Qmultipliers) Then  PropertyValue[11] := PropertyValue[11];

        FNumPoints := Value;   // Now assign the value

end;

procedure TLoadShapeObj.Set_StdDev(const Value: Double);
begin
      FStdDevCalculated := TRUE;
      FStdDev := Value;
end;

end.
