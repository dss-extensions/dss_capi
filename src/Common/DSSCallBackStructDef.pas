TYPE

  // NOTE: Maxlen argument is to better accommodate Fortran strings.  VB also
  //       Caller must allocate space for pchar values       
   pDSSCallBacks = ^TDSSCallBacks;  {Pointer to callback structure}
   TDSSCallBacks = {$IFNDEF DSS_CAPI_NO_PACKED_RECORDS}Packed{$ENDIF} Record

        MsgCallBack: Procedure (S : pAnsiChar; Maxlen:UInt32);Stdcall; {Make use of DSS Message handling}

        // Routines for using DSS Parser.  This allows you to write models that accept
        // syntax like other DSS scripts.
        GetIntValue: Procedure(var i : Int32);Stdcall; {Get next param as an Int32}
        GetDblValue: Procedure(var x : Double); Stdcall;  {Get next param as a double}
        GetStrValue: Procedure(s : pAnsiChar; maxlen : UInt32); Stdcall;
        //Get next param as a string <= maxlen characters  (UInt32 = 32-bit unsigned)
        //caller must allocate space for s (Maxlen chars)
        LoadParser:  Procedure(S : pAnsiChar; maxlen : UInt32); Stdcall; // Copies a string into a special instance of the DSS parser
        NextParam:   Function (ParamName : pAnsiChar; Maxlen : UInt32):Int32; Stdcall;
        //  Advance to the next parameter and
        //  Get name of the param just retrieved, if one was given.
        //  Returns length of parameter found.  If 0, then end of string.
        //  This is to handle the syntax "paramname=paramvalue" commonly used in DSS scripts
        //  Copies the string to the location specified by s up to maxlen characters.
        //  Caller must allocate space (Maxlen chars)

        DoDSSCommand:             Procedure(S : pAnsiChar; Maxlen : UInt32); StdCall;
        GetActiveElementBusNames: Procedure(Name1 : pAnsiChar; Len1 : UInt32; Name2 : pAnsiChar; Len2 : UInt32); StdCall;
        GetActiveElementVoltages: Procedure(Var NumVoltages : Int32; V : pComplexArray); StdCall;
        GetActiveElementCurrents: Procedure(Var NumCurrents : Int32; Curr : pComplexArray); StdCall;
        GetActiveElementLosses:   Procedure(Var TotalLosses, LoadLosses, NoLoadLosses : Complex); StdCall;
        GetActiveElementPower:    Procedure(Terminal : Int32; Var TotalPower : Complex); StdCall;
        GetActiveElementNumCust:  Procedure(Var NumCust, TotalCust : Int32); StdCall;
        GetActiveElementNodeRef:  Procedure(Maxsize : Int32; NodeReferenceArray : pIntegerArray);  StdCall;// calling program must allocate
        GetActiveElementBusRef:   Function(Terminal : Int32) : Int32;  StdCall;
        GetActiveElementTerminalInfo: Procedure(Var NumTerminals, NumConds, NumPhases : Int32); StdCall;
        GetPtrToSystemVarray:     Procedure(var V : Pointer; var iNumNodes : Int32); StdCall; // Returns pointer to Solution.V and size
        GetActiveElementIndex:    Function() : Int32; StdCall;
        IsActiveElementEnabled:   Function() : Boolean; StdCall;
        IsBusCoordinateDefined:   Function(BusRef : Int32) : Boolean; StdCall;
        GetBusCoordinate:         Procedure(BusRef : Int32; Var X, Y : Double); StdCall;
        GetBuskVBase:             Function(BusRef : Int32) : Double; StdCall;
        GetBusDistFromMeter:      Function(BusRef : Int32) : Double; StdCall;

        GetDynamicsStruct:        Procedure(var pDynamicsStruct : Pointer); StdCall;  // Returns pointer to dynamics variables structure
        GetStepSize:              Function() : Double; StdCall;  // Return just 'h' from dynamics record
        GetTimeSec:               Function() : Double; StdCall; // returns t in sec from top of hour
        GetTimeHr:                Function() : Double; StdCall; // returns time as a double in hours

        GetPublicDataPtr:         Procedure(var pPublicData : Pointer; Var PublicDataBytes : Int32); StdCall;
        GetActiveElementName:     Function(FullName : pAnsiChar; MaxNameLen : UInt32) : Int32; StdCall;
        GetActiveElementPtr:      Function() : Pointer; StdCall;  // Returns pointer to active circuit element
        ControlQueuePush:         Function(Const Hour:Int32; Const Sec:Double; Const Code, ProxyHdl:Int32; Owner:Pointer):Int32; StdCall;
        GetResultStr:             Procedure(S : pAnsiChar; Maxlen : UInt32); StdCall;

   End;


