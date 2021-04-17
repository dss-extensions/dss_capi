TYPE

  {NOTE: Maxlen argument is to better accommodate Fortran strings.  VB also}
  {      Caller must allocate space for pchar values       }
   pDSSCallBacks = ^TDSSCallBacks;  {Pointer to callback structure}
   TDSSCallBacks = Packed Record

        MsgCallBack: Procedure (S : pUTF8Char; Maxlen:Cardinal);Stdcall; {Make use of DSS Message handling}

        {Routines for using DSS Parser.  This allows you to write models that accept
         syntax like other DSS scripts.}
        GetIntValue: Procedure(var i : Integer);Stdcall; {Get next param as an integer}
        GetDblValue: Procedure(var x : Double); Stdcall;  {Get next param as a double}
        GetStrValue: Procedure(s : pUTF8Char ; maxlen : Cardinal); Stdcall;
             {Get next param as a string <= maxlen characters  (Cardinal = 32-bit unsigned)}
             {caller must allocate space for s (Maxlen chars)}
        LoadParser:  Procedure(S : pUTF8Char; maxlen : Cardinal); Stdcall; // Copies a string into a special instance of the DSS parser
        NextParam:   Function (ParamName : pUTF8Char; Maxlen : Cardinal):Integer; Stdcall;
             {Advance to the next parameter and
              Get name of the param just retrieved, if one was given.
              Returns length of parameter found.  If 0, then end of string.
              This is to handle the syntax "paramname=paramvalue" commonly used in DSS scripts
              Copies the string to the location specified by s up to maxlen characters.
              Caller must allocate space (Maxlen chars)}

        DoDSSCommand:             Procedure(S : pUTF8Char; Maxlen : Cardinal); StdCall;
        GetActiveElementBusNames: Procedure(Name1 : pUTF8Char; Len1 : Cardinal; Name2 : pUTF8Char; Len2 : Cardinal); StdCall;
        GetActiveElementVoltages: Procedure(Var NumVoltages : Integer; V : pComplexArray); StdCall;
        GetActiveElementCurrents: Procedure(Var NumCurrents : Integer; Curr : pComplexArray; ActorID : Integer); StdCall;
        GetActiveElementLosses:   Procedure(Var TotalLosses, LoadLosses, NoLoadLosses : Complex; ActorID : Integer); StdCall;
        GetActiveElementPower:    Procedure(Terminal : Integer; Var TotalPower : Complex); StdCall;
        GetActiveElementNumCust:  Procedure(Var NumCust, TotalCust : Integer); StdCall;
        GetActiveElementNodeRef:  Procedure(Maxsize : Integer; NodeReferenceArray : pIntegerArray);  StdCall;// calling program must allocate
        GetActiveElementBusRef:   Function(Terminal : Integer) : Integer;  StdCall;
        GetActiveElementTerminalInfo: Procedure(Var NumTerminals, NumConds, NumPhases : Integer); StdCall;
        GetPtrToSystemVarray:     Procedure(var V : Pointer; var iNumNodes : Integer); StdCall; // Returns pointer to Solution.V and size
        GetActiveElementIndex:    Function() : Integer; StdCall;
        IsActiveElementEnabled:   Function() : Boolean; StdCall;
        IsBusCoordinateDefined:   Function(BusRef : Integer; ActorID : Integer) : Boolean; StdCall;
        GetBusCoordinate:         Procedure(BusRef : Integer; Var X, Y : Double; ActorID : Integer); StdCall;
        GetBuskVBase:             Function(BusRef : Integer; ActorID : Integer) : Double; StdCall;
        GetBusDistFromMeter:      Function(BusRef : Integer; ActorID : Integer) : Double; StdCall;

        GetDynamicsStruct:        Procedure(var pDynamicsStruct : Pointer; ActorID : Integer); StdCall;  // Returns pointer to dynamics variables structure
        GetStepSize:              Function(ActorID : Integer) : Double; StdCall;  // Return just 'h' from dynamics record
        GetTimeSec:               Function(ActorID : Integer) : Double; StdCall; // returns t in sec from top of hour
        GetTimeHr:                Function(ActorID: Integer) : Double; StdCall; // returns time as a double in hours

        GetPublicDataPtr:         Procedure(var pPublicData : Pointer; Var PublicDataBytes : Integer; ActorID : Integer); StdCall;
        GetActiveElementName:     Function(FullName : pUTF8Char; MaxNameLen : Cardinal; ActorID : Integer) : Integer; StdCall;
        GetActiveElementPtr:      Function(ActorID : Integer) : Pointer; StdCall;  // Returns pointer to active circuit element
        ControlQueuePush:         Function(Const Hour:Integer; Const Sec:Double; Const Code, ProxyHdl:Integer; Owner:Pointer; ActorID : Integer):Integer; StdCall;
        GetResultStr:             Procedure(S : pUTF8Char; Maxlen : Cardinal); StdCall;

   End;


