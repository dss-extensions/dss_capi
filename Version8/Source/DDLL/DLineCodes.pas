unit DLineCodes;

interface

uses
  ComObj, ActiveX, {$IFNDEF FPC}OpenDSSengine_TLB, StdVcl,{$ENDIF} LineCode;

function LineCodesI(mode: longint; arg: longint): longint; cdecl;
function LineCodesF(mode: longint; arg: double): double; cdecl;
function LineCodesS(mode: longint; arg: pAnsiChar): pAnsiChar; cdecl;
procedure LineCodesV(mode:longint; var myPointer: Pointer; var myType, mySize: longint);cdecl;

implementation

uses ComServ, sysutils, DSSGlobals, LineUnits, ParserDel, Variants, Ucomplex;

{$IFDEF FPC}
const
  dssLineUnitsMaxnum = $00000009;  // from OpenDSSEngine_TLB.pas
{$ENDIF}

//*****************************Integer interface***************************************

function LineCodesI(mode: longint; arg: longint): longint; cdecl;

Var
   pLineCode:TLineCodeObj;

begin
  Result:=0;
  case mode of
  0:  begin  // LineCodes.Count
        If ActiveCircuit[ActiveActor] <> Nil Then
          Result := LineCodeClass.ElementCount;
      end;
  1:  begin  // LineCodes.First
        If ActiveCircuit[ActiveActor] <> Nil Then
          Result := LineCodeClass.First;
      end;
  2:  begin  // LineCodes.Next
        If ActiveCircuit[ActiveActor] <> Nil Then
          Result := LineCodeClass.Next;
      end;
  3:  begin  // LineCodes.Units Read
        IF ActiveCircuit[ActiveActor] <> NIL
        THEN Begin
             pLineCode := LineCodeClass.GetActiveObj ;
             Result := pLineCode.Units;
        End
      end;
  4:  begin  // LineCodes.Units Write
        IF ActiveCircuit[ActiveActor] <> NIL
        THEN Begin
            pLineCode := LineCodeClass.GetActiveObj ;
             WITH pLineCode Do Begin
                if arg < dssLineUnitsMaxnum  then
                  begin
                     Parser[ActiveActor].CmdString := Format('units=%s', [LineUnitsStr(arg)]);
                     Edit(ActiveActor);
                  end
                else
                  DoSimpleMsg('Invalid line units integer sent via COM interface.  Please enter a value within range.',183);

             END;
        End;
      end;
  5:  begin  // LineCodes.Phases Read
        IF ActiveCircuit[ActiveActor] <> NIL
        THEN Begin
             pLineCode := LineCodeClass.GetActiveObj ;
             Result := pLineCode.FNPhases;
        End
      end;
  6:  begin  // LineCodes.Phases Write
        IF ActiveCircuit[ActiveActor] <> NIL
        THEN Begin
             pLineCode := LineCodeClass.GetActiveObj ;
             pLineCode.NumPhases := arg;   // use property value to force reallocations
        End
      end;
  7:  begin  // LineCodes.IsZ1Z0
        Result  :=  1;
        If ActiveCircuit[ActiveActor] <> Nil Then
        Begin
             pLineCode := LineCodeClass.GetActiveObj ;
             If pLineCode <> Nil Then
             Begin
                   if pLineCode.SymComponentsModel then Result:= 1
                   else Result  :=  0;
             End;
        End;
      end
  else
      begin
        Result  :=  -1;
      end;
  end;
end;

//*****************************Floating point interface***************************************

function LineCodesF(mode: longint; arg: double): double; cdecl;
Var
   pLineCode:TLineCodeObj;

begin
  Result:=0.0;
  case mode of
  0:  begin  // LineCodes.R1 Read
        IF ActiveCircuit[ActiveActor] <> NIL
        THEN Begin
             pLineCode := LineCodeClass.GetActiveObj ;
             Result := pLineCode.R1 ;
        End
      end;
  1:  begin  // LineCodes.R1 Write
        IF ActiveCircuit[ActiveActor] <> NIL
        THEN Begin
            pLineCode := LineCodeClass.GetActiveObj ;
             WITH pLineCode Do Begin
                     Parser[ActiveActor].CmdString := Format('R1=%g', [arg]);
                     Edit(ActiveActor);
             END;
        End;
      end;
  2:  begin  // LineCodes.X1 Read
        IF ActiveCircuit[ActiveActor] <> NIL
        THEN Begin
             pLineCode := LineCodeClass.GetActiveObj ;
             Result := pLineCode.X1 ;
        End
      end;
  3:  begin  // LineCodes.X1 Write
        IF ActiveCircuit[ActiveActor] <> NIL
        THEN Begin
            pLineCode := LineCodeClass.GetActiveObj ;
             WITH pLineCode Do Begin
                     Parser[ActiveActor].CmdString := Format('X1=%g', [arg]);
                     Edit(ActiveActor);
             END;
        End;
      end;
  4:  begin  // LineCodes.R0 Read
        IF ActiveCircuit[ActiveActor] <> NIL
        THEN Begin
             pLineCode := LineCodeClass.GetActiveObj ;
             Result := pLineCode.R0 ;
        End
      end;
  5:  begin  // LineCodes.R0 Write
        IF ActiveCircuit[ActiveActor] <> NIL
        THEN Begin
            pLineCode := LineCodeClass.GetActiveObj ;
             WITH pLineCode Do Begin
                     Parser[ActiveActor].CmdString := Format('R0=%g', [arg]);
                     Edit(ActiveActor);
             END;
        End;
      end;
  6:  begin  // LineCodes.X0 Read
        IF ActiveCircuit[ActiveActor] <> NIL
        THEN Begin
             pLineCode := LineCodeClass.GetActiveObj ;
             Result := pLineCode.X0 ;
        End
      end;
  7:  begin  // LineCodes.X0 Write
        IF ActiveCircuit[ActiveActor] <> NIL
        THEN Begin
            pLineCode := LineCodeClass.GetActiveObj ;
             WITH pLineCode Do Begin
                     Parser[ActiveActor].CmdString := Format('X0=%g', [arg]);
                     Edit(ActiveActor);
             END;
        End;
      end;
  8:  begin  // LineCodes.C1 Read
        IF ActiveCircuit[ActiveActor] <> NIL
        THEN Begin
             pLineCode := LineCodeClass.GetActiveObj ;
             Result := pLineCode.C1 * 1.0e9;
        End
      end;
  9:  begin  // LineCodes.C1 Write
        IF ActiveCircuit[ActiveActor] <> NIL
        THEN Begin
            pLineCode := LineCodeClass.GetActiveObj ;
             WITH pLineCode Do Begin
                     Parser[ActiveActor].CmdString := Format('C1=%g', [arg]);
                     Edit(ActiveActor);
             END;
        End;
      end;
  10: begin  // LineCodes.C0 Read
        IF ActiveCircuit[ActiveActor] <> NIL
        THEN Begin
             pLineCode := LineCodeClass.GetActiveObj ;
             Result := pLineCode.C0 * 1.0e9;
        End
      end;
  11: begin  // LineCodes.C0 Write
        IF ActiveCircuit[ActiveActor] <> NIL
        THEN Begin
            pLineCode := LineCodeClass.GetActiveObj ;
             WITH pLineCode Do Begin
                     Parser[ActiveActor].CmdString := Format('C0=%g', [arg]);
                     Edit(ActiveActor);
             END;
        End;
      end;
  12: begin  // LineCodes.NormAmps Read
        IF ActiveCircuit[ActiveActor] <> NIL
        THEN Begin
          pLineCode := LineCodeClass.GetActiveObj ;
          Result := pLineCode.NormAmps  ;
        End
      end;
  13: begin  // LineCodes.NormAmps Write
        IF ActiveCircuit[ActiveActor] <> NIL
        THEN Begin
             pLineCode := LineCodeClass.GetActiveObj ;
             pLineCode.NormAmps  := arg;
        End
      end;
  14: begin  // LineCodes.EmergAmps Read
        IF ActiveCircuit[ActiveActor] <> NIL
        THEN Begin
             pLineCode := LineCodeClass.GetActiveObj ;
             Result := pLineCode.EmergAmps   ;
        End
      end;
  15: begin  // LineCodes.NormAmps Write
        IF ActiveCircuit[ActiveActor] <> NIL
        THEN Begin
             pLineCode := LineCodeClass.GetActiveObj ;
             pLineCode.EmergAmps := arg   ;
        End
      end
  else
      begin
      Result  :=  -1.0;
      end;
  end;
end;

//*****************************String interface***************************************

function LineCodesS(mode: longint; arg: pAnsiChar): pAnsiChar; cdecl;
Var
   pLineCode:TLineCodeObj;

begin
  Result:='';
  case mode of
  0:  begin  // LineCodes.Name Read
         If ActiveCircuit[ActiveActor] <> Nil Then
         Begin
              pLineCode := LineCodeClass.GetActiveObj ;
              If pLineCode <> Nil Then
              Begin
                    Result := pAnsiChar(AnsiString(pLineCode.Name));
              End;
         End;
      end;
  1:  begin  // LineCodes.Name Write
         If ActiveCircuit[ActiveActor] <> Nil Then
         Begin
              If Not LineCodeClass.SetActive (arg) Then
               DoSimpleMsg('LineCode "'+ arg +'" Not Found in Active Circuit.', 51008);

               // Still same active object if not found
         End;
      end
  else
      begin
        Result:=pAnsiChar(AnsiString('Parameter not identified'));
      end;
  end;
end;

//*****************************Variants interface***************************************

procedure LineCodesV(mode:longint; var myPointer: Pointer; var myType, mySize: longint);cdecl;
Var
   pLineCode: TLineCodeObj;
   i,
   j,
   k        : Integer;
   Ztemp    : Complex;
   Factor   : Double;
   Pint     : ^Integer;
   PDouble  : ^Double;

begin
  case mode of
    0:  begin  // LineCodes.Rmatrix Read
          myType  :=  2;        // Double
          setlength(myDBLArray, 1);
          IF ActiveCircuit[ActiveActor] <> NIL THEN
          Begin
            pLineCode := LineCodeClass.GetActiveObj ;
            WITH pLineCode DO
            Begin
              setlength(myDBLArray, Sqr(FNphases));
              k := 0;
              FOR i := 1 to FNPhases DO
              Begin
                FOR j := 1 to FNphases DO
                Begin
                   myDBLArray[k] :=  Z.GetElement(i,j).re;
                   Inc(k);
                End;
              End;
            End;
          End
          else myDBLArray[0] := 0;
          myPointer :=  @(myDBLArray[0]);
          mySize    :=  SizeOf(myDBLArray[0]) * Length(myDBLArray);
        end;
    1:  begin  // LineCodes.Rmatrix Write
          myType  :=  2;        // Double
          k :=  0;
          IF ActiveCircuit[ActiveActor] <> NIL
          THEN
          Begin
            pLineCode := LineCodeClass.GetActiveObj ;
            WITH pLineCode DO
            Begin
              FOR i := 1 to FNPhases DO
              Begin
                FOR j := 1 to FNphases DO
                Begin
                 PDouble  :=  myPointer;
                 ZTemp := Z.GetElement(i,j);
                 Z.SetElement(i,j, Cmplx( PDouble^, ZTemp.im));
                 Inc(k);
                 inc(PByte(myPointer),8);
                End;
              End;
            End;
          End;
          mySize  :=  k;
        end;
    2:  begin  // LineCodes.Xmatrix Read
          myType  :=  2;        // Double
          setlength(myDBLArray, 1);
          IF ActiveCircuit[ActiveActor] <> NIL THEN
          Begin
            pLineCode := LineCodeClass.GetActiveObj ;
            WITH pLineCode DO
            Begin
              setlength(myDBLArray, Sqr(FNphases));
              k := 0;
              FOR i := 1 to FNPhases DO
              Begin
                FOR j := 1 to FNphases DO
                Begin
                  myDBLArray[k] :=  Z.GetElement(i,j).im;
                  Inc(k);
                End;
              End;
            End;
          End
          else myDBLArray[0] := 0;
          myPointer :=  @(myDBLArray[0]);
          mySize    :=  SizeOf(myDBLArray[0]) * Length(myDBLArray);
        end;
    3:  begin  // LineCodes.Xmatrix Write
          myType  :=  2;        // Double
          k :=  0;
          IF ActiveCircuit[ActiveActor] <> NIL THEN
          Begin
            pLineCode := LineCodeClass.GetActiveObj ;
            WITH pLineCode DO
            Begin
              FOR i := 1 to FNPhases DO
              Begin
                FOR j := 1 to FNphases DO
                Begin
                  PDouble  :=  myPointer;
                  ZTemp := Z.GetElement(i,j);
                  Z.SetElement(i,j, Cmplx( ZTemp.re, PDouble^ ));
                  Inc(k);
                  inc(PByte(myPointer),8);
                End;
              End;
            End;
          End;
          mySize  :=  k;
        end;
    4:  begin  // LineCodes.Cmatrix Read
          myType  :=  2;        // Double
          setlength(myDBLArray, 1);
          IF ActiveCircuit[ActiveActor] <> NIL
          THEN
          Begin
            pLineCode := LineCodeClass.GetActiveObj ;
            WITH pLineCode DO
            Begin
              Factor := (TwoPi * BaseFrequency  * 1.0e-9);
              setlength(myDBLArray, Sqr(FNphases));
              k := 0;
              FOR i := 1 to FNPhases DO
              Begin
                FOR j := 1 to FNphases DO
                Begin
                 myDBLArray[k] :=  YC.GetElement(i,j).im/Factor;
                 Inc(k);
                End;
              End;
            End;
          End
          else myDBLArray[0] := 0;
          myPointer :=  @(myDBLArray[0]);
          mySize    :=  SizeOf(myDBLArray[0]) * Length(myDBLArray);
        end;
    5:  begin  // LineCodes.Cmatrix Write
          myType  :=  2;        // Double
          k :=  0;
          IF ActiveCircuit[ActiveActor] <> NIL THEN
          Begin
            pLineCode := LineCodeClass.GetActiveObj ;
            WITH pLineCode DO
            Begin
              Factor  := TwoPi * BaseFrequency  * 1.0e-9;
              FOR i := 1 to FNPhases DO
              Begin
                FOR j := 1 to FNphases DO
                Begin
                  PDouble :=  myPointer;
                  Yc.SetElement(i,j, Cmplx(0.0, (PDouble^)*Factor));
                  Inc(k);
                  inc(PByte(myPointer),8);
                End;
              End;
            End;
          End;
          mySize  :=  k;
        end;
    6:  begin  // LineCodes.AllNames
          myType  :=  4;        // String
          setlength(myStrArray,0);
          mySize        :=  0;
          IF ActiveCircuit[ActiveActor] <> Nil THEN
          Begin
            WITH ActiveCircuit[ActiveActor] DO
            Begin
              If LineCodeClass.ElementList.ListSize  >0 Then
              Begin
                k:=0;
                pLineCode := LineCodeClass.ElementList.First;
                WHILE pLineCode<>Nil DO
                Begin
                  WriteStr2Array(pLineCode.Name);
                  WriteStr2Array(Char(0)); ;
                  pLineCode := LineCodeClass.ElementList.Next;
                End;
              End;
            End;
          end
          Else  WriteStr2Array('');
          myPointer :=  @(myStrArray[0]);
          mySize    :=  Length(myStrArray);
        end
    else
    begin
      myType  :=  4;        // String
      setlength(myStrArray, 0);
      WriteStr2Array('Error, parameter not recognized');
      myPointer :=  @(myStrArray[0]);
      mySize    :=  Length(myStrArray);
    end;

  end;
end;

end.
