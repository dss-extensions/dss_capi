unit MyDSSClassDefs;

{
  ----------------------------------------------------------
  Copyright (c) 2008-2015, Electric Power Research Institute, Inc.
  All rights reserved.
  ----------------------------------------------------------
}
{
    Prototype unit for creating custom version of DSS

}

interface


const

    MYCLASS_ELEMENT_CONST = 99 * 8;  // make unique constants for your classes
                                          // SEE DSSClassDefs.pas
     {Assign (typically by adding) this constant to DSSClassType when objects of
      your custom class are instantiated. See Tline.Create in Line.Pas, for example}

procedure CreateMyDSSClasses;  // Called in DSSClassDefs

implementation

uses
    DSSClass
  {Add Special Uses clauses here:  }
  {,MyDSSClass};

procedure CreateMyDSSClasses;

begin
     {Put your custom class instantiations here}

     { Example:
         DSSClasses.New := TMyDSSClass.Create;

     }

end;

initialization

finalization

end.
