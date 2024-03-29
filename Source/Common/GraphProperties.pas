unit GraphProperties;


interface

uses
    Graphics,
    rchart;

type
//     GridStyleType = (gsNone, gsPoints, gsVertLines, gsHorizLines, gsLines, gsHorizDotLines, gsVertDotLines, gsDotLines);

    TDSSGraphProperties = packed record
        Xmin: Double;
        Xmax: Double;
        Ymin: Double;
        YMax: Double;
        ChartColor: TColor;
        WindColor: TColor;
        Isometric: Boolean;
        GridStyle: GridStyleType;
    end;

implementation

end.
