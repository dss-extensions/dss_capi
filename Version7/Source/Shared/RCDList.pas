unit RCDList;

interface

uses
    Classes;

type
    TRCDList = class(TList)

    PRIVATE
        PresentItem: Smallint;

    PUBLIC
        constructor Create;
        destructor Destroy; OVERRIDE;

        function FirstItem: Pointer;
        function NextItem(PrevItem: Pointer): Pointer;
        function ItemIndex: Integer;

    end;


implementation

constructor TRCDList.Create;

begin
    inherited Create;

    PresentItem := -1;

end;

destructor TRCDList.Destroy;

begin

    inherited Destroy;
end;

function TRCDList.FirstItem: Pointer;
begin
    if Count > 0 then
    begin
        Result := Items[0];
        PresentItem := 0;
    end
    else
    begin
        Result := NIL;
        PresentItem := -1;
    end;

end;

function TRCDList.NextItem(PrevItem: Pointer): Pointer;
var
    i: Integer;
begin
    if PrevItem <> Items[PresentItem] then
    begin
        {List has been used by someone after it was initiated.. Reset list to match PreviousItem}
        PresentItem := Count - 1;
        for i := 0 to Count - 1 do
            if Items[i] = PrevItem then
            begin
                PresentItem := i;
                Break;
            end;
        {If we can't make it match, PresentItem will point to last Item and
         the next operation (below) will return nil}
    end;

    Inc(PresentItem);
    if Count > PresentItem then
    begin
        Result := Items[PresentItem]
    end
    else
    begin
        Result := NIL;
        PresentItem := Count - 1;
    end;
end;

function TRCDList.ItemIndex: Integer;
begin
    Result := PresentItem;
end;

end.
