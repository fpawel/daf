unit UnitFormPartyData;

interface

uses
    Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants,
    System.Classes, Vcl.Graphics,
    Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.Grids, Vcl.StdCtrls,
    server_data_types, System.Generics.Collections;

type
    TMeregedRow = record
        Text: string;
        Row: integer;
    end;

    TCellDetail = record
        Color: TColor;
        Detail: string;
    end;

    TFormPartyData = class(TForm)
        StringGrid2: TStringGrid;
        procedure StringGrid2DrawCell(Sender: TObject; ACol, ARow: integer;
          Rect: TRect; State: TGridDrawState);
        procedure StringGrid2DblClick(Sender: TObject);
        procedure StringGrid2TopLeftChanged(Sender: TObject);
        procedure FormCreate(Sender: TObject);
        procedure FormShow(Sender: TObject);
    private
        { Private declarations }
        FMeregedRows: TArray<TMeregedRow>;
        FRows: TArray<TArray<string>>;

        FCells : TDictionary<TCoord,TCellDetail>;

    public
        { Public declarations }
        procedure FetchParty(partyID: int64);
    end;

var
    FormPartyData: TFormPartyData;

implementation

{$R *.dfm}

uses stringgridutils, UnitFormPopup, app, services;

function coord(x,y:integer):TCoord;
begin
    result.X := x;
    Result.Y := y;
end;

procedure TFormPartyData.FormCreate(Sender: TObject);
begin
    FCells := TDictionary<TCoord,TCellDetail>.create;
    with StringGrid2 do
    begin
        ColCount := 10;
        RowCount := 2;
        FixedCols := 1;
        FixedRows := 1;

        Cells[0, 0] := 'a';
        Cells[1, 1] := 'b';
        Cells[2, 2] := 'c';
    end;
end;

procedure TFormPartyData.FormShow(Sender: TObject);
begin
    FetchParty(0);
end;

procedure TFormPartyData.StringGrid2DblClick(Sender: TObject);
var
    r: TRect;
    pt: TPoint;
    c: TCellDetail;
begin

    with StringGrid2 do
    begin
        if not FCells.ContainsKey(coord(Row,Col)) then
            exit;
        c := FCells[coord(Row,Col)];

        if length(c.Detail) = 0 then
            exit;

        FormPopup.RichEdit1.Text := c.Detail;
        FormPopup.RichEdit1.Font.Color := c.Color;
        r := CellRect(Col, Row);
        pt := StringGrid2.ClientToScreen(r.TopLeft);
        FormPopup.Left := pt.X + ColWidths[Col] + 3;
        FormPopup.Top := pt.Y + RowHeights[Row] + 3;
        FormPopup.Show;
    end;
end;

procedure TFormPartyData.StringGrid2DrawCell(Sender: TObject;
  ACol, ARow: integer; Rect: TRect; State: TGridDrawState);
var
    grd: TStringGrid;
    cnv: TCanvas;
    ta: TAlignment;
    AMergeRect: TMeregedRow;
begin
    grd := StringGrid2;
    cnv := grd.Canvas;
    cnv.Font.Assign(grd.Font);

    cnv.Brush.Color := clWhite;
    if gdSelected in State then
        cnv.Brush.Color := clGradientInactiveCaption
    else if gdFixed in State then
        cnv.Brush.Color := cl3DLight;
    cnv.Font.Color := clBlack;
    cnv.Pen.Color := $00BCBCBC;
    cnv.Pen.Width := -1;

    if ACol > 0 then
        for AMergeRect in FMeregedRows do
            if ARow = AMergeRect.Row then
            begin
                cnv.Brush.Color := clInfoBk;
                cnv.Font.Color := clNavy;
                cnv.Font.Style := [fsBold];

                StringGrid_DrawMeregedCell(grd, AMergeRect.Text,
                  AMergeRect.Row, Rect);
                exit;
            end;

    ta := taCenter;
    if (ACol > 0) ANd (ARow > 0) then
    begin
        ta := taRightJustify;
        if FCells.ContainsKey(coord(ACol, ARow)) then
            cnv.Font.Color := FCells[coord(ACol, ARow)].Color;
    end;

    DrawCellText(StringGrid2, ACol, ARow, Rect, ta,
      StringGrid2.Cells[ACol, ARow]);

    StringGrid_DrawCellBounds(cnv, ACol, ARow, Rect);

end;

procedure TFormPartyData.StringGrid2TopLeftChanged(Sender: TObject);
var
    ACol, ARow: integer;
begin
    with StringGrid2 do
    begin
        for ACol := LeftCol to LeftCol + VisibleColCount - 1 do
            for ARow := TopRow to TopRow + VisibleRowCount - 1 do
                Cells[ACol, ARow] := Cells[ACol, ARow];

        for ACol := 0 to LeftCol + VisibleColCount - 1 do
            Cells[ACol, 0] := Cells[ACol, 0];

        for ARow := TopRow to TopRow + VisibleRowCount - 1 do
            Cells[0, ARow] := Cells[0, ARow];
    end;
end;

procedure TFormPartyData.FetchParty(partyID: int64);
var
    xs: TArray<TArray<string>>;
    _row: TArray<string>;
    ACol, ARow, I: integer;
begin

    FMeregedRows := [];
    if partyID = 0 then
        partyID := TLastPartySvc.Party.partyID;
    xs := TPartiesSvc.ReportParty(partyID);

    with StringGrid2 do
    begin

        ColCount := length(xs[0]);
        RowCount := length(xs);

        for I := 0 to length(xs[0]) - 1 do
            Cells[I, 0] := xs[0][I];

        ARow := 0;

        for _row in xs do
        begin

            if length(_row) = 1 then
            begin
                SetLength(FMeregedRows, length(FMeregedRows) + 1);
                FMeregedRows[length(FMeregedRows) - 1].Row := ARow;
                FMeregedRows[length(FMeregedRows) - 1].Text := _row[0];

            end
            else
            begin

                for ACol := 0 to ColCount - 1 do
                    Cells[ACol, ARow] := _row[ACol];

            end;
            ARow := ARow + 1;
        end;

    end;
    StringGrid_SetupColumnsWidth(StringGrid2);

end;

end.
