unit UnitFormDataTable;

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

    TFormDataTable = class(TForm)
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

        FCells: TDictionary<TCoord, TCellDetail>;

    public
        { Public declarations }
        procedure SetTable(ATable: TArray<TArray<string>>);
    end;

//var
//    FormDataTable: TFormDataTable;

implementation

{$R *.dfm}

uses types, StrUtils, stringgridutils, UnitFormPopup, app, services,
  stringutils;

function coord(x, y: integer): TCoord;
begin
    result.x := x;
    result.y := y;
end;

procedure TFormDataTable.FormCreate(Sender: TObject);
begin
    FCells := TDictionary<TCoord, TCellDetail>.create;
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

procedure TFormDataTable.FormShow(Sender: TObject);
begin
    //
end;

procedure TFormDataTable.StringGrid2DblClick(Sender: TObject);
var
    r: TRect;
    pt: TPoint;
    c: TCellDetail;
begin

    with StringGrid2 do
    begin
        if not FCells.ContainsKey(coord(Row, Col)) then
            exit;
        c := FCells[coord(Row, Col)];

        if length(c.Detail) = 0 then
            exit;

        FormPopup.RichEdit1.Text := c.Detail;
        FormPopup.RichEdit1.Font.Color := c.Color;
        r := CellRect(Col, Row);
        pt := StringGrid2.ClientToScreen(r.TopLeft);
        FormPopup.Left := pt.x + ColWidths[Col] + 3;
        FormPopup.Top := pt.y + RowHeights[Row] + 3;
        FormPopup.Show;
    end;
end;

procedure TFormDataTable.StringGrid2DrawCell(Sender: TObject;
  ACol, ARow: integer; Rect: TRect; State: TGridDrawState);
var
    grd: TStringGrid;
    cnv: TCanvas;
    ta: TAlignment;
    AMergeRect: TMeregedRow;
    not_used:double;
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
        if try_str_to_float(grd.Cells[ACol, ARow], not_used) then
            ta := taRightJustify
        else
            ta := taLeftJustify;
        if FCells.ContainsKey(coord(ACol, ARow)) then
            cnv.Font.Color := FCells[coord(ACol, ARow)].Color;
    end;

    DrawCellText(StringGrid2, ACol, ARow, Rect, ta,
      StringGrid2.Cells[ACol, ARow]);

    StringGrid_DrawCellBounds(cnv, ACol, ARow, Rect);

end;

procedure TFormDataTable.StringGrid2TopLeftChanged(Sender: TObject);
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

procedure TFormDataTable.SetTable(ATable: TArray<TArray<string>>);
var
    _row: TArray<string>;
    ACol, ARow, I: integer;
    s:string;
    strs:TStringDynArray;
    cc : TCellDetail;
begin
    FCells.Clear;
    FMeregedRows := [];
    StringGrid_Clear(StringGrid2);

    with StringGrid2 do
    begin

        ColCount := length(ATable[0]);
        RowCount := length(ATable);
        if rowcount > 1 then
            FixedRows := 1;

        for I := 0 to length(ATable[0]) - 1 do
            Cells[I, 0] := ATable[0][I];

        ARow := 0;

        for _row in ATable do
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
                begin
                    s := _row[ACol];
                    strs := SplitString(S, ';');

                    if Length(strs) > 0 then
                        Cells[ACol, ARow] := strs[0];

                    if Length(strs) > 2 then
                        cc.Detail := strs[2];

                    if Length(strs) > 1 then
                    begin
                        cc.Color := StringToColor(strs[1]);
                        FCells.Add(Coord(ACol,ARow), cc);
                    end;

                end;

            end;
            ARow := ARow + 1;
        end;

    end;
    StringGrid_SetupColumnsWidth(StringGrid2);

end;

end.
