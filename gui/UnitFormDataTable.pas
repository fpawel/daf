unit UnitFormDataTable;

interface

uses
    Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants,
    System.Classes, Vcl.Graphics,  Vcl.grids,
    Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls,
    server_data_types, System.Generics.Collections;

type
    TMeregedRow = record
        Text: string;
        Row: integer;
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
        FCells: TArray<TArray<TCell>>;

    public
        { Public declarations }
        procedure SetTable(ATable: TArray < TArray < TCell >> );
    end;

    // var
    // FormDataTable: TFormDataTable;

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
begin
    FormPopup.ShowStringGridCellText(StringGrid2);
end;

procedure TFormDataTable.StringGrid2DrawCell(Sender: TObject;
  ACol, ARow: integer; Rect: TRect; State: TGridDrawState);
var
    grd: TStringGrid;
    cnv: TCanvas;
    AMergeRect: TMeregedRow;
    not_used: double;
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

    if (ACol > 0) ANd (ARow > 0) then
    begin
        if length(FCells[ARow][ACol].Color) > 0 then
            cnv.Font.Color := StringToColor(FCells[ARow][ACol].Color);
    end;

    DrawCellText(StringGrid2, ACol, ARow, Rect,
      TAlignment(FCells[ARow][ACol].Alignment), StringGrid2.Cells[ACol, ARow]);

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

procedure TFormDataTable.SetTable(ATable: TArray < TArray < TCell >> );
var
    _row: TArray<TCell>;
    ACol, ARow, I: integer;
    s: string;
begin
    FCells := ATable;
    FMeregedRows := [];
    StringGrid_Clear(StringGrid2);

    with StringGrid2 do
    begin

        ColCount := length(ATable[0]);
        RowCount := length(ATable);
        if RowCount > 1 then
            FixedRows := 1;

        for I := 0 to length(ATable[0]) - 1 do
            Cells[I, 0] := ATable[0][I].Text;

        ARow := 0;

        for _row in ATable do
        begin

            if length(_row) = 1 then
            begin
                SetLength(FMeregedRows, length(FMeregedRows) + 1);
                FMeregedRows[length(FMeregedRows) - 1].Row := ARow;
                FMeregedRows[length(FMeregedRows) - 1].Text := _row[0].Text;

            end
            else

                for ACol := 0 to ColCount - 1 do
                    Cells[ACol, ARow] := _row[ACol].Text;

            ARow := ARow + 1;
        end;

    end;
    StringGrid_SetupColumnsWidth(StringGrid2);

end;

end.
