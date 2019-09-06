unit UnitFormData;

interface

uses
    server_data_types,
    Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants,
    System.Classes, Vcl.Graphics,
    Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.ExtCtrls, Vcl.StdCtrls, Vcl.Grids,
    Vcl.ComCtrls, UnitFormDataTable;

type

    TFormData = class(TForm)
        Panel1: TPanel;
        Panel3: TPanel;
        ComboBox1: TComboBox;
        Splitter1: TSplitter;
        StringGrid1: TStringGrid;
        Panel2: TPanel;
        procedure FormCreate(Sender: TObject);
        procedure ComboBox1Change(Sender: TObject);
        procedure StringGrid1SelectCell(Sender: TObject; ACol, ARow: integer;
          var CanSelect: Boolean);
        procedure StringGrid1DrawCell(Sender: TObject; ACol, ARow: integer;
          Rect: TRect; State: TGridDrawState);
        procedure Panel1Resize(Sender: TObject);
        procedure FormShow(Sender: TObject);
        procedure ListBox1Click(Sender: TObject);
    private
        { Private declarations }
        FYearMonth: TArray<TYearMonth>;
        FProducts: TArray<TProductInfo>;
        FFormDataTable1, FFormDataTable2: TFormDataTable;

        procedure FetchProductData(productID: int64);
        procedure SetupColumnsWidth2;

    public
        { Public declarations }
        procedure FetchYearsMonths;
    end;

var
    FormData: TFormData;

implementation

{$R *.dfm}

uses app, HttpClient, services, dateutils, stringgridutils, stringutils,
    UnitFormPopup;

function NewMeregedRow(ARow: integer; Atext: string): TMeregedRow;
begin
    Result.Text := Atext;
    Result.Row := ARow;
end;

procedure TFormData.FormCreate(Sender: TObject);
begin
    FFormDataTable1 := TFormDataTable.Create(self);
    FFormDataTable2 := TFormDataTable.Create(self);
end;

procedure TFormData.FormShow(Sender: TObject);
begin
    //

end;

procedure TFormData.ListBox1Click(Sender: TObject);
begin
    if StringGrid1.Row - 1 >= length(FProducts) then
        exit;
    FetchProductData(FProducts[StringGrid1.Row - 1].productID);
end;

procedure TFormData.Panel1Resize(Sender: TObject);
var ACol:integer;
begin

    with StringGrid1 do
    begin
//        Panel1.Constraints.MinWidth := ColWidths[0] + ColWidths[1] +
//          ColWidths[2] + 10;
        ColWidths[ColCount-1] := Panel1.Width - 10;
        for ACol := ColCount-2 downto 0 do
            ColWidths[ColCount-1] := ColWidths[ColCount-1] - ColWidths[ACol];
        Repaint;
    end;

end;

procedure TFormData.StringGrid1DrawCell(Sender: TObject; ACol, ARow: integer;
  Rect: TRect; State: TGridDrawState);
var
    grd: TStringGrid;
    cnv: TCanvas;
    ta: TAlignment;

begin
    grd := Sender as TStringGrid;
    cnv := grd.Canvas;
    cnv.Font.Assign(grd.Font);
    cnv.Brush.Color := clWhite;

    if gdSelected in State then
        cnv.Brush.Color := clGradientInactiveCaption
    else if gdFixed in State then
        cnv.Brush.Color := cl3DLight;

    ta := taLeftJustify;
    case ACol of
        0:
            begin
                ta := taCenter;
                cnv.Font.Color := clGreen;
            end;
        1:
            begin
                ta := taLeftJustify;
                cnv.Font.Color := clBlack;
            end;
    end;
    DrawCellText(grd, ACol, ARow, Rect, ta, grd.Cells[ACol, ARow]);
end;

procedure TFormData.StringGrid1SelectCell(Sender: TObject; ACol, ARow: integer;
  var CanSelect: Boolean);
var
    Products: TArray<TProduct>;
    tbs: TTabSheet;
    I: integer;
begin
    if ARow - 1 >= length(FProducts) then
        exit;
    FetchProductData(FProducts[ARow - 1].productID);
    Caption := 'ДАФ-М ' + IntToStr(FProducts[ARow - 1].productID);
end;

procedure TFormData.ComboBox1Change(Sender: TObject);
var
    I: integer;
    CanSelect: Boolean;
begin

    with StringGrid1 do
    begin
        ColCount := 5;

        OnSelectCell := nil;
        with FYearMonth[ComboBox1.ItemIndex] do
            FProducts := TProductsSvc.ProductsOfYearMonth(year, month);
        RowCount := length(FProducts) + 1;
        if RowCount = 1 then
            exit;

        FixedRows := 1;
        Cells[0, 0] := 'День';
        Cells[1, 0] := 'Вермя';
        Cells[2, 0] := '№ ДАФ';
        Cells[3, 0] := 'Сер.№';
        Cells[4, 0] := '№ партии';

        for I := 0 to length(FProducts) - 1 do
            with FProducts[I] do
            begin
                Cells[0, I + 1] := IntToStr2(day);
                Cells[1, I + 1] :=
                  Format('%s:%s', [IntToStr2(hour), IntToStr2(minute)]);
                Cells[2, I + 1] := IntToStr(productID);
                Cells[3, I + 1] := IntToStr(Serial);
                Cells[4, I + 1] := IntToStr(partyID);
            end;

        Row := RowCount - 1;
        OnSelectCell := StringGrid1SelectCell;

        CanSelect := true;
        StringGrid1SelectCell(StringGrid1, 1, Row, CanSelect);

    end;
    StringGrid_SetupColumnsWidth(StringGrid1);

end;

procedure TFormData.FetchYearsMonths;
var
    I: integer;
    ym: TYearMonth;
begin
    ComboBox1.Clear;
    FYearMonth := TProductsSvc.YearsMonths;
    if length(FYearMonth) = 0 then
        with ym do
        begin
            year := YearOf(now);
            month := MonthOf(now);
            FYearMonth := [ym];
        end;

    for I := 0 to length(FYearMonth) - 1 do
        with FYearMonth[I] do
            ComboBox1.Items.Add(Format('%d %s',
              [year, FormatDateTime('MMMM', IncMonth(0, month))]));

    ComboBox1.ItemIndex := 0;
    ComboBox1Change(nil);
end;

procedure TFormData.FetchProductData(productID: int64);
var pp :TProductPassport;
begin
    pp := TProductsSvc.IndividualPassport(productID);
    with FFormDataTable1 do
    begin
        Font.Assign(self.Font);
        Parent := Panel2;
        BorderStyle := bsNone;
        Align := alTop;
        SetTable(pp.T1);
        with StringGrid2 do
            FFormDataTable1.Height := DefaultRowHeight * RowCount + 20;
        Show;
    end;

    with FFormDataTable2 do
    begin
        Font.Assign(self.Font);
        Parent := Panel2;
        BorderStyle := bsNone;
        Align := alClient;
        SetTable(pp.T2);
        SetupColumnsWidth2;
        Visible := StringGrid2.RowCount > 1;
    end;

end;

procedure TFormData.SetupColumnsWidth2;
var
    ACol: Integer;
begin
    with FFormDataTable2.StringGrid2 do
    begin
        ColWidths[ColCount - 1] := Panel2.Width - 50;
        for ACol := 0 to ColCount - 2 do
        begin
            StringGrid_SetupColumnWidth(FFormDataTable2.StringGrid2, ACol);
            ColWidths[ColCount - 1] :=
              ColWidths[ColCount - 1] - ColWidths[ACol];
        end;
    end;
end;

end.
