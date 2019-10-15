unit UnitFormProductData;

interface

uses
    Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants,
    System.Classes, Vcl.Graphics,
    Vcl.Controls, Vcl.Forms, Vcl.Dialogs, UnitFormDataTable, server_data_types,
    services, stringgridutils, Vcl.ExtCtrls, Vcl.Menus;

type
    TFormProductData = class(TForm)
        Panel1: TPanel;
        Panel2: TPanel;
        PopupMenu1: TPopupMenu;
        N1: TMenuItem;
        N2: TMenuItem;
        procedure FormCreate(Sender: TObject);
        procedure N1Click(Sender: TObject);
        procedure PopupMenu1Popup(Sender: TObject);
        procedure N2Click(Sender: TObject);
    private
        { Private declarations }
        FFormDataTable1, FFormDataTable2: TFormDataTable;
        FProductID: int64;
        procedure SetupColumnsWidth2;
        procedure SetupProduct;
    public
        { Public declarations }
        procedure FetchProductData(productID: int64);
        procedure OnProductDataChanged(x: int64);

    end;

var
    FormProductData: TFormProductData;

implementation

uses dateutils;

{$R *.dfm}

procedure TFormProductData.FormCreate(Sender: TObject);
begin
    FFormDataTable1 := TFormDataTable.Create(self);
    FFormDataTable2 := TFormDataTable.Create(self);

    FFormDataTable2.StringGrid2.PopupMenu := PopupMenu1;

end;

procedure TFormProductData.N1Click(Sender: TObject);
var
    ro: integer;
    entryID: int64;
    Entries: TArray<int64>;
begin
    with FFormDataTable2.StringGrid2 do
    begin
        for ro := Selection.Top to Selection.Bottom do
        begin
            SetLength(Entries, Length(Entries) + 1);
            Entries[Length(Entries) - 1] := StrToInt64(Cells[0, ro]);
        end;
    end;
    TProductsSvc.DeleteEntries(Entries);
    SetupProduct;

end;

procedure TFormProductData.N2Click(Sender: TObject);
begin
    with FFormDataTable2.StringGrid2 do
        TPartySvc.DeleteTestEntries(Cells[2, Row]);
    SetupProduct;

end;

procedure TFormProductData.FetchProductData(productID: int64);
begin
    FProductID := productID;
    SetupProduct;
end;

procedure TFormProductData.SetupProduct;
var
    pp: TProductPassport;
begin
    pp := TProductsSvc.ProductPassport(FProductID);
    Panel1.Caption := Format('ДАФ-М %d №%d партия %d %s',
      [FProductID, pp.Serial, pp.PartyID, DateTimeToStr(pp.CreatedAt)]);
    Panel1.Top := 0;
    with FFormDataTable1 do
    begin
        Font.Assign(self.Font);
        Parent := Panel2;
        BorderStyle := bsNone;
        Align := alTop;
        Top := 100500;
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

procedure TFormProductData.SetupColumnsWidth2;
var
    ACol: integer;
begin
    with FFormDataTable2.StringGrid2 do
    begin
        ColWidths[ColCount - 1] := self.Width - 50;
        for ACol := 0 to ColCount - 2 do
        begin
            StringGrid_SetupColumnWidth(FFormDataTable2.StringGrid2, ACol);
            ColWidths[ColCount - 1] := ColWidths[ColCount - 1] -
              ColWidths[ACol];
        end;
    end;
end;

procedure TFormProductData.OnProductDataChanged(x: int64);
begin
    if FProductID = x then
        SetupProduct;
end;

procedure TFormProductData.PopupMenu1Popup(Sender: TObject);
begin
    with FFormDataTable2.StringGrid2 do
    begin
        if Row > 0 then
        begin
            N2.Caption := 'Удалить записи "' + Cells[2, Row] +
              '" для всей партии';
            N2.Visible := true;
        end
        else
        begin
            N2.Visible := false;
        end;
    end;
end;

end.
