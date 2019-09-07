unit UnitFormProductData;

interface

uses
    Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants,
    System.Classes, Vcl.Graphics,
    Vcl.Controls, Vcl.Forms, Vcl.Dialogs, UnitFormDataTable, server_data_types,
    services, stringgridutils, Vcl.ExtCtrls;

type
    TFormProductData = class(TForm)
        Panel1: TPanel;
    Panel2: TPanel;
        procedure FormCreate(Sender: TObject);
    private
        { Private declarations }
        FFormDataTable1, FFormDataTable2: TFormDataTable;
        FProductID: int64;
        procedure SetupColumnsWidth2;
        procedure SetupProduct;
    public
        { Public declarations }
        procedure FetchProductData(productID: int64);
        procedure OnProductDataChanged(x:Int64);

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
    Panel1.top := 0;
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
    ACol: Integer;
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

procedure TFormProductData.OnProductDataChanged(x:Int64);
begin
    if FProductID = x then
        SetupProduct;
end;

end.
