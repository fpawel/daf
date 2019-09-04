unit UnitFormData;

interface

uses
    server_data_types,
    Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants,
    System.Classes, Vcl.Graphics,
    Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.ExtCtrls, Vcl.StdCtrls, Vcl.Grids,
    Vcl.ComCtrls;

type

    TFormData = class(TForm)
        Panel1: TPanel;
        Panel3: TPanel;
        ComboBox1: TComboBox;
        Splitter1: TSplitter;
        StringGrid1: TStringGrid;
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
        FParties: TArray<TPartyCatalogue>;

    public
        { Public declarations }
        procedure FetchYearsMonths;
    end;

var
    FormData: TFormData;

implementation

{$R *.dfm}

uses app, HttpClient, services, dateutils, stringgridutils, stringutils,
    UnitFormPopup, UnitFormPartyData;

function NewMeregedRow(ARow: integer; Atext: string): TMeregedRow;
begin
    Result.Text := Atext;
    Result.Row := ARow;
end;

procedure TFormData.FormCreate(Sender: TObject);
begin
    //
end;

procedure TFormData.FormShow(Sender: TObject);
begin
    //

end;

procedure TFormData.ListBox1Click(Sender: TObject);
begin
    if StringGrid1.Row - 1 >= length(FParties) then
        exit;
    FormPartyData.Parent := self;
    FormPartyData.Align := alClient;
    FormPartyData.FetchParty(FParties[StringGrid1.Row - 1].partyID);
end;

procedure TFormData.Panel1Resize(Sender: TObject);
begin
    with StringGrid1 do
    begin
        ColWidths[0] := 70;
        ColWidths[1] := 70;
        ColWidths[2] := 70;
        ColWidths[3] := Panel1.Width - ColWidths[0] - ColWidths[1] -
          ColWidths[2] - 10;
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
        cnv.Brush.Color := clGradientInactiveCaption;

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
    if ARow - 1 >= length(FParties) then
        exit;

    FormPartyData.FetchParty(FParties[ARow - 1].partyID);
    Caption := 'Загрузка ' + IntToStr(FParties[ARow - 1].partyID);
end;

procedure TFormData.ComboBox1Change(Sender: TObject);
var
    I: integer;
    CanSelect: Boolean;
begin

    with StringGrid1 do
    begin
        ColCount := 4;

        OnSelectCell := nil;
        with FYearMonth[ComboBox1.ItemIndex] do
            FParties := TPartiesSvc.PartiesOfYearMonth(year, month);
        RowCount := length(FParties) + 1;
        if RowCount = 1 then
            exit;

        FixedRows := 1;
        Cells[0, 0] := 'День';
        Cells[1, 0] := 'Вермя';
        Cells[2, 0] := '№';
        Cells[3, 0] := 'Исполнение';

        for I := 0 to length(FParties) - 1 do
            with FParties[I] do
            begin
                Cells[0, I + 1] := IntToStr2(day);
                Cells[1, I + 1] :=
                  Format('%s:%s', [IntToStr2(hour), IntToStr2(minute)]);
                Cells[2, I + 1] := IntToStr(partyID);
                Cells[3, I + 1] := FParties[I].ProductType;
            end;

        Row := RowCount - 1;
        OnSelectCell := StringGrid1SelectCell;

        CanSelect := true;
        StringGrid1SelectCell(StringGrid1, 1, Row, CanSelect);

    end;

end;

procedure TFormData.FetchYearsMonths;
var
    I: integer;
    ym: TYearMonth;
begin
    ComboBox1.Clear;
    FYearMonth := TPartiesSvc.YearsMonths;
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

end.
