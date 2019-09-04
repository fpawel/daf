unit UnitFormLastParty;

interface

uses
    Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants,
    System.Classes, Vcl.Graphics,
    Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.Grids, Vcl.StdCtrls,
    Vcl.Imaging.pngimage, Vcl.ExtCtrls, System.ImageList, Vcl.ImgList,
    Vcl.Menus, Vcl.ComCtrls, Vcl.ToolWin, server_data_types,
    System.Generics.Collections;

type

    TConnectionInfo = record
        Text: string;
        Ok: boolean;

    end;

    TFormLastParty = class(TForm)
        StringGrid1: TStringGrid;
        ImageList1: TImageList;
        ToolBarParty: TToolBar;
        ToolButtonParty: TToolButton;
        ToolButton2: TToolButton;
        ImageList2: TImageList;
        procedure StringGrid1DrawCell(Sender: TObject; ACol, ARow: Integer;
          Rect: TRect; State: TGridDrawState);
        procedure StringGrid1MouseDown(Sender: TObject; Button: TMouseButton;
          Shift: TShiftState; X, Y: Integer);
        procedure FormShow(Sender: TObject);
        procedure FormCreate(Sender: TObject);
        procedure ToolButtonPartyClick(Sender: TObject);
    private
        { Private declarations }
        FhWndTip: THandle;
        FPlaceInterrogate: Integer;
        FPlaceConnection: TDictionary<Integer, TConnectionInfo>;

        procedure WMWindowPosChanged(var AMessage: TMessage);
          message WM_WINDOWPOSCHANGED;
        procedure WMEnterSizeMove(var Msg: TMessage); message WM_ENTERSIZEMOVE;

        procedure WMActivateApp(var AMessage: TMessage); message WM_ACTIVATEAPP;

    public
        { Public declarations }
        FProducts: TArray<TProduct>;
        procedure reload_data;
        procedure setup_products;

        procedure OnProductError(X: TProductError);
        procedure OnProductValue(X: TProductValue);
        procedure OnWorkComplete;
    end;

var
    FormLastParty: TFormLastParty;

implementation

uses stringgridutils, stringutils, dateutils,
    vclutils, ComponentBaloonHintU, services, HttpRpcClient, app;

{$R *.dfm}

function KeyAddrVar(addr, nvar: Integer): string;
begin
    result := Format('%d-%d', [addr, nvar]);
end;

procedure TFormLastParty.FormCreate(Sender: TObject);

begin
    FPlaceInterrogate := -1;
    FPlaceConnection := TDictionary<Integer, TConnectionInfo>.create;

end;

procedure TFormLastParty.FormShow(Sender: TObject);
// var
// place, n: Integer;
begin
    reload_data;

    // for n := 0 to Length(AppVars) - 1 do
    // for place := 0 to Length(FProducts) - 1 do
    // FormChartSeries.SetAddrVarSeries(FProducts[place].addr,
    // AppVars[n].Code, AppSets.ReadBool('series', KeyPlaceNVar(place,
    // n), false));
end;

procedure TFormLastParty.WMEnterSizeMove(var Msg: TMessage);
begin
    CloseWindow(FhWndTip);
    inherited;
end;

procedure TFormLastParty.WMWindowPosChanged(var AMessage: TMessage);
begin
    CloseWindow(FhWndTip);
    inherited;
end;

procedure TFormLastParty.WMActivateApp(var AMessage: TMessage);
begin
    CloseWindow(FhWndTip);
    inherited;
end;

procedure TFormLastParty.ToolButtonPartyClick(Sender: TObject);
var
    r: Integer;
begin
    r := MessageBox(Handle, 'Подтвердите необходимость создания новой партии.',
      'Запрос подтверждения', mb_IconQuestion or mb_YesNo);
    if r <> mrYes then
        exit;
    TPartiesSvc.CreateNewParty('');
    reload_data;

end;

procedure TFormLastParty.StringGrid1MouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var
    ACol, ARow: Integer;
    p: TProduct;
begin
    if (GetAsyncKeyState(VK_LBUTTON) >= 0) then
        exit;
    StringGrid1.MouseToCell(X, Y, ACol, ARow);
    if (ACol > 0) or (ARow < 1) then
        exit;

    TConfigSvc.SetPlaceChecked(ARow - 1, not FProducts[ARow - 1].Checked);
    FProducts[ARow - 1].Checked := not FProducts[ARow - 1].Checked;
    StringGrid_RedrawRow(StringGrid1, ARow);

end;

procedure TFormLastParty.StringGrid1DrawCell(Sender: TObject;
  ACol, ARow: Integer; Rect: TRect; State: TGridDrawState);
var
    grd: TStringGrid;
    cnv: TCanvas;
    p: TProduct;
    connInfo: TConnectionInfo;
    connBmpIndex1, connBmpIndex2: Integer;

    procedure DrawCellConnection;
    var
        bmp: TBitmap;
    begin
        bmp := TBitmap.create;
        ImageList2.GetBitmap(connBmpIndex1 + connBmpIndex2, bmp);
        StringGrid1.Canvas.FillRect(Rect);
        StringGrid_DrawCellBmp(StringGrid1, Rect, bmp);
        bmp.Free
    end;

begin
    grd := StringGrid1;
    cnv := grd.Canvas;
    cnv.Font.Assign(grd.Font);
    cnv.Brush.Color := clWhite;

    if (ARow = 0) or (ACol = 0) then
        cnv.Brush.Color := cl3DLight;

    if ARow = 0 then
    begin
        DrawCellText(StringGrid1, ACol, ARow, Rect, taCenter,
          StringGrid1.Cells[ACol, ARow]);
        StringGrid_DrawCellBounds(StringGrid1.Canvas, ACol, 0, Rect);
        exit;
    end;

    p := FProducts[ARow - 1];

    connBmpIndex2 := 0;
    if ARow = FPlaceInterrogate + 1 then
    begin
        cnv.Brush.Color := clSkyBlue;
        connBmpIndex2 := 2;
    end;

    if ACol = 0 then
    begin
        grd.Canvas.FillRect(Rect);
        DrawCheckbox(grd, grd.Canvas, Rect, p.Checked, grd.Cells[ACol, ARow]);
        StringGrid_DrawCellBounds(cnv, ACol, ARow, Rect);
        exit;
    end;

    if gdSelected in State then
    begin
        cnv.Brush.Color := clGradientInactiveCaption;
        connBmpIndex2 := 1;
    end;

    connBmpIndex1 := 0;

    if (ACol = 3) AND FPlaceConnection.TryGetValue(ARow - 1, connInfo) then
    begin
        if not connInfo.Ok then
            connBmpIndex1 := 3;
        DrawCellConnection;
    end
    else
        DrawCellText(StringGrid1, ACol, ARow, Rect, taLeftJustify,
          StringGrid1.Cells[ACol, ARow]);

    StringGrid_DrawCellBounds(cnv, ACol, ARow, Rect);
end;

procedure TFormLastParty.setup_products;
var
    place, n, ARow, ACol: Integer;

begin
    Height := StringGrid1.DefaultRowHeight * (Length(FProducts) + 1) + 50;
    StringGrid_Clear(StringGrid1);
    with StringGrid1 do
    begin
        self.Height := DefaultRowHeight * (Length(FProducts) + 1) + 50;

        ColCount := 3;
        RowCount := Length(FProducts) + 1;
        if Length(FProducts) = 0 then
            exit;

        FixedRows := 1;
        FixedCols := 1;
        ColWidths[0] := 80;

        Cells[0, 0] := 'Адрес';
        Cells[1, 0] := 'ID';
        Cells[2, 0] := 'ДАФ';

        for ARow := 1 to RowCount - 1 do
        begin
            Cells[0, ARow] := IntToStr(FProducts[ARow - 1].addr);
            Cells[1, ARow] := Inttostr(FProducts[ARow - 1].ProductID);
            Cells[2, ARow] := Inttostr(FProducts[ARow - 1].Serial);
        end;

    end;
    StringGrid_SetupColumnsWidth(StringGrid1);
end;

procedure TFormLastParty.reload_data;
begin
    with Application.MainForm do
        with TLastPartySvc.Party do
            Caption := Format('Загрузка ДАФ-М %d от %s',
              [PartyID, FormatDateTime('dd MMMM yyyy hh:nn',
              IncHour(CreatedAt, 3))]);
    FProducts := TLastPartySvc.Products;
    setup_products;

end;

procedure TFormLastParty.OnProductValue(X: TProductValue);
var
    ACol, prevPlaceInterrogate: Integer;
    connInfo: TConnectionInfo;
begin
    if (X.place < 0) Or (X.place >= Length(FProducts)) then
        exit;

    prevPlaceInterrogate := FPlaceInterrogate;

    FPlaceInterrogate := X.place;

    connInfo.Ok := true;
    connInfo.Text := Format('%s[%d]=%s', [X.Column, floatToStr(X.Value)]);
    FPlaceConnection.AddOrSetValue(X.place, connInfo);

    with StringGrid1 do
    begin
        ACol := Rows[0].IndexOf(X.Column);
        if ACol = -1 then
        begin
            ColCount := ColCount + 1;
            ACol := ColCount - 1;
            Cells[ACol, 0] := X.Column;
        end;
        StringGrid1.Cells[ACol, X.place + 1] := floatToStr(X.Value);
        StringGrid_SetupColumnWidth(StringGrid1, ACol);
    end;

    if (prevPlaceInterrogate > -1) AND (prevPlaceInterrogate <> X.place) then
        StringGrid_RedrawRow(StringGrid1, prevPlaceInterrogate + 1);

    StringGrid_RedrawRow(StringGrid1, X.place + 1);

end;

procedure TFormLastParty.OnProductError(X: TProductError);
var
    prevPlaceInterrogate: Integer;
    connInfo: TConnectionInfo;
begin
    prevPlaceInterrogate := FPlaceInterrogate;

    FPlaceInterrogate := x.Place;

    connInfo.Ok := false;
    connInfo.Text := X.Message;
    FPlaceConnection.AddOrSetValue(x.Place, connInfo);

    if (prevPlaceInterrogate > -1) AND (prevPlaceInterrogate <> x.Place) then
        StringGrid_RedrawRow(StringGrid1, prevPlaceInterrogate + 1);
    StringGrid_RedrawRow(StringGrid1, x.Place + 1);
end;

procedure TFormLastParty.OnWorkComplete;
var
    prevPlaceInterrogate: Integer;
begin
    prevPlaceInterrogate := FPlaceInterrogate;
    FPlaceInterrogate := -1;
    if prevPlaceInterrogate > -1 then
        StringGrid_RedrawRow(StringGrid1, prevPlaceInterrogate + 1);

end;

end.
