unit UnitFormPopup;

interface

uses
    Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants,
    System.Classes, Vcl.Graphics,
    Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, Vcl.grids, Vcl.ComCtrls;

type
    TFormPopup = class(TForm)
        RichEdit1: TRichEdit;
        procedure FormDeactivate(Sender: TObject);
    private
        { Private declarations }
    public
        { Public declarations }
        procedure ShowStringGridCellText(AStringGrid: TStringGrid);
    end;

var
    FormPopup: TFormPopup;

implementation

{$R *.dfm}

procedure TFormPopup.FormDeactivate(Sender: TObject);
begin
    Hide;
end;

procedure TFormPopup.ShowStringGridCellText(AStringGrid: TStringGrid);
var
    r: TRect;
    pt: TPoint;
begin
    RichEdit1.Text := stringreplace(AStringGrid.Cells[AStringGrid.Col,
      AStringGrid.Row], ': ', #13#10#9' - ', [rfReplaceAll, rfIgnoreCase]);

    r := AStringGrid.CellRect(AStringGrid.Col, AStringGrid.Row);
    pt := AStringGrid.ClientToScreen(r.TopLeft);
    Left := pt.x + 3;
    Top := pt.y + AStringGrid.RowHeights[AStringGrid.Row] + 3;
    Show;

end;

end.
