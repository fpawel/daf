unit UnitFormPopup;

interface

uses
    Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants,
    System.Classes, Vcl.Graphics,
    Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, Vcl.grids, Vcl.ComCtrls,
  Vcl.Imaging.pngimage, Vcl.ExtCtrls;

type
    TFormPopup = class(TForm)
        RichEdit1: TRichEdit;
    ImageError: TImage;
    ImageInfo: TImage;
        procedure FormDeactivate(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure RichEdit1Enter(Sender: TObject);
    private
        { Private declarations }
    public
        { Public declarations }
        procedure SetText(s:string);
        procedure ShowStringGridCellText(AStringGrid: TStringGrid);
        procedure ShowAtStringGridCell(AStringGrid: TStringGrid);
    end;

var
    FormPopup: TFormPopup;

implementation

{$R *.dfm}

procedure TFormPopup.FormCreate(Sender: TObject);
begin
    //
end;

procedure TFormPopup.FormDeactivate(Sender: TObject);
begin
    Hide;
end;

procedure TFormPopup.RichEdit1Enter(Sender: TObject);
begin
    HideCaret(RichEdit1.Handle);
end;

procedure TFormPopup.SetText(s:string);
begin
    RichEdit1.Text := stringreplace(s, ': ', #13#10#9' - ', [rfReplaceAll, rfIgnoreCase]);
end;


procedure TFormPopup.ShowAtStringGridCell(AStringGrid: TStringGrid);
var
    r: TRect;
    pt: TPoint;
begin
    r := AStringGrid.CellRect(AStringGrid.Col, AStringGrid.Row);
    pt := AStringGrid.ClientToScreen(r.TopLeft);
    Left := pt.x + 3;
    Top := pt.y + AStringGrid.RowHeights[AStringGrid.Row] + 3;
    Show;
end;

procedure TFormPopup.ShowStringGridCellText(AStringGrid: TStringGrid);
begin
    SetText(AStringGrid.Cells[AStringGrid.Col,
      AStringGrid.Row]);
    ImageError.Hide;
    ImageInfo.Show;
    ShowAtStringGridCell(AStringGrid);
end;

end.
