unit UnitFormModalMessage;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, System.ImageList,
  Vcl.ImgList, Vcl.ComCtrls, Vcl.ToolWin, Vcl.Imaging.pngimage, Vcl.ExtCtrls;

type
  TFormModalMessage = class(TForm)
    PanelMessageBox: TPanel;
    ImageError: TImage;
    ImageInfo: TImage;
    PanelMessageBoxTitle: TPanel;
    RichEditlMessageBoxText: TRichEdit;
    Button1: TButton;
    Button2: TButton;
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  FormModalMessage: TFormModalMessage;

implementation

{$R *.dfm}

procedure TFormModalMessage.Button1Click(Sender: TObject);
begin
    ModalResult := mrOk;
end;

procedure TFormModalMessage.Button2Click(Sender: TObject);
begin
    ModalResult := mrCancel;
end;

end.
