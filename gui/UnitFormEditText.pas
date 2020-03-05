unit UnitFormEditText;

interface

uses
    Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants,
    System.Classes, Vcl.Graphics,
    Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.ComCtrls, Vcl.ToolWin,
    System.ImageList, Vcl.ImgList, Vcl.StdCtrls;

type
    TFormEditText = class(TForm)
        ImageList4: TImageList;
        ToolBarStop: TToolBar;
        ToolButton2: TToolButton;
        RichEdit1: TRichEdit;
        ToolButton1: TToolButton;
    ToolButton3: TToolButton;
        procedure ToolButton2Click(Sender: TObject);
        procedure FormShow(Sender: TObject);
        procedure ToolButton1Click(Sender: TObject);
        procedure FormDeactivate(Sender: TObject);
    procedure ToolButton3Click(Sender: TObject);
    procedure RichEdit1Change(Sender: TObject);
    private
        { Private declarations }
    public
        { Public declarations }
    end;

var
    FormEditText: TFormEditText;

implementation

{$R *.dfm}

uses services, Winapi.RichEdit, System.Character;

procedure TFormEditText.FormDeactivate(Sender: TObject);
begin
    Hide;
end;

procedure TFormEditText.FormShow(Sender: TObject);
begin
    RichEdit1.Text :=  TConfigSvc.GetConfigYaml;
end;

procedure TFormEditText.RichEdit1Change(Sender: TObject);
begin
    ToolButton2.Enabled := true;
end;

procedure TFormEditText.ToolButton1Click(Sender: TObject);
var
    r: Integer;
begin
    r := MessageBox(Handle, 'Подтвердите необходимость установки всех настроек приложения по умолчанию.',
      'Запрос подтверждения', mb_IconQuestion or mb_YesNo);
    if r <> mrYes then
        exit;
    RichEdit1.Text := TConfigSvc.SetDefault;
end;

procedure TFormEditText.ToolButton2Click(Sender: TObject);
var
    ASelStart : integer;
begin
    ASelStart := RichEdit1.SelStart;
    RichEdit1.Text := TConfigSvc.SetConfigYaml(RichEdit1.Text);
    RichEdit1.SelStart := ASelStart;
     RichEdit1.SelLength := 0 ;
end;

procedure TFormEditText.ToolButton3Click(Sender: TObject);
var
    ASelStart : integer;
begin
    ASelStart := RichEdit1.SelStart;
    RichEdit1.Text :=  TConfigSvc.GetConfigYaml;
    ToolButton2.Enabled := false;
    RichEdit1.SelStart := ASelStart;
     RichEdit1.SelLength := 0 ;
end;

end.
