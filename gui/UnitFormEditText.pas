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
        procedure Colorize;
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
    RichEdit1.Text :=  TConfigSvc.GetConfigToml;
    Colorize;
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
    Colorize;

end;

procedure TFormEditText.ToolButton2Click(Sender: TObject);
var
    ASelStart : integer;
begin
    ASelStart := RichEdit1.SelStart;
    RichEdit1.Text := TConfigSvc.SetConfigToml(RichEdit1.Text);
    RichEdit1.SelStart := ASelStart;
     RichEdit1.SelLength := 0 ;
    Colorize;
end;

procedure TFormEditText.ToolButton3Click(Sender: TObject);
var
    ASelStart : integer;
begin
    ASelStart := RichEdit1.SelStart;
    RichEdit1.Text :=  TConfigSvc.GetConfigToml;
    ToolButton2.Enabled := false;
    RichEdit1.SelStart := ASelStart;
     RichEdit1.SelLength := 0 ;
    Colorize;
end;

procedure TFormEditText.Colorize;
var
    ASelStart, iPos, iPosWord, iLen, n: integer;


begin
    ASelStart := RichEdit1.SelStart;
    RichEdit1.Hide;
    iLen := Length(RichEdit1.Lines.Text);

    iPos := 0;
    iPos := RichEdit1.FindText('#', iPos, iLen, []);
    while iPos <> -1 do
    begin
        iPosWord := RichEdit1.FindText(#13, iPos, iLen, []);
        RichEdit1.SelStart := iPos;
        RichEdit1.SelLength := iPosWord - iPos;
        RichEdit1.SelAttributes.Color := clGray;
        iPos := RichEdit1.FindText('#', iPosWord, iLen, []);
    end;

    iPos := 0;
    iPos := RichEdit1.FindText('[', iPos, iLen, []);
    while iPos <> -1 do
    begin
        iPosWord := RichEdit1.FindText(']', iPos, iLen, []);
        RichEdit1.SelStart := iPos;
        RichEdit1.SelLength := iPosWord - iPos + 1;
        RichEdit1.SelAttributes.Color := clNavy;
        iPos := RichEdit1.FindText('[', iPosWord, iLen, []);
    end;

    iPos := 0;
    iPos := RichEdit1.FindText('=', iPos, iLen, []);
    while iPos > 3 do
    begin
        RichEdit1.SelStart := iPos - 2;
        RichEdit1.SelLength := 2;
        while RichEdit1.SelText[1] <> ' ' do
        begin
            n := RichEdit1.SelLength;
            RichEdit1.SelStart := RichEdit1.SelStart - 1;
            RichEdit1.SelLength := n + 1;
        end;
        RichEdit1.SelAttributes.Color := clGreen;

        RichEdit1.SelStart := iPos + 1;
        RichEdit1.SelLength := 2;
        while RichEdit1.SelText[Length(RichEdit1.SelText)] <> #13 do
        begin
            n := RichEdit1.SelLength;
            RichEdit1.SelLength := n + 1;
        end;
        RichEdit1.SelAttributes.Color := clMaroon;

        iPos := RichEdit1.FindText('=', iPos + 1, iLen, []);
    end;
    RichEdit1.SelStart := ASelStart ;
    RichEdit1.SelLength := 0 ;
    RichEdit1.Show;
    //RichEdit1.SelLength := 0;
    ToolButton2.Enabled := false;

end;

end.
