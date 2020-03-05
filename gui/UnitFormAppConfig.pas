unit UnitFormAppConfig;

interface

uses
    Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants,
    System.Classes, Vcl.Graphics, System.Generics.collections,
    Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.ExtCtrls, Vcl.StdCtrls,
    ComponentBaloonHintU;

type
    EWrongInputExcpetion = class(Exception);

    TFormAppConfig = class(TForm)
    GroupBox2: TGroupBox;
    Panel1: TPanel;
    Shape1: TShape;
    Panel2: TPanel;
    ComboBoxComportProducts: TComboBox;
    Panel17: TPanel;
    Panel18: TPanel;
    ComboBoxComportHart: TComboBox;
    Panel3: TPanel;
    Panel4: TPanel;
    Shape3: TShape;
    CheckBox1: TCheckBox;
        procedure FormCreate(Sender: TObject);
        procedure FormDeactivate(Sender: TObject);
        procedure FormShow(Sender: TObject);
        procedure ComboBoxComportProductsChange(Sender: TObject);
    procedure CheckBox1Click(Sender: TObject);
    private
        { Private declarations }
        FhWndTip: THandle;
        FEnableOnEdit: boolean;
        procedure WMWindowPosChanged(var AMessage: TMessage);
          message WM_WINDOWPOSCHANGED;
        procedure WMEnterSizeMove(var Msg: TMessage); message WM_ENTERSIZEMOVE;

        procedure WMActivateApp(var AMessage: TMessage); message WM_ACTIVATEAPP;
        procedure ShowBalloonTip(c: TWinControl; Icon: TIconKind;
          Title, Text: string);

        function TryEditToInt(ed: TEdit): Integer;
        function TryEditToFloat(ed: TEdit): double;

    public
        { Public declarations }
    end;

var
    FormAppConfig: TFormAppConfig;

implementation

{$R *.dfm}

uses stringutils, services, server_data_types, comport, UnitFormModbus;

procedure setupCB(cb: TComboBox; s: string);
begin
    cb.ItemIndex := cb.Items.IndexOf(s);
end;

procedure TFormAppConfig.FormCreate(Sender: TObject);
begin
    FEnableOnEdit := false;
end;

procedure TFormAppConfig.FormShow(Sender: TObject);
var
    s: string;
    v: TAppConfig;

    p: TParty;

begin
    FEnableOnEdit := false;
    EnumComports(ComboBoxComportProducts.Items);
    EnumComports(ComboBoxComportHart.Items);
    v := TConfigSvc.GetConfig;

    setupCB(ComboBoxComportProducts, v.ComportProducts);
    setupCB(ComboBoxComportHart, v.ComportHart);

    FEnableOnEdit := true;
end;

procedure TFormAppConfig.FormDeactivate(Sender: TObject);
begin
    Hide;
end;

procedure TFormAppConfig.CheckBox1Click(Sender: TObject);
begin
    FormModbus.Height := 51;
    FormModbus.Visible := CheckBox1.Checked;
end;

procedure TFormAppConfig.ComboBoxComportProductsChange(Sender: TObject);
var
    v: TAppConfig;
begin
    if not FEnableOnEdit then
        exit;
    CloseWindow(FhWndTip);

    try
        v := TConfigSvc.GetConfig;
        v.ComportProducts := ComboBoxComportProducts.Text;
        v.ComportHart := ComboBoxComporthart.Text;
        TConfigSvc.SetConfig(v);
        (Sender as TWinControl).SetFocus;
    except
        on EWrongInputExcpetion do
            exit;
    end;
end;

procedure TFormAppConfig.WMEnterSizeMove(var Msg: TMessage);
begin
    CloseWindow(FhWndTip);
    inherited;
end;

procedure TFormAppConfig.WMWindowPosChanged(var AMessage: TMessage);
begin
    CloseWindow(FhWndTip);
    inherited;
end;

procedure TFormAppConfig.WMActivateApp(var AMessage: TMessage);
begin
    CloseWindow(FhWndTip);
    inherited;
end;

procedure TFormAppConfig.ShowBalloonTip(c: TWinControl; Icon: TIconKind;
  Title, Text: string);
begin
    CloseWindow(FhWndTip);
    FhWndTip := ComponentBaloonHintU.ShowBalloonTip(c, Icon, Title, Text);
end;

function TFormAppConfig.TryEditToInt(ed: TEdit): Integer;
begin
    if TryStrToInt(ed.Text, result) then
        exit(result);
    ShowBalloonTip(ed, TIconKind.Error, 'не допустимое значение',
      'ожидалось целое число');
    ed.SetFocus;
    raise EWrongInputExcpetion.Create('');


end;

function TFormAppConfig.TryEditToFloat(ed: TEdit): double;
begin
    if try_str_to_float(ed.Text, result) then
        exit(result);
    ShowBalloonTip(ed, TIconKind.Error, 'не допустимое значение',
      'ожидалось число c плавающей точкой');
    ed.SetFocus;
    raise EWrongInputExcpetion.Create('');

end;

end.
