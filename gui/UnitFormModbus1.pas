unit UnitFormModbus1;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, Vcl.ExtCtrls;

type
  TFormModbus1 = class(TForm)
    Label1: TLabel;
    Panel1: TPanel;
    Edit1: TEdit;
    Label2: TLabel;
    Panel2: TPanel;
    Edit2: TEdit;
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
  FormModbus1: TFormModbus1;

implementation

{$R *.dfm}

uses services;

procedure TFormModbus1.Button1Click(Sender: TObject);
begin
    TRunnerSvc.Write32(strtoint(Edit1.Text), strtofloat(Edit2.Text));
end;

procedure TFormModbus1.Button2Click(Sender: TObject);
begin
    TRunnerSvc.Read3(strtoint(Edit1.Text));
end;

end.
