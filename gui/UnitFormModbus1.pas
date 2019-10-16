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

end.
