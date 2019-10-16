unit UnitFormModbus;

interface

uses
    Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants,
    System.Classes, Vcl.Graphics,
    Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, Vcl.ComCtrls,
  Vcl.ToolWin, Vcl.ExtCtrls, System.ImageList, Vcl.ImgList;

type
    TFormModbus = class(TForm)
        GroupBox1: TGroupBox;
        Label1: TLabel;
        Edit1: TEdit;
        Label2: TLabel;
        Edit2: TEdit;
        Button1: TButton;
        GroupBox2: TGroupBox;
        Label3: TLabel;
        Edit3: TEdit;
        Button2: TButton;
        CheckBox1: TCheckBox;
        GroupBox3: TGroupBox;
        Label4: TLabel;
        Edit4: TEdit;
        Button3: TButton;
        GroupBox4: TGroupBox;
        Label5: TLabel;
        Edit5: TEdit;
        Button4: TButton;
    ImageList4: TImageList;
    PanelMessageBoxTitle: TPanel;
    ToolBar2: TToolBar;
    ToolButton3: TToolButton;
        procedure Button3Click(Sender: TObject);
        procedure Button4Click(Sender: TObject);
        procedure Button1Click(Sender: TObject);
        procedure Button2Click(Sender: TObject);
    procedure ToolButton3Click(Sender: TObject);
    private
        { Private declarations }
    public
        { Public declarations }
    end;

var
    FormModbus: TFormModbus;

implementation

{$R *.dfm}

uses services;

procedure TFormModbus.Button1Click(Sender: TObject);
var n:integer;
    v:double;
begin
    n := StrToInt(Edit1.Text);
    v := StrToFloat(Edit2.Text);
    TThread.CreateAnonymousThread(
        procedure
        begin
            TRunnerSvc.Write32(n, v);
        end).Start;
end;

procedure TFormModbus.Button2Click(Sender: TObject);
var v, n : integer;
begin
    if CheckBox1.Checked then
        n := 1
    else
        n := 0;
    v := StrToInt(Edit3.Text);
    TThread.CreateAnonymousThread(
        procedure
        begin
            TRunnerSvc.Read3(v, n)
        end).Start;
end;

procedure TFormModbus.Button3Click(Sender: TObject);
var n : integer;
begin
    n := StrToInt(Edit4.Text);
    TThread.CreateAnonymousThread(
        procedure
        begin
            TRunnerSvc.SwitchGas(n);
        end).Start;

end;

procedure TFormModbus.Button4Click(Sender: TObject);
var n : integer;
begin
    n := StrToInt(Edit5.Text);
    TThread.CreateAnonymousThread(
        procedure
        begin
            TRunnerSvc.SetNetAddress(n);
        end).Start;

end;

procedure TFormModbus.ToolButton3Click(Sender: TObject);
begin
    Hide;
end;

end.
