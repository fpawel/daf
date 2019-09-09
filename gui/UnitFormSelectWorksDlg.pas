unit UnitFormSelectWorksDlg;

interface

uses
    Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants,
    System.Classes, Vcl.Graphics,
    Vcl.Controls, Vcl.Forms, Vcl.Dialogs, System.ImageList, Vcl.ImgList,
    Vcl.ComCtrls, Vcl.ToolWin, Vcl.StdCtrls, Vcl.CheckLst, Vcl.ExtCtrls;

type
    TFormSelectWorksDlg = class(TForm)
        ImageList3: TImageList;
        Panel14: TPanel;
        CheckListBox1: TCheckListBox;
        ToolBarStop: TToolBar;
        ToolButton2: TToolButton;
        procedure FormDeactivate(Sender: TObject);
        procedure FormCreate(Sender: TObject);
        procedure ToolButton2Click(Sender: TObject);
    private
        { Private declarations }
    public
        { Public declarations }
    end;

var
    FormSelectWorksDlg: TFormSelectWorksDlg;

implementation

{$R *.dfm}

uses services;

procedure TFormSelectWorksDlg.FormCreate(Sender: TObject);
var
    i: integer;
begin
    for I := 0 to CheckListBox1.Items.Count - 1 do
        CheckListBox1.Checked[i] := true;
end;

procedure TFormSelectWorksDlg.FormDeactivate(Sender: TObject);
begin
    hide;
end;

procedure TFormSelectWorksDlg.ToolButton2Click(Sender: TObject);
var Works:TArray<boolean>;
    i:integer;
begin
    SetLength(works, CheckListBox1.Items.Count);
    for I := 0 to CheckListBox1.Items.Count - 1 do
        works[i] := CheckListBox1.Checked[i];
    hide;
    TRunnerSvc.RunMainWork(works);

end;

end.
