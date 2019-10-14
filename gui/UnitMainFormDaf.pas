unit UnitMainFormDaf;

interface

uses
    Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants,
    System.Classes, Vcl.Graphics,
    Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.ExtCtrls, Vcl.Menus, Vcl.ComCtrls,
    Vcl.StdCtrls, Vcl.ToolWin, Vcl.Imaging.pngimage, System.ImageList,
    server_data_types, Vcl.ImgList, UnitFormProductData;

type
    EHostApplicationPanic = class(Exception);

    TMainFormDaf = class(TForm)
        ImageList4: TImageList;
        PageControlMain: TPageControl;
        TabSheetParty: TTabSheet;
        TabSheetConsole: TTabSheet;
        PanelMessageBox: TPanel;
        ImageError: TImage;
        ImageInfo: TImage;
        PanelMessageBoxTitle: TPanel;
        ToolBar2: TToolBar;
        ToolButton3: TToolButton;
        RichEditlMessageBoxText: TRichEdit;
        PanelTop: TPanel;
        LabelStatusTop: TLabel;
        ToolBar1: TToolBar;
        ToolButtonRun: TToolButton;
        ToolBar3: TToolBar;
        ToolButton1: TToolButton;
        ToolButton4: TToolButton;
        PanelDelay: TPanel;
        LabelDelayElepsedTime: TLabel;
        LabelProgress: TLabel;
        ToolBar6: TToolBar;
        ToolButtonStop: TToolButton;
        Panel2: TPanel;
        ProgressBar1: TProgressBar;
        ToolBarStop: TToolBar;
        ToolButton2: TToolButton;
        PopupMenu1: TPopupMenu;
        N1: TMenuItem;
        TimerPerforming: TTimer;
        LabelStatusBottom1: TLabel;
        N821: TMenuItem;
        TabSheetData: TTabSheet;
        N2: TMenuItem;
        N11: TMenuItem;
        N21: TMenuItem;
        N31: TMenuItem;
        N41: TMenuItem;
        N3: TMenuItem;
        MenuSetAddress: TMenuItem;
        procedure FormShow(Sender: TObject);
        procedure FormCreate(Sender: TObject);
        procedure PageControlMainDrawTab(Control: TCustomTabControl;
          TabIndex: Integer; const Rect: TRect; Active: Boolean);
        procedure PageControlMainChange(Sender: TObject);
        procedure FormClose(Sender: TObject; var Action: TCloseAction);
        procedure ToolButton4Click(Sender: TObject);
        procedure N1Click(Sender: TObject);
        procedure ToolButtonRunClick(Sender: TObject);
        procedure TimerPerformingTimer(Sender: TObject);
        procedure FormResize(Sender: TObject);
        procedure ToolButton2Click(Sender: TObject);
        procedure ToolButton3Click(Sender: TObject);
        procedure N821Click(Sender: TObject);
        procedure ToolButtonStopClick(Sender: TObject);
        procedure ToolButton1Click(Sender: TObject);
        procedure N41Click(Sender: TObject);
        procedure PopupMenu1Popup(Sender: TObject);
    private
        { Private declarations }
        procedure AppException(Sender: TObject; E: Exception);
        procedure HandleCopydata(var Message: TMessage); message WM_COPYDATA;
        procedure SetupDelay(i: TDelayInfo);
        procedure OnWorkComplete(x: TWorkResultInfo);
        procedure OnWarning(content: string);

        procedure SetAddresClick(Sender: TObject);
    public
        { Public declarations }

    end;

var
    MainFormDaf: TMainFormDaf;

implementation

{$R *.dfm}

uses UnitFormLastParty, vclutils, JclDebug, ioutils, app,
    services, UnitFormAppConfig, notify_services, HttpRpcClient, superobject,
    dateutils, math, HttpExceptions, UnitFormData,
    stringgridutils, UnitFormModalMessage, UnitFormEditText, UnitFormDataTable,
    UnitFormSelectWorksDlg, UnitFormConsole;

function color_work_result(r: Integer): Tcolor;
begin
    if r = 0 then
        exit(clNavy)
    else if r = 1 then
        exit(clMaroon)
    else
        exit(clRed);
end;

procedure TMainFormDaf.FormCreate(Sender: TObject);
begin
    Application.OnException := AppException;
    LabelStatusTop.Caption := '';
    PanelMessageBox.Width := 700;
    PanelMessageBox.Height := 350;
    LabelStatusBottom1.Caption := '';
end;

procedure TMainFormDaf.FormClose(Sender: TObject; var Action: TCloseAction);
var
    wp: WINDOWPLACEMENT;
    fs: TFileStream;
begin
    NotifyServices_SetEnabled(false);
    HttpRpcClient.TIMEOUT_CONNECT := 10;
     //notify_services.CloseServerWindow;

    fs := TFileStream.Create(ChangeFileExt(paramstr(0), '.position'),
      fmOpenWrite or fmCreate);
    if not GetWindowPlacement(Handle, wp) then
        raise Exception.Create('GetWindowPlacement: false');
    fs.Write(wp, SizeOf(wp));
    fs.Free;

end;

procedure TMainFormDaf.FormShow(Sender: TObject);
var
    FileName: String;
    wp: WINDOWPLACEMENT;
    fs: TFileStream;
begin

    FileName := ChangeFileExt(paramstr(0), '.position');
    if FileExists(FileName) then
    begin
        fs := TFileStream.Create(FileName, fmOpenRead);
        fs.Read(wp, SizeOf(wp));
        fs.Free;
        SetWindowPlacement(Handle, wp);
    end;

    with FormProductData do
    begin
        Font.Assign(self.Font);
        BorderStyle := bsNone;
        Align := alClient;
    end;


    with FormLastParty do
    begin
        Font.Assign(self.Font);
        Parent := TabSheetParty;
        BorderStyle := bsNone;
        Align := alClient;
        reload_data;
        Show;
    end;

    with FormData do
    begin
        Font.Assign(self.Font);
        Parent := TabSheetData;
        BorderStyle := bsNone;
        Align := alClient;
        Show;
        // FetchYearsMonths;
    end;

    with FormConsole do
    begin
        Font.Assign(self.Font);
        Parent := TabSheetConsole;
        BorderStyle := bsNone;
        Align := alClient;
        // FFileName := ExtractFileDir(paramstr(0)) + '\elco.log';
        FFileName := '';
        Show;
    end;

    PageControlMain.ActivePageIndex := 0;

    SetOnWorkStarted(
        procedure(s: string)
        begin
            PanelMessageBox.Hide;
            ToolBarStop.Show;
            LabelStatusTop.Caption := TimeToStr(now) + ' ' + s;
            TimerPerforming.Enabled := true;
            LabelStatusBottom1.Caption := '';
        end);

    SetOnWorkComplete(OnWorkComplete);

    SetOnPlaceConnection(FormLastParty.OnProductConnection);

    SetOnDelay(SetupDelay);

    SetOnEndDelay(
        procedure(s: string)
        begin
            PanelDelay.Hide;
        end);

    SetOnWarning(OnWarning);

    SetOnStatus(
        procedure(s: string)
        begin
            LabelStatusTop.Caption := TimeToStr(now) + ' ' + s;
        end);

    SetOnProductDataChanged(FormProductData.OnProductDataChanged);

    SetOnWriteConsole(
        procedure(s: String)
        begin
            FormConsole.NewLine(s);
            if not TabSheetConsole.TabVisible then
                TabSheetConsole.TabVisible := true;
            Application.ProcessMessages;
        end);

    NotifyServices_SetEnabled(true);
end;

procedure TMainFormDaf.FormResize(Sender: TObject);
begin
    if PanelMessageBox.Visible then
    begin
        PanelMessageBox.Left := ClientWidth div 2 - PanelMessageBox.Width div 2;
        PanelMessageBox.Top := ClientHeight div 2 -
          PanelMessageBox.Height div 2;
    end;
end;

procedure TMainFormDaf.PageControlMainChange(Sender: TObject);
var
    PageControl: TPageControl;
begin
    PageControl := Sender as TPageControl;
    PageControl.Repaint;
    PanelMessageBox.Hide;

    if PageControl.ActivePage = TabSheetData then
    begin
        // FormProductData.Parent :=FormData;
        FormData.FetchYearsMonths;
    end
    else if PageControl.ActivePage = TabSheetParty then
    begin
        FormLastParty.setup_products;
    end;

end;

procedure TMainFormDaf.PageControlMainDrawTab(Control: TCustomTabControl;
TabIndex: Integer; const Rect: TRect; Active: Boolean);
begin
    PageControl_DrawVerticalTab(Control, TabIndex, Rect, Active);
end;

procedure TMainFormDaf.PopupMenu1Popup(Sender: TObject);
var
    i: Integer;
    m: TMenuItem;
begin
    MenuSetAddress.Clear;
    for i := 0 to length(FormLastParty.FProducts) - 1 do
    begin
        m := TMenuItem.Create(nil);
        m.Caption := Inttostr(FormLastParty.FProducts[i].Addr);
        m.Tag := FormLastParty.FProducts[i].Addr;
        m.OnClick := SetAddresClick;
        MenuSetAddress.Add(m);
    end;

end;

procedure TMainFormDaf.TimerPerformingTimer(Sender: TObject);
var
    v: Integer;
begin
    with LabelStatusTop.Font do
        if Color = clRed then
            Color := clblue
        else
            Color := clRed;
end;

procedure TMainFormDaf.ToolButton1Click(Sender: TObject);
begin
    FormEditText.Show;
end;

procedure TMainFormDaf.ToolButton2Click(Sender: TObject);
begin
    TRunnerSvc.Cancel;
end;

procedure TMainFormDaf.ToolButton3Click(Sender: TObject);
begin
    PanelMessageBox.Hide;
end;

procedure TMainFormDaf.ToolButton4Click(Sender: TObject);
begin
    with ToolButton4 do
        with ClientToScreen(Point(0, Height)) do
        begin
            with FormAppconfig do
            begin
                Left := x - 5 - Width;
                Top := Y + 5;
                Show;
            end;
        end;
end;

procedure TMainFormDaf.ToolButtonRunClick(Sender: TObject);
begin
    with ToolButtonRun do
        with ClientToScreen(Point(0, Height)) do
            PopupMenu1.Popup(x, Y);
end;

procedure TMainFormDaf.ToolButtonStopClick(Sender: TObject);
begin
    TRunnerSvc.SkipDelay;
end;

procedure TMainFormDaf.HandleCopydata(var Message: TMessage);
begin
    notify_services.HandleCopydata(Message);
end;

procedure TMainFormDaf.N1Click(Sender: TObject);
begin
    TRunnerSvc.RunReadVars;
end;

procedure TMainFormDaf.N41Click(Sender: TObject);
begin
    TRunnerSvc.SwitchGas((Sender as TComponent).Tag);

end;

procedure TMainFormDaf.N821Click(Sender: TObject);
begin
    with ToolButtonRun do
        with ClientToScreen(Point(0, Height)) do
        begin
            ToolButtonRun.PopupMenu.CloseMenu;
            FormSelectWorksDlg.Left := x + 5;
            FormSelectWorksDlg.Top := Y + 5;
            FormSelectWorksDlg.Show;
        end;
end;

procedure TMainFormDaf.AppException(Sender: TObject; E: Exception);
var
    stackList: TJclStackInfoList; // JclDebug.pas
    sl: TStringList;
    stacktrace: string;

    FErrorLog: TextFile;
    ErrorLogFileName: string;
begin

    stackList := JclCreateStackList(false, 0, Caller(0, false));
    sl := TStringList.Create;
    stackList.AddToStrings(sl, true, false, true, false);
    stacktrace := sl.Text;
    sl.Free;
    stackList.Free;
    OutputDebugStringW(PWideChar(E.Message + #10#13 + stacktrace));

    ErrorLogFileName := ChangeFileExt(paramstr(0), '.log');
    AssignFile(FErrorLog, ErrorLogFileName, CP_UTF8);
    if FileExists(ErrorLogFileName) then
        Append(FErrorLog)
    else
        Rewrite(FErrorLog);

    Writeln(FErrorLog, FormatDateTime('dd/MM/yy hh:nn:ss', now), ' ',
      'delphi_exception', ' ', E.ClassName, ' ', stringreplace(Trim(E.Message),
      #13, ' ', [rfReplaceAll, rfIgnoreCase]));

    Writeln(FErrorLog, StringOfChar('-', 120));

    Writeln(FErrorLog, stringreplace(Trim(stacktrace), #13, ' ',
      [rfReplaceAll, rfIgnoreCase]));

    Writeln(FErrorLog, StringOfChar('-', 120));

    CloseFile(FErrorLog);

    if E is EHostApplicationPanic then
    begin
        Application.ShowException(E);
        Application.Terminate;
        exit;
    end;

    if MessageDlg(E.Message, mtError, [mbAbort, mbIgnore], 0) = mrAbort then
    begin
        NotifyServices_SetEnabled(false);
        HttpRpcClient.TIMEOUT_CONNECT := 10;
        notify_services.CloseServerWindow;

        Application.OnException := nil;
        Application.Terminate;
        exit;
    end;
end;

procedure TMainFormDaf.SetupDelay(i: TDelayInfo);
var
    v: TDateTime;
begin

    PanelDelay.Visible := true;

    LabelProgress.Caption := '';
    ProgressBar1.Position := i.ElapsedSeconds * 1000;
    ProgressBar1.Max := i.TotalSeconds * 1000;
    v := 0;
    LabelDelayElepsedTime.Caption := FormatDateTime('HH:mm:ss',
      IncSecond(0, i.ElapsedSeconds));
    LabelProgress.Caption :=
      Inttostr(ceil(ProgressBar1.Position * 100 / ProgressBar1.Max)) + '%';
end;

procedure TMainFormDaf.OnWorkComplete(x: TWorkResultInfo);
var
    s: string;
begin
    ToolBarStop.Visible := false;

    TimerPerforming.Enabled := false;

    if RichEditlMessageBoxText.Lines.Count > 14 then
        RichEditlMessageBoxText.ScrollBars := ssVertical
    else
        RichEditlMessageBoxText.ScrollBars := ssNone;

    LabelStatusTop.Caption := TimeToStr(now) + ': ' + x.Work;

    case x.Result of
        0:
            begin
                ImageInfo.Show;
                ImageError.Hide;
                LabelStatusTop.Font.Color := clNavy;
                RichEditlMessageBoxText.Font.Color := clNavy;
                PanelMessageBoxTitle.Caption := x.Work + ': успешно';
                LabelStatusTop.Caption := LabelStatusTop.Caption + ': успешно';
            end;
        1:
            begin
                ImageInfo.Show;
                ImageError.Hide;
                LabelStatusTop.Font.Color := clMaroon;
                RichEditlMessageBoxText.Font.Color := clMaroon;
                PanelMessageBoxTitle.Caption := x.Work + ': прервано';
                LabelStatusTop.Caption := LabelStatusTop.Caption + ': прервано';
            end;
        2:
            begin
                ImageInfo.Hide;
                ImageError.Show;
                LabelStatusTop.Font.Color := clRed;
                RichEditlMessageBoxText.Font.Color := clRed;
                PanelMessageBoxTitle.Caption := x.Work + ': не выполнено';
                LabelStatusTop.Caption := LabelStatusTop.Caption +
                  ': не выполнено';
            end;
    else
        raise Exception.Create('unknown work result: ' + Inttostr(x.Result));
    end;

    RichEditlMessageBoxText.Text := stringreplace(x.Message, ': ',
      #13#10#9' - ', [rfReplaceAll, rfIgnoreCase]);

    LabelStatusBottom1.Caption := '';
    PanelMessageBox.Show;
    PanelMessageBox.BringToFront;
    FormResize(self);
    FormLastParty.OnWorkComplete;
end;

procedure TMainFormDaf.OnWarning(content: string);
var
    s: string;
begin
    s := content + #10#13#10#13;
    s := s + '"Принять" - игнорировать ошибку и продолжить.'#10#13#10#13;
    s := s + '"Отмена" - прервать выполнение.';
    FormModalMessage.PanelMessageBoxTitle.Caption := 'Предупреждение';
    FormModalMessage.RichEditlMessageBoxText.Text := content;
    FormModalMessage.ImageInfo.Hide;
    FormModalMessage.ImageError.Show;

    FormModalMessage.ShowModal;
    if FormModalMessage.ModalResult <> mrOk then
        TRunnerSvc.Cancel;
end;

procedure TMainFormDaf.SetAddresClick(Sender: TObject);
begin
    TRunnerSvc.SetNetAddress((Sender as TComponent).Tag);
end;

end.
