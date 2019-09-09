
unit notify_services;

interface

uses superobject, Winapi.Windows, Winapi.Messages, server_data_types;

type
    TPlaceConnectionHandler = reference to procedure (x:TPlaceConnection);
    TWorkResultInfoHandler = reference to procedure (x:TWorkResultInfo);
    TDelayInfoHandler = reference to procedure (x:TDelayInfo);
    TInt64Handler = reference to procedure (x:Int64);
    TStringHandler = reference to procedure (x:string);
    

procedure HandleCopydata(var Message: TMessage);
procedure CloseServerWindow;

procedure SetOnPanic( AHandler : TStringHandler);
procedure SetOnPlaceConnection( AHandler : TPlaceConnectionHandler);
procedure SetOnWorkStarted( AHandler : TStringHandler);
procedure SetOnWorkComplete( AHandler : TWorkResultInfoHandler);
procedure SetOnWarning( AHandler : TStringHandler);
procedure SetOnDelay( AHandler : TDelayInfoHandler);
procedure SetOnEndDelay( AHandler : TStringHandler);
procedure SetOnStatus( AHandler : TStringHandler);
procedure SetOnProductDataChanged( AHandler : TInt64Handler);
procedure SetOnWriteConsole( AHandler : TStringHandler);

procedure NotifyServices_SetEnabled(enabled:boolean);

implementation 

uses Grijjy.Bson.Serialization, stringutils, sysutils;

type
    TServerAppCmd = (CmdPanic, CmdPlaceConnection, CmdWorkStarted, CmdWorkComplete, CmdWarning, CmdDelay, CmdEndDelay, CmdStatus, CmdProductDataChanged, 
    CmdWriteConsole);

    type _deserializer = record
        class function deserialize<T>(str:string):T;static;
    end;

var
    _OnPanic : TStringHandler;
    _OnPlaceConnection : TPlaceConnectionHandler;
    _OnWorkStarted : TStringHandler;
    _OnWorkComplete : TWorkResultInfoHandler;
    _OnWarning : TStringHandler;
    _OnDelay : TDelayInfoHandler;
    _OnEndDelay : TStringHandler;
    _OnStatus : TStringHandler;
    _OnProductDataChanged : TInt64Handler;
    _OnWriteConsole : TStringHandler;
    _enabled:boolean;

procedure CloseServerWindow;
begin
    SendMessage(FindWindow('DafServerWindow', nil), WM_CLOSE, 0, 0)
end;

class function _deserializer.deserialize<T>(str:string):T;
begin
    TgoBsonSerializer.Deserialize(str, Result);
end;

procedure NotifyServices_SetEnabled(enabled:boolean);
begin
   _enabled := enabled;
end;

procedure HandleCopydata(var Message: TMessage);
var
    cd: PCOPYDATASTRUCT;
    cmd: TServerAppCmd;
    str:string;
begin
    if not _enabled then
        exit;
    cd := PCOPYDATASTRUCT(Message.LParam);
    cmd := TServerAppCmd(Message.WParam);
    Message.result := 1;
    SetString(str, PWideChar(cd.lpData), cd.cbData div 2);
    case cmd of
        CmdPanic:
        begin
            if not Assigned(_OnPanic) then
                raise Exception.Create('_OnPanic must be set');
            _OnPanic(str);
        end;
        CmdPlaceConnection:
        begin
            if not Assigned(_OnPlaceConnection) then
                raise Exception.Create('_OnPlaceConnection must be set');
            _OnPlaceConnection(_deserializer.deserialize<TPlaceConnection>(str));
        end;
        CmdWorkStarted:
        begin
            if not Assigned(_OnWorkStarted) then
                raise Exception.Create('_OnWorkStarted must be set');
            _OnWorkStarted(str);
        end;
        CmdWorkComplete:
        begin
            if not Assigned(_OnWorkComplete) then
                raise Exception.Create('_OnWorkComplete must be set');
            _OnWorkComplete(_deserializer.deserialize<TWorkResultInfo>(str));
        end;
        CmdWarning:
        begin
            if not Assigned(_OnWarning) then
                raise Exception.Create('_OnWarning must be set');
            _OnWarning(str);
        end;
        CmdDelay:
        begin
            if not Assigned(_OnDelay) then
                raise Exception.Create('_OnDelay must be set');
            _OnDelay(_deserializer.deserialize<TDelayInfo>(str));
        end;
        CmdEndDelay:
        begin
            if not Assigned(_OnEndDelay) then
                raise Exception.Create('_OnEndDelay must be set');
            _OnEndDelay(str);
        end;
        CmdStatus:
        begin
            if not Assigned(_OnStatus) then
                raise Exception.Create('_OnStatus must be set');
            _OnStatus(str);
        end;
        CmdProductDataChanged:
        begin
            if not Assigned(_OnProductDataChanged) then
                raise Exception.Create('_OnProductDataChanged must be set');
            _OnProductDataChanged(StrToInt(str));
        end;
        CmdWriteConsole:
        begin
            if not Assigned(_OnWriteConsole) then
                raise Exception.Create('_OnWriteConsole must be set');
            _OnWriteConsole(str);
        end;
        
    else
        raise Exception.Create('wrong message: ' + IntToStr(Message.WParam));
    end;
end;

procedure SetOnPanic( AHandler : TStringHandler);
begin
    if Assigned(_OnPanic) then
        raise Exception.Create('_OnPanic already set');
    _OnPanic := AHandler;
end;
procedure SetOnPlaceConnection( AHandler : TPlaceConnectionHandler);
begin
    if Assigned(_OnPlaceConnection) then
        raise Exception.Create('_OnPlaceConnection already set');
    _OnPlaceConnection := AHandler;
end;
procedure SetOnWorkStarted( AHandler : TStringHandler);
begin
    if Assigned(_OnWorkStarted) then
        raise Exception.Create('_OnWorkStarted already set');
    _OnWorkStarted := AHandler;
end;
procedure SetOnWorkComplete( AHandler : TWorkResultInfoHandler);
begin
    if Assigned(_OnWorkComplete) then
        raise Exception.Create('_OnWorkComplete already set');
    _OnWorkComplete := AHandler;
end;
procedure SetOnWarning( AHandler : TStringHandler);
begin
    if Assigned(_OnWarning) then
        raise Exception.Create('_OnWarning already set');
    _OnWarning := AHandler;
end;
procedure SetOnDelay( AHandler : TDelayInfoHandler);
begin
    if Assigned(_OnDelay) then
        raise Exception.Create('_OnDelay already set');
    _OnDelay := AHandler;
end;
procedure SetOnEndDelay( AHandler : TStringHandler);
begin
    if Assigned(_OnEndDelay) then
        raise Exception.Create('_OnEndDelay already set');
    _OnEndDelay := AHandler;
end;
procedure SetOnStatus( AHandler : TStringHandler);
begin
    if Assigned(_OnStatus) then
        raise Exception.Create('_OnStatus already set');
    _OnStatus := AHandler;
end;
procedure SetOnProductDataChanged( AHandler : TInt64Handler);
begin
    if Assigned(_OnProductDataChanged) then
        raise Exception.Create('_OnProductDataChanged already set');
    _OnProductDataChanged := AHandler;
end;
procedure SetOnWriteConsole( AHandler : TStringHandler);
begin
    if Assigned(_OnWriteConsole) then
        raise Exception.Create('_OnWriteConsole already set');
    _OnWriteConsole := AHandler;
end;


initialization
    _enabled := false;

end.