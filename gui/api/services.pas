
unit services;

interface

uses superobject, server_data_types;

type 
    TLastPartySvc = class
    public
        class function Party:TParty;static;
        class function Products:TArray<TProduct>;static;
         
    end;

    TConfigSvc = class
    public
        class function GetConfig:TConfig;static;
        class function GetConfigToml:string;static;
        class procedure SetConfig(C:TConfig);static;
        class function SetConfigToml(param1:string):string;static;
        class function SetDefault:string;static;
        class procedure SetPlaceChecked(Place:Integer; Checked:Boolean);static;
         
    end;

    TRunnerSvc = class
    public
        class procedure Cancel;static;
        class procedure RunMainWork;static;
        class procedure RunReadVars;static;
        class procedure SkipDelay;static;
         
    end;

    TPartiesSvc = class
    public
        class procedure CreateNewParty(param1:string);static;
        class function NewPartyTemplate:string;static;
        class function PartiesOfYearMonth(Year:Integer; Month:Integer):TArray<TPartyCatalogue>;static;
        class function PartyProducts(param1:Int64):TArray<TProduct>;static;
        class function ReportParty(param1:Int64):TArray<TArray<string>>;static;
        class function YearsMonths:TArray<TYearMonth>;static;
         
    end;

    

implementation 

uses System.SysUtils, registry, winapi.windows, HttpRpcClient, SuperObjectHelp, Grijjy.Bson.Serialization;

function GetHttpServerAddr: string;
var
    key: TRegistry;
begin
    key := TRegistry.Create(KEY_READ);
    try
        if not key.OpenKey( 'daf\http', False) then
            raise Exception.Create('cant open daf\http');
        result := key.ReadString('addr');
    finally
        key.CloseKey;
        key.Free;
    end;
end;

 
class function TLastPartySvc.Party:TParty;
var
    req : ISuperobject;
begin
    req := SO;
    
    ThttpRpcClient.Call(GetHttpServerAddr + '/rpc', 'LastPartySvc.Party', req, Result); 
end;


class function TLastPartySvc.Products:TArray<TProduct>;
var
    req : ISuperobject;
begin
    req := SO;
    
    ThttpRpcClient.Call(GetHttpServerAddr + '/rpc', 'LastPartySvc.Products', req, Result); 
end;

 
class function TConfigSvc.GetConfig:TConfig;
var
    req : ISuperobject;
begin
    req := SO;
    
    ThttpRpcClient.Call(GetHttpServerAddr + '/rpc', 'ConfigSvc.GetConfig', req, Result); 
end;


class function TConfigSvc.GetConfigToml:string;
var
    req : ISuperobject;
begin
    req := SO;
    
    SuperObject_Get(ThttpRpcClient.GetResponse(GetHttpServerAddr + '/rpc', 'ConfigSvc.GetConfigToml', req), Result); 
end;


class procedure TConfigSvc.SetConfig(C:TConfig);
var
    req : ISuperobject;s:string;
begin
    req := SO;
    TgoBsonSerializer.serialize(C, s); req['C'] := SO(s); 
    ThttpRpcClient.GetResponse(GetHttpServerAddr + '/rpc', 'ConfigSvc.SetConfig', req); 
end;


class function TConfigSvc.SetConfigToml(param1:string):string;
var
    req : ISuperobject;
begin
    req := SA([]);
    req.AsArray.Add(param1); 
    SuperObject_Get(ThttpRpcClient.GetResponse(GetHttpServerAddr + '/rpc', 'ConfigSvc.SetConfigToml', req), Result); 
end;


class function TConfigSvc.SetDefault:string;
var
    req : ISuperobject;
begin
    req := SO;
    
    SuperObject_Get(ThttpRpcClient.GetResponse(GetHttpServerAddr + '/rpc', 'ConfigSvc.SetDefault', req), Result); 
end;


class procedure TConfigSvc.SetPlaceChecked(Place:Integer; Checked:Boolean);
var
    req : ISuperobject;s:string;
begin
    req := SO;
    SuperObject_SetField(req, 'Place', Place); SuperObject_SetField(req, 'Checked', Checked); 
    ThttpRpcClient.GetResponse(GetHttpServerAddr + '/rpc', 'ConfigSvc.SetPlaceChecked', req); 
end;

 
class procedure TRunnerSvc.Cancel;
var
    req : ISuperobject;
begin
    req := SO;
    
    ThttpRpcClient.GetResponse(GetHttpServerAddr + '/rpc', 'RunnerSvc.Cancel', req); 
end;


class procedure TRunnerSvc.RunMainWork;
var
    req : ISuperobject;
begin
    req := SO;
    
    ThttpRpcClient.GetResponse(GetHttpServerAddr + '/rpc', 'RunnerSvc.RunMainWork', req); 
end;


class procedure TRunnerSvc.RunReadVars;
var
    req : ISuperobject;
begin
    req := SO;
    
    ThttpRpcClient.GetResponse(GetHttpServerAddr + '/rpc', 'RunnerSvc.RunReadVars', req); 
end;


class procedure TRunnerSvc.SkipDelay;
var
    req : ISuperobject;
begin
    req := SO;
    
    ThttpRpcClient.GetResponse(GetHttpServerAddr + '/rpc', 'RunnerSvc.SkipDelay', req); 
end;

 
class procedure TPartiesSvc.CreateNewParty(param1:string);
var
    req : ISuperobject;
begin
    req := SA([]);
    req.AsArray.Add(param1); 
    ThttpRpcClient.GetResponse(GetHttpServerAddr + '/rpc', 'PartiesSvc.CreateNewParty', req); 
end;


class function TPartiesSvc.NewPartyTemplate:string;
var
    req : ISuperobject;
begin
    req := SO;
    
    SuperObject_Get(ThttpRpcClient.GetResponse(GetHttpServerAddr + '/rpc', 'PartiesSvc.NewPartyTemplate', req), Result); 
end;


class function TPartiesSvc.PartiesOfYearMonth(Year:Integer; Month:Integer):TArray<TPartyCatalogue>;
var
    req : ISuperobject;s:string;
begin
    req := SO;
    SuperObject_SetField(req, 'Year', Year); SuperObject_SetField(req, 'Month', Month); 
    ThttpRpcClient.Call(GetHttpServerAddr + '/rpc', 'PartiesSvc.PartiesOfYearMonth', req, Result); 
end;


class function TPartiesSvc.PartyProducts(param1:Int64):TArray<TProduct>;
var
    req : ISuperobject;
begin
    req := SA([]);
    req.AsArray.Add(param1); 
    ThttpRpcClient.Call(GetHttpServerAddr + '/rpc', 'PartiesSvc.PartyProducts', req, Result); 
end;


class function TPartiesSvc.ReportParty(param1:Int64):TArray<TArray<string>>;
var
    req : ISuperobject;
begin
    req := SA([]);
    req.AsArray.Add(param1); 
    ThttpRpcClient.Call(GetHttpServerAddr + '/rpc', 'PartiesSvc.ReportParty', req, Result); 
end;


class function TPartiesSvc.YearsMonths:TArray<TYearMonth>;
var
    req : ISuperobject;
begin
    req := SO;
    
    ThttpRpcClient.Call(GetHttpServerAddr + '/rpc', 'PartiesSvc.YearsMonths', req, Result); 
end;

 
end.