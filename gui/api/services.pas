
unit services;

interface

uses superobject, server_data_types;

type 
    TPartySvc = class
    public
        class function AddNewProduct:TArray<TProduct>;static;
        class function DeleteProduct(param1:Int64):TArray<TProduct>;static;
        class procedure NewParty;static;
        class function Party:TParty;static;
        class function Products:TArray<TProduct>;static;
        class procedure SetProductAddr(Place:Integer; Addr:Byte);static;
        class procedure SetProductSerial(ProductID:Int64; SerialStr:string);static;
         
    end;

    TConfigSvc = class
    public
        class function GetConfig:TGuiSettings;static;
        class function GetConfigToml:string;static;
        class procedure SetConfig(C:TGuiSettings);static;
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

    TProductsSvc = class
    public
        class function PartyProducts(param1:Int64):TArray<TProduct>;static;
        class function ProductPassport(param1:Int64):TProductPassport;static;
        class function ProductsOfYearMonth(Year:Integer; Month:Integer):TArray<TProductInfo>;static;
        class function YearsMonths:TArray<TYearMonth>;static;
         
    end;

    

function GetHttpServerAddr: string;

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

 
class function TPartySvc.AddNewProduct:TArray<TProduct>;
var
    req : ISuperobject;
begin
    req := SO;
    
    ThttpRpcClient.Call(GetHttpServerAddr + '/rpc', 'PartySvc.AddNewProduct', req, Result); 
end;


class function TPartySvc.DeleteProduct(param1:Int64):TArray<TProduct>;
var
    req : ISuperobject;
begin
    req := SA([]);
    req.AsArray.Add(param1); 
    ThttpRpcClient.Call(GetHttpServerAddr + '/rpc', 'PartySvc.DeleteProduct', req, Result); 
end;


class procedure TPartySvc.NewParty;
var
    req : ISuperobject;
begin
    req := SO;
    
    ThttpRpcClient.GetResponse(GetHttpServerAddr + '/rpc', 'PartySvc.NewParty', req); 
end;


class function TPartySvc.Party:TParty;
var
    req : ISuperobject;
begin
    req := SO;
    
    ThttpRpcClient.Call(GetHttpServerAddr + '/rpc', 'PartySvc.Party', req, Result); 
end;


class function TPartySvc.Products:TArray<TProduct>;
var
    req : ISuperobject;
begin
    req := SO;
    
    ThttpRpcClient.Call(GetHttpServerAddr + '/rpc', 'PartySvc.Products', req, Result); 
end;


class procedure TPartySvc.SetProductAddr(Place:Integer; Addr:Byte);
var
    req : ISuperobject;s:string;
begin
    req := SO;
    SuperObject_SetField(req, 'Place', Place); SuperObject_SetField(req, 'Addr', Addr); 
    ThttpRpcClient.GetResponse(GetHttpServerAddr + '/rpc', 'PartySvc.SetProductAddr', req); 
end;


class procedure TPartySvc.SetProductSerial(ProductID:Int64; SerialStr:string);
var
    req : ISuperobject;s:string;
begin
    req := SO;
    SuperObject_SetField(req, 'ProductID', ProductID); SuperObject_SetField(req, 'SerialStr', SerialStr); 
    ThttpRpcClient.GetResponse(GetHttpServerAddr + '/rpc', 'PartySvc.SetProductSerial', req); 
end;

 
class function TConfigSvc.GetConfig:TGuiSettings;
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


class procedure TConfigSvc.SetConfig(C:TGuiSettings);
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

 
class function TProductsSvc.PartyProducts(param1:Int64):TArray<TProduct>;
var
    req : ISuperobject;
begin
    req := SA([]);
    req.AsArray.Add(param1); 
    ThttpRpcClient.Call(GetHttpServerAddr + '/rpc', 'ProductsSvc.PartyProducts', req, Result); 
end;


class function TProductsSvc.ProductPassport(param1:Int64):TProductPassport;
var
    req : ISuperobject;
begin
    req := SA([]);
    req.AsArray.Add(param1); 
    ThttpRpcClient.Call(GetHttpServerAddr + '/rpc', 'ProductsSvc.ProductPassport', req, Result); 
end;


class function TProductsSvc.ProductsOfYearMonth(Year:Integer; Month:Integer):TArray<TProductInfo>;
var
    req : ISuperobject;s:string;
begin
    req := SO;
    SuperObject_SetField(req, 'Year', Year); SuperObject_SetField(req, 'Month', Month); 
    ThttpRpcClient.Call(GetHttpServerAddr + '/rpc', 'ProductsSvc.ProductsOfYearMonth', req, Result); 
end;


class function TProductsSvc.YearsMonths:TArray<TYearMonth>;
var
    req : ISuperobject;
begin
    req := SO;
    
    ThttpRpcClient.Call(GetHttpServerAddr + '/rpc', 'ProductsSvc.YearsMonths', req, Result); 
end;

 
end.