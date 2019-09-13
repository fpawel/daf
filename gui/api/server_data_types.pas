
unit server_data_types;

interface

uses Grijjy.Bson, Grijjy.Bson.Serialization;

type
    
    TProduct = record
    public
        Checked : Boolean;
        ProductID : Int64;
        Serial : Integer;
        Place : Integer;
        Addr : Byte;
        
    end;
    
    TParty = record
    public
        AbsErrorLimit1 : Double;
        AbsErrorLimit3 : Double;
        ProductType : Integer;
        ScaleBegin : Double;
        ScaleEnd : Double;
        AbsErrorLimit4 : Double;
        CreatedAt : TDateTime;
        Component : Integer;
        PartyID : Int64;
        C1 : Double;
        C3 : Double;
        C4 : Double;
        AbsErrorLimit2 : Double;
        VariationLimit3 : Double;
        C2 : Double;
        
    end;
    
    TConfig = record
    public
        ReadTimeoutMillis : Integer;
        ReadByteTimeoutMillis : Integer;
        MaxAttemptsRead : Integer;
        
    end;
    
    TComm = record
    public
        Log : Boolean;
        Daf : TConfig;
        Gas : TConfig;
        EN6408 : TConfig;
        Hart : TConfig;
        
    end;
    
    TAppConfig = record
    public
        SoftVersion : Byte;
        SoftVersionID : Word;
        Comm : TComm;
        ComportProducts : string;
        ComportHart : string;
        DurationBlowGasMinutes : TArray<Integer>;
        DurationBlowOutMinutes : Integer;
        PauseReadPlaceMillis : Integer;
        Temperature : Double;
        
    end;
    
    TCell = record
    public
        Text : string;
        Alignment : Integer;
        Color : string;
        
    end;
    
    TProductPassport = record
    public
        T1 : TArray<TArray<TCell>>;
        T2 : TArray<TArray<TCell>>;
        PartyID : Int64;
        Serial : Integer;
        CreatedAt : TDateTime;
        
    end;
    
    TProductInfo = record
    public
        Serial : Integer;
        Day : Integer;
        Hour : Integer;
        Minute : Integer;
        ProductID : Int64;
        PartyID : Int64;
        
    end;
    
    TYearMonth = record
    public
        Year : Integer;
        Month : Integer;
        
    end;
    
    TPlaceConnection = record
    public
        Place : Integer;
        Text : string;
        Column : string;
        Ok : Boolean;
        
    end;
    
    TWorkResultInfo = record
    public
        Result : Integer;
        Message : string;
        Work : string;
        
    end;
    
    TDelayInfo = record
    public
        TotalSeconds : Integer;
        ElapsedSeconds : Integer;
        What : string;
        
    end;
    

implementation

end.