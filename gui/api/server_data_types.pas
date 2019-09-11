
unit server_data_types;

interface

uses Grijjy.Bson, Grijjy.Bson.Serialization;

type
    
    TProduct = record
    public
        Addr : Byte;
        Checked : Boolean;
        ProductID : Int64;
        Serial : Integer;
        Place : Integer;
        
    end;
    
    TParty = record
    public
        C3 : Double;
        AbsErrorLimit3 : Double;
        ScaleBegin : Double;
        C1 : Double;
        AbsErrorLimit1 : Double;
        AbsErrorLimit4 : Double;
        VariationLimit3 : Double;
        CreatedAt : TDateTime;
        Component : Integer;
        C4 : Double;
        C2 : Double;
        AbsErrorLimit2 : Double;
        PartyID : Int64;
        ProductType : Integer;
        ScaleEnd : Double;
        
    end;
    
    TConfig = record
    public
        ReadTimeoutMillis : Integer;
        ReadByteTimeoutMillis : Integer;
        MaxAttemptsRead : Integer;
        
    end;
    
    TComm = record
    public
        Daf : TConfig;
        Gas : TConfig;
        EN6408 : TConfig;
        Hart : TConfig;
        
    end;
    
    TAppConfig = record
    public
        ComportProducts : string;
        ComportHart : string;
        SoftVersion : Byte;
        DurationBlowGasMinutes : TArray<Integer>;
        DurationBlowOutMinutes : Integer;
        PauseReadPlaceMillis : Integer;
        SoftVersionID : Word;
        Temperature : Double;
        Comm : TComm;
        
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
        Month : Integer;
        Year : Integer;
        
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
        Work : string;
        Result : Integer;
        Message : string;
        
    end;
    
    TDelayInfo = record
    public
        TotalSeconds : Integer;
        ElapsedSeconds : Integer;
        What : string;
        
    end;
    

implementation

end.