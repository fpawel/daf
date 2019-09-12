
unit server_data_types;

interface

uses Grijjy.Bson, Grijjy.Bson.Serialization;

type
    
    TProduct = record
    public
        ProductID : Int64;
        Serial : Integer;
        Place : Integer;
        Addr : Byte;
        Checked : Boolean;
        
    end;
    
    TParty = record
    public
        Component : Integer;
        VariationLimit3 : Double;
        CreatedAt : TDateTime;
        C4 : Double;
        ProductType : Integer;
        ScaleEnd : Double;
        C1 : Double;
        C2 : Double;
        C3 : Double;
        AbsErrorLimit1 : Double;
        AbsErrorLimit2 : Double;
        AbsErrorLimit4 : Double;
        ScaleBegin : Double;
        AbsErrorLimit3 : Double;
        PartyID : Int64;
        
    end;
    
    TConfig = record
    public
        ReadByteTimeoutMillis : Integer;
        MaxAttemptsRead : Integer;
        ReadTimeoutMillis : Integer;
        
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
        Temperature : Double;
        Comm : TComm;
        ComportProducts : string;
        DurationBlowGasMinutes : TArray<Integer>;
        DurationBlowOutMinutes : Integer;
        SoftVersion : Byte;
        ComportHart : string;
        PauseReadPlaceMillis : Integer;
        SoftVersionID : Word;
        
    end;
    
    TCell = record
    public
        Text : string;
        Alignment : Integer;
        Color : string;
        
    end;
    
    TProductPassport = record
    public
        Serial : Integer;
        CreatedAt : TDateTime;
        T1 : TArray<TArray<TCell>>;
        T2 : TArray<TArray<TCell>>;
        PartyID : Int64;
        
    end;
    
    TProductInfo = record
    public
        ProductID : Int64;
        PartyID : Int64;
        Serial : Integer;
        Day : Integer;
        Hour : Integer;
        Minute : Integer;
        
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
        Work : string;
        Result : Integer;
        Message : string;
        
    end;
    
    TDelayInfo = record
    public
        What : string;
        TotalSeconds : Integer;
        ElapsedSeconds : Integer;
        
    end;
    

implementation

end.