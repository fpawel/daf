
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
        ScaleEnd : Double;
        C3 : Double;
        C4 : Double;
        AbsErrorLimit1 : Double;
        AbsErrorLimit4 : Double;
        C1 : Double;
        C2 : Double;
        AbsErrorLimit2 : Double;
        VariationLimit3 : Double;
        PartyID : Int64;
        CreatedAt : TDateTime;
        ProductType : Integer;
        Component : Integer;
        ScaleBegin : Double;
        AbsErrorLimit3 : Double;
        
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
    
    TGuiSettings = record
    public
        SoftVersion : Byte;
        Temperature : Double;
        ComportProducts : string;
        ComportHart : string;
        DurationBlowGasMinutes : TArray<Integer>;
        DurationBlowOutMinutes : Integer;
        PauseReadPlaceMillis : Integer;
        SoftVersionID : Word;
        Comm : TComm;
        
    end;
    
    TCell = record
    public
        Color : string;
        Text : string;
        Alignment : Integer;
        
    end;
    
    TProductPassport = record
    public
        T2 : TArray<TArray<TCell>>;
        PartyID : Int64;
        Serial : Integer;
        CreatedAt : TDateTime;
        T1 : TArray<TArray<TCell>>;
        
    end;
    
    TProductInfo = record
    public
        Hour : Integer;
        Minute : Integer;
        ProductID : Int64;
        PartyID : Int64;
        Serial : Integer;
        Day : Integer;
        
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
        ElapsedSeconds : Integer;
        What : string;
        TotalSeconds : Integer;
        
    end;
    

implementation

end.