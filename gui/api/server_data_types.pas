
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
        C2 : Double;
        PartyID : Int64;
        C1 : Double;
        C3 : Double;
        AbsErrorLimit3 : Double;
        AbsErrorLimit4 : Double;
        ScaleEnd : Double;
        Component : Integer;
        C4 : Double;
        AbsErrorLimit1 : Double;
        AbsErrorLimit2 : Double;
        CreatedAt : TDateTime;
        ProductType : Integer;
        VariationLimit3 : Double;
        ScaleBegin : Double;
        
    end;
    
    TGuiSettings = record
    public
        SoftVersionID : Word;
        ComportProducts : string;
        ComportHart : string;
        DurationBlowGasMinutes : Integer;
        DurationBlowAirMinutes : Integer;
        PauseReadPlaceMillis : Integer;
        SoftVersion : Byte;
        
    end;
    
    TCell = record
    public
        Detail : string;
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
        Column : string;
        Ok : Boolean;
        Place : Integer;
        Text : string;
        
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