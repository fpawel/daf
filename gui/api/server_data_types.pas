
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
        Scale : Double;
        Thr1Prod : Double;
        CreatedAt : TDateTime;
        ProductType : Integer;
        C1 : Double;
        AbsErrLim : Double;
        RelErrLim : Double;
        Thr2Prod : Double;
        Thr2Test : Double;
        C2 : Double;
        C4 : Double;
        AbsErrRng : Double;
        Thr1Test : Double;
        C3 : Double;
        PartyID : Int64;
        
    end;
    
    TGuiSettings = record
    public
        DurationBlowAirMinutes : Integer;
        PauseReadPlaceMillis : Integer;
        ComportProducts : string;
        ComportHart : string;
        DurationBlowGasMinutes : Integer;
        
    end;
    
    TProductPassport = record
    public
        T1 : TArray<TArray<string>>;
        T2 : TArray<TArray<string>>;
        
    end;
    
    TProductInfo = record
    public
        Day : Integer;
        Hour : Integer;
        Minute : Integer;
        ProductID : Int64;
        PartyID : Int64;
        Serial : Integer;
        
    end;
    
    TYearMonth = record
    public
        Year : Integer;
        Month : Integer;
        
    end;
    
    TProductValue = record
    public
        Place : Integer;
        Column : string;
        Value : string;
        
    end;
    
    TProductError = record
    public
        Place : Integer;
        Message : string;
        
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