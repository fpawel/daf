
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
        C2 : Double;
        AbsErrRng : Double;
        Thr1Prod : Double;
        Thr1Test : Double;
        ProductType : Integer;
        Scale : Double;
        AbsErrLim : Double;
        RelErrLim : Double;
        Thr2Prod : Double;
        PartyID : Int64;
        C4 : Double;
        Thr2Test : Double;
        C1 : Double;
        C3 : Double;
        CreatedAt : TDateTime;
        
    end;
    
    TGuiSettings = record
    public
        ComportProducts : string;
        ComportHart : string;
        DurationBlowGasMinutes : Integer;
        DurationBlowAirMinutes : Integer;
        PauseReadPlaceMillis : Integer;
        
    end;
    
    TProductPassport = record
    public
        T1 : TArray<TArray<string>>;
        T2 : TArray<TArray<string>>;
        
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
    
    TProductValue = record
    public
        Place : Integer;
        Column : string;
        Value : Double;
        
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