
unit server_data_types;

interface

uses Grijjy.Bson, Grijjy.Bson.Serialization;

type
    
    TProduct = record
    public
        Serial : Integer;
        Place : Integer;
        Addr : Byte;
        Checked : Boolean;
        ProductID : Int64;
        
    end;
    
    TParty = record
    public
        Scale : Double;
        Thr1Prod : Double;
        PartyID : Int64;
        ProductType : Integer;
        C1 : Double;
        C3 : Double;
        AbsErrRng : Double;
        C2 : Double;
        C4 : Double;
        AbsErrLim : Double;
        RelErrLim : Double;
        Component : Integer;
        Thr2Prod : Double;
        Thr1Test : Double;
        Thr2Test : Double;
        CreatedAt : TDateTime;
        
    end;
    
    TGuiSettings = record
    public
        PauseReadPlaceMillis : Integer;
        SoftVersion : Byte;
        SoftVersionID : Word;
        ComportProducts : string;
        ComportHart : string;
        DurationBlowGasMinutes : Integer;
        DurationBlowAirMinutes : Integer;
        
    end;
    
    TProductPassport = record
    public
        T1 : TArray<TArray<string>>;
        T2 : TArray<TArray<string>>;
        PartyID : Int64;
        Serial : Integer;
        CreatedAt : TDateTime;
        
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