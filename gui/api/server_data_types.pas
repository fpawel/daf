
unit server_data_types;

interface

uses Grijjy.Bson, Grijjy.Bson.Serialization;

type
    
    TProduct = record
    public
        Place : Integer;
        Addr : Byte;
        Checked : Boolean;
        ProductID : Int64;
        Serial : Integer;
        
    end;
    
    TParty = record
    public
        C4 : Double;
        Scale : Double;
        AbsErrRng : Double;
        ProductType : Integer;
        C2 : Double;
        C3 : Double;
        Thr1Test : Double;
        CreatedAt : TDateTime;
        Component : Integer;
        Thr2Prod : Double;
        Thr2Test : Double;
        Thr1Prod : Double;
        PartyID : Int64;
        C1 : Double;
        AbsErrLim : Double;
        RelErrLim : Double;
        
    end;
    
    TGuiSettings = record
    public
        SoftVersion : Byte;
        SoftVersionID : Word;
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