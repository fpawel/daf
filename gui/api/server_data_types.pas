
unit server_data_types;

interface

uses Grijjy.Bson, Grijjy.Bson.Serialization;

type
    
    TParty = record
    public
        C1 : Double;
        C3 : Double;
        Scale : Double;
        Thr1Prod : Double;
        Thr2Test : Double;
        Component : Integer;
        RelErrLim : Double;
        Thr2Prod : Double;
        PartyID : Int64;
        AbsErrLim : Double;
        C2 : Double;
        C4 : Double;
        AbsErrRng : Double;
        Thr1Test : Double;
        ProductType : Integer;
        CreatedAt : TDateTime;
        
    end;
    
    TProduct = record
    public
        Addr : Byte;
        Checked : Boolean;
        ProductID : Int64;
        Serial : Integer;
        Place : Integer;
        
    end;
    
    TPlace = record
    public
        Addr : Byte;
        Checked : Boolean;
        
    end;
    
    TConfig = record
    public
        DurationBlowAirMinutes : Integer;
        PauseReadPlaceMillis : Integer;
        Network : TArray<TPlace>;
        ComportProducts : string;
        ComportHart : string;
        DurationBlowGasMinutes : Integer;
        
    end;
    
    TPartyCatalogue = record
    public
        PartyID : Int64;
        ProductType : string;
        Last : Boolean;
        Day : Integer;
        Hour : Integer;
        Minute : Integer;
        
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