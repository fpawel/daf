unit app;

interface
uses  inifiles, server_data_types;

var
    AppSets :  TInifile;

implementation

uses registry, winapi.windows, sysutils, stringutils;



initialization

AppSets := TIniFile.Create(ChangeFileExt(paramstr(0), '.ini'));


end.

