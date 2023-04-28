program project1;

{$mode objfpc}{$H+}

uses
 {$IFDEF UNIX}
  cthreads,
              {$ENDIF}
  Classes { you can add units after this },
  SysUtils,
  AbbWebServices;

var
  Robot: TAbbWebServices;
  L: TStringList;
  X: Integer;
begin

  Robot := TAbbWebServices.Create('https://localhost:80');
  try
    try
      L := TStringList.Create;
      Robot.RobotWare.GetListTasks(L);
      For X := 0 TO L.Count-1 do
      begin
        Writeln (L[x]);
      end;
    except
      on E: Exception do
        Writeln(e.Message);
    end;
  finally
    FreeAndNil(L);
    FreeAndNil(Robot);
    ReadLn;
  end;

end.
