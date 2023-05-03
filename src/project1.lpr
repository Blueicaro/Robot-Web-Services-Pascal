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
begin

  Robot := TAbbWebServices.Create('https://localhost:80');
  try
    try
      Robot.RobotWare.RequestMastership;
    except
      on e: Exception do
        WriteLn(e.Message)
    end;
  finally
    FreeAndNil(Robot);
    ReadLn;
  end;

end.
