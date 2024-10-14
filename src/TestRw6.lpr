program TestRw6;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}
  cthreads,
  {$ENDIF}
  Classes,
  { you can add units after this }
  SysUtils,
  {$IFDEF abbdebug}
    LazLogger,
  {$ENDIF}
  abbconexion,
  abbwstypes;

var
  R: TRobotConnection;

begin
  //R := TRobotConnection.Create('http://192.168.125.1');
  try
    try
      R := TRobotConnection.Create('https://localhost:80');
    except
      On E: Exception do
        writeln(E.Message)
    end;
  finally
    FreeAndNil(R)
  end;
  Readln();
end.
