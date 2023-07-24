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
  Lista: TStringList;
begin

  try
    Robot := TAbbWebServices.Create('https://localhost:80');
    try
      Lista := TStringList.Create;

      Robot.RobotWare.GetNetWorksList(Lista);
      WriteLn(Lista.text);
      Lista.clear;

      Robot.RobotWare.GetDevicesList(Lista);
      Writeln(Lista.text);
      Lista.Clear         ;
      Robot.RobotWare.GetSignalsList(Lista);
      Writeln(Lista.Text);
    except
      on e: Exception do
        WriteLn(e.Message)
    end;
  finally
    FreeAndNil(Robot);
    FreeAndNil(Lista);
    ReadLn;
  end;

end.
