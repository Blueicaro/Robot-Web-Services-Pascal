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
    Robot := TAbbWebServices.Create('https://localhost:80/ctrl');
    try
      Lista := TStringList.Create;

      //Robot.RobotWare.GetNetWorksList(Lista);
      //WriteLn(Lista.Text);
      Lista.Clear;
      Robot.Controller.GetListOfServices(Lista);
      Writeln(Lista.Text);
      Lista.Clear;
      WriteLn('Pulsa enter para continuar');

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
