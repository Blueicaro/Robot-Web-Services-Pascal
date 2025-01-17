program TestSDK;

uses
  SysUtils,
  Classes,
  RobotWareService,
  abbwstypes,
  rapid,
  robotwaredata,
  io;

var
  R: TRWS;
  Lista: TTaskList;
  Propiedades: TTaksProperties;
  I: Integer;
begin

  try
    try
      R := TRWS.Create('https://localhost:80');
      WriteLn('Operation mode: ' + R.Controller.getOperationMode);
      Writeln('IsVirtual: ' + BoolToStr(R.Controller.isVirtualController, True));
      Writeln('Controler state: ' + R.Controller.GetControllerState);
      WriteLn('Controller Identity: ' + R.Controller.GetIdentity);
      Writeln('Time: '+R.Controller.GetTime);
      // R.Controller.restartController(rmRestart);
      Lista := R.Rapid.GetTasks;
      For I := 0 To Lista.Count-1 do
      begin
        Propiedades := Lista[I].Properties;
      end;
    except
      on E: Exception do
      begin
        Writeln(e.Message);
      end;
    end;
  finally
    FreeAndNil(Lista);
    FreeAndNil(R);
    WriteLn('Pulsa enter para terminar');
    ReadLn;
  end;

end.
