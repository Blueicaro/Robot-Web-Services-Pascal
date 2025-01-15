program TestSDK;

uses
  SysUtils,
  RobotWareService,
  abbwstypes,
  rapid,
  robotwaredata;

var
  R: TRWS;
  Tasks: TTaskList;
  I: integer;
  Cadena: string;
begin
  R := TRWS.Create('http://localhost');
  try
    try
      WriteLn('Operation mode: ' + R.Controller.getOperationMode);
      Writeln('Control State: ' + R.Controller.getControllerState);
      Writeln('Identity: ' + R.Controller.getIdentity);
      //Writeln('Virtual: ' + BoolToStr(R.Controller.isVirtualController, True));
      //Writeln('Fecha y hora: ' + R.Controller.GetTime);
      //Writeln('TimeZone: ' + R.Controller.getTimezone);
      Tasks := R.Rapid.GetTasks;
      for I := 0 to Tasks.Count - 1 do
      begin
        WriteLn(Tasks[I].GetName);
      end;
    except
      on E: Exception do
        Writeln(e.Message);
    end;

  finally
    FreeAndNil(Tasks);
    FreeAndNil(R);
  end;
  ReadLn;
end.
