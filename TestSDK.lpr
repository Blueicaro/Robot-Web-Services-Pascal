program TestSDK;

uses
  SysUtils,
  Classes,
  RobotWareService,
  abbwstypes,
  rapid,
  robotwaredata, io;

var
  R: TRWS;
  Tasks: TTaskList;
  I: integer;
  Cadena: string;
  It: TTaskItem;
  SystemModule, ProgramModule: TStringList;
begin
  R := TRWS.Create('http://localhost');
  SystemModule := TStringList.Create;
  ProgramModule := TStringList.Create;
  try
    try
      WriteLn('Operation mode: ' + R.Controller.getOperationMode);
      Writeln('Control State: ' + R.Controller.getControllerState);
      Writeln('Identity: ' + R.Controller.getIdentity);
      Writeln('Virtual: ' + BoolToStr(R.Controller.isVirtualController, True));
      //Writeln('Fecha y hora: ' + R.Controller.GetTime);
      //Writeln('TimeZone: ' + R.Controller.getTimezone);
      Tasks := R.Rapid.GetTasks;
      for I := 0 to Tasks.Count - 1 do
      begin
        WriteLn(Tasks[I].GetName);
        Writeln(Tasks[i].Properties.TaskType);
        Tasks[I].GetModuleNames(ProgramModule, SystemModule);
        WriteLn(ProgramModule.text);
        WriteLn(SystemModule.text);
      end;



    except
      on E: Exception do
        Writeln(e.Message);
    end;

  finally
    FreeAndNil(SystemModule);
    FreeAndNil(ProgramModule);
    FreeAndNil(Tasks);
    FreeAndNil(R);
  end;
  ReadLn;
end.
