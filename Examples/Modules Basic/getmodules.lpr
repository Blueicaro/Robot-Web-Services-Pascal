program GetModules;

uses
  SysUtils,
  Classes,
  abbconexion,
  rw6abbwstypes,
  rw6robotwareservices;

var
  I, X: integer;
  Robot: TRobotConnection;
  RobotWareServices: TRw6RobotWareServices;
  TaskList: TRw6TaskList;
  Modules: TRw6ModuleInfoList;
begin
  try
    Robot := TRobotConnection.Create('http://192.168.125.1');
    //Robot := TRobotConnection.Create('http://Localhost');

    RobotWareServices := TRw6RobotWareServices.Create(Robot);
    TaskList := TRw6TaskList.Create;
    RobotWareServices.GetTaskList(TaskList);

    for I := 0 to TaskList.Count - 1 do
    begin
      Writeln('Modules in task:' + TaskList[I].Name);
      Modules:=TRw6ModuleInfoList.Create;
      RobotWareServices.GetRapidModules(Modules,TaskList[I].Name);
      For X := 0 To Modules.Count-1 do
      begin
        Writeln (#09+Modules[x].Name);
      end;
      FreeAndNil(Modules);
    end;
  finally
    FreeAndNil(TaskList);
    FreeAndNil(Robot);
    ReadLn();
  end;
end.
