program GetModules;

uses
  SysUtils,
  Classes,
  AbbWebServices;

var
  Robot: TAbbWebServices;
  TaskList, ListaModulos, contenido: TStringList;
  I, X: integer;
begin
  try
    Robot := TAbbWebServices.Create('https://localhost:80');
    TaskList := TStringList.Create;
    Robot.RobotWare.GetTasksList(TaskList);
    for I := 0 to TaskList.Count - 1 do
    begin
      Writeln('Modules in task:' + TaskList[I]);
      ListaModulos := TStringList.Create;
      Robot.RobotWare.GetModulesList(TaskList[i], ListaModulos);
      for X := 0 to ListaModulos.Count - 1 do
      begin
        WriteLn(#09 + ListaModulos[x]);
      end;
      FreeAndNil(ListaModulos);
    end;
  finally
    FreeAndNil(TaskList);
    FreeAndNil(Robot);
    ReadLn();
  end;
end.
