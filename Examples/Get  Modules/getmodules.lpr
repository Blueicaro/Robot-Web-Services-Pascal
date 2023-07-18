program GetModules;

uses
  SysUtils, Classes,
  AbbWebServices;

var
  Robot: TAbbWebServices;
  TaskList, ListaModulos: TStringList;
  I, X: Integer;
begin
  try
    Robot := TAbbWebServices.Create('https://localhost:80');
    TaskList := TStringList.Create;
    Robot.RobotWare.GetTasksList(TaskList);
    For I := 0 To TaskList.Count-1 do
    Begin
      Writeln ('Modules in task:'+TaskList[I]);
      ListaModulos := TStringList.Create;
      Robot.RobotWare.GetModulesList(TaskList[i],ListaModulos);
      For X := 0 To ListaModulos.Count-1 do
      begin
        WriteLn(#09+ListaModulos[x]);
      end;
      FreeAndNil(ListaModulos);
    end;
  finally
    FreeAndNil(TaskList);
    FreeAndNil(Robot);
    ReadLn();
  end;
end.
