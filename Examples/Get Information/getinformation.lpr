program getinformation;

uses
  Classes,
  AbbWebServices,
  abbwstypes;

var
  Robot: TAbbWebServices;
  RobotType, Licencia: string;
  ListOptions, ListProducts: TStringList;
  I: integer;
  Info: TSysSystemInfo;
begin
  //Open conexión with robotcontroler
  //in this case we connect to virtual controller
  Robot := TAbbWebServices.Create('https://localhost:80');
  Licencia := Robot.RobotWare.GetSystemLicence;
  RobotType := Robot.RobotWare.GetRobotType;
  ListOptions := TStringList.Create;
  Robot.RobotWare.GetSystemOptions(ListOptions);
  ListProducts := TStringList.Create;
  Robot.RobotWare.GetSystemProducts(ListProducts);
  Info := Robot.RobotWare.GetSystemInfo;
  Writeln('Robot type: ' + RobotType);
  Writeln('License: ' + Licencia);
  Writeln('Products:');
  for I := 0 to ListProducts.Count - 1 do
  begin
    Writeln(#09 + ListProducts[I]);
  end;
  WriteLn('Opciones: ');
  for I := 0 to ListOptions.Count - 1 do
  begin
    Writeln(#09 + ListOptions[i]);
  end;
  Writeln('System Information:');
  with Info do
  begin
    Writeln(#09 + 'Title: ' + Title);
    Writeln(#09 + 'Description: ' + Description);
    Writeln(#09 + 'Type: ' + TypeOs);
    Writeln(#09 + 'RobotWare name: ' + RwVersionName);
    Writeln(#09 + 'RobotWare Version: ' + RwVersion);
    Writeln(#09 + 'RobotWare SubVersion: ' + SubRevision);
    Writeln(#09 + 'RobotWare Build: ' + Build);
    Writeln(#09 + 'RobotWare BuildTag: ' + BuildTag);
    Writeln(#09 + 'RobotWare Revision: ' + Revision);
    Writeln(#09+ 'Robot Api Compatible version: '+RobapiCompatibilityRevison);
    Writeln(#09+ 'SysId: '+SysId);
    Writeln(#09+ 'Start Time: '+StartTm);
  end;
  Readln;
  ListOptions.Free;
  ListProducts.Free;
  Robot.Free;

end.
