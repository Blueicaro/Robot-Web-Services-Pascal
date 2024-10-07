program getinformation;

uses
  Classes,
  rw6robotwareservices,
  rw6controllerservice,
  rw6abbwstypes,
  abbconexion;

var

  info: TRw6RwServiceList;
  Robot: TRobotConnection;
  RobotService: TRw6RobotWareServices;
  Datos: TSysOptionList;

begin
  //Open conexión with robotcontroler

  try
    Datos := TSysOptionList.Create;
    Robot := TRobotConnection.Create('http://192.168.125.1/');
    RobotService := TRw6RobotWareServices.Create(Robot);

    RobotService.GetSystemOptions(Datos);
  finally
    RobotService.Free;
    Robot.Free;
  end;

end.
