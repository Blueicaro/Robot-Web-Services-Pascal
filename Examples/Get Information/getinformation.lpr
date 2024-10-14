program getinformation;

uses
  Classes,
  SysUtils,
  rw6robotwareservices,
  rw6controllerservice,
  rw6abbwstypes,
  abbconexion;

var

  Robot: TRobotConnection;
  RobotService: TRw6RobotWareServices;
  Datos: TSysOptionList;
  I: integer;
  Robots: TRobotTypeList;

begin
  //Open conexión with robotcontroler

  try
    Datos := TSysOptionList.Create;
    Robots := TRobotTypeList.Create;
    Robot := TRobotConnection.Create('http://192.168.125.1');
    //Uncoment to use with RobotStudio
    //Robot := TRobotConnection.Create('http:localhost);
    RobotService := TRw6RobotWareServices.Create(Robot);
    RobotService.GetSystemOptions(Datos);
    Writeln('Robotware Options');
    for I := 0 to datos.Count - 1 do
    begin
      WriteLn(#09+Datos[I].option);
    end;
    RobotService.GetRobotType(Robots);
    Writeln ('Robots Type');
    For I := 0 to Robots.Count-1 do
    begin
      WriteLn(#09+Robots[I].robot_type);
    end;
  finally
    FreeAndNil(Robots);
    FreeAndNil(Datos);
    RobotService.Free;
    Robot.Free;
  end;

  WriteLn('Press any key to continue');
  ReadLn;
end.
