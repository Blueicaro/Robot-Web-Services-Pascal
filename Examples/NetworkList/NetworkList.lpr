program NetworkList;

uses
  Classes,
  SysUtils,
  rw6abbwstypes,
  abbconexion,
  rw6robotwareservices;
  //rw6controllerservice,

var

  Robot: TRobotConnection;
  Datos: TIoNetWorkList;
  RobotService: TRw6RobotWareServices;
  I: integer;

begin
  //Open conexión with robotcontroler

  try
    Datos := TIoNetWorkList.Create;
    //  Robots := TRobotTypeList.Create;
    Robot := TRobotConnection.Create('http://192.168.125.1');
    //  //Uncoment to use with RobotStudio
    //  //Robot := TRobotConnection.Create('http:localhost);
    RobotService := TRw6RobotWareServices.Create(Robot);
    RobotService.GetIONetworks(Datos);
    For   I:=0 TO Datos.Count-1 do
    begin
      WriteLn(Datos[I].Name);
    end;
  finally
    FreeAndNil(Datos);
    FreeAndNil(Robot);
  end;
end.
