program TestRw6;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}
  cthreads,
  {$ENDIF}
  Classes,
  { you can add units after this }
  SysUtils,
  {$IFDEF abbdebug}
    LazLogger,
  {$ENDIF}
  abbconexion,
  rw6robotwareservices,
  rw6abbwstypes;

var
  Modo: TOpMode;
  Tarea, Modulo, NombreVariable, Valor: string;
  Re, I: integer;
  R: TRobotConnection;
  RobotWareService: TRw6RobotWareServices;



begin
  R := TRobotConnection.Create('http://192.168.125.1');
  RobotWareService := TRw6RobotWareServices.Create(R);
  Modo := RobotWareService.GetOperationMode;
  if Modo.opmode = opAUTO then
  begin
    WriteLn('auto');
  end;

  Readln();
  FreeAndNil(RobotWareService);
  FreeAndNil(R);

end.
