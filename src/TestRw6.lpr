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
  Rw6WebServices,
  rw6abbwstypes,
  URIParser,
  rw6fileservices,
  rw6controllerservice,
  rw6userservices;

var
  R: TRw6WebServices;
  Modo: TOpMode;
  Tarea, Modulo, NombreVariable, Valor: string;
  Re, I: integer;



begin
 // R := TRw6WebServices.Create('http://localhost');
  R := TRw6WebServices.Create('http://192.168.125.1');
  R.Connect;
  for I := 0 to 100 do
  begin
    Modo := R.RobotWareServices.GetOperationMode;
    if Modo.opmode = opAUTO then
      WriteLn('auto');
    Tarea := 'T_ROB1';
    Modulo := 'Datos';
    NombreVariable := 'M1_C03';
    //Valor := '[0,0,60,200,"Cordón 3",3,TRUE,0.01,200,0,0,0,FALSE]';
    Valor := '[6,0,0,0,"Cordón 3",3,TRUE,0.01,200,0,0,0,FALSE]';

    Re := R.RobotWareServices.UpdateRapidVariable(Tarea, Modulo, NombreVariable, Valor);
    //if Re <> PostOk then
    writeln(IntToStr(Re));
    //  Modo := R.RobotWareServices.GetOperationMode;
    // WriteLn(Modo.opmode);

    Sleep(1000);
  end;




  Readln();
  FreeAndNil(R);

end.
