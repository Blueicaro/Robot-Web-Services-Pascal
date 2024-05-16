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
  rw6controllerservice, rw6userservices;

var
  R: TRw6WebServices;
  Modo: TOpMode;


begin
  R := TRw6WebServices.Create('http://localhost');
  R.Connect;
  try
     Modo :=R.RobotWareServices.GetOperationMode;
     if Modo.opmode =opAUTO Then
      WriteLn('auto') ;
      R.RobotWareServices.UpdateRapidVariable;
  finally


    FreeAndNil(R);
    Readln();
  end;

end.
