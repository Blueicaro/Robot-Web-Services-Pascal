program TestSDK;

uses
  SysUtils,
  RobotWareService,
  abbwstypes;

var
  R: TRWS;
begin
  R := TRWS.Create('http://localhost');
  try
    WriteLn('Operation mode: ' + R.Controller.getOperationMode);
    Writeln('Control State: ' + R.Controller.getControllerState);
    Writeln('Identity: ' + R.Controller.getIdentity);
    Writeln('Virtual: ' + BoolToStr(R.Controller.isVirtualController, True));
    Writeln('Fecha y hora: '+R.Controller.GetTime);
    Writeln ('TimeZone: '+R.Controller.getTimezone);
  finally
    FreeAndNil(R);
  end;
  ReadLn;
end.
