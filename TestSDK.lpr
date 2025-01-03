program TestSDK;
uses SysUtils,RobotWareService, abbwstypes;
var
  R: TRWS;
begin
  R := TRWS.Create();
  If R.Controller.isVirtualController then
  begin
    Writeln ('6');
  end
  else
  begin
    WriteLn('7');
  end;
  FreeAndNil(R);
  ReadLn;
end.

