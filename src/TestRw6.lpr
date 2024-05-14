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
  rw6controllerservice;

var
  R: TRw6WebServices;
  I: integer;
  Lista: TCtrlIdentifyItem;




begin
  R := TRw6WebServices.Create('http://localhost');
  R.Connect;
  try
    Lista := TCtrlIdentifyItem.Create(nil);
    R.ControllerService.GetIdentityResource(Lista)   ;
    WriteLn(Lista.ctrl_name);
  finally

    FreeAndNil(Lista);
    FreeAndNil(R);
    Readln();
  end;

end.
