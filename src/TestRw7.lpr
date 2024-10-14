program TestRw7;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}
  cthreads,
  {$ENDIF}
  Classes { you can add units after this },
  SysUtils,
  rw7webservices,
  rw7elogservices,
  rw7abbwstypes;

var
  Robot: TRw7WebServices;
  Lista: TStringList;
  P: TRw7ElogDomainList;
  I: integer;
  ListaMensajes: TRw7ElogMessageList;
  r: TRw7ElogMessageInfo;
begin

  try
    Robot := TRw7WebServices.Create('https://localhost:80');
    Lista := TStringList.Create;
    try
      begin
        Robot.Connection.PrimeraConexion;
        Robot.RobotWare.GetDomainList(Lista);
        for I := 0 to Lista.Count - 1 do
        begin
          WriteLn(Lista[i]);
        end;
      end;
    except
      on e: Exception do
        WriteLn(e.Message)
    end;
  finally
    FreeAndNil(Robot);
    FreeAndNil(ListaMensajes);
    FreeAndNil(P);
    FreeAndNil(Lista);
    ReadLn;
  end;

end.
