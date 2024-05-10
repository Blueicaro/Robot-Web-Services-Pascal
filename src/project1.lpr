program project1;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}
  cthreads,
  {$ENDIF}
  Classes { you can add units after this },
  SysUtils,
  AbbWebServices,
  elogservices,
  abbwstypes;

var
  Robot: TAbbWebServices;
  Lista: TStringList;
  P: TElogDomainList;
  I: integer;
  ListaMensajes: TElogMessageList;
  r: TElogMessageInfo;
begin

  try
    //Robot := TAbbWebServices.Create('https://localhost:80');
    Robot := TAbbWebServices.Create('http://localhost');
    Lista := TStringList.Create;
    try
      begin
        Robot.Connection.PrimeraConexion;
        Robot.RobotWare.GetDomainList(Lista);
        For I:= 0 To Lista.Count-1 do
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
