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
    Robot := TAbbWebServices.Create('https://localhost:80');
    try
      begin
        p := TElogDomainList.Create;
        Lista := TStringList.Create;
        Robot.ElogService.GetListDomains(P);
        Lista.Clear;
        ListaMensajes := TElogMessageList.Create;
        robot.ElogService.GetElogDomain(ListaMensajes, p[0]);
        For I := 0 to ListaMensajes.Count-1 do
        begin
          Writeln (ListaMensajes[i].code+'.'+ListaMensajes[i].desc+' '+ListaMensajes[i].href);
        end;
        r := robot.ElogService.GetElogMessageInfo(ListaMensajes[0]);
        WriteLn('Pulsa enter para continuar');
      end;
      Robot.ElogService.ClearElogDomain(P[0]);
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
