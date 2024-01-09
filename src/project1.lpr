program project1;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}
  cthreads,
  {$ENDIF}
  Classes { you can add units after this },
  SysUtils,
  AbbWebServices, elogservices, abbwstypes;

var
  Robot: TAbbWebServices;
  Lista: TStringList;
  P: TElogDomainList;
  I: Integer;
begin

  try
    Robot := TAbbWebServices.Create('https://localhost:80');
    try
      begin
        p := TElogDomainList.Create  ;
        Lista := TStringList.Create   ;
        Robot.ElogService.GetListDomains(P);
        For I := 0 To P.Count-1 do
        begin
          Writeln (p[I].domain_name);
        end;
        Lista.Clear;
        Robot.ElogService.ClearAll;
        WriteLn('Pulsa enter para continuar');
      end;
    except
      on e: Exception do
        WriteLn(e.Message)
    end;
  finally
    FreeAndNil(Robot);
    FreeAndNil(P) ;
    FreeAndNil(Lista);
    ReadLn;
  end;

end.
