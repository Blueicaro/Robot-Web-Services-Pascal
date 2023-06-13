program project1;

{$mode objfpc}{$H+}

uses
 {$IFDEF UNIX}
  cthreads,
                 {$ENDIF}
  Classes { you can add units after this },
  SysUtils,
  AbbWebServices;

var
  Robot: TAbbWebServices;
  Lista: TStringList;
begin

  Robot := TAbbWebServices.Create('https://localhost:80');
  try
    try
      Lista := TStringList.Create;
      Robot.RobotWare.GetListModules('T_ROB1',Lista);
      Writeln ('Elementos: '+IntToStr(Lista.Count));
      WriteLn(Lista.Text)
    except
      on e: Exception do
        WriteLn(e.Message)
    end;
  finally
    FreeAndNil(Robot);
    FreeAndNil(Lista);
    ReadLn;
  end;

end.
