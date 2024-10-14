program project1;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}
  cthreads,
  {$ENDIF}
  Classes,
  openssl,
  SysUtils,
  opensslsockets,
  fpopenssl,
  fphttpclient;

var
  HttpSend: TFPHTTPClient;
  Respuesta: TStringList;
begin
  try
    HttpSend := TFPHTTPClient.Create(nil);
    Respuesta := TStringList.Create;
    try
      HttpSend.Get('https://www.google.es', Respuesta);
      Writeln(Respuesta.Text);
    except
      on E: Exception do
        WriteLn(e.Message);
    end;
  finally
    FreeAndNil(Respuesta);
    FreeAndNil(HttpSend);
  end;
  Writeln ('Pues enter para continuar');
  Readln();

end.
