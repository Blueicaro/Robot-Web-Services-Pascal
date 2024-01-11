unit Unit1;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ExtCtrls, StdCtrls,
  fphttpclient, opensslsockets, LazLogger;

type

  { TForm1 }

  TForm1 = class(TForm)
    Button1: TButton;
    Button2: TButton;
    Button3: TButton;
    Edit1: TEdit;
    Edit2: TEdit;
    Memo1: TMemo;
    Memo2: TMemo;
    Panel1: TPanel;
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure Button3Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
  private
    PeticionHttp: TFPHTTPClient;
    Cook1: string;
    Cook2: string;
  public

  end;

var
  Form1: TForm1;

implementation

uses StrUtils;
  {$R *.lfm}

  { TForm1 }

procedure TForm1.FormCreate(Sender: TObject);
begin
  PeticionHttp := TFPHTTPClient.Create(nil);
  PeticionHttp.KeepConnection := False;
end;

procedure TForm1.Button1Click(Sender: TObject);
var
  Respuesta: TStringList;
  Content_type, Accept, Cadena: string;
  I: integer;
  P: SizeInt;
begin
  Respuesta := TStringList.Create;
  Content_type := 'application/hal+json;v=2.0';
  Accept := 'application/hal+json;v=2.0';
  memo1.Lines.Clear;
  try
    with PeticionHttp do
    begin
      UserName := 'Default User';
      Password := 'robotics';

      //  AddHeader('Authorization', 'Basic ' + clave);
      AddHeader('Content-Type', Content_type);
      AddHeader('Accept', Accept);

      try
        get('https://localhost:80/rw/iosystem/signals', Respuesta);
        DebugLn(RequestHeaders.Text);
      except
        on E: Exception do
          Memo1.Lines.Add(E.Message);
      end;
      Memo2.Lines.Clear;
      Memo2.Lines.Add(Cookies.Text);
      Memo2.Lines.AddStrings(Cookies);
      Cook1 := #32 + Cookies[0] + '; ' + Cookies[5];
      Memo2.Lines.Add('Status:');
      Memo2.Lines.Add(ResponseStatusText + ' ' + IntToStr(ResponseStatusCode));
      Memo2.Lines.Add('');
      Memo2.Lines.Add('ResponseHeaders:');
      for I := 0 to ResponseHeaders.Count - 1 do
      begin
        Cadena := ResponseHeaders[I];
        Memo1.Lines.Add(Cadena);
        if StartsStr('Set-Cookie:', Cadena) then
        begin
          Cadena := ExtractDelimited(1, Cadena, [';']);
          Cadena := ExtractDelimited(2, Cadena, [#32]);
          Cadena := Trim(Cadena);
          if Edit1.Text = '' then
          begin
            Edit1.Text := Cadena;
          end
          else
          begin
            Edit2.Text := Cadena;
          end;
        end;

      end;

      DebugLn('Cookies stored at TFPHTTPClient');
      for I := 0 to PeticionHttp.Cookies.Count - 1 do
      begin
        DebugLn(PeticionHttp.Cookies[I]);
        // DebugLn(PeticionHttp.Cookies.Names[I]+','+PeticionHttp.Cookies.ValueFromIndex[I]);
      end;

    end;
  finally
    FreeAndNil(Respuesta);
  end;
end;

procedure TForm1.Button2Click(Sender: TObject);
var
  Respuesta: TStringList;
  Cadena: string;
  I, n: integer;
begin

  Respuesta := TStringList.Create;
  //Content_type := 'application/hal+json;v=2.0';
  //Accept := 'application/hal+json;v=2.0';
  memo1.Lines.Clear;
  try



    with PeticionHttp do
    begin
      Cookies.Clear;
      Cookies.Add(Edit1.Text);
      Cookies.Add(edit2.Text);

      try
        get('https://localhost:80/rw/', Respuesta);
      except
        on E: Exception do
          Memo1.Lines.Add(E.Message);
      end;
      Memo1.Lines.Add('Status:');
      Memo1.Lines.Add(ResponseStatusText + ' ' + IntToStr(ResponseStatusCode));
      Memo1.Lines.Add('');
      Memo1.Lines.Add('ResponseHeaders:');
      Memo1.Lines.Text := Respuesta.Text;
      DebugLn(ResponseHeaders.Text);
    end;

  finally
    FreeAndNil(Respuesta);
  end;

end;

procedure TForm1.Button3Click(Sender: TObject);
begin

end;

procedure TForm1.FormDestroy(Sender: TObject);
begin
  FreeAndNil(PeticionHttp);
end;

end.
