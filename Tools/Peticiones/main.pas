unit main;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ExtCtrls, StdCtrls,
  SynEdit, SynHighlighterXML;

type

  { TForm1 }

  TForm1 = class(TForm)
    btEnviar: TButton;
    chkJson: TCheckBox;
    cbTextoToSend: TComboBox;
    Panel1: TPanel;
    SynEditRespuesta: TSynEdit;
    synEditInfo: TSynEdit;
    SynXMLSyn1: TSynXMLSyn;
    procedure btEnviarClick(Sender: TObject);
    procedure cbTextoToSendKeyPress(Sender: TObject; var Key: char);
  private
    procedure EnviarPeticion(aUrl: string);
    procedure AddUrl(aUrl:string);
  public

  end;

var
  Form1: TForm1;

implementation

{$R *.lfm}

{ TForm1 }
uses fphttpclient, opensslsockets, base64;

procedure TForm1.btEnviarClick(Sender: TObject);
begin
  EnviarPeticion(cbTextoToSend.Text);
end;

procedure TForm1.cbTextoToSendKeyPress(Sender: TObject; var Key: char);
var
  n: Integer;
begin
  if key =#13  then
  begin
    n:= cbTextoToSend.Items.IndexOf(cbTextoToSend.Text);
    if cbTextoToSend.Items.IndexOf(cbTextoToSend.Text) =-1 then
    begin
      if cbTextoToSend.Items.Count = 20 then
      begin
        cbTextoToSend.Items.Delete(19);
      end;
      cbTextoToSend.Items.Add(cbTextoToSend.Text);
    end;
     EnviarPeticion(cbTextoToSend.Text);
  end;
end;

procedure TForm1.EnviarPeticion(aUrl: string);
var
  Respuesta: TStringList;
  PeticionHttp: TFPHTTPClient;
  usuario, password, clave, stTemp, Cadena: string;
  I: integer;
begin

  PeticionHttp := TFPHTTPClient.Create(nil);
  Respuesta := TStringList.Create;
  try
    stTemp := '';
    usuario := 'Default User';
    password := 'robotics';
    clave := Format('%s:%s', [usuario, password]);
    clave := EncodeStringBase64(clave);
    synEditInfo.Clear;
    SynEditRespuesta.Clear;
    with PeticionHttp do
    begin
      AddHeader('Authorization', 'Basic ' + clave);
      AddHeader('Content-Type', 'application/xhtml+xml;v=2.0');
      if chkJson.Checked = false then
      begin
        AddHeader('Accept', 'application/xhtml+xml;v=2.0');
      end
      else
      begin
        AddHeader('Accept','application/hal+json;v=2.0');
      end;
      //Dirección IP del robot. Localhost, puerto 80 si el controlador es virtual
      try
        get(aUrl, Respuesta);
      except
        on E: Exception do
          synEditInfo.Lines.Add(E.Message);
      end;
      synEditInfo.Lines.Add('Status:');
      synEditInfo.Lines.Add(ResponseStatusText + ' ' +
        IntToStr(ResponseStatusCode));
      synEditInfo.Lines.Add('');
      synEditInfo.Lines.Add('ResponseHeaders:');
      for I := 0 to ResponseHeaders.Count - 1 do
      begin
        synEditInfo.Lines.Add(ResponseHeaders[I]);
      end;

      SynEditRespuesta.Lines.Text := Respuesta.Text;

    end;

  finally
    FreeAndNil(Respuesta);
    FreeAndNil(PeticionHttp);
  end;

end;

procedure TForm1.AddUrl(aUrl: string);
begin

end;

end.
