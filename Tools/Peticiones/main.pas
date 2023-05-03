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
    edBody: TEdit;
    Panel1: TPanel;
    rbPost: TRadioButton;
    rbGet: TRadioButton;
    SynEditRespuesta: TSynEdit;
    synEditInfo: TSynEdit;
    SynXMLSyn1: TSynXMLSyn;
    procedure btEnviarClick(Sender: TObject);
    procedure cbTextoToSendKeyPress(Sender: TObject; var Key: char);
  private
    procedure EnviarGet(aUrl: string);
    procedure EnviarPost(aUrl: string; Params: string);
    procedure Enviar;
    procedure AddUrl(aUrl: string);
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
  Enviar;
end;

procedure TForm1.cbTextoToSendKeyPress(Sender: TObject; var Key: char);
var
  n: integer;
begin
  if key = #13 then
  begin
    n := cbTextoToSend.Items.IndexOf(cbTextoToSend.Text);
    if cbTextoToSend.Items.IndexOf(cbTextoToSend.Text) = -1 then
    begin
      if cbTextoToSend.Items.Count = 20 then
      begin
        cbTextoToSend.Items.Delete(19);
      end;
      cbTextoToSend.Items.Add(cbTextoToSend.Text);
    end;
    EnviarGet(cbTextoToSend.Text);
  end;
end;

procedure TForm1.EnviarGet(aUrl: string);
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
      if chkJson.Checked = False then
      begin
        AddHeader('Accept', 'application/xhtml+xml;v=2.0');
      end
      else
      begin
        AddHeader('Accept', 'application/hal+json;v=2.0');
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

procedure TForm1.EnviarPost(aUrl: string; Params: string);
var
  PeticionHttp: TFPHTTPClient;
  usuario, password, clave, stTemp, Cadena: string;
  I: integer;
  Respuesta: TStringStream;
begin

  PeticionHttp := TFPHTTPClient.Create(nil);
  Respuesta := TStringStream.Create('');
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
      AddHeader('Content-Type', 'application/x-www-form-urlencoded;v=2.0');
      if chkJson.Checked = False then
      begin
        AddHeader('Accept', 'application/xhtml+xml;v=2.0');
      end
      else
      begin
        AddHeader('Accept', 'application/hal+json;v=2.0');

      end;
      RequestBody := TRawByteStringStream.Create(Params);
      //Dirección IP del robot. Localhost, puerto 80 si el controlador es virtual
      try
        post(aUrl, Respuesta);
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

      SynEditRespuesta.Lines.Append(Respuesta.DataString);

    end;

  finally
    PeticionHttp.RequestBody.Free;
    FreeAndNil(Respuesta);
    FreeAndNil(PeticionHttp);
  end;

end;

procedure TForm1.Enviar;
begin
  if rbGet.Checked then
  begin
    EnviarGet(cbTextoToSend.Text);
  end;
  if rbPost.Checked then
  begin
    EnviarPost(cbTextoToSend.Text, edBody.Text);
  end;
end;

procedure TForm1.AddUrl(aUrl: string);
begin

end;

end.
