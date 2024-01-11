unit main;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, fpjson, Forms, Controls, Graphics, Dialogs, ExtCtrls,
  StdCtrls, AsyncProcess, SynEdit, SynHighlighterXML;

type

  { TForm1 }

  TForm1 = class(TForm)
    AsyncProcess1: TAsyncProcess;
    btEnviar: TButton;
    cbTextoToSend: TComboBox;
    edBody: TEdit;
    Panel1: TPanel;
    rbXml: TRadioButton;
    rbJsonV20: TRadioButton;
    rbJsonV21: TRadioButton;
    RadioGroup1: TRadioGroup;
    rbPost: TRadioButton;
    rbGet: TRadioButton;
    SynEditRespuesta: TSynEdit;
    synEditInfo: TSynEdit;
    SynXMLSyn1: TSynXMLSyn;
    procedure btEnviarClick(Sender: TObject);
    procedure cbTextoToSendKeyPress(Sender: TObject; var Key: char);
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure FormCreate(Sender: TObject);
    procedure rbJsonV20Click(Sender: TObject);
    procedure rbJsonV21Click(Sender: TObject);
    procedure rbXmlClick(Sender: TObject);
  private
    Content_type: string;
    Accept: string;
    procedure EnviarGet(aUrl: string);
    procedure EnviarPost(aUrl: string; Params: string);
    procedure Enviar;
  public

  end;

var
  Form1: TForm1;

implementation

{$R *.lfm}

{ TForm1 }
uses fphttpclient, opensslsockets, base64, jsonparser, jsonscanner;

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

procedure TForm1.FormClose(Sender: TObject; var CloseAction: TCloseAction);
begin
  cbTextoToSend.Items.SaveToFile('cache');
end;

procedure TForm1.FormCreate(Sender: TObject);
begin

  try
    if FileExists('cache') then
    begin
      cbTextoToSend.Items.LoadFromFile('cache');
    end;
  finally
    Content_type := 'application/xhtml+xml;v=2.0';
    Accept := 'application/xhtml+xml;v=2.0';
  end;
end;

procedure TForm1.rbJsonV20Click(Sender: TObject);
begin
  Content_type := 'application/hal+json;v=2.0';
  Accept := 'application/hal+json;v=2.0';

end;

procedure TForm1.rbJsonV21Click(Sender: TObject);
begin
  Content_type := 'application/hal+json;v=2.1';
  Accept := 'application/hal+json;v=2.1';
end;

procedure TForm1.rbXmlClick(Sender: TObject);
begin
  Content_type := 'application/xhtml+xml;v=2.0';
  Accept := 'application/xhtml+xml;v=2.0';
end;

procedure TForm1.EnviarGet(aUrl: string);
var
  Respuesta: TStringList;
  PeticionHttp: TFPHTTPClient;
  usuario, password, clave, stTemp, Cadena: string;
  I: integer;
  jpar: TJSONParser;
  d: TJSONStringType;
begin

  PeticionHttp := TFPHTTPClient.Create(nil);
  Respuesta := TStringList.Create;
  stTemp := '';
  usuario := 'Default User';
  password := 'robotics';
  clave := Format('%s:%s', [usuario, password]);
  clave := EncodeStringBase64(clave);
  synEditInfo.Clear;
  SynEditRespuesta.Clear;
  try

    with PeticionHttp do
    begin

      AddHeader('Authorization', 'Basic ' + clave);
      AddHeader('Content-Type', Content_type);
      AddHeader('Accept', Accept);

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
      Respuesta.SaveToFile('temp.txt');

      SynEditRespuesta.Lines.Text := Respuesta.Text;

    end;

  finally
    if PeticionHttp.ResponseStatusCode = 200 then
    begin
      if Accept <> 'application/xhtml+xml;v=2.0' then
      begin
        jpar := TJSONParser.Create(SynEditRespuesta.Lines.Text, [joUTF8]);
        SynEditRespuesta.Lines.Text := jpar.Parse.FormatJSON([], 2);
        FreeAndNil(jpar);
      end;
    end;
    Respuesta.SaveToFile('Respuesta.txt');
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
      AddHeader('Accept', Accept);
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
    Respuesta.SaveToFile('Respuesta.txt');
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

end.
