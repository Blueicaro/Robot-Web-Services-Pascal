unit Unit1;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ExtCtrls, StdCtrls,
  fphttpclient, opensslsockets, LazLogger, fpwebsocketclient, fpwebsocket,
  XMLRead, DOM, URIParser, httpprotocol;

type

  { TForm1 }

  TForm1 = class(TForm)
    Button1: TButton;
    Button2: TButton;
    Button3: TButton;
    Edit1: TEdit;
    Edit2: TEdit;
    Edit3: TEdit;
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
    FClient: TWebsocketClient;
    Cook1: string;
    Cook2: string;
    Ruta: TURI;
    procedure DoHandshakeResponse(Sender: TObject; aResponse: TWSHandShakeResponse;
      var aAllow: boolean);
  private
    procedure DoConection(Sender: TObject);
    procedure DoIncomingMessage(Sender: TObject; const aMessage: TWSMessage);
    procedure doDisconnect(Sender: TObject);
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
  PeticionHttp.KeepConnection := True;
  FClient := TWebsocketClient.Create(Self);
  FClient.UseSSL := True;
  FClient.OnConnect := @DoConection;
  FClient.OnMessageReceived := @DoIncomingMessage;
  FClient.OnHandshakeResponse := @DoHandshakeResponse;
  FClient.Active := False;
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
      DebugLn('Version http: ' + PeticionHttp.HTTPversion);
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
  Pt: rawbytestring;
  Respuesta: TStringList;
  Params: string;
  fsXml: TStringStream;
  XmlFichero: TXMLDocument;
  Nodo, hijo: TDOMNode;
  I, X: integer;
  Cadena: DOMString;
  Url: TURI;
begin
  PeticionHttp.Cookies.Clear;
  PeticionHttp.Cookies.Add(Edit1.Text);
  PeticionHttp.Cookies.Add(Edit2.Text);
  PeticionHttp.RequestHeaders.Clear;
  //Params := 'lang=en';
  Params := 'resources=1&1=/rw/iosystem/signals/ethernet/board1/do0;state&1-p=0';

  PeticionHttp.AddHeader('Content-Type', 'application/x-www-form-urlencoded;v=2.0');
  PeticionHttp.AddHeader('Accept', 'application/xhtml+xml;v=2.0');
  PeticionHttp.AddHeader('Content-Length', IntToStr(Length(Params)));
  Respuesta := TStringList.Create();

  PeticionHttp.RequestBody := TRawByteStringStream.Create(Params);
  PeticionHttp.Post('https://localhost:80/subscription', Respuesta);

  fsXml := TStringStream.Create(Respuesta.Text);
  ReadXMLFile(XmlFichero, fsXml);
  Nodo := XmlFichero.FindNode('html').FindNode('body').FindNode('div');
  for I := 0 to Nodo.ChildNodes.Count - 1 do
  begin
    hijo := Nodo.ChildNodes[I];
    for X := 0 to hijo.Attributes.Length - 1 do
    begin
      Cadena := hijo.Attributes[X].NodeValue;

      if StartsStr('wss:', Cadena) then
      begin
        Ruta := ParseURI(Cadena, False);
        Cadena := ExtractDelimited(3, Cadena, ['/']);
        Cadena := ExtractDelimited(1, Cadena, [':']);
        edit3.Text := Cadena;
      end;
    end;
  end;

  Memo2.Lines.Clear;
  Memo2.Lines.Assign(Respuesta);
  Memo2.Lines.Add(IntToStr(PeticionHttp.ResponseStatusCode));
  Memo2.Lines.Add(PeticionHttp.ResponseStatusText);
  FreeAndNil(Respuesta);
end;

procedure TForm1.Button3Click(Sender: TObject);
var
  Res: string;
  It: TIncomingResult;
  h: TCloseState;
begin
  FClient.HostName := Ruta.Host;
  FClient.Port := Ruta.Port;
  Res := Ruta.Path;
  Res := IncludeHTTPPathDelimiter(Res) + Ruta.Document;
// FClient.Connection;


  //FClient.Connect;
end;

procedure TForm1.FormDestroy(Sender: TObject);
begin
  FreeAndNil(FClient);
  FreeAndNil(PeticionHttp);
end;

procedure TForm1.DoHandshakeResponse(Sender: TObject; aResponse: TWSHandShakeResponse;
  var aAllow: boolean);
begin
  //aResponse.RawHeaders.Assign();
end;

procedure TForm1.DoConection(Sender: TObject);
begin
  Memo2.Lines.Add('Conectado');
end;

procedure TForm1.DoIncomingMessage(Sender: TObject; const aMessage: TWSMessage);
begin
  if aMessage.IsText then
  begin
    Memo2.Lines.Add('Mensaje entrante es texto');
  end
  else
  begin
    Memo2.Lines.Add('Mensaje entrante no texto');
  end;
end;

procedure TForm1.doDisconnect(Sender: TObject);
begin
  Memo2.Lines.Add('Desconectado');
end;

end.
