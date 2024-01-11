unit abbconexion;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, fphttpclient, opensslsockets, fpjson;

type

  { TRobotConnection }

  TRobotConnection = class
  private
    FCookie: TStringList;
    FPassword: string;
    FRespuesta: TStringList;
    FReturnHeader: TStringList;
    FRobotUrl: string;
    FStatusCode: integer;
    FStatusText: string;
    FUser: string;
    FHttpSend: TFPHTTPClient;
    FClave: string;
    procedure GenerarCabeceras(Get: boolean = True);
    procedure GenerarClave;
    procedure GenerarCookie;
    procedure CargarCookie;
  public
    procedure SetRobotUrl(Url: string);
    property Cookie: TStringList read FCookie write FCookie;
    property StatusCode: integer read FStatusCode;
    property StatusText: string read FStatusText;
    property Respuesta: TStringList read FRespuesta;
    property ReturnHeader: TStringList read FReturnHeader;
  public
    procedure SetUserPassword(aUser, aPassword: string);
    procedure Get(UrlRelative: string);
    procedure Post(UrlRelative: string; BodyText: string = '');
  public
    procedure GetListResources(aJson: TStringList; Lista: TStringList);
    procedure GetListStates(aJson: TStringList; ListaStates: TStringList);
    procedure GetDataResources(aJson: TStringList; aDataArray: string);
  public
    constructor Create;
    destructor Destroy; override;
  end;

implementation

{$IFDEF abbdebug}
   uses base64, StrUtils,laz_looger;
{$ELSE}

uses base64, StrUtils;
  {$ENDIF}

//{ TRobotConexionCustom }
//{ #todo : Añadir más verificaciones al parámetro URL }
procedure TRobotConnection.SetRobotUrl(Url: string);
begin
  if Url[Length(Url)] <> '/' then
  begin
    FRobotUrl := Url + '/';
  end
  else
  begin
    FRobotUrl := Url;
  end;
end;
{ #note -oJorge : Cambiando. FHTpSend ahora lleva el usuario y la clave }
procedure TRobotConnection.SetUserPassword(aUser, aPassword: string);
begin
  FUser := aUser;
  FPassword := aPassword;
  GenerarClave;
  FHttpSend.UserName := aUser;
  FHttpSend.Password := aPassword;
end;


procedure TRobotConnection.Get(UrlRelative: string);
var
  RutaAbsoluta, Cadena: string;
  I: integer;
begin
  RutaAbsoluta := FRobotUrl + UrlRelative;
  FRespuesta.Clear;

  GenerarCabeceras;




  try
    CargarCookie;
    FHttpSend.Get(RutaAbsoluta, FRespuesta);
    GenerarCookie;
    FStatusText := FHttpSend.ResponseStatusText;
  finally

    FStatusCode := FHttpSend.ResponseStatusCode;
  end;

end;

procedure TRobotConnection.Post(UrlRelative: string; BodyText: string);
var
  RutaAbsoluta: string;
  Response: TStringStream;
begin
  RutaAbsoluta := FRobotUrl + UrlRelative;
  FRespuesta.Clear;
  GenerarCabeceras(False);
  CargarCookie;
  try
    Response := TStringStream.Create('');
    if BodyText <> '' then;
    begin
      FHttpSend.RequestBody := TRawByteStringStream(BodyText);
    end;
    FHttpSend.Post(RutaAbsoluta, Response);
    FStatusText := FHttpSend.ResponseStatusText;
    FStatusCode := FHttpSend.ResponseStatusCode;
    FRespuesta.Append(Response.DataString);
    GenerarCookie;
  finally
    FHttpSend.RequestBody.Free;
    FreeAndNil(Response);
  end;

end;



procedure TRobotConnection.GenerarCabeceras(Get: boolean);
begin
  FHttpSend.RequestHeaders.Clear;
  FHttpSend.AddHeader('Authorization', 'Basic ' + FClave);
  FHttpSend.AddHeader('Accept', 'application/hal+json;v=2.0');

  if get then
  begin
    FHttpSend.AddHeader('Content-Type', 'application/hal+json;v=2.0');
  end
  else
  begin
    FHttpSend.AddHeader('Content-Type', 'application/x-www-form-urlencoded;v=2.0');
  end;
end;

procedure TRobotConnection.GenerarClave;
begin
  FClave := EncodeStringBase64(FUser + ':' + FPassword);
end;

procedure TRobotConnection.GenerarCookie;
var
  I: integer;
  Cadena: string;
begin

  {$IFDEF abbdebug}
   debugln ('GenerarCookie:')
   debugln('FHttpSend.ResponseHeaders.Text: 'FHttpSend.ResponseHeaders.Text);}
  {$ENDIF}


  if AnsiContainsText(FHttpSend.ResponseHeaders.Text, 'Set-Cookie') = False then
  begin
    Exit;
  end;

  for I := 0 to FHttpSend.ResponseHeaders.Count - 1 do
  begin
    if StartsStr('Set-Cookie:', FHttpSend.ResponseHeaders[I]) then
    begin
      Cadena := ExtractDelimited(1, FHttpSend.ResponseHeaders[I], [';']);
      Cadena := trim(ExtractDelimited(2, Cadena, [#32]));
      FCookie.Add(Cadena);
    end;
  end;

end;

procedure TRobotConnection.CargarCookie;
var
  I: integer;
begin
  FHttpSend.Cookies.Clear;
  for I := 0 to FCookie.Count - 1 do
  begin
    FHttpSend.Cookies.Add(FCookie[I]);
  end;
end;




{ TRobotConnection }


procedure TRobotConnection.GetListResources(aJson: TStringList; Lista: TStringList);
var
  jData, Data: TJSONData;
  myJsonObject: TJSONObject;
  I: integer;
begin
  jData := GetJSON(aJson.Text);
  try
    myJsonObject := jData as TJSONObject;
    if myJsonObject.JSONType = jtObject then
    begin
      Data := myJsonObject.GetPath('_embedded').GetPath('resources');
      for I := 0 to Data.Count - 1 do
      begin
        if Data.Items[I].FindPath('_title').JSONType = jtString then
        begin
          Lista.Add(Data.Items[I].FindPath('_title').AsString);
        end;
      end;
    end;
  finally
    FreeAndNil(jData);
  end;

end;

{ #todo : Pendiente de terminar }
procedure TRobotConnection.GetListStates(aJson: TStringList; ListaStates: TStringList);
var
  jData: TJSONData;
  myJsonObject: TJSONObject;
begin
  jData := GetJSON(aJson.Text);
  try
    myJsonObject := jData as TJSONObject;
  finally
    FreeAndNil(jData);
  end;
end;

procedure TRobotConnection.GetDataResources(aJson: TStringList; aDataArray: string);
var
  myJsonObject: TJSONObject;
  jData: TJSONData;
begin

  jData := GetJSON(aJson.Text);
  myJsonObject := jData as TJSONObject;
  if myJsonObject.JSONType = jtObject then
  begin
    aDataArray := myJsonObject.GetPath('_embedded').GetPath('resources').AsJSON;

  end;

end;


constructor TRobotConnection.Create;
begin
  FRespuesta := TStringList.Create;
  FReturnHeader := TStringList.Create;
  FHttpSend := TFPHTTPClient.Create(nil);
  FHttpSend.KeepConnection := True;
  FCookie := TStringList.Create;
end;

destructor TRobotConnection.Destroy;
begin
  FreeAndNil(FHttpSend);
  FreeAndNil(FRespuesta);
  FreeAndNil(FReturnHeader);
  FreeAndNil(FCookie);
  inherited Destroy;
end;

end.
