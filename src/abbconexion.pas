unit abbconexion;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, fphttpclient, opensslsockets, fpjson;

type

  { TRobotConexion }

  TRobotConexion = class
  private
    FCookie: string;
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
  public
    procedure SetRobotUrl(Url: string);
    property Cookie: string read FCookie;
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

uses base64;

//{ TRobotConexionCustom }
//{ #todo : Añadir más verificaciones al parámetro URL }
procedure TRobotConexion.SetRobotUrl(Url: string);
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

procedure TRobotConexion.SetUserPassword(aUser, aPassword: string);
begin
  FUser := aUser;
  FPassword := aPassword;
  GenerarClave;
end;


procedure TRobotConexion.Get(UrlRelative: string);
var
  RutaAbsoluta: string;
begin
  RutaAbsoluta := FRobotUrl + UrlRelative;
  FRespuesta.Clear;

  GenerarCabeceras;
  FHttpSend.Get(RutaAbsoluta, FRespuesta);
  FStatusText := FHttpSend.ResponseStatusText;
  FStatusCode := FHttpSend.ResponseStatusCode;
  FCookie := FHttpSend.ResponseHeaders.Values['Set-Cookie'];

end;

procedure TRobotConexion.Post(UrlRelative: string; BodyText: string);
var
  RutaAbsoluta: string;
  Response: TStringStream;
begin
  RutaAbsoluta := FRobotUrl + UrlRelative;
  FRespuesta.Clear;
  GenerarCabeceras(False);
  try
    Response := TStringStream.Create('');
    FHttpSend.RequestBody := TRawByteStringStream(BodyText);
    FHttpSend.Post(RutaAbsoluta, Response);
    FStatusText := FHttpSend.ResponseStatusText;
    FStatusCode := FHttpSend.ResponseStatusCode;
    FCookie := FHttpSend.ResponseHeaders.Values['Set-Cookie'];
    FRespuesta.Append(Response.DataString);
  finally
    FHttpSend.RequestBody.Free;
    FreeAndNil(Response);
  end;

end;



procedure TRobotConexion.GenerarCabeceras(Get: boolean);
begin
  FHttpSend.AddHeader('Authorization', 'Basic ' + FClave);
  FHttpSend.AddHeader('Accept', 'application/hal+json;v=2.0');
  if get then
  begin
    FHttpSend.AddHeader('Content-Type', 'application/xhtml+xml;v=2.0');
  end
  else
  begin
    FHttpSend.AddHeader('Content-Type', 'application/x-www-form-urlencoded;v=2.0');
  end;
end;

procedure TRobotConexion.GenerarClave;
begin
  FClave := EncodeStringBase64(FUser + ':' + FPassword);
end;



{ TRobotConexion }


procedure TRobotConexion.GetListResources(aJson: TStringList; Lista: TStringList);
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
procedure TRobotConexion.GetListStates(aJson: TStringList; ListaStates: TStringList);
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

procedure TRobotConexion.GetDataResources(aJson: TStringList; aDataArray: string);
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


constructor TRobotConexion.Create;
begin
  FRespuesta := TStringList.Create;
  FReturnHeader := TStringList.Create;
  FHttpSend := TFPHTTPClient.Create(nil);
end;

destructor TRobotConexion.Destroy;
begin
  FreeAndNil(FHttpSend);
  FreeAndNil(FRespuesta);
  FreeAndNil(FReturnHeader);
  inherited Destroy;
end;

end.
