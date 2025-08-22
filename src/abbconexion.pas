unit abbconexion;
{ This unit contains the first class that you must use to connect to any robot.
  With Robotware 6 o 7, it's doesn't matter
  }
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
    procedure PrimeraConexion;
  protected

  public
    function GetUrlRobot: string;
    procedure SetRobotUrl(Url: string);
    property RobotUrl: string read FRobotUrl write SetRobotUrl;
    property Cookie: TStringList read FCookie write FCookie;
    property StatusCode: integer read FStatusCode;
    property StatusText: string read FStatusText;
    property Respuesta: TStringList read FRespuesta;
    property ReturnHeader: TStringList read FReturnHeader;
  public
    procedure Conectar;
    procedure SetUserPassword(aUser, aPassword: string);
    procedure Get(UrlRelative: string);
    procedure Post(UrlRelative: string; BodyText: string = '');
  public
    procedure GetListResources(aJson: TStringList; Lista: TStringList);
    procedure GetListStates(aJson: TStringList; ListaStates: TStringList);
    procedure GetDataResources(aJson: TStringList; aDataArray: string);
  public
    constructor Create;
    //Creates a Robot conexion.
    constructor Create(RobotAddrs: string; User: string = 'Default User';
      Password: string = 'robotics'; Connect: boolean = True); overload;
    destructor Destroy; override;
  end;

type

  { TRobotConnectionHelper }

  TRobotConnectionHelper = class helper for TRobotConnection
    function GetResourceItem(NombreCampo: string;
      SubArrayIndice: integer = 0): string;
    function GetStateName(ItemName: string): string;
    function GetCodeError: string;
    function GetHref(Index: integer): string;
    function GetName(Index: integer = 0): string;
    function GetResourceName(Index: integer): string;
    function ResourcesCount: integer;
  end;

implementation

uses
  {$IFDEF abbdebug}
    LazLogger,
  {$ENDIF}
  base64, StrUtils, md5, dateutils, URIParser, robotwaredata;
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

procedure TRobotConnection.Conectar;
begin
  PrimeraConexion;
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
  RutaAbsoluta: string;
begin
  RutaAbsoluta := FRobotUrl + UrlRelative;
  GenerarCabeceras;
  try
    CargarCookie;
    {$IFDEF abbdebug}
       DebugLn('Procesando get');
       Debugln (FHttpSend.RequestHeaders.Text);
       DebugLn('Ruta absoluta:' +RutaAbsoluta);
        try
      FHttpSend.Get(RutaAbsoluta, FRespuesta);
    except
      on E: Exception do
      DebugLn(E.Message);
    end;
    {$ELSE}
    FHttpSend.Get(RutaAbsoluta, FRespuesta);
    {$ENDIF}
    GenerarCookie;
    FStatusText := FHttpSend.ResponseStatusText;
  finally
    FStatusCode := FHttpSend.ResponseStatusCode;
    {$IFDEF abbdebug}
       DebugLn('ResponseStatusText: '+FHttpSend.ResponseStatusText);
       DebugLn('Fin Get');
    {$ENDIF}

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
      FHttpSend.RequestBody := TRawByteStringStream.Create(BodyText);
    end;
    {$IFDEF abbdebug}
       DebugLn('Inicio post');
       DebugLn('Ruta: '+RutaAbsoluta);
    {$ENDIF}
    FHttpSend.Post(RutaAbsoluta, Response);
    FStatusText := FHttpSend.ResponseStatusText;
    FStatusCode := FHttpSend.ResponseStatusCode;
    // FRespuesta.Append(Response.DataString);
    FRespuesta.Text := (Response.DataString);
    {$IFDEF abbdebug}
      DebugLn('Respuesta: '+FRespuesta.Text);
      DebugLn('Fin Post');
    {$ENDIF}
    GenerarCookie;
  finally
    FreeAndNil(Response);
    //FHttpSend.RequestBody := nil;
    FHttpSend.RequestBody.Free;
  end;

end;



procedure TRobotConnection.GenerarCabeceras(Get: boolean);
begin
  GenerarClave;
  FHttpSend.RequestHeaders.Clear;
  FHttpSend.AddHeader('Connection', 'Keep-Alive');
  FHttpSend.AddHeader('Accept', 'application/hal+json;v=2.0');
  FHttpSend.AddHeader('Authorization', 'Basic ' + FClave);
  if get then
  begin
    FHttpSend.AddHeader('Content-Type', 'application/hal+json;v=2.0');
  end
  else
  begin
    FHttpSend.AddHeader('Content-Type', 'application/x-www-form-urlencoded');
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

  //{$IFDEF abbdebug}
  // debugln ('GenerarCookie:') ;
  // debugln('FHttpSend.ResponseHeaders.Text: '+FHttpSend.ResponseHeaders.Text);
  //{$ENDIF}


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
{ #todo -oJorge : Modificaciónes para loggin Rw6. Trabajando aqui }
procedure TRobotConnection.PrimeraConexion;
begin
  GenerarCabeceras();
  FHttpSend.Get(FRobotUrl);
  GenerarCookie;
end;

function TRobotConnection.GetUrlRobot: string;
begin
  Result := FRobotUrl;
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

constructor TRobotConnection.Create(RobotAddrs: string; User: string;
  Password: string; Connect: boolean);
begin
  RobotUrl := RobotAddrs;
  FUser := User;
  FPassword := Password;
  Create;
  FHttpSend.UserName := User;
  FHttpSend.Password := Password;
  if Connect then
  begin
    PrimeraConexion;
  end;
end;

destructor TRobotConnection.Destroy;
begin
  FreeAndNil(FHttpSend);
  FreeAndNil(FRespuesta);
  FreeAndNil(FReturnHeader);
  FreeAndNil(FCookie);
  inherited Destroy;
end;

{ TRobotConnectionHelper }
function TRobotConnectionHelper.GetResourceItem(NombreCampo: string;
  SubArrayIndice: integer): string;
var
  dato, json: TJSONData;
  I: integer;
  Campo: string;
  j: TJSONtype;
begin
  Result := '';
  try
    json := GetJSON(FRespuesta.Text);
    try
      dato := json.GetPath('_embedded.resources');
      for I := 0 to dato.Items[SubArrayIndice].Count - 1 do
      begin
        j := dato.Items[0].JSONType;
        Campo := TJSONObject(dato.Items[SubArrayIndice]).Names[I];
        if Campo = NombreCampo then
        begin
          Result := dato.Items[SubArrayIndice].Items[I].AsString;
          Break;
        end;
      end;
    except
      FRespuesta.SaveToFile('GetResourceItem.txt');
    end;
  finally
    FreeAndNil(json);
  end;
end;

function TRobotConnectionHelper.GetStateName(ItemName: string): string;
var
  json, dato: TJSONData;
  I: integer;
  j: TJSONtype;
  Campo: string;
begin
  Result := '';
  try
    json := GetJSON(FRespuesta.Text);
    try
      dato := json.GetPath('state');
      for I := 0 to dato.items[0].Count - 1 do
      begin
        j := dato.Items[0].JSONType;
        Campo := TJSONObject(dato.Items[0]).Names[I];
        if Campo = ItemName then
        begin
          Result := dato.Items[0].items[I].AsString;
          Break;
        end;
      end;
    except
      FRespuesta.SaveToFile('GetStateItem.txt');
    end;
  finally
    FreeAndNil(json);
  end;
end;

function TRobotConnectionHelper.GetCodeError: string;
var
  Dato: TJSONData;
  Cadena: TJSONStringType;
begin
  Result := '';
  if StatusCode <> 200 then
  begin
    Dato := GetJSON(FRespuesta.Text);
    Cadena := Dato.GetPath('_embedded.status.msg').AsString;
    Cadena := trim(ExtractDelimited(2, Cadena, [']']));
    Cadena := trim(ExtractDelimited(1, Cadena, ['-']));
    Result := Cadena;
    FreeAndNil(Dato);
  end;
end;

function TRobotConnectionHelper.GetHref(Index: integer): string;
var
  Data: TJSONData;
begin
  Result := '';
  Data := GetJSON(FRespuesta.Text);
  Result := Data.GetPath('_embedded.resources').items[index].GetPath(
    '_links.self.href').AsString;
  FreeAndNil(Data);
end;

function TRobotConnectionHelper.GetName(Index: integer): string;
var
  Data, jCampo: TJSONData;
  Cadena: TJSONStringType;
begin
  Result := '';
  Data := GetJSON(FRespuesta.Text);
  Result := Data.GetPath('_embedded._state').Items[Index].GetPath('name').AsString;
  FreeAndNil(Data);

end;

function TRobotConnectionHelper.GetResourceName(Index: integer): string;
begin
  Result := GetResourceItem('name', Index);
end;

//Obtiene la longuitud de los elementos de _embedded.resources
function TRobotConnectionHelper.ResourcesCount: integer;
var
  Data: TJSONData;
begin
  Data := GetJSON(FRespuesta.Text);
  Result := Data.GetPath('_embedded.resources').Count;
  FreeAndNil(Data);
end;

end.
