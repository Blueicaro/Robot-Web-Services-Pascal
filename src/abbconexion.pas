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
    FDigestAuthentication: boolean;
    procedure GenerarCabeceras(Get: boolean = True);
    procedure GenerarClave;
    procedure GenerarCookie;
    procedure CargarCookie;
  public
    procedure PrimeraConexion;
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

uses
  {$IFDEF abbdebug}
    LazLogger,
  {$ENDIF}
  base64, StrUtils, md5, dateutils, URIParser;
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
  //if FDigestAuthentication then
  //begin
  //  RutaAbsoluta:=RutaAbsoluta+'?json=1';
  //end;
  // FRespuesta.Clear;
  GenerarCabeceras;
  try
    CargarCookie;
    {$IFDEF abbdebug}
       DebugLn('Procesando get');
       Debugln (FHttpSend.RequestHeaders.Text);
       DebugLn('Ruta absoluta:' +RutaAbsoluta);
    {$ENDIF}
    try
      FHttpSend.Get(RutaAbsoluta, FRespuesta);
    except
      on E: Exception do
        DebugLn(E.Message);
    end;
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
    FRespuesta.Text:=(Response.DataString);
    {$IFDEF abbdebug}
      DebugLn('Respuesta: '+FRespuesta.Text);
      DebugLn('Fin Post');
    {$ENDIF}
    GenerarCookie;
  finally
    FreeAndNil(Response);
    FHttpSend.RequestBody := nil;
  end;

end;



procedure TRobotConnection.GenerarCabeceras(Get: boolean);
begin
  FHttpSend.RequestHeaders.Clear;

  FHttpSend.AddHeader('Connection', 'Keep-Alive');
  if FDigestAuthentication = False then  //RobotWare 7
  begin
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
  end
  else
  begin
    FHttpSend.AddHeader('Accept', 'application/hal+json');
    if get then
    begin
      FHttpSend.AddHeader('Content-Type', 'application/hal+json');
    end
    else
    begin
      FHttpSend.AddHeader('Content-Type', 'application/x-www-form-urlencoded');
    end;
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
  //

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
var
  Codigo, Posicion, I, J: integer;
  Cadena, realm, nonce, qop, opaque, cnonce, aStr, h1, h2, h3, UriStr,
  h4, login, pass, URL: string;
  aList: TStringList;
  URI: TURI;
  Stream: TStringStream;
begin

  if ExtractDelimited(1, FRobotUrl, [':']) = 'http' then
  begin
    FDigestAuthentication := True;
  end
  else
  begin
    FDigestAuthentication := False;
  end;

  if FDigestAuthentication = False then
  begin
    FHttpSend.Get(FRobotUrl);
    GenerarCookie;
  end
  else
  begin
    try
      login := FUser;
      pass := FPassword;
      URL := FRobotUrl;
      Stream := TStringStream.Create;
      aList := TStringList.Create;
      URI := ParseURI(URL, False);
      UriStr := URI.Path;
      begin
        if (Length(URI.Path) > 0) and ((Length(UriStr) = 0) or
          (UriStr[Length(UriStr)] <> '/')) then
          UriStr := UriStr + '/';
        UriStr := UriStr + URI.Document;
      end;
      if Length(URI.Params) > 0 then
        UriStr := UriStr + '?' + URI.Params;
      {$IFDEF abbdebug}
        DebugLn(UriStr);
      {$ENDIF}
      FHttpSend.KeepConnection := True;
      FHttpSend.AllowRedirect := True;
      FHttpSend.HTTPMethod('GET', URL, Stream, [200, 401]);
      if FHttpSend.ResponseStatusCode = 401 then
      begin

        for I := 0 to FHttpSend.ResponseHeaders.Count - 1 do
        begin
          if LeftStr(uppercase(FHttpSend.ResponseHeaders.Strings[I]), 24) =
            'WWW-AUTHENTICATE: DIGEST' then
          begin
            realm := '';
            nonce := '';
            qop := '';
            opaque := '';
            cnonce := md5Print(md5String(IntToStr(DateTimeToUnix(Now()))));

            aList.Clear;
            aList.StrictDelimiter := True;
            aList.Delimiter := ',';
            aList.DelimitedText :=
              trim(Copy(FHttpSend.ResponseHeaders.Strings[I], 25));
            for J := 0 to pred(aList.Count) do
            begin
              aStr := trim(aList.Strings[J]);
              if LeftStr(aStr, 5) = 'realm' then
                realm := Copy(aStr, 7, Length(aStr)).DeQuotedString(#34);
              if LeftStr(aStr, 5) = 'nonce' then
                nonce := Copy(aStr, 7, Length(aStr)).DeQuotedString(#34);
              if LeftStr(aStr, 3) = 'qop' then
                qop := Copy(aStr, 5, Length(aStr)).DeQuotedString(#34);
              if LeftStr(aStr, 6) = 'opaque' then
                opaque := Copy(aStr, 8, Length(aStr)).DeQuotedString(#34);
            end;

            h1 := md5Print(md5String(login + ':' + realm + ':' + pass));
            h2 := md5Print(md5String('GET' + ':' + UriStr));
            if (qop = 'auth') or (qop = 'auth-int') then
              h3 := md5Print(md5String(h1 + ':' + nonce + ':00000001:' +
                cnonce + ':' + qop + ':' + h2))
            else
              h3 := md5Print(md5String(h1 + ':' + nonce + ':' + h2));

            h4 := 'username=' + AnsiQuotedStr(login, #34);
            h4 := h4 + ', realm=' + AnsiQuotedStr(realm, #34);
            h4 := h4 + ', nonce=' + AnsiQuotedStr(nonce, #34);
            h4 := h4 + ', uri=' + AnsiQuotedStr(UriStr, #34);
            if (qop = 'auth') or (qop = 'auth-int') then
            begin
              h4 := h4 + ', qop=' + qop;
              h4 := h4 + ', nc=00000001';
            end;
            h4 := h4 + ', cnonce=' + AnsiQuotedStr(cnonce, #34);
            h4 := h4 + ', response=' + AnsiQuotedStr(h3, #34);
            if opaque <> '' then
              h4 := h4 + ', opaque=' + AnsiQuotedStr(opaque, #34);

            FHttpSend.RequestHeaders.Add('Authorization: Digest ' + h4);

            Stream.Clear; // clear the previous request in the stream
            FHttpSend.Password := '';
            FHttpSend.UserName := '';
            FHttpSend.HTTPMethod('GET', URL, Stream, [200]);
            GenerarCookie;

            {$IFDEF abbdebug}
              Debugln(FHttpSend.ResponseHeaders.Text);
              Debugln(Stream.DataString);
            {$ENDIF}

          end;
        end;
      end;
    finally
      Stream.Free;
      aList.Free;
    end;
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
