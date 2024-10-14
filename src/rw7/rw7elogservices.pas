unit rw7elogservices;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, abbwstypes, abbconexion;

type

  { TRw7ElogService }

  TRw7ElogService = class
  private
    FLocalUrl: string;
    FConection: TRobotConnection;
  public
    procedure ClearAll;
    procedure ClearElogDomain(aDomain: TElogDomainItem);
    procedure GetListDomains(aDomainList: TElogDomainList; aLenguage: string = 'en');
    procedure GetElogDomain(aElogMessageList: TElogMessageList;
      aDomainItem: TElogDomainItem; aLenguage: string = 'en');
    function GetElogMessageInfo(aElogMessageItem: TElogMessageItem): TElogMessageInfo;
  public
    constructor Create(aRobotConexion: TRobotConnection);
    destructor Destroy; override;
  end;

implementation

uses jsonparser, fpjson;
  { TRw7ElogService }

procedure TRw7ElogService.GetListDomains(aDomainList: TElogDomainList; aLenguage: string);
var
  Lista: TCollection;
begin
  try
    FConection.Get(FLocalUrl + '?lang=' + aLenguage);
  except
    ErrorWebService('Error conexión. codigo: ' + FConection.StatusText);
  end;
  if FConection.StatusCode = 200 then
  begin
    try
      Lista := TCollection.Create(TElogDomainItem);
      GetEmbeddedClassList(FConection.Respuesta.Text, Lista, TElogDomainItem,
        ELOG_DOMAIN_LI);
      aDomainList.Assign(Lista);
    finally
      FreeAndNil(Lista);
    end;
  end;
end;

procedure TRw7ElogService.GetElogDomain(aElogMessageList: TElogMessageList;
  aDomainItem: TElogDomainItem; aLenguage: string);
var
  Lista: TCollection;
begin
  try
    FConection.Get(FLocalUrl + aDomainItem._title + '?lang=' + aLenguage);
  except
    ErrorWebService('Error conexión. codigo: ' + FConection.StatusText);
  end;
  if FConection.StatusCode = 200 then
  begin
    try
      Lista := TCollection.Create(TElogMessageItem);
      GetEmbeddedClassList(FConection.Respuesta.Text, Lista, TElogMessageItem,
        ELOG_MESSAGE_LI);
      aElogMessageList.Assign(Lista);
    finally
      FreeAndNil(Lista);
    end;
  end;
end;
{ #todo -oJorge : Gestionar cuando el Campo Json es un array }
function TRw7ElogService.GetElogMessageInfo(aElogMessageItem: TElogMessageItem):
TElogMessageInfo;
var
  jData, info: TJSONData;
  myJsonObject: TJSONObject;
  I: integer;
  Campo: string;
  valor: TJSONVariant;
begin
  try
    FConection.Get(FLocalUrl + aElogMessageItem.href);
  except
    ErrorWebService('Error conexión. codigo: ' + FConection.StatusText);
  end;
  if FConection.StatusCode = 200 then
  begin
    try
      jData := GetJSON(FConection.Respuesta.Text);
      myJsonObject := jData as TJSONObject;
      info := myJsonObject.FindPath('status');
      if info <> nil then
      begin
        Result.code := info.Items[0].AsJSON;
      end;
      info := myJsonObject.FindPath('state');
      if info <> nil then
      begin
        for I := 0 to info.items[0].Count - 1 do
        begin
          Campo := TJSONObject(info.Items[0]).Names[I];
          begin
            if info.Items[0].Items[I].JSONType = jtString then
              valor := info.Items[0].Items[I].Value;
            case Campo of
              'title':
                Result.title := Valor;
              'code':
                Result.code := Valor;
              'tstamp':
                Result.tstamp := valor;
              'desc':
                Result.description := Valor;
              'conseqs':
                Result.consequences := Valor;
              'causes':
                Result.causes := Valor;
              'actions':
                Result.actions := Valor;
              'args':
                Result.argc := Valor;
            end;
          end;
        end;
      end;
    finally
      FreeAndNil(jData);
    end;
  end;
end;

procedure TRw7ElogService.ClearAll;
begin
  try
    FConection.Post(FLocalUrl + 'clearall');
  except
    ErrorWebService('Error conexión. codigo: ' + FConection.StatusText);
  end;
  if FConection.StatusCode <> 204 then
  begin
    ErrorWebService('Error conexión. codigo: ' + IntToStr(FConection.StatusCode));
  end;
end;

procedure TRw7ElogService.ClearElogDomain(aDomain: TElogDomainItem);
begin
  try
    FConection.Post(FLocalUrl +aDomain._title+'/clear');
  except
    ErrorWebService('Error conexión. codigo: ' + FConection.StatusText);
  end;
  if FConection.StatusCode <> 204 then
  begin
    ErrorWebService('Error conexión. codigo: ' + IntToStr(FConection.StatusCode));
  end;
end;

constructor TRw7ElogService.Create(aRobotConexion: TRobotConnection);
begin
  FLocalUrl := 'rw/elog/';
  FConection := aRobotConexion;
end;

destructor TRw7ElogService.Destroy;
begin
  FConection := nil;
  inherited Destroy;
end;

end.
