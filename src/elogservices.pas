unit elogservices;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, abbwstypes, abbconexion;

type

  { TElogService }

  TElogService = class
  private
    FLocalUrl: string;
    FConection: TRobotConnection;
  public
    procedure GetListDomains(aDomainList: TElogDomainList; aLenguage: string = 'en');
    procedure ClearAll;
  public
    constructor Create(aRobotConexion: TRobotConnection);
    destructor Destroy; override;
  end;

implementation

{ TElogService }

procedure TElogService.GetListDomains(aDomainList: TElogDomainList; aLenguage: string);
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

procedure TElogService.ClearAll;
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

constructor TElogService.Create(aRobotConexion: TRobotConnection);
begin
  FLocalUrl := 'rw/elog/';
  FConection := aRobotConexion;
end;

destructor TElogService.Destroy;
begin
  FConection := nil;
  inherited Destroy;
end;

end.
