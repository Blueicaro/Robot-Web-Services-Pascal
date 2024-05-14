unit rw6controllerservice;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, abbconexion, rw6abbwstypes;

type

  { Trw6controllerservice }

  Trw6controllerservice = class
  private
    FLocalUrl: string;
    FConexion: TRobotConnection;
    public
      procedure GetIdentityResource(Var Identity: TCtrlIdentifyItem);
  public
    constructor Create(aRobotConexion: TRobotConnection);
    destructor Destroy; override;
  end;

implementation

{ Trw6controllerservice }

procedure Trw6controllerservice.GetIdentityResource(
  var Identity: TCtrlIdentifyItem);
var
  Info: TCtrlIdentifyList;
begin
  try
    FConexion.Get(FLocalUrl+'/identity?json=1');
  except
     ErrorWebService('Error conexión. codigo: ' + FConexion.StatusText);
  end;

  try
    Info := TCtrlIdentifyList.Create;
    if FConexion.StatusCode = 200 then
    begin
       GetEmbeddedStateList(FConexion.Respuesta.Text, Info as
          TCollection, TCtrlIdentifyItem, CTRL_IDENTITY_INFO);
    end;
     Identity.Assign(Info[0]);
  finally
     FreeAndNil(Info);
  end;
end;

constructor Trw6controllerservice.Create(aRobotConexion: TRobotConnection);
begin
  FConexion := aRobotConexion;
  FLocalUrl := 'ctrl';
end;

destructor Trw6controllerservice.Destroy;
begin
  FConexion := nil;
  inherited Destroy;
end;

end.
