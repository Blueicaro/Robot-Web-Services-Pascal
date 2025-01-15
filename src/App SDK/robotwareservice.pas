unit RobotWareService;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, abbconexion, controller,rapid;

type

  { TRWS }

  TRWS = class
  strict private
    FConexion: TRobotConnection;
    FController: TController;
    FRapid: TRapid;
  published
    property Conexion: TRobotConnection read FConexion;
  public
    property Controller: TController read FController;
    property Rapid:TRapid read FRapid;
  public
    constructor Create;
    constructor Create(RobotAddrs: string; User: string = 'Default User';
      Password: string = 'robotics'; Connect: boolean = True); overload;
    destructor Destroy; override;
  end;

implementation

{ TRWS }



constructor TRWS.Create;
begin
  FConexion := TRobotConnection.Create;
end;

constructor TRWS.Create(RobotAddrs: string; User: string; Password: string;
  Connect: boolean);
var
  aFRapid: TControllerRw7;
begin
  { #todo : Capturar excepcion }
  FConexion := TRobotConnection.Create(RobotAddrs, User, Password, Connect);
  if FConexion.DigestAuthentication then
  begin
    FController := TControllerRw6.Create(FConexion);
    FRapid := TRapidRw6.Create(FConexion);
  end
  else
  begin
    FController := TControllerRw7.Create(FConexion);
  end;
end;

destructor TRWS.Destroy;
begin
  FreeAndNil(FConexion);
  FreeAndNil(FRapid);
  FreeAndNil(FController);
  inherited Destroy;
end;

end.
