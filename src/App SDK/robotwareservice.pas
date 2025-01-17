unit RobotWareService;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, abbconexion, controller, rapid, robotwaredata;

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
    property Rapid: TRapid read FRapid;
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

  try
    FConexion := TRobotConnection.Create(RobotAddrs, User, Password, Connect);
    FController := TControllerRw7.Create(FConexion);
    FRapid := TRapidRw7.Create(FConexion);
  except

    //FreeAndNil(FConexion);
    raise TAbbWebServicesError.Create('Error conexion');
  end;

end;

destructor TRWS.Destroy;
begin
  FreeAndNil(FConexion);
  FreeAndNil(FController);
  FreeAndNil(FRapid);
  inherited Destroy;
end;

end.
