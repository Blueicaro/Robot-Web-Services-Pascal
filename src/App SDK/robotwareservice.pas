unit RobotWareService;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, abbconexion,controller;

type

  { TRWS }

  TRWS = class
  strict private
    FConexion: TRobotConnection;
    FController: TController;
  published
    property Conexion: TRobotConnection read FConexion;
    public
      property Controller: TController read FController;
  public
    constructor Create;
    constructor  Create(RobotAddrs: string; User: string='Default User'; Password: string='robotics';
      Connect: boolean = True); overload;
    destructor Destroy; override;
  end;

implementation

{ TRWS }



constructor TRWS.Create;
begin

  FConexion := TRobotConnection.Create;
  FController := TControllerRw7.Create(nil);
end;

constructor TRWS.Create(RobotAddrs: string; User: string; Password: string;
  Connect: boolean);
begin
  { #todo : Capturar excepcion }
  FConexion.Create(RobotAddrs,User,Password,Connect);
end;

destructor TRWS.Destroy;
begin
  FreeAndNil(FConexion);
  FreeAndNil(FController);
  inherited Destroy;
end;

end.
