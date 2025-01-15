unit controller;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, abbconexion, robotwaredata, fpjson;

type

  { TControllerBase }

  TControllerBase = class(Tbase)
  public
   // class property Conexion: TRobotConnection read FConexion write SetConexion;
    class function isVirtualController: boolean; virtual; abstract;
    class function getControllerState: string virtual; abstract;
    class function setMotorsState(aMotorState: TMotorState): string; virtual; abstract;
    class function getOperationMode: string; virtual; abstract;
    class function setOperationMode(aMode: string): string; virtual; abstract;
    class function restartController(aRestartMode: TRestartModes): string;
      virtual; abstract;
    class function getEnvironmentVariable: string; virtual; abstract;
    class function getTimezone: string; virtual; abstract;
    class function getIdentity: string; virtual; abstract;
    class function getNetworkSettings: string; virtual; abstract;
    class function getNetworkConnections: string; virtual; abstract;
    class function verifyOption(aValue: string): boolean; virtual; abstract;
    class function createBackup(Path: string; TimeOut: integer): string;
      virtual; abstract;
    class function verifyBackup(Path: string): string; virtual; abstract;
    class function restoreBackup(Path: string): string; virtual; abstract;
    class function saveDiagnostics(Path: string; TimeOut: integer): string;
      virtual; abstract;
    class function GetTime: string; virtual; abstract;
  end;

type

  { TControllerRw6 }

  TControllerRw6 = class(TControllerBase)
    class function CreateBackup(Path: string; TimeOut: integer): string; override;
    class function GetControllerState: string; override;
    class function GetEnvironmentVariable: string; override;
    class function GetIdentity: string; override;
    class function getNetworkConnections: string; override;
    class function getNetworkSettings: string; override;
    class function getOperationMode: string; override;
    class function GetTimezone: string; override;
    class function isVirtualController: boolean; override;
    class function restoreBackup(Path: string): string; override;
    class function saveDiagnostics(Path: string; TimeOut: integer): string; override;
    class function setOperationMode(aMode: string): string; override;
    class function verifyBackup(Path: string): string; override;
    class function verifyOption(aValue: string): boolean; override;
    class function setMotorsState(aMotorState: TMotorState): string; override;
  public
    constructor Create(aConexion: TRobotConnection);
    destructor Destroy; override;
    class function GetTime: string; override;
    class function RestartController(aRestartMode: TRestartModes): string; override;

  end;

type

  { TControllerRw7 }

  TControllerRw7 = class(TControllerBase)
    class function createBackup(Path: string; TimeOut: integer): string; override;
    class function getControllerState: string virtual;
    class function getEnvironmentVariable: string; override;
    class function getIdentity: string; override;
    class function getNetworkConnections: string; override;
    class function getNetworkSettings: string; override;
    class function getOperationMode: string; override;
    class function getTimezone: string; override;
    class function isVirtualController: boolean; override;
    class function restoreBackup(Path: string): string; override;
    class function saveDiagnostics(Path: string; TimeOut: integer): string; override;
    class function setOperationMode(aMode: string): string; override;
    class function verifyBackup(Path: string): string; override;
    class function verifyOption(aValue: string): boolean; override;
  public
    constructor Create(aConexion: TRobotConnection);
    class function restartController(aRestartMode: TRestartModes): string; override;
    class function setMotorsState(aMotorState: TMotorState): string; override;
  end;

type
  TController = TControllerBase;




implementation


{ TControllerRw6 }

class function TControllerRw6.CreateBackup(Path: string; TimeOut: integer): string;
begin

end;

class function TControllerRw6.GetControllerState: string;
begin
  Result := '';
  try
    FConexion.Get('rw/panel/ctrlstate?json=1');
  except
    raise TAbbWebServicesError.Create('Error de conexión');
  end;
  Result := FConexion.GetStateArrayElemento('ctrlstate');
end;

class function TControllerRw6.GetEnvironmentVariable: string;
begin

end;

class function TControllerRw6.GetIdentity: string;
begin
  try
    FConexion.Get('ctrl/identity?json=1 ');
  except
    raise TAbbWebServicesError.Create('Error de conexión');
  end;
  Result := FConexion.GetStateArrayElemento('ctrl-name');
end;

class function TControllerRw6.getNetworkConnections: string;
begin

end;

class function TControllerRw6.getNetworkSettings: string;
begin

end;

class function TControllerRw6.getOperationMode: string;
begin
  try
    FConexion.Get('rw/panel/opmode?json=1');
  except
    raise TAbbWebServicesError.Create('Error de conexión');
  end;
  Result := FConexion.GetStateArrayElemento('opmode');
end;

class function TControllerRw6.GetTimezone: string;
begin
  Result := '';
  try
    FConexion.Get('ctrl/clock/timezone?json=1');
  except
    raise TAbbWebServicesError.Create('Error de conexión');
  end;
  Result := FConexion.GetCodeError;
  if Result = '' then
  begin
    Result := FConexion.GetStateArrayElemento('timezone');
  end;
end;

class function TControllerRw6.isVirtualController: boolean;
var
  Cadena: string;
begin
  Result := False;
  try
    FConexion.Get('ctrl/identity?json=1');
  except
    raise TAbbWebServicesError.Create('Error de conexión');
  end;
  cadena := FConexion.GetStateArrayElemento('ctrl-type');
  Result := 'Virtual Controller' = Cadena;

end;


class function TControllerRw6.restoreBackup(Path: string): string;
begin

end;

class function TControllerRw6.saveDiagnostics(Path: string; TimeOut: integer): string;
begin

end;


class function TControllerRw6.setOperationMode(aMode: string): string;
begin

end;

class function TControllerRw6.verifyBackup(Path: string): string;
begin

end;

class function TControllerRw6.verifyOption(aValue: string): boolean;
begin

end;

constructor TControllerRw6.Create(aConexion: TRobotConnection);
begin
  Self.FConexion := aConexion;
end;

destructor TControllerRw6.Destroy;
begin
  Self.FConexion := nil;
  inherited Destroy;
end;

class function TControllerRw6.GetTime: string;
begin
  try
    FConexion.Get('ctrl/clock?json=1');
  except
    raise TAbbWebServicesError.Create('Error de conexión');
  end;

  Result := FConexion.getStateArrayElemento('datetime');

end;

class function TControllerRw6.RestartController(aRestartMode: TRestartModes): string;
var
  modo: string;
begin
  Result := '';
  modo := '';
  case aRestartMode of
    rmResetSystem:
      modo := 'istart';
    rmBootAplication:
      modo := 'xstart';
    rmResetRapid:
      modo := 'pstart';
    rmRestart:
      modo := 'restart';
    rmRevertToAutoSave:
      modo := 'bstart';
    rmShutDown:
      modo := 'shutdown';
  end;

  modo := 'restart-mode=' + modo;
  try
    FConexion.Post('ctrl?json=1', modo);
  except
    raise TAbbWebServicesError.Create('Error de conexión');
  end;
  Result := FConexion.GetCodeError;

end;

class function TControllerRw6.setMotorsState(aMotorState: TMotorState): string;
begin

end;


{ TControllerRw7 }

class function TControllerRw7.createBackup(Path: string; TimeOut: integer): string;
begin

end;

class function TControllerRw7.getControllerState: string;
begin

end;

class function TControllerRw7.getEnvironmentVariable: string;
begin

end;

class function TControllerRw7.getIdentity: string;
begin

end;

class function TControllerRw7.getNetworkConnections: string;
begin

end;

class function TControllerRw7.getNetworkSettings: string;
begin

end;

class function TControllerRw7.getOperationMode: string;
begin

end;

class function TControllerRw7.getTimezone: string;
begin

end;

class function TControllerRw7.isVirtualController: boolean;
begin
  Result := False;
end;


class function TControllerRw7.restoreBackup(Path: string): string;
begin

end;

class function TControllerRw7.saveDiagnostics(Path: string; TimeOut: integer): string;
begin

end;


class function TControllerRw7.setOperationMode(aMode: string): string;
begin

end;

class function TControllerRw7.verifyBackup(Path: string): string;
begin

end;

class function TControllerRw7.verifyOption(aValue: string): boolean;
begin

end;

constructor TControllerRw7.Create(aConexion: TRobotConnection);
begin

end;

class function TControllerRw7.restartController(aRestartMode: TRestartModes): string;
begin

end;

class function TControllerRw7.setMotorsState(aMotorState: TMotorState): string;
begin

end;



end.
