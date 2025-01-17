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
    class function GetControllerState: string virtual; abstract;
    class function setMotorsState(aMotorState: TMotorState): string; virtual; abstract;
    class function getOperationMode: string; virtual; abstract;
    class function setOperationMode(aMode: string): string; virtual; abstract;
    class function restartController(aRestartMode: TRestartModes): string;
      virtual; abstract;
    class function getEnvironmentVariable: string; virtual; abstract;
    class function getTimezone: string; virtual; abstract;
    class function GetIdentity: string; virtual; abstract;
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

  { TControllerRw7 }

  TControllerRw7 = class(TControllerBase)
    class function createBackup(Path: string; TimeOut: integer): string; override;
    class function GetControllerState: string override;
    class function GetEnvironmentVariable: string; override;
    class function GetIdentity: string; override;
    class function GetNetworkConnections: string; override;
    class function GetNetworkSettings: string; override;
    class function GetOperationMode: string; override;
    class function GetTimezone: string; override;
    class function IsVirtualController: boolean; override;
    class function RestoreBackup(Path: string): string; override;
    class function SaveDiagnostics(Path: string; TimeOut: integer): string; override;
    class function SetOperationMode(aMode: string): string; override;
    class function VerifyBackup(Path: string): string; override;
    class function VerifyOption(aValue: string): boolean; override;
    class function RestartController(aRestartMode: TRestartModes): string; override;
    class function SetMotorsState(aMotorState: TMotorState): string; override;
  public
    constructor Create(aConexion: TRobotConnection);
    destructor Destroy; override;
    class function GetTime: string; override;
  end;

type
  TController = TControllerBase;




implementation


{ TControllerRw7 }

class function TControllerRw7.createBackup(Path: string; TimeOut: integer): string;
begin

end;

class function TControllerRw7.GetControllerState: string;
begin
  Result := '';
  try
    FConexion.Get('rw/panel/ctrl-state');
  except
    raise TAbbWebServicesError.Create('Error de conexión');
  end;
  Result := FConexion.GetStateName('ctrlstate');
end;

class function TControllerRw7.GetEnvironmentVariable: string;
begin

end;

class function TControllerRw7.GetIdentity: string;
begin
  try
    FConexion.Get('ctrl/identity');
  except
    raise TAbbWebServicesError.Create('Error de conexión');
  end;
  Result := FConexion.GetStateName('ctrl-name');
end;

class function TControllerRw7.GetNetworkConnections: string;
begin

end;

class function TControllerRw7.GetNetworkSettings: string;
begin

end;

class function TControllerRw7.GetOperationMode: string;
begin
  try
    FConexion.Get('rw/panel/opmode');
  except
    raise TAbbWebServicesError.Create('Error de conexión');
  end;
  Result := FConexion.GetStateName('opmode');
end;

class function TControllerRw7.GetTimezone: string;
begin
  Result := '';
  try
    FConexion.Get('ctrl/clock/timezone');
  except
    raise TAbbWebServicesError.Create('Error de conexión');
  end;
  Result := FConexion.GetCodeError;
  if Result = '' then
  begin
    Result := FConexion.GetResourceItem('timezone');
  end;
end;

class function TControllerRw7.IsVirtualController: boolean;
var
  cadena: string;
begin
  Result := False;
  try
    FConexion.Get('ctrl/identity');
  except
    raise TAbbWebServicesError.Create('Error de conexión');
  end;
  cadena := FConexion.GetStateName('ctrl-type');
  Result := 'VIRTUAL_CONTROLLER' = Cadena;
end;


class function TControllerRw7.RestoreBackup(Path: string): string;
begin

end;

class function TControllerRw7.SaveDiagnostics(Path: string; TimeOut: integer): string;
begin

end;


class function TControllerRw7.SetOperationMode(aMode: string): string;
begin

end;

class function TControllerRw7.VerifyBackup(Path: string): string;
begin

end;

class function TControllerRw7.VerifyOption(aValue: string): boolean;
begin

end;

constructor TControllerRw7.Create(aConexion: TRobotConnection);
begin
  FConexion := aConexion;
end;

destructor TControllerRw7.Destroy;
begin
  FConexion := nil;
  inherited Destroy;
end;

class function TControllerRw7.GetTime: string;
begin
  try
    FConexion.Get('ctrl/clock');
  except
    raise TAbbWebServicesError.Create('Error de conexión');
  end;
  Result := FConexion.GetStateName('datetime');
end;

class function TControllerRw7.RestartController(aRestartMode: TRestartModes): string;
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
    FConexion.Post('ctrl/restart', modo);
  except
    raise TAbbWebServicesError.Create('Error de conexión');
  end;
  Result := FConexion.GetCodeError;
end;

class function TControllerRw7.SetMotorsState(aMotorState: TMotorState): string;
begin

end;



end.
