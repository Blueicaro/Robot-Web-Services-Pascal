unit controller;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, abbwstypes;

type
  TControllerBase = class(TComponent)
    class function isVirtualController: boolean; virtual; abstract;
    class function getControllerState: string virtual; abstract;
    class function setMotorsState: string; virtual; abstract;
    class function getOperationMode: string; virtual; abstract;
    class function setOperationMode(aMode: string): string; virtual; abstract;
    class function restartController: string; virtual; abstract;
    class function getEnvironmentVariable: string; virtual; abstract;
    class function getTimezone: string; virtual; abstract;
    class function getIdentity: string; virtual; abstract;
    class function getNetworkSettings: string; virtual; abstract;
    class function getNetworkConnections: string; virtual; abstract;
    class function verifyOption(aValue: string): boolean; virtual; abstract;
    class function createBackup(Path: string; TimeOut: integer): string; virtual; abstract;
    class function verifyBackup(Path: string): string; virtual; abstract;
    class function restoreBackup(Path: string): string; virtual; abstract;
    class function saveDiagnostics(Path: string; TimeOut: integer): string;
      virtual; abstract;
  end;

type

  { TControllerRw6 }

  TControllerRw6 = class(TControllerBase)
    class function createBackup(Path: string; TimeOut: integer): string; override;
    class function getControllerState: string virtual;
    class function getEnvironmentVariable: string; override;
    class function getIdentity: string; override;
    class function getNetworkConnections: string; override;
    class function getNetworkSettings: string; override;
    class function getOperationMode: string; override;
    class function getTimezone: string; override;
    class function isVirtualController: boolean; override;
    class function restartController: string; override;
    class function restoreBackup(Path: string): string; override;
    class function saveDiagnostics(Path: string; TimeOut: integer): string; override;
    class function setMotorsState: string; override;
    class function setOperationMode(aMode: string): string; override;
    class function verifyBackup(Path: string): string; override;
    class function verifyOption(aValue: string): boolean; override;
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
    class function restartController: string; override;
    class function restoreBackup(Path: string): string; override;
    class function saveDiagnostics(Path: string; TimeOut: integer): string; override;
    class function setMotorsState: string; override;
    class function setOperationMode(aMode: string): string; override;
    class function verifyBackup(Path: string): string; override;
    class function verifyOption(aValue: string): boolean; override;

  end;

type
  TController = TControllerBase;



implementation

{ TControllerRw6 }

class function TControllerRw6.createBackup(Path: string; TimeOut: integer): string;
begin

end;

class function TControllerRw6.getControllerState: string;
begin

end;

class function TControllerRw6.getEnvironmentVariable: string;
begin

end;

class function TControllerRw6.getIdentity: string;
begin

end;

class function TControllerRw6.getNetworkConnections: string;
begin

end;

class function TControllerRw6.getNetworkSettings: string;
begin

end;

class function TControllerRw6.getOperationMode: string;
begin

end;

class function TControllerRw6.getTimezone: string;
begin

end;

class function TControllerRw6.isVirtualController: boolean;
begin
  Result := True;
end;

class function TControllerRw6.restartController: string;
begin

end;

class function TControllerRw6.restoreBackup(Path: string): string;
begin

end;

class function TControllerRw6.saveDiagnostics(Path: string; TimeOut: integer): string;
begin

end;

class function TControllerRw6.setMotorsState: string;
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

class function TControllerRw7.restartController: string;
begin

end;

class function TControllerRw7.restoreBackup(Path: string): string;
begin

end;

class function TControllerRw7.saveDiagnostics(Path: string; TimeOut: integer): string;
begin

end;

class function TControllerRw7.setMotorsState: string;
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

end.
