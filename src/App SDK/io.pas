unit io;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, robotwaredata, abbconexion;

type

  { TRapidBase }

  TIoBase = class(TBase)
    class function GetNetWork(aNetWorkName: string): TNetWork; virtual; abstract;
  end;

type

  { TIoRw6 }

  TIoRw6 = class(TIoBase)
    constructor Create(aRobotConexion: TRobotConnection); override;
    destructor Destroy; override;
    class function GetNetWork(aNetWorkName: string): TNetWork; override;
  end;

type
  TIo = TIoBase;

implementation

{ TIoRw6 }

constructor TIoRw6.Create(aRobotConexion: TRobotConnection);
begin
  FConexion := aRobotConexion;
end;

destructor TIoRw6.Destroy;
begin
  FreeAndNil(FConexion);
  inherited Destroy;
end;

class function TIoRw6.GetNetWork(aNetWorkName: string): TNetWork;
begin
   //Try
   //  FConexion.Get (''');
   //Except
   //  Raise TAbbWebServicesError.Create('Error en conexión');
   //end;
end;

end.
