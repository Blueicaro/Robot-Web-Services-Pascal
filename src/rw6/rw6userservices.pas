unit rw6userservices;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils,abbconexion;

type

  { TRw6UserServices }

  TRw6UserServices = class
  private
    FLocalUrl: string;
    FConexion: TRobotConnection;
  public
    function RequestRMMP: boolean;
  public
    constructor Create(aRobotConexion: TRobotConnection);
    destructor Destroy; override;
  end;

implementation

{ TRw6UserServices }

function TRw6UserServices.RequestRMMP: boolean;
begin
  FConexion.Post('/rmmp?json=1');
end;

constructor TRw6UserServices.Create(aRobotConexion: TRobotConnection);
begin
  FConexion := aRobotConexion;
  FLocalUrl := 'users';
end;

destructor TRw6UserServices.Destroy;
begin
  FConexion := nil;
  inherited Destroy;
end;

end.
