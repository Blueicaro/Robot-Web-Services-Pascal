unit rw6fileservices;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils,abbconexion,rw6abbwstypes;

type

  { TRw6FileServices }

  TRw6FileServices = class
  private
    FLocalUrl: string;
    FConexion: TRobotConnection;
    public
      procedure Getfile(ruta:string; var modulo:TStringList);
  public
    constructor Create(aRobotConexion: TRobotConnection);
    destructor Destroy; override;
  end;

implementation

{ TRw6FileServices }

procedure TRw6FileServices.Getfile(ruta: string; var modulo: TStringList);
begin
  try
    FConexion.Get(FLocalUrl+'/'+ruta);
  except
     ErrorWebService('Error conexión. codigo: ' + FConexion.StatusText);
  end;

  if FConexion.StatusCode = 200 then
  begin
    modulo.Text:=FConexion.Respuesta.Text;
  end;

end;

constructor TRw6FileServices.Create(aRobotConexion: TRobotConnection);
begin
  FLocalUrl:='fileservice';
  FConexion:=aRobotConexion;
end;

destructor TRw6FileServices.Destroy;
begin
  FConexion:= nil;
  inherited Destroy;
end;

end.
