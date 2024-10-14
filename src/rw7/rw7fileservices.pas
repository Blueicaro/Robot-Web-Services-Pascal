unit rw7fileservices;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils,
  abbconexion, abbwstypes;

type

  { TControllerServices }

  { TRw7FileService }

  TRw7FileService = class(TRobotConnection)
  private
    FLocalUrl: string;
    FConection: TRobotConnection;
  public
     procedure GetFile(Filename:string);
  public
    constructor Create(aRobotConexion: TRobotConnection);
    destructor Destroy; override;
  end;

implementation

{ TRw7FileService }

procedure TRw7FileService.GetFile(Filename: string);
begin
  try
    FConection.Get(FLocalUrl+'\'+Filename);
  except

  end;
end;

constructor TRw7FileService.Create(aRobotConexion: TRobotConnection);
begin
  FLocalUrl := 'fileservice';
  FConection := aRobotConexion;
end;

destructor TRw7FileService.Destroy;
begin
  inherited Destroy;
end;

end.
