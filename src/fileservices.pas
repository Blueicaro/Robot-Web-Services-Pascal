unit FileServices;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils,
  abbconexion, abbwstypes;

type

  { TControllerServices }

  { TFileService }

  TFileService = class(TRobotConexion)
  private
    FLocalUrl: string;
    FConection: TRobotConexion;
  public
     procedure GetFile(Filename:string);
  public
    constructor Create(aRobotConexion: TRobotConexion);
    destructor Destroy; override;
  end;

implementation

{ TFileService }

procedure TFileService.GetFile(Filename: string);
begin
  try
    FConection.Get(FLocalUrl+'\'+Filename);
  except

  end;
end;

constructor TFileService.Create(aRobotConexion: TRobotConexion);
begin
  FLocalUrl := 'fileservice';
  FConection := aRobotConexion;
end;

destructor TFileService.Destroy;
begin
  inherited Destroy;
end;

end.
