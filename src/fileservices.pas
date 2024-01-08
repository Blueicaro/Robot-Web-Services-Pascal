unit FileServices;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils,
  abbconexion, abbwstypes;

type

  { TControllerServices }

  { TFileService }

  TFileService = class(TRobotConnection)
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

{ TFileService }

procedure TFileService.GetFile(Filename: string);
begin
  try
    FConection.Get(FLocalUrl+'\'+Filename);
  except

  end;
end;

constructor TFileService.Create(aRobotConexion: TRobotConnection);
begin
  FLocalUrl := 'fileservice';
  FConection := aRobotConexion;
end;

destructor TFileService.Destroy;
begin
  inherited Destroy;
end;

end.
