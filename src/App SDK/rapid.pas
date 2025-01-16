unit rapid;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, robotwaredata, abbconexion;

type

  { TRapidBase }

  TRapidBase = class(TBase)
    class function GetTasks: TTaskList; virtual; abstract;
  end;

type

  { TRapidRw6 }

  TRapidRw6 = class(TRapidBase)
  public
    constructor Create(aConexion: TRobotConnection);
    destructor Destroy; override;
    class function GetTasks: TTaskList; override;
  end;

type

  { TRapidRw7 }

  TRapidRw7 = class(TRapidBase)
    constructor Create(aConexion: TRobotConnection);
    class function GetTasks: TTaskList; override;
  end;

type
  TRapid = TRapidBase;


implementation

uses fpjson;
  { TRapidRw6 }

constructor TRapidRw6.Create(aConexion: TRobotConnection);
begin
  FConexion := aConexion;
end;

destructor TRapidRw6.Destroy;
begin
  FConexion := nil;
  inherited Destroy;
end;

class function TRapidRw6.GetTasks: TTaskList;
var
  aItem: TTaskItem;
  I: integer;
begin
  Result := TTaskList.Create;
  try
    FConexion.Get('rw/rapid/tasks?json=1');
  except
    raise TAbbWebServicesError.Create('Error de conexión');
  end;

  for I := 0 to FConexion.GetLengthArray-1 do
  begin
    aItem := TTaskItem.Create;
    aItem.GetName := FConexion.GetName(I);
    aItem.href := FConexion.GetHref(I);
    aItem.RobotConexion:=FConexion;
    Result.Add(aItem);
  end;
end;

{ TRapidRw7 }

constructor TRapidRw7.Create(aConexion: TRobotConnection);
begin

end;

class function TRapidRw7.GetTasks: TTaskList;
begin

end;

end.
