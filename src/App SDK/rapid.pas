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

  { TRapidRw7 }

  TRapidRw7 = class(TRapidBase)
    constructor Create(aConexion: TRobotConnection);
    destructor Destroy; override;
    class function GetTasks: TTaskList; override;
  end;

type
  TRapid = TRapidBase;


implementation

uses fpjson;


  { TRapidRw7 }

constructor TRapidRw7.Create(aConexion: TRobotConnection);
begin
  FConexion := aConexion;
end;

destructor TRapidRw7.Destroy;
begin
  FConexion := nil;
  inherited Destroy;
end;

class function TRapidRw7.GetTasks: TTaskList;
var
  aItem: TTaskItem;
  I: integer;
  Propiedades: TTaksProperties;
begin
  Result := TTaskList.Create;
  try
    FConexion.Get('rw/rapid/tasks');
  except
    raise TAbbWebServicesError.Create('Error de conexión');
  end;

  for I := 0 to FConexion.ResourcesCount - 1 do
  begin
    if FConexion.GetResourceItem('_type', I) = 'rap-task-li' then
    begin
      aItem := TTaskItem.Create;
      aItem.GetName := FConexion.GetResourceName(I);
      aItem.href := FConexion.GetHref(I);
      aItem.RobotConexion := FConexion;
      Result.Add(aItem);
    end;
  end;

end;

end.
