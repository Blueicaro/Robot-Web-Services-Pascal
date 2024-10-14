unit rw7controllerservices;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils,
  abbconexion,abbwstypes;

type

  { TRw7ControllerServices }

  TRw7ControllerServices = class(TRobotConnection)
  private
    FLocalUrl: string;
    FConection: TRobotConnection;
  public
    function GetVariableEnviroment(aVar: string): string;
    procedure GetListOfServices(aList: TStringList);
  public
    constructor Create(aRobotConexion: TRobotConnection);
    destructor Destroy; override;
  end;

implementation


{ TRw7ControllerServices }

function TRw7ControllerServices.GetVariableEnviroment(aVar: string): string;
begin
  Result := '';
  if not Assigned(FConection) then
  begin
    ErrorWebService('GetListServices. Missing RobotConexion');
  end;
  try
    FConection.Get(FLocalUrl + '$' + aVar);
    if FConection.StatusCode = 200 then
    begin
      { #todo : Pediente de terminar }
    end;
  finally
  end;
end;

procedure TRw7ControllerServices.GetListOfServices(aList: TStringList);
begin
  if not Assigned(FConection) then
  begin
    ErrorWebService('GetListServices. Missing RobotConexion');
  end;
  try
    FConection.Get(FLocalUrl);
    if FConection.StatusCode = 200 then
    begin
      //debugln(FConection.Respuesta.Text);
      GetListResources(FConection.Respuesta, aList);
    end
    else
    begin
      ErrorWebService('GetListServices. Error code: ' + FConection.StatusText);
    end;
  except
    on E: Exception do
    begin
      ErrorWebService(E.Message);
    end;
  end;

end;

constructor TRw7ControllerServices.Create(aRobotConexion: TRobotConnection);
begin
  inherited Create;
  FLocalUrl := 'ctrl';
  FConection := aRobotConexion;
end;

destructor TRw7ControllerServices.Destroy;
begin
  inherited Destroy;
end;

end.
