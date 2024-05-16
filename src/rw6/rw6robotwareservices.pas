unit rw6robotwareservices;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, abbconexion, rw6abbwstypes;

type

  { TRw6RobotWareServices }

  TRw6RobotWareServices = class
  private
    FLocalUrl: string;
    FConexion: TRobotConnection;
  public
    procedure GetRobotWareservices(aServicesList: TRw6RwServiceList);
    procedure GetTaskList(aListTaskList: TRw6TaskList);
    procedure GetRapidModules(aModulesList: TRw6ModuleInfoList; aTaskName: string);
    procedure GetModuleText(aModuleName: string; aTaskName: string;
      var ModuleContent: TRw6ModuleTextItem);
    procedure UpdateRapidVariable;
  public
    function GetOperationMode: TOpMode;
    function MastershipRequest: boolean;
    function MastershipRelease: boolean;
  public
    constructor Create(aRobotConexion: TRobotConnection);
    destructor Destroy; override;
  end;

implementation

{ TRw6RobotWareServices }



procedure TRw6RobotWareServices.GetRobotWareservices(aServicesList: TRw6RwServiceList);
begin
  try
    FConexion.Get(FLocalUrl + '?json=1');
  except
    ErrorWebService('Error conexión. codigo: ' + FConexion.StatusText);
  end;
  if FConexion.StatusCode = 200 then
  begin
    GetEmbeddedStateList(FConexion.Respuesta.Text, aServicesList as
      TCollection, TRw6RwServiceItem, RWSERVICE_LI);
  end;
end;

procedure TRw6RobotWareServices.GetTaskList(aListTaskList: TRw6TaskList);
begin
  try
    FConexion.Get(FLocalUrl + '/rapid/tasks?json=1');
  except
    ErrorWebService('Error conexión. codigo: ' + FConexion.StatusText);
  end;
  if FConexion.StatusCode = 200 then
  begin
    GetEmbeddedStateList(FConexion.Respuesta.Text, aListTaskList as
      TCollection, TRw6TaskItem, rap_task_li);
  end;
end;

procedure TRw6RobotWareServices.GetRapidModules(aModulesList: TRw6ModuleInfoList;
  aTaskName: string);
begin
  try
    //http://127.0.0.1/rw/rapid/modules?task=T_ROB1&json=1
    FConexion.Get(FLocalUrl + '/rapid/modules?task=' + aTaskName + '&json=1');
  except
    ErrorWebService('Error conexión. codigo: ' + FConexion.StatusText);
  end;
  if FConexion.StatusCode = 200 then
  begin
    GetEmbeddedStateList(FConexion.Respuesta.Text, aModulesList as
      TCollection, TRw6ModuleInfoItem, RAP_MODULE_INFO_LI);
  end;
end;

procedure TRw6RobotWareServices.GetModuleText(aModuleName: string;
  aTaskName: string; var ModuleContent: TRw6ModuleTextItem);
var
  ModuleInfo: TRw6ModuleTextList;
begin
  //http://localhost/rw/rapid/modules/User?resource=module-text&task=T_ROB1&json=1
  try
    FConexion.Get(FLocalUrl + '/rapid/modules/' + aModuleName +
      '?resource=module-text&task=' + aTaskName + '&json=1');
  except
    ErrorWebService('Error conexión. codigo: ' + FConexion.StatusText);
  end;
  try
    ModuleInfo := TRw6ModuleTextList.Create;
    if FConexion.StatusCode = 200 then
    begin
      GetEmbeddedStateList(FConexion.Respuesta.Text, ModuleInfo as
        TCollection, TRw6ModuleTextItem, RAP_MODULE_TEXT);
    end;

    ModuleContent.Assign(ModuleInfo[0]);

  finally
    FreeAndNil(ModuleInfo);
  end;
end;

procedure TRw6RobotWareServices.UpdateRapidVariable;
var
  Parametros: String;
begin
  Parametros:= '[0,0,60,200,"Cordón 40",3,TRUE,0.01,200,0,0,0,TRUE]'  ;
  FConexion.Post('/rw/rapid/symbol/data/RAPID/T_ROB1/Datos/M1_C03?action=set','value='+Parametros);
  //FConexion.Post('/rw/rapid/symbol/data/RAPID/T_ROB1/user/reg1?action=set');
end;

function TRw6RobotWareServices.GetOperationMode: TOpMode;
begin
  try
    FConexion.Get(FLocalUrl + '/panel/opmode?json=1');
  except
    ErrorWebService('Error conexión. codigo: ' + FConexion.StatusText);
  end;
  if FConexion.StatusCode = 200 then
  begin
    try
     Result := LeerModoFuncionamiento(FConexion.Respuesta.Text);
    finally

    end;
  end;
end;

function TRw6RobotWareServices.MastershipRequest: boolean;
begin
  Result := False;
  try
    FConexion.Post(FLocalUrl + '/mastership?action=request');
  except
    ErrorWebService('Error conexión. codigo: ' + FConexion.StatusText);
  end;
  if FConexion.StatusCode = 204 then
  begin
    Result := True;
  end;
end;

function TRw6RobotWareServices.MastershipRelease: boolean;
begin
  //mastership?action=release
  Result := False;
  try
    FConexion.Post(FLocalUrl + '/mastership?action=release');
  except
    ErrorWebService('Error conexión. codigo: ' + FConexion.StatusText);
  end;
  if FConexion.StatusCode = 204 then
  begin
    Result := True;
  end;
end;

constructor TRw6RobotWareServices.Create(aRobotConexion: TRobotConnection);
begin
  FConexion := aRobotConexion;
  FLocalUrl := 'rw';
end;

destructor TRw6RobotWareServices.Destroy;
begin
  FConexion := nil;
  inherited Destroy;
end;

end.
