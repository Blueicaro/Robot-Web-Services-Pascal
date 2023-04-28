unit robotwareservices;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, abbwstypes, TypInfo,
  fpjson, abbconexion;

type

  { TRobotWareService }

  TRobotWareService = class
  private
    FLocalUrl: string;
    FConexion: TRobotConexion;
  public
    //Returns a list  RobotWare services. The content of the list depends on which services are installed
    procedure GetListServices(aList: TStringList);
    //Returns a list of all rapid tasks.
    procedure GetListTasks(aListItems: TCollection);
    procedure GetListTasks(aListTask: TStringList); overload;
  public
    procedure GetListModules(aListModule: TStringList);
  public
    constructor Create(aRobotConexion: TRobotConexion);
    destructor Destroy; override;
  end;

implementation

{ TRobotWareService }

procedure TRobotWareService.GetListServices(aList: TStringList);
begin

  try
    FConexion.Get(FLocalUrl);
    if FConexion.StatusCode = 200 then
    begin
      FConexion.GetListResources(FConexion.Respuesta, aList);
    end
    else
    begin
      ErrorWebService('GetListServices. Error code: ' + FConexion.StatusText);
    end;
  except
    on E: Exception do
    begin
      ErrorWebService(E.Message);
    end;
  end;

end;

{
          "_title": "T_ROB1",
               "_type": "rap-task-li",
               "active": "On",
               "excstate": "ready",
               "motiontask": "TRUE",
               "name": "T_ROB1",
               "taskstate": "linked",
               "type": "normal"}
procedure TRobotWareService.GetListTasks(aListItems: TCollection);
var
  I, X: integer;
  //RapTaskObject: TRapTaskObject;
  Cadena: TJSONStringType;
  jData, DataResources: TJSONData;
  myJsonObject: TJSONObject;
  NombreClave: string;
  propInfo: PPropInfo;
begin

  //if not Assigned(FConexion) then
  //begin
  //  ErrorWebService('Missing RobotConexion: ' + {$I %CURRENTROUTINE%});
  //end;
  //try
  //  FConexion.Get(FLocalUrl + '/rapid/tasks');
  //  if FConexion.StatusCode = 200 then
  //  begin
  //    jData := GetJSON(FConexion.Respuesta.Text);
  //    myJsonObject := jData as TJSONObject;
  //    DataResources := myJsonObject.GetPath('_embedded').GetPath('resources');
  //    if DataResources = nil then
  //    begin
  //      ErrorWebService('No se puede procesar la respuesta');
  //    end;
  //    for I := 0 to DataResources.Count - 1 do
  //      if DataResources.Items[I].FindPath('_type').AsString = 'rap-task-li' then
  //      begin
  //        RapTaskObject := TRapTaskObject.Create;
  //        for X := 2 to DataResources.Items[I].Count - 1 do
  //        begin
  //          if DataResources.Items[I].JSONType = jtObject then
  //          begin
  //            Cadena := DataResources.Items[I].Items[X].AsString;
  //            NombreClave := TJSONObject(DataResources.Items[I]).Names[X];
  //            propInfo := GetPropInfo(RapTaskObject, NombreClave);
  //            if propInfo <> nil then
  //            begin
  //              SetPropValue(RapTaskObject, propInfo, Cadena);
  //            end;
  //          end;
  //        end;
  //        aListTask.Add(RapTaskObject);
  //      end;
  //    jData.Free;
  //  end
  //  else
  //  begin
  //    ErrorWebService('GetListServices. Error code: ' + FConexion.StatusText);
  //  end;
  //except
  //  on E: Exception do
  //  begin
  //    ErrorWebService(E.Message);
  //  end;
  //end;

end;


procedure TRobotWareService.GetListTasks(aListTask: TStringList);
var
  I: integer;
  Lista: TCollection;
begin

  try
    FConexion.Get(FLocalUrl + '/rapid/tasks');
  except
    ErrorWebService('Error conexión. codigo: ' + FConexion.StatusText);
  end;


  Lista := TCollection.Create(TTaskItem);
  try
    if FConexion.StatusCode = 200 then
    begin
      GetClassList(FConexion.Respuesta.Text, Lista, TTaskItem);
    end;
    for I := 0 to Lista.Count - 1 do
    begin
      with Lista.Items[I] as TTaskItem do
      begin
        aListTask.Add(Name);
      end;
    end;
  finally
    FreeAndNil(Lista);
  end;

end;

procedure TRobotWareService.GetListModules(aListModule: TStringList);
begin
   try
    FConexion.Get(FLocalUrl + '/rapid/tasks');
  except
    ErrorWebService('Error conexión. codigo: ' + FConexion.StatusText);
  end;
end;

constructor TRobotWareService.Create(aRobotConexion: TRobotConexion);
begin
  FConexion := aRobotConexion;
  FLocalUrl := 'rw';

end;

destructor TRobotWareService.Destroy;
begin
  FConexion := nil;
  inherited Destroy;
end;



end.
