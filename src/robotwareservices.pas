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
  protected
    procedure doMasterShip(Operation: string);
  public
    //Returns a list  RobotWare services. The content of the list depends on which services are installed
    procedure GetListServices(aList: TStringList);
    //Returns a list of all rapid tasks.
    procedure GetTasksList(aListItems: TCollection);
    procedure GetTasksList(aListTask: TStringList); overload;
  public
    procedure GetModulesList(TaksName: string; aListModule: TStringList);
  public //cfg domain
    procedure GetDomainList(aList: TStringList);
    procedure GetDomainDomain(aDomain: string; ListDomain: TStringList);
  public //MasterShip
    procedure RequestMastership;
    procedure ReleaseMastership;
    procedure RemoveMastership;
  public //rw/iosystem
    procedure GetNetWorksList(aListItems: TCollection);
    procedure GetNetWorksList(aList: TStringList);
    procedure GetDevicesList(aListItems: TCollection);
    procedure GetDevicesList(aList: TStringList);
    procedure GetSignalsList(aListItems: TCollection);
    procedure GetSignalsList(aList: TStringList);


  public
    constructor Create(aRobotConexion: TRobotConexion);
    destructor Destroy; override;
  end;

implementation

{ TRobotWareService }

procedure TRobotWareService.doMasterShip(Operation: string);
begin
  try
    FConexion.Post(FLocalUrl + Operation);
  except
    ErrorWebService('Error conexión. codigo: ' + FConexion.StatusText);
  end;
  if FConexion.StatusCode <> 204 then
  begin
    raise TAbbWebServicesError.Create(FConexion.StatusText);
  end;
end;

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

procedure TRobotWareService.GetTasksList(aListItems: TCollection);
begin

  try
    FConexion.Get(FLocalUrl + '/rapid/tasks');
  except
    ErrorWebService('Error conexión. codigo: ' + FConexion.StatusText);
  end;
  if FConexion.StatusCode = 200 then
  begin
    GetEmbeddedClassList(FConexion.Respuesta.Text, aListItems, TTaskItem, rap_task_li);
  end;

end;


procedure TRobotWareService.GetTasksList(aListTask: TStringList);
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
      GetEmbeddedClassList(FConexion.Respuesta.Text, Lista, TTaskItem, rap_task_li);
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

procedure TRobotWareService.GetModulesList(TaksName: string; aListModule: TStringList);
var
  Lista: TCollection;
  I: integer;
begin
  try
    FConexion.Get(FLocalUrl + '/rapid/tasks/' + TaksName + '/modules');
  except
    ErrorWebService('Error conexión. codigo: ' + FConexion.StatusText);
  end;

  Lista := TCollection.Create(TModuleInfoItem);

  try
    if FConexion.StatusCode = 200 then
    begin
      GetStatusClassList(FConexion.Respuesta.Text, Lista, TModuleInfoItem,
        rap_module_info_li);
      for I := 0 to Lista.Count - 1 do
      begin
        with Lista.Items[I] as TModuleInfoItem do
        begin
          aListModule.Add(Name);
        end;
      end;
    end;
  finally
    FreeAndNil(Lista);
  end;

end;

procedure TRobotWareService.GetDomainList(aList: TStringList);
var
  Lista: TListItems;
  I: integer;
begin
  try
    FConexion.Get(FLocalUrl + '/cfg');
  except
    ErrorWebService('Error conexión. codigo: ' + FConexion.StatusText);
  end;
  Lista := TListItems.Create(TResourceItem);
  try
    if FConexion.StatusCode = 200 then
    begin
      GetEmbeddedClassList(FConexion.Respuesta.Text, Lista, TResourceItem,
        cfg_domain_li);
      for I := 0 to Lista.Count - 1 do
      begin
        with Lista.Items[I] as TResourceItem do
        begin
          aList.Add(_title);
        end;
      end;
    end;
  finally
    FreeAndNil(Lista);
  end;

end;

procedure TRobotWareService.GetDomainDomain(aDomain: string; ListDomain: TStringList);
var
  Lista: TListItems;
  I: integer;
begin
  try
    FConexion.Get(FLocalUrl + '/cfg' + '/' + aDomain);
  except
    ErrorWebService('Error conexión. codigo: ' + FConexion.StatusText);
  end;
  Lista := TListItems.Create(TResourceItem);
  try
    if FConexion.StatusCode = 200 then
    begin
      GetEmbeddedClassList(FConexion.Respuesta.Text, Lista, TResourceItem, CFG_DT_LI);
      for I := 0 to Lista.Count - 1 do
      begin
        with Lista.Items[I] as TResourceItem do
        begin
          ListDomain.Add(_title);
        end;
      end;
    end;
  finally
    FreeAndNil(Lista);
  end;
end;

procedure TRobotWareService.RequestMastership;
begin
  doMasterShip('/mastership/request');
end;

procedure TRobotWareService.ReleaseMastership;
begin
  doMasterShip('/mastership/release');
end;

procedure TRobotWareService.RemoveMastership;
begin
  doMasterShip('mastership/watchdog');
end;

procedure TRobotWareService.GetNetWorksList(aListItems: TCollection);
var
  Lista: TCollection;
  I: integer;
  It, Elemento: TCollectionItem;
  jData, DataResources, TypeProperty: TJSONData;
  myJsonObject: TJSONObject;
begin
  try
    FConexion.Get(FLocalUrl + '/iosystem/networks');
  except
    ErrorWebService('Error conexión. codigo: ' + FConexion.StatusText);
  end;

  Lista := TCollection.Create(TIosNetworkItem);

  try
    if FConexion.StatusCode = 200 then
    begin

      GetEmbeddedClassList(FConexion.Respuesta.Text, aListItems,
        TIosNetworkItem, IOS_NETWORK_LI);

    end;
  finally
    FreeAndNil(Lista);
  end;

end;

procedure TRobotWareService.GetNetWorksList(aList: TStringList);
var
  Lista: TCollection;
  I: integer;
begin
  Lista := TCollection.Create(TIosNetworkItem);
  try
    GetNetworksList(Lista);
    for I := 0 to Lista.Count - 1 do
    begin
      with Lista.Items[I] as TIosNetworkItem do
      begin
        aList.Add(Name);
      end;
    end;
  finally
    Lista.Free;
  end;

end;

procedure TRobotWareService.GetDevicesList(aListItems: TCollection);
begin
  try
    FConexion.Get(FLocalUrl + '/iosystem/devices');
  except
    ErrorWebService('Error conexión. codigo: ' + FConexion.StatusText);
  end;

  if FConexion.StatusCode = 200 then
  begin
    GetEmbeddedClassList(FConexion.Respuesta.Text, aListItems,
      TIoDeviceItem, IOS_DEVICE_LI);
  end;

end;

procedure TRobotWareService.GetDevicesList(aList: TStringList);
var
  Lista: TCollection;
  I: integer;
begin
  Lista := TCollection.Create(TIoDeviceItem);
  try
    GetNetworksList(Lista);
    for I := 0 to Lista.Count - 1 do
    begin
      with Lista.Items[I] as TIoDeviceItem do
      begin
        aList.Add(Name);
      end;
    end;
  finally
    Lista.Free;
  end;

end;

procedure TRobotWareService.GetSignalsList(aListItems: TCollection);
begin
  try
    FConexion.Get(FLocalUrl + '/iosystem/signals');
  except
    ErrorWebService('Error conexión. codigo: ' + FConexion.StatusText);
  end;

  if FConexion.StatusCode = 200 then
  begin
    GetEmbeddedClassList(FConexion.Respuesta.Text, aListItems,
      TIoSignalItem, IOS_SIGNAL_LI);
  end;

end;

procedure TRobotWareService.GetSignalsList(aList: TStringList);
var
  Lista: TCollection;
  I: integer;
begin
  Lista := TCollection.Create(TIoSignalItem);
  try
    GetNetworksList(Lista);
    for I := 0 to Lista.Count - 1 do
    begin
      with Lista.Items[I] as TIoSignalItem do
      begin
        aList.Add(Name);
      end;
    end;
  finally
    Lista.Free;
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
