unit rw7robotwareservices;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Rw7abbwstypes, TypInfo,
  fpjson, abbconexion;

type

  { TRw7RobotWareService }

  TRw7RobotWareService = class
  private
    FLocalUrl: string;
    FConexion: TRobotConnection;
  protected
    procedure doMasterShip(Operation: string);
  public
    procedure GetListServices(aList: TStringList);
    procedure GetTasksList(aListItems: TCollection);
    procedure GetTasksList(aListTask: TStringList); overload;
  public
    //Lista de módulos en dentro de la tarea pasada como parámetro
    procedure GetModulesList(TaksName: string; aListModule: TStringList);
    //Devuelve el contenido del un modulo
    procedure GetModuleText(TaskName, ModuleName: string; aListContent: TStringList);
  public
    procedure GetDomainList(aList: TStringList);
    procedure GetDomainDomain(aDomain: string; ListDomain: TStringList);
  public

    procedure RequestMastership;
    procedure ReleaseMastership;
    procedure RemoveMastership;
  public
    procedure GetNetWorksList(aListItems: TCollection);
    procedure GetNetWorksList(aList: TStringList);
    procedure GetDevicesList(aListItems: TCollection);
    procedure GetDevicesList(aList: TStringList);

    procedure GetSignalsList(aList: TStringList);
    procedure GetSignalsList(aListItems: TRw7IoSignalList);
  public
    function GetSystemInfo: TRw7SysSystemInfo;
    function GetRobotType: string; //Obtiene el tipo de manipulador
    function GetSystemLicence: string; //Obtiene la licencia del robot
    procedure GetSystemProducts(aListItems: TCollection); //Obtiene la lista de productos
    procedure GetSystemProducts(aLista: TStringList); overload;
    procedure GetSystemOptions(aLista: TStringList); //Obtiene la lista de opciones
  public
    constructor Create(aRobotConexion: TRobotConnection);
    destructor Destroy; override;
  end;

implementation

uses StrUtils;

  { TRw7RobotWareService }

procedure TRw7RobotWareService.doMasterShip(Operation: string);
begin
  try
    FConexion.Post(FLocalUrl + Operation);
  except
    ErrorWebService('Error conexión. codigo: ' + FConexion.StatusText);
  end;
  if FConexion.StatusCode <> 204 then
  begin
    raise TRw7AbbWebServicesError.Create(FConexion.StatusText);
  end;
end;

procedure TRw7RobotWareService.GetListServices(aList: TStringList);
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

procedure TRw7RobotWareService.GetTasksList(aListItems: TCollection);
begin

  try
    FConexion.Get(FLocalUrl + '/rapid/tasks');
  except
    ErrorWebService('Error conexión. codigo: ' + FConexion.StatusText);
  end;
  if FConexion.StatusCode = 200 then
  begin
    GetEmbeddedClassList(FConexion.Respuesta.Text, aListItems, TRw7TaskItem, rap_task_li);
  end;

end;


procedure TRw7RobotWareService.GetTasksList(aListTask: TStringList);
var
  I: integer;
  Lista: TCollection;
begin

  try
    FConexion.Get(FLocalUrl + '/rapid/tasks');
  except
    ErrorWebService('Error conexión. codigo: ' + FConexion.StatusText);
  end;


  Lista := TCollection.Create(TRw7TaskItem);
  try
    if FConexion.StatusCode = 200 then
    begin
      GetEmbeddedClassList(FConexion.Respuesta.Text, Lista, TRw7TaskItem, rap_task_li);
    end;
    for I := 0 to Lista.Count - 1 do
    begin
      with Lista.Items[I] as TRw7TaskItem do
      begin
        aListTask.Add(Name);
      end;
    end;
  finally
    FreeAndNil(Lista);
  end;

end;

procedure TRw7RobotWareService.GetModulesList(TaksName: string; aListModule: TStringList);
var
  Lista: TCollection;
  I: integer;
begin
  try
    FConexion.Get(FLocalUrl + '/rapid/tasks/' + TaksName + '/modules');
  except
    ErrorWebService('Error conexión. codigo: ' + FConexion.StatusText);
  end;

  Lista := TCollection.Create(TRw7ModuleInfoItem);

  try
    if FConexion.StatusCode = 200 then
    begin
      GetStatusClassList(FConexion.Respuesta.Text, Lista, TRw7ModuleInfoItem,
        rap_module_info_li);
      for I := 0 to Lista.Count - 1 do
      begin
        with Lista.Items[I] as TRw7ModuleInfoItem do
        begin
          aListModule.Add(Name);
        end;
      end;
    end;
  finally
    FreeAndNil(Lista);
  end;

end;

procedure TRw7RobotWareService.GetModuleText(TaskName, ModuleName: string;
  aListContent: TStringList);
var
  Lista: TCollection;
  fichero: string;
begin
  //Ejemplo: https://localhost:80/rw/rapid/tasks/T_IFM/modules/IFM/text
  try
    FConexion.Get(FLocalUrl + '/rapid/tasks/' + TaskName + '/modules/' +
      ModuleName + '/text');
  except
    ErrorWebService('Error conexión. codigo: ' + FConexion.StatusText);
  end;
  try
     {
     Por algún motivo desconocido, el controlador, hay veces
     que el campo "file-path" lo devuelve con un formato incorrecto. Por ejemplo
     "file-path": ""/TEMP/pusres.711130""
     Por hay que comprobarlo y modificaro
     }
    FConexion.Respuesta.Text :=
      StringReplace(FConexion.Respuesta.Text, '""', '"', [rfIgnoreCase, rfReplaceAll]);

    Lista := TCollection.Create(TRw7ModuleTextItem);
    try
      GetStatusClassList(FConexion.Respuesta.Text, Lista, TRw7ModuleTextItem,
        RAP_MODULE_TEXT);
    finally
      if Lista.Count = 1 then
      begin
        with Lista.Items[0] as TRw7ModuleTextItem do
        begin
          if module_text <> '' then
          begin
            aListContent.Text := module_text;
          end
          else
          begin
            fichero := file_path;
            RemovePadChars(fichero, ['"']);
            FConexion.Get('fileservice/' + fichero);
            if FConexion.StatusCode = 200 then
            begin
              aListContent.Text := FConexion.Respuesta.Text;
            end;
          end;
        end;
      end;
    end;


  finally
    FreeAndNil(Lista);
  end;
end;

procedure TRw7RobotWareService.GetDomainList(aList: TStringList);
var
  Lista: TListItems;
  I: integer;
begin
  try
    FConexion.Get(FLocalUrl + '/cfg');
  except
    ErrorWebService('Error conexión. codigo: ' + FConexion.StatusText);
  end;
  Lista := TListItems.Create(TRw7ResourceItem);
  try
    if FConexion.StatusCode = 200 then
    begin
      GetEmbeddedClassList(FConexion.Respuesta.Text, Lista, TRw7ResourceItem,
        cfg_domain_li);
      for I := 0 to Lista.Count - 1 do
      begin
        with Lista.Items[I] as TRw7ResourceItem do
        begin
          aList.Add(_title);
        end;
      end;
    end;
  finally
    FreeAndNil(Lista);
  end;

end;

procedure TRw7RobotWareService.GetDomainDomain(aDomain: string; ListDomain: TStringList);
var
  Lista: TListItems;
  I: integer;
begin
  try
    FConexion.Get(FLocalUrl + '/cfg' + '/' + aDomain);
  except
    ErrorWebService('Error conexión. codigo: ' + FConexion.StatusText);
  end;
  Lista := TListItems.Create(TRw7ResourceItem);
  try
    if FConexion.StatusCode = 200 then
    begin
      GetEmbeddedClassList(FConexion.Respuesta.Text, Lista, TRw7ResourceItem, CFG_DT_LI);
      for I := 0 to Lista.Count - 1 do
      begin
        with Lista.Items[I] as TRw7ResourceItem do
        begin
          ListDomain.Add(_title);
        end;
      end;
    end;
  finally
    FreeAndNil(Lista);
  end;
end;

procedure TRw7RobotWareService.RequestMastership;
begin
  doMasterShip('/mastership/request');
end;

procedure TRw7RobotWareService.ReleaseMastership;
begin
  doMasterShip('/mastership/release');
end;

procedure TRw7RobotWareService.RemoveMastership;
begin
  doMasterShip('mastership/watchdog');
end;

procedure TRw7RobotWareService.GetNetWorksList(aListItems: TCollection);
var
  Lista: TCollection;
begin
  try
    FConexion.Get(FLocalUrl + '/iosystem/networks');
  except
    ErrorWebService('Error conexión. codigo: ' + FConexion.StatusText);
  end;

  Lista := TCollection.Create(TRw7IosNetworkItem);

  try
    if FConexion.StatusCode = 200 then
    begin

      GetEmbeddedClassList(FConexion.Respuesta.Text, aListItems,
        TRw7IosNetworkItem, IOS_NETWORK_LI);

    end;
  finally
    FreeAndNil(Lista);
  end;

end;

procedure TRw7RobotWareService.GetNetWorksList(aList: TStringList);
var
  Lista: TCollection;
  I: integer;
begin
  Lista := TCollection.Create(TRw7IosNetworkItem);
  try
    GetNetworksList(Lista);
    for I := 0 to Lista.Count - 1 do
    begin
      with Lista.Items[I] as TRw7IosNetworkItem do
      begin
        aList.Add(Name);
      end;
    end;
  finally
    Lista.Free;
  end;

end;

procedure TRw7RobotWareService.GetDevicesList(aListItems: TCollection);
var
  cadena: string;
begin
  try

    FConexion.Get(FLocalUrl + '/iosystem/devices');
  except
    ErrorWebService('Error conexión. codigo: ' + FConexion.StatusText);
  end;

  if FConexion.StatusCode = 200 then
  begin
    GetEmbeddedClassList(FConexion.Respuesta.Text, aListItems,
      TRw7IoDeviceItem, IOS_DEVICE_LI);
  end;

end;

procedure TRw7RobotWareService.GetDevicesList(aList: TStringList);
var
  Lista: TCollection;
  I: integer;
begin
  Lista := TCollection.Create(TRw7IoDeviceItem);
  try
    GetDevicesList(Lista);
    for I := 0 to Lista.Count - 1 do
    begin

      with Lista.Items[I] as TRw7IoDeviceItem do
      begin
        aList.Add(Name);
      end;
    end;
  finally
    Lista.Free;
  end;

end;

procedure TRw7RobotWareService.GetSignalsList(aListItems: TRw7IoSignalList);
var
  Lista: TCollection;
  cadena: String;
begin

  try
    FConexion.Get(FLocalUrl + '/iosystem/signals');
  except
    ErrorWebService('Error conexión. codigo: ' + FConexion.StatusText);
  end;

  if FConexion.StatusCode = 200 then
  begin
    try
      Lista := TCollection.Create(TRw7IoSignalItem);
      GetEmbeddedClassList(FConexion.Respuesta.Text, Lista,
        TRw7IoSignalItem, IOS_SIGNAL_LI);
      aListItems.Assign(Lista);
    finally
      FreeAndNil(Lista);
    end;
  end;

end;

procedure TRw7RobotWareService.GetSignalsList(aList: TStringList);
var
  I: integer;
  Lista: TRw7IoSignalList;
begin

  try
    Lista := TRw7IoSignalList.Create;
    GetSignalsList(Lista);
    for I := 0 to Lista.Count - 1 do
    begin
      with Lista.Items[I] as TRw7IoSignalItem do
      begin
        aList.Add(Name);
      end;
    end;
  finally
    Lista.Free;
  end;

end;

function TRw7RobotWareService.GetSystemInfo: TRw7SysSystemInfo;
var
  Lista: TCollection;
begin
  try
    FConexion.Get(FLocalUrl + '/system');
  except
    ErrorWebService('Error conexión. codigo: ' + FConexion.StatusText);
  end;
  try
    Lista := TCollection.Create(TRw7SysSytemItem);
    GetStatusClassList(FConexion.Respuesta.Text, Lista, TRw7SysSytemItem, SYS_SYSTEM);
    if Lista.Count = 1 then
    begin
      with Lista.Items[0] as TRw7SysSytemItem do
      begin
        Result.Build := Build;
        Result.BuildTag := BuildTag;
        Result.Date := Date;
        Result.Description := Description;
        Result.Name := Name;
        Result.Revision := Revision;
        Result.RobapiCompatibilityRevison := robapi_compatible;
        Result.RwVersion := RwVersion;
        Result.RwVersionName := RwVersionName;
        Result.StartTm := StartTm;
        Result.SubRevision := Sub_Revision;
        Result.SysId := SysId;
        Result.Title := Title;
        Result.TypeOs := Ttype;
      end;
    end;
  finally
    FreeAndNil(Lista);
  end;
end;

function TRw7RobotWareService.GetRobotType: string;
var
  Lista: TCollection;
begin
  Result := '';
  try
    FConexion.get(FLocalUrl + '/system/robottype');
  except
    ErrorWebService('Error conexión. codigo: ' + FConexion.StatusText);
  end;
  try
    Lista := TCollection.Create(TRw7RobotTypeItem);
    if FConexion.StatusCode = 200 then
    begin
      GetStatusClassList(FConexion.Respuesta.Text, Lista, TRw7RobotTypeItem, SYS_ROBOTTYPE);

      if Lista.Count = 1 then
      begin
        with Lista.Items[0] as TRw7RobotTypeItem do
        begin
          Result := robot_type;
        end;
      end;
    end;
  finally
    FreeAndNil(Lista);
  end;
end;

function TRw7RobotWareService.GetSystemLicence: string;
var
  Lista: TCollection;
begin
  Result := '';
  try
    FConexion.Get(FLocalUrl + '/system/license');
  except
    ErrorWebService('Error conexión. codigo: ' + FConexion.StatusText);
  end;
  try
    Lista := TCollection.Create(TRw7SysLicenceItem);
    GetStatusClassList(FConexion.Respuesta.Text, Lista, TRw7SysLicenceItem, SYS_LICENSE);
    if Lista.Count = 1 then
    begin
      with Lista.Items[0] as TRw7SysLicenceItem do
      begin
        Result := license;
      end;
    end;
  finally
    FreeAndNil(Lista);
  end;
end;

procedure TRw7RobotWareService.GetSystemProducts(aListItems: TCollection);
begin
  try
    FConexion.Get(FLocalUrl + '/system/products');
  except
    ErrorWebService('Error conexión. codigo: ' + FConexion.StatusText);
  end;

  if FConexion.StatusCode = 200 then
  begin
    GetStatusClassList(FConexion.Respuesta.Text, aListItems, TRw7SysProductItem,
      SYS_PRODUCT);
  end;
end;

procedure TRw7RobotWareService.GetSystemProducts(aLista: TStringList);
var
  Lista: TCollection;
  I: integer;
begin
  try
    Lista := TCollection.Create(TRw7SysProductItem);
    GetSystemProducts(Lista);
    for I := 0 to Lista.Count - 1 do
    begin
      with Lista.Items[I] as TRw7SysProductItem do
      begin
        aLista.Add(_title);
      end;
    end;
  finally
    FreeAndNil(Lista);
  end;
end;

procedure TRw7RobotWareService.GetSystemOptions(aLista: TStringList);
var
  lista: TCollection;
  I: integer;
begin
  try
    FConexion.Get(FLocalUrl + '/system/options');
  except
    ErrorWebService('Error conexión. codigo: ' + FConexion.StatusText);
  end;
  if FConexion.StatusCode = 200 then
  begin
    try
      lista := TCollection.Create(TRw7SysOptionItem);
      GetStatusClassList(FConexion.Respuesta.Text, Lista, TRw7SysOptionItem, SYS_OPTION);
      for I := 0 to lista.Count - 1 do
      begin
        with Lista.Items[I] as TRw7SysOptionItem do
        begin
          aLista.Add(option);
        end;
      end;
    finally
      FreeAndNil(Lista);
    end;

  end;
end;


constructor TRw7RobotWareService.Create(aRobotConexion: TRobotConnection);
begin
  FConexion := aRobotConexion;
  FLocalUrl := 'rw';

end;

destructor TRw7RobotWareService.Destroy;
begin
  FConexion := nil;
  inherited Destroy;
end;


end.
