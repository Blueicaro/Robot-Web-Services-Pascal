unit robotwaredata;

{$mode ObjFPC}{$H+}

interface

uses
  Classes,
  SysUtils,
  fgl, fpjson,
  abbconexion;

type
  TAbbWebServicesError = class(Exception);

type
  TMotorState = (msMotorOn, msMotorOf);

type
  TRestartModes = (rmRestart, rmShutDown, rmBootAplication, rmResetSystem,
    rmResetRapid, rmRevertToAutoSave);

type
  opModes = (opINIT, opAUTO_CH, opMANF_CH, opMANR, opMANF, opAUTO, opUNDEF);


type

  { TBase }

  TBase = class
  class var FConexion: TRobotConnection;
    constructor Create(aRobotConexion: TRobotConnection); virtual; abstract;
    class procedure SetConexion(AValue: TRobotConnection); static;
  end;


type
  TDataValue = packed record
    Value: string;
  end;

type
  TDataProperties = packed record
    TaskName: string;
    ModuleName: string;
    SymbolName: string;
    DataType: string;
    Dimensions: integer;
    Scope: string;
    DataTypeUrl: string;
  end;

type

  { TDataObject }

  TDataObject = class
  private
    FArrayItem: TDataValue;
    FGetDataType: string;
    FGetDimension: string;
    FGetModuleName: string;
    FGetName: string;
    FGetProperties: TDataProperties;
    FGetResourceString: string;
    FGetScope: string;
    FGetSymbolType: string;
    FGetTaskName: string;
    FGetTitle: string;
    FGetTypeURL: string;
    FRawData: string;
    FRecordItem: TDataValue;
    FValue: TDataValue;
    procedure SetArrayItem(AValue: TDataValue);
    procedure SetGetDataType(AValue: string);
    procedure SetGetDimension(AValue: string);
    procedure SetGetModuleName(AValue: string);
    procedure SetGetName(AValue: string);
    procedure SetGetProperties(AValue: TDataProperties);
    procedure SetGetScope(AValue: string);
    procedure SetGetSymbolType(AValue: string);
    procedure SetGetTaskName(AValue: string);
    procedure SetGetTitle(AValue: string);
    procedure SetGetTypeURL(AValue: string);
    procedure SetRawData(AValue: string);
    procedure SetRecordItem(AValue: TDataValue);
    procedure SetValue(AValue: TDataValue);
  public
    property GetTitle: string read FGetTitle write SetGetTitle;
    property GetProperties: TDataProperties read FGetProperties write SetGetProperties;
    property GetName: string read FGetName write SetGetName;
    property GetModuleName: string read FGetModuleName write SetGetModuleName;
    property GetTaskName: string read FGetTaskName write SetGetTaskName;
    property GetDataType: string read FGetDataType write SetGetDataType;
    property GetSymbolType: string read FGetSymbolType write SetGetSymbolType;
    property GetDimension: string read FGetDimension write SetGetDimension;
    property GetScope: string read FGetScope write SetGetScope;
    property GetTypeURL: string read FGetTypeURL write SetGetTypeURL;
    property Value: TDataValue read FValue write SetValue;
    property ArrayItem: TDataValue read FArrayItem write SetArrayItem;
    property RecordItem: TDataValue read FRecordItem write SetRecordItem;
    property RawData: string read FRawData write SetRawData;
    procedure Fetch;
    property GetResourceString: string read FGetResourceString;
  end;

  {%region Task }


  { TTaskItem }

type
  TTaksProperties = packed record
    Name: string;
    TaskType: string;
    TaskState: string;
    ExecutionState: string;
    ActiveState: string;
    IsMotionTask: boolean;
    TrustLevel: string;
    id: string;
    ExecutionLevel: string;
    ExecutionMode: string;
    ExecutionType: string;
    ProgEntryPoint: string;
    BindRef: boolean;
    TaskForeground: string;
  end;

type
  TProgramInfo = packed record
    EntryPoint: string;
    BindRef: boolean;
  end;



  TTaskItem = class
  private
    FGetData: TDataObject;
    FGetName: string;
    FProgramInfo: TProgramInfo;
    FProperties: TTaksProperties;
    Fhref: string;
    FRobotConexion: TRobotConnection;
    function GetProperties: TTaksProperties;
    procedure SetGetData(AValue: TDataObject);
    procedure SetGetName(AValue: string);
    procedure SetProgramInfo(AValue: TProgramInfo);
    procedure SetProperties(AValue: TTaksProperties);
    procedure SetRobotConexion(AValue: TRobotConnection);
  public
    property RobotConexion: TRobotConnection read FRobotConexion write SetRobotConexion;
    property href: string read Fhref write Fhref;
    property GetName: string read FGetName write SetGetName;
    property Properties: TTaksProperties read GetProperties write SetProperties;
    //property GetServiceRoutine:
    property GetData: TDataObject read FGetData write SetGetData;
    property ProgramInfo: TProgramInfo read FProgramInfo write SetProgramInfo;
    procedure GetModuleNames(var ProgramModulesList: TStringList;
      var SystemModuleList: TStringList);
    //property GetPointers
    //function MovePPToRoutine;string;
    //function AbortServiceRoutine
  public
    destructor Destroy; override;
  end;

type

  { TTaskListHelper }



  TTaskList = specialize TFPGObjectList<TTaskItem>;

  TTaskListHelper = class helper for TTaskList
    function GetTaskFromJsonRw6(Data: TJSONData; Index: integer): TTaskItem;
  end;
  {%endregion}

  {region Module}
type
  TModuleProperties = packed record
    TaskName: string;
    ModuleName: string;
    FileName: string;
    Attributes: string;
  end;


type

  { TModuleData }

  TModuleData = class
  private
    FData: TDataObject;
    FGetname: string;
    FGetTaskName: string;
    FProperties: TModuleProperties;
    procedure SetData(AValue: TDataObject);
    procedure SetGetname(AValue: string);
    procedure SetGetTaskName(AValue: string);
    procedure SetProperties(AValue: TModuleProperties);
  public
    property GetName: string read FGetname write SetGetname;
    property GetTaskName: string read FGetTaskName write SetGetTaskName;
    property Properties: TModuleProperties read FProperties write SetProperties;
    property Data: TDataObject read FData write SetData;
  end;


type
  TModuleBase = class
    class function GetName: string; virtual; abstract;
    class function GetTaskName: string; virtual; abstract;
    class function GetProperties: TModuleProperties; virtual; abstract;
    class function GetData(DataName: string): string; virtual; abstract;
  end;

  {endregion}

type

  { TNetworkSettingBase }

  TNetworkSettingBase = class
  private
  class var FAddr: string;
    class procedure SetAddr(AValue: string); static;
  public
    class property Addr: string read FAddr write SetAddr;
    class function mask: string; virtual; abstract;
    class function Name: string; virtual; abstract;
    class function dhcp: string; virtual; abstract;
    class function gateway: string; virtual; abstract;
    class function title: string; virtual; abstract;
  end;

type

  { TNetworkSettingRw6 }

  TNetworkSettingRw6 = class(TNetworkSettingBase)
    class function dhcp: string; override;
    class function gateway: string; override;
    class function mask: string; override;
    class function Name: string; override;
    class function title: string; override;

  end;

type

  { TNetworkSettingRw7 }

  TNetworkSettingRw7 = class(TNetworkSettingBase)
    class function dhcp: string; override;
    class function gateway: string; override;
    class function mask: string; override;
    class function Name: string; override;
    class function title: string; override;
    function DnsPrimario: string;
    function DnsSecundario: string;
  end;

type
  TNetworkSetting = TNetworkSettingBase;


type
  TNetWork = class

  end;

type
  TDevice = class

  end;

type

  { TSignal }

  TSignal = class
  private
    FGetDeviceName: string;
    FGetName: string;
    FgetNetWorkName: string;
    FGetPath: string;
    procedure SetGetDeviceName(AValue: string);
    procedure SetGetName(AValue: string);
    procedure SetgetNetWorkName(AValue: string);
    procedure SetGetPath(AValue: string);
  published
    property GetName: string read FGetName write SetGetName;
    property GetPath: string read FGetPath write SetGetPath;
    property getNetWorkName: string read FgetNetWorkName write SetgetNetWorkName;
    property GetDeviceName: string read FGetDeviceName write SetGetDeviceName;
  end;

implementation

{ TTaskItem }

procedure TTaskItem.SetGetName(AValue: string);
begin
  if FGetName = AValue then Exit;
  FGetName := AValue;
end;

procedure TTaskItem.SetProgramInfo(AValue: TProgramInfo);
begin
  //if FProgramInfo = AValue then Exit;
  FProgramInfo := AValue;
end;

procedure TTaskItem.SetGetData(AValue: TDataObject);
begin
  if FGetData = AValue then Exit;
  FGetData := AValue;
end;

function TTaskItem.GetProperties: TTaksProperties;
begin
  Result := Self.FProperties;
  if not Assigned(FRobotConexion) then
  begin
    raise TAbbWebServicesError.Create('TaksItem conexion not set');
  end;
  try
    FRobotConexion.Get('rw/rapid/' + Fhref);
  except
    raise TAbbWebServicesError.Create('Error de conexión');
  end;
  Result.Name := FRobotConexion.GetStateName('name');
  Result.TaskType := FRobotConexion.GetStateName('type');
  Result.TaskState:= FRobotConexion.GetStateName('taskstate');
  Result.ExecutionState := FRobotConexion.GetStateName('excstate');
  Result.ActiveState := FRobotConexion.GetStateName('active');
  Result.IsMotionTask := StrToBoolDef(FRobotConexion.GetStateName(
    'motiontask'), True);
  Result.TrustLevel := FRobotConexion.GetStateName('trust');
  Result.id := FRobotConexion.GetStateName('taskID');
  Result.ExecutionMode := FRobotConexion.GetStateName('excstate');
  Result.ExecutionType := FRobotConexion.GetStateName('exectype');
  Result.ProgEntryPoint := FRobotConexion.GetStateName('prodentrypt');
  Result.BindRef := StrToBoolDef(FRobotConexion.GetStateName('bind_ref'), True);
  Result.TaskForeground := FRobotConexion.GetStateName('task_in_forgnd');
end;

procedure TTaskItem.SetProperties(AValue: TTaksProperties);
begin
  //if FProperties = AValue then Exit;
  FProperties := AValue;
end;

procedure TTaskItem.SetRobotConexion(AValue: TRobotConnection);
begin
  if FRobotConexion = AValue then Exit;
  FRobotConexion := AValue;
end;

destructor TTaskItem.Destroy;
begin
  FRobotConexion := nil;
  inherited Destroy;
end;

procedure TTaskItem.GetModuleNames(var ProgramModulesList: TStringList;
  var SystemModuleList: TStringList);
var
  I: integer;
begin
  ProgramModulesList.Clear;
  SystemModuleList.Clear;
  if not Assigned(FRobotConexion) then
  begin
    raise TAbbWebServicesError.Create('TaksItem conexion not set');
  end;
  try
    FRobotConexion.Get('rw/rapid/modules?task=' + GetName + '&json=1');
  except
    raise TAbbWebServicesError.Create('Error de conexión');
  end;

  for I := 0 to FRobotConexion.ResourcesCount - 1 do
  begin
    if FRobotConexion.GetResourceItem('type', I) = 'ProgMod' then
    begin
      ProgramModulesList.Add(FRobotConexion.GetResourceItem('name', I));
    end
    else
    begin
      SystemModuleList.Add(FRobotConexion.GetResourceItem('name', I));
    end;
  end;

end;

{ TTaskListHelper }
//Obtiene un elemento del array que hay debajo de :_embedded._state
function TTaskListHelper.GetTaskFromJsonRw6(Data: TJSONData; Index: integer): TTaskItem;
begin
  Result := TTaskItem.Create;
  try
    Result.Fhref := Data.GetPath('_embedded._state').Items[Index].Items[0].GetPath(
      'self.href').AsString;
    Result.FGetName := Data.GetPath('_embedded._state').Items[Index].GetPath(
      'name').AsString;
  except
    raise TAbbWebServicesError.Create('Error in json');
  end;
end;

{ TBase }

class procedure TBase.SetConexion(AValue: TRobotConnection);
begin
  if FConexion = AValue then Exit;
  FConexion := AValue;
end;

{ TDataObject }

procedure TDataObject.SetGetTitle(AValue: string);
begin
  if FGetTitle = AValue then Exit;
  FGetTitle := AValue;
end;

procedure TDataObject.SetGetTypeURL(AValue: string);
begin
  if FGetTypeURL = AValue then Exit;
  FGetTypeURL := AValue;
end;

procedure TDataObject.SetRawData(AValue: string);
begin
  if FRawData = AValue then Exit;
  FRawData := AValue;
end;

procedure TDataObject.SetRecordItem(AValue: TDataValue);
begin
  //if FRecordItem = AValue then Exit;
  FRecordItem := AValue;
end;

procedure TDataObject.SetValue(AValue: TDataValue);
begin
  //if FValue = AValue then Exit;
  FValue := AValue;
end;

procedure TDataObject.Fetch;
begin

end;

procedure TDataObject.SetGetProperties(AValue: TDataProperties);
begin
  //if FGetProperties = AValue then Exit;
  FGetProperties := AValue;
end;

procedure TDataObject.SetGetScope(AValue: string);
begin
  if FGetScope = AValue then Exit;
  FGetScope := AValue;
end;

procedure TDataObject.SetGetSymbolType(AValue: string);
begin
  if FGetSymbolType = AValue then Exit;
  FGetSymbolType := AValue;
end;

procedure TDataObject.SetGetTaskName(AValue: string);
begin
  if FGetTaskName = AValue then Exit;
  FGetTaskName := AValue;
end;

procedure TDataObject.SetGetName(AValue: string);
begin
  if FGetName = AValue then Exit;
  FGetName := AValue;
end;

procedure TDataObject.SetGetModuleName(AValue: string);
begin
  if FGetModuleName = AValue then Exit;
  FGetModuleName := AValue;
end;

procedure TDataObject.SetGetDataType(AValue: string);
begin
  if FGetDataType = AValue then Exit;
  FGetDataType := AValue;
end;

procedure TDataObject.SetArrayItem(AValue: TDataValue);
begin
  //if FArrayItem = AValue then Exit;
  FArrayItem := AValue;
end;

procedure TDataObject.SetGetDimension(AValue: string);
begin
  if FGetDimension = AValue then Exit;
  FGetDimension := AValue;
end;

{ TModuleData }

procedure TModuleData.SetGetname(AValue: string);
begin
  if FGetname = AValue then Exit;
  FGetname := AValue;
end;

procedure TModuleData.SetData(AValue: TDataObject);
begin
  if FData = AValue then Exit;
  FData := AValue;
end;

procedure TModuleData.SetGetTaskName(AValue: string);
begin
  if FGetTaskName = AValue then Exit;
  FGetTaskName := AValue;
end;

procedure TModuleData.SetProperties(AValue: TModuleProperties);
begin
  //if FProperties = AValue then Exit;
  FProperties := AValue;
end;

{ TNetworkSettingBase }

class procedure TNetworkSettingBase.SetAddr(AValue: string);
begin
  if FAddr = AValue then Exit;
  FAddr := AValue;
end;

{ TNetworkSettingRw6 }


class function TNetworkSettingRw6.dhcp: string;
begin

end;

class function TNetworkSettingRw6.gateway: string;
begin

end;

class function TNetworkSettingRw6.mask: string;
begin

end;

class function TNetworkSettingRw6.Name: string;
begin

end;

class function TNetworkSettingRw6.title: string;
begin

end;

{ TNetworkSettingRw7 }


class function TNetworkSettingRw7.dhcp: string;
begin

end;

class function TNetworkSettingRw7.gateway: string;
begin

end;

class function TNetworkSettingRw7.mask: string;
begin

end;

class function TNetworkSettingRw7.Name: string;
begin

end;

class function TNetworkSettingRw7.title: string;
begin

end;

function TNetworkSettingRw7.DnsPrimario: string;
begin

end;

function TNetworkSettingRw7.DnsSecundario: string;
begin

end;

{ TSignal }

procedure TSignal.SetGetName(AValue: string);
begin
  if FGetName = AValue then Exit;
  FGetName := AValue;
end;

procedure TSignal.SetGetDeviceName(AValue: string);
begin
  if FGetDeviceName = AValue then Exit;
  FGetDeviceName := AValue;
end;

procedure TSignal.SetgetNetWorkName(AValue: string);
begin
  if FgetNetWorkName = AValue then Exit;
  FgetNetWorkName := AValue;
end;

procedure TSignal.SetGetPath(AValue: string);
begin
  if FGetPath = AValue then Exit;
  FGetPath := AValue;
end;

end.
