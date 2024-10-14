unit abbwstypes;
 {
  This unit contains all services under Robotware service category
  Robotware services. More information:
  https://developercenter.robotstudio.com/api/rwsApi/rwservices_main_page.html
 }
{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils;

type
  TAbbWebServicesError = class(Exception);
  {
    INIT : State init
    AUTO_CH : State change request for automatic mode
    MANF_CH : State change request for manual mode & full speed
    MANR : State manual mode & reduced speed
    MANF : State manual mode & full speed
    AUTO : State automatic mode
    UNDEF : Undefined
}
type
  opModes = (opINIT, opAUTO_CH, opMANF_CH, opMANR, opMANF, opAUTO, opUNDEF);

type
  TOpMode = record

    _type: string;
    _title: string;
    opmode: opmodes;
  end;

  {%REGION  RobotWareServices}

type

  { TRwServiceItem }

  TRwServiceItem = class(TCollectionItem)
  private
    Fhref: string;
    Ftitle: string;
    F_type: string;
  published
    property href: string read Fhref write Fhref;
    property _title: string read Ftitle write Ftitle;
    property _type: string read F_type write F_type;
  end;

type

  { TRwServiceList }

  TRwServiceList = class(TCollection)
    procedure SetItems(Index: integer; AValue: TRwServiceItem);
    function GetItems(Index: integer): TRwServiceItem;
  public
    constructor Create;
    function Add: TRwServiceItem;
    property Items[Index: integer]: TRwServiceItem read GetItems write SetItems;
      default;
  end;

  {%ENDREGION}

  {%REGION  Tasks}
type

  { TTaskItem }

  TTaskItem = class(TCollectionItem)
  private
    Factive: string;
    Fexcstate: string;
    Fmotiontask: boolean;
    fname: string;
    Ftaskstate: string;
    Ftitle: string;
    FTType: string;
  public
    procedure Assign(Source: TPersistent); override;
  published
    property Name: string read fname write fname;
    property _title: string read Ftitle write Ftitle;
    property active: string read Factive write Factive;
    property excstate: string read Fexcstate write Fexcstate;
    property motiontask: boolean read Fmotiontask write Fmotiontask;
    property taskstate: string read Ftaskstate write Ftaskstate;
    property TType: string read FTType write FTType;
  end;

type

  {%REGION TRw6TaskList }

  TTaskList = class(TCollection)
    procedure SetItems(Index: integer; AValue: TTaskItem);
    function GetItems(Index: integer): TTaskItem;
  public
    constructor Create;
    function Add: TTaskItem;
    property Items[Index: integer]: TTaskItem read GetItems write SetItems; default;
  end;

  {%ENDREGION}
type

  { TModuleInfoItem }

  TModuleInfoItem = class(TCollectionItem)
  private
    Fhref: string;
    Fname: string;
    Ftitle: string;
    FTType: string;
    F_type: string;
  published
    property href: string read Fhref write Fhref;
    property _type: string read F_type write F_type;
    property _title: string read Ftitle write Ftitle;
    property Name: string read Fname write Fname;
    property TType: string read FTType write FTType;

  end;

type

  { TModuleInfoList }

  TModuleInfoList = class(TCollection)
    procedure SetItems(Index: integer; AValue: TModuleInfoItem);
    function GetItems(Index: integer): TModuleInfoItem;
  public
    constructor Create;
    function Add: TModuleInfoItem;
    property Items[Index: integer]: TModuleInfoItem read GetItems write SetItems;
      default;
  end;

type

  { TModuleTextItem }

  TModuleTextItem = class(TCollectionItem)
  private
    Fchange_count: integer;
    Ffile_path: string;
    Fhref: string;
    Fmodule_length: string;
    Fmodule_text: string;
    F_title: string;
    F_type: string;
    function GetModuleText: string;
    function GetFilePath: string;
  public
    procedure Assign(Source: TPersistent); override;
  published
    property href: string read Fhref write Fhref;
    property _type: string read F_type write F_type;
    property _title: string read F_title write F_title;
    property change_count: integer read Fchange_count write Fchange_count;
    property file_path: string read GetFilePath write Ffile_path;
    property module_length: string read Fmodule_length write Fmodule_length;
    property module_text: string read GetModuleText write Fmodule_text;
  end;

type

  { TModuleTextList }

  TModuleTextList = class(TCollection)
    procedure SetItems(Index: integer; AValue: TModuleTextItem);
    function GetItems(Index: integer): TModuleTextItem;
  public
    constructor Create;
    function Add: TModuleTextItem;
    property Items[Index: integer]: TModuleTextItem read GetItems write SetItems;
      default;
  end;

type

  { TCtrlIdentifyItem }

  TCtrlIdentifyItem = class(TCollectionItem)
  private
    Fctrl_level: string;
    Fctrl_name: string;
    F_title: string;
    F_type: string;
  public
    procedure Assign(Source: TPersistent); override;
  published
    property _type: string read F_type write F_type;
    property _title: string read F_title write F_title;
    property ctrl_name: string read Fctrl_name write Fctrl_name;
    property ctrl_level: string read Fctrl_level write Fctrl_level;
  end;

type

  { TCtrlIdentifyList }

  TCtrlIdentifyList = class(TCollection)
    procedure SetItems(Index: integer; AValue: TCtrlIdentifyItem);
    function GetItems(Index: integer): TCtrlIdentifyItem;
  public
    constructor Create;
    function Add: TCtrlIdentifyItem;
    property Items[Index: integer]: TCtrlIdentifyItem read GetItems write SetItems;
      default;
  end;

type

  { TSysOptionItem }

  TSysOptionItem = class(TCollectionItem)
  private
    Foption: string;
    F_title: string;
    F_type: string;
  published
    property _type: string read F_type write F_type;
    property _title: string read F_title write F_title;
    property option: string read Foption write Foption;
  public
    procedure Assign(Source: TPersistent); override;
  end;

type

  { TSysOptionList }

  TSysOptionList = class(TCollection)
    procedure SetItems(Index: integer; AValue: TSysOptionItem);
    function GetItems(Index: integer): TSysOptionItem;
  public
    constructor Create;
    function Add: TSysOptionItem;
    property Items[Index: integer]: TSysOptionItem read GetItems write SetItems;
      default;
  end;

type

  { TRobotTypeItem }

  TRobotTypeItem = class(TCollectionItem)
  private
    Frobot_type: string;
    F_title: string;
    F_type: string;
  published
    property _type: string read F_type write F_type;
    property _title: string read F_title write F_title;
    property robot_type: string read Frobot_type write Frobot_type;
    procedure Assign(Source: TPersistent); override;
  end;

type

  { TRobotTypeList }

  TRobotTypeList = class(TCollection)
  private
    function GetItems(Index: integer): TRobotTypeItem;
    procedure SetItems(Index: integer; AValue: TRobotTypeItem);
  public
    constructor Create;
    function Add: TRobotTypeItem;
    property Items[Index: integer]: TRobotTypeItem read GetItems write SetItems;
      default;
  end;

type

  { TIoNetWorkItem }

  TIoNetWorkItem = class(TCollectionItem)
  private
    Fname: string;
    Fpstate: string;
    F_Title: string;
    F_type: string;
    procedure Setname(AValue: string);
    procedure Setpstate(AValue: string);
    procedure Set_Title(AValue: string);
    procedure Set_type(AValue: string);
  public
    procedure Assign(Source: TPersistent); override;
  published
    property _title: string read F_Title write Set_Title;
    property Name: string read Fname write Setname;
    property pstate: string read Fpstate write Setpstate;
    property _type: string read F_type write Set_type;
  end;

type

  { TIoNetWorkList }

  TIoNetWorkList = class(TCollection)
  private
    function GetItems(Index: integer): TIoNetWorkItem;
    procedure SetItems(Index: integer; AValue: TIoNetWorkItem);
  public
    constructor Create;
    function Add: TIoNetWorkItem;
    property Items[Index: integer]: TIoNetWorkItem read GetItems write SetItems;
      default;
  end;

type

  { TIoSignalItem }

  TIoSignalItem = class(TCollectionItem)
  private
    Fcategory: string;
    Fhref: string;
    Flstate: string;
    Flvalue: string;
    Fname: string;
    Ftitle: string;
    FTType: string;
    F_type: string;
  public
    procedure Assign(Source: TPersistent); override;
    function Device: string;
  published
    property href: string read Fhref write Fhref;
    property _title: string read Ftitle write Ftitle;
    property _type: string read F_type write F_type;
    property Name: string read Fname write Fname;
    property TType: string read FTType write FTType;
    property category: string read Fcategory write Fcategory;
    property lvalue: string read Flvalue write Flvalue;
    property lstate: string read Flstate write Flstate;
  end;


type

  { TIoSignalList }

  TIoSignalList = class(TCollection)
  private
    procedure SetItems(Index: integer; AValue: TIoSignalItem);
    function GetItems(Index: integer): TIoSignalItem;
  public
    constructor Create;
    function Add: TIoSignalItem;
    property Items[Index: integer]: TIoSignalItem read GetItems write SetItems; default;
  end;


type
  TListItems = class(TCollection)
  end;

const
  RAP_TASK_LI: string = 'rap-task-li';
  RWSERVICE_LI: string = 'rwservice-li';
  RAP_MODULE_INFO_LI = 'rap-module-info-li';
  RAP_MODULE_TEXT = 'rap-module-text';
  CTRL_IDENTITY_INFO = 'ctrl-identity-info';
  SYS_OPTION_LI = 'sys-option-li';
  SYS_ROBOTTYPE = 'sys-robottype';
  IOS_NETWORK_LI = 'ios-network-li';
  IOS_SIGNAL_LI = 'ios-signal-li';

const
  PostOk: integer = 204;


function Formatjsonkey(aKeyName: string): string;
function LeerModoFuncionamiento(aDatos: string): TOpMode;
procedure ErrorWebService(ainfo: string);
procedure GetEmbeddedStateList(aDatos: string; aListItems: TCollection;
  aItemClass: TCollectionItemClass; TipoLista: string);

implementation

uses StrUtils, jsonparser, fpjson, TypInfo, URIParser;

function LeerModoFuncionamiento(aDatos: string): TOpMode;
var
  jData, DataResources: TJSONData;
  myJsonObject: TJSONObject;
  Cadena: TJSONStringType;
  temp: opModes;
begin
  Result.opmode := opUNDEF;
  Result._title := '';
  Result._type := '';

  try
    jData := GetJSON(aDatos);
    myJsonObject := jData as TJSONObject;
    DataResources := myJsonObject.GetPath('_embedded').GetPath('_state');
    if DataResources <> nil then
    begin
      Result._type := DataResources.Items[0].FindPath('_type').AsString;
      Result._title := DataResources.Items[0].FindPath('_title').AsString;
      Cadena := DataResources.Items[0].FindPath('opmode').AsString;
      Cadena := 'op' + Cadena;
      for temp in opModes do
      begin
        if GetEnumName(TypeInfo(opModes), integer(temp)) = Cadena then
        begin
          Result.opmode := temp;
        end;
      end;
    end;
  finally
    FreeAndNil(jData);
  end;

end;

procedure ErrorWebService(ainfo: string);
begin
  raise TAbbWebServicesError.Create(ainfo);
end;

function Formatjsonkey(aKeyName: string): string;
const
  palabras: array [1..2] of string = ('type', 'private');
begin
  Result := aKeyName;
  if aKeyName in (palabras) then
  begin
    Result := 'T' + aKeyName;
    Exit;
  end;
  Result := StringReplace(aKeyName, '-', '_', [rfReplaceAll]);

end;

procedure GetEmbeddedStateList(aDatos: string; aListItems: TCollection;
  aItemClass: TCollectionItemClass; TipoLista: string);
var
  I, X, Valor: integer;
  Cadena: TJSONStringType;
  jData, DataResources: TJSONData;
  myJsonObject: TJSONObject;
  NombreClave: string;
  propInfo: PPropInfo;
  ItemActual: TCollectionItem;
  //tipo: TJSONtype;
begin
  try
    jData := GetJSON(aDatos);
    myJsonObject := jData as TJSONObject;
    DataResources := myJsonObject.GetPath('_embedded').GetPath('_state');
    for I := 0 to DataResources.Count - 1 do
      if DataResources.Items[I].FindPath('_type').AsString = TipoLista then
      begin
        ItemActual := aListItems.Add;
        for X := 0 to DataResources.Items[I].Count - 1 do
        begin
          if DataResources.Items[I].JSONType = jtObject then
          begin
            Cadena := DataResources.Items[I].AsJSON;
            //tipo := DataResources.Items[I].Items[X].JSONType;
            if DataResources.Items[I].Items[X].JSONType = jtString then
            begin
              Cadena := DataResources.Items[I].Items[X].AsString;
              NombreClave := TJSONObject(DataResources.Items[I]).Names[X];
              NombreClave := Formatjsonkey(NombreClave);
              propInfo := GetPropInfo(aItemClass, NombreClave);
              if propInfo <> nil then
              begin
                SetPropValue(ItemActual, propInfo, Cadena);
              end;
            end
            else if DataResources.Items[I].Items[X].JSONType = jtObject then
            begin
              if DataResources.Items[I].Items[X].Count > 0 then
              begin
                if DataResources.Items[I].Items[X].Items[0].FindPath(
                  'href') <> nil then
                begin
                  Cadena := DataResources.Items[I].Items[X].Items[0].Items[0].AsString;
                  NombreClave := 'href';
                  propInfo := GetPropInfo(aItemClass, NombreClave);
                  if propInfo <> nil then
                  begin
                    SetPropValue(ItemActual, propInfo, Cadena);
                  end;
                end;
              end;
            end
            else if DataResources.Items[I].Items[X].JSONType = jtNumber then
            begin
              Valor := DataResources.Items[I].Items[X].AsInteger;
              NombreClave := TJSONObject(DataResources.Items[I]).Names[X];
              NombreClave := Formatjsonkey(NombreClave);
              propInfo := GetPropInfo(aItemClass, NombreClave);
              if propInfo <> nil then
              begin
                SetPropValue(ItemActual, propInfo, Valor);
              end;
            end;
          end;
        end;
      end;
    jData.Free
  except
    on E: Exception do
    begin
      if Assigned(jdata) then
      begin
        jData.Free;
      end;
      ErrorWebService(E.Message);
    end;
  end;
end;



{ TRwServiceList }

procedure TRwServiceList.SetItems(Index: integer; AValue: TRwServiceItem);
begin
  Items[Index].Assign(AValue);
end;

function TRwServiceList.GetItems(Index: integer): TRwServiceItem;
begin
  Result := TRwServiceItem(inherited items[index]);
end;

constructor TRwServiceList.Create;
begin
  inherited Create(TRwServiceItem);
end;

function TRwServiceList.Add: TRwServiceItem;
begin
  Result := inherited Add as TRwServiceItem;
  ;
end;

{ TTaskItem }

procedure TTaskItem.Assign(Source: TPersistent);
begin
  if Source is TTaskItem then
  begin
    FTType := TTaskItem(Source).FTType;
    Ftitle := TTaskItem(Source).Ftitle;
    Fname := TTaskItem(Source).fname;
    Factive := TTaskItem(Source).Factive;
    Fexcstate := TTaskItem(Source).Fexcstate;
    Fmotiontask := TTaskItem(Source).Fmotiontask;
    Ftaskstate := TTaskItem(Source).Ftaskstate;
  end
  else
  begin
    inherited Assign(Source);
  end;
end;

{ TTaskList }

procedure TTaskList.SetItems(Index: integer; AValue: TTaskItem);
begin
  Items[Index].Assign(AValue);
end;

function TTaskList.GetItems(Index: integer): TTaskItem;
begin
  Result := TTaskItem(inherited Items[Index]);
end;

constructor TTaskList.Create;
begin
  inherited Create(TTaskItem);
end;

function TTaskList.Add: TTaskItem;
begin
  Result := inherited Add as TTaskItem;
end;

{ TModuleInfoList }

procedure TModuleInfoList.SetItems(Index: integer; AValue: TModuleInfoItem);
begin
  Items[Index].Assign(AValue);
end;

function TModuleInfoList.GetItems(Index: integer): TModuleInfoItem;
begin
  Result := TModuleInfoItem(inherited Items[Index]);

end;

constructor TModuleInfoList.Create;
begin
  inherited Create(TModuleInfoItem);
end;

function TModuleInfoList.Add: TModuleInfoItem;
begin
  Result := inherited Add as TModuleInfoItem;
end;

{ TModuleTextItem }

function TModuleTextItem.GetModuleText: string;
var
  Uri: TURI;
begin
  Uri := ParseURI(Fmodule_text);
  Result := uri.Document;
end;

function TModuleTextItem.GetFilePath: string;
var
  Uri: TURI;
  ruta, Documento: string;
begin
  Uri := ParseURI(Ffile_path);
  ruta := Uri.Path;
  Documento := Uri.Document;
  RemovePadChars(ruta, ['"']);
  RemovePadChars(Documento, ['"']);
  Result := ruta + Documento;
end;

procedure TModuleTextItem.Assign(Source: TPersistent);
begin
  if Source is TModuleTextItem then
  begin
    Fchange_count := TModuleTextItem(Source).Fchange_count;
    Ffile_path := TModuleTextItem(Source).Ffile_path;
    Fhref := TModuleTextItem(Source).Fhref;
    Fmodule_length := TModuleTextItem(Source).Fmodule_length;
    Fmodule_text := TModuleTextItem(Source).module_text;
    F_title := TModuleTextItem(Source).F_title;
    F_type := TModuleTextItem(Source).F_type;
  end
  else
  begin
    inherited Assign(Source);
  end;
end;

{ TModuleTextList }

procedure TModuleTextList.SetItems(Index: integer; AValue: TModuleTextItem);
begin
  items[Index].Assign(AValue);
end;

function TModuleTextList.GetItems(Index: integer): TModuleTextItem;
begin
  Result := TModuleTextItem(inherited Items[Index]);
end;

constructor TModuleTextList.Create;
begin
  inherited Create(TModuleTextItem);
end;

function TModuleTextList.Add: TModuleTextItem;
begin
  Result := inherited Add as TModuleTextItem;
end;

{ TCtrlIdentifyItem }

procedure TCtrlIdentifyItem.Assign(Source: TPersistent);
begin
  if Source is TCtrlIdentifyItem then
  begin
    Fctrl_level := TCtrlIdentifyItem(Source).Fctrl_level;
    Fctrl_name := TCtrlIdentifyItem(Source).ctrl_name;
    F_title := TCtrlIdentifyItem(Source).F_title;
    F_type := TCtrlIdentifyItem(Source).F_type;
  end
  else
  begin
    inherited Assign(Source);
  end;
end;

{ TCtrlIdentifyList }

procedure TCtrlIdentifyList.SetItems(Index: integer; AValue: TCtrlIdentifyItem);
begin
  Items[Index].Assign(AValue);
end;

function TCtrlIdentifyList.GetItems(Index: integer): TCtrlIdentifyItem;
begin
  Result := TCtrlIdentifyItem(inherited Items[Index]);
end;

constructor TCtrlIdentifyList.Create;
begin
  inherited Create(TCtrlIdentifyItem);
end;

function TCtrlIdentifyList.Add: TCtrlIdentifyItem;
begin
  Result := inherited Add as TCtrlIdentifyItem;
end;

{ TSysOptionItem }

procedure TSysOptionItem.Assign(Source: TPersistent);
begin
  if Source is TSysOptionItem then
  begin
    F_title := TSysOptionItem(Source).F_title;
    F_type := TSysOptionItem(Source).F_type;
    Foption := TSysOptionItem(Source).Foption;
  end
  else
  begin
    inherited Assign(Source);
  end;
end;

{ TSysOptionList }

procedure TSysOptionList.SetItems(Index: integer; AValue: TSysOptionItem);
begin
  Items[Index].Assign(AValue);
end;

function TSysOptionList.GetItems(Index: integer): TSysOptionItem;
begin
  Result := TSysOptionItem(inherited Items[Index]);
end;

constructor TSysOptionList.Create;
begin
  inherited Create(TSysOptionItem);
end;

function TSysOptionList.Add: TSysOptionItem;
begin
  Result := inherited Add as TSysOptionItem;
end;

{ TRobotTypeItem }

procedure TRobotTypeItem.Assign(Source: TPersistent);
begin
  if Source is TRobotTypeItem then
  begin
    F_title := TRobotTypeItem(Source).F_title;
    F_type := TRobotTypeItem(Source).F_type;
    Frobot_type := TRobotTypeItem(Source).Frobot_type;
  end
  else
  begin
    inherited Assign(Source);
  end;
end;

{ TRobotTypeList }

function TRobotTypeList.GetItems(Index: integer): TRobotTypeItem;
begin
  Result := TRobotTypeItem(inherited Items[Index]);
end;

procedure TRobotTypeList.SetItems(Index: integer; AValue: TRobotTypeItem);
begin
  Items[Index].Assign(AValue);
end;

constructor TRobotTypeList.Create;
begin
  inherited Create(TRobotTypeItem);
end;

function TRobotTypeList.Add: TRobotTypeItem;
begin
  Result := inherited Add as TRobotTypeItem;
end;

{%REGION TIoNetWorkItem }

procedure TIoNetWorkItem.Set_Title(AValue: string);
begin
  if F_Title = AValue then Exit;
  F_Title := AValue;
end;

procedure TIoNetWorkItem.Set_type(AValue: string);
begin
  if F_type = AValue then Exit;
  F_type := AValue;
end;

procedure TIoNetWorkItem.Assign(Source: TPersistent);
begin
  if Source is TIoNetWorkItem then
  begin
    Fname := TIoNetWorkItem(Source).Fname;
    F_Title := TIoNetWorkItem(Source).F_Title;
    F_type := TIoNetWorkItem(Source).F_type;
    Fpstate := TIoNetWorkItem(Source).Fpstate;
  end
  else
  begin
    inherited Assign(Source);
  end;
end;

procedure TIoNetWorkItem.Setname(AValue: string);
begin
  if Fname = AValue then Exit;
  Fname := AValue;
end;

procedure TIoNetWorkItem.Setpstate(AValue: string);
begin
  if Fpstate = AValue then Exit;
  Fpstate := AValue;
end;
{%ENDREGION}

{ TIoNetWorkList }

function TIoNetWorkList.GetItems(Index: integer): TIoNetWorkItem;
begin
  Result := TIoNetWorkItem(inherited items[Index]);
end;

procedure TIoNetWorkList.SetItems(Index: integer; AValue: TIoNetWorkItem);
begin
  Items[Index].Assign(aValue);
end;

constructor TIoNetWorkList.Create;
begin
  inherited Create(TIoNetWorkItem);
end;

function TIoNetWorkList.Add: TIoNetWorkItem;
begin
  Result := inherited Add as TIoNetWorkItem;
end;

{ TIoSignalItem }
procedure TIoSignalItem.Assign(Source: TPersistent);
begin
  if Source is TIoSignalItem then
  begin
    Fcategory := TIoSignalItem(Source).Fcategory;
    Fhref := TIoSignalItem(Source).Fhref;
    Flstate := TIoSignalItem(Source).Flstate;
    Flvalue := TIoSignalItem(Source).Flvalue;
    Fname := TIoSignalItem(Source).Fname;
    Ftitle := TIoSignalItem(Source).Ftitle;
    FTType := TIoSignalItem(Source).FTType;
    F_type := TIoSignalItem(Source).F_type;
  end
  else
  begin
    inherited Assign(Source);
  end;
end;

function TIoSignalItem.Device: string;
var
  Partes: SizeInt;
begin
  if Fhref = '' then
  begin
    Result := '';
    exit;
  end;
  Partes := WordCount(Fhref, ['/']);
  if Partes = 2 then
  begin
    Result := '';
    Exit;
  end;
  Result := ExtractWord(Partes - 1, Fhref, ['/']);
end;

{ TIoSignalList }

procedure TIoSignalList.SetItems(Index: integer; AValue: TIoSignalItem);
begin
  Items[Index].Assign(aValue);
end;

function TIoSignalList.GetItems(Index: integer): TIoSignalItem;
begin
  Result := TIoSignalItem(inherited items[Index]);
end;

constructor TIoSignalList.Create;
begin
  inherited Create(TIoSignalItem);
end;

function TIoSignalList.Add: TIoSignalItem;
begin
  Result := inherited Add as TIoSignalItem;
end;

end.


