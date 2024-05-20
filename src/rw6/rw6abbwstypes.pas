unit rw6abbwstypes;
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
  TRw6AbbWebServicesError = class(Exception);
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

type

  { TRw6RwServiceItem }

  TRw6RwServiceItem = class(TCollectionItem)
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

  { TRw6RwServiceList }

  TRw6RwServiceList = class(TCollection)
    procedure SetItems(Index: integer; AValue: TRw6RwServiceItem);
    function GetItems(Index: integer): TRw6RwServiceItem;
  public
    constructor Create;
    function Add: TRw6RwServiceItem;
    property Items[Index: integer]: TRw6RwServiceItem read GetItems write SetItems;
      default;
  end;

type
  TRw6TaskItem = class(TCollectionItem)
  private
    Factive: string;
    Fexcstate: string;
    Fmotiontask: boolean;
    fname: string;
    Ftaskstate: string;
    Ftitle: string;
    FTType: string;
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

  { TRw6TaskList }

  TRw6TaskList = class(TCollection)
    procedure SetItems(Index: integer; AValue: TRw6TaskItem);
    function GetItems(Index: integer): TRw6TaskItem;
  public
    constructor Create;
    function Add: TRw6TaskItem;
    property Items[Index: integer]: TRw6TaskItem read GetItems write SetItems; default;
  end;

type

  { TRw6ModuleInfoItem }

  TRw6ModuleInfoItem = class(TCollectionItem)
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

  { TRw6ModuleInfoList }

  TRw6ModuleInfoList = class(TCollection)
    procedure SetItems(Index: integer; AValue: TRw6ModuleInfoItem);
    function GetItems(Index: integer): TRw6ModuleInfoItem;
  public
    constructor Create;
    function Add: TRw6ModuleInfoItem;
    property Items[Index: integer]: TRw6ModuleInfoItem read GetItems write SetItems;
      default;
  end;

type

  { TRw6ModuleTextItem }

  TRw6ModuleTextItem = class(TCollectionItem)
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

  { TRw6ModuleTextList }

  TRw6ModuleTextList = class(TCollection)
    procedure SetItems(Index: integer; AValue: TRw6ModuleTextItem);
    function GetItems(Index: integer): TRw6ModuleTextItem;
  public
    constructor Create;
    function Add: TRw6ModuleTextItem;
    property Items[Index: integer]: TRw6ModuleTextItem read GetItems write SetItems;
      default;
  end;
     { {
                "_type": "ctrl-identity-info",
                "_title": "identity",
                "ctrl-name": "UNMT-JTF-PT",
                "ctrl-type": "Virtual Controller",
                "ctrl-level": "System Level"
            }}

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
  TListItems = class(TCollection)
  end;

const
  RAP_TASK_LI: string = 'rap-task-li';
  RWSERVICE_LI: string = 'rwservice-li';
  RAP_MODULE_INFO_LI = 'rap-module-info-li';
  RAP_MODULE_TEXT = 'rap-module-text';
  CTRL_IDENTITY_INFO = 'ctrl-identity-info';

Const
  PostOk : integer= 204;


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
  Valor: integer;
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
     Cadena := 'op'+Cadena;
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
  raise TRw6AbbWebServicesError.Create(ainfo);
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
  tipo: TJSONtype;
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
            tipo := DataResources.Items[I].Items[X].JSONType;
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



{ TRw6RwServiceList }

procedure TRw6RwServiceList.SetItems(Index: integer; AValue: TRw6RwServiceItem);
begin
  Items[Index].Assign(AValue);
end;

function TRw6RwServiceList.GetItems(Index: integer): TRw6RwServiceItem;
begin
  Result := TRw6RwServiceItem(inherited items[index]);
end;

constructor TRw6RwServiceList.Create;
begin
  inherited Create(TRw6RwServiceItem);
end;

function TRw6RwServiceList.Add: TRw6RwServiceItem;
begin
  Result := inherited Add as TRw6RwServiceItem;
  ;
end;

{ TRw6TaskList }

procedure TRw6TaskList.SetItems(Index: integer; AValue: TRw6TaskItem);
begin
  Items[Index].Assign(AValue);
end;

function TRw6TaskList.GetItems(Index: integer): TRw6TaskItem;
begin
  Result := TRw6TaskItem(inherited Items[Index]);
end;

constructor TRw6TaskList.Create;
begin
  inherited Create(TRw6TaskItem);
end;

function TRw6TaskList.Add: TRw6TaskItem;
begin
  Result := inherited Add as TRw6TaskItem;
end;

{ TRw6ModuleInfoList }

procedure TRw6ModuleInfoList.SetItems(Index: integer; AValue: TRw6ModuleInfoItem);
begin
  Items[Index].Assign(AValue);
end;

function TRw6ModuleInfoList.GetItems(Index: integer): TRw6ModuleInfoItem;
begin
  Result := TRw6ModuleInfoItem(inherited Items[Index]);

end;

constructor TRw6ModuleInfoList.Create;
begin
  inherited Create(TRw6ModuleInfoItem);
end;

function TRw6ModuleInfoList.Add: TRw6ModuleInfoItem;
begin
  Result := inherited Add as TRw6ModuleInfoItem;
end;

{ TRw6ModuleTextItem }

function TRw6ModuleTextItem.GetModuleText: string;
var
  Uri: TURI;
begin
  Uri := ParseURI(Fmodule_text);
  Result := uri.Document;
end;

function TRw6ModuleTextItem.GetFilePath: string;
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

procedure TRw6ModuleTextItem.Assign(Source: TPersistent);
begin
  if Source is TRw6ModuleTextItem then
  begin
    Fchange_count := TRw6ModuleTextItem(Source).Fchange_count;
    Ffile_path := TRw6ModuleTextItem(Source).Ffile_path;
    Fhref := TRw6ModuleTextItem(Source).Fhref;
    Fmodule_length := TRw6ModuleTextItem(Source).Fmodule_length;
    Fmodule_text := TRw6ModuleTextItem(Source).module_text;
    F_title := TRw6ModuleTextItem(Source).F_title;
    F_type := TRw6ModuleTextItem(Source).F_type;
  end
  else
  begin
    inherited Assign(Source);
  end;
end;

{ TRw6ModuleTextList }

procedure TRw6ModuleTextList.SetItems(Index: integer; AValue: TRw6ModuleTextItem);
begin
  items[Index].Assign(AValue);
end;

function TRw6ModuleTextList.GetItems(Index: integer): TRw6ModuleTextItem;
begin
  Result := TRw6ModuleTextItem(inherited Items[Index]);
end;

constructor TRw6ModuleTextList.Create;
begin
  inherited Create(TRw6ModuleTextItem);
end;

function TRw6ModuleTextList.Add: TRw6ModuleTextItem;
begin
  Result := inherited Add as TRw6ModuleTextItem;
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

end.
