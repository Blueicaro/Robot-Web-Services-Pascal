unit rw7abbwstypes;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, fgl;

type
  TRw7AbbWebServicesError = class(Exception);



type

  { TRw7ResourceItem }

  TRw7ResourceItem = class(TCollectionItem)
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

  { TRw7IoSignalItem }

  TRw7IoSignalItem = class(TCollectionItem)
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

  { TRw7IoSignalList }

  TRw7IoSignalList = class(TCollection)
  private
    procedure SetItems(Index: integer; AValue: TRw7IoSignalItem);
    function GetItems(Index: integer): TRw7IoSignalItem;
  public
    constructor Create;
    function Add: TRw7IoSignalItem;
    property Items[Index: integer]: TRw7IoSignalItem read GetItems write SetItems;
      default;
  end;

type

  { TRw7IoDeviceItem }

  TRw7IoDeviceItem = class(TCollectionItem)
  private
    Faddress: string;
    Fhref: string;
    Flstate: string;
    Fname: string;
    Fpstate: string;
    Ftitle: string;
    F_type: string;
  published
    property href: string read Fhref write Fhref;
    property _title: string read Ftitle write Ftitle;
    property _type: string read F_type write F_type;
    property title: string read Ftitle write Ftitle;
    property Name: string read Fname write Fname;
    property pstate: string read Fpstate write Fpstate;
    property lstate: string read Flstate write Flstate;
    property address: string read Faddress write Faddress;
  end;

type

  { TRw7IosNetworkItem }

  TRw7IosNetworkItem = class(TCollectionItem)
  private
    Fhref: string;
    Flstate: string;
    Fname: string;
    Fpstate: string;
    Ftitle: string;
    Ftype: string;
    F_type: string;
  public
    procedure Assign(Source: TPersistent); override;
  published
    property href: string read Fhref write Fhref;
    property _title: string read Ftitle write Ftitle;
    property _type: string read F_type write F_type;
    property title: string read Ftitle write Ftitle;
    property Name: string read Fname write Fname;
    property pstate: string read Fpstate write Fpstate;
    property lstate: string read Flstate write Flstate;
  end;

type

  { TRw7CfgDtAtributeItem }

  TRw7CfgDtAtributeItem = class(TCollectionItem)
  private
    Finit: string;
    Fmandatory: string;
    Fmax: string;
    Fmin: string;
    Fname: string;
    Fnumbers: string;
    Ftitle: string;
    Ftype: string;
  published
    property title: string read Ftitle write Ftitle;
    property Ttype: string read Ftype write Ftype;
    property Name: string read Fname write Fname;
    property numbers: string read Fnumbers write Fnumbers;
    property min: string read Fmin write Fmin;
    property max: string read Fmax write Fmax;
    property init: string read Finit write Finit;
    property mandatory: string read Fmandatory write Fmandatory;

  end;

{
 Información sobre una tarea (task)
}
type

  { TRw7TaskItem }

  TRw7TaskItem = class(TCollectionItem)
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

  {Información sobre el módulo}
type
  TRw7ModuleInfoItem = class(TCollectionItem)

  private
    fname: string;
    Ftitle: string;
    FTtype: string;
    F_Type: string;
  published
    property _type: string read F_Type write F_type;
    property title: string read Ftitle write Ftitle;
    property Name: string read fname write fname;
    property Ttype: string read FTtype write FTtype;
  end;

  {Tipo de mecánica}
type

  { TRw7RobotTypeItem }

  TRw7RobotTypeItem = class(TCollectionItem)
  private
    Frobot_type: string;
    F_title: string;
    F_type: string;
  published
    property _type: string read F_type write F_type;
    property _title: string read F_title write F_title;
    property robot_type: string read Frobot_type write Frobot_type;
  end;


  {Licencia}
type

  { TRw7SysLicenceItem }

  TRw7SysLicenceItem = class(TCollectionItem)
  private
    Flicense: string;
    F_title: string;
    F_type: string;
  published
    property _type: string read F_type write F_type;
    property _title: string read F_title write F_title;
    property license: string read Flicense write Flicense;
  end;

{
 Información sobre productos instalados
}
type

  { TRw7SysProductItem }

  TRw7SysProductItem = class(TCollectionItem)
  private
    Fversion: string;
    Fversion_name: string;
    F_title: string;
    F_type: string;
  published
    property _type: string read F_type write F_type;
    property _title: string read F_title write F_title;
    property version: string read Fversion write Fversion;
    property version_name: string read Fversion_name write Fversion_name;
  end;


{
 Lista de opciones instaladas en el controlador
}
type

  { TRw7SysOptionItem }

  TRw7SysOptionItem = class(TCollectionItem)
  private
    Foption: string;
    F_title: string;
    F_type: string;
  published
    property _type: string read F_type write F_type;
    property _title: string read F_title write F_title;
    property option: string read Foption write Foption;
  end;


{
 Descripción del sistema. Esta clase se usa como apoyo para generar luego
 un estructura Record. El motivo de usar una clase, es para rehusar el código que
 analiza el json
}

type

  { TRw7SysSytemItem }

  TRw7SysSytemItem = class(TCollectionItem)
  private
    Fbuild: string;
    FBuilTag: string;
    Fdate: string;
    Fdescription: string;
    Fmajor: string;
    Fminor: string;
    Fname: string;
    Frevision: string;
    Frobapi_compatible: string;
    Frwversion: string;
    Frwversionname: string;
    Fstarttm: string;
    Fsub_revision: string;
    Fsub_version: string;
    Fsysid: string;
    Ftitle: string;
    FTtype: string;
    F_title: string;
    F_type: string;
  published
    property _type: string read F_type write F_type;
    property _title: string read F_title write F_title;
    property major: string read Fmajor write Fmajor;
    property minor: string read Fminor write Fminor;
    property build: string read Fbuild write Fbuild;
    property revision: string read Frevision write Frevision;
    property sub_revision: string read Fsub_version write Fsub_revision;
    property buildtag: string read FBuilTag write FBuilTag;
    property robapi_compatible: string read Frobapi_compatible write Frobapi_compatible;
    property title: string read Ftitle write Ftitle;
    property Ttype: string read FTtype write FTtype;
    property description: string read Fdescription write Fdescription;
    property date: string read Fdate write Fdate;
    property Name: string read Fname write Fname;
    property rwversion: string read Frwversion write Frwversion;
    property sysid: string read Fsysid write Fsysid;
    property starttm: string read Fstarttm write Fstarttm;
    property rwversionname: string read Frwversionname write Frwversionname;
  end;

{
  Contenido de una modulo. En el caso que  el modulo no sea muy grande, se
  devuelve su contenido en la propiedad module_text.
  En el caso que sea muy grande, devuelve la ruta a un archivo que contiene
  el módulo, en la propiedad file_path
}
type

  { TRw7ModuleTextItem }

  TRw7ModuleTextItem = class(TCollectionItem)
  private
    Ffile_path: string;
    Fmodule_length: string;
    Fmodule_text: string;
    F_title: string;
    F_type: string;
  published
    property _type: string read F_type write F_type;
    property _title: string read F_title write F_title;
    property module_text: string read Fmodule_text write Fmodule_text;
    property file_path: string read Ffile_path write Ffile_path;
    property module_length: string read Fmodule_length write Fmodule_length;
  end;


type

  { TRw7ElogMessageItem }

  TRw7ElogMessageItem = class(TCollectionItem)
  private
    Fargc: string;
    Fcauses: string;
    Fcode: string;
    Fconseqs: string;
    Fdesc: string;
    Fhref: string;
    Fmsgtype: string;
    Ftitle: string;
    Ftstamp: string;
    F_title: string;
    F_type: string;
  public
    procedure Assign(Source: TPersistent); override;
  published
    property _type: string read F_type write F_type;
    property _title: string read F_title write F_title;
    property msgtype: string read Fmsgtype write Fmsgtype;
    property code: string read Fcode write Fcode;
    property tstamp: string read Ftstamp write Ftstamp;
    property title: string read Ftitle write Ftitle;
    property desc: string read Fdesc write Fdesc;
    property conseqs: string read Fconseqs write Fconseqs;
    property causes: string read Fcauses write Fcauses;
    property argc: string read Fargc write Fargc;
    property href: string read Fhref write Fhref;
  end;


type

  { TRw7ElogMessageList }

  TRw7ElogMessageList = class(TCollection)

    procedure SetItems(Index: integer; AValue: TRw7ElogMessageItem);
    function GetItems(Index: integer): TRw7ElogMessageItem;
  public
    constructor Create;
    function Add: TRw7ElogMessageItem;
    property Items[Index: integer]: TRw7ElogMessageItem read GetItems write SetItems;
      default;
  end;

type

  { TRw7ElogDomainItem }

  TRw7ElogDomainItem = class(TCollectionItem)
  private
    Fbuffsize: string;
    Fdomain_name: string;
    Fhref: string;
    Fnumevts: string;
    F_title: string;
    F_type: string;
  public
    procedure Assign(Source: TPersistent); override;
  published
    property href: string read Fhref write Fhref;
    property _type: string read F_type write F_type;
    property _title: string read F_title write F_title;
    property domain_name: string read Fdomain_name write Fdomain_name;
    property numevts: string read Fnumevts write Fnumevts;
    property buffsize: string read Fbuffsize write Fbuffsize;
  end;

type

  { TRw7ElogDomainList }

  TRw7ElogDomainList = class(TCollection)
    procedure SetItems(Index: integer; AValue: TRw7ElogDomainItem);
    function GetItems(Index: integer): TRw7ElogDomainItem;
  public
    constructor Create;
    function Add: TRw7ElogDomainItem;
    property Items[Index: integer]: TRw7ElogDomainItem read GetItems write SetItems;
      default;
  end;

type
  TListItems = class(TCollection)
  end;

const
  CFG_DOMAIN_LI: string = 'cfg-domain-li';
  RAP_TASK_LI: string = 'rap-task-li';
  CFG_DT_LI: string = 'cfg-dt-li';
  CFG_DT_ATTRIBUTE: string = 'cfg-dt-attribute';
  RAP_MODULE_INFO_LI: string = 'rap-module-info-li';
  IOS_NETWORK_LI: string = 'ios-network-li';
  IOS_DEVICE_LI: string = 'ios-device-li';
  IOS_SIGNAL_LI: string = 'ios-signal-li';
  SYS_ROBOTTYPE: string = 'sys-robottype';
  SYS_LICENSE: string = 'sys-license';
  SYS_PRODUCT: string = 'sys-product-li';
  SYS_OPTION: string = 'sys-option-li';
  SYS_SYSTEM: string = 'sys-system';
  RAP_MODULE_TEXT: string = 'rap-module-text';
  ELOG_DOMAIN_LI: string = 'elog-domain-li';
  ELOG_MESSAGE_LI: string = 'elog-message-li';

{
 Estructura que contiene la información del sistema o controlador
 Por ejemplo:

              "major": "7",
              "minor": "10",
              "build": "1816",
              "revision": "0",
              "sub-revision": "0",
              "buildtag": "",
              "robapi-compatibility-revision": "3",
              "title": "RobotControl",
              "type": "RobotWare",
              "description": "Controller Software",
              "date": "2023-06-19",
              "name": "RestApi",
              "rwversion": "7.10.0+329",
              "sysid": "{5E442FF3-37AB-4238-9DE7-F5624B735DC3}",
              "starttm": "2023-07-17 T 15:53:14",
              "rwversionname": "7.10.0"
}
type
  TRw7SysSystemInfo = record
    Major: string;
    Minor: string;
    Build: string;
    Revision: string;
    SubRevision: string;
    BuildTag: string;
    RobapiCompatibilityRevison: string;
    Title: string;
    TypeOs: string;
    Description: string;
    Date: string;
    Name: string;
    RwVersion: string;
    SysId: string;
    StartTm: string;
    RwVersionName: string;
  end;
{
  Estructura que contiene información sobre un mensaje
}
type
  TRw7ElogMessageInfo = record
    msgtype: string;
    code: string;
    tstamp: string;
    title: string;
    description: string;
    consequences: string;
    causes: string;
    actions: string;
    argc: string;
  end;

procedure ErrorWebService(ainfo: string);
{
 Gets a key a checks if is a protected word of pascal
}
function Formatjsonkey(aKeyName: string): string;

procedure GetEmbeddedClassList(aDatos: string; aListItems: TCollection;
  aItemClass: TCollectionItemClass; TipoLista: string);

procedure GetStatusClassList(aDatos: string; aListItems: TCollection;
  aItemClass: TCollectionItemClass; TipoLista: string);

implementation

uses StrUtils, jsonparser, fpjson, TypInfo;

procedure ErrorWebService(ainfo: string);
begin
  raise TRw7AbbWebServicesError.Create(ainfo);
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

procedure GetEmbeddedClassList(aDatos: string; aListItems: TCollection;
  aItemClass: TCollectionItemClass; TipoLista: string);
var
  I, X: integer;
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
    DataResources := myJsonObject.GetPath('_embedded').GetPath('resources');
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

procedure GetStatusClassList(aDatos: string; aListItems: TCollection;
  aItemClass: TCollectionItemClass; TipoLista: string);
var
  jData, DataResources: TJSONData;
  myJsonObject: TJSONObject;
  I, X: integer;
  Cadena: TJSONStringType;
  NombreClave: string;
  propInfo: PPropInfo;
  ItemActual: TCollectionItem;
begin
  try
    jData := GetJSON(aDatos);
    myJsonObject := jData as TJSONObject;
    DataResources := myJsonObject.GetPath('state');
    for I := 0 to DataResources.Count - 1 do
    begin
      if DataResources.Items[I].JSONType = jtObject then
      begin
        if DataResources.Items[I].FindPath('_type').AsString = TipoLista then
        begin
          ItemActual := aListItems.Add;
          for X := 0 to DataResources.Items[I].Count - 1 do
          begin
            if DataResources.Items[I].Items[X].JSONType = jtString then
            begin
              Cadena := DataResources.Items[I].Items[X].AsString;
              NombreClave := TJSONObject(DataResources.Items[I]).Names[X];
              NombreClave := Formatjsonkey(NombreClave);
              propInfo := GetPropInfo(aItemClass, NombreClave);
              if (propInfo <> nil) and (cadena <> '') then
              begin

                SetPropValue(ItemActual, propInfo, Cadena);
              end;
            end;
          end;
        end;
      end;
    end;
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
  if Assigned(jData) then
  begin
    jData.Free;
  end;
end;

{ TRw7IoSignalItem }

procedure TRw7IoSignalItem.Assign(Source: TPersistent);
begin
  if Source is TRw7IoSignalItem then
  begin
    Fcategory := TRw7IoSignalItem(Source).category;
    Fhref := TRw7IoSignalItem(Source).href;
    Flstate := TRw7IoSignalItem(Source).lstate;
    Flvalue := TRw7IoSignalItem(Source).lvalue;
    Fname := TRw7IoSignalItem(Source).Name;
    Ftitle := TRw7IoSignalItem(Source)._title;
    FTType := TRw7IoSignalItem(Source).TType;
    F_type := TRw7IoSignalItem(Source)._type;
  end
  else
  begin
    inherited Assign(Source);
  end;
end;

function TRw7IoSignalItem.Device: string;
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

{ TRw7IoSignalList }

procedure TRw7IoSignalList.SetItems(Index: integer; AValue: TRw7IoSignalItem);
begin
  Items[Index].Assign(AValue);
end;

function TRw7IoSignalList.GetItems(Index: integer): TRw7IoSignalItem;
begin
  Result := TRw7IoSignalItem(inherited items[index]);
end;

constructor TRw7IoSignalList.Create;
begin
  inherited Create(TRw7IoSignalItem);
end;

function TRw7IoSignalList.Add: TRw7IoSignalItem;
begin
  Result := inherited Add as TRw7IoSignalItem;
end;



{ TRw7IosNetworkItem }

procedure TRw7IosNetworkItem.Assign(Source: TPersistent);
begin
  if Source is TRw7IosNetworkItem then
  begin
    Fhref := TRw7IosNetworkItem(Source).href;
    Ftitle := TRw7IosNetworkItem(Source)._title;
    Ftype := TRw7IosNetworkItem(Source)._type;
    Ftitle := TRw7IosNetworkItem(Source).title;
    Fpstate := TRw7IosNetworkItem(Source).pstate;
    Flstate := TRw7IosNetworkItem(Source).lstate;
  end
  else
  begin
    inherited Assign(Source);
  end;
end;

{ TRw7ElogMessageItem }

procedure TRw7ElogMessageItem.Assign(Source: TPersistent);
begin
  if Source is TRw7ElogMessageItem then
  begin
    Fargc := TRw7ElogMessageItem(Source).Fargc;
    Fcauses := TRw7ElogMessageItem(Source).Fcauses;
    Fcode := TRw7ElogMessageItem(Source).Fcode;
    Fconseqs := TRw7ElogMessageItem(Source).Fconseqs;
    Fdesc := TRw7ElogMessageItem(Source).Fdesc;
    Fmsgtype := TRw7ElogMessageItem(Source).Fmsgtype;
    Ftitle := TRw7ElogMessageItem(Source).Ftitle;
    Ftstamp := TRw7ElogMessageItem(Source).Ftstamp;
    F_title := TRw7ElogMessageItem(Source).F_title;
    F_type := TRw7ElogMessageItem(Source).F_type;
    Fhref := TRw7ElogMessageItem(Source).Fhref;
  end
  else
  begin
    inherited Assign(Source);
  end;
end;

{ TRw7ElogMessageList }

procedure TRw7ElogMessageList.SetItems(Index: integer; AValue: TRw7ElogMessageItem);
begin
  Items[Index].Assign(AValue);
end;

function TRw7ElogMessageList.GetItems(Index: integer): TRw7ElogMessageItem;
begin
  Result := TRw7ElogMessageItem(inherited items[Index]);
end;

constructor TRw7ElogMessageList.Create;
begin
  inherited Create(TRw7ElogMessageItem);
end;

function TRw7ElogMessageList.Add: TRw7ElogMessageItem;
begin
  Result := inherited Add as TRw7ElogMessageItem;
end;

{ TRw7ElogDomainItem }

procedure TRw7ElogDomainItem.Assign(Source: TPersistent);
begin

  if Source is TRw7ElogDomainItem then
  begin
    Fbuffsize := TRw7ElogDomainItem(Source).Fbuffsize;
    Fdomain_name := TRw7ElogDomainItem(Source).Fdomain_name;
    Fhref := TRw7ElogDomainItem(Source).Fhref;
    Fnumevts := TRw7ElogDomainItem(Source).Fnumevts;
    F_title := TRw7ElogDomainItem(Source).F_title;
    F_type := TRw7ElogDomainItem(Source).F_type;
  end
  else
  begin
    inherited Assign(Source);
  end;

end;

{ TRw7ElogDomainList }

procedure TRw7ElogDomainList.SetItems(Index: integer; AValue: TRw7ElogDomainItem);
begin
  Items[Index].Assign(AValue);
end;

function TRw7ElogDomainList.GetItems(Index: integer): TRw7ElogDomainItem;
begin
  Result := TRw7ElogDomainItem(inherited items[Index]);
end;

constructor TRw7ElogDomainList.Create;
begin
  inherited Create(TRw7ElogDomainItem);
end;

function TRw7ElogDomainList.Add: TRw7ElogDomainItem;
begin
  Result := inherited Add as TRw7ElogDomainItem;
end;




end.
