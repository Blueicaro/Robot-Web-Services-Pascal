unit abbwstypes;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, fgl;

type
  TAbbWebServicesError = class(Exception);



type

  { TResourceItem }

  TResourceItem = class(TCollectionItem)
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

  { TIoDeviceItem }

  TIoDeviceItem = class(TCollectionItem)
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

  { TIosNetworkItem }

  TIosNetworkItem = class(TCollectionItem)
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

  { TCfgDtAtributeItem }

  TCfgDtAtributeItem = class(TCollectionItem)
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
  TModuleInfoItem = class(TCollectionItem)

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
  end;


  {Licencia}
type

  { TSysLicenceItem }

  TSysLicenceItem = class(TCollectionItem)
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

  { TSysProductItem }

  TSysProductItem = class(TCollectionItem)
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
  end;


{
 Descripción del sistema. Esta clase se usa como apoyo para generar luego
 un estructura Record. El motivo de usar una clase, es para rehusar el código que
 analiza el json
}

type

  { TSysSytemItem }

  TSysSytemItem = class(TCollectionItem)
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

  { TModuleTextItem }

  TModuleTextItem = class(TCollectionItem)
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

  { TElogDomainItem }

  TElogDomainItem = class(TCollectionItem)
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

  { TElogDomainList }

  TElogDomainList = class(TCollection)
    procedure SetItems(Index: integer; AValue: TElogDomainItem);
    function GetItems(Index: integer): TElogDomainItem;
  public
    constructor Create;
    function Add: TElogDomainItem;
    property Items[Index: integer]: TElogDomainItem read GetItems write SetItems;
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
  TSysSystemInfo = record
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

{ TIoSignalItem }

procedure TIoSignalItem.Assign(Source: TPersistent);
begin
  if Source is TIoSignalItem then
  begin
    Fcategory := TIoSignalItem(Source).category;
    Fhref := TIoSignalItem(Source).href;
    Flstate := TIoSignalItem(Source).lstate;
    Flvalue := TIoSignalItem(Source).lvalue;
    Fname := TIoSignalItem(Source).Name;
    Ftitle := TIoSignalItem(Source)._title;
    FTType := TIoSignalItem(Source).TType;
    F_type := TIoSignalItem(Source)._type;
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
  Items[Index].Assign(AValue);
end;

function TIoSignalList.GetItems(Index: integer): TIoSignalItem;
begin
  Result := TIoSignalItem(inherited items[index]);
end;

constructor TIoSignalList.Create;
begin
  inherited Create(TIoSignalItem);
end;

function TIoSignalList.Add: TIoSignalItem;
begin
  Result := inherited Add as TIoSignalItem;
end;



{ TIosNetworkItem }

procedure TIosNetworkItem.Assign(Source: TPersistent);
begin
  if Source is TIosNetworkItem then
  begin
    Fhref := TIosNetworkItem(Source).href;
    Ftitle := TIosNetworkItem(Source)._title;
    Ftype := TIosNetworkItem(Source)._type;
    Ftitle := TIosNetworkItem(Source).title;
    Fpstate := TIosNetworkItem(Source).pstate;
    Flstate := TIosNetworkItem(Source).lstate;
  end
  else
  begin
    inherited Assign(Source);
  end;
end;

{ TElogDomainItem }

procedure TElogDomainItem.Assign(Source: TPersistent);
begin

  if Source is TElogDomainItem then
  begin
    Fbuffsize := TElogDomainItem(Source).Fbuffsize;
    Fdomain_name := TElogDomainItem(Source).Fdomain_name;
    Fhref := TElogDomainItem(Source).Fhref;
    Fnumevts := TElogDomainItem(Source).Fnumevts;
    F_title := TElogDomainItem(Source).F_title;
    F_type := TElogDomainItem(Source).F_type;
    //inherited Assign(Source);
  end;

end;

{ TElogDomainList }

procedure TElogDomainList.SetItems(Index: integer; AValue: TElogDomainItem);
begin
  Items[Index].Assign(AValue);
end;

function TElogDomainList.GetItems(Index: integer): TElogDomainItem;
begin
  Result := TElogDomainItem(inherited items[Index]);
end;

constructor TElogDomainList.Create;
begin
  inherited Create(TElogDomainItem);
end;

function TElogDomainList.Add: TElogDomainItem;
begin
  Result := inherited Add as TElogDomainItem;
end;




end.
