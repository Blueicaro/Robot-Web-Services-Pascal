unit abbwstypes;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils;

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


//ios-signal-li

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



//ios-device-li
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
  end;

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
  end;
  inherited Assign(Source);
end;




end.
