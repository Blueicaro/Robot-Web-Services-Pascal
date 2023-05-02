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
    Ftitle: string;
    F_type: string;
  published
    property _title: string read Ftitle write Ftitle;
    property _type: string read F_type write F_type;
  end;
  { "_type": "cfg-dt-attribute",
            "_title": "Name",
            "name": "Name",
            "type": "string",
            "numbers": "1",
            "min": "",
            "max": "",
            "init": "",
            "mandatory": "false"}
   Type

   { TCfgDtAtributeItem }

 TCfgDtAtributeItem = class (TCollectionItem)
   private
     Finit: string;
     Fmandatory: string;
     Fmax: string;
     Fmin: string;
     Fname: strign;
     Fnumbers: String;
     Ftitle: string;
     Ttype: string;
   published
    property title: string read Ftitle write Ftitle;
    property Ttype: string read Ttype write Ttype;
    property name : string read Fname write Fname;
    property numbers: String read Fnumbers write Fnumbers;
    property min:string read Fmin write Fmin;
    property max: string read Fmax write Fmax;
    property init: string read Finit write Finit;
    property mandatory : string read Fmandatory write Fmandatory;

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
  CFG_DT_LI   :string = 'cfg-dt-li';
  CFG_DT_ATTRIBUTE : string = 'cfg-dt-attribute';

procedure ErrorWebService(ainfo: string);
{
 Gets a key a checks if is a protected word of pascal
}
function Formatjsonkey(aKeyName: string): string;

procedure GetClassList(aDatos: string; aListItems: TCollection;
  aItemClass: TCollectionItemClass; TipoLista:string);

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

procedure GetClassList(aDatos: string; aListItems: TCollection;
  aItemClass: TCollectionItemClass; TipoLista: string);
var
  I, X: integer;
  Cadena: TJSONStringType;
  jData, DataResources: TJSONData;
  myJsonObject: TJSONObject;
  NombreClave: string;
  propInfo: PPropInfo;
  ItemActual: TCollectionItem;
begin

  try
    jData := GetJSON(aDatos);
    myJsonObject := jData as TJSONObject;
    DataResources := myJsonObject.GetPath('_embedded').GetPath('resources');
    if DataResources = nil then
    begin
      ErrorWebService('No se puede procesar la respuesta');
    end;

    for I := 0 to DataResources.Count - 1 do
      if DataResources.Items[I].FindPath('_type').AsString = TipoLista then
      begin
        // RapTaskObject := TRapTaskObject.Create;
        ItemActual := aListItems.Add;
        for X := 2 to DataResources.Items[I].Count - 1 do
        begin
          if DataResources.Items[I].JSONType = jtObject then
          begin
            Cadena := DataResources.Items[I].Items[X].AsString;
            NombreClave := TJSONObject(DataResources.Items[I]).Names[X];
            propInfo := GetPropInfo(aItemClass, NombreClave);
            if propInfo <> nil then
            begin
              SetPropValue(ItemActual, propInfo, Cadena);
            end;
          end;
        end;
      end;
    jData.Free
  except
    on E: Exception do
    begin
      ErrorWebService(E.Message);
    end;
  end;

end;



end.
