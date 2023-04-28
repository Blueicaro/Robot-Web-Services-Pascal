unit abbwstypes;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils;

type
  TAbbWebServicesError = class(Exception);

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






procedure ErrorWebService(ainfo: string);
{
 Gets a key a checks if is a protected word of pascal
}
function Formatjsonkey(aKeyName: string): string;

procedure GetClassList(aDatos: string; aListItems: TCollection;
  aItemClass: TCollectionItemClass);

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
  aItemClass: TCollectionItemClass);
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
      if DataResources.Items[I].FindPath('_type').AsString = 'rap-task-li' then
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
