unit main;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ExtCtrls,
  JSONPropStorage, StdCtrls, SynEdit;

type

  { TForm1 }

  TForm1 = class(TForm)
    brProbar: TButton;
    Memo1: TMemo;
    Memo2: TMemo;
    Panel1: TPanel;
    Splitter1: TSplitter;
    procedure brProbarClick(Sender: TObject);
    procedure Memo1Change(Sender: TObject);
  private

  public

  end;

var
  Form1: TForm1;

implementation

uses fpjson;

{$R *.lfm}

{ TForm1 }

procedure TForm1.brProbarClick(Sender: TObject);
var
  jData, Data, Data1, m, jorge, jj, g, hh: TJSONData;
  myJsonObject: TJSONObject;
  tipo, info: TJSONtype;
  I, x, n, H, j, p, K: integer;
  cadena: TJSONStringType;
  v: TJSONVariant;
  Cadena1: string;
begin
  Memo2.Clear;

  jData := GetJSON(Memo1.Lines.Text);

  myJsonObject := jData as TJSONObject;


  if myJsonObject.JSONType = jtObject then
  begin
    Data := myJsonObject.GetPath('_embedded').GetPath('resources');
    Memo2.Lines.Text := IntToStr(Data.Count);
    for I := 0 to Data.Count - 1 do
    begin
      Tipo := Data.Items[I].JSONType;
      Cadena := Data.Items[I].AsJSON;

      hh := Data.Items[I].FindPath('_type');
        tipo := hh.JSONType;
      for X := 0 to Data.Items[I].Count - 1 do
      begin

          Cadena := Data.Items[I].Items[X].AsJSON;

      end;
    end;

  end;
  jData.Free;
end;

procedure TForm1.Memo1Change(Sender: TObject);
begin

end;

end.
