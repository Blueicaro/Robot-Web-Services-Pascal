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
  jData, Info: TJSONData;
  myJsonObject: TJSONObject;
  tipo: TJSONtype;
  Cadena: TJSONStringType;
  I: integer;
begin
  Memo2.Clear;

  jData := GetJSON(Memo1.Lines.Text);

  myJsonObject := jData as TJSONObject;

  Info := myJsonObject.FindPath('status');

  Cadena := Info.Items[0].AsString;

  memo2.Lines.Add('Status: ' + Cadena);

  info := myJsonObject.FindPath('state');
  //for I := 0 to info.Count - 1 do
  //begin
  //   cadena := TJSONObject(info.Items[I]).Names[0] ;
  //   Cadena:= info.Items[I].Items[0].value;
  //end;
  cadena := TJSONObject(info.items[0]).Names[0];
  Cadena := info.Items[0].items[0].Value;

  cadena := TJSONObject(info.Items[0]).Names[1];
  Cadena := info.Items[0].Items[1].Value;
  for I := 0 to info.Items[0].Count - 1 do
  begin
    memo2.Lines.add(TJSONObject(info.Items[0]).Names[I]);
    memo2.Lines.add(info.Items[0].Items[I].Value);
  end;
  jData.Free;
end;

procedure TForm1.Memo1Change(Sender: TObject);
begin

end;

end.
