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
  jData, Data, Data1, m, jorge, jj: TJSONData;
  myJsonObject: TJSONObject;
  tipo, info: TJSONtype;
  I, x, n, H, j, p: integer;
  cadena: TJSONStringType;
  v: TJSONVariant;
  Cadena1: string;
begin
  Memo2.Clear;
  try
    try
      jData := GetJSON(Memo1.Lines.Text);

      myJsonObject := jData as TJSONObject;


      if myJsonObject.JSONType = jtObject then
      begin
        Data := myJsonObject.GetPath('_embedded').GetPath('resources');
        tipo := Data.JSONType;
        Memo2.Lines.Add(IntToStr(Data.Count));
        for I := 0 to Data.Count - 1 do
        begin

          if Data.Items[I].FindPath('_links').JSONType = jtObject then
          begin
            Data1 := Data.Items[I];
            Tipo := Data1.JSONType;



            Tipo := Data1.JSONType;
            cadena := data1.AsJSON;
            if Data1.Items[0].Items[0].FindPath('href') <> nil then
            begin
              tipo := Data1.Items[0].Items[0].Items[0].JSONType;
              cadena := Data1.Items[0].Items[0].Items[0].AsString;
              Memo2.Lines.Add(Cadena);
            end;

          end;

          if Data.Items[I].FindPath('_type').AsString = 'cfg-domain-li' then
          begin
          {
           "_title": "T_ROB1",
                "_type": "rap-task-li",
                "active": "On",
                "excstate": "ready",
                "motiontask": "TRUE",
                "name": "T_ROB1",
                "taskstate": "linked",
                "type": "normal"}

            Data1 := Data.Items[I];
            info := Data.Items[I].JSONType;
            n := Data.Items[I].Count;
            for X := 2 to Data.Items[I].Count - 1 do
            begin
              tipo := Data.Items[I].Items[X].JSONType;
              if tipo = jtString then
              begin
                Cadena := Data.Items[I].Items[X].AsString;

                Cadena1 := TJSONObject(Data.Items[I]).Names[X];
                Memo2.Lines.Add(Cadena1 + ':' + Cadena);
              end;
            end;
            Memo2.Lines.Add('--------------------');
            //Memo2.Lines.Add(Data.Items[I].FindPath('name').AsString);
          end;

        end;
      end;
    except
      on E: Exception do
        ShowMessage(E.Message);
    end;


  finally
    FreeAndNil(jData);
  end;

end;

procedure TForm1.Memo1Change(Sender: TObject);
begin

end;

end.
