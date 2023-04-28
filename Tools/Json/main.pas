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
  jData, Data, Data1, m: TJSONData;
  myJsonObject: TJSONObject;
  tipo, info: TJSONtype;
  I, x, n: integer;
  cadena: TJSONStringType;
  v: TJSONVariant;
  Cadena1: String;
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
          // Memo2.Lines.Add(data.Items[I].AsJSON);
          // Memo2.Lines.Add(Data.Items[I].FindPath('_title').AsString);
          if Data.Items[I].FindPath('_type').AsString = 'rap-task-li' then
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

            //For  x := 0 To Data.Items[I].Count do
            //begin
            //  n:= Data.Items[I].Items[X].Count;
            //end;
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
                Memo2.Lines.Add(Cadena1+':'+Cadena);
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

end.
