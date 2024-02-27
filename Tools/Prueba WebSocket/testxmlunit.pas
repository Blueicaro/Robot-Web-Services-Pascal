unit TestXmlUnit;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, DOM, Forms, Controls, Graphics, Dialogs, ExtCtrls,
  StdCtrls;

type

  { TForm1 }

  TForm1 = class(TForm)
    Button1: TButton;
    Memo1: TMemo;
    Memo2: TMemo;
    Panel1: TPanel;
    procedure Button1Click(Sender: TObject);
  private

  public

  end;

var
  Form1: TForm1;

implementation


{$R *.lfm}

{ TForm1 }
uses XMLRead;

procedure TForm1.Button1Click(Sender: TObject);
var
  XmlFile: TXMLDocument;
  fsXml: TStringStream;
  C, hijo: TDOMNode;
  I: LongWord;
  x: Integer;
begin
  try
    fsXml := TStringStream.Create(Memo1.Lines.Text);
    try
      ReadXMLFile(XmlFile, fsXml);
      C := XmlFile.FindNode('html').FindNode('body').FindNode('div');
      For I := 0 to C.ChildNodes.Count-1 do
      begin
        hijo := C.ChildNodes[I];
        if hijo.HasAttributes then
        begin
          For x := 0 to Hijo.Attributes.Length-1 do
          begin
            Memo2.Lines.Add(Hijo.Attributes[x].NodeName);
            Memo2.Lines.Add(Hijo.Attributes[X].NodeValue);
          end;
        end;
      end;

    finally
      FreeAndNil(XmlFile);
    end;
  finally
    FreeAndNil(fsXml);
  end;

end;

end.
