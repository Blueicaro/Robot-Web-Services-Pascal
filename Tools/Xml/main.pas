unit main;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ExtCtrls, StdCtrls;

type

  { TForm1 }

  TForm1 = class(TForm)
    btProcresar: TButton;
    Memo1: TMemo;
    Memo2: TMemo;
    Panel1: TPanel;
    Splitter1: TSplitter;
    procedure btProcresarClick(Sender: TObject);
  private

  public

  end;

var
  Form1: TForm1;

implementation

uses Laz2_DOM, laz2_XMLRead;

{$R *.lfm}

{ TForm1 }
function GetClass(aXml: TXMLDocument): string;
var
  body: TDOMNode;
begin
  Result := '';
  body := aXml.FirstChild.ChildNodes[1];
  if body = nil then
  begin
    exit;
  end;
  if body.FirstChild = nil then
  begin
    exit;
  end;
  if body.FirstChild.HasAttributes = false then
  begin
    exit;
  end;
  Result := body.FirstChild.Attributes[0].NodeValue;
end;

procedure TForm1.btProcresarClick(Sender: TObject);
var
  XmlDoc: TXMLDocument;
  St: TStringStream;
  Node: TDOMNode;
begin
  St := TStringStream.Create(Memo1.Lines.Text);
  try
    ReadXMLFile(XmlDoc, St);
    //Node := XmlDoc.FirstChild;
    //Memo2.Lines.Add(BoolToStr(Node.HasChildNodes, True));
    //Memo2.Lines.Add(XmlDoc.FirstChild.ChildNodes[1].NodeName);
    Memo2.Lines.Add(GetClass(XmlDoc));
  finally
    St.Free;
    xmlDoc.Free;
  end;
end;

end.
