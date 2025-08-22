unit main;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ExtCtrls, StdCtrls,
  abbconexion;

type

  { TForm1 }

  TForm1 = class(TForm)
    Button1: TButton;
    Memo1: TMemo;
    Panel1: TPanel;
    procedure Button1Click(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
  private
     Robot: TRobotConnection;
  public

  end;

var
  Form1: TForm1;

implementation

{$R *.lfm}

{ TForm1 }

procedure TForm1.Button1Click(Sender: TObject);

var
  Parametros: TStringList;
begin
  Robot := TRobotConnection.Create('https://localhost:80');
  Robot.Get('/rw/panel/opmode') ;
  Memo1.Lines.Assign(Robot.Respuesta);
  Parametros := TStringList.Create;
  Parametros.Add()
end;

procedure TForm1.FormDestroy(Sender: TObject);
begin
  if Assigned(Robot) then
  begin
    FreeAndNil(Robot);
  end;
end;

end.

