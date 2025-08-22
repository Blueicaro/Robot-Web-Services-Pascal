unit main;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, ExtCtrls,
  rw7webservices, rw7abbwstypes, rw7robotwareservices;

type

  { TForm1 }

  TForm1 = class(TForm)
    btConectar: TButton;
    btWebSocket: TButton;
    edConectar: TEdit;
    Memo1: TMemo;
    Panel1: TPanel;
    procedure btConectarClick(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure Panel1MouseMove(Sender: TObject; Shift: TShiftState; X, Y: integer);

  private
    Robot: TRw7WebServices;
    procedure Change(Sender: TObject; Rw7SysSystemInfo: TRw7SysSystemInfo; status:integer);
  public

  end;

var
  Form1: TForm1;

implementation

{$R *.lfm}

{ TForm1 }

procedure TForm1.btConectarClick(Sender: TObject);
var
  Lista: TStringList;
begin
  if not Assigned(Robot) then
  begin
    Robot := TRw7WebServices.Create('https://localhost:80');
  end;
  Robot.RobotWare.OnSystemInfo := @Form1.change;
  Robot.RobotWare.GetSystemInfo;
end;

procedure TForm1.FormDestroy(Sender: TObject);
begin
  if Assigned(Robot) then
  begin
    FreeAndNil(Robot);
  end;
end;

procedure TForm1.Panel1MouseMove(Sender: TObject; Shift: TShiftState; X, Y: integer);
begin

end;

procedure TForm1.Change(Sender: TObject; Rw7SysSystemInfo: TRw7SysSystemInfo;
  status: integer);
begin
  Memo1.Lines.Add('Evento ' + Sender.ToString);
  Memo1.Lines.Add(Rw7SysSystemInfo.RwVersion);
  Memo1.Lines.Add('Status '+InttoStr(Status));

end;

end.
