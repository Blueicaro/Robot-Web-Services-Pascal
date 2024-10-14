unit main;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ExtCtrls, StdCtrls,
  ComCtrls, Grids, abbconexion, rw6abbwstypes, rw6robotwareservices;

type

  { Tmainfrm }

  Tmainfrm = class(TForm)
    btConnect: TButton;
    edUser: TEdit;
    edPassword: TEdit;
    edRobotAddress: TEdit;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Panel1: TPanel;
    Splitter1: TSplitter;
    stGridSenales: TStringGrid;
    procedure btConnectClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
  private
    Robot: TRobotConnection;
    function Connect: boolean;
  public

  end;

var
  mainfrm: Tmainfrm;

implementation

{$R *.lfm}

{ Tmainfrm }

procedure Tmainfrm.btConnectClick(Sender: TObject);
var
  I: integer;
  Data: array [1..6] of string;
  RobotWareServices: TRw6RobotWareServices;
  SenalesList: TIoSignalList;
begin
  if not Connect then Exit;
  try
    RobotWareServices := TRw6RobotWareServices.Create(Robot);
    try
      SenalesList := TIoSignalList.Create;
      RobotWareServices.GetIOSignals(SenalesList);
      for I := 0 to SenalesList.Count - 1 do
      begin
        //It := SenalesList[i];
        with SenalesList[I] do
        begin
          Data[1] := Name;
          Data[2] := Device;
          Data[3] := TType;
          Data[4] := category;
          Data[5] := lvalue;
          Data[6] := lstate;
        end;
        stGridSenales.InsertRowWithValues(1, Data);
      end;
    except
      on E: Exception do
      begin
        ShowMessage(E.Message);
      end;
    end;
  finally
    FreeAndNil(SenalesList);
    FreeAndNil(RobotWareServices);
  end;
end;

procedure Tmainfrm.FormCreate(Sender: TObject);
begin

end;

procedure Tmainfrm.FormDestroy(Sender: TObject);
begin
  FreeAndNil(Robot);
end;

function Tmainfrm.Connect: boolean;
begin
  Result := False;
  try
    if not Assigned(Robot) then
    begin
      Robot := TRobotConnection.Create(edRobotAddress.Text, edUser.Text,
        edPassword.Text);
    end
    else
    begin
      Robot.Conectar;
    end;
    Result := True;
  except
    on E: Exception do
      ShowMessage(E.Message);
  end;
end;

end.
