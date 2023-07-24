unit main;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ExtCtrls, StdCtrls,
  ComCtrls, Grids, AbbWebServices, abbwstypes;

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
    Robot: TAbbWebServices;
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
  SenalesList: TIoSignalList;
  data:  Array [1..6] of string;
  It: TIoSignalItem;
begin
  Robot.SetRobotUrl(edRobotAddress.Text);
  Robot.SetUserPassword(edUser.Text, edPassword.Text);
  try
    SenalesList := TIoSignalList.Create;
    try
      Robot.RobotWare.GetSignalsList(SenalesList);
      for I := 0 to SenalesList.Count - 1 do
      begin
        It := SenalesList[i];
        with SenalesList[I] do
        begin
        Data[1]:= Name;
        Data[2]:=Device;
        Data[3] := TType;
        data[4]:=category;
        Data[5] := lvalue;
        Data[6] := lstate;
        end;
        stGridSenales.InsertRowWithValues(1,Data);
      end;
    except
      on E: Exception do
      begin
        ShowMessage(E.Message);
      end;
    end;
  finally
    FreeAndNil(SenalesList);
  end;
end;

procedure Tmainfrm.FormCreate(Sender: TObject);
begin
  Robot := TAbbWebServices.Create;
end;

procedure Tmainfrm.FormDestroy(Sender: TObject);
begin
  FreeAndNil(Robot);
end;

end.
