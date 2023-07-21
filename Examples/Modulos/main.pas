unit main;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ExtCtrls, StdCtrls,
  ComCtrls, AbbWebServices;

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
    mmContent: TMemo;
    Panel1: TPanel;
    Splitter1: TSplitter;
    TreeView1: TTreeView;
    procedure btConnectClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure TreeView1Click(Sender: TObject);
  private
    Robot: TAbbWebServices;
    procedure GetModule(aTaskName, aModuleName: string);
  public

  end;

var
  mainfrm: Tmainfrm;

implementation

{$R *.lfm}

{ Tmainfrm }

procedure Tmainfrm.btConnectClick(Sender: TObject);
var
  TaskList, ModulesList: TStringList;
  I, J: integer;
  root, TaskNode: TTreeNode;
begin
  TreeView1.Items.Clear;
  //Robot.Conection.SetRobotUrl(edRobotAddress.Text);
  //Robot.Conection.SetUserPassword(edUser.Text, edPassword.Text);
  root := TreeView1.Items.Add(nil, edRobotAddress.Text);
  try
    TaskList := TStringList.Create;
    ModulesList := TStringList.Create;
    try
      Robot.RobotWare.GetTasksList(TaskList);
      for I := 0 to TaskList.Count - 1 do
      begin
        TaskNode := TreeView1.Items.AddChild(root, TaskList[i]);
        ModulesList.Clear;
        Robot.RobotWare.GetModulesList(TaskList[i], ModulesList);
        for J := 0 to ModulesList.Count - 1 do
        begin
          TreeView1.Items.AddChild(TaskNode, ModulesList[J]);
        end;
      end;
      TreeView1.Enabled := True;
    except
      on E: Exception do
      begin
        ShowMessage(E.Message);
        TreeView1.Items.Clear;
      end;
    end;
  finally
    FreeAndNil(TaskList);
    FreeAndNil(ModulesList);
  end;
end;

procedure Tmainfrm.FormCreate(Sender: TObject);
begin
  Robot := TAbbWebServices.Create('https://localhost:80');
end;

procedure Tmainfrm.FormDestroy(Sender: TObject);
begin
  FreeAndNil(Robot);
end;

procedure Tmainfrm.TreeView1Click(Sender: TObject);
var
  Selected: TTreeNode;
  ModuleName, TaskName: string;
begin
  if TreeView1.Selected = nil then Exit;
  Selected := TreeView1.Selected;
  if Selected.HasChildren = False then
  begin
    ModuleName := Selected.Text;
    TaskName := Selected.Parent.Text;
    GetModule(TaskName, ModuleName);
  end;
end;

procedure Tmainfrm.GetModule(aTaskName, aModuleName: string);
var
  content: TStringList;
begin
  try
    mmContent.Clear;
    content := TStringList.Create;
    try
      Robot.RobotWare.GetModuleText(aTaskName, aModuleName, content);
      mmContent.Lines.Text := content.Text;
    except
      on E: Exception do
        ShowMessage(E.Message);
    end;
  finally
    FreeAndNil(content)
  end;
end;

end.
