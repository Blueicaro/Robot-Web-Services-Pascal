unit main;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ExtCtrls, StdCtrls,
  ComCtrls, abbconexion, abbwstypes, rw6robotwareservices, rw6fileservices;

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
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure TreeView1Click(Sender: TObject);
  private
    Robot: TRobotConnection;
    RobotwareServices: TRw6RobotWareServices;
    procedure GetModule(aTaskName: string; aModuleName: string);
  public

  end;

var
  mainfrm: Tmainfrm;

implementation

uses StrUtils;
  {$R *.lfm}

  { Tmainfrm }

procedure Tmainfrm.btConnectClick(Sender: TObject);
var
  root, TaskNode: TTreeNode;

  TaskList: TRw6TaskList;
  ModuleList: TRw6ModuleInfoList;
  I, J: integer;
begin

  TreeView1.Items.Clear;

  try
    if not assigned(robot) then
    begin
      Robot := TRobotConnection.Create(edRobotAddress.Text, edUser.Text,
        edPassword.Text);
    end
    else
    begin
      Robot.RobotUrl := edRobotAddress.Text;
      Robot.SetUserPassword(edUser.Text, edPassword.Text);
      Robot.Conectar;
    end;
    if not Assigned(RobotwareServices) then
    begin
      RobotwareServices := TRw6RobotWareServices.Create(Robot);
    end;
    root := TreeView1.Items.Add(nil, edRobotAddress.Text);
    try
      TaskList := TRw6TaskList.Create;
      RobotwareServices.GetTaskList(TaskList);
      for I := 0 to TaskList.Count - 1 do
      begin
        TaskNode := TreeView1.Items.AddChild(root, TaskList[I].Name);
        ModuleList := TRw6ModuleInfoList.Create;
        RobotwareServices.GetRapidModules(ModuleList, TaskList[I].Name);
        for J := 0 to ModuleList.Count - 1 do
        begin
          TreeView1.Items.AddChild(TaskNode, ModuleList[J]._title);
        end;
        FreeAndNil(ModuleList);
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
    FreeAndNil(ModuleList);
  end;
end;

procedure Tmainfrm.FormClose(Sender: TObject; var CloseAction: TCloseAction);
begin
  FreeAndNil(RobotwareServices);
  FreeAndNil(Robot);
end;

procedure Tmainfrm.FormCreate(Sender: TObject);
begin

end;

procedure Tmainfrm.FormDestroy(Sender: TObject);
begin

end;

procedure Tmainfrm.TreeView1Click(Sender: TObject);
var
  Nodo: TTreeNode;
  Nombre, TaskName: string;
begin
  if TreeView1.Selected = nil then Exit;
  Nodo := TreeView1.Selected;
  if Nodo.HasChildren = False then
  begin
    Nombre := Nodo.Text;
    Nombre := ExtractDelimited(2, Nombre, ['/']);
    TaskName := Nodo.Parent.Text;
    GetModule(TaskName, Nombre);
  end;
end;

procedure Tmainfrm.GetModule(aTaskName: string; aModuleName: string);
var
  ModuleContent: TRw6ModuleTextItem;
  Cadena: string;
  FileService: TRw6FileServices;
  Contenido: TStringList;
begin

  try
    ModuleContent := TRw6ModuleTextItem.Create(nil);
    RobotwareServices.GetModuleText(aModuleName, aTaskName, ModuleContent);
    mmContent.Clear;
    Cadena := ModuleContent.file_path;
    if Cadena = '' then
    begin
      mmContent.Text := ModuleContent.module_text;
    end
    else
    begin
      try
        Contenido := TStringList.Create;
        FileService := TRw6FileServices.Create(Robot);
        FileService.Getfile(Cadena, Contenido);
        mmContent.Text := Contenido.Text;
      finally
        FreeAndNil(Contenido);
        FreeAndNil(FileService);
      end;
    end;
  finally
    FreeAndNil(ModuleContent);
  end;

end;

end.
