unit rw7webservices;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, fpjson, jsonparser,
  abbconexion, rw7robotwareservices, rw7ControllerServices, rw7FileServices,
  rw7elogservices;

type

  { TRw7WebServices }

  TRw7WebServices = class
  private
    FConection: TRobotConnection;
    FController: TRw7ControllerServices;
    FElogService: TRw7ElogService;
    FFileService: TRw7FileService;
    FRobotWareService: TRw7RobotWareService;
  public
    // URL or ip of the robot
    procedure SetRobotUrl(aUrl: string);
    procedure SetUserPassword(aUser: string; aPassword: string);
    procedure LogOut;
    property Connection: TRobotConnection read FConection;
    property RobotWare: TRw7RobotWareService read FRobotWareService;
    property Controller: TRw7ControllerServices read FController;
    property FileService: TRw7FileService read FFileService;
    property ElogService: TRw7ElogService read FElogService write FElogService;


  public
    constructor Create;
    constructor Create(aUrlRobot: string; aUser: string = 'Default User';
      aPassword: string = 'robotics'); overload;
    destructor Destroy; override;
  end;


implementation

uses TypInfo;

  { TRw7WebServices }

procedure TRw7WebServices.SetRobotUrl(aUrl: string);
begin
  FConection.SetRobotUrl(aUrl);
end;

procedure TRw7WebServices.SetUserPassword(aUser: string; aPassword: string);
begin
  FConection.SetUserPassword(aUser, aPassword);
end;

procedure TRw7WebServices.LogOut;
begin
  FConection.Get('logout');
end;



constructor TRw7WebServices.Create;
begin
  FConection := TRobotConnection.Create;

  FRobotWareService := TRw7RobotWareService.Create(FConection);
  FController := TRw7ControllerServices.Create(FConection);
  FFileService := TRw7FileService.Create(FConection);
  FElogService := TRw7ElogService.Create(FConection);
end;

constructor TRw7WebServices.Create(aUrlRobot: string; aUser: string; aPassword: string);
begin
  Create;
  FConection.SetRobotUrl(aUrlRobot);
  FConection.SetUserPassword(aUser, aPassword);

end;

destructor TRw7WebServices.Destroy;
begin

  FreeAndNil(FRobotWareService);
  FreeAndNil(FController);
  FreeAndNil(FConection);
  FreeAndNil(FFileService);
  FreeAndNil(FElogService);
  inherited Destroy;
end;



end.
