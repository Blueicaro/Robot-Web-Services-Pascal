unit AbbWebServices;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, fpjson, jsonparser,
  abbconexion, robotwareservices, ControllerServices, FileServices,
  elogservices;

type

  { TAbbWebServices }

  TAbbWebServices = class
  private
    FConection: TRobotConnection;
    FController: TControllerServices;
    FElogService: TElogService;
    FFileService: TFileService;
    FRobotWareService: TRobotWareService;
  public
    // URL or ip of the robot
    procedure SetRobotUrl(aUrl: string);
    procedure SetUserPassword(aUser: string; aPassword: string);
    procedure LogOut;
    property Connection: TRobotConnection read FConection;
    property RobotWare: TRobotWareService read FRobotWareService;
    property Controller: TControllerServices read FController;
    property FileService: TFileService read FFileService;
    property ElogService: TElogService read FElogService write FElogService;

  public
    constructor Create;
    constructor Create(aUrlRobot: string; aUser: string = 'Default User';
      aPassword: string = 'robotics'); overload;
    destructor Destroy; override;
  end;


implementation

uses TypInfo;

  { TAbbWebServices }

procedure TAbbWebServices.SetRobotUrl(aUrl: string);
begin
  FConection.SetRobotUrl(aUrl);
end;

procedure TAbbWebServices.SetUserPassword(aUser: string; aPassword: string);
begin
  FConection.SetUserPassword(aUser, aPassword);
end;

procedure TAbbWebServices.LogOut;
begin
  FConection.Get('logout');
end;



constructor TAbbWebServices.Create;
begin
  FConection := TRobotConnection.Create;

  FRobotWareService := TRobotWareService.Create(FConection);
  FController := TControllerServices.Create(FConection);
  FFileService := TFileService.Create(FConection);
  FElogService := TElogService.Create(FConection);
end;

constructor TAbbWebServices.Create(aUrlRobot: string; aUser: string; aPassword: string);
begin
  Create;
  FConection.SetRobotUrl(aUrlRobot);
  FConection.SetUserPassword(aUser, aPassword);

end;

destructor TAbbWebServices.Destroy;
begin

  FreeAndNil(FRobotWareService);
  FreeAndNil(FController);
  FreeAndNil(FConection);
  FreeAndNil(FFileService);
  FreeAndNil(FElogService);
  inherited Destroy;
end;



end.
