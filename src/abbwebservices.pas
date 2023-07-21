unit AbbWebServices;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, fpjson, jsonparser,
  abbconexion, robotwareservices, ControllerServices, FileServices;

type

  { TAbbWebServices }

  TAbbWebServices = class
  private
    FConection: TRobotConexion;
    FController: TControllerServices;
    FFileService: TFileService;
    FRobotWareService: TRobotWareService;
  public
    procedure SetRobotUrl(aUrl: string);
    procedure  SetUserPassword(aUser: string;aPassword: string);
    property Conection: TRobotConexion read FConection;
    property RobotWare: TRobotWareService read FRobotWareService;
    property Controller: TControllerServices read FController;
    property FileService: TFileService read FFileService;
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
  FConection.SetUserPassword(aUser,aPassword);
end;

constructor TAbbWebServices.Create;
begin
  FConection := TRobotConexion.Create;
  FRobotWareService := TRobotWareService.Create(FConection);
  FController := TControllerServices.Create(FConection);
  FFileService := TFileService.Create(FConection);

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
  inherited Destroy;
end;



end.
