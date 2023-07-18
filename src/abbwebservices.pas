unit AbbWebServices;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, fpjson, jsonparser,
  abbconexion, robotwareservices, ControllerServices,FileServices;

type

  { TAbbWebServices }

  TAbbWebServices = class
  private
    FConection: TRobotConexion;
    FController: TControllerServices;
    FFileService: TFileService;
    FRobotWareService: TRobotWareService;
  public
    property Conection: TRobotConexion read FConection;
    property RobotWare: TRobotWareService read FRobotWareService;
    property Controller: TControllerServices read FController;
    property FileService: TFileService read FFileService;
  public
    constructor Create(aUrlRobot: string; aUser: string = 'Default User';
      aPassword: string = 'robotics');
    destructor Destroy; override;
  end;


implementation

uses TypInfo;

{ TAbbWebServices }

constructor TAbbWebServices.Create(aUrlRobot: string; aUser: string; aPassword: string);
begin
  FConection := TRobotConexion.Create;
  FConection.SetRobotUrl(aUrlRobot);
  FConection.SetUserPassword(aUser, aPassword);

  FRobotWareService := TRobotWareService.Create(FConection);
  FController := TControllerServices.Create(FConection);
  FFileService := TFileService.Create(FConection);
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
