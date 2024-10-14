unit rw6webservices;
{
This the main unit, that you must use to connect a ABB robot with Robotware 6
}

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, abbconexion, rw6robotwareservices,
  rw6fileservices, rw6controllerservice, rw6userservices;

type

  { TRw6WebServices }

  TRw6WebServices = class

  private
    FConection: TRobotConnection;
    FControllerService: Trw6controllerservice;
    FFileService: TRw6FileServices;
    FRobotWareServices: TRw6RobotWareServices;
    FUserServices: TRw6UserServices;
  public
    procedure SetRobotUrl(aUrl: string);
    procedure SetUserPassword(aUser: string; aPassword: string);
    procedure Connect;
  public
    property RobotWareServices: TRw6RobotWareServices read FRobotWareServices;
    property FileService: TRw6FileServices read FFileService;
    property ControllerService: Trw6controllerservice read FControllerService;
  public
    constructor Create;
    constructor Create(aUrlRobot: string; aUser: string = 'Default User';
      aPassword: string = 'robotics'); overload;
    destructor Destroy; override;
  end;

implementation

{ TRw6WebServices }

procedure TRw6WebServices.SetRobotUrl(aUrl: string);
begin
  FConection.RobotUrl:=aUrl;
end;

procedure TRw6WebServices.SetUserPassword(aUser: string; aPassword: string);
begin
  FConection.SetUserPassword(aUser, aPassword);
end;

procedure TRw6WebServices.Connect;
begin
  FConection.Conectar;

end;

constructor TRw6WebServices.Create;
begin
  FConection := TRobotConnection.Create;
  FRobotWareServices := TRw6RobotWareServices.Create(FConection);
  FControllerService := Trw6controllerservice.Create(FConection);
  FFileService := TRw6FileServices.Create(FConection);
  FUserServices := TRw6UserServices.Create(FConection);
end;

constructor TRw6WebServices.Create(aUrlRobot: string; aUser: string; aPassword: string);
begin
  Create;
  FConection.RobotUrl:=(aUrlRobot);
  FConection.SetUserPassword(aUser, aPassword);
end;

destructor TRw6WebServices.Destroy;
begin
  FreeAndNil(FUserServices);
  FreeAndNil(FControllerService);
  FreeAndNil(FFileService);
  FreeAndNil(FRobotWareServices);
  FreeAndNil(FConection);
  inherited Destroy;
end;

end.
