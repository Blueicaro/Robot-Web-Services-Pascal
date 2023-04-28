{
 Controller, contains basic functionality for manipulating the controller,
 e.g. get system information, create/restore backup, set motors on/off,
 restart controller, etc. To facilitate easy subscription to controller states
 there is a Monitor object.
}
unit controllerunit;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils;

{
 Valid Resources for Controller monitor
  crControllerState: State of the controller
  crOperatonMode: Operation mode of the controller
}
type
  TControllerResource = (crControllerState, crOperationMode);
{
 Controller’s system state.
}
type
  TControllerState = (csInitializing, csMotors_on, csMotors_off, csGuard_stop,
    csEmergency_stop, csEmergency_stop_resetting, csSystem_failure);

type

  { TController }

  TController = class

  public
    function GetMonitor: string;
    function GetControllerState: TControllerState;
  end;

implementation

{ TController }

function TController.GetMonitor: string;
begin

end;

end.
