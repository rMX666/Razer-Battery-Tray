unit uSettings;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ExtCtrls, Menus,
  StdCtrls, uRazerUtil,
  LibUsbOop;

type

  { TfSettings }

  TfSettings = class(TForm)
    btnExit: TButton;
    btnRefresh: TButton;
    cmDeviceList: TComboBox;
    Label1: TLabel;
    miExit: TMenuItem;
    Separator1: TMenuItem;
    miSettings: TMenuItem;
    pmTray: TPopupMenu;
    BatteryTimer: TTimer;
    RefreshListTimer: TTimer;
    TrayIcon: TTrayIcon;
    procedure BatteryTimerTimer(Sender: TObject);
    procedure RefreshDeviceList(Sender: TObject);
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormWindowStateChange(Sender: TObject);
    procedure ExitClick(Sender: TObject);
    procedure ShowSettingsForm(Sender: TObject);
    procedure TrayIconMouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
  private
    FConfig: TConfig;
    procedure PaintTrayIcon(const Value: Integer);
    procedure DevicesLoaded(Sender: TObject);
    function GetDeviceCaption(const Device: TRazerDevice): String;
  public

  end;

var
  fSettings: TfSettings;

implementation

{$R *.lfm}

{ TfSettings }

procedure TfSettings.PaintTrayIcon(const Value: Integer);

  function GetColor: TColor;
  begin
    Result := clBlack;
    if Value < 25 then
      Result := clRed
    else if Value < 50 then
      Result := $FF6600 // Orange
    else if Value < 75 then
      Result := clYellow
    else
      Result := clGreen;
  end;

var
  B: TBitmap;
begin
  // Repaint icon and stuff
  try
    B := TBitmap.Create;
    B.SetSize(32, 32);
    B.Transparent := True;
    B.TransparentColor := clBlack;
    with B.Canvas do
      begin
        AntialiasingMode := amOn;
        Brush.Color := clBlack;
        FillRect(0, 0, 32, 32);
        if Value = 0 then
          begin
            Pen.Color := clRed;
            Line(0, 0, 32, 32);
            Line(0, 32, 32, 0);
          end
        else
          begin
            Font.Size := 20;
            Font.Color := GetColor;
            Font.Name := 'Courier New';
            Font.Bold := True;
            TextOut(0, 0, Format('%0.2d', [Value]));
          end;
      end;
    with TrayIcon.Icon do
      begin
        TransparentColor := clBlack;
        Transparent := True;
        Assign(B);
      end;
  finally
    FreeAndNil(B);
  end;
end;

procedure TfSettings.ShowSettingsForm(Sender: TObject);
begin
  fSettings.Show;
  WindowState := wsNormal;
end;

procedure TfSettings.TrayIconMouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  if Button = mbRight then
    pmTray.PopUp(X, Y);
end;

procedure TfSettings.FormWindowStateChange(Sender: TObject);
begin
  if WindowState = wsMinimized then
    Hide;
end;

procedure TfSettings.ExitClick(Sender: TObject);
begin
  Application.Terminate;
end;

procedure TfSettings.FormCloseQuery(Sender: TObject; var CanClose: Boolean);
begin
  CanClose := False;
  Hide;
end;

procedure TfSettings.DevicesLoaded(Sender: TObject);
var
  I: Integer;
begin
  cmDeviceList.Caption := '';

  with cmDeviceList.Items do
    try
      BeginUpdate;
      Clear;
      for I := 0 to FConfig.Count - 1 do
        Add(GetDeviceCaption(FConfig.SupportedDevices[I]));
    finally
      EndUpdate;
    end;

  // TODO: Save item index somewhere and restore on load.
  if FConfig.Count > 0 then
    cmDeviceList.ItemIndex := 0;
end;

function TfSettings.GetDeviceCaption(const Device: TRazerDevice): String;
var
  Charge: Integer;
begin
  with Device do
    begin
      Charge := ChargeLevel;
      if Charge > 0 then
        begin
          Result := Format('%s - %0.2d%%', [Name, Charge]);
          if IsCharging then
            Result := Result + ' (charging)';
        end
      else
        Result := Name;
    end;
end;

procedure TfSettings.FormCreate(Sender: TObject);
begin
  FConfig := TConfig.Create;
  FConfig.OnLoad := @DevicesLoaded;
  FConfig.Refresh;
  BatteryTimer.Enabled := True;
  BatteryTimerTimer(Sender);
end;

procedure TfSettings.FormDestroy(Sender: TObject);
begin
  FConfig.Free;
end;

procedure TfSettings.BatteryTimerTimer(Sender: TObject);
var
  Device: TRazerDevice;
begin
  if FConfig.Count = 0 then
    begin
      TrayIcon.Hint := 'Not connected';
      PaintTrayIcon(0);
      FConfig.Refresh;
    end
  else
    try
      Device := FConfig.SupportedDevices[cmDeviceList.ItemIndex];
      TrayIcon.Hint := GetDeviceCaption(Device);
      PaintTrayIcon(Device.ChargeLevel);
    except
      on E: ELibUsb do
        FConfig.Refresh;
    end;
end;

procedure TfSettings.RefreshDeviceList(Sender: TObject);
begin
  BatteryTimer.Enabled := False;
  FConfig.Refresh;
  BatteryTimer.Enabled := True;
end;

end.

