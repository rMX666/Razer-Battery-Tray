unit uSettings;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ExtCtrls, Menus,
  StdCtrls,
  uRazerUtil, uLogger,
  LibUsbOop;

type

  { TfSettings }

  TfSettings = class(TForm)
    btnExit: TButton;
    btnRefresh: TButton;
    cbIconTransparent: TCheckBox;
    cbAutorun: TCheckBox;
    cbLogLastCharge: TCheckBox;
    cmDeviceList: TComboBox;
    cbIconBgNoCharge: TColorButton;
    cbIconBgCharge: TColorButton;
    cbIconRed: TColorButton;
    cbIconOrange: TColorButton;
    cbIconYellow: TColorButton;
    cbIconGreen: TColorButton;
    GroupBox1: TGroupBox;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    Label5: TLabel;
    Label6: TLabel;
    Label7: TLabel;
    miExit: TMenuItem;
    Separator1: TMenuItem;
    miSettings: TMenuItem;
    pmTray: TPopupMenu;
    BatteryTimer: TTimer;
    RefreshListTimer: TTimer;
    TrayIcon: TTrayIcon;
    procedure BatteryTimerTimer(Sender: TObject);
    procedure cbAutorunChange(Sender: TObject);
    procedure cbLogLastChargeChange(Sender: TObject);
    procedure RefreshDeviceList(Sender: TObject);
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormWindowStateChange(Sender: TObject);
    procedure ExitClick(Sender: TObject);
    procedure SetIconColor(Sender: TObject);
    procedure SetIconTransparent(Sender: TObject);
    procedure ShowSettingsForm(Sender: TObject);
  private
    type TAutorunAction = ( aaAdd, aaRemove, aaCheck );
  private
    FConfig: TConfig;
    FChargeWatcher: TChargeWatcher;
    procedure PaintTrayIcon(const Value: Integer; const IsCharging: Boolean);
    procedure DevicesLoaded(Sender: TObject);
    function GetDeviceCaption(const Device: TRazerDevice): String;
    procedure InitializeSettings;
    function DoAutorun(const AutorunAction: TAutorunAction): Boolean;
  public

  end;

var
  fSettings: TfSettings;

implementation

uses
  Windows, Registry,
  uUtil;

{$R *.lfm}

{ TfSettings }

procedure TfSettings.PaintTrayIcon(const Value: Integer; const IsCharging: Boolean);

  function GetColor: TColor;
  begin
    Result := clNone;

    if Value < 25 then
      Result := AppSettings.IconColors[ciRed]
    else if Value < 50 then
      Result := AppSettings.IconColors[ciOrange]
    else if Value < 75 then
      Result := AppSettings.IconColors[ciYellow]
    else
      Result := AppSettings.IconColors[ciGreen];
  end;

var
  B: Graphics.TBitmap;
  Size, FontSize: Integer;
begin
  Size := GetSystemMetrics(SM_CXSMICON);
  FontSize := 12;
  // Repaint icon and stuff
  try
    B := Graphics.TBitmap.Create;
    B.SetSize(Size, Size);
    with B.Canvas do
      begin
        AntialiasingMode := amOff;
        if IsCharging then
          Brush.Color := AppSettings.IconColors[ciBgCharge]
        else
          Brush.Color := AppSettings.IconColors[ciBgNoCharge];
        FillRect(0, 0, Size, Size);

        case Value of
          0:
            begin
              Pen.Color := clRed;
              Line(0, 0, Size, Size);
              Line(0, Size, Size, 0);
            end;
          100:
            begin
              Brush.Color := AppSettings.IconColors[ciGreen];
              Ellipse(ClipRect);
            end;
          else
            begin
              Font.Size := FontSize;
              Font.Color := GetColor;
              Font.Name := 'Courier';
              Font.Bold := False;
              Font.Quality := fqNonAntialiased;
              TextOut(-1, 0, Format('%0.2d', [Value]));
            end;
        end;
      end;
    if AppSettings.IconTransparent then
      B.Mask(AppSettings.IconColors[ciBgNoCharge]);
    TrayIcon.Icon.Assign(B);
  finally
    FreeAndNil(B);
  end;
end;

procedure TfSettings.ShowSettingsForm(Sender: TObject);
begin
  fSettings.Show;
  WindowState := wsNormal;
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

procedure TfSettings.SetIconColor(Sender: TObject);
begin
  AppSettings.IconColors[TColorIndex(TColorButton(Sender).Tag)] := TColorButton(Sender).ButtonColor;
end;

procedure TfSettings.SetIconTransparent(Sender: TObject);
begin
  AppSettings.IconTransparent := TCheckBox(Sender).Checked;
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
            Result := Result + ' (charging)'
          else
            if AppSettings.LastChargeDate <> MinDateTime then
              Result := Format('%s'#13#10'Last charged %s', [Result, PrettyDateFormat(AppSettings.LastChargeDate)]);
        end
      else
        Result := Name;
    end;
end;

procedure TfSettings.InitializeSettings;
begin
  cbIconBgCharge.ButtonColor   := AppSettings.IconColors[ciBgCharge];
  cbIconBgNoCharge.ButtonColor := AppSettings.IconColors[ciBgNoCharge];
  cbIconRed.ButtonColor        := AppSettings.IconColors[ciRed];
  cbIconOrange.ButtonColor     := AppSettings.IconColors[ciOrange];
  cbIconYellow.ButtonColor     := AppSettings.IconColors[ciYellow];
  cbIconGreen.ButtonColor      := AppSettings.IconColors[ciGreen];

  cbIconTransparent.Checked := AppSettings.IconTransparent;

  cbLogLastCharge.Checked := AppSettings.LogLastCharge;

  cbAutorun.Checked := DoAutorun(aaCheck);
end;

procedure TfSettings.FormCreate(Sender: TObject);
begin
  FConfig := TConfig.Create;
  FConfig.OnLoad := @DevicesLoaded;
  FConfig.Refresh;

  InitializeSettings;

  FChargeWatcher := TChargeWatcher.Create;

  BatteryTimer.Enabled := True;
  BatteryTimerTimer(Sender);
end;

procedure TfSettings.FormDestroy(Sender: TObject);
begin
  FConfig.Free;
  FChargeWatcher.Free;
end;

procedure TfSettings.BatteryTimerTimer(Sender: TObject);
var
  Device: TRazerDevice;
begin
  if FConfig.Count = 0 then
    begin
      TrayIcon.Hint := 'Not connected';
      PaintTrayIcon(0, False);
      FConfig.Refresh;
    end
  else
    try
      Device := FConfig.SupportedDevices[cmDeviceList.ItemIndex];
      TrayIcon.Hint := GetDeviceCaption(Device);
      PaintTrayIcon(Device.ChargeLevel, Device.IsCharging);
      FChargeWatcher.CheckChargeStatus(Device.IsCharging);
    except
      on E: ELibUsb do
        FConfig.Refresh;
    end;
end;

function TfSettings.DoAutorun(const AutorunAction: TAutorunAction): Boolean;

  function HandleError(const IsSuccess: Boolean): Boolean;
  const
    MESSAGES: array [TAutorunAction] of String =
      (
        'Unable to add application to autorun',
        'Unable to remove application from autorun',
        'Unable to check application autorun status'
      );
  begin
    Result := not IsSuccess;

    if Result then
      Application.MessageBox(PChar(MESSAGES[AutorunAction]), 'Error', MB_OK or MB_ICONERROR);
  end;

begin
  Result := False;

  with TRegistry.Create do
    try
      RootKey := HKEY_CURRENT_USER;

      if HandleError(OpenKey('SOFTWARE\Microsoft\Windows\CurrentVersion\Run\', false)) then
        Exit;

      case AutorunAction of
        aaAdd:
          WriteString(Application.Name, Application.ExeName);
        aaRemove:
          HandleError(DeleteValue(Application.Name));
        aaCheck:
          Result := ValueExists(Application.Name);
      end;
    finally
      Free;
    end;
end;

procedure TfSettings.cbAutorunChange(Sender: TObject);
begin
  if TCheckBox(Sender).Checked then
    DoAutorun(aaAdd)
  else
    DoAutorun(aaRemove);
end;

procedure TfSettings.cbLogLastChargeChange(Sender: TObject);
begin
  AppSettings.LogLastCharge := TCheckBox(Sender).Checked;
end;

procedure TfSettings.RefreshDeviceList(Sender: TObject);
begin
  BatteryTimer.Enabled := False;
  FConfig.Refresh;
  BatteryTimer.Enabled := True;
end;

end.

