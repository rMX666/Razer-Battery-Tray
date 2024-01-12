unit uUtil;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, IniFiles, Graphics;

type

  { TAppSettings }

  TColorIndex = ( ciRed, ciOrange, ciYellow, ciGreen, ciBgCharge, ciBgNoCharge );

  {$push}{$warn 3018 off} // Disable "Constructor should be public" warning
  TAppSettings = class
  private
    FIniFile: TIniFile;
    FIconColors: array [TColorIndex] of TColor;
    FIconTransparent: ShortInt;
    FLogLastCharge: ShortInt;
    FLastChargeDate: TDateTime;
    function GetIconColor(const Index: TColorIndex): TColor;
    function GetIconTransparent: Boolean;
    function GetLastChargeDate: TDateTime;
    function GetLogLastCharge: Boolean;
    procedure SetIconColor(const Index: TColorIndex; AValue: TColor);
    procedure SetIconTransparent(AValue: Boolean);
    procedure SetLastChargeDate(AValue: TDateTime);
    procedure SetLogLastCharge(AValue: Boolean);
  protected
    constructor Create;
  public
    destructor Destroy; override;
    property IconColors[const Index: TColorIndex]: TColor read GetIconColor write SetIconColor;
    property IconTransparent: Boolean read GetIconTransparent write SetIconTransparent;
    property LogLastCharge: Boolean read GetLogLastCharge write SetLogLastCharge;
    property LastChargeDate: TDateTime read GetLastChargeDate write SetLastChargeDate;
  end;
  {$pop}

function AppSettings: TAppSettings;
function AppDirectory: String;

function PrettyDateFormat(const DateTime: TDateTime; const WithTime: Boolean = True): String;

implementation

uses
  Forms;

const
  INI_SEC_ICON_COLORS = 'IconColors';
  INI_ICON_TRANSPARENT_NAME = 'Transparent';
  INI_ICON_COLOR_NAMES: array [TColorIndex] of String =
    (
      'ColorRed',
      'ColorOrange',
      'ColorYellow',
      'ColorGreen',
      'ColorBgCharge',
      'ColorBgNoCharge'
    );
  INI_ICON_COLOR_DEFAULTS: array [TColorIndex] of TColor =
    (
      clRed,
      $0066FF, // Orange
      clYellow,
      clGreen,
      $003000, // Dark green
      clBlack
    );

  INI_SEC_CHARGE_STATUS = 'ChargeStatus';
  INI_CHARGE_LOG_LAST_CHARGE = 'LogLastCharge';
  INI_CHARGE_LAST_CHARGE_DATE = 'LastChargeDate';

var
  FAppSettings: TAppSettings;

function AppSettings: TAppSettings;
begin
  if not Assigned(FAppSettings) then
    FAppSettings := TAppSettings.Create;

  Result := FAppSettings;
end;

// Returns directory with trailing separator
function AppDirectory: String;
begin
  Result := ExtractFilePath(Application.ExeName);
end;

// Checks date and returns it in a format like: Today, Yesterday, X days ago
function PrettyDateFormat(const DateTime: TDateTime; const WithTime: Boolean = True): String;
begin
  if Trunc(Now) - Trunc(DateTime) < 15 then
    begin
      if Trunc(DateTime) = Trunc(Now) then
        Result := 'Today'
      else if Trunc(Now) - Trunc(DateTime) = 1 then
        Result := 'Yesterday'
      else
        Result := Format('%d days ago', [Trunc(Now) - Trunc(DateTime)]);

      if WithTime then
        Result := Format('%s at %s', [Result, FormatDateTime('hh:nn', DateTime)]);
    end
  else
    begin
      if WithTime then
        Result := 'at ' + FormatDateTime('dd.mm.yyyy hh:nn', DateTime)
      else
        Result := 'at ' + FormatDateTime('dd.mm.yyyy', DateTime);
    end;
end;

{ TAppSettings }

function TAppSettings.GetIconColor(const Index: TColorIndex): TColor;
begin
  if FIconColors[Index] = clNone then
    begin
      if not FIniFile.ValueExists(INI_SEC_ICON_COLORS, INI_ICON_COLOR_NAMES[Index]) then
        SetIconColor(Index, INI_ICON_COLOR_DEFAULTS[Index]);

      FIconColors[Index] := FIniFile.ReadInteger(INI_SEC_ICON_COLORS, INI_ICON_COLOR_NAMES[Index], INI_ICON_COLOR_DEFAULTS[Index]);
    end;

  Result := FIconColors[Index];
end;

function TAppSettings.GetIconTransparent: Boolean;
begin
  if FIconTransparent = -1 then
    begin
      if not FIniFile.ValueExists(INI_SEC_ICON_COLORS, INI_ICON_TRANSPARENT_NAME) then
        SetIconTransparent(True);

      FIconTransparent := ShortInt(FIniFile.ReadBool(INI_SEC_ICON_COLORS, INI_ICON_TRANSPARENT_NAME, True));
    end;

  Result := FIconTransparent = 1;
end;

function TAppSettings.GetLastChargeDate: TDateTime;
begin
  if FLastChargeDate = MinDateTime then
    begin
      if not FIniFile.ValueExists(INI_SEC_CHARGE_STATUS, INI_CHARGE_LAST_CHARGE_DATE) then
        SetLastChargeDate(MinDateTime);

      FLastChargeDate := FIniFile.ReadDateTime(INI_SEC_CHARGE_STATUS, INI_CHARGE_LAST_CHARGE_DATE, MinDateTime);
    end;

  Result := FLastChargeDate;
end;

function TAppSettings.GetLogLastCharge: Boolean;
begin
  if FLogLastCharge = -1 then
    begin
      if not FIniFile.ValueExists(INI_SEC_CHARGE_STATUS, INI_CHARGE_LOG_LAST_CHARGE) then
        SetLogLastCharge(False);

      FLogLastCharge := ShortInt(FIniFile.ReadBool(INI_SEC_CHARGE_STATUS, INI_CHARGE_LOG_LAST_CHARGE, False));
    end;

  Result := FLogLastCharge = 1;
end;

procedure TAppSettings.SetIconColor(const Index: TColorIndex; AValue: TColor);
begin
  FIniFile.WriteInteger(INI_SEC_ICON_COLORS, INI_ICON_COLOR_NAMES[Index], AValue);
  FIconColors[Index] := AValue;
end;

procedure TAppSettings.SetIconTransparent(AValue: Boolean);
begin
  FIniFile.WriteBool(INI_SEC_ICON_COLORS, INI_ICON_TRANSPARENT_NAME, AValue);
  FIconTransparent := ShortInt(AValue);
end;

procedure TAppSettings.SetLastChargeDate(AValue: TDateTime);
begin
  FIniFile.WriteDateTime(INI_SEC_CHARGE_STATUS, INI_CHARGE_LAST_CHARGE_DATE, AValue);
  FLastChargeDate := AValue;
end;

procedure TAppSettings.SetLogLastCharge(AValue: Boolean);
begin
  FIniFile.WriteBool(INI_SEC_CHARGE_STATUS, INI_CHARGE_LOG_LAST_CHARGE, AValue);
  FLogLastCharge := ShortInt(AValue);
end;

constructor TAppSettings.Create;
var
  I: TColorIndex;
begin
  FIniFile := TIniFile.Create(AppDirectory + 'config.ini');
  for I := Low(TColorIndex) to High(TColorIndex) do
    FIconColors[I] := clNone;

  FIconTransparent := -1;

  FLogLastCharge := -1;
  FLastChargeDate := MinDateTime;
end;

destructor TAppSettings.Destroy;
begin
  FreeAndNil(FIniFile);
  inherited Destroy;
end;

finalization
  FreeAndNil(FAppSettings);

end.

