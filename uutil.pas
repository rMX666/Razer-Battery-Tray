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
    function GetIconColor(const Index: TColorIndex): TColor;
    function GetIconTransparent: Boolean;
    procedure SetIconColor(const Index: TColorIndex; AValue: TColor);
    procedure SetIconTransparent(AValue: Boolean);
  protected
    constructor Create;
  public
    destructor Destroy; override;
    property IconColors[const Index: TColorIndex]: TColor read GetIconColor write SetIconColor;
    property IconTransparent: Boolean read GetIconTransparent write SetIconTransparent;
  end;
  {$pop}

function AppSettings: TAppSettings;
function AppDirectory: String;

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

procedure TAppSettings.SetIconColor(const Index: TColorIndex; AValue: TColor);
begin
  FIconColors[Index] := AValue;
  FIniFile.WriteInteger(INI_SEC_ICON_COLORS, INI_ICON_COLOR_NAMES[Index], AValue);
end;

procedure TAppSettings.SetIconTransparent(AValue: Boolean);
begin
  FIniFile.WriteBool(INI_SEC_ICON_COLORS, INI_ICON_TRANSPARENT_NAME, AValue);
  FIconTransparent := ShortInt(AValue);
end;

constructor TAppSettings.Create;
var
  I: TColorIndex;
begin
  FIniFile := TIniFile.Create(AppDirectory + 'config.ini');
  for I := Low(TColorIndex) to High(TColorIndex) do
    FIconColors[I] := clNone;

  FIconTransparent := -1;
end;

destructor TAppSettings.Destroy;
begin
  FreeAndNil(FIniFile);
  inherited Destroy;
end;

finalization
  FreeAndNil(FAppSettings);

end.

