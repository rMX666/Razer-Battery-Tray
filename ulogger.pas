unit uLogger;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils;

type

  { TLogger }

  TLogger = class
  private
    FLogStream: TFileStream;
  public
    constructor Create(const FileName: String);
    destructor Destroy; override;
    procedure Log(const S: String);
    procedure LogFmt(const S: String; const Args: array of const);
  end;

  { TChargeWatcher }

  TChargeWatcher = class
  private
    FIsCharging: Boolean;
  public
    procedure CheckChargeStatus(const IsCharging: Boolean);
  end;

function Logger: TLogger;

implementation

uses
  DateUtils, uUtil;

const
  LOG_FILENAME = 'razbat.log';

var
  FLogger: TLogger;

function Logger: TLogger;
begin
  if not Assigned(FLogger) then
    FLogger := TLogger.Create(AppDirectory + LOG_FILENAME);

  Result := FLogger;
end;

{ TLogger }

constructor TLogger.Create(const FileName: String);
begin
  FLogStream := TFileStream.Create(FileName, fmCreate or fmOpenReadWrite or fmShareDenyWrite);
  FLogStream.Seek(0, soEnd);
end;

destructor TLogger.Destroy;
begin
  FreeAndNil(FLogStream);
  inherited Destroy;
end;

procedure TLogger.Log(const S: String);
var
  Buf: String;
begin
  // Da hell TFileStream can't simply write a string?..
  Buf := Format('[%s] %s'#13#10, [FormatDateTime('YYYY-MM-DD HH:NN:SS', Now), S]);
  FLogStream.WriteBuffer(Pointer(Buf)^, Length(Buf));
end;

procedure TLogger.LogFmt(const S: String; const Args: array of const);
begin
  Log(Format(S, Args));
end;

{ TChargeWatcher }

procedure TChargeWatcher.CheckChargeStatus(const IsCharging: Boolean);
begin
  if FIsCharging = IsCharging then
    Exit;

  if not FIsCharging and IsCharging then
    Logger.Log('Charge started')
  else
    begin
      Logger.Log('Charge finished');
      AppSettings.LastChargeDate := Now;
    end;

  FIsCharging := IsCharging;
end;

finalization
  FreeAndNil(FLogger);

end.

