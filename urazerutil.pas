unit uRazerUtil;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, contnrs, fpjson,
  LibUsb, LibUsbOop;

type

  { TRazerDevice }

  TRazerDevice = class
  private
    FVID, FPID, FTransactionID: Integer;
    FName: String;
    FUSBDevice: TLibUsbDevice;
    FUSBContext: TLibUsbContext;
    function GetChargeLevel: Byte;
    function GetIsAttached: Boolean;
    function GetIsCharging: Boolean;
    procedure InitUSBDevice;
    function SimpleRazerQuery(const ACommandClass, ACommandID, ADataSize: Byte): Byte;
  public
    constructor Create(const AVID, APID, ATransactionID: Integer; const AName: String);
    destructor Destroy; override;
    property VID: Integer read FVID;
    property PID: Integer read FPID;
    property TransactionID: Integer read FTransactionID;
    property Name: String read FName;
    property IsAttached: Boolean read GetIsAttached;
    property ChargeLevel: Byte read GetChargeLevel;
    property IsCharging: Boolean read GetIsCharging;
  end;

  { TConfig }

  TConfig = class (TFPObjectList)
  private
    FOnLoad: TNotifyEvent;
    procedure TryAddDeviceFromJSON(const Data: TJSONData);
    function GetSupportedDevice(const Index: Integer): TRazerDevice;
    procedure LoadSupportedDevices;
  public
    constructor Create;
    procedure Refresh;
    property SupportedDevices[const Index: Integer]: TRazerDevice read GetSupportedDevice;
    property OnLoad: TNotifyEvent read FOnLoad write FOnLoad;
  end;

implementation

uses
  StrUtils, jsonparser,
  uUtil;

type
  PRazerReport = ^TRazerReport;
  TRazerReport = packed record
    Status:           Byte;
    TransactionID:    Byte;
    RemainingPackets: Word; // !!!
    ProtocolType:     Byte;
    DataSize:         Byte;
    CommandClass:     Byte;
    CommandID:        Byte;
    Arguments:        array [0..79] of Byte;
    CRC:              Byte;
    Reserved:         Byte;
  end;

const
  RAZER_REPORT_SIZE = SizeOf(TRazerReport);

function CreateEmptyRazerReport: PRazerReport;
begin
  New(Result);
  FillByte(Result^, SizeOf(TRazerReport), 0);
end;

function CreateRazerReport(const ACommandClass, ACommandID, ADataSize, ATransactionID: Byte): PRazerReport;
begin
  Result := CreateEmptyRazerReport;
  with Result^ do
    begin
      CommandClass  := ACommandClass;
      CommandID     := ACommandID;
      DataSize      := ADataSize;
      TransactionID := ATransactionID;
    end;
end;

function GetRazerReportCRC(const Report: PRazerReport): Byte;
type
  TByteReport = array [0..89] of Byte;
  PByteReport = ^TByteReport;
var
  P: PByteReport;
  I: Integer;
begin
  Result := 0;
  P := PByteReport(Report);
  for I := 2 to SizeOf(TRazerReport) - 1 do
    Result := Result xor P^[I];
end;

{ TRazerDevice }

function TRazerDevice.SimpleRazerQuery(const ACommandClass, ACommandID, ADataSize: Byte): Byte;
var
  Request, Response: PRazerReport;
begin
  Request := CreateRazerReport(ACommandClass, ACommandID, ADataSize, FTransactionID);
  Response := CreateEmptyRazerReport;

  try
    Request^.CRC := GetRazerReportCRC(Request);

    ELibUsb.Check(FUSBDevice.Control.ControlMsg( LIBUSB_REQUEST_TYPE_CLASS or LIBUSB_RECIPIENT_INTERFACE or LIBUSB_ENDPOINT_OUT
                                               , $09 // HID_REQ_SET_REPORT
                                               , $300
                                               , 0
                                               , Request^
                                               , RAZER_REPORT_SIZE
                                               , 500), 'device IN');

    // Sleep a bit after IN request for wireless devices to sync.
    Sleep(50);

    ELibUsb.Check(FUSBDevice.Control.ControlMsg( LIBUSB_REQUEST_TYPE_CLASS or LIBUSB_RECIPIENT_INTERFACE or LIBUSB_ENDPOINT_IN
                                               , $01 // HID_REQ_GET_REPORT
                                               , $300
                                               , 0
                                               , Response^
                                               , RAZER_REPORT_SIZE
                                               , 500), 'device OUT');

    Result := Response^.Arguments[1];
  finally
    Dispose(Request);
    Dispose(Response);
  end;
end;

function TRazerDevice.GetIsAttached: Boolean;
begin
  Result := Assigned(FUSBDevice);
end;

function TRazerDevice.GetChargeLevel: Byte;
begin
  // Response comes in 0..255 range, scale it down to 0..100
  Result := Round(SimpleRazerQuery($07, $80, $02) * 100 / 255);
end;

function TRazerDevice.GetIsCharging: Boolean;
begin
  Result := SimpleRazerQuery($07, $84, $02) > 0;
end;

procedure TRazerDevice.InitUSBDevice;
var
  A: TLibUsbDeviceArray;
begin
  FUSBDevice := nil;

  FUSBContext := TLibUsbContext.Create;
  A := FUSBContext.FindDevices(FVID, FPID);
  if Length(A) = 1 then
    FUSBDevice := TLibUsbDevice.Create(FUSBContext, A[0])
end;

constructor TRazerDevice.Create(const AVID, APID, ATransactionID: Integer; const AName: String);
begin
  FVID := AVID;
  FPID := APID;
  FTransactionID := ATransactionID;
  FName := AName;

  InitUSBDevice;
end;

destructor TRazerDevice.Destroy;
begin
  if Assigned(FUSBDevice) then
    FreeAndNil(FUSBDevice);
  if Assigned(FUSBContext) then
    FreeAndNil(FUSBContext);
  inherited Destroy;
end;

{ TConfig }


procedure TConfig.TryAddDeviceFromJSON(const Data: TJSONData);
var
  Device: TRazerDevice;
begin
  Device := TRazerDevice.Create(Hex2Dec(Data.GetPath('VID').AsString),
                                Hex2Dec(Data.GetPath('PID').AsString),
                                Hex2Dec(Data.GetPath('TransactionID').AsString),
                                Data.GetPath('name').AsString);

  if Device.IsAttached then
    Add(Device)
  else
    FreeAndNil(Device);
end;

procedure TConfig.LoadSupportedDevices;
var
  ConfigFile: TFileStream;
  Parser: TJSONParser;
  Data: TJSONArray;
  I: Integer;
begin
  try
    ConfigFile := TFileStream.Create(AppDirectory + 'devices.json', fmOpenRead or fmShareDenyWrite);
    if ConfigFile.Size <= 0 then
      Exit;

    Parser := TJSONParser.Create(ConfigFile, []);
    Data := Parser.Parse as TJSONArray;
    for I := 0 to Data.Count - 1 do
      TryAddDeviceFromJSON(Data.Items[I]);
  finally
    ConfigFile.Free;
    Parser.Free;
    Data.Free;
  end;

  if Assigned(FOnLoad) then
    FOnLoad(Self);
end;

function TConfig.GetSupportedDevice(const Index: Integer): TRazerDevice;
begin
  Result := TRazerDevice(Items[Index]);
end;

constructor TConfig.Create;
begin
  inherited Create(True);
end;

procedure TConfig.Refresh;
begin
  Clear;
  LoadSupportedDevices;
end;

end.

