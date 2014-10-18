unit splash;

{$mode objfpc}{$H+}
//{$DEFINE debug}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, ExtCtrls,
  StdCtrls, Menus, lhttp, lNet, LCLIntf;

const
  UsedApiVersion = 1;

  IconActive = 22;
  IconInActive = 0;

type
  TTrafficStatus = (tsError, tsOK, tsCritical);

  { TSplashform }

  TSplashform = class(TForm)
    ImageList1: TImageList;
    logoimage: TImage;
    MenuItem1: TMenuItem;
    MenuItem2: TMenuItem;
    MenuItem3: TMenuItem;
    traypop: TPopupMenu;
    statuslabel: TLabel;
    requesttimer: TTimer;
    backgroundshape: TShape;
    minimizetimer: TTimer;
    tray: TTrayIcon;
    procedure FormCreate(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure MenuItem1Click(Sender: TObject);
    procedure MenuItem3Click(Sender: TObject);
    procedure minimizetimerTimer(Sender: TObject);
    procedure requesttimerTimer(Sender: TObject);
    procedure trafgetDoneInput(ASocket: TLHTTPClientSocket);
    procedure trafgetError(const msg: string; aSocket: TLSocket);
    function trafgetInput(ASocket: TLHTTPClientSocket; ABuffer: PChar;
      ASize: integer): integer;
    procedure trayPaint(Sender: TObject);
  private
    HTTPBuffer: string;
    lastPercentValue: single;
    status: TTrafficStatus;
    trafget: TLHTTPClient;

    procedure reportError(msg: string);
    procedure reportMessage(msg: string);
    procedure processResult();

    procedure UpdateIcon;
  public

  end;

var
  Splashform: TSplashform;

implementation

{$R *.lfm}

{ TSplashform }

procedure TSplashform.FormCreate(Sender: TObject);
var
  temp: TIcon;
begin
  lastPercentValue := 0;
  status := tsOK;

  UpdateIcon;
  tray.Show;
  reportMessage('Trafficinformation wird abgefragt...');

  trafget := TLHTTPClient.Create(self);
  {$IFDEF debug}
  trafget.Host := 'localhost';
  trafget.Port := 8080;
  {$ELSE}
  trafget.Host := 'wwww.wh2.tu-dresden.de';
  trafget.URI :=  'http://www.wh2.tu-dresden.de/traffic/getMyTraffic.php';
  {$ENDIF}
  trafget.OnDoneInput := @trafgetDoneInput;
  trafget.OnError := @trafgetError;
  trafget.OnInput := @trafgetInput;
end;

procedure TSplashform.FormShow(Sender: TObject);
begin
  minimizetimer.Enabled := True;
  requesttimerTimer(Sender);
end;

procedure TSplashform.MenuItem1Click(Sender: TObject);
begin
  tray.Hide;
  Application.Terminate;
end;

procedure TSplashform.MenuItem3Click(Sender: TObject);
begin
  OpenURL('https://www.wh2.tu-dresden.de/');
end;

procedure TSplashform.minimizetimerTimer(Sender: TObject);
begin
  Hide;
  lastPercentValue := 0;

  minimizetimer.Enabled := False;
end;

procedure TSplashform.requesttimerTimer(Sender: TObject);
begin
  HTTPBuffer := '';
  trafget.SendRequest;
end;

procedure TSplashform.trafgetDoneInput(ASocket: TLHTTPClientSocket);
begin
  aSocket.Disconnect();
  processResult();
end;

procedure TSplashform.trafgetError(const msg: string; aSocket: TLSocket);
begin
  reportError(msg);
end;

function TSplashform.trafgetInput(ASocket: TLHTTPClientSocket;
  ABuffer: PChar; ASize: integer): integer;
var
  oldLength: dword;
begin
  oldLength := Length(HTTPBuffer);
  setlength(HTTPBuffer, oldLength + ASize);
  move(ABuffer^, HTTPBuffer[oldLength + 1], ASize);
  Result := aSize;
end;

procedure TSplashform.trayPaint(Sender: TObject);
begin

end;

procedure TSplashform.reportError(msg: string);
begin
  msg := 'Trafficstatus kann nicht angezeigt werden. ' + #13#10 + msg;
  statuslabel.Caption := msg;
  statuslabel.Font.Color := clred;
  status := tsError;
  tray.Hint := msg;
  UpdateIcon;
  Refresh;
end;

procedure TSplashform.reportMessage(msg: string);
begin
  statuslabel.Caption := msg;
  statuslabel.font.color := clblack;
  status := tsOK;
  tray.Hint := msg;
  UpdateIcon;
  Refresh;
end;

procedure TSplashform.processResult;
var
  percent: single;
begin
  if (UsedApiVersion = 1) then
  begin
    if (HTTPBuffer = '-1') then
    begin
      reportError('Nicht im AGDSN-Netz oder keine Verbindung möglich.');
      Exit;
    end;
    percent := StrToFloatDef(httpBuffer, -1);
    if (percent < 0) then
    begin
      reportError('Ungültige Antwort vom Server.');
      Exit;
    end;

    if (percent > 80) and (lastPercentValue <= percent - 10) then
    begin
      tray.BalloonFlags := bfWarning;
      tray.BalloonTitle := 'Hoher Trafficverbrauch';
      tray.BalloonHint := 'Du hast ' + FormatFloat('##0.##', percent) +
        ' % Traffic verbraucht. Achtung: Bei 100 % wirst du gesperrt!';
      tray.BalloonTimeout := 10000;
      tray.ShowBalloonHint;
      lastPercentValue := percent;
    end;

    reportMessage('Traffic zu ' + FormatFloat('##0.##', percent) + ' % aufgebraucht.');

    ImageList1.GetIcon(1 + round(percent / 5), tray.Icon);
  end;
end;

procedure TSplashform.UpdateIcon;
begin
  // Workaround: Keine Transparenz für Icon im Tray unter Linux
  if (status = tsOK) then
  begin
    ImageList1.GetIcon(IconActive, tray.Icon);
  end
  else if (status = tsCritical) then
  begin

  end
  else if (status = tsError) then
  begin
    ImageList1.GetIcon(IconInActive, tray.Icon);
  end;
end;

end.
