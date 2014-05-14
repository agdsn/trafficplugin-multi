unit splash;

{$mode objfpc}{$H+}
{$DEFINE debug}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, ExtCtrls,
  StdCtrls, Menus, lNetComponents, lhttp, lNet, LCLIntf;

const
  UsedApiVersion = 1;

type

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
    trafget: TLHTTPClientComponent;
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
    function trafgetInput(ASocket: TLHTTPClientSocket; ABuffer: pchar;
      ASize: integer): integer;
  private
    HTTPBuffer: string;

    procedure reportError(msg: String);
    procedure reportMessage(msg: String);
    procedure processResult();

    procedure BuildIcons;
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
  BuildIcons;
  tray.Show;
  reportMessage('Trafficinformation wird abgefragt...');

  // Workaround: Keine Transparenz für Icon im Tray unter Linux
  {$IFNDEF Windows}
  ImageList1.GetIcon(1, tray.Icon);
  {$ELSE}
  ImageList1.GetIcon(0, tray.Icon);
  {$ENDIF}

  {$IFDEF debug}
  trafget.Host := 'localhost';
  trafget.Port := 8080;
  {$ENDIF}
end;

procedure TSplashform.FormShow(Sender: TObject);
begin
  minimizetimer.Enabled := true;
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
  minimizetimer.Enabled := false;
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

function TSplashform.trafgetInput(ASocket: TLHTTPClientSocket; ABuffer: pchar;
  ASize: integer): integer;
var
  oldLength: dword;
begin
  oldLength := Length(HTTPBuffer);
  setlength(HTTPBuffer, oldLength + ASize);
  move(ABuffer^, HTTPBuffer[oldLength + 1], ASize);
  Result := aSize;
end;

procedure TSplashform.reportError(msg: String);
begin
  statuslabel.caption := msg;
  statuslabel.Font.Color := clred;
  {$IFNDEF Windows}
  ImageList1.GetIcon(2, tray.Icon);
  {$ELSE}
  ImageList1.GetIcon(3, tray.Icon);
  {$ENDIF}
  Refresh;
end;

procedure TSplashform.reportMessage(msg: String);
begin
  statuslabel.caption := msg;
  statuslabel.font.color := clblack;
  {$IFNDEF Windows}
  ImageList1.GetIcon(1, tray.Icon);
  {$ELSE}
  ImageList1.GetIcon(0, tray.Icon);
  {$ENDIF}
  tray.Hint := msg;
  Refresh;
end;

procedure TSplashform.processResult;
var
  percent: Single;
begin
  if(UsedApiVersion = 1) then begin
    if(HTTPBuffer = '-1') then begin
      reportError('Nicht im AGDSN-Netz oder keine Verbindung möglich.');
      Exit;
    end;
    percent := StrToFloatDef(httpBuffer, -1);
    if(percent < 0) then begin
      reportError('Ungültige Antwort vom Server.');
      Exit;
    end;
    reportMessage('Traffic zu ' + FormatFloat('##0.##', percent) + ' % aufgebraucht.');
  end;
end;

procedure TSplashform.BuildIcons;
begin
end;

end.

