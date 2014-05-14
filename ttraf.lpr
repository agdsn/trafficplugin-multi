program ttraf;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Interfaces, // this includes the LCL widgetset
  Forms, splash, settings, lnetvisual;

{$R *.res}

begin
  Application.Title:='TTraf';
  RequireDerivedFormResource := True;
  Application.Initialize;
  Application.CreateForm(TSplashform, Splashform);
  Application.Run;
end.

