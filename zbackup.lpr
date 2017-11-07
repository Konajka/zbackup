program zbackup;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Interfaces, Forms,
  UMainF;

{$R *.res}

resourcestring
  sAppTitle = 'ZBakcup';

procedure SetAppTitleFromResources;
begin
  Application.Title := sAppTitle;
end;

begin
  Application.Title := 'ZBackup';
  SetAppTitleFromResources;
  RequireDerivedFormResource := True;
  Application.Initialize;
  Application.CreateForm(TMainForm, MainForm);
  Application.Run;
end.

