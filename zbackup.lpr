{
  Project ZBackup

  @author(Tomáš Borek <tomas.borek@post.cz>)
}
program zbackup;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Interfaces, Forms,
  UMainF, UFormSettings;

{$R *.res}

resourcestring
  sAppTitle = 'ZBackup';
  sAppIdent = 'ZBackup';
  sIniFileName = 'zbackup.ini';

{ Set application title from resources. It overrides Lazarus behaviour that
sets up Title when Project Options dialog closed. }
procedure SetAppTitleFromResources;
begin
  Application.Title := sAppTitle;
end;

begin
  FormSettingsManager.UseRoamingFolder(sAppIdent);
  FormSettingsManager.SetIniFileName(sIniFileName);

  Application.Title := 'ZBackup';
  SetAppTitleFromResources;
  RequireDerivedFormResource := True;
  Application.Initialize;
  Application.CreateForm(TMainForm, MainForm);
  Application.Run;
end.

