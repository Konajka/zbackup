{
  Tools for serializing and deserializing TForm position on screen.

  @author(Tomáš Borek <tomas.borek@post.cz>)
}
unit UFormSettings;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, IniFiles;

type

  { Manages form settings shared data. }

  { TFormSettingsManager }

  TFormSettingsManager = class
  private
    FIniFilePath: String;
    FIniFileName: String;
  public
    { Gets configured ini file path. }
    property IniFilePath: String read FIniFilePath;

    { Gets configured ini file name. }
    property IniFileName: String read FIniFileName;

    { Initializes manager default values.

      Ini file folder is set to application folder. Ini file name is set to
      application exe file name with extension changed to ini. }
    constructor Create;

    { Sets ini file location to application folder. }
    procedure UseAppFolder;

    { Sets ini file location to <User>\AppData\<AFolder>.
      When AFolder not set, exe name without folder will be used. }
    procedure UseRoamingFolder(const AFolder: String = '');

    { Sets custom ini file location and name. }
    procedure UseCustomFolder(const AIniFilePath: String);

    { Sets ini file name to given value. if empty string given, ni file name is
      set to application exe file name with extension changed to ini. }
    procedure SetIniFileName(const AIniFileName: String = '');
  end;

  { TFormSettings }

  TFormSettings = class
  private
    FForm: TForm;

  protected

    { Builds settings file name. }
    function GetIniFileName: String;
  public

    { Binds current instance to AForm. }
    constructor Create(AForm: TForm);

    { Loads stored form settings. }
    procedure Load;

    { Stores current from settings. }
    procedure Save;
  end;

var
  FormSettingsManager: TFormSettingsManager;

implementation

{ TFormSettingsManager }

constructor TFormSettingsManager.Create;
begin
  UseAppFolder;
  SetIniFileName('');
end;

procedure TFormSettingsManager.UseAppFolder;
begin
  if (Application <> nil) then
    FIniFilePath
        := IncludeTrailingPathDelimiter(ExtractFilePath(Application.ExeName));
end;

procedure TFormSettingsManager.UseRoamingFolder(const AFolder: String);
var
  SubFolder: String;
  AppData: String;
begin
  SubFolder := AFolder;
  if (SubFolder = '') and (Application <> nil) then
      SubFolder := ExtractFileName(ChangeFileExt(Application.ExeName, ''));

  AppData := GetEnvironmentVariable('appdata');
  FIniFilePath := IncludeTrailingPathDelimiter(
      IncludeTrailingPathDelimiter(AppData) + SubFolder);
end;

procedure TFormSettingsManager.UseCustomFolder(const AIniFilePath: String);
begin
  FIniFilePath := IncludeTrailingPathDelimiter(AIniFilePath);
end;

procedure TFormSettingsManager.SetIniFileName(const AIniFileName: String);
begin
  FIniFileName := AIniFileName;

  // Set same name as app
  if (AIniFileName = '') and (Application <> nil) then
    FIniFileName := ExtractFileName(ChangeFileExt(Application.ExeName, '.ini'));
end;

{ TFormSettings }

function TFormSettings.GetIniFileName: String;
begin
  Result := FormSettingsManager.IniFilePath + FormSettingsManager.IniFileName;
end;

constructor TFormSettings.Create(AForm: TForm);
begin
  inherited Create;
  FForm := AForm;
end;

procedure TFormSettings.Load;
begin

end;

procedure TFormSettings.Save;
begin

end;

initialization
  FormSettingsManager := TFormSettingsManager.Create;
finalization;
  if (FormSettingsManager <> nil) then
    FreeAndNil(FormSettingsManager);
end.

