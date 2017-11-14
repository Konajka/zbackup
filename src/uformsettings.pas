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
    { Returns first value if non zero else second. }
    function NonZero(AValue1, AValue2: Integer): Integer;
    { Return position of centered dimension. }
    function Center(ADim, ARange: Integer): Integer;
    { Checks if value is in rage. If not, value is corrected. }
    function Range(AValue, AMin, AMax): Integer;
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

function TFormSettings.NonZero(AValue1, AValue2: Integer): Integer;
begin
  if (Value1 = 0) then
    Result := Value2
  else
    Result := Value1;
end;

function TFormSettings.Center(ADim, ARange: Integer): Integer;
begin
  Result := (Range - Dim) div 2;
end;

function TFormSettings.Range(AValue, AMin, AMax): Integer;
begin
  if (AValue < AMin)
end;

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
var
  Filename: String;
  LoadSize, LoadPos: Boolean;
begin
  if (FForm = nil) then Exit;

  LoadPos := (FForm.Position = poDesigned)
      or (FForm.Position = poDefaultSizeOnly);
  LoadSize := (FForm.Position = poDesigned)
      or (FForm.Position = poDefaultPosOnly);

  Filename := FormSettingsManager.IniFilePath + FormSettingsManager.IniFilePath;
  with TIniFile.Create(Filename) do try

    if LoadSize then begin
      FForm.Width := ReadInteger(FForm.Name, 'Width',
          NonZero(FForm.Constraints.MinWidth, 320));
      FForm.Height := ReadInteger(FForm.Name, 'Height',
          NonZero(FForm.Constraints.MinHeight, 240));
    end;

    if LoadPos then begin
      FForm.Left := Range(
          ReadInteger(FForm.Name, 'Left', Center(FForm.Width, Screen.Width)),
          0, Screen.Width - 10);
      FForm.Top := Range(
          ReadInteger(FForm.Name, 'Top', Center(FForm.Height, Screen.Height)),
          0, Screen.Height - 10);
    end;

  finally
    Free;
  end;
end;

procedure TFormSettings.Save;
var
  Filename: String;
  SaveSize, SavePos: Boolean;
begin
  if (FForm = nil) then Exit;

  SavePos := (FForm.Position = poDesigned)
      or (FForm.Position = poDefaultSizeOnly);
  SaveSize := (FForm.Position = poDesigned)
      or (FForm.Position = poDefaultPosOnly);

  Filename := FormSettingsManager.IniFilePath + FormSettingsManager.IniFilePath;
  with TIniFile.Create(Filename) do try

    if SavePos then begin
      WriteInteger(FForm.Name, 'WindowState', Integer(FForm.WindowState));
    end;

    if (FForm.WindowState = wsNormal) then begin

      if SavePos then begin
        WriteInteger(FForm.Name, 'Left', FForm.Left);
        WriteInteger(FForm.Name, 'Top', FForm.Top);
      end;

      if SaveSize then begin
        WriteInteger(FForm.Name, 'Width', FForm.Width);
        WriteInteger(FForm.Name, 'Height', FForm.Height);
      end;

    end;

  finally
    Free;
  end;
end;

initialization
  FormSettingsManager := TFormSettingsManager.Create;
finalization;
  if (FormSettingsManager <> nil) then
    FreeAndNil(FormSettingsManager);
end.

