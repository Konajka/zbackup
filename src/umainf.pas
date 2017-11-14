{
  Application main form.

  @author(Tomáš Borek <tomas.borek@post.cz>)
}
unit UMainF;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs,
  UFormSettings;

type

  { TMainForm }

  TMainForm = class(TForm)
  private
    FFormSettings: TFormSettings;
  public

    { Customizes form when created. }
    constructor Create(AOwner: TComponent); override;

    { Frees allocated resources. }
    destructor Destroy; override;
  end;

var
  MainForm: TMainForm;

implementation

{$R *.lfm}

resourcestring
  sFormTitle = 'ZBackup';

{ TMainForm }

constructor TMainForm.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  Caption := sFormTitle;

  FFormSettings := TFormSettings.Create(self);
  FFormSettings.Load;
end;

destructor TMainForm.Destroy;
begin
  if (FFormSettings  <> nil) then begin
    FFormSettings.Save;
    FreeAndNil(FFormSettings);
  end;

  inherited Destroy;
end;



end.

