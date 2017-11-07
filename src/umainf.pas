{
  Application main form.

  @author(Tomáš Borek <tomas.borek@post.cz>)
}
unit UMainF;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs;

type

  { TMainForm }

  TMainForm = class(TForm)
  public
    { Customizes form when created. }
    constructor Create(AOwner: TComponent); override;
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
end;



end.

