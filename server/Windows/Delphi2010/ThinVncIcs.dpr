program ThinVncIcs;



uses
  Forms,
  ThinVnc.OverbyteIcsWebServ in 'ThinVnc.OverbyteIcsWebServ.pas' {WebServForm};

{$R *.RES}

begin
  Application.Title := 'ThinVNC Server';
  Application.CreateForm(TWebServForm, WebServForm);
  Application.Run;
end.
