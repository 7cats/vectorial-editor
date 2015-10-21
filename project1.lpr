program project1;

{$mode objfpc}{$H+}

uses
    {$IFDEF UNIX}{$IFDEF UseCThreads}
    cthreads,
    {$ENDIF}{$ENDIF}
    Interfaces, // this includes the LCL widgetset
    Forms, main_unit, UTools, UFigures
    { you can add units after this };

{$R *.res}

begin
    RequireDerivedFormResource := True;
    Application.Initialize;
    Application.CreateForm(TDesk, Desk);
    Application.Run;
end.

