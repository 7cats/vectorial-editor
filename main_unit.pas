unit main_unit;

{$mode objfpc}{$H+}

interface

uses
    Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs,
    ExtCtrls, Menus, ComCtrls, StdCtrls, UTools, UFigures;

type

    { TDesk }

    TDesk = class(TForm)
        MainMenu: TMainMenu;
        FileMenu: TMenuItem;
        ExitMItem: TMenuItem;
        HelpMenu: TMenuItem;
        AboutMItem: TMenuItem;
        PaintDesk: TPaintBox;
        ToolsBar: TToolBar;
        procedure AboutMItemClick(Sender: TObject);
        procedure ExitItemClick(Sender: TObject);
        procedure FormCreate(Sender: TObject);
        procedure FormPaint(Sender: TObject);
        procedure PaintDeskMouseDown(Sender: TObject; Button: TMouseButton;
            Shift: TShiftState; X, Y: Integer);
        procedure PaintDeskMouseMove(Sender: TObject; Shift: TShiftState; X,
            Y: Integer);
        procedure PanelBarButtonClick (Sender: TObject);
        private
            IndexTool: integer;
            DrawContinue : boolean;
        public
            { public declarations }
    end;

var
    Desk: TDesk;

implementation

{$R *.lfm}

{ TDesk }


procedure TDesk.PaintDeskMouseMove(Sender: TObject; Shift: TShiftState; X,
    Y: Integer);
begin
    if (ssLeft in Shift) then begin
        Tools[IndexTool].StartDrawing(point(x,y));
        Invalidate;
    end;
end;


procedure TDesk.PanelBarButtonClick(Sender: TObject);
begin
    IndexTool:= (Sender as TToolButton).Tag;
    DrawContinue:= false;
end;


procedure TDesk.AboutMItemClick(Sender: TObject);
begin
    ShowMessage('Разумов Максим, Б8103а, 2015 г.');
end;

procedure TDesk.ExitItemClick(Sender: TObject);
begin
    Close;
end;

procedure TDesk.FormCreate(Sender: TObject);
var
    button: TToolButton;
    i: integer;
begin
    IndexTool:= 0;
    DrawContinue:= false;
    ToolsBar.Images := ToolsImages;
    for i := 0 to High(Tools) do begin
        button := TToolButton.Create(self);
        button.Parent := ToolsBar;
        button.Tag := i;
        button.ImageIndex:= i;
        button.OnClick := @PanelBarButtonClick;
    end;
end;


procedure TDesk.FormPaint(Sender: TObject);
var
    figure: TFigure;
begin
    PaintDesk.Canvas.brush.style := bsClear;
    for figure in Figures do
        figure.Draw(PaintDesk.Canvas);
end;


procedure TDesk.PaintDeskMouseDown(Sender: TObject; Button: TMouseButton;
    Shift: TShiftState; X, Y: Integer);
begin
    if (ssLeft in Shift) then begin
        Tools[IndexTool].AddFigure(Point(x,y));
        DrawContinue:= true;
    end
    else if (ssRight in Shift) and (DrawContinue) then
        Tools[IndexTool].AdditionalDraw(Point(x,y));
    Invalidate;
end;

end.

