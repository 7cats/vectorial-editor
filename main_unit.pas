unit main_unit;

{$mode objfpc}{$H+}

interface

uses
    Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs,
    ExtCtrls, Menus, ComCtrls, StdCtrls, UTools, UFigures, UField;

type

    { TDesk }

    TDesk = class(TForm)
        MainMenu: TMainMenu;
        FileMenu: TMenuItem;
        ExitMItem: TMenuItem;
        HelpMenu: TMenuItem;
        AboutMItem: TMenuItem;
        PaintDesk: TPaintBox;
        XcoordinateText: TStaticText;
        YcoordinateText: TStaticText;
        ToolsBar: TToolBar;
        procedure AboutMItemClick(Sender: TObject);
        procedure ExitItemClick(Sender: TObject);
        procedure FormCreate(Sender: TObject);
        procedure FormPaint(Sender: TObject);
        procedure PaintDeskMouseDown(Sender: TObject; Button: TMouseButton;
            Shift: TShiftState; X, Y: Integer);
        procedure PaintDeskMouseMove(Sender: TObject; Shift: TShiftState; X,
            Y: Integer);
        procedure PaintDeskMouseUp(Sender: TObject; Button: TMouseButton;
          Shift: TShiftState; X, Y: Integer);
        procedure PaintDeskResize(Sender: TObject);
        procedure PanelBarButtonClick (Sender: TObject);
        private
            IndexTool: integer;
            DrawContinue, IsMouseDown : boolean;
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
    if (ssLeft in Shift) and (IsMouseDown) then begin
        Tools[IndexTool].ToolMove(point(X,y));
        Invalidate;
    end;
end;

procedure TDesk.PaintDeskMouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  if (Button = mbLeft) then begin
      IsMouseDown := False;
      Tools[IndexTool].SpecificAction(point(X,Y), Desk.Width, Desk.Height);
  end;
  Invalidate;
end;

procedure TDesk.PaintDeskResize(Sender: TObject);
begin
    ViewPort.PaintBoxResize(PaintDesk.Width / 2, PaintDesk.Height / 2);
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
    ViewPort := TViewPort.Create(PaintDesk.Width / 2, PaintDesk.Height / 2);
    DrawContinue:= false;
    ViewPort.AddDisplacement(PaintDesk.Width / 2, PaintDesk.Height / 2);
    ToolsBar.Images := ToolsImages;
    IsMouseDown:= false;
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
    // вывод положения центра ViewPoint
    YcoordinateText.Caption:= FloatToStr(ViewPort.FCenter.y);
    XcoordinateText.Caption:= FloatToStr(ViewPort.FCenter.x);
    for figure in Figures do
        figure.Draw(PaintDesk.Canvas);
end;


procedure TDesk.PaintDeskMouseDown(Sender: TObject; Button: TMouseButton;
    Shift: TShiftState; X, Y: Integer);
begin
    if (Button = mbLeft) then begin
        Tools[IndexTool].MakeActive(Point(X,y));
        DrawContinue:= true;
        IsMouseDown:= true;
    end
    else if (Button = mbRight) and (DrawContinue) then
        Tools[IndexTool].AdditionalAction(Point(X,y));

    if (Button = mbRight) and (Tools[IndexTool].FName = 'zoom') then
        Tools[IndexTool].AdditionalAction(Point(X,y));
    Invalidate;
end;

end.

