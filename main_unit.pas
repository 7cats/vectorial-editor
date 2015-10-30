unit main_unit;

{$mode objfpc}{$H+}

interface

uses
    Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs,
    ExtCtrls, Menus, ComCtrls, StdCtrls, DbCtrls, UTools, UFigures, UField, Math;

type

    { TDesk }

    TDesk = class(TForm)
        MainMenu: TMainMenu;
        FileMenu: TMenuItem;
        ExitMItem: TMenuItem;
        HelpMenu: TMenuItem;
        AboutMItem: TMenuItem;
        ClearCanvasItem: TMenuItem;
        ToolMenu: TMenuItem;
        ShowAllItem: TMenuItem;
        PaintDesk: TPaintBox;
        XcoordinateText: TStaticText;
        YcoordinateText: TStaticText;
        ToolsBar: TToolBar;
        ZoomBox: TComboBox;
        procedure AboutMItemClick(Sender: TObject);
        procedure ClearCanvasItemClick(Sender: TObject);
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
        procedure ShowAllItemClick(Sender: TObject);
        procedure ChangeComboBox(Sender: TObject);
        function IsFloat(str :string): boolean;
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
        Tools[IndexTool].MouseMove(point(X,y));
        Invalidate;
    end;
end;

procedure TDesk.PaintDeskMouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  if (Button = mbLeft) then begin
      IsMouseDown := False;
      Tools[IndexTool].Deactive(point(X,Y));
  end;
  Invalidate;
end;

procedure TDesk.PaintDeskResize(Sender: TObject);
begin
    ViewPort.PaintBoxResize(PaintDesk.Width, PaintDesk.Height);
end;


procedure TDesk.PanelBarButtonClick(Sender: TObject);
begin
    IndexTool:= (Sender as TToolButton).Tag;
    DrawContinue:= false;
end;

procedure TDesk.ShowAllItemClick(Sender: TObject);
begin
    ViewPort.ShowAll();
    Invalidate;
end;


procedure TDesk.AboutMItemClick(Sender: TObject);
begin
    ShowMessage('Разумов Максим, Б8103а, 2015 г.');
end;

procedure TDesk.ClearCanvasItemClick(Sender: TObject);
begin
    SetLength(Figures, 0);
    ViewPort:= TViewPort.Create(PaintDesk.Width, PaintDesk.Height);
    Invalidate;
end;

procedure TDesk.ChangeComboBox(Sender: TObject);
begin
    if (IsFloat((Sender as TComboBox).Caption) and (StrToFloat((Sender as TComboBox).Caption) > 0)) then begin
        ViewPort.FZoom:= StrToFloat((Sender as TComboBox).Caption) / 100;
    end;
    Invalidate;
end;

function TDesk.IsFloat(str: string): boolean;
var
    i, count: integer;
begin
    count := 0;
    for i := 0 to Length(str) do begin
        if (str = '.') then
            inc(count);
        if ((str[i] < '0') and (str > '9') and (str <> '.')) then begin
            result := false;
            exit();
        end;
    end;
    if (count <= 1) then
        exit(true);
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
    ViewPort := TViewPort.Create(PaintDesk.Width, PaintDesk.Height);
    DrawContinue:= false;
    ViewPort.AddDisplacement(PaintDesk.Width / 2, PaintDesk.Height / 2);
    ToolsBar.Images := ToolsImages;
    IsMouseDown:= false;
    ShowAllItem.Enabled:= false;
    for i := 0 to High(Tools) do begin
        button := TToolButton.Create(self);
        button.Parent := ToolsBar;
        button.Tag := i;
        button.ImageIndex:= i;
        button.OnClick := @PanelBarButtonClick;
    end;
    ZoomBox := TComboBox.Create(self);
    ZoomBox.Name:= 'ZoomBox';
    ZoomBox.Parent := ToolsBar;
    ZoomBox.AddItem('500', ZoomBox);
    ZoomBox.AddItem('250', ZoomBox);
    ZoomBox.AddItem('100', ZoomBox);
    ZoomBox.AddItem('75', ZoomBox);
    ZoomBox.AddItem('50', ZoomBox);
    ZoomBox.AddItem('25', ZoomBox);
    ZoomBox.AddItem('1', ZoomBox);
    ZoomBox.Caption:='100';
    ZoomBox.TabStop:= true;
    ZoomBox.OnEditingDone:= @ChangeComboBox;
end;


procedure TDesk.FormPaint(Sender: TObject);
var
    figure: TFigure;
begin
    PaintDesk.Canvas.brush.style := bsClear;
    // вывод положения центра ViewPoint
    YcoordinateText.Caption:= FloatToStr(ViewPort.FCenter.y);
    XcoordinateText.Caption:= FloatToStr(ViewPort.FCenter.x);
    if (Length(Figures) > 0) then begin
       ShowAllItem.Enabled := true;
       ViewPort.FTopBoarder := Figures[0].FPoints[0].y;
       ViewPort.FBottomBoarder := Figures[0].FPoints[0].y;
       ViewPort.FLeftBoarder := Figures[0].FPoints[0].x;
       ViewPort.FRightBoarder := Figures[0].FPoints[0].x;
    end
    else
       ShowAllItem.Enabled := false;
    ZoomBox.Caption := FloatToStr(round(ViewPort.FZoom * 10000) / 100);
    for figure in Figures do begin
        figure.Draw(PaintDesk.Canvas);
        ViewPort.FRightBoarder := max(ViewPort.FRightBoarder, figure.MaxX);
        ViewPort.FLeftBoarder := min(ViewPort.FLeftBoarder, figure.MinX);
        ViewPort.FTopBoarder := min(ViewPort.FTopBoarder, figure.MinY);
        ViewPort.FBottomBoarder := max(ViewPort.FBottomBoarder, figure.MaxY);
    end;
end;


procedure TDesk.PaintDeskMouseDown(Sender: TObject; Button: TMouseButton;
    Shift: TShiftState; X, Y: Integer);
begin
    if (Button = mbLeft) then begin
        Tools[IndexTool].Active(Point(X,y));
        DrawContinue:= true;
        IsMouseDown:= true;
    end
    else if (Button = mbRight) and (DrawContinue) then
        Tools[IndexTool].RightClick(Point(X,y));

    if (Button = mbRight) and (Tools[IndexTool].FName = 'zoom') then
        Tools[IndexTool].RightClick(Point(X,y));
    Invalidate;
end;

end.

