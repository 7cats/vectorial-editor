unit UMain;

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
        HorizontalScrollBar: TScrollBar;
        VecticalScrollBar: TScrollBar;
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
        procedure ScrollBarsOnScroll(Sender: TObject;
            ScrollCode: TScrollCode; var ScrollPos: Integer);
        procedure OnPaint(Sender: TObject);
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
            PosCenter: TFloatPoint;
            ScrollCount: TPoint;
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
var
    tmp: integer;
begin
    if (ssLeft in Shift) and (IsMouseDown) and not (Sender is TScrollBar) then begin
        //ShowMessage(intToStr(ScrollCount.X));
        Tools[IndexTool].MouseMove(point(X,y));

        if (X + HorizontalScrollBar.Position < HorizontalScrollBar.Min) then
            HorizontalScrollBar.Min := min(HorizontalScrollBar.Min, X - ScrollCount.X + HorizontalScrollBar.Min);

        //HorizontalScrollBar.Max := max(HorizontalScrollBar.Max, X + HorizontalScrollBar.Max - PaintDesk.Width - 30);

        if (Y + VecticalScrollBar.Position < VecticalScrollBar.Min) then
            VecticalScrollBar.Min := min(VecticalScrollBar.Min, Y  - ScrollCount.Y + VecticalScrollBar.Min);
        //VecticalScrollBar.Max := max(VecticalScrollBar.Max, Y + VecticalScrollBar.Max - PaintDesk.Height - 30);

        ScrollCount.X := X;
        ScrollCount.Y := Y;

        //ShowMessage(intToStr(X));
        //ShowMessage(inttostr(x - PaintDesk.Left));
        Invalidate;
    end;
end;

procedure TDesk.PaintDeskMouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
    if (Button = mbLeft) then begin
        IsMouseDown := False;
        Tools[IndexTool].Deactive(point(X,Y));
        ScrollCount.X:= 0;
        ScrollCount.Y:= 0;
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
    VecticalScrollBar.Min:= 0;
    VecticalScrollBar.Max:= PaintDesk.Height ;
    VecticalScrollBar.Position:= PaintDesk.Height div 2;
    VecticalScrollBar.PageSize:= PaintDesk.Height;
    ScrollCount.X:= 0;
    ScrollCount.Y:= 0;


    HorizontalScrollBar.Min:= 0;
    HorizontalScrollBar.Max:= PaintDesk.Width ;
    HorizontalScrollBar.Position:= PaintDesk.Width div 2;
    HorizontalScrollBar.PageSize:= PaintDesk.Width;

    ViewPort.AddDisplacement(PaintDesk.Width / 2, PaintDesk.Height / 2);
    PosCenter.X := ViewPort.FCenter.X;
    PosCenter.Y := ViewPort.FCenter.Y;
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

procedure TDesk.ScrollBarsOnScroll(Sender: TObject;
    ScrollCode: TScrollCode; var ScrollPos: Integer);
begin
    if (Sender as TScrollBar).Name = 'VecticalScrollBar' then begin
         //showMessage(inttostr(VecticalScrollBar.Position) + '  ' + inttostr(ScrollPos));
         ViewPort.AddDisplacement(0, VecticalScrollBar.Position - ScrollPos);
         //ShowMessage(IntToStr(VecticalScrollBar.Min) + ' ' + IntToStr(VecticalScrollBar.Max) + ' ' + IntToStr(ScrollPos));
    end
    else begin
        ViewPort.AddDisplacement(HorizontalScrollBar.Position - ScrollPos, 0);
        //ShowMessage(IntToStr(HorizontalScrollBar.Min) + ' ' + IntToStr(HorizontalScrollBar.Max) + ' ' + IntToStr(ScrollPos));
    end;
    Invalidate;
end;


procedure TDesk.OnPaint(Sender: TObject);
var
    figure: TFigure;
begin
    PaintDesk.Canvas.brush.style := bsClear;
    // вывод положения центра ViewPoint
    YcoordinateText.Caption:= FloatToStr(ViewPort.FCenter.y);
    XcoordinateText.Caption:= FloatToStr(ViewPort.FCenter.x);
    if (Length(Figures) > 0) then begin
       ShowAllItem.Enabled := true;
       ViewPort.FLeftTop.Y := Figures[0].FPoints[0].y;
       ViewPort.FRightBottom.Y := Figures[0].FPoints[0].y;
       ViewPort.FLeftTop.X := Figures[0].FPoints[0].x;
       ViewPort.FRightBottom.X := Figures[0].FPoints[0].x;
    end
    else
       ShowAllItem.Enabled := false;
    ZoomBox.Caption := FloatToStr(round(ViewPort.FZoom * 10000) / 100);
    for figure in Figures do begin
        figure.Draw(PaintDesk.Canvas);
        ViewPort.FRightBottom.x := max(ViewPort.FRightBottom.x, figure.MaxX);
        ViewPort.FLeftTop.x := min(ViewPort.FLeftTop.x, figure.MinX);
        ViewPort.FLeftTop.Y := min(ViewPort.FLeftTop.Y, figure.MinY);
        ViewPort.FRightBottom.Y := max(ViewPort.FRightBottom.Y, figure.MaxY);
    end;

{    VecticalScrollBar.Min := min(VecticalScrollBar.Min, VecticalScrollBar.Position + round(ViewPort.FCenter.Y - PosCenter.Y));
    VecticalScrollBar.Max := max(VecticalScrollBar.Max, VecticalScrollBar.Position + round(ViewPort.FCenter.Y - PosCenter.Y));

    HorizontalScrollBar.Min := min(HorizontalScrollBar.Min, HorizontalScrollBar.Position + round(ViewPort.FCenter.X - PosCenter.X));
    HorizontalScrollBar.Max := max(HorizontalScrollBar.Max, HorizontalScrollBar.Position + round(ViewPort.FCenter.X - PosCenter.X)); }



{    VecticalScrollBar.Position := VecticalScrollBar.Position + round(ViewPort.FCenter.Y - PosCenter.Y);
    HorizontalScrollBar.Position := HorizontalScrollBar.Position + round(ViewPort.FCenter.X - PosCenter.X);

    PosCenter.X := ViewPort.FCenter.X;
    PosCenter.Y := ViewPort.FCenter.Y;}
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
