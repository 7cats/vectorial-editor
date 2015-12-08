unit UMain;

{$mode objfpc}{$H+}

interface

uses
    Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, ExtCtrls,
    Menus, ComCtrls, StdCtrls, DbCtrls, Grids, UTools, UFigures, UField, Math,
    ULocation, UInspector;

type

    { TDesk }

    TDesk = class(TForm)
        ColorDialog: TColorDialog;
        PaletteGrid: TDrawGrid;
        MainMenu: TMainMenu;
        FileMenu: TMenuItem;
        ExitMItem: TMenuItem;
        HelpMenu: TMenuItem;
        AboutMItem: TMenuItem;
        ClearCanvasItem: TMenuItem;
        HorizontalScrollBar: TScrollBar;
        Panel1: TPanel;
        FiguresPropPanel: TPanel;
        Panel3: TPanel;
        MainColorShape: TShape;
        AdditionalColorShape: TShape;
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
        procedure PaletteGridDrawCell(Sender: TObject; aCol, aRow: Integer;
            aRect: TRect; aState: TGridDrawState);
        procedure ExitItemClick(Sender: TObject);
        procedure FormCreate(Sender: TObject);
        procedure PaletteGridMouseDown(Sender: TObject; Button: TMouseButton;
            Shift: TShiftState; X, Y: Integer);
        procedure PalleteGridOnDblClick(Sender: TObject);
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
        procedure ShapeMouseDown(Sender: TObject; Button: TMouseButton;
            Shift: TShiftState; X, Y: Integer);
        procedure ShowAllItemClick(Sender: TObject);
        procedure ChangeComboBox(Sender: TObject);
        function IsFloat(str :string): boolean;
        procedure ScrollBarsChange();
        private
            IndexTool: integer;
            DrawContinue, IsMouseDown : boolean;
            PosCenter: TFloatPoint;
            ScrollCount: TPoint;
            PaletteColors : array of TColor;
            MCol, Mrow: integer;
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
    if (ssLeft in Shift) and (IsMouseDown) and not (Sender is TScrollBar) then begin
        Tools[IndexTool].MouseMove(point(X,y), ssShift in Shift);
        Invalidate;
    end;
end;

procedure TDesk.PaintDeskMouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
    if (Button = mbLeft) then begin
        IsMouseDown := False;
        Tools[IndexTool].MouseUp(point(X,Y), ssShift in Shift);
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

procedure TDesk.ShapeMouseDown(Sender: TObject; Button: TMouseButton;
    Shift: TShiftState; X, Y: Integer);
begin
    if (ColorDialog.Execute) then
        (Sender as TShape).Brush.Color := ColorDialog.Color;
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

procedure TDesk.PalleteGridOnDblClick(Sender: TObject);
begin
  if ColorDialog.Execute then begin
      PaletteColors[PaletteGrid.ColCount * Mrow + Mcol] := ColorDialog.Color;
      PaletteGrid.InvalidateCell(Mcol, Mrow);
  end;
end;


procedure TDesk.PaletteGridDrawCell(Sender: TObject; aCol, aRow: Integer;
    aRect: TRect; aState: TGridDrawState);
begin
    PaletteGrid.Canvas.Brush.Color := PaletteColors[aRow * PaletteGrid.ColCount + aCol];
    PaletteGrid.Canvas.FillRect(aRect);
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

procedure TDesk.ScrollBarsChange;
begin
    HorizontalScrollBar.Min:=  min(HorizontalScrollBar.min, round(ViewPort.WorldToScreen(ViewPort.FLeftTop).x));
    HorizontalScrollBar.Max:= max(HorizontalScrollBar.Max,round(ViewPort.WorldToScreen(ViewPort.FRightBottom).x));
    VecticalScrollBar.Min:=  min(VecticalScrollBar.Min, round(ViewPort.WorldToScreen(ViewPort.FLeftTop).y));
    VecticalScrollBar.Max:= max(VecticalScrollBar.Max, round(ViewPort.WorldToScreen(ViewPort.FRightBottom).y));
end;

procedure TDesk.ExitItemClick(Sender: TObject);
begin
    Close;
end;

procedure TDesk.FormCreate(Sender: TObject);
const
    tint = 25;
var
    button: TToolButton;
    i, j: integer;
    r, g, b: integer;
begin

    IndexTool:= 0;
    ViewPort := TViewPort.Create(PaintDesk.Width, PaintDesk.Height);
    DrawContinue:= false;
    VecticalScrollBar.Min:= 0;
    VecticalScrollBar.Max:= PaintDesk.Height;
    VecticalScrollBar.Position:= PaintDesk.Height div 2;
    //VecticalScrollBar.PageSize:= PaintDesk.Height;


    HorizontalScrollBar.Min:= 0;
    HorizontalScrollBar.Max:= PaintDesk.Width;
    HorizontalScrollBar.Position:= PaintDesk.Width div 2;
    //HorizontalScrollBar.PageSize:= PaintDesk.Width;

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

    assignFile (input, 'primary_colors.txt');
    reset(input);
    i := 0;
    SetLength(PaletteColors, PaletteGrid.RowCount * PaletteGrid.ColCount);
    while not EOF do begin
         read(r, g, b);
         PaletteColors[i] := RGBToColor(r, g, b);
         for j := 1 to PaletteGrid.RowCount - 1 do begin
              if (r < 128) then
                  r += tint
              else
                  r -= tint;
              if (g < 128) then
                  g += tint
              else
                  g -= tint;
              if (b < 128) then
                  b += tint
              else
                  b -= tint;
              PaletteColors[i + (PaletteGrid.ColCount * j)] := RGBToColor(r, g, b);
         end;
         inc(i);
    end;
    closeFile(input);
end;

procedure TDesk.PaletteGridMouseDown(Sender: TObject; Button: TMouseButton;
    Shift: TShiftState; X, Y: Integer);
var
    Col, Row: integer;
begin
    PaletteGrid.MouseToCell(X, Y, Col, Row);
    if Button = mbLeft then begin
        MainColorShape.Brush.Color := PaletteColors[PaletteGrid.ColCount * Row + Col];
    end;
    if ssRight in Shift then begin
        AdditionalColorShape.Brush.Color := PaletteColors[PaletteGrid.ColCount * Row + Col];
    end;
    Mcol := col;
    Mrow := row;
end;


procedure TDesk.ScrollBarsOnScroll(Sender: TObject;
    ScrollCode: TScrollCode; var ScrollPos: Integer);
begin
    if (Sender as TScrollBar).Name = 'VecticalScrollBar' then begin
         ViewPort.AddDisplacement(0, VecticalScrollBar.Position - ScrollPos);
    end
    else begin
        ViewPort.AddDisplacement(HorizontalScrollBar.Position - ScrollPos, 0);
    end;
    Invalidate;
end;


procedure TDesk.OnPaint(Sender: TObject);
var
    figure: TFigure;
begin
    // вывод положения центра ViewPoint
    YcoordinateText.Caption:= FloatToStr(ViewPort.FCenter.y);
    XcoordinateText.Caption:= FloatToStr(ViewPort.FCenter.x);
    if (Length(Figures) > 0) then begin
       ScrollBarsChange();
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
        PaintDesk.Canvas.brush.style := bsClear;
        figure.Draw(PaintDesk.Canvas);
        ViewPort.FRightBottom.x := max(ViewPort.FRightBottom.x, figure.MaxX);
        ViewPort.FLeftTop.x := min(ViewPort.FLeftTop.x, figure.MinX);
        ViewPort.FLeftTop.Y := min(ViewPort.FLeftTop.Y, figure.MinY);
        ViewPort.FRightBottom.Y := max(ViewPort.FRightBottom.Y, figure.MaxY);
    end;
end;


procedure TDesk.PaintDeskMouseDown(Sender: TObject; Button: TMouseButton;
    Shift: TShiftState; X, Y: Integer);
var
    i : integer;
begin
    if (Button = mbLeft) then begin
        if not (ssShift in Shift) then begin
            for i := 0 to High(Figures) do begin
                Figures[i].CleanSelect;
            end;
        end;

        Tools[IndexTool].MouseDown(Point(X,y), ssShift in Shift);

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
