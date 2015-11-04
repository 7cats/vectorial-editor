unit UTools;

{$mode objfpc}{$H+}{$M+}{$R+}

interface

uses
    Classes, SysUtils, UFigures, Forms, Controls, Graphics, Dialogs,
    UField, Math;

type

    { TTool }

    TTool = class
        procedure MouseMove(point : TPoint); virtual; abstract;
        constructor Create(name : string);
        procedure MouseDown(point : TPoint; penColor: TColor); virtual; abstract;
        procedure RightClick(point : TPoint); virtual;
        procedure MouseUp(point : TPoint); virtual;
        public
            FName: string;
    end;

    { TPencilTool }

    TPencilTool = class(TTool)
        procedure MouseDown(point : TPoint; penColor: TColor); override;
        procedure MouseMove (point : TPoint); override;
    end;

    { TPolylineTool }

    TPolylineTool = class(TTool)
        procedure MouseDown(point : TPoint; penColor: TColor); override;
        procedure MouseMove (point : TPoint); override;
        procedure RightClick (point : TPoint); override;
    end;

    { TEllipseTool }

    TEllipseTool = class(TPolylineTool)
        procedure MouseDown(point : TPoint; penColor: TColor); override;
    end;

    { TRecnungleTool }

    TRecnungleTool = class(TPolylineTool)
        procedure MouseDown(point : TPoint; penColor: TColor); override;
    end;

    { TRoundRectangleTool }

    TRoundRectangleTool = class(TRecnungleTool)
        procedure MouseDown(point : TPoint; penColor: TColor); override;
    end;

    {THandTool}

    THandTool = class(TTool)
        private
            FBeginCoordinate: TFloatPoint;
            procedure MouseDown(point : TPoint; penColor: TColor); override;
            procedure MouseMove(point : TPoint); override;
    end;

    { TZoomTool }

    TZoomTool = class(TTool)
        procedure MouseDown(point : TPoint; penColor: TColor); override;
        procedure MouseMove(point : TPoint); override;
        procedure RightClick(point: TPoint); override;
        procedure MouseUp(point : TPoint); override;
        procedure ClickZoom ();
        procedure RectZoom ();
        private
            FBeginZoomRect, FEndZoomRect: TFloatPoint;
    end;

var
    Tools: array of TTool;
    ToolsImages: TImageList;

implementation

procedure AddFigure(figure: TFigure);
begin
    SetLength(Figures, Length(Figures) + 1);
    Figures[High(Figures)] := figure;
end;

procedure AddTool(tool : TTool);
begin
    SetLength(Tools, Length(Tools) + 1);
    Tools[High(Tools)] := tool;
end;

{ TZoomTool }

procedure TZoomTool.MouseDown(point: TPoint; penColor: TColor);
begin
    AddFigure(TRectangle.Create(point, penColor));
    FBeginZoomRect := ViewPort.ScreenToWorld(point);
end;

procedure TZoomTool.MouseMove(point: TPoint);
begin
    Figures[High(Figures)].Stranch(point);
end;

procedure TZoomTool.RightClick(point: TPoint);
begin
    ViewPort.FZoom := max(0.01, ViewPort.FZoom - 0.09);
end;

procedure TZoomTool.MouseUp(point: TPoint);
begin
    FEndZoomRect := ViewPort.ScreenToWorld(point);
    SetLength(Figures, Length(Figures) - 1);

    if (sqrt(sqr(FEndZoomRect.X - FBeginZoomRect.X) + sqr(FEndZoomRect.Y - FBeginZoomRect.Y)) < 10) then
        ClickZoom()
    else
        RectZoom();
end;

procedure TZoomTool.ClickZoom;
begin
    ViewPort.AddDisplacement(ViewPort.FCenter.X - FBeginZoomRect.X,
                             ViewPort.FCenter.Y - FBeginZoomRect.Y);
    ViewPort.FZoom += 0.09;
        ViewPort.FZoom := min(50, ViewPort.FZoom);
end;

procedure TZoomTool.RectZoom();
begin
    ViewPort.AddDisplacement(ViewPort.FCenter.X - (FBeginZoomRect.X + FEndZoomRect.X) / 2,
                             ViewPort.FCenter.Y - (FBeginZoomRect.Y + FEndZoomRect.Y) / 2);
    ViewPort.FZoom += min((ViewPort.FPaintBoxSize.Y) / abs(ViewPort.WorldToScreen(FEndZoomRect).Y  - ViewPort.WorldToScreen(FBeginZoomRect).Y),
                          (ViewPort.FPaintBoxSize.X) / abs(ViewPort.WorldToScreen(FEndZoomRect).X - ViewPort.WorldToScreen(FBeginZoomRect).X));
    ViewPort.FZoom := min(50, ViewPort.FZoom);
end;

{ THandTool }

procedure THandTool.MouseDown(point: TPoint; penColor: TColor);
begin
    FBeginCoordinate:= ViewPort.ScreenToWorld(point);
end;

procedure THandTool.MouseMove(point: TPoint);
begin
    ViewPort.AddDisplacement(ViewPort.ScreenToWorld(point).x - FBeginCoordinate.x,
                             ViewPort.ScreenToWorld(point).y - FBeginCoordinate.y);
    FBeginCoordinate:= ViewPort.ScreenToWorld(point);

end;

{ TPolylineTool }

procedure TPolylineTool.MouseDown(point: TPoint; penColor: TColor);
begin
    AddFigure(TPolyline.Create(point, penColor));
end;

procedure TPolylineTool.MouseMove(point: TPoint);
begin
    Figures[High(Figures)].Stranch(point);
end;

procedure TPolylineTool.RightClick(point: TPoint);
begin
    Figures[High(Figures)].AddPoint(point);
end;

{ TRoundRectangleTool }

procedure TRoundRectangleTool.MouseDown(point: TPoint; penColor: TColor);
begin
    AddFigure(TRoundRectangle.Create(point, penColor));
end;

{ TRecnungleTool }

procedure TRecnungleTool.MouseDown(point: TPoint; penColor: TColor);
begin
    AddFigure(TRectangle.Create(point, penColor));
end;


{ TEllipseTool }

procedure TEllipseTool.MouseDown(point: TPoint; penColor: TColor);
begin
    AddFigure(TEllipse.Create(point, penColor));
end;

{ TPencilTool }

procedure TPencilTool.MouseDown(point: TPoint; penColor: TColor);
begin
    AddFigure(TPencil.Create(point, penColor));
end;

procedure TPencilTool.MouseMove(point: TPoint);
begin
    Figures[High(Figures)].AddPoint(point);
end;

{ TTool }

constructor TTool.Create(name: string);
var
    i : TIcon;
begin
    FName:= name;
    i := TIcon.Create;
    i.LoadFromFile('IconPanel/' + name + '.ico');
    ToolsImages.AddIcon(i);
end;

procedure TTool.RightClick(point: TPoint);
begin
end;

procedure TTool.MouseUp(point: TPoint);
begin
end;

initialization
    ToolsImages := TImageList.Create(Nil);
    ToolsImages.Height:= 48;
    ToolsImages.Width:= 48;
    AddTool(TPencilTool.Create('pencil'));
    AddTool(TPolylineTool.Create('polyline'));
    AddTool(TEllipseTool.Create('ellipse'));
    AddTool(TRecnungleTool.Create('rectungle'));
    AddTool(TRoundRectangleTool.Create('roundrect'));
    AddTool(THandTool.Create('hand'));
    AddTool(TZoomTool.Create('zoom'));
end.

