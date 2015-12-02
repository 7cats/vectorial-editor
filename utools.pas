unit UTools;

{$mode objfpc}{$H+}{$M+}{$R+}

interface

uses
    Classes, SysUtils, UFigures, Forms, Controls, Graphics, Dialogs,
    UField, Math, ULocation, LCLIntf, LCLType;

type

    { TTool }

    TTool = class
        procedure Options(); virtual; abstract;
        procedure MouseMove(point : TPoint; shift : boolean); virtual; abstract;
        constructor Create(name : string);
        procedure MouseDown(point : TPoint; penColor: TColor; shift : boolean); virtual; abstract;
        procedure RightClick(point : TPoint); virtual;
        procedure MouseUp(point : TPoint; shift : boolean); virtual;
        procedure GiveParam(point : TPoint); virtual; abstract;
        procedure GetOptions(point : TPoint); virtual; abstract;
        public
            FName: string;
    end;

    { TPencilTool }

    TPencilTool = class(TTool)
        procedure MouseDown(point : TPoint; penColor: TColor; shift : boolean); override;
        procedure MouseMove (point : TPoint; shift : boolean); override;
    end;

    { TPolylineTool }

    TPolylineTool = class(TTool)
        procedure MouseDown(point : TPoint; penColor: TColor; shift : boolean); override;
        procedure MouseMove (point : TPoint; shift : boolean); override;
        procedure RightClick (point : TPoint); override;
    end;

    { TEllipseTool }

    TEllipseTool = class(TPolylineTool)
        procedure MouseDown(point : TPoint; penColor: TColor; shift : boolean); override;
    end;

    { TRecungleTool }

    TRecungleTool = class(TPolylineTool)
        procedure MouseDown(point : TPoint; penColor: TColor; shift : boolean); override;
    end;

    { TRoundRectangleTool }

    TRoundRectangleTool = class(TRecungleTool)
        procedure MouseDown(point : TPoint; penColor: TColor; shift : boolean); override;
    end;

    {THandTool}

    THandTool = class(TTool)
        private
            FBeginCoordinate: TFloatPoint;
            procedure MouseDown(point : TPoint; penColor: TColor; shift : boolean); override;
            procedure MouseMove(point : TPoint; shift : boolean); override;
    end;

    { TZoomTool }

    TZoomTool = class(TTool)
        procedure MouseDown(point : TPoint; penColor: TColor; shift : boolean); override;
        procedure MouseMove(point : TPoint; shift : boolean); override;
        procedure RightClick(point: TPoint); override;
        procedure MouseUp(point : TPoint; shift : boolean); override;
        procedure ClickZoom ();
        procedure RectZoom ();
        private
            FBeginZoomRect, FEndZoomRect: TFloatPoint;
    end;

    { TSelectionTool }

    TSelectionTool = class(TTool)
        procedure MouseDown(point : TPoint; penColor: TColor; shift : boolean); override;
        procedure MouseMove(point : TPoint; shift : boolean); override;
        procedure MouseUp (point: TPoint; shift : boolean); override;
        private
            FFirstPoint, FSecondPoint : TPoint;
            FSelected : boolean;
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

{ TSelectionTool }

procedure TSelectionTool.MouseDown(point: TPoint; penColor: TColor; shift : boolean);
var
    i : integer;
begin
    FSelected := shift;
    FFirstPoint := point;
    FSecondPoint := point;
    if (not FSelected) then begin

        AddFigure(TRectangle.Create(point, penColor));
    end;
end;

procedure TSelectionTool.MouseMove(point: TPoint; shift: boolean);
var
    ClickRec : TRect;
begin
    if (not FSelected) then begin
        FSecondPoint := point;

        Figures[High(Figures)].Stranch(FSecondPoint);

        ClickRec.Left:= min(FFirstPoint.X, FSecondPoint.X);
        ClickRec.Right := max(FSecondPoint.X, FFirstPoint.X);
        ClickRec.Top:= min(FFirstPoint.Y, FSecondPoint.Y);
        ClickRec.Bottom := max(FSecondPoint.Y, FFirstPoint.Y);

        Location.CheckIntersectionAllFigures(ClickRec);

    end
    else begin
        FFirstPoint := FSecondPoint;
        FSecondPoint := point;
        Location.ChangePos(FFirstPoint.X - FSecondPoint.X, FFirstPoint.Y - FSecondPoint.Y);
    end;

end;

procedure TSelectionTool.MouseUp(point: TPoint; shift : boolean);
var
    ClickRec : TRect;
begin
    if (not FSelected) then begin
        ClickRec.Left:= min(FFirstPoint.X, FSecondPoint.X);
        ClickRec.Right := max(FSecondPoint.X, FFirstPoint.X);
        ClickRec.Top:= min(FFirstPoint.Y, FSecondPoint.Y);
        ClickRec.Bottom := max(FSecondPoint.Y, FFirstPoint.Y);

        Location.CheckIntersectionAllFigures(ClickRec);

        SetLength(Figures, Max(Length(Figures) - 1, 0));
    end;
end;

{ TZoomTool }

procedure TZoomTool.MouseDown(point: TPoint; penColor: TColor; shift : boolean);
begin
    AddFigure(TRectangle.Create(point, penColor));
    FBeginZoomRect := ViewPort.ScreenToWorld(point);
end;

procedure TZoomTool.MouseMove(point: TPoint; shift : boolean);
begin
    Figures[High(Figures)].Stranch(point);
end;

procedure TZoomTool.RightClick(point: TPoint);
begin
    ViewPort.FZoom := max(0.01, ViewPort.FZoom - 0.09);
end;

procedure TZoomTool.MouseUp(point: TPoint; shift : boolean);
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

procedure THandTool.MouseDown(point: TPoint; penColor: TColor; shift : boolean);
begin
    FBeginCoordinate:= ViewPort.ScreenToWorld(point);
end;

procedure THandTool.MouseMove(point: TPoint; shift : boolean);
begin
    ViewPort.AddDisplacement(ViewPort.ScreenToWorld(point).x - FBeginCoordinate.x,
                             ViewPort.ScreenToWorld(point).y - FBeginCoordinate.y);
    FBeginCoordinate:= ViewPort.ScreenToWorld(point);

end;

{ TPolylineTool }

procedure TPolylineTool.MouseDown(point: TPoint; penColor: TColor; shift : boolean);
begin
    AddFigure(TPolyline.Create(point, penColor));
end;

procedure TPolylineTool.MouseMove(point: TPoint; shift : boolean);
begin
    Figures[High(Figures)].Stranch(point);
end;

procedure TPolylineTool.RightClick(point: TPoint);
begin
    Figures[High(Figures)].AddPoint(point);
end;

{ TRoundRectangleTool }

procedure TRoundRectangleTool.MouseDown(point: TPoint; penColor: TColor; shift : boolean);
begin
    AddFigure(TRoundRectangle.Create(point, penColor));
end;

{ TRecungleTool }

procedure TRecungleTool.MouseDown(point: TPoint; penColor: TColor; shift : boolean);
begin
    AddFigure(TRectangle.Create(point, penColor));
end;


{ TEllipseTool }

procedure TEllipseTool.MouseDown(point: TPoint; penColor: TColor; shift : boolean);
begin
    AddFigure(TEllipse.Create(point, penColor));
end;

{ TPencilTool }

procedure TPencilTool.MouseDown(point: TPoint; penColor: TColor; shift : boolean);
begin
    AddFigure(TPencil.Create(point, penColor));
end;

procedure TPencilTool.MouseMove(point: TPoint; shift : boolean);
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

procedure TTool.MouseUp(point: TPoint; shift: boolean);
begin
end;

initialization
    ToolsImages := TImageList.Create(Nil);
    ToolsImages.Height:= 48;
    ToolsImages.Width:= 48;
    AddTool(TPencilTool.Create('pencil'));
    AddTool(TPolylineTool.Create('polyline'));
    AddTool(TEllipseTool.Create('ellipse'));
    AddTool(TRecungleTool.Create('rectungle'));
    AddTool(TRoundRectangleTool.Create('roundrect'));
    AddTool(THandTool.Create('hand'));
    AddTool(TZoomTool.Create('zoom'));
    AddTool(TSelectionTool.Create('select'));
end.

