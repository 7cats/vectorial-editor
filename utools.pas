unit UTools;

{$mode objfpc}{$H+}{$M+}{$R+}

interface

uses
    Classes, SysUtils, UFigures, Forms, Controls, Graphics, Dialogs,
    UField, Math;

type

    { TTool }

    TTool = class
        procedure ToolMove(point : TPoint); virtual; abstract;
        constructor Create(name : string);
        procedure MakeActive(point : TPoint); virtual; abstract;
        procedure AdditionalAction(point : TPoint); virtual;
        procedure SpecificAction(point : TPoint; w, h: integer); virtual;
        public
            FName: string;
    end;

    { TPencilTool }

    TPencilTool = class(TTool)
        procedure MakeActive(point : TPoint); override;
        procedure ToolMove (point : TPoint); override;
    end;

    { TPolylineTool }

    TPolylineTool = class(TTool)
        procedure MakeActive(point : TPoint); override;
        procedure ToolMove (point : TPoint); override;
        procedure AdditionalAction (point : TPoint); override;
    end;

    { TEllipseTool }

    TEllipseTool = class(TPolylineTool)
        procedure MakeActive(point : TPoint); override;
    end;

    { TRecnungleTool }

    TRecnungleTool = class(TPolylineTool)
        procedure MakeActive(point : TPoint); override;
    end;

    { TRoundRectangleTool }

    TRoundRectangleTool = class(TRecnungleTool)
        procedure MakeActive(point : TPoint); override;
    end;

    {THandTool}

    THandTool = class(TTool)
        private
            FBeginCoordinate: TFloatPoint;
            procedure MakeActive(point : TPoint); override;
            procedure ToolMove(point : TPoint); override;
    end;

    { TZoomTool }

    TZoomTool = class(TTool)
        procedure MakeActive(point : TPoint); override;
        procedure ToolMove(point : TPoint); override;
        procedure AdditionalAction(point: TPoint); override;
        procedure SpecificAction(point : TPoint; w, h: integer); override;
        procedure ClickZoom ();
        procedure RectClick (w, h: integer);
        private
            FBeginZoomRect, FEndZoomRect: TFloatPoint;
    end;

var
    Tools: array of TTool;
    ToolsImages: TImageList;

implementation

procedure AddTool(tool : TTool);
begin
    SetLength(Tools, Length(Tools) + 1);
    Tools[High(Tools)] := tool;
end;

{ TZoomTool }

procedure TZoomTool.MakeActive(point: TPoint);
begin
    SetLength(Figures, Length(Figures) + 1);
    Figures[High(Figures)] := TRectangle.Create(point);
    FBeginZoomRect := ViewPort.ScreenToWorld(point);
end;

procedure TZoomTool.ToolMove(point: TPoint);
begin
    Figures[High(Figures)].Stranch(point);
end;

procedure TZoomTool.AdditionalAction(point: TPoint);
begin
    ViewPort.FZoom := max(0.01, ViewPort.FZoom - 0.09);
end;

procedure TZoomTool.SpecificAction(point: TPoint; w, h: integer);
begin
    FEndZoomRect := ViewPort.ScreenToWorld(point);
    SetLength(Figures, Length(Figures) - 1);

    if (sqrt(sqr(FEndZoomRect.X - FBeginZoomRect.X) + sqr(FEndZoomRect.Y - FBeginZoomRect.Y)) < 10) then
        ClickZoom()
    else
        RectClick(w, h);
end;

procedure TZoomTool.ClickZoom;
begin
    ViewPort.AddDisplacement(ViewPort.FCenter.X - FBeginZoomRect.X,
                             ViewPort.FCenter.Y - FBeginZoomRect.Y);
    ViewPort.FZoom += 0.5;
end;

procedure TZoomTool.RectClick(w, h: integer);
begin
    ViewPort.AddDisplacement(ViewPort.FCenter.X - (FBeginZoomRect.X + FEndZoomRect.X) / 2,
                             ViewPort.FCenter.Y - (FBeginZoomRect.Y + FEndZoomRect.Y) / 2);
    if (w / abs(FEndZoomRect.X - FBeginZoomRect.X) > h / abs(FEndZoomRect.Y - FBeginZoomRect.Y)) then
        ViewPort.FZoom *= h /  abs(FEndZoomRect.Y - FBeginZoomRect.Y)
    else
        ViewPort.FZoom *= w / abs(FEndZoomRect.X - FBeginZoomRect.X)
end;

{ THandTool }

procedure THandTool.MakeActive(point: TPoint);
begin
    FBeginCoordinate:= ViewPort.ScreenToWorld(point);
end;

procedure THandTool.ToolMove(point: TPoint);
begin
    ViewPort.AddDisplacement(ViewPort.ScreenToWorld(point).x - FBeginCoordinate.x,
                             ViewPort.ScreenToWorld(point).y - FBeginCoordinate.y);
    FBeginCoordinate:= ViewPort.ScreenToWorld(point);
end;

{ TPolylineTool }

procedure TPolylineTool.MakeActive(point: TPoint);
begin
    SetLength(Figures, Length(Figures) + 1);
    Figures[High(Figures)] := TPolyline.Create(point);
end;

procedure TPolylineTool.ToolMove(point: TPoint);
begin
    Figures[High(Figures)].Stranch(point);
end;

procedure TPolylineTool.AdditionalAction(point: TPoint);
begin
    Figures[High(Figures)].AddPoint(point);
end;

{ TRoundRectangleTool }

procedure TRoundRectangleTool.MakeActive(point: TPoint);
begin
    SetLength(Figures, Length(Figures) + 1);
    Figures[High(Figures)] := TRoundRectangle.Create(point);
end;

{ TRecnungleTool }

procedure TRecnungleTool.MakeActive(point: TPoint);
begin
    SetLength(Figures, Length(Figures) + 1);
    Figures[High(Figures)] := TRectangle.Create(point);
end;


{ TEllipseTool }

procedure TEllipseTool.MakeActive(point: TPoint);
begin
    SetLength(Figures, Length(Figures) + 1);
    Figures[High(Figures)] := TEllipse.Create(point);
end;

{ TPencilTool }

procedure TPencilTool.MakeActive(point: TPoint);
begin
    SetLength(Figures, Length(Figures) + 1);
    Figures[High(Figures)] := TPencil.Create(point);
end;

procedure TPencilTool.ToolMove(point: TPoint);
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

procedure TTool.AdditionalAction(point: TPoint);
begin
end;

procedure TTool.SpecificAction(point: TPoint; w, h: integer);
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

