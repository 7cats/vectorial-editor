unit UField;

{$mode objfpc}{$H+}

interface

uses
    Classes, SysUtils, Math;

type

    TFloatPoint = record
        x, y: extended;
    end;

    { TViewPort }

    TViewPort = class
        FCenter: TFloatPoint;
        FDisplacement, FZoom, FLeftBoarder, FRightBoarder, FBottomBoarder, FTopBoarder: extended;
        FPaintBoxCenterX, FPaintBoxCenterY: double;
        constructor Create(width, height: double);
        function WorldToScreen(fpoint: TFloatPoint): TPoint;
        function ScreenToWorld(point: TPoint): TFloatPoint;
        procedure AddDisplacement(dispacementX, dispacementY: extended);
        procedure CalcAndAddDisplacement(oldPoint, newPoint: TPoint);
        procedure PaintBoxResize(BoxCentreX, BoxCentreY: double);
        procedure ShowAll (w,h:integer);
    end;

var
    ViewPort: TViewPort;

implementation


{ TViewPort }

function TViewPort.WorldToScreen(fpoint: TFloatPoint): TPoint;
begin
    WorldToScreen.X := round(FPaintBoxCenterX + (fpoint.X - FCenter.X) * FZoom);
    WorldToScreen.Y := round(FPaintBoxCenterY + (fpoint.Y - FCenter.Y) * FZoom);
end;

function TViewPort.ScreenToWorld(point: TPoint): TFloatPoint;
begin
    ScreenToWorld.X := FCenter.X + (point.X - FPaintBoxCenterX) / FZoom;
    ScreenToWorld.Y := FCenter.Y + (point.Y - FPaintBoxCenterY) / FZoom;
end;

constructor TViewPort.Create(width, height: double);
begin
    FCenter.x:= 0;
    FCenter.y := 0;
    FZoom := 1;
    FPaintBoxCenterY:= height;
    FPaintBoxCenterX:= width;
    FDisplacement:= 0;
end;

procedure TViewPort.AddDisplacement(dispacementX, dispacementY: extended);
begin
    FCenter.x -= dispacementX;
    FCenter.y -= dispacementY;
end;

procedure TViewPort.CalcAndAddDisplacement(oldPoint, newPoint: TPoint);
begin
    AddDisplacement(newPoint.x - oldPoint.x, newPoint.y - oldPoint.y);
end;

procedure TViewPort.PaintBoxResize(boxCentreX, boxCentreY: double);
begin
    FCenter.x += boxCentreX - FPaintBoxCenterX;
    FCenter.y += boxCentreY - FPaintBoxCenterY;
    FPaintBoxCenterX := boxCentreX;
    FPaintBoxCenterY := boxCentreY;
end;

procedure TViewPort.ShowAll(w, h: integer);
begin
    ViewPort.FCenter.x := (FRightBoarder + FLeftBoarder) / 2;
    ViewPort.FCenter.y := (FTopBoarder + FBottomBoarder) / 2;
    ViewPort.FZoom := min(h / (FBottomBoarder  - FTopBoarder + 1),
                          w / (FRightBoarder - FLeftBoarder + 1))
end;

end.

