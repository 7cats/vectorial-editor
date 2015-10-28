unit UField;

{$mode objfpc}{$H+}

interface

uses
    Classes, SysUtils;

type

    TFloatPoint = record
        x, y: extended;
    end;

    { TViewPort }

    TViewPort = class
        FCenter: TFloatPoint;
        FDisplacement, FZoom: extended;
        FPaintBoxCenterX, FPaintBoxCenterY: double;
        constructor Create(width, height: double);
        function WorldToScreen(fpoint: TFloatPoint): TPoint;
        function ScreenToWorld(point: TPoint): TFloatPoint;
        procedure AddDisplacement(dispacementX, dispacementY: extended);
        procedure CalcAndAddDisplacement(oldPoint, newPoint: TPoint);
        procedure PaintBoxResize(BoxCentreX, BoxCentreY: double);
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

end.

