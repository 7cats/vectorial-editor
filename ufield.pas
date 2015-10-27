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
    WorldToScreen.X := round((fpoint.x - (FPaintBoxCenterX - FCenter.x)) / FZoom);
    WorldToScreen.Y := round((fpoint.y - (FPaintBoxCenterY - FCenter.y)) / FZoom);
end;

function TViewPort.ScreenToWorld(point: TPoint): TFloatPoint;
begin
    ScreenToWorld.x := point.x * FZoom + (FPaintBoxCenterX - FCenter.x);
    ScreenToWorld.y := point.y * FZoom + (FPaintBoxCenterY - FCenter.y);
end;

constructor TViewPort.Create(width, height: double);
begin
    FCenter.x := 0;
    FCenter.y := 0;
    FZoom := 1;
    FPaintBoxCenterY:= height;
    FPaintBoxCenterX:= width;
    FDisplacement:= 0;
end;

procedure TViewPort.AddDisplacement(dispacementX, dispacementY: extended);
begin
    FCenter.x += dispacementX;
    FCenter.y += dispacementY;
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

