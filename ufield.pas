unit UField;

{$mode objfpc}{$H+}

interface

uses
    Classes, SysUtils, Math;

type

    { TFloatPoint }

    TFloatPoint = record
        x, y: extended;
    end;

    { TViewPort }

    TViewPort = class
        FCenter, FLeftTop, FRightBottom: TFloatPoint;
        FPaintBoxSize: TPoint;
        FDisplacement, FZoom: extended;
        constructor Create(width, height: integer);
        function WorldToScreen(fpoint: TFloatPoint): TPoint;
        function ScreenToWorld(point: TPoint): TFloatPoint;
        procedure AddDisplacement(dispacementX, dispacementY: extended);
        procedure CalcAndAddDisplacement(oldPoint, newPoint: TPoint);
        procedure PaintBoxResize(PbWidth, PbHeight: integer);
        function FloatPoint(x, y : extended) : TFloatPoint;
        procedure ShowAll ();
    end;

var
    ViewPort: TViewPort;

implementation

{ TViewPort }

function TViewPort.FloatPoint(x, y : extended) : TFloatPoint;
begin
    result.X := x;
    result.Y := y;
end;

function TViewPort.WorldToScreen(fpoint: TFloatPoint): TPoint;
begin
    WorldToScreen.X := round(FPaintBoxSize.X / 2 + (fpoint.X - FCenter.X) * FZoom);
    WorldToScreen.Y := round(FPaintBoxSize.Y / 2 + (fpoint.Y - FCenter.Y) * FZoom);
end;

function TViewPort.ScreenToWorld(point: TPoint): TFloatPoint;
begin
    ScreenToWorld.X := FCenter.X + (point.X - FPaintBoxSize.X / 2) / FZoom;
    ScreenToWorld.Y := FCenter.Y + (point.Y - FPaintBoxSize.Y / 2) / FZoom;
end;

constructor TViewPort.Create(width, height: integer);
begin
    FCenter.x:= 0;
    FCenter.y := 0;
    FZoom := 1;
    FPaintBoxSize.Y:= height;
    FPaintBoxSize.X:= width;
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

procedure TViewPort.PaintBoxResize(PbWidth, PbHeight: integer);
begin
    FCenter.x += PbWidth / 2  - FPaintBoxSize.X / 2;
    FCenter.y += PbHeight / 2 - FPaintBoxSize.Y / 2;
    FPaintBoxSize.X := PbWidth;
    FPaintBoxSize.Y := PbHeight;
end;

procedure TViewPort.ShowAll();
begin
    ViewPort.FCenter.x := (FRightBottom.X + FLeftTop.X) / 2;
    ViewPort.FCenter.y := (FRightBottom.Y + FLeftTop.Y) / 2 ;
    ViewPort.FZoom := min(FPaintBoxSize.Y /  (FRightBottom.Y - FLeftTop.Y),
                          FPaintBoxSize.X /  (FRightBottom.X - FLeftTop.X) );
end;

end.


