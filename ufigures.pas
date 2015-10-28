unit UFigures;

{$mode objfpc}{$H+}

interface

uses
    Classes, SysUtils, Forms, Controls, Graphics, Dialogs, UField;
type
    { TFigures }

    TFigure = class
        FPoints: array of TFloatPoint;
        procedure AddPoint(point: TPoint);
        procedure Draw(ACanvas: TCanvas); virtual; abstract;
        procedure Stranch (point: TPoint); virtual; abstract;
        constructor Create(point: TPoint);
        function MaxX() : extended;
        function MinX() : extended;
        function MaxY() : extended;
        function MinY() : extended;
    end;

    { TPencil }

    TPencil = class(TFigure)
        procedure Draw(ACanvas: TCanvas); override;
    end;

    { TPolyline }

    TPolyline = class(TPencil)
        procedure Stranch(point : TPoint); override;
    end;

    { TEllipse }

    TEllipse = class(TPolyline)
        procedure Draw(ACanvas: TCanvas); override;
    end;

    { TRectangle }

    TRectangle = class(TPolyline)
        procedure Draw(ACanvas: TCanvas); override;
    end;

    { TRoundRectangle }

    TRoundRectangle = class(TRectangle)
        procedure Draw(ACanvas: TCanvas); override;
    end;

var
    Figures: array of TFigure;

implementation

{ TRoundRectangle }

procedure TRoundRectangle.Draw(ACanvas: TCanvas);
begin
    ACanvas.RoundRect(ViewPort.WorldToScreen(FPoints[0]).x, ViewPort.WorldToScreen(FPoints[0]).y,
                    ViewPort.WorldToScreen(FPoints[1]).x, ViewPort.WorldToScreen(FPoints[1]).y, 20, 20);
end;

{ TEllipse }

procedure TEllipse.Draw(ACanvas: TCanvas);
begin
    ACanvas.Ellipse(ViewPort.WorldToScreen(FPoints[0]).x, ViewPort.WorldToScreen(FPoints[0]).y,
                  ViewPort.WorldToScreen(FPoints[1]).x, ViewPort.WorldToScreen(FPoints[1]).y);
end;

{ TRectangle }

procedure TRectangle.Draw(ACanvas: TCanvas);
begin
    ACanvas.Rectangle(ViewPort.WorldToScreen(FPoints[0]).x, ViewPort.WorldToScreen(FPoints[0]).y,
                    ViewPort.WorldToScreen(FPoints[1]).x, ViewPort.WorldToScreen(FPoints[1]).y);
end;

{ TPencil }

procedure TPencil.Draw(ACanvas: TCanvas);
var
    i: integer;
begin
    ACanvas.MoveTo(ViewPort.WorldToScreen(FPoints[0]));
    for i := 1 to High(FPoints) do
        ACanvas.LineTo(ViewPort.WorldToScreen(FPoints[i]));
end;

{ TPolyLine }

procedure TPolyline.Stranch(point: TPoint);
begin
   FPoints[1] := ViewPort.ScreenToWorld(point);
end;

{ TFigure }

constructor TFigure.Create(point: TPoint);
begin
    AddPoint(Point);
    AddPoint(Point);
end;

function TFigure.MaxX: extended;
var
    tmp: extended;
    i: integer;
begin
    tmp := FPoints[0].x;
    for i := 1 to High(FPoints) do
        if (tmp < FPoints[i].x) then
           tmp := FPoints[i].x;
    result := tmp;
end;

function TFigure.MinX: extended;
var
    tmp: extended;
    i: integer;
begin
    tmp := FPoints[0].x;
    for i := 1 to High(FPoints) do
        if (tmp > FPoints[i].x) then
           tmp := FPoints[i].x;
    result := tmp;
end;

function TFigure.MaxY: extended;
var
    tmp: extended;
    i: integer;
begin
    tmp := FPoints[0].y;
    for i := 1 to High(FPoints) do
        if (tmp < FPoints[i].y) then
           tmp := FPoints[i].y;
    result := tmp;
end;

function TFigure.MinY: extended;
var
    tmp: extended;
    i: integer;
begin
    tmp := FPoints[0].y;
    for i := 1 to High(FPoints) do
        if (tmp > FPoints[i].y) then
           tmp := FPoints[i].y;
    result := tmp;
end;


{ TPencil }

procedure TFigure.AddPoint(point : TPoint);
begin
    SetLength(FPoints, Length(FPoints) + 1);
    FPoints[High(FPoints)] := ViewPort.ScreenToWorld(point);
end;


end.

