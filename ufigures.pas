unit UFigures;

{$mode objfpc}{$H+}

interface

uses
    Classes, SysUtils, Forms, Controls, Graphics, Dialogs, UField, Math;
type
    { TFigures }

    TFigure = class
        FPenColor: TColor;
        FPoints: array of TFloatPoint;
        procedure AddPoint(point: TPoint);
        procedure Draw(ACanvas: TCanvas); virtual; abstract;
        procedure Stranch (point: TPoint); virtual; abstract;
        constructor Create(point: TPoint; penColor: TColor);
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
    ACanvas.Pen.Color:= FPenColor;
    ACanvas.RoundRect(ViewPort.WorldToScreen(FPoints[0]).x, ViewPort.WorldToScreen(FPoints[0]).y,
                    ViewPort.WorldToScreen(FPoints[1]).x, ViewPort.WorldToScreen(FPoints[1]).y, 20, 20);
end;

{ TEllipse }

procedure TEllipse.Draw(ACanvas: TCanvas);
begin
    ACanvas.Pen.Color:= FPenColor;
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
    ACanvas.Pen.Color:= FPenColor;
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

constructor TFigure.Create(point: TPoint; penColor: TColor);
begin
    AddPoint(Point);
    AddPoint(Point);
    FPenColor:= penColor;
end;

function TFigure.MaxX: extended;
var
    i: integer;
begin
    result := FPoints[0].x;
    for i := 1 to High(FPoints) do
        result := max(result,  FPoints[i].x);
end;

function TFigure.MinX: extended;
var
    i: integer;
begin
    result := FPoints[0].x;
    for i := 1 to High(FPoints) do
        result := min(result, FPoints[i].x);
end;

function TFigure.MaxY: extended;
var
    i: integer;
begin
    result := FPoints[0].y;
    for i := 1 to High(FPoints) do
        result := max(result, FPoints[i].y);
end;

function TFigure.MinY: extended;
var
    i: integer;
begin
    result := FPoints[0].y;
    for i := 1 to High(FPoints) do
        result := min(result, FPoints[i].y);
end;


{ TPencil }

procedure TFigure.AddPoint(point : TPoint);
begin
    SetLength(FPoints, Length(FPoints) + 1);
    FPoints[High(FPoints)] := ViewPort.ScreenToWorld(point);
end;


end.

