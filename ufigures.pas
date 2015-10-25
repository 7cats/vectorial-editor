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
        procedure Stranch (point: TPoint) virtual; abstract;
        constructor Create(point: TPoint);
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
    ACanvas.RoundRect(round(FPoints[0].x), round(FPoints[0].y), round(FPoints[1].x), round(FPoints[1].y), 20, 20);
end;

{ TEllipse }

procedure TEllipse.Draw(ACanvas: TCanvas);
begin
    ACanvas.Ellipse(round(FPoints[0].x), round(FPoints[0].y), round(FPoints[1].x), round(FPoints[1].y));
end;

{ TRectangle }

procedure TRectangle.Draw(ACanvas: TCanvas);
begin
    ACanvas.Rectangle(round(FPoints[0].x), round(FPoints[0].y), round(FPoints[1].x), round(FPoints[1].y));
end;

{ TPencil }

procedure TPencil.Draw(ACanvas: TCanvas);
var
    i: integer;
begin
    ACanvas.MoveTo(round(FPoints[0].x), round(FPoints[0].y));
    for i := 1 to High(FPoints) do
        ACanvas.LineTo(round(FPoints[i].x), round(FPoints[i].y));
end;

{ TPolyLine }

procedure TPolyline.Stranch(point: TPoint);
begin
    FPoints[1].x := trunc(point.x);
    FPoints[1].y := trunc(point.y);
end;

{ TFigures }


constructor TFigure.Create(point: TPoint);
begin
    AddPoint(Point);
    AddPoint(Point);
end;

{ TPencil }

procedure TFigure.AddPoint(point : TPoint);
begin
    SetLength(FPoints, Length(FPoints) + 1);
    FPoints[High(FPoints)].x := trunc(point.x);
    FPoints[High(FPoints)].y := trunc(point.y);
end;


end.

