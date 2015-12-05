unit UFigures;

{$mode objfpc}{$H+}

interface

uses
    Classes, SysUtils, Forms, Controls, Graphics, Dialogs, UField, Math;
type
    { TFigures }

    TFigure = class
        private
            FPenWidth: integer;
            FPenStyle: TPenStyle;
            FPenColor, FBrushColor: TColor;
            FSelected : Boolean;
            //procedure SetPenWidth(AValue: integer);
        public
            FPoints: array of TFloatPoint;
            procedure AddPoint(point: TPoint);
            procedure Draw(ACanvas: TCanvas); virtual; abstract;
            procedure Stranch (point: TPoint); virtual; abstract;
            constructor Create(point: TPoint; penColor: TColor);
            function Top() : TPoint;
            function Bottom() : TPoint;
            function MaxX() : extended;
            function MinX() : extended;
            function MaxY() : extended;
            function MinY() : extended;
            procedure CleanSelect();
        published
            //property PenWidth: integer read FPenWidth write SetPenWidth;
            property PenColor: TColor read FPenColor write FPenColor;
            property Selected : Boolean read FSelected write FSelected;
    end;

    { TPencil }

    TPencil = class(TFigure)
        procedure Draw(ACanvas: TCanvas); override;
        {function Top() : TPoint; override;
        function Bottom() : TPoint; override;}
    end;

    { TPolyline }

    TPolyline = class(TPencil)
        private
            procedure Stranch(point1 : TPoint); override;
    end;

    { TEllipse }

    TEllipse = class(TPolyline)
        public
            procedure Draw(ACanvas: TCanvas); override;
        private
            FBrushStyle: TBrushStyle;
        published
            property BrushStyle: TBrushStyle read FBrushStyle write FBrushStyle;
    end;

    { TRectangle }

    TRectangle = class(TPolyline)
        public
            procedure Draw(ACanvas: TCanvas); override;
        private
            FBrushStyle: TBrushStyle;
        published
            property BrushStyle: TBrushStyle read FBrushStyle write FBrushStyle;
    end;

    { TRoundRectangle }

    TRoundRectangle = class(TRectangle)
        private
            FRadius : integer;
        public
            procedure Draw(ACanvas: TCanvas); override;
        published
            property Radius: integer read FRadius write FRadius;
            property BrushStyle: TBrushStyle read FBRushStyle write FBrushStyle;
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

    if (Selected) then begin
        //ACanvas.Brush.Style := bsCross;
        ACanvas.Pen.Style := psDot;
        ACanvas.Pen.Color := clRed;
        ACanvas.RoundRect(ViewPort.WorldToScreen(ViewPort.FloatPoint(MinX(), 0)).x - 5, ViewPort.WorldToScreen(ViewPort.FloatPoint(0, MinY())).y - 5,
                    ViewPort.WorldToScreen(ViewPort.FloatPoint(MaxX(), 0)).x + 5, ViewPort.WorldToScreen(ViewPort.FloatPoint(0, MaxY())).y + 5, 5, 5);
        ACanvas.Pen.Style := psSolid;
        ACanvas.Pen.Color := FPenColor;
    end;
end;

{ TEllipse }

procedure TEllipse.Draw(ACanvas: TCanvas);
begin
    ACanvas.Pen.Color:= FPenColor;
    ACanvas.Ellipse(ViewPort.WorldToScreen(FPoints[0]).x, ViewPort.WorldToScreen(FPoints[0]).y,
                  ViewPort.WorldToScreen(FPoints[1]).x, ViewPort.WorldToScreen(FPoints[1]).y);

    if (Selected) then begin
        //ACanvas.Brush.Style := bsCross;
        ACanvas.Pen.Style := psDot;
        ACanvas.Pen.Color := clRed;
        ACanvas.Ellipse(ViewPort.WorldToScreen(ViewPort.FloatPoint(MinX(), 0)).x - 5, ViewPort.WorldToScreen(ViewPort.FloatPoint(0, MinY())).y - 5,
                    ViewPort.WorldToScreen(ViewPort.FloatPoint(MaxX(), 0)).x + 5, ViewPort.WorldToScreen(ViewPort.FloatPoint(0, MaxY())).y + 5);
        ACanvas.Pen.Style := psSolid;
        ACanvas.Pen.Color := FPenColor;
    end;
end;

{ TRectangle }

procedure TRectangle.Draw(ACanvas: TCanvas);
var
    rect : TRect;
begin
    ACanvas.Rectangle(ViewPort.WorldToScreen(FPoints[0]).x, ViewPort.WorldToScreen(FPoints[0]).y,
                    ViewPort.WorldToScreen(FPoints[1]).x, ViewPort.WorldToScreen(FPoints[1]).y);
    if (Selected) then begin
        //ACanvas.Brush.Style := bsCross;
        ACanvas.Pen.Style := psDot;
        ACanvas.Pen.Color := clRed;
        ACanvas.Rectangle(ViewPort.WorldToScreen(ViewPort.FloatPoint(MinX(), 0)).x - 5, ViewPort.WorldToScreen(ViewPort.FloatPoint(0, MinY())).y - 5,
                    ViewPort.WorldToScreen(ViewPort.FloatPoint(MaxX(), 0)).x + 5, ViewPort.WorldToScreen(ViewPort.FloatPoint(0, MaxY())).y + 5);
        ACanvas.Pen.Style := psSolid;
        ACanvas.Pen.Color := FPenColor;
    end;
end;

{ TPencil }

procedure TPencil.Draw(ACanvas: TCanvas);
var
    i: integer;
begin
    ACanvas.Pen.Color:= FPenColor;
    ACanvas.MoveTo(ViewPort.WorldToScreen(FPoints[0]));

    for i := 1 to High(FPoints) do begin
        ACanvas.Brush.Style:= bsClear;
        ACanvas.LineTo(ViewPort.WorldToScreen(FPoints[i]));
    end;
    if (Selected) then begin
        ACanvas.Pen.Style := psDot;
        ACanvas.Pen.Color := clRed;
        ACanvas.Rectangle(ViewPort.WorldToScreen(ViewPort.FloatPoint(MinX(), 0)).X - 5, ViewPort.WorldToScreen(ViewPort.FloatPoint(0, MinY())).Y - 5,
                          ViewPort.WorldToScreen(ViewPort.FloatPoint(MaxX(), 0)).X + 5, ViewPort.WorldToScreen(ViewPort.FloatPoint(0, MaxY())).Y + 5);
        ACanvas.Pen.Style := psSolid;
        ACanvas.Pen.Color := FPenColor;
    end;
end;

{function TPencil.Top: TPoint;
begin
end;    }

{function TPencil.Bottom: TPoint;
begin
end;}

{ TPolyLine }

procedure TPolyline.Stranch(point1 : TPoint);
begin
   FPoints[1] := ViewPort.ScreenToWorld(point1);
end;

{ TFigure }

constructor TFigure.Create(point: TPoint; penColor: TColor);
begin
    AddPoint(Point);
    AddPoint(Point);
    Selected:= false;
    FPenColor:= penColor;
end;

function TFigure.Top: TPoint;
begin
     result := ViewPort.WorldToScreen(FPoints[0]);
end;

function TFigure.Bottom: TPoint;
begin
    result := ViewPort.WorldToScreen(FPoints[1]);
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

procedure TFigure.CleanSelect;
begin
    Selected := false;
end;


{ TPencil }

{procedure TFigure.SetPenWidth(AValue: integer);
begin
  if FPenWidth=AValue then Exit;
  FPenWidth:=AValue;
end;                 }

procedure TFigure.AddPoint(point : TPoint);
begin
    SetLength(FPoints, Length(FPoints) + 1);
    FPoints[High(FPoints)] := ViewPort.ScreenToWorld(point);
end;


end.

