unit UFigures;

{$mode objfpc}{$H+}

interface

uses
    Classes, SysUtils, Forms, Controls, Graphics, Dialogs, UField, Math;
type
    { TFigures }

    TFigure = class(TPersistent)
        private
            FPenWidth : integer;
            FPenStyle : TPenStyle;
            FPenColor : TColor;
            FSelected : Boolean;
        public
            FPoints: array of TFloatPoint;
            procedure AddPoint(point: TPoint);
            procedure Draw(ACanvas: TCanvas); virtual; abstract;
            procedure Stranch (point: TPoint); virtual; abstract;
            constructor Create();
            constructor Create(point: TPoint);
            function Top() : TPoint;
            function Bottom() : TPoint;
            function MaxX() : extended;
            function MinX() : extended;
            function MaxY() : extended;
            function MinY() : extended;
            procedure CleanSelect();
        published
            property Width: integer read FPenWidth write FPenWidth;
            property PenColor : TColor read FPenColor write FPenColor;
            property PenStyle : TPenStyle read FPenStyle write FPenStyle;
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
            FBrushStyle : TBrushStyle;
            FBrushColor : TColor;
        published
            property BrushStyle : TBrushStyle read FBrushStyle write FBrushStyle;
            property BrushColor : TColor read FBrushColor write FBrushColor;
    end;

    { TRectangle }

    TRectangle = class(TPolyline)
        public
            procedure Draw(ACanvas: TCanvas); override;
        private
            FBrushStyle : TBrushStyle;
            FBrushColor : TColor;
        published
            property BrushStyle : TBrushStyle read FBrushStyle write FBrushStyle;
            property BrushColor : TColor read FBrushColor write FBrushColor;
    end;

    { TRoundRectangle }

    TRoundRectangle = class(TRectangle)
        private
            FRadius : integer;
        public
            procedure Draw(ACanvas: TCanvas); override;
        published
            property Radius : integer read FRadius write FRadius;
    end;

var
    Figures: array of TFigure;

implementation

{ TRoundRectangle }

procedure TRoundRectangle.Draw(ACanvas: TCanvas);

begin
    ACanvas.Pen.Color:= FPenColor;
    ACanvas.Pen.Style := FPenStyle;
    ACanvas.Brush.Style := FBrushStyle;
    ACanvas.Brush.Color := FBrushColor;
    ACanvas.Pen.Width := FPenWidth;
    ACanvas.RoundRect(ViewPort.WorldToScreen(FPoints[0]).x, ViewPort.WorldToScreen(FPoints[0]).y,
                    ViewPort.WorldToScreen(FPoints[1]).x, ViewPort.WorldToScreen(FPoints[1]).y, FRadius, FRadius);

    if (Selected) then begin
        //ACanvas.Brush.Style := bsCross;
        ACanvas.Brush.Style := bsClear;
        ACanvas.Pen.Style := psDot;
        ACanvas.Pen.Color := clRed;
        ACanvas.RoundRect(ViewPort.WorldToScreen(ViewPort.FloatPoint(MinX(), 0)).x - 5, ViewPort.WorldToScreen(ViewPort.FloatPoint(0, MinY())).y - 5,
                    ViewPort.WorldToScreen(ViewPort.FloatPoint(MaxX(), 0)).x + 5, ViewPort.WorldToScreen(ViewPort.FloatPoint(0, MaxY())).y + 5, FRadius, FRadius);
        ACanvas.Pen.Style := FPenStyle;
        ACanvas.Pen.Color := FPenColor;
        ACanvas.Brush.Style := FBrushStyle;
        ACanvas.Brush.Color := FBrushColor;
    end;
end;

{ TEllipse }

procedure TEllipse.Draw(ACanvas: TCanvas);
begin
    ACanvas.Pen.Color:= FPenColor;
    ACanvas.Pen.Style := FPenStyle;
    ACanvas.Brush.Style := FBrushStyle;
    ACanvas.Brush.Color := FBrushColor;
    ACanvas.Pen.Width := FPenWidth;
    ACanvas.Ellipse(ViewPort.WorldToScreen(FPoints[0]).x, ViewPort.WorldToScreen(FPoints[0]).y,
                  ViewPort.WorldToScreen(FPoints[1]).x, ViewPort.WorldToScreen(FPoints[1]).y);

    if (Selected) then begin
        //ACanvas.Brush.Style := bsCross;
        ACanvas.Brush.Style := bsClear;
        ACanvas.Pen.Style := psDot;
        ACanvas.Pen.Color := clRed;
        ACanvas.Ellipse(ViewPort.WorldToScreen(ViewPort.FloatPoint(MinX(), 0)).x - 5, ViewPort.WorldToScreen(ViewPort.FloatPoint(0, MinY())).y - 5,
                    ViewPort.WorldToScreen(ViewPort.FloatPoint(MaxX(), 0)).x + 5, ViewPort.WorldToScreen(ViewPort.FloatPoint(0, MaxY())).y + 5);
         ACanvas.Pen.Style := FPenStyle;
        ACanvas.Pen.Color := FPenColor;
        ACanvas.Brush.Style := FBrushStyle;
        ACanvas.Brush.Color := FBrushColor;
    end;
end;

{ TRectangle }

procedure TRectangle.Draw(ACanvas: TCanvas);
var
    rect : TRect;
begin
    ACanvas.Pen.Style := FPenStyle;
    ACanvas.Brush.Style := FBrushStyle;
    ACanvas.Brush.Color := FBrushColor;
    ACanvas.Pen.Color := FPenColor;
    ACanvas.Pen.Width := FPenWidth;
    ACanvas.Rectangle(ViewPort.WorldToScreen(FPoints[0]).x, ViewPort.WorldToScreen(FPoints[0]).y,
                    ViewPort.WorldToScreen(FPoints[1]).x, ViewPort.WorldToScreen(FPoints[1]).y);
    if (Selected) then begin
        //ACanvas.Brush.Style := bsCross;
        ACanvas.Brush.Style := bsClear;
        ACanvas.Pen.Style := psDot;
        ACanvas.Pen.Color := clRed;
        ACanvas.Rectangle(ViewPort.WorldToScreen(ViewPort.FloatPoint(MinX(), 0)).x - 5, ViewPort.WorldToScreen(ViewPort.FloatPoint(0, MinY())).y - 5,
                    ViewPort.WorldToScreen(ViewPort.FloatPoint(MaxX(), 0)).x + 5, ViewPort.WorldToScreen(ViewPort.FloatPoint(0, MaxY())).y + 5);
        ACanvas.Pen.Style := FPenStyle;
        ACanvas.Pen.Color := FPenColor;
        ACanvas.Brush.Style := FBrushStyle;
        ACanvas.Brush.Color := FBrushColor;
    end;
end;

{ TPencil }

procedure TPencil.Draw(ACanvas: TCanvas);
var
    i: integer;
begin
    ACanvas.Pen.Color:= FPenColor;
    ACanvas.MoveTo(ViewPort.WorldToScreen(FPoints[0]));
    ACanvas.Pen.Style := FPenStyle;
    ACanvas.Pen.Width := FPenWidth;

    for i := 1 to High(FPoints) do begin
        ACanvas.Brush.Style:= bsClear;
        ACanvas.LineTo(ViewPort.WorldToScreen(FPoints[i]));
    end;
    if (Selected) then begin
        ACanvas.Brush.Style := bsClear;
        ACanvas.Pen.Style := psDot;
        ACanvas.Pen.Color := clRed;
        ACanvas.Rectangle(ViewPort.WorldToScreen(ViewPort.FloatPoint(MinX(), 0)).X - 5, ViewPort.WorldToScreen(ViewPort.FloatPoint(0, MinY())).Y - 5,
                          ViewPort.WorldToScreen(ViewPort.FloatPoint(MaxX(), 0)).X + 5, ViewPort.WorldToScreen(ViewPort.FloatPoint(0, MaxY())).Y + 5);
        ACanvas.Pen.Style := FPenStyle;
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

constructor TFigure.Create(point: TPoint);
begin
    AddPoint(Point);
    AddPoint(Point);
    Selected := false;
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

procedure TFigure.AddPoint(point : TPoint);
begin
    SetLength(FPoints, Length(FPoints) + 1);
    FPoints[High(FPoints)] := ViewPort.ScreenToWorld(point);
end;

constructor TFigure.Create;
begin
    SetLength(FPoints, 0);
    Selected := false;
end;

initialization

    RegisterClass(TEllipse);
    RegisterClass(TRectangle);
    RegisterClass(TRoundRectangle);
    RegisterClass(TPencil);
    RegisterClass(TPolyline);

end.

