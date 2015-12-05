unit ULocation;

{$mode objfpc}{$H+}

interface

uses
    Classes, SysUtils, UFigures, LCLIntf, LCLType, UField;

type

    { TLocation }

    TLocation = class
//       function dskfjsd;
        //FIndexsSelectedFigures : array of integer;
        procedure CheckIntersectionAllFigures (ClickReg : TRect);
        procedure CheckIntersectionRectungle (ClickReg : TRect; index : integer);
        procedure CheckIntersectionRoundRectungle (ClickReg : TRect; index : integer);
        procedure CheckIntersectionEllipce (ClickReg : TRect; index : integer);
        procedure CheckIntersectionPolyline (ClickReg : TRect; index : integer);
        procedure Select(index : integer);
        procedure Deselect(index : integer);
        procedure ChangePos(dX, dY : integer);
        constructor Create();
    end;

var
    Location : TLocation;
implementation

{ TLocation }

procedure TLocation.CheckIntersectionAllFigures(ClickReg: TRect);
var
    i : integer;
begin
    for i := 0 to High(Figures) - 1 do begin
        case (Figures[i].ClassName) of
             'TEllipse': CheckIntersectionEllipce(ClickReg, i);
             'TPencil' : CheckIntersectionPolyline(ClickReg, i);
             'TPolyline' : CheckIntersectionPolyline(ClickReg, i);
             'TRectangle' : CheckIntersectionRectungle(ClickReg, i);
             'TRoundRectangle' : CheckIntersectionRoundRectungle(ClickReg, i);
         else
             Raise Exception.Create('Type of figure isn`t denied');
        end;

    end;

end;

procedure TLocation.CheckIntersectionRectungle(ClickReg: TRect; index: integer);
begin
    if RectInRegion(CreateRectRgn(ViewPort.WorldToScreen(Figures[index].FPoints[0]).X, ViewPort.WorldToScreen(Figures[index].FPoints[0]).Y,
                                  ViewPort.WorldToScreen(Figures[index].FPoints[1]).X, ViewPort.WorldToScreen(Figures[index].FPoints[1]).Y),
                                                                                       ClickReg) then begin
        Select(index);
    end
    else begin
        Deselect(index);
    end;
end;

procedure TLocation.CheckIntersectionRoundRectungle(ClickReg: TRect;
  index: integer);
begin
    if   RectInRegion(CreateRoundRectRgn(ViewPort.WorldToScreen(Figures[index].FPoints[0]).X, ViewPort.WorldToScreen(Figures[index].FPoints[0]).Y,
                                  ViewPort.WorldToScreen(Figures[index].FPoints[1]).X, ViewPort.WorldToScreen(Figures[index].FPoints[1]).Y, 5, 5), ClickReg) then begin
        Select(index);
    end
    else begin
        Deselect(index);
    end;
end;

procedure TLocation.CheckIntersectionEllipce(ClickReg: TRect; index: integer);
begin
    if RectInRegion(CreateEllipticRgn(ViewPort.WorldToScreen(Figures[index].FPoints[0]).X, ViewPort.WorldToScreen(Figures[index].FPoints[0]).Y,
                                      ViewPort.WorldToScreen(Figures[index].FPoints[1]).X, ViewPort.WorldToScreen(Figures[index].FPoints[1]).Y), ClickReg) then begin
        Select(index);
    end
    else begin
        Deselect(index);
    end;
end;

procedure TLocation.CheckIntersectionPolyline(ClickReg: TRect; index: integer);
var
    i : integer;
    fPoint, sPoint : TPoint;
begin
    if RectInRegion(CreateRectRgn(ViewPort.WorldToScreen(ViewPort.FloatPoint(Figures[index].MinX(), 0)).X, ViewPort.WorldToScreen(ViewPort.FloatPoint(0, Figures[index].MinY())).Y,
                                       ViewPort.WorldToScreen(ViewPort.FloatPoint(Figures[index].MaxX(), 0)).X, ViewPort.WorldToScreen(ViewPort.FloatPoint(0, Figures[index].MaxY())).Y), ClickReg) then begin
            Select(index);
    end
    else begin
        Deselect(index);
    end;
end;

procedure TLocation.Select(index: integer);
begin
     Figures[index].Selected := true;
end;

procedure TLocation.Deselect(index: integer);
begin
    Figures[index].Selected := false;
end;

procedure TLocation.ChangePos(dX, dY: integer);
var
    i, j : integer;
begin
    for i := 0 to High(Figures) do begin
        if (Figures[i].Selected) then begin
            for j := 0 to High(Figures[i].FPoints) do begin
                Figures[i].FPoints[j].X -= dX;
                Figures[i].FPoints[j].Y -= dY;
            end;
        end;
    end;
end;

constructor TLocation.Create;
begin
end;

initialization
    Location := TLocation.Create;

end.


