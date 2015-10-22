unit UTools;

{$mode objfpc}{$H+}{$M+}{$R+}

interface

uses
    Classes, SysUtils, UFigures, Forms, Controls, Graphics, Dialogs;
type

    { TTool }

    TTool = class
        procedure StartDrawing (point : TPoint); virtual; abstract;
        constructor Create(name : string);
        procedure AddFigure(point : TPoint); virtual; abstract;
        procedure AdditionalDraw(point : TPoint); virtual;
    end;

    { TPencilTool }

    TPencilTool = class(TTool)
        procedure AddFigure(point : TPoint); override;
        procedure StartDrawing (point : TPoint); override;
    end;

    { TPolylineTool }

    TPolylineTool = class(TTool)
        procedure AddFigure(point : TPoint); override;
        procedure StartDrawing (point : TPoint); override;
        procedure AdditionalDraw (point : TPoint); override;
    end;

    { TEllipseTool }

    TEllipseTool = class(TPolylineTool)
        procedure AddFigure(point : TPoint); override;
    end;

    { TRecnungleTool }

    TRecnungleTool = class(TPolylineTool)
        procedure AddFigure(point : TPoint); override;
    end;

    { TRoundRectangleTool }

    TRoundRectangleTool = class(TRecnungleTool)
        procedure AddFigure(point : TPoint); override;
    end;


var
    Tools: array of TTool;
    ToolsImages: TImageList;
implementation

procedure AddTool(tool : TTool);
begin
    SetLength(Tools, Length(Tools) + 1);
    Tools[High(Tools)] := tool;
end;

{ TPolylineTool }

procedure TPolylineTool.AddFigure(point: TPoint);
begin
    SetLength(Figures, Length(Figures) + 1);
    Figures[High(Figures)] := TPolyline.Create(point);
end;

procedure TPolylineTool.StartDrawing(point: TPoint);
begin
    Figures[High(Figures)].Stranch(point);
end;

procedure TPolylineTool.AdditionalDraw(point: TPoint);
begin
    Figures[High(Figures)].AddPoint(point);
end;

{ TRoundRectangleTool }

procedure TRoundRectangleTool.AddFigure(point: TPoint);
begin
    SetLength(Figures, Length(Figures) + 1);
    Figures[High(Figures)] := TRoundRectangle.Create(point);
end;

{ TRecnungleTool }

procedure TRecnungleTool.AddFigure(point: TPoint);
begin
    SetLength(Figures, Length(Figures) + 1);
    Figures[High(Figures)] := TRectangle.Create(point);
end;


{ TEllipseTool }

procedure TEllipseTool.AddFigure(point: TPoint);
begin
    SetLength(Figures, Length(Figures) + 1);
    Figures[High(Figures)] := TEllipse.Create(point);
end;

{ TPencilTool }

procedure TPencilTool.AddFigure(point: TPoint);
begin
    SetLength(Figures, Length(Figures) + 1);
    Figures[High(Figures)] := TPencil.Create(point);
end;

procedure TPencilTool.StartDrawing(point: TPoint);
begin
    Figures[High(Figures)].AddPoint(point);
end;

{ TTool }

constructor TTool.Create(name: string);
var
    i : TIcon;
begin
    i := TIcon.Create;
    i.LoadFromFile('IconPanel/' + name + '.ico');
    ToolsImages.AddIcon(i);
end;

procedure TTool.AdditionalDraw(point: TPoint);
begin
end;

initialization
    ToolsImages := TImageList.Create(Nil);
    ToolsImages.Height:= 48;
    ToolsImages.Width:= 48;
    AddTool(TPencilTool.Create('pencil'));
    AddTool(TPolylineTool.Create('polyline'));
    AddTool(TEllipseTool.Create('ellipse'));
    AddTool(TRecnungleTool.Create('rectungle'));
    AddTool(TRoundRectangleTool.Create('roundrect'));
end.

