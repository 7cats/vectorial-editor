unit UTools;

{$mode objfpc}{$H+}{$M+}{$R+}

interface

uses
    Classes, SysUtils, UFigures, Forms, Controls, Graphics, Dialogs;
type

    { TTool }

    TTool = class
        procedure ToolMove(point : TPoint); virtual; abstract;
        constructor Create(name : string);
        procedure MakeActive(point : TPoint); virtual; abstract;
        procedure AdditionalAction(point : TPoint); virtual;
    end;

    { TPencilTool }

    TPencilTool = class(TTool)
        procedure MakeActive(point : TPoint); override;
        procedure ToolMove (point : TPoint); override;
    end;

    { TPolylineTool }

    TPolylineTool = class(TTool)
        procedure MakeActive(point : TPoint); override;
        procedure ToolMove (point : TPoint); override;
        procedure AdditionalAction (point : TPoint); override;
    end;

    { TEllipseTool }

    TEllipseTool = class(TPolylineTool)
        procedure MakeActive(point : TPoint); override;
    end;

    { TRecnungleTool }

    TRecnungleTool = class(TPolylineTool)
        procedure MakeActive(point : TPoint); override;
    end;

    { TRoundRectangleTool }

    TRoundRectangleTool = class(TRecnungleTool)
        procedure MakeActive(point : TPoint); override;
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

procedure TPolylineTool.MakeActive(point: TPoint);
begin
    SetLength(Figures, Length(Figures) + 1);
    Figures[High(Figures)] := TPolyline.Create(point);
end;

procedure TPolylineTool.ToolMove(point: TPoint);
begin
    Figures[High(Figures)].Stranch(point);
end;

procedure TPolylineTool.AdditionalAction(point: TPoint);
begin
    Figures[High(Figures)].AddPoint(point);
end;

{ TRoundRectangleTool }

procedure TRoundRectangleTool.MakeActive(point: TPoint);
begin
    SetLength(Figures, Length(Figures) + 1);
    Figures[High(Figures)] := TRoundRectangle.Create(point);
end;

{ TRecnungleTool }

procedure TRecnungleTool.MakeActive(point: TPoint);
begin
    SetLength(Figures, Length(Figures) + 1);
    Figures[High(Figures)] := TRectangle.Create(point);
end;


{ TEllipseTool }

procedure TEllipseTool.MakeActive(point: TPoint);
begin
    SetLength(Figures, Length(Figures) + 1);
    Figures[High(Figures)] := TEllipse.Create(point);
end;

{ TPencilTool }

procedure TPencilTool.MakeActive(point: TPoint);
begin
    SetLength(Figures, Length(Figures) + 1);
    Figures[High(Figures)] := TPencil.Create(point);
end;

procedure TPencilTool.ToolMove(point: TPoint);
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

procedure TTool.AdditionalAction(point: TPoint);
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

