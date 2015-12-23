unit UInspector;

{$mode objfpc}{$H+}

interface

uses
    Classes, SysUtils, Controls, Spin, UFigures, math, ComCtrls, StdCtrls, ExtCtrls,
    Dialogs, typinfo, Graphics;

type

    { TInspector }

    TInspector = class
        constructor Create();
        procedure GetParam(obj : TObject; selectRect : boolean);
        procedure SetPenColor(penC : TColor);
        procedure SetBrushColor(brushC : TColor);
        procedure GetSelected(obj : TObject);
        procedure ShowEditorsSelected(val: integer);
        procedure LoadPanel(panel: TPanel; pb: TPaintBox);
        procedure HideEditors();
        procedure OnChange(Sender : TObject);
        procedure ClearEditors();
        procedure LoadSelected();
        procedure ClearProp();
        private
            FPropMin : integer;
            FPropObjs : array of TObject;
            FPropSelected : array of PPropList;
            FWidthEditor, FRaduisEditor : TSpinEdit;
            FPenStyleCB, FBrushStyleCB : TComboBox;
            FPenColor, FBrushColor : TColor;
            FPb : TPaintBox;
    end;

var
   Insp : TInspector;

implementation

{ TInspector }

procedure TInspector.GetParam(obj: TObject; selectRect : boolean);
var
    NewProp : PPropList;
    i, j : integer;
begin
    ClearProp();
    ClearEditors();
    HideEditors();
    if (not selectRect) then begin
        for i := 0 to GetPropList(obj, NewProp) - 1 do begin
            case (NewProp^[i]^.Name) of
                 'Width' : begin
                               FWidthEditor.Visible := true;
                               SetInt64Prop(obj, NewProp^[i], FWidthEditor.Value);
                           end;
                 'PenStyle' : begin
                                  FPenStyleCB.Visible := true;
                                  SetPropValue(obj, 'PenStyle', FPenStyleCB.Caption);
                              end;
                 'BrushStyle' : begin
                                    FBrushStyleCB.Visible := true;
                                    SetPropValue(obj, 'BrushStyle', FBrushStyleCB.Caption);
                                end;
                 'Radius' : begin
                                FRaduisEditor.Visible := true;
                                SetInt64Prop(obj, NewProp^[i], FRaduisEditor.Value);
                            end;
                 'PenColor' : SetPropValue(obj, 'PenColor', FPenColor);
                 'BrushColor' : SetPropValue(obj, 'BrushColor', FBrushColor);
            end;
        end;
    end
    else begin
        SetPropValue(obj, 'BrushStyle', bsCross);
        SetPropValue(obj, 'PenColor', clBlack);
        SetPropValue(obj, 'BrushColor', clDefault);
    end;
end;

procedure TInspector.SetPenColor(penC: TColor);
var
    i : integer;
begin
    FPenColor := penC;
    for i := 0 to High(FPropObjs) do begin
        SetPropValue(FPropObjs[i], 'PenColor', FPenColor);
    end;
    FPb.Invalidate;
end;

procedure TInspector.SetBrushColor(brushC: TColor);
var
    i : integer;
begin
    FBrushColor := brushC;
    for i := 0 to High(FPropObjs) do begin
        SetPropValue(FPropObjs[i], 'BrushColor', FBrushColor);
    end;
    FPb.Invalidate;
end;

procedure TInspector.GetSelected(obj: TObject);
begin
    SetLength(FPropSelected, Length(FPropSelected) + 1);
    FPropMin := min(FPropMin, GetPropList(obj, FPropSelected[High(FPropSelected)]));
    SetLength(FPropObjs, Length(FPropObjs) + 1);
    FPropObjs[High(FPropObjs)] := obj;
end;

procedure TInspector.ShowEditorsSelected(val : integer);
begin
    if (val >= 4) then begin
        FRaduisEditor.Visible := true;
        FRaduisEditor.Caption := '' ;
    end;
    if (val >= 3) then begin
        FBrushStyleCB.Visible := true;
        FBrushStyleCB.Caption := '';
    end;
    if (val >= 2) then begin
        FWidthEditor.Visible := true;
        FWidthEditor.Caption := '';
        FPenStyleCB.Visible := true;
        FPenStyleCB.Caption := '';
    end;
end;

procedure TInspector.LoadPanel(panel: TPanel; pb : TPaintBox);
var
   top : integer = 16;
begin
    FPb := pb;

    FWidthEditor.Parent := panel;
    FWidthEditor.Left := 16;
    FWidthEditor.Top := top;
    FWidthEditor.Height := 27;
    FWidthEditor.Width := 100;
    FWidthEditor.Visible := false;
    FWidthEditor.OnChange := @OnChange;
    FWidthEditor.Value := 1;
    FWidthEditor.MinValue := 1;
    FWidthEditor.MaxValue := 100;

    top += 40;
    FPenStyleCB.Parent := panel;
    FPenStyleCB.Left := 16;
    FPenStyleCB.Top := top;
    FPenStyleCB.Height := 27;
    FPenStyleCB.Width := 100;
    FPenStyleCB.Visible := false;
    FPenStyleCB.OnChange := @OnChange;
    FPenStyleCB.Caption := 'psSolid';
    FPenStyleCB.AddItem('psSolid', FPenStyleCB);
    FPenStyleCB.AddItem('psDash', FPenStyleCB);
    FPenStyleCB.AddItem('psDot', FPenStyleCB);
    FPenStyleCB.AddItem('psDashDot', FPenStyleCB);
    FPenStyleCB.AddItem('psDashDotDot', FPenStyleCB);

    top += 40;
    FBrushStyleCB.Parent := panel;
    FBrushStyleCB.Left := 16;
    FBrushStyleCB.Top := top;
    FBrushStyleCB.Height := 27;
    FBrushStyleCB.Width := 100;
    FBrushStyleCB.Visible := false;
    FBrushStyleCB.OnChange := @OnChange;
    FBrushStyleCB.Caption:= 'bsClear';
    FBrushStyleCB.AddItem('bsClear', FBrushStyleCB);
    FBrushStyleCB.AddItem('bsSolid', FBrushStyleCB);
    FBrushStyleCB.AddItem('bsHorizontal', FBrushStyleCB);
    FBrushStyleCB.AddItem('bsVertical', FBrushStyleCB);
    FBrushStyleCB.AddItem('bsFDiagonal', FBrushStyleCB);
    FBrushStyleCB.AddItem('bsBDiagonal', FBrushStyleCB);
    FBrushStyleCB.AddItem('bsCross', FBrushStyleCB);
    FBrushStyleCB.AddItem('bsDiagCross', FBrushStyleCB);

    top += 40;
    FRaduisEditor.Parent := panel;
    FRaduisEditor.Left := 16;
    FRaduisEditor.Top := top;
    FRaduisEditor.Height := 27;
    FRaduisEditor.Width := 100;
    FRaduisEditor.Visible := false;
    FRaduisEditor.OnChange := @OnChange;
    FRaduisEditor.Value := 1;
    FRaduisEditor.MinValue := 1;
    FRaduisEditor.MaxValue := 100;

end;

procedure TInspector.HideEditors;
var
   i : integer;
begin
    FBrushStyleCB.Visible := false;
    FPenStyleCB.Visible := false;
    FWidthEditor.Visible := false;
    FRaduisEditor.Visible := false;;
end;

procedure TInspector.OnChange(Sender: TObject);
var
   i : integer;
begin
    if (FRaduisEditor.Visible) and (FRaduisEditor.Caption <> '' )then begin
        for i := 0 to High(FPropObjs) do begin
            SetPropValue(FPropObjs[i], 'Radius', FRaduisEditor.Caption);
        end;
    end;

    if (FWidthEditor.Visible) and (FWidthEditor.Caption <> '') then begin
        for i := 0 to High(FPropObjs) do begin
            SetPropValue(FPropObjs[i], 'Width', FWidthEditor.Value);
        end;
    end;

    if (FPenStyleCB.Visible) and (FPenStyleCB.Caption <> '') then begin
        for i := 0 to High(FPropObjs) do begin
            SetPropValue(FPropObjs[i], 'PenStyle', FPenStyleCB.Caption);
        end;
    end;

    if (FBrushStyleCB.Visible) and (FBrushStyleCB.Caption <> '') then begin
        for i := 0 to High(FPropObjs) do begin
            SetPropValue(FPropObjs[i], 'BrushStyle', FBrushStyleCB.Caption);
        end;
    end;

    FPb.Invalidate;
end;

procedure TInspector.ClearEditors;
begin
    if (FBrushStyleCB.Caption = '') then
       FBrushStyleCB.Caption := 'bsClear';
    if (FPenStyleCB.Caption = '') then
       FPenStyleCB.Caption := 'psSolid';
    if (FWidthEditor.Caption = '') then begin
       FWidthEditor.Value := 1;
    end;
    if (FRaduisEditor.Caption = '') then begin
       FRaduisEditor.Value := 1;
    end;
end;

procedure TInspector.LoadSelected;
begin
    case (FPropMin) of
         7 : ShowEditorsSelected(4);
         6 : ShowEditorsSelected(3);
         4 : ShowEditorsSelected(2);
    end;
end;

procedure TInspector.ClearProp;
begin
    SetLength(FPropSelected, 0);
    FPropMin := High(Integer);
    SetLength(FPropObjs, 0);
end;

constructor TInspector.Create();
begin
    FBrushStyleCB := TComboBox.Create(nil);
    FPenStyleCB := TComboBox.Create(nil);
    FWidthEditor := TSpinEdit.Create(nil);
    FRaduisEditor := TSpinEdit.Create(nil);
end;

initialization
    Insp := TInspector.Create();
end.

