unit LabeledCtrls;

{$MODE Delphi}

interface

uses
  SysUtils, Classes, Controls, StdCtrls, LCLIntf, LCLType, LMessages, ExtCtrls, Graphics, Messages,
  ComCtrls, MaskEdit, MaskUtils, {Consts,} Clipbrd, Forms, curredit, tooledit;

type

  TLabeledComboBox = class(TComboBox)
  private
    FEditLabel: TBoundLabel;
    FLabelPosition: TLabelPosition;
    FLabelSpacing: Integer;
    procedure SetLabelPosition(const Value: TLabelPosition);
    procedure SetLabelSpacing(const Value: Integer);
  protected
    procedure SetParent(AParent: TWinControl); override;
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
    procedure SetName(const Value: TComponentName); override;
    procedure CMVisiblechanged(var Message: TMessage); message CM_VISIBLECHANGED;
    procedure CMEnabledchanged(var Message: TMessage); message CM_ENABLEDCHANGED;
    procedure CMBidimodechanged(var Message: TMessage); message CM_BIDIMODECHANGED;
  public
    constructor Create(AOwner: TComponent); override;
    procedure SetBounds(ALeft: Integer; ATop: Integer; AWidth: Integer; AHeight: Integer); override;
    procedure SetupInternalLabel;
  published
    property ComboBoxLabel: TBoundLabel read FEditLabel;
    property LabelPosition: TLabelPosition read FLabelPosition write SetLabelPosition default lpAbove;
    property LabelSpacing: Integer read FLabelSpacing write SetLabelSpacing default 3;
  end;

  TLabeledMemo = class(TMemo)
  private
    FMemoLabel: TBoundLabel;
    FLabelPosition: TLabelPosition;
    FLabelSpacing: Integer;
    procedure SetLabelPosition(const Value: TLabelPosition);
    procedure SetLabelSpacing(const Value: Integer);
  protected
    procedure SetParent(AParent: TWinControl); override;
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
    procedure SetName(const Value: TComponentName); override;
    procedure CMVisiblechanged(var Message: TMessage);
      message CM_VISIBLECHANGED;
    procedure CMEnabledchanged(var Message: TMessage);
      message CM_ENABLEDCHANGED;
    procedure CMBidimodechanged(var Message: TMessage);
      message CM_BIDIMODECHANGED;
  public
    constructor Create(AOwner: TComponent); override;
    procedure SetBounds(ALeft: Integer; ATop: Integer; AWidth: Integer; AHeight: Integer); override;
    procedure SetupInternalLabel;
  published
    property MemoLabel: TBoundLabel read FMemoLabel;
    property LabelPosition: TLabelPosition read FLabelPosition write SetLabelPosition default lpAbove;
    property LabelSpacing: Integer read FLabelSpacing write SetLabelSpacing default 3;
  end;

  TLabeledCalcEdit = class(TCurrencyEdit)
  private
    FEditLabel: TBoundLabel;
    FLabelPosition: TLabelPosition;
    FLabelSpacing: Integer;
    procedure SetLabelPosition(const Value: TLabelPosition);
    procedure SetLabelSpacing(const Value: Integer);
  protected
    procedure SetParent(AParent: TWinControl); override;
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
    procedure SetName(const Value: TComponentName); override;
    procedure CMVisiblechanged(var Message: TMessage);
      message CM_VISIBLECHANGED;
    procedure CMEnabledchanged(var Message: TMessage);
      message CM_ENABLEDCHANGED;
    procedure CMBidimodechanged(var Message: TMessage);
      message CM_BIDIMODECHANGED;
  public
    constructor Create(AOwner: TComponent); override;
    procedure SetBounds(ALeft: Integer; ATop: Integer; AWidth: Integer; AHeight: Integer); override;
    procedure SetupInternalLabel;
  published
    property CalcEditLabel: TBoundLabel read FEditLabel;
    property LabelPosition: TLabelPosition read FLabelPosition write SetLabelPosition default lpAbove;
    property LabelSpacing: Integer read FLabelSpacing write SetLabelSpacing default 3;
  end;

  TLabeledDateEdit = class(TRxDateEdit)
  private
    FDateEditLabel: TBoundLabel;
    FLabelPosition: TLabelPosition;
    FLabelSpacing: Integer;
    procedure SetLabelPosition(const Value: TLabelPosition);
    procedure SetLabelSpacing(const Value: Integer);
  protected
    procedure SetParent(AParent: TWinControl); override;
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
    procedure SetName(const Value: TComponentName); override;
    procedure CMVisiblechanged(var Message: TMessage);
      message CM_VISIBLECHANGED;
    procedure CMEnabledchanged(var Message: TMessage);
      message CM_ENABLEDCHANGED;
    procedure CMBidimodechanged(var Message: TMessage);
      message CM_BIDIMODECHANGED;
  public
    constructor Create(AOwner: TComponent); override;
    procedure SetBounds(ALeft: Integer; ATop: Integer; AWidth: Integer; AHeight: Integer); override;
    procedure SetupInternalLabel;
  published
    property DateEditLabel: TBoundLabel read FDateEditLabel;
    property LabelPosition: TLabelPosition read FLabelPosition write SetLabelPosition default lpAbove;
    property LabelSpacing: Integer read FLabelSpacing write SetLabelSpacing default 3;
  end;

  TLabeledMaskEdit = class(TMaskEdit)
  private
    FMaskEditLabel: TBoundLabel;
    FLabelPosition: TLabelPosition;
    FLabelSpacing: Integer;
    procedure SetLabelPosition(const Value: TLabelPosition);
    procedure SetLabelSpacing(const Value: Integer);
  protected
    procedure SetParent(AParent: TWinControl); override;
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
    procedure SetName(const Value: TComponentName); override;
    procedure CMVisiblechanged(var Message: TMessage);
      message CM_VISIBLECHANGED;
    procedure CMEnabledchanged(var Message: TMessage);
      message CM_ENABLEDCHANGED;
    procedure CMBidimodechanged(var Message: TMessage);
      message CM_BIDIMODECHANGED;
  public
    constructor Create(AOwner: TComponent); override;
    procedure SetBounds(ALeft: Integer; ATop: Integer; AWidth: Integer; AHeight: Integer); override;
    procedure SetupInternalLabel;
  published
    property MaskEditLabel: TBoundLabel read FMaskEditLabel;
    property LabelPosition: TLabelPosition read FLabelPosition write SetLabelPosition default lpAbove;
    property LabelSpacing: Integer read FLabelSpacing write SetLabelSpacing default 3;
  end;

procedure Register;

implementation

procedure Register;
begin
  RegisterComponents('LabeledControls', [TLabeledComboBox, TLabeledMemo, TLabeledCalcEdit, TLabeledDateEdit, TLabeledMaskEdit]);
end;

function AdjustedAlignment(RightToLeftAlignment: Boolean;
  Alignment: TAlignment): TAlignment;
begin
  Result := Alignment;
  if RightToLeftAlignment then
    case Result of
      taLeftJustify: Result := taRightJustify;
      taRightJustify: Result := taLeftJustify;
    end;
end;

{ TLabeledComboBox }

procedure TLabeledComboBox.CMBidimodechanged(var Message: TMessage);
begin
  inherited;

  if FEditLabel <> nil then
    FEditLabel.BiDiMode := BiDiMode;
end;

procedure TLabeledComboBox.CMEnabledchanged(var Message: TMessage);
begin
  inherited;

  if FEditLabel <> nil then
    FEditLabel.Enabled := Enabled;
end;

procedure TLabeledComboBox.CMVisiblechanged(var Message: TMessage);
begin
  inherited;

  if FEditLabel <> nil then
    FEditLabel.Visible := Visible;
end;

constructor TLabeledComboBox.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);

  Style := csDropDownList;

  FLabelPosition := lpAbove;
  FLabelSpacing := 3;
  SetupInternalLabel;
end;

procedure TLabeledComboBox.Notification(AComponent: TComponent;
  Operation: TOperation);
begin
  inherited Notification(AComponent, Operation);
  if (AComponent = FEditLabel) and (Operation = opRemove) then
    FEditLabel := nil;
end;

procedure TLabeledComboBox.SetBounds(ALeft, ATop, AWidth, AHeight: Integer);
begin
  inherited SetBounds(ALeft, ATop, AWidth, AHeight);
  SetLabelPosition(FLabelPosition);
end;

procedure TLabeledComboBox.SetLabelPosition(const Value: TLabelPosition);
var
  P: TPoint;
begin
  if FEditLabel = nil then
    Exit;

  FLabelPosition := Value;

  case Value of
    lpAbove:
      case AdjustedAlignment(UseRightToLeftAlignment, taLeftJustify) of
        taLeftJustify:  P := Point(Left, Top - FEditLabel.Height - FLabelSpacing);
        taRightJustify: P := Point(Left + Width - FEditLabel.Width,
                                   Top - FEditLabel.Height - FLabelSpacing);
        taCenter:       P := Point(Left + (Width - FEditLabel.Width) div 2,
                                   Top - FEditLabel.Height - FLabelSpacing);
      end;
    lpBelow:
      case AdjustedAlignment(UseRightToLeftAlignment, taLeftJustify) of
        taLeftJustify:  P := Point(Left, Top + Height + FLabelSpacing);
        taRightJustify: P := Point(Left + Width - FEditLabel.Width,
                                   Top + Height + FLabelSpacing);
        taCenter:       P := Point(Left + (Width - FEditLabel.Width) div 2,
                                   Top + Height + FLabelSpacing);
      end;
    lpLeft : P := Point(Left - FEditLabel.Width - FLabelSpacing,
                        Top + ((Height - FEditLabel.Height) div 2));
    lpRight: P := Point(Left + Width + FLabelSpacing,
                        Top + ((Height - FEditLabel.Height) div 2));
  end;
  FEditLabel.SetBounds(P.x, P.y, FEditLabel.Width, FEditLabel.Height);
end;

procedure TLabeledComboBox.SetLabelSpacing(const Value: Integer);
begin
  FLabelSpacing := Value;
  SetLabelPosition(FLabelPosition);
end;

procedure TLabeledComboBox.SetName(const Value: TComponentName);
var
  LClearText: Boolean;
begin
  if (csDesigning in ComponentState) and (FEditLabel <> nil) and
     ((FEditlabel.GetTextLen = 0) or
     (CompareText(FEditLabel.Caption, Name) = 0)) then
  begin
    FEditLabel.Caption := Value;
  end;

  LClearText := (csDesigning in ComponentState) and (Text = '');

  inherited SetName(Value);

  if LClearText then
    Text := '';
end;

procedure TLabeledComboBox.SetParent(AParent: TWinControl);
begin
  inherited SetParent(AParent);

  if FEditLabel = nil then
    Exit;
  FEditLabel.Parent := AParent;
  FEditLabel.Visible := True;
end;

procedure TLabeledComboBox.SetupInternalLabel;
begin
  if Assigned(FEditLabel) then
    Exit;

  FEditLabel := TBoundLabel.Create(Self);
  FEditLabel.FreeNotification(Self);
  FEditLabel.FocusControl := Self;
end;

{ TLabeledMemo }

procedure TLabeledMemo.CMBidimodechanged(var Message: TMessage);
begin
  inherited;

  if FMemoLabel <> nil then
    FMemoLabel.BiDiMode := BiDiMode;
end;

procedure TLabeledMemo.CMEnabledchanged(var Message: TMessage);
begin
  inherited;

  if FMemoLabel <> nil then
    FMemoLabel.Enabled := Enabled;
end;

procedure TLabeledMemo.CMVisiblechanged(var Message: TMessage);
begin
  inherited;

  if FMemoLabel <> nil then
    FMemoLabel.Visible := Visible;
end;

constructor TLabeledMemo.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);

  FLabelPosition := lpAbove;
  FLabelSpacing := 3;
  SetupInternalLabel;
end;

procedure TLabeledMemo.Notification(AComponent: TComponent; Operation: TOperation);
begin
  inherited Notification(AComponent, Operation);
  if (AComponent = FMemoLabel) and (Operation = opRemove) then
    FMemoLabel := nil;
end;

procedure TLabeledMemo.SetBounds(ALeft, ATop, AWidth, AHeight: Integer);
begin
  inherited SetBounds(ALeft, ATop, AWidth, AHeight);
  SetLabelPosition(FLabelPosition);
end;

procedure TLabeledMemo.SetLabelPosition(const Value: TLabelPosition);
var
  P: TPoint;
begin
  if FMemoLabel = nil then
    Exit;

  FLabelPosition := Value;

  case Value of
    lpAbove:
      case AdjustedAlignment(UseRightToLeftAlignment, taLeftJustify) of
        taLeftJustify:  P := Point(Left, Top - FMemoLabel.Height - FLabelSpacing);
        taRightJustify: P := Point(Left + Width - FMemoLabel.Width,
                                   Top - FMemoLabel.Height - FLabelSpacing);
        taCenter:       P := Point(Left + (Width - FMemoLabel.Width) div 2,
                                   Top - FMemoLabel.Height - FLabelSpacing);
      end;
    lpBelow:
      case AdjustedAlignment(UseRightToLeftAlignment, taLeftJustify) of
        taLeftJustify:  P := Point(Left, Top + Height + FLabelSpacing);
        taRightJustify: P := Point(Left + Width - FMemoLabel.Width,
                                   Top + Height + FLabelSpacing);
        taCenter:       P := Point(Left + (Width - FMemoLabel.Width) div 2,
                                   Top + Height + FLabelSpacing);
      end;
    lpLeft : P := Point(Left - FMemoLabel.Width - FLabelSpacing,
                        Top + ((Height - FMemoLabel.Height) div 2));
    lpRight: P := Point(Left + Width + FLabelSpacing,
                        Top + ((Height - FMemoLabel.Height) div 2));
  end;
  FMemoLabel.SetBounds(P.x, P.y, FMemoLabel.Width, FMemoLabel.Height);
end;

procedure TLabeledMemo.SetLabelSpacing(const Value: Integer);
begin
  FLabelSpacing := Value;
  SetLabelPosition(FLabelPosition);
end;

procedure TLabeledMemo.SetName(const Value: TComponentName);
var
  LClearText: Boolean;
begin
  if (csDesigning in ComponentState) and (FMemoLabel <> nil) and
     ((FMemoLabel.GetTextLen = 0) or
     (CompareText(FMemoLabel.Caption, Name) = 0)) then
  begin
    FMemoLabel.Caption := Value;
  end;

  LClearText := (csDesigning in ComponentState) and (Text = '');

  inherited SetName(Value);

  if LClearText then
    Text := '';
end;

procedure TLabeledMemo.SetParent(AParent: TWinControl);
begin
  inherited SetParent(AParent);

  if FMemoLabel = nil then
    Exit;
  FMemoLabel.Parent := AParent;
  FMemoLabel.Visible := True;
end;

procedure TLabeledMemo.SetupInternalLabel;
begin
  if Assigned(FMemoLabel) then
    Exit;

  FMemoLabel := TBoundLabel.Create(Self);
  FMemoLabel.FreeNotification(Self);
  FMemoLabel.FocusControl := Self;
end;


{ TLabeledCalcEdit }

procedure TLabeledCalcEdit.CMBidimodechanged(var Message: TMessage);
begin
  inherited;

  if FEditLabel <> nil then
    FEditLabel.BiDiMode := BiDiMode;
end;

procedure TLabeledCalcEdit.CMEnabledchanged(var Message: TMessage);
begin
  inherited;

  if FEditLabel <> nil then
    FEditLabel.Enabled := Enabled;
end;

procedure TLabeledCalcEdit.CMVisiblechanged(var Message: TMessage);
begin
  inherited;

  if FEditLabel <> nil then
    FEditLabel.Visible := Visible;
end;

constructor TLabeledCalcEdit.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);

  FLabelPosition := lpAbove;
  FLabelSpacing := 3;
  SetupInternalLabel;
end;

procedure TLabeledCalcEdit.Notification(AComponent: TComponent;
  Operation: TOperation);
begin
  inherited Notification(AComponent, Operation);
  if (AComponent = FEditLabel) and (Operation = opRemove) then
    FEditLabel := nil;
end;

procedure TLabeledCalcEdit.SetBounds(ALeft, ATop, AWidth, AHeight: Integer);
begin
  inherited SetBounds(ALeft, ATop, AWidth, AHeight);
  SetLabelPosition(FLabelPosition);
end;

procedure TLabeledCalcEdit.SetLabelPosition(const Value: TLabelPosition);
var
  P: TPoint;
begin
  if FEditLabel = nil then
    Exit;

  FLabelPosition := Value;

  case Value of
    lpAbove:
      case AdjustedAlignment(UseRightToLeftAlignment, taLeftJustify) of
        taLeftJustify:  P := Point(Left, Top - FEditLabel.Height - FLabelSpacing);
        taRightJustify: P := Point(Left + Width - FEditLabel.Width,
                                   Top - FEditLabel.Height - FLabelSpacing);
        taCenter:       P := Point(Left + (Width - FEditLabel.Width) div 2,
                                   Top - FEditLabel.Height - FLabelSpacing);
      end;
    lpBelow:
      case AdjustedAlignment(UseRightToLeftAlignment, taLeftJustify) of
        taLeftJustify:  P := Point(Left, Top + Height + FLabelSpacing);
        taRightJustify: P := Point(Left + Width - FEditLabel.Width,
                                   Top + Height + FLabelSpacing);
        taCenter:       P := Point(Left + (Width - FEditLabel.Width) div 2,
                                   Top + Height + FLabelSpacing);
      end;
    lpLeft : P := Point(Left - FEditLabel.Width - FLabelSpacing,
                        Top + ((Height - FEditLabel.Height) div 2));
    lpRight: P := Point(Left + Width + FLabelSpacing,
                        Top + ((Height - FEditLabel.Height) div 2));
  end;
  FEditLabel.SetBounds(P.x, P.y, FEditLabel.Width, FEditLabel.Height);
end;

procedure TLabeledCalcEdit.SetLabelSpacing(const Value: Integer);
begin
  FLabelSpacing := Value;
  SetLabelPosition(FLabelPosition);
end;

procedure TLabeledCalcEdit.SetName(const Value: TComponentName);
var
  LClearText: Boolean;
begin
  if (csDesigning in ComponentState) and (FEditLabel <> nil) and
     ((FEditLabel.GetTextLen = 0) or
     (CompareText(FEditLabel.Caption, Name) = 0)) then
  begin
    FEditLabel.Caption := Value;
  end;

  LClearText := (csDesigning in ComponentState) and (Text = '');

  inherited SetName(Value);

  if LClearText then
    Text := '';
end;

procedure TLabeledCalcEdit.SetParent(AParent: TWinControl);
begin
  inherited SetParent(AParent);

  if FEditLabel = nil then
    Exit;
  FEditLabel.Parent := AParent;
  FEditLabel.Visible := True;
end;

procedure TLabeledCalcEdit.SetupInternalLabel;
begin
  if Assigned(FEditLabel) then
    Exit;

  FEditLabel := TBoundLabel.Create(Self);
  FEditLabel.FreeNotification(Self);
  FEditLabel.FocusControl := Self;
end;

{ TLabeledDateEdit }

procedure TLabeledDateEdit.CMBidimodechanged(var Message: TMessage);
begin
  inherited;

  if FDateEditLabel <> nil then
    FDateEditLabel.BiDiMode := BiDiMode;
end;

procedure TLabeledDateEdit.CMEnabledchanged(var Message: TMessage);
begin
  inherited;

  if FDateEditLabel <> nil then
    FDateEditLabel.Enabled := Enabled;
end;

procedure TLabeledDateEdit.CMVisiblechanged(var Message: TMessage);
begin
  inherited;

  if FDateEditLabel <> nil then
    FDateEditLabel.Visible := Visible;
end;

constructor TLabeledDateEdit.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);

  FLabelPosition := lpAbove;
  FLabelSpacing := 3;
  SetupInternalLabel;
end;

procedure TLabeledDateEdit.Notification(AComponent: TComponent;
  Operation: TOperation);
begin
  inherited Notification(AComponent, Operation);
  if (AComponent = FDateEditLabel) and (Operation = opRemove) then
    FDateEditLabel := nil;
end;

procedure TLabeledDateEdit.SetBounds(ALeft, ATop, AWidth, AHeight: Integer);
begin
  inherited SetBounds(ALeft, ATop, AWidth, AHeight);
  SetLabelPosition(FLabelPosition);
end;

procedure TLabeledDateEdit.SetLabelPosition(const Value: TLabelPosition);
var
  P: TPoint;
begin
  if FDateEditLabel = nil then
    Exit;

  FLabelPosition := Value;

  case Value of
    lpAbove:
      case AdjustedAlignment(UseRightToLeftAlignment, taLeftJustify) of
        taLeftJustify:  P := Point(Left, Top - FDateEditLabel.Height - FLabelSpacing);
        taRightJustify: P := Point(Left + Width - FDateEditLabel.Width,
                                   Top - FDateEditLabel.Height - FLabelSpacing);
        taCenter:       P := Point(Left + (Width - FDateEditLabel.Width) div 2,
                                   Top - FDateEditLabel.Height - FLabelSpacing);
      end;
    lpBelow:
      case AdjustedAlignment(UseRightToLeftAlignment, taLeftJustify) of
        taLeftJustify:  P := Point(Left, Top + Height + FLabelSpacing);
        taRightJustify: P := Point(Left + Width - FDateEditLabel.Width,
                                   Top + Height + FLabelSpacing);
        taCenter:       P := Point(Left + (Width - FDateEditLabel.Width) div 2,
                                   Top + Height + FLabelSpacing);
      end;
    lpLeft : P := Point(Left - FDateEditLabel.Width - FLabelSpacing,
                        Top + ((Height - FDateEditLabel.Height) div 2));
    lpRight: P := Point(Left + Width + FLabelSpacing,
                        Top + ((Height - FDateEditLabel.Height) div 2));
  end;
  FDateEditLabel.SetBounds(P.x, P.y, FDateEditLabel.Width, FDateEditLabel.Height);
end;

procedure TLabeledDateEdit.SetLabelSpacing(const Value: Integer);
begin
  FLabelSpacing := Value;
  SetLabelPosition(FLabelPosition);
end;

procedure TLabeledDateEdit.SetName(const Value: TComponentName);
var
  LClearText: Boolean;
begin
  if (csDesigning in ComponentState) and (FDateEditLabel <> nil) and
     ((FDateEditLabel.GetTextLen = 0) or
     (CompareText(FDateEditLabel.Caption, Name) = 0)) then
  begin
    FDateEditLabel.Caption := Value;
  end;

  LClearText := (csDesigning in ComponentState) and (Text = '');

  inherited SetName(Value);

  if LClearText then
    Text := '';
end;

procedure TLabeledDateEdit.SetParent(AParent: TWinControl);
begin
  inherited SetParent(AParent);

  if FDateEditLabel = nil then
    Exit;
  FDateEditLabel.Parent := AParent;
  FDateEditLabel.Visible := True;
end;

procedure TLabeledDateEdit.SetupInternalLabel;
begin
  if Assigned(FDateEditLabel) then
    Exit;

  FDateEditLabel := TBoundLabel.Create(Self);
  FDateEditLabel.FreeNotification(Self);
  FDateEditLabel.FocusControl := Self;
end;

{ TLabeledMaskEdit }

procedure TLabeledMaskEdit.CMBidimodechanged(var Message: TMessage);
begin
  inherited;

  if FMaskEditLabel <> nil then
    FMaskEditLabel.BiDiMode := BiDiMode;
end;

procedure TLabeledMaskEdit.CMEnabledchanged(var Message: TMessage);
begin
  inherited;

  if FMaskEditLabel <> nil then
    FMaskEditLabel.Enabled := Enabled;
end;

procedure TLabeledMaskEdit.CMVisiblechanged(var Message: TMessage);
begin
  inherited;

  if FMaskEditLabel <> nil then
    FMaskEditLabel.Visible := Visible;
end;

constructor TLabeledMaskEdit.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);

  FLabelPosition := lpAbove;
  FLabelSpacing := 3;
  SetupInternalLabel;
end;

procedure TLabeledMaskEdit.Notification(AComponent: TComponent; Operation: TOperation);
begin
  inherited Notification(AComponent, Operation);
  if (AComponent = FMaskEditLabel) and (Operation = opRemove) then
    FMaskEditLabel := nil;
end;

procedure TLabeledMaskEdit.SetBounds(ALeft, ATop, AWidth, AHeight: Integer);
begin
  inherited SetBounds(ALeft, ATop, AWidth, AHeight);
  SetLabelPosition(FLabelPosition);
end;

procedure TLabeledMaskEdit.SetLabelPosition(const Value: TLabelPosition);
var
  P: TPoint;
begin
  if FMaskEditLabel = nil then
    Exit;

  FLabelPosition := Value;

  case Value of
    lpAbove:
      case AdjustedAlignment(UseRightToLeftAlignment, taLeftJustify) of
        taLeftJustify:  P := Point(Left, Top - FMaskEditLabel.Height - FLabelSpacing);
        taRightJustify: P := Point(Left + Width - FMaskEditLabel.Width,
                                   Top - FMaskEditLabel.Height - FLabelSpacing);
        taCenter:       P := Point(Left + (Width - FMaskEditLabel.Width) div 2,
                                   Top - FMaskEditLabel.Height - FLabelSpacing);
      end;
    lpBelow:
      case AdjustedAlignment(UseRightToLeftAlignment, taLeftJustify) of
        taLeftJustify:  P := Point(Left, Top + Height + FLabelSpacing);
        taRightJustify: P := Point(Left + Width - FMaskEditLabel.Width,
                                   Top + Height + FLabelSpacing);
        taCenter:       P := Point(Left + (Width - FMaskEditLabel.Width) div 2,
                                   Top + Height + FLabelSpacing);
      end;
    lpLeft : P := Point(Left - FMaskEditLabel.Width - FLabelSpacing,
                        Top + ((Height - FMaskEditLabel.Height) div 2));
    lpRight: P := Point(Left + Width + FLabelSpacing,
                        Top + ((Height - FMaskEditLabel.Height) div 2));
  end;
  FMaskEditLabel.SetBounds(P.x, P.y, FMaskEditLabel.Width, FMaskEditLabel.Height);
end;

procedure TLabeledMaskEdit.SetLabelSpacing(const Value: Integer);
begin
  FLabelSpacing := Value;
  SetLabelPosition(FLabelPosition);
end;

procedure TLabeledMaskEdit.SetName(const Value: TComponentName);
var
  LClearText: Boolean;
begin
  if (csDesigning in ComponentState) and (FMaskEditLabel <> nil) and
     ((FMaskEditLabel.GetTextLen = 0) or
     (CompareText(FMaskEditLabel.Caption, Name) = 0)) then
  begin
    FMaskEditLabel.Caption := Value;
  end;

  LClearText := (csDesigning in ComponentState) and (Text = '');

  inherited SetName(Value);

  if LClearText then
    Text := '';
end;

procedure TLabeledMaskEdit.SetParent(AParent: TWinControl);
begin
  inherited SetParent(AParent);

  if FMaskEditLabel = nil then
    Exit;
  FMaskEditLabel.Parent := AParent;
  FMaskEditLabel.Visible := True;
end;

procedure TLabeledMaskEdit.SetupInternalLabel;
begin
  if Assigned(FMaskEditLabel) then
    Exit;

  FMaskEditLabel := TBoundLabel.Create(Self);
  FMaskEditLabel.FreeNotification(Self);
  FMaskEditLabel.FocusControl := Self;
end;

end.
