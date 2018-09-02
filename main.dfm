object Form1: TForm1
  Left = 0
  Top = 0
  Caption = 'Form1'
  ClientHeight = 663
  ClientWidth = 527
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  OnCreate = FormCreate
  DesignSize = (
    527
    663)
  PixelsPerInch = 96
  TextHeight = 13
  object Memo1: TMemo
    Left = -2
    Top = 0
    Width = 529
    Height = 585
    Anchors = [akLeft, akTop, akRight, akBottom]
    Lines.Strings = (
      'Memo1')
    ScrollBars = ssBoth
    TabOrder = 0
    ExplicitHeight = 281
  end
  object Button1: TButton
    Left = 136
    Top = 616
    Width = 121
    Height = 25
    Anchors = [akBottom]
    Caption = 'Compute Solutions'
    TabOrder = 1
    OnClick = Button1Click
  end
  object CheckFromScratch: TCheckBox
    Left = 304
    Top = 620
    Width = 97
    Height = 17
    Caption = 'From Scratch'
    Checked = True
    State = cbChecked
    TabOrder = 2
  end
end
