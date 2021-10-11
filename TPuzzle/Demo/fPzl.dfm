object Form1: TForm1
  Left = 228
  Top = 128
  BorderIcons = [biSystemMenu, biMinimize]
  BorderStyle = bsSingle
  Caption = 'TPuzzle'
  ClientHeight = 228
  ClientWidth = 377
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -14
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  PixelsPerInch = 120
  TextHeight = 16
  object Puzzle1: TPuzzle
    Left = 8
    Top = 8
    Width = 201
    Height = 209
    PuzzleSize = 4
    GridColor = clWhite
    NumberFont.Charset = DEFAULT_CHARSET
    NumberFont.Color = clWindowText
    NumberFont.Height = -11
    NumberFont.Name = 'MS Sans Serif'
    NumberFont.Style = []
    ShowNumber = False
    ShowGrid = True
    OnGameOver = Puzzle1GameOver
  end
  object Button1: TButton
    Left = 264
    Top = 8
    Width = 105
    Height = 25
    Caption = #1053#1072#1095#1072#1090#1100
    TabOrder = 0
    OnClick = Button1Click
  end
  object CheckBox1: TCheckBox
    Left = 264
    Top = 40
    Width = 65
    Height = 17
    Caption = #1057#1077#1090#1082#1072
    Checked = True
    State = cbChecked
    TabOrder = 1
    OnClick = CheckBox1Click
  end
  object CheckBox2: TCheckBox
    Left = 264
    Top = 64
    Width = 97
    Height = 17
    Caption = #1053#1091#1084#1077#1088#1072#1094#1080#1103
    TabOrder = 2
    OnClick = CheckBox2Click
  end
  object Button2: TButton
    Left = 264
    Top = 160
    Width = 105
    Height = 25
    Caption = #1056#1080#1089'.1'
    TabOrder = 3
    OnClick = Button2Click
  end
  object Button3: TButton
    Left = 264
    Top = 192
    Width = 105
    Height = 25
    Caption = #1056#1080#1089'.2'
    TabOrder = 4
    OnClick = Button3Click
  end
end
