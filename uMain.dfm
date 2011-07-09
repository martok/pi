object Form1: TForm1
  Left = 192
  Top = 135
  Width = 870
  Height = 640
  Caption = 'pi - Tiny Math Tool'
  Color = clBtnFace
  Font.Charset = ANSI_CHARSET
  Font.Color = clWindowText
  Font.Height = -13
  Font.Name = 'Courier New'
  Font.Style = []
  OldCreateOrder = False
  ShowHint = True
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 16
  object Splitter1: TSplitter
    Left = 645
    Top = 0
    Height = 583
    Align = alRight
    Beveled = True
    ResizeStyle = rsUpdate
  end
  object Memo1: TMemo
    Left = 0
    Top = 0
    Width = 645
    Height = 583
    Align = alClient
    Lines.Strings = (
      'Memo1')
    ScrollBars = ssVertical
    TabOrder = 0
    WordWrap = False
  end
  object Panel1: TPanel
    Left = 648
    Top = 0
    Width = 214
    Height = 583
    Align = alRight
    BevelOuter = bvNone
    TabOrder = 1
    object lbContext: TListBox
      Left = 0
      Top = 0
      Width = 214
      Height = 555
      Align = alClient
      ItemHeight = 16
      TabOrder = 0
    end
    object ToolBar1: TToolBar
      Left = 0
      Top = 555
      Width = 214
      Height = 28
      Align = alBottom
      ButtonHeight = 24
      ButtonWidth = 28
      Caption = 'ToolBar1'
      TabOrder = 1
      object ToolButton1: TToolButton
        Left = 0
        Top = 2
        Action = acRunCmd
      end
      object ToolButton2: TToolButton
        Left = 28
        Top = 2
        Action = acRunTest
      end
      object ToolButton3: TToolButton
        Left = 56
        Top = 2
        Width = 8
        Caption = 'ToolButton3'
        ImageIndex = 0
        Style = tbsSeparator
      end
      object ToolButton4: TToolButton
        Left = 64
        Top = 2
        Action = acExit
      end
    end
  end
  object Panel2: TPanel
    Left = 0
    Top = 583
    Width = 862
    Height = 30
    Align = alBottom
    BevelOuter = bvNone
    TabOrder = 2
    DesignSize = (
      862
      30)
    object cbInput: TComboBox
      Left = 3
      Top = 3
      Width = 856
      Height = 24
      Anchors = [akLeft, akTop, akRight, akBottom]
      ItemHeight = 16
      TabOrder = 0
      Text = 'cbInput'
      OnKeyPress = cbInputKeyPress
    end
  end
  object ActionList1: TActionList
    Left = 600
    Top = 40
    object acRunCmd: TAction
      Caption = 'C'
      Hint = 'Run Command'
      OnExecute = acRunCmdExecute
    end
    object acExit: TAction
      Category = 'Toolbar'
      Caption = 'X'
      Hint = 'Exit'
      OnExecute = acExitExecute
    end
    object acRunTest: TAction
      Category = 'Toolbar'
      Caption = 'T'
      Hint = 'Run Selftest'
      OnExecute = acRunTestExecute
    end
  end
end
