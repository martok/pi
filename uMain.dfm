object fmPiMain: TfmPiMain
  Left = 601
  Top = 149
  Caption = 'X'
  ClientHeight = 593
  ClientWidth = 852
  Color = clBtnFace
  Font.Charset = ANSI_CHARSET
  Font.Color = clWindowText
  Font.Height = -16
  Font.Name = 'Courier New'
  Font.Style = []
  OldCreateOrder = False
  Position = poScreenCenter
  ShowHint = True
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  OnShow = FormShow
  PixelsPerInch = 120
  TextHeight = 18
  object pnWorkspace: TPanel
    Left = 0
    Top = 0
    Width = 852
    Height = 593
    Align = alClient
    BevelOuter = bvNone
    TabOrder = 0
    object Splitter1: TSplitter
      Left = 608
      Top = 0
      Height = 593
      Align = alRight
      Beveled = True
      ResizeStyle = rsUpdate
    end
    object Panel3: TPanel
      Left = 0
      Top = 0
      Width = 608
      Height = 593
      Align = alClient
      BevelOuter = bvNone
      TabOrder = 0
      object spltInput: TSplitter
        Left = 0
        Top = 469
        Width = 608
        Height = 3
        Cursor = crVSplit
        Align = alBottom
        AutoSnap = False
        Beveled = True
        MinSize = 20
        ResizeStyle = rsUpdate
      end
      object reOutput: TRichEdit
        Left = 0
        Top = 0
        Width = 608
        Height = 469
        Align = alClient
        Font.Charset = ANSI_CHARSET
        Font.Color = clWindowText
        Font.Height = -15
        Font.Name = 'Courier New'
        Font.Style = []
        Lines.Strings = (
          'reOutput')
        ParentFont = False
        ReadOnly = True
        ScrollBars = ssVertical
        TabOrder = 0
      end
      inline frmInput: TfrmInput
        Left = 0
        Top = 472
        Width = 608
        Height = 121
        Align = alBottom
        TabOrder = 1
        inherited meInput: TMemo
          Width = 586
          Height = 121
        end
        inherited Panel1: TPanel
          Left = 586
          Width = 22
          Height = 121
          Font.Height = -13
          inherited sbHistory: TSpeedButton
            Top = 102
            Width = 23
            Height = 20
            Font.Height = -15
          end
        end
      end
    end
    object Panel1: TPanel
      Left = 611
      Top = 0
      Width = 241
      Height = 593
      Align = alRight
      BevelOuter = bvNone
      TabOrder = 1
      object ToolBar1: TToolBar
        Left = 0
        Top = 565
        Width = 241
        Height = 28
        Align = alBottom
        ButtonHeight = 26
        ButtonWidth = 28
        Caption = 'ToolBar1'
        Images = ilButtons
        TabOrder = 1
        object ToolButton1: TToolButton
          Left = 0
          Top = 0
          Action = acRunCmd
        end
        object ToolButton5: TToolButton
          Left = 28
          Top = 0
          Action = acHelp
        end
        object ToolButton2: TToolButton
          Left = 56
          Top = 0
          Action = acRunTest
        end
        object ToolButton3: TToolButton
          Left = 84
          Top = 0
          Width = 8
          Caption = 'ToolButton3'
          ImageIndex = 0
          Style = tbsSeparator
        end
        object ToolButton4: TToolButton
          Left = 92
          Top = 0
          Action = acExit
        end
      end
      object trContext: TTreeView
        Left = 0
        Top = 0
        Width = 241
        Height = 565
        Align = alClient
        Indent = 20
        RightClickSelect = True
        RowSelect = True
        ShowRoot = False
        TabOrder = 0
        ToolTips = False
        OnCollapsing = trContextCollapsing
        OnEdited = trContextEdited
        OnEditing = trContextEditing
      end
    end
  end
  object ActionList1: TActionList
    Images = ilButtons
    Left = 436
    Top = 176
    object acRunCmd: TAction
      Category = 'Toolbar'
      Caption = 'C'
      Hint = 'Run Command'
      ImageIndex = 0
      OnExecute = acRunCmdExecute
    end
    object acExit: TAction
      Category = 'Toolbar'
      Caption = 'X'
      Hint = 'Exit'
      ImageIndex = 3
      OnExecute = acExitExecute
    end
    object acRunTest: TAction
      Category = 'Toolbar'
      Caption = 'T'
      Hint = 'Run Selftest'
      ImageIndex = 2
      OnExecute = acRunTestExecute
    end
    object acHelp: TAction
      Category = 'Toolbar'
      Caption = 'acHelp'
      ImageIndex = 1
      OnExecute = acHelpExecute
    end
  end
  object ilButtons: TImageList
    Left = 668
    Top = 464
    Bitmap = {
      494C0101040009000C0010001000FFFFFFFFFF10FFFFFFFFFFFFFFFF424D3600
      0000000000003600000028000000400000002000000001002000000000000020
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000B93D0000B93D0000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000F7F7F7000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000000000000000000000000000000B9
      3D0016E7230004C6310000B93D00000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000000000000000000000000000E3E3
      E300AAAAAA000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000008E0000008E000000
      8E00000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000008E0000008E00000000000000000000B93D000AC9
      390016E7230016E7230004C6310000B93D000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000E3E3E300AAAA
      AA00AAAAAA0000000000000000000000000000000000000000000000FF000000
      FF000000FF000000FF000000FF000000FF000000FF000000FF000000FF000000
      FF000000FF000000FF0000000000000000000000000000008E0000009D000000
      9D0000008E000000000000000000000000000000000000000000000000000000
      00000000000000008E0000008E0000008E00000000000000000000B93D0027EF
      3E001EEB30001EEB300016E7230000B93D000000000000000000000000000000
      000000000000000000000000000000000000000000000000000000000000F7F7
      F700F7F7F700F7F7F700F7F7F700F7F7F700F7F7F700E3E3E300AAAAAA00AFE3
      E300AAAAAA00D8D8D8000000000000000000000000000000FF000000FF003E3E
      FF006E6EFF006E6EFF006E6EFF006E6EFF006E6EFF006E6EFF006E6EFF006E6E
      FF003E3EFF000000FF000000FF00000000000000000000008E0000009D000000
      9D0000009D000000000000000000000000000000000000000000000000000000
      00000000000000009D0000009D00000000000000000000B93D000DCA410030F3
      4C0016D73E0000B93D001EEB30000CD32E0000B93D0000000000000000000000
      00000000000000000000000000000000000000000000E3E3E300BBBBBB00AAAA
      AA00AAAAAA00AAAAAA00AAAAAA00AAAAAA00AAAAAA00A4A4A400AFE3E300AFF0
      F000ADC1C100B4B4B400E3E3E30000000000000000000000FF000000FF00BABA
      FF00FFFFFF00FFFFFF00FFFFFF00EAEAEA00EAEAEA00FFFFFF00FFFFFF00FFFF
      FF00BABAFF000000FF000000FF00000000000000000000008E0000009D000000
      AA000000AA0000009D0000000000000000000000000000000000000000000000
      00000000AA000000AA0000000000000000000000000000B93D0038F75A0030F3
      4C0000B93D0000B93D0027EF3E001EEB300000B93D0000000000000000000000
      00000000000000000000000000000000000000000000B5B5B500AFD9D900AFE8
      E800AFE9E900AFE9E900AFE9E900B9C8B800B9C8B800AFF0F000B1FFFF00B1FF
      FF00AFE3E300AFE3E300C6C6C60000000000000000000000FF000000FF007272
      FF00FFFFFF00FFFFFF00EAEAEA005252520077777700EAEAEA00FFFFFF00FFFF
      FF008585FF000000FF000000FF0000000000000000000000000000009D000000
      AA000000AA000000AA000000AA00000000000000000000000000000000000000
      B7000000B70000000000000000000000000000B93D001AD44E0041FC68001FDC
      4D0000B93D000000000000B93D0027EF3E0011D5360000B93D00000000000000
      000000000000000000000000000000000000F7F7F700AAAAAA00AFEEEE00B1FF
      FF00B1FFFF00B1FFFF00BBD7C700CA966C00CA966C00BBD7C700B1FFFF00B1FF
      FF00B1FFFF00AFF0F000AEAEAE00F7F7F70000000000000000000000FF000000
      FF00DCDCFF00FFFFFF00EAEAEA00EAEAEA00EAEAEA00EAEAEA00FFFFFF00DCDC
      FF000000FF000000FF000000FF0000000000000000000000000000009D000000
      9D000000AA000000B7000000B7000000000000000000000000000000D2000000
      C5000000000000000000000000000000000000B93D0048FF730048FF730000B9
      3D00000000000000000000B93D0013D1430027EF3E0000B93D00000000000000
      000000000000000000000000000000000000F7F7F700AAAAAA00B1FFFF00B1FF
      FF00B1FFFF00B1FFFF00B3F9F600BADCCD00BADCCD00B3F9F600B1FFFF00B1FF
      FF00B1FFFF00AFF5F500ABABAB00F7F7F70000000000000000000000FF000000
      FF007272FF00DCDCFF00EAEAEA00565656007F7F7F00EAEAEA00E1E1FF007272
      FF000000FF000000FF0000000000000000000000000000000000000000000000
      00000000C5000000B7000000C5000000D200000000000000D2000000D2000000
      0000000000000000000000000000000000000000000000B93D0028E05B0000B9
      3D0000000000000000000000000000B93D0030F34C0016D73E0000B93D000000
      000000000000000000000000000000000000F7F7F700AAAAAA00B1FFFF00B1FF
      FF00B1FFFF00B1FFFF00B5EEE700CE835100CA966C00B9E0D300B1FFFF00B1FF
      FF00B1FFFF00B1FFFF00AAAAAA00F7F7F70000000000000000000000FF000000
      FF000000FF00B2B2FF00EAEAEA004747470056565600EAEAEA00B2B2FF000000
      FF000000FF000000FF0000000000000000000000000000000000000000000000
      0000000000000000C5000000D2000000D2000000DE000000DE00000000000000
      000000000000000000000000000000000000000000000000000000B93D000000
      000000000000000000000000000000B93D001FDC4D0030F34C0000B93D000000
      000000000000000000000000000000000000F7F7F700AAAAAA00B1FFFF00B1FF
      FF00B1FFFF00B5EEE700B5EEE700BCD2C000CA966C00CA966C00B8E4D800B3F9
      F600B1FFFF00B1FFFF00AAAAAA00F7F7F7000000000000000000000000000000
      FF000000FF004141FF00C7C7EA004949490064646400C7C7EA005B5BFF000000
      FF000000FF000000000000000000000000000000000000000000000000000000
      000000000000000000000000C5000000DE000000E5000000E500000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000000B93D0038F75A001BDA450000B9
      3D0000000000000000000000000000000000F7F7F700AAAAAA00B1FFFF00B1FF
      FF00B5EEE700CA966C00CA966C00B5F7F500B8E4D800CA966C00CA966C00B6EC
      E400B1FFFF00B1FFFF00AAAAAA00F7F7F7000000000000000000000000000000
      00000000FF000000FF007272FF00D7D7D700D7D7D7007272FF000000FF000000
      FF000000FF000000000000000000000000000000000000000000000000000000
      000000000000000000000000DE000000E5000000EC000000EC000000EC000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000000B93D0024DF550038F75A0000B9
      3D0000000000000000000000000000000000F7F7F700AAAAAA00B1FFFF00B1FF
      FF00B5EEE700CA966C00CA966C00BCD2C000BCD2C000CA966C00CA966C00B6EC
      E400B1FFFF00B1FFFF00AAAAAA00F7F7F7000000000000000000000000000000
      00000000FF000000FF000000FF00A4A4FF008A8AFF000000FF000000FF000000
      FF00000000000000000000000000000000000000000000000000000000000000
      00000000D2000000DE000000E5000000E5000000E5000000F3000000F3000000
      F300000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000B93D0041FC680024DF
      550000B93D00000000000000000000000000F7F7F700AAAAAA00AFEEEE00B1FF
      FF00B1FFFF00BCD2C000CA966C00CA966C00CA966C00CA966C00BCD2C000B3F9
      F600B1FFFF00AFEEEE00AAAAAA00F7F7F7000000000000000000000000000000
      0000000000000000FF000000FF000000FF000000FF000000FF000000FF000000
      0000000000000000000000000000000000000000000000000000000000000000
      D2000000D2000000DE000000E5000000E500000000000000E5000000F3000000
      F3000000F3000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000B93D0028E05B0041FC
      680000B93D0000000000000000000000000000000000C1C1C100AFD9D900AFEE
      EE00B1FFFF00B1FFFF00B6ECE400BCD2C000BBD7C700B6ECE400B1FFFF00B1FF
      FF00AFEEEE00AFD9D900C9C9C900000000000000000000000000000000000000
      000000000000000000000000FF000000FF000000FF000000FF00000000000000
      00000000000000000000000000000000000000000000000000000000D2000000
      D2000000D2000000DE00000000000000000000000000000000000000F3000000
      F3000000F3000000EC0000000000000000000000000000000000000000000000
      000000000000000000000000000000000000000000000000000000B93D0048FF
      730028E05B0000B93D00000000000000000000000000E9E9E900C3C3C300AAAA
      AA00AAAAAA00AAAAAA00AAAAAA00AAAAAA00AAAAAA00AAAAAA00AAAAAA00AAAA
      AA00AAAAAA00C9C9C900EFEFEF00000000000000000000000000000000000000
      00000000000000000000000000000000FF000000FF0000000000000000000000
      000000000000000000000000000000000000000000000000D2000000D2000000
      DE000000DE000000000000000000000000000000000000000000000000000000
      F3000000F3000000F3000000E500000000000000000000000000000000000000
      00000000000000000000000000000000000000000000000000000000000000B9
      3D0000B93D00000000000000000000000000000000000000000000000000F7F7
      F700F7F7F700F7F7F700F7F7F700F7F7F700F7F7F700F7F7F700F7F7F700F7F7
      F700F7F7F7000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000424D3E000000000000003E000000
      2800000040000000200000000100010000000000000100000000000000000000
      000000000000000000000000FFFFFF0000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000E1FFFFF7F001FFFCC0FFFFE7C00087F8
      C07FFFC7800083F0807FE003800081F0003F8001800081E1003F80018000C0C3
      001F0000C000C087041F0000C001F00F040F0000C003F80F9E0F0000E003FC1F
      FE070000F003FC0FFF070000F007F007FF030000F80FE083FF838001FC1FC3C0
      FFC38001FE3F87E0FFE7E007FFFFBFF900000000000000000000000000000000
      000000000000}
  end
end
