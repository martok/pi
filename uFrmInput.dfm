object frmInput: TfrmInput
  Left = 0
  Top = 0
  Width = 402
  Height = 106
  TabOrder = 0
  object meInput: TMemo
    Left = 0
    Top = 0
    Width = 384
    Height = 106
    Align = alClient
    TabOrder = 0
    OnChange = meInputChange
    OnKeyDown = meInputKeyDown
    OnKeyPress = meInputKeyPress
  end
  object Panel1: TPanel
    Left = 384
    Top = 0
    Width = 18
    Height = 106
    Align = alRight
    BevelOuter = bvNone
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'MS Sans Serif'
    Font.Style = []
    ParentFont = False
    TabOrder = 1
    DesignSize = (
      18
      106)
    object sbHistory: TSpeedButton
      Left = 0
      Top = 88
      Width = 19
      Height = 17
      Anchors = [akLeft, akRight, akBottom]
      Caption = 'u'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -13
      Font.Name = 'Marlett'
      Font.Style = [fsBold]
      ParentFont = False
      OnClick = sbHistoryClick
    end
  end
  object pmHistory: TPopupMenu
    AutoHotkeys = maManual
    AutoLineReduction = maManual
    OwnerDraw = True
    OnPopup = pmHistoryPopup
    Left = 308
    Top = 12
    object miHistLast: TMenuItem
      Caption = '-'
    end
    object miClearHistory: TMenuItem
      Caption = 'Clear History'
      OnClick = miClearHistoryClick
    end
  end
end
