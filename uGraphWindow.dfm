object GraphWindow: TGraphWindow
  Left = 192
  Top = 107
  Width = 266
  Height = 264
  Caption = 'GraphWindow'
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Arial'
  Font.Style = []
  OldCreateOrder = False
  PopupMenu = pmGraph
  Position = poDefault
  OnClose = FormClose
  OnCreate = FormCreate
  OnMouseDown = FormMouseDown
  OnMouseMove = FormMouseMove
  OnMouseUp = FormMouseUp
  OnPaint = FormPaint
  OnResize = FormResize
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 14
  object pmGraph: TPopupMenu
    Left = 160
    Top = 28
    object miLegend: TMenuItem
      Caption = 'Show Legend'
      object miLegendNone: TMenuItem
        AutoCheck = True
        Caption = 'None'
        Checked = True
        GroupIndex = 1
        RadioItem = True
        OnClick = miLegendChange
      end
      object N1: TMenuItem
        Caption = '-'
        GroupIndex = 1
      end
      object miLegendTop: TMenuItem
        AutoCheck = True
        Caption = 'Top'
        GroupIndex = 1
        RadioItem = True
        OnClick = miLegendChange
      end
      object miLegendBottom: TMenuItem
        AutoCheck = True
        Caption = 'Bottom'
        GroupIndex = 1
        RadioItem = True
        OnClick = miLegendChange
      end
    end
    object miGrid: TMenuItem
      AutoCheck = True
      Caption = 'Show Grid'
      OnClick = miGridClick
    end
    object miFineGrid: TMenuItem
      AutoCheck = True
      Caption = 'Show Fine Grid'
      OnClick = miFineGridClick
    end
    object miResetZoom: TMenuItem
      Caption = 'Reset Zoom'
      OnClick = miResetZoomClick
    end
    object N2: TMenuItem
      Caption = '-'
    end
    object miCopyEMF: TMenuItem
      Caption = 'Copy EMF'
      OnClick = miCopyEMFClick
    end
  end
end
