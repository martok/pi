object GraphWindow: TGraphWindow
  Left = 192
  Top = 107
  Caption = 'GraphWindow'
  ClientHeight = 217
  ClientWidth = 248
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
    object N3: TMenuItem
      Caption = '-'
    end
    object miToolRead: TMenuItem
      Caption = 'Read Value'
      GroupIndex = 2
      RadioItem = True
      OnClick = miToolReadClick
    end
    object miToolZoom: TMenuItem
      Caption = 'Zoom'
      Checked = True
      GroupIndex = 2
      RadioItem = True
      OnClick = miToolZoomClick
    end
    object miToolPan: TMenuItem
      Caption = 'Pan'
      GroupIndex = 2
      RadioItem = True
      OnClick = miToolPanClick
    end
    object miResetView: TMenuItem
      Caption = 'Reset View'
      GroupIndex = 2
      OnClick = miResetViewClick
    end
    object N2: TMenuItem
      Caption = '-'
      GroupIndex = 2
    end
    object miCopyEMF: TMenuItem
      Caption = 'Copy EMF'
      GroupIndex = 2
      OnClick = miCopyEMFClick
    end
    object miSaveEMF: TMenuItem
      Caption = 'Save EMF'
      GroupIndex = 2
      OnClick = miSaveEMFClick
    end
  end
  object sdGraph: TSavePictureDialog
    DefaultExt = 'emf'
    Filter = 'Enhanced Metafile (*.emf)|*.emf|All Files (*.*)|*.*'
    Left = 160
    Top = 60
  end
end
