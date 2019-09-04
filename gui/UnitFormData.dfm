object FormData: TFormData
  Left = 0
  Top = 0
  Caption = 'FormData'
  ClientHeight = 592
  ClientWidth = 1000
  Color = clWindow
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  OnCreate = FormCreate
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 13
  object Splitter1: TSplitter
    Left = 270
    Top = 0
    Width = 5
    Height = 592
    ExplicitLeft = 265
    ExplicitHeight = 300
  end
  object Panel1: TPanel
    Left = 0
    Top = 0
    Width = 270
    Height = 592
    Align = alLeft
    BevelOuter = bvNone
    Constraints.MinWidth = 265
    TabOrder = 0
    OnResize = Panel1Resize
    object Panel3: TPanel
      Left = 0
      Top = 0
      Width = 270
      Height = 31
      Align = alTop
      Alignment = taLeftJustify
      BevelOuter = bvNone
      Caption = '   '#1044#1072#1090#1072
      Color = clGradientInactiveCaption
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clBlack
      Font.Height = -15
      Font.Name = 'Tahoma'
      Font.Style = []
      ParentBackground = False
      ParentFont = False
      TabOrder = 0
      object ComboBox1: TComboBox
        Left = 77
        Top = 2
        Width = 132
        Height = 26
        Style = csOwnerDrawFixed
        Color = clHighlightText
        ItemHeight = 20
        ItemIndex = 0
        TabOrder = 0
        Text = '11.11.2018'
        OnChange = ComboBox1Change
        Items.Strings = (
          '11.11.2018')
      end
    end
    object StringGrid1: TStringGrid
      AlignWithMargins = True
      Left = 3
      Top = 34
      Width = 264
      Height = 555
      Align = alClient
      BorderStyle = bsNone
      DefaultRowHeight = 22
      DefaultDrawing = False
      FixedColor = clBackground
      FixedCols = 0
      RowCount = 1
      FixedRows = 0
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -15
      Font.Name = 'Tahoma'
      Font.Style = []
      GradientEndColor = clBlack
      Options = [goFixedVertLine, goFixedHorzLine, goVertLine, goHorzLine, goRangeSelect, goRowSelect]
      ParentFont = False
      TabOrder = 1
      OnDrawCell = StringGrid1DrawCell
      OnSelectCell = StringGrid1SelectCell
      ColWidths = (
        64
        64
        64
        64
        64)
      RowHeights = (
        22)
    end
  end
  object Panel2: TPanel
    Left = 275
    Top = 0
    Width = 725
    Height = 592
    Align = alClient
    BevelOuter = bvNone
    Caption = 'Panel2'
    ShowCaption = False
    TabOrder = 1
    ExplicitLeft = 736
    ExplicitTop = 376
    ExplicitWidth = 185
    ExplicitHeight = 41
  end
end
