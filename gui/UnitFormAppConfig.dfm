object FormAppConfig: TFormAppConfig
  Left = 0
  Top = 0
  BorderStyle = bsToolWindow
  Caption = #1053#1072#1089#1090#1088#1086#1081#1082#1080
  ClientHeight = 150
  ClientWidth = 329
  Color = clWindow
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -16
  Font.Name = 'Segoe UI'
  Font.Style = []
  OldCreateOrder = False
  OnCreate = FormCreate
  OnDeactivate = FormDeactivate
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 21
  object GroupBox2: TGroupBox
    AlignWithMargins = True
    Left = 5
    Top = 5
    Width = 319
    Height = 140
    Margins.Left = 5
    Margins.Top = 5
    Margins.Right = 5
    Margins.Bottom = 5
    Align = alTop
    Caption = #1057#1054#1052' '#1087#1086#1088#1090#1099
    TabOrder = 0
    object Panel1: TPanel
      Left = 2
      Top = 23
      Width = 315
      Height = 38
      Align = alTop
      BevelOuter = bvNone
      BorderWidth = 1
      TabOrder = 0
      object Shape1: TShape
        Left = 1
        Top = 36
        Width = 313
        Height = 1
        Align = alBottom
        Pen.Color = cl3DLight
        ExplicitLeft = 168
        ExplicitTop = 152
        ExplicitWidth = 65
      end
      object Panel2: TPanel
        Left = 1
        Top = 1
        Width = 200
        Height = 35
        Align = alLeft
        Alignment = taRightJustify
        BevelOuter = bvNone
        Caption = #1057#1090#1077#1085#1076
        TabOrder = 1
      end
      object ComboBoxComportProducts: TComboBox
        Left = 210
        Top = 4
        Width = 98
        Height = 26
        Style = csOwnerDrawFixed
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -15
        Font.Name = 'Tahoma'
        Font.Style = []
        ItemHeight = 20
        ItemIndex = 0
        ParentFont = False
        TabOrder = 0
        Text = 'COM1'
        OnChange = ComboBoxComportProductsChange
        Items.Strings = (
          'COM1')
      end
    end
    object Panel17: TPanel
      Left = 2
      Top = 61
      Width = 315
      Height = 38
      Align = alTop
      BevelOuter = bvNone
      BorderWidth = 1
      TabOrder = 1
      object Shape3: TShape
        Left = 1
        Top = 36
        Width = 313
        Height = 1
        Align = alBottom
        Pen.Color = cl3DLight
        ExplicitLeft = 2
        ExplicitTop = 37
        ExplicitWidth = 291
      end
      object Panel18: TPanel
        Left = 1
        Top = 1
        Width = 200
        Height = 35
        Align = alLeft
        Alignment = taRightJustify
        BevelOuter = bvNone
        Caption = 'HART '#1084#1086#1076#1077#1084
        TabOrder = 0
      end
      object ComboBoxComportHart: TComboBox
        Left = 210
        Top = 4
        Width = 98
        Height = 26
        Style = csOwnerDrawFixed
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -15
        Font.Name = 'Tahoma'
        Font.Style = []
        ItemHeight = 20
        ItemIndex = 0
        ParentFont = False
        TabOrder = 1
        Text = 'COM1'
        OnChange = ComboBoxComportProductsChange
        Items.Strings = (
          'COM1')
      end
    end
    object Panel3: TPanel
      Left = 2
      Top = 99
      Width = 315
      Height = 38
      Align = alTop
      BevelOuter = bvNone
      BorderWidth = 1
      TabOrder = 2
      object Panel4: TPanel
        Left = 1
        Top = 1
        Width = 200
        Height = 36
        Align = alLeft
        Alignment = taRightJustify
        BevelOuter = bvNone
        Caption = #1055#1086#1082#1072#1079#1072#1090#1100' '#1087#1072#1085#1077#1083#1100' MODBUS'
        TabOrder = 0
      end
      object CheckBox1: TCheckBox
        Left = 210
        Top = 12
        Width = 23
        Height = 17
        TabOrder = 1
        OnClick = CheckBox1Click
      end
    end
  end
end
