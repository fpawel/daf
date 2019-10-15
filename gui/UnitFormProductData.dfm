object FormProductData: TFormProductData
  Left = 0
  Top = 0
  Caption = 'FormProductData'
  ClientHeight = 446
  ClientWidth = 887
  Color = clWindow
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 13
  object Panel1: TPanel
    Left = 0
    Top = 0
    Width = 887
    Height = 25
    Align = alTop
    Alignment = taLeftJustify
    BevelOuter = bvNone
    Caption = 'Panel1'
    Font.Charset = RUSSIAN_CHARSET
    Font.Color = clNavy
    Font.Height = -16
    Font.Name = 'Segoe UI'
    Font.Style = []
    ParentFont = False
    TabOrder = 0
  end
  object Panel2: TPanel
    Left = 0
    Top = 25
    Width = 887
    Height = 421
    Align = alClient
    Alignment = taLeftJustify
    BevelOuter = bvNone
    Caption = 'Panel1'
    Font.Charset = RUSSIAN_CHARSET
    Font.Color = clNavy
    Font.Height = -16
    Font.Name = 'Segoe UI'
    Font.Style = []
    ParentFont = False
    ShowCaption = False
    TabOrder = 1
  end
  object PopupMenu1: TPopupMenu
    OnPopup = PopupMenu1Popup
    Left = 544
    Top = 136
    object N1: TMenuItem
      Caption = #1059#1076#1072#1083#1080#1090#1100' '#1074#1099#1073#1088#1072#1085#1085#1099#1077' '#1079#1072#1087#1080#1089#1080
      OnClick = N1Click
    end
    object N2: TMenuItem
      Caption = #1059#1076#1072#1083#1080#1090#1100' "'#1079#1072#1087#1080#1089#1100'" '#1076#1083#1103' '#1074#1089#1077#1081' '#1087#1072#1088#1090#1080#1080
      OnClick = N2Click
    end
  end
end
