object Main: TMain
  Left = 0
  Top = 0
  Caption = 'pyeKVS Testbench by kxMaxx'
  ClientHeight = 966
  ClientWidth = 777
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  PixelsPerInch = 96
  TextHeight = 13
  object PTop: TPanel
    Left = 0
    Top = 0
    Width = 777
    Height = 59
    Align = alTop
    TabOrder = 0
    object BWriteFile: TButton
      AlignWithMargins = True
      Left = 269
      Top = 5
      Width = 124
      Height = 49
      Margins.Left = 0
      Margins.Top = 4
      Margins.Right = 4
      Margins.Bottom = 4
      Align = alLeft
      Caption = 'Write to file'
      TabOrder = 0
      OnClick = BWriteFileClick
    end
    object BReadFile: TButton
      AlignWithMargins = True
      Left = 397
      Top = 5
      Width = 124
      Height = 49
      Margins.Left = 0
      Margins.Top = 4
      Margins.Right = 4
      Margins.Bottom = 4
      Align = alLeft
      Caption = 'Read from file'
      TabOrder = 1
      OnClick = BReadFileClick
    end
    object BStreamCopy: TButton
      AlignWithMargins = True
      Left = 525
      Top = 5
      Width = 124
      Height = 49
      Margins.Left = 0
      Margins.Top = 4
      Margins.Right = 4
      Margins.Bottom = 4
      Align = alLeft
      Caption = 'Stream copy'
      TabOrder = 2
      OnClick = BStreamCopyClick
    end
    object RGLog: TRadioGroup
      AlignWithMargins = True
      Left = 653
      Top = 1
      Width = 124
      Height = 53
      Margins.Left = 0
      Margins.Top = 0
      Margins.Right = 4
      Margins.Bottom = 4
      Align = alLeft
      Caption = 'Log mode'
      ItemIndex = 0
      Items.Strings = (
        'Simple'
        'JSON')
      TabOrder = 3
      OnClick = RGLogClick
    end
    object PButton1: TPanel
      AlignWithMargins = True
      Left = 5
      Top = 5
      Width = 128
      Height = 49
      Margins.Left = 4
      Margins.Top = 4
      Margins.Right = 4
      Margins.Bottom = 4
      Align = alLeft
      BevelOuter = bvNone
      TabOrder = 4
      object BNewRnd: TButton
        AlignWithMargins = True
        Left = 0
        Top = 0
        Width = 128
        Height = 25
        Margins.Left = 0
        Margins.Top = 0
        Margins.Right = 0
        Margins.Bottom = 2
        Align = alTop
        Caption = 'Create Random'
        TabOrder = 0
        OnClick = BNewRndClick
      end
    end
    object PButton2: TPanel
      AlignWithMargins = True
      Left = 137
      Top = 5
      Width = 128
      Height = 49
      Margins.Left = 0
      Margins.Top = 4
      Margins.Right = 4
      Margins.Bottom = 4
      Align = alLeft
      BevelOuter = bvNone
      TabOrder = 5
      object BNewHandMade: TButton
        Left = 0
        Top = 25
        Width = 128
        Height = 25
        Margins.Left = 0
        Margins.Top = 0
        Margins.Right = 0
        Margins.Bottom = 2
        Align = alTop
        Caption = 'Create handmade'
        TabOrder = 0
        OnClick = BNewHandMadeClick
      end
      object BNewSimple1: TButton
        Left = 0
        Top = 0
        Width = 128
        Height = 25
        Margins.Left = 0
        Margins.Top = 0
        Margins.Right = 0
        Margins.Bottom = 0
        Align = alTop
        Caption = 'Create Simple'
        TabOrder = 1
        OnClick = BNewSimple1Click
      end
    end
  end
  object PMemo: TPanel
    Left = 0
    Top = 59
    Width = 777
    Height = 907
    Align = alClient
    TabOrder = 1
    object Memo: TMemo
      AlignWithMargins = True
      Left = 5
      Top = 5
      Width = 767
      Height = 878
      Margins.Left = 4
      Margins.Top = 4
      Margins.Right = 4
      Margins.Bottom = 4
      Align = alClient
      ReadOnly = True
      ScrollBars = ssBoth
      TabOrder = 0
    end
    object StatusBar: TStatusBar
      Left = 1
      Top = 887
      Width = 775
      Height = 19
      Panels = <
        item
          Text = 'Time'
          Width = 150
        end>
    end
  end
end
