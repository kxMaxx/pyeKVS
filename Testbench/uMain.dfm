object Main: TMain
  Left = 0
  Top = 0
  Caption = 'pyeKVS Testbench by kxMaxx'
  ClientHeight = 750
  ClientWidth = 944
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  PixelsPerInch = 96
  TextHeight = 13
  object PMemo: TPanel
    Left = 201
    Top = 0
    Width = 743
    Height = 731
    Align = alClient
    TabOrder = 0
    object Memo: TMemo
      AlignWithMargins = True
      Left = 5
      Top = 5
      Width = 733
      Height = 721
      Margins.Left = 4
      Margins.Top = 4
      Margins.Right = 4
      Margins.Bottom = 4
      Align = alClient
      ReadOnly = True
      ScrollBars = ssBoth
      TabOrder = 0
    end
  end
  object PCreator: TPanel
    Left = 0
    Top = 0
    Width = 201
    Height = 731
    Align = alLeft
    ParentBackground = False
    ParentColor = True
    TabOrder = 1
    object GBMemory: TGroupBox
      AlignWithMargins = True
      Left = 4
      Top = 138
      Width = 193
      Height = 159
      Align = alTop
      Caption = 'Memory stream'
      TabOrder = 0
      object BMemorySimple1: TButton
        AlignWithMargins = True
        Left = 4
        Top = 44
        Width = 185
        Height = 25
        Margins.Left = 2
        Margins.Top = 2
        Margins.Right = 2
        Margins.Bottom = 0
        Align = alTop
        Caption = 'Create Simple in RAM'
        TabOrder = 0
        OnClick = BMemorySimple1Click
      end
      object BMemoryHandMade: TButton
        AlignWithMargins = True
        Left = 4
        Top = 17
        Width = 185
        Height = 25
        Margins.Left = 2
        Margins.Top = 2
        Margins.Right = 2
        Margins.Bottom = 0
        Align = alTop
        Caption = 'Create handmade in RAM'
        TabOrder = 1
        OnClick = BMemoryHandMadeClick
      end
      object BMemoryRnd: TButton
        AlignWithMargins = True
        Left = 4
        Top = 71
        Width = 185
        Height = 25
        Margins.Left = 2
        Margins.Top = 2
        Margins.Right = 2
        Margins.Bottom = 0
        Align = alTop
        Caption = 'Create Random in RAM'
        TabOrder = 2
        OnClick = BMemoryRndClick
      end
      object BMemoryWriteFile: TButton
        AlignWithMargins = True
        Left = 4
        Top = 98
        Width = 185
        Height = 25
        Margins.Left = 2
        Margins.Top = 2
        Margins.Right = 2
        Margins.Bottom = 0
        Align = alTop
        Caption = 'Write to file'
        TabOrder = 3
        OnClick = BMemoryWriteFileClick
      end
      object BMemoryReadFile: TButton
        AlignWithMargins = True
        Left = 4
        Top = 125
        Width = 185
        Height = 25
        Margins.Left = 2
        Margins.Top = 2
        Margins.Right = 2
        Margins.Bottom = 0
        Align = alTop
        Caption = 'Read from file'
        TabOrder = 4
        OnClick = BMemoryReadFileClick
      end
    end
    object GBUserStream: TGroupBox
      AlignWithMargins = True
      Left = 4
      Top = 423
      Width = 193
      Height = 114
      Align = alTop
      Caption = 'External stream'
      TabOrder = 1
      object BUserStreamHandMade: TButton
        AlignWithMargins = True
        Left = 4
        Top = 17
        Width = 185
        Height = 25
        Margins.Left = 2
        Margins.Top = 2
        Margins.Right = 2
        Margins.Bottom = 0
        Align = alTop
        Caption = 'Create handmade to file'
        TabOrder = 0
        OnClick = BUserStreamHandMadeClick
      end
      object BUserStreamRandom: TButton
        AlignWithMargins = True
        Left = 4
        Top = 44
        Width = 185
        Height = 25
        Margins.Left = 2
        Margins.Top = 2
        Margins.Right = 2
        Margins.Bottom = 0
        Align = alTop
        Caption = 'Create Random to file'
        TabOrder = 1
        OnClick = BUserStreamRandomClick
      end
      object BUserStreamlRead: TButton
        AlignWithMargins = True
        Left = 4
        Top = 71
        Width = 185
        Height = 25
        Margins.Left = 2
        Margins.Top = 2
        Margins.Right = 2
        Margins.Bottom = 0
        Align = alTop
        Caption = 'Read direct from file'
        TabOrder = 2
        OnClick = BUserStreamlReadClick
      end
    end
    object RGLog: TRadioGroup
      AlignWithMargins = True
      Left = 4
      Top = 4
      Width = 193
      Height = 64
      Align = alTop
      Caption = 'Log mode'
      ItemIndex = 0
      Items.Strings = (
        'Simple'
        'JSON')
      TabOrder = 2
      OnClick = RGLogClick
    end
    object GBInfo: TGroupBox
      AlignWithMargins = True
      Left = 4
      Top = 677
      Width = 193
      Height = 50
      Align = alBottom
      Caption = 'Info'
      TabOrder = 3
      object BAbout: TButton
        AlignWithMargins = True
        Left = 4
        Top = 17
        Width = 185
        Height = 25
        Margins.Left = 2
        Margins.Top = 2
        Margins.Right = 2
        Margins.Bottom = 0
        Align = alTop
        Caption = 'About'
        TabOrder = 0
        OnClick = BAboutClick
      end
    end
    object GBFile: TGroupBox
      AlignWithMargins = True
      Left = 4
      Top = 303
      Width = 193
      Height = 114
      Align = alTop
      Caption = 'File stream'
      TabOrder = 4
      object BFileHandmade: TButton
        AlignWithMargins = True
        Left = 4
        Top = 17
        Width = 185
        Height = 25
        Margins.Left = 2
        Margins.Top = 2
        Margins.Right = 2
        Margins.Bottom = 0
        Align = alTop
        Caption = 'Create handmade to file'
        TabOrder = 0
        OnClick = BFileHandmadeClick
      end
      object BFileRnd: TButton
        AlignWithMargins = True
        Left = 4
        Top = 44
        Width = 185
        Height = 25
        Margins.Left = 2
        Margins.Top = 2
        Margins.Right = 2
        Margins.Bottom = 0
        Align = alTop
        Caption = 'Create Random to file'
        TabOrder = 1
        OnClick = BFileRndClick
      end
      object BFileRead: TButton
        AlignWithMargins = True
        Left = 4
        Top = 71
        Width = 185
        Height = 25
        Margins.Left = 2
        Margins.Top = 2
        Margins.Right = 2
        Margins.Bottom = 0
        Align = alTop
        Caption = 'Read direct from file'
        TabOrder = 2
        OnClick = BFileReadClick
      end
    end
    object GBSpecial: TGroupBox
      AlignWithMargins = True
      Left = 4
      Top = 543
      Width = 193
      Height = 58
      Align = alTop
      Caption = 'Specials'
      TabOrder = 5
      object BMemoryStreamCopy: TButton
        AlignWithMargins = True
        Left = 4
        Top = 17
        Width = 185
        Height = 25
        Margins.Left = 2
        Margins.Top = 2
        Margins.Right = 2
        Margins.Bottom = 0
        Align = alTop
        Caption = 'Stream copy (e.g. TCP)'
        TabOrder = 0
        OnClick = BMemoryStreamCopyClick
      end
    end
    object GBGeneral: TGroupBox
      AlignWithMargins = True
      Left = 4
      Top = 74
      Width = 193
      Height = 58
      Align = alTop
      Caption = 'General'
      TabOrder = 6
      object BClear: TButton
        AlignWithMargins = True
        Left = 4
        Top = 17
        Width = 185
        Height = 25
        Margins.Left = 2
        Margins.Top = 2
        Margins.Right = 2
        Margins.Bottom = 0
        Align = alTop
        Caption = 'Clear'
        TabOrder = 0
        OnClick = BClearClick
      end
    end
  end
  object StatusBar: TStatusBar
    Left = 0
    Top = 731
    Width = 944
    Height = 19
    Panels = <
      item
        Text = 'Time:'
        Width = 150
      end
      item
        Text = 'Size:'
        Width = 50
      end>
  end
end
