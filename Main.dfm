object Form1: TForm1
  Left = 0
  Top = 0
  Caption = 'QTHLOOKUP'
  ClientHeight = 260
  ClientWidth = 881
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -12
  Font.Name = #65325#65331' '#65328#12468#12471#12483#12463
  Font.Style = []
  Menu = MainMenu1
  OldCreateOrder = False
  OnActivate = FormActivate
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 12
  object Panel1: TPanel
    Left = 0
    Top = 0
    Width = 881
    Height = 33
    Align = alTop
    TabOrder = 0
    object Label1: TLabel
      Left = 8
      Top = 10
      Width = 66
      Height = 12
      Caption = #12467#12540#12523#12469#12452#12531
    end
    object buttonQuery: TButton
      Left = 215
      Top = 5
      Width = 58
      Height = 25
      Caption = #29031#20250
      TabOrder = 1
      OnClick = buttonQueryClick
    end
    object editCallsign: TEdit
      Left = 80
      Top = 7
      Width = 129
      Height = 20
      CharCase = ecUpperCase
      TabOrder = 0
      OnEnter = editCallsignEnter
      OnExit = editCallsignExit
    end
    object checkZlog: TCheckBox
      Left = 324
      Top = 9
      Width = 73
      Height = 17
      Caption = 'zLog'#36899#21205
      TabOrder = 2
      OnClick = checkZlogClick
    end
    object checkAround: TCheckBox
      Left = 412
      Top = 9
      Width = 109
      Height = 17
      Caption = #21069#24460#12398#23616#12418#34920#31034
      TabOrder = 3
      OnClick = checkAroundClick
    end
    object checkNoSameAddress: TCheckBox
      Left = 536
      Top = 9
      Width = 81
      Height = 17
      Caption = #37325#35079#12399#38500#22806
      Checked = True
      State = cbChecked
      TabOrder = 4
      OnClick = checkAroundClick
    end
  end
  object ListView1: TListView
    Left = 0
    Top = 33
    Width = 881
    Height = 208
    Align = alClient
    Columns = <
      item
        Caption = 'CALLSIGN'
        Width = 80
      end
      item
        Caption = #37117#36947#24220#30476
        Width = 60
      end
      item
        Caption = #24066#21306#30010#26449
        Width = 100
      end
      item
        Caption = #30476#12467#12540#12489
        Width = 55
      end
      item
        Caption = 'JCC/G'
        Width = 80
      end
      item
        Caption = 'QTH'
        Width = 180
      end
      item
        Caption = #21517#31216
        Width = 150
      end
      item
        Caption = #20813#35377#12398#24180#26376#26085
        Width = 100
      end
      item
        Caption = 'Power'
      end>
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -12
    Font.Name = #65325#65331' '#12468#12471#12483#12463
    Font.Style = []
    GridLines = True
    OwnerDraw = True
    ReadOnly = True
    RowSelect = True
    ParentFont = False
    TabOrder = 1
    ViewStyle = vsReport
    OnAdvancedCustomDrawItem = ListView1AdvancedCustomDrawItem
  end
  object StatusBar1: TStatusBar
    Left = 0
    Top = 241
    Width = 881
    Height = 19
    Panels = <
      item
        Alignment = taCenter
        Width = 80
      end
      item
        Alignment = taCenter
        Width = 80
      end
      item
        Alignment = taCenter
        Width = 120
      end
      item
        Alignment = taCenter
        Width = 100
      end
      item
        Width = 50
      end>
  end
  object timerLogSync: TTimer
    Enabled = False
    Interval = 500
    OnTimer = timerLogSyncTimer
    Left = 688
  end
  object MainMenu1: TMainMenu
    Left = 716
    object F1: TMenuItem
      Caption = #12501#12449#12452#12523'(&F)'
      object menuOffline: TMenuItem
        AutoCheck = True
        Caption = #12458#12501#12521#12452#12531#12514#12540#12489
        Checked = True
        OnClick = menuOfflineClick
      end
      object N4: TMenuItem
        Caption = '-'
      end
      object menuOpen: TMenuItem
        Caption = #12467#12540#12523#12469#12452#12531#12522#12473#12488#12391#29031#20250'(&O)'
        OnClick = menuOpenClick
      end
      object N1: TMenuItem
        Caption = '-'
      end
      object menuSaveAs: TMenuItem
        Caption = #21517#21069#12434#20184#12369#12390#20445#23384'(&S)'
        OnClick = menuSaveAsClick
      end
      object menuZlogSpc: TMenuItem
        Caption = 'ZLOG.SPC'#12392#12375#12390#20445#23384
        OnClick = menuZlogSpcClick
      end
      object N3: TMenuItem
        Caption = '-'
      end
      object menuExit: TMenuItem
        Caption = #32066#20102'(&X)'
        OnClick = menuExitClick
      end
    end
    object menuNotice: TMenuItem
      Caption = #21033#29992#19978#12398#27880#24847#20107#38917'(&H)'
      OnClick = menuNoticeClick
    end
  end
  object OpenFileDialog: TOpenDialog
    Left = 756
    Top = 4
  end
  object SaveFileDialog: TSaveDialog
    Filter = #12486#12461#12473#12488#12501#12449#12452#12523'(*.txt)|*.txt|'#12459#12531#12510#21306#20999#12426#12501#12449#12452#12523'(*.csv)|*.csv|'#20840#12390#12398#12501#12449#12452#12523'(*.*)|*.*'
    Left = 784
    Top = 4
  end
  object SaveFileDialog2: TSaveDialog
    Filter = 'SPC'#12501#12449#12452#12523'|*.SPC|'#20840#12390#12398#12501#12449#12452#12523'(*.*)|*.*'
    Left = 808
    Top = 4
  end
  object SaveFileDialog3: TSaveDialog
    Left = 836
    Top = 4
  end
  object NetHTTPClient1: TNetHTTPClient
    UserAgent = 'Embarcadero URI Client/1.0'
    Left = 352
    Top = 60
  end
  object NetHTTPRequest1: TNetHTTPRequest
    Client = NetHTTPClient1
    Left = 400
    Top = 60
  end
end
