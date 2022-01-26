object formImpotantNotice: TformImpotantNotice
  Left = 0
  Top = 0
  BorderStyle = bsDialog
  Caption = 'Impotant Notice'
  ClientHeight = 201
  ClientWidth = 338
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -15
  Font.Name = #65325#65331' '#65328#12468#12471#12483#12463
  Font.Style = []
  OldCreateOrder = False
  Position = poOwnerFormCenter
  DesignSize = (
    338
    201)
  PixelsPerInch = 96
  TextHeight = 15
  object buttonOK: TButton
    Left = 128
    Top = 160
    Width = 89
    Height = 33
    Anchors = [akLeft, akBottom]
    Caption = 'OK'
    Default = True
    ModalResult = 1
    TabOrder = 0
    ExplicitTop = 129
  end
  object GroupBox1: TGroupBox
    Left = 8
    Top = 8
    Width = 321
    Height = 145
    Caption = #21033#29992#19978#12398#27880#24847#20107#38917
    TabOrder = 1
    object Label1: TLabel
      Left = 16
      Top = 24
      Width = 289
      Height = 81
      AutoSize = False
      Caption = 
        #12371#12398#12469#12540#12499#12473#65288#12477#12501#12488#12454#12455#12450#65289#12399#12289#32207#21209#30465' '#38651#27874#21033#29992#12507#12540#12512#12506#12540#12472#12398'Web-API '#27231#33021#12434#21033#29992#12375#12390#21462#24471#12375#12383#24773#22577#12434#12418#12392#12395#20316#25104#12375#12390#12356#12427#12364#12289 +
        #12469#12540#12499#12473#12398#20869#23481#12399#32207#21209#30465#12395#12424#12387#12390#20445#35388#12373#12428#12383#12418#12398#12391#12399#12354#12426#12414#12379#12435#12290
      WordWrap = True
    end
    object Label2: TLabel
      Left = 28
      Top = 119
      Width = 277
      Height = 15
      AutoSize = False
      Caption = 'Web-API'#21033#29992#35215#32004#31532#65299#26465#12395#22522#12389#12367#34920#31034#12391#12377#12290
    end
  end
end
