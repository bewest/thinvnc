object WebServForm: TWebServForm
  Left = 250
  Top = 155
  BorderStyle = bsDialog
  Caption = 'ThinVNC Server'
  ClientHeight = 258
  ClientWidth = 320
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = True
  Position = poDesktopCenter
  OnClose = FormClose
  OnCloseQuery = FormCloseQuery
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  PixelsPerInch = 96
  TextHeight = 13
  object ToolsPanel: TPanel
    Left = 0
    Top = 0
    Width = 320
    Height = 239
    Align = alClient
    BevelOuter = bvNone
    TabOrder = 0
    object Label3: TLabel
      Left = 25
      Top = 181
      Width = 19
      Height = 13
      Caption = 'Port'
    end
    object StartButton: TButton
      Left = 143
      Top = 202
      Width = 75
      Height = 25
      Action = actStart
      TabOrder = 0
    end
    object StopButton: TButton
      Left = 222
      Top = 202
      Width = 75
      Height = 25
      Action = actStop
      TabOrder = 1
    end
    object edPort: TEdit
      Left = 49
      Top = 177
      Width = 53
      Height = 21
      TabOrder = 2
      Text = 'edPort'
    end
    object GroupBox1: TGroupBox
      Left = 24
      Top = 20
      Width = 273
      Height = 143
      Caption = 'Authentication'
      TabOrder = 3
      object lblPassword: TLabel
        Left = 48
        Top = 96
        Width = 49
        Height = 13
        Caption = 'Password:'
      end
      object lblUser: TLabel
        Left = 48
        Top = 68
        Width = 25
        Height = 13
        Caption = 'User:'
      end
      object rbAuthDigest: TRadioButton
        Left = 112
        Top = 31
        Width = 89
        Height = 17
        Caption = 'Digest'
        TabOrder = 1
      end
      object rbAuthNone: TRadioButton
        Left = 37
        Top = 31
        Width = 69
        Height = 17
        Caption = 'None'
        Checked = True
        TabOrder = 2
        TabStop = True
      end
      object rbAuthNTLM: TRadioButton
        Left = 192
        Top = 31
        Width = 89
        Height = 17
        Caption = 'NTLM'
        TabOrder = 0
      end
      object edPassword: TEdit
        Left = 112
        Top = 92
        Width = 121
        Height = 21
        PasswordChar = '*'
        TabOrder = 3
      end
      object edUser: TEdit
        Left = 112
        Top = 65
        Width = 121
        Height = 21
        TabOrder = 4
      end
    end
    object chkAutoStart: TCheckBox
      Left = 200
      Top = 179
      Width = 97
      Height = 17
      Caption = 'Auto-start'
      TabOrder = 4
    end
  end
  object StatusBar1: TStatusBar
    Left = 0
    Top = 239
    Width = 320
    Height = 19
    Panels = <
      item
        Width = 200
      end>
  end
  object HttpServer1: THttpServer
    ListenBacklog = 5
    Port = '80'
    Addr = '0.0.0.0'
    MaxClients = 0
    DocDir = 'c:\wwwroot'
    TemplateDir = '\'
    DefaultDoc = 'index.html'
    LingerOnOff = wsLingerNoSet
    LingerTimeout = 1
    Options = [hoContentEncoding]
    KeepAliveTimeSec = 10
    MaxRequestsKeepAlive = 100
    SizeCompressMin = 500
    SizeCompressMax = 5000000
    OnServerStarted = HttpServer1ServerStarted
    OnServerStopped = HttpServer1ServerStopped
    OnGetDocument = HttpServer1GetDocument
    OnAuthGetPassword = HttpServer1AuthGetPassword
    OnAuthResult = HttpServer1AuthResult
    OnAuthGetType = HttpServer1AuthGetType
    OnAuthNtlmBeforeValidate = HttpServer1AuthNtlmBeforeValidate
    AuthTypes = []
    AuthRealm = 'ics'
    Left = 290
    Top = 75
  end
  object TrayIcon1: TTrayIcon
    PopupMenu = PopupMenu1
    OnDblClick = TrayIcon1DblClick
    Left = 288
    Top = 112
  end
  object PopupMenu1: TPopupMenu
    Left = 296
    Top = 152
    object Start1: TMenuItem
      Action = actStart
    end
    object Stop1: TMenuItem
      Action = actStop
    end
    object N1: TMenuItem
      Caption = '-'
    end
    object Exit1: TMenuItem
      Action = actExit
    end
  end
  object ActionList1: TActionList
    Left = 296
    Top = 24
    object actStart: TAction
      Caption = 'Start'
      OnExecute = actStartExecute
    end
    object actStop: TAction
      Caption = 'Stop'
      Enabled = False
      OnExecute = actStopExecute
    end
    object actExit: TAction
      Caption = 'Exit'
      OnExecute = actExitExecute
    end
  end
end
