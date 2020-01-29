VERSION 5.00
Begin VB.Form train 
   Caption         =   "Train"
   ClientHeight    =   4740
   ClientLeft      =   3360
   ClientTop       =   2595
   ClientWidth     =   6435
   LinkTopic       =   "Form1"
   PaletteMode     =   1  'UseZOrder
   ScaleHeight     =   4740
   ScaleWidth      =   6435
   Begin VB.CommandButton cmdQuit 
      Caption         =   "Quit"
      BeginProperty Font 
         Name            =   "Arial"
         Size            =   12
         Charset         =   238
         Weight          =   400
         Underline       =   0   'False
         Italic          =   0   'False
         Strikethrough   =   0   'False
      EndProperty
      Height          =   615
      Left            =   480
      TabIndex        =   2
      Top             =   2760
      Width           =   1575
   End
   Begin VB.CommandButton cmdRun 
      Caption         =   "Run"
      BeginProperty Font 
         Name            =   "Arial"
         Size            =   12
         Charset         =   238
         Weight          =   400
         Underline       =   0   'False
         Italic          =   0   'False
         Strikethrough   =   0   'False
      EndProperty
      Height          =   615
      Left            =   480
      TabIndex        =   1
      Top             =   480
      Width           =   1575
   End
   Begin VB.ListBox listConnections 
      BeginProperty Font 
         Name            =   "Arial"
         Size            =   12
         Charset         =   238
         Weight          =   400
         Underline       =   0   'False
         Italic          =   0   'False
         Strikethrough   =   0   'False
      EndProperty
      Height          =   3570
      Left            =   2730
      TabIndex        =   0
      Top             =   390
      Width           =   3015
   End
End
Attribute VB_Name = "train"
Attribute VB_GlobalNameSpace = False
Attribute VB_Creatable = False
Attribute VB_PredeclaredId = True
Attribute VB_Exposed = False

Private Sub cmdQuit_Click()
    Unload Me
End Sub

Private Sub cmdRun_Click()
    Dim qid As Long
    Dim result As String
    Dim s As String
    Dim rc As Integer
    
    qid = -1 ' make it safe to PrologCloseQuery(qid) in Err:
    
    'load the train.pl Prolog file
    rc = PrologQueryCutFail("ensure_loaded(app(train))")
    If rc < 1 Then
        Msg = "ensure_loaded(train)"
        GoTo Err
    End If
    'open the query
    qid = PrologOpenQuery("places('Stockholm','Orebro',Way)")
    If qid = -1 Then
        rc = 0
        Msg = "Open places/3"
        GoTo Err
    End If
    'generate solutions
    Do
        rc = PrologNextSolution(qid)
        If rc = 0 Then Exit Do ' failed
        If rc < 0 Then
            Msg = "places/3"
            GoTo Err
        End If
        If PrologGetString(qid, "Way", result) < 1 Then
            rc = 0
            Msg = "PrologGetString Way"
            GoTo Err
        End If
        listConnections.AddItem result
    Loop While True
    'after all solutions are found, the query is closed
    Call PrologCloseQuery(qid)
    Exit Sub
    
Err:
    Call PrologCloseQuery(qid) ' Always close opened queries
    
    'error message is prepared, adding either the - failed - or
    'the - raised exception - suffix to the Msg string specific
    'to the function called
    If rc = 0 Then
         Msg = Msg + " failed"
    Else
        Call PrologGetException(s)
        Msg = Msg + " raised exception: " + s
    End If
    MsgBox Msg, 48, "Error"
End Sub

Private Sub Form_Load()
    If PrologInit() <> 1 Then
        MsgBox "PrologInit failed", 48, "Error"
        Unload Me
    End If
End Sub

Private Sub listConnections_Click()

End Sub
