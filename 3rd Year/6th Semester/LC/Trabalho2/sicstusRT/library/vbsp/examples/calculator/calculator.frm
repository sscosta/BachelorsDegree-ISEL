VERSION 5.00
Begin VB.Form Calculate 
   Caption         =   "Calculate arithmetic expressions"
   ClientHeight    =   1755
   ClientLeft      =   1350
   ClientTop       =   2700
   ClientWidth     =   6165
   BeginProperty Font 
      Name            =   "Arial"
      Size            =   12
      Charset         =   238
      Weight          =   400
      Underline       =   0   'False
      Italic          =   0   'False
      Strikethrough   =   0   'False
   EndProperty
   LinkTopic       =   "Form1"
   PaletteMode     =   1  'UseZOrder
   ScaleHeight     =   1755
   ScaleWidth      =   6165
   Begin VB.CommandButton cmdCalc 
      Caption         =   "Calculate"
      BeginProperty Font 
         Name            =   "MS Sans Serif"
         Size            =   9.75
         Charset         =   0
         Weight          =   400
         Underline       =   0   'False
         Italic          =   0   'False
         Strikethrough   =   0   'False
      EndProperty
      Height          =   405
      Left            =   3315
      TabIndex        =   5
      Top             =   1170
      Width           =   1380
   End
   Begin VB.TextBox txtValue 
      BeginProperty Font 
         Name            =   "MS Sans Serif"
         Size            =   9.75
         Charset         =   0
         Weight          =   400
         Underline       =   0   'False
         Italic          =   0   'False
         Strikethrough   =   0   'False
      EndProperty
      Height          =   390
      Left            =   3900
      TabIndex        =   2
      Top             =   585
      Width           =   1965
   End
   Begin VB.TextBox txtExpr 
      BeginProperty Font 
         Name            =   "MS Sans Serif"
         Size            =   9.75
         Charset         =   0
         Weight          =   400
         Underline       =   0   'False
         Italic          =   0   'False
         Strikethrough   =   0   'False
      EndProperty
      Height          =   390
      Left            =   240
      TabIndex        =   1
      Top             =   600
      Width           =   3285
   End
   Begin VB.CommandButton cmdQuit 
      Caption         =   "Quit"
      BeginProperty Font 
         Name            =   "MS Sans Serif"
         Size            =   9.75
         Charset         =   0
         Weight          =   400
         Underline       =   0   'False
         Italic          =   0   'False
         Strikethrough   =   0   'False
      EndProperty
      Height          =   420
      Left            =   4875
      TabIndex        =   0
      Top             =   1170
      Width           =   990
   End
   Begin VB.Label Label2 
      Caption         =   "Value:"
      BeginProperty Font 
         Name            =   "MS Sans Serif"
         Size            =   9.75
         Charset         =   0
         Weight          =   400
         Underline       =   0   'False
         Italic          =   0   'False
         Strikethrough   =   0   'False
      EndProperty
      Height          =   255
      Left            =   3900
      TabIndex        =   4
      Top             =   195
      Width           =   1695
   End
   Begin VB.Label Label1 
      Caption         =   "Enter expression:"
      BeginProperty Font 
         Name            =   "MS Sans Serif"
         Size            =   9.75
         Charset         =   0
         Weight          =   400
         Underline       =   0   'False
         Italic          =   0   'False
         Strikethrough   =   0   'False
      EndProperty
      Height          =   255
      Left            =   240
      TabIndex        =   3
      Top             =   195
      Width           =   3090
   End
End
Attribute VB_Name = "Calculate"
Attribute VB_GlobalNameSpace = False
Attribute VB_Creatable = False
Attribute VB_PredeclaredId = True
Attribute VB_Exposed = False

Private Sub cmdQuit_Click()
    Unload Me
End Sub

Public Function calculate(ByVal Expr As String) As String
    Dim qid As Long
    Dim result As String
    Dim ret As Long
    Dim Q As String

    Q = "prolog_calculate(" & Expr & ",Value)"
    qid = PrologOpenQuery(Q)
    If qid = -1 Then GoTo Err ' e.g., syntax error

    ret = PrologNextSolution(qid)
    If ret <> 1 Then GoTo Err ' failed or error

    ret = PrologGetString(qid, "Value", result)
    If ret <> 1 Then GoTo Err
    calculate = result
    Call PrologCloseQuery(qid)

    Exit Function

Err:
    MsgBox "Bad expression", 48, "Error!"
    calculate = ""
End Function

Private Sub cmdCalc_Click()
    txtValue.Text = calculate(txtExpr)
End Sub

Private Sub Form_Load()
    If PrologInit() <> 1 Then GoTo Err
    If PrologQueryCutFail("ensure_loaded(app(calc))") <> 1 Then GoTo Err
    Exit Sub

Err:
    MsgBox "Prolog initialization failed", 48, "Error"
    Unload Me
End Sub

