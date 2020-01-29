VERSION 5.00
Begin VB.Form frmQueens 
   Caption         =   "Queens"
   ClientHeight    =   8925
   ClientLeft      =   4125
   ClientTop       =   540
   ClientWidth     =   11385
   LinkTopic       =   "Form1"
   PaletteMode     =   1  'UseZOrder
   ScaleHeight     =   8925
   ScaleWidth      =   11385
   Begin VB.TextBox txtSolNo 
      BackColor       =   &H00C0C0C0&
      BorderStyle     =   0  'None
      BeginProperty Font 
         Name            =   "Arial"
         Size            =   12
         Charset         =   238
         Weight          =   400
         Underline       =   0   'False
         Italic          =   0   'False
         Strikethrough   =   0   'False
      EndProperty
      ForeColor       =   &H00404040&
      Height          =   405
      Left            =   3120
      TabIndex        =   5
      Top             =   195
      Width           =   2940
   End
   Begin VB.TextBox textSpecNo 
      BeginProperty Font 
         Name            =   "Arial"
         Size            =   12
         Charset         =   238
         Weight          =   400
         Underline       =   0   'False
         Italic          =   0   'False
         Strikethrough   =   0   'False
      EndProperty
      Height          =   420
      Left            =   780
      TabIndex        =   3
      Top             =   1365
      Width           =   795
   End
   Begin VB.PictureBox pctQueens 
      Height          =   8010
      Left            =   3120
      ScaleHeight     =   7950
      ScaleWidth      =   7950
      TabIndex        =   2
      Top             =   780
      Width           =   8010
   End
   Begin VB.CommandButton cmdClose 
      Caption         =   "Close"
      BeginProperty Font 
         Name            =   "Arial"
         Size            =   12
         Charset         =   238
         Weight          =   400
         Underline       =   0   'False
         Italic          =   0   'False
         Strikethrough   =   0   'False
      EndProperty
      Height          =   795
      Left            =   390
      TabIndex        =   1
      Top             =   3900
      Width           =   1770
   End
   Begin VB.CommandButton cmdNext 
      Caption         =   "Next solution"
      BeginProperty Font 
         Name            =   "Arial"
         Size            =   12
         Charset         =   238
         Weight          =   400
         Underline       =   0   'False
         Italic          =   0   'False
         Strikethrough   =   0   'False
      EndProperty
      Height          =   795
      Left            =   390
      TabIndex        =   0
      Top             =   2925
      Width           =   1770
   End
   Begin VB.Label askNoQueens 
      Caption         =   "How many queens?"
      BeginProperty Font 
         Name            =   "Arial"
         Size            =   12
         Charset         =   238
         Weight          =   400
         Underline       =   0   'False
         Italic          =   0   'False
         Strikethrough   =   0   'False
      EndProperty
      Height          =   405
      Left            =   390
      TabIndex        =   4
      Top             =   780
      Width           =   2550
   End
End
Attribute VB_Name = "frmQueens"
Attribute VB_GlobalNameSpace = False
Attribute VB_Creatable = False
Attribute VB_PredeclaredId = True
Attribute VB_Exposed = False
Dim nQueens As Long       'number of queens
Dim nSol As Long          'index of solution
Dim nActqid As Long       'actual query identifier
Dim nQueryOpen As Boolean 'there is an open query

Private Sub draw_grid(ByVal N As Long)

    pctQueens.Cls

    xWidth = pctQueens.Width
    xHeight = pctQueens.Height

    xWidth0 = xWidth / N
    xHeight0 = xHeight / N

    'draw vertical lines
    For i = 1 To N - 1 Step 1
        pctQueens.Line (i * xWidth0, 0)-(i * xWidth0, xHeight), RGB(0, 0, 255)
    Next i

    'draw horizontal lines
    For i = 1 To N - 1 Step 1
        pctQueens.Line (0, i * xHeight0)-(xWidth, i * xHeight0), RGB(0, 0, 255)
    Next i

End Sub

Private Sub draw_circle(ByVal i As Long, ByVal j As Long, ByVal N As Long)

    xWidth = pctQueens.Width / N
    xHeight = pctQueens.Height / N

    X = (j - 1) * xWidth + xWidth / 2
    Y = (i - 1) * xWidth + xWidth / 2
    r = xWidth * 0.4

    pctQueens.FillStyle = 0
    pctQueens.FillColor = RGB(0, 0, 255)

    pctQueens.Circle (X, Y), r, RGB(0, 0, 255)

End Sub


Private Sub cmdClose_Click()
    Unload Me
End Sub


Private Sub cmdNext_Click()
    Dim nPos As Long
    Dim aPos(100) As Long

    If Not nQueryOpen Then
        MsgBox "Specify number of queens first!", 48, ""
        Exit Sub
    End If
    If PrologNextSolution(nActqid) < 1 Then
        MsgBox "No more solutions!", 48, ""
    Else
        For i = 1 To nQueens Step 1
           If PrologGetLong(nActqid, "X" & i, nPos) = 1 Then
               aPos(i - 1) = nPos
           End If
        Next i

        'display nth solution
        txtSolNo = "Solution number: " & Str(nSol)
        Call draw_grid(nQueens)

        nLine = 1
        For Each xElem In aPos
            Call draw_circle(nLine, xElem, nQueens)
            nLine = nLine + 1
        Next

        nSol = nSol + 1

    End If

End Sub

Private Sub Form_Load()
    nQueens = 0
    nSol = 1
    nQueryOpen = False

    'initialize Prolog
    If PrologInit() <> 1 Then GoTo Err
    'Load queens.pl
    If PrologQueryCutFail("load_files(app(queens))") <> 1 Then GoTo Err
    Exit Sub

Err:
    MsgBox "Prolog initialization failed", 48, "Error"
    Unload Me
End Sub

Private Sub textSpecNo_Change()
    nQueens = Val(textSpecNo)
    nSol = 1

    If nQueryOpen Then PrologCloseQuery (nActqid)

    'create Prolog query in form: queens(4,[X1,X2,X3,X4])
   
    Q = "queens(" & Str(nQueens) & ", ["
    For i = 1 To nQueens - 1 Step 1
        Q = Q & "X" & i & ","
    Next
    Q = Q & "X" & nQueens & "])"
    
    nActqid = PrologOpenQuery(Q)
    nQueryOpen = True
End Sub




