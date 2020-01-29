Attribute VB_Name = "VBSP"
' The VB - Prolog interface declarations
' **************************************

'-------------------------------------------------------------------
' Loads and Initiates the interface. Returns 1 on success
' Public Function PrologInit() As Long
'   Defined in VB following declarations

Private Declare Function VBSPPrologInit Lib "VBSP" Alias "_PrologInit@4" _
(ByVal AppPath As String) As Integer

'-------------------------------------------------------------------
' Opens a query for successive retrival of solutions
' Goal is the textual repr. of the Prolog goal
' Return a query identifier
'
Declare Function PrologOpenQuery Lib "VBSP" Alias "_PrologOpenQuery@4" _
(ByVal Goal As String) As Long

'-------------------------------------------------------------------
' Retrives a solution to the open query represented by the query
' identifier - QID. Returns 1 on success, 0 on failure, -1 on error
'
Declare Function PrologNextSolution Lib "VBSP" Alias "_PrologNextSolution@4" _
(ByVal qid As Long) As Integer

'-------------------------------------------------------------------
' Closes the query represented by QID
'
Declare Sub PrologCloseQuery Lib "VBSP" Alias "_PrologCloseQuery@4" _
(ByVal qid As Long)

'-------------------------------------------------------------------
' Runs a query for side effects
'
Declare Function PrologQueryCutFail Lib "VBSP" Alias "_PrologQueryCutFail@4" _
(ByVal Goal As String) As Integer

'-------------------------------------------------------------------
' Retrieves the value bound to a variable of a goal, as an integer.
' Returns 1 on success, < 1 if the value is not an integer.
'
Declare Function PrologGetLong Lib "VBSP" Alias "_PrologGetLong@12" _
(ByVal qid As Long, ByVal VarName As String, Value As Long) As Integer

'-------------------------------------------------------------------
' Retrieves the value bound to a variable of a goal, as a string. Conversion
' uses Prolog write/2.
' Returns 1 on success

Declare Function PrologGetString Lib "VBSP" Alias "_PrologGetString@12" _
(ByVal qid As Long, ByVal VarName As String, Value As String) As Integer

'-------------------------------------------------------------------
' Same as PrologGetString but conversion uses Prolog writeq/2.
' Returns 1 on success.

Declare Function PrologGetStringQuoted Lib "VBSP" Alias "_PrologGetStringQuoted@12" _
(ByVal qid As Long, ByVal VarName As String, Value As String) As Integer

'-------------------------------------------------------------------
' If called after a PrologQueryCutFail or PrologNextSolution returned < 0
' this will return the Prolog exception term converted to a string
'
Declare Sub PrologGetException Lib "VBSP" Alias "_PrologGetException@4" _
(Value As String)

'-------------------------------------------------------------------
Public Function PrologInit() As Long
    PrologInit = VBSPPrologInit(App.Path)
End Function


'-------------------------------------------------------------------
'Example
'  Dim result As String
'  Dim qid As Long
'
'  qid = PrologOpenQuery("search(a(1),X)")
'  Do
'    If PrologNextSolution(qid) = 0
'      Exit Do
'    PrologGetString(qid, "X", result)
'    myList.AddItem = result
'  Loop While True
'  PrologCloseQuery(qid)



