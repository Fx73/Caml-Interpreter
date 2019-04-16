Imports System.Text
Imports System.IO

#Region "Mes Classes de variables"
Public Class Lexeme
    Enum VarType
        Generic
        Lint
        Lfloat
        Lmathop
        Llogop
        Lchar
        Lstring
        Lvar
        Lmot
        Lbool
    End Enum

    Public value As String
    Public type As VarType
    Public node As noeud

    Public Class noeud
        Public lex As Lexeme
        Public suiv As noeud
    End Class

    Sub New(ByVal mavaleur As String, montype As VarType)
        value = mavaleur
        type = montype
    End Sub
End Class

Public Class VarPerso
    Public nom As String
    Public value As Object
    Public suiv As VarPerso
    Public Sub New(newnom As String, Optional ByVal mavaleur As Object = Nothing)
        nom = newnom
        value = mavaleur
    End Sub
End Class
Public Class Vint : Inherits VarPerso
    Public Shadows value As Integer
    Public Shadows suiv As Vint
    Public Sub New(newnom As String, Optional mavaleur As Integer = Nothing)
        MyBase.New(newnom, mavaleur)
    End Sub
End Class
Public Class Vfloat : Inherits VarPerso
    Public Shadows value As Double
    Public Shadows suiv As Vfloat
    Public Sub New(newnom As String, Optional mavaleur As Double = Nothing)
        MyBase.New(newnom, mavaleur)
    End Sub
End Class
Public Class Vbool : Inherits VarPerso
    Public Shadows value As Boolean
    Public Shadows suiv As Vbool
    Public Sub New(newnom As String, Optional mavaleur As Boolean = Nothing)
        MyBase.New(newnom, mavaleur)
    End Sub
End Class
Public Class Vchar : Inherits VarPerso
    Public Shadows value As Char
    Public Shadows suiv As Vchar
    Public Sub New(newnom As String, Optional mavaleur As Char = Nothing)
        MyBase.New(newnom, mavaleur)
    End Sub
End Class
Public Class Vstring : Inherits VarPerso
    Public Shadows value As String
    Public Shadows suiv As Vstring
    Public Sub New(newnom As String, Optional mavaleur As String = Nothing)
        MyBase.New(newnom, mavaleur)
    End Sub
End Class
#End Region
Public Class Form1
    'Mes Constantes utilitaires
    Public path = Application.StartupPath() + "\Default.ml"
    Const QUOTE = """"

    'Mes Chaines de stocks de Variables
    Public variables As VarPerso
    Public phrase As Lexeme = New Lexeme("DEBUT", Lexeme.VarType.Lstring)
    Public courant As Lexeme = phrase
    Public result
    'Ma Grammaire
    Public GrammaireMots() As String = New String() {"(", ")", "let", "true", "false"}

    Enum ETAT As Int16
        PHRASE
        EXPR
        CONS
        LINT
        LFLOAT
        LBOOL
        LCHAR
        LSTRING
        LMOT
        LVAR
        LOP
    End Enum

    Public GrammaireMathOperateurs() As Char = New Char() {"+", "-", "*", "/", "<", ">", "="}
    Public GrammaireLogicOperateurs() As Char = New Char() {"<", ">", "=", "&", "|"}


#Region "Description Fonctionnelle"
    Private Sub BOpen_Click(sender As Object, e As EventArgs) Handles BOpen.Click
        If Not My.Computer.FileSystem.FileExists(path) Then
            Try
                My.Computer.FileSystem.WriteAllText(path, "", False)
            Catch ex As Exception
                BoxSortie.AppendText(ex.Message + vbCrLf)
            End Try
            BoxSortie.AppendText("Fichier créé : " + path + vbCrLf)
        End If
        Try
            Process.Start(path)
        Catch ex As Exception
            BoxSortie.AppendText(ex.Message + vbCrLf)
        End Try
    End Sub
    Private Sub EnterPressed(sender As Object, e As KeyEventArgs) Handles Me.KeyDown
        If e.KeyCode = Keys.Enter Then
            BExec_Click()
        End If
    End Sub
    Private Sub BExec_Click() Handles BExec.Click
        Dim text = BoxEditeur.Text
        If Not (Suppr_comment(text)) Then
            Exit Sub
        End If

        Dim start = 0
        For i = 0 To text.Length - 2
            If text(i) = ";" And text(i + 1) = ";" Then
                Lecture(text.Substring(start, i + 2 - start), start)
                i += 2
                start = i
            End If
        Next

    End Sub
#End Region

#Region "Interpretation"
    Private Function Suppr_comment(ByRef s As String) As Boolean
        Dim sb As New StringBuilder
        Dim skip = 0

        If s(0) = "(" And s(1) = "*" Then
            skip = 1
        End If

        Dim text = s.Split(New String() {"(*"}, StringSplitOptions.RemoveEmptyEntries)
        For i = 0 To text.Length - 1
            While (text(i).IndexOf("*)") <> -1)
                Dim a = text(i).IndexOf("*)") + 2
                text(i) = text(i).Substring(a)
                skip -= 1
                If skip < 0 Then
                    BoxSortie.AppendText("Erreur, fin de commentaire en trop" + vbCrLf)
                    Return False
                End If
            End While
            If skip = 0 Then
                sb.Append(text(i))
            End If
            skip += 1
        Next
        If skip > 1 Then
            BoxSortie.AppendText("Attention,fin de commentaires manquantes" + vbCrLf)
        End If

        s = sb.ToString
        Return True
    End Function

    Private Sub Lecture(s As String, start As Integer)
        If Not Decoupage(s, phrase) Then
            Exit Sub
        End If





    End Sub

    Private Function Lire(state As ETAT) As Object
        Select Case (state)

            Case ETAT.PHRASE
                If courant.value = "let" Then
                    Dim var As VarPerso = Lire(ETAT.LVAR)
                    If var Is Nothing Then
                        Return False
                    End If
                    If courant.value <> "=" Then
                        Return False
                    Else
                        Avancer()
                    End If
                    Dim value = Lire(ETAT.EXPR)
                    If value Is Nothing Then
                        Return False
                    End If
                    var.value = value
                    Return True
                Else
                    Return Lire(ETAT.EXPR)
                End If

            Case ETAT.EXPR

            Case ETAT.LBOOL

            Case ETAT.LVAR
                If courant.type = Lexeme.VarType.Lvar Then
                    Return EmpileVar(courant.value)
                Else
                    Erreur("NOM DE VARIABLE CORRECT ATTENDU")
                End If
        End Select
    End Function

    Private Function Decoupage(ByRef s As String, l As Lexeme) As Lexeme.VarType
        For i = 0 To s.Length - 1
            'PARENTHESE
            If s(i) = "(" Then
                i += 1
                Dim fin = Findparenthese(s.Substring(i))
                Dim lex = AddLexeme(l, "()")
                Dim atype = Decoupage(s.Substring(i, fin), lex)
                lex.type = atype
                i += fin + 1
                'OPERATEURS MATHEMATIQUES
            ElseIf EstMathOp(s(i)) Then
                If OpePrio(s(i)) Then
                    Dim lex = AddLexeme(l)
                    Dim atype = Decoupage(s.Substring(0, i), lex)
                    lex.type = atype
                    AddLexeme(l, s(i), Lexeme.VarType.Lmathop)
                    lex = AddLexeme(l)
                    atype = Decoupage(s.Substring(0, i), lex)
                    lex.type = atype
                Else

                End If







                'ENTIER et FLOAT
            ElseIf EstInt(s(i)) Then
                    Dim j = 1
                    While (EstInt(s(i + j)))
                        j += 1
                    End While

                    If (s(i + j) = ".") Then
                        j += 1
                        While (EstInt(s(i + j)))
                            j += 1
                        End While
                        AddLexeme(s.Substring(i, j), Lexeme.VarType.Lfloat)

                    ElseIf (s(i + j) = " " Or s(i + j) = ";" Or EstMathOp(s(i + j)) Or EstPar(s(i + j))) Then
                        AddLexeme(s.Substring(i, j), Lexeme.VarType.Lint)

                    ElseIf (EstVar(s(i + j))) Then
                        While (EstVar(s(i + j)))
                            j += 1
                        End While
                        AddLexeme(s.Substring(i, j), Lexeme.VarType.Lvar)

                    Else
                        Erreur("NUMBER ")
                        Return False
                    End If


                ElseIf s(i) = "'" Then
                    If s(i + 2) = "'" Then
                        AddLexeme(s(i + 1), Lexeme.VarType.Lchar)
                        i += 3
                    Else
                        Erreur("CHAR ")
                        Return False
                    End If
                    'STRING
                ElseIf s(i) = """" Then
                    Dim j = 1
                    While (s(i + j) <> """")
                        If (i + j >= s.Length - 1) Then
                            Erreur("STRING ")
                            Return False
                        End If
                        j += 1
                    End While
                    AddLexeme(s.Substring(i + 1, j), Lexeme.VarType.Lstring)
                    'NOM DE VAR
                ElseIf EstVar(s(i)) Then
                    Dim j = 1
                    While (EstVar(s(i + j)))
                        j += 1
                    End While
                    If EstMot(s.Substring(i, j)) Then
                        AddLexeme(s.Substring(i, j), Lexeme.VarType.Lmot)
                    Else
                        AddLexeme(s.Substring(i, j), Lexeme.VarType.Lvar)
                    End If
                Else
                    Erreur("DEBUT DE LEXEME INCONNU ")
                Return False
            End If
        Next
        Return True
    End Function

#End Region

#Region "Fonctions manipulation de variables"
    Private Function EmpileVar(nom As String, Optional type As Lexeme.VarType = Nothing, Optional valeur As Object = Nothing)
        Dim var = SearchVar(nom)
        If var Is Nothing Then
            var = variables
            While var.suiv IsNot Nothing
                var = var.suiv
            End While
            Select Case type
                Case Lexeme.VarType.Lint
                    var.suiv = New Vint(nom, valeur)
                Case Lexeme.VarType.Lfloat
                    var.suiv = New Vfloat(nom, valeur)
                Case Lexeme.VarType.Lbool
                    var.suiv = New Vbool(nom, valeur)
                Case Lexeme.VarType.Lchar
                    var.suiv = New Vchar(nom, valeur)
                Case Lexeme.VarType.Lstring
                    var.suiv = New Vstring(nom, valeur)
                Case Else
                    var.suiv = New VarPerso(nom, valeur)
            End Select
            var = var.suiv
        End If
        Return var
    End Function

    Private Function AddLexeme(root As Lexeme, Optional valeur As String = Nothing, Optional type As Lexeme.VarType = Lexeme.VarType.Generic) As Lexeme
        Dim l = NewNode(root)
        l.lex = New Lexeme(valeur, type)
        Return l.lex
    End Function

    Private Function NewNode(ByRef l As Lexeme) As Lexeme.noeud
        If l.node Is Nothing Then
            l.node = New Lexeme.noeud
            Return l.node
        Else
            Dim last = l.node
            While last.suiv IsNot Nothing
                last = last.suiv
            End While
            last.suiv = New Lexeme.noeud
            Return last.suiv
        End If
    End Function

    Private Function Avancer()
    End Function

    Private Function SearchVar(s As String)
        Dim index As VarPerso
        index = Variables
        While index IsNot Nothing
            If index.nom = s Then
                Return index
            End If
            index = index.suiv
        End While
        Return Nothing
    End Function


#End Region

#Region "Fonctions utilitaires"
    Private Function Findparenthese(s As String) As Integer
        Dim compt = 1
        For i = 0 To s.Length - 1
            If s(i) = "(" Then
                compt += 1
            End If
            If s(i) = ")" Then
                compt -= 1
            End If
            If compt = 0 Then
                Return i
            End If
        Next
        Erreur("Parenthese droite manquante")
        Return s.Length - 1
    End Function

    Private Function Findopeprio(s As String) As Integer
        For i = 0 To s.Length - 1
            If OpePrio(s(i)) Then
                Return i
            End If
            If s(i) = "(" Then
                i += Findparenthese(s.Substring(i + 1))
            End If
            If s(i) = "=" Then

            End If
        Next
    End Function

    Private Function EstChar(s As Char) As Boolean
        Return ((s > "a" And s < "z") Or (s > "A" And s < "Z"))
    End Function
    Private Function EstInt(s As Char) As Boolean
        Return (s > "9" And s < "0")
    End Function
    Private Function EstMathOp(s As Char) As Boolean
        For Each e In GrammaireMathOperateurs
            If e = s Then
                Return True
            End If
        Next
        Return False
    End Function
    Private Function EstLogicOp(s As Char) As Boolean
        For Each e In GrammaireLogicOperateurs
            If e = s Then
                Return True
            End If
        Next
        Return False
    End Function
    Private Function EstVar(s As Char)
        If EstInt(s) Or EstChar(s) Or s = "_" Then
            Return True
        End If
        Return False
    End Function
    Private Function EstMot(s As String) As Boolean
        For Each e In GrammaireMots
            If e = s Then
                Return True
            End If
        Next
        Return False
    End Function
    Private Function EstPar(s As Char)
        Return s = "(" Or s = ")"
    End Function
    Private Function OpePrio(s As Char)
        Return s = "*" Or s = "/"
    End Function
    Private Function Erreur(s As String)
        BoxSortie.AppendText("ERREUR : " + s + vbCrLf)
    End Function
#End Region
End Class
