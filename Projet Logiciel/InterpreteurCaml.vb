Imports System.Text
Imports System.IO

#Region "Mes Classes de variables"
Public Class Lexeme
    Enum LType
        Generic
        Phrase
        Expr
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
    Public type As LType
    Public node As noeud

    Public Class noeud
        Public lex As Lexeme
        Public suiv As noeud
    End Class

    Sub New(ByVal mavaleur As String, montype As LType)
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
    Public stock As VarPerso
    Public phrase As Lexeme = New Lexeme("DEBUT", Lexeme.LType.Lstring)
    Public result
    'Ma Grammaire
    Public GrammaireMots() As String = New String() {"let", "true", "false"}

    Enum ETAT As Int16
        PHRASE
        EXPR
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
    Public GrammaireLogicOperateurs() As Char = New Char() {"<", ">", "=", "!", "&", "|"}


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
        phrase = New Lexeme("DEBUT", Lexeme.LType.Lstring)
        Dim text = BoxEditeur.Text
        If Not (Suppr_comment(text)) Then
            Exit Sub
        End If

        Dim start = 0
        For i = 0 To text.Length - 2
            If text(i) = ";" And text(i + 1) = ";" Then
                Lecture(text.Substring(start, i - start), start)
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
        ''Interpretaion des strings et des chars a part
        If Not Predecoupage(s) Then
            Exit Sub
        End If

        ''Decoupage en arbre de priorité et verification lexicale
        If Not Decoupage(s, phrase) Then
            Exit Sub
        End If

        ''Verification syntaxique de l'arbre
        AnalyseSynthaxique()




    End Sub

    Private Function AnalyseSynthaxique() As Object
        Select Case (1)

        End Select
    End Function


    Private Function Decoupage(ByRef s As String, l As Lexeme) As Boolean
        'Decoupage en arbre par priorité
        Dim searcher
        s = s.Trim()

        'Supprimer les parentheses aux extremités
        If (s(0) = "(" And s(s.Length - 1) = ")") Then
            l.value = "()"
            l.type = Lexeme.LType.Generic

            Decoupage(s.Substring(1, s.Length - 2), AddLexeme(l))

            Return True
        End If


        '0 Chercher les attributions "let"
        searcher = Recherche("let ", s)
        If searcher <> -1 Then
            l.value = "let"
            l.type = Lexeme.LType.Phrase

            Dim i = searcher + 4
            While i < s.Length - 1
                If EstVar(s(i)) Then
                    i += 1
                Else
                    Exit While
                End If
            End While

            Decoupage(s.Substring(searcher + 4, i - searcher - 4), AddLexeme(l))

            While i < s.Length - 1
                If s(i) = " " Then
                    i += 1
                Else
                    Exit While
                End If
            End While
            If (s(i) <> "=") Then
                Erreur("Mauvais operateur apres le let")
                Return False
            End If

            Decoupage(s.Substring(i + 1), AddLexeme(l))

            Return True
        End If

        '1 Chercher les séparateurs |
        searcher = Recherche("|", s)
        If searcher <> -1 Then
            l.value = "|"
            l.type = Lexeme.LType.Lbool
            Decoupage(s.Substring(0, searcher), AddLexeme(l))
            Decoupage(s.Substring(searcher + 1), AddLexeme(l))
            Return True
        End If

        '2 Chercher les séparateurs &
        searcher = Recherche("&", s)
        If searcher <> -1 Then
            l.value = "&"
            l.type = Lexeme.LType.Lbool
            Decoupage(s.Substring(0, searcher), AddLexeme(l))
            Decoupage(s.Substring(searcher + 1), AddLexeme(l))
            Return True
        End If

        '3 Chercher les séparateurs < > = !
        searcher = Recherche(New Char() {"<", ">", "=", "!"}, s)
        If searcher <> -1 Then
            l.value = s(searcher)
            l.type = Lexeme.LType.Lbool
            Decoupage(s.Substring(0, searcher), AddLexeme(l))
            Decoupage(s.Substring(searcher + 1), AddLexeme(l))
            Return True
        End If

        '4 Chercher les séparateurs * /
        searcher = Recherche(New Char() {"*", "/"}, s)
        If searcher <> -1 Then
            l.value = s(searcher)
            l.type = Lexeme.LType.Expr
            Decoupage(s.Substring(0, searcher), AddLexeme(l))
            Decoupage(s.Substring(searcher + 1), AddLexeme(l))
            Return True
        End If

        '5 Chercher les séparateurs + -
        searcher = Recherche(New Char() {"+", "-"}, s)
        If searcher <> -1 Then
            l.value = s(searcher)
            l.type = Lexeme.LType.Expr
            Decoupage(s.Substring(0, searcher), AddLexeme(l))
            Decoupage(s.Substring(searcher + 1), AddLexeme(l))
            Return True
        End If

        'Identifier les opérandes et Analyse syntaxique
        Select Case s(0)
            Case "#"
                l.value = Extract(s)
                l.type = Lexeme.LType.Lstring
                Return True
            Case "$"
                l.value = Extract(s)
                l.type = Lexeme.LType.Lchar
                Return True
        End Select
        If EstInt(s(0)) Then
            If (s.IndexOf(".") = -1) Then
                l.value = s
                l.type = Lexeme.LType.Lint
                For Each c In s
                    If (Not EstInt(c)) Then
                        Erreur("Entier attendu : <<" + s + ">>")
                        Return False
                    End If
                Next
                Return True
            Else
                l.value = s
                l.type = Lexeme.LType.Lfloat
                For Each c In s.Remove(s.IndexOf("."), 1)
                    If (Not EstInt(c)) Then
                        Erreur("Float attendu : <<" + s + ">>")
                        Return False
                    End If
                Next
                Return True
            End If
        End If
        If EstVar(s(0)) Then
            l.value = s
            l.type = Lexeme.LType.Lvar
            For Each c In s
                If (Not EstInt(c)) Then
                    Erreur("Nom erroné: <<" + s + ">>")
                    Return False
                End If
            Next
            Return True
        End If

        Erreur("Caractere inconnu : <<" + s(0) + ">>")
        Return False
    End Function
    Private Function Predecoupage(s As String)
        Dim varname = "aaaa"
        Dim last = stock

        Dim a = s.IndexOf("""")
        While (a <> -1)
            Dim b = s.Substring(a + 1).IndexOf("""")
            If b = -1 Then
                Erreur("Fin de string manquant")
                Return False
            End If
            last.suiv = New VarPerso("#" + varname, s.Substring(a + 1, b))
            s = s.Substring(0, a) + "#" + varname + " " + s.Substring(a + 1 + b)
            IncrName(varname)
        End While

        a = s.IndexOf("'")
        While (a <> -1)
            Dim b = s.Substring(a + 1).IndexOf("'")
            If b = -1 Then
                Erreur("Fin de Char manquant")
                Return False
            End If
            last.suiv = New VarPerso("$" + varname, s.Substring(a + 1, b))
            s = s.Substring(0, a) + "$" + varname + " " + s.Substring(a + 1 + b)
            IncrName(varname)
        End While
        Return True
    End Function
#End Region

#Region "Fonctions manipulation de variables"
    Private Function EmpileVar(nom As String, Optional type As Lexeme.LType = Nothing, Optional valeur As Object = Nothing)
        Dim var = SearchVar(nom)
        If var Is Nothing Then
            var = variables
            While var.suiv IsNot Nothing
                var = var.suiv
            End While
            Select Case type
                Case Lexeme.LType.Lint
                    var.suiv = New Vint(nom, valeur)
                Case Lexeme.LType.Lfloat
                    var.suiv = New Vfloat(nom, valeur)
                Case Lexeme.LType.Lbool
                    var.suiv = New Vbool(nom, valeur)
                Case Lexeme.LType.Lchar
                    var.suiv = New Vchar(nom, valeur)
                Case Lexeme.LType.Lstring
                    var.suiv = New Vstring(nom, valeur)
                Case Else
                    var.suiv = New VarPerso(nom, valeur)
            End Select
            var = var.suiv
        End If
        Return var
    End Function

    Private Function AddLexeme(root As Lexeme, Optional valeur As String = Nothing, Optional type As Lexeme.LType = Lexeme.LType.Generic) As Lexeme
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

    Private Function Extract(s As String)
        Dim index As VarPerso = stock
        While index.suiv IsNot Nothing
            If index.suiv.nom = s Then
                Dim temp = index.suiv
                index.suiv = index.suiv.suiv
                Return temp.value
            End If
            index = index.suiv
        End While
        Return Nothing
    End Function

    Private Function SearchVar(s As String)
        Dim index As VarPerso = variables
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
    Private Function Recherche(mot As String, s As String)
        Dim first = s.IndexOf(mot)
        While (first <> -1 AndAlso Not IsOutOfParenthese(first, s))
            s.Substring(first + 1).IndexOf(mot)
        End While
        Return first
    End Function
    Private Function Recherche(mot As Char(), s As String)
        Dim first = s.IndexOfAny(mot)
        While (first <> -1 AndAlso Not IsOutOfParenthese(first, s))
            first = s.Substring(first + 1).IndexOfAny(mot)
        End While
        Return first
    End Function

    Private Function IsOutOfParenthese(index As Integer, s As String) As Boolean
        Dim compt = 0

        For i = 0 To index
            If s(i) = "(" Then
                compt += 1
            End If
            If s(i) = ")" Then
                compt -= 1
            End If
        Next

        If compt < 0 Then
            Erreur("Parenthese fermante de trop")
            Return True
        End If

        Return compt = 0
    End Function



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

    Private Function IncrName(ByRef s As String)
        Dim sb As New StringBuilder(s, s.Length)
        Dim i
        For i = s.Length To 0 Step -1
            If (s(i) >= "z") Then
                sb(i) = "a"
            Else
                Exit For
            End If
        Next
        sb(i) = ChrW(AscW(sb(i)) + 1)
        Return sb.ToString
    End Function

    Private Function Findopeprio(s As String) As Integer
        Dim lim = s.Length
        For Each mot In (GrammaireMots)
            Dim a = s.IndexOf(mot)
            If (a <> -1 And lim > a) Then
                lim = a
            End If
        Next
        For Each mot In (GrammaireLogicOperateurs)
            Dim a = s.IndexOf(mot)
            If (a <> -1 And lim > a) Then
                lim = a
            End If
        Next
        For i = 0 To lim - 1
            If OpePrio(s(i)) Then
                Return i
            End If
            If s(i) = "(" Then
                i += Findparenthese(s.Substring(i + 1))
            End If
            If s(i) = "=" Then
                Return 0
            End If
        Next
        Return 0
    End Function

    Private Function EstChar(s As Char) As Boolean
        Return ((s >= "a" And s <= "z") Or (s >= "A" And s <= "Z"))
    End Function
    Private Function EstInt(s As Char) As Boolean
        Return (s <= "9" And s >= "0")
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
