Imports System.Text
Imports System.IO

#Region "Mes Classes de variables"
Public Class Lexeme
    Enum LType
        GENERIC
        PHRASE
        EXPR
        LBOOL
        Lfun
        Lint
        Lfloat
        Lmathop
        Llogop
        Lchar
        Lstring
        Lvar
        Lmot
    End Enum

    Public value As String
    Public type As LType
    Public fils As noeud

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
Public Class Vfun : Inherits VarPerso
    Public Shadows value As String
    Public Sub New(newnom As String, Optional mavaleur As String = Nothing)
        MyBase.New(newnom, mavaleur)
    End Sub
End Class
Public Class Vint : Inherits VarPerso
    Public Shadows value As Integer
    Public Sub New(newnom As String, Optional mavaleur As Integer = Nothing)
        MyBase.New(newnom, mavaleur)
    End Sub
End Class
Public Class Vfloat : Inherits VarPerso
    Public Shadows value As Double
    Public Sub New(newnom As String, Optional mavaleur As Double = Nothing)
        MyBase.New(newnom, mavaleur)
    End Sub
End Class
Public Class Vbool : Inherits VarPerso
    Public Shadows value As Boolean
    Public Sub New(newnom As String, Optional mavaleur As Boolean = Nothing)
        MyBase.New(newnom, mavaleur)
    End Sub
End Class
Public Class Vchar : Inherits VarPerso
    Public Shadows value As Char
    Public Sub New(newnom As String, Optional mavaleur As Char = Nothing)
        MyBase.New(newnom, mavaleur)
    End Sub
End Class
Public Class Vstring : Inherits VarPerso
    Public Shadows value As String
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
    Public variables As VarPerso = New VarPerso("@VARIABLES")
    Public stock As VarPerso = New VarPerso("@STOCK")

    'Ma Grammaire
    Public GrammaireMots() As String = New String() {"let", "true", "false"}



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
        If (BoxEditeur.Text.Trim = "") Then
            Exit Sub
        End If
        BoxSortie.AppendText("Execution : " + vbCrLf)
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
        BoxSortie.AppendText(vbCrLf)
    End Sub
#End Region

#Region "Interpretation"
    Private Function Suppr_comment(ByRef s As String) As Boolean
        Dim sb As New StringBuilder
        Dim skip = 0
        If s.Length < 2 Then
            Return True
        End If

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
        Dim arbre = New Lexeme("", Lexeme.LType.Lstring)

        ''Interpretaion des strings et des chars a part
        s = Predecoupage(s)
        If s = "" Then
            Exit Sub
        End If

        ''Decoupage en arbre de priorité et verification lexicale
        If Not Decoupage(s, arbre) Then
            Exit Sub
        End If

        ''Verification syntaxique de l'arbre
        If Not AnalyseSynthaxique(arbre) Then
            Exit Sub
        End If

        ''Execution aveugle
        Dim result = Execution(arbre)
        BoxSortie.AppendText(result.ToString + vbCrLf)

    End Sub

    Private Function Execution(l As Lexeme) As Object
        Dim gauche, droite
        If Not Zerofils(l) Then
            gauche = l.fils.lex
            If Not Unfils(l) Then
                droite = l.fils.suiv.lex
            End If
        End If

        Select Case l.value
            Case "let"
                Dim var = EmpileVar(Execution(gauche), Execution(droite))
                Return var.nom + " = " + var.value.ToString
            Case "|"
                Return Execution(gauche) Or Execution(droite)
            Case "&"
                Return Execution(gauche) And Execution(droite)
            Case "<"
                Return Execution(gauche) < Execution(droite)
            Case ">"
                Return Execution(gauche) > Execution(droite)
            Case "="
                Return Execution(gauche) = Execution(droite)
            Case "!"
                Return Execution(gauche) <> Execution(droite)
            Case "*"
                Return Execution(gauche) * Execution(droite)
            Case "/"
                Return Execution(gauche) / Execution(droite)
            Case "+"
                Return Execution(gauche) + Execution(droite)
            Case "-"
                Return Execution(gauche) - Execution(droite)
            Case "()"
                Return Execution(gauche)
            Case Else
                Select Case l.type
                    Case Lexeme.LType.Lint
                        Return CInt(l.value)
                    Case Lexeme.LType.Lfloat
                        Return CDbl(l.value)
                    Case Lexeme.LType.LBOOL
                        Return l.value = "True" Or l.value = "true"
                    Case Lexeme.LType.Lchar
                        Return CChar(l.value)
                    Case Lexeme.LType.Lstring
                        Return l.value
                    Case Lexeme.LType.Lvar
                        Return l.value
                    Case Lexeme.LType.Lfun
                        Return l.value.Trim
                End Select
        End Select
        Return ""
    End Function

    Private Function AnalyseSynthaxique(l As Lexeme) As Boolean
        Dim f1, f2 As Lexeme.LType
        If Not Zerofils(l) AndAlso (Unfils(l) OrElse Deuxfils(l)) Then
            f1 = l.fils.lex.type
            If Not Unfils(l) AndAlso Deuxfils(l) Then
                f2 = l.fils.suiv.lex.type
            End If
        End If

        'Verification
        Select Case (l.type)
            Case Lexeme.LType.PHRASE
                If Zerofils(l) Then
                    Return False
                ElseIf Unfils(l) Then
                    If EXPRverif(f1) Or f1 = Lexeme.LType.LBOOL Or f1 = Lexeme.LType.Lstring Or f1 = Lexeme.LType.Lchar Or f1 = Lexeme.LType.Lfun Then
                        Return AnalyseSynthaxique(l.fils.lex)
                    End If
                ElseIf Deuxfils(l) Then
                    If EXPRverif(f2) Or f2 = Lexeme.LType.LBOOL Or f2 = Lexeme.LType.Lstring Or f2 = Lexeme.LType.Lchar Or f2 = Lexeme.LType.Lfun Then
                        If (l.fils.lex.type = Lexeme.LType.Lvar) Then
                            Return AnalyseSynthaxique(l.fils.suiv.lex)
                        Else
                            Erreur("Une variable est atttendue pour l'assignation : " + l.value)
                        End If
                    End If
                End If

            Case Lexeme.LType.EXPR
                If Zerofils(l) Then
                    Return False
                ElseIf Unfils(l) Then
                    If EXPRverif(f1) Then
                        Return AnalyseSynthaxique(l.fils.lex)
                    ElseIf f1 = Lexeme.LType.Lvar Then
                        Erreur("Variable non assignée " + l.fils.lex.value)
                    Else
                        Erreur("Une expression correcte est attendue : " + l.value)
                    End If
                ElseIf Deuxfils(l) Then
                    If EXPRverif(f1) And EXPRverif(f2) Then
                        Return AnalyseSynthaxique(l.fils.lex) And AnalyseSynthaxique(l.fils.suiv.lex)
                    ElseIf f1 = Lexeme.LType.Lvar Then
                        Erreur("Variable non assignée " + l.fils.lex.value)
                    ElseIf f2 = Lexeme.LType.Lvar Then
                        Erreur("Variable non assignée " + l.fils.suiv.lex.value)
                    Else
                        Erreur("Le type attendu de chaque coté est int ou float : " + l.value)
                    End If
                End If

            Case Lexeme.LType.LBOOL
                If Zerofils(l) Then
                    Return True
                ElseIf Unfils(l) Then
                    If (f1 = Lexeme.LType.Lbool) Then
                        AnalyseSynthaxique(l.fils.lex)
                    Else
                        Erreur("Un bool est attendu : " + l.value)
                    End If
                ElseIf Deuxfils(l) Then
                    If (l.value = "&" Or l.value = "|") Then
                        If (f1 = Lexeme.LType.Lbool And f2 = Lexeme.LType.Lbool) Then
                            Return AnalyseSynthaxique(l.fils.lex) And AnalyseSynthaxique(l.fils.suiv.lex)
                        Else
                            Erreur("Un booleen est attendu de chaque coté" + l.value)
                        End If
                    Else
                        If (EXPRverif(f1) And EXPRverif(f2)) Or (f1 = Lexeme.LType.Lchar And f2 = Lexeme.LType.Lchar) Or (f1 = Lexeme.LType.Lstring And f2 = Lexeme.LType.Lstring) Then
                            Return AnalyseSynthaxique(l.fils.lex) And AnalyseSynthaxique(l.fils.suiv.lex)
                        Else
                            Erreur("Le type doit etre le meme de chaque coté : " + l.value)
                        End If
                    End If
                End If

            Case Lexeme.LType.Lint, Lexeme.LType.Lfloat, Lexeme.LType.Lchar, Lexeme.LType.Lstring, Lexeme.LType.Lmathop, Lexeme.LType.Llogop, Lexeme.LType.Lvar, Lexeme.LType.Lfun
                Return True
            Case Lexeme.LType.Lmot
                Erreur("Etrange, il n'y a pas encore de mots ...")
            Case Lexeme.LType.GENERIC
                Erreur("Erreur de variable : " + l.value)
            Case Else
                Erreur("Type inconnu " + l.value)
        End Select
        Return False
    End Function


    Private Function Decoupage(ByRef s As String, l As Lexeme) As Boolean
        'Decoupage en arbre par priorité + Analyse Lexicale
        Dim searcher
        s = s.Trim()

        'Supprimer les parentheses aux extremités
        If (s(0) = "(" And s(s.Length - 1) = ")") Then
            l.value = "()"
            l.type = Lexeme.LType.GENERIC

            Decoupage(s.Substring(1, s.Length - 2), AddLexeme(l))
            SetParentheseType(l)
            Return True
        End If


        '0 Chercher les attributions "let"
        searcher = Recherche("let ", s)
        If searcher <> -1 Then
            l.value = "let"
            l.type = Lexeme.LType.Phrase

            Dim i = searcher + 4
            While i < s.Length - 1 And EstVar(s(i))
                i += 1
            End While

            AddLexeme(l, s.Substring(searcher + 4, i - searcher - 4), Lexeme.LType.Lvar)

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

        '1 Chercher les declarations de fonctions
        searcher = Recherche("function", s)
        If searcher <> -1 Then
            l.value = s.Substring(searcher + 8).Trim
            l.type = Lexeme.LType.Lfun
            Dim finarg = s.Substring(searcher + 8).IndexOf("->")
            If finarg = -1 Then
                Erreur("Il faut mettre -> après les arguments pour declarer une fonction")
                Return False
            End If
            For i = 8 To finarg + searcher + 7
                If Not EstVar(s(i)) And s(i) <> " " Then
                    Erreur("arguments de fonction erronés")
                    Return False
                End If
            Next
            Return True
        End If

        '2 Chercher les séparateurs |
        searcher = Recherche("|", s)
        If searcher <> -1 Then
            l.value = "|"
            l.type = Lexeme.LType.Lbool
            Decoupage(s.Substring(0, searcher), AddLexeme(l))
            Decoupage(s.Substring(searcher + 1), AddLexeme(l))
            Return True
        End If

        '3 Chercher les séparateurs &
        searcher = Recherche("&", s)
        If searcher <> -1 Then
            l.value = "&"
            l.type = Lexeme.LType.Lbool
            Decoupage(s.Substring(0, searcher), AddLexeme(l))
            Decoupage(s.Substring(searcher + 1), AddLexeme(l))
            Return True
        End If

        '4 Chercher les séparateurs < > = !
        searcher = Recherche(New Char() {"<", ">", "=", "!"}, s)
        If searcher <> -1 Then
            l.value = s(searcher)
            l.type = Lexeme.LType.Lbool
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

        '6 Chercher les séparateurs * /
        searcher = Recherche(New Char() {"*", "/"}, s)
        If searcher <> -1 Then
            l.value = s(searcher)
            l.type = Lexeme.LType.EXPR
            Decoupage(s.Substring(0, searcher), AddLexeme(l))
            Decoupage(s.Substring(searcher + 1), AddLexeme(l))
            Return True
        End If

        'Identifier les opérandes
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
        If Estbool(s) Then
            l.value = s
            l.type = Lexeme.LType.LBOOL
            Return True
        End If
        If EstVar(s(0)) Then
            If (s.IndexOf(" ") = -1) Then
                For Each c In s
                    If Not EstVar(c) Then
                        Erreur("Nom erroné: <<" + s + ">>")
                        Return False
                    End If
                Next
                If GetVar(s) Is Nothing Then
                    Erreur("Variable non attribuée : " + s)
                    Return False
                End If
                l.value = GetVar(s).value.ToString
                l.type = GetVarType(s)
                If l.type = Lexeme.LType.Lfun Then
                    Decoupage(l.value, AddLexeme(l))
                    l.value = "()"
                    SetParentheseType(l)
                End If
                Return True
            Else
                Dim appel = s.Split(" ")
                Dim fun = GetVar(appel(0))
                If fun Is Nothing Then
                    Erreur("Variable non attribuée : " + s)
                    Return False
                End If
                Dim args = fun.value.substring(0, fun.value.indexof("->")).Trim.Split(" ")
                If args.Length <> appel.Length - 1 Then
                    Erreur("Mauvais nombre d'arguments pour " + appel(0))
                    Return False
                End If
                l.value = ReplaceArgFunction(fun.value.substring(fun.value.indexof("->") + 2), args, appel.Skip(1).ToArray)
                Decoupage(l.value, AddLexeme(l))
                l.value = "()"
                SetParentheseType(l)
                Return True
            End If
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
                Return ""
            End If
            last.suiv = New VarPerso("#" + varname, s.Substring(a + 1, b))
            s = s.Substring(0, a) + "#" + varname + " " + s.Substring(a + 2 + b)
            varname = IncrName(varname)
            last = last.suiv
            a = s.IndexOf("""")
        End While

        a = s.IndexOf("'")
        While (a <> -1)
            If s(a + 2) <> "'" Then
                Erreur("Fin de Char manquant")
                Return ""
            End If
            last.suiv = New VarPerso("$" + varname, s(a + 1))
            s = s.Substring(0, a) + "$" + varname + " " + s.Substring(a + 3)
            varname = IncrName(varname)
            last = last.suiv
            a = s.IndexOf("'")
        End While
        Return s
    End Function
#End Region

#Region "Fonctions manipulation de variables"
    Private Function FindFunctionType(f As String) As Lexeme.LType
        Dim l = New Lexeme("FunctionTypeFinder", Lexeme.LType.Lfun)
        Decoupage(f, l)
        Return l.fils.lex.type
    End Function

    Private Function EmpileVar(nom As String, Optional value As Object = Nothing) As VarPerso
        Dim var = GetVar(nom)
        If var IsNot Nothing Then
            Dim prec = variables
            While (prec.suiv IsNot var)
                prec = prec.suiv
            End While
            prec.suiv = prec.suiv.suiv
        End If

        Dim type = GetVarType(nom, value)
        var = variables
        While var.suiv IsNot Nothing
            var = var.suiv
        End While
        Select Case type
            Case Lexeme.LType.Lint
                var.suiv = New Vint(nom, value)
            Case Lexeme.LType.Lfloat
                var.suiv = New Vfloat(nom, value)
            Case Lexeme.LType.LBOOL
                var.suiv = New Vbool(nom, value)
            Case Lexeme.LType.Lchar
                var.suiv = New Vchar(nom, value)
            Case Lexeme.LType.Lstring
                var.suiv = New Vstring(nom, value)
            Case Lexeme.LType.Lvar
                var.suiv = New Vfun(nom, value)
            Case Else
                var.suiv = New VarPerso(nom, value)
        End Select
        var = var.suiv
        Return var
    End Function

    Private Function AddLexeme(root As Lexeme, Optional valeur As String = Nothing, Optional type As Lexeme.LType = Lexeme.LType.GENERIC) As Lexeme
        Dim l = NewNode(root)
        l.lex = New Lexeme(valeur, type)
        Return l.lex
    End Function

    Private Function NewNode(ByRef l As Lexeme) As Lexeme.noeud
        If l.fils Is Nothing Then
            l.fils = New Lexeme.noeud
            Return l.fils
        Else
            Dim last = l.fils
            While last.suiv IsNot Nothing
                last = last.suiv
            End While
            last.suiv = New Lexeme.noeud
            Return last.suiv
        End If
    End Function

    Private Function Extract(s As String) As String
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

    Private Function GetVar(s As String) As VarPerso
        Dim index As VarPerso = variables
        While index IsNot Nothing
            If index.nom = s Then
                Return index
            End If
            index = index.suiv
        End While
        Return Nothing
    End Function

    Private Function GetVarType(name As String, Optional varvalue As Object = Nothing) As Lexeme.LType
        If varvalue Is Nothing Then varvalue = GetVar(name).value
        If varvalue IsNot Nothing Then
            Select Case Type.GetTypeCode(varvalue.GetType)
                Case TypeCode.Int32
                    Return Lexeme.LType.Lint
                Case TypeCode.Double
                    Return Lexeme.LType.Lfloat
                Case TypeCode.Boolean
                    Return Lexeme.LType.LBOOL
                Case TypeCode.Char
                    Return Lexeme.LType.Lchar
                Case TypeCode.String
                    If varvalue.indexof("->") <> -1 Then
                        Return Lexeme.LType.Lvar
                    Else
                        Return Lexeme.LType.Lstring
                    End If
            End Select
        Else
            Return Lexeme.LType.Lvar
        End If
        Return Lexeme.LType.GENERIC
    End Function

    Private Function ReplaceArgFunction(f As String, arg As String(), argvalue As String())
        Dim sb = New StringBuilder(f)
        For i = 0 To arg.Length - 1
            sb.Replace(arg(i), argvalue(i))
        Next
        Return sb.ToString
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

    Private Function SetParentheseType(l As Lexeme)
        If l.fils.lex.type = Lexeme.LType.LBOOL Then
            l.type = Lexeme.LType.LBOOL
        Else
            l.type = Lexeme.LType.EXPR
        End If
        Return True
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

    Private Function IncrName(ByRef s As String) As String
        Dim sb As New StringBuilder(s, s.Length)
        Dim i
        For i = s.Length - 1 To 0 Step -1
            If (s(i) >= "z") Then
                sb(i) = "a"
            Else
                Exit For
            End If
        Next
        sb(i) = ChrW(AscW(sb(i)) + 1)
        Return sb.ToString
    End Function

    Private Function EXPRverif(t As Lexeme.LType)
        Return t = Lexeme.LType.EXPR Or t = Lexeme.LType.Lint Or t = Lexeme.LType.Lfloat
    End Function

    Private Function Zerofils(l As Lexeme) As Boolean
        Return l.fils Is Nothing
    End Function
    Private Function Unfils(l As Lexeme) As Boolean
        Return l.fils.suiv Is Nothing
    End Function
    Private Function Deuxfils(l As Lexeme) As Boolean
        Return l.fils.suiv.suiv Is Nothing
    End Function

    Private Function EstBool(s As String) As Boolean
        Return s = "True" Or s = "False" Or s = "true" Or s = "false"
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
    Private Sub Erreur(s As String)
        BoxSortie.AppendText("ERREUR : " + s + vbCrLf)
    End Sub
#End Region
End Class
