<Global.Microsoft.VisualBasic.CompilerServices.DesignerGenerated()> _
Partial Class Form1
    Inherits System.Windows.Forms.Form

    'Form remplace la méthode Dispose pour nettoyer la liste des composants.
    <System.Diagnostics.DebuggerNonUserCode()> _
    Protected Overrides Sub Dispose(ByVal disposing As Boolean)
        Try
            If disposing AndAlso components IsNot Nothing Then
                components.Dispose()
            End If
        Finally
            MyBase.Dispose(disposing)
        End Try
    End Sub

    'Requise par le Concepteur Windows Form
    Private components As System.ComponentModel.IContainer

    'REMARQUE : la procédure suivante est requise par le Concepteur Windows Form
    'Elle peut être modifiée à l'aide du Concepteur Windows Form.  
    'Ne la modifiez pas à l'aide de l'éditeur de code.
    <System.Diagnostics.DebuggerStepThrough()> _
    Private Sub InitializeComponent()
        Me.BoxEditeur = New System.Windows.Forms.RichTextBox()
        Me.BoxSortie = New System.Windows.Forms.TextBox()
        Me.BExec = New System.Windows.Forms.Button()
        Me.BOpen = New System.Windows.Forms.Button()
        Me.BExt = New System.Windows.Forms.Button()
        Me.SuspendLayout()
        '
        'BoxEditeur
        '
        Me.BoxEditeur.Location = New System.Drawing.Point(12, 12)
        Me.BoxEditeur.Name = "BoxEditeur"
        Me.BoxEditeur.Size = New System.Drawing.Size(426, 394)
        Me.BoxEditeur.TabIndex = 0
        Me.BoxEditeur.Text = ""
        '
        'BoxSortie
        '
        Me.BoxSortie.BackColor = System.Drawing.SystemColors.InactiveCaption
        Me.BoxSortie.BorderStyle = System.Windows.Forms.BorderStyle.FixedSingle
        Me.BoxSortie.Location = New System.Drawing.Point(445, 12)
        Me.BoxSortie.Multiline = True
        Me.BoxSortie.Name = "BoxSortie"
        Me.BoxSortie.ReadOnly = True
        Me.BoxSortie.Size = New System.Drawing.Size(343, 394)
        Me.BoxSortie.TabIndex = 2
        '
        'BExec
        '
        Me.BExec.Location = New System.Drawing.Point(12, 413)
        Me.BExec.Name = "BExec"
        Me.BExec.Size = New System.Drawing.Size(426, 23)
        Me.BExec.TabIndex = 1
        Me.BExec.Text = "Executer"
        Me.BExec.UseVisualStyleBackColor = True
        '
        'BOpen
        '
        Me.BOpen.Location = New System.Drawing.Point(445, 413)
        Me.BOpen.Name = "BOpen"
        Me.BOpen.Size = New System.Drawing.Size(168, 23)
        Me.BOpen.TabIndex = 3
        Me.BOpen.Text = "Restaurer"
        Me.BOpen.UseVisualStyleBackColor = True
        '
        'BExt
        '
        Me.BExt.Location = New System.Drawing.Point(619, 412)
        Me.BExt.Name = "BExt"
        Me.BExt.Size = New System.Drawing.Size(168, 23)
        Me.BExt.TabIndex = 4
        Me.BExt.Text = "Sauvegarder"
        Me.BExt.UseVisualStyleBackColor = True
        '
        'Form1
        '
        Me.AutoScaleDimensions = New System.Drawing.SizeF(6.0!, 13.0!)
        Me.AutoScaleMode = System.Windows.Forms.AutoScaleMode.Font
        Me.ClientSize = New System.Drawing.Size(800, 450)
        Me.Controls.Add(Me.BExt)
        Me.Controls.Add(Me.BOpen)
        Me.Controls.Add(Me.BExec)
        Me.Controls.Add(Me.BoxSortie)
        Me.Controls.Add(Me.BoxEditeur)
        Me.Name = "Form1"
        Me.Text = "Interpreteur Caml"
        Me.ResumeLayout(False)
        Me.PerformLayout()

    End Sub

    Friend WithEvents BoxEditeur As RichTextBox
    Friend WithEvents BoxSortie As TextBox
    Friend WithEvents BExec As Button
    Friend WithEvents BOpen As Button
    Friend WithEvents BExt As Button
End Class
