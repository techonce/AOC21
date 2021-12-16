
Module Module1
    ReadOnly AOCC As New AOCClass
    Public filereader As System.IO.StreamReader
    Public stringreader As String
    Dim day As Int16
    Dim part As Int16
    Dim dataFileName As String
    Dim UseActual As Boolean

    Sub Main()

        day = 17
        part = 2
        UseActual = False
        UseActual = True

        If UseActual Then
            dataFileName = "C:\Users\jyani\source\repos\AOC21\AOC21\" & "data_" & day & ".txt"
        Else
            dataFileName = "C:\Users\jyani\source\repos\AOC21\AOC21\" & "sample_" & day & ".txt"
        End If

        filereader = My.Computer.FileSystem.OpenTextFileReader(dataFileName)

        MsgBox(CallByName(AOCC, "Day" & day, CallType.Method, part))

        filereader.Close()

    End Sub

    Function Bin2Dec(value As String) As Int64

        Dim fact As Int64

        For i = Len(value) To 1 Step -1
            fact = Len(value) - i
            Bin2Dec += Val(Mid(value, i, 1)) * 2 ^ fact
        Next

        Return Bin2Dec

    End Function


End Module
