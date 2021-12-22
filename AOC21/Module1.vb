
Module Module1

    ReadOnly AOCC As New AOCClass
    Public filereader As System.IO.StreamReader
    Public stringreader As String
    Dim day As Int16
    Dim part As Int16
    Dim dataFileName As String
    Dim UseActual As Boolean
    Dim AtHome As Boolean

    Sub Main()

        day = 22
        part = 1
        UseActual = False
        UseActual = True

        AtHome = True

        If AtHome Then
            dataFileName = "C:\Users\jyani\source\repos\AOC21\AOC21\"
        Else
            dataFileName = "C:\Users\jyanity\source\repos\techonce\AOC21\AOC21\"
        End If

        If UseActual Then
            dataFileName &= "data_" & day & ".txt"
        Else
            dataFileName &= "sample_" & day & ".txt"
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

    Function Hex2Bin(value As String) As String

        Dim response As String

        If value = "" Then
            response = ""
        Else
            For t = 1 To Len(value)

                Select Case Mid(value, t, 1)
                    Case "0"
                        response &= "0000"
                    Case "1"
                        response &= "0001"
                    Case "2"
                        response &= "0010"
                    Case "3"
                        response &= "0011"
                    Case "4"
                        response &= "0100"
                    Case "5"
                        response &= "0101"
                    Case "6"
                        response &= "0110"
                    Case "7"
                        response &= "0111"
                    Case "8"
                        response &= "1000"
                    Case "9"
                        response &= "1001"
                    Case "A"
                        response &= "1010"
                    Case "B"
                        response &= "1011"
                    Case "C"
                        response &= "1100"
                    Case "D"
                        response &= "1101"
                    Case "E"
                        response &= "1110"
                    Case "F"
                        response &= "1111"

                    Case Else
                        response &= ""
                End Select
            Next
        End If

        Return response

    End Function



End Module
