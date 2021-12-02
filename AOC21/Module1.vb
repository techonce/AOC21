Class AOCClass
    Function day1(value As String, part As Int16) As UInt32

        Dim i As Int16
        Dim lastdepth As Int16
        Dim curDepth As Int16
        Dim incCount As Int16
        Dim reading(0 To 3) As Int16

        If part = 1 Then

            stringreader = filereader.ReadLine()
            lastdepth = Val(stringreader)
            curDepth = lastdepth

            Do While (Not stringreader Is Nothing)
                If curDepth > lastdepth Then incCount = incCount + 1

                lastdepth = curDepth
                stringreader = filereader.ReadLine()
                curDepth = Val(stringreader)
            Loop
        Else
            stringreader = filereader.ReadLine()
            reading(0) = Val(stringreader)
            stringreader = filereader.ReadLine()
            reading(1) = Val(stringreader)
            stringreader = filereader.ReadLine()
            reading(2) = Val(stringreader)
            stringreader = filereader.ReadLine()

            Do While (Not stringreader Is Nothing)

                reading(3) = Val(stringreader)
                If reading(3) > reading(0) Then incCount = incCount + 1

                For i = 0 To 2
                    reading(i) = reading(i + 1)
                Next

                stringreader = filereader.ReadLine()
            Loop

        End If

        day1 = incCount

    End Function

    Function day2(value As String, part As Int16) As Int64
        Dim depth As Int64
        Dim posX As Int64
        Dim direction As String, amount As Int16, spaceLocation As Int16
        Dim aim As Int64

        If part = 1 Then

            stringreader = filereader.ReadLine()

            Do While (Not stringreader Is Nothing)

                spaceLocation = InStr(stringreader, " ")
                direction = Left(stringreader, spaceLocation - 1)
                amount = Val(Mid(stringreader, spaceLocation + 1, 10))

                Select Case direction
                    Case "forward"
                        posX = posX + amount
                    Case "up"
                        depth = depth - amount
                        If depth < 0 Then depth = 0
                    Case "down"
                        depth = depth + amount
                End Select

                stringreader = filereader.ReadLine()
            Loop

            day2 = depth * posX

        Else

            stringreader = filereader.ReadLine()

            Do While (Not stringreader Is Nothing)

                spaceLocation = InStr(stringreader, " ")
                direction = Left(stringreader, spaceLocation - 1)
                amount = Val(Mid(stringreader, spaceLocation + 1, 10))

                Select Case direction
                    Case "forward"
                        posX = posX + amount
                        depth = depth + amount * aim
                    Case "up"
                        aim = aim - amount
                    Case "down"
                        aim = aim + amount
                End Select

                Debug.Print(posX & "  " & aim & "  " & depth)

                stringreader = filereader.ReadLine()
            Loop

            day2 = depth * posX

        End If


    End Function

End Class

Module Module1

    Dim AOCC As New AOCClass
    Public filereader As System.IO.StreamReader
    Public stringreader As String
    Dim day As Int16
    Dim part As Int16
    Dim dataFileName As String
    Dim UseActual As Boolean

    Sub Main()

        day = 2
        part = 2
        UseActual = False
        UseActual = True

        If UseActual Then
            dataFileName = "C:\Users\jyani\source\repos\AOC21\AOC21\data_" & day & ".txt"
        Else
            dataFileName = "C:\Users\jyani\source\repos\AOC21\AOC21\sample_" & day & ".txt"
        End If

        filereader = My.Computer.FileSystem.OpenTextFileReader(dataFileName)

        MsgBox(CallByName(AOCC, "day" & day, CallType.Method, dataFileName, part))

        'Select Case day
        '    Case 1
        '        MsgBox(day1(dataFileName, part))
        '    Case 2
        '        MsgBox(day2(dataFileName, part))
        '    Case 3
        '        MsgBox(day2(dataFileName, part))
        '    Case 4

        '    Case 5

        '    Case 6

        '    Case 7

        '    Case 8

        '    Case 9

        '    Case 10

        '    Case 11
        '    Case 12
        '    Case 13
        '    Case 14
        '    Case 15
        '    Case 16
        '    Case 17
        '    Case 18
        '    Case 19
        '    Case 20
        '    Case 21
        '    Case 22
        '    Case 23
        '    Case 24
        '    Case 25
        '    Case Else
        '        MsgBox("day not in range")
        'End Select






    End Sub



End Module
