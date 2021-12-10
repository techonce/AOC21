Class AOCClass
    Public basins(1000) As Integer

    Structure StrucBoard
        Public ID As Integer
        Public grid(,) As Integer
        Public marked(,) As Boolean
        Public eliminated As Boolean
    End Structure

    Structure Vector2
        Public x As Int64
        Public y As Int64
    End Structure

    Structure Ray
        Public startpoint As Vector2
        Public endpoint As Vector2
    End Structure

    Function Day1(part As Int16) As UInt32

        Dim i As Int16
        Dim lastdepth As Int16
        Dim curDepth As Int16
        Dim incCount As Int16
        Dim reading(0 To 3) As Int16

        If part = 1 Then

            stringreader = filereader.ReadLine()
            lastdepth = Val(stringreader)
            curDepth = lastdepth

            Do While (stringreader IsNot Nothing)
                If curDepth > lastdepth Then incCount += 1

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

            Do While (stringreader IsNot Nothing)

                reading(3) = Val(stringreader)
                If reading(3) > reading(0) Then incCount += 1

                For i = 0 To 2
                    reading(i) = reading(i + 1)
                Next

                stringreader = filereader.ReadLine()
            Loop

        End If

        Day1 = incCount

    End Function

    Function Day2(part As Int16) As Int64
        Dim depth As Int64
        Dim posX As Int64
        Dim direction As String, amount As Int16, spaceLocation As Int16
        Dim aim As Int64

        If part = 1 Then

            stringreader = filereader.ReadLine()

            Do While (stringreader IsNot Nothing)

                spaceLocation = InStr(stringreader, " ")
                direction = Left(stringreader, spaceLocation - 1)
                amount = Val(Mid(stringreader, spaceLocation + 1, 10))

                Select Case direction
                    Case "forward"
                        posX += amount
                    Case "up"
                        depth -= amount
                        If depth < 0 Then depth = 0
                    Case "down"
                        depth += amount
                End Select

                stringreader = filereader.ReadLine()
            Loop

            Day2 = depth * posX

        Else

            stringreader = filereader.ReadLine()

            Do While (stringreader IsNot Nothing)

                spaceLocation = InStr(stringreader, " ")
                direction = Left(stringreader, spaceLocation - 1)
                amount = Val(Mid(stringreader, spaceLocation + 1, 10))

                Select Case direction
                    Case "forward"
                        posX += amount
                        depth += amount * aim
                    Case "up"
                        aim -= amount
                    Case "down"
                        aim += amount
                End Select

                Debug.Print(posX & "  " & aim & "  " & depth)

                stringreader = filereader.ReadLine()
            Loop

            Day2 = depth * posX

        End If


    End Function

    Function Day3(part As Int16) As Int64

        Dim gammaBin As String, gammaInt As Int64
        Dim epsilonBin As String, epsilonInt As Int64
        Dim datapointsOGR As List(Of String)
        Dim datapointsCOS As List(Of String)
        Dim tempdata As List(Of String)

        Dim totalRows As Int64, counts(0 To 11) As Int64, binLength As Int16
        Dim goodChar As String

        If part = 1 Then

            stringreader = filereader.ReadLine()
            binLength = Len(stringreader)
            gammaBin = ""
            epsilonBin = ""

            Do While (stringreader IsNot Nothing)
                totalRows += 1
                For i = 1 To binLength
                    If Mid(stringreader, i, 1) = 1 Then
                        counts(i - 1) = counts(i - 1) + 1
                    End If
                Next

                stringreader = filereader.ReadLine()
            Loop

            For i = 1 To binLength
                If counts(i - 1) > totalRows / 2 Then
                    gammaBin &= "1"
                    epsilonBin &= "0"
                Else
                    gammaBin &= "0"
                    epsilonBin &= "1"
                End If
            Next

            gammaInt = Bin2Dec(gammaBin)
            epsilonInt = Bin2Dec(epsilonBin)

            Debug.Print("GAMMA: " & gammaBin & " = " & gammaInt)
            Debug.Print("EPSILON: " & epsilonBin & " = " & epsilonInt)

            Day3 = gammaInt * epsilonInt

        Else

            datapointsOGR = New List(Of String)
            datapointsCOS = New List(Of String)
            tempdata = New List(Of String)

            stringreader = filereader.ReadLine()
            binLength = Len(stringreader)

            Do While (stringreader IsNot Nothing)
                datapointsOGR.Add(stringreader)
                datapointsCOS.Add(stringreader)

                stringreader = filereader.ReadLine()
            Loop

            'find most common

            For i = 1 To binLength

                If datapointsOGR.Count > 1 Then

                    counts(i - 1) = 0

                    For Each datapoint In datapointsOGR
                        If Mid(datapoint, i, 1) = 1 Then
                            counts(i - 1) = counts(i - 1) + 1
                        End If
                    Next

                    If counts(i - 1) >= datapointsOGR.Count / 2 Then
                        goodChar = "1"
                    Else
                        goodChar = "0"
                    End If

                    For Each datapoint In datapointsOGR
                        If Mid(datapoint, i, 1) = goodChar Then
                            tempdata.Add(datapoint)
                        End If
                    Next

                    datapointsOGR.Clear()
                    datapointsOGR.AddRange(tempdata)
                    tempdata.Clear()

                End If

                counts(i - 1) = 0

                If datapointsCOS.Count > 1 Then

                    For Each datapoint In datapointsCOS
                        If Mid(datapoint, i, 1) = 1 Then
                            counts(i - 1) = counts(i - 1) + 1
                        End If
                    Next

                    If counts(i - 1) >= datapointsCOS.Count / 2 Then
                        goodChar = "0"
                    Else
                        goodChar = "1"
                    End If

                    For Each datapoint In datapointsCOS
                        If Mid(datapoint, i, 1) = goodChar Then
                            tempdata.Add(datapoint)
                        End If
                    Next

                    datapointsCOS.Clear()
                    datapointsCOS.AddRange(tempdata)
                    tempdata.Clear()

                End If

            Next

            Day3 = Bin2Dec(datapointsOGR(0)) * Bin2Dec(datapointsCOS(0))

        End If

    End Function

    Function Day4(part As Int16) As Int64

        Dim answer As Int64
        Dim boards As Dictionary(Of Int64, StrucBoard), tempBoards As Dictionary(Of Int64, StrucBoard)
        Dim picks As List(Of Int64)
        Dim r As Int16, c As Int16, spot As Int16
        Dim board As New StrucBoard
        Dim foundOne As Boolean

        ' read in pick values

        boards = New Dictionary(Of Int64, StrucBoard)
        tempBoards = New Dictionary(Of Int64, StrucBoard)
        picks = New List(Of Int64)

        stringreader = filereader.ReadLine()

        ' parse pick values
        Do Until Len(stringreader) = 0
            spot = InStr(stringreader, ",")
            If spot = 0 Then
                picks.Add(Val(stringreader))
                Exit Do
            ElseIf spot = 1 Then
                stringreader.Trim(",")
            Else
                picks.Add(Val(Left(stringreader, InStr(stringreader, ",") - 1)))
                stringreader = Mid(stringreader, InStr(stringreader, ",") + 1, 1000)
            End If
        Loop

        ' read in boards
        Do
            stringreader = filereader.ReadLine()
            If stringreader Is Nothing Then
                Exit Do
            ElseIf stringreader = "" Then
                board = New StrucBoard
                ReDim board.grid(4, 4)
                ReDim board.marked(4, 4)
                board.ID = boards.Count + 1
                r = -1
                c = 0
            Else
                r += 1
                For c = 0 To 4
                    board.grid(r, c) = Val(Mid(stringreader, c * 3 + 1, 2))
                Next

                If r = 4 Then
                    boards.Add(board.ID, board)
                End If

            End If
        Loop Until stringreader Is Nothing

        If part = 1 Then

            For Each pick In picks

                For t = 1 To boards.Count
                    board = boards.Item(t)
                    For r = 0 To 4
                        For c = 0 To 4
                            If board.grid(r, c) = pick Then
                                board.marked(r, c) = True
                            End If
                        Next
                    Next

                    If Checkboard(board) Then
                        answer = Totalboard(board) * pick
                        foundOne = True
                        Exit For
                    End If

                Next

                If foundOne Then Exit For

            Next

            Day4 = answer
        Else

            For Each pick In picks

                tempBoards = CloneDictionary(boards)

                For Each Item In boards

                    board = boards.Item(Item.Key)
                    For r = 0 To 4
                        For c = 0 To 4
                            If board.grid(r, c) = pick Then
                                board.marked(r, c) = True
                            End If
                        Next
                    Next

                    If Checkboard(board) Then
                        If boards.Count > 1 Then
                            tempBoards.Remove(board.ID)
                        Else
                            answer = Totalboard(board) * pick
                            foundOne = True
                            Exit For
                        End If
                    End If

                Next

                boards.Clear()
                boards = CloneDictionary(tempBoards)

                If foundOne Then Exit For

            Next

            Day4 = answer

        End If

    End Function

    Function Day5(part As Int16) As Int64
        Dim x1 As Int64, y1 As Int64, x2 As Int64, y2 As Int64, datapoint As Ray, spot As Int16

        Dim grid(1000, 1000) As Int16

        Dim answer As Int64

        If part = 1 Then

            stringreader = filereader.ReadLine()

            Do While (stringreader IsNot Nothing)
                spot = InStr(stringreader, " -> ")
                datapoint.startpoint = Getxy(Left(stringreader, spot - 1))
                datapoint.endpoint = Getxy(Mid(stringreader, spot + 4, 10))

                x1 = datapoint.startpoint.x
                y1 = datapoint.startpoint.y
                x2 = datapoint.endpoint.x
                y2 = datapoint.endpoint.y

                If x1 = x2 Then
                    If y1 < y2 Then
                        For y = y1 To y2
                            grid(x1, y) = grid(x1, y) + 1
                        Next
                    Else
                        For y = y1 To y2 Step -1
                            grid(x1, y) = grid(x1, y) + 1
                        Next
                    End If
                ElseIf y1 = y2 Then
                    If x1 < x2 Then
                        For x = x1 To x2
                            grid(x, y1) = grid(x, y1) + 1
                        Next
                    Else
                        For x = x1 To x2 Step -1
                            grid(x, y1) = grid(x, y1) + 1
                        Next
                    End If
                End If

                stringreader = filereader.ReadLine()

            Loop

            For x = 0 To 1000
                For y = 0 To 1000
                    If grid(x, y) > 1 Then answer += 1
                Next
            Next

            Day5 = answer
        Else

            Dim x As Int16, y As Int16

            stringreader = filereader.ReadLine()

            Do While (stringreader IsNot Nothing)

                spot = InStr(stringreader, " -> ")
                datapoint.startpoint = Getxy(Left(stringreader, spot - 1))
                datapoint.endpoint = Getxy(Mid(stringreader, spot + 4, 10))

                x1 = datapoint.startpoint.x
                y1 = datapoint.startpoint.y
                x2 = datapoint.endpoint.x
                y2 = datapoint.endpoint.y

                x = x1
                y = y1

                Do Until x = x2 And y = y2
                    grid(x, y) = grid(x, y) + 1
                    x += GetAngle(x1, x2)
                    y += GetAngle(y1, y2)
                Loop

                grid(x, y) = grid(x, y) + 1

                stringreader = filereader.ReadLine()

            Loop

            For x = 0 To 1000
                For y = 0 To 1000
                    If grid(x, y) > 1 Then answer += 1
                Next
            Next

            Day5 = answer

        End If
    End Function

    Function Day6(part As Int16) As Int64

        Dim answer As Int64
        Dim fish As Int16
        Dim fishes As List(Of Int16)
        Dim newfishes As List(Of Int16)
        Dim fishes2(0 To 8) As Int64
        Dim newfishes2(0 To 8) As Int64
        Dim spot As Int16

        fishes = New List(Of Short)
        newfishes = New List(Of Short)

        stringreader = filereader.ReadLine()

        Do Until Len(stringreader) = 0
            spot = InStr(stringreader, ",")
            If spot = 0 Then
                fish = Val(stringreader)
                fishes2(Val(stringreader)) = fishes2(Val(stringreader)) + 1
                stringreader = ""
            Else
                fish = Val(Left(stringreader, spot - 1))
                fishes2(Val(stringreader)) = fishes2(Val(stringreader)) + 1
                stringreader = Right(stringreader, Len(stringreader) - spot)
            End If

            fishes.Add(fish)

        Loop

        If part = 1 Then

            For c = 1 To 80
                For f = 0 To fishes.Count - 1
                    If fishes(f) = 0 Then
                        fishes(f) = 6
                        newfishes.Add(8)
                    Else
                        fishes(f) = fishes(f) - 1
                    End If
                Next

                fishes.AddRange(newfishes)
                newfishes.Clear()

            Next

            answer = fishes.Count

            Day6 = answer
        Else
            For c = 1 To 256

                For f = 0 To 8
                    Select Case f
                        Case 0 To 5, 7
                            newfishes2(f) = fishes2(f + 1)
                        Case 6
                            newfishes2(f) = fishes2(0) + fishes2(7)
                        Case 8
                            newfishes2(f) = fishes2(0)
                    End Select
                Next

                fishes2 = newfishes2.Clone

                answer = fishes2.Sum

                Console.WriteLine("Day: " & c & "  Fishes: " & answer)

            Next

            Day6 = answer

        End If
    End Function

    Function Day7(part As Int16) As Int64

        Dim crabs As List(Of Int16)
        Dim crab As Int16, spot As Int16, rollingSum As Int64
        Dim minGuess As Int16, maxGuess As Int16
        Dim fuelcost_new As Int64, fuelcost_old As Int64

        Dim answer As Int64

        stringreader = filereader.ReadLine()

        crabs = New List(Of Short)

        Do Until Len(stringreader) = 0
            spot = InStr(stringreader, ",")
            If spot = 0 Then
                crab = Val(stringreader)

                stringreader = ""
            Else
                crab = Val(Left(stringreader, spot - 1))
                stringreader = Right(stringreader, Len(stringreader) - spot)
            End If

            If crab > maxGuess Then maxGuess = crab
            If crab < minGuess Then minGuess = crab

            rollingSum += crab
            crabs.Add(crab)

        Loop

        If part = 1 Then

            'curGuess = (minGuess + maxGuess) / 2

            fuelcost_old = 100000000

            For i = minGuess To maxGuess

                fuelcost_new = CalcFuelCost(i, crabs)

                If fuelcost_new > fuelcost_old Then
                    answer = fuelcost_old
                    Exit For
                Else
                    fuelcost_old = fuelcost_new
                End If

            Next

            Day7 = answer
        Else

            fuelcost_old = 10000000000000

            For i = minGuess To maxGuess

                fuelcost_new = CalcFuelCost2(i, crabs)

                If fuelcost_new > fuelcost_old Then
                    answer = fuelcost_old
                    Exit For
                Else
                    fuelcost_old = fuelcost_new
                End If

            Next

            Day7 = answer

        End If
    End Function

    Function Day8(part As Int16) As Int64

        Dim answer As Int64, spot As Int16, strlen As Int16
        Dim inputDigits(9) As String, outputDigits(3) As String, inputString As String, outputString As String
        Dim inputSolved(9) As Boolean
        Dim stringValue(9) As String, inputValue(9) As Int16
        Dim currentString As String
        Dim c As Int16, temp_answer As Int16

        If part = 1 Then

            stringreader = filereader.ReadLine()

            Do While (stringreader IsNot Nothing)

                spot = InStr(stringreader, "|") + 2

                stringreader = Mid(stringreader, spot, 100)

                Do Until Len(stringreader) = 0
                    spot = InStr(stringreader, " ")
                    If spot = 0 Then
                        strlen = Len(stringreader)

                        stringreader = ""
                    Else
                        strlen = spot - 1
                        stringreader = Right(stringreader, Len(stringreader) - spot)
                    End If

                    Select Case strlen
                        Case 2, 3, 4, 7
                            answer += 1
                    End Select

                Loop

                stringreader = filereader.ReadLine()

            Loop

            Day8 = answer
        Else
            stringreader = filereader.ReadLine()

            Do While (stringreader IsNot Nothing)

                ReDim inputDigits(9)
                ReDim outputDigits(3)
                ReDim inputSolved(9)
                ReDim stringValue(9)
                ReDim inputValue(9)

                c = 0
                spot = InStr(stringreader, "|")

                inputString = Left(stringreader, spot - 2)
                outputString = Mid(stringreader, spot + 2, 100)

                Do Until Len(inputString) = 0
                    spot = InStr(inputString, " ")
                    If spot = 0 Then
                        inputDigits(c) = inputString
                        inputString = ""
                    Else
                        inputDigits(c) = Left(inputString, spot - 1)
                        inputString = (Right(inputString, Len(inputString) - spot))
                    End If

                    currentString = inputDigits(c)

                    Select Case Len(currentString) ' Find 1,4,7,8
                        Case 2 ' Value is 1
                            stringValue(1) = currentString
                            inputValue(c) = 1
                            inputSolved(c) = True
                        Case 3 ' Value is 7
                            stringValue(7) = currentString
                            inputValue(c) = 7
                            inputSolved(c) = True
                        Case 4 ' Value is 4
                            stringValue(4) = currentString
                            inputValue(c) = 4
                            inputSolved(c) = True
                        Case 7 ' Value is 8
                            stringValue(8) = currentString
                            inputValue(c) = 8
                            inputSolved(c) = True
                    End Select
                    c += 1
                Loop

                stringreader = filereader.ReadLine()

                ' len 2 = 1
                ' len 3 = 7
                ' len 4 = 4
                ' len 5 = 2,3,5
                ' len 6 = 0,6,9
                ' len 7 = 8

                For t = 0 To 9 ' find 3, 9, 6, 0
                    If inputSolved(t) = False Then

                        currentString = inputDigits(t)

                        If Len(currentString) = 5 Then
                            If IsSubset(currentString, stringValue(1)) Then ' find 3
                                stringValue(3) = currentString
                                inputValue(t) = 3
                                inputSolved(t) = True
                            End If
                        End If

                        If Len(currentString) = 6 Then
                            If IsSubset(currentString, stringValue(4)) Then ' find 9
                                stringValue(9) = currentString
                                inputValue(t) = 9
                                inputSolved(t) = True
                            ElseIf Not (IsSubset(currentString, stringValue(1))) Then ' find 6
                                stringValue(6) = currentString
                                inputValue(t) = 6
                                inputSolved(t) = True
                            Else ' find 0
                                stringValue(0) = currentString
                                inputValue(t) = 0
                                inputSolved(t) = True
                            End If

                        End If

                    End If

                Next

                For t = 0 To 9 ' find 2, 5
                    If inputSolved(t) = False Then

                        currentString = inputDigits(t)

                        If IsSubset(stringValue(6), currentString) Then ' find 5
                            stringValue(5) = currentString
                            inputValue(t) = 5
                            inputSolved(t) = True
                        Else ' find 2
                            stringValue(2) = currentString
                            inputValue(t) = 2
                            inputSolved(t) = True
                        End If

                    End If

                Next

                ' calculate output

                c = 0

                Do Until Len(outputString) = 0
                    spot = InStr(outputString, " ")
                    If spot = 0 Then
                        outputDigits(c) = outputString
                        outputString = ""
                    Else
                        outputDigits(c) = Left(outputString, spot - 1)
                        outputString = (Right(outputString, Len(outputString) - spot))
                        c += 1
                    End If
                Loop

                temp_answer = 0

                For t = 0 To 3

                    For u = 0 To 9
                        If SortString(outputDigits(t)) = SortString(stringValue(u)) Then
                            temp_answer += u * 10 ^ (3 - t)
                            Exit For
                        End If
                    Next

                Next

                answer += temp_answer

            Loop

            Day8 = answer

        End If
    End Function

    Function Day9(part As Int16) As Int64

        Dim grid(,) As Integer, r As Integer, c As Integer, gridWidth As Integer, gridHeight As Integer
        Dim inputStrings(1000) As String
        Dim IsLow(,) As Boolean, currentBasin As Integer

        Dim answer As Int64

        stringreader = filereader.ReadLine()
        gridWidth = Len(stringreader) - 1
        r = 0

        Do While (stringreader IsNot Nothing)

            inputStrings(r) = stringreader

            r = r + 1

            stringreader = filereader.ReadLine()

        Loop

        gridHeight = r - 1
        ReDim Preserve inputStrings(gridHeight)
        ReDim grid(gridHeight, gridWidth)
        ReDim IsLow(gridHeight, gridWidth)

        If part = 1 Then

            For r = 0 To gridHeight
                For c = 0 To gridWidth
                    grid(r, c) = Val(Mid(inputStrings(r), c + 1, 1))
                    IsLow(r, c) = True
                Next
            Next

            For r = 0 To gridHeight
                For c = 0 To gridWidth
                    Select Case c
                        Case 0
                            Select Case r
                                Case 0
                                    If grid(r + 1, c) <= grid(r, c) Then IsLow(r, c) = False
                                    If grid(r, c + 1) <= grid(r, c) Then IsLow(r, c) = False
                                Case 1 To gridHeight - 1
                                    If grid(r + 1, c) <= grid(r, c) Then IsLow(r, c) = False
                                    If grid(r - 1, c) <= grid(r, c) Then IsLow(r, c) = False
                                    If grid(r, c + 1) <= grid(r, c) Then IsLow(r, c) = False
                                Case gridHeight
                                    If grid(r - 1, c) <= grid(r, c) Then IsLow(r, c) = False
                                    If grid(r, c + 1) <= grid(r, c) Then IsLow(r, c) = False
                            End Select
                        Case 1 To gridWidth - 1
                            Select Case r
                                Case 0
                                    If grid(r + 1, c) <= grid(r, c) Then IsLow(r, c) = False
                                    If grid(r, c + 1) <= grid(r, c) Then IsLow(r, c) = False
                                    If grid(r, c - 1) <= grid(r, c) Then IsLow(r, c) = False
                                Case 1 To gridHeight - 1
                                    If grid(r + 1, c) <= grid(r, c) Then IsLow(r, c) = False
                                    If grid(r - 1, c) <= grid(r, c) Then IsLow(r, c) = False
                                    If grid(r, c + 1) <= grid(r, c) Then IsLow(r, c) = False
                                    If grid(r, c - 1) <= grid(r, c) Then IsLow(r, c) = False
                                Case gridHeight
                                    If grid(r - 1, c) <= grid(r, c) Then IsLow(r, c) = False
                                    If grid(r, c + 1) <= grid(r, c) Then IsLow(r, c) = False
                                    If grid(r, c - 1) <= grid(r, c) Then IsLow(r, c) = False
                            End Select
                        Case gridWidth
                            Select Case r
                                Case 0
                                    If grid(r + 1, c) <= grid(r, c) Then IsLow(r, c) = False
                                    If grid(r, c - 1) <= grid(r, c) Then IsLow(r, c) = False
                                Case 1 To gridHeight - 1
                                    If grid(r + 1, c) <= grid(r, c) Then IsLow(r, c) = False
                                    If grid(r - 1, c) <= grid(r, c) Then IsLow(r, c) = False
                                    If grid(r, c - 1) <= grid(r, c) Then IsLow(r, c) = False
                                Case gridHeight
                                    If grid(r - 1, c) <= grid(r, c) Then IsLow(r, c) = False
                                    If grid(r, c - 1) <= grid(r, c) Then IsLow(r, c) = False
                            End Select

                    End Select

                    If IsLow(r, c) Then answer += 1 + grid(r, c)
                Next
            Next


            Day9 = answer
        Else

            Dim temp As Integer

            currentBasin = 0

            For r = 0 To gridHeight
                For c = 0 To gridWidth
                    temp = Val(Mid(inputStrings(r), c + 1, 1))
                    If temp = 9 Then grid(r, c) = -1 Else grid(r, c) = 0
                Next
            Next

            For r = 0 To gridHeight
                For c = 0 To gridWidth

                    If grid(r, c) = 0 Then

                        currentBasin += 1

                        CheckNeighbors(grid, r, c, currentBasin)

                    End If

                Next
            Next

            Array.Sort(basins)
            Array.Reverse(basins)

            answer = basins(0) * basins(1) * basins(2)

            Day9 = answer

        End If
    End Function

    Function Day10(part As Int16) As Int64

        Dim answer As Int64
        Dim paren As Int16, brack As Int16, brace As Int16, gtlt As Int16
        Dim noChange As Boolean, strLen As Int16, spot As Int16, target As String, tempSpot As Int16, oldstring As String
        Dim score() As Int64, c As Integer

        If part = 1 Then

            stringreader = filereader.ReadLine()

            Do While (stringreader IsNot Nothing)

                strLen = Len(stringreader)

                Do Until noChange = True
                    noChange = True
                    oldstring = stringreader.Clone()
                    For t = 1 To 4
                        Select Case t
                            Case 1
                                target = "()"
                            Case 2
                                target = "[]"
                            Case 3
                                target = "{}"
                            Case 4
                                target = "<>"
                            Case Else
                                target = ""
                        End Select

                        oldstring = Replace(oldstring, target, "")
                    Next

                    If stringreader = oldstring Then
                        noChange = True
                    Else
                        noChange = False
                        stringreader = oldstring
                    End If

                Loop

                noChange = False

                stringreader = Replace(stringreader, "(", "")
                stringreader = Replace(stringreader, "[", "")
                stringreader = Replace(stringreader, "{", "")
                stringreader = Replace(stringreader, "<", "")

                If Len(stringreader) > 0 Then
                    Select Case Left(stringreader, 1)
                        Case ")"
                            answer += 3
                        Case "]"
                            answer += 57
                        Case "}"
                            answer += 1197
                        Case ">"
                            answer += 25137
                    End Select
                End If

                stringreader = filereader.ReadLine()

            Loop

            Day10 = answer
        Else
            c = 0
            stringreader = filereader.ReadLine()

            Do While (stringreader IsNot Nothing)

                strLen = Len(stringreader)

                Do Until noChange = True
                    noChange = True
                    oldstring = stringreader.Clone()
                    For t = 1 To 4
                        Select Case t
                            Case 1
                                target = "()"
                            Case 2
                                target = "[]"
                            Case 3
                                target = "{}"
                            Case 4
                                target = "<>"
                            Case Else
                                target = ""
                        End Select

                        oldstring = Replace(oldstring, target, "")
                    Next

                    If stringreader = oldstring Then
                        noChange = True
                    Else
                        noChange = False
                        stringreader = oldstring
                    End If

                Loop

                noChange = False

                If stringreader.Contains(")") Or stringreader.Contains(">") Or stringreader.Contains("]") Or stringreader.Contains("}") Then
                Else
                    stringreader = StrReverse(stringreader)
                    ReDim Preserve score(c)
                    score(c) = 0
                    For t = 1 To Len(stringreader)
                        score(c) *= 5
                        Select Case Mid(stringreader, t, 1)
                            Case "("
                                score(c) += 1
                            Case "["
                                score(c) += 2
                            Case "{"
                                score(c) += 3
                            Case "<"
                                score(c) += 4
                        End Select
                    Next
                    c = c + 1
                End If

                stringreader = filereader.ReadLine()

            Loop

            Array.Sort(score)

            answer = score((score.Length - 1) / 2)

            Day10 = answer

        End If
    End Function

    Function Dayx(part As Int16) As Int64


        Dim answer As Int64

        If part = 1 Then

            stringreader = filereader.ReadLine()

            Do While (stringreader IsNot Nothing)



                stringreader = filereader.ReadLine()

            Loop

            Dayx = answer
        Else
            stringreader = filereader.ReadLine()

            Do While (stringreader IsNot Nothing)



                stringreader = filereader.ReadLine()
            Loop

            Dayx = answer

        End If
    End Function


    ' *************  Put all real Functions Below this line

    Sub CheckNeighbors(ByRef grid(,) As Integer, r As Integer, c As Integer, currentbasin As Integer)

        grid(r, c) = currentbasin
        basins(currentbasin) += 1

        'check north
        If r > 0 Then
            If grid(r - 1, c) = 0 Then
                CheckNeighbors(grid, r - 1, c, currentbasin)
            End If
        End If

        'Check east
        If c < UBound(grid, 2) Then
            If grid(r, c + 1) = 0 Then
                CheckNeighbors(grid, r, c + 1, currentbasin)
            End If
        End If

        'check south
        If r < UBound(grid, 1) Then
            If grid(r + 1, c) = 0 Then
                CheckNeighbors(grid, r + 1, c, currentbasin)
            End If
        End If

        'check west
        If c > 0 Then
            If grid(r, c - 1) = 0 Then
                CheckNeighbors(grid, r, c - 1, currentbasin)
            End If
        End If

    End Sub

    Function Getxy(value As String) As Vector2
        Dim spot As Int16

        spot = InStr(value, ",")

        Getxy.x = Val(Left(value, spot - 1))
        Getxy.y = Val(Mid(value, spot + 1, 100))

    End Function

    Function GetAngle(v1 As Int64, v2 As Int64) As Integer
        If v2 > v1 Then
            GetAngle = 1
        ElseIf v2 < v1 Then
            GetAngle = -1
        Else
            GetAngle = 0
        End If

    End Function

    Function Checkboard(board As StrucBoard) As Boolean

        For r = 0 To 4
            If board.marked(r, 0) And board.marked(r, 1) And board.marked(r, 2) And board.marked(r, 3) And board.marked(r, 4) Then
                Return True
            End If
        Next

        For c = 0 To 4
            If board.marked(0, c) And board.marked(1, c) And board.marked(2, c) And board.marked(3, c) And board.marked(4, c) Then
                Return True
            End If
        Next

        Return False

    End Function

    Function Totalboard(board As StrucBoard) As Int64
        Dim answer As Int64

        For r = 0 To 4
            For c = 0 To 4
                If board.marked(r, c) = False Then
                    answer += board.grid(r, c)
                End If
            Next
        Next

        Return answer

    End Function

    Function CloneDictionary(Dict) As Dictionary(Of Int64, StrucBoard)
        Dim newDict As Dictionary(Of Int64, StrucBoard)
        newDict = New Dictionary(Of Int64, StrucBoard)

        For Each key In Dict.Keys
            newDict.Add(key, Dict(key))
        Next

        CloneDictionary = newDict
    End Function

    Function CalcFuelCost(pos As Int16, creatures As List(Of Int16)) As Int64

        Dim cost As Int64

        For i = 0 To creatures.Count - 1

            cost += Math.Abs(pos - creatures(i))

        Next

        CalcFuelCost = cost

    End Function

    Function CalcFuelCost2(pos As Int16, creatures As List(Of Int16)) As Int64

        Dim cost As Int64, differ As Int64

        For i = 0 To creatures.Count - 1

            differ = Math.Abs(pos - creatures(i))

            For t = 0 To differ
                cost += t
            Next

        Next

        CalcFuelCost2 = cost

    End Function

    Function SortString(value As String) As String

        Dim chars() = value.ToArray

        If Len(value) = 0 Then
            Return ""
        ElseIf Len(value) = 1 Then
            Return value
        Else

            Array.Sort(chars)

            Dim temp As New String(chars)

            Return temp

        End If

    End Function

    Function IsSubset(value As String, subset As String) As Boolean

        For t = 1 To Len(subset)
            If value.Contains(Mid(subset, t, 1)) = False Then
                Return False
            End If
        Next

        Return True

    End Function

End Class
