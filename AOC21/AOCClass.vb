Class AOCClass
	Public basins(1000) As Integer
	Public flashes As Int16
	Public cavePaths As List(Of List(Of String))

	Structure SFPair
		Public IsLeftValue As Boolean
		Public LeftPair As String
		Public LeftValue As Int16

		Public IsRightValue As Boolean
		Public RightPair As String
		Public RightValue As Int16
	End Structure

	Structure Packet
		Public version As Int16
		Public PType As Int16
		Public LenType As Int16
		Public SubpackLen As Int16
		Public Subpackets As List(Of Packet)
		Public LitValue As Int16
	End Structure

	Structure strucNode
		Public loc As Vector2
		Public diff As Int16
		Public f As Int16
		Public g As Int16
		Public h As Int16
		Public parent As Vector2
		Public name As String
	End Structure

	Structure StrucBoard
		Public ID As Integer
		Public grid(,) As Integer
		Public marked(,) As Boolean
		Public eliminated As Boolean
	End Structure

	Structure strucCave
		Public ID As String
		Public CaveExits As List(Of String)
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
		Dim noChange As Boolean, strLen As Int16, target As String, oldstring As String
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

	Function Day11(part As Int16) As Int64

		Dim octopus(,) As Int16, r As Int16, c As Int16, stringinput(9) As String
		Dim answer As Int64, simDay As Int16

		stringreader = filereader.ReadLine()

		r = 0
		c = Len(stringreader)

		Do While (stringreader IsNot Nothing)

			stringinput(r) = stringreader
			r += 1
			stringreader = filereader.ReadLine()

		Loop

		ReDim octopus(r - 1, c - 1)

		For r = 0 To UBound(octopus, 1)
			For c = 0 To UBound(octopus, 2)
				octopus(r, c) = Val(Mid(stringinput(r), c + 1, 1))
			Next
		Next

		If part = 1 Then

			For simDay = 1 To 100

				For r = 0 To UBound(octopus, 1)
					For c = 0 To UBound(octopus, 2)
						octopus(r, c) += 1
					Next
				Next

				For r = 0 To UBound(octopus, 1)
					For c = 0 To UBound(octopus, 2)

						octoFlash(octopus, r, c)

					Next
				Next
			Next

			answer = flashes

			Day11 = answer
		Else

			simDay = 0

			Do Until arraySum(octopus) = 0
				simDay += 1

				For r = 0 To UBound(octopus, 1)
					For c = 0 To UBound(octopus, 2)
						octopus(r, c) += 1
					Next
				Next

				For r = 0 To UBound(octopus, 1)
					For c = 0 To UBound(octopus, 2)

						octoFlash(octopus, r, c)

					Next
				Next
			Loop

			answer = simDay

			Day11 = answer

		End If
	End Function

	Function Day12(part As Int16) As Int64

		Dim stringinput(1000) As String
		Dim answer As Int64, r As Int16, spot As Int16, tcave(1) As String
		Dim caves As Dictionary(Of String, strucCave), tempcave As strucCave, tempPath As String
		Dim currPath As List(Of String)

		stringreader = filereader.ReadLine()

		Do While (stringreader IsNot Nothing)

			stringinput(r) = stringreader
			r += 1
			stringreader = filereader.ReadLine()

		Loop

		ReDim Preserve stringinput(r - 1)

		caves = New Dictionary(Of String, strucCave)

		For t = 0 To r - 1

			spot = InStr(stringinput(t), "-")
			tcave(0) = Left(stringinput(t), spot - 1)
			tcave(1) = Mid(stringinput(t), spot + 1, 100)

			For u = 0 To 1

				If caves.ContainsKey(tcave(u)) Then
					caves(tcave(u)).CaveExits.Add(tcave(1 - u))
				Else
					tempcave = New strucCave
					tempcave.ID = tcave(u)
					tempcave.CaveExits = New List(Of String)
					tempcave.CaveExits.Add(tcave(1 - u))

					caves.Add(tempcave.ID, tempcave)
				End If

			Next

		Next

		If part = 1 Then

			currPath = New List(Of String)
			currPath.Add("start")
			cavePaths = New List(Of List(Of String))

			findNeighbors(caves, currPath, caves("start"))

			answer = cavePaths.Count

			Day12 = answer

		Else

			currPath = New List(Of String)
			currPath.Add("start")
			cavePaths = New List(Of List(Of String))

			findNeighbors2(caves, currPath, caves("start"))

			answer = cavePaths.Count
			Console.WriteLine("")
			For Each cpath In cavePaths
				Console.WriteLine(List2String(cpath))
			Next

			Day12 = answer

		End If
	End Function

	Function Day13(part As Int16) As Int64

		Dim stringinput(1000) As String
		Dim rules(50) As String
		Dim grid(,) As Int64, r As Int16, spot As Int16, maxX As Int64, maxy As Int64
		Dim tempX As Int16, tempY As Int16
		Dim answer As Int64
		Dim tempgrid(,) As Int64, foldline As Int64

		stringreader = filereader.ReadLine()

		Do While (stringreader IsNot "")

			stringinput(r) = stringreader
			r += 1
			stringreader = filereader.ReadLine()

		Loop

		ReDim Preserve stringinput(r - 1)

		stringreader = filereader.ReadLine()

		r = 0

		Do While (stringreader IsNot Nothing)

			rules(r) = stringreader
			r += 1
			stringreader = filereader.ReadLine()

		Loop

		ReDim Preserve rules(r - 1)

		For t = 0 To stringinput.Length - 1

			spot = InStr(stringinput(t), ",")

			tempX = Val(Left(stringinput(t), spot - 1))
			tempY = Val(Mid(stringinput(t), spot + 1, 10))

			If tempX > maxX Then maxX = tempX
			If tempY > maxy Then maxy = tempY

		Next

		ReDim Preserve grid(maxX, maxy)

		For t = 0 To stringinput.Length - 1

			spot = InStr(stringinput(t), ",")

			tempX = Val(Left(stringinput(t), spot - 1))
			tempY = Val(Mid(stringinput(t), spot + 1, 10))

			grid(tempX, tempY) = 1

		Next


		ReDim Preserve grid(maxX, maxy)

		If part = 1 Then

			For r = 0 To 0 'rules.Length - 1
				spot = InStr(rules(r), "=")
				If Mid(rules(r), spot - 1, 1) = "x" Then
					foldline = Val(Mid(rules(r), spot + 1, 10))
					ReDim tempgrid(foldline - 1, UBound(grid, 2))
					For x = 0 To foldline - 1
						For y = 0 To UBound(grid, 2)
							tempgrid(x, y) = grid(x, y)
						Next
					Next

					For x = foldline + 1 To UBound(grid, 1)
						For y = 0 To UBound(grid, 2)
							tempgrid(2 * foldline - x, y) += grid(x, y)
						Next
					Next

					ReDim grid(UBound(tempgrid, 1), UBound(tempgrid, 2))

					grid = tempgrid.Clone()

				Else
					foldline = Val(Mid(rules(r), spot + 1, 10))
					ReDim tempgrid(UBound(grid, 1), foldline - 1)
					For y = 0 To foldline - 1
						For x = 0 To UBound(grid, 1)
							tempgrid(x, y) = grid(x, y)
						Next
					Next

					For y = foldline + 1 To UBound(grid, 2)
						For x = 0 To UBound(grid, 1)
							tempgrid(x, 2 * foldline - y) += grid(x, y)
						Next
					Next

					ReDim grid(UBound(tempgrid, 1), UBound(tempgrid, 2))

					grid = tempgrid.Clone()
				End If

			Next

			For x = 0 To UBound(grid, 1)
				For y = 0 To UBound(grid, 2)
					If grid(x, y) > 0 Then answer += 1
				Next
			Next

			Day13 = answer
		Else
			For r = 0 To rules.Length - 1
				spot = InStr(rules(r), "=")
				If Mid(rules(r), spot - 1, 1) = "x" Then
					foldline = Val(Mid(rules(r), spot + 1, 10))
					ReDim tempgrid(foldline - 1, UBound(grid, 2))
					For x = 0 To foldline - 1
						For y = 0 To UBound(grid, 2)
							tempgrid(x, y) = grid(x, y)
						Next
					Next

					For x = foldline + 1 To UBound(grid, 1)
						For y = 0 To UBound(grid, 2)
							tempgrid(2 * foldline - x, y) += grid(x, y)
						Next
					Next

					ReDim grid(UBound(tempgrid, 1), UBound(tempgrid, 2))

					grid = tempgrid.Clone()

				Else
					foldline = Val(Mid(rules(r), spot + 1, 10))
					ReDim tempgrid(UBound(grid, 1), foldline - 1)
					For y = 0 To foldline - 1
						For x = 0 To UBound(grid, 1)
							tempgrid(x, y) = grid(x, y)
						Next
					Next

					For y = foldline + 1 To UBound(grid, 2)
						For x = 0 To UBound(grid, 1)
							tempgrid(x, 2 * foldline - y) += grid(x, y)
						Next
					Next

					ReDim grid(UBound(tempgrid, 1), UBound(tempgrid, 2))

					grid = tempgrid.Clone()
				End If

			Next

			For x = 0 To UBound(grid, 1)
				For y = 0 To UBound(grid, 2)
					If grid(x, y) > 0 Then answer += 1
				Next
			Next

			Dim tempstring As String

			For x = 0 To UBound(grid, 1)
				tempstring = ""
				For y = 0 To UBound(grid, 2)
					If grid(x, y) = 0 Then
						tempstring &= "."
					Else
						tempstring &= "#"
					End If
				Next
				Console.WriteLine(tempstring)
			Next


			Day13 = answer

		End If
	End Function

	Function Day14(part As Int16) As Int64

		Dim answer As Int64
		Dim PolyTemp As String, t As Int64
		Dim PairInsertions As Dictionary(Of String, String)
		Dim newFormula As String, currentFormula As String, activePair As String, score As Dictionary(Of String, Int16)
		Dim tempString As String

		stringreader = filereader.ReadLine()
		PolyTemp = stringreader

		stringreader = filereader.ReadLine()
		stringreader = filereader.ReadLine()

		PairInsertions = New Dictionary(Of String, String)

		Do While (stringreader IsNot Nothing)
			PairInsertions.Add(Left(stringreader, 2), Right(stringreader, 1))
			stringreader = filereader.ReadLine()
		Loop

		currentFormula = PolyTemp

		If part = 1 Then

			For d = 1 To 10
				newFormula = Left(currentFormula, 1)
				For t = 1 To Len(currentFormula) - 1
					activePair = Mid(currentFormula, t, 2)
					newFormula &= PairInsertions(activePair) & Right(activePair, 1)
				Next
				currentFormula = newFormula
				newFormula = ""
			Next

		Else

			For d = 1 To 40
				Console.WriteLine("Day: " & d)
				newFormula = Left(currentFormula, 1)
				For t = 1 To Len(currentFormula) - 1
					activePair = Mid(currentFormula, t, 2)
					newFormula &= PairInsertions(activePair) & Right(activePair, 1)
				Next
				currentFormula = newFormula
				newFormula = ""
			Next

		End If

		score = New Dictionary(Of String, Short)

		For t = 1 To Len(currentFormula)
			tempString = Mid(currentFormula, t, 1)
			If score.ContainsKey(tempString) Then
				score(tempString) += 1
			Else
				score.Add(tempString, 1)
			End If
		Next

		Dim smax As Int16 = 1
		Dim smin As Int16 = 10000

		For Each scoreItem In score
			If scoreItem.Value > smax Then smax = scoreItem.Value
			If scoreItem.Value < smin Then smin = scoreItem.Value
		Next

		Day14 = smax - smin

	End Function

	Function Day15(part As Int16) As Int64

		Dim OpenList As Dictionary(Of Vector2, strucNode)
		Dim ClosedList As Dictionary(Of Vector2, strucNode)
		Dim answer As Int64
		Dim nodes As Dictionary(Of Vector2, strucNode)
		Dim node As strucNode
		Dim x As Int16, y As Int16, tempvec As Vector2, tempvec2 As Vector2
		Dim activeNode As strucNode, tempnode As strucNode, goal As Vector2, tempstring As String

		stringreader = filereader.ReadLine()

		nodes = New Dictionary(Of Vector2, strucNode)

		Do While stringreader IsNot Nothing

			For y = 0 To Len(stringreader) - 1
				tempvec = New Vector2
				tempvec.x = x
				tempvec.y = y

				node = New strucNode
				node.loc.x = x
				node.loc.y = y
				node.diff = Val(Mid(stringreader, y + 1, 1))
				nodes.Add(node.loc, node)
			Next

			x = x + 1
			stringreader = filereader.ReadLine()

		Loop

		tempvec.x = 0
		tempvec.y = 0

		goal.x = x - 1
		goal.y = y - 1

		OpenList = New Dictionary(Of Vector2, strucNode)
		ClosedList = New Dictionary(Of Vector2, strucNode)

		OpenList.Add(tempvec, nodes(tempvec))

		If part = 1 Then

		Else
			For t = 0 To 4
				For u = 0 To 4
					If t + u <> 0 Then

						For x = 0 To goal.x
							For y = 0 To goal.y
								tempvec = New Vector2
								tempvec.x = t * (goal.x + 1) + x
								tempvec.y = u * (goal.y + 1) + y
								tempvec2.x = x
								tempvec2.y = y

								node = New strucNode
								node.loc.x = tempvec.x
								node.loc.y = tempvec.y
								node.diff = nodes(tempvec2).diff + u + t
								If node.diff > 9 Then node.diff -= 9
								nodes.Add(node.loc, node)


							Next
						Next
					End If
				Next
			Next

			goal.x = (goal.x + 1) * 5 - 1
			goal.y = (goal.y + 1) * 5 - 1
		End If

		Do While OpenList.Count > 0

			If ClosedList.ContainsKey(goal) Then

			End If

			activeNode = OpenList(FindSmallF(OpenList))

			OpenList.Remove(activeNode.loc)
			ClosedList.Add(activeNode.loc, activeNode)

			For t = 1 To 4
				Select Case t
					Case 1
						tempvec.x = activeNode.loc.x
						tempvec.y = activeNode.loc.y - 1
					Case 2
						tempvec.x = activeNode.loc.x
						tempvec.y = activeNode.loc.y + 1
					Case 3
						tempvec.x = activeNode.loc.x - 1
						tempvec.y = activeNode.loc.y
					Case 4
						tempvec.x = activeNode.loc.x + 1
						tempvec.y = activeNode.loc.y
				End Select

				If nodes.ContainsKey(tempvec) Then

					If Not (ClosedList.ContainsKey(tempvec)) Then ' do nothing further

						If OpenList.ContainsKey(tempvec) Then
							If activeNode.g + nodes(tempvec).diff < OpenList(tempvec).g Then
								tempnode = New strucNode
								tempnode = OpenList(tempvec)
								OpenList.Remove(tempvec)
								tempnode.parent = activeNode.loc
								tempnode.g = activeNode.g + nodes(tempvec).diff
								tempnode.h = Math.Abs(goal.x - tempvec.x) + Math.Abs(goal.y - tempvec.y)
								tempnode.f = tempnode.g + tempnode.h
								OpenList.Add(tempvec, tempnode)
							End If
						Else
							tempnode = New strucNode

							tempnode.parent = activeNode.loc
							tempnode.g = activeNode.g + nodes(tempvec).diff
							tempnode.h = Math.Abs(goal.x - tempvec.x) + Math.Abs(goal.y - tempvec.y)
							tempnode.f = tempnode.g + tempnode.h
							tempnode.loc = tempvec

							OpenList.Add(tempvec, tempnode)
						End If ' see if f is smaller

					End If

				End If

			Next

		Loop

		tempvec = goal


		Do Until tempvec.x = 0 And tempvec.y = 0
			answer += nodes(tempvec).diff
			tempvec = ClosedList(tempvec).parent

		Loop

		For t = 0 To goal.y
			tempvec.x = 0
			tempvec.y = t
			tempstring &= nodes(tempvec).diff
		Next

		Console.WriteLine(tempstring)

		tempstring = ""

		For t = 0 To goal.y
			tempvec.x = goal.x
			tempvec.y = t
			tempstring &= nodes(tempvec).diff
		Next

		Console.WriteLine(tempstring)

		Day15 = answer

	End Function

	Function Day17(part As Int16) As Int64

		Dim answer As Int64
		Dim pos As Vector2
		Dim velocity As Vector2
		Dim tarxmin As Int64, tarymin As Int64
		Dim tarxmax As Int64, tarymax As Int64
		Dim spot As Int64, spot2 As Int64, spot3 As Int64, spot4 As Int64, spot5 As Int64
		Dim heightmax As Int64, InTarget As Boolean, tempvec As Vector2
		Dim drag As Int64
		Dim gravity As Int64
		Dim goodShots As Int16

		stringreader = filereader.ReadLine()

		spot = InStr(stringreader, "x=")
		spot2 = InStr(stringreader, "..")
		spot3 = InStr(stringreader, ",")
		spot4 = InStr(stringreader, "y=")
		spot5 = InStr(Mid(stringreader, spot4, 10), "..") + spot4

		tarxmin = Val(Mid(stringreader, spot + 2, spot2 - spot - 1))
		tarxmax = Val(Mid(stringreader, spot2 + 2, spot3 - spot2 - 1))
		tarymin = Val(Mid(stringreader, spot4 + 2, spot5 - spot4 - 1))
		tarymax = Val(Right(stringreader, Len(stringreader) - spot5))

		If tarymin > tarymax Then
			tempvec = swap(tarymax, tarymin)
			tarymin = tempvec.x
			tarymax = tempvec.y
		End If

		If tarxmin > tarxmax Then
			tempvec = swap(tarxmax, tarxmin)
			tarxmin = tempvec.x
			tarxmax = tempvec.y
		End If

		drag = -1
		gravity = -1

		For velspx = 1 To tarxmax
			For velspy = tarymin To 10000

				InTarget = False
				velocity.x = velspx
				velocity.y = velspy
				heightmax = 0
				pos.x = 0
				pos.y = 0

				Do Until InTarget = True Or pos.y < tarymin Or pos.x > tarxmax

					pos.x += velocity.x

					pos.y += velocity.y

					If pos.x >= tarxmin And pos.x <= tarxmax And pos.y <= tarymax And pos.y >= tarymin Then
						Console.WriteLine("Used vx= " & velspx & " vy= " & velspy)
						Console.WriteLine(" hit.  Max height = " & heightmax)
						goodShots += 1
						InTarget = True
						If heightmax > answer Then answer = heightmax
					End If

					If pos.y > heightmax Then heightmax = pos.y

					velocity.y += gravity

					If velocity.x > 0 Then
						velocity.x += drag
					ElseIf velocity.x < 0 Then
						velocity.x -= drag
					End If

				Loop
			Next
		Next

		If part = 1 Then
			Day17 = answer
		Else
			Day17 = goodShots
		End If
	End Function

	Function Day21(part As Int16) As Int64

		Dim p1pos As Int64
		Dim p2pos As Int64
		Dim currentDieNum As Int64
		Dim p1Score As Int64
		Dim p2Score As Int64
		Dim p1Turn As Boolean
		Dim rollsum As Int64
		Dim answer As Int64

		p1pos = 1
		p2pos = 10
		p1Turn = True

		If part = 1 Then

			currentDieNum = 0

			Do Until p1Score >= 1000 Or p2Score >= 1000

				rollsum = 0

				For t = 1 To 3
					currentDieNum += 1
					rollsum += currentDieNum Mod 100
				Next

				If p1Turn Then
					p1pos += rollsum
					p1Score += p1pos Mod 10
					If p1pos Mod 10 = 0 Then p1Score += 10
					p1Turn = False
				Else
					p2pos += rollsum
					p2Score += p2pos Mod 10
					If p2pos Mod 10 = 0 Then p2Score += 10
					p1Turn = True
				End If

				Do Until LitOper = 0
				LitOper = Mid(trans, cPos, 1)
				cPos += 1
				litString &= Mid(trans, cPos, 4)
				cPos += 4
			Loop

				If p1Turn Then
					answer = currentDieNum * p1Score
				Else
					answer = currentDieNum * p2Score

				End If

				Else
				Dim tracker As Int64
				Dim tempscore As Int64
				Dim targ As Int64
				Dim gamestate(11, 11, 30, 30)
				Dim tempstate(11, 11, 30, 30)

				' p1 pos, p2 pos, p1 score, p2 score, status (0 - ongoing, 1 - p1 won, 2 - p2 won)

				Dim Possibles(10) As Int64

				Possibles = {0, 0, 0, 1, 3, 6, 7, 6, 3, 1}

				gamestate(1, 10, 0, 0) = 1

				tracker = 1

				p1Turn = True

				Do Until tracker = 0

					tracker = 0

					ReDim tempstate(11, 11, 30, 30)

					For p1s = 0 To 30

						For p2s = 0 To 30

							For p1p = 1 To 10

								For p2p = 1 To 10

									If gamestate(p1p, p2p, p1s, p2s) > 0 Then

										If p1s > 20 Or p2s > 20 Then

											tempstate(p1p, p2p, p1s, p2s) += gamestate(p1p, p2p, p1s, p2s)

										Else

											For r = 3 To 9

												If p1Turn Then
													targ = p1p + r
												Else
													targ = p2p + r
												End If

												If targ > 10 Then targ -= 10

												tempscore = targ Mod 10
												If tempscore = 0 Then tempscore = 10

												If p1Turn Then
													tempstate(targ, p2p, p1s + tempscore, p2s) += gamestate(p1p, p2p, p1s, p2s) * Possibles(r)
												Else
													tempstate(p1p, targ, p1s, p2s + tempscore) += gamestate(p1p, p2p, p1s, p2s) * Possibles(r)
												End If

											Next

											tracker += gamestate(p1p, p2p, p1s, p2s)

										End If

									End If
								Next
							Next
						Next
					Next

					p1Turn = Not p1Turn

			gamestate = tempstate.Clone

			Console.WriteLine("P1Turn: " & p1Turn)
			Console.WriteLine("Tracker: " & tracker)

		Loop

		Dayx = answer

				For p1p = 1 To 10
					For p2p = 1 To 10
						answer += gamestate(p1p, p2p, p1s, p2s)
					Next
				Next
				Next
				Next

		End If

		Day21 = answer


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

	Sub findNeighbors(ByRef caves As Dictionary(Of String, strucCave), ByVal currentPath As List(Of String), currentCave As strucCave)

		Dim temppath As List(Of String), temptext As String

		For t = 0 To currentCave.CaveExits.Count - 1

			temptext = List2String(currentPath)
			Console.WriteLine(temptext & " going to try " & currentCave.CaveExits(t))

			If currentCave.CaveExits(t) = "end" Then

				Console.WriteLine("     -found end")

				currentPath.Add(currentCave.CaveExits(t))
				temppath = New List(Of String)
				temppath = currentPath.ToList()
				cavePaths.Add(temppath)
				currentPath.RemoveAt(currentPath.Count - 1)

			ElseIf currentCave.CaveExits(t) = "start" Then

				'temptext = ListtoString(currentPath)

				Console.WriteLine("      -tried to go back to start")

			ElseIf currentPath.Contains(currentCave.CaveExits(t)) And LCase(currentCave.CaveExits(t)) = currentCave.CaveExits(t) Then

				'temptext = ListtoString(currentPath)

				Console.WriteLine("     -tried to visit lower case again.")

			Else
				'temptext = ListtoString(currentPath)

				'Console.WriteLine(temptext & "  going down path " & currentCave.CaveExits(t) & ".")

				currentPath.Add(currentCave.CaveExits(t))
				findNeighbors(caves, currentPath, caves(currentCave.CaveExits(t)))

				Console.WriteLine("    - Stepping back")

				currentPath.RemoveAt(currentPath.Count - 1)

			End If
		Next
	End Sub

	Sub findNeighbors2(ByRef caves As Dictionary(Of String, strucCave), ByVal currentPath As List(Of String), currentCave As strucCave)

		Dim temppath As List(Of String), temptext As String
		Dim doubleFound As Boolean

		For t = 0 To currentCave.CaveExits.Count - 1

			temptext = List2String(currentPath)
			Console.WriteLine(temptext & " going to try " & currentCave.CaveExits(t))

			If currentCave.CaveExits(t) = "end" Then

				Console.WriteLine("     -found end")

				currentPath.Add(currentCave.CaveExits(t))
				temppath = New List(Of String)
				temppath = currentPath.ToList()
				cavePaths.Add(temppath)
				currentPath.RemoveAt(currentPath.Count - 1)

			ElseIf currentCave.CaveExits(t) = "start" Then

				'temptext = ListtoString(currentPath)

				Console.WriteLine("      -tried to go back to start")

			ElseIf LCase(currentCave.CaveExits(t)) = currentCave.CaveExits(t) Then

				doubleFound = False

				If currentPath.Contains(currentCave.CaveExits(t)) Then

					' find other doubles

					'temppath = New List(Of String)
					'temppath = currentPath.ToList()
					For Each tcave In currentPath
						If LCase(tcave) = tcave Then
							If tcave = currentCave.CaveExits(t) Then
								If CountOf(currentPath, tcave) > 1 Then
									doubleFound = True
									Console.WriteLine("     -tried to visit lower case for a third time.")
									Exit For
								End If
							Else
								If CountOf(currentPath, tcave) > 1 Then
									doubleFound = True
									Console.WriteLine("     -tried to visit lower for a second time, but had already seen " & tcave & " twice.")
									Exit For
								End If
							End If
						End If
					Next

				End If

				If doubleFound Then

				Else
					currentPath.Add(currentCave.CaveExits(t))
					findNeighbors2(caves, currentPath, caves(currentCave.CaveExits(t)))

					Console.WriteLine("    - Stepping back")

					currentPath.RemoveAt(currentPath.Count - 1)

				End If

				'temptext = ListtoString(currentPath)

				'Console.WriteLine("     -tried to visit lower case again.")

			Else
				'temptext = ListtoString(currentPath)

				'Console.WriteLine(temptext & "  going down path " & currentCave.CaveExits(t) & ".")

				currentPath.Add(currentCave.CaveExits(t))
				findNeighbors2(caves, currentPath, caves(currentCave.CaveExits(t)))

				Console.WriteLine("    - Stepping back")

				currentPath.RemoveAt(currentPath.Count - 1)

			End If

		Next

	End Sub

	Function List2String(inList As List(Of String)) As String

		For t = 0 To inList.Count - 1
			List2String &= inList(t) & ","
		Next

	End Function

	Sub octoFlash(ByRef octopus(,) As Int16, r As Int16, c As Int16)

		If octopus(r, c) > 9 Then
			flashes += 1
			octopus(r, c) = 0

			For r2 = r - 1 To r + 1
				For c2 = c - 1 To c + 1
					If r2 >= 0 And r2 <= UBound(octopus, 1) And c2 >= 0 And c2 <= UBound(octopus, 2) And (r2 <> r Or c2 <> c) Then
						If octopus(r2, c2) <> 0 Then
							octopus(r2, c2) += 1
							octoFlash(octopus, r2, c2)
						End If
					End If
				Next
			Next
		End If

	End Sub

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

	Function FindSmallF(nodeList As Dictionary(Of Vector2, strucNode)) As Vector2

		Dim lowestF As Int16

		lowestF = 10000

		For Each node In nodeList

			If node.Value.f < lowestF Then
				lowestF = node.Value.f
				FindSmallF = node.Value.loc
			End If
		Next

		Return FindSmallF
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

	Function arraySum(value(,) As Int16) As Int16
		For r = 0 To UBound(value, 1)
			For c = 0 To UBound(value, 2)
				arraySum += value(r, c)
			Next
		Next

		Return arraySum
	End Function

	Function IsSubset(value As String, subset As String) As Boolean

		For t = 1 To Len(subset)
			If value.Contains(Mid(subset, t, 1)) = False Then
				Return False
			End If
		Next

		Return True

	End Function

	Function CountOf(lst As List(Of String), value As String) As Int16
		Dim answer As Int16

		For Each item In lst
			If item = value Then answer += 1
		Next

		Return answer
	End Function

	Function swap(v1 As Int64, v2 As Int64) As Vector2

		swap.x = v2
		swap.y = v1

	End Function


End Class
