Public Class Form1

    Sub run() Handles Button1.Click

        ' indicator of how much I don't care
        Dim initialFramePath As String = "TestResouces\MenuNewSkirmishSingleBackground.png"
        Dim destinationFramePath As String = "TestResouces\DLG_RANDOM_SCENARIO_BG.png"
        Dim maskDirectory = "TestResouces\mask"

        ' read stuff
        Dim initialImage As ImageIO.ColorMap = ImageIO.ReadFile(initialFramePath)
        Dim destinationImage As ImageIO.ColorMap = ImageIO.ReadFile(destinationFramePath)

        Dim readMask As Boolean = True
        Dim mask() As ImageIO.ColorMap
        If readMask Then
            mask = ImageIO.ReadDirectory(maskDirectory)
        Else
            mask = MaskGenerator.Create(initialImage.xBound + 1, initialImage.yBound + 1, 31)
            Dim outMaskDir As String = ".\GeneratedMask"
            Call RecreateDir(outMaskDir)
            For i As Integer = 0 To UBound(mask) Step 1
                Call ImageIO.WriteFile(outMaskDir & "\gen_" & Format(i, "0000") & ".png", mask(i), Imaging.ImageFormat.Png)
            Next i
        End If

        ' mix images
        Dim mixed() As ImageIO.ColorMap = GenerateAnimation(initialImage, destinationImage, mask, New Settings)

        ' clear output folder
        Dim outDir As String = ".\Result"
        Call RecreateDir(outDir)
        'save result
        For i As Integer = 0 To UBound(mixed) Step 1
            Call ImageIO.WriteFile(outDir & "\mixed_" & Format(i, "0000") & ".png", mixed(i), Imaging.ImageFormat.Png)
        Next i
        End
    End Sub
    Private Sub RecreateDir(path As String)
        If IO.Directory.Exists(path) Then
            Dim files() As String = IO.Directory.GetFiles(path)
            For Each f As String In files
                Kill(f)
            Next f
        Else
            IO.Directory.CreateDirectory(path)
        End If
    End Sub

    Class Settings
        Public transparancyChangeFramesWindow As Integer = 2
        Public transparancyChangeSmoothPixelsWindow As Integer = 25
        Public initialTransparancyGradientFramesWindow As Integer = 3
    End Class

    Function GenerateAnimation(initialImage As ImageIO.ColorMap,
                               destinationImage As ImageIO.ColorMap,
                               mask() As ImageIO.ColorMap,
                               s As Settings) As ImageIO.ColorMap()
        Dim destinationImageFirstFrame(initialImage.xBound, initialImage.yBound) As Integer
        Dim destinationImageWeightMap(UBound(mask))(,) As Double
        Dim result(UBound(mask)) As ImageIO.ColorMap

        ' find frames where a pixel becomes one from destinationImage
        For j As Integer = 0 To initialImage.yBound Step 1
            For i As Integer = 0 To initialImage.xBound Step 1
                destinationImageFirstFrame(i, j) = UBound(mask)
            Next i
        Next j
        System.Threading.Tasks.Parallel.For(0, initialImage.yBound + 1,
        Sub(j As Integer)
            For i As Integer = 0 To initialImage.xBound Step 1
                For n As Integer = 0 To UBound(mask) Step 1
                    If mask(n).AverageRGB(i, j) > 5 Then
                        destinationImageFirstFrame(i, j) = n
                        Exit For
                    End If
                Next n
            Next i
        End Sub)

        System.Threading.Tasks.Parallel.For(0, mask.Length,
        Sub(n As Integer)
            ReDim destinationImageWeightMap(n)(initialImage.xBound, initialImage.yBound)
            Dim weightMap(initialImage.xBound, initialImage.yBound) As Double
            ' create rough transparancy map for frame n. Will be applied for inital image
            'Dim gradientLen As Integer = 0
            'If n < s.initialTransparancyGradientFramesWindow Then
            '    For j As Integer = 0 To initialImage.yBound Step 1
            '        For i As Integer = gradientLen To initialImage.xBound Step 1
            '            If mask(n + s.initialTransparancyGradientFramesWindow).AverageRGB(i, j) < 5 Then
            '                gradientLen = i
            '                Exit For
            '            End If
            '        Next i
            '    Next j
            'End If
            For j As Integer = 0 To initialImage.yBound Step 1
                For i As Integer = 0 To initialImage.xBound Step 1
                    Dim d As Integer = destinationImageFirstFrame(i, j) - n
                    If d <= 0 Then
                        weightMap(i, j) = mask(n).AverageRGB(i, j) / CDbl(Byte.MaxValue)
                    ElseIf d < s.transparancyChangeFramesWindow Then
                        weightMap(i, j) = 1 - d / s.transparancyChangeFramesWindow
                        Dim m As Double = mask(n).AverageRGB(i, j) / CDbl(Byte.MaxValue)
                        If m > 0 Then weightMap(i, j) *= m
                    ElseIf d >= s.transparancyChangeFramesWindow Then
                        weightMap(i, j) = 0
                    Else
                        Throw New Exception
                    End If
                    'If i < gradientLen Then
                    '    Dim m1 As Double = 1
                    '    Dim m2 As Double = 0.5 + 0.5 * n / s.initialTransparancyGradientFramesWindow
                    '    Dim gMultiplier As Double = m1 - (m1 - m2) * i / gradientLen
                    '    weightMap(i, j) *= gMultiplier
                    'End If
                Next i
            Next j

            ' apply median smooth
            Dim w As Integer = s.transparancyChangeSmoothPixelsWindow
            Dim weightSum, weightN, sumI(initialImage.xBound) As Double
            Dim p1, p2, q1, q2 As Integer
            For j As Integer = 0 To initialImage.yBound Step 1
                weightSum = 0
                p1 = Math.Max(j - w, 0)
                p2 = Math.Min(j + w, initialImage.yBound)
                weightN = 1 / (p2 - p1 + 1)
                For i As Integer = 0 To initialImage.xBound Step 1
                    sumI(i) = 0
                    For p As Integer = p1 To p2 Step 1
                        sumI(i) += weightMap(i, p)
                    Next p
                    sumI(i) *= weightN
                Next i
                For i As Integer = 0 To initialImage.xBound Step 1
                    weightSum = 0
                    q1 = Math.Max(i - w, 0)
                    q2 = Math.Min(i + w, initialImage.xBound)
                    For q As Integer = q1 To q2 Step 1
                        weightSum += sumI(q)
                    Next q
                    destinationImageWeightMap(n)(i, j) = weightSum / (q2 - q1 + 1)
                Next i
            Next j
        End Sub)

        System.Threading.Tasks.Parallel.For(0, mask.Length,
        Sub(n As Integer)
            result(n) = New ImageIO.ColorMap(initialImage.xBound + 1, initialImage.yBound + 1)
            ' mix images for frame n
            For j As Integer = 0 To initialImage.yBound Step 1
                For i As Integer = 0 To initialImage.xBound Step 1
                    result(n).pixels(i, j) = ImageIO.ColorMap.Mix(initialImage.pixels(i, j), _
                                                                  destinationImage.pixels(i, j), _
                                                                  1 - destinationImageWeightMap(n)(i, j), _
                                                                   destinationImageWeightMap(n)(i, j))
                Next i
            Next j
        End Sub)
        Return result
    End Function

End Class

Public Class ImageIO

    Public Class ColorMap
        Public xBound As Integer
        Public yBound As Integer
        Public pixels(,) As Color

        Public Sub New(ByRef img As Bitmap)
            xBound = img.Width - 1
            yBound = img.Height - 1
            ReDim pixels(xBound, yBound)
            For j As Integer = 0 To yBound Step 1
                For i As Integer = 0 To xBound Step 1
                    pixels(i, j) = img.GetPixel(i, j)
                Next i
            Next j
        End Sub
        Public Sub New(width As Integer, height As Integer)
            xBound = width - 1
            yBound = height - 1
            ReDim pixels(xBound, yBound)
            For j As Integer = 0 To yBound Step 1
                For i As Integer = 0 To xBound Step 1
                    pixels(i, j) = Color.Black
                Next i
            Next j
        End Sub

        Public Function ToBitmap() As Bitmap
            Dim img As New Bitmap(xBound + 1, yBound + 1)
            For j As Integer = 0 To yBound Step 1
                For i As Integer = 0 To xBound Step 1
                    img.SetPixel(i, j, pixels(i, j))
                Next i
            Next j
            Return img
        End Function

        Public Function Brightness(i As Integer, j As Integer) As Double
            Return 0.2126 * pixels(i, j).R + 0.7152 * pixels(i, j).G + 0.0722 * pixels(i, j).B
        End Function
        Public Function AverageRGB(i As Integer, j As Integer) As Double
            Return 0.333333333333 * (CDbl(pixels(i, j).R) + CDbl(pixels(i, j).G) + CDbl(pixels(i, j).B))
        End Function

        Public Shared Function Mix(color1 As Color, color2 As Color, weight1 As Double, weight2 As Double) As Color
            Dim R As Byte = MixChannel(color1.R, color2.R, weight1, weight2)
            Dim G As Byte = MixChannel(color1.G, color2.G, weight1, weight2)
            Dim B As Byte = MixChannel(color1.B, color2.B, weight1, weight2)
            Return Color.FromArgb(R, G, B)
        End Function
        Private Shared Function MixChannel(c1 As Byte, c2 As Byte, w1 As Double, w2 As Double) As Byte
            Return CByte(Math.Min((CDbl(c1) * w1 + CDbl(c2) * w2) / (w1 + w2), CDbl(Byte.MaxValue)))
        End Function

    End Class

    Public Shared Function ReadFile(path As String) As ColorMap
        Dim img As New Bitmap(path)
        Return New ColorMap(img)
    End Function

    Public Shared Function ReadDirectory(path As String) As ColorMap()
        Dim files() As String = IO.Directory.GetFiles(path)
        Array.Sort(files)
        Dim result(UBound(files)) As ColorMap
        System.Threading.Tasks.Parallel.For(0, files.Length,
        Sub(i As Integer)
            result(i) = ReadFile(files(i))
        End Sub)
        Return result
    End Function

    Public Shared Sub WriteFile(path As String, content As ColorMap, format As Imaging.ImageFormat)
        content.ToBitmap.Save(path, format)
    End Sub

End Class

Public Class MaskGenerator


    Public Shared Function Create(width As Integer, height As Integer, nFrames As Integer) As ImageIO.ColorMap()
        Dim rnd As New RandomStackGenerator.RndValueGen()
        Dim bank As New RecollersBank(width, height, nFrames + 2)
        Dim mask(nFrames - 1)(,), gagMask(,) As Double

        For i As Integer = 0 To nFrames Step 1
            Call bank.AddRecollers()
            Call bank.MoveRecollers()
        Next i
        System.Threading.Tasks.Parallel.For(0, mask.Length,
        Sub(i As Integer)
            mask(i) = bank.MakeMask(i + 1)
        End Sub)

        'For n As Integer = 0 To nFrames Step 1
        '    ReDim gagMask(width - 1, height - 1)
        '    Dim minX As Integer
        '    For j As Integer = 0 To height - 1 Step 1
        '        For i As Integer = 0 To width - 1 Step 1
        '
        '        Next i
        '    Next j
        '
        'Next n

        Dim result(UBound(mask)) As ImageIO.ColorMap
        System.Threading.Tasks.Parallel.For(0, mask.Length,
        Sub(n As Integer)
            result(n) = New ImageIO.ColorMap(width, height)
            Dim v As Byte
            For j As Integer = 0 To height - 1 Step 1
                For i As Integer = 0 To width - 1 Step 1
                    v = CByte(Math.Max(0, Math.Min(Byte.MaxValue, CInt(255 * mask(n)(i, j)))))
                    result(n).pixels(i, j) = Color.FromArgb(v, v, v)
                Next i
            Next j
        End Sub)
        Return result
    End Function

    Private Class RecollersBank
        Public content As New List(Of Recolorer)
        Public currentFrame As Integer
        Public nFrames As Integer
        Public rnd As New RandomStackGenerator.RndValueGen
        Public frameSize As Size

        Public Sub New(_width As Integer, _height As Integer, _nFrames As Integer)
            frameSize = New Size(_width, _height)
            nFrames = _nFrames
        End Sub

        Public Sub AddRecollers()
            For i As Integer = 1 To 20 + currentFrame Step 1
                content.Add(New Recolorer(nFrames, 0))
                content.Item(content.Count - 1).SetInitial(currentFrame, frameSize.Width - 1, frameSize.Height - 1, rnd)
            Next i
        End Sub

        Public Sub MoveRecollers()
            Dim m(content.Count - 1) As List(Of Recolorer)
            System.Threading.Tasks.Parallel.For(0, content.Count,
            Sub(i As Integer)
                m(i) = content.Item(i).Move(rnd)
            End Sub)
            For i As Integer = 0 To UBound(m) Step 1
                For Each item In m(i)
                    content.Add(item)
                Next item
            Next i
            currentFrame += 1
        End Sub

        Public Function MakeMask(frame As Integer) As Double(,)
            Dim result(frameSize.Width - 1, frameSize.Height - 1) As Double
            For Each item As Recolorer In content
                Call item.AddToMask(result, frame)
            Next item
            Return result
        End Function

    End Class

    Private Class Recolorer
        Public pos() As Vector
        Public velocity() As Vector
        Public initialFrame, currentFrame As Integer
        Public generation As Integer

        Public acceleration As Double

        Public Sub New(nFrames As Integer, g As Integer)
            ReDim pos(nFrames), velocity(nFrames)
            generation = g
        End Sub

        Private Shared Function GetMinX(_currentFrame As Integer, _delimiter As Integer, maxX As Integer) As Integer
            Return CInt(maxX * _currentFrame / (1.5 * _delimiter))
        End Function
        Private Shared Function GetMaxX(_currentFrame As Integer, _delimiter As Integer, maxX As Integer) As Integer
            Return CInt(maxX * _currentFrame / (0.9 * _delimiter))
        End Function

        Public Sub SetInitial(_currentFrame As Integer, maxX As Integer, maxY As Integer, rnd As RandomStackGenerator.RndValueGen)
            Dim x1 As Integer = GetMinX(_currentFrame, pos.Length, maxX)
            Dim x2 As Integer = GetMaxX(_currentFrame, pos.Length, maxX)
            pos(_currentFrame) = New Vector(rnd.RndInt(x1, x2), rnd.RndInt(0, maxY))
            Dim vx As Integer = CInt(maxX * rnd.RndDbl(-0.05, 0.1))
            Dim vy As Integer = CInt(maxY * rnd.RndDbl(-0.1, 0.1))
            velocity(_currentFrame) = New Vector(vx, vy)
            acceleration = rnd.RndDbl(0.3, 0.85)
            currentFrame = _currentFrame
            initialFrame = _currentFrame
        End Sub

        Public Function Move(rnd As RandomStackGenerator.RndValueGen) As List(Of Recolorer)
            pos(currentFrame + 1) = pos(currentFrame) + velocity(currentFrame)
            velocity(currentFrame + 1) = acceleration * velocity(currentFrame)
            Dim result, r As New List(Of Recolorer)
            If generation < 2 Then
                Dim addN As Integer = CInt(Math.Floor(velocity(currentFrame).GetLen * 0.5))
                If addN > 0 Then
                    For i As Integer = 0 To addN Step 1
                        result.Add(CreateChild(rnd.RndDbl(0.01, 0.95), rnd))
                        r = result.Item(i).Move(rnd)
                        For Each item In r
                            result.Add(item)
                        Next item
                    Next i
                End If
            End If
            currentFrame += 1
            Return result
        End Function

        Public Function CreateChild(posMultiplier As Double, rnd As RandomStackGenerator.RndValueGen) As Recolorer
            Dim r As New Recolorer(UBound(pos), generation + 1)
            r.currentFrame = currentFrame
            r.initialFrame = currentFrame
            r.pos(currentFrame) = pos(currentFrame) + posMultiplier * velocity(currentFrame)
            r.velocity(currentFrame) = velocity(currentFrame).CreateChild(rnd)
            r.acceleration = acceleration * rnd.RndDbl(0.1, 0.9)
            Return r
        End Function

        Public Sub AddToMask(ByRef mask(,) As Double, toFrame As Integer)
            If initialFrame >= toFrame Then
                Exit Sub
            End If

            Dim cos1 As Double = 0.999
            Dim cos2 As Double = 0.995
            Dim cosDiff As Double = cos1 - cos2
            Dim lenDiff As Double = 3

            Dim p1 As Vector = pos(initialFrame)
            Dim p2 As Vector = pos(toFrame)

            Dim v1 As Vector = p1 - p2
            Dim v2 As Vector
            Dim v1len, v2len, cos, val As Double
            v1len = v1.GetLen
            Dim d As Double = Math.Ceiling(v1len * (1 - cos1 * cos1))

            Dim x1 As Integer = Math.Max(0, CInt(Math.Floor(Math.Min(p1.x, p2.x) - d)))
            Dim x2 As Integer = Math.Min(UBound(mask, 1), CInt(Math.Floor(Math.Max(p1.x, p2.x) + d)))
            Dim y1 As Integer = Math.Max(0, CInt(Math.Floor(Math.Min(p1.y, p2.y) - d)))
            Dim y2 As Integer = Math.Min(UBound(mask, 2), CInt(Math.Floor(Math.Max(p1.y, p2.y) + d)))

            For j As Integer = y1 To y2 Step 1
                For i As Integer = x1 To x2 Step 1
                    v2 = New Vector(i, j) - p2
                    v2len = v2.GetLen
                    If v2len <= v1len + lenDiff Then
                        If v2len > 0 Then
                            cos = v1.DotProduct(v2) / (v1len * v2len)
                            If cos >= cos1 Then
                                val = 1
                            ElseIf cos > cos2 Then
                                val = (cos - cos2) / cosDiff
                            Else
                                val = 0
                            End If
                            If val > 0 And v2len > v1len Then val *= 1 - (v2len - v1len) / lenDiff
                        Else
                            val = 1
                        End If
                        mask(i, j) += val
                    End If
                Next i
            Next j
        End Sub

        Public Class Vector
            Public x As Double = 0
            Public y As Double = 0

            Public Sub New(_x As Double, _y As Double)
                x = _x
                y = _y
            End Sub

            Public Function GetLen() As Double
                Return Math.Sqrt(x * x + y * y)
            End Function
            Public Function GetDist(v As Vector) As Double
                Dim x1 As Double = x - v.x
                Dim y1 As Double = y - v.y
                Return Math.Sqrt(x1 * x1 + y1 * y1)
            End Function

            Public Function DotProduct(v As Vector) As Double
                Return x * v.x + y * v.y
            End Function

            Public Function CreateChild(rnd As RandomStackGenerator.RndValueGen) As Vector
                Dim sin As Double = rnd.RndDbl(-0.15, 0.15)
                Dim cos As Double = Math.Sqrt(1 - sin * sin)
                Dim x1, y1 As Double
                If rnd.RndDbl(0, 2) > 1 Then
                    x1 = -y
                    y1 = x
                Else
                    x1 = y
                    y1 = -x
                End If
                Dim m As Double = rnd.RndDbl(0.3, 0.7)
                Return New Vector(m * (x1 * cos - y1 * sin), m * (x1 * sin + y1 * cos))
            End Function

            Public Function Copy() As Vector
                Return New Vector(x, y)
            End Function

            Public Shared Operator +(v1 As Vector, v2 As Vector) As Vector
                Return New Vector(v1.x + v2.x, v1.y + v2.y)
            End Operator
            Public Shared Operator -(v1 As Vector, v2 As Vector) As Vector
                Return New Vector(v1.x - v2.x, v1.y - v2.y)
            End Operator
            Public Shared Operator *(v1 As Double, v2 As Vector) As Vector
                Return New Vector(v1 * v2.x, v1 * v2.y)
            End Operator

        End Class

    End Class

End Class