Public Class Form1

    Sub run() Handles Button1.Click

        ' indicator of how much I don't care
        Dim initialFramePath As String = "TestResouces\MenuNewSkirmishSingleBackground.png"
        Dim destinationFramePath As String = "TestResouces\DLG_RANDOM_SCENARIO_BG.png"
        Dim maskDirectory = "TestResouces\mask"

        ' read stuff
        Dim initialImage As ImageIO.ColorMap = ImageIO.ReadFile(initialFramePath)
        Dim destinationImage As ImageIO.ColorMap = ImageIO.ReadFile(destinationFramePath)
        Dim mask() As ImageIO.ColorMap = ImageIO.ReadDirectory(maskDirectory)

        ' mix images
        Dim mixed() As ImageIO.ColorMap = GenerateAnimation(initialImage, destinationImage, mask, 5, 10)

        ' clear output folder
        Dim outDir As String = ".\Result"
        If IO.Directory.Exists(outDir) Then
            Dim files() As String = IO.Directory.GetFiles(outDir)
            For Each f As String In files
                Kill(f)
            Next f
        Else
            IO.Directory.CreateDirectory(outDir)
        End If

        'save result
        For i As Integer = 0 To UBound(mixed) Step 1
            Call ImageIO.WriteFile(outDir & "\mixed_" & Format(i, "0000") & ".png", mixed(i), Imaging.ImageFormat.Png)
        Next i
        End
    End Sub

    Function GenerateAnimation(initialImage As ImageIO.ColorMap, destinationImage As ImageIO.ColorMap, mask() As ImageIO.ColorMap, transparancyChangeFramesWindow As Integer, transparancyChangeSmoothPixelsWindow As Integer) As ImageIO.ColorMap()
        Dim destinationImageFirstFrame(initialImage.xBound, initialImage.yBound) As Integer
        Dim initialImageWeightMap(UBound(mask))(,) As Double
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
                    If mask(n).Brightness(i, j) < ImageIO.ColorMap.averageBrightness Then
                        destinationImageFirstFrame(i, j) = n
                        Exit For
                    End If
                Next n
            Next i
        End Sub)

        System.Threading.Tasks.Parallel.For(0, mask.Length,
        Sub(n As Integer)
            ReDim initialImageWeightMap(n)(initialImage.xBound, initialImage.yBound)
            Dim weightMap(initialImage.xBound, initialImage.yBound) As Double
            ' create rough transparancy map for frame n. Will be applied for inital image
            For j As Integer = 0 To initialImage.yBound Step 1
                For i As Integer = 0 To initialImage.xBound Step 1
                    Dim d As Integer = destinationImageFirstFrame(i, j) - n
                    If d <= 0 Then
                        weightMap(i, j) = 0
                    ElseIf d < transparancyChangeFramesWindow Then
                        weightMap(i, j) = d / transparancyChangeFramesWindow
                    ElseIf d >= transparancyChangeFramesWindow Then
                        weightMap(i, j) = 1
                    Else
                        Throw New Exception
                    End If
                Next i
            Next j

            ' apply median smooth
            Dim w As Integer = transparancyChangeSmoothPixelsWindow
            Dim weightSum, weightN As Double
            For j As Integer = 0 To initialImage.yBound Step 1
                For i As Integer = 0 To initialImage.xBound Step 1
                    weightSum = 0
                    weightN = 0
                    For p As Integer = Math.Max(j - w, 0) To Math.Min(j + w, initialImage.yBound) Step 1
                        For q As Integer = Math.Max(i - w, 0) To Math.Min(i + w, initialImage.xBound) Step 1
                            weightSum += weightMap(q, p)
                            weightN += 1
                        Next q
                    Next p
                    initialImageWeightMap(n)(i, j) = weightSum / weightN
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
                                                                  initialImageWeightMap(n)(i, j), _
                                                                  1 - initialImageWeightMap(n)(i, j))
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

        Public Const averageBrightness As Double = (0.2126 * 255 + 0.7152 * 255 + 0.0722 * 255) / 2
        Public Function Brightness(i As Integer, j As Integer) As Double
            Return 0.2126 * pixels(i, j).R + 0.7152 * pixels(i, j).G + 0.0722 * pixels(i, j).B
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