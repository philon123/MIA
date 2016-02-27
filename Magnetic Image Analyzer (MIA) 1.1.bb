;Changelog
;
;v1.1, 26.05.2011
;- changed name of input file to fix bug that prevented images to be previewed
;- all variables are reset once a picture is fully analysed
;- small changes in GUI
;
;v1.0, summer 2010
; original release

AppTitle "MIA - Magnetic Image Analyzer"


Graphics 800,600,2,2
Global bubbleSizeLimit%=0
Global colorFilter%=0
Global noiseFilter%=0

Global imgBank=CreateBank(1536*1536*2) ;0=black, 1=white, 2=marked, 3=visited

Type domain
	Field colour%
	Field size%=0
End Type

Type markedPixel
	Field xPos%
	Field yPos%
End Type

Dim bwData#(1,4) ;0=number of pixels, 1=num of domains, 2=average domain size, 3=number of bubbles, 4=area of bubbles

Global image
Global imgPath$
Global HEIGHT%=0
Global WIDTH%=0
Global numPixels%=0

;################## USER INPUT
Print "At any time, type 'help' to recieve information on the next parameter."
Print "If you find a bug or have any suggestions, please contact me at psalter234@gmail.com"
Print ""

;set bubbleSizeLimit
.menu1
tmp$=Input$("Please enter bubble size limit (160): >>")
If tmp="help" Then
	Print "  This number determines what size a domain has to be (in pixels) to be counted as a stripe."
	Print ""
	Goto menu1
Else If Int(tmp)=0 Then
	Print "Faulty input. Using default value (160)"
	Print ""
	bubbleSizeLimit=160
Else 
	bubbleSizeLimit=Int(tmp)
EndIf

;set colorFilter
.menu2
tmp$=Input$("Please enter filter for zeropoint (78): >>")
If tmp="help" Then
	Print "  This number adjusts the threshhold between up domains and down domains (higher value = more black!)."
	Print "  Recomended values: 78 for brown/yellow images, 128 for grayscale."
	Print ""
	Goto menu2
Else If Int(tmp)=0 Then
	Print "Faulty input. Using default value (78)"
	Print ""
	colorFilter=78
Else 
	colorFilter=Int(tmp)
EndIf

;set noiseFilter
.menu3
tmp$=Input$("Please enter filter for noise (30): >>")
If tmp="help" Then
	Print "  Any domain smaller than this number (in pixels) will be ignored."
	Print ""
	Goto menu3
Else If Int(tmp)=0 Then
	Print "Faulty input. Using default value (30)"
	Print ""
	noiseFilter=30
Else 
	colorFilter=Int(tmp)
EndIf

;ask for preview
.menu4
tmp$=Input$("Do you want to preview only? >>")
If tmp="help" Then
	Print "  This will only use the first image in your 'MIA_inputFilenames.txt' file."
	Print "  If you type 'yes' the image will NOT be analysed, you will only see the black/white conversion."
	Print "  While previewing, press any key to restart the program."
	Print ""
	Goto menu4
Else If tmp="yes" Or tmp="y" Then
	
	Cls
	
	filInput=ReadFile("MIA_inputFilenames.txt")
	imgPath$=ReadLine(filInput)
	If imgPath="" RuntimeError "Please specify an image to analysein the 'MIA_inputFilenames.txt' file!"
	CloseFile filInput
	
	image=LoadImage(imgPath)
	If image=0 RuntimeError "There is no image at this location!"
	HEIGHT =ImageHeight (image)
	WIDTH = ImageWidth (image)
	numPixels = HEIGHT*WIDTH
	
	prepareImage()
	WaitKey()
	
	Cls
	Goto menu1
Else
	Print "Image will be analysed normally."
	Print ""
EndIf

Print "Press any key to start image analysis or [ESC] to restart."
key=WaitKey()
If key=27 Cls : Goto menu1
Print "Starting..."

;##################### analyse multiple images

filInput=ReadFile("MIA_inputFilenames.txt")
If filInput=0 RuntimeError "Please create an 'MIA_inputFilenames.txt' !"

Repeat
	imgPath$=ReadLine(filInput);readClipBoardText()
	If imgPath="" RuntimeError "Please specify an image to analyse!"
	
	image=LoadImage(imgPath) 
	If image=0 RuntimeError "There is no image at this location!"
	
	HEIGHT =ImageHeight (image)
	WIDTH = ImageWidth (image)
	numPixels = HEIGHT*WIDTH
	analyseImage()
	
	;reset all variables---
	For i=0 To 1
		For j=0 To 4
			bwData(i,j) = 0
		Next
	Next
	
	For d.domain = Each domain
		Delete d
	Next
	
	For m.markedPixel = Each markedPixel
		Delete m
	Next
	;----------------------
	
Until Eof(filInput)

End




;########################################
;FUNCTIONS
;########################################

Function analyseImage()
	
	startTime%=MilliSecs()
	prepareImage()
	drawBank()
	processImage()
	processDomains()
	countThings()
	
	filBlack = WriteFile(imgPath + " - Black.txt")
	filWhite = WriteFile(imgPath + " - White.txt")
	filData  = WriteFile(imgPath + " - Data.txt")
	filOutput = OpenFile("MIA_outputData.txt")
	If filOutput=0 RuntimeError "Please create an 'MIA_outputData.txt' !"
	
	While Not Eof(filOutput)
		ReadLine filoutput
	Wend
	
	;write filData
	WriteLine filData, "Time taken (millisecs): " + (MilliSecs()-startTime)
	WriteLine filData, "Black:"
	WriteLine filData, "    Pixels:       " + bwData(0,0)
	WriteLine filData, "    Domains:      " + bwData(0,1)
	WriteLine filData, "    Average Size: " + bwData(0,2)
	WriteLine filData, "    Area:         " + (bwData(0,0)/numPixels)*100 + "%"
	WriteLine filData, "    Bubbles:      " + bwData(0,3)
	WriteLine filData, "    Stripes:      " + (bwData(0,1) - bwData(0,3))
	WriteLine filData, "    Bub/Str:      " + bwData(0,3)/(bwData(0,1) - bwData(0,3))
	WriteLine filData, "    Bub Area/Str Area: " + bwData(0,4)/(bwData(0,0)-bwData(0,4))
	WriteLine filData, ""
	WriteLine filData, "White:"
	WriteLine filData, "    Pixels:       " + bwData(1,0)
	WriteLine filData, "    Domains:      " + bwData(1,1)
	WriteLine filData, "    Average Size: " + bwData(1,2)
	WriteLine filData, "    Area:         " + (bwData(1,0)/numPixels)*100 + "%"
	WriteLine filData, "    Bubbles:      " + bwData(1,3)
	WriteLine filData, "    Stripes:      " + (bwData(1,1) - bwData(1,3))
	WriteLine filData, "    Bub/Str:      " + bwData(1,3)/(bwData(1,1) - bwData(1,3))
	WriteLine filData, "    Bub Area/Str Area: " + bwData(1,4)/(bwData(1,0)-bwData(1,4))
	WriteLine filData, ""
	WriteLine filData, "Ratio B/W Pixels: " + bwData(0,0)/bwData(1,0)
	WriteLine filData, "Noise:            " + (1 - ( (bwData(0,0)/numPixels) + (bwData(1,0)/numPixels) )) * 100 + "%"
	WriteLine filData, "Net magnetization: " + ((bwData(0,0)/numPixels) - (bwData(1,0)/numPixels) )
	
	;write filBlack and filWhite
	
	bLine$=""
	wLine$=""
	
	For d.domain = Each domain
		If d\colour = 1 Then
			WriteLine filWhite,d\size + ""
			;wLine = wLine + d\size + " "
		Else
			WriteLine filBlack,d\size + ""
			;bLine = bLine + d\size + " "
		EndIf
	Next
	;wLine = Mid(wLine,1,Len(wLine)-1)
	;WriteLine filWhite,wLine
	;bLine = Mid(bLine,1,Len(bLine)-1)
	;WriteLine filBlack,bLine
	
	;write filOutput
	temp$ = ""
	temp = temp + imgPath + " "
	temp = temp + bwData(0,0) +" ";black pixels
	temp = temp + bwData(0,1) + " " ;black domains
	temp = temp + bwData(0,2) + " ";average domain size
	temp = temp + (bwData(0,0)/numPixels)*100 + " ";%black
	temp = temp + bwData(0,3) + " " ;black bubbles
	temp = temp + (bwData(0,1) - bwData(0,3)) + " " ;black stripes
	temp = temp + bwData(0,3)/(bwData(0,1) - bwData(0,3))+ " " ;black bubbles/stripes
	temp = temp + bwData(0,4)/(bwData(0,0)-bwData(0,4)) + " " ;black bubble area / stripe area
	temp = temp + bwData(1,0) + " ";white pixels
	temp = temp + bwData(1,1) + " " ;white domains
	temp = temp + bwData(1,2) + " ";average domain size
	temp = temp + (bwData(1,0)/numPixels)*100 + " ";%white
	temp = temp + bwData(1,3) + " " ;white bubbles
	temp = temp + (bwData(1,1) - bwData(1,3)) + " " ;white stripes
	temp = temp + bwData(1,3)/(bwData(1,1) - bwData(1,3)) + " " ;white bubbles/stripes
	temp = temp + bwData(1,4)/(bwData(1,0)-bwData(1,4)) + " ";white bubble area / stripe area
	temp = temp + bwData(0,0)/bwData(1,0) + " ";ratio b/w
	temp = temp + (1 - ( (bwData(0,0)/numPixels) + (bwData(1,0)/numPixels) )) * 100 + " ";%noise
	temp = temp + ((bwData(0,0)/numPixels) - (bwData(1,0)/numPixels) ) ;net magnetization
	
	WriteLine filOutput, temp
	
	CloseFile filBlack
	CloseFile filWhite
	CloseFile filData
	CloseFile filOutput
	
	Cls
	
	
End Function
;########################################
Function processImage()

	currDomainColor%=-1
	marked%=2
	visited%=3
	Local d.domain
	
	
	For x=0 To (WIDTH-1)
		For y=0 To HEIGHT-1
			
			finished=0
			
			currDomainColor=PeekShort(imgBank, (x+y*WIDTH)*2)
			
			If currDomainColor=0 Or currDomainColor=1 Then
				
				d.domain = New domain
				d\colour = currDomainColor
				
				m.markedPixel = New markedPixel
				m\xPos=x
				m\yPos=y
				
				Repeat
					
					PokeShort imgBank,(m\xPos+m\yPos*WIDTH)*2, visited
					d\size = d\size+1
					
					For xx=m\xPos-1 To m\xPos+1
						For yy=m\yPos-1 To m\yPos+1
							;If Not (xx=0 And yy=0) Then
								If xx>-1 And xx<WIDTH
									If yy>-1 And yy<HEIGHT
									
										If PeekShort(imgBank, (xx+yy*WIDTH)*2)=currDomainColor Then
											PokeShort imgBank, (xx+yy*WIDTH)*2,marked
											mm.markedPixel = New markedPixel
											mm\xPos=xx
											mm\yPos=yy
										EndIf
										
									EndIf
								EndIf
							;EndIf
						Next
					Next
										
					Delete m
					m = First markedPixel
					
					;drawBank()
					;Delay 1000
					
					If m=Null finished=1
				
				Until finished=1
				If KeyDown(28) drawBank()
				
				;Stop
				If countPixelsInList()>0 Then
					drawBank()
					RuntimeError "FAIL"
				EndIf
				
			EndIf
			
		Next
	Next
End Function
;########################################
Function countPixelsInList%()
	x%=0
	For counter.markedPixel = Each markedPixel
		x=x+1
	Next
	
	Return x
	
End Function
;########################################
Function prepareImage()
	
	Cls
	
	SetBuffer ImageBuffer(image)
	LockBuffer ImageBuffer(image)
	
	;make image black and white
	For x=0 To WIDTH-1
		For y=0 To HEIGHT-1
			rgb = ReadPixelFast(x, y)
			r = (rgb And $FF0000) / $10000
			g = (rgb And $FF00) / $100
			b = rgb And $FF
			
			If (r+g+b)/3 > colorFilter Then ;FILTER change as needed (higher means more black)<<<<<<<<<<<<<<<
				PokeShort imgBank, (x+y*WIDTH)*2, 1 ;white
				WritePixelFast x, y, $FFFFFF
			Else 
				PokeShort imgBank, (x+y*WIDTH)*2, 0 ;black
				WritePixelFast x, y, $000000
			EndIf
			
		Next
	Next
	
	UnlockBuffer ImageBuffer(image)
	SetBuffer FrontBuffer()
	
	drawBank()
	
End Function
;########################################
Function drawBank()
	
	SetBuffer ImageBuffer(image)
	LockBuffer ImageBuffer(image)
	
	For x=0 To WIDTH-1
		For y=0 To HEIGHT-1
			colour=PeekShort(imgBank,(x+y*WIDTH)*2)
			If colour=0 WritePixelFast x,y,$000000
			If colour=1 WritePixelFast x,y,$FFFFFF
			If colour=2 WritePixelFast x,y,$0000FF
			If colour=3 WritePixelFast x,y,$FF0000
		Next
	Next
	
	UnlockBuffer ImageBuffer(image)
	SetBuffer FrontBuffer()
	
	DrawImage image,0,0
	
End Function
;########################################
Function countThings()
	
	For d.domain = Each domain
		If d\colour=1 Then
			bwData(1,0) = bwData(1,0) + d\size ;pixels
			bwData(1,1) = bwData(1,1) + 1	   ;domains
			If d\size<bubbleSizeLimit Then 
				bwData(1,3) = bwData(1,3) + 1 ;bubble?
				bwData(1,4) = bwData(1,4) + d\size
			EndIf
		Else
			bwData(0,0) = bwData(0,0) + d\size ;pixels
			bwData(0,1) = bwData(0,1) + 1	   ;domains
			If d\size<bubbleSizeLimit Then
				bwData(0,3) = bwData(0,3) + 1 ;bubble?
				bwData(0,4) = bwData(0,4) + d\size
			EndIf
		EndIf
	Next
	
	bwData(0,2) = bwData(0,0)/bwData(0,1) ;pixels/domain
	bwData(1,2) = bwData(1,0)/bwData(1,1) ;pixels/domain

End Function
;########################################
Function processDomains()
	
	For d.domain=Each domain
		If d\size<30 Delete d
	Next
	
End Function
;########################################