' CMM2 Tool to convert binaries into CSUBs
' (c) 2026 Matthias Grimm

OPTION EXPLICIT
OPTION BASE 0
OPTION CONSOLE SCREEN
OPTION DEFAULT NONE

CONST VER.VER$ = "1"
CONST VER.REV$ = "0"
CONST VER.DATE$ = "1.5.2026"

#INCLUDE "inc/config.inc"
#INCLUDE "inc/error.inc"
#INCLUDE "inc/fileio.inc"

DIM as.input$
DIM as.output$

PRINT
PRINT "Binary to CSUB converter"
PRINT "Version V" + VER.VER$ + "." + VER.REV$ + " (" + VER.DATE$ + ") by Matthias Grimm"

DIM n%=1, c%
DIM arg$, sig$, path$
DO WHILE as.input$=""
    arg$ = FIELD$(MM.CMDLINE$,n%," ")
    IF arg$="" THEN EXIT DO
    IF LEFT$(arg$,1) = "-" THEN
	FOR c%=2 TO LEN(arg$)
	    SELECT CASE MID$(arg$,c%,1)
	    CASE "o"
		IF c% <> LEN(arg$) THEN printError("Output file missing!") : EXIT DO
		inc n%
		as.output$ = FIELD$(MM.CMDLINE$,n%," ")
	    CASE ELSE
		printError("Unknown switch '-"+MID$(arg$,c%,1)+"'!") : EXIT DO
	    END SELECT
	NEXT
    ELSE
	as.input$ = arg$
    ENDIF
    inc n%
LOOP

IF as.input$="" THEN
    PRINT "Usage: "+CHR$(34)+"bin2csub.bas"+CHR$(34)+", "+CHR$(34)+"[-o <output file>] <input file>"+CHR$(34)
    PRINT "       -o = output file or <input>.csub if missing"
    PRINT "       If -o is omited, it will be tried to extract the CSUB signature from the filename"
    PRINT "       <input> is mandatory. if the extension is missing '.bin' will be attached."

ELSE
    IF getExt$(as.input$)="" THEN as.input$ = as.input$+".bin"
    sig$=stripPath$(stripExt$(as.input$))
    csub.name$=FIELD$(sig$,1,"_")
    FOR n%=0 TO 10
	csub.prms$(n%)=FIELD$(sig$,n%+2,"_")
    NEXT
    
    readBinary(as.input$)

    IF as.output$="" THEN
	IF csub.name$ <> "" THEN
	    path$=getPath$(as.input$)
	    as.output$=path$+csub.name$+".csub"
	    sig$ = csub.name$
	    FOR n%=0 TO 10
		IF csub.prms$(n%)="" THEN EXIT FOR
		sig$ = sig$ + choice(n%=0," ",",") + csub.prms$(n%)
	    NEXT
	    printCSUBSignature()
	ELSE
	    as.output$ = stripExt$(as.input$) + ".csub"
	    sig$ = stripPath$(stripExt$(as.output$))
	ENDIF
    ENDIF

    io.entry%=0
    writeCSUB(as.output$, sig$)
    PRINT "Out: " + as.output$
ENDIF

